{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Generate a flow chart by from annotations from a code base.
--
-- The syntax is as follows:
--
--    expr  <- label / next / do / if / task
--    label <- "label" name
--    task <- "task" text
--    next  <- "next" name / "trigger" name
--    do    <- "do" text
--    if    <- "if" name "\n" "then" name ("\n" "else" name)?
-- 
--  where `name' and `text' are both arbitrary text.
--
-- A `label' is used to label a node in the graph.  `next' is used to
-- link the current node to another node by its label.  The text for a
-- node is written by `do', which explains what this node does, or by
-- using `if' which makes this node a conditional which goes to one of
-- two possible nodes.
--
-- Example (assuming '///' to be the declaration prefix):
--
-- /// label main
-- /// if Logged in?
-- /// then display_overview
-- /// else display_login

-- /// label display_overview
-- /// do Display overview.
-- /// next display_event
-- /// next display_paper
-- // Event list code here.
-- event_list();

-- /// label display_login
-- /// do Display login.
-- /// next try_login
-- // Login display code here.
-- display_login();

-- /// label try_login
-- /// do Check login.
-- /// next main
-- /// trigger log_access_time
-- // Login attempt code here.
-- if(check_login()) log_attempt_success();

-- /// label display_event
-- /// do Display a single event.
-- /// next display_paper
-- // Event list code here.
-- display_event();

-- /// label display_paper
-- /// do Display a single paper.
-- // Paper display code here.
-- display_paper();

-- /// label log_access_time
-- /// task Log login accesses.
-- log_login();
--
-- In other words: You have a main page which either displays a login
-- screen or lists the user's events if logged in. From the events
-- page you can get to the event page.

module Development.Flo where

import           Control.Applicative
import           Control.Monad.Error  ()
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import           Data.Char
import           Data.Maybe
import           Data.List
import           Text.Parsec          hiding ((<|>))

-- | A workflow node.
data Node =
  Node { nodeName  :: Name
       , nodeEdges :: [Edge]
       , nodeDesc  :: String
       , nodeType  :: Type
       } deriving Show

-- | Type of the node.
data Type = Action | Condition | Background
  deriving (Eq,Enum,Show)

-- | A workflow connection.
data Edge =
  Edge { edgeLabel :: String
       , edgeTo    :: Name
       } deriving Show

-- | A workflow declaration.
data Decl
  = Label Name                  -- ^ Sets the current node.
  | Next Name                   -- ^ Links to a next node (an edge).
  | Do String                   -- ^ Describes this node.
  | Task String                 -- ^ Run some task (create db entry,
                                --   delete file, send email etc.).
  | If String Name (Maybe Name) -- ^ Makes this node a conditional.
  deriving Show

-- | A node name.
newtype Name = Name String
  deriving (Eq,Show)

-- | Simple alias for the parser type.
type P = Parsec ByteString ()

-- | Wrap a string up in a digraph.
digraph :: String -> String
digraph x = "digraph G {\n" ++ x ++ "\n}"

-- | Convert a list of nodes to a Graphviz dot document.
nodesToDot :: [Node] -> String
nodesToDot nodes = concat . map nodeToDot $ nodes where
  nodeToDot Node{..} =
    normalizeName nodeName ++ " [" ++ props ++ "]\n" ++
    concat (map (edgeToDot nodeName) nodeEdges)
    where props = intercalate ","
                  ["label=" ++ show nodeDesc
                  ,"shape=" ++ case nodeType of
                                 Condition -> "house"
                                 Action -> "box"
                                 Background -> "oval"
                  ]
  edgeToDot from Edge{..} = normalizeName from ++ " -> " ++ normalizeName edgeTo ++
                            " [label=" ++ show edgeLabel ++ 
                            ",style=" ++ (if trig then "dotted" else "solid") ++
                            "]\n"
     where trig = maybe False ((==Background).nodeType) $
                    find ((==edgeTo).nodeName) nodes

-- | Normalize a node name to fit Dot syntax.
normalizeName :: Name -> String
normalizeName (Name name) = replace name where
  replace [] = []
  replace (x:xs) | isDigit x || isLetter x || x== '_' = x : replace xs
                 | otherwise = "_" ++ show (fromEnum x) ++ replace xs

-- | Converts a list of declarations to a list of nodes.
declsToNodes :: [Decl] -> [Node]
declsToNodes ds = snd $ runWriter (runStateT (go ds) Nothing) where
  go (Label name@(Name desc):xs) = do
    let setNew = put (Just $ Node name [] desc Action)
    get >>= maybe setNew (\x -> do tell [x]; setNew)
    go xs
  go (Next edge:xs) = do
    modify $ fmap $ \node ->
      if nodeType node /= Condition
         then node { nodeEdges = Edge "" edge : nodeEdges node }
         else node
    go xs
  go (Do desc:xs) = do
    modify $ fmap $ \node -> node { nodeDesc = desc }
    go xs
  go (Task desc:xs) = do
    modify $ fmap $ \node -> node { nodeDesc = desc, nodeType = Background }
    go xs
  go (If cond xthen xelse:xs) = do
    modify $ fmap $ \node ->
      node { nodeType = Condition
           , nodeDesc = cond
           , nodeEdges = [Edge "Yes" xthen] ++
                         maybe [] (return . Edge "No") xelse
           }
    go xs
  go [] = get >>= maybe (return ()) (tell . return)

-- | Parse a source file containing commented declarations.
parseFile :: FilePath -> String -> Maybe String -> IO (Either ParseError [Decl])
parseFile path start end = do
  contents <- B.readFile path
  return $ parse (parseDeclsInSource startP endP)
                 path
                 (contents `mappend` "\n")
  
  where startP = spaces *> string start *> pure ()
        endP = maybe (void $ lookAhead newline)
                     (void.string)
                      end
        void p = p *> pure ()

-- | Parse all line-separated prefixed declarations in a source file.
parseDeclsInSource :: P () -> P () -> P [Decl]
parseDeclsInSource start end = do
  ls <- many1 (floComment <|> normalSource) <* eof
  return $ catMaybes ls
  
  where floComment = try (Just <$> parseDecl start end)
        normalSource = const Nothing <$> manyTill anyChar newline

-- | Parse a declaration (spanning many lines in some cases e.g. "if").
parseDecl :: P () -> P () -> P Decl
parseDecl start end = do
  start
  keyword <- choice $ map (try.string) ["label","next","do","if","trigger","task"]
  space; spaces
  value <- manyTill anyChar (try $ lookAhead end)
  end
  case keyword of
    "if"      -> parseIfClauses value start end
    "next"    -> return $ Next $ Name value
    "trigger" -> return $ Next $ Name value
    "do"      -> return $ Do value
    "task"    -> return $ Task value
    _         -> return $ Label $ Name value

-- | Parse the then/else clauses of the if with the given condition.
parseIfClauses :: String -> P () -> P () -> P Decl
parseIfClauses cond start end = do
  start
  string "then"
  space; spaces
  value <- manyTill anyChar (try $ lookAhead end)
  end
  elseClause <- Just <$> (parseElseClause start end) <|> return Nothing
  return $ If cond (Name value) elseClause

-- | Parse the else clause for an `if' expression.
parseElseClause :: P () -> P () -> P Name
parseElseClause start end = do
  start
  string "else"
  space; spaces
  value <- manyTill anyChar (try $ lookAhead newline)
  end
  return $ Name value

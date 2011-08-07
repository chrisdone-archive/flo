{-# LANGUAGE ViewPatterns #-}

-- | Generate a flow chart by scanning for annotations from a code base.

module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Development.Flo
import System.Environment
import System.IO
import Text.Regex

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  let files = filter (not.opt) args
      optish = filter opt args
      opts  = catMaybes (map parseOpt optish)
      opt   = isPrefixOf "-"
  when (any (=="-v") optish) (hPrint stderr opts)
  mapM (scanAndPrint opts) files >>= putStrLn.digraph.join.join

    where scanAndPrint opts file =
            fmap (either (error.show) (return.nodesToDot.declsToNodes))
                 (parseFile file start end)
                 
            where start = fromMaybe (error $ "No pattern specified for " ++ ext)
                                    (lookup ext opts)
                  end = lookup (ext++"-end") opts
                  ext = reverse.takeWhile (/='.').reverse $ file

-- | Parse an -x=y option.
parseOpt :: String -> Maybe (String,String)
parseOpt (matchRegex (mkRegex "^-([a-zA-Z-]+)=(.+)$") -> Just [lang,start])
  | all (not.null) [lang,start] = Just (lang,start)
parseOpt _ = Nothing

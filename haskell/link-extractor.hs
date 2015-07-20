#!/usr/bin/env runhaskell
-- dependencies: libghc-pandoc-dev

-- usage: 'link-extract.hs [file]'; prints out a newline-delimited list of hyperlinks found in targeted files when parsed
-- as Pandoc markdown. Hyperlinks are not necessarily to the WWW but can be internal or interwiki hyperlinks (eg '/local/file.pdf' or '!Wikipedia').

import System.Environment (getArgs)
import Text.Pandoc (bottomUpM, def, readMarkdown, Inline(Link), Pandoc)

main :: IO ()
main = getArgs >>= mapM readFile >>= mapM_ analyzePage

analyzePage :: String -> IO Pandoc
analyzePage x = bottomUpM printLinks (readMarkdown def (unlines . drop 1 . lines $ x))

printLinks :: Inline -> IO Inline
printLinks (Link _ (x, _)) = putStrLn x >> return undefined
printLinks x                   = return x

{- draft attempt at separation of concerns:

import Control.Monad (when)
import System.Environment (getArgs)
import Text.Pandoc -- (bottomUpM, def, readMarkdown, Inline(Link), Pandoc)

main :: IO ()
main = do args <- getArgs
          let printFiles = "--print" == (args !! 0)
          fileContents <- if printFiles then mapM readFile (drop 1 args) else mapM readFile args
          let files = if printFiles then zip (drop 1 args) fileContents  else zip args fileContents
          let results = zip files (map analyzePage fileContents)
          print results

analyzePage :: String -> Block
analyzePage x = let (Pandoc _ block) = bottomUp parseLink (readMarkdown def (unlines . drop 1 . lines $ x)) in concat block

parseLink :: Inline -> Inline
parseLink (Link _ (x, _)) = Str x
parseLink _               = Str ""
-}

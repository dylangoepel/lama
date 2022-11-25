module Main where

import Parser (fparse, expr)
import Term (reprExpr)

import System.Environment  (getArgs)
import Control.Monad (join)

main :: IO ()
main = do args <- getArgs
          contents <- sequence $ map readFile args
          sequence $ map (putStrLn . reprExpr . fparse expr) contents
          return ()

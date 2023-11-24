module Main where

import Parser (lexer, arithmetic)
import System.IO (hFlush, stdout)
import Control.Monad (unless)

read' :: IO String
read' = do
  putStr "> "
  hFlush stdout
  getLine

main :: IO ()
main = do
  input <- read'
  unless (input == ":q") $ (print . arithmetic . lexer) input >> main
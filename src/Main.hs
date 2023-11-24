module Main where

import Parser (lexer, arithmetic)
import Arithmetic
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
  unless (input == ":q") $ (print . eval . arithmetic . lexer) input >> main
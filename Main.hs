module Main where

import REPL

import System.IO

header = putStrLn "clacnpr, a simple RPN calculator"
main :: IO ()
main = do
  header
  repl

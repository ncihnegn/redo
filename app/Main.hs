module Main where

import System.Process

main :: IO ()
main = do
  _ <- createProcess $ shell "sh redo.do"
  return()

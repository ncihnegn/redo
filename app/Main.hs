module Main where

import System.Process

main :: IO ()
main = do
  (_, _, _, ph) <- createProcess $ shell "sh redo.do"
  _ <- waitForProcess ph
  return()

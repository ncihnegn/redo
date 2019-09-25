module Main where

import System.Directory (renameFile)
import System.Environment (getArgs)
import System.Process (createProcess, shell, waitForProcess)

main :: IO()
main = do
  args <- getArgs
  mapM_ redo args

redo :: String-> IO ()
redo target = do
  let tmp = target ++ "---redoing"
  (_, _, _, ph) <- createProcess $ shell $ "sh " ++ target ++ ".do - - " ++ tmp ++ " > " ++ tmp
  _ <- waitForProcess ph
  renameFile tmp target
  return()

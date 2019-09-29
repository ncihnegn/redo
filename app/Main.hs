module Main where

import           Control.Monad      (filterM, liftM)
import           System.Directory   (doesFileExist, removeFile, renameFile)
import           System.Environment (getArgs)
import           System.Exit        (ExitCode (..))
import           System.FilePath    (hasExtension, replaceBaseName,
                                     takeBaseName)
import           System.IO          (hPutStrLn, stderr)
import           System.Process     (createProcess, shell, waitForProcess)

main :: IO ()
main = mapM_ redo =<< getArgs

redo :: String -> IO ()
redo target = maybe printMissing redo' =<< redoPath target
  where
    printMissing = error $ "No .do file found for target " ++ target
    redo' path = do
          (_, _, _, ph) <-
            createProcess $ shell $ "sh " ++ path ++ " 0 " ++ takeBaseName target ++ " " ++ tmp ++ " > " ++ tmp
          exit <- waitForProcess ph
          case exit of
            ExitSuccess -> renameFile tmp target
            ExitFailure code -> do
              hPutStrLn stderr $
                "Redo script exited with non-zero exit code " ++ show code
              removeFile tmp
    tmp = target ++ "---redoing"

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = safeHead `liftM` filterM doesFileExist candidates
  where
    candidates =
      [target ++ ".do"] ++
      if hasExtension target
        then [replaceBaseName target "default" ++ ".do"]
        else []
    safeHead []    = Nothing
    safeHead (x:_) = Just x

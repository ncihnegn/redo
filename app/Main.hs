module Main where

import           Control.Monad      (filterM, liftM)
import           Data.Maybe         (listToMaybe)
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
      (_, _, _, ph) <- createProcess $ shell $ cmd path
      exit <- waitForProcess ph
      case exit of
        ExitSuccess -> renameFile tmp target
        ExitFailure code -> do
          hPutStrLn stderr $
            "Redo script exited with non-zero exit code " ++ show code
          removeFile tmp
    cmd path =
      unwords ["sh ", path, " 0 ", takeBaseName target, " ", tmp, " > ", tmp]
    tmp = target ++ "---redoing"

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe `liftM` filterM doesFileExist candidates
  where
    candidates =
      [target ++ ".do"] ++
      if hasExtension target
        then [replaceBaseName target "default" ++ ".do"]
        else []

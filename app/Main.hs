module Main where

import           Control.Monad      (filterM, liftM)
import           Data.Map.Lazy      (adjust, fromList, insert, toList)
import           Data.Maybe         (listToMaybe)
--import           Debug.Trace        (traceShow)
import           System.Directory   (doesFileExist, removeFile, renameFile)
import           System.Environment (getArgs, getEnvironment)
import           System.Exit        (ExitCode (..))
import           System.FilePath    (hasExtension, replaceBaseName,
                                     takeBaseName)
import           System.IO          (hPutStrLn, stderr)
import           System.Process     (createProcess, env, shell, waitForProcess)

--traceShow' arg = traceShow arg arg

main :: IO ()
main = mapM_ redo =<< getArgs

redo :: String -> IO ()
redo target = maybe printMissing redo' =<< redoPath target
  where
    printMissing = error $ "No .do file found for target " ++ target
    redo' path = do
      oldEnv <- getEnvironment
      let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
      (_, _, _, ph) <-
        createProcess $
        --traceShow' $
        (shell $ cmd path) {env = Just newEnv}
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
      (target ++ ".do") :
        [replaceBaseName target "default" ++ ".do" | hasExtension target]

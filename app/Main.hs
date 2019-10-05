{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception    (IOException, catch, catchJust, throw)
import           Control.Monad        (filterM, guard, liftM, unless)
import qualified Data.ByteString.Lazy as BL
import           Data.Digest.Pure.MD5 (md5)
import           Data.Map.Lazy        (adjust, fromList, insert, toList)
import           Data.Maybe           (listToMaybe)
import           Data.Typeable        (typeOf)
import           Debug.Trace          (traceShow)
import           GHC.IO.Exception     (IOErrorType (..))
import           System.Directory     (createDirectoryIfMissing, doesFileExist,
                                       getDirectoryContents,
                                       removeDirectoryRecursive, removeFile,
                                       renameFile)
import           System.Environment   (getArgs, getEnvironment)
import           System.Exit          (ExitCode (..))
import           System.FilePath      (hasExtension, replaceBaseName,
                                       takeBaseName, (</>))
import           System.IO            (IOMode (..), hGetLine, hPutStrLn, stderr,
                                       withFile)
import           System.IO.Error      (ioeGetErrorType, isDoesNotExistError)
import           System.Process       (createProcess, env, shell,
                                       waitForProcess)

traceShow' arg = traceShow arg arg

main :: IO ()
main = mapM_ redo =<< getArgs

md5' ::FilePath -> IO String
md5' path = (show . md5) `liftM` BL.readFile path

redo :: String -> IO ()
redo target = do
  upToDate' <- upToDate target metaDepsDir
  unless (traceShow' upToDate') $ maybe printMissing redo' =<< redoPath target
  where
    cmd path =
      unwords ["sh ", path, " 0 ", takeBaseName target, " ", tmp, " > ", tmp]
    printMissing = error $ "No .do file found for target " ++ target
    redo' path = do
      catchJust
        (guard . isDoesNotExistError)
        (removeDirectoryRecursive metaDepsDir)
        (\_ -> return ())
      createDirectoryIfMissing True metaDepsDir
      writeFile (metaDepsDir </> path) =<< md5' path
      oldEnv <- getEnvironment
      let newEnv =
            toList $
            adjust (++ ":.") "PATH" $
            insert "REDO_TARGET" target $ fromList oldEnv
      (_, _, _, ph) <-
        createProcess $ (shell $ cmd path) {env = Just newEnv}
      exit <- waitForProcess ph
      case exit of
        ExitSuccess -> renameFile tmp target
        ExitFailure code -> do
          hPutStrLn stderr $
            "Redo script exited with non-zero exit code " ++ show code
          removeFile tmp
    metaDepsDir = ".redo" </> target
    tmp = target ++ "---redoing"

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe `liftM` filterM doesFileExist candidates
  where
    candidates =
      (target ++ ".do") :
      [replaceBaseName target "default" ++ ".do" | hasExtension target]

upToDate :: String -> FilePath -> IO Bool
upToDate target metaDepsDir = do
  deps <- getDirectoryContents metaDepsDir
  and `liftM` mapM depUpToDate deps
  where
    depUpToDate :: FilePath -> IO Bool
    depUpToDate dep =
      catch
        (do oldMD5 <- withFile (metaDepsDir </> dep) ReadMode hGetLine
            newMD5 <- md5' dep
            return $ oldMD5 == newMD5)
        (\(e :: IOException) -> return (ioeGetErrorType e == InappropriateType))

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception    (IOException, catch, catchJust)
import           Control.Monad        (filterM, guard, liftM, unless)
import qualified Data.ByteString.Lazy as BL
import           Data.Digest.Pure.MD5 (md5)
import           Data.Map.Lazy        (adjust, fromList, insert, toList)
import           Data.Maybe           (listToMaybe)
import           Debug.Trace          (traceShow)
import           GHC.IO.Exception     (IOErrorType (..))
import           System.Directory     (createDirectoryIfMissing, doesFileExist,
                                       getDirectoryContents,
                                       removeDirectoryRecursive, removeFile,
                                       renameFile)
import           System.Environment   (getArgs, getEnvironment, getProgName,
                                       lookupEnv)
import           System.Exit          (ExitCode (..))
import           System.FilePath      (hasExtension, replaceBaseName,
                                       takeBaseName, (</>))
import           System.IO            (IOMode (..), hGetLine, hPutStrLn, stderr,
                                       withFile)
import           System.IO.Error      (ioeGetErrorType, isDoesNotExistError)
import           System.Process       (createProcess, env, shell,
                                       waitForProcess)

traceShow' :: Show a => a -> a
traceShow' arg = traceShow arg arg

main :: IO ()
main = do
  mapM_ redo =<< getArgs
  progName <- getProgName
  redoTarget' <- lookupEnv "REDO_TARGET"
  case (progName, redoTarget') of
    ("redo-ifchange", Just redoTarget) ->
      mapM_ (writeMD5 redoTarget) =<< getArgs
    ("redo-ifchange", Nothing) ->
      error "Missing REDO_TARGET environment variable"
    _ -> return ()
  where
    writeMD5 redoTarget dep =
      writeFile (metaDir </> redoTarget </> dep) =<< md5' dep

md5' ::FilePath -> IO String
md5' path = (show . md5) `liftM` BL.readFile path

metaDir :: String
metaDir = ".redo"

--TODO: redo when target is missing
redo :: String -> IO ()
redo target = do
  upToDate' <- upToDate metaDepsDir
  unless upToDate' $ maybe missingDo redo' =<< redoPath target
  where
    metaDepsDir = metaDir </> target
    missingDo = do
      exists <- doesFileExist target
      unless exists . error $ "No .do file found for target " ++ target
    redo' path = do
      catchJust
        (guard . isDoesNotExistError)
        (removeDirectoryRecursive metaDepsDir)
        (\_ -> return ())
      createDirectoryIfMissing True metaDepsDir
      writeFile (metaDepsDir </> path) =<< md5' path
      oldEnv <- getEnvironment
      let newEnv =
            toList . adjust (++ ":.") "PATH" $
            insert "REDO_TARGET" target $ fromList oldEnv
      (_, _, _, ph) <-
        createProcess (shell . traceShow' $ cmd path) {env = Just newEnv}
      exit <- waitForProcess ph
      case exit of
        ExitSuccess -> renameFile tmp target
        ExitFailure code -> do
          hPutStrLn stderr $
            "Redo script exited with non-zero exit code: " ++ show code
          removeFile tmp
      where
        tmp = target ++ "---redoing"
        cmd path =
          unwords
            ["sh ", path, " 0 ", takeBaseName target, " ", tmp, " > ", tmp]

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe `liftM` filterM doesFileExist candidates
  where
    candidates =
      (target ++ ".do") :
      [replaceBaseName target "default" ++ ".do" | hasExtension target]

upToDate :: FilePath -> IO Bool
upToDate metaDepsDir =
  catch
    (do deps <- getDirectoryContents metaDepsDir
        and `liftM` mapM depUpToDate deps)
    (\(_ :: IOException) -> return False)
  where
    depUpToDate :: FilePath -> IO Bool
    depUpToDate dep =
      catch
        (do oldMD5 <- withFile (metaDepsDir </> dep) ReadMode hGetLine
            newMD5 <- md5' dep
            return $ oldMD5 == newMD5)
        (\(e :: IOException) -> return (ioeGetErrorType e == InappropriateType))

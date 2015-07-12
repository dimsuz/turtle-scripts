{-# LANGUAGE OverloadedStrings #-}

import Turtle
import System.Exit
import System.Environment
import Filesystem.Path.CurrentOS hiding (null)
import qualified Control.Foldl as Fold
import Prelude hiding (FilePath)
import Data.List (intersect)
import Control.Monad (filterM, when)
import Data.Either (rights)

-- lists all possible and supported names of drawable directory names
dirNames = [ "drawable-ldpi",
             "drawable-mdpi",
             "drawable-hdpi",
             "drawable-xhdpi",
             "drawable-xxhdpi",
             "drawable-xxxhdpi"
           ]

-- targetRootDir :: String
-- targetRootDir = "/tmp/target"

-- sourceRootDir :: String
-- sourceRootDir = "/home/dima/projects/treto/TretoAndroid/app/src/main/res"

-- drawableName :: String
-- drawableName = "ic_home"

drawableDirsIn :: FilePath -> Shell FilePath
drawableDirsIn path = find (choice dirPatterns) path
               where dirPatterns = map suffix dirNames

type ErrorString = String

checkDirs :: (String,String) -> IO (Either ErrorString (FilePath, FilePath))
checkDirs (sourceRootDir, targetRootDir) = do
  dirsExist <- mapM (testdir . decodeString) [sourceRootDir, targetRootDir]
  return $ case dirsExist of
     (_:False:_) -> Left $ "Target directory " ++ targetRootDir ++ " does not exist"
     (False:_)   -> Left $ "Source directory " ++ sourceRootDir ++ " does not exist"
     otherwise -> Right ( (decodeString sourceRootDir), (decodeString targetRootDir) )

drawableFilename :: String -> FilePath
drawableFilename name = decodeString $ name ++ ".png"

-- a file with respective name
fileListInRoot
  :: String -- a drawable name (without extension)
  -> FilePath -- a path to drawable root dir ('/res')
  -> [FilePath] -- a list of drawable dir paths within a root dir to check (relative paths)
  -> IO [FilePath] -- a list of existing files with the given name in all drawable dirs
fileListInRoot name root dirs = do
  filterM testfile (fileList root)
    where fileList dir = map (\d -> dir </> d </> drawableFilename name) dirs

getDestPath :: String -> FilePath -> FilePath -> FilePath
getDestPath name root filename = root </> (dirname filename) </> (drawableFilename name)

printActionSummary :: [FilePath] -> [FilePath] -> IO ()
printActionSummary sourceFiles destFiles = do
  putStrLn "Copying files:"
  mapM_ (\(src,dst) -> putStrLn $ show src ++ " => " ++ show dst) (zip sourceNames destNames)
  where sourceNames = rights $ map toText sourceFiles
        destNames = rights $ map toText destFiles

copyFiles :: [FilePath] -> [FilePath] -> IO ()
copyFiles sourceFiles destFiles = do
  mapM_ (\(src,dst) -> cp src dst) (zip sourceFiles destFiles)

printUsage :: IO [String]
printUsage = do
  putStrLn "Usage: dscp drawableName sourceDirectory targetDirectory"
  exitFailure
  return []

main = do
  args <- getArgs
  (name:sourceRootDir:targetRootDir:_) <- do if length args < 3 then printUsage else return args
  checkResult <- checkDirs (sourceRootDir,targetRootDir)
  (srcRoot, dstRoot) <- case checkResult of
    Left error -> do putStrLn error; exitFailure
    Right dirs -> return dirs
  sDrawableDirs <- fold (drawableDirsIn srcRoot) Fold.list
  dDrawableDirs <- fold (drawableDirsIn dstRoot) Fold.list
  let sDirNames = map filename sDrawableDirs
  let dDirNames = map filename dDrawableDirs
  let commonDirNames = intersect sDirNames dDirNames
  when (null commonDirNames) $ do putStrLn "No common drawable dirs in source/target"; exitFailure
  -- TODO find a correct extension?
  -- TODO check which dirs contain the needed file and warn if it misses in some configs
  sourceFiles <- fileListInRoot name srcRoot commonDirNames
  let destFiles = map (getDestPath name dstRoot) sourceFiles
  when (null sourceFiles) $ do
    putStrLn ("No drawables named '" ++ name ++ "' found in source dirs")
    exitFailure
  printActionSummary sourceFiles destFiles
  copyFiles sourceFiles destFiles
  putStrLn $ "Successfully copied " ++ (show $ length sourceFiles) ++ " files"

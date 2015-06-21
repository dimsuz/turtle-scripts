{-# LANGUAGE OverloadedStrings #-}

import Turtle
import System.Exit
import Filesystem.Path.CurrentOS
import qualified Control.Foldl as Fold
import Prelude hiding (FilePath)
import Data.List (intersect)

-- lists all possible and supported names of drawable directory names
dirNames = [ "drawable-ldpi",
             "drawable-mdpi",
             "drawable-hdpi",
             "drawable-xhdpi",
             "drawable-xxhdpi",
             "drawable-xxxhdpi"
           ]

targetRootDir :: String
targetRootDir = "/tmp/target"

sourceRootDir :: String
sourceRootDir = "/tmp/source"

drawableName :: String
drawableName = "ic_launcher"

drawableDirsIn :: FilePath -> Shell FilePath
drawableDirsIn path = find (choice dirPatterns) path
               where dirPatterns = map has dirNames

type ErrorString = String

checkDirs :: (String,String) -> IO (Either ErrorString (FilePath, FilePath))
checkDirs (sourceRootDir, targetRootDir) = do
  dirsExist <- mapM (testdir . decodeString) [sourceRootDir, targetRootDir]
  return $ case dirsExist of
     (_:False:_) -> Left $ "Target directory " ++ targetRootDir ++ " does not exist"
     (False:_)   -> Left $ "Source directory " ++ sourceRootDir ++ " does not exist"
     otherwise -> Right ( (decodeString sourceRootDir), (decodeString targetRootDir) )

printActionSummary :: String -> FilePath -> FilePath -> [FilePath] -> IO ()
printActionSummary name srcRoot dstRoot dirs = do
  putStrLn "Copying files:"


main = do
  let name = drawableName
  checkResult <- checkDirs (sourceRootDir,targetRootDir)
  (srcRoot, dstRoot) <- case checkResult of
    Left error -> do putStrLn error; exitFailure
    Right dirs -> return dirs
  putStrLn $ "srcRoot: " ++ (show srcRoot)
  putStrLn $ "dstRoot: " ++ (show dstRoot)
  sDrawableDirs <- fold (drawableDirsIn srcRoot) Fold.list
  dDrawableDirs <- fold (drawableDirsIn dstRoot) Fold.list
  let sDirNames = map filename sDrawableDirs
  let dDirNames = map filename dDrawableDirs
  let commonDirNames = intersect sDirNames dDirNames
  putStrLn $ "srcDirs: " ++ (show sDrawableDirs)
  putStrLn $ "dstDirs: " ++ (show dDrawableDirs)
  putStrLn $ "commonDirs: " ++ (show commonDirNames)
  -- TODO find a correct extension?
  -- TODO check which dirs contain the needed file and warn if it misses in some configs
  printActionSummary name srcRoot dstRoot commonDirNames

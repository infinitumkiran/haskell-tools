{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Directory-based migration runner - processes all .hs files in a directory tree
module Language.Haskell.Tools.Parser.DirectoryRunner
  ( runMigrationOnDirectory
  , runMigrationOnDirectories
  , findAllModules
  , processFileWithBackup
  , processFileDirect
  ) where

import Language.Haskell.Tools.Parser.Ghc984Migration hiding (runMigrationOnDirectory, migrateModule, migrateModules)
import qualified Language.Haskell.Tools.Parser.Ghc984Migration as Migration
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import System.Directory
import System.FilePath
import Control.Exception

-- | Run migration on all Haskell files found in directory recursively
runMigrationOnDirectory :: FilePath -> IO ()
runMigrationOnDirectory dirPath = do
    putStrLn $ "Scanning directory: " ++ dirPath
    files <- findAllModules dirPath
    putStrLn $ "Found " ++ show (length files) ++ " Haskell files"
    putStrLn ""

    -- Process each file with the root directory context
    results <- forM files $ \(rootDir, filePath) -> do
        result <- try @SomeException $ processFileDirect rootDir filePath
        case result of
            Left err -> do
                putStrLn $ "  FAILED: " ++ takeFileName filePath
                putStrLn $ "    Error: " ++ show err
                return False
            Right () -> return True

    let successes = length $ filter id results
    let failures = length results - successes
    putStrLn $ "\n========================================="
    putStrLn $ "Migration complete!"
    putStrLn $ "  Success: " ++ show successes
    putStrLn $ "  Failed:  " ++ show failures
    putStrLn $ "========================================="

-- | Run migration on multiple directories
runMigrationOnDirectories :: [FilePath] -> IO ()
runMigrationOnDirectories dirs = do
    putStrLn $ "Processing " ++ show (length dirs) ++ " directories"
    forM_ dirs runMigrationOnDirectory

-- | Find all .hs files recursively (excluding dist, .stack-work, etc.)
-- Returns list of (rootDir, relativePath) pairs to compute module names
findAllModules :: FilePath -> IO [(FilePath, FilePath)]
findAllModules dir = findFiltered isHaskellFile isSearchableDir dir dir
  where
    isHaskellFile = (== ".hs") . takeExtension
    isSearchableDir path = do
        let dirname = takeFileName path
        let excluded = [".git", ".stack-work", "dist", "dist-newstyle", ".cabal-sandbox", ".hpc", ".ghc.environment", "_cache"]
        return $ not (dirname `elem` excluded) && not ("." `isPrefixOf` dirname)

-- | Recursively find files matching predicate, skipping directories that fail the dir predicate
-- Takes rootDir separately to preserve it for module name computation
findFiltered :: (FilePath -> Bool) -> (FilePath -> IO Bool) -> FilePath -> FilePath -> IO [(FilePath, FilePath)]
findFiltered filePred dirPred rootDir currentDir = go currentDir
  where
    go dir = do
        entries <- listDirectory dir
        let fullPaths = map (dir </>) entries
        files <- filterM doesFileExist fullPaths
        dirs <- filterM doesDirectoryExist fullPaths
        let matchingFiles = [(rootDir, f) | f <- files, filePred f]
        searchableDirs <- filterM dirPred dirs
        subFiles <- concat <$> mapM go searchableDirs
        return (matchingFiles ++ subFiles)

-- | Common source directory names
sourceDirNames :: [FilePath]
sourceDirNames = ["src", "src-extras", "src-generated", "src-javascript"]

-- | Find all source directories under a root directory
findSourceDirs :: FilePath -> IO [FilePath]
findSourceDirs rootDir = do
    existing <- filterM doesDirectoryExist [rootDir </> d | d <- sourceDirNames]
    -- Also recursively find src dirs
    return $ rootDir : existing

-- | Process a single Haskell file (reads, transforms, writes back)
-- Takes rootDir (project root) and full file path
processFileDirect :: FilePath -> FilePath -> IO ()
processFileDirect rootDir filePath = do
    putStrLn $ "Processing: " ++ filePath

    -- Compute the source directory (e.g., rootDir/src-extras) and module name
    let (sourceDir, modName) = computeModuleInfo rootDir filePath

    putStrLn $ "  Module: " ++ modName
    putStrLn $ "  Source Dir: " ++ sourceDir

    -- Call the migration with the actual source directory
    Migration.migrateModule (sourceDir ++ "/") modName

-- | Given a project root and full file path, return (sourceDir, moduleName)
-- e.g., root="/path/ecPrelude", file="/path/ecPrelude/src-extras/Eng/Net/Shims.hs"
-- returns ("/path/ecPrelude/src-extras", "Eng.Net.Shims")
computeModuleInfo :: FilePath -> FilePath -> (FilePath, String)
computeModuleInfo rootDir filePath =
    let relPath = makeRelative rootDir filePath
        parts = splitDirectories (dropExtension relPath)
        -- First component(s) are the source directory (src, src-extras, etc.)
        (srcDirParts, modParts) = span isSourceDir parts
        sourceDir = rootDir </> joinPath srcDirParts
        modName = intercalate "." modParts
    in (sourceDir, modName)

-- | Convert a file path to a module name
-- e.g., "src/Nau/Utils/Env.hs" -> "Nau.Utils.Env"
-- Strips source directory prefixes like src/, src-extras/, etc.
pathToModuleName :: FilePath -> String
pathToModuleName path =
    let pathWithoutExt = dropExtension path
        -- Use splitDirectories which properly separates path components
        parts = splitDirectories pathWithoutExt
        -- Strip common source directory prefixes
        strippedParts = dropWhile isSourceDir parts
    in intercalate "." strippedParts

-- | Check if a path component is a source directory prefix
isSourceDir :: String -> Bool
isSourceDir s = s `elem` ["src", "src-extras", "src-generated", "src-javascript", "app", "lib", "library", "test", "tests", "bench", "benchmarks"]

-- | Process file with backup (creates .bak before modifying)
processFileWithBackup :: FilePath -> FilePath -> IO ()
processFileWithBackup rootDir filePath = do
    putStrLn $ "Processing with backup: " ++ filePath

    -- Create backup
    let backupPath = filePath ++ ".bak"
    copyFile filePath backupPath

    -- Process normally
    processFileDirect rootDir filePath

    putStrLn $ "  Backup saved to: " ++ backupPath

-- Note: filterM is imported from Control.Monad

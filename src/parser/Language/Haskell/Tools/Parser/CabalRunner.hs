{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Cabal-based migration runner - processes all modules from a cabal file
module Language.Haskell.Tools.Parser.CabalRunner
  ( runMigrationOnCabalProject
  , parseCabalModules
  , findModuleFile
  , migrateCabalProject
  ) where

import Language.Haskell.Tools.Parser.Ghc984Migration
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Control.Exception

-- | Cabal project information extracted from .cabal file
data CabalProject = CabalProject
  { projectName :: String
  , sourceDirs :: [FilePath]
  , exposedModules :: [String]
  , otherModules :: [String]
  } deriving (Show)

-- | Main entry point: run migration on all modules in a cabal project
runMigrationOnCabalProject :: FilePath -> IO ()
runMigrationOnCabalProject cabalPath = do
    putStrLn $ "Processing cabal project: " ++ cabalPath
    project <- parseCabalFile cabalPath
    putStrLn $ "Found " ++ show (length (exposedModules project)) ++ " exposed modules"
    putStrLn $ "Source directories: " ++ show (sourceDirs project)

    let allModules = exposedModules project ++ otherModules project
    let baseDir = takeDirectory cabalPath

    -- Process each module
    forM_ allModules $ \modName -> do
        result <- try @SomeException $ processModule baseDir (sourceDirs project) modName
        case result of
            Left err -> putStrLn $ "  ERROR processing " ++ modName ++ ": " ++ show err
            Right () -> return ()

    putStrLn "\nMigration complete!"

-- | Parse a cabal file to extract module information
parseCabalFile :: FilePath -> IO CabalProject
parseCabalFile path = do
    content <- readFile path
    let lines' = lines content
    let name = fromMaybe "unknown" $ extractName lines'
    let dirs = extractSourceDirs lines'
    let exposed = extractExposedModules lines'
    let other = extractOtherModules lines'
    return $ CabalProject name dirs exposed other

extractName :: [String] -> Maybe String
extractName = findExtract "name:"

extractSourceDirs :: [String] -> [FilePath]
extractSourceDirs lines' =
    let dirsSection = takeWhile (not . isEmptyLine) $
                      dropWhile (not . ("hs-source-dirs:" `isPrefixOf`)) lines'
    in concatMap extractValues dirsSection
  where
    extractValues s = case break (== ':') s of
        (_, ':':rest) -> map trim $ splitOn ',' rest
        _ -> []

extractExposedModules :: [String] -> [String]
extractExposedModules lines' =
    let startIdx = findIndex ("exposed-modules:" `isPrefixOf`) lines'
        endIdx = findNextSection lines' startIdx
        sectionLines = case (startIdx, endIdx) of
            (Just s, Just e) -> take (e - s) $ drop s lines'
            (Just s, Nothing) -> drop s lines'
            _ -> []
    in concatMap extractModuleNames sectionLines

extractOtherModules :: [String] -> [String]
extractOtherModules lines' =
    let startIdx = findIndex ("other-modules:" `isPrefixOf`) lines'
        endIdx = findNextSection lines' startIdx
        sectionLines = case (startIdx, endIdx) of
            (Just s, Just e) -> take (e - s) $ drop s lines'
            (Just s, Nothing) -> drop s lines'
            _ -> []
    in concatMap extractModuleNames sectionLines

findNextSection :: [String] -> Maybe Int -> Maybe Int
findNextSection _ Nothing = Nothing
findNextSection lines' (Just start) =
    let remaining = drop (start + 1) lines'
        mIdx = findIndex (not . isContinuationLine) remaining
    in fmap (\x -> x + start + 1) mIdx

isContinuationLine :: String -> Bool
isContinuationLine s =
    isEmptyLine s ||
    (not (null s) && isSpace (head s)) ||
    any (`isPrefixOf` s) ["    ", "     "]

isEmptyLine :: String -> Bool
isEmptyLine = all isSpace

extractModuleNames :: String -> [String]
extractModuleNames s
    | "exposed-modules:" `isPrefixOf` s = extractAfterColon s
    | "other-modules:" `isPrefixOf` s = extractAfterColon s
    | otherwise = words s

extractAfterColon :: String -> [String]
extractAfterColon s = case break (== ':') s of
    (_, ':':rest) -> words rest
    _ -> []

findExtract :: String -> [String] -> Maybe String
findExtract prefix lines' =
    case find (prefix `isPrefixOf`) lines' of
        Just line -> case break (== ':') line of
            (_, ':':rest) -> Just $ trim rest
            _ -> Nothing
        Nothing -> Nothing

-- | Split a string by a delimiter
splitOn :: Char -> String -> [String]
splitOn delim s = case break (== delim) s of
    (before, _:after) -> before : splitOn delim after
    (before, _) -> [before]

-- | Trim whitespace from both ends
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

isSpace :: Char -> Bool
isSpace c = c `elem` [' ', '\t', '\n', '\r']

-- | Find the file path for a module given source directories
findModuleFile :: FilePath -> [FilePath] -> String -> IO (Maybe FilePath)
findModuleFile baseDir sourceDirs modName = do
    let relPath = replace '.' '/' modName ++ ".hs"
    let candidates = map (\dir -> baseDir </> dir </> relPath) sourceDirs
    findM doesFileExist candidates

replace :: Char -> Char -> String -> String
replace old new = map (\c -> if c == old then new else c)

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM p (x:xs) = do
    ok <- p x
    if ok then return (Just x) else findM p xs

-- | Process a single module
processModule :: FilePath -> [FilePath] -> String -> IO ()
processModule baseDir dirs modName = do
    mfile <- findModuleFile baseDir dirs modName
    case mfile of
        Nothing -> do
            putStrLn $ "  WARNING: Could not find file for module " ++ modName
        Just filePath -> do
            let dir = takeDirectory filePath ++ "/"
            let fname = takeBaseName filePath
            putStrLn $ "Processing: " ++ modName ++ " (" ++ filePath ++ ")"
            migrateModule dir fname

-- | Alternative: parse modules and return as list without processing
parseCabalModules :: FilePath -> IO [(String, FilePath)]
parseCabalModules cabalPath = do
    project <- parseCabalFile cabalPath
    let baseDir = takeDirectory cabalPath
    let allModules = exposedModules project ++ otherModules project
    catMaybes <$> mapM (\modName -> fmap (modName,) <$> findModuleFile baseDir (sourceDirs project) modName) allModules

-- | Run migration returning success/failure count
migrateCabalProject :: FilePath -> IO (Int, Int)
migrateCabalProject cabalPath = do
    modules <- parseCabalModules cabalPath
    results <- forM modules $ \(modName, filePath) -> do
        let dir = takeDirectory filePath ++ "/"
        let fname = takeBaseName filePath
        result <- try @SomeException $ migrateModule dir fname
        case result of
            Left err -> do
                putStrLn $ "  FAILED: " ++ modName ++ " - " ++ show err
                return False
            Right () -> return True
    let successes = length $ filter id results
    let failures = length results - successes
    return (successes, failures)

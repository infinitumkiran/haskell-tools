#!/usr/bin/env runhaskell
{-# LANGUAGE TypeApplications #-}

-- | Standalone script to run GHC 9.8.4 migration on a directory
-- Usage: runhaskell run-migration.hs <directory>

import System.Environment (getArgs)
import Language.Haskell.Tools.Parser.DirectoryRunner

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> runMigrationOnDirectory dir
        [] -> do
            putStrLn "Usage: runhaskell run-migration.hs <directory>"
            putStrLn "Example: runhaskell run-migration.hs /path/to/ecPrelude"
        _ -> putStrLn "Error: Too many arguments. Provide only one directory path."

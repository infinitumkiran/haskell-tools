-- | GHC 9.8.4 Migration executable
-- Usage: ht-migrate <directory>

import System.Environment (getArgs)
import Language.Haskell.Tools.Parser.DirectoryRunner

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> runMigrationOnDirectory dir
        [] -> do
            putStrLn "Usage: ht-migrate <directory>"
            putStrLn "Example: ht-migrate /path/to/ecPrelude"
        _ -> putStrLn "Error: Too many arguments. Provide only one directory path."

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

-- | GHC 9.8.4 Rollback Script - Pure AST transformations
module Language.Haskell.Tools.Parser.Ghc984Migration
  ( migrateModule
  , migrateModules
  , runMigrationOnDirectory
  ) where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.References
import Language.Haskell.Tools.AST.Ann (Ann(..), AnnListG(..), AnnMaybeG(..))
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Parser.ParseModule
import Language.Haskell.Tools.Rewrite.Create.Utils
  (mkAnn', mkAnnList', mkAnnMaybe', noth')
import Language.Haskell.Tools.Rewrite.Create.Names (mkSimpleName'', mkNamePart')
import Language.Haskell.Tools.Rewrite.Create.Modules (mkImportDecl')
import Language.Haskell.Tools.PrettyPrint.Prepare (child, list, opt)

import GHC hiding (Name, mkModuleName)
import Control.Reference
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import Data.String (fromString)
import Data.List (isSuffixOf, isInfixOf)
import System.FilePath
import System.Directory

-- =============================================================================
-- MAIN ENTRY POINTS
-- =============================================================================

migrateModule :: String -> String -> IO ()
migrateModule modulePath moduleName = do
    putStrLn $ "Migrating: " ++ modulePath ++ moduleName
    moduleAST <- moduleParser modulePath moduleName
    transformed <- transformModule moduleAST
    -- Convert module name dots to directory separators
    let filePath = map (\c -> if c == '.' then '/' else c) moduleName ++ ".hs"
    let outputPath = modulePath ++ filePath
    -- Create parent directories if they don't exist
    let dir = takeDirectory outputPath
    createDirectoryIfMissing True dir
    writeFile outputPath (prettyPrint transformed)
    putStrLn $ "Written: " ++ outputPath
    -- Post-processing for specific modules
    appendExceptFunction modulePath moduleName

migrateModules :: [(String, String)] -> IO ()
migrateModules = mapM_ (uncurry migrateModule)

runMigrationOnDirectory :: FilePath -> IO ()
runMigrationOnDirectory dir = do
    putStrLn $ "Scanning directory: " ++ dir
    files <- findHsFiles dir
    let modules = map (splitFileName . dropExtension) files
    forM_ modules $ \(path, name) -> do
        let fullPath = if "/" `isSuffixOf` path then path else path ++ "/"
        migrateModule fullPath name

findHsFiles :: FilePath -> IO [FilePath]
findHsFiles dir = do
    contents <- listDirectory dir
    let fullPaths = map (dir </>) contents
    files <- filterM doesFileExist fullPaths
    dirs <- filterM doesDirectoryExist fullPaths
    let hsFiles = filter (\f -> takeExtension f == ".hs") files
    subFiles <- concat <$> mapM findHsFiles dirs
    return $ hsFiles ++ subFiles

-- =============================================================================
-- MODULE TRANSFORMATION PIPELINE
-- =============================================================================

transformModule :: Ann UModule (Dom GhcPs) SrcTemplateStage -> IO (Ann UModule (Dom GhcPs) SrcTemplateStage)
transformModule modAst = do
    -- Step 1: Transform imports (cryptonite, deriveGenerics, _getCurrentDate, DateParser replicateM, Except MonadTrans)
    ast1 <- (!~) (biplateRef @_ @(Ann UImportDecl (Dom GhcPs) SrcTemplateStage)) (transformImportDecl modAst) modAst
    -- Step 2: Special handling for mtl modules (full replacement)
    ast2 <- transformMtlModule ast1
    -- Step 3: Add missing imports for specific modules
    ast3 <- addMissingImports ast2
    return ast3

-- | Add missing imports that can't be handled by biplateRef
addMissingImports :: Ann UModule (Dom GhcPs) SrcTemplateStage -> IO (Ann UModule (Dom GhcPs) SrcTemplateStage)
addMissingImports modAst@(Ann ann (UModule filePragmas head imports decls)) =
    case head of
      AnnMaybeG _ (Just (Ann _ (UModuleHead (Ann _ (UModuleName mn)) _ _))) -> do
          if "DateParser" `isSuffixOf` mn || mn == "DateParser"
            then do
              -- Check if Control.Monad is already imported
              let hasControlMonad = any isControlMonadImport (getImportsList imports)
              if hasControlMonad
                then return modAst  -- Already has Control.Monad import
                else do
                  -- Add import Control.Monad (replicateM)
                  let newImport = mkImportMinimal "Control.Monad" ["replicateM"]
                  let newImports = case imports of
                        AnnListG annImp impList -> AnnListG annImp (impList ++ [newImport])
                  return $ Ann ann (UModule filePragmas head newImports decls)
            else return modAst
      _ -> return modAst
  where
    getImportsList (AnnListG _ imps) = imps
    isControlMonadImport (Ann _ (UImportDecl _ _ _ _ (Ann _ (UModuleName n)) _ _)) =
        n == "Control.Monad" || n == "qualified Control.Monad"

-- =============================================================================
-- TRANSFORMATION 1: Add RecordDotPreprocessor Pragma
-- =============================================================================

addRecordDotPragma :: Ann UModule (Dom GhcPs) SrcTemplateStage -> IO (Ann UModule (Dom GhcPs) SrcTemplateStage)
addRecordDotPragma modAst@(Ann ann (UModule filePragmas head imports decls)) = do
    let hasPragma = hasRecordDotPragma filePragmas
    if hasPragma
      then return modAst
      else do
        let newPragma = mkRecordDotPragma "-fplugin=RecordDotPreprocessor"
        let newPragmas = case filePragmas of
              AnnListG prAnn (pr:prs) -> AnnListG prAnn (newPragma : pr : prs)
              AnnListG prAnn [] -> AnnListG prAnn [newPragma]
        return $ Ann ann (UModule newPragmas head imports decls)

hasRecordDotPragma :: AnnListG UFilePragma (Dom GhcPs) SrcTemplateStage -> Bool
hasRecordDotPragma (AnnListG _ pragmas) = any isRecordDot pragmas
  where
    isRecordDot (Ann _ (ULanguagePragma (AnnListG _ exts))) =
        any (\(Ann _ (ULanguageExtension ext)) -> "RecordDotPreprocessor" `isInfixOf` ext) exts
    isRecordDot (Ann _ (UOptionsPragma (Ann _ (UStringNode opts)))) =
        "RecordDotPreprocessor" `isInfixOf` opts
    isRecordDot _ = False

mkRecordDotPragma :: String -> Ann UFilePragma (Dom GhcPs) SrcTemplateStage
mkRecordDotPragma opts = mkAnn' ("{-# OPTIONS_GHC " <> child <> " #-}")
                               (UOptionsPragma (mkAnn' (fromString opts) (UStringNode opts)))

-- =============================================================================
-- TRANSFORMATION 2: Import Declarations
-- =============================================================================

transformImportDecl :: Ann UModule (Dom GhcPs) SrcTemplateStage -> Ann UImportDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UImportDecl (Dom GhcPs) SrcTemplateStage)
transformImportDecl modAst imp = do
    imp1 <- transformCryptonImport imp
    imp2 <- transformNauRuntimeImport imp1
    imp3 <- transformDBTypesImport imp2
    imp4 <- transformDateParserImport modAst imp3
    return imp4

-- | cryptonite -> crypton
transformCryptonImport :: Ann UImportDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UImportDecl (Dom GhcPs) SrcTemplateStage)
transformCryptonImport imp@(Ann ann (UImportDecl src qual safe pkg name rename spec)) =
    case pkg of
      AnnMaybeG pkgAnn (Just (Ann strAnn (UStringNode pkgStr))) ->
          let firstWord = takeWhile (/= ' ') pkgStr
          in if firstWord == "cryptonite"
            then do
              let newPkg = AnnMaybeG pkgAnn (Just (Ann strAnn (UStringNode "crypton")))
              return $ Ann ann (UImportDecl src qual safe newPkg name rename spec)
            else return imp
      _ -> return imp
transformCryptonImport imp = return imp

-- | Add deriveGenerics to Nau.Runtime imports
transformNauRuntimeImport :: Ann UImportDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UImportDecl (Dom GhcPs) SrcTemplateStage)
transformNauRuntimeImport imp@(Ann ann (UImportDecl src qual safe pkg name rename spec)) = do
    case name of
      Ann _ (UModuleName mn) | "Nau.Runtime" `isInfixOf` mn -> do
          newSpec <- addDeriveGenerics spec
          return $ Ann ann (UImportDecl src qual safe pkg name rename newSpec)
      _ -> return imp
transformNauRuntimeImport imp = return imp

addDeriveGenerics :: AnnMaybeG UImportSpec (Dom GhcPs) SrcTemplateStage -> IO (AnnMaybeG UImportSpec (Dom GhcPs) SrcTemplateStage)
addDeriveGenerics spec@(AnnMaybeG ann Nothing) = return spec
addDeriveGenerics (AnnMaybeG ann (Just (Ann b (UImportSpecList (AnnListG c specs))))) = do
    let hasDG = any isDeriveGenerics specs
    if hasDG
      then return $ AnnMaybeG ann (Just (Ann b (UImportSpecList (AnnListG c specs))))
      else do
        -- Create deriveGenerics import spec using proper templates
        -- UIESpec structure: modifier + name + subspec
        let dgSpec = mkAnn' "deriveGenerics"
                       (UIESpec noth'
                                (mkAnn' "deriveGenerics" (UNormalName (mkSimpleName'' "deriveGenerics")))
                                (mkAnnMaybe' opt Nothing))
        return $ AnnMaybeG ann (Just (Ann b (UImportSpecList (AnnListG c (specs ++ [dgSpec])))))
addDeriveGenerics spec = return spec

isDeriveGenerics :: Ann UIESpec (Dom GhcPs) SrcTemplateStage -> Bool
isDeriveGenerics (Ann _ (UIESpec _ name _)) = getNameString name == "deriveGenerics"
isDeriveGenerics _ = False

getNameString :: Ann UName (Dom GhcPs) SrcTemplateStage -> String
getNameString nameAnn =
    case nameAnn of
      Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart n))))) -> n
      _ -> ""

-- | Remove _getCurrentDate from DB.Types
transformDBTypesImport :: Ann UImportDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UImportDecl (Dom GhcPs) SrcTemplateStage)
transformDBTypesImport imp@(Ann ann (UImportDecl src qual safe pkg name rename spec)) = do
    case name of
      Ann _ (UModuleName mn) | "DB.Types" `isInfixOf` mn -> do
          newSpec <- removeGetCurrentDate spec
          return $ Ann ann (UImportDecl src qual safe pkg name rename newSpec)
      _ -> return imp
transformDBTypesImport imp = return imp

removeGetCurrentDate :: AnnMaybeG UImportSpec (Dom GhcPs) SrcTemplateStage -> IO (AnnMaybeG UImportSpec (Dom GhcPs) SrcTemplateStage)
removeGetCurrentDate spec@(AnnMaybeG ann Nothing) = return spec
removeGetCurrentDate (AnnMaybeG ann (Just (Ann b (UImportSpecList (AnnListG c specs))))) = do
    let filtered = filter (not . isGetCurrentDate) specs
    return $ AnnMaybeG ann (Just (Ann b (UImportSpecList (AnnListG c filtered))))
removeGetCurrentDate spec = return spec

isGetCurrentDate :: Ann UIESpec (Dom GhcPs) SrcTemplateStage -> Bool
isGetCurrentDate (Ann _ (UIESpec _ name _)) = getNameString name == "_getCurrentDate"
isGetCurrentDate _ = False

-- | Check if module name is DateParser
transformDateParserImport :: Ann UModule (Dom GhcPs) SrcTemplateStage -> Ann UImportDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UImportDecl (Dom GhcPs) SrcTemplateStage)
transformDateParserImport modAst imp@(Ann ann (UImportDecl src qual safe pkg name rename spec)) = do
    -- Check if this is the DateParser module
    let isDateParser = case modAst of
                         Ann _ (UModule _ head _ _) ->
                             case head of
                               AnnMaybeG _ (Just (Ann _ (UModuleHead (Ann _ (UModuleName mn)) _ _))) ->
                                   "DateParser" `isSuffixOf` mn || mn == "DateParser"
                               _ -> False
    case name of
      Ann _ (UModuleName mn) | mn == "Control.Monad.Identity" -> do
          -- Check if replicateM is in the spec
          case spec of
            AnnMaybeG specAnn (Just (Ann b (UImportSpecList (AnnListG c specs)))) ->
                if any (isImportedName "replicateM") specs
                  then do
                    -- Remove replicateM from the spec, keep only Identity
                    let newSpecs = filter (not . isImportedName "replicateM") specs
                    let newSpec = if null newSpecs
                          then AnnMaybeG specAnn (Just (Ann b (UImportSpecList (AnnListG c [mkSimpleIESpec "Identity"]))))
                          else AnnMaybeG specAnn (Just (Ann b (UImportSpecList (AnnListG c newSpecs))))
                    return $ Ann ann (UImportDecl src qual safe pkg name rename newSpec)
                  else return imp
            _ -> return imp
      Ann _ (UModuleName mn) | isDateParser && mn == "Control.Monad" -> do
          -- Add replicateM to Control.Monad imports in DateParser
          addReplicateMToSpec imp
      _ -> return imp
transformDateParserImport _ imp = return imp

-- | Add replicateM to Control.Monad import spec
addReplicateMToSpec :: Ann UImportDecl (Dom GhcPs) SrcTemplateStage -> IO (Ann UImportDecl (Dom GhcPs) SrcTemplateStage)
addReplicateMToSpec (Ann ann (UImportDecl src qual safe pkg name rename spec)) = do
    case spec of
      AnnMaybeG specAnn (Just (Ann b (UImportSpecList (AnnListG c specs)))) -> do
          -- Check if replicateM is already imported
          if any (isImportedName "replicateM") specs
            then return $ Ann ann (UImportDecl src qual safe pkg name rename spec)
            else do
              -- Add replicateM to the import list
              let newSpecs = specs ++ [mkSimpleIESpec "replicateM"]
              let newSpec = AnnMaybeG specAnn (Just (Ann b (UImportSpecList (AnnListG c newSpecs))))
              return $ Ann ann (UImportDecl src qual safe pkg name rename newSpec)
      AnnMaybeG specAnn Nothing -> do
          -- Add explicit import list with replicateM
          let newSpec = AnnMaybeG specAnn (Just (mkAnn' "(replicateM)" (UImportSpecList (mkAnnList' (separatedBy ", " list) [mkSimpleIESpec "replicateM"]))))
          return $ Ann ann (UImportDecl src qual safe pkg name rename newSpec)
      _ -> return $ Ann ann (UImportDecl src qual safe pkg name rename spec)
addReplicateMToSpec imp = return imp

isImportedName :: String -> Ann UIESpec (Dom GhcPs) SrcTemplateStage -> Bool
isImportedName n (Ann _ (UIESpec _ name _)) = getNameString name == n
isImportedName _ _ = False

mkSimpleIESpec :: String -> Ann UIESpec (Dom GhcPs) SrcTemplateStage
mkSimpleIESpec n = mkAnn' child (UIESpec noth'
                                          (mkAnn' child (UNormalName (mkSimpleName'' n)))
                                          (mkAnnMaybe' opt Nothing))

-- =============================================================================
-- TRANSFORMATION 3: MTl-2.3 Import Adjustments
-- =============================================================================

-- | Fix specific mtl-2.3 issues for PS.Control.Monad.* modules
-- These modules need Control.Monad re-exported functions that mtl-2.3 removed.
-- NOTE: This adds Control.Monad import but does NOT add value declarations
-- like 'except = throwError' - those need manual handling.
transformMtlModule :: Ann UModule (Dom GhcPs) SrcTemplateStage -> IO (Ann UModule (Dom GhcPs) SrcTemplateStage)
transformMtlModule modAst@(Ann ann (UModule filePragmas head imports decls))
    | isTargetModule head "PS.Control.Monad.Reader" = addControlMonadImport imports decls
    | isTargetModule head "PS.Control.Monad.State" = addControlMonadImport imports decls
    | isTargetModule head "PS.Control.Monad.Except" = addControlMonadImport imports decls
    -- Also fix the .Trans modules that re-export from the base modules
    | isTargetModule head "PS.Control.Monad.Reader.Trans" = addControlMonadImport imports decls
    | isTargetModule head "PS.Control.Monad.State.Trans" = addControlMonadImport imports decls
    | isTargetModule head "PS.Control.Monad.Except.Trans" = addControlMonadImport imports decls
    | otherwise = return modAst
  where
    addControlMonadImport impList declList = do
        let hasControlMonad = any (isImportOf "Control.Monad") (getImportsList impList)
        if hasControlMonad
          then return modAst
          else do
            let newImport = mkImportFull "Control.Monad"
            let newImports = case impList of
                  AnnListG annImp imps -> AnnListG annImp (newImport : imps)
            return $ Ann ann (UModule filePragmas head newImports declList)
    getImportsList (AnnListG _ imps) = imps
    isImportOf mn (Ann _ (UImportDecl _ _ _ _ (Ann _ (UModuleName n)) _ _)) = n == mn

isTargetModule :: AnnMaybeG UModuleHead (Dom GhcPs) SrcTemplateStage -> String -> Bool
isTargetModule (AnnMaybeG _ (Just (Ann _ (UModuleHead (Ann _ (UModuleName mn)) _ _)))) target = mn == target
isTargetModule _ _ = False

mkImportFull :: String -> Ann UImportDecl (Dom GhcPs) SrcTemplateStage
mkImportFull mn = mkImportDecl' False False False Nothing (mkModuleName' mn) Nothing Nothing

-- | Post-processing hook: Append the except function to PS.Control.Monad.Except
appendExceptFunction :: String -> String -> IO ()
appendExceptFunction modulePath moduleName
  | moduleName == "PS.Control.Monad.Except" = do
      let filePath = modulePath ++ "PS/Control/Monad/Except.hs"
      content <- readFile filePath
      let hasExceptDef = "except =" `isInfixOf` content
      unless hasExceptDef $ do
        let exceptFunc = "\n-- | Compatibility shim for mtl-2.3 which removed 'except'\nexcept :: MonadError e m => Prelude.Either e a -> m a\nexcept = Prelude.either throwError return\n"
        appendFile filePath exceptFunc
        putStrLn $ "  Added except function to " ++ filePath
  | otherwise = return ()

mkImportMinimal :: String -> [String] -> Ann UImportDecl (Dom GhcPs) SrcTemplateStage
mkImportMinimal mn names = mkImportDecl' False False False Nothing (mkModuleName' mn) Nothing
                                 (Just (mkAnn' ("(" <> fromString nameList <> ")")
                                                (UImportSpecList (mkAnnList' (separatedBy ", " list)
                                                                                           (map mkIESpecName names)))))
  where
    nameList = intercalate ", " names

mkIESpecName :: String -> Ann UIESpec (Dom GhcPs) SrcTemplateStage
mkIESpecName n = mkAnn' child
                       (UIESpec noth'
                                (mkAnn' child (UNormalName (mkSimpleName'' n)))
                                (mkAnnMaybe' opt Nothing))

-- | Check if the current module is a PS.Control.Monad.* module
isPsControlMonadModule :: Ann UModule (Dom GhcPs) SrcTemplateStage -> Bool
isPsControlMonadModule (Ann _ (UModule _ head _ _)) =
    case head of
      AnnMaybeG _ (Just (Ann _ (UModuleHead (Ann _ (UModuleName mn)) _ _))) ->
          "PS.Control.Monad." `isPrefixOf` mn
      _ -> False


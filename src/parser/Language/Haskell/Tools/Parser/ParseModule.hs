{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Language.Haskell.Tools.Parser.ParseModule where
  
import Debug.Trace
import Data.Data
import GHC hiding (loadModule)
import qualified GHC
import Outputable (Outputable(..), showSDocUnsafe, cat)
import GHC.Paths ( libdir )
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Char (toLower)
import Data.List.Extra (splitOn,trim,replace, cons)
import GHC.LanguageExtensions
import Control.Exception
import Data.Functor
import Data.Maybe
import System.Directory
import System.FilePath as FP
import System.IO (withFile, hGetContents, hSetBinaryMode, IOMode(..), writeFile)
import Control.Exception (bracket)
import Debug.Trace (traceShowId)
import Shelly
import Control.Concurrent
import SrcLoc (noSrcSpan, combineSrcSpans)
import DynFlags
import Language.Haskell.Tools.BackendGHC
import Language.Haskell.Tools.PrettyPrint.Prepare
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.BackendGHC.Decls (trfDecls, trfDeclsGroup)
import Language.Haskell.Tools.BackendGHC.Exprs (trfText')
import Language.Haskell.Tools.BackendGHC.Names (TransformName, trfName)
import Language.Haskell.Tools.BackendGHC.Modules hiding (trfModuleHead)
import Language.Haskell.Tools.AST

useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }

initGhcFlags :: Ghc ()
initGhcFlags = initGhcFlags' False True

initGhcFlags' :: Bool -> Bool -> Ghc ()
initGhcFlags' needsCodeGen errorsSuppressed = do
  dflags <- getSessionDynFlags
  void $ setSessionDynFlags
    $ flip gopt_set Opt_KeepRawTokenStream
    $ flip gopt_set Opt_NoHsMain
    $ (if errorsSuppressed then flip gopt_set Opt_DeferTypeErrors
                                  . flip gopt_set Opt_DeferTypedHoles
                                  . flip gopt_set Opt_DeferOutOfScopeVariables
                           else id)
    $ foldl' (\acc x -> xopt_set acc x) (dflags { importPaths = []
             , ghcLink = if needsCodeGen then LinkInMemory else NoLink
             , ghcMode = CompManager
             , packageFlags = ExposePackage "template-haskell" (PackageArg "template-haskell") (ModRenaming True []) : packageFlags dflags
             }) [
                                BlockArguments
                                ,ConstraintKinds
                                ,DataKinds
                                ,DeriveAnyClass
                                ,DeriveDataTypeable
                                ,DeriveFoldable
                                ,DeriveFunctor
                                ,DeriveGeneric
                                ,DeriveTraversable
                                ,DerivingStrategies
                                ,DerivingVia
                                ,DuplicateRecordFields
                                ,EmptyCase
                                ,ExplicitForAll
                                ,ExplicitNamespaces
                                ,FlexibleContexts
                                ,FlexibleInstances
                                ,GADTs
                                ,GeneralizedNewtypeDeriving
                                ,ImplicitParams
                                ,ImplicitPrelude
                                ,InstanceSigs
                                ,KindSignatures
                                ,LambdaCase
                                ,MagicHash
                                ,MultiParamTypeClasses
                                ,MultiWayIf
                                ,OverloadedLabels
                                ,OverloadedStrings
                                ,PatternSynonyms
                                ,QuasiQuotes
                                ,RankNTypes
                                ,RecordWildCards
                                ,ScopedTypeVariables
                                ,TemplateHaskell
                                ,TupleSections
                                ,TypeApplications
                                ,TypeFamilies
                                ,TypeOperators
                                ,TypeSynonymInstances
                                ,UndecidableInstances
                                ,ViewPatterns
                                ,BangPatterns
                                ,AllowAmbiguousTypes
                                ,UnicodeSyntax
                                ,StandaloneDeriving
                                ,EmptyDataDecls
                                ,FunctionalDependencies
                                ,PartialTypeSignatures
                                -- ,NamedFieldPuns
                                -- ,NoImplicitPrelude
                                ,Strict
                                ,EmptyDataDeriving
                                ,PolyKinds
                                ,ExistentialQuantification 
                          ]

initGhcFlagsForTest :: Ghc ()
initGhcFlagsForTest = do initGhcFlags' True False
                         dfs <- getSessionDynFlags
                         void $ setSessionDynFlags dfs  

loadModule :: FilePath -> String -> Ghc ModSummary
loadModule workingDir moduleName
  = do initGhcFlagsForTest
       useDirs [workingDir]
       target <- guessTarget moduleName Nothing
       setTargets [target]
       void $ load (LoadUpTo $ mkModuleName moduleName)
       getModSummary $ mkModuleName moduleName         

foldLocs :: [SrcSpan] -> SrcSpan
foldLocs = foldl combineSrcSpans noSrcSpan

moduleParser :: String -> String -> IO ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage))
moduleParser modulePath moduleName = do
    -- Preprocess: strip problematic pragmas and fix package names
    let filePath = modulePath ++ map (\c -> if c == '.' then '/' else c) moduleName ++ ".hs"
    -- Use strict IO to avoid file locking issues
    !content <- readFileStrict filePath
    -- Apply text transformations BEFORE parsing:
    -- 1. Strip RecordDotPreprocessor pragmas
    -- 2. Change cryptonite -> crypton in package imports
    let !processedLines = map fixPackageName $ filter (not . isProblematicPragma) (lines content)
    let !filteredContent = unlines processedLines
    -- Only process if changes were made
    if content == filteredContent
      then parseModuleUnsafe modulePath moduleName
      else do
        -- Create a temp directory with modified file
        tmpDir <- getTemporaryDirectory
        let tempBase = tmpDir FP.</> "ht-migrate" FP.</> map (\c -> if c == '/' then '_' else c) moduleName
        createDirectoryIfMissing True tempBase
        let tempPath = tempBase FP.</> takeFileName filePath
        writeFile tempPath filteredContent
        -- Parse from temp location with proper module path setup
        let tempModulePath = tempBase ++ "/"
        result <- parseModuleUnsafe tempModulePath moduleName
        -- Cleanup
        removeFile tempPath
        removeDirectory tempBase
        return result

-- | Fix package names in import statements (text-level replacement)
fixPackageName :: String -> String
fixPackageName line = replaceAll "\"cryptonite\"" "\"crypton\"" line

-- | Replace all occurrences of old with new in a string
replaceAll :: String -> String -> String -> String
replaceAll old new str = go str
  where
    go [] = []
    go s@(c:cs)
        | old `isPrefixOf` s = new ++ go (drop (length old) s)
        | otherwise = c : go cs

-- Strict readFile to avoid lazy IO issues
readFileStrict :: FilePath -> IO String
readFileStrict fp = withFile fp ReadMode $ \h -> do
    hSetBinaryMode h False
    hGetContents h >>= \s -> length s `seq` return s

parseModuleUnsafe :: String -> String -> IO ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage))
parseModuleUnsafe modulePath moduleName = do
    dflags <- runGhc (Just libdir) getSessionDynFlags
    pp <- getCurrentDirectory
    modSum <- runGhc (Just libdir) $ loadModule modulePath moduleName
    y <- runGhc (Just libdir) $ parseModule modSum
    let annots = pm_annotations y
    valsss <- runGhc (Just libdir) $ runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule' modSum (pm_parsed_source y)
    sourceOrigin <- return (fromJust $ ms_hspp_buf $ pm_mod_summary y)
    newAst <- runGhc (Just libdir) $ (prepareAST) sourceOrigin . placeComments (fst annots) (getNormalComments $ snd annots)
        <$> (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule' modSum $ pm_parsed_source y)
    pure newAst

withTempFile :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempFile base content action = bracket createTemp cleanupTemp action
  where
    createTemp = do
        let tempPath = base ++ ".tmp"
        writeFile tempPath content
        return tempPath
    cleanupTemp tempPath = do
        exists <- doesFileExist tempPath
        when exists $ removeFile tempPath

-- | Check if a line contains a problematic pragma
isProblematicPragma :: String -> Bool
isProblematicPragma line =
    let lower = map toLower line
    in any (\p -> p `isInfixOf` lower)
        [ "-fplugin=recorddotpreprocessor"
        , "-fplugin recorddotpreprocessor"
        ]

isFunction :: _ -> Bool
isFunction (L _ (SigD _ sigDecls)) = True
isFunction (L _ (ValD _ valDecls)) = True
isFunction _ = False

isFunSig :: _ -> Bool
isFunSig (SigD _ sigDecls) = True
isFunSig _ = False

isFunVal :: _ -> Bool
isFunVal (ValD _ valDecls) = True
isFunVal _ = False

getFunctionName :: _ -> String
getFunctionName str = 
  if any (\x -> x `isInfixOf` str) ["infixl", "infixr", "infix", "INLINE", "NOINLINE"]
    then (splitOn " " $  replace "]" "" $ replace "[" "" str) !! 2
    else head . splitOn " " . replace "(" "" . replace ")" "" . replace "]" "" $ replace "[" "" $ str

groupByUltimate :: [[HsDecl GhcPs]] -> [(String,[[HsDecl GhcPs]])]
groupByUltimate = (HM.toList . foldl' (\acc x -> addToBucket acc x) HM.empty)
  where
    addToBucket :: HM.HashMap String [[HsDecl GhcPs]] -> [HsDecl GhcPs] -> HM.HashMap String [[HsDecl GhcPs]]
    addToBucket acc el = 
      let funcName = getFunctionName $ showSDocUnsafe $ ppr $ el
      in (HM.insert funcName $ 
              case HM.lookup funcName acc of
                Just x -> x ++ [el]
                _ -> [el]
          ) acc
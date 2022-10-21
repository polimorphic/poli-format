module Poli.Format (format) where

import Control.Monad (join, unless)
import Data.Char (isSpace)
import Data.Foldable (toList, traverse_)
import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Traversable (for)
import Language.Haskell.Exts
    ( Context, Decl(TypeSig)
    , Exp(LeftSection, InfixApp, List, RightSection)
    , Extension(EnableExtension), ParseMode(extensions, fixities, parseFilename)
    , KnownExtension
        ( DataKinds, DefaultSignatures, DerivingStrategies, DerivingVia
        , GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses
        , MultiWayIf, ScopedTypeVariables, TemplateHaskell, TupleSections
        , TypeApplications, TypeFamilies, TypeOperators
        )
    , ParseResult(ParseFailed, ParseOk)
    , SrcSpan
        ( SrcSpan
        , srcSpanFilename, srcSpanEndColumn, srcSpanEndLine, srcSpanStartColumn, srcSpanStartLine
        )
    , SrcSpanInfo(SrcSpanInfo, srcInfoPoints), Type(TyFun)
    , ann, baseFixities, defaultParseMode, infixl_, infixr_
    , parseFileContentsWithMode, srcSpanEnd, srcSpanStart
    )
import System.Exit (exitFailure)
import System.Directory (doesDirectoryExist, listDirectory)
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, openFile, utf8)

format :: FilePath -> IO ()
format path = do
    fmts <- formatUnknown path
    unless (null fmts) $ do
        traverse_ putStrLn fmts
        exitFailure

formatUnknown :: FilePath -> IO [String]
formatUnknown path = do
    isDir <- doesDirectoryExist path
    if | isDir -> do
           paths <- listDirectory path
           fmap join . for paths $ \entr -> do
               formatUnknown $ path <> "/" <> entr
       | ".hs" `L.isSuffixOf` path -> formatFile path
       | otherwise -> pure []

formatFile :: FilePath -> IO [String]
formatFile path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    file <- hGetContents h
    let res = parseFileContentsWithMode parseMode file
    pure $ case res of
        ParseOk m -> formatRaw path file
                  <> (formatDecl =<< universeBi m)
                  <> (formatExp =<< universeBi m)
        ParseFailed _ e -> [path <> " - " <> e]
  where
    parseMode = defaultParseMode
        { parseFilename = path
        , extensions = EnableExtension <$>
            [ DataKinds
            , DefaultSignatures
            , DerivingStrategies
            , DerivingVia
            , GeneralizedNewtypeDeriving
            , LambdaCase
            , MultiParamTypeClasses
            , MultiWayIf
            , ScopedTypeVariables
            , TemplateHaskell
            , TupleSections
            , TypeApplications
            , TypeFamilies
            , TypeOperators
            ]
        , fixities = Just $ baseFixities <> join
            [ infixl_ 8 ["^.", "^..", "^?"]
            , infixr_ 8 [".="]
            , infixl_ 4 ["!=.", "<.", "<=.", "==.", ">.", ">=."]
            , infixr_ 4 ["%~", ".~", "?~"]
            , infixr_ 3 ["&&.", ":<|>"]
            , infixr_ 2 ["||."]
            , infixl_ 1 ["&", "<&>"]
            , infixl_ 0 [":-"]
            ]
        }

formatRaw :: FilePath -> String -> [String]
formatRaw nm fl = join res
  where
    lns = lines fl
    spn = SrcSpan
        { srcSpanFilename = nm
        , srcSpanStartLine = 1
        , srcSpanStartColumn = 1
        , srcSpanEndLine = 1 + length lns
        , srcSpanEndColumn = fromMaybe 1 $ length <$> listToMaybe (reverse lns)
        }
    lspn n ln = spn
        { srcSpanStartLine = n
        , srcSpanEndLine = n
        , srcSpanEndColumn = 1 + length ln
        }
    res = zipWith (\n ln -> formatLine (lspn n ln) ln) [1 ..] (lines fl)

formatLine :: SrcSpan -> String -> [String]
formatLine spn ln
    | spacetrail > 0 = [formatError spacespn "trailing whitespace"]
    | otherwise = []
  where
    spacetrail = length . takeWhile isSpace $ reverse ln
    spacespn = spn { srcSpanStartColumn = 1 + length ln - spacetrail }

formatDecl :: Decl SrcSpanInfo -> [String]
formatDecl (TypeSig spn _ ty) = formatTypeSig spn ty
formatDecl _ = []

formatTypeSig :: SrcSpanInfo -> Type SrcSpanInfo -> [String]
formatTypeSig (SrcSpanInfo spn pts) ty
    | and [ null ctxs || checkAligment (ctxs <> funs)
          , checkAligment (pts <> ctxs <> funs)
          ] = []
    | otherwise = [formatError spn "misaligned type signature"]
  where
    ctxs = take 1 . reverse . srcInfoPoints . ann @Context =<< universeBi ty
    funs = srcInfoPoints =<< mapMaybe getTyFun (universeBi ty)

formatExp :: Exp SrcSpanInfo -> [String]
formatExp (InfixApp _ e1 op e2) = formatInfixApp (ann e1) (ann op) (ann e2)
formatExp (List spn _) = formatList spn
formatExp (LeftSection _ e1 op) = formatLeftSection (ann e1) (ann op)
formatExp (RightSection _ op e1) = formatRightSection (ann op) (ann e1)
formatExp _ = []

formatLeftSection  :: SrcSpanInfo -> SrcSpanInfo -> [String]
formatLeftSection (SrcSpanInfo e1 _) (SrcSpanInfo op _)
    | srcSpanEnd e1 == srcSpanStart op = [formatError e1 "no space left of operator"]
    | otherwise = []

formatRightSection :: SrcSpanInfo -> SrcSpanInfo -> [String]
formatRightSection (SrcSpanInfo op _) (SrcSpanInfo e1 _)
    | srcSpanEnd op == srcSpanStart e1 = [formatError e1 "no space right of operator"]
    | otherwise = []

formatInfixApp :: SrcSpanInfo -> SrcSpanInfo -> SrcSpanInfo -> [String]
formatInfixApp (SrcSpanInfo e1 _) (SrcSpanInfo op _) (SrcSpanInfo e2 _)
    | srcSpanEnd e1 == srcSpanStart op = [formatError e1 "no space left of operator"]
    | srcSpanEnd op == srcSpanStart e2 = [formatError e2 "no space right of operator"]
    | otherwise = []

formatList :: SrcSpanInfo -> [String]
formatList (SrcSpanInfo spn pts)
    | checkAligment pts = []
    | otherwise = [formatError spn "misaligned list literal"]

formatError :: SrcSpan -> String -> String
formatError spn msg = srcSpanFilename spn
                   <> ":"
                   <> show (srcSpanStartLine spn)
                   <> ":"
                   <> show (srcSpanStartColumn spn)
                   <> " - "
                   <> msg

getTyFun :: Type SrcSpanInfo -> Maybe SrcSpanInfo
getTyFun (TyFun spn _ _) = Just spn
getTyFun _ = Nothing

checkAligment :: [SrcSpan] -> Bool
checkAligment spns = (<= 1) . length . S.fromList . toList
                   . M.fromListWith min $ srcSpanEnd <$> spns

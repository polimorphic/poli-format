module Poli.Format (format) where

import Control.Monad (join, unless)
import Data.Foldable (toList, traverse_)
import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
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
    , SrcSpan(srcSpanFilename, srcSpanStartColumn, srcSpanStartLine)
    , SrcSpanInfo(SrcSpanInfo, srcInfoPoints), Type(TyFun)
    , ann, baseFixities, defaultParseMode, infixl_, infixr_, parseFileWithMode, srcSpanEnd, srcSpanStart
    )
import System.Exit (exitFailure)
import System.Directory (doesDirectoryExist, listDirectory)

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
    res <- parseFileWithMode parseMode path
    pure $ case res of
        ParseOk m -> (formatDecl =<< universeBi m)
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

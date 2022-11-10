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
    , Exp(InfixApp, LeftSection, List, Paren, RecConstr, RecUpdate, RightSection, Tuple)
    , Extension(EnableExtension), ParseMode(extensions, fixities, parseFilename)
    , KnownExtension
        ( DataKinds, DefaultSignatures, DerivingStrategies, DerivingVia
        , GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses
        , MultiWayIf, ScopedTypeVariables, TemplateHaskell, TupleSections
        , TypeApplications, TypeFamilies, TypeOperators
        )
    , ParseResult(ParseFailed, ParseOk)
    , Pat(PInfixApp, PList, PParen, PRec, PTuple)
    , SrcSpan
        ( SrcSpan
        , srcSpanFilename, srcSpanEndColumn, srcSpanEndLine, srcSpanStartColumn, srcSpanStartLine
        )
    , SrcSpanInfo(SrcSpanInfo, srcInfoPoints, srcInfoSpan), Type(TyFun)
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
                  <> (formatPat =<< universeBi m)
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
formatRaw nm fl = formatLines $ zipWith (\n ln -> (lspn n ln, ln)) [1 ..] lns
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

formatLines :: [(SrcSpan, String)] -> [String]
formatLines lns
    = foldMap (uncurry formatLine) lns
   <> join (zipWith checkDouble lns (drop 1 lns))
  where
    checkDouble (_, "") (spn, "") = [formatError spn "double blank line"]
    checkDouble _ _ = []

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

formatPat :: Pat SrcSpanInfo -> [String]
formatPat (PInfixApp _ e1 op e2) = formatInfixSpacing (ann e1) (ann op) (ann e2)
formatPat (PTuple spn _ es) = formatCommaSeparated spn (ann <$> es)
formatPat (PList spn es) = formatCommaSeparated spn (ann <$> es)
formatPat (PParen spn e) = formatCommaSeparated spn [ann e]
formatPat (PRec spn _ pfs) = formatCommaSeparated spn (ann <$> pfs)
formatPat _ = []

formatExp :: Exp SrcSpanInfo -> [String]
formatExp (InfixApp _ e1 op e2) = formatInfixSpacing (ann e1) (ann op) (ann e2)
formatExp (Tuple spn _ es) = formatCommaSeparated spn (ann <$> es)
formatExp (List spn es) = formatCommaSeparated spn (ann <$> es)
formatExp (Paren spn e) = formatCommaSeparated spn [ann e]
formatExp (LeftSection _ e1 op) = formatLeftSection (ann e1) (ann op)
formatExp (RightSection _ op e1) = formatRightSection (ann op) (ann e1)
formatExp (RecConstr spn _ fus) = formatCommaSeparated spn (ann <$> fus)
formatExp (RecUpdate spn _ fus) = formatCommaSeparated spn (ann <$> fus)
formatExp _ = []

formatLeftSection  :: SrcSpanInfo -> SrcSpanInfo -> [String]
formatLeftSection (SrcSpanInfo e1 _) (SrcSpanInfo op _)
    | srcSpanEnd e1 == srcSpanStart op = [formatError e1 "no space left of operator"]
    | otherwise = []

formatRightSection :: SrcSpanInfo -> SrcSpanInfo -> [String]
formatRightSection (SrcSpanInfo op _) (SrcSpanInfo e1 _)
    | srcSpanEnd op == srcSpanStart e1 = [formatError e1 "no space right of operator"]
    | otherwise = []

formatInfixSpacing :: SrcSpanInfo -> SrcSpanInfo -> SrcSpanInfo -> [String]
formatInfixSpacing (SrcSpanInfo e1 _) (SrcSpanInfo op _) (SrcSpanInfo e2 _)
    | srcSpanEnd e1 == srcSpanStart op = [formatError e1 "no space left of operator"]
    | srcSpanEnd op == srcSpanStart e2 = [formatError e2 "no space right of operator"]
    | otherwise = []

formatCommaSeparated :: SrcSpanInfo -> [SrcSpanInfo] -> [String]
formatCommaSeparated (SrcSpanInfo spn pts) es
    | not $ checkAligment pts = [formatError spn "inconsistent literal indentation"]
    | not $ checkAligment (take 1 pts <> take 1 (reverse pts))
        = [formatError spn "inconsistent bracket indentation"]
    | not $ allEqual (postOpen <> preClose) || all (== Newline) preClose
        = [formatError spn "inconsistent bracket padding"]
    | any (== Space) preComma = [formatError spn "erroneous space before comma"]
    | any (== Touching) postComma = [formatError spn "missing space after comma"]
    | any (== Newline) postComma = [formatError spn "trailing comma"]
    | otherwise = []
  where
    postOpen = combine (take 1 pts) (srcInfoSpan <$> take 1 es)
    preComma = combine (srcInfoSpan <$> take (length es - 1) es) (drop 1 pts)
    postComma = combine (drop 1 pts) (srcInfoSpan <$> drop 1 es)
    preClose = combine (srcInfoSpan <$> take 1 (reverse es)) (take 1 $ reverse pts)
    combine = zipWith compareSpans

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
checkAligment spns = allEqual . M.fromListWith min $ srcSpanEnd <$> spns

allEqual :: (Foldable f, Ord a) => f a -> Bool
allEqual = (<= 1) . length . S.fromList . toList

compareSpans :: SrcSpan -> SrcSpan -> SpanComparison
compareSpans a b
    | srcSpanEnd a == srcSpanStart b = Touching
    | srcSpanEndLine a == srcSpanStartLine b = Space
    | otherwise = Newline

data SpanComparison
    = Touching
    | Space
    | Newline
    deriving (Eq, Ord, Show)

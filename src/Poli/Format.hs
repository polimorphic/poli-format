module Poli.Format (format) where

import Control.Arrow ((&&&))
import Control.Monad (join, unless)
import Data.Foldable (toList, traverse_)
import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable (for)
import Language.Haskell.Exts
    ( Exp(List), Extension(EnableExtension), ParseMode(extensions, fixities, parseFilename)
    , KnownExtension
        ( DataKinds, DefaultSignatures, DerivingStrategies, DerivingVia
        , GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses
        , MultiWayIf, ScopedTypeVariables, TemplateHaskell, TupleSections
        , TypeApplications, TypeFamilies, TypeOperators
        )
    , ParseResult(ParseFailed, ParseOk)
    , SrcSpan(srcSpanFilename, srcSpanStartColumn, srcSpanStartLine), SrcSpanInfo(SrcSpanInfo)
    , baseFixities, defaultParseMode, infixl_, infixr_, parseFileWithMode
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
        ParseOk m -> formatExp =<< universeBi m
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

formatExp :: Exp SrcSpanInfo -> [String]
formatExp (List spn _) = formatList spn
formatExp _ = []

formatList :: SrcSpanInfo -> [String]
formatList (SrcSpanInfo spn pts)
    | length (S.fromList . toList $ getIndentations pts) == 1 = []
    | otherwise = [formatError spn "misaligned list literal"]

formatError :: SrcSpan -> String -> String
formatError spn msg = srcSpanFilename spn
                   <> ":"
                   <> show (srcSpanStartLine spn)
                   <> ":"
                   <> show (srcSpanStartColumn spn)
                   <> " - "
                   <> msg

getIndentations :: [SrcSpan] -> Map Int Int
getIndentations spns = M.fromListWith min $ (srcSpanStartLine &&& srcSpanStartColumn) <$> spns

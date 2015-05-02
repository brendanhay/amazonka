{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.Rewrite.Prefix
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Prefix
    ( prefixes
    ) where

import           Compiler.AST
import           Compiler.Text
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict  as Map
import qualified Data.HashMap.Strict  as Map
import qualified Data.HashSet         as Set
import           Data.List            (intercalate)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Lazy       as LText
import           Data.Text.Manipulate
import           Formatting

type Memo = Map Text (Set Text)

prefixes :: Monad m
         => Map Text (Shape f)
         -> Compiler m (Map Text Text)
prefixes = (`evalStateT` mempty) . traverseMaybeKV go
  where
    go n = \case
        Struct _ s  -> Just <$> uniq n (heuristics n) (Set.fromList $ Map.keys (_members s))
        Enum   _ vs -> Just <$> uniq n (mempty : heuristics n) (Set.fromList $ Map.keys vs)
        _           -> return Nothing

    uniq :: Monad m
         => Text
         -> [Text]
         -> Set Text
         -> StateT Memo (Compiler m) Text
    uniq n (h:hs) xs = do
        m <- gets (Map.lookup h)
        case m of
            Just ys | not (Set.null (Set.intersection ys xs))
                -> uniq n hs xs
            _   -> modify (Map.insertWith (<>) h xs) >> return h
    uniq n []     xs = do
        s <- get
        let imply h = show h <> " => " <> show (Map.lookup h s)
        throwError . LText.pack . intercalate "\n" $
              ("Error prefixing: " <> Text.unpack n)
            : ("Fields: " <> show xs)
            : map imply (heuristics n)

-- | Acronym preference list.
heuristics :: Text -> [Text]
heuristics n = catMaybes [r1, r2, r3, r4] ++ ordinals
  where
    -- Append an ordinal to the generated acronyms.
    ordinals = concatMap (\i -> map (\x -> mappend x (num i)) rules) [1..3]

    -- Acronym preference list.
    rules = catMaybes [r1, r2, r3, r4]

    -- SomeTestTType -> STT
    r1 = toAcronym n

    -- SomeTestTType -> S
    r3 = Text.toUpper <$> safeHead n

    -- Some -> Some || SomeTestTType -> Some
    r2 | Text.length n <= 3 = Just n
       | otherwise          = Just (Text.take 3 n)

    -- SomeTestTType -> Som
    r4 = upperHead <$> listToMaybe (splitWords n)

    num :: Int -> Text
    num = Text.pack . show

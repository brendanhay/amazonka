{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

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

import           Compiler.Formatting
import           Compiler.Text
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Hashable
import qualified Data.HashMap.Strict  as Map
import qualified Data.HashSet         as Set
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Manipulate

prefixes :: Monad m => Map Id (Shape f) -> Compiler m (Map Id Text)
prefixes = (`evalStateT` mempty) . kvTraverseMaybe go
  where
    go n = \case
        Struct _ s  -> Just <$> uniq n (heuristics k) (keys (^. keyCI) (_members s))
        Enum   _ vs -> Just <$> uniq n (mempty : heuristics k) (keys CI.mk vs)
        _           -> return Nothing
      where
        k = n ^. keyOriginal

    uniq :: Monad m
         => Id
         -> [CI Text]
         -> Set (CI Text)
         -> StateT (Map (CI Text) (Set (CI Text))) (Compiler m) Text

    uniq n [] xs = do
--        s <- get
--        let imply h = "\n" <> h <> " => " <> Text.pack (show (Map.lookup h s))
        throwError "error!"
--             format ("Error prefixing: " % fid % ", fields: " % shown % fcomma)
--                     n (Set.toList xs) (map imply (heuristics n))

    uniq n (h:hs) xs = do
        m <- gets (Map.lookup h)
        case m of
            Just ys | overlap ys xs
                -> uniq n hs xs
            _   -> modify (Map.insertWith (<>) h xs) >> return (CI.original h)

    overlap :: (Eq a, Hashable a) => Set a -> Set a -> Bool
    overlap xs ys = not . Set.null $ Set.intersection xs ys

--    keys :: (Eq k, Hashable k) => Map k v -> Set k
    keys f = Set.fromList . map f . Map.keys

-- | Acronym preference list.
heuristics :: Text -> [CI Text]
heuristics (camelAcronym -> n) = map CI.mk (rules ++ ordinals)
  where
    -- Append an ordinal to the generated acronyms.
    ordinals = concatMap (\i -> map (\x -> mappend x (num i)) rules) [1..3]

    -- Acronym preference list.
    rules = catMaybes [r1, r2, r3, r4, r5]

    -- SomeTestTType -> STT
    r1 = toAcronym n

    -- SomeTestTType -> S
    r3 = Text.toUpper <$> safeHead n

    -- SomeTestTType -> Som
    r2 | Text.length n <= 3 = Just n
       | otherwise          = Just (Text.take 3 n)

    -- Some -> Some || SomeTestTType -> Some
    r4 = upperHead <$> listToMaybe (splitWords n)

    -- VpcPeeringInfo -> VPCPI
    r5 = toAcronym (upperAcronym n)

    num :: Int -> Text
    num = Text.pack . show

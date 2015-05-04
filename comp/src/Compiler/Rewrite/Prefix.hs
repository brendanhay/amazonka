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
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Hashable
import qualified Data.HashMap.Strict  as Map
import qualified Data.HashSet         as Set
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Manipulate

prefixes :: Monad m
         => Map Text (Shape f)
         -> Compiler m (Map Text Text)
prefixes = (`evalStateT` mempty) . kvTraverseMaybe go
  where
    go (camelAcronym -> n) = \case
        Struct _ s  -> Just <$> uniq n (heuristics n) (keys (_members s))
        Enum   _ vs -> Just <$> uniq n (mempty : heuristics n) (keys vs)
        _           -> return Nothing

    keys :: Map Text a -> Set Text
    keys = Set.fromList . Map.keys

    uniq :: Monad m
         => Text
         -> [Text]
         -> Set Text
         -> StateT (Map Text (Set Text)) (Compiler m) Text

    uniq n [] xs = do
        s <- get
        let imply h = "\n" <> h <> " => " <> Text.pack (show (Map.lookup h s))
        throwError $
            format ("Error prefixing: " % stext % ", fields: " % scomma % scomma)
                   n (Set.toList xs) (map imply (heuristics n))

    uniq n (h:hs) xs = do
        m <- gets (Map.lookup h)
        case m of
            Just ys | overlap ys xs
                -> uniq n hs xs
            _   -> modify (Map.insertWith (<>) h xs) >> return h

    overlap :: (Eq a, Hashable a) => Set a -> Set a -> Bool
    overlap xs ys = not . Set.null $ Set.intersection xs ys

-- | Acronym preference list.
heuristics :: Text -> [Text]
heuristics n = rules ++ ordinals
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

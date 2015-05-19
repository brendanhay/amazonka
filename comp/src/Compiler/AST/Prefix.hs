{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Compiler.AST.Prefix
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Prefix
    ( prefixes
    ) where

import           Compiler.AST.Cofree
import           Compiler.Formatting
import           Compiler.Text
import           Compiler.Types
import           Control.Comonad.Cofree
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.CaseInsensitive   (CI)
import qualified Data.CaseInsensitive   as CI
import           Data.Hashable
import qualified Data.HashMap.Strict    as Map
import qualified Data.HashSet           as Set
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Manipulate

data Env = Env
    { _memo :: Map Id (Maybe Text)
    , _seen :: Map (CI Text) (Set (CI Text))
    }

makeLenses ''Env

type MemoP = StateT Env (Either Error)

prefixes :: (Show a, Traversable t, HasId a)
         => t (Shape a)
         -> Either Error (t (Shape (a ::: Maybe Text)))
prefixes = (`evalStateT` Env mempty mempty) . traverse prefix

prefix :: (Show a, HasId a) => Shape a -> MemoP (Shape (a ::: Maybe Text))
prefix = annotate memo go
  where
    go :: HasId a => Shape a -> MemoP (Maybe Text)
    go (x :< s) =
        let n = identifier x ^. memberId
         in case s of
            Struct st -> Just <$> do
                let hs = heuristics n
                    xs = keys (st ^.. members . each . _1)
                uniq n hs xs

            Enum _ vs -> Just <$> do
                let hs = mempty : heuristics n
                    xs = keys (vs ^.. ifolded . asIndex)
                uniq n hs xs

            _           -> return Nothing

    uniq :: Text -> [CI Text] -> Set (CI Text) -> MemoP Text
    uniq n [] xs = do
        s <- use seen
        let hs  = heuristics n
            f x = sformat ("\n" % soriginal % " => " % shown) x (Map.lookup x s)
        throwError $
            format ("Error prefixing: " % stext %
                    ", fields: "        % shown %
                    scomma)
                   n (Set.toList xs) (map f hs)

    uniq n (h:hs) xs = do
        m <- uses seen (Map.lookup h)
        case m of
            Just ys | overlap ys xs
                -> uniq n hs xs
            _   -> do
                seen %= Map.insertWith (<>) h xs
                return (CI.original h)

overlap :: (Eq a, Hashable a) => Set a -> Set a -> Bool
overlap xs ys = not . Set.null $ Set.intersection xs ys

keys :: [Id] -> Set (CI Text)
keys = Set.fromList . map (^. ciId)

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

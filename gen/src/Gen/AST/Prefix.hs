{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

-- Module      : Gen.AST.Prefix
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.AST.Prefix
    ( prefixes
    ) where

import           Control.Comonad.Cofree
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.CaseInsensitive   (CI)
import qualified Data.CaseInsensitive   as CI
import           Data.Char              (isLower)
import           Data.Hashable
import qualified Data.HashMap.Strict    as Map
import qualified Data.HashSet           as Set
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Gen.AST.Cofree
import           Gen.Formatting
import           Gen.Text
import           Gen.Types

type Seen = Map (CI Text) (Set (CI Text))

data Env = Env
    { _memo     :: Map Id (Maybe Text)
    , _branches :: Seen
    , _fields   :: Seen
    }

makeLenses ''Env

type MemoP = StateT Env (Either Error)

prefixes :: Map Id (Shape Related) -> Either Error (Map Id (Shape Prefixed))
prefixes ss = evalStateT (traverse assignPrefix ss) env
  where
    env = Env mempty mempty (smartCtors ss)

-- | Record projected smart constructors in set of seen field names.
smartCtors :: Map Id (Shape a) -> Seen
smartCtors = Map.fromListWith (<>) . mapMaybe go . Map.toList
  where
    go :: (Id, Shape a) -> Maybe (CI Text, Set (CI Text))
    go (s, _ :< Struct {}) = Just (k, Set.singleton v)
      where
        n = smartCtorId s
        k = CI.mk (Text.takeWhile isLower n)
        v = CI.mk (dropLower n)
    go _                   = Nothing

assignPrefix :: Shape Related -> MemoP (Shape Prefixed)
assignPrefix = annotate Prefixed memo go
  where
    go :: HasId a => Shape a -> MemoP (Maybe Text)
    go (x :< s) =
        let n = typeId (identifier x)
         in case s of
            Enum _ vs -> Just <$> do
                let hs = mempty : acronymPrefixes n
                    ks = keys vs
                unique branches n hs ks

            Struct st -> Just <$> do
                let hs = acronymPrefixes n
                    ks = keys (st ^. members)
                unique fields n hs ks

            _         -> return Nothing

    unique :: Lens' Env Seen
           -> Text
           -> [CI Text]
           -> Set (CI Text)
           -> MemoP Text
    unique seen n [] ks = do
        s <- use seen
        let hs  = acronymPrefixes n
            f x = sformat ("\n" % soriginal % " => " % shown) x (Map.lookup x s)
        throwError $
            format ("Error prefixing: " % stext %
                    ", fields: "        % shown %
                    scomma)
                   n (Set.toList ks) (map f hs)

    unique seen n (h:hs) ks = do
        m <- uses seen (Map.lookup h)
        -- Find if this particular naming heuristic is used already, and if
        -- it is, then is there overlap with this set of ks?
        case m of
            Just ys | overlap ys ks
                -> unique seen n hs ks
            _   -> do
                seen %= Map.insertWith (<>) h ks
                return (CI.original h)

overlap :: (Eq a, Hashable a) => Set a -> Set a -> Bool
overlap xs ys = not . Set.null $ Set.intersection xs ys

keys :: Map Id a -> Set (CI Text)
keys = Set.fromList . map (CI.mk . typeId) . Map.keys

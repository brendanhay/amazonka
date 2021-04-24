{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.AST.Prefix
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.Prefix
  ( prefixes,
  )
where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Lens hiding ((:<))
import Control.Monad.Except
import Control.Monad.State
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Char (isLower)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Hashable
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Manipulate
import Gen.AST.Cofree
import Gen.Formatting
import Gen.Text
import Gen.Types

type Seen = Map (CI Text) (Set (CI Text))

data Env = Env
  { _memo :: Map Id (Maybe Text),
    _branches :: Seen,
    _fields :: Seen
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
    go _ = Nothing

assignPrefix :: Shape Related -> MemoP (Shape Prefixed)
assignPrefix = annotate Prefixed memo go
  where
    go :: (HasId a, HasRelation a) => Shape a -> MemoP (Maybe Text)
    go (x :< s) =
      let n = typeId (identifier x)
          r = x ^. relation
       in case s of
            Enum _ vs ->
              Just <$> do
                let hs = mempty : acronymPrefixes r n
                    ks = keys vs
                unique r branches n hs ks
            Struct st ->
              Just <$> do
                let hs = acronymPrefixes r n
                    ks = keys (st ^. members)
                unique r fields n hs ks
            _ -> return Nothing

    unique ::
      Relation ->
      Lens' Env Seen ->
      Text ->
      [CI Text] ->
      Set (CI Text) ->
      MemoP Text
    unique r seen n [] ks = do
      s <- use seen
      let hs = acronymPrefixes r n
          f x = sformat ("\n" % soriginal % " => " % shown) x (Map.lookup x s)
      throwError $
        format
          ( "Error prefixing: " % stext
              % ", fields: "
              % shown
              % scomma
          )
          n
          (Set.toList ks)
          (map f hs)
    unique r seen n (h : hs) ks = do
      m <- uses seen (Map.lookup h)
      -- Find if this particular naming heuristic is used already, and if
      -- it is, then is there overlap with this set of ks?
      case m of
        Just ys
          | overlap ys ks ->
            unique r seen n hs ks
        _ -> do
          seen %= Map.insertWith (<>) h ks
          return (CI.original h)

overlap :: (Eq a, Hashable a) => Set a -> Set a -> Bool
overlap xs ys = not . Set.null $ Set.intersection xs ys

keys :: Map Id a -> Set (CI Text)
keys = Set.fromList . map (CI.mk . typeId) . Map.keys

-- | Acronym preference list.
--
-- Prefixing occurs as follows:
-- * Requests  - prefer acronyms
-- * Responses - prefer acronyms, and append 'rs'
-- * Shapes    - prefer single letters, then acronyms
acronymPrefixes :: Relation -> Text -> [CI Text]
acronymPrefixes r n
  | isOrphan r,
    Uni d <- _relMode r =
    case d of
      Input -> ci ss
      Output -> ci rs
  | otherwise = ci ss
  where
    rs = map (<> "rs") ss
    ss = xs ++ map suffix ys

    ci = map CI.mk

    -- Take the next char
    suffix x = Text.snoc x c
      where
        c
          | Text.length x >= 2 = Text.head (Text.drop 1 x)
          | otherwise = Text.head x

    xs = catMaybes [r1, r2, r3, r4, r5, r6]
    ys = catMaybes [r1, r2, r3, r4, r6, r7]

    a = camelAcronym n
    a' = upperAcronym n

    limit = 3

    -- Full name if leq limit
    r1
      | Text.length n <= limit = Just n
      | otherwise = Nothing

    -- VpcPeeringInfo -> VPI
    r2 = toAcronym a

    -- VpcPeeringInfo -> VPCPI
    r3
      | x /= r2 = x
      | otherwise = Nothing
      where
        x = toAcronym a'

    -- SomeTestTType -> S
    r4 = Text.toUpper <$> safeHead n

    -- SomeTypes -> STS (retain pural)
    r5
      | Text.isSuffixOf "s" n = flip Text.snoc 's' <$> (r2 <|> r3)
      | otherwise = Nothing

    -- SomeTestTType -> Som
    r6 = Text.take limit <$> listToMaybe (splitWords a)

    -- SomeTestTType -> SomeTestTType
    r7 = Just n

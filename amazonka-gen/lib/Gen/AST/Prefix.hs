{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Gen.AST.Prefix
-- Copyright   : (c) 2013-2020 Brendan Hay
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

import Control.Comonad.Cofree (Cofree ((:<)))
import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Hashable as Hashable
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Manipulate as Manipulate
import Gen.AST.Cofree (annotate)
import Gen.Prelude
import Gen.Text
import Gen.Types

type Seen = HashMap (CI Text) (HashSet (CI Text))

data Env = Env
  { _memo :: HashMap Id (Maybe Text),
    _branches :: Seen,
    _fields :: Seen
  }

$(Lens.makeLenses ''Env)

type MemoP = StateT Env (Either String)

prefixes :: HashMap Id (Shape Related) -> Either String (HashMap Id (Shape Prefixed))
prefixes ss = State.evalStateT (traverse assignPrefix ss) env
  where
    env = Env mempty mempty mempty

-- -- | Record projected smart constructors in set of seen field names.
-- smartCtors :: HashMap Id (Shape a) -> Seen
-- smartCtors = HashMap.fromListWith (<>) . mapMaybe go . HashMap.toList
--   where
--     go :: (Id, Shape a) -> Maybe (CI Text, HashSet (CI Text))
--     go (s, _ :< Struct {}) = Just (k, HashSet.singleton v)
--       where
--         n = smartCtorId s
--         k = CI.mk (Text.takeWhile Char.isLower n)
--         v = CI.mk (dropLower n)
--     go _ = Nothing

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
            _ -> pure Nothing

    unique ::
      Relation ->
      Lens' Env Seen ->
      Text ->
      [CI Text] ->
      HashSet (CI Text) ->
      MemoP Text
    unique r seen n [] ks = do
      s <- Lens.use seen

      let hs = acronymPrefixes r n
          f x = show x ++ " => " ++ show (HashMap.lookup x s)

      Except.throwError $
        "Error prefixing: " ++ show n
          ++ ", fields: "
          ++ show (HashSet.toList ks)
          ++ show (map f hs)
    --
    unique r seen n (h : hs) ks = do
      m <- Lens.uses seen (HashMap.lookup h)
      -- Find if this particular naming heuristic is used already, and if
      -- it is, then is there overlap with this set of ks?
      case m of
        Just ys
          | overlap ys ks ->
            unique r seen n hs ks
        _ -> do
          seen %= HashMap.insertWith (<>) h ks
          pure (CI.original h)

overlap :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
overlap xs ys = not . HashSet.null $ HashSet.intersection xs ys

keys :: HashMap Id a -> HashSet (CI Text)
keys = HashSet.fromList . map (CI.mk . typeId) . HashMap.keys

-- | Acronym preference list.
--
-- Prefixing occurs as follows:
-- * Requests  - prefer acronyms
-- * Responses - prefer acronyms, and append 'rs'
-- * Shapes    - prefer single letters, then acronyms
acronymPrefixes :: Relation -> Text -> [CI Text]
acronymPrefixes r (stripSuffix "Response" -> n)
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
    r2 = Manipulate.toAcronym a

    -- VpcPeeringInfo -> VPCPI
    r3
      | x /= r2 = x
      | otherwise = Nothing
      where
        x = Manipulate.toAcronym a'

    -- SomeTestTType -> S
    r4 = Text.toUpper <$> safeHead n

    -- SomeTypes -> STS (retain pural)
    r5
      | Text.isSuffixOf "s" n = flip Text.snoc 's' <$> (r2 <|> r3)
      | otherwise = Nothing

    -- SomeTestTType -> Som
    r6 = Text.take limit <$> listToMaybe (Manipulate.splitWords a)

    -- SomeTestTType -> SomeTestTType
    r7 = Just n

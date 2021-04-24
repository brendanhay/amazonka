{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.Types.Ann
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Ann where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Lens hiding ((:<))
import Data.Aeson
import Data.Function (on)
import Data.HashSet qualified as Set
import Data.Hashable
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Gen.TH
import Gen.Types.Id

type Set = Set.HashSet

data Direction
  = Output
  | Input
  deriving (Eq, Show, Generic)

instance Hashable Direction

data Mode
  = Bi
  | Uni !Direction
  deriving (Eq, Show)

instance Semigroup Mode where
  (Uni i) <> (Uni o)
    | i == o = Uni o
  _ <> _ = Bi

instance Monoid Mode where
  mempty = Bi
  mappend = (<>)

data Relation = Relation
  { _relShared :: !Int, -- FIXME: get around to using something more sensible.
    _relMode :: !Mode
  }
  deriving (Eq, Show)

makeClassy ''Relation

instance Semigroup Relation where
  a <> b = Relation (on add _relShared b a) (on (<>) _relMode b a)
    where
      add 0 0 = 2
      add 1 0 = 2
      add 0 1 = 2
      add x y = x + y

instance Monoid Relation where
  mempty = Relation 0 mempty
  mappend = (<>)

instance (Functor f, HasRelation a) => HasRelation (Cofree f a) where
  relation = lens extract (flip (:<) . unwrap) . relation

mkRelation :: Maybe Id -> Direction -> Relation
mkRelation p = Relation (maybe 0 (const 1) p) . Uni

isShared :: HasRelation a => a -> Bool
isShared = (> 1) . view relShared

isOrphan :: HasRelation a => a -> Bool
isOrphan = (== 0) . view relShared

data Derive
  = DEq
  | DOrd
  | DRead
  | DShow
  | DEnum
  | DBounded
  | DNum
  | DIntegral
  | DReal
  | DRealFrac
  | DRealFloat
  | DMonoid
  | DSemigroup
  | DIsString
  | DData
  | DTypeable
  | DGeneric
  | DHashable
  | DNFData
  deriving (Eq, Ord, Show, Generic)

instance Hashable Derive

instance FromJSON Derive where
  parseJSON = gParseJSON' (spinal & ctor %~ (. Text.drop 1))

derivingName :: Derive -> Maybe String
derivingName DHashable = Nothing
derivingName DNFData = Nothing
derivingName d = Just $ drop 1 (show d)

data Lit
  = Int
  | Long
  | Double
  | Text
  | Base64
  | Bytes
  | Time
  | Bool
  | Json
  deriving (Eq, Show)

data TypeF a
  = TType Text [Derive]
  | TLit a
  | TNatural
  | TStream
  | TMaybe (TypeF a)
  | TSensitive (TypeF a)
  | TList (TypeF a)
  | TList1 (TypeF a)
  | TMap (TypeF a) (TypeF a)
  deriving (Eq, Show, Functor)

-- FIXME: Moving to a fixpoint required too many initial changes - revisit.
type TType = TypeF Lit

data Related = Related
  { _annId :: Id,
    _annRelation :: Relation
  }
  deriving (Eq, Show)

makeClassy ''Related

instance (Functor f, HasRelated a) => HasRelated (Cofree f a) where
  related = lens extract (flip (:<) . unwrap) . related

instance HasId Related where
  identifier = view annId

instance HasRelation Related where
  relation = annRelation

data Prefixed = Prefixed
  { _annRelated :: Related,
    _annPrefix :: Maybe Text
  }
  deriving (Eq, Show)

makeClassy ''Prefixed

instance (Functor f, HasPrefixed a) => HasPrefixed (Cofree f a) where
  prefixed = lens extract (flip (:<) . unwrap) . prefixed

instance HasRelation Prefixed where
  relation = related . relation

instance HasRelated Prefixed where
  related = annRelated

instance HasId Prefixed where
  identifier = view annId

data Solved = Solved
  { _annPrefixed :: Prefixed,
    _annType :: TType
  }
  deriving (Eq, Show)

makeClassy ''Solved

instance (Functor f, HasSolved a) => HasSolved (Cofree f a) where
  solved = lens extract (flip (:<) . unwrap) . solved

instance HasRelation Solved where
  relation = prefixed . relation

instance HasRelated Solved where
  related = prefixed . related

instance HasPrefixed Solved where
  prefixed = annPrefixed

instance HasId Solved where
  identifier = view annId

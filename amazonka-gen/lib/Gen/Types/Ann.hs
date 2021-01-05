{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Gen.Types.Ann
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.Ann where

import qualified Control.Comonad as Comonad
import qualified Control.Comonad.Cofree as Comonad.Cofree
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Function as Function
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Gen.Prelude
import qualified Gen.TH
import Gen.Types.Id

data Direction
  = Output
  | Input
  deriving stock (Eq, Show, Generic)

instance Hashable Direction

data Mode
  = Bi
  | Uni !Direction
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show)

$(Lens.makeClassy ''Relation)

instance Semigroup Relation where
  a <> b =
    Relation
      (Function.on add _relShared b a)
      (Function.on (<>) _relMode b a)
    where
      add 0 0 = 2
      add 1 0 = 2
      add 0 1 = 2
      add x y = x + y

instance Monoid Relation where
  mempty = Relation 0 mempty
  mappend = (<>)

instance (Functor f, HasRelation a) => HasRelation (Cofree f a) where
  relation =
    Lens.lens Comonad.extract (flip (:<) . Comonad.Cofree.unwrap)
      . relation

mkRelation :: Maybe Id -> Direction -> Relation
mkRelation p = Relation (maybe 0 (const 1) p) . Uni

isShared :: HasRelation a => a -> Bool
isShared = (> 1) . Lens.view relShared

isOrphan :: HasRelation a => a -> Bool
isOrphan = (== 0) . Lens.view relShared

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
  | DGeneric
  | DHashable
  | DNFData
  | DToJSONKey
  | DFromJSONKey
  | DToJSON
  | DFromJSON
  | DToXML
  | DFromXML
  | DToHttpApiData
  | DFromHttpApiData
  | DToQuery
  | DToHeader
  deriving stock (Eq, Ord, Show, Generic)

instance Hashable Derive

instance FromJSON Derive where
  parseJSON = Gen.TH.genericParseJSON (Gen.TH.spinal & Gen.TH.ctor %~ (. Text.drop 1))

derivingName :: Derive -> Text
derivingName = Text.pack . drop 1 . show

derivingStrategy :: Derive -> Either Text Text
derivingStrategy derive =
  ($ derivingName derive) $
    if isStockDerivable derive
      then Left
      else Right

isStockDerivable :: Derive -> Bool
isStockDerivable =
  flip
    elem
    [ DEq,
      DOrd,
      DRead,
      DShow,
      DGeneric
    ]

data Timestamp
  = ISO8601
  | POSIX
  deriving stock (Eq, Show, Generic)

tsToText :: Timestamp -> Text
tsToText = \case
  ISO8601 -> "UTCTime"
  POSIX -> "NominalDiffTime"

instance FromJSON Timestamp where
  parseJSON =
    Aeson.withText "timestamp" $ \case
      "iso8601" -> pure ISO8601
      "unixTimestamp" -> pure POSIX
      e -> fail ("Unknown Timestamp: " ++ Text.unpack e)

instance ToJSON Timestamp where
  toJSON = Aeson.toJSON . tsToText

data Lit
  = Int
  | Long
  | Double
  | Text
  | Base64
  | Bytes
  | Time (Maybe Timestamp)
  | Bool
  | JsonValue
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show, Functor)

-- FIXME: Moving to a fixpoint required too many initial changes - revisit.
type TType = TypeF Lit

data Related = Related
  { _annId :: Id,
    _annRelation :: Relation
  }
  deriving stock (Eq, Show)

$(Lens.makeClassy ''Related)

instance (Functor f, HasRelated a) => HasRelated (Cofree f a) where
  related =
    Lens.lens Comonad.extract (flip (:<) . Comonad.Cofree.unwrap)
      . related

instance HasId Related where
  identifier = Lens.view annId

instance HasRelation Related where
  relation = annRelation

data Prefixed = Prefixed
  { _annRelated :: Related,
    _annPrefix :: Maybe Text
  }
  deriving stock (Eq, Show)

$(Lens.makeClassy ''Prefixed)

instance (Functor f, HasPrefixed a) => HasPrefixed (Cofree f a) where
  prefixed =
    Lens.lens Comonad.extract (flip (:<) . Comonad.Cofree.unwrap)
      . prefixed

instance HasRelation Prefixed where
  relation = related . relation

instance HasRelated Prefixed where
  related = annRelated

instance HasId Prefixed where
  identifier = Lens.view annId

data Solved = Solved
  { _annPrefixed :: Prefixed,
    _annType :: TType
  }
  deriving stock (Eq, Show)

$(Lens.makeClassy ''Solved)

instance (Functor f, HasSolved a) => HasSolved (Cofree f a) where
  solved =
    Lens.lens Comonad.extract (flip (:<) . Comonad.Cofree.unwrap)
      . solved

instance HasRelation Solved where
  relation = prefixed . relation

instance HasRelated Solved where
  related = prefixed . related

instance HasPrefixed Solved where
  prefixed = annPrefixed

instance HasId Solved where
  identifier = Lens.view annId

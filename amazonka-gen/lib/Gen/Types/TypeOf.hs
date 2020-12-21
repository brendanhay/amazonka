-- |
-- Module      : Gen.Types.TypeOf
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.TypeOf
  ( TypeOf (..),
    typeNames,
    derivingOf,
    derivable,
    pointerTo,
    isEq,
    isHashable,
    isNFData,
    typeDefault,
  )
where

import qualified Control.Comonad.Cofree as Comonad.Cofree
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Gen.Prelude
import Gen.Types.Ann
import Gen.Types.Id
import Gen.Types.Service

class TypeOf a where
  typeOf :: a -> TType

instance TypeOf TType where
  typeOf = id

instance TypeOf Solved where
  typeOf = _annType

instance HasId a => TypeOf (Shape a) where
  typeOf (x :< s) = shape s
    where
      n = identifier x

      shape = \case
        Ptr _ t -> t
        Struct st -> TType (typeId n) (struct st)
        Enum {} -> TType (typeId n) (synonym <> derivable)
        List (ListF i e)
          | nonEmpty i -> TList1 (typeOf e)
          | otherwise -> TList (typeOf e)
        Map (MapF _ k v) -> TMap (typeOf k) (typeOf v)
        Lit i l -> lit i l

      lit i = \case
        Int -> sensitive s (natural i (TLit Int))
        Long -> sensitive s (natural i (TLit Long))
        Base64 | isStreaming i -> TStream
        Bytes | isStreaming i -> TStream
        l -> sensitive s (TLit l)

      struct st
        | isStreaming st = stream
        | otherwise =
          uniq $
            Foldable.foldr' (List.intersect . derivingOf) derivable (st ^.. references)

instance HasId a => TypeOf (RefF (Shape a)) where
  typeOf r
    | isStreaming r = TStream
    | otherwise = typeOf (r ^. refAnn)

isEq, isHashable, isNFData :: TypeOf a => a -> Bool
isEq = elem DEq . derivingOf
isHashable = elem DHashable . derivingOf
isNFData = elem DNFData . derivingOf

-- FIXME: this whole concept of pointers and limiting the recursion stack
-- when calculating types is broken - there are plenty of more robust/sane
-- ways to acheive this, revisit.
pointerTo :: Id -> ShapeF a -> TType
pointerTo n = \case
  List (ListF i e)
    | nonEmpty i -> TList1 (t (_refShape e))
    | otherwise -> TList (t (_refShape e))
  Map (MapF _ k v) -> TMap (t (_refShape k)) (t (_refShape v))
  _ -> t n
  where
    t x = TType (typeId x) derivable

typeNames :: TypeOf a => (Text -> Maybe Text) -> a -> Set.Set Text
typeNames match = go . typeOf
  where
    go = \case
      TType x _ -> maybe Set.empty Set.singleton (match x)
      TLit _ -> Set.empty
      TNatural -> Set.empty
      TStream -> Set.empty
      TMaybe x -> go x
      TSensitive x -> go x
      TList x -> go x
      TList1 x -> go x
      TMap k v -> go k <> go v

derivingOf :: TypeOf a => a -> [Derive]
derivingOf = uniq . typ . typeOf
  where
    typ = \case
      TType _ ds -> ds
      TLit l -> lit l
      TNatural -> derivable <> num
      TStream -> stream
      TMaybe t -> typ t
      TSensitive t -> DShow : List.delete DRead (typ t)
      TList e -> monoid <> List.intersect derivable (typ e)
      TList1 e -> DSemigroup : List.intersect derivable (typ e)
      TMap k v -> monoid <> List.intersect (typ k) (typ v)

    lit = \case
      Int -> derivable <> num
      Long -> derivable <> num
      Double -> derivable <> frac
      Text -> derivable <> string
      Base64 -> derivable
      Bytes -> derivable
      Time {} -> List.delete DHashable derivable
      Bool -> derivable <> enum
      JsonValue -> [DEq, DShow, DGeneric, DHashable, DNFData]

stream, string, num, frac, monoid, enum, synonym :: [Derive]
stream = [DShow, DGeneric]
string = [DOrd, DIsString]
num = [DEnum, DBounded, DNum, DIntegral, DReal]
frac = [DRealFrac, DRealFloat]
monoid = [DMonoid, DSemigroup]
enum = [DEnum, DBounded]
synonym =
  [ DIsString,
    DToText,
    DFromText,
    DToByteString,
    DToQuery,
    DToHeader,
    DToJSONKey,
    DFromJSONKey,
    DToJSON,
    DFromJSON,
    DToXML,
    DFromXML
  ]

derivable :: [Derive]
derivable = [DEq, DOrd, DRead, DShow, DGeneric, DHashable, DNFData]

typeDefault :: TType -> Bool
typeDefault = \case
  TSensitive t -> typeDefault t
  TMaybe {} -> True
  TList {} -> True
  TMap {} -> True
  _ -> False

natural :: HasInfo a => a -> TType -> TType
natural x
  | Just i <- x ^. infoMin,
    i >= 0 =
    const TNatural
  | otherwise = id

sensitive :: HasInfo a => a -> TType -> TType
sensitive x
  | x ^. infoSensitive = TSensitive
  | otherwise = id

uniq :: Ord a => [a] -> [a]
uniq = List.sort . List.nub

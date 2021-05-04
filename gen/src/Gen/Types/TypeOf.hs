-- Module      : Gen.Types.TypeOf
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.TypeOf
  ( TypeOf (..),
    derivingOf,
    derivingBase,
    pointerTo,
    isEq,
    isHashable,
    isNFData,
    typeDefault,
  )
where

import Control.Comonad.Cofree
import Control.Lens hiding (List, enum, mapping, (:<), (??))
import Data.Foldable (foldr')
import Data.List (delete, intersect, nub, sort)
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
  typeOf (x :< s) = sensitive s (shape s)
    where
      n = identifier x

      shape = \case
        Ptr _ t -> t
        Struct st -> TType (typeId n) (struct st)
        Enum {} -> TType (typeId n) (ord <> derivingBase)
        List (ListF i e)
          | nonEmpty i -> TList1 (typeOf e)
          | otherwise -> TList (typeOf e)
        Map (MapF _ k v) -> TMap (typeOf k) (typeOf v)
        Lit i l -> lit i l

      lit i = \case
        Int -> natural i (TLit Int)
        Long -> natural i (TLit Long)
        Base64 | isStreaming i -> TStream
        Bytes | isStreaming i -> TStream
        l -> TLit l

      struct st
        | isStreaming st = stream
        | otherwise =
          uniq $
            foldr' (intersect . derivingOf) derivingBase (st ^.. references)

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
    t x = TType (typeId x) derivingBase

derivingOf :: TypeOf a => a -> [Derive]
derivingOf = uniq . typ . typeOf
  where
    typ = \case
      TType _ ds -> ds
      TLit l -> lit l
      TNatural -> derivingBase <> num
      TStream -> stream
      TMaybe t -> typ t
      TSensitive t -> DShow : delete DRead (typ t)
      TList e -> monoid <> intersect derivingBase (typ e)
      TList1 e -> DSemigroup : intersect derivingBase (typ e)
      TMap k v -> monoid <> intersect (typ k) (typ v)

    lit = \case
      Int -> derivingBase <> num
      Long -> derivingBase <> num
      Double -> derivingBase <> frac
      Text -> derivingBase <> string
      Base64 -> derivingBase
      Bytes -> derivingBase
      Time -> DOrd : derivingBase
      Bool -> derivingBase <> enum
      Json -> [DEq, DShow, DData, DTypeable, DGeneric, DHashable, DNFData]

stream, string, num, frac, monoid, enum, ord :: [Derive]
stream = [DShow, DGeneric]
string = [DOrd, DIsString]
num = DNum : DIntegral : DReal : enum
frac = [DOrd, DRealFrac, DRealFloat]
monoid = [DMonoid, DSemigroup]
enum = [DOrd, DEnum, DBounded]
ord = [DOrd]

derivingBase :: [Derive]
derivingBase = [DEq, DRead, DShow, DData, DTypeable, DGeneric, DHashable, DNFData]

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
uniq = sort . nub

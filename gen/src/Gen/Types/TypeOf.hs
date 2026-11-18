module Gen.Types.TypeOf
  ( TypeOf (..),
    derivingOf,
    derivingBase,
    isEq,
    isHashable,
    isNFData,
    typeDefault,
    typeMember,
  )
where

import qualified Control.Comonad as Comonad
import qualified Data.List as List
import Gen.Prelude
import Gen.Types.Ann
import Gen.Types.Service

class TypeOf a where
  typeOf :: a -> TType

instance TypeOf TType where
  typeOf = id

instance TypeOf Solved where
  typeOf = _annType

-- | A solved shape's type is its annotation, which "Gen.AST.Solve" assigns
-- per shape identifier.
instance TypeOf (Shape Solved) where
  typeOf = _annType . Comonad.extract

instance TypeOf (RefF (Shape Solved)) where
  typeOf r
    | isStreaming r = TStream
    | otherwise = typeOf (r ^. refAnn)

isEq, isHashable, isNFData :: (TypeOf a) => a -> Bool
isEq = elem DEq . derivingOf
isHashable = elem DHashable . derivingOf
isNFData = elem DNFData . derivingOf

derivingOf :: (TypeOf a) => a -> [Derive]
derivingOf = uniq . typ . typeOf
  where
    typ = \case
      TType _ ds -> ds
      TLit l -> lit l
      TNatural -> derivingBase <> num
      TStream -> stream
      TMaybe t -> typ t
      TSensitive t -> DShow : List.delete DRead (typ t)
      TList e -> monoid <> List.intersect derivingBase (typ e)
      TList1 e -> DSemigroup : List.intersect derivingBase (typ e)
      TMap k v -> monoid <> List.intersect (typ k) (typ v)

    lit = \case
      Int -> derivingBase <> num
      Long -> derivingBase <> num
      Double -> derivingBase <> frac
      Text -> derivingBase <> string
      Base64 -> derivingBase
      Bytes -> derivingBase
      Time _ -> DOrd : derivingBase
      Bool -> derivingBase <> enum
      Json -> [DEq, DShow, DGeneric, DHashable, DNFData]

stream, string, num, frac, monoid, enum :: [Derive]
stream = [DShow, DGeneric]
string = [DOrd, DIsString]
num = DNum : DIntegral : DReal : enum
frac = [DOrd, DRealFrac, DRealFloat]
monoid = [DMonoid, DSemigroup]
enum = [DOrd, DEnum, DBounded]

derivingBase :: [Derive]
derivingBase = [DEq, DRead, DShow, DGeneric, DHashable, DNFData]

typeDefault :: TType -> Bool
typeDefault = \case
  TSensitive t -> typeDefault t
  TMaybe {} -> True
  TList {} -> True
  TMap {} -> True
  _ -> False

-- FIXME: This would be much more sane with a proper fixpoint datatype.
typeMember :: Either Text Lit -> TType -> Bool
typeMember x = \case
  TType t _ -> x == Left t
  TLit l -> x == Right l
  TStream -> False
  TNatural -> False
  TMaybe e -> typeMember x e
  TSensitive e -> typeMember x e
  TList e -> typeMember x e
  TList1 e -> typeMember x e
  TMap k v -> typeMember x k || typeMember x v

uniq :: (Ord a) => [a] -> [a]
uniq = List.sort . List.nub

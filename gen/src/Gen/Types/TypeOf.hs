module Gen.Types.TypeOf
  ( TypeOf (..),
    derivingOf,
    derivingBase,
    pointerTo,
    isEq,
    isHashable,
    isNFData,
    typeDefault,
    typeMember,
  )
where

import qualified Data.List as List
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
        Map (MapF _ k v) ->
          case typeOf k of
            TSensitive t -> TMap t (typeOf v)
            t -> TMap t (typeOf v)
        Lit i l -> literal i l

      literal i = \case
        Int -> natural i (TLit Int)
        Long -> natural i (TLit Long)
        Base64 | isStreaming i -> TStream
        Bytes | isStreaming i -> TStream
        l -> TLit l

      struct st
        | isStreaming st = stream
        | otherwise =
          uniq $
            foldr (List.intersect . derivingOf) derivingBase (st ^.. references)

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
      Time -> DOrd : derivingBase
      Bool -> derivingBase <> enum
      Json -> [DEq, DShow, DGeneric, DHashable, DNFData]

stream, string, num, frac, monoid, enum, ord :: [Derive]
stream = [DShow, DGeneric]
string = [DOrd, DIsString]
num = DNum : DIntegral : DReal : enum
frac = [DOrd, DRealFrac, DRealFloat]
monoid = [DMonoid, DSemigroup]
enum = [DOrd, DEnum, DBounded]
ord = [DOrd]

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

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Amazonka.DynamoDB.Item.Value
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Amazonka.DynamoDB.Item.Value
    (
    -- * Native Types
      NativeType  (..)
    , getAttributeType
    , isSingleAttribute

    -- * Errors
    , ValueError  (..)
    , malformedError
    , mismatchError

    -- * Type Class
    , DynamoList
    , DynamoNumber
    , DynamoNumberOrSet
    , DynamoSet
    , DynamoValue (..)
    , toValue
    , fromValue

    -- * Values
    , Value
    , newValue
    , getValue

    -- ** Safe Values
    , NativeValue
    , newNativeValue
    , getNativeValue

    -- ** Null
    , setNull
    , getNull

    -- ** Bool
    , setBool
    , getBool

    -- ** List
    , setList
    , getList

    , getVector
    , setVector

    -- ** Map
    , setMap
    , getMap

    -- ** Number
    , setNumber
    , getNumber

    , setIntegral
    , getIntegral

    , setNumberSet
    , getNumberSet

    , setIntegralSet
    , getIntegralSet

    -- ** Binary
    , setBinarySet
    , getBinarySet

    , setBinary
    , getBinary

    -- ** String
    , setStringSet
    , getStringSet

    , setString
    , getString
    ) where

import Amazonka.DynamoDB.Item.Internal

import Control.Applicative (Const (..))
import Control.Exception   (Exception)
import Control.Lens        (set)
import Control.Monad       ((>=>))

import Data.Bifunctor        (bimap, first)
import Data.ByteString       (ByteString)
import Data.CaseInsensitive  (CI)
import Data.Coerce           (coerce)
import Data.Foldable         (toList)
import Data.Functor.Identity (Identity (..))
import Data.Hashable         (Hashable)
import Data.HashMap.Strict   (HashMap)
import Data.HashSet          (HashSet)
import Data.Int              (Int, Int16, Int32, Int64, Int8)
import Data.IntMap           (IntMap)
import Data.IntSet           (IntSet)
import Data.List.NonEmpty    (NonEmpty (..))
import Data.Map.Strict       (Map)
import Data.Maybe            (catMaybes)
import Data.Monoid           (Dual (..), (<>))
import Data.Proxy            (Proxy (..))
import Data.Scientific       (Scientific)
import Data.Sequence         (Seq)
import Data.Set              (Set)
import Data.Tagged           (Tagged (..))
import Data.Text             (Text)
import Data.Typeable         (Typeable)
import Data.Vector           (Vector)
import Data.Word             (Word, Word16, Word32, Word64, Word8)

import Foreign.Storable (Storable)

import GHC.Exts (Constraint)

import Network.AWS.Data.Base64            (Base64 (..))
import Network.AWS.Data.Map               (toMap)
import Network.AWS.Data.Text
import Network.AWS.DynamoDB               hiding (ScalarAttributeType (..))
import Network.AWS.DynamoDB.Types.Product (AttributeValue (..))

import Numeric.Natural (Natural)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Scientific      as Sci
import qualified Data.Text            as Text
import qualified Data.Text.Lazy       as LText

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as HashSet
import qualified Data.IntMap.Strict  as IntMap
import qualified Data.IntSet         as IntSet
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map.Strict     as Map
import qualified Data.Sequence       as Seq
import qualified Data.Set            as Set

import qualified Data.Vector           as Vector
import qualified Data.Vector.Generic   as VectorGen
import qualified Data.Vector.Primitive as VectorPrim
import qualified Data.Vector.Storable  as VectorStore
import qualified Data.Vector.Unboxed   as VectorUnbox

data ValueError
    = AttributeMalformed [NativeType]
    | AttributeMismatch  !NativeType (Maybe NativeType)
    | ParseFailure       !NativeType Text
      deriving (Eq, Show, Typeable)

instance Exception ValueError

malformedError :: AttributeValue -> ValueError
malformedError AttributeValue'{..} =
    AttributeMalformed $ catMaybes
        [ NULL <$ _avNULL
        , BOOL <$ _avBOOL
        , L    <$ _avL
        , M    <$ _avM
        , NS   <$ _avNS
        , N    <$ _avN
        , BS   <$ _avBS
        , B    <$ _avB
        , SS   <$ _avSS
        , S    <$ _avS
        ]

mismatchError :: NativeType -> AttributeValue -> ValueError
mismatchError expect = AttributeMismatch expect . getAttributeType

newValue :: AttributeValue -> Either ValueError Value
newValue v
    | isSingleAttribute v = Right (UnsafeValue v)
    | otherwise           = Left  (malformedError v)

newNativeValue :: forall a. DynamoNative a
               => Value
               -> Either ValueError (NativeValue a)
newNativeValue (UnsafeValue v)
    | getAttributeType v == Just (getNativeType (Proxy :: Proxy a))
        = Right (UnsafeNativeValue v)
    | otherwise
        = Left  (malformedError v)

type DynamoList        a = (DynamoValue a, DynamoType a ~ 'L)
type DynamoNumber      a = (DynamoValue a, DynamoType a ~ 'N)
type DynamoNumberOrSet a = (DynamoValue a, IsDynamoNumberOrSet (DynamoType a))
type DynamoSet         a = (DynamoValue a, IsDynamoSet (DynamoType a))

type family IsDynamoNumberOrSet a :: Constraint where
    IsDynamoNumberOrSet 'N =  ()
    IsDynamoNumberOrSet 'NS = ()
    IsDynamoNumberOrSet 'BS = ()
    IsDynamoNumberOrSet 'SS = ()

type family IsDynamoSet a :: Constraint where
    IsDynamoSet 'NS = ()
    IsDynamoSet 'BS = ()
    IsDynamoSet 'SS = ()

-- | Serialise a value to a simple DynamoDB attribute.
--
-- Note about how this is for 'primitive' types that will be stored in
-- individual fields. No instances Either, Maybe, etc.
-- Only one attribute value can be set.
-- Empty set invariants.
--
-- Documentation about DynamoDB attributes.
--
-- The cumulative size of attributes per item must fit within the maximum DynamoDB
-- item size (400 KB).
--
-- There is no limit on the number of values in a List, a Map, or a Set, as long
-- as the item containing the values fits within the 400 KB item size limit.
--
-- An attribute value cannot be an empty String or empty Set (String Set, Number
-- Set, or Binary Set). However, empty Lists and Maps are allowed.
--
-- DynamoDB supports nested attributes up to 32 levels deep.
--
-- An 'Attribute' is subject to the following law:
--
-- @
-- fromAttr (toAttr x) ≡ Right x
-- @
--
-- That is, you get back what you put in.
class DynamoNative (DynamoType a) => DynamoValue a where
    type DynamoType a :: NativeType

    toNative   :: a -> NativeValue (DynamoType a)
    fromNative :: NativeValue (DynamoType a) -> Either ValueError a

toValue :: DynamoValue a => a -> Value
toValue = coerce . toNative

fromValue :: DynamoValue a
          => Value
          -> Either ValueError a
fromValue = newNativeValue >=> fromNative

instance DynamoNative a => DynamoValue (NativeValue (a :: NativeType)) where
    type DynamoType (NativeValue a) = a

    toNative   = id
    fromNative = Right

instance DynamoValue NativeType where
    type DynamoType NativeType = 'S

    toNative   = setString . toText
    fromNative = getString >=> parseText S

instance DynamoValue () where
    type DynamoType () = 'NULL

    toNative   = const (setNull True)
    fromNative = fmap (const ()). getNull

instance DynamoValue Ordering where
    type DynamoType Ordering = 'S

    toNative   = setString . toText
    fromNative = getString >=> parseText S

instance DynamoValue Char where
    type DynamoType Char = 'S

    toNative   = setString . toText
    fromNative = getString >=> parseText S

instance DynamoValue Text where
    type DynamoType Text = 'S

    toNative   = setString
    fromNative = getString

instance DynamoValue LText.Text where
    type DynamoType LText.Text = 'S

    toNative   = toNative . LText.toStrict
    fromNative = fmap LText.fromStrict . fromNative

instance DynamoValue ByteString where
    type DynamoType ByteString = 'B

    toNative   = setBinary
    fromNative = getBinary

instance DynamoValue LBS.ByteString where
    type DynamoType LBS.ByteString = 'B

    toNative   = toNative . LBS.toStrict
    fromNative = fmap LBS.fromStrict . fromNative

instance DynamoValue Bool where
    type DynamoType Bool = 'BOOL

    toNative   = setBool
    fromNative = getBool

instance DynamoValue Word where
    type DynamoType Word = 'N

    toNative   = setIntegral
    fromNative = getIntegral

instance DynamoValue Word8 where
    type DynamoType Word8 = 'N

    toNative   = setIntegral
    fromNative = getIntegral

instance DynamoValue Word32 where
    type DynamoType Word32 = 'N

    toNative   = setIntegral
    fromNative = getIntegral

instance DynamoValue Word64 where
    type DynamoType Word64 = 'N

    toNative   = setIntegral
    fromNative = getIntegral

instance DynamoValue Int where
    type DynamoType Int = 'N

    toNative   = setIntegral
    fromNative = getIntegral

instance DynamoValue Int8 where
    type DynamoType Int8 = 'N

    toNative   = setIntegral
    fromNative = getIntegral

instance DynamoValue Int32 where
    type DynamoType Int32 = 'N

    toNative   = setIntegral
    fromNative = getIntegral

instance DynamoValue Int64 where
    type DynamoType Int64 = 'N

    toNative   = setIntegral
    fromNative = getIntegral

instance DynamoValue Integer where
    type DynamoType Integer = 'N

    toNative   = setIntegral
    fromNative = getIntegral

instance DynamoValue Natural where
    type DynamoType Natural = 'N

    toNative   = setIntegral
    fromNative = getIntegral

instance DynamoValue Double where
    type DynamoType Double = 'N

    toNative   = toNative . Sci.fromFloatDigits
    fromNative = fmap Sci.toRealFloat . fromNative

instance DynamoValue Float where
    type DynamoType Float = 'N

    toNative   = toNative . Sci.fromFloatDigits
    fromNative = fmap Sci.toRealFloat . fromNative

instance DynamoValue Scientific where
    type DynamoType Scientific = 'N

    toNative   = setNumber
    fromNative = getNumber

instance DynamoValue a => DynamoValue (Identity a) where
    type DynamoType (Identity a) = DynamoType a

    toNative   = toNative . runIdentity
    fromNative = fmap Identity . fromNative

instance DynamoValue a => DynamoValue (Const a b) where
    type DynamoType (Const a b) = DynamoType a

    toNative   = toNative . getConst
    fromNative = fmap Const . fromNative

instance DynamoValue b => DynamoValue (Tagged s b) where
    type DynamoType (Tagged s b) = DynamoType b

    toNative   = toNative . unTagged
    fromNative = fmap Tagged . fromNative

instance DynamoValue a => DynamoValue (Dual a) where
    type DynamoType (Dual a) = DynamoType a

    toNative   = toNative . getDual
    fromNative = fmap Dual . fromNative

instance (DynamoValue a, CI.FoldCase a) => DynamoValue (CI a) where
    type DynamoType (CI a) = DynamoType a

    toNative   = toNative . CI.original
    fromNative = fmap CI.mk . fromNative

instance DynamoValue a => DynamoValue [a] where
    type DynamoType [a] = 'L

    toNative   = setList . map toValue
    fromNative = getList >=> traverse fromValue

instance DynamoValue a => DynamoValue (NonEmpty a) where
     type DynamoType (NonEmpty a) = 'L

     toNative   = toNative . NE.toList
     fromNative =
         fromNative >=> \case
             x:xs -> Right (x :| xs)
             _    -> Left (ParseFailure L "Unexpected empty list.")

instance DynamoValue a => DynamoValue (Vector a) where
    type DynamoType (Vector a) = 'L

    toNative   = setVector
    fromNative = getVector

instance (VectorGen.Vector VectorUnbox.Vector a, DynamoValue a)
      => DynamoValue (VectorUnbox.Vector a) where
    type DynamoType (VectorUnbox.Vector a) = 'L

    toNative   = setVector
    fromNative = getVector

instance (Storable a, DynamoValue a)
      => DynamoValue (VectorStore.Vector a) where
    type DynamoType (VectorStore.Vector a) = 'L

    toNative   = setVector
    fromNative = getVector

instance (VectorPrim.Prim a, DynamoValue a)
      => DynamoValue (VectorPrim.Vector a) where
    type DynamoType (VectorPrim.Vector a) = 'L

    toNative   = setVector
    fromNative = getVector

instance DynamoValue a => DynamoValue (Seq a) where
    type DynamoType (Seq a) = 'L

    toNative   = toNative . toList
    fromNative = fmap Seq.fromList . fromNative

instance DynamoValue IntSet where
    type DynamoType IntSet = 'NS

    toNative   = setNumberSet . map fromIntegral . IntSet.toList
    fromNative = getNumberSet >=> fmap IntSet.fromList . traverse go
      where
        go    = first err . Sci.floatingOrInteger
        err r = ParseFailure NS $
            "Expected integral value, got: " <> toText (r :: Double)

instance DynamoValue a => DynamoValue (IntMap a) where
    type DynamoType (IntMap a) = 'M

    toNative =
        setMap . HashMap.fromList . map (bimap toText toValue) . IntMap.toList

    fromNative =
        getMap >=> fmap IntMap.fromList . traverse go . HashMap.toList
      where
        go (k, v) =
           (,) <$> first (ParseFailure M . toText) (fromText k)
               <*> fromValue v

instance DynamoValue a => DynamoValue (Map Text a) where
    type DynamoType (Map Text a) = 'M

    toNative   = toNative . HashMap.fromList . Map.toList
    fromNative = fmap (Map.fromList . HashMap.toList) . fromNative

instance DynamoValue (Set Text) where
    type DynamoType (Set Text) = 'SS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set LText.Text) where
    type DynamoType (Set LText.Text) = 'SS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set ByteString) where
    type DynamoType (Set ByteString) = 'BS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set LBS.ByteString) where
    type DynamoType (Set LBS.ByteString) = 'BS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Word) where
    type DynamoType (Set Word) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Word8) where
    type DynamoType (Set Word8) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Word16) where
    type DynamoType (Set Word16) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Word32) where
    type DynamoType (Set Word32) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Word64) where
    type DynamoType (Set Word64) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Int) where
    type DynamoType (Set Int) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Int8) where
    type DynamoType (Set Int8) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Int16) where
    type DynamoType (Set Int16) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Int32) where
    type DynamoType (Set Int32) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Int64) where
    type DynamoType (Set Int64) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Integer) where
    type DynamoType (Set Integer) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Natural) where
    type DynamoType (Set Natural) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Float) where
    type DynamoType (Set Float) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Double) where
    type DynamoType (Set Double) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue (Set Scientific) where
    type DynamoType (Set Scientific) = 'NS

    toNative   = toNative . toHashSet
    fromNative = fmap fromHashSet . fromNative

instance DynamoValue a => DynamoValue (HashMap Text a) where
    type DynamoType (HashMap Text a) = 'M

    toNative   = setMap . HashMap.map toValue
    fromNative = getMap >=> traverse fromValue

instance DynamoValue (HashSet Text) where
    type DynamoType (HashSet Text) = 'SS

    toNative   = setStringSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getStringSet

instance DynamoValue (HashSet LText.Text) where
    type DynamoType (HashSet LText.Text) = 'SS

    toNative   = setStringSet . map LText.toStrict . HashSet.toList
    fromNative = fmap (HashSet.fromList . map LText.fromStrict) . getStringSet

instance DynamoValue (HashSet ByteString) where
    type DynamoType (HashSet ByteString) = 'BS

    toNative   = setBinarySet . HashSet.toList
    fromNative = fmap HashSet.fromList . getBinarySet

instance DynamoValue (HashSet LBS.ByteString) where
    type DynamoType (HashSet LBS.ByteString) = 'BS

    toNative   = setBinarySet . map LBS.toStrict . HashSet.toList
    fromNative = fmap (HashSet.fromList . map LBS.fromStrict) . getBinarySet

instance DynamoValue (HashSet Word) where
    type DynamoType (HashSet Word) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Word8) where
    type DynamoType (HashSet Word8) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Word16) where
    type DynamoType (HashSet Word16) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Word32) where
    type DynamoType (HashSet Word32) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Word64) where
    type DynamoType (HashSet Word64) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Int) where
    type DynamoType (HashSet Int) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Int8) where
    type DynamoType (HashSet Int8) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Int16) where
    type DynamoType (HashSet Int16) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Int32) where
    type DynamoType (HashSet Int32) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Int64) where
    type DynamoType (HashSet Int64) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Integer) where
    type DynamoType (HashSet Integer) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Natural) where
    type DynamoType (HashSet Natural) = 'NS

    toNative   = setIntegralSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Float) where
    type DynamoType (HashSet Float) = 'NS

    toNative   = setNumberSet . map Sci.fromFloatDigits . HashSet.toList
    fromNative = fmap (HashSet.fromList . map Sci.toRealFloat) . getNumberSet

instance DynamoValue (HashSet Double) where
    type DynamoType (HashSet Double) = 'NS

    toNative   = setNumberSet . map Sci.fromFloatDigits . HashSet.toList
    fromNative = fmap (HashSet.fromList . map Sci.toRealFloat) . getNumberSet

instance DynamoValue (HashSet Scientific) where
    type DynamoType (HashSet Scientific) = 'NS

    toNative   = setNumberSet . HashSet.toList
    fromNative = fmap HashSet.fromList . getNumberSet

-- instance DynamoValue Version where
--     toNative   = toNative . toJSON
--     fromNative = fromNative >=> getJSON

-- instance DynamoValue Day where
--     toNative   = toNative . toJSON
--     fromNative = fromNative >=> getJSON

-- instance DynamoValue TimeOfDay where
--     toNative   = toNative . toJSON
--     fromNative = fromNative >=> getJSON

-- instance DynamoValue ZonedTime where
--     toNative   = toNative . toJSON
--     fromNative = fromNative >=> getJSON

-- instance DynamoValue LocalTime where
--     toNative   = toNative . toJSON
--     fromNative = fromNative >=> getJSON

-- instance DynamoValue NominalDiffTime where
--     toNative   = toNative . toJSON
--     fromNative = fromNative >=> getJSON

-- instance DynamoValue UTCTime where
--     toNative   = toNative . toJSON
--     fromNative = fromNative >=> getJSON

-- instance DynamoValue DotNetTime where
--     toNative   = toNative . toJSON
--     fromNative = fromNative >=> getJSON

-- -- FIXME: add top-level note about binary sets/strings.
-- instance DynamoValue JS.Value where
--     toNative = \case
--         JS.String x -> setString x
--         JS.Number x -> setNumber x
--         JS.Bool   x -> setBool   x
--         JS.Array  x -> setList . map toNative $ Vector.toList x
--         JS.Object x -> setMap  $ HashMap.map toNative x
--         JS.Null     -> setNull True

--     fromNative v = case getValueType v of
--         S    -> JS.String <$> getString v
--         N    -> JS.Number <$> getNumber v
--         BOOL -> JS.Bool   <$> getBool v
--         M    -> JS.Object <$> (getMap v >>= traverse fromNative)
--         L    -> JS.Array . Vector.fromList <$> (getList v >>= traverse fromNative)
--         NULL -> JS.Null   <$  getNull v
--         t    -> Left (ParseFailure t "Unable to parse unsupported JSON value.")

setNull :: Bool -> NativeValue 'NULL
setNull x = UnsafeNativeValue (set avNULL (Just x) attributeValue)

getNull :: NativeValue 'NULL -> Either ValueError Bool
getNull (UnsafeNativeValue v) = note (mismatchError NULL v) (_avNULL v)

setBool :: Bool -> NativeValue 'BOOL
setBool x = UnsafeNativeValue (set avBOOL (Just x) attributeValue)

getBool :: NativeValue 'BOOL -> Either ValueError Bool
getBool (UnsafeNativeValue v) = note (mismatchError BOOL v) (_avBOOL v)

setList :: [Value] -> NativeValue 'L
setList x = UnsafeNativeValue (set avL (coerce x) attributeValue)

getList :: NativeValue 'L -> Either ValueError [Value]
getList (UnsafeNativeValue v) = note (mismatchError L v) (coerce <$> _avL v)

setVector :: (VectorGen.Vector v a, DynamoValue a) => v a -> NativeValue 'L
setVector = toNative . Vector.toList . Vector.convert

getVector :: (VectorGen.Vector v a, DynamoValue a)
          => NativeValue 'L
          -> Either ValueError (v a)
getVector = fmap (Vector.convert . Vector.fromList) . fromNative

setMap :: HashMap Text Value -> NativeValue 'M
setMap x = UnsafeNativeValue (set avM (coerce x) attributeValue)

getMap :: NativeValue 'M -> Either ValueError (HashMap Text Value)
getMap (UnsafeNativeValue v) = note (mismatchError M v) (coerce . toMap <$> _avM v)

setNumberSet :: [Scientific] -> NativeValue 'NS
setNumberSet x = UnsafeNativeValue (set avNS (map toText x) attributeValue)

getNumberSet :: NativeValue 'NS -> Either ValueError [Scientific]
getNumberSet (UnsafeNativeValue v) =
    note (mismatchError NS v) (_avNS v)
        >>= first (ParseFailure NS . Text.pack)
          . traverse fromText

setNumber :: Scientific -> NativeValue 'N
setNumber x = UnsafeNativeValue (set avN (Just (toText x)) attributeValue)

getNumber :: NativeValue 'N -> Either ValueError Scientific
getNumber (UnsafeNativeValue v) =
    note (mismatchError N v) (_avN v)
        >>= first (ParseFailure N . Text.pack)
          . fromText

setIntegralSet :: Integral a => [a] -> NativeValue 'NS
setIntegralSet = setNumberSet . map fromIntegral

getIntegralSet :: Integral a => NativeValue 'NS -> Either ValueError [a]
getIntegralSet = getNumberSet >=> traverse parse
  where
    parse = first err . Sci.floatingOrInteger
    err r = ParseFailure NS $
        "Expected integral value, got: " <> toText (r :: Double)

setIntegral :: Integral a => a -> NativeValue 'N
setIntegral = setNumber . fromIntegral

getIntegral :: Integral a => NativeValue 'N -> Either ValueError a
getIntegral = getNumber >=> first err . Sci.floatingOrInteger
  where
    err r = ParseFailure NS $
        "Expected integral value, got: " <> toText (r :: Double)

setBinarySet :: [ByteString] -> NativeValue 'BS
setBinarySet x = UnsafeNativeValue (set avBS x attributeValue)

getBinarySet :: NativeValue 'BS -> Either ValueError [ByteString]
getBinarySet (UnsafeNativeValue v) = note (mismatchError BS v) (coerce (_avBS v))

setBinary :: ByteString -> NativeValue 'B
setBinary x = UnsafeNativeValue (set avB (Just x) attributeValue)

getBinary :: NativeValue 'B -> Either ValueError ByteString
getBinary (UnsafeNativeValue v) = note (mismatchError B v) (coerce (_avB v))

setStringSet :: [Text] -> NativeValue 'SS
setStringSet x = UnsafeNativeValue (set avSS x attributeValue)

getStringSet :: NativeValue 'SS -> Either ValueError [Text]
getStringSet (UnsafeNativeValue v) = note (mismatchError SS v) (_avSS v)

setString :: Text -> NativeValue 'S
setString x = UnsafeNativeValue (set avS (Just x) attributeValue)

getString :: NativeValue 'S -> Either ValueError Text
getString (UnsafeNativeValue v) = note (mismatchError S v) (_avS v)

note :: e -> Maybe a -> Either e a
note e Nothing  = Left  e
note _ (Just x) = Right x

parseText :: FromText a => NativeType -> Text -> Either ValueError a
parseText t = first (ParseFailure t . Text.pack) . fromText

toHashSet :: (Eq a, Hashable a) => Set a -> HashSet a
toHashSet = HashSet.fromList . Set.toList

fromHashSet :: Ord a => HashSet a -> Set a
fromHashSet = Set.fromList . HashSet.toList

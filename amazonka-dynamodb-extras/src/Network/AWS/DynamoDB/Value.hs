{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Network.AWS.DynamoDB.Value
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Value
    (
    -- * Native Types
      DynamoType  (..)
    , getAttributeType
    , isAttributeValue

    -- * Errors
    , ValueError  (..)
    , malformedError
    , mismatchError

    -- * Type Class
    , DynamoValue (..)

    -- * Safe Values
    , Value
    , newValue
    , getValue
    , getValueType

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

import Control.Applicative (Const (..))
import Control.Exception   (Exception)
import Control.Lens        (set)
import Control.Monad       ((>=>))

import Data.Aeson            (FromJSON (..), ToJSON (..))
import Data.Aeson.Types      (DotNetTime, parseEither)
import Data.Bifunctor        (bimap, first)
import Data.Bits             (popCount, setBit, zeroBits)
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
import Data.Maybe            (catMaybes, fromMaybe, isJust)
import Data.Monoid           (Dual (..), (<>))
import Data.Scientific       (Scientific)
import Data.Sequence         (Seq)
import Data.Set              (Set)
import Data.Tagged           (Tagged (..))
import Data.Text             (Text)
import Data.Time             (Day, LocalTime, NominalDiffTime, TimeOfDay,
                              UTCTime, ZonedTime)
import Data.Typeable         (Typeable)
import Data.Vector           (Vector)
import Data.Version          (Version)
import Data.Word             (Word, Word16, Word32, Word64, Word8)

import Foreign.Storable (Storable)

import Network.AWS.Data.Base64            (Base64 (..))
import Network.AWS.Data.Map               (toMap)
import Network.AWS.Data.Text              (FromText (..), ToText (..), fromText)
import Network.AWS.DynamoDB               hiding (ScalarAttributeType (..))
import Network.AWS.DynamoDB.Types.Product (AttributeValue (..))
import Network.AWS.DynamoDB.Value.Unsafe

import Numeric.Natural (Natural)

import qualified Data.Aeson           as JS
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
    = AttributeMalformed [DynamoType]
    | AttributeMismatch  !DynamoType (Maybe DynamoType)
    | ParseFailure       !DynamoType Text
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

mismatchError :: DynamoType -> AttributeValue -> ValueError
mismatchError expect = AttributeMismatch expect . getAttributeType

-- | The closed set of types that can be stored natively in DynamoDB.
--
-- FIXME: Add note about binary base64 transparency.
data DynamoType
    = NULL -- ^ Null.
    | BOOL -- ^ Boolean.
    | L    -- ^ List.
    | M    -- ^ Map.
    | NS   -- ^ Number Set.
    | N    -- ^ Number.
    | BS   -- ^ Binary Set.
    | B    -- ^ Binary.
    | SS   -- ^ String Set.
    | S    -- ^ String.
      deriving (Eq, Show)

-- | Determine the populated 'AttributeValue' type.
getAttributeType :: AttributeValue -> Maybe DynamoType
getAttributeType AttributeValue'{..}
    | isJust _avNULL = Just NULL
    | isJust _avBOOL = Just BOOL
    | isJust _avL    = Just L
    | isJust _avM    = Just M
    | isJust _avNS   = Just NS
    | isJust _avN    = Just N
    | isJust _avBS   = Just BS
    | isJust _avB    = Just B
    | isJust _avSS   = Just SS
    | isJust _avS    = Just S
    | otherwise      = Nothing

-- | Check the invariant that one, and only one field is set.
isAttributeValue :: AttributeValue -> Bool
isAttributeValue AttributeValue'{..} =
    popCount board == 1
  where
    board :: Word16
    board =
          bit 0 _avBOOL
        $ bit 1 _avNULL
        $ bit 2 _avL
        $ bit 3 _avM
        $ bit 4 _avNS
        $ bit 5 _avN
        $ bit 6 _avBS
        $ bit 7 _avB
        $ bit 8 _avSS
        $ bit 9 _avS
        $ zeroBits

    bit x (Just _) = flip setBit x
    bit _ Nothing  = id

newValue :: AttributeValue -> Either ValueError Value
newValue v
    | isAttributeValue v = Right (Value v)
    | otherwise          = Left  (malformedError v)

getValueType :: Value -> DynamoType
getValueType =
    fromMaybe (error "Broken invariant for Value, no attribute field set.")
        . getAttributeType
        . getValue

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
class DynamoValue a where
    toValue   :: a -> Value
    fromValue :: Value -> Either ValueError a

instance DynamoValue Value where
    toValue   = id
    fromValue = Right

instance DynamoValue () where
    toValue   = const (setNull True)
    fromValue = fmap (const ()). getNull

instance DynamoValue Ordering where
    toValue   = setString . toText
    fromValue = getString >=> parseText S

instance DynamoValue Char where
    toValue   = setString . toText
    fromValue = getString >=> parseText S

instance DynamoValue Text where
    toValue   = setString
    fromValue = getString

instance DynamoValue LText.Text where
    toValue   = toValue . LText.toStrict
    fromValue = fmap LText.fromStrict . fromValue

instance DynamoValue ByteString where
    toValue   = setBinary
    fromValue = getBinary

instance DynamoValue LBS.ByteString where
    toValue   = toValue . LBS.toStrict
    fromValue = fmap LBS.fromStrict . fromValue

instance DynamoValue Bool where
    toValue   = setBool
    fromValue = getBool

instance DynamoValue Word where
    toValue   = setIntegral
    fromValue = getIntegral

instance DynamoValue Word8 where
    toValue   = setIntegral
    fromValue = getIntegral

instance DynamoValue Word32 where
    toValue   = setIntegral
    fromValue = getIntegral

instance DynamoValue Word64 where
    toValue   = setIntegral
    fromValue = getIntegral

instance DynamoValue Int where
    toValue   = setIntegral
    fromValue = getIntegral

instance DynamoValue Int8 where
    toValue   = setIntegral
    fromValue = getIntegral

instance DynamoValue Int32 where
    toValue   = setIntegral
    fromValue = getIntegral

instance DynamoValue Int64 where
    toValue   = setIntegral
    fromValue = getIntegral

instance DynamoValue Integer where
    toValue   = setIntegral
    fromValue = getIntegral

instance DynamoValue Natural where
    toValue   = setIntegral
    fromValue = getIntegral

instance DynamoValue Double where
    toValue   = toValue . Sci.fromFloatDigits
    fromValue = fmap Sci.toRealFloat . fromValue

instance DynamoValue Float where
    toValue   = toValue . Sci.fromFloatDigits
    fromValue = fmap Sci.toRealFloat . fromValue

instance DynamoValue Scientific where
    toValue   = setNumber
    fromValue = getNumber

instance DynamoValue a => DynamoValue (Identity a) where
    toValue   = toValue . runIdentity
    fromValue = fmap Identity . fromValue

instance DynamoValue a => DynamoValue (Const a b) where
    toValue   = toValue . getConst
    fromValue = fmap Const . fromValue

instance DynamoValue b => DynamoValue (Tagged s b) where
    toValue   = toValue . unTagged
    fromValue = fmap Tagged . fromValue

instance DynamoValue a => DynamoValue (Dual a) where
    toValue   = toValue . getDual
    fromValue = fmap Dual . fromValue

instance (DynamoValue a, CI.FoldCase a) => DynamoValue (CI a) where
    toValue   = toValue . CI.original
    fromValue = fmap CI.mk . fromValue

instance DynamoValue a => DynamoValue [a] where
    toValue   = setList . map toValue
    fromValue = getList >=> traverse fromValue

instance DynamoValue a => DynamoValue (NonEmpty a) where
     toValue   = toValue . NE.toList
     fromValue =
         fromValue >=> \case
             x:xs -> Right (x :| xs)
             _    -> Left (ParseFailure L "Unexpected empty list.")

instance DynamoValue a => DynamoValue (Vector a) where
    toValue   = setVector
    fromValue = getVector

instance (VectorGen.Vector VectorUnbox.Vector a, DynamoValue a)
      => DynamoValue (VectorUnbox.Vector a) where
    toValue   = setVector
    fromValue = getVector

instance (Storable a, DynamoValue a)
      => DynamoValue (VectorStore.Vector a) where
    toValue   = setVector
    fromValue = getVector

instance (VectorPrim.Prim a, DynamoValue a)
      => DynamoValue (VectorPrim.Vector a) where
    toValue   = setVector
    fromValue = getVector

instance DynamoValue a => DynamoValue (Seq a) where
    toValue   = toValue . toList
    fromValue = fmap Seq.fromList . fromValue

instance DynamoValue IntSet where
    toValue   = setNumberSet . map fromIntegral . IntSet.toList
    fromValue = getNumberSet >=> fmap IntSet.fromList . traverse go
      where
        go    = first err . Sci.floatingOrInteger
        err r = ParseFailure NS $
            "Expected integral value, got: " <> toText (r :: Double)

instance DynamoValue a => DynamoValue (IntMap a) where
    toValue =
        setMap . HashMap.fromList . map (bimap toText toValue) . IntMap.toList

    fromValue =
        getMap >=> fmap IntMap.fromList . traverse go . HashMap.toList
      where
        go (k, v) =
           (,) <$> first (ParseFailure M . toText) (fromText k)
               <*> fromValue v

instance DynamoValue a => DynamoValue (Map Text a) where
    toValue   = toValue . HashMap.fromList . Map.toList
    fromValue = fmap (Map.fromList . HashMap.toList) . fromValue

instance DynamoValue (Set Text) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set LText.Text) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set ByteString) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set LBS.ByteString) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Word) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Word8) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Word16) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Word32) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Word64) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Int) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Int8) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Int16) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Int32) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Int64) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Integer) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Natural) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Float) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Double) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue (Set Scientific) where
    toValue   = toValue . toHashSet
    fromValue = fmap fromHashSet . fromValue

instance DynamoValue a => DynamoValue (HashMap Text a) where
    toValue   = setMap . HashMap.map toValue
    fromValue = getMap >=> traverse fromValue

instance DynamoValue (HashSet Text) where
    toValue   = setStringSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getStringSet

instance DynamoValue (HashSet LText.Text) where
    toValue   = setStringSet . map LText.toStrict . HashSet.toList
    fromValue = fmap (HashSet.fromList . map LText.fromStrict) . getStringSet

instance DynamoValue (HashSet ByteString) where
    toValue   = setBinarySet . HashSet.toList
    fromValue = fmap HashSet.fromList . getBinarySet

instance DynamoValue (HashSet LBS.ByteString) where
    toValue   = setBinarySet . map LBS.toStrict . HashSet.toList
    fromValue = fmap (HashSet.fromList . map LBS.fromStrict) . getBinarySet

instance DynamoValue (HashSet Word) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Word8) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Word16) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Word32) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Word64) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Int) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Int8) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Int16) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Int32) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Int64) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Integer) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Natural) where
    toValue   = setIntegralSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getIntegralSet

instance DynamoValue (HashSet Float) where
    toValue   = setNumberSet . map Sci.fromFloatDigits . HashSet.toList
    fromValue = fmap (HashSet.fromList . map Sci.toRealFloat) . getNumberSet

instance DynamoValue (HashSet Double) where
    toValue   = setNumberSet . map Sci.fromFloatDigits . HashSet.toList
    fromValue = fmap (HashSet.fromList . map Sci.toRealFloat) . getNumberSet

instance DynamoValue (HashSet Scientific) where
    toValue   = setNumberSet . HashSet.toList
    fromValue = fmap HashSet.fromList . getNumberSet

instance DynamoValue Version where
    toValue   = toValue . toJSON
    fromValue = fromValue >=> getJSON

instance DynamoValue Day where
    toValue   = toValue . toJSON
    fromValue = fromValue >=> getJSON

instance DynamoValue TimeOfDay where
    toValue   = toValue . toJSON
    fromValue = fromValue >=> getJSON

instance DynamoValue ZonedTime where
    toValue   = toValue . toJSON
    fromValue = fromValue >=> getJSON

instance DynamoValue LocalTime where
    toValue   = toValue . toJSON
    fromValue = fromValue >=> getJSON

instance DynamoValue NominalDiffTime where
    toValue   = toValue . toJSON
    fromValue = fromValue >=> getJSON

instance DynamoValue UTCTime where
    toValue   = toValue . toJSON
    fromValue = fromValue >=> getJSON

instance DynamoValue DotNetTime where
    toValue   = toValue . toJSON
    fromValue = fromValue >=> getJSON

-- FIXME: add top-level note about binary sets/strings.
instance DynamoValue JS.Value where
    toValue = \case
        JS.String x -> setString x
        JS.Number x -> setNumber x
        JS.Bool   x -> setBool   x
        JS.Array  x -> setList . map toValue $ Vector.toList x
        JS.Object x -> setMap  $ HashMap.map toValue x
        JS.Null     -> setNull True

    fromValue v = case getValueType v of
        S    -> JS.String <$> getString v
        N    -> JS.Number <$> getNumber v
        BOOL -> JS.Bool   <$> getBool v
        M    -> JS.Object <$> (getMap v >>= traverse fromValue)
        L    -> JS.Array . Vector.fromList <$> (getList v >>= traverse fromValue)
        NULL -> JS.Null   <$  getNull v
        t    -> Left (ParseFailure t "Unable to parse unsupported JSON value.")

setNull :: Bool -> Value
setNull x = Value (set avNULL (Just x) attributeValue)

getNull :: Value -> Either ValueError Bool
getNull (Value v) = note (mismatchError NULL v) (_avNULL v)

setBool :: Bool -> Value
setBool x = Value (set avBOOL (Just x) attributeValue)

getBool :: Value -> Either ValueError Bool
getBool (Value v) = note (mismatchError BOOL v) (_avBOOL v)

setList :: [Value] -> Value
setList x = Value (set avL (coerce x) attributeValue)

getList :: Value -> Either ValueError [Value]
getList (Value v) = note (mismatchError L v) (coerce <$> _avL v)

setVector :: (VectorGen.Vector v a, DynamoValue a) => v a -> Value
setVector = toValue . Vector.toList . Vector.convert

getVector :: (VectorGen.Vector v a, DynamoValue a)
          => Value
          -> Either ValueError (v a)
getVector = fmap (Vector.convert . Vector.fromList) . fromValue

setMap :: HashMap Text Value -> Value
setMap x = Value (set avM (coerce x) attributeValue)

getMap :: Value -> Either ValueError (HashMap Text Value)
getMap (Value v) = note (mismatchError M v) (coerce . toMap <$> _avM v)

setNumberSet :: [Scientific] -> Value
setNumberSet x = Value (set avNS (map toText x) attributeValue)

getNumberSet :: Value -> Either ValueError [Scientific]
getNumberSet (Value v) =
    note (mismatchError NS v) (_avNS v)
        >>= first (ParseFailure NS . Text.pack)
          . traverse fromText

setNumber :: Scientific -> Value
setNumber x = Value (set avN (Just (toText x)) attributeValue)

getNumber :: Value -> Either ValueError Scientific
getNumber (Value v) =
    note (mismatchError N v) (_avN v)
        >>= first (ParseFailure N . Text.pack)
          . fromText

setIntegralSet :: Integral a => [a] -> Value
setIntegralSet = setNumberSet . map fromIntegral

getIntegralSet :: Integral a => Value -> Either ValueError [a]
getIntegralSet = getNumberSet >=> traverse parse
  where
    parse = first err . Sci.floatingOrInteger
    err r = ParseFailure NS $
        "Expected integral value, got: " <> toText (r :: Double)

setIntegral :: Integral a => a -> Value
setIntegral = setNumber . fromIntegral

getIntegral :: Integral a => Value -> Either ValueError a
getIntegral = getNumber >=> first err . Sci.floatingOrInteger
  where
    err r = ParseFailure NS $
        "Expected integral value, got: " <> toText (r :: Double)

setBinarySet :: [ByteString] -> Value
setBinarySet x = Value (set avBS x attributeValue)

getBinarySet :: Value -> Either ValueError [ByteString]
getBinarySet (Value v) = note (mismatchError BS v) (coerce (_avBS v))

setBinary :: ByteString -> Value
setBinary x = Value (set avB (Just x) attributeValue)

getBinary :: Value -> Either ValueError ByteString
getBinary (Value v) = note (mismatchError B v) (coerce (_avB v))

setStringSet :: [Text] -> Value
setStringSet x = Value (set avSS x attributeValue)

getStringSet :: Value -> Either ValueError [Text]
getStringSet (Value v) = note (mismatchError SS v) (_avSS v)

setString :: Text -> Value
setString x = Value (set avS (Just x) attributeValue)

getString :: Value -> Either ValueError Text
getString (Value v) = note (mismatchError S v) (_avS v)

note :: e -> Maybe a -> Either e a
note e Nothing  = Left  e
note _ (Just x) = Right x

getJSON :: FromJSON a => JS.Value -> Either ValueError a
getJSON = first (ParseFailure S . Text.pack) . parseEither parseJSON

parseText :: FromText a => DynamoType -> Text -> Either ValueError a
parseText t = first (ParseFailure t . Text.pack) . fromText

toHashSet :: (Eq a, Hashable a) => Set a -> HashSet a
toHashSet = HashSet.fromList . Set.toList

fromHashSet :: Ord a => HashSet a -> Set a
fromHashSet = Set.fromList . HashSet.toList

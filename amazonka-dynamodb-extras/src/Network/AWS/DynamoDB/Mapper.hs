{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Description about DynamoDB items and attributes.
--
-- Notes:
--   * Available representations, and instances (such as missing, '()', 'Maybe').
--   * Principles of small values (Attribute) vs large/complex values (Item).
--   * Performance concerns, and the underlying representations.
--      - Time values run through Aeson's parser
--      - Vectors and sets stored as lists
module Network.AWS.DynamoDB.Mapper where
    -- (

    -- -- * Items
    --   ItemError (..)
    -- , Item      (..)

    -- -- * Attributes
    -- , Attribute (..)
    -- ) where

import Control.Applicative (Const (..))
import Control.Exception   (Exception)
import Control.Lens        (ASetter', Getting, set, view, (^.))
import Control.Monad       ((>=>))

import Data.Aeson            (FromJSON (..), ToJSON (..))
import Data.Aeson.Types      (DotNetTime, parseEither)
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
import Data.Map.Strict       (Map (..))
import Data.Maybe            (catMaybes, fromMaybe, isJust)
import Data.Monoid           (Dual (..), (<>))
import Data.Proxy            (Proxy (..))
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

import Network.AWS.Data.Text
import Network.AWS.DynamoDB  hiding (ScalarAttributeType (..))

import Network.AWS.DynamoDB.Mapper.Value
import Network.AWS.DynamoDB.Mapper.Value.Unsafe

import Numeric.Natural (Natural)

import qualified Data.Aeson             as JS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.CaseInsensitive   as CI
import qualified Data.Scientific        as Sci
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as LText

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

type Attribute = (Text, Value)

item :: [Attribute] -> HashMap Text Value
item = HashMap.fromList
{-# INLINE item #-}

attr :: DynamoValue a => Text -> a -> Attribute
attr k v = (k, toValue v)
{-# INLINE attr #-}

parse :: DynamoValue a => Text -> HashMap Text Value -> Either ItemError a
parse k m =
    case HashMap.lookup k m of
        Nothing -> Left (MissingAttribute k)
        Just v  -> first (ValueError k) (fromValue v)
{-# INLINE parse #-}

encode :: DynamoItem a => a -> HashMap Text AttributeValue
encode = coerce . toItem
{-# INLINE encode #-}

decode :: DynamoItem a => HashMap Text AttributeValue -> Either ItemError a
decode = HashMap.traverseWithKey go >=> fromItem
  where
    go k = first (ValueError k) . newValue
{-# INLINE decode #-}

-- | You can use this if you know the AttributeValue's are safe, as in they
-- have been returned unmodified from DynamoDB.
unsafeDecode :: DynamoItem a => HashMap Text AttributeValue -> Either ItemError a
unsafeDecode = fromItem . coerce

data ItemError
    = ValueError       Text ValueError
    | MissingAttribute Text
      deriving (Eq, Show, Typeable)

instance Exception ItemError

-- | Serialise a value to a complex DynamoDB item.
--
-- Note about complex types
--
-- The maximum item size in DynamoDB is 400 KB, which includes both attribute name
-- binary length (UTF-8 length) and attribute value lengths (again binary
-- length). The attribute name counts towards the size limit.
--
-- For example, consider an item with two attributes: one attribute named
-- "shirt-color" with value "R" and another attribute named "shirt-size" with
-- value "M". The total size of that item is 23 bytes.
--
-- An 'Item' is subject to the following law:
--
-- @
-- fromItem (toItem x) ≡ Right x
-- @
--
-- That is, you get back what you put in.
class DynamoItem a where
    toItem   :: a -> HashMap Text Value
    fromItem :: HashMap Text Value -> Either ItemError a

instance DynamoValue a => DynamoItem (Map Text a) where
    toItem   = toItem . HashMap.fromList . Map.toList
    fromItem = fmap (Map.fromList . HashMap.toList) . fromItem

instance DynamoValue a => DynamoItem (HashMap Text a) where
    toItem   = HashMap.map toValue
    fromItem = HashMap.traverseWithKey (\k -> first (ValueError k) . fromValue)

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

getJSON :: FromJSON a => JS.Value -> Either ValueError a
getJSON = first (ParseFailure S . Text.pack) . parseEither parseJSON

parseText :: FromText a => DynamoType -> Text -> Either ValueError a
parseText t = first (ParseFailure t . Text.pack) . fromText

toHashSet :: (Eq a, Hashable a) => Set a -> HashSet a
toHashSet = HashSet.fromList . Set.toList

fromHashSet :: Ord a => HashSet a -> Set a
fromHashSet = Set.fromList . HashSet.toList

setVector :: (VectorGen.Vector v a, DynamoValue a) => v a -> Value
setVector = toValue . Vector.toList . Vector.convert

getVector :: (VectorGen.Vector v a, DynamoValue a)
          => Value
          -> Either ValueError (v a)
getVector = fmap (Vector.convert . Vector.fromList) . fromValue

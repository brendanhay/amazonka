{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.AWS.DynamoDB.Mapper.Value
    (
    -- * Native Types
      DynamoType (..)
    , getAttributeType
    , isAttributeValue

    -- * Errors
    , ValueError (..)
    , malformedError
    , mismatchError

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

    -- ** Map
    , setMap
    , getMap

    -- ** Number
    , setNumberSet
    , getNumberSet

    , setNumber
    , getNumber

    , setIntegralSet
    , getIntegralSet

    , setIntegral
    , getIntegral

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

import Control.Exception (Exception)
import Control.Lens      (set)
import Control.Monad     ((>=>))

import Data.Bifunctor      (first)
import Data.Bits           (popCount, setBit, zeroBits)
import Data.ByteString     (ByteString)
import Data.Coerce         (coerce)
import Data.HashMap.Strict (HashMap)
import Data.Maybe          (catMaybes, fromMaybe, isJust)
import Data.Monoid         ((<>))
import Data.Scientific     (Scientific)
import Data.Text           (Text)
import Data.Typeable       (Typeable)
import Data.Word           (Word16)

import Network.AWS.Data.Base64                  (Base64 (..))
import Network.AWS.Data.Map                     (toMap)
import Network.AWS.Data.Text                    (fromText, toText)
import Network.AWS.DynamoDB                     hiding
                                                 (ScalarAttributeType (..))
import Network.AWS.DynamoDB.Mapper.Value.Unsafe
import Network.AWS.DynamoDB.Types.Product       (AttributeValue (..))

import qualified Data.Scientific     as Sci
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector
import qualified Data.Vector.Generic as VectorGen

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

note :: e -> Maybe a -> Either e a
note e Nothing  = Left  e
note _ (Just x) = Right x

newValue :: AttributeValue -> Either ValueError Value
newValue v
    | isAttributeValue v = Right (Value v)
    | otherwise          = Left  (malformedError v)

getValueType :: Value -> DynamoType
getValueType =
    fromMaybe (error "Broken invariant for Value, no attribute field set.")
        . getAttributeType
        . getValue

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

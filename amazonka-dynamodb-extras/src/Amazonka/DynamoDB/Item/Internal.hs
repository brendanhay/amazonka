{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Amazonka.DynamoDB.Item.Internal
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Amazonka.DynamoDB.Item.Internal where

import Data.Bits      (popCount, setBit, zeroBits)
import Data.Hashable  (Hashable (..))
import Data.Maybe     (isJust)
import Data.Proxy     (Proxy (..))
import Data.Semigroup ((<>))
import Data.Word      (Word16)

import GHC.Generics (Generic)

import Network.AWS.Data.Text
import Network.AWS.DynamoDB.Types.Product (AttributeValue (..))

import qualified Data.Text as Text

-- | An opaque DynamoDB storable-value that ensures the invariant
-- that only a single field in an 'AttributeValue' may be set.
newtype Value = UnsafeValue { getValue :: AttributeValue }
    deriving (Eq, Show)

instance Hashable Value where
    hashWithSalt s (UnsafeValue v) = hashWithSalt s v

newtype NativeValue a = UnsafeNativeValue { getNativeValue :: AttributeValue }
    deriving (Eq, Show)
    -- Note: While a GADT here with branches corresponding to the 'NativeType'
    -- could be used - a newtype and the assumption that set/get functions
    -- are used to a enforce invariants is one less layer of indirection.

instance DynamoNative a => Hashable (NativeValue a) where
    hashWithSalt s (UnsafeNativeValue v) =
        s `hashWithSalt` getNativeType (Proxy :: Proxy a) `hashWithSalt` v

class DynamoNative a where
    getNativeType :: Proxy a -> NativeType

instance DynamoNative 'NULL where getNativeType = const NULL
instance DynamoNative 'BOOL where getNativeType = const BOOL
instance DynamoNative 'L    where getNativeType = const L
instance DynamoNative 'M    where getNativeType = const M
instance DynamoNative 'NS   where getNativeType = const NS
instance DynamoNative 'N    where getNativeType = const N
instance DynamoNative 'BS   where getNativeType = const BS
instance DynamoNative 'B    where getNativeType = const B
instance DynamoNative 'SS   where getNativeType = const SS
instance DynamoNative 'S    where getNativeType = const S

instance DynamoNative a => DynamoNative (NativeValue a) where
    getNativeType _ = getNativeType (Proxy :: Proxy a)

-- | The closed set of types that can be stored natively in DynamoDB.
--
-- FIXME: Add note about binary base64 transparency.
data NativeType
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
      deriving (Eq, Generic)

instance Hashable NativeType

instance Show NativeType where
    show = Text.unpack . toText

instance ToText NativeType where
    toText = \case
        NULL -> "NULL"
        BOOL -> "BOOL"
        L    -> "L"
        M    -> "M"
        NS   -> "NS"
        N    -> "N"
        BS   -> "BS"
        B    -> "B"
        SS   -> "SS"
        S    -> "S"

instance FromText NativeType where
    parser = takeText >>= \case
        "NULL" -> pure NULL
        "BOOL" -> pure BOOL
        "L"    -> pure L
        "M"    -> pure M
        "NS"   -> pure NS
        "N"    -> pure N
        "BS"   -> pure BS
        "B"    -> pure B
        "SS"   -> pure SS
        "S"    -> pure S
        e      -> fromTextError ("Unable to parse NativeType from: " <> e)

-- | Determine the populated 'AttributeValue' type.
getAttributeType :: AttributeValue -> Maybe NativeType
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
isSingleAttribute :: AttributeValue -> Bool
isSingleAttribute AttributeValue'{..} =
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

    bit n = maybe id (const (`setBit` n))

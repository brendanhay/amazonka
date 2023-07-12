{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Amazonka.DynamoDBStreams.Internal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Internal where

import Amazonka.Data
import Amazonka.Prelude
import Data.Aeson (pairs)
import Data.Hashable
import Data.Map (Map)
import Data.Vector (Vector)

#if MIN_VERSION_aeson(2,0,0)
import qualified  Data.Aeson.KeyMap as KeyMap
#else
import qualified  Data.HashMap.Strict as KeyMap
#endif

-- | Represents the data for an attribute.
--
-- DynamoDB sends and receives JSON objects which contain a single
-- item whose key is a data type and the value is the data itself. We
-- provide an actual sum type to interact with these.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types>
-- in the /Amazon DynamoDB Developer Guide/.
data AttributeValue
  = -- | An attribute of type List. For example:
    --
    -- @\"L\": [ {\"S\": \"Cookies\"} , {\"S\": \"Coffee\"}, {\"N\", \"3.14159\"}]@
    L (Vector AttributeValue)
  | -- | An attribute of type Number Set. For example:
    --
    -- @\"NS\": [\"42.2\", \"-19\", \"7.5\", \"3.14\"]@
    --
    -- Numbers are sent across the network to DynamoDB as strings, to maximize
    -- compatibility across languages and libraries. However, DynamoDB treats
    -- them as number type attributes for mathematical operations.
    NS (Vector Text)
  | -- | An attribute of type Map. For example:
    --
    -- @\"M\": {\"Name\": {\"S\": \"Joe\"}, \"Age\": {\"N\": \"35\"}}@
    M (Map Text AttributeValue)
  | -- | An attribute of type Null. For example:
    --
    -- @\"NULL\": true@
    NULL
  | -- | An attribute of type Number. For example:
    --
    -- @\"N\": \"123.45\"@
    --
    -- Numbers are sent across the network to DynamoDB as strings, to maximize
    -- compatibility across languages and libraries. However, DynamoDB treats
    -- them as number type attributes for mathematical operations.
    N Text
  | -- | An attribute of type Binary Set. For example:
    --
    -- @\"BS\": [\"U3Vubnk=\", \"UmFpbnk=\", \"U25vd3k=\"]@
    BS (Vector Base64)
  | -- | An attribute of type Binary. For example:
    --
    -- @\"B\": \"dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk\"@
    B Base64
  | -- | An attribute of type String Set. For example:
    --
    -- @\"SS\": [\"Giraffe\", \"Hippo\" ,\"Zebra\"]@
    SS (Vector Text)
  | -- | An attribute of type String. For example:
    --
    -- @\"S\": \"Hello\"@
    S Text
  | -- | An attribute of type Boolean. For example:
    --
    -- @\"BOOL\": true@
    BOOL Bool
  deriving stock (Eq, Read, Show, Generic)
  deriving anyclass (NFData)

instance Hashable AttributeValue where
  hashWithSalt salt = \case
    L avs -> salt `hashWithSalt` (0 :: Int) `hashVector` avs
    NS ns -> salt `hashWithSalt` (1 :: Int) `hashVector` ns
    M m -> salt `hashWithSalt` (2 :: Int) `hashWithSalt` m
    NULL -> salt `hashWithSalt` (3 :: Int) `hashWithSalt` ()
    N n -> salt `hashWithSalt` (4 :: Int) `hashWithSalt` n
    BS bs -> salt `hashWithSalt` (5 :: Int) `hashVector` bs
    B b -> salt `hashWithSalt` (6 :: Int) `hashWithSalt` b
    SS ss -> salt `hashWithSalt` (7 :: Int) `hashVector` ss
    S s -> salt `hashWithSalt` (8 :: Int) `hashWithSalt` s
    BOOL b -> salt `hashWithSalt` (9 :: Int) `hashWithSalt` b
    where
      hashVector :: Hashable a => Int -> Vector a -> Int
      hashVector = hashUsing toList

instance FromJSON AttributeValue where
  parseJSON = withObject "AttributeValue" $ \o ->
    case KeyMap.toList o of
      [("L", v)] -> L <$> parseJSON v
      [("NS", v)] -> NS <$> parseJSON v
      [("M", v)] -> M <$> parseJSON v
      [("NULL", _)] -> pure NULL
      [("N", v)] -> N <$> parseJSON v
      [("BS", v)] -> BS <$> parseJSON v
      [("B", v)] -> B <$> parseJSON v
      [("SS", v)] -> SS <$> parseJSON v
      [("S", v)] -> S <$> parseJSON v
      [("BOOL", v)] -> BOOL <$> parseJSON v
      [] -> fail "No keys"
      _ -> fail $ "Multiple or unrecognized keys: " ++ show (KeyMap.keys o)

instance ToJSON AttributeValue where
  toJSON =
    object . pure . \case
      L avs -> "L" .= avs
      NS ns -> "NS" .= ns
      M m -> "M" .= m
      NULL -> "NULL" .= True
      N n -> "N" .= n
      BS bs -> "BS" .= bs
      B b -> "B" .= b
      SS ss -> "SS" .= ss
      S s -> "S" .= s
      BOOL b -> "BOOL" .= b
  toEncoding =
    pairs . \case
      L avs -> "L" .= avs
      NS ns -> "NS" .= ns
      M m -> "M" .= m
      NULL -> "NULL" .= True
      N n -> "N" .= n
      BS bs -> "BS" .= bs
      B b -> "B" .= b
      SS ss -> "SS" .= ss
      S s -> "S" .= s
      BOOL b -> "BOOL" .= b

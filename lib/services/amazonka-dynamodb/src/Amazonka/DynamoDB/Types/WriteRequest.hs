{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Amazonka.DynamoDB.WriteRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.WriteRequest where

import Amazonka.DynamoDB.Types.AttributeValue (AttributeValue)
import Amazonka.Prelude
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    object,
    pairs,
    withObject,
    (.:),
    (.=),
  )
import Data.Map (Map)

#if MIN_VERSION_aeson(2,0,0)
import qualified  Data.Aeson.KeyMap as KeyMap
#else
import qualified  Data.HashMap.Strict as KeyMap
#endif

-- | Represents an operation to perform - either @DeleteItem@ or @PutItem@.
-- You can only request one of these operations, not both, in a single
-- @WriteRequest@. If you do need to perform both of these operations, you
-- need to provide two separate @WriteRequest@ objects.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_BatchWriteItem.html BatchWriteItem>
-- in the /Amazon DynamoDB Developer Guide/.
data WriteRequest
  = -- | A request to perform a @DeleteItem@ operation.
    --
    -- A map of attribute name to attribute values, representing the primary
    -- key of the item to delete. All of the table\'s primary key attributes
    -- must be specified, and their data types must match those of the table\'s
    -- key schema.
    DeleteRequest (Map Text AttributeValue)
  | -- | A request to perform a @PutItem@ operation.
    --
    -- A map of attribute name to attribute values, representing the primary
    -- key of an item to be processed by @PutItem@. All of the table\'s primary
    -- key attributes must be specified, and their data types must match those
    -- of the table\'s key schema. If any attributes are present in the item
    -- that are part of an index key schema for the table, their types must
    -- match the index key schema.
    PutRequest (Map Text AttributeValue)
  deriving stock (Eq, Read, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance FromJSON WriteRequest where
  parseJSON = withObject "WriteRequest" $ \o ->
    case KeyMap.toList o of
      [("DeleteRequest", v)] -> DeleteRequest <$> key v
      [("PutRequest", v)] -> PutRequest <$> item v
      [] -> fail "No keys"
      _ -> fail $ "Multiple or unrecognized keys: " ++ show (KeyMap.keys o)
    where
      item = withObject "Item" (.: "Item")
      key = withObject "Key" (.: "Key")

instance ToJSON WriteRequest where
  toJSON =
    object . pure . \case
      DeleteRequest key -> "DeleteRequest" .= object ["Key" .= key]
      PutRequest item -> "PutRequest" .= object ["Item" .= item]

  toEncoding =
    pairs . \case
      DeleteRequest key -> "DeleteRequest" .= object ["Key" .= key]
      PutRequest item -> "PutRequest" .= object ["Item" .= item]

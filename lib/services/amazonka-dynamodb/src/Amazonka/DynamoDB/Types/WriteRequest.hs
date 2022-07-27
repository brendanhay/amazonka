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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.WriteRequest where

import Amazonka.Core
import Amazonka.DynamoDB.Types.AttributeValue (AttributeValue)
import Amazonka.Prelude
import Data.Aeson (pairs)
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_WriteRequest.html WriteRequest>
-- in the /Amazon DynamoDB Developer Guide/.
data WriteRequest
  = -- | A request to perform a @DeleteItem@ operation.
    DeleteRequest (Map Text AttributeValue)
  | -- | A request to perform a @PutItem@ operation.
    PutRequest (Map Text AttributeValue)
  deriving stock (Eq, Read, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance FromJSON WriteRequest where
  parseJSON = withObject "WriteRequest" $ \o ->
    case KeyMap.toList o of
      [("DeleteRequest", v)] -> DeleteRequest <$> item v
      [("PutRequest", v)] -> PutRequest <$> item v
      [] -> fail "No keys"
      _ -> fail $ "Multiple or unrecognized keys: " ++ show (KeyMap.keys o)
    where
      item = withObject "Item" (.: "Item")

instance ToJSON WriteRequest where
  toJSON =
    object . pure . \case
      DeleteRequest item -> "DeleteRequest" .= object ["Item" .= item]
      PutRequest item -> "PutRequest" .= object ["Item" .= item]

  toEncoding = pairs . \case
    DeleteRequest item -> "DeleteRequest" .= (object ["Item" .= item])
    PutRequest item -> "PutRequest" .= (object ["Item" .= item])

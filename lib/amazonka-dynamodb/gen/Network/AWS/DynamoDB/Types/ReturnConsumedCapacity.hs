{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReturnConsumedCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReturnConsumedCapacity where

import Network.AWS.Prelude

-- | Determines the level of detail about provisioned throughput consumption that is returned in the response:
--
--
--     * @INDEXES@ - The response includes the aggregate @ConsumedCapacity@ for the operation, together with @ConsumedCapacity@ for each table and secondary index that was accessed.
--
-- Note that some operations, such as @GetItem@ and @BatchGetItem@ , do not access any indexes at all. In these cases, specifying @INDEXES@ will only return @ConsumedCapacity@ information for table(s).
--
--     * @TOTAL@ - The response includes only the aggregate @ConsumedCapacity@ for the operation.
--
--     * @NONE@ - No @ConsumedCapacity@ details are included in the response.
data ReturnConsumedCapacity
  = RCCIndexes
  | RCCNone
  | RCCTotal
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ReturnConsumedCapacity where
  parser =
    takeLowerText >>= \case
      "indexes" -> pure RCCIndexes
      "none" -> pure RCCNone
      "total" -> pure RCCTotal
      e ->
        fromTextError $
          "Failure parsing ReturnConsumedCapacity from value: '" <> e
            <> "'. Accepted values: indexes, none, total"

instance ToText ReturnConsumedCapacity where
  toText = \case
    RCCIndexes -> "INDEXES"
    RCCNone -> "NONE"
    RCCTotal -> "TOTAL"

instance Hashable ReturnConsumedCapacity

instance NFData ReturnConsumedCapacity

instance ToByteString ReturnConsumedCapacity

instance ToQuery ReturnConsumedCapacity

instance ToHeader ReturnConsumedCapacity

instance ToJSON ReturnConsumedCapacity where
  toJSON = toJSONText

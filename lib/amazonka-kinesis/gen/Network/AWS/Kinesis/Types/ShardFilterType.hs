{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ShardFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ShardFilterType where

import Network.AWS.Prelude

data ShardFilterType
  = AfterShardId
  | AtLatest
  | AtTimestamp
  | AtTrimHorizon
  | FromTimestamp
  | FromTrimHorizon
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

instance FromText ShardFilterType where
  parser =
    takeLowerText >>= \case
      "after_shard_id" -> pure AfterShardId
      "at_latest" -> pure AtLatest
      "at_timestamp" -> pure AtTimestamp
      "at_trim_horizon" -> pure AtTrimHorizon
      "from_timestamp" -> pure FromTimestamp
      "from_trim_horizon" -> pure FromTrimHorizon
      e ->
        fromTextError $
          "Failure parsing ShardFilterType from value: '" <> e
            <> "'. Accepted values: after_shard_id, at_latest, at_timestamp, at_trim_horizon, from_timestamp, from_trim_horizon"

instance ToText ShardFilterType where
  toText = \case
    AfterShardId -> "AFTER_SHARD_ID"
    AtLatest -> "AT_LATEST"
    AtTimestamp -> "AT_TIMESTAMP"
    AtTrimHorizon -> "AT_TRIM_HORIZON"
    FromTimestamp -> "FROM_TIMESTAMP"
    FromTrimHorizon -> "FROM_TRIM_HORIZON"

instance Hashable ShardFilterType

instance NFData ShardFilterType

instance ToByteString ShardFilterType

instance ToQuery ShardFilterType

instance ToHeader ShardFilterType

instance ToJSON ShardFilterType where
  toJSON = toJSONText

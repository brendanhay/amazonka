{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ShardIteratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ShardIteratorType where

import Network.AWS.Prelude

data ShardIteratorType
  = SITAfterSequenceNumber
  | SITAtSequenceNumber
  | SITAtTimestamp
  | SITLatest
  | SITTrimHorizon
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

instance FromText ShardIteratorType where
  parser =
    takeLowerText >>= \case
      "after_sequence_number" -> pure SITAfterSequenceNumber
      "at_sequence_number" -> pure SITAtSequenceNumber
      "at_timestamp" -> pure SITAtTimestamp
      "latest" -> pure SITLatest
      "trim_horizon" -> pure SITTrimHorizon
      e ->
        fromTextError $
          "Failure parsing ShardIteratorType from value: '" <> e
            <> "'. Accepted values: after_sequence_number, at_sequence_number, at_timestamp, latest, trim_horizon"

instance ToText ShardIteratorType where
  toText = \case
    SITAfterSequenceNumber -> "AFTER_SEQUENCE_NUMBER"
    SITAtSequenceNumber -> "AT_SEQUENCE_NUMBER"
    SITAtTimestamp -> "AT_TIMESTAMP"
    SITLatest -> "LATEST"
    SITTrimHorizon -> "TRIM_HORIZON"

instance Hashable ShardIteratorType

instance NFData ShardIteratorType

instance ToByteString ShardIteratorType

instance ToQuery ShardIteratorType

instance ToHeader ShardIteratorType

instance ToJSON ShardIteratorType where
  toJSON = toJSONText

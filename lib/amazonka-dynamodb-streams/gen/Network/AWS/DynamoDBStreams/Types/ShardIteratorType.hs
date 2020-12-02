{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.ShardIteratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.ShardIteratorType where

import Network.AWS.Prelude

data ShardIteratorType
  = AfterSequenceNumber
  | AtSequenceNumber
  | Latest
  | TrimHorizon
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
      "after_sequence_number" -> pure AfterSequenceNumber
      "at_sequence_number" -> pure AtSequenceNumber
      "latest" -> pure Latest
      "trim_horizon" -> pure TrimHorizon
      e ->
        fromTextError $
          "Failure parsing ShardIteratorType from value: '" <> e
            <> "'. Accepted values: after_sequence_number, at_sequence_number, latest, trim_horizon"

instance ToText ShardIteratorType where
  toText = \case
    AfterSequenceNumber -> "AFTER_SEQUENCE_NUMBER"
    AtSequenceNumber -> "AT_SEQUENCE_NUMBER"
    Latest -> "LATEST"
    TrimHorizon -> "TRIM_HORIZON"

instance Hashable ShardIteratorType

instance NFData ShardIteratorType

instance ToByteString ShardIteratorType

instance ToQuery ShardIteratorType

instance ToHeader ShardIteratorType

instance ToJSON ShardIteratorType where
  toJSON = toJSONText

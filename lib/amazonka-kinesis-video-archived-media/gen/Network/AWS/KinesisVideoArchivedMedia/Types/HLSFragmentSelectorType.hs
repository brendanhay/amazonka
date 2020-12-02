{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSFragmentSelectorType where

import Network.AWS.Prelude

data HLSFragmentSelectorType
  = ProducerTimestamp
  | ServerTimestamp
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

instance FromText HLSFragmentSelectorType where
  parser =
    takeLowerText >>= \case
      "producer_timestamp" -> pure ProducerTimestamp
      "server_timestamp" -> pure ServerTimestamp
      e ->
        fromTextError $
          "Failure parsing HLSFragmentSelectorType from value: '" <> e
            <> "'. Accepted values: producer_timestamp, server_timestamp"

instance ToText HLSFragmentSelectorType where
  toText = \case
    ProducerTimestamp -> "PRODUCER_TIMESTAMP"
    ServerTimestamp -> "SERVER_TIMESTAMP"

instance Hashable HLSFragmentSelectorType

instance NFData HLSFragmentSelectorType

instance ToByteString HLSFragmentSelectorType

instance ToQuery HLSFragmentSelectorType

instance ToHeader HLSFragmentSelectorType

instance ToJSON HLSFragmentSelectorType where
  toJSON = toJSONText

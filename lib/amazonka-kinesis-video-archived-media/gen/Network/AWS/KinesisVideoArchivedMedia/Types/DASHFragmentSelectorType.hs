{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelectorType where

import Network.AWS.Prelude

data DASHFragmentSelectorType
  = DASHFSTProducerTimestamp
  | DASHFSTServerTimestamp
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

instance FromText DASHFragmentSelectorType where
  parser =
    takeLowerText >>= \case
      "producer_timestamp" -> pure DASHFSTProducerTimestamp
      "server_timestamp" -> pure DASHFSTServerTimestamp
      e ->
        fromTextError $
          "Failure parsing DASHFragmentSelectorType from value: '" <> e
            <> "'. Accepted values: producer_timestamp, server_timestamp"

instance ToText DASHFragmentSelectorType where
  toText = \case
    DASHFSTProducerTimestamp -> "PRODUCER_TIMESTAMP"
    DASHFSTServerTimestamp -> "SERVER_TIMESTAMP"

instance Hashable DASHFragmentSelectorType

instance NFData DASHFragmentSelectorType

instance ToByteString DASHFragmentSelectorType

instance ToQuery DASHFragmentSelectorType

instance ToHeader DASHFragmentSelectorType

instance ToJSON DASHFragmentSelectorType where
  toJSON = toJSONText

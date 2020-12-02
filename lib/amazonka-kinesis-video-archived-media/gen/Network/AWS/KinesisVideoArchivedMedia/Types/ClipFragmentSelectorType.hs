{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelectorType where

import Network.AWS.Prelude

data ClipFragmentSelectorType
  = CFSTProducerTimestamp
  | CFSTServerTimestamp
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

instance FromText ClipFragmentSelectorType where
  parser =
    takeLowerText >>= \case
      "producer_timestamp" -> pure CFSTProducerTimestamp
      "server_timestamp" -> pure CFSTServerTimestamp
      e ->
        fromTextError $
          "Failure parsing ClipFragmentSelectorType from value: '" <> e
            <> "'. Accepted values: producer_timestamp, server_timestamp"

instance ToText ClipFragmentSelectorType where
  toText = \case
    CFSTProducerTimestamp -> "PRODUCER_TIMESTAMP"
    CFSTServerTimestamp -> "SERVER_TIMESTAMP"

instance Hashable ClipFragmentSelectorType

instance NFData ClipFragmentSelectorType

instance ToByteString ClipFragmentSelectorType

instance ToQuery ClipFragmentSelectorType

instance ToHeader ClipFragmentSelectorType

instance ToJSON ClipFragmentSelectorType where
  toJSON = toJSONText

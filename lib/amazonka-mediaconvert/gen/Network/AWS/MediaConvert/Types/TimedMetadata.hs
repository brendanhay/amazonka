{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimedMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimedMetadata where

import Network.AWS.Prelude

-- | Applies only to HLS outputs. Use this setting to specify whether the service inserts the ID3 timed metadata from the input in this output.
data TimedMetadata
  = TMNone
  | TMPassthrough
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

instance FromText TimedMetadata where
  parser =
    takeLowerText >>= \case
      "none" -> pure TMNone
      "passthrough" -> pure TMPassthrough
      e ->
        fromTextError $
          "Failure parsing TimedMetadata from value: '" <> e
            <> "'. Accepted values: none, passthrough"

instance ToText TimedMetadata where
  toText = \case
    TMNone -> "NONE"
    TMPassthrough -> "PASSTHROUGH"

instance Hashable TimedMetadata

instance NFData TimedMetadata

instance ToByteString TimedMetadata

instance ToQuery TimedMetadata

instance ToHeader TimedMetadata

instance ToJSON TimedMetadata where
  toJSON = toJSONText

instance FromJSON TimedMetadata where
  parseJSON = parseJSONText "TimedMetadata"

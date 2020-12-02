{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TimecodeConfigSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TimecodeConfigSource where

import Network.AWS.Prelude

-- | Timecode Config Source
data TimecodeConfigSource
  = TCSEmbedded
  | TCSSystemclock
  | TCSZerobased
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

instance FromText TimecodeConfigSource where
  parser =
    takeLowerText >>= \case
      "embedded" -> pure TCSEmbedded
      "systemclock" -> pure TCSSystemclock
      "zerobased" -> pure TCSZerobased
      e ->
        fromTextError $
          "Failure parsing TimecodeConfigSource from value: '" <> e
            <> "'. Accepted values: embedded, systemclock, zerobased"

instance ToText TimecodeConfigSource where
  toText = \case
    TCSEmbedded -> "EMBEDDED"
    TCSSystemclock -> "SYSTEMCLOCK"
    TCSZerobased -> "ZEROBASED"

instance Hashable TimecodeConfigSource

instance NFData TimecodeConfigSource

instance ToByteString TimecodeConfigSource

instance ToQuery TimecodeConfigSource

instance ToHeader TimecodeConfigSource

instance ToJSON TimecodeConfigSource where
  toJSON = toJSONText

instance FromJSON TimecodeConfigSource where
  parseJSON = parseJSONText "TimecodeConfigSource"

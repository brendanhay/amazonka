{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState where

import Network.AWS.Prelude

-- | State of Nielsen PCM to ID3 tagging
data NielsenPcmToId3TaggingState
  = NPTITSDisabled
  | NPTITSEnabled
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

instance FromText NielsenPcmToId3TaggingState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure NPTITSDisabled
      "enabled" -> pure NPTITSEnabled
      e ->
        fromTextError $
          "Failure parsing NielsenPcmToId3TaggingState from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText NielsenPcmToId3TaggingState where
  toText = \case
    NPTITSDisabled -> "DISABLED"
    NPTITSEnabled -> "ENABLED"

instance Hashable NielsenPcmToId3TaggingState

instance NFData NielsenPcmToId3TaggingState

instance ToByteString NielsenPcmToId3TaggingState

instance ToQuery NielsenPcmToId3TaggingState

instance ToHeader NielsenPcmToId3TaggingState

instance ToJSON NielsenPcmToId3TaggingState where
  toJSON = toJSONText

instance FromJSON NielsenPcmToId3TaggingState where
  parseJSON = parseJSONText "NielsenPcmToId3TaggingState"

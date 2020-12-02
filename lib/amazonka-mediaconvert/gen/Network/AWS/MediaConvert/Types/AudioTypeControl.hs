{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioTypeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioTypeControl where

import Network.AWS.Prelude

-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
data AudioTypeControl
  = ATCFollowInput
  | ATCUseConfigured
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

instance FromText AudioTypeControl where
  parser =
    takeLowerText >>= \case
      "follow_input" -> pure ATCFollowInput
      "use_configured" -> pure ATCUseConfigured
      e ->
        fromTextError $
          "Failure parsing AudioTypeControl from value: '" <> e
            <> "'. Accepted values: follow_input, use_configured"

instance ToText AudioTypeControl where
  toText = \case
    ATCFollowInput -> "FOLLOW_INPUT"
    ATCUseConfigured -> "USE_CONFIGURED"

instance Hashable AudioTypeControl

instance NFData AudioTypeControl

instance ToByteString AudioTypeControl

instance ToQuery AudioTypeControl

instance ToHeader AudioTypeControl

instance ToJSON AudioTypeControl where
  toJSON = toJSONText

instance FromJSON AudioTypeControl where
  parseJSON = parseJSONText "AudioTypeControl"

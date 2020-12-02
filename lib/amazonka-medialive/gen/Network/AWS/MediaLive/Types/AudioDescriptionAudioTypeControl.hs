{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioDescriptionAudioTypeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioDescriptionAudioTypeControl where

import Network.AWS.Prelude

-- | Audio Description Audio Type Control
data AudioDescriptionAudioTypeControl
  = ADATCFollowInput
  | ADATCUseConfigured
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

instance FromText AudioDescriptionAudioTypeControl where
  parser =
    takeLowerText >>= \case
      "follow_input" -> pure ADATCFollowInput
      "use_configured" -> pure ADATCUseConfigured
      e ->
        fromTextError $
          "Failure parsing AudioDescriptionAudioTypeControl from value: '" <> e
            <> "'. Accepted values: follow_input, use_configured"

instance ToText AudioDescriptionAudioTypeControl where
  toText = \case
    ADATCFollowInput -> "FOLLOW_INPUT"
    ADATCUseConfigured -> "USE_CONFIGURED"

instance Hashable AudioDescriptionAudioTypeControl

instance NFData AudioDescriptionAudioTypeControl

instance ToByteString AudioDescriptionAudioTypeControl

instance ToQuery AudioDescriptionAudioTypeControl

instance ToHeader AudioDescriptionAudioTypeControl

instance ToJSON AudioDescriptionAudioTypeControl where
  toJSON = toJSONText

instance FromJSON AudioDescriptionAudioTypeControl where
  parseJSON = parseJSONText "AudioDescriptionAudioTypeControl"

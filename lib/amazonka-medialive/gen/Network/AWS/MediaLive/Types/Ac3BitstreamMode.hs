{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3BitstreamMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3BitstreamMode where

import Network.AWS.Prelude

-- | Ac3 Bitstream Mode
data Ac3BitstreamMode
  = ABMCommentary
  | ABMCompleteMain
  | ABMDialogue
  | ABMEmergency
  | ABMHearingImpaired
  | ABMMusicAndEffects
  | ABMVisuallyImpaired
  | ABMVoiceOver
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

instance FromText Ac3BitstreamMode where
  parser =
    takeLowerText >>= \case
      "commentary" -> pure ABMCommentary
      "complete_main" -> pure ABMCompleteMain
      "dialogue" -> pure ABMDialogue
      "emergency" -> pure ABMEmergency
      "hearing_impaired" -> pure ABMHearingImpaired
      "music_and_effects" -> pure ABMMusicAndEffects
      "visually_impaired" -> pure ABMVisuallyImpaired
      "voice_over" -> pure ABMVoiceOver
      e ->
        fromTextError $
          "Failure parsing Ac3BitstreamMode from value: '" <> e
            <> "'. Accepted values: commentary, complete_main, dialogue, emergency, hearing_impaired, music_and_effects, visually_impaired, voice_over"

instance ToText Ac3BitstreamMode where
  toText = \case
    ABMCommentary -> "COMMENTARY"
    ABMCompleteMain -> "COMPLETE_MAIN"
    ABMDialogue -> "DIALOGUE"
    ABMEmergency -> "EMERGENCY"
    ABMHearingImpaired -> "HEARING_IMPAIRED"
    ABMMusicAndEffects -> "MUSIC_AND_EFFECTS"
    ABMVisuallyImpaired -> "VISUALLY_IMPAIRED"
    ABMVoiceOver -> "VOICE_OVER"

instance Hashable Ac3BitstreamMode

instance NFData Ac3BitstreamMode

instance ToByteString Ac3BitstreamMode

instance ToQuery Ac3BitstreamMode

instance ToHeader Ac3BitstreamMode

instance ToJSON Ac3BitstreamMode where
  toJSON = toJSONText

instance FromJSON Ac3BitstreamMode where
  parseJSON = parseJSONText "Ac3BitstreamMode"

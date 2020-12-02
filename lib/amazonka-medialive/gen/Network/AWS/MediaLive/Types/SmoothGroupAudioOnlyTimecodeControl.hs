{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl where

import Network.AWS.Prelude

-- | Smooth Group Audio Only Timecode Control
data SmoothGroupAudioOnlyTimecodeControl
  = SGAOTCPassthrough
  | SGAOTCUseConfiguredClock
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

instance FromText SmoothGroupAudioOnlyTimecodeControl where
  parser =
    takeLowerText >>= \case
      "passthrough" -> pure SGAOTCPassthrough
      "use_configured_clock" -> pure SGAOTCUseConfiguredClock
      e ->
        fromTextError $
          "Failure parsing SmoothGroupAudioOnlyTimecodeControl from value: '" <> e
            <> "'. Accepted values: passthrough, use_configured_clock"

instance ToText SmoothGroupAudioOnlyTimecodeControl where
  toText = \case
    SGAOTCPassthrough -> "PASSTHROUGH"
    SGAOTCUseConfiguredClock -> "USE_CONFIGURED_CLOCK"

instance Hashable SmoothGroupAudioOnlyTimecodeControl

instance NFData SmoothGroupAudioOnlyTimecodeControl

instance ToByteString SmoothGroupAudioOnlyTimecodeControl

instance ToQuery SmoothGroupAudioOnlyTimecodeControl

instance ToHeader SmoothGroupAudioOnlyTimecodeControl

instance ToJSON SmoothGroupAudioOnlyTimecodeControl where
  toJSON = toJSONText

instance FromJSON SmoothGroupAudioOnlyTimecodeControl where
  parseJSON = parseJSONText "SmoothGroupAudioOnlyTimecodeControl"

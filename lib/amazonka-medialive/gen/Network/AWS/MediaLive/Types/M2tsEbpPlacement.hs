{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsEbpPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsEbpPlacement where

import Network.AWS.Prelude

-- | M2ts Ebp Placement
data M2tsEbpPlacement
  = VideoAndAudioPids
  | VideoPid
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

instance FromText M2tsEbpPlacement where
  parser =
    takeLowerText >>= \case
      "video_and_audio_pids" -> pure VideoAndAudioPids
      "video_pid" -> pure VideoPid
      e ->
        fromTextError $
          "Failure parsing M2tsEbpPlacement from value: '" <> e
            <> "'. Accepted values: video_and_audio_pids, video_pid"

instance ToText M2tsEbpPlacement where
  toText = \case
    VideoAndAudioPids -> "VIDEO_AND_AUDIO_PIDS"
    VideoPid -> "VIDEO_PID"

instance Hashable M2tsEbpPlacement

instance NFData M2tsEbpPlacement

instance ToByteString M2tsEbpPlacement

instance ToQuery M2tsEbpPlacement

instance ToHeader M2tsEbpPlacement

instance ToJSON M2tsEbpPlacement where
  toJSON = toJSONText

instance FromJSON M2tsEbpPlacement where
  parseJSON = parseJSONText "M2tsEbpPlacement"

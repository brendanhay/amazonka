{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdAudioDuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdAudioDuration where

import Network.AWS.Prelude

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
data MpdAudioDuration
  = MDefaultCodecDuration
  | MMatchVideoDuration
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

instance FromText MpdAudioDuration where
  parser =
    takeLowerText >>= \case
      "default_codec_duration" -> pure MDefaultCodecDuration
      "match_video_duration" -> pure MMatchVideoDuration
      e ->
        fromTextError $
          "Failure parsing MpdAudioDuration from value: '" <> e
            <> "'. Accepted values: default_codec_duration, match_video_duration"

instance ToText MpdAudioDuration where
  toText = \case
    MDefaultCodecDuration -> "DEFAULT_CODEC_DURATION"
    MMatchVideoDuration -> "MATCH_VIDEO_DURATION"

instance Hashable MpdAudioDuration

instance NFData MpdAudioDuration

instance ToByteString MpdAudioDuration

instance ToQuery MpdAudioDuration

instance ToHeader MpdAudioDuration

instance ToJSON MpdAudioDuration where
  toJSON = toJSONText

instance FromJSON MpdAudioDuration where
  parseJSON = parseJSONText "MpdAudioDuration"

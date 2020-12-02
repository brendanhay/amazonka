{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8AudioDuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M3u8AudioDuration where

import Network.AWS.Prelude

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
data M3u8AudioDuration
  = MADDefaultCodecDuration
  | MADMatchVideoDuration
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

instance FromText M3u8AudioDuration where
  parser =
    takeLowerText >>= \case
      "default_codec_duration" -> pure MADDefaultCodecDuration
      "match_video_duration" -> pure MADMatchVideoDuration
      e ->
        fromTextError $
          "Failure parsing M3u8AudioDuration from value: '" <> e
            <> "'. Accepted values: default_codec_duration, match_video_duration"

instance ToText M3u8AudioDuration where
  toText = \case
    MADDefaultCodecDuration -> "DEFAULT_CODEC_DURATION"
    MADMatchVideoDuration -> "MATCH_VIDEO_DURATION"

instance Hashable M3u8AudioDuration

instance NFData M3u8AudioDuration

instance ToByteString M3u8AudioDuration

instance ToQuery M3u8AudioDuration

instance ToHeader M3u8AudioDuration

instance ToJSON M3u8AudioDuration where
  toJSON = toJSONText

instance FromJSON M3u8AudioDuration where
  parseJSON = parseJSONText "M3u8AudioDuration"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmfcAudioDuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmfcAudioDuration where

import Network.AWS.Prelude

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
data CmfcAudioDuration
  = CADDefaultCodecDuration
  | CADMatchVideoDuration
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

instance FromText CmfcAudioDuration where
  parser =
    takeLowerText >>= \case
      "default_codec_duration" -> pure CADDefaultCodecDuration
      "match_video_duration" -> pure CADMatchVideoDuration
      e ->
        fromTextError $
          "Failure parsing CmfcAudioDuration from value: '" <> e
            <> "'. Accepted values: default_codec_duration, match_video_duration"

instance ToText CmfcAudioDuration where
  toText = \case
    CADDefaultCodecDuration -> "DEFAULT_CODEC_DURATION"
    CADMatchVideoDuration -> "MATCH_VIDEO_DURATION"

instance Hashable CmfcAudioDuration

instance NFData CmfcAudioDuration

instance ToByteString CmfcAudioDuration

instance ToQuery CmfcAudioDuration

instance ToHeader CmfcAudioDuration

instance ToJSON CmfcAudioDuration where
  toJSON = toJSONText

instance FromJSON CmfcAudioDuration where
  parseJSON = parseJSONText "CmfcAudioDuration"

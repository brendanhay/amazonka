{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsEbpAudioInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsEbpAudioInterval where

import Network.AWS.Prelude

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
data M2tsEbpAudioInterval
  = VideoAndFixedIntervals
  | VideoInterval
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

instance FromText M2tsEbpAudioInterval where
  parser =
    takeLowerText >>= \case
      "video_and_fixed_intervals" -> pure VideoAndFixedIntervals
      "video_interval" -> pure VideoInterval
      e ->
        fromTextError $
          "Failure parsing M2tsEbpAudioInterval from value: '" <> e
            <> "'. Accepted values: video_and_fixed_intervals, video_interval"

instance ToText M2tsEbpAudioInterval where
  toText = \case
    VideoAndFixedIntervals -> "VIDEO_AND_FIXED_INTERVALS"
    VideoInterval -> "VIDEO_INTERVAL"

instance Hashable M2tsEbpAudioInterval

instance NFData M2tsEbpAudioInterval

instance ToByteString M2tsEbpAudioInterval

instance ToQuery M2tsEbpAudioInterval

instance ToHeader M2tsEbpAudioInterval

instance ToJSON M2tsEbpAudioInterval where
  toJSON = toJSONText

instance FromJSON M2tsEbpAudioInterval where
  parseJSON = parseJSONText "M2tsEbpAudioInterval"

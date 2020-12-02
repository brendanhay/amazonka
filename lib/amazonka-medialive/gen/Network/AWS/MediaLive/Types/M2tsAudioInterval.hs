{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAudioInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAudioInterval where

import Network.AWS.Prelude

-- | M2ts Audio Interval
data M2tsAudioInterval
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

instance FromText M2tsAudioInterval where
  parser =
    takeLowerText >>= \case
      "video_and_fixed_intervals" -> pure VideoAndFixedIntervals
      "video_interval" -> pure VideoInterval
      e ->
        fromTextError $
          "Failure parsing M2tsAudioInterval from value: '" <> e
            <> "'. Accepted values: video_and_fixed_intervals, video_interval"

instance ToText M2tsAudioInterval where
  toText = \case
    VideoAndFixedIntervals -> "VIDEO_AND_FIXED_INTERVALS"
    VideoInterval -> "VIDEO_INTERVAL"

instance Hashable M2tsAudioInterval

instance NFData M2tsAudioInterval

instance ToByteString M2tsAudioInterval

instance ToQuery M2tsAudioInterval

instance ToHeader M2tsAudioInterval

instance ToJSON M2tsAudioInterval where
  toJSON = toJSONText

instance FromJSON M2tsAudioInterval where
  parseJSON = parseJSONText "M2tsAudioInterval"

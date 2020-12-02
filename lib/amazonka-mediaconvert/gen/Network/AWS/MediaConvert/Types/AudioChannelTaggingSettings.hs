{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AudioChannelTag
import Network.AWS.Prelude

-- | When you mimic a multi-channel audio layout with multiple mono-channel tracks, you can tag each channel layout manually. For example, you would tag the tracks that contain your left, right, and center audio with Left (L), Right (R), and Center (C), respectively. When you don't specify a value, MediaConvert labels your track as Center (C) by default. To use audio layout tagging, your output must be in a QuickTime (.mov) container; your audio codec must be AAC, WAV, or AIFF; and you must set up your audio track to have only one channel.
--
-- /See:/ 'audioChannelTaggingSettings' smart constructor.
newtype AudioChannelTaggingSettings = AudioChannelTaggingSettings'
  { _actsChannelTag ::
      Maybe AudioChannelTag
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioChannelTaggingSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'actsChannelTag' - You can add a tag for this mono-channel audio track to mimic its placement in a multi-channel layout.  For example, if this track is the left surround channel, choose Left surround (LS).
audioChannelTaggingSettings ::
  AudioChannelTaggingSettings
audioChannelTaggingSettings =
  AudioChannelTaggingSettings' {_actsChannelTag = Nothing}

-- | You can add a tag for this mono-channel audio track to mimic its placement in a multi-channel layout.  For example, if this track is the left surround channel, choose Left surround (LS).
actsChannelTag :: Lens' AudioChannelTaggingSettings (Maybe AudioChannelTag)
actsChannelTag = lens _actsChannelTag (\s a -> s {_actsChannelTag = a})

instance FromJSON AudioChannelTaggingSettings where
  parseJSON =
    withObject
      "AudioChannelTaggingSettings"
      (\x -> AudioChannelTaggingSettings' <$> (x .:? "channelTag"))

instance Hashable AudioChannelTaggingSettings

instance NFData AudioChannelTaggingSettings

instance ToJSON AudioChannelTaggingSettings where
  toJSON AudioChannelTaggingSettings' {..} =
    object (catMaybes [("channelTag" .=) <$> _actsChannelTag])

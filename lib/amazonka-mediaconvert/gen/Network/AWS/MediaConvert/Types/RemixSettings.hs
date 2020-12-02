{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.RemixSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.RemixSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.ChannelMapping
import Network.AWS.Prelude

-- | Use Manual audio remixing (RemixSettings) to adjust audio levels for each audio channel in each output of your job. With audio remixing, you can output more or fewer audio channels than your input audio source provides.
--
-- /See:/ 'remixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { _rsChannelMapping ::
      !(Maybe ChannelMapping),
    _rsChannelsIn :: !(Maybe Nat),
    _rsChannelsOut :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemixSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsChannelMapping' - Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
--
-- * 'rsChannelsIn' - Specify the number of audio channels from your input that you want to use in your output. With remixing, you might combine or split the data in these channels, so the number of channels in your final output might be different.
--
-- * 'rsChannelsOut' - Specify the number of channels in this output after remixing. Valid values: 1, 2, 4, 6, 8... 64. (1 and even numbers to 64.)
remixSettings ::
  RemixSettings
remixSettings =
  RemixSettings'
    { _rsChannelMapping = Nothing,
      _rsChannelsIn = Nothing,
      _rsChannelsOut = Nothing
    }

-- | Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
rsChannelMapping :: Lens' RemixSettings (Maybe ChannelMapping)
rsChannelMapping = lens _rsChannelMapping (\s a -> s {_rsChannelMapping = a})

-- | Specify the number of audio channels from your input that you want to use in your output. With remixing, you might combine or split the data in these channels, so the number of channels in your final output might be different.
rsChannelsIn :: Lens' RemixSettings (Maybe Natural)
rsChannelsIn = lens _rsChannelsIn (\s a -> s {_rsChannelsIn = a}) . mapping _Nat

-- | Specify the number of channels in this output after remixing. Valid values: 1, 2, 4, 6, 8... 64. (1 and even numbers to 64.)
rsChannelsOut :: Lens' RemixSettings (Maybe Natural)
rsChannelsOut = lens _rsChannelsOut (\s a -> s {_rsChannelsOut = a}) . mapping _Nat

instance FromJSON RemixSettings where
  parseJSON =
    withObject
      "RemixSettings"
      ( \x ->
          RemixSettings'
            <$> (x .:? "channelMapping")
            <*> (x .:? "channelsIn")
            <*> (x .:? "channelsOut")
      )

instance Hashable RemixSettings

instance NFData RemixSettings

instance ToJSON RemixSettings where
  toJSON RemixSettings' {..} =
    object
      ( catMaybes
          [ ("channelMapping" .=) <$> _rsChannelMapping,
            ("channelsIn" .=) <$> _rsChannelsIn,
            ("channelsOut" .=) <$> _rsChannelsOut
          ]
      )

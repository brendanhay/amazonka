{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp2Settings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
--
-- /See:/ 'mp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'
  { _mChannels :: !(Maybe Nat),
    _mSampleRate :: !(Maybe Nat),
    _mBitrate :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Mp2Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mChannels' - Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
--
-- * 'mSampleRate' - Sample rate in hz.
--
-- * 'mBitrate' - Specify the average bitrate in bits per second.
mp2Settings ::
  Mp2Settings
mp2Settings =
  Mp2Settings'
    { _mChannels = Nothing,
      _mSampleRate = Nothing,
      _mBitrate = Nothing
    }

-- | Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
mChannels :: Lens' Mp2Settings (Maybe Natural)
mChannels = lens _mChannels (\s a -> s {_mChannels = a}) . mapping _Nat

-- | Sample rate in hz.
mSampleRate :: Lens' Mp2Settings (Maybe Natural)
mSampleRate = lens _mSampleRate (\s a -> s {_mSampleRate = a}) . mapping _Nat

-- | Specify the average bitrate in bits per second.
mBitrate :: Lens' Mp2Settings (Maybe Natural)
mBitrate = lens _mBitrate (\s a -> s {_mBitrate = a}) . mapping _Nat

instance FromJSON Mp2Settings where
  parseJSON =
    withObject
      "Mp2Settings"
      ( \x ->
          Mp2Settings'
            <$> (x .:? "channels") <*> (x .:? "sampleRate") <*> (x .:? "bitrate")
      )

instance Hashable Mp2Settings

instance NFData Mp2Settings

instance ToJSON Mp2Settings where
  toJSON Mp2Settings' {..} =
    object
      ( catMaybes
          [ ("channels" .=) <$> _mChannels,
            ("sampleRate" .=) <$> _mSampleRate,
            ("bitrate" .=) <$> _mBitrate
          ]
      )

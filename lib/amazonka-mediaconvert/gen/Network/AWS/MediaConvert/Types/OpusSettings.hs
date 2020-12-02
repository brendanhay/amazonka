{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OpusSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OpusSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value OPUS.
--
-- /See:/ 'opusSettings' smart constructor.
data OpusSettings = OpusSettings'
  { _osChannels :: !(Maybe Nat),
    _osSampleRate :: !(Maybe Nat),
    _osBitrate :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpusSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osChannels' - Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
--
-- * 'osSampleRate' - Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000. The default value is 48000.
--
-- * 'osBitrate' - Optional. Specify the average bitrate in bits per second. Valid values are multiples of 8000, from 32000 through 192000. The default value is 96000, which we recommend for quality and bandwidth.
opusSettings ::
  OpusSettings
opusSettings =
  OpusSettings'
    { _osChannels = Nothing,
      _osSampleRate = Nothing,
      _osBitrate = Nothing
    }

-- | Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
osChannels :: Lens' OpusSettings (Maybe Natural)
osChannels = lens _osChannels (\s a -> s {_osChannels = a}) . mapping _Nat

-- | Optional. Sample rate in hz. Valid values are 16000, 24000, and 48000. The default value is 48000.
osSampleRate :: Lens' OpusSettings (Maybe Natural)
osSampleRate = lens _osSampleRate (\s a -> s {_osSampleRate = a}) . mapping _Nat

-- | Optional. Specify the average bitrate in bits per second. Valid values are multiples of 8000, from 32000 through 192000. The default value is 96000, which we recommend for quality and bandwidth.
osBitrate :: Lens' OpusSettings (Maybe Natural)
osBitrate = lens _osBitrate (\s a -> s {_osBitrate = a}) . mapping _Nat

instance FromJSON OpusSettings where
  parseJSON =
    withObject
      "OpusSettings"
      ( \x ->
          OpusSettings'
            <$> (x .:? "channels") <*> (x .:? "sampleRate") <*> (x .:? "bitrate")
      )

instance Hashable OpusSettings

instance NFData OpusSettings

instance ToJSON OpusSettings where
  toJSON OpusSettings' {..} =
    object
      ( catMaybes
          [ ("channels" .=) <$> _osChannels,
            ("sampleRate" .=) <$> _osSampleRate,
            ("bitrate" .=) <$> _osBitrate
          ]
      )

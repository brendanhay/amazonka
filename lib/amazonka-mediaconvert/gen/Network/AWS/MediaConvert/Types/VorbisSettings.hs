{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VorbisSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VorbisSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value Vorbis.
--
-- /See:/ 'vorbisSettings' smart constructor.
data VorbisSettings = VorbisSettings'
  { _vsChannels :: !(Maybe Nat),
    _vsSampleRate :: !(Maybe Nat),
    _vsVbrQuality :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VorbisSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsChannels' - Optional. Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2. The default value is 2.
--
-- * 'vsSampleRate' - Optional. Specify the audio sample rate in Hz. Valid values are 22050, 32000, 44100, and 48000. The default value is 48000.
--
-- * 'vsVbrQuality' - Optional. Specify the variable audio quality of this Vorbis output from -1 (lowest quality, ~45 kbit/s) to 10 (highest quality, ~500 kbit/s). The default value is 4 (~128 kbit/s). Values 5 and 6 are approximately 160 and 192 kbit/s, respectively.
vorbisSettings ::
  VorbisSettings
vorbisSettings =
  VorbisSettings'
    { _vsChannels = Nothing,
      _vsSampleRate = Nothing,
      _vsVbrQuality = Nothing
    }

-- | Optional. Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2. The default value is 2.
vsChannels :: Lens' VorbisSettings (Maybe Natural)
vsChannels = lens _vsChannels (\s a -> s {_vsChannels = a}) . mapping _Nat

-- | Optional. Specify the audio sample rate in Hz. Valid values are 22050, 32000, 44100, and 48000. The default value is 48000.
vsSampleRate :: Lens' VorbisSettings (Maybe Natural)
vsSampleRate = lens _vsSampleRate (\s a -> s {_vsSampleRate = a}) . mapping _Nat

-- | Optional. Specify the variable audio quality of this Vorbis output from -1 (lowest quality, ~45 kbit/s) to 10 (highest quality, ~500 kbit/s). The default value is 4 (~128 kbit/s). Values 5 and 6 are approximately 160 and 192 kbit/s, respectively.
vsVbrQuality :: Lens' VorbisSettings (Maybe Int)
vsVbrQuality = lens _vsVbrQuality (\s a -> s {_vsVbrQuality = a})

instance FromJSON VorbisSettings where
  parseJSON =
    withObject
      "VorbisSettings"
      ( \x ->
          VorbisSettings'
            <$> (x .:? "channels")
            <*> (x .:? "sampleRate")
            <*> (x .:? "vbrQuality")
      )

instance Hashable VorbisSettings

instance NFData VorbisSettings

instance ToJSON VorbisSettings where
  toJSON VorbisSettings' {..} =
    object
      ( catMaybes
          [ ("channels" .=) <$> _vsChannels,
            ("sampleRate" .=) <$> _vsSampleRate,
            ("vbrQuality" .=) <$> _vsVbrQuality
          ]
      )

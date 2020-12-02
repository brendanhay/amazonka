{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp3Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Mp3RateControlMode
import Network.AWS.Prelude

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value MP3.
--
-- /See:/ 'mp3Settings' smart constructor.
data Mp3Settings = Mp3Settings'
  { _mp3Channels :: !(Maybe Nat),
    _mp3RateControlMode :: !(Maybe Mp3RateControlMode),
    _mp3SampleRate :: !(Maybe Nat),
    _mp3Bitrate :: !(Maybe Nat),
    _mp3VbrQuality :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Mp3Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mp3Channels' - Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
--
-- * 'mp3RateControlMode' - Specify whether the service encodes this MP3 audio output with a constant bitrate (CBR) or a variable bitrate (VBR).
--
-- * 'mp3SampleRate' - Sample rate in hz.
--
-- * 'mp3Bitrate' - Specify the average bitrate in bits per second.
--
-- * 'mp3VbrQuality' - Required when you set Bitrate control mode (rateControlMode) to VBR. Specify the audio quality of this MP3 output from 0 (highest quality) to 9 (lowest quality).
mp3Settings ::
  Mp3Settings
mp3Settings =
  Mp3Settings'
    { _mp3Channels = Nothing,
      _mp3RateControlMode = Nothing,
      _mp3SampleRate = Nothing,
      _mp3Bitrate = Nothing,
      _mp3VbrQuality = Nothing
    }

-- | Specify the number of channels in this output audio track. Choosing Mono on the console gives you 1 output channel; choosing Stereo gives you 2. In the API, valid values are 1 and 2.
mp3Channels :: Lens' Mp3Settings (Maybe Natural)
mp3Channels = lens _mp3Channels (\s a -> s {_mp3Channels = a}) . mapping _Nat

-- | Specify whether the service encodes this MP3 audio output with a constant bitrate (CBR) or a variable bitrate (VBR).
mp3RateControlMode :: Lens' Mp3Settings (Maybe Mp3RateControlMode)
mp3RateControlMode = lens _mp3RateControlMode (\s a -> s {_mp3RateControlMode = a})

-- | Sample rate in hz.
mp3SampleRate :: Lens' Mp3Settings (Maybe Natural)
mp3SampleRate = lens _mp3SampleRate (\s a -> s {_mp3SampleRate = a}) . mapping _Nat

-- | Specify the average bitrate in bits per second.
mp3Bitrate :: Lens' Mp3Settings (Maybe Natural)
mp3Bitrate = lens _mp3Bitrate (\s a -> s {_mp3Bitrate = a}) . mapping _Nat

-- | Required when you set Bitrate control mode (rateControlMode) to VBR. Specify the audio quality of this MP3 output from 0 (highest quality) to 9 (lowest quality).
mp3VbrQuality :: Lens' Mp3Settings (Maybe Natural)
mp3VbrQuality = lens _mp3VbrQuality (\s a -> s {_mp3VbrQuality = a}) . mapping _Nat

instance FromJSON Mp3Settings where
  parseJSON =
    withObject
      "Mp3Settings"
      ( \x ->
          Mp3Settings'
            <$> (x .:? "channels")
            <*> (x .:? "rateControlMode")
            <*> (x .:? "sampleRate")
            <*> (x .:? "bitrate")
            <*> (x .:? "vbrQuality")
      )

instance Hashable Mp3Settings

instance NFData Mp3Settings

instance ToJSON Mp3Settings where
  toJSON Mp3Settings' {..} =
    object
      ( catMaybes
          [ ("channels" .=) <$> _mp3Channels,
            ("rateControlMode" .=) <$> _mp3RateControlMode,
            ("sampleRate" .=) <$> _mp3SampleRate,
            ("bitrate" .=) <$> _mp3Bitrate,
            ("vbrQuality" .=) <$> _mp3VbrQuality
          ]
      )

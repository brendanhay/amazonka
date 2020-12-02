{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.WavSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WavSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.WavCodingMode
import Network.AWS.Prelude

-- | Wav Settings
--
-- /See:/ 'wavSettings' smart constructor.
data WavSettings = WavSettings'
  { _wsBitDepth :: !(Maybe Double),
    _wsCodingMode :: !(Maybe WavCodingMode),
    _wsSampleRate :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WavSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wsBitDepth' - Bits per sample.
--
-- * 'wsCodingMode' - The audio coding mode for the WAV audio. The mode determines the number of channels in the audio.
--
-- * 'wsSampleRate' - Sample rate in Hz.
wavSettings ::
  WavSettings
wavSettings =
  WavSettings'
    { _wsBitDepth = Nothing,
      _wsCodingMode = Nothing,
      _wsSampleRate = Nothing
    }

-- | Bits per sample.
wsBitDepth :: Lens' WavSettings (Maybe Double)
wsBitDepth = lens _wsBitDepth (\s a -> s {_wsBitDepth = a})

-- | The audio coding mode for the WAV audio. The mode determines the number of channels in the audio.
wsCodingMode :: Lens' WavSettings (Maybe WavCodingMode)
wsCodingMode = lens _wsCodingMode (\s a -> s {_wsCodingMode = a})

-- | Sample rate in Hz.
wsSampleRate :: Lens' WavSettings (Maybe Double)
wsSampleRate = lens _wsSampleRate (\s a -> s {_wsSampleRate = a})

instance FromJSON WavSettings where
  parseJSON =
    withObject
      "WavSettings"
      ( \x ->
          WavSettings'
            <$> (x .:? "bitDepth")
            <*> (x .:? "codingMode")
            <*> (x .:? "sampleRate")
      )

instance Hashable WavSettings

instance NFData WavSettings

instance ToJSON WavSettings where
  toJSON WavSettings' {..} =
    object
      ( catMaybes
          [ ("bitDepth" .=) <$> _wsBitDepth,
            ("codingMode" .=) <$> _wsCodingMode,
            ("sampleRate" .=) <$> _wsSampleRate
          ]
      )

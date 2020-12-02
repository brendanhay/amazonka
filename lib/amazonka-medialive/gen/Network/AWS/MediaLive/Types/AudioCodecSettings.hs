{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioCodecSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AacSettings
import Network.AWS.MediaLive.Types.Ac3Settings
import Network.AWS.MediaLive.Types.Eac3Settings
import Network.AWS.MediaLive.Types.Mp2Settings
import Network.AWS.MediaLive.Types.PassThroughSettings
import Network.AWS.MediaLive.Types.WavSettings
import Network.AWS.Prelude

-- | Audio Codec Settings
--
-- /See:/ 'audioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { _acsPassThroughSettings ::
      !(Maybe PassThroughSettings),
    _acsAc3Settings :: !(Maybe Ac3Settings),
    _acsMp2Settings :: !(Maybe Mp2Settings),
    _acsWavSettings :: !(Maybe WavSettings),
    _acsAacSettings :: !(Maybe AacSettings),
    _acsEac3Settings :: !(Maybe Eac3Settings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioCodecSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsPassThroughSettings' - Undocumented member.
--
-- * 'acsAc3Settings' - Undocumented member.
--
-- * 'acsMp2Settings' - Undocumented member.
--
-- * 'acsWavSettings' - Undocumented member.
--
-- * 'acsAacSettings' - Undocumented member.
--
-- * 'acsEac3Settings' - Undocumented member.
audioCodecSettings ::
  AudioCodecSettings
audioCodecSettings =
  AudioCodecSettings'
    { _acsPassThroughSettings = Nothing,
      _acsAc3Settings = Nothing,
      _acsMp2Settings = Nothing,
      _acsWavSettings = Nothing,
      _acsAacSettings = Nothing,
      _acsEac3Settings = Nothing
    }

-- | Undocumented member.
acsPassThroughSettings :: Lens' AudioCodecSettings (Maybe PassThroughSettings)
acsPassThroughSettings = lens _acsPassThroughSettings (\s a -> s {_acsPassThroughSettings = a})

-- | Undocumented member.
acsAc3Settings :: Lens' AudioCodecSettings (Maybe Ac3Settings)
acsAc3Settings = lens _acsAc3Settings (\s a -> s {_acsAc3Settings = a})

-- | Undocumented member.
acsMp2Settings :: Lens' AudioCodecSettings (Maybe Mp2Settings)
acsMp2Settings = lens _acsMp2Settings (\s a -> s {_acsMp2Settings = a})

-- | Undocumented member.
acsWavSettings :: Lens' AudioCodecSettings (Maybe WavSettings)
acsWavSettings = lens _acsWavSettings (\s a -> s {_acsWavSettings = a})

-- | Undocumented member.
acsAacSettings :: Lens' AudioCodecSettings (Maybe AacSettings)
acsAacSettings = lens _acsAacSettings (\s a -> s {_acsAacSettings = a})

-- | Undocumented member.
acsEac3Settings :: Lens' AudioCodecSettings (Maybe Eac3Settings)
acsEac3Settings = lens _acsEac3Settings (\s a -> s {_acsEac3Settings = a})

instance FromJSON AudioCodecSettings where
  parseJSON =
    withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            <$> (x .:? "passThroughSettings")
            <*> (x .:? "ac3Settings")
            <*> (x .:? "mp2Settings")
            <*> (x .:? "wavSettings")
            <*> (x .:? "aacSettings")
            <*> (x .:? "eac3Settings")
      )

instance Hashable AudioCodecSettings

instance NFData AudioCodecSettings

instance ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    object
      ( catMaybes
          [ ("passThroughSettings" .=) <$> _acsPassThroughSettings,
            ("ac3Settings" .=) <$> _acsAc3Settings,
            ("mp2Settings" .=) <$> _acsMp2Settings,
            ("wavSettings" .=) <$> _acsWavSettings,
            ("aacSettings" .=) <$> _acsAacSettings,
            ("eac3Settings" .=) <$> _acsEac3Settings
          ]
      )

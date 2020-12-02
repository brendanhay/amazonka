{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mp2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mp2Settings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Mp2CodingMode
import Network.AWS.Prelude

-- | Mp2 Settings
--
-- /See:/ 'mp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'
  { _msCodingMode ::
      !(Maybe Mp2CodingMode),
    _msSampleRate :: !(Maybe Double),
    _msBitrate :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Mp2Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msCodingMode' - The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
--
-- * 'msSampleRate' - Sample rate in Hz.
--
-- * 'msBitrate' - Average bitrate in bits/second.
mp2Settings ::
  Mp2Settings
mp2Settings =
  Mp2Settings'
    { _msCodingMode = Nothing,
      _msSampleRate = Nothing,
      _msBitrate = Nothing
    }

-- | The MPEG2 Audio coding mode.  Valid values are codingMode10 (for mono) or codingMode20 (for stereo).
msCodingMode :: Lens' Mp2Settings (Maybe Mp2CodingMode)
msCodingMode = lens _msCodingMode (\s a -> s {_msCodingMode = a})

-- | Sample rate in Hz.
msSampleRate :: Lens' Mp2Settings (Maybe Double)
msSampleRate = lens _msSampleRate (\s a -> s {_msSampleRate = a})

-- | Average bitrate in bits/second.
msBitrate :: Lens' Mp2Settings (Maybe Double)
msBitrate = lens _msBitrate (\s a -> s {_msBitrate = a})

instance FromJSON Mp2Settings where
  parseJSON =
    withObject
      "Mp2Settings"
      ( \x ->
          Mp2Settings'
            <$> (x .:? "codingMode")
            <*> (x .:? "sampleRate")
            <*> (x .:? "bitrate")
      )

instance Hashable Mp2Settings

instance NFData Mp2Settings

instance ToJSON Mp2Settings where
  toJSON Mp2Settings' {..} =
    object
      ( catMaybes
          [ ("codingMode" .=) <$> _msCodingMode,
            ("sampleRate" .=) <$> _msSampleRate,
            ("bitrate" .=) <$> _msBitrate
          ]
      )

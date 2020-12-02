{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StandardHlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StandardHlsSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.M3u8Settings
import Network.AWS.Prelude

-- | Standard Hls Settings
--
-- /See:/ 'standardHlsSettings' smart constructor.
data StandardHlsSettings = StandardHlsSettings'
  { _shsAudioRenditionSets ::
      !(Maybe Text),
    _shsM3u8Settings :: !M3u8Settings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StandardHlsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'shsAudioRenditionSets' - List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- * 'shsM3u8Settings' - Undocumented member.
standardHlsSettings ::
  -- | 'shsM3u8Settings'
  M3u8Settings ->
  StandardHlsSettings
standardHlsSettings pM3u8Settings_ =
  StandardHlsSettings'
    { _shsAudioRenditionSets = Nothing,
      _shsM3u8Settings = pM3u8Settings_
    }

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
shsAudioRenditionSets :: Lens' StandardHlsSettings (Maybe Text)
shsAudioRenditionSets = lens _shsAudioRenditionSets (\s a -> s {_shsAudioRenditionSets = a})

-- | Undocumented member.
shsM3u8Settings :: Lens' StandardHlsSettings M3u8Settings
shsM3u8Settings = lens _shsM3u8Settings (\s a -> s {_shsM3u8Settings = a})

instance FromJSON StandardHlsSettings where
  parseJSON =
    withObject
      "StandardHlsSettings"
      ( \x ->
          StandardHlsSettings'
            <$> (x .:? "audioRenditionSets") <*> (x .: "m3u8Settings")
      )

instance Hashable StandardHlsSettings

instance NFData StandardHlsSettings

instance ToJSON StandardHlsSettings where
  toJSON StandardHlsSettings' {..} =
    object
      ( catMaybes
          [ ("audioRenditionSets" .=) <$> _shsAudioRenditionSets,
            Just ("m3u8Settings" .= _shsM3u8Settings)
          ]
      )

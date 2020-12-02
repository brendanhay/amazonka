{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsCdnSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsCdnSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.HlsAkamaiSettings
import Network.AWS.MediaLive.Types.HlsBasicPutSettings
import Network.AWS.MediaLive.Types.HlsMediaStoreSettings
import Network.AWS.MediaLive.Types.HlsWebdavSettings
import Network.AWS.Prelude

-- | Hls Cdn Settings
--
-- /See:/ 'hlsCdnSettings' smart constructor.
data HlsCdnSettings = HlsCdnSettings'
  { _hcsHlsAkamaiSettings ::
      !(Maybe HlsAkamaiSettings),
    _hcsHlsMediaStoreSettings :: !(Maybe HlsMediaStoreSettings),
    _hcsHlsBasicPutSettings :: !(Maybe HlsBasicPutSettings),
    _hcsHlsWebdavSettings :: !(Maybe HlsWebdavSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsCdnSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcsHlsAkamaiSettings' - Undocumented member.
--
-- * 'hcsHlsMediaStoreSettings' - Undocumented member.
--
-- * 'hcsHlsBasicPutSettings' - Undocumented member.
--
-- * 'hcsHlsWebdavSettings' - Undocumented member.
hlsCdnSettings ::
  HlsCdnSettings
hlsCdnSettings =
  HlsCdnSettings'
    { _hcsHlsAkamaiSettings = Nothing,
      _hcsHlsMediaStoreSettings = Nothing,
      _hcsHlsBasicPutSettings = Nothing,
      _hcsHlsWebdavSettings = Nothing
    }

-- | Undocumented member.
hcsHlsAkamaiSettings :: Lens' HlsCdnSettings (Maybe HlsAkamaiSettings)
hcsHlsAkamaiSettings = lens _hcsHlsAkamaiSettings (\s a -> s {_hcsHlsAkamaiSettings = a})

-- | Undocumented member.
hcsHlsMediaStoreSettings :: Lens' HlsCdnSettings (Maybe HlsMediaStoreSettings)
hcsHlsMediaStoreSettings = lens _hcsHlsMediaStoreSettings (\s a -> s {_hcsHlsMediaStoreSettings = a})

-- | Undocumented member.
hcsHlsBasicPutSettings :: Lens' HlsCdnSettings (Maybe HlsBasicPutSettings)
hcsHlsBasicPutSettings = lens _hcsHlsBasicPutSettings (\s a -> s {_hcsHlsBasicPutSettings = a})

-- | Undocumented member.
hcsHlsWebdavSettings :: Lens' HlsCdnSettings (Maybe HlsWebdavSettings)
hcsHlsWebdavSettings = lens _hcsHlsWebdavSettings (\s a -> s {_hcsHlsWebdavSettings = a})

instance FromJSON HlsCdnSettings where
  parseJSON =
    withObject
      "HlsCdnSettings"
      ( \x ->
          HlsCdnSettings'
            <$> (x .:? "hlsAkamaiSettings")
            <*> (x .:? "hlsMediaStoreSettings")
            <*> (x .:? "hlsBasicPutSettings")
            <*> (x .:? "hlsWebdavSettings")
      )

instance Hashable HlsCdnSettings

instance NFData HlsCdnSettings

instance ToJSON HlsCdnSettings where
  toJSON HlsCdnSettings' {..} =
    object
      ( catMaybes
          [ ("hlsAkamaiSettings" .=) <$> _hcsHlsAkamaiSettings,
            ("hlsMediaStoreSettings" .=) <$> _hcsHlsMediaStoreSettings,
            ("hlsBasicPutSettings" .=) <$> _hcsHlsBasicPutSettings,
            ("hlsWebdavSettings" .=) <$> _hcsHlsWebdavSettings
          ]
      )

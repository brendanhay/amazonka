{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.BaiduChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.BaiduChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the status and settings of the Baidu (Baidu Cloud Push) channel for an application.
--
--
--
-- /See:/ 'baiduChannelResponse' smart constructor.
data BaiduChannelResponse = BaiduChannelResponse'
  { _bcLastModifiedDate ::
      !(Maybe Text),
    _bcEnabled :: !(Maybe Bool),
    _bcIsArchived :: !(Maybe Bool),
    _bcApplicationId :: !(Maybe Text),
    _bcVersion :: !(Maybe Int),
    _bcId :: !(Maybe Text),
    _bcCreationDate :: !(Maybe Text),
    _bcLastModifiedBy :: !(Maybe Text),
    _bcHasCredential :: !(Maybe Bool),
    _bcCredential :: !Text,
    _bcPlatform :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BaiduChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcLastModifiedDate' - The date and time when the Baidu channel was last modified.
--
-- * 'bcEnabled' - Specifies whether the Baidu channel is enabled for the application.
--
-- * 'bcIsArchived' - Specifies whether the Baidu channel is archived.
--
-- * 'bcApplicationId' - The unique identifier for the application that the Baidu channel applies to.
--
-- * 'bcVersion' - The current version of the Baidu channel.
--
-- * 'bcId' - (Deprecated) An identifier for the Baidu channel. This property is retained only for backward compatibility.
--
-- * 'bcCreationDate' - The date and time when the Baidu channel was enabled.
--
-- * 'bcLastModifiedBy' - The user who last modified the Baidu channel.
--
-- * 'bcHasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- * 'bcCredential' - The API key that you received from the Baidu Cloud Push service to communicate with the service.
--
-- * 'bcPlatform' - The type of messaging or notification platform for the channel. For the Baidu channel, this value is BAIDU.
baiduChannelResponse ::
  -- | 'bcCredential'
  Text ->
  -- | 'bcPlatform'
  Text ->
  BaiduChannelResponse
baiduChannelResponse pCredential_ pPlatform_ =
  BaiduChannelResponse'
    { _bcLastModifiedDate = Nothing,
      _bcEnabled = Nothing,
      _bcIsArchived = Nothing,
      _bcApplicationId = Nothing,
      _bcVersion = Nothing,
      _bcId = Nothing,
      _bcCreationDate = Nothing,
      _bcLastModifiedBy = Nothing,
      _bcHasCredential = Nothing,
      _bcCredential = pCredential_,
      _bcPlatform = pPlatform_
    }

-- | The date and time when the Baidu channel was last modified.
bcLastModifiedDate :: Lens' BaiduChannelResponse (Maybe Text)
bcLastModifiedDate = lens _bcLastModifiedDate (\s a -> s {_bcLastModifiedDate = a})

-- | Specifies whether the Baidu channel is enabled for the application.
bcEnabled :: Lens' BaiduChannelResponse (Maybe Bool)
bcEnabled = lens _bcEnabled (\s a -> s {_bcEnabled = a})

-- | Specifies whether the Baidu channel is archived.
bcIsArchived :: Lens' BaiduChannelResponse (Maybe Bool)
bcIsArchived = lens _bcIsArchived (\s a -> s {_bcIsArchived = a})

-- | The unique identifier for the application that the Baidu channel applies to.
bcApplicationId :: Lens' BaiduChannelResponse (Maybe Text)
bcApplicationId = lens _bcApplicationId (\s a -> s {_bcApplicationId = a})

-- | The current version of the Baidu channel.
bcVersion :: Lens' BaiduChannelResponse (Maybe Int)
bcVersion = lens _bcVersion (\s a -> s {_bcVersion = a})

-- | (Deprecated) An identifier for the Baidu channel. This property is retained only for backward compatibility.
bcId :: Lens' BaiduChannelResponse (Maybe Text)
bcId = lens _bcId (\s a -> s {_bcId = a})

-- | The date and time when the Baidu channel was enabled.
bcCreationDate :: Lens' BaiduChannelResponse (Maybe Text)
bcCreationDate = lens _bcCreationDate (\s a -> s {_bcCreationDate = a})

-- | The user who last modified the Baidu channel.
bcLastModifiedBy :: Lens' BaiduChannelResponse (Maybe Text)
bcLastModifiedBy = lens _bcLastModifiedBy (\s a -> s {_bcLastModifiedBy = a})

-- | (Not used) This property is retained only for backward compatibility.
bcHasCredential :: Lens' BaiduChannelResponse (Maybe Bool)
bcHasCredential = lens _bcHasCredential (\s a -> s {_bcHasCredential = a})

-- | The API key that you received from the Baidu Cloud Push service to communicate with the service.
bcCredential :: Lens' BaiduChannelResponse Text
bcCredential = lens _bcCredential (\s a -> s {_bcCredential = a})

-- | The type of messaging or notification platform for the channel. For the Baidu channel, this value is BAIDU.
bcPlatform :: Lens' BaiduChannelResponse Text
bcPlatform = lens _bcPlatform (\s a -> s {_bcPlatform = a})

instance FromJSON BaiduChannelResponse where
  parseJSON =
    withObject
      "BaiduChannelResponse"
      ( \x ->
          BaiduChannelResponse'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "Enabled")
            <*> (x .:? "IsArchived")
            <*> (x .:? "ApplicationId")
            <*> (x .:? "Version")
            <*> (x .:? "Id")
            <*> (x .:? "CreationDate")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "HasCredential")
            <*> (x .: "Credential")
            <*> (x .: "Platform")
      )

instance Hashable BaiduChannelResponse

instance NFData BaiduChannelResponse

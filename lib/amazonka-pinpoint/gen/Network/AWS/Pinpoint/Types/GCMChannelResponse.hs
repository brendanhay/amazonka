{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GCMChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GCMChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the status and settings of the GCM channel for an application. The GCM channel enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
--
--
--
-- /See:/ 'gcmChannelResponse' smart constructor.
data GCMChannelResponse = GCMChannelResponse'
  { _gcLastModifiedDate ::
      !(Maybe Text),
    _gcEnabled :: !(Maybe Bool),
    _gcIsArchived :: !(Maybe Bool),
    _gcApplicationId :: !(Maybe Text),
    _gcVersion :: !(Maybe Int),
    _gcId :: !(Maybe Text),
    _gcCreationDate :: !(Maybe Text),
    _gcLastModifiedBy :: !(Maybe Text),
    _gcHasCredential :: !(Maybe Bool),
    _gcCredential :: !Text,
    _gcPlatform :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GCMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcLastModifiedDate' - The date and time when the GCM channel was last modified.
--
-- * 'gcEnabled' - Specifies whether the GCM channel is enabled for the application.
--
-- * 'gcIsArchived' - Specifies whether the GCM channel is archived.
--
-- * 'gcApplicationId' - The unique identifier for the application that the GCM channel applies to.
--
-- * 'gcVersion' - The current version of the GCM channel.
--
-- * 'gcId' - (Deprecated) An identifier for the GCM channel. This property is retained only for backward compatibility.
--
-- * 'gcCreationDate' - The date and time when the GCM channel was enabled.
--
-- * 'gcLastModifiedBy' - The user who last modified the GCM channel.
--
-- * 'gcHasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- * 'gcCredential' - The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
--
-- * 'gcPlatform' - The type of messaging or notification platform for the channel. For the GCM channel, this value is GCM.
gcmChannelResponse ::
  -- | 'gcCredential'
  Text ->
  -- | 'gcPlatform'
  Text ->
  GCMChannelResponse
gcmChannelResponse pCredential_ pPlatform_ =
  GCMChannelResponse'
    { _gcLastModifiedDate = Nothing,
      _gcEnabled = Nothing,
      _gcIsArchived = Nothing,
      _gcApplicationId = Nothing,
      _gcVersion = Nothing,
      _gcId = Nothing,
      _gcCreationDate = Nothing,
      _gcLastModifiedBy = Nothing,
      _gcHasCredential = Nothing,
      _gcCredential = pCredential_,
      _gcPlatform = pPlatform_
    }

-- | The date and time when the GCM channel was last modified.
gcLastModifiedDate :: Lens' GCMChannelResponse (Maybe Text)
gcLastModifiedDate = lens _gcLastModifiedDate (\s a -> s {_gcLastModifiedDate = a})

-- | Specifies whether the GCM channel is enabled for the application.
gcEnabled :: Lens' GCMChannelResponse (Maybe Bool)
gcEnabled = lens _gcEnabled (\s a -> s {_gcEnabled = a})

-- | Specifies whether the GCM channel is archived.
gcIsArchived :: Lens' GCMChannelResponse (Maybe Bool)
gcIsArchived = lens _gcIsArchived (\s a -> s {_gcIsArchived = a})

-- | The unique identifier for the application that the GCM channel applies to.
gcApplicationId :: Lens' GCMChannelResponse (Maybe Text)
gcApplicationId = lens _gcApplicationId (\s a -> s {_gcApplicationId = a})

-- | The current version of the GCM channel.
gcVersion :: Lens' GCMChannelResponse (Maybe Int)
gcVersion = lens _gcVersion (\s a -> s {_gcVersion = a})

-- | (Deprecated) An identifier for the GCM channel. This property is retained only for backward compatibility.
gcId :: Lens' GCMChannelResponse (Maybe Text)
gcId = lens _gcId (\s a -> s {_gcId = a})

-- | The date and time when the GCM channel was enabled.
gcCreationDate :: Lens' GCMChannelResponse (Maybe Text)
gcCreationDate = lens _gcCreationDate (\s a -> s {_gcCreationDate = a})

-- | The user who last modified the GCM channel.
gcLastModifiedBy :: Lens' GCMChannelResponse (Maybe Text)
gcLastModifiedBy = lens _gcLastModifiedBy (\s a -> s {_gcLastModifiedBy = a})

-- | (Not used) This property is retained only for backward compatibility.
gcHasCredential :: Lens' GCMChannelResponse (Maybe Bool)
gcHasCredential = lens _gcHasCredential (\s a -> s {_gcHasCredential = a})

-- | The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
gcCredential :: Lens' GCMChannelResponse Text
gcCredential = lens _gcCredential (\s a -> s {_gcCredential = a})

-- | The type of messaging or notification platform for the channel. For the GCM channel, this value is GCM.
gcPlatform :: Lens' GCMChannelResponse Text
gcPlatform = lens _gcPlatform (\s a -> s {_gcPlatform = a})

instance FromJSON GCMChannelResponse where
  parseJSON =
    withObject
      "GCMChannelResponse"
      ( \x ->
          GCMChannelResponse'
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

instance Hashable GCMChannelResponse

instance NFData GCMChannelResponse

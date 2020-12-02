{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) channel for an application.
--
--
--
-- /See:/ 'apnsChannelResponse' smart constructor.
data APNSChannelResponse = APNSChannelResponse'
  { _acLastModifiedDate ::
      !(Maybe Text),
    _acEnabled :: !(Maybe Bool),
    _acHasTokenKey :: !(Maybe Bool),
    _acDefaultAuthenticationMethod :: !(Maybe Text),
    _acIsArchived :: !(Maybe Bool),
    _acApplicationId :: !(Maybe Text),
    _acVersion :: !(Maybe Int),
    _acId :: !(Maybe Text),
    _acCreationDate :: !(Maybe Text),
    _acLastModifiedBy :: !(Maybe Text),
    _acHasCredential :: !(Maybe Bool),
    _acPlatform :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'APNSChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acLastModifiedDate' - The date and time when the APNs channel was last modified.
--
-- * 'acEnabled' - Specifies whether the APNs channel is enabled for the application.
--
-- * 'acHasTokenKey' - Specifies whether the APNs channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- * 'acDefaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
--
-- * 'acIsArchived' - Specifies whether the APNs channel is archived.
--
-- * 'acApplicationId' - The unique identifier for the application that the APNs channel applies to.
--
-- * 'acVersion' - The current version of the APNs channel.
--
-- * 'acId' - (Deprecated) An identifier for the APNs channel. This property is retained only for backward compatibility.
--
-- * 'acCreationDate' - The date and time when the APNs channel was enabled.
--
-- * 'acLastModifiedBy' - The user who last modified the APNs channel.
--
-- * 'acHasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- * 'acPlatform' - The type of messaging or notification platform for the channel. For the APNs channel, this value is APNS.
apnsChannelResponse ::
  -- | 'acPlatform'
  Text ->
  APNSChannelResponse
apnsChannelResponse pPlatform_ =
  APNSChannelResponse'
    { _acLastModifiedDate = Nothing,
      _acEnabled = Nothing,
      _acHasTokenKey = Nothing,
      _acDefaultAuthenticationMethod = Nothing,
      _acIsArchived = Nothing,
      _acApplicationId = Nothing,
      _acVersion = Nothing,
      _acId = Nothing,
      _acCreationDate = Nothing,
      _acLastModifiedBy = Nothing,
      _acHasCredential = Nothing,
      _acPlatform = pPlatform_
    }

-- | The date and time when the APNs channel was last modified.
acLastModifiedDate :: Lens' APNSChannelResponse (Maybe Text)
acLastModifiedDate = lens _acLastModifiedDate (\s a -> s {_acLastModifiedDate = a})

-- | Specifies whether the APNs channel is enabled for the application.
acEnabled :: Lens' APNSChannelResponse (Maybe Bool)
acEnabled = lens _acEnabled (\s a -> s {_acEnabled = a})

-- | Specifies whether the APNs channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
acHasTokenKey :: Lens' APNSChannelResponse (Maybe Bool)
acHasTokenKey = lens _acHasTokenKey (\s a -> s {_acHasTokenKey = a})

-- | The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
acDefaultAuthenticationMethod :: Lens' APNSChannelResponse (Maybe Text)
acDefaultAuthenticationMethod = lens _acDefaultAuthenticationMethod (\s a -> s {_acDefaultAuthenticationMethod = a})

-- | Specifies whether the APNs channel is archived.
acIsArchived :: Lens' APNSChannelResponse (Maybe Bool)
acIsArchived = lens _acIsArchived (\s a -> s {_acIsArchived = a})

-- | The unique identifier for the application that the APNs channel applies to.
acApplicationId :: Lens' APNSChannelResponse (Maybe Text)
acApplicationId = lens _acApplicationId (\s a -> s {_acApplicationId = a})

-- | The current version of the APNs channel.
acVersion :: Lens' APNSChannelResponse (Maybe Int)
acVersion = lens _acVersion (\s a -> s {_acVersion = a})

-- | (Deprecated) An identifier for the APNs channel. This property is retained only for backward compatibility.
acId :: Lens' APNSChannelResponse (Maybe Text)
acId = lens _acId (\s a -> s {_acId = a})

-- | The date and time when the APNs channel was enabled.
acCreationDate :: Lens' APNSChannelResponse (Maybe Text)
acCreationDate = lens _acCreationDate (\s a -> s {_acCreationDate = a})

-- | The user who last modified the APNs channel.
acLastModifiedBy :: Lens' APNSChannelResponse (Maybe Text)
acLastModifiedBy = lens _acLastModifiedBy (\s a -> s {_acLastModifiedBy = a})

-- | (Not used) This property is retained only for backward compatibility.
acHasCredential :: Lens' APNSChannelResponse (Maybe Bool)
acHasCredential = lens _acHasCredential (\s a -> s {_acHasCredential = a})

-- | The type of messaging or notification platform for the channel. For the APNs channel, this value is APNS.
acPlatform :: Lens' APNSChannelResponse Text
acPlatform = lens _acPlatform (\s a -> s {_acPlatform = a})

instance FromJSON APNSChannelResponse where
  parseJSON =
    withObject
      "APNSChannelResponse"
      ( \x ->
          APNSChannelResponse'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "Enabled")
            <*> (x .:? "HasTokenKey")
            <*> (x .:? "DefaultAuthenticationMethod")
            <*> (x .:? "IsArchived")
            <*> (x .:? "ApplicationId")
            <*> (x .:? "Version")
            <*> (x .:? "Id")
            <*> (x .:? "CreationDate")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "HasCredential")
            <*> (x .: "Platform")
      )

instance Hashable APNSChannelResponse

instance NFData APNSChannelResponse

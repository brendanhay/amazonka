{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) sandbox channel for an application.
--
--
--
-- /See:/ 'apnsSandboxChannelResponse' smart constructor.
data APNSSandboxChannelResponse = APNSSandboxChannelResponse'
  { _ascLastModifiedDate ::
      !(Maybe Text),
    _ascEnabled :: !(Maybe Bool),
    _ascHasTokenKey :: !(Maybe Bool),
    _ascDefaultAuthenticationMethod ::
      !(Maybe Text),
    _ascIsArchived :: !(Maybe Bool),
    _ascApplicationId :: !(Maybe Text),
    _ascVersion :: !(Maybe Int),
    _ascId :: !(Maybe Text),
    _ascCreationDate :: !(Maybe Text),
    _ascLastModifiedBy :: !(Maybe Text),
    _ascHasCredential :: !(Maybe Bool),
    _ascPlatform :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'APNSSandboxChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascLastModifiedDate' - The date and time when the APNs sandbox channel was last modified.
--
-- * 'ascEnabled' - Specifies whether the APNs sandbox channel is enabled for the application.
--
-- * 'ascHasTokenKey' - Specifies whether the APNs sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- * 'ascDefaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
--
-- * 'ascIsArchived' - Specifies whether the APNs sandbox channel is archived.
--
-- * 'ascApplicationId' - The unique identifier for the application that the APNs sandbox channel applies to.
--
-- * 'ascVersion' - The current version of the APNs sandbox channel.
--
-- * 'ascId' - (Deprecated) An identifier for the APNs sandbox channel. This property is retained only for backward compatibility.
--
-- * 'ascCreationDate' - The date and time when the APNs sandbox channel was enabled.
--
-- * 'ascLastModifiedBy' - The user who last modified the APNs sandbox channel.
--
-- * 'ascHasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- * 'ascPlatform' - The type of messaging or notification platform for the channel. For the APNs sandbox channel, this value is APNS_SANDBOX.
apnsSandboxChannelResponse ::
  -- | 'ascPlatform'
  Text ->
  APNSSandboxChannelResponse
apnsSandboxChannelResponse pPlatform_ =
  APNSSandboxChannelResponse'
    { _ascLastModifiedDate = Nothing,
      _ascEnabled = Nothing,
      _ascHasTokenKey = Nothing,
      _ascDefaultAuthenticationMethod = Nothing,
      _ascIsArchived = Nothing,
      _ascApplicationId = Nothing,
      _ascVersion = Nothing,
      _ascId = Nothing,
      _ascCreationDate = Nothing,
      _ascLastModifiedBy = Nothing,
      _ascHasCredential = Nothing,
      _ascPlatform = pPlatform_
    }

-- | The date and time when the APNs sandbox channel was last modified.
ascLastModifiedDate :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascLastModifiedDate = lens _ascLastModifiedDate (\s a -> s {_ascLastModifiedDate = a})

-- | Specifies whether the APNs sandbox channel is enabled for the application.
ascEnabled :: Lens' APNSSandboxChannelResponse (Maybe Bool)
ascEnabled = lens _ascEnabled (\s a -> s {_ascEnabled = a})

-- | Specifies whether the APNs sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
ascHasTokenKey :: Lens' APNSSandboxChannelResponse (Maybe Bool)
ascHasTokenKey = lens _ascHasTokenKey (\s a -> s {_ascHasTokenKey = a})

-- | The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
ascDefaultAuthenticationMethod :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascDefaultAuthenticationMethod = lens _ascDefaultAuthenticationMethod (\s a -> s {_ascDefaultAuthenticationMethod = a})

-- | Specifies whether the APNs sandbox channel is archived.
ascIsArchived :: Lens' APNSSandboxChannelResponse (Maybe Bool)
ascIsArchived = lens _ascIsArchived (\s a -> s {_ascIsArchived = a})

-- | The unique identifier for the application that the APNs sandbox channel applies to.
ascApplicationId :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascApplicationId = lens _ascApplicationId (\s a -> s {_ascApplicationId = a})

-- | The current version of the APNs sandbox channel.
ascVersion :: Lens' APNSSandboxChannelResponse (Maybe Int)
ascVersion = lens _ascVersion (\s a -> s {_ascVersion = a})

-- | (Deprecated) An identifier for the APNs sandbox channel. This property is retained only for backward compatibility.
ascId :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascId = lens _ascId (\s a -> s {_ascId = a})

-- | The date and time when the APNs sandbox channel was enabled.
ascCreationDate :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascCreationDate = lens _ascCreationDate (\s a -> s {_ascCreationDate = a})

-- | The user who last modified the APNs sandbox channel.
ascLastModifiedBy :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascLastModifiedBy = lens _ascLastModifiedBy (\s a -> s {_ascLastModifiedBy = a})

-- | (Not used) This property is retained only for backward compatibility.
ascHasCredential :: Lens' APNSSandboxChannelResponse (Maybe Bool)
ascHasCredential = lens _ascHasCredential (\s a -> s {_ascHasCredential = a})

-- | The type of messaging or notification platform for the channel. For the APNs sandbox channel, this value is APNS_SANDBOX.
ascPlatform :: Lens' APNSSandboxChannelResponse Text
ascPlatform = lens _ascPlatform (\s a -> s {_ascPlatform = a})

instance FromJSON APNSSandboxChannelResponse where
  parseJSON =
    withObject
      "APNSSandboxChannelResponse"
      ( \x ->
          APNSSandboxChannelResponse'
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

instance Hashable APNSSandboxChannelResponse

instance NFData APNSSandboxChannelResponse

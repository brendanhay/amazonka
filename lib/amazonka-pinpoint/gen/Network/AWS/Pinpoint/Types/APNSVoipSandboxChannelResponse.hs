{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) VoIP sandbox channel for an application.
--
--
--
-- /See:/ 'apnsVoipSandboxChannelResponse' smart constructor.
data APNSVoipSandboxChannelResponse = APNSVoipSandboxChannelResponse'
  { _avscLastModifiedDate ::
      !(Maybe Text),
    _avscEnabled :: !(Maybe Bool),
    _avscHasTokenKey ::
      !(Maybe Bool),
    _avscDefaultAuthenticationMethod ::
      !(Maybe Text),
    _avscIsArchived ::
      !(Maybe Bool),
    _avscApplicationId ::
      !(Maybe Text),
    _avscVersion :: !(Maybe Int),
    _avscId :: !(Maybe Text),
    _avscCreationDate ::
      !(Maybe Text),
    _avscLastModifiedBy ::
      !(Maybe Text),
    _avscHasCredential ::
      !(Maybe Bool),
    _avscPlatform :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'APNSVoipSandboxChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avscLastModifiedDate' - The date and time when the APNs VoIP sandbox channel was last modified.
--
-- * 'avscEnabled' - Specifies whether the APNs VoIP sandbox channel is enabled for the application.
--
-- * 'avscHasTokenKey' - Specifies whether the APNs VoIP sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- * 'avscDefaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
--
-- * 'avscIsArchived' - Specifies whether the APNs VoIP sandbox channel is archived.
--
-- * 'avscApplicationId' - The unique identifier for the application that the APNs VoIP sandbox channel applies to.
--
-- * 'avscVersion' - The current version of the APNs VoIP sandbox channel.
--
-- * 'avscId' - (Deprecated) An identifier for the APNs VoIP sandbox channel. This property is retained only for backward compatibility.
--
-- * 'avscCreationDate' - The date and time when the APNs VoIP sandbox channel was enabled.
--
-- * 'avscLastModifiedBy' - The user who last modified the APNs VoIP sandbox channel.
--
-- * 'avscHasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- * 'avscPlatform' - The type of messaging or notification platform for the channel. For the APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
apnsVoipSandboxChannelResponse ::
  -- | 'avscPlatform'
  Text ->
  APNSVoipSandboxChannelResponse
apnsVoipSandboxChannelResponse pPlatform_ =
  APNSVoipSandboxChannelResponse'
    { _avscLastModifiedDate = Nothing,
      _avscEnabled = Nothing,
      _avscHasTokenKey = Nothing,
      _avscDefaultAuthenticationMethod = Nothing,
      _avscIsArchived = Nothing,
      _avscApplicationId = Nothing,
      _avscVersion = Nothing,
      _avscId = Nothing,
      _avscCreationDate = Nothing,
      _avscLastModifiedBy = Nothing,
      _avscHasCredential = Nothing,
      _avscPlatform = pPlatform_
    }

-- | The date and time when the APNs VoIP sandbox channel was last modified.
avscLastModifiedDate :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscLastModifiedDate = lens _avscLastModifiedDate (\s a -> s {_avscLastModifiedDate = a})

-- | Specifies whether the APNs VoIP sandbox channel is enabled for the application.
avscEnabled :: Lens' APNSVoipSandboxChannelResponse (Maybe Bool)
avscEnabled = lens _avscEnabled (\s a -> s {_avscEnabled = a})

-- | Specifies whether the APNs VoIP sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
avscHasTokenKey :: Lens' APNSVoipSandboxChannelResponse (Maybe Bool)
avscHasTokenKey = lens _avscHasTokenKey (\s a -> s {_avscHasTokenKey = a})

-- | The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
avscDefaultAuthenticationMethod :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscDefaultAuthenticationMethod = lens _avscDefaultAuthenticationMethod (\s a -> s {_avscDefaultAuthenticationMethod = a})

-- | Specifies whether the APNs VoIP sandbox channel is archived.
avscIsArchived :: Lens' APNSVoipSandboxChannelResponse (Maybe Bool)
avscIsArchived = lens _avscIsArchived (\s a -> s {_avscIsArchived = a})

-- | The unique identifier for the application that the APNs VoIP sandbox channel applies to.
avscApplicationId :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscApplicationId = lens _avscApplicationId (\s a -> s {_avscApplicationId = a})

-- | The current version of the APNs VoIP sandbox channel.
avscVersion :: Lens' APNSVoipSandboxChannelResponse (Maybe Int)
avscVersion = lens _avscVersion (\s a -> s {_avscVersion = a})

-- | (Deprecated) An identifier for the APNs VoIP sandbox channel. This property is retained only for backward compatibility.
avscId :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscId = lens _avscId (\s a -> s {_avscId = a})

-- | The date and time when the APNs VoIP sandbox channel was enabled.
avscCreationDate :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscCreationDate = lens _avscCreationDate (\s a -> s {_avscCreationDate = a})

-- | The user who last modified the APNs VoIP sandbox channel.
avscLastModifiedBy :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscLastModifiedBy = lens _avscLastModifiedBy (\s a -> s {_avscLastModifiedBy = a})

-- | (Not used) This property is retained only for backward compatibility.
avscHasCredential :: Lens' APNSVoipSandboxChannelResponse (Maybe Bool)
avscHasCredential = lens _avscHasCredential (\s a -> s {_avscHasCredential = a})

-- | The type of messaging or notification platform for the channel. For the APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
avscPlatform :: Lens' APNSVoipSandboxChannelResponse Text
avscPlatform = lens _avscPlatform (\s a -> s {_avscPlatform = a})

instance FromJSON APNSVoipSandboxChannelResponse where
  parseJSON =
    withObject
      "APNSVoipSandboxChannelResponse"
      ( \x ->
          APNSVoipSandboxChannelResponse'
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

instance Hashable APNSVoipSandboxChannelResponse

instance NFData APNSVoipSandboxChannelResponse

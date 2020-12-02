{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipChannelResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) VoIP channel for an application.
--
--
--
-- /See:/ 'apnsVoipChannelResponse' smart constructor.
data APNSVoipChannelResponse = APNSVoipChannelResponse'
  { _avcLastModifiedDate ::
      !(Maybe Text),
    _avcEnabled :: !(Maybe Bool),
    _avcHasTokenKey :: !(Maybe Bool),
    _avcDefaultAuthenticationMethod ::
      !(Maybe Text),
    _avcIsArchived :: !(Maybe Bool),
    _avcApplicationId :: !(Maybe Text),
    _avcVersion :: !(Maybe Int),
    _avcId :: !(Maybe Text),
    _avcCreationDate :: !(Maybe Text),
    _avcLastModifiedBy :: !(Maybe Text),
    _avcHasCredential :: !(Maybe Bool),
    _avcPlatform :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'APNSVoipChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avcLastModifiedDate' - The date and time when the APNs VoIP channel was last modified.
--
-- * 'avcEnabled' - Specifies whether the APNs VoIP channel is enabled for the application.
--
-- * 'avcHasTokenKey' - Specifies whether the APNs VoIP channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- * 'avcDefaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
--
-- * 'avcIsArchived' - Specifies whether the APNs VoIP channel is archived.
--
-- * 'avcApplicationId' - The unique identifier for the application that the APNs VoIP channel applies to.
--
-- * 'avcVersion' - The current version of the APNs VoIP channel.
--
-- * 'avcId' - (Deprecated) An identifier for the APNs VoIP channel. This property is retained only for backward compatibility.
--
-- * 'avcCreationDate' - The date and time when the APNs VoIP channel was enabled.
--
-- * 'avcLastModifiedBy' - The user who last modified the APNs VoIP channel.
--
-- * 'avcHasCredential' - (Not used) This property is retained only for backward compatibility.
--
-- * 'avcPlatform' - The type of messaging or notification platform for the channel. For the APNs VoIP channel, this value is APNS_VOIP.
apnsVoipChannelResponse ::
  -- | 'avcPlatform'
  Text ->
  APNSVoipChannelResponse
apnsVoipChannelResponse pPlatform_ =
  APNSVoipChannelResponse'
    { _avcLastModifiedDate = Nothing,
      _avcEnabled = Nothing,
      _avcHasTokenKey = Nothing,
      _avcDefaultAuthenticationMethod = Nothing,
      _avcIsArchived = Nothing,
      _avcApplicationId = Nothing,
      _avcVersion = Nothing,
      _avcId = Nothing,
      _avcCreationDate = Nothing,
      _avcLastModifiedBy = Nothing,
      _avcHasCredential = Nothing,
      _avcPlatform = pPlatform_
    }

-- | The date and time when the APNs VoIP channel was last modified.
avcLastModifiedDate :: Lens' APNSVoipChannelResponse (Maybe Text)
avcLastModifiedDate = lens _avcLastModifiedDate (\s a -> s {_avcLastModifiedDate = a})

-- | Specifies whether the APNs VoIP channel is enabled for the application.
avcEnabled :: Lens' APNSVoipChannelResponse (Maybe Bool)
avcEnabled = lens _avcEnabled (\s a -> s {_avcEnabled = a})

-- | Specifies whether the APNs VoIP channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
avcHasTokenKey :: Lens' APNSVoipChannelResponse (Maybe Bool)
avcHasTokenKey = lens _avcHasTokenKey (\s a -> s {_avcHasTokenKey = a})

-- | The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
avcDefaultAuthenticationMethod :: Lens' APNSVoipChannelResponse (Maybe Text)
avcDefaultAuthenticationMethod = lens _avcDefaultAuthenticationMethod (\s a -> s {_avcDefaultAuthenticationMethod = a})

-- | Specifies whether the APNs VoIP channel is archived.
avcIsArchived :: Lens' APNSVoipChannelResponse (Maybe Bool)
avcIsArchived = lens _avcIsArchived (\s a -> s {_avcIsArchived = a})

-- | The unique identifier for the application that the APNs VoIP channel applies to.
avcApplicationId :: Lens' APNSVoipChannelResponse (Maybe Text)
avcApplicationId = lens _avcApplicationId (\s a -> s {_avcApplicationId = a})

-- | The current version of the APNs VoIP channel.
avcVersion :: Lens' APNSVoipChannelResponse (Maybe Int)
avcVersion = lens _avcVersion (\s a -> s {_avcVersion = a})

-- | (Deprecated) An identifier for the APNs VoIP channel. This property is retained only for backward compatibility.
avcId :: Lens' APNSVoipChannelResponse (Maybe Text)
avcId = lens _avcId (\s a -> s {_avcId = a})

-- | The date and time when the APNs VoIP channel was enabled.
avcCreationDate :: Lens' APNSVoipChannelResponse (Maybe Text)
avcCreationDate = lens _avcCreationDate (\s a -> s {_avcCreationDate = a})

-- | The user who last modified the APNs VoIP channel.
avcLastModifiedBy :: Lens' APNSVoipChannelResponse (Maybe Text)
avcLastModifiedBy = lens _avcLastModifiedBy (\s a -> s {_avcLastModifiedBy = a})

-- | (Not used) This property is retained only for backward compatibility.
avcHasCredential :: Lens' APNSVoipChannelResponse (Maybe Bool)
avcHasCredential = lens _avcHasCredential (\s a -> s {_avcHasCredential = a})

-- | The type of messaging or notification platform for the channel. For the APNs VoIP channel, this value is APNS_VOIP.
avcPlatform :: Lens' APNSVoipChannelResponse Text
avcPlatform = lens _avcPlatform (\s a -> s {_avcPlatform = a})

instance FromJSON APNSVoipChannelResponse where
  parseJSON =
    withObject
      "APNSVoipChannelResponse"
      ( \x ->
          APNSVoipChannelResponse'
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

instance Hashable APNSVoipChannelResponse

instance NFData APNSVoipChannelResponse

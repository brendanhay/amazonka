{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the status and settings of the APNs (Apple Push Notification service) VoIP sandbox channel for an application.
--
--
--
-- /See:/ 'apnsVoipSandboxChannelRequest' smart constructor.
data APNSVoipSandboxChannelRequest = APNSVoipSandboxChannelRequest'
  { _avscrTokenKey ::
      !(Maybe Text),
    _avscrPrivateKey ::
      !(Maybe Text),
    _avscrEnabled :: !(Maybe Bool),
    _avscrTeamId :: !(Maybe Text),
    _avscrBundleId :: !(Maybe Text),
    _avscrDefaultAuthenticationMethod ::
      !(Maybe Text),
    _avscrCertificate ::
      !(Maybe Text),
    _avscrTokenKeyId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'APNSVoipSandboxChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avscrTokenKey' - The authentication key to use for APNs tokens.
--
-- * 'avscrPrivateKey' - The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with the APNs sandbox environment.
--
-- * 'avscrEnabled' - Specifies whether the APNs VoIP sandbox channel is enabled for the application.
--
-- * 'avscrTeamId' - The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
--
-- * 'avscrBundleId' - The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
--
-- * 'avscrDefaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use when authenticating with the APNs sandbox environment for this channel, key or certificate.
--
-- * 'avscrCertificate' - The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using an APNs certificate.
--
-- * 'avscrTokenKeyId' - The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using APNs tokens.
apnsVoipSandboxChannelRequest ::
  APNSVoipSandboxChannelRequest
apnsVoipSandboxChannelRequest =
  APNSVoipSandboxChannelRequest'
    { _avscrTokenKey = Nothing,
      _avscrPrivateKey = Nothing,
      _avscrEnabled = Nothing,
      _avscrTeamId = Nothing,
      _avscrBundleId = Nothing,
      _avscrDefaultAuthenticationMethod = Nothing,
      _avscrCertificate = Nothing,
      _avscrTokenKeyId = Nothing
    }

-- | The authentication key to use for APNs tokens.
avscrTokenKey :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrTokenKey = lens _avscrTokenKey (\s a -> s {_avscrTokenKey = a})

-- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with the APNs sandbox environment.
avscrPrivateKey :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrPrivateKey = lens _avscrPrivateKey (\s a -> s {_avscrPrivateKey = a})

-- | Specifies whether the APNs VoIP sandbox channel is enabled for the application.
avscrEnabled :: Lens' APNSVoipSandboxChannelRequest (Maybe Bool)
avscrEnabled = lens _avscrEnabled (\s a -> s {_avscrEnabled = a})

-- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
avscrTeamId :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrTeamId = lens _avscrTeamId (\s a -> s {_avscrTeamId = a})

-- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
avscrBundleId :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrBundleId = lens _avscrBundleId (\s a -> s {_avscrBundleId = a})

-- | The default authentication method that you want Amazon Pinpoint to use when authenticating with the APNs sandbox environment for this channel, key or certificate.
avscrDefaultAuthenticationMethod :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrDefaultAuthenticationMethod = lens _avscrDefaultAuthenticationMethod (\s a -> s {_avscrDefaultAuthenticationMethod = a})

-- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using an APNs certificate.
avscrCertificate :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrCertificate = lens _avscrCertificate (\s a -> s {_avscrCertificate = a})

-- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using APNs tokens.
avscrTokenKeyId :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrTokenKeyId = lens _avscrTokenKeyId (\s a -> s {_avscrTokenKeyId = a})

instance Hashable APNSVoipSandboxChannelRequest

instance NFData APNSVoipSandboxChannelRequest

instance ToJSON APNSVoipSandboxChannelRequest where
  toJSON APNSVoipSandboxChannelRequest' {..} =
    object
      ( catMaybes
          [ ("TokenKey" .=) <$> _avscrTokenKey,
            ("PrivateKey" .=) <$> _avscrPrivateKey,
            ("Enabled" .=) <$> _avscrEnabled,
            ("TeamId" .=) <$> _avscrTeamId,
            ("BundleId" .=) <$> _avscrBundleId,
            ("DefaultAuthenticationMethod" .=)
              <$> _avscrDefaultAuthenticationMethod,
            ("Certificate" .=) <$> _avscrCertificate,
            ("TokenKeyId" .=) <$> _avscrTokenKeyId
          ]
      )

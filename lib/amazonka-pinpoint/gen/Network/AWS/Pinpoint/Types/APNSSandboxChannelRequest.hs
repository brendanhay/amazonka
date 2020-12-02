{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSSandboxChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSSandboxChannelRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the status and settings of the APNs (Apple Push Notification service) sandbox channel for an application.
--
--
--
-- /See:/ 'apnsSandboxChannelRequest' smart constructor.
data APNSSandboxChannelRequest = APNSSandboxChannelRequest'
  { _ascrTokenKey ::
      !(Maybe Text),
    _ascrPrivateKey :: !(Maybe Text),
    _ascrEnabled :: !(Maybe Bool),
    _ascrTeamId :: !(Maybe Text),
    _ascrBundleId :: !(Maybe Text),
    _ascrDefaultAuthenticationMethod ::
      !(Maybe Text),
    _ascrCertificate :: !(Maybe Text),
    _ascrTokenKeyId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'APNSSandboxChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascrTokenKey' - The authentication key to use for APNs tokens.
--
-- * 'ascrPrivateKey' - The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with the APNs sandbox environment.
--
-- * 'ascrEnabled' - Specifies whether to enable the APNs sandbox channel for the application.
--
-- * 'ascrTeamId' - The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
--
-- * 'ascrBundleId' - The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
--
-- * 'ascrDefaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use when authenticating with the APNs sandbox environment, key or certificate.
--
-- * 'ascrCertificate' - The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using an APNs certificate.
--
-- * 'ascrTokenKeyId' - The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using APNs tokens.
apnsSandboxChannelRequest ::
  APNSSandboxChannelRequest
apnsSandboxChannelRequest =
  APNSSandboxChannelRequest'
    { _ascrTokenKey = Nothing,
      _ascrPrivateKey = Nothing,
      _ascrEnabled = Nothing,
      _ascrTeamId = Nothing,
      _ascrBundleId = Nothing,
      _ascrDefaultAuthenticationMethod = Nothing,
      _ascrCertificate = Nothing,
      _ascrTokenKeyId = Nothing
    }

-- | The authentication key to use for APNs tokens.
ascrTokenKey :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrTokenKey = lens _ascrTokenKey (\s a -> s {_ascrTokenKey = a})

-- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with the APNs sandbox environment.
ascrPrivateKey :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrPrivateKey = lens _ascrPrivateKey (\s a -> s {_ascrPrivateKey = a})

-- | Specifies whether to enable the APNs sandbox channel for the application.
ascrEnabled :: Lens' APNSSandboxChannelRequest (Maybe Bool)
ascrEnabled = lens _ascrEnabled (\s a -> s {_ascrEnabled = a})

-- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
ascrTeamId :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrTeamId = lens _ascrTeamId (\s a -> s {_ascrTeamId = a})

-- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
ascrBundleId :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrBundleId = lens _ascrBundleId (\s a -> s {_ascrBundleId = a})

-- | The default authentication method that you want Amazon Pinpoint to use when authenticating with the APNs sandbox environment, key or certificate.
ascrDefaultAuthenticationMethod :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrDefaultAuthenticationMethod = lens _ascrDefaultAuthenticationMethod (\s a -> s {_ascrDefaultAuthenticationMethod = a})

-- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using an APNs certificate.
ascrCertificate :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrCertificate = lens _ascrCertificate (\s a -> s {_ascrCertificate = a})

-- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with the APNs sandbox environment by using APNs tokens.
ascrTokenKeyId :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrTokenKeyId = lens _ascrTokenKeyId (\s a -> s {_ascrTokenKeyId = a})

instance Hashable APNSSandboxChannelRequest

instance NFData APNSSandboxChannelRequest

instance ToJSON APNSSandboxChannelRequest where
  toJSON APNSSandboxChannelRequest' {..} =
    object
      ( catMaybes
          [ ("TokenKey" .=) <$> _ascrTokenKey,
            ("PrivateKey" .=) <$> _ascrPrivateKey,
            ("Enabled" .=) <$> _ascrEnabled,
            ("TeamId" .=) <$> _ascrTeamId,
            ("BundleId" .=) <$> _ascrBundleId,
            ("DefaultAuthenticationMethod" .=)
              <$> _ascrDefaultAuthenticationMethod,
            ("Certificate" .=) <$> _ascrCertificate,
            ("TokenKeyId" .=) <$> _ascrTokenKeyId
          ]
      )

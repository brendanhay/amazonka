{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSChannelRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the status and settings of the APNs (Apple Push Notification service) channel for an application.
--
--
--
-- /See:/ 'apnsChannelRequest' smart constructor.
data APNSChannelRequest = APNSChannelRequest'
  { _acrTokenKey ::
      !(Maybe Text),
    _acrPrivateKey :: !(Maybe Text),
    _acrEnabled :: !(Maybe Bool),
    _acrTeamId :: !(Maybe Text),
    _acrBundleId :: !(Maybe Text),
    _acrDefaultAuthenticationMethod :: !(Maybe Text),
    _acrCertificate :: !(Maybe Text),
    _acrTokenKeyId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'APNSChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acrTokenKey' - The authentication key to use for APNs tokens.
--
-- * 'acrPrivateKey' - The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with APNs.
--
-- * 'acrEnabled' - Specifies whether to enable the APNs channel for the application.
--
-- * 'acrTeamId' - The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
--
-- * 'acrBundleId' - The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
--
-- * 'acrDefaultAuthenticationMethod' - The default authentication method that you want Amazon Pinpoint to use when authenticating with APNs, key or certificate.
--
-- * 'acrCertificate' - The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with APNs by using an APNs certificate.
--
-- * 'acrTokenKeyId' - The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with APNs by using APNs tokens.
apnsChannelRequest ::
  APNSChannelRequest
apnsChannelRequest =
  APNSChannelRequest'
    { _acrTokenKey = Nothing,
      _acrPrivateKey = Nothing,
      _acrEnabled = Nothing,
      _acrTeamId = Nothing,
      _acrBundleId = Nothing,
      _acrDefaultAuthenticationMethod = Nothing,
      _acrCertificate = Nothing,
      _acrTokenKeyId = Nothing
    }

-- | The authentication key to use for APNs tokens.
acrTokenKey :: Lens' APNSChannelRequest (Maybe Text)
acrTokenKey = lens _acrTokenKey (\s a -> s {_acrTokenKey = a})

-- | The private key for the APNs client certificate that you want Amazon Pinpoint to use to communicate with APNs.
acrPrivateKey :: Lens' APNSChannelRequest (Maybe Text)
acrPrivateKey = lens _acrPrivateKey (\s a -> s {_acrPrivateKey = a})

-- | Specifies whether to enable the APNs channel for the application.
acrEnabled :: Lens' APNSChannelRequest (Maybe Bool)
acrEnabled = lens _acrEnabled (\s a -> s {_acrEnabled = a})

-- | The identifier that's assigned to your Apple developer account team. This identifier is used for APNs tokens.
acrTeamId :: Lens' APNSChannelRequest (Maybe Text)
acrTeamId = lens _acrTeamId (\s a -> s {_acrTeamId = a})

-- | The bundle identifier that's assigned to your iOS app. This identifier is used for APNs tokens.
acrBundleId :: Lens' APNSChannelRequest (Maybe Text)
acrBundleId = lens _acrBundleId (\s a -> s {_acrBundleId = a})

-- | The default authentication method that you want Amazon Pinpoint to use when authenticating with APNs, key or certificate.
acrDefaultAuthenticationMethod :: Lens' APNSChannelRequest (Maybe Text)
acrDefaultAuthenticationMethod = lens _acrDefaultAuthenticationMethod (\s a -> s {_acrDefaultAuthenticationMethod = a})

-- | The APNs client certificate that you received from Apple, if you want Amazon Pinpoint to communicate with APNs by using an APNs certificate.
acrCertificate :: Lens' APNSChannelRequest (Maybe Text)
acrCertificate = lens _acrCertificate (\s a -> s {_acrCertificate = a})

-- | The key identifier that's assigned to your APNs signing key, if you want Amazon Pinpoint to communicate with APNs by using APNs tokens.
acrTokenKeyId :: Lens' APNSChannelRequest (Maybe Text)
acrTokenKeyId = lens _acrTokenKeyId (\s a -> s {_acrTokenKeyId = a})

instance Hashable APNSChannelRequest

instance NFData APNSChannelRequest

instance ToJSON APNSChannelRequest where
  toJSON APNSChannelRequest' {..} =
    object
      ( catMaybes
          [ ("TokenKey" .=) <$> _acrTokenKey,
            ("PrivateKey" .=) <$> _acrPrivateKey,
            ("Enabled" .=) <$> _acrEnabled,
            ("TeamId" .=) <$> _acrTeamId,
            ("BundleId" .=) <$> _acrBundleId,
            ("DefaultAuthenticationMethod" .=)
              <$> _acrDefaultAuthenticationMethod,
            ("Certificate" .=) <$> _acrCertificate,
            ("TokenKeyId" .=) <$> _acrTokenKeyId
          ]
      )

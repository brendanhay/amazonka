{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.NetworkProfile where

import Network.AWS.AlexaBusiness.Types.NetworkEapMethod
import Network.AWS.AlexaBusiness.Types.NetworkSecurityType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The network profile associated with a device.
--
--
--
-- /See:/ 'networkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { _npNetworkProfileName ::
      !(Maybe Text),
    _npSsid :: !(Maybe Text),
    _npNetworkProfileARN :: !(Maybe Text),
    _npSecurityType :: !(Maybe NetworkSecurityType),
    _npCurrentPassword :: !(Maybe (Sensitive Text)),
    _npNextPassword :: !(Maybe (Sensitive Text)),
    _npEapMethod :: !(Maybe NetworkEapMethod),
    _npDescription :: !(Maybe Text),
    _npTrustAnchors :: !(Maybe (List1 Text)),
    _npCertificateAuthorityARN :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npNetworkProfileName' - The name of the network profile associated with a device.
--
-- * 'npSsid' - The SSID of the Wi-Fi network.
--
-- * 'npNetworkProfileARN' - The ARN of the network profile associated with a device.
--
-- * 'npSecurityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- * 'npCurrentPassword' - The current password of the Wi-Fi network.
--
-- * 'npNextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
--
-- * 'npEapMethod' - The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
--
-- * 'npDescription' - Detailed information about a device's network profile.
--
-- * 'npTrustAnchors' - The root certificates of your authentication server, which is installed on your devices and used to trust your authentication server during EAP negotiation.
--
-- * 'npCertificateAuthorityARN' - The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
networkProfile ::
  NetworkProfile
networkProfile =
  NetworkProfile'
    { _npNetworkProfileName = Nothing,
      _npSsid = Nothing,
      _npNetworkProfileARN = Nothing,
      _npSecurityType = Nothing,
      _npCurrentPassword = Nothing,
      _npNextPassword = Nothing,
      _npEapMethod = Nothing,
      _npDescription = Nothing,
      _npTrustAnchors = Nothing,
      _npCertificateAuthorityARN = Nothing
    }

-- | The name of the network profile associated with a device.
npNetworkProfileName :: Lens' NetworkProfile (Maybe Text)
npNetworkProfileName = lens _npNetworkProfileName (\s a -> s {_npNetworkProfileName = a})

-- | The SSID of the Wi-Fi network.
npSsid :: Lens' NetworkProfile (Maybe Text)
npSsid = lens _npSsid (\s a -> s {_npSsid = a})

-- | The ARN of the network profile associated with a device.
npNetworkProfileARN :: Lens' NetworkProfile (Maybe Text)
npNetworkProfileARN = lens _npNetworkProfileARN (\s a -> s {_npNetworkProfileARN = a})

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
npSecurityType :: Lens' NetworkProfile (Maybe NetworkSecurityType)
npSecurityType = lens _npSecurityType (\s a -> s {_npSecurityType = a})

-- | The current password of the Wi-Fi network.
npCurrentPassword :: Lens' NetworkProfile (Maybe Text)
npCurrentPassword = lens _npCurrentPassword (\s a -> s {_npCurrentPassword = a}) . mapping _Sensitive

-- | The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
npNextPassword :: Lens' NetworkProfile (Maybe Text)
npNextPassword = lens _npNextPassword (\s a -> s {_npNextPassword = a}) . mapping _Sensitive

-- | The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
npEapMethod :: Lens' NetworkProfile (Maybe NetworkEapMethod)
npEapMethod = lens _npEapMethod (\s a -> s {_npEapMethod = a})

-- | Detailed information about a device's network profile.
npDescription :: Lens' NetworkProfile (Maybe Text)
npDescription = lens _npDescription (\s a -> s {_npDescription = a})

-- | The root certificates of your authentication server, which is installed on your devices and used to trust your authentication server during EAP negotiation.
npTrustAnchors :: Lens' NetworkProfile (Maybe (NonEmpty Text))
npTrustAnchors = lens _npTrustAnchors (\s a -> s {_npTrustAnchors = a}) . mapping _List1

-- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
npCertificateAuthorityARN :: Lens' NetworkProfile (Maybe Text)
npCertificateAuthorityARN = lens _npCertificateAuthorityARN (\s a -> s {_npCertificateAuthorityARN = a})

instance FromJSON NetworkProfile where
  parseJSON =
    withObject
      "NetworkProfile"
      ( \x ->
          NetworkProfile'
            <$> (x .:? "NetworkProfileName")
            <*> (x .:? "Ssid")
            <*> (x .:? "NetworkProfileArn")
            <*> (x .:? "SecurityType")
            <*> (x .:? "CurrentPassword")
            <*> (x .:? "NextPassword")
            <*> (x .:? "EapMethod")
            <*> (x .:? "Description")
            <*> (x .:? "TrustAnchors")
            <*> (x .:? "CertificateAuthorityArn")
      )

instance Hashable NetworkProfile

instance NFData NetworkProfile

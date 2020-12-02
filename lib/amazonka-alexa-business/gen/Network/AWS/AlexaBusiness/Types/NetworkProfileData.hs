{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkProfileData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.NetworkProfileData where

import Network.AWS.AlexaBusiness.Types.NetworkEapMethod
import Network.AWS.AlexaBusiness.Types.NetworkSecurityType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The data associated with a network profile.
--
--
--
-- /See:/ 'networkProfileData' smart constructor.
data NetworkProfileData = NetworkProfileData'
  { _npdNetworkProfileName ::
      !(Maybe Text),
    _npdSsid :: !(Maybe Text),
    _npdNetworkProfileARN :: !(Maybe Text),
    _npdSecurityType :: !(Maybe NetworkSecurityType),
    _npdEapMethod :: !(Maybe NetworkEapMethod),
    _npdDescription :: !(Maybe Text),
    _npdCertificateAuthorityARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkProfileData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npdNetworkProfileName' - The name of the network profile associated with a device.
--
-- * 'npdSsid' - The SSID of the Wi-Fi network.
--
-- * 'npdNetworkProfileARN' - The ARN of the network profile associated with a device.
--
-- * 'npdSecurityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- * 'npdEapMethod' - The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
--
-- * 'npdDescription' - Detailed information about a device's network profile.
--
-- * 'npdCertificateAuthorityARN' - The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
networkProfileData ::
  NetworkProfileData
networkProfileData =
  NetworkProfileData'
    { _npdNetworkProfileName = Nothing,
      _npdSsid = Nothing,
      _npdNetworkProfileARN = Nothing,
      _npdSecurityType = Nothing,
      _npdEapMethod = Nothing,
      _npdDescription = Nothing,
      _npdCertificateAuthorityARN = Nothing
    }

-- | The name of the network profile associated with a device.
npdNetworkProfileName :: Lens' NetworkProfileData (Maybe Text)
npdNetworkProfileName = lens _npdNetworkProfileName (\s a -> s {_npdNetworkProfileName = a})

-- | The SSID of the Wi-Fi network.
npdSsid :: Lens' NetworkProfileData (Maybe Text)
npdSsid = lens _npdSsid (\s a -> s {_npdSsid = a})

-- | The ARN of the network profile associated with a device.
npdNetworkProfileARN :: Lens' NetworkProfileData (Maybe Text)
npdNetworkProfileARN = lens _npdNetworkProfileARN (\s a -> s {_npdNetworkProfileARN = a})

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
npdSecurityType :: Lens' NetworkProfileData (Maybe NetworkSecurityType)
npdSecurityType = lens _npdSecurityType (\s a -> s {_npdSecurityType = a})

-- | The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
npdEapMethod :: Lens' NetworkProfileData (Maybe NetworkEapMethod)
npdEapMethod = lens _npdEapMethod (\s a -> s {_npdEapMethod = a})

-- | Detailed information about a device's network profile.
npdDescription :: Lens' NetworkProfileData (Maybe Text)
npdDescription = lens _npdDescription (\s a -> s {_npdDescription = a})

-- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
npdCertificateAuthorityARN :: Lens' NetworkProfileData (Maybe Text)
npdCertificateAuthorityARN = lens _npdCertificateAuthorityARN (\s a -> s {_npdCertificateAuthorityARN = a})

instance FromJSON NetworkProfileData where
  parseJSON =
    withObject
      "NetworkProfileData"
      ( \x ->
          NetworkProfileData'
            <$> (x .:? "NetworkProfileName")
            <*> (x .:? "Ssid")
            <*> (x .:? "NetworkProfileArn")
            <*> (x .:? "SecurityType")
            <*> (x .:? "EapMethod")
            <*> (x .:? "Description")
            <*> (x .:? "CertificateAuthorityArn")
      )

instance Hashable NetworkProfileData

instance NFData NetworkProfileData

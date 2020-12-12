{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkProfileData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.NetworkProfileData
  ( NetworkProfileData (..),

    -- * Smart constructor
    mkNetworkProfileData,

    -- * Lenses
    npdNetworkProfileName,
    npdSsid,
    npdNetworkProfileARN,
    npdSecurityType,
    npdEapMethod,
    npdDescription,
    npdCertificateAuthorityARN,
  )
where

import Network.AWS.AlexaBusiness.Types.NetworkEapMethod
import Network.AWS.AlexaBusiness.Types.NetworkSecurityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The data associated with a network profile.
--
-- /See:/ 'mkNetworkProfileData' smart constructor.
data NetworkProfileData = NetworkProfileData'
  { networkProfileName ::
      Lude.Maybe Lude.Text,
    ssid :: Lude.Maybe Lude.Text,
    networkProfileARN :: Lude.Maybe Lude.Text,
    securityType :: Lude.Maybe NetworkSecurityType,
    eapMethod :: Lude.Maybe NetworkEapMethod,
    description :: Lude.Maybe Lude.Text,
    certificateAuthorityARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkProfileData' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
-- * 'description' - Detailed information about a device's network profile.
-- * 'eapMethod' - The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
-- * 'networkProfileARN' - The ARN of the network profile associated with a device.
-- * 'networkProfileName' - The name of the network profile associated with a device.
-- * 'securityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
-- * 'ssid' - The SSID of the Wi-Fi network.
mkNetworkProfileData ::
  NetworkProfileData
mkNetworkProfileData =
  NetworkProfileData'
    { networkProfileName = Lude.Nothing,
      ssid = Lude.Nothing,
      networkProfileARN = Lude.Nothing,
      securityType = Lude.Nothing,
      eapMethod = Lude.Nothing,
      description = Lude.Nothing,
      certificateAuthorityARN = Lude.Nothing
    }

-- | The name of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npdNetworkProfileName :: Lens.Lens' NetworkProfileData (Lude.Maybe Lude.Text)
npdNetworkProfileName = Lens.lens (networkProfileName :: NetworkProfileData -> Lude.Maybe Lude.Text) (\s a -> s {networkProfileName = a} :: NetworkProfileData)
{-# DEPRECATED npdNetworkProfileName "Use generic-lens or generic-optics with 'networkProfileName' instead." #-}

-- | The SSID of the Wi-Fi network.
--
-- /Note:/ Consider using 'ssid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npdSsid :: Lens.Lens' NetworkProfileData (Lude.Maybe Lude.Text)
npdSsid = Lens.lens (ssid :: NetworkProfileData -> Lude.Maybe Lude.Text) (\s a -> s {ssid = a} :: NetworkProfileData)
{-# DEPRECATED npdSsid "Use generic-lens or generic-optics with 'ssid' instead." #-}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npdNetworkProfileARN :: Lens.Lens' NetworkProfileData (Lude.Maybe Lude.Text)
npdNetworkProfileARN = Lens.lens (networkProfileARN :: NetworkProfileData -> Lude.Maybe Lude.Text) (\s a -> s {networkProfileARN = a} :: NetworkProfileData)
{-# DEPRECATED npdNetworkProfileARN "Use generic-lens or generic-optics with 'networkProfileARN' instead." #-}

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- /Note:/ Consider using 'securityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npdSecurityType :: Lens.Lens' NetworkProfileData (Lude.Maybe NetworkSecurityType)
npdSecurityType = Lens.lens (securityType :: NetworkProfileData -> Lude.Maybe NetworkSecurityType) (\s a -> s {securityType = a} :: NetworkProfileData)
{-# DEPRECATED npdSecurityType "Use generic-lens or generic-optics with 'securityType' instead." #-}

-- | The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
--
-- /Note:/ Consider using 'eapMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npdEapMethod :: Lens.Lens' NetworkProfileData (Lude.Maybe NetworkEapMethod)
npdEapMethod = Lens.lens (eapMethod :: NetworkProfileData -> Lude.Maybe NetworkEapMethod) (\s a -> s {eapMethod = a} :: NetworkProfileData)
{-# DEPRECATED npdEapMethod "Use generic-lens or generic-optics with 'eapMethod' instead." #-}

-- | Detailed information about a device's network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npdDescription :: Lens.Lens' NetworkProfileData (Lude.Maybe Lude.Text)
npdDescription = Lens.lens (description :: NetworkProfileData -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: NetworkProfileData)
{-# DEPRECATED npdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npdCertificateAuthorityARN :: Lens.Lens' NetworkProfileData (Lude.Maybe Lude.Text)
npdCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: NetworkProfileData -> Lude.Maybe Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: NetworkProfileData)
{-# DEPRECATED npdCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.FromJSON NetworkProfileData where
  parseJSON =
    Lude.withObject
      "NetworkProfileData"
      ( \x ->
          NetworkProfileData'
            Lude.<$> (x Lude..:? "NetworkProfileName")
            Lude.<*> (x Lude..:? "Ssid")
            Lude.<*> (x Lude..:? "NetworkProfileArn")
            Lude.<*> (x Lude..:? "SecurityType")
            Lude.<*> (x Lude..:? "EapMethod")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "CertificateAuthorityArn")
      )

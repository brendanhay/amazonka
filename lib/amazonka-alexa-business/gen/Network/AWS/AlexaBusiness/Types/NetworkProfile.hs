{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.NetworkProfile
  ( NetworkProfile (..),

    -- * Smart constructor
    mkNetworkProfile,

    -- * Lenses
    npNetworkProfileName,
    npSsid,
    npNetworkProfileARN,
    npSecurityType,
    npCurrentPassword,
    npNextPassword,
    npEapMethod,
    npDescription,
    npTrustAnchors,
    npCertificateAuthorityARN,
  )
where

import Network.AWS.AlexaBusiness.Types.NetworkEapMethod
import Network.AWS.AlexaBusiness.Types.NetworkSecurityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The network profile associated with a device.
--
-- /See:/ 'mkNetworkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { networkProfileName ::
      Lude.Maybe Lude.Text,
    ssid :: Lude.Maybe Lude.Text,
    networkProfileARN :: Lude.Maybe Lude.Text,
    securityType :: Lude.Maybe NetworkSecurityType,
    currentPassword :: Lude.Maybe (Lude.Sensitive Lude.Text),
    nextPassword :: Lude.Maybe (Lude.Sensitive Lude.Text),
    eapMethod :: Lude.Maybe NetworkEapMethod,
    description :: Lude.Maybe Lude.Text,
    trustAnchors :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    certificateAuthorityARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkProfile' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
-- * 'currentPassword' - The current password of the Wi-Fi network.
-- * 'description' - Detailed information about a device's network profile.
-- * 'eapMethod' - The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
-- * 'networkProfileARN' - The ARN of the network profile associated with a device.
-- * 'networkProfileName' - The name of the network profile associated with a device.
-- * 'nextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
-- * 'securityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
-- * 'ssid' - The SSID of the Wi-Fi network.
-- * 'trustAnchors' - The root certificates of your authentication server, which is installed on your devices and used to trust your authentication server during EAP negotiation.
mkNetworkProfile ::
  NetworkProfile
mkNetworkProfile =
  NetworkProfile'
    { networkProfileName = Lude.Nothing,
      ssid = Lude.Nothing,
      networkProfileARN = Lude.Nothing,
      securityType = Lude.Nothing,
      currentPassword = Lude.Nothing,
      nextPassword = Lude.Nothing,
      eapMethod = Lude.Nothing,
      description = Lude.Nothing,
      trustAnchors = Lude.Nothing,
      certificateAuthorityARN = Lude.Nothing
    }

-- | The name of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNetworkProfileName :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Text)
npNetworkProfileName = Lens.lens (networkProfileName :: NetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {networkProfileName = a} :: NetworkProfile)
{-# DEPRECATED npNetworkProfileName "Use generic-lens or generic-optics with 'networkProfileName' instead." #-}

-- | The SSID of the Wi-Fi network.
--
-- /Note:/ Consider using 'ssid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npSsid :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Text)
npSsid = Lens.lens (ssid :: NetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {ssid = a} :: NetworkProfile)
{-# DEPRECATED npSsid "Use generic-lens or generic-optics with 'ssid' instead." #-}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNetworkProfileARN :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Text)
npNetworkProfileARN = Lens.lens (networkProfileARN :: NetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {networkProfileARN = a} :: NetworkProfile)
{-# DEPRECATED npNetworkProfileARN "Use generic-lens or generic-optics with 'networkProfileARN' instead." #-}

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- /Note:/ Consider using 'securityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npSecurityType :: Lens.Lens' NetworkProfile (Lude.Maybe NetworkSecurityType)
npSecurityType = Lens.lens (securityType :: NetworkProfile -> Lude.Maybe NetworkSecurityType) (\s a -> s {securityType = a} :: NetworkProfile)
{-# DEPRECATED npSecurityType "Use generic-lens or generic-optics with 'securityType' instead." #-}

-- | The current password of the Wi-Fi network.
--
-- /Note:/ Consider using 'currentPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npCurrentPassword :: Lens.Lens' NetworkProfile (Lude.Maybe (Lude.Sensitive Lude.Text))
npCurrentPassword = Lens.lens (currentPassword :: NetworkProfile -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {currentPassword = a} :: NetworkProfile)
{-# DEPRECATED npCurrentPassword "Use generic-lens or generic-optics with 'currentPassword' instead." #-}

-- | The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
--
-- /Note:/ Consider using 'nextPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNextPassword :: Lens.Lens' NetworkProfile (Lude.Maybe (Lude.Sensitive Lude.Text))
npNextPassword = Lens.lens (nextPassword :: NetworkProfile -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {nextPassword = a} :: NetworkProfile)
{-# DEPRECATED npNextPassword "Use generic-lens or generic-optics with 'nextPassword' instead." #-}

-- | The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
--
-- /Note:/ Consider using 'eapMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npEapMethod :: Lens.Lens' NetworkProfile (Lude.Maybe NetworkEapMethod)
npEapMethod = Lens.lens (eapMethod :: NetworkProfile -> Lude.Maybe NetworkEapMethod) (\s a -> s {eapMethod = a} :: NetworkProfile)
{-# DEPRECATED npEapMethod "Use generic-lens or generic-optics with 'eapMethod' instead." #-}

-- | Detailed information about a device's network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDescription :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Text)
npDescription = Lens.lens (description :: NetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: NetworkProfile)
{-# DEPRECATED npDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The root certificates of your authentication server, which is installed on your devices and used to trust your authentication server during EAP negotiation.
--
-- /Note:/ Consider using 'trustAnchors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npTrustAnchors :: Lens.Lens' NetworkProfile (Lude.Maybe (Lude.NonEmpty Lude.Text))
npTrustAnchors = Lens.lens (trustAnchors :: NetworkProfile -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {trustAnchors = a} :: NetworkProfile)
{-# DEPRECATED npTrustAnchors "Use generic-lens or generic-optics with 'trustAnchors' instead." #-}

-- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npCertificateAuthorityARN :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Text)
npCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: NetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: NetworkProfile)
{-# DEPRECATED npCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.FromJSON NetworkProfile where
  parseJSON =
    Lude.withObject
      "NetworkProfile"
      ( \x ->
          NetworkProfile'
            Lude.<$> (x Lude..:? "NetworkProfileName")
            Lude.<*> (x Lude..:? "Ssid")
            Lude.<*> (x Lude..:? "NetworkProfileArn")
            Lude.<*> (x Lude..:? "SecurityType")
            Lude.<*> (x Lude..:? "CurrentPassword")
            Lude.<*> (x Lude..:? "NextPassword")
            Lude.<*> (x Lude..:? "EapMethod")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "TrustAnchors")
            Lude.<*> (x Lude..:? "CertificateAuthorityArn")
      )

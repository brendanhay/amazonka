{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network profile with the specified details.
module Network.AWS.AlexaBusiness.CreateNetworkProfile
  ( -- * Creating a request
    CreateNetworkProfile (..),
    mkCreateNetworkProfile,

    -- ** Request lenses
    cnpNetworkProfileName,
    cnpSsid,
    cnpSecurityType,
    cnpCurrentPassword,
    cnpNextPassword,
    cnpEapMethod,
    cnpClientRequestToken,
    cnpDescription,
    cnpTrustAnchors,
    cnpCertificateAuthorityARN,

    -- * Destructuring the response
    CreateNetworkProfileResponse (..),
    mkCreateNetworkProfileResponse,

    -- ** Response lenses
    cnprsNetworkProfileARN,
    cnprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { -- | The name of the network profile associated with a device.
    networkProfileName :: Lude.Text,
    -- | The SSID of the Wi-Fi network.
    ssid :: Lude.Text,
    -- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
    securityType :: NetworkSecurityType,
    -- | The current password of the Wi-Fi network.
    currentPassword :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
    nextPassword :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
    eapMethod :: Lude.Maybe NetworkEapMethod,
    clientRequestToken :: Lude.Text,
    -- | Detailed information about a device's network profile.
    description :: Lude.Maybe Lude.Text,
    -- | The root certificates of your authentication server that is installed on your devices and used to trust your authentication server during EAP negotiation.
    trustAnchors :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
    certificateAuthorityARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkProfile' with the minimum fields required to make a request.
--
-- * 'networkProfileName' - The name of the network profile associated with a device.
-- * 'ssid' - The SSID of the Wi-Fi network.
-- * 'securityType' - The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
-- * 'currentPassword' - The current password of the Wi-Fi network.
-- * 'nextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
-- * 'eapMethod' - The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
-- * 'clientRequestToken' -
-- * 'description' - Detailed information about a device's network profile.
-- * 'trustAnchors' - The root certificates of your authentication server that is installed on your devices and used to trust your authentication server during EAP negotiation.
-- * 'certificateAuthorityARN' - The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
mkCreateNetworkProfile ::
  -- | 'networkProfileName'
  Lude.Text ->
  -- | 'ssid'
  Lude.Text ->
  -- | 'securityType'
  NetworkSecurityType ->
  -- | 'clientRequestToken'
  Lude.Text ->
  CreateNetworkProfile
mkCreateNetworkProfile
  pNetworkProfileName_
  pSsid_
  pSecurityType_
  pClientRequestToken_ =
    CreateNetworkProfile'
      { networkProfileName = pNetworkProfileName_,
        ssid = pSsid_,
        securityType = pSecurityType_,
        currentPassword = Lude.Nothing,
        nextPassword = Lude.Nothing,
        eapMethod = Lude.Nothing,
        clientRequestToken = pClientRequestToken_,
        description = Lude.Nothing,
        trustAnchors = Lude.Nothing,
        certificateAuthorityARN = Lude.Nothing
      }

-- | The name of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpNetworkProfileName :: Lens.Lens' CreateNetworkProfile Lude.Text
cnpNetworkProfileName = Lens.lens (networkProfileName :: CreateNetworkProfile -> Lude.Text) (\s a -> s {networkProfileName = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpNetworkProfileName "Use generic-lens or generic-optics with 'networkProfileName' instead." #-}

-- | The SSID of the Wi-Fi network.
--
-- /Note:/ Consider using 'ssid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpSsid :: Lens.Lens' CreateNetworkProfile Lude.Text
cnpSsid = Lens.lens (ssid :: CreateNetworkProfile -> Lude.Text) (\s a -> s {ssid = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpSsid "Use generic-lens or generic-optics with 'ssid' instead." #-}

-- | The security type of the Wi-Fi network. This can be WPA2_ENTERPRISE, WPA2_PSK, WPA_PSK, WEP, or OPEN.
--
-- /Note:/ Consider using 'securityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpSecurityType :: Lens.Lens' CreateNetworkProfile NetworkSecurityType
cnpSecurityType = Lens.lens (securityType :: CreateNetworkProfile -> NetworkSecurityType) (\s a -> s {securityType = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpSecurityType "Use generic-lens or generic-optics with 'securityType' instead." #-}

-- | The current password of the Wi-Fi network.
--
-- /Note:/ Consider using 'currentPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpCurrentPassword :: Lens.Lens' CreateNetworkProfile (Lude.Maybe (Lude.Sensitive Lude.Text))
cnpCurrentPassword = Lens.lens (currentPassword :: CreateNetworkProfile -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {currentPassword = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpCurrentPassword "Use generic-lens or generic-optics with 'currentPassword' instead." #-}

-- | The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
--
-- /Note:/ Consider using 'nextPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpNextPassword :: Lens.Lens' CreateNetworkProfile (Lude.Maybe (Lude.Sensitive Lude.Text))
cnpNextPassword = Lens.lens (nextPassword :: CreateNetworkProfile -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {nextPassword = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpNextPassword "Use generic-lens or generic-optics with 'nextPassword' instead." #-}

-- | The authentication standard that is used in the EAP framework. Currently, EAP_TLS is supported.
--
-- /Note:/ Consider using 'eapMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpEapMethod :: Lens.Lens' CreateNetworkProfile (Lude.Maybe NetworkEapMethod)
cnpEapMethod = Lens.lens (eapMethod :: CreateNetworkProfile -> Lude.Maybe NetworkEapMethod) (\s a -> s {eapMethod = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpEapMethod "Use generic-lens or generic-optics with 'eapMethod' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpClientRequestToken :: Lens.Lens' CreateNetworkProfile Lude.Text
cnpClientRequestToken = Lens.lens (clientRequestToken :: CreateNetworkProfile -> Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Detailed information about a device's network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpDescription :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Text)
cnpDescription = Lens.lens (description :: CreateNetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The root certificates of your authentication server that is installed on your devices and used to trust your authentication server during EAP negotiation.
--
-- /Note:/ Consider using 'trustAnchors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpTrustAnchors :: Lens.Lens' CreateNetworkProfile (Lude.Maybe (Lude.NonEmpty Lude.Text))
cnpTrustAnchors = Lens.lens (trustAnchors :: CreateNetworkProfile -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {trustAnchors = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpTrustAnchors "Use generic-lens or generic-optics with 'trustAnchors' instead." #-}

-- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnpCertificateAuthorityARN :: Lens.Lens' CreateNetworkProfile (Lude.Maybe Lude.Text)
cnpCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: CreateNetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: CreateNetworkProfile)
{-# DEPRECATED cnpCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest CreateNetworkProfile where
  type Rs CreateNetworkProfile = CreateNetworkProfileResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateNetworkProfileResponse'
            Lude.<$> (x Lude..?> "NetworkProfileArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNetworkProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.CreateNetworkProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateNetworkProfile where
  toJSON CreateNetworkProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("NetworkProfileName" Lude..= networkProfileName),
            Lude.Just ("Ssid" Lude..= ssid),
            Lude.Just ("SecurityType" Lude..= securityType),
            ("CurrentPassword" Lude..=) Lude.<$> currentPassword,
            ("NextPassword" Lude..=) Lude.<$> nextPassword,
            ("EapMethod" Lude..=) Lude.<$> eapMethod,
            Lude.Just ("ClientRequestToken" Lude..= clientRequestToken),
            ("Description" Lude..=) Lude.<$> description,
            ("TrustAnchors" Lude..=) Lude.<$> trustAnchors,
            ("CertificateAuthorityArn" Lude..=)
              Lude.<$> certificateAuthorityARN
          ]
      )

instance Lude.ToPath CreateNetworkProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNetworkProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateNetworkProfileResponse' smart constructor.
data CreateNetworkProfileResponse = CreateNetworkProfileResponse'
  { -- | The ARN of the network profile associated with a device.
    networkProfileARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkProfileResponse' with the minimum fields required to make a request.
--
-- * 'networkProfileARN' - The ARN of the network profile associated with a device.
-- * 'responseStatus' - The response status code.
mkCreateNetworkProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNetworkProfileResponse
mkCreateNetworkProfileResponse pResponseStatus_ =
  CreateNetworkProfileResponse'
    { networkProfileARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnprsNetworkProfileARN :: Lens.Lens' CreateNetworkProfileResponse (Lude.Maybe Lude.Text)
cnprsNetworkProfileARN = Lens.lens (networkProfileARN :: CreateNetworkProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {networkProfileARN = a} :: CreateNetworkProfileResponse)
{-# DEPRECATED cnprsNetworkProfileARN "Use generic-lens or generic-optics with 'networkProfileARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnprsResponseStatus :: Lens.Lens' CreateNetworkProfileResponse Lude.Int
cnprsResponseStatus = Lens.lens (responseStatus :: CreateNetworkProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNetworkProfileResponse)
{-# DEPRECATED cnprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

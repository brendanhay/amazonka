{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a network profile by the network profile ARN.
module Network.AWS.AlexaBusiness.UpdateNetworkProfile
  ( -- * Creating a request
    UpdateNetworkProfile (..),
    mkUpdateNetworkProfile,

    -- ** Request lenses
    unpNetworkProfileName,
    unpCurrentPassword,
    unpNextPassword,
    unpDescription,
    unpTrustAnchors,
    unpCertificateAuthorityARN,
    unpNetworkProfileARN,

    -- * Destructuring the response
    UpdateNetworkProfileResponse (..),
    mkUpdateNetworkProfileResponse,

    -- ** Response lenses
    unprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateNetworkProfile' smart constructor.
data UpdateNetworkProfile = UpdateNetworkProfile'
  { networkProfileName ::
      Lude.Maybe Lude.Text,
    currentPassword ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    nextPassword ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    description :: Lude.Maybe Lude.Text,
    trustAnchors ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    certificateAuthorityARN :: Lude.Maybe Lude.Text,
    networkProfileARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNetworkProfile' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
-- * 'currentPassword' - The current password of the Wi-Fi network.
-- * 'description' - Detailed information about a device's network profile.
-- * 'networkProfileARN' - The ARN of the network profile associated with a device.
-- * 'networkProfileName' - The name of the network profile associated with a device.
-- * 'nextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
-- * 'trustAnchors' - The root certificate(s) of your authentication server that will be installed on your devices and used to trust your authentication server during EAP negotiation.
mkUpdateNetworkProfile ::
  -- | 'networkProfileARN'
  Lude.Text ->
  UpdateNetworkProfile
mkUpdateNetworkProfile pNetworkProfileARN_ =
  UpdateNetworkProfile'
    { networkProfileName = Lude.Nothing,
      currentPassword = Lude.Nothing,
      nextPassword = Lude.Nothing,
      description = Lude.Nothing,
      trustAnchors = Lude.Nothing,
      certificateAuthorityARN = Lude.Nothing,
      networkProfileARN = pNetworkProfileARN_
    }

-- | The name of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpNetworkProfileName :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Text)
unpNetworkProfileName = Lens.lens (networkProfileName :: UpdateNetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {networkProfileName = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpNetworkProfileName "Use generic-lens or generic-optics with 'networkProfileName' instead." #-}

-- | The current password of the Wi-Fi network.
--
-- /Note:/ Consider using 'currentPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpCurrentPassword :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe (Lude.Sensitive Lude.Text))
unpCurrentPassword = Lens.lens (currentPassword :: UpdateNetworkProfile -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {currentPassword = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpCurrentPassword "Use generic-lens or generic-optics with 'currentPassword' instead." #-}

-- | The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
--
-- /Note:/ Consider using 'nextPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpNextPassword :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe (Lude.Sensitive Lude.Text))
unpNextPassword = Lens.lens (nextPassword :: UpdateNetworkProfile -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {nextPassword = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpNextPassword "Use generic-lens or generic-optics with 'nextPassword' instead." #-}

-- | Detailed information about a device's network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpDescription :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Text)
unpDescription = Lens.lens (description :: UpdateNetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The root certificate(s) of your authentication server that will be installed on your devices and used to trust your authentication server during EAP negotiation.
--
-- /Note:/ Consider using 'trustAnchors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpTrustAnchors :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe (Lude.NonEmpty Lude.Text))
unpTrustAnchors = Lens.lens (trustAnchors :: UpdateNetworkProfile -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {trustAnchors = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpTrustAnchors "Use generic-lens or generic-optics with 'trustAnchors' instead." #-}

-- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpCertificateAuthorityARN :: Lens.Lens' UpdateNetworkProfile (Lude.Maybe Lude.Text)
unpCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: UpdateNetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unpNetworkProfileARN :: Lens.Lens' UpdateNetworkProfile Lude.Text
unpNetworkProfileARN = Lens.lens (networkProfileARN :: UpdateNetworkProfile -> Lude.Text) (\s a -> s {networkProfileARN = a} :: UpdateNetworkProfile)
{-# DEPRECATED unpNetworkProfileARN "Use generic-lens or generic-optics with 'networkProfileARN' instead." #-}

instance Lude.AWSRequest UpdateNetworkProfile where
  type Rs UpdateNetworkProfile = UpdateNetworkProfileResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateNetworkProfileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateNetworkProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.UpdateNetworkProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateNetworkProfile where
  toJSON UpdateNetworkProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NetworkProfileName" Lude..=) Lude.<$> networkProfileName,
            ("CurrentPassword" Lude..=) Lude.<$> currentPassword,
            ("NextPassword" Lude..=) Lude.<$> nextPassword,
            ("Description" Lude..=) Lude.<$> description,
            ("TrustAnchors" Lude..=) Lude.<$> trustAnchors,
            ("CertificateAuthorityArn" Lude..=)
              Lude.<$> certificateAuthorityARN,
            Lude.Just ("NetworkProfileArn" Lude..= networkProfileARN)
          ]
      )

instance Lude.ToPath UpdateNetworkProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateNetworkProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateNetworkProfileResponse' smart constructor.
newtype UpdateNetworkProfileResponse = UpdateNetworkProfileResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNetworkProfileResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateNetworkProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateNetworkProfileResponse
mkUpdateNetworkProfileResponse pResponseStatus_ =
  UpdateNetworkProfileResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unprsResponseStatus :: Lens.Lens' UpdateNetworkProfileResponse Lude.Int
unprsResponseStatus = Lens.lens (responseStatus :: UpdateNetworkProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateNetworkProfileResponse)
{-# DEPRECATED unprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms tracking of the device. This API call is the call that begins device tracking.
module Network.AWS.CognitoIdentityProvider.ConfirmDevice
  ( -- * Creating a request
    ConfirmDevice (..),
    mkConfirmDevice,

    -- ** Request lenses
    cdDeviceSecretVerifierConfig,
    cdDeviceName,
    cdAccessToken,
    cdDeviceKey,

    -- * Destructuring the response
    ConfirmDeviceResponse (..),
    mkConfirmDeviceResponse,

    -- ** Response lenses
    cdrsUserConfirmationNecessary,
    cdrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Confirms the device request.
--
-- /See:/ 'mkConfirmDevice' smart constructor.
data ConfirmDevice = ConfirmDevice'
  { deviceSecretVerifierConfig ::
      Lude.Maybe DeviceSecretVerifierConfigType,
    deviceName :: Lude.Maybe Lude.Text,
    accessToken :: Lude.Sensitive Lude.Text,
    deviceKey :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmDevice' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token.
-- * 'deviceKey' - The device key.
-- * 'deviceName' - The device name.
-- * 'deviceSecretVerifierConfig' - The configuration of the device secret verifier.
mkConfirmDevice ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  -- | 'deviceKey'
  Lude.Text ->
  ConfirmDevice
mkConfirmDevice pAccessToken_ pDeviceKey_ =
  ConfirmDevice'
    { deviceSecretVerifierConfig = Lude.Nothing,
      deviceName = Lude.Nothing,
      accessToken = pAccessToken_,
      deviceKey = pDeviceKey_
    }

-- | The configuration of the device secret verifier.
--
-- /Note:/ Consider using 'deviceSecretVerifierConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeviceSecretVerifierConfig :: Lens.Lens' ConfirmDevice (Lude.Maybe DeviceSecretVerifierConfigType)
cdDeviceSecretVerifierConfig = Lens.lens (deviceSecretVerifierConfig :: ConfirmDevice -> Lude.Maybe DeviceSecretVerifierConfigType) (\s a -> s {deviceSecretVerifierConfig = a} :: ConfirmDevice)
{-# DEPRECATED cdDeviceSecretVerifierConfig "Use generic-lens or generic-optics with 'deviceSecretVerifierConfig' instead." #-}

-- | The device name.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeviceName :: Lens.Lens' ConfirmDevice (Lude.Maybe Lude.Text)
cdDeviceName = Lens.lens (deviceName :: ConfirmDevice -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: ConfirmDevice)
{-# DEPRECATED cdDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAccessToken :: Lens.Lens' ConfirmDevice (Lude.Sensitive Lude.Text)
cdAccessToken = Lens.lens (accessToken :: ConfirmDevice -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: ConfirmDevice)
{-# DEPRECATED cdAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeviceKey :: Lens.Lens' ConfirmDevice Lude.Text
cdDeviceKey = Lens.lens (deviceKey :: ConfirmDevice -> Lude.Text) (\s a -> s {deviceKey = a} :: ConfirmDevice)
{-# DEPRECATED cdDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

instance Lude.AWSRequest ConfirmDevice where
  type Rs ConfirmDevice = ConfirmDeviceResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ConfirmDeviceResponse'
            Lude.<$> (x Lude..?> "UserConfirmationNecessary")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfirmDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ConfirmDevice" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ConfirmDevice where
  toJSON ConfirmDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeviceSecretVerifierConfig" Lude..=)
              Lude.<$> deviceSecretVerifierConfig,
            ("DeviceName" Lude..=) Lude.<$> deviceName,
            Lude.Just ("AccessToken" Lude..= accessToken),
            Lude.Just ("DeviceKey" Lude..= deviceKey)
          ]
      )

instance Lude.ToPath ConfirmDevice where
  toPath = Lude.const "/"

instance Lude.ToQuery ConfirmDevice where
  toQuery = Lude.const Lude.mempty

-- | Confirms the device response.
--
-- /See:/ 'mkConfirmDeviceResponse' smart constructor.
data ConfirmDeviceResponse = ConfirmDeviceResponse'
  { userConfirmationNecessary ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmDeviceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'userConfirmationNecessary' - Indicates whether the user confirmation is necessary to confirm the device response.
mkConfirmDeviceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfirmDeviceResponse
mkConfirmDeviceResponse pResponseStatus_ =
  ConfirmDeviceResponse'
    { userConfirmationNecessary = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether the user confirmation is necessary to confirm the device response.
--
-- /Note:/ Consider using 'userConfirmationNecessary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsUserConfirmationNecessary :: Lens.Lens' ConfirmDeviceResponse (Lude.Maybe Lude.Bool)
cdrsUserConfirmationNecessary = Lens.lens (userConfirmationNecessary :: ConfirmDeviceResponse -> Lude.Maybe Lude.Bool) (\s a -> s {userConfirmationNecessary = a} :: ConfirmDeviceResponse)
{-# DEPRECATED cdrsUserConfirmationNecessary "Use generic-lens or generic-optics with 'userConfirmationNecessary' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' ConfirmDeviceResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: ConfirmDeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfirmDeviceResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

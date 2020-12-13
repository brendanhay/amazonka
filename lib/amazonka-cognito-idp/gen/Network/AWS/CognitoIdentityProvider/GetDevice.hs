{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the device.
module Network.AWS.CognitoIdentityProvider.GetDevice
  ( -- * Creating a request
    GetDevice (..),
    mkGetDevice,

    -- ** Request lenses
    gdAccessToken,
    gdDeviceKey,

    -- * Destructuring the response
    GetDeviceResponse (..),
    mkGetDeviceResponse,

    -- ** Response lenses
    gdrsDevice,
    gdrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to get the device.
--
-- /See:/ 'mkGetDevice' smart constructor.
data GetDevice = GetDevice'
  { -- | The access token.
    accessToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The device key.
    deviceKey :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDevice' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token.
-- * 'deviceKey' - The device key.
mkGetDevice ::
  -- | 'deviceKey'
  Lude.Text ->
  GetDevice
mkGetDevice pDeviceKey_ =
  GetDevice' {accessToken = Lude.Nothing, deviceKey = pDeviceKey_}

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAccessToken :: Lens.Lens' GetDevice (Lude.Maybe (Lude.Sensitive Lude.Text))
gdAccessToken = Lens.lens (accessToken :: GetDevice -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {accessToken = a} :: GetDevice)
{-# DEPRECATED gdAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDeviceKey :: Lens.Lens' GetDevice Lude.Text
gdDeviceKey = Lens.lens (deviceKey :: GetDevice -> Lude.Text) (\s a -> s {deviceKey = a} :: GetDevice)
{-# DEPRECATED gdDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

instance Lude.AWSRequest GetDevice where
  type Rs GetDevice = GetDeviceResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeviceResponse'
            Lude.<$> (x Lude..:> "Device") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityProviderService.GetDevice" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDevice where
  toJSON GetDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AccessToken" Lude..=) Lude.<$> accessToken,
            Lude.Just ("DeviceKey" Lude..= deviceKey)
          ]
      )

instance Lude.ToPath GetDevice where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDevice where
  toQuery = Lude.const Lude.mempty

-- | Gets the device response.
--
-- /See:/ 'mkGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { -- | The device.
    device :: DeviceType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeviceResponse' with the minimum fields required to make a request.
--
-- * 'device' - The device.
-- * 'responseStatus' - The response status code.
mkGetDeviceResponse ::
  -- | 'device'
  DeviceType ->
  -- | 'responseStatus'
  Lude.Int ->
  GetDeviceResponse
mkGetDeviceResponse pDevice_ pResponseStatus_ =
  GetDeviceResponse'
    { device = pDevice_,
      responseStatus = pResponseStatus_
    }

-- | The device.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDevice :: Lens.Lens' GetDeviceResponse DeviceType
gdrsDevice = Lens.lens (device :: GetDeviceResponse -> DeviceType) (\s a -> s {device = a} :: GetDeviceResponse)
{-# DEPRECATED gdrsDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDeviceResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeviceResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

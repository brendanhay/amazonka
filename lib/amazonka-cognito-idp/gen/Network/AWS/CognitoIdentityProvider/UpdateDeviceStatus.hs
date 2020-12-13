{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device status.
module Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
  ( -- * Creating a request
    UpdateDeviceStatus (..),
    mkUpdateDeviceStatus,

    -- ** Request lenses
    udsAccessToken,
    udsDeviceRememberedStatus,
    udsDeviceKey,

    -- * Destructuring the response
    UpdateDeviceStatusResponse (..),
    mkUpdateDeviceStatusResponse,

    -- ** Response lenses
    udsrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to update the device status.
--
-- /See:/ 'mkUpdateDeviceStatus' smart constructor.
data UpdateDeviceStatus = UpdateDeviceStatus'
  { -- | The access token.
    accessToken :: Lude.Sensitive Lude.Text,
    -- | The status of whether a device is remembered.
    deviceRememberedStatus :: Lude.Maybe DeviceRememberedStatusType,
    -- | The device key.
    deviceKey :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeviceStatus' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token.
-- * 'deviceRememberedStatus' - The status of whether a device is remembered.
-- * 'deviceKey' - The device key.
mkUpdateDeviceStatus ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  -- | 'deviceKey'
  Lude.Text ->
  UpdateDeviceStatus
mkUpdateDeviceStatus pAccessToken_ pDeviceKey_ =
  UpdateDeviceStatus'
    { accessToken = pAccessToken_,
      deviceRememberedStatus = Lude.Nothing,
      deviceKey = pDeviceKey_
    }

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsAccessToken :: Lens.Lens' UpdateDeviceStatus (Lude.Sensitive Lude.Text)
udsAccessToken = Lens.lens (accessToken :: UpdateDeviceStatus -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: UpdateDeviceStatus)
{-# DEPRECATED udsAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The status of whether a device is remembered.
--
-- /Note:/ Consider using 'deviceRememberedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDeviceRememberedStatus :: Lens.Lens' UpdateDeviceStatus (Lude.Maybe DeviceRememberedStatusType)
udsDeviceRememberedStatus = Lens.lens (deviceRememberedStatus :: UpdateDeviceStatus -> Lude.Maybe DeviceRememberedStatusType) (\s a -> s {deviceRememberedStatus = a} :: UpdateDeviceStatus)
{-# DEPRECATED udsDeviceRememberedStatus "Use generic-lens or generic-optics with 'deviceRememberedStatus' instead." #-}

-- | The device key.
--
-- /Note:/ Consider using 'deviceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDeviceKey :: Lens.Lens' UpdateDeviceStatus Lude.Text
udsDeviceKey = Lens.lens (deviceKey :: UpdateDeviceStatus -> Lude.Text) (\s a -> s {deviceKey = a} :: UpdateDeviceStatus)
{-# DEPRECATED udsDeviceKey "Use generic-lens or generic-optics with 'deviceKey' instead." #-}

instance Lude.AWSRequest UpdateDeviceStatus where
  type Rs UpdateDeviceStatus = UpdateDeviceStatusResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateDeviceStatusResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDeviceStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.UpdateDeviceStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDeviceStatus where
  toJSON UpdateDeviceStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccessToken" Lude..= accessToken),
            ("DeviceRememberedStatus" Lude..=) Lude.<$> deviceRememberedStatus,
            Lude.Just ("DeviceKey" Lude..= deviceKey)
          ]
      )

instance Lude.ToPath UpdateDeviceStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDeviceStatus where
  toQuery = Lude.const Lude.mempty

-- | The response to the request to update the device status.
--
-- /See:/ 'mkUpdateDeviceStatusResponse' smart constructor.
newtype UpdateDeviceStatusResponse = UpdateDeviceStatusResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeviceStatusResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateDeviceStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDeviceStatusResponse
mkUpdateDeviceStatusResponse pResponseStatus_ =
  UpdateDeviceStatusResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrsResponseStatus :: Lens.Lens' UpdateDeviceStatusResponse Lude.Int
udsrsResponseStatus = Lens.lens (responseStatus :: UpdateDeviceStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDeviceStatusResponse)
{-# DEPRECATED udsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

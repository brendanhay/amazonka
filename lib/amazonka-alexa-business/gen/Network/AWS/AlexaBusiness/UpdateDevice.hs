{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device name by device ARN.
module Network.AWS.AlexaBusiness.UpdateDevice
  ( -- * Creating a request
    UpdateDevice (..),
    mkUpdateDevice,

    -- ** Request lenses
    udDeviceARN,
    udDeviceName,

    -- * Destructuring the response
    UpdateDeviceResponse (..),
    mkUpdateDeviceResponse,

    -- ** Response lenses
    udrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDevice' smart constructor.
data UpdateDevice = UpdateDevice'
  { -- | The ARN of the device to update. Required.
    deviceARN :: Lude.Maybe Lude.Text,
    -- | The updated device name. Required.
    deviceName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDevice' with the minimum fields required to make a request.
--
-- * 'deviceARN' - The ARN of the device to update. Required.
-- * 'deviceName' - The updated device name. Required.
mkUpdateDevice ::
  UpdateDevice
mkUpdateDevice =
  UpdateDevice'
    { deviceARN = Lude.Nothing,
      deviceName = Lude.Nothing
    }

-- | The ARN of the device to update. Required.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDeviceARN :: Lens.Lens' UpdateDevice (Lude.Maybe Lude.Text)
udDeviceARN = Lens.lens (deviceARN :: UpdateDevice -> Lude.Maybe Lude.Text) (\s a -> s {deviceARN = a} :: UpdateDevice)
{-# DEPRECATED udDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

-- | The updated device name. Required.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDeviceName :: Lens.Lens' UpdateDevice (Lude.Maybe Lude.Text)
udDeviceName = Lens.lens (deviceName :: UpdateDevice -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: UpdateDevice)
{-# DEPRECATED udDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

instance Lude.AWSRequest UpdateDevice where
  type Rs UpdateDevice = UpdateDeviceResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateDeviceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.UpdateDevice" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDevice where
  toJSON UpdateDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeviceArn" Lude..=) Lude.<$> deviceARN,
            ("DeviceName" Lude..=) Lude.<$> deviceName
          ]
      )

instance Lude.ToPath UpdateDevice where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDevice where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDeviceResponse' smart constructor.
newtype UpdateDeviceResponse = UpdateDeviceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeviceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateDeviceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDeviceResponse
mkUpdateDeviceResponse pResponseStatus_ =
  UpdateDeviceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UpdateDeviceResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UpdateDeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDeviceResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

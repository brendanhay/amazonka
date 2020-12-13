{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a device by device ARN.
module Network.AWS.AlexaBusiness.GetDevice
  ( -- * Creating a request
    GetDevice (..),
    mkGetDevice,

    -- ** Request lenses
    gdDeviceARN,

    -- * Destructuring the response
    GetDeviceResponse (..),
    mkGetDeviceResponse,

    -- ** Response lenses
    gdrsDevice,
    gdrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDevice' smart constructor.
newtype GetDevice = GetDevice'
  { -- | The ARN of the device for which to request details. Required.
    deviceARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDevice' with the minimum fields required to make a request.
--
-- * 'deviceARN' - The ARN of the device for which to request details. Required.
mkGetDevice ::
  GetDevice
mkGetDevice = GetDevice' {deviceARN = Lude.Nothing}

-- | The ARN of the device for which to request details. Required.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDeviceARN :: Lens.Lens' GetDevice (Lude.Maybe Lude.Text)
gdDeviceARN = Lens.lens (deviceARN :: GetDevice -> Lude.Maybe Lude.Text) (\s a -> s {deviceARN = a} :: GetDevice)
{-# DEPRECATED gdDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

instance Lude.AWSRequest GetDevice where
  type Rs GetDevice = GetDeviceResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeviceResponse'
            Lude.<$> (x Lude..?> "Device") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetDevice" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDevice where
  toJSON GetDevice' {..} =
    Lude.object
      (Lude.catMaybes [("DeviceArn" Lude..=) Lude.<$> deviceARN])

instance Lude.ToPath GetDevice where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDevice where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { -- | The details of the device requested. Required.
    device :: Lude.Maybe Device,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeviceResponse' with the minimum fields required to make a request.
--
-- * 'device' - The details of the device requested. Required.
-- * 'responseStatus' - The response status code.
mkGetDeviceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeviceResponse
mkGetDeviceResponse pResponseStatus_ =
  GetDeviceResponse'
    { device = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the device requested. Required.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDevice :: Lens.Lens' GetDeviceResponse (Lude.Maybe Device)
gdrsDevice = Lens.lens (device :: GetDeviceResponse -> Lude.Maybe Device) (\s a -> s {device = a} :: GetDeviceResponse)
{-# DEPRECATED gdrsDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDeviceResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeviceResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

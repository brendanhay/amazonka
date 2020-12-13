{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a unique device type.
module Network.AWS.DeviceFarm.GetDevice
  ( -- * Creating a request
    GetDevice (..),
    mkGetDevice,

    -- ** Request lenses
    gdArn,

    -- * Destructuring the response
    GetDeviceResponse (..),
    mkGetDeviceResponse,

    -- ** Response lenses
    gdrsDevice,
    gdrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the get device request.
--
-- /See:/ 'mkGetDevice' smart constructor.
newtype GetDevice = GetDevice'
  { -- | The device type's ARN.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDevice' with the minimum fields required to make a request.
--
-- * 'arn' - The device type's ARN.
mkGetDevice ::
  -- | 'arn'
  Lude.Text ->
  GetDevice
mkGetDevice pArn_ = GetDevice' {arn = pArn_}

-- | The device type's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdArn :: Lens.Lens' GetDevice Lude.Text
gdArn = Lens.lens (arn :: GetDevice -> Lude.Text) (\s a -> s {arn = a} :: GetDevice)
{-# DEPRECATED gdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetDevice where
  type Rs GetDevice = GetDeviceResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeviceResponse'
            Lude.<$> (x Lude..?> "device") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetDevice" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDevice where
  toJSON GetDevice' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetDevice where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDevice where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a get device request.
--
-- /See:/ 'mkGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { -- | An object that contains information about the requested device.
    device :: Lude.Maybe Device,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeviceResponse' with the minimum fields required to make a request.
--
-- * 'device' - An object that contains information about the requested device.
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

-- | An object that contains information about the requested device.
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

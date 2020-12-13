{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetDevicePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a device pool.
module Network.AWS.DeviceFarm.GetDevicePool
  ( -- * Creating a request
    GetDevicePool (..),
    mkGetDevicePool,

    -- ** Request lenses
    gdpArn,

    -- * Destructuring the response
    GetDevicePoolResponse (..),
    mkGetDevicePoolResponse,

    -- ** Response lenses
    gdprsDevicePool,
    gdprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the get device pool operation.
--
-- /See:/ 'mkGetDevicePool' smart constructor.
newtype GetDevicePool = GetDevicePool'
  { -- | The device pool's ARN.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDevicePool' with the minimum fields required to make a request.
--
-- * 'arn' - The device pool's ARN.
mkGetDevicePool ::
  -- | 'arn'
  Lude.Text ->
  GetDevicePool
mkGetDevicePool pArn_ = GetDevicePool' {arn = pArn_}

-- | The device pool's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpArn :: Lens.Lens' GetDevicePool Lude.Text
gdpArn = Lens.lens (arn :: GetDevicePool -> Lude.Text) (\s a -> s {arn = a} :: GetDevicePool)
{-# DEPRECATED gdpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetDevicePool where
  type Rs GetDevicePool = GetDevicePoolResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDevicePoolResponse'
            Lude.<$> (x Lude..?> "devicePool") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDevicePool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetDevicePool" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDevicePool where
  toJSON GetDevicePool' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetDevicePool where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDevicePool where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a get device pool request.
--
-- /See:/ 'mkGetDevicePoolResponse' smart constructor.
data GetDevicePoolResponse = GetDevicePoolResponse'
  { -- | An object that contains information about the requested device pool.
    devicePool :: Lude.Maybe DevicePool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDevicePoolResponse' with the minimum fields required to make a request.
--
-- * 'devicePool' - An object that contains information about the requested device pool.
-- * 'responseStatus' - The response status code.
mkGetDevicePoolResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDevicePoolResponse
mkGetDevicePoolResponse pResponseStatus_ =
  GetDevicePoolResponse'
    { devicePool = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about the requested device pool.
--
-- /Note:/ Consider using 'devicePool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprsDevicePool :: Lens.Lens' GetDevicePoolResponse (Lude.Maybe DevicePool)
gdprsDevicePool = Lens.lens (devicePool :: GetDevicePoolResponse -> Lude.Maybe DevicePool) (\s a -> s {devicePool = a} :: GetDevicePoolResponse)
{-# DEPRECATED gdprsDevicePool "Use generic-lens or generic-optics with 'devicePool' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprsResponseStatus :: Lens.Lens' GetDevicePoolResponse Lude.Int
gdprsResponseStatus = Lens.lens (responseStatus :: GetDevicePoolResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDevicePoolResponse)
{-# DEPRECATED gdprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

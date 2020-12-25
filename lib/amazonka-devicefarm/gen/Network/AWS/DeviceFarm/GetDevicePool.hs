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
    gdprrsDevicePool,
    gdprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get device pool operation.
--
-- /See:/ 'mkGetDevicePool' smart constructor.
newtype GetDevicePool = GetDevicePool'
  { -- | The device pool's ARN.
    arn :: Types.AmazonResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevicePool' value with any optional fields omitted.
mkGetDevicePool ::
  -- | 'arn'
  Types.AmazonResourceName ->
  GetDevicePool
mkGetDevicePool arn = GetDevicePool' {arn}

-- | The device pool's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpArn :: Lens.Lens' GetDevicePool Types.AmazonResourceName
gdpArn = Lens.field @"arn"
{-# DEPRECATED gdpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON GetDevicePool where
  toJSON GetDevicePool {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetDevicePool where
  type Rs GetDevicePool = GetDevicePoolResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetDevicePool")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevicePoolResponse'
            Core.<$> (x Core..:? "devicePool") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of a get device pool request.
--
-- /See:/ 'mkGetDevicePoolResponse' smart constructor.
data GetDevicePoolResponse = GetDevicePoolResponse'
  { -- | An object that contains information about the requested device pool.
    devicePool :: Core.Maybe Types.DevicePool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevicePoolResponse' value with any optional fields omitted.
mkGetDevicePoolResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDevicePoolResponse
mkGetDevicePoolResponse responseStatus =
  GetDevicePoolResponse' {devicePool = Core.Nothing, responseStatus}

-- | An object that contains information about the requested device pool.
--
-- /Note:/ Consider using 'devicePool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprrsDevicePool :: Lens.Lens' GetDevicePoolResponse (Core.Maybe Types.DevicePool)
gdprrsDevicePool = Lens.field @"devicePool"
{-# DEPRECATED gdprrsDevicePool "Use generic-lens or generic-optics with 'devicePool' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprrsResponseStatus :: Lens.Lens' GetDevicePoolResponse Core.Int
gdprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

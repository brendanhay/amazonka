{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetDevicePool (..)
    , mkGetDevicePool
    -- ** Request lenses
    , gdpArn

    -- * Destructuring the response
    , GetDevicePoolResponse (..)
    , mkGetDevicePoolResponse
    -- ** Response lenses
    , gdprrsDevicePool
    , gdprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get device pool operation.
--
-- /See:/ 'mkGetDevicePool' smart constructor.
newtype GetDevicePool = GetDevicePool'
  { arn :: Types.AmazonResourceName
    -- ^ The device pool's ARN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevicePool' value with any optional fields omitted.
mkGetDevicePool
    :: Types.AmazonResourceName -- ^ 'arn'
    -> GetDevicePool
mkGetDevicePool arn = GetDevicePool'{arn}

-- | The device pool's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpArn :: Lens.Lens' GetDevicePool Types.AmazonResourceName
gdpArn = Lens.field @"arn"
{-# INLINEABLE gdpArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery GetDevicePool where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDevicePool where
        toHeaders GetDevicePool{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetDevicePool")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDevicePool where
        toJSON GetDevicePool{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetDevicePool where
        type Rs GetDevicePool = GetDevicePoolResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDevicePoolResponse' Core.<$>
                   (x Core..:? "devicePool") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of a get device pool request.
--
-- /See:/ 'mkGetDevicePoolResponse' smart constructor.
data GetDevicePoolResponse = GetDevicePoolResponse'
  { devicePool :: Core.Maybe Types.DevicePool
    -- ^ An object that contains information about the requested device pool.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevicePoolResponse' value with any optional fields omitted.
mkGetDevicePoolResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDevicePoolResponse
mkGetDevicePoolResponse responseStatus
  = GetDevicePoolResponse'{devicePool = Core.Nothing, responseStatus}

-- | An object that contains information about the requested device pool.
--
-- /Note:/ Consider using 'devicePool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprrsDevicePool :: Lens.Lens' GetDevicePoolResponse (Core.Maybe Types.DevicePool)
gdprrsDevicePool = Lens.field @"devicePool"
{-# INLINEABLE gdprrsDevicePool #-}
{-# DEPRECATED devicePool "Use generic-lens or generic-optics with 'devicePool' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprrsResponseStatus :: Lens.Lens' GetDevicePoolResponse Core.Int
gdprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

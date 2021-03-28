{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetDeviceInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a device instance that belongs to a private device fleet.
module Network.AWS.DeviceFarm.GetDeviceInstance
    (
    -- * Creating a request
      GetDeviceInstance (..)
    , mkGetDeviceInstance
    -- ** Request lenses
    , gdiArn

    -- * Destructuring the response
    , GetDeviceInstanceResponse (..)
    , mkGetDeviceInstanceResponse
    -- ** Response lenses
    , gdirrsDeviceInstance
    , gdirrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDeviceInstance' smart constructor.
newtype GetDeviceInstance = GetDeviceInstance'
  { arn :: Types.AmazonResourceName
    -- ^ The Amazon Resource Name (ARN) of the instance you're requesting information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeviceInstance' value with any optional fields omitted.
mkGetDeviceInstance
    :: Types.AmazonResourceName -- ^ 'arn'
    -> GetDeviceInstance
mkGetDeviceInstance arn = GetDeviceInstance'{arn}

-- | The Amazon Resource Name (ARN) of the instance you're requesting information about.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdiArn :: Lens.Lens' GetDeviceInstance Types.AmazonResourceName
gdiArn = Lens.field @"arn"
{-# INLINEABLE gdiArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery GetDeviceInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDeviceInstance where
        toHeaders GetDeviceInstance{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.GetDeviceInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDeviceInstance where
        toJSON GetDeviceInstance{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetDeviceInstance where
        type Rs GetDeviceInstance = GetDeviceInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDeviceInstanceResponse' Core.<$>
                   (x Core..:? "deviceInstance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDeviceInstanceResponse' smart constructor.
data GetDeviceInstanceResponse = GetDeviceInstanceResponse'
  { deviceInstance :: Core.Maybe Types.DeviceInstance
    -- ^ An object that contains information about your device instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeviceInstanceResponse' value with any optional fields omitted.
mkGetDeviceInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDeviceInstanceResponse
mkGetDeviceInstanceResponse responseStatus
  = GetDeviceInstanceResponse'{deviceInstance = Core.Nothing,
                               responseStatus}

-- | An object that contains information about your device instance.
--
-- /Note:/ Consider using 'deviceInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdirrsDeviceInstance :: Lens.Lens' GetDeviceInstanceResponse (Core.Maybe Types.DeviceInstance)
gdirrsDeviceInstance = Lens.field @"deviceInstance"
{-# INLINEABLE gdirrsDeviceInstance #-}
{-# DEPRECATED deviceInstance "Use generic-lens or generic-optics with 'deviceInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdirrsResponseStatus :: Lens.Lens' GetDeviceInstanceResponse Core.Int
gdirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

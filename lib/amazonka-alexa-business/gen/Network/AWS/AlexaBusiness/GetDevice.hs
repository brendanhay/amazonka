{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetDevice (..)
    , mkGetDevice
    -- ** Request lenses
    , gdDeviceArn

    -- * Destructuring the response
    , GetDeviceResponse (..)
    , mkGetDeviceResponse
    -- ** Response lenses
    , gdrrsDevice
    , gdrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDevice' smart constructor.
newtype GetDevice = GetDevice'
  { deviceArn :: Core.Maybe Types.DeviceArn
    -- ^ The ARN of the device for which to request details. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevice' value with any optional fields omitted.
mkGetDevice
    :: GetDevice
mkGetDevice = GetDevice'{deviceArn = Core.Nothing}

-- | The ARN of the device for which to request details. Required.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDeviceArn :: Lens.Lens' GetDevice (Core.Maybe Types.DeviceArn)
gdDeviceArn = Lens.field @"deviceArn"
{-# INLINEABLE gdDeviceArn #-}
{-# DEPRECATED deviceArn "Use generic-lens or generic-optics with 'deviceArn' instead"  #-}

instance Core.ToQuery GetDevice where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDevice where
        toHeaders GetDevice{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.GetDevice") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDevice where
        toJSON GetDevice{..}
          = Core.object
              (Core.catMaybes [("DeviceArn" Core..=) Core.<$> deviceArn])

instance Core.AWSRequest GetDevice where
        type Rs GetDevice = GetDeviceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDeviceResponse' Core.<$>
                   (x Core..:? "Device") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { device :: Core.Maybe Types.Device
    -- ^ The details of the device requested. Required.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDeviceResponse' value with any optional fields omitted.
mkGetDeviceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDeviceResponse
mkGetDeviceResponse responseStatus
  = GetDeviceResponse'{device = Core.Nothing, responseStatus}

-- | The details of the device requested. Required.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDevice :: Lens.Lens' GetDeviceResponse (Core.Maybe Types.Device)
gdrrsDevice = Lens.field @"device"
{-# INLINEABLE gdrrsDevice #-}
{-# DEPRECATED device "Use generic-lens or generic-optics with 'device' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDeviceResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

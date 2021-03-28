{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a device from Alexa For Business.
module Network.AWS.AlexaBusiness.DeleteDevice
    (
    -- * Creating a request
      DeleteDevice (..)
    , mkDeleteDevice
    -- ** Request lenses
    , ddfDeviceArn

    -- * Destructuring the response
    , DeleteDeviceResponse (..)
    , mkDeleteDeviceResponse
    -- ** Response lenses
    , ddrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDevice' smart constructor.
newtype DeleteDevice = DeleteDevice'
  { deviceArn :: Types.Arn
    -- ^ The ARN of the device for which to request details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDevice' value with any optional fields omitted.
mkDeleteDevice
    :: Types.Arn -- ^ 'deviceArn'
    -> DeleteDevice
mkDeleteDevice deviceArn = DeleteDevice'{deviceArn}

-- | The ARN of the device for which to request details.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDeviceArn :: Lens.Lens' DeleteDevice Types.Arn
ddfDeviceArn = Lens.field @"deviceArn"
{-# INLINEABLE ddfDeviceArn #-}
{-# DEPRECATED deviceArn "Use generic-lens or generic-optics with 'deviceArn' instead"  #-}

instance Core.ToQuery DeleteDevice where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDevice where
        toHeaders DeleteDevice{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteDevice")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDevice where
        toJSON DeleteDevice{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DeviceArn" Core..= deviceArn)])

instance Core.AWSRequest DeleteDevice where
        type Rs DeleteDevice = DeleteDeviceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteDeviceResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDeviceResponse' smart constructor.
newtype DeleteDeviceResponse = DeleteDeviceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeviceResponse' value with any optional fields omitted.
mkDeleteDeviceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDeviceResponse
mkDeleteDeviceResponse responseStatus
  = DeleteDeviceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DeleteDeviceResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

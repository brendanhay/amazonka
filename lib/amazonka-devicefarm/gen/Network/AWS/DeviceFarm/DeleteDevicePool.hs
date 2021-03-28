{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteDevicePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device pool given the pool ARN. Does not allow deletion of curated pools owned by the system.
module Network.AWS.DeviceFarm.DeleteDevicePool
    (
    -- * Creating a request
      DeleteDevicePool (..)
    , mkDeleteDevicePool
    -- ** Request lenses
    , ddpArn

    -- * Destructuring the response
    , DeleteDevicePoolResponse (..)
    , mkDeleteDevicePoolResponse
    -- ** Response lenses
    , ddprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the delete device pool operation.
--
-- /See:/ 'mkDeleteDevicePool' smart constructor.
newtype DeleteDevicePool = DeleteDevicePool'
  { arn :: Types.Arn
    -- ^ Represents the Amazon Resource Name (ARN) of the Device Farm device pool to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDevicePool' value with any optional fields omitted.
mkDeleteDevicePool
    :: Types.Arn -- ^ 'arn'
    -> DeleteDevicePool
mkDeleteDevicePool arn = DeleteDevicePool'{arn}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm device pool to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpArn :: Lens.Lens' DeleteDevicePool Types.Arn
ddpArn = Lens.field @"arn"
{-# INLINEABLE ddpArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery DeleteDevicePool where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDevicePool where
        toHeaders DeleteDevicePool{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.DeleteDevicePool")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDevicePool where
        toJSON DeleteDevicePool{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteDevicePool where
        type Rs DeleteDevicePool = DeleteDevicePoolResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteDevicePoolResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of a delete device pool request.
--
-- /See:/ 'mkDeleteDevicePoolResponse' smart constructor.
newtype DeleteDevicePoolResponse = DeleteDevicePoolResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDevicePoolResponse' value with any optional fields omitted.
mkDeleteDevicePoolResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDevicePoolResponse
mkDeleteDevicePoolResponse responseStatus
  = DeleteDevicePoolResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsResponseStatus :: Lens.Lens' DeleteDevicePoolResponse Core.Int
ddprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

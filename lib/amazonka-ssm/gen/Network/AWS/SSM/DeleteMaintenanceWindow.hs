{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a maintenance window.
module Network.AWS.SSM.DeleteMaintenanceWindow
    (
    -- * Creating a request
      DeleteMaintenanceWindow (..)
    , mkDeleteMaintenanceWindow
    -- ** Request lenses
    , dmwWindowId

    -- * Destructuring the response
    , DeleteMaintenanceWindowResponse (..)
    , mkDeleteMaintenanceWindowResponse
    -- ** Response lenses
    , dmwrrsWindowId
    , dmwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeleteMaintenanceWindow' smart constructor.
newtype DeleteMaintenanceWindow = DeleteMaintenanceWindow'
  { windowId :: Types.MaintenanceWindowId
    -- ^ The ID of the maintenance window to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMaintenanceWindow' value with any optional fields omitted.
mkDeleteMaintenanceWindow
    :: Types.MaintenanceWindowId -- ^ 'windowId'
    -> DeleteMaintenanceWindow
mkDeleteMaintenanceWindow windowId
  = DeleteMaintenanceWindow'{windowId}

-- | The ID of the maintenance window to delete.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwWindowId :: Lens.Lens' DeleteMaintenanceWindow Types.MaintenanceWindowId
dmwWindowId = Lens.field @"windowId"
{-# INLINEABLE dmwWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

instance Core.ToQuery DeleteMaintenanceWindow where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteMaintenanceWindow where
        toHeaders DeleteMaintenanceWindow{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DeleteMaintenanceWindow")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteMaintenanceWindow where
        toJSON DeleteMaintenanceWindow{..}
          = Core.object
              (Core.catMaybes [Core.Just ("WindowId" Core..= windowId)])

instance Core.AWSRequest DeleteMaintenanceWindow where
        type Rs DeleteMaintenanceWindow = DeleteMaintenanceWindowResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteMaintenanceWindowResponse' Core.<$>
                   (x Core..:? "WindowId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteMaintenanceWindowResponse' smart constructor.
data DeleteMaintenanceWindowResponse = DeleteMaintenanceWindowResponse'
  { windowId :: Core.Maybe Types.WindowId
    -- ^ The ID of the deleted maintenance window.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMaintenanceWindowResponse' value with any optional fields omitted.
mkDeleteMaintenanceWindowResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteMaintenanceWindowResponse
mkDeleteMaintenanceWindowResponse responseStatus
  = DeleteMaintenanceWindowResponse'{windowId = Core.Nothing,
                                     responseStatus}

-- | The ID of the deleted maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwrrsWindowId :: Lens.Lens' DeleteMaintenanceWindowResponse (Core.Maybe Types.WindowId)
dmwrrsWindowId = Lens.field @"windowId"
{-# INLINEABLE dmwrrsWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwrrsResponseStatus :: Lens.Lens' DeleteMaintenanceWindowResponse Core.Int
dmwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

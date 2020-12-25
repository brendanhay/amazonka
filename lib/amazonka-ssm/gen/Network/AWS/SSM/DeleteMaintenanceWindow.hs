{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteMaintenanceWindow (..),
    mkDeleteMaintenanceWindow,

    -- ** Request lenses
    dmwWindowId,

    -- * Destructuring the response
    DeleteMaintenanceWindowResponse (..),
    mkDeleteMaintenanceWindowResponse,

    -- ** Response lenses
    dmwrrsWindowId,
    dmwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeleteMaintenanceWindow' smart constructor.
newtype DeleteMaintenanceWindow = DeleteMaintenanceWindow'
  { -- | The ID of the maintenance window to delete.
    windowId :: Types.MaintenanceWindowId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMaintenanceWindow' value with any optional fields omitted.
mkDeleteMaintenanceWindow ::
  -- | 'windowId'
  Types.MaintenanceWindowId ->
  DeleteMaintenanceWindow
mkDeleteMaintenanceWindow windowId =
  DeleteMaintenanceWindow' {windowId}

-- | The ID of the maintenance window to delete.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwWindowId :: Lens.Lens' DeleteMaintenanceWindow Types.MaintenanceWindowId
dmwWindowId = Lens.field @"windowId"
{-# DEPRECATED dmwWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Core.FromJSON DeleteMaintenanceWindow where
  toJSON DeleteMaintenanceWindow {..} =
    Core.object
      (Core.catMaybes [Core.Just ("WindowId" Core..= windowId)])

instance Core.AWSRequest DeleteMaintenanceWindow where
  type Rs DeleteMaintenanceWindow = DeleteMaintenanceWindowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DeleteMaintenanceWindow")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMaintenanceWindowResponse'
            Core.<$> (x Core..:? "WindowId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteMaintenanceWindowResponse' smart constructor.
data DeleteMaintenanceWindowResponse = DeleteMaintenanceWindowResponse'
  { -- | The ID of the deleted maintenance window.
    windowId :: Core.Maybe Types.WindowId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMaintenanceWindowResponse' value with any optional fields omitted.
mkDeleteMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMaintenanceWindowResponse
mkDeleteMaintenanceWindowResponse responseStatus =
  DeleteMaintenanceWindowResponse'
    { windowId = Core.Nothing,
      responseStatus
    }

-- | The ID of the deleted maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwrrsWindowId :: Lens.Lens' DeleteMaintenanceWindowResponse (Core.Maybe Types.WindowId)
dmwrrsWindowId = Lens.field @"windowId"
{-# DEPRECATED dmwrrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwrrsResponseStatus :: Lens.Lens' DeleteMaintenanceWindowResponse Core.Int
dmwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

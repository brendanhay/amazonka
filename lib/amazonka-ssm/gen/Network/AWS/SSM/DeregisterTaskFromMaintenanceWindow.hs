{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a task from a maintenance window.
module Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow
  ( -- * Creating a request
    DeregisterTaskFromMaintenanceWindow (..),
    mkDeregisterTaskFromMaintenanceWindow,

    -- ** Request lenses
    dtfmwfWindowId,
    dtfmwfWindowTaskId,

    -- * Destructuring the response
    DeregisterTaskFromMaintenanceWindowResponse (..),
    mkDeregisterTaskFromMaintenanceWindowResponse,

    -- ** Response lenses
    dtfmwrfrsWindowId,
    dtfmwrfrsWindowTaskId,
    dtfmwrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeregisterTaskFromMaintenanceWindow' smart constructor.
data DeregisterTaskFromMaintenanceWindow = DeregisterTaskFromMaintenanceWindow'
  { -- | The ID of the maintenance window the task should be removed from.
    windowId :: Types.WindowId,
    -- | The ID of the task to remove from the maintenance window.
    windowTaskId :: Types.WindowTaskId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTaskFromMaintenanceWindow' value with any optional fields omitted.
mkDeregisterTaskFromMaintenanceWindow ::
  -- | 'windowId'
  Types.WindowId ->
  -- | 'windowTaskId'
  Types.WindowTaskId ->
  DeregisterTaskFromMaintenanceWindow
mkDeregisterTaskFromMaintenanceWindow windowId windowTaskId =
  DeregisterTaskFromMaintenanceWindow' {windowId, windowTaskId}

-- | The ID of the maintenance window the task should be removed from.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwfWindowId :: Lens.Lens' DeregisterTaskFromMaintenanceWindow Types.WindowId
dtfmwfWindowId = Lens.field @"windowId"
{-# DEPRECATED dtfmwfWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The ID of the task to remove from the maintenance window.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwfWindowTaskId :: Lens.Lens' DeregisterTaskFromMaintenanceWindow Types.WindowTaskId
dtfmwfWindowTaskId = Lens.field @"windowTaskId"
{-# DEPRECATED dtfmwfWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

instance Core.FromJSON DeregisterTaskFromMaintenanceWindow where
  toJSON DeregisterTaskFromMaintenanceWindow {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowId" Core..= windowId),
            Core.Just ("WindowTaskId" Core..= windowTaskId)
          ]
      )

instance Core.AWSRequest DeregisterTaskFromMaintenanceWindow where
  type
    Rs DeregisterTaskFromMaintenanceWindow =
      DeregisterTaskFromMaintenanceWindowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DeregisterTaskFromMaintenanceWindow")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTaskFromMaintenanceWindowResponse'
            Core.<$> (x Core..:? "WindowId")
            Core.<*> (x Core..:? "WindowTaskId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterTaskFromMaintenanceWindowResponse' smart constructor.
data DeregisterTaskFromMaintenanceWindowResponse = DeregisterTaskFromMaintenanceWindowResponse'
  { -- | The ID of the maintenance window the task was removed from.
    windowId :: Core.Maybe Types.MaintenanceWindowId,
    -- | The ID of the task removed from the maintenance window.
    windowTaskId :: Core.Maybe Types.MaintenanceWindowTaskId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTaskFromMaintenanceWindowResponse' value with any optional fields omitted.
mkDeregisterTaskFromMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterTaskFromMaintenanceWindowResponse
mkDeregisterTaskFromMaintenanceWindowResponse responseStatus =
  DeregisterTaskFromMaintenanceWindowResponse'
    { windowId =
        Core.Nothing,
      windowTaskId = Core.Nothing,
      responseStatus
    }

-- | The ID of the maintenance window the task was removed from.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwrfrsWindowId :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse (Core.Maybe Types.MaintenanceWindowId)
dtfmwrfrsWindowId = Lens.field @"windowId"
{-# DEPRECATED dtfmwrfrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The ID of the task removed from the maintenance window.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwrfrsWindowTaskId :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse (Core.Maybe Types.MaintenanceWindowTaskId)
dtfmwrfrsWindowTaskId = Lens.field @"windowTaskId"
{-# DEPRECATED dtfmwrfrsWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwrfrsResponseStatus :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse Core.Int
dtfmwrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtfmwrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

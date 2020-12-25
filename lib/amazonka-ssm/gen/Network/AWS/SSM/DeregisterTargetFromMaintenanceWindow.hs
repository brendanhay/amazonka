{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeregisterTargetFromMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a target from a maintenance window.
module Network.AWS.SSM.DeregisterTargetFromMaintenanceWindow
  ( -- * Creating a request
    DeregisterTargetFromMaintenanceWindow (..),
    mkDeregisterTargetFromMaintenanceWindow,

    -- ** Request lenses
    dtfmwWindowId,
    dtfmwWindowTargetId,
    dtfmwSafe,

    -- * Destructuring the response
    DeregisterTargetFromMaintenanceWindowResponse (..),
    mkDeregisterTargetFromMaintenanceWindowResponse,

    -- ** Response lenses
    dtfmwrrsWindowId,
    dtfmwrrsWindowTargetId,
    dtfmwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeregisterTargetFromMaintenanceWindow' smart constructor.
data DeregisterTargetFromMaintenanceWindow = DeregisterTargetFromMaintenanceWindow'
  { -- | The ID of the maintenance window the target should be removed from.
    windowId :: Types.WindowId,
    -- | The ID of the target definition to remove.
    windowTargetId :: Types.WindowTargetId,
    -- | The system checks if the target is being referenced by a task. If the target is being referenced, the system returns an error and does not deregister the target from the maintenance window.
    safe :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTargetFromMaintenanceWindow' value with any optional fields omitted.
mkDeregisterTargetFromMaintenanceWindow ::
  -- | 'windowId'
  Types.WindowId ->
  -- | 'windowTargetId'
  Types.WindowTargetId ->
  DeregisterTargetFromMaintenanceWindow
mkDeregisterTargetFromMaintenanceWindow windowId windowTargetId =
  DeregisterTargetFromMaintenanceWindow'
    { windowId,
      windowTargetId,
      safe = Core.Nothing
    }

-- | The ID of the maintenance window the target should be removed from.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwWindowId :: Lens.Lens' DeregisterTargetFromMaintenanceWindow Types.WindowId
dtfmwWindowId = Lens.field @"windowId"
{-# DEPRECATED dtfmwWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The ID of the target definition to remove.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwWindowTargetId :: Lens.Lens' DeregisterTargetFromMaintenanceWindow Types.WindowTargetId
dtfmwWindowTargetId = Lens.field @"windowTargetId"
{-# DEPRECATED dtfmwWindowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead." #-}

-- | The system checks if the target is being referenced by a task. If the target is being referenced, the system returns an error and does not deregister the target from the maintenance window.
--
-- /Note:/ Consider using 'safe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwSafe :: Lens.Lens' DeregisterTargetFromMaintenanceWindow (Core.Maybe Core.Bool)
dtfmwSafe = Lens.field @"safe"
{-# DEPRECATED dtfmwSafe "Use generic-lens or generic-optics with 'safe' instead." #-}

instance Core.FromJSON DeregisterTargetFromMaintenanceWindow where
  toJSON DeregisterTargetFromMaintenanceWindow {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowId" Core..= windowId),
            Core.Just ("WindowTargetId" Core..= windowTargetId),
            ("Safe" Core..=) Core.<$> safe
          ]
      )

instance Core.AWSRequest DeregisterTargetFromMaintenanceWindow where
  type
    Rs DeregisterTargetFromMaintenanceWindow =
      DeregisterTargetFromMaintenanceWindowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DeregisterTargetFromMaintenanceWindow")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTargetFromMaintenanceWindowResponse'
            Core.<$> (x Core..:? "WindowId")
            Core.<*> (x Core..:? "WindowTargetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterTargetFromMaintenanceWindowResponse' smart constructor.
data DeregisterTargetFromMaintenanceWindowResponse = DeregisterTargetFromMaintenanceWindowResponse'
  { -- | The ID of the maintenance window the target was removed from.
    windowId :: Core.Maybe Types.MaintenanceWindowId,
    -- | The ID of the removed target definition.
    windowTargetId :: Core.Maybe Types.MaintenanceWindowTargetId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTargetFromMaintenanceWindowResponse' value with any optional fields omitted.
mkDeregisterTargetFromMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterTargetFromMaintenanceWindowResponse
mkDeregisterTargetFromMaintenanceWindowResponse responseStatus =
  DeregisterTargetFromMaintenanceWindowResponse'
    { windowId =
        Core.Nothing,
      windowTargetId = Core.Nothing,
      responseStatus
    }

-- | The ID of the maintenance window the target was removed from.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwrrsWindowId :: Lens.Lens' DeregisterTargetFromMaintenanceWindowResponse (Core.Maybe Types.MaintenanceWindowId)
dtfmwrrsWindowId = Lens.field @"windowId"
{-# DEPRECATED dtfmwrrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The ID of the removed target definition.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwrrsWindowTargetId :: Lens.Lens' DeregisterTargetFromMaintenanceWindowResponse (Core.Maybe Types.MaintenanceWindowTargetId)
dtfmwrrsWindowTargetId = Lens.field @"windowTargetId"
{-# DEPRECATED dtfmwrrsWindowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfmwrrsResponseStatus :: Lens.Lens' DeregisterTargetFromMaintenanceWindowResponse Core.Int
dtfmwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtfmwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

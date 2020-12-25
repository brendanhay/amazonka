{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CancelMaintenanceWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a maintenance window execution that is already in progress and cancels any tasks in the window that have not already starting running. (Tasks already in progress will continue to completion.)
module Network.AWS.SSM.CancelMaintenanceWindowExecution
  ( -- * Creating a request
    CancelMaintenanceWindowExecution (..),
    mkCancelMaintenanceWindowExecution,

    -- ** Request lenses
    cmweWindowExecutionId,

    -- * Destructuring the response
    CancelMaintenanceWindowExecutionResponse (..),
    mkCancelMaintenanceWindowExecutionResponse,

    -- ** Response lenses
    cmwerrsWindowExecutionId,
    cmwerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkCancelMaintenanceWindowExecution' smart constructor.
newtype CancelMaintenanceWindowExecution = CancelMaintenanceWindowExecution'
  { -- | The ID of the maintenance window execution to stop.
    windowExecutionId :: Types.WindowExecutionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelMaintenanceWindowExecution' value with any optional fields omitted.
mkCancelMaintenanceWindowExecution ::
  -- | 'windowExecutionId'
  Types.WindowExecutionId ->
  CancelMaintenanceWindowExecution
mkCancelMaintenanceWindowExecution windowExecutionId =
  CancelMaintenanceWindowExecution' {windowExecutionId}

-- | The ID of the maintenance window execution to stop.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmweWindowExecutionId :: Lens.Lens' CancelMaintenanceWindowExecution Types.WindowExecutionId
cmweWindowExecutionId = Lens.field @"windowExecutionId"
{-# DEPRECATED cmweWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

instance Core.FromJSON CancelMaintenanceWindowExecution where
  toJSON CancelMaintenanceWindowExecution {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WindowExecutionId" Core..= windowExecutionId)]
      )

instance Core.AWSRequest CancelMaintenanceWindowExecution where
  type
    Rs CancelMaintenanceWindowExecution =
      CancelMaintenanceWindowExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.CancelMaintenanceWindowExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelMaintenanceWindowExecutionResponse'
            Core.<$> (x Core..:? "WindowExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCancelMaintenanceWindowExecutionResponse' smart constructor.
data CancelMaintenanceWindowExecutionResponse = CancelMaintenanceWindowExecutionResponse'
  { -- | The ID of the maintenance window execution that has been stopped.
    windowExecutionId :: Core.Maybe Types.WindowExecutionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelMaintenanceWindowExecutionResponse' value with any optional fields omitted.
mkCancelMaintenanceWindowExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelMaintenanceWindowExecutionResponse
mkCancelMaintenanceWindowExecutionResponse responseStatus =
  CancelMaintenanceWindowExecutionResponse'
    { windowExecutionId =
        Core.Nothing,
      responseStatus
    }

-- | The ID of the maintenance window execution that has been stopped.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwerrsWindowExecutionId :: Lens.Lens' CancelMaintenanceWindowExecutionResponse (Core.Maybe Types.WindowExecutionId)
cmwerrsWindowExecutionId = Lens.field @"windowExecutionId"
{-# DEPRECATED cmwerrsWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwerrsResponseStatus :: Lens.Lens' CancelMaintenanceWindowExecutionResponse Core.Int
cmwerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmwerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

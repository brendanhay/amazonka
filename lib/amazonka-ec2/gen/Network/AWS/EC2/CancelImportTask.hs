{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelImportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-process import virtual machine or import snapshot task.
module Network.AWS.EC2.CancelImportTask
  ( -- * Creating a request
    CancelImportTask (..),
    mkCancelImportTask,

    -- ** Request lenses
    citCancelReason,
    citDryRun,
    citImportTaskId,

    -- * Destructuring the response
    CancelImportTaskResponse (..),
    mkCancelImportTaskResponse,

    -- ** Response lenses
    citrrsImportTaskId,
    citrrsPreviousState,
    citrrsState,
    citrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelImportTask' smart constructor.
data CancelImportTask = CancelImportTask'
  { -- | The reason for canceling the task.
    cancelReason :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the import image or import snapshot task to be canceled.
    importTaskId :: Core.Maybe Types.ImportTaskId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelImportTask' value with any optional fields omitted.
mkCancelImportTask ::
  CancelImportTask
mkCancelImportTask =
  CancelImportTask'
    { cancelReason = Core.Nothing,
      dryRun = Core.Nothing,
      importTaskId = Core.Nothing
    }

-- | The reason for canceling the task.
--
-- /Note:/ Consider using 'cancelReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citCancelReason :: Lens.Lens' CancelImportTask (Core.Maybe Types.String)
citCancelReason = Lens.field @"cancelReason"
{-# DEPRECATED citCancelReason "Use generic-lens or generic-optics with 'cancelReason' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citDryRun :: Lens.Lens' CancelImportTask (Core.Maybe Core.Bool)
citDryRun = Lens.field @"dryRun"
{-# DEPRECATED citDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the import image or import snapshot task to be canceled.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citImportTaskId :: Lens.Lens' CancelImportTask (Core.Maybe Types.ImportTaskId)
citImportTaskId = Lens.field @"importTaskId"
{-# DEPRECATED citImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

instance Core.AWSRequest CancelImportTask where
  type Rs CancelImportTask = CancelImportTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CancelImportTask")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "CancelReason" Core.<$> cancelReason)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "ImportTaskId" Core.<$> importTaskId)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CancelImportTaskResponse'
            Core.<$> (x Core..@? "importTaskId")
            Core.<*> (x Core..@? "previousState")
            Core.<*> (x Core..@? "state")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCancelImportTaskResponse' smart constructor.
data CancelImportTaskResponse = CancelImportTaskResponse'
  { -- | The ID of the task being canceled.
    importTaskId :: Core.Maybe Types.ImportTaskId,
    -- | The current state of the task being canceled.
    previousState :: Core.Maybe Types.PreviousState,
    -- | The current state of the task being canceled.
    state :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelImportTaskResponse' value with any optional fields omitted.
mkCancelImportTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelImportTaskResponse
mkCancelImportTaskResponse responseStatus =
  CancelImportTaskResponse'
    { importTaskId = Core.Nothing,
      previousState = Core.Nothing,
      state = Core.Nothing,
      responseStatus
    }

-- | The ID of the task being canceled.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrrsImportTaskId :: Lens.Lens' CancelImportTaskResponse (Core.Maybe Types.ImportTaskId)
citrrsImportTaskId = Lens.field @"importTaskId"
{-# DEPRECATED citrrsImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | The current state of the task being canceled.
--
-- /Note:/ Consider using 'previousState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrrsPreviousState :: Lens.Lens' CancelImportTaskResponse (Core.Maybe Types.PreviousState)
citrrsPreviousState = Lens.field @"previousState"
{-# DEPRECATED citrrsPreviousState "Use generic-lens or generic-optics with 'previousState' instead." #-}

-- | The current state of the task being canceled.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrrsState :: Lens.Lens' CancelImportTaskResponse (Core.Maybe Types.String)
citrrsState = Lens.field @"state"
{-# DEPRECATED citrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
citrrsResponseStatus :: Lens.Lens' CancelImportTaskResponse Core.Int
citrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED citrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

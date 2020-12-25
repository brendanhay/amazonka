{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.CancelExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified export task.
--
-- The task must be in the @PENDING@ or @RUNNING@ state.
module Network.AWS.CloudWatchLogs.CancelExportTask
  ( -- * Creating a request
    CancelExportTask (..),
    mkCancelExportTask,

    -- ** Request lenses
    cetTaskId,

    -- * Destructuring the response
    CancelExportTaskResponse (..),
    mkCancelExportTaskResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask'
  { -- | The ID of the export task.
    taskId :: Types.ExportTaskId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelExportTask' value with any optional fields omitted.
mkCancelExportTask ::
  -- | 'taskId'
  Types.ExportTaskId ->
  CancelExportTask
mkCancelExportTask taskId = CancelExportTask' {taskId}

-- | The ID of the export task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetTaskId :: Lens.Lens' CancelExportTask Types.ExportTaskId
cetTaskId = Lens.field @"taskId"
{-# DEPRECATED cetTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Core.FromJSON CancelExportTask where
  toJSON CancelExportTask {..} =
    Core.object
      (Core.catMaybes [Core.Just ("taskId" Core..= taskId)])

instance Core.AWSRequest CancelExportTask where
  type Rs CancelExportTask = CancelExportTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.CancelExportTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull CancelExportTaskResponse'

-- | /See:/ 'mkCancelExportTaskResponse' smart constructor.
data CancelExportTaskResponse = CancelExportTaskResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelExportTaskResponse' value with any optional fields omitted.
mkCancelExportTaskResponse ::
  CancelExportTaskResponse
mkCancelExportTaskResponse = CancelExportTaskResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CancelMLTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels (stops) a task run. Machine learning task runs are asynchronous tasks that AWS Glue runs on your behalf as part of various machine learning workflows. You can cancel a machine learning task run at any time by calling @CancelMLTaskRun@ with a task run's parent transform's @TransformID@ and the task run's @TaskRunId@ .
module Network.AWS.Glue.CancelMLTaskRun
  ( -- * Creating a request
    CancelMLTaskRun (..),
    mkCancelMLTaskRun,

    -- ** Request lenses
    cmltrTransformId,
    cmltrTaskRunId,

    -- * Destructuring the response
    CancelMLTaskRunResponse (..),
    mkCancelMLTaskRunResponse,

    -- ** Response lenses
    cmltrrrsStatus,
    cmltrrrsTaskRunId,
    cmltrrrsTransformId,
    cmltrrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelMLTaskRun' smart constructor.
data CancelMLTaskRun = CancelMLTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Types.HashString,
    -- | A unique identifier for the task run.
    taskRunId :: Types.HashString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelMLTaskRun' value with any optional fields omitted.
mkCancelMLTaskRun ::
  -- | 'transformId'
  Types.HashString ->
  -- | 'taskRunId'
  Types.HashString ->
  CancelMLTaskRun
mkCancelMLTaskRun transformId taskRunId =
  CancelMLTaskRun' {transformId, taskRunId}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrTransformId :: Lens.Lens' CancelMLTaskRun Types.HashString
cmltrTransformId = Lens.field @"transformId"
{-# DEPRECATED cmltrTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | A unique identifier for the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrTaskRunId :: Lens.Lens' CancelMLTaskRun Types.HashString
cmltrTaskRunId = Lens.field @"taskRunId"
{-# DEPRECATED cmltrTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

instance Core.FromJSON CancelMLTaskRun where
  toJSON CancelMLTaskRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformId" Core..= transformId),
            Core.Just ("TaskRunId" Core..= taskRunId)
          ]
      )

instance Core.AWSRequest CancelMLTaskRun where
  type Rs CancelMLTaskRun = CancelMLTaskRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CancelMLTaskRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelMLTaskRunResponse'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "TaskRunId")
            Core.<*> (x Core..:? "TransformId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCancelMLTaskRunResponse' smart constructor.
data CancelMLTaskRunResponse = CancelMLTaskRunResponse'
  { -- | The status for this run.
    status :: Core.Maybe Types.TaskStatusType,
    -- | The unique identifier for the task run.
    taskRunId :: Core.Maybe Types.HashString,
    -- | The unique identifier of the machine learning transform.
    transformId :: Core.Maybe Types.HashString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelMLTaskRunResponse' value with any optional fields omitted.
mkCancelMLTaskRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelMLTaskRunResponse
mkCancelMLTaskRunResponse responseStatus =
  CancelMLTaskRunResponse'
    { status = Core.Nothing,
      taskRunId = Core.Nothing,
      transformId = Core.Nothing,
      responseStatus
    }

-- | The status for this run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrrsStatus :: Lens.Lens' CancelMLTaskRunResponse (Core.Maybe Types.TaskStatusType)
cmltrrrsStatus = Lens.field @"status"
{-# DEPRECATED cmltrrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique identifier for the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrrsTaskRunId :: Lens.Lens' CancelMLTaskRunResponse (Core.Maybe Types.HashString)
cmltrrrsTaskRunId = Lens.field @"taskRunId"
{-# DEPRECATED cmltrrrsTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrrsTransformId :: Lens.Lens' CancelMLTaskRunResponse (Core.Maybe Types.HashString)
cmltrrrsTransformId = Lens.field @"transformId"
{-# DEPRECATED cmltrrrsTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrrsResponseStatus :: Lens.Lens' CancelMLTaskRunResponse Core.Int
cmltrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmltrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

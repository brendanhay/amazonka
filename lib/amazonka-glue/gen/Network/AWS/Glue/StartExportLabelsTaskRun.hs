{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartExportLabelsTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins an asynchronous task to export all labeled data for a particular transform. This task is the only label-related API call that is not part of the typical active learning workflow. You typically use @StartExportLabelsTaskRun@ when you want to work with all of your existing labels at the same time, such as when you want to remove or change labels that were previously submitted as truth. This API operation accepts the @TransformId@ whose labels you want to export and an Amazon Simple Storage Service (Amazon S3) path to export the labels to. The operation returns a @TaskRunId@ . You can check on the status of your task run by calling the @GetMLTaskRun@ API.
module Network.AWS.Glue.StartExportLabelsTaskRun
  ( -- * Creating a request
    StartExportLabelsTaskRun (..),
    mkStartExportLabelsTaskRun,

    -- ** Request lenses
    seltrTransformId,
    seltrOutputS3Path,

    -- * Destructuring the response
    StartExportLabelsTaskRunResponse (..),
    mkStartExportLabelsTaskRunResponse,

    -- ** Response lenses
    seltrrrsTaskRunId,
    seltrrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartExportLabelsTaskRun' smart constructor.
data StartExportLabelsTaskRun = StartExportLabelsTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Types.TransformId,
    -- | The Amazon S3 path where you export the labels.
    outputS3Path :: Types.OutputS3Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartExportLabelsTaskRun' value with any optional fields omitted.
mkStartExportLabelsTaskRun ::
  -- | 'transformId'
  Types.TransformId ->
  -- | 'outputS3Path'
  Types.OutputS3Path ->
  StartExportLabelsTaskRun
mkStartExportLabelsTaskRun transformId outputS3Path =
  StartExportLabelsTaskRun' {transformId, outputS3Path}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seltrTransformId :: Lens.Lens' StartExportLabelsTaskRun Types.TransformId
seltrTransformId = Lens.field @"transformId"
{-# DEPRECATED seltrTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The Amazon S3 path where you export the labels.
--
-- /Note:/ Consider using 'outputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seltrOutputS3Path :: Lens.Lens' StartExportLabelsTaskRun Types.OutputS3Path
seltrOutputS3Path = Lens.field @"outputS3Path"
{-# DEPRECATED seltrOutputS3Path "Use generic-lens or generic-optics with 'outputS3Path' instead." #-}

instance Core.FromJSON StartExportLabelsTaskRun where
  toJSON StartExportLabelsTaskRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformId" Core..= transformId),
            Core.Just ("OutputS3Path" Core..= outputS3Path)
          ]
      )

instance Core.AWSRequest StartExportLabelsTaskRun where
  type Rs StartExportLabelsTaskRun = StartExportLabelsTaskRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.StartExportLabelsTaskRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExportLabelsTaskRunResponse'
            Core.<$> (x Core..:? "TaskRunId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartExportLabelsTaskRunResponse' smart constructor.
data StartExportLabelsTaskRunResponse = StartExportLabelsTaskRunResponse'
  { -- | The unique identifier for the task run.
    taskRunId :: Core.Maybe Types.HashString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartExportLabelsTaskRunResponse' value with any optional fields omitted.
mkStartExportLabelsTaskRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartExportLabelsTaskRunResponse
mkStartExportLabelsTaskRunResponse responseStatus =
  StartExportLabelsTaskRunResponse'
    { taskRunId = Core.Nothing,
      responseStatus
    }

-- | The unique identifier for the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seltrrrsTaskRunId :: Lens.Lens' StartExportLabelsTaskRunResponse (Core.Maybe Types.HashString)
seltrrrsTaskRunId = Lens.field @"taskRunId"
{-# DEPRECATED seltrrrsTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seltrrrsResponseStatus :: Lens.Lens' StartExportLabelsTaskRunResponse Core.Int
seltrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED seltrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

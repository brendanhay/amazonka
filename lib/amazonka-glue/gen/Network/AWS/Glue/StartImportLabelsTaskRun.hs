{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartImportLabelsTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables you to provide additional labels (examples of truth) to be used to teach the machine learning transform and improve its quality. This API operation is generally used as part of the active learning workflow that starts with the @StartMLLabelingSetGenerationTaskRun@ call and that ultimately results in improving the quality of your machine learning transform.
--
-- After the @StartMLLabelingSetGenerationTaskRun@ finishes, AWS Glue machine learning will have generated a series of questions for humans to answer. (Answering these questions is often called 'labeling' in the machine learning workflows). In the case of the @FindMatches@ transform, these questions are of the form, “What is the correct way to group these rows together into groups composed entirely of matching records?” After the labeling process is finished, users upload their answers/labels with a call to @StartImportLabelsTaskRun@ . After @StartImportLabelsTaskRun@ finishes, all future runs of the machine learning transform use the new and improved labels and perform a higher-quality transformation.
-- By default, @StartMLLabelingSetGenerationTaskRun@ continually learns from and combines all labels that you upload unless you set @Replace@ to true. If you set @Replace@ to true, @StartImportLabelsTaskRun@ deletes and forgets all previously uploaded labels and learns only from the exact set that you upload. Replacing labels can be helpful if you realize that you previously uploaded incorrect labels, and you believe that they are having a negative effect on your transform quality.
-- You can check on the status of your task run by calling the @GetMLTaskRun@ operation.
module Network.AWS.Glue.StartImportLabelsTaskRun
  ( -- * Creating a request
    StartImportLabelsTaskRun (..),
    mkStartImportLabelsTaskRun,

    -- ** Request lenses
    siltrTransformId,
    siltrInputS3Path,
    siltrReplaceAllLabels,

    -- * Destructuring the response
    StartImportLabelsTaskRunResponse (..),
    mkStartImportLabelsTaskRunResponse,

    -- ** Response lenses
    siltrrrsTaskRunId,
    siltrrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartImportLabelsTaskRun' smart constructor.
data StartImportLabelsTaskRun = StartImportLabelsTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Types.HashString,
    -- | The Amazon Simple Storage Service (Amazon S3) path from where you import the labels.
    inputS3Path :: Types.InputS3Path,
    -- | Indicates whether to overwrite your existing labels.
    replaceAllLabels :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartImportLabelsTaskRun' value with any optional fields omitted.
mkStartImportLabelsTaskRun ::
  -- | 'transformId'
  Types.HashString ->
  -- | 'inputS3Path'
  Types.InputS3Path ->
  StartImportLabelsTaskRun
mkStartImportLabelsTaskRun transformId inputS3Path =
  StartImportLabelsTaskRun'
    { transformId,
      inputS3Path,
      replaceAllLabels = Core.Nothing
    }

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siltrTransformId :: Lens.Lens' StartImportLabelsTaskRun Types.HashString
siltrTransformId = Lens.field @"transformId"
{-# DEPRECATED siltrTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The Amazon Simple Storage Service (Amazon S3) path from where you import the labels.
--
-- /Note:/ Consider using 'inputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siltrInputS3Path :: Lens.Lens' StartImportLabelsTaskRun Types.InputS3Path
siltrInputS3Path = Lens.field @"inputS3Path"
{-# DEPRECATED siltrInputS3Path "Use generic-lens or generic-optics with 'inputS3Path' instead." #-}

-- | Indicates whether to overwrite your existing labels.
--
-- /Note:/ Consider using 'replaceAllLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siltrReplaceAllLabels :: Lens.Lens' StartImportLabelsTaskRun (Core.Maybe Core.Bool)
siltrReplaceAllLabels = Lens.field @"replaceAllLabels"
{-# DEPRECATED siltrReplaceAllLabels "Use generic-lens or generic-optics with 'replaceAllLabels' instead." #-}

instance Core.FromJSON StartImportLabelsTaskRun where
  toJSON StartImportLabelsTaskRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformId" Core..= transformId),
            Core.Just ("InputS3Path" Core..= inputS3Path),
            ("ReplaceAllLabels" Core..=) Core.<$> replaceAllLabels
          ]
      )

instance Core.AWSRequest StartImportLabelsTaskRun where
  type Rs StartImportLabelsTaskRun = StartImportLabelsTaskRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.StartImportLabelsTaskRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImportLabelsTaskRunResponse'
            Core.<$> (x Core..:? "TaskRunId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartImportLabelsTaskRunResponse' smart constructor.
data StartImportLabelsTaskRunResponse = StartImportLabelsTaskRunResponse'
  { -- | The unique identifier for the task run.
    taskRunId :: Core.Maybe Types.HashString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartImportLabelsTaskRunResponse' value with any optional fields omitted.
mkStartImportLabelsTaskRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartImportLabelsTaskRunResponse
mkStartImportLabelsTaskRunResponse responseStatus =
  StartImportLabelsTaskRunResponse'
    { taskRunId = Core.Nothing,
      responseStatus
    }

-- | The unique identifier for the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siltrrrsTaskRunId :: Lens.Lens' StartImportLabelsTaskRunResponse (Core.Maybe Types.HashString)
siltrrrsTaskRunId = Lens.field @"taskRunId"
{-# DEPRECATED siltrrrsTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siltrrrsResponseStatus :: Lens.Lens' StartImportLabelsTaskRunResponse Core.Int
siltrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED siltrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

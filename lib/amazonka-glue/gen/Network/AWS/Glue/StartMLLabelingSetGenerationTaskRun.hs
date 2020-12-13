{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the active learning workflow for your machine learning transform to improve the transform's quality by generating label sets and adding labels.
--
-- When the @StartMLLabelingSetGenerationTaskRun@ finishes, AWS Glue will have generated a "labeling set" or a set of questions for humans to answer.
-- In the case of the @FindMatches@ transform, these questions are of the form, “What is the correct way to group these rows together into groups composed entirely of matching records?”
-- After the labeling process is finished, you can upload your labels with a call to @StartImportLabelsTaskRun@ . After @StartImportLabelsTaskRun@ finishes, all future runs of the machine learning transform will use the new and improved labels and perform a higher-quality transformation.
module Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun
  ( -- * Creating a request
    StartMLLabelingSetGenerationTaskRun (..),
    mkStartMLLabelingSetGenerationTaskRun,

    -- ** Request lenses
    smllsgtrOutputS3Path,
    smllsgtrTransformId,

    -- * Destructuring the response
    StartMLLabelingSetGenerationTaskRunResponse (..),
    mkStartMLLabelingSetGenerationTaskRunResponse,

    -- ** Response lenses
    smllsgtrrsTaskRunId,
    smllsgtrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartMLLabelingSetGenerationTaskRun' smart constructor.
data StartMLLabelingSetGenerationTaskRun = StartMLLabelingSetGenerationTaskRun'
  { -- | The Amazon Simple Storage Service (Amazon S3) path where you generate the labeling set.
    outputS3Path :: Lude.Text,
    -- | The unique identifier of the machine learning transform.
    transformId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMLLabelingSetGenerationTaskRun' with the minimum fields required to make a request.
--
-- * 'outputS3Path' - The Amazon Simple Storage Service (Amazon S3) path where you generate the labeling set.
-- * 'transformId' - The unique identifier of the machine learning transform.
mkStartMLLabelingSetGenerationTaskRun ::
  -- | 'outputS3Path'
  Lude.Text ->
  -- | 'transformId'
  Lude.Text ->
  StartMLLabelingSetGenerationTaskRun
mkStartMLLabelingSetGenerationTaskRun pOutputS3Path_ pTransformId_ =
  StartMLLabelingSetGenerationTaskRun'
    { outputS3Path =
        pOutputS3Path_,
      transformId = pTransformId_
    }

-- | The Amazon Simple Storage Service (Amazon S3) path where you generate the labeling set.
--
-- /Note:/ Consider using 'outputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smllsgtrOutputS3Path :: Lens.Lens' StartMLLabelingSetGenerationTaskRun Lude.Text
smllsgtrOutputS3Path = Lens.lens (outputS3Path :: StartMLLabelingSetGenerationTaskRun -> Lude.Text) (\s a -> s {outputS3Path = a} :: StartMLLabelingSetGenerationTaskRun)
{-# DEPRECATED smllsgtrOutputS3Path "Use generic-lens or generic-optics with 'outputS3Path' instead." #-}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smllsgtrTransformId :: Lens.Lens' StartMLLabelingSetGenerationTaskRun Lude.Text
smllsgtrTransformId = Lens.lens (transformId :: StartMLLabelingSetGenerationTaskRun -> Lude.Text) (\s a -> s {transformId = a} :: StartMLLabelingSetGenerationTaskRun)
{-# DEPRECATED smllsgtrTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

instance Lude.AWSRequest StartMLLabelingSetGenerationTaskRun where
  type
    Rs StartMLLabelingSetGenerationTaskRun =
      StartMLLabelingSetGenerationTaskRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartMLLabelingSetGenerationTaskRunResponse'
            Lude.<$> (x Lude..?> "TaskRunId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartMLLabelingSetGenerationTaskRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.StartMLLabelingSetGenerationTaskRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartMLLabelingSetGenerationTaskRun where
  toJSON StartMLLabelingSetGenerationTaskRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OutputS3Path" Lude..= outputS3Path),
            Lude.Just ("TransformId" Lude..= transformId)
          ]
      )

instance Lude.ToPath StartMLLabelingSetGenerationTaskRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StartMLLabelingSetGenerationTaskRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartMLLabelingSetGenerationTaskRunResponse' smart constructor.
data StartMLLabelingSetGenerationTaskRunResponse = StartMLLabelingSetGenerationTaskRunResponse'
  { -- | The unique run identifier that is associated with this task run.
    taskRunId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMLLabelingSetGenerationTaskRunResponse' with the minimum fields required to make a request.
--
-- * 'taskRunId' - The unique run identifier that is associated with this task run.
-- * 'responseStatus' - The response status code.
mkStartMLLabelingSetGenerationTaskRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMLLabelingSetGenerationTaskRunResponse
mkStartMLLabelingSetGenerationTaskRunResponse pResponseStatus_ =
  StartMLLabelingSetGenerationTaskRunResponse'
    { taskRunId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique run identifier that is associated with this task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smllsgtrrsTaskRunId :: Lens.Lens' StartMLLabelingSetGenerationTaskRunResponse (Lude.Maybe Lude.Text)
smllsgtrrsTaskRunId = Lens.lens (taskRunId :: StartMLLabelingSetGenerationTaskRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskRunId = a} :: StartMLLabelingSetGenerationTaskRunResponse)
{-# DEPRECATED smllsgtrrsTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smllsgtrrsResponseStatus :: Lens.Lens' StartMLLabelingSetGenerationTaskRunResponse Lude.Int
smllsgtrrsResponseStatus = Lens.lens (responseStatus :: StartMLLabelingSetGenerationTaskRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMLLabelingSetGenerationTaskRunResponse)
{-# DEPRECATED smllsgtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

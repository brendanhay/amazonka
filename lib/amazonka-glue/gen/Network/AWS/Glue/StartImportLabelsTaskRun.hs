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
    siltrInputS3Path,
    siltrReplaceAllLabels,
    siltrTransformId,

    -- * Destructuring the response
    StartImportLabelsTaskRunResponse (..),
    mkStartImportLabelsTaskRunResponse,

    -- ** Response lenses
    siltrrsTaskRunId,
    siltrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartImportLabelsTaskRun' smart constructor.
data StartImportLabelsTaskRun = StartImportLabelsTaskRun'
  { -- | The Amazon Simple Storage Service (Amazon S3) path from where you import the labels.
    inputS3Path :: Lude.Text,
    -- | Indicates whether to overwrite your existing labels.
    replaceAllLabels :: Lude.Maybe Lude.Bool,
    -- | The unique identifier of the machine learning transform.
    transformId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartImportLabelsTaskRun' with the minimum fields required to make a request.
--
-- * 'inputS3Path' - The Amazon Simple Storage Service (Amazon S3) path from where you import the labels.
-- * 'replaceAllLabels' - Indicates whether to overwrite your existing labels.
-- * 'transformId' - The unique identifier of the machine learning transform.
mkStartImportLabelsTaskRun ::
  -- | 'inputS3Path'
  Lude.Text ->
  -- | 'transformId'
  Lude.Text ->
  StartImportLabelsTaskRun
mkStartImportLabelsTaskRun pInputS3Path_ pTransformId_ =
  StartImportLabelsTaskRun'
    { inputS3Path = pInputS3Path_,
      replaceAllLabels = Lude.Nothing,
      transformId = pTransformId_
    }

-- | The Amazon Simple Storage Service (Amazon S3) path from where you import the labels.
--
-- /Note:/ Consider using 'inputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siltrInputS3Path :: Lens.Lens' StartImportLabelsTaskRun Lude.Text
siltrInputS3Path = Lens.lens (inputS3Path :: StartImportLabelsTaskRun -> Lude.Text) (\s a -> s {inputS3Path = a} :: StartImportLabelsTaskRun)
{-# DEPRECATED siltrInputS3Path "Use generic-lens or generic-optics with 'inputS3Path' instead." #-}

-- | Indicates whether to overwrite your existing labels.
--
-- /Note:/ Consider using 'replaceAllLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siltrReplaceAllLabels :: Lens.Lens' StartImportLabelsTaskRun (Lude.Maybe Lude.Bool)
siltrReplaceAllLabels = Lens.lens (replaceAllLabels :: StartImportLabelsTaskRun -> Lude.Maybe Lude.Bool) (\s a -> s {replaceAllLabels = a} :: StartImportLabelsTaskRun)
{-# DEPRECATED siltrReplaceAllLabels "Use generic-lens or generic-optics with 'replaceAllLabels' instead." #-}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siltrTransformId :: Lens.Lens' StartImportLabelsTaskRun Lude.Text
siltrTransformId = Lens.lens (transformId :: StartImportLabelsTaskRun -> Lude.Text) (\s a -> s {transformId = a} :: StartImportLabelsTaskRun)
{-# DEPRECATED siltrTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

instance Lude.AWSRequest StartImportLabelsTaskRun where
  type Rs StartImportLabelsTaskRun = StartImportLabelsTaskRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartImportLabelsTaskRunResponse'
            Lude.<$> (x Lude..?> "TaskRunId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartImportLabelsTaskRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.StartImportLabelsTaskRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartImportLabelsTaskRun where
  toJSON StartImportLabelsTaskRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InputS3Path" Lude..= inputS3Path),
            ("ReplaceAllLabels" Lude..=) Lude.<$> replaceAllLabels,
            Lude.Just ("TransformId" Lude..= transformId)
          ]
      )

instance Lude.ToPath StartImportLabelsTaskRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StartImportLabelsTaskRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartImportLabelsTaskRunResponse' smart constructor.
data StartImportLabelsTaskRunResponse = StartImportLabelsTaskRunResponse'
  { -- | The unique identifier for the task run.
    taskRunId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartImportLabelsTaskRunResponse' with the minimum fields required to make a request.
--
-- * 'taskRunId' - The unique identifier for the task run.
-- * 'responseStatus' - The response status code.
mkStartImportLabelsTaskRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartImportLabelsTaskRunResponse
mkStartImportLabelsTaskRunResponse pResponseStatus_ =
  StartImportLabelsTaskRunResponse'
    { taskRunId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siltrrsTaskRunId :: Lens.Lens' StartImportLabelsTaskRunResponse (Lude.Maybe Lude.Text)
siltrrsTaskRunId = Lens.lens (taskRunId :: StartImportLabelsTaskRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskRunId = a} :: StartImportLabelsTaskRunResponse)
{-# DEPRECATED siltrrsTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siltrrsResponseStatus :: Lens.Lens' StartImportLabelsTaskRunResponse Lude.Int
siltrrsResponseStatus = Lens.lens (responseStatus :: StartImportLabelsTaskRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartImportLabelsTaskRunResponse)
{-# DEPRECATED siltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

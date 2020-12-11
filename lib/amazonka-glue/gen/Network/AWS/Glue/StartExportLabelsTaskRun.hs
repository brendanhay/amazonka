{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    seltrrsTaskRunId,
    seltrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartExportLabelsTaskRun' smart constructor.
data StartExportLabelsTaskRun = StartExportLabelsTaskRun'
  { transformId ::
      Lude.Text,
    outputS3Path :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartExportLabelsTaskRun' with the minimum fields required to make a request.
--
-- * 'outputS3Path' - The Amazon S3 path where you export the labels.
-- * 'transformId' - The unique identifier of the machine learning transform.
mkStartExportLabelsTaskRun ::
  -- | 'transformId'
  Lude.Text ->
  -- | 'outputS3Path'
  Lude.Text ->
  StartExportLabelsTaskRun
mkStartExportLabelsTaskRun pTransformId_ pOutputS3Path_ =
  StartExportLabelsTaskRun'
    { transformId = pTransformId_,
      outputS3Path = pOutputS3Path_
    }

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seltrTransformId :: Lens.Lens' StartExportLabelsTaskRun Lude.Text
seltrTransformId = Lens.lens (transformId :: StartExportLabelsTaskRun -> Lude.Text) (\s a -> s {transformId = a} :: StartExportLabelsTaskRun)
{-# DEPRECATED seltrTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The Amazon S3 path where you export the labels.
--
-- /Note:/ Consider using 'outputS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seltrOutputS3Path :: Lens.Lens' StartExportLabelsTaskRun Lude.Text
seltrOutputS3Path = Lens.lens (outputS3Path :: StartExportLabelsTaskRun -> Lude.Text) (\s a -> s {outputS3Path = a} :: StartExportLabelsTaskRun)
{-# DEPRECATED seltrOutputS3Path "Use generic-lens or generic-optics with 'outputS3Path' instead." #-}

instance Lude.AWSRequest StartExportLabelsTaskRun where
  type Rs StartExportLabelsTaskRun = StartExportLabelsTaskRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartExportLabelsTaskRunResponse'
            Lude.<$> (x Lude..?> "TaskRunId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartExportLabelsTaskRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.StartExportLabelsTaskRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartExportLabelsTaskRun where
  toJSON StartExportLabelsTaskRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TransformId" Lude..= transformId),
            Lude.Just ("OutputS3Path" Lude..= outputS3Path)
          ]
      )

instance Lude.ToPath StartExportLabelsTaskRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StartExportLabelsTaskRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartExportLabelsTaskRunResponse' smart constructor.
data StartExportLabelsTaskRunResponse = StartExportLabelsTaskRunResponse'
  { taskRunId ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartExportLabelsTaskRunResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'taskRunId' - The unique identifier for the task run.
mkStartExportLabelsTaskRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartExportLabelsTaskRunResponse
mkStartExportLabelsTaskRunResponse pResponseStatus_ =
  StartExportLabelsTaskRunResponse'
    { taskRunId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seltrrsTaskRunId :: Lens.Lens' StartExportLabelsTaskRunResponse (Lude.Maybe Lude.Text)
seltrrsTaskRunId = Lens.lens (taskRunId :: StartExportLabelsTaskRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskRunId = a} :: StartExportLabelsTaskRunResponse)
{-# DEPRECATED seltrrsTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seltrrsResponseStatus :: Lens.Lens' StartExportLabelsTaskRunResponse Lude.Int
seltrrsResponseStatus = Lens.lens (responseStatus :: StartExportLabelsTaskRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartExportLabelsTaskRunResponse)
{-# DEPRECATED seltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

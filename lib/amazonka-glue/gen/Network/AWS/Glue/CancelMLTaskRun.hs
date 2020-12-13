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
    cmltrrsStatus,
    cmltrrsTransformId,
    cmltrrsTaskRunId,
    cmltrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelMLTaskRun' smart constructor.
data CancelMLTaskRun = CancelMLTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Lude.Text,
    -- | A unique identifier for the task run.
    taskRunId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelMLTaskRun' with the minimum fields required to make a request.
--
-- * 'transformId' - The unique identifier of the machine learning transform.
-- * 'taskRunId' - A unique identifier for the task run.
mkCancelMLTaskRun ::
  -- | 'transformId'
  Lude.Text ->
  -- | 'taskRunId'
  Lude.Text ->
  CancelMLTaskRun
mkCancelMLTaskRun pTransformId_ pTaskRunId_ =
  CancelMLTaskRun'
    { transformId = pTransformId_,
      taskRunId = pTaskRunId_
    }

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrTransformId :: Lens.Lens' CancelMLTaskRun Lude.Text
cmltrTransformId = Lens.lens (transformId :: CancelMLTaskRun -> Lude.Text) (\s a -> s {transformId = a} :: CancelMLTaskRun)
{-# DEPRECATED cmltrTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | A unique identifier for the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrTaskRunId :: Lens.Lens' CancelMLTaskRun Lude.Text
cmltrTaskRunId = Lens.lens (taskRunId :: CancelMLTaskRun -> Lude.Text) (\s a -> s {taskRunId = a} :: CancelMLTaskRun)
{-# DEPRECATED cmltrTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

instance Lude.AWSRequest CancelMLTaskRun where
  type Rs CancelMLTaskRun = CancelMLTaskRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelMLTaskRunResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "TransformId")
            Lude.<*> (x Lude..?> "TaskRunId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelMLTaskRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CancelMLTaskRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelMLTaskRun where
  toJSON CancelMLTaskRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TransformId" Lude..= transformId),
            Lude.Just ("TaskRunId" Lude..= taskRunId)
          ]
      )

instance Lude.ToPath CancelMLTaskRun where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelMLTaskRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelMLTaskRunResponse' smart constructor.
data CancelMLTaskRunResponse = CancelMLTaskRunResponse'
  { -- | The status for this run.
    status :: Lude.Maybe TaskStatusType,
    -- | The unique identifier of the machine learning transform.
    transformId :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the task run.
    taskRunId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelMLTaskRunResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status for this run.
-- * 'transformId' - The unique identifier of the machine learning transform.
-- * 'taskRunId' - The unique identifier for the task run.
-- * 'responseStatus' - The response status code.
mkCancelMLTaskRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelMLTaskRunResponse
mkCancelMLTaskRunResponse pResponseStatus_ =
  CancelMLTaskRunResponse'
    { status = Lude.Nothing,
      transformId = Lude.Nothing,
      taskRunId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status for this run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrsStatus :: Lens.Lens' CancelMLTaskRunResponse (Lude.Maybe TaskStatusType)
cmltrrsStatus = Lens.lens (status :: CancelMLTaskRunResponse -> Lude.Maybe TaskStatusType) (\s a -> s {status = a} :: CancelMLTaskRunResponse)
{-# DEPRECATED cmltrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrsTransformId :: Lens.Lens' CancelMLTaskRunResponse (Lude.Maybe Lude.Text)
cmltrrsTransformId = Lens.lens (transformId :: CancelMLTaskRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {transformId = a} :: CancelMLTaskRunResponse)
{-# DEPRECATED cmltrrsTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The unique identifier for the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrsTaskRunId :: Lens.Lens' CancelMLTaskRunResponse (Lude.Maybe Lude.Text)
cmltrrsTaskRunId = Lens.lens (taskRunId :: CancelMLTaskRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskRunId = a} :: CancelMLTaskRunResponse)
{-# DEPRECATED cmltrrsTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrsResponseStatus :: Lens.Lens' CancelMLTaskRunResponse Lude.Int
cmltrrsResponseStatus = Lens.lens (responseStatus :: CancelMLTaskRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelMLTaskRunResponse)
{-# DEPRECATED cmltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

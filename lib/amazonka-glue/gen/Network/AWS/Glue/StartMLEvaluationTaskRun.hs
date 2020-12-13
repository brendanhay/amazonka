{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartMLEvaluationTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task to estimate the quality of the transform.
--
-- When you provide label sets as examples of truth, AWS Glue machine learning uses some of those examples to learn from them. The rest of the labels are used as a test to estimate quality.
-- Returns a unique identifier for the run. You can call @GetMLTaskRun@ to get more information about the stats of the @EvaluationTaskRun@ .
module Network.AWS.Glue.StartMLEvaluationTaskRun
  ( -- * Creating a request
    StartMLEvaluationTaskRun (..),
    mkStartMLEvaluationTaskRun,

    -- ** Request lenses
    smletrTransformId,

    -- * Destructuring the response
    StartMLEvaluationTaskRunResponse (..),
    mkStartMLEvaluationTaskRunResponse,

    -- ** Response lenses
    smletrrsTaskRunId,
    smletrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartMLEvaluationTaskRun' smart constructor.
newtype StartMLEvaluationTaskRun = StartMLEvaluationTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMLEvaluationTaskRun' with the minimum fields required to make a request.
--
-- * 'transformId' - The unique identifier of the machine learning transform.
mkStartMLEvaluationTaskRun ::
  -- | 'transformId'
  Lude.Text ->
  StartMLEvaluationTaskRun
mkStartMLEvaluationTaskRun pTransformId_ =
  StartMLEvaluationTaskRun' {transformId = pTransformId_}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smletrTransformId :: Lens.Lens' StartMLEvaluationTaskRun Lude.Text
smletrTransformId = Lens.lens (transformId :: StartMLEvaluationTaskRun -> Lude.Text) (\s a -> s {transformId = a} :: StartMLEvaluationTaskRun)
{-# DEPRECATED smletrTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

instance Lude.AWSRequest StartMLEvaluationTaskRun where
  type Rs StartMLEvaluationTaskRun = StartMLEvaluationTaskRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartMLEvaluationTaskRunResponse'
            Lude.<$> (x Lude..?> "TaskRunId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartMLEvaluationTaskRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.StartMLEvaluationTaskRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartMLEvaluationTaskRun where
  toJSON StartMLEvaluationTaskRun' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TransformId" Lude..= transformId)])

instance Lude.ToPath StartMLEvaluationTaskRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StartMLEvaluationTaskRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartMLEvaluationTaskRunResponse' smart constructor.
data StartMLEvaluationTaskRunResponse = StartMLEvaluationTaskRunResponse'
  { -- | The unique identifier associated with this run.
    taskRunId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMLEvaluationTaskRunResponse' with the minimum fields required to make a request.
--
-- * 'taskRunId' - The unique identifier associated with this run.
-- * 'responseStatus' - The response status code.
mkStartMLEvaluationTaskRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMLEvaluationTaskRunResponse
mkStartMLEvaluationTaskRunResponse pResponseStatus_ =
  StartMLEvaluationTaskRunResponse'
    { taskRunId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier associated with this run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smletrrsTaskRunId :: Lens.Lens' StartMLEvaluationTaskRunResponse (Lude.Maybe Lude.Text)
smletrrsTaskRunId = Lens.lens (taskRunId :: StartMLEvaluationTaskRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskRunId = a} :: StartMLEvaluationTaskRunResponse)
{-# DEPRECATED smletrrsTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smletrrsResponseStatus :: Lens.Lens' StartMLEvaluationTaskRunResponse Lude.Int
smletrrsResponseStatus = Lens.lens (responseStatus :: StartMLEvaluationTaskRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMLEvaluationTaskRunResponse)
{-# DEPRECATED smletrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

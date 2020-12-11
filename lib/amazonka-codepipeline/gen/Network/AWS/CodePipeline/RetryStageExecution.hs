{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.RetryStageExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes the pipeline execution by retrying the last failed actions in a stage. You can retry a stage immediately if any of the actions in the stage fail. When you retry, all actions that are still in progress continue working, and failed actions are triggered again.
module Network.AWS.CodePipeline.RetryStageExecution
  ( -- * Creating a request
    RetryStageExecution (..),
    mkRetryStageExecution,

    -- ** Request lenses
    rsePipelineName,
    rseStageName,
    rsePipelineExecutionId,
    rseRetryMode,

    -- * Destructuring the response
    RetryStageExecutionResponse (..),
    mkRetryStageExecutionResponse,

    -- ** Response lenses
    rsersPipelineExecutionId,
    rsersResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @RetryStageExecution@ action.
--
-- /See:/ 'mkRetryStageExecution' smart constructor.
data RetryStageExecution = RetryStageExecution'
  { pipelineName ::
      Lude.Text,
    stageName :: Lude.Text,
    pipelineExecutionId :: Lude.Text,
    retryMode :: StageRetryMode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetryStageExecution' with the minimum fields required to make a request.
--
-- * 'pipelineExecutionId' - The ID of the pipeline execution in the failed stage to be retried. Use the 'GetPipelineState' action to retrieve the current pipelineExecutionId of the failed stage
-- * 'pipelineName' - The name of the pipeline that contains the failed stage.
-- * 'retryMode' - The scope of the retry attempt. Currently, the only supported value is FAILED_ACTIONS.
-- * 'stageName' - The name of the failed stage to be retried.
mkRetryStageExecution ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  -- | 'pipelineExecutionId'
  Lude.Text ->
  -- | 'retryMode'
  StageRetryMode ->
  RetryStageExecution
mkRetryStageExecution
  pPipelineName_
  pStageName_
  pPipelineExecutionId_
  pRetryMode_ =
    RetryStageExecution'
      { pipelineName = pPipelineName_,
        stageName = pStageName_,
        pipelineExecutionId = pPipelineExecutionId_,
        retryMode = pRetryMode_
      }

-- | The name of the pipeline that contains the failed stage.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsePipelineName :: Lens.Lens' RetryStageExecution Lude.Text
rsePipelineName = Lens.lens (pipelineName :: RetryStageExecution -> Lude.Text) (\s a -> s {pipelineName = a} :: RetryStageExecution)
{-# DEPRECATED rsePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The name of the failed stage to be retried.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rseStageName :: Lens.Lens' RetryStageExecution Lude.Text
rseStageName = Lens.lens (stageName :: RetryStageExecution -> Lude.Text) (\s a -> s {stageName = a} :: RetryStageExecution)
{-# DEPRECATED rseStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | The ID of the pipeline execution in the failed stage to be retried. Use the 'GetPipelineState' action to retrieve the current pipelineExecutionId of the failed stage
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsePipelineExecutionId :: Lens.Lens' RetryStageExecution Lude.Text
rsePipelineExecutionId = Lens.lens (pipelineExecutionId :: RetryStageExecution -> Lude.Text) (\s a -> s {pipelineExecutionId = a} :: RetryStageExecution)
{-# DEPRECATED rsePipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The scope of the retry attempt. Currently, the only supported value is FAILED_ACTIONS.
--
-- /Note:/ Consider using 'retryMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rseRetryMode :: Lens.Lens' RetryStageExecution StageRetryMode
rseRetryMode = Lens.lens (retryMode :: RetryStageExecution -> StageRetryMode) (\s a -> s {retryMode = a} :: RetryStageExecution)
{-# DEPRECATED rseRetryMode "Use generic-lens or generic-optics with 'retryMode' instead." #-}

instance Lude.AWSRequest RetryStageExecution where
  type Rs RetryStageExecution = RetryStageExecutionResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          RetryStageExecutionResponse'
            Lude.<$> (x Lude..?> "pipelineExecutionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RetryStageExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.RetryStageExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RetryStageExecution where
  toJSON RetryStageExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineName" Lude..= pipelineName),
            Lude.Just ("stageName" Lude..= stageName),
            Lude.Just ("pipelineExecutionId" Lude..= pipelineExecutionId),
            Lude.Just ("retryMode" Lude..= retryMode)
          ]
      )

instance Lude.ToPath RetryStageExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery RetryStageExecution where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @RetryStageExecution@ action.
--
-- /See:/ 'mkRetryStageExecutionResponse' smart constructor.
data RetryStageExecutionResponse = RetryStageExecutionResponse'
  { pipelineExecutionId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetryStageExecutionResponse' with the minimum fields required to make a request.
--
-- * 'pipelineExecutionId' - The ID of the current workflow execution in the failed stage.
-- * 'responseStatus' - The response status code.
mkRetryStageExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RetryStageExecutionResponse
mkRetryStageExecutionResponse pResponseStatus_ =
  RetryStageExecutionResponse'
    { pipelineExecutionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the current workflow execution in the failed stage.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsersPipelineExecutionId :: Lens.Lens' RetryStageExecutionResponse (Lude.Maybe Lude.Text)
rsersPipelineExecutionId = Lens.lens (pipelineExecutionId :: RetryStageExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: RetryStageExecutionResponse)
{-# DEPRECATED rsersPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsersResponseStatus :: Lens.Lens' RetryStageExecutionResponse Lude.Int
rsersResponseStatus = Lens.lens (responseStatus :: RetryStageExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RetryStageExecutionResponse)
{-# DEPRECATED rsersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

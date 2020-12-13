{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.StopPipelineExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified pipeline execution. You choose to either stop the pipeline execution by completing in-progress actions without starting subsequent actions, or by abandoning in-progress actions. While completing or abandoning in-progress actions, the pipeline execution is in a @Stopping@ state. After all in-progress actions are completed or abandoned, the pipeline execution is in a @Stopped@ state.
module Network.AWS.CodePipeline.StopPipelineExecution
  ( -- * Creating a request
    StopPipelineExecution (..),
    mkStopPipelineExecution,

    -- ** Request lenses
    spePipelineName,
    speAbandon,
    speReason,
    spePipelineExecutionId,

    -- * Destructuring the response
    StopPipelineExecutionResponse (..),
    mkStopPipelineExecutionResponse,

    -- ** Response lenses
    spersPipelineExecutionId,
    spersResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopPipelineExecution' smart constructor.
data StopPipelineExecution = StopPipelineExecution'
  { -- | The name of the pipeline to stop.
    pipelineName :: Lude.Text,
    -- | Use this option to stop the pipeline execution by abandoning, rather than finishing, in-progress actions.
    abandon :: Lude.Maybe Lude.Bool,
    -- | Use this option to enter comments, such as the reason the pipeline was stopped.
    reason :: Lude.Maybe Lude.Text,
    -- | The ID of the pipeline execution to be stopped in the current stage. Use the @GetPipelineState@ action to retrieve the current pipelineExecutionId.
    pipelineExecutionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopPipelineExecution' with the minimum fields required to make a request.
--
-- * 'pipelineName' - The name of the pipeline to stop.
-- * 'abandon' - Use this option to stop the pipeline execution by abandoning, rather than finishing, in-progress actions.
-- * 'reason' - Use this option to enter comments, such as the reason the pipeline was stopped.
-- * 'pipelineExecutionId' - The ID of the pipeline execution to be stopped in the current stage. Use the @GetPipelineState@ action to retrieve the current pipelineExecutionId.
mkStopPipelineExecution ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'pipelineExecutionId'
  Lude.Text ->
  StopPipelineExecution
mkStopPipelineExecution pPipelineName_ pPipelineExecutionId_ =
  StopPipelineExecution'
    { pipelineName = pPipelineName_,
      abandon = Lude.Nothing,
      reason = Lude.Nothing,
      pipelineExecutionId = pPipelineExecutionId_
    }

-- | The name of the pipeline to stop.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spePipelineName :: Lens.Lens' StopPipelineExecution Lude.Text
spePipelineName = Lens.lens (pipelineName :: StopPipelineExecution -> Lude.Text) (\s a -> s {pipelineName = a} :: StopPipelineExecution)
{-# DEPRECATED spePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | Use this option to stop the pipeline execution by abandoning, rather than finishing, in-progress actions.
--
-- /Note:/ Consider using 'abandon' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speAbandon :: Lens.Lens' StopPipelineExecution (Lude.Maybe Lude.Bool)
speAbandon = Lens.lens (abandon :: StopPipelineExecution -> Lude.Maybe Lude.Bool) (\s a -> s {abandon = a} :: StopPipelineExecution)
{-# DEPRECATED speAbandon "Use generic-lens or generic-optics with 'abandon' instead." #-}

-- | Use this option to enter comments, such as the reason the pipeline was stopped.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speReason :: Lens.Lens' StopPipelineExecution (Lude.Maybe Lude.Text)
speReason = Lens.lens (reason :: StopPipelineExecution -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: StopPipelineExecution)
{-# DEPRECATED speReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The ID of the pipeline execution to be stopped in the current stage. Use the @GetPipelineState@ action to retrieve the current pipelineExecutionId.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spePipelineExecutionId :: Lens.Lens' StopPipelineExecution Lude.Text
spePipelineExecutionId = Lens.lens (pipelineExecutionId :: StopPipelineExecution -> Lude.Text) (\s a -> s {pipelineExecutionId = a} :: StopPipelineExecution)
{-# DEPRECATED spePipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

instance Lude.AWSRequest StopPipelineExecution where
  type Rs StopPipelineExecution = StopPipelineExecutionResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopPipelineExecutionResponse'
            Lude.<$> (x Lude..?> "pipelineExecutionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopPipelineExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.StopPipelineExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopPipelineExecution where
  toJSON StopPipelineExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineName" Lude..= pipelineName),
            ("abandon" Lude..=) Lude.<$> abandon,
            ("reason" Lude..=) Lude.<$> reason,
            Lude.Just ("pipelineExecutionId" Lude..= pipelineExecutionId)
          ]
      )

instance Lude.ToPath StopPipelineExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StopPipelineExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopPipelineExecutionResponse' smart constructor.
data StopPipelineExecutionResponse = StopPipelineExecutionResponse'
  { -- | The unique system-generated ID of the pipeline execution that was stopped.
    pipelineExecutionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopPipelineExecutionResponse' with the minimum fields required to make a request.
--
-- * 'pipelineExecutionId' - The unique system-generated ID of the pipeline execution that was stopped.
-- * 'responseStatus' - The response status code.
mkStopPipelineExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopPipelineExecutionResponse
mkStopPipelineExecutionResponse pResponseStatus_ =
  StopPipelineExecutionResponse'
    { pipelineExecutionId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique system-generated ID of the pipeline execution that was stopped.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spersPipelineExecutionId :: Lens.Lens' StopPipelineExecutionResponse (Lude.Maybe Lude.Text)
spersPipelineExecutionId = Lens.lens (pipelineExecutionId :: StopPipelineExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: StopPipelineExecutionResponse)
{-# DEPRECATED spersPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spersResponseStatus :: Lens.Lens' StopPipelineExecutionResponse Lude.Int
spersResponseStatus = Lens.lens (responseStatus :: StopPipelineExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopPipelineExecutionResponse)
{-# DEPRECATED spersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

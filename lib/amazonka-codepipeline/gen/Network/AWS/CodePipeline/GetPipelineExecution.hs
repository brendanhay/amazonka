{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetPipelineExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an execution of a pipeline, including details about artifacts, the pipeline execution ID, and the name, version, and status of the pipeline.
module Network.AWS.CodePipeline.GetPipelineExecution
  ( -- * Creating a request
    GetPipelineExecution (..),
    mkGetPipelineExecution,

    -- ** Request lenses
    gpePipelineName,
    gpePipelineExecutionId,

    -- * Destructuring the response
    GetPipelineExecutionResponse (..),
    mkGetPipelineExecutionResponse,

    -- ** Response lenses
    gpersPipelineExecution,
    gpersResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetPipelineExecution@ action.
--
-- /See:/ 'mkGetPipelineExecution' smart constructor.
data GetPipelineExecution = GetPipelineExecution'
  { pipelineName ::
      Lude.Text,
    pipelineExecutionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPipelineExecution' with the minimum fields required to make a request.
--
-- * 'pipelineExecutionId' - The ID of the pipeline execution about which you want to get execution details.
-- * 'pipelineName' - The name of the pipeline about which you want to get execution details.
mkGetPipelineExecution ::
  -- | 'pipelineName'
  Lude.Text ->
  -- | 'pipelineExecutionId'
  Lude.Text ->
  GetPipelineExecution
mkGetPipelineExecution pPipelineName_ pPipelineExecutionId_ =
  GetPipelineExecution'
    { pipelineName = pPipelineName_,
      pipelineExecutionId = pPipelineExecutionId_
    }

-- | The name of the pipeline about which you want to get execution details.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpePipelineName :: Lens.Lens' GetPipelineExecution Lude.Text
gpePipelineName = Lens.lens (pipelineName :: GetPipelineExecution -> Lude.Text) (\s a -> s {pipelineName = a} :: GetPipelineExecution)
{-# DEPRECATED gpePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The ID of the pipeline execution about which you want to get execution details.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpePipelineExecutionId :: Lens.Lens' GetPipelineExecution Lude.Text
gpePipelineExecutionId = Lens.lens (pipelineExecutionId :: GetPipelineExecution -> Lude.Text) (\s a -> s {pipelineExecutionId = a} :: GetPipelineExecution)
{-# DEPRECATED gpePipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

instance Lude.AWSRequest GetPipelineExecution where
  type Rs GetPipelineExecution = GetPipelineExecutionResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPipelineExecutionResponse'
            Lude.<$> (x Lude..?> "pipelineExecution")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPipelineExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodePipeline_20150709.GetPipelineExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPipelineExecution where
  toJSON GetPipelineExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineName" Lude..= pipelineName),
            Lude.Just ("pipelineExecutionId" Lude..= pipelineExecutionId)
          ]
      )

instance Lude.ToPath GetPipelineExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPipelineExecution where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetPipelineExecution@ action.
--
-- /See:/ 'mkGetPipelineExecutionResponse' smart constructor.
data GetPipelineExecutionResponse = GetPipelineExecutionResponse'
  { pipelineExecution ::
      Lude.Maybe PipelineExecution,
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

-- | Creates a value of 'GetPipelineExecutionResponse' with the minimum fields required to make a request.
--
-- * 'pipelineExecution' - Represents information about the execution of a pipeline.
-- * 'responseStatus' - The response status code.
mkGetPipelineExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPipelineExecutionResponse
mkGetPipelineExecutionResponse pResponseStatus_ =
  GetPipelineExecutionResponse'
    { pipelineExecution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents information about the execution of a pipeline.
--
-- /Note:/ Consider using 'pipelineExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpersPipelineExecution :: Lens.Lens' GetPipelineExecutionResponse (Lude.Maybe PipelineExecution)
gpersPipelineExecution = Lens.lens (pipelineExecution :: GetPipelineExecutionResponse -> Lude.Maybe PipelineExecution) (\s a -> s {pipelineExecution = a} :: GetPipelineExecutionResponse)
{-# DEPRECATED gpersPipelineExecution "Use generic-lens or generic-optics with 'pipelineExecution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpersResponseStatus :: Lens.Lens' GetPipelineExecutionResponse Lude.Int
gpersResponseStatus = Lens.lens (responseStatus :: GetPipelineExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPipelineExecutionResponse)
{-# DEPRECATED gpersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

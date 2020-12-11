{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.StartPipelineExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified pipeline. Specifically, it begins processing the latest commit to the source location specified as part of the pipeline.
module Network.AWS.CodePipeline.StartPipelineExecution
  ( -- * Creating a request
    StartPipelineExecution (..),
    mkStartPipelineExecution,

    -- ** Request lenses
    speClientRequestToken,
    speName,

    -- * Destructuring the response
    StartPipelineExecutionResponse (..),
    mkStartPipelineExecutionResponse,

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

-- | Represents the input of a @StartPipelineExecution@ action.
--
-- /See:/ 'mkStartPipelineExecution' smart constructor.
data StartPipelineExecution = StartPipelineExecution'
  { clientRequestToken ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartPipelineExecution' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - The system-generated unique ID used to identify a unique execution request.
-- * 'name' - The name of the pipeline to start.
mkStartPipelineExecution ::
  -- | 'name'
  Lude.Text ->
  StartPipelineExecution
mkStartPipelineExecution pName_ =
  StartPipelineExecution'
    { clientRequestToken = Lude.Nothing,
      name = pName_
    }

-- | The system-generated unique ID used to identify a unique execution request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speClientRequestToken :: Lens.Lens' StartPipelineExecution (Lude.Maybe Lude.Text)
speClientRequestToken = Lens.lens (clientRequestToken :: StartPipelineExecution -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartPipelineExecution)
{-# DEPRECATED speClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The name of the pipeline to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speName :: Lens.Lens' StartPipelineExecution Lude.Text
speName = Lens.lens (name :: StartPipelineExecution -> Lude.Text) (\s a -> s {name = a} :: StartPipelineExecution)
{-# DEPRECATED speName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StartPipelineExecution where
  type Rs StartPipelineExecution = StartPipelineExecutionResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartPipelineExecutionResponse'
            Lude.<$> (x Lude..?> "pipelineExecutionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartPipelineExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodePipeline_20150709.StartPipelineExecution" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartPipelineExecution where
  toJSON StartPipelineExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath StartPipelineExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StartPipelineExecution where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @StartPipelineExecution@ action.
--
-- /See:/ 'mkStartPipelineExecutionResponse' smart constructor.
data StartPipelineExecutionResponse = StartPipelineExecutionResponse'
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

-- | Creates a value of 'StartPipelineExecutionResponse' with the minimum fields required to make a request.
--
-- * 'pipelineExecutionId' - The unique system-generated ID of the pipeline execution that was started.
-- * 'responseStatus' - The response status code.
mkStartPipelineExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartPipelineExecutionResponse
mkStartPipelineExecutionResponse pResponseStatus_ =
  StartPipelineExecutionResponse'
    { pipelineExecutionId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique system-generated ID of the pipeline execution that was started.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spersPipelineExecutionId :: Lens.Lens' StartPipelineExecutionResponse (Lude.Maybe Lude.Text)
spersPipelineExecutionId = Lens.lens (pipelineExecutionId :: StartPipelineExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: StartPipelineExecutionResponse)
{-# DEPRECATED spersPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spersResponseStatus :: Lens.Lens' StartPipelineExecutionResponse Lude.Int
spersResponseStatus = Lens.lens (responseStatus :: StartPipelineExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartPipelineExecutionResponse)
{-# DEPRECATED spersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    speName,
    speClientRequestToken,

    -- * Destructuring the response
    StartPipelineExecutionResponse (..),
    mkStartPipelineExecutionResponse,

    -- ** Response lenses
    srsPipelineExecutionId,
    srsResponseStatus,
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
  { -- | The name of the pipeline to start.
    name :: Lude.Text,
    -- | The system-generated unique ID used to identify a unique execution request.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartPipelineExecution' with the minimum fields required to make a request.
--
-- * 'name' - The name of the pipeline to start.
-- * 'clientRequestToken' - The system-generated unique ID used to identify a unique execution request.
mkStartPipelineExecution ::
  -- | 'name'
  Lude.Text ->
  StartPipelineExecution
mkStartPipelineExecution pName_ =
  StartPipelineExecution'
    { name = pName_,
      clientRequestToken = Lude.Nothing
    }

-- | The name of the pipeline to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speName :: Lens.Lens' StartPipelineExecution Lude.Text
speName = Lens.lens (name :: StartPipelineExecution -> Lude.Text) (\s a -> s {name = a} :: StartPipelineExecution)
{-# DEPRECATED speName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The system-generated unique ID used to identify a unique execution request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speClientRequestToken :: Lens.Lens' StartPipelineExecution (Lude.Maybe Lude.Text)
speClientRequestToken = Lens.lens (clientRequestToken :: StartPipelineExecution -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartPipelineExecution)
{-# DEPRECATED speClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

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
          [ Lude.Just ("name" Lude..= name),
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken
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
  { -- | The unique system-generated ID of the pipeline execution that was started.
    pipelineExecutionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
srsPipelineExecutionId :: Lens.Lens' StartPipelineExecutionResponse (Lude.Maybe Lude.Text)
srsPipelineExecutionId = Lens.lens (pipelineExecutionId :: StartPipelineExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {pipelineExecutionId = a} :: StartPipelineExecutionResponse)
{-# DEPRECATED srsPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartPipelineExecutionResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartPipelineExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartPipelineExecutionResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

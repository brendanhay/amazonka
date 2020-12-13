{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ActivatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the specified pipeline and starts processing pipeline tasks. If the pipeline does not pass validation, activation fails.
--
-- If you need to pause the pipeline to investigate an issue with a component, such as a data source or script, call 'DeactivatePipeline' .
-- To activate a finished pipeline, modify the end date for the pipeline and then activate it.
module Network.AWS.DataPipeline.ActivatePipeline
  ( -- * Creating a request
    ActivatePipeline (..),
    mkActivatePipeline,

    -- ** Request lenses
    apPipelineId,
    apStartTimestamp,
    apParameterValues,

    -- * Destructuring the response
    ActivatePipelineResponse (..),
    mkActivatePipelineResponse,

    -- ** Response lenses
    aprsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ActivatePipeline.
--
-- /See:/ 'mkActivatePipeline' smart constructor.
data ActivatePipeline = ActivatePipeline'
  { -- | The ID of the pipeline.
    pipelineId :: Lude.Text,
    -- | The date and time to resume the pipeline. By default, the pipeline resumes from the last completed execution.
    startTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | A list of parameter values to pass to the pipeline at activation.
    parameterValues :: Lude.Maybe [ParameterValue]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivatePipeline' with the minimum fields required to make a request.
--
-- * 'pipelineId' - The ID of the pipeline.
-- * 'startTimestamp' - The date and time to resume the pipeline. By default, the pipeline resumes from the last completed execution.
-- * 'parameterValues' - A list of parameter values to pass to the pipeline at activation.
mkActivatePipeline ::
  -- | 'pipelineId'
  Lude.Text ->
  ActivatePipeline
mkActivatePipeline pPipelineId_ =
  ActivatePipeline'
    { pipelineId = pPipelineId_,
      startTimestamp = Lude.Nothing,
      parameterValues = Lude.Nothing
    }

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPipelineId :: Lens.Lens' ActivatePipeline Lude.Text
apPipelineId = Lens.lens (pipelineId :: ActivatePipeline -> Lude.Text) (\s a -> s {pipelineId = a} :: ActivatePipeline)
{-# DEPRECATED apPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The date and time to resume the pipeline. By default, the pipeline resumes from the last completed execution.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apStartTimestamp :: Lens.Lens' ActivatePipeline (Lude.Maybe Lude.Timestamp)
apStartTimestamp = Lens.lens (startTimestamp :: ActivatePipeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTimestamp = a} :: ActivatePipeline)
{-# DEPRECATED apStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

-- | A list of parameter values to pass to the pipeline at activation.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apParameterValues :: Lens.Lens' ActivatePipeline (Lude.Maybe [ParameterValue])
apParameterValues = Lens.lens (parameterValues :: ActivatePipeline -> Lude.Maybe [ParameterValue]) (\s a -> s {parameterValues = a} :: ActivatePipeline)
{-# DEPRECATED apParameterValues "Use generic-lens or generic-optics with 'parameterValues' instead." #-}

instance Lude.AWSRequest ActivatePipeline where
  type Rs ActivatePipeline = ActivatePipelineResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ActivatePipelineResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ActivatePipeline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.ActivatePipeline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ActivatePipeline where
  toJSON ActivatePipeline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineId" Lude..= pipelineId),
            ("startTimestamp" Lude..=) Lude.<$> startTimestamp,
            ("parameterValues" Lude..=) Lude.<$> parameterValues
          ]
      )

instance Lude.ToPath ActivatePipeline where
  toPath = Lude.const "/"

instance Lude.ToQuery ActivatePipeline where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of ActivatePipeline.
--
-- /See:/ 'mkActivatePipelineResponse' smart constructor.
newtype ActivatePipelineResponse = ActivatePipelineResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivatePipelineResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkActivatePipelineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ActivatePipelineResponse
mkActivatePipelineResponse pResponseStatus_ =
  ActivatePipelineResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprsResponseStatus :: Lens.Lens' ActivatePipelineResponse Lude.Int
aprsResponseStatus = Lens.lens (responseStatus :: ActivatePipelineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ActivatePipelineResponse)
{-# DEPRECATED aprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

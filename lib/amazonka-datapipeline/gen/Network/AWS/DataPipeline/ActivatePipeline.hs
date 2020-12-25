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
    apParameterValues,
    apStartTimestamp,

    -- * Destructuring the response
    ActivatePipelineResponse (..),
    mkActivatePipelineResponse,

    -- ** Response lenses
    aprrsResponseStatus,
  )
where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ActivatePipeline.
--
-- /See:/ 'mkActivatePipeline' smart constructor.
data ActivatePipeline = ActivatePipeline'
  { -- | The ID of the pipeline.
    pipelineId :: Types.PipelineId,
    -- | A list of parameter values to pass to the pipeline at activation.
    parameterValues :: Core.Maybe [Types.ParameterValue],
    -- | The date and time to resume the pipeline. By default, the pipeline resumes from the last completed execution.
    startTimestamp :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ActivatePipeline' value with any optional fields omitted.
mkActivatePipeline ::
  -- | 'pipelineId'
  Types.PipelineId ->
  ActivatePipeline
mkActivatePipeline pipelineId =
  ActivatePipeline'
    { pipelineId,
      parameterValues = Core.Nothing,
      startTimestamp = Core.Nothing
    }

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPipelineId :: Lens.Lens' ActivatePipeline Types.PipelineId
apPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED apPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | A list of parameter values to pass to the pipeline at activation.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apParameterValues :: Lens.Lens' ActivatePipeline (Core.Maybe [Types.ParameterValue])
apParameterValues = Lens.field @"parameterValues"
{-# DEPRECATED apParameterValues "Use generic-lens or generic-optics with 'parameterValues' instead." #-}

-- | The date and time to resume the pipeline. By default, the pipeline resumes from the last completed execution.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apStartTimestamp :: Lens.Lens' ActivatePipeline (Core.Maybe Core.NominalDiffTime)
apStartTimestamp = Lens.field @"startTimestamp"
{-# DEPRECATED apStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

instance Core.FromJSON ActivatePipeline where
  toJSON ActivatePipeline {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineId" Core..= pipelineId),
            ("parameterValues" Core..=) Core.<$> parameterValues,
            ("startTimestamp" Core..=) Core.<$> startTimestamp
          ]
      )

instance Core.AWSRequest ActivatePipeline where
  type Rs ActivatePipeline = ActivatePipelineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DataPipeline.ActivatePipeline")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ActivatePipelineResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of ActivatePipeline.
--
-- /See:/ 'mkActivatePipelineResponse' smart constructor.
newtype ActivatePipelineResponse = ActivatePipelineResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ActivatePipelineResponse' value with any optional fields omitted.
mkActivatePipelineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ActivatePipelineResponse
mkActivatePipelineResponse responseStatus =
  ActivatePipelineResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprrsResponseStatus :: Lens.Lens' ActivatePipelineResponse Core.Int
aprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

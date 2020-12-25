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
    spePipelineExecutionId,
    speAbandon,
    speReason,

    -- * Destructuring the response
    StopPipelineExecutionResponse (..),
    mkStopPipelineExecutionResponse,

    -- ** Response lenses
    srsPipelineExecutionId,
    srsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopPipelineExecution' smart constructor.
data StopPipelineExecution = StopPipelineExecution'
  { -- | The name of the pipeline to stop.
    pipelineName :: Types.PipelineName,
    -- | The ID of the pipeline execution to be stopped in the current stage. Use the @GetPipelineState@ action to retrieve the current pipelineExecutionId.
    pipelineExecutionId :: Types.PipelineExecutionId,
    -- | Use this option to stop the pipeline execution by abandoning, rather than finishing, in-progress actions.
    abandon :: Core.Maybe Core.Bool,
    -- | Use this option to enter comments, such as the reason the pipeline was stopped.
    reason :: Core.Maybe Types.StopPipelineExecutionReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopPipelineExecution' value with any optional fields omitted.
mkStopPipelineExecution ::
  -- | 'pipelineName'
  Types.PipelineName ->
  -- | 'pipelineExecutionId'
  Types.PipelineExecutionId ->
  StopPipelineExecution
mkStopPipelineExecution pipelineName pipelineExecutionId =
  StopPipelineExecution'
    { pipelineName,
      pipelineExecutionId,
      abandon = Core.Nothing,
      reason = Core.Nothing
    }

-- | The name of the pipeline to stop.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spePipelineName :: Lens.Lens' StopPipelineExecution Types.PipelineName
spePipelineName = Lens.field @"pipelineName"
{-# DEPRECATED spePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The ID of the pipeline execution to be stopped in the current stage. Use the @GetPipelineState@ action to retrieve the current pipelineExecutionId.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spePipelineExecutionId :: Lens.Lens' StopPipelineExecution Types.PipelineExecutionId
spePipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# DEPRECATED spePipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | Use this option to stop the pipeline execution by abandoning, rather than finishing, in-progress actions.
--
-- /Note:/ Consider using 'abandon' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speAbandon :: Lens.Lens' StopPipelineExecution (Core.Maybe Core.Bool)
speAbandon = Lens.field @"abandon"
{-# DEPRECATED speAbandon "Use generic-lens or generic-optics with 'abandon' instead." #-}

-- | Use this option to enter comments, such as the reason the pipeline was stopped.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
speReason :: Lens.Lens' StopPipelineExecution (Core.Maybe Types.StopPipelineExecutionReason)
speReason = Lens.field @"reason"
{-# DEPRECATED speReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON StopPipelineExecution where
  toJSON StopPipelineExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            Core.Just ("pipelineExecutionId" Core..= pipelineExecutionId),
            ("abandon" Core..=) Core.<$> abandon,
            ("reason" Core..=) Core.<$> reason
          ]
      )

instance Core.AWSRequest StopPipelineExecution where
  type Rs StopPipelineExecution = StopPipelineExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.StopPipelineExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopPipelineExecutionResponse'
            Core.<$> (x Core..:? "pipelineExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopPipelineExecutionResponse' smart constructor.
data StopPipelineExecutionResponse = StopPipelineExecutionResponse'
  { -- | The unique system-generated ID of the pipeline execution that was stopped.
    pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopPipelineExecutionResponse' value with any optional fields omitted.
mkStopPipelineExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopPipelineExecutionResponse
mkStopPipelineExecutionResponse responseStatus =
  StopPipelineExecutionResponse'
    { pipelineExecutionId =
        Core.Nothing,
      responseStatus
    }

-- | The unique system-generated ID of the pipeline execution that was stopped.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsPipelineExecutionId :: Lens.Lens' StopPipelineExecutionResponse (Core.Maybe Types.PipelineExecutionId)
srsPipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# DEPRECATED srsPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopPipelineExecutionResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

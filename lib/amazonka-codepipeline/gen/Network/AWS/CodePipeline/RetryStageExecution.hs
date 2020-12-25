{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    rserrsPipelineExecutionId,
    rserrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @RetryStageExecution@ action.
--
-- /See:/ 'mkRetryStageExecution' smart constructor.
data RetryStageExecution = RetryStageExecution'
  { -- | The name of the pipeline that contains the failed stage.
    pipelineName :: Types.PipelineName,
    -- | The name of the failed stage to be retried.
    stageName :: Types.StageName,
    -- | The ID of the pipeline execution in the failed stage to be retried. Use the 'GetPipelineState' action to retrieve the current pipelineExecutionId of the failed stage
    pipelineExecutionId :: Types.PipelineExecutionId,
    -- | The scope of the retry attempt. Currently, the only supported value is FAILED_ACTIONS.
    retryMode :: Types.StageRetryMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetryStageExecution' value with any optional fields omitted.
mkRetryStageExecution ::
  -- | 'pipelineName'
  Types.PipelineName ->
  -- | 'stageName'
  Types.StageName ->
  -- | 'pipelineExecutionId'
  Types.PipelineExecutionId ->
  -- | 'retryMode'
  Types.StageRetryMode ->
  RetryStageExecution
mkRetryStageExecution
  pipelineName
  stageName
  pipelineExecutionId
  retryMode =
    RetryStageExecution'
      { pipelineName,
        stageName,
        pipelineExecutionId,
        retryMode
      }

-- | The name of the pipeline that contains the failed stage.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsePipelineName :: Lens.Lens' RetryStageExecution Types.PipelineName
rsePipelineName = Lens.field @"pipelineName"
{-# DEPRECATED rsePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The name of the failed stage to be retried.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rseStageName :: Lens.Lens' RetryStageExecution Types.StageName
rseStageName = Lens.field @"stageName"
{-# DEPRECATED rseStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

-- | The ID of the pipeline execution in the failed stage to be retried. Use the 'GetPipelineState' action to retrieve the current pipelineExecutionId of the failed stage
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsePipelineExecutionId :: Lens.Lens' RetryStageExecution Types.PipelineExecutionId
rsePipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# DEPRECATED rsePipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The scope of the retry attempt. Currently, the only supported value is FAILED_ACTIONS.
--
-- /Note:/ Consider using 'retryMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rseRetryMode :: Lens.Lens' RetryStageExecution Types.StageRetryMode
rseRetryMode = Lens.field @"retryMode"
{-# DEPRECATED rseRetryMode "Use generic-lens or generic-optics with 'retryMode' instead." #-}

instance Core.FromJSON RetryStageExecution where
  toJSON RetryStageExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            Core.Just ("stageName" Core..= stageName),
            Core.Just ("pipelineExecutionId" Core..= pipelineExecutionId),
            Core.Just ("retryMode" Core..= retryMode)
          ]
      )

instance Core.AWSRequest RetryStageExecution where
  type Rs RetryStageExecution = RetryStageExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.RetryStageExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RetryStageExecutionResponse'
            Core.<$> (x Core..:? "pipelineExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @RetryStageExecution@ action.
--
-- /See:/ 'mkRetryStageExecutionResponse' smart constructor.
data RetryStageExecutionResponse = RetryStageExecutionResponse'
  { -- | The ID of the current workflow execution in the failed stage.
    pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetryStageExecutionResponse' value with any optional fields omitted.
mkRetryStageExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RetryStageExecutionResponse
mkRetryStageExecutionResponse responseStatus =
  RetryStageExecutionResponse'
    { pipelineExecutionId = Core.Nothing,
      responseStatus
    }

-- | The ID of the current workflow execution in the failed stage.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rserrsPipelineExecutionId :: Lens.Lens' RetryStageExecutionResponse (Core.Maybe Types.PipelineExecutionId)
rserrsPipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# DEPRECATED rserrsPipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rserrsResponseStatus :: Lens.Lens' RetryStageExecutionResponse Core.Int
rserrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rserrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

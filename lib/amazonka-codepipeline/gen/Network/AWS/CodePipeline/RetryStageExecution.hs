{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RetryStageExecution (..)
    , mkRetryStageExecution
    -- ** Request lenses
    , rsePipelineName
    , rseStageName
    , rsePipelineExecutionId
    , rseRetryMode

    -- * Destructuring the response
    , RetryStageExecutionResponse (..)
    , mkRetryStageExecutionResponse
    -- ** Response lenses
    , rserrsPipelineExecutionId
    , rserrsResponseStatus
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @RetryStageExecution@ action.
--
-- /See:/ 'mkRetryStageExecution' smart constructor.
data RetryStageExecution = RetryStageExecution'
  { pipelineName :: Types.PipelineName
    -- ^ The name of the pipeline that contains the failed stage.
  , stageName :: Types.StageName
    -- ^ The name of the failed stage to be retried.
  , pipelineExecutionId :: Types.PipelineExecutionId
    -- ^ The ID of the pipeline execution in the failed stage to be retried. Use the 'GetPipelineState' action to retrieve the current pipelineExecutionId of the failed stage
  , retryMode :: Types.StageRetryMode
    -- ^ The scope of the retry attempt. Currently, the only supported value is FAILED_ACTIONS.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetryStageExecution' value with any optional fields omitted.
mkRetryStageExecution
    :: Types.PipelineName -- ^ 'pipelineName'
    -> Types.StageName -- ^ 'stageName'
    -> Types.PipelineExecutionId -- ^ 'pipelineExecutionId'
    -> Types.StageRetryMode -- ^ 'retryMode'
    -> RetryStageExecution
mkRetryStageExecution pipelineName stageName pipelineExecutionId
  retryMode
  = RetryStageExecution'{pipelineName, stageName,
                         pipelineExecutionId, retryMode}

-- | The name of the pipeline that contains the failed stage.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsePipelineName :: Lens.Lens' RetryStageExecution Types.PipelineName
rsePipelineName = Lens.field @"pipelineName"
{-# INLINEABLE rsePipelineName #-}
{-# DEPRECATED pipelineName "Use generic-lens or generic-optics with 'pipelineName' instead"  #-}

-- | The name of the failed stage to be retried.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rseStageName :: Lens.Lens' RetryStageExecution Types.StageName
rseStageName = Lens.field @"stageName"
{-# INLINEABLE rseStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | The ID of the pipeline execution in the failed stage to be retried. Use the 'GetPipelineState' action to retrieve the current pipelineExecutionId of the failed stage
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsePipelineExecutionId :: Lens.Lens' RetryStageExecution Types.PipelineExecutionId
rsePipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# INLINEABLE rsePipelineExecutionId #-}
{-# DEPRECATED pipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead"  #-}

-- | The scope of the retry attempt. Currently, the only supported value is FAILED_ACTIONS.
--
-- /Note:/ Consider using 'retryMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rseRetryMode :: Lens.Lens' RetryStageExecution Types.StageRetryMode
rseRetryMode = Lens.field @"retryMode"
{-# INLINEABLE rseRetryMode #-}
{-# DEPRECATED retryMode "Use generic-lens or generic-optics with 'retryMode' instead"  #-}

instance Core.ToQuery RetryStageExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RetryStageExecution where
        toHeaders RetryStageExecution{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.RetryStageExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RetryStageExecution where
        toJSON RetryStageExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineName" Core..= pipelineName),
                  Core.Just ("stageName" Core..= stageName),
                  Core.Just ("pipelineExecutionId" Core..= pipelineExecutionId),
                  Core.Just ("retryMode" Core..= retryMode)])

instance Core.AWSRequest RetryStageExecution where
        type Rs RetryStageExecution = RetryStageExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RetryStageExecutionResponse' Core.<$>
                   (x Core..:? "pipelineExecutionId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @RetryStageExecution@ action.
--
-- /See:/ 'mkRetryStageExecutionResponse' smart constructor.
data RetryStageExecutionResponse = RetryStageExecutionResponse'
  { pipelineExecutionId :: Core.Maybe Types.PipelineExecutionId
    -- ^ The ID of the current workflow execution in the failed stage.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetryStageExecutionResponse' value with any optional fields omitted.
mkRetryStageExecutionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RetryStageExecutionResponse
mkRetryStageExecutionResponse responseStatus
  = RetryStageExecutionResponse'{pipelineExecutionId = Core.Nothing,
                                 responseStatus}

-- | The ID of the current workflow execution in the failed stage.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rserrsPipelineExecutionId :: Lens.Lens' RetryStageExecutionResponse (Core.Maybe Types.PipelineExecutionId)
rserrsPipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# INLINEABLE rserrsPipelineExecutionId #-}
{-# DEPRECATED pipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rserrsResponseStatus :: Lens.Lens' RetryStageExecutionResponse Core.Int
rserrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rserrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

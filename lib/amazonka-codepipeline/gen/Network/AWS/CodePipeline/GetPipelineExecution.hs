{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetPipelineExecution (..)
    , mkGetPipelineExecution
    -- ** Request lenses
    , gpePipelineName
    , gpePipelineExecutionId

    -- * Destructuring the response
    , GetPipelineExecutionResponse (..)
    , mkGetPipelineExecutionResponse
    -- ** Response lenses
    , gperrsPipelineExecution
    , gperrsResponseStatus
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetPipelineExecution@ action.
--
-- /See:/ 'mkGetPipelineExecution' smart constructor.
data GetPipelineExecution = GetPipelineExecution'
  { pipelineName :: Types.PipelineName
    -- ^ The name of the pipeline about which you want to get execution details.
  , pipelineExecutionId :: Types.PipelineExecutionId
    -- ^ The ID of the pipeline execution about which you want to get execution details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPipelineExecution' value with any optional fields omitted.
mkGetPipelineExecution
    :: Types.PipelineName -- ^ 'pipelineName'
    -> Types.PipelineExecutionId -- ^ 'pipelineExecutionId'
    -> GetPipelineExecution
mkGetPipelineExecution pipelineName pipelineExecutionId
  = GetPipelineExecution'{pipelineName, pipelineExecutionId}

-- | The name of the pipeline about which you want to get execution details.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpePipelineName :: Lens.Lens' GetPipelineExecution Types.PipelineName
gpePipelineName = Lens.field @"pipelineName"
{-# INLINEABLE gpePipelineName #-}
{-# DEPRECATED pipelineName "Use generic-lens or generic-optics with 'pipelineName' instead"  #-}

-- | The ID of the pipeline execution about which you want to get execution details.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpePipelineExecutionId :: Lens.Lens' GetPipelineExecution Types.PipelineExecutionId
gpePipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# INLINEABLE gpePipelineExecutionId #-}
{-# DEPRECATED pipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead"  #-}

instance Core.ToQuery GetPipelineExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPipelineExecution where
        toHeaders GetPipelineExecution{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.GetPipelineExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPipelineExecution where
        toJSON GetPipelineExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineName" Core..= pipelineName),
                  Core.Just ("pipelineExecutionId" Core..= pipelineExecutionId)])

instance Core.AWSRequest GetPipelineExecution where
        type Rs GetPipelineExecution = GetPipelineExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPipelineExecutionResponse' Core.<$>
                   (x Core..:? "pipelineExecution") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @GetPipelineExecution@ action.
--
-- /See:/ 'mkGetPipelineExecutionResponse' smart constructor.
data GetPipelineExecutionResponse = GetPipelineExecutionResponse'
  { pipelineExecution :: Core.Maybe Types.PipelineExecution
    -- ^ Represents information about the execution of a pipeline.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetPipelineExecutionResponse' value with any optional fields omitted.
mkGetPipelineExecutionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPipelineExecutionResponse
mkGetPipelineExecutionResponse responseStatus
  = GetPipelineExecutionResponse'{pipelineExecution = Core.Nothing,
                                  responseStatus}

-- | Represents information about the execution of a pipeline.
--
-- /Note:/ Consider using 'pipelineExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gperrsPipelineExecution :: Lens.Lens' GetPipelineExecutionResponse (Core.Maybe Types.PipelineExecution)
gperrsPipelineExecution = Lens.field @"pipelineExecution"
{-# INLINEABLE gperrsPipelineExecution #-}
{-# DEPRECATED pipelineExecution "Use generic-lens or generic-optics with 'pipelineExecution' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gperrsResponseStatus :: Lens.Lens' GetPipelineExecutionResponse Core.Int
gperrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gperrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ActivatePipeline (..)
    , mkActivatePipeline
    -- ** Request lenses
    , apPipelineId
    , apParameterValues
    , apStartTimestamp

    -- * Destructuring the response
    , ActivatePipelineResponse (..)
    , mkActivatePipelineResponse
    -- ** Response lenses
    , aprrsResponseStatus
    ) where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ActivatePipeline.
--
-- /See:/ 'mkActivatePipeline' smart constructor.
data ActivatePipeline = ActivatePipeline'
  { pipelineId :: Types.PipelineId
    -- ^ The ID of the pipeline.
  , parameterValues :: Core.Maybe [Types.ParameterValue]
    -- ^ A list of parameter values to pass to the pipeline at activation.
  , startTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time to resume the pipeline. By default, the pipeline resumes from the last completed execution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ActivatePipeline' value with any optional fields omitted.
mkActivatePipeline
    :: Types.PipelineId -- ^ 'pipelineId'
    -> ActivatePipeline
mkActivatePipeline pipelineId
  = ActivatePipeline'{pipelineId, parameterValues = Core.Nothing,
                      startTimestamp = Core.Nothing}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPipelineId :: Lens.Lens' ActivatePipeline Types.PipelineId
apPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE apPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | A list of parameter values to pass to the pipeline at activation.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apParameterValues :: Lens.Lens' ActivatePipeline (Core.Maybe [Types.ParameterValue])
apParameterValues = Lens.field @"parameterValues"
{-# INLINEABLE apParameterValues #-}
{-# DEPRECATED parameterValues "Use generic-lens or generic-optics with 'parameterValues' instead"  #-}

-- | The date and time to resume the pipeline. By default, the pipeline resumes from the last completed execution.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apStartTimestamp :: Lens.Lens' ActivatePipeline (Core.Maybe Core.NominalDiffTime)
apStartTimestamp = Lens.field @"startTimestamp"
{-# INLINEABLE apStartTimestamp #-}
{-# DEPRECATED startTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead"  #-}

instance Core.ToQuery ActivatePipeline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ActivatePipeline where
        toHeaders ActivatePipeline{..}
          = Core.pure ("X-Amz-Target", "DataPipeline.ActivatePipeline")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ActivatePipeline where
        toJSON ActivatePipeline{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineId" Core..= pipelineId),
                  ("parameterValues" Core..=) Core.<$> parameterValues,
                  ("startTimestamp" Core..=) Core.<$> startTimestamp])

instance Core.AWSRequest ActivatePipeline where
        type Rs ActivatePipeline = ActivatePipelineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ActivatePipelineResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of ActivatePipeline.
--
-- /See:/ 'mkActivatePipelineResponse' smart constructor.
newtype ActivatePipelineResponse = ActivatePipelineResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ActivatePipelineResponse' value with any optional fields omitted.
mkActivatePipelineResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ActivatePipelineResponse
mkActivatePipelineResponse responseStatus
  = ActivatePipelineResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprrsResponseStatus :: Lens.Lens' ActivatePipelineResponse Core.Int
aprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

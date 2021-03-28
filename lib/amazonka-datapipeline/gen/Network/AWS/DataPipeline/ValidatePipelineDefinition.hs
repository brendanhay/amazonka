{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ValidatePipelineDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the specified pipeline definition to ensure that it is well formed and can be run without error.
module Network.AWS.DataPipeline.ValidatePipelineDefinition
    (
    -- * Creating a request
      ValidatePipelineDefinition (..)
    , mkValidatePipelineDefinition
    -- ** Request lenses
    , vpdPipelineId
    , vpdPipelineObjects
    , vpdParameterObjects
    , vpdParameterValues

    -- * Destructuring the response
    , ValidatePipelineDefinitionResponse (..)
    , mkValidatePipelineDefinitionResponse
    -- ** Response lenses
    , vpdrrsErrored
    , vpdrrsValidationErrors
    , vpdrrsValidationWarnings
    , vpdrrsResponseStatus
    ) where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ValidatePipelineDefinition.
--
-- /See:/ 'mkValidatePipelineDefinition' smart constructor.
data ValidatePipelineDefinition = ValidatePipelineDefinition'
  { pipelineId :: Types.PipelineId
    -- ^ The ID of the pipeline.
  , pipelineObjects :: [Types.PipelineObject]
    -- ^ The objects that define the pipeline changes to validate against the pipeline.
  , parameterObjects :: Core.Maybe [Types.ParameterObject]
    -- ^ The parameter objects used with the pipeline.
  , parameterValues :: Core.Maybe [Types.ParameterValue]
    -- ^ The parameter values used with the pipeline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidatePipelineDefinition' value with any optional fields omitted.
mkValidatePipelineDefinition
    :: Types.PipelineId -- ^ 'pipelineId'
    -> ValidatePipelineDefinition
mkValidatePipelineDefinition pipelineId
  = ValidatePipelineDefinition'{pipelineId,
                                pipelineObjects = Core.mempty, parameterObjects = Core.Nothing,
                                parameterValues = Core.Nothing}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdPipelineId :: Lens.Lens' ValidatePipelineDefinition Types.PipelineId
vpdPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE vpdPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | The objects that define the pipeline changes to validate against the pipeline.
--
-- /Note:/ Consider using 'pipelineObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdPipelineObjects :: Lens.Lens' ValidatePipelineDefinition [Types.PipelineObject]
vpdPipelineObjects = Lens.field @"pipelineObjects"
{-# INLINEABLE vpdPipelineObjects #-}
{-# DEPRECATED pipelineObjects "Use generic-lens or generic-optics with 'pipelineObjects' instead"  #-}

-- | The parameter objects used with the pipeline.
--
-- /Note:/ Consider using 'parameterObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdParameterObjects :: Lens.Lens' ValidatePipelineDefinition (Core.Maybe [Types.ParameterObject])
vpdParameterObjects = Lens.field @"parameterObjects"
{-# INLINEABLE vpdParameterObjects #-}
{-# DEPRECATED parameterObjects "Use generic-lens or generic-optics with 'parameterObjects' instead"  #-}

-- | The parameter values used with the pipeline.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdParameterValues :: Lens.Lens' ValidatePipelineDefinition (Core.Maybe [Types.ParameterValue])
vpdParameterValues = Lens.field @"parameterValues"
{-# INLINEABLE vpdParameterValues #-}
{-# DEPRECATED parameterValues "Use generic-lens or generic-optics with 'parameterValues' instead"  #-}

instance Core.ToQuery ValidatePipelineDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ValidatePipelineDefinition where
        toHeaders ValidatePipelineDefinition{..}
          = Core.pure
              ("X-Amz-Target", "DataPipeline.ValidatePipelineDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ValidatePipelineDefinition where
        toJSON ValidatePipelineDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineId" Core..= pipelineId),
                  Core.Just ("pipelineObjects" Core..= pipelineObjects),
                  ("parameterObjects" Core..=) Core.<$> parameterObjects,
                  ("parameterValues" Core..=) Core.<$> parameterValues])

instance Core.AWSRequest ValidatePipelineDefinition where
        type Rs ValidatePipelineDefinition =
             ValidatePipelineDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ValidatePipelineDefinitionResponse' Core.<$>
                   (x Core..: "errored") Core.<*> x Core..:? "validationErrors"
                     Core.<*> x Core..:? "validationWarnings"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of ValidatePipelineDefinition.
--
-- /See:/ 'mkValidatePipelineDefinitionResponse' smart constructor.
data ValidatePipelineDefinitionResponse = ValidatePipelineDefinitionResponse'
  { errored :: Core.Bool
    -- ^ Indicates whether there were validation errors.
  , validationErrors :: Core.Maybe [Types.ValidationError]
    -- ^ Any validation errors that were found.
  , validationWarnings :: Core.Maybe [Types.ValidationWarning]
    -- ^ Any validation warnings that were found.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidatePipelineDefinitionResponse' value with any optional fields omitted.
mkValidatePipelineDefinitionResponse
    :: Core.Bool -- ^ 'errored'
    -> Core.Int -- ^ 'responseStatus'
    -> ValidatePipelineDefinitionResponse
mkValidatePipelineDefinitionResponse errored responseStatus
  = ValidatePipelineDefinitionResponse'{errored,
                                        validationErrors = Core.Nothing,
                                        validationWarnings = Core.Nothing, responseStatus}

-- | Indicates whether there were validation errors.
--
-- /Note:/ Consider using 'errored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdrrsErrored :: Lens.Lens' ValidatePipelineDefinitionResponse Core.Bool
vpdrrsErrored = Lens.field @"errored"
{-# INLINEABLE vpdrrsErrored #-}
{-# DEPRECATED errored "Use generic-lens or generic-optics with 'errored' instead"  #-}

-- | Any validation errors that were found.
--
-- /Note:/ Consider using 'validationErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdrrsValidationErrors :: Lens.Lens' ValidatePipelineDefinitionResponse (Core.Maybe [Types.ValidationError])
vpdrrsValidationErrors = Lens.field @"validationErrors"
{-# INLINEABLE vpdrrsValidationErrors #-}
{-# DEPRECATED validationErrors "Use generic-lens or generic-optics with 'validationErrors' instead"  #-}

-- | Any validation warnings that were found.
--
-- /Note:/ Consider using 'validationWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdrrsValidationWarnings :: Lens.Lens' ValidatePipelineDefinitionResponse (Core.Maybe [Types.ValidationWarning])
vpdrrsValidationWarnings = Lens.field @"validationWarnings"
{-# INLINEABLE vpdrrsValidationWarnings #-}
{-# DEPRECATED validationWarnings "Use generic-lens or generic-optics with 'validationWarnings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpdrrsResponseStatus :: Lens.Lens' ValidatePipelineDefinitionResponse Core.Int
vpdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE vpdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

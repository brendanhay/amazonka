{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.GetPipelineDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the definition of the specified pipeline. You can call @GetPipelineDefinition@ to retrieve the pipeline definition that you provided using 'PutPipelineDefinition' .
module Network.AWS.DataPipeline.GetPipelineDefinition
    (
    -- * Creating a request
      GetPipelineDefinition (..)
    , mkGetPipelineDefinition
    -- ** Request lenses
    , gpdPipelineId
    , gpdVersion

    -- * Destructuring the response
    , GetPipelineDefinitionResponse (..)
    , mkGetPipelineDefinitionResponse
    -- ** Response lenses
    , gpdrrsParameterObjects
    , gpdrrsParameterValues
    , gpdrrsPipelineObjects
    , gpdrrsResponseStatus
    ) where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for GetPipelineDefinition.
--
-- /See:/ 'mkGetPipelineDefinition' smart constructor.
data GetPipelineDefinition = GetPipelineDefinition'
  { pipelineId :: Types.PipelineId
    -- ^ The ID of the pipeline.
  , version :: Core.Maybe Core.Text
    -- ^ The version of the pipeline definition to retrieve. Set this parameter to @latest@ (default) to use the last definition saved to the pipeline or @active@ to use the last definition that was activated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPipelineDefinition' value with any optional fields omitted.
mkGetPipelineDefinition
    :: Types.PipelineId -- ^ 'pipelineId'
    -> GetPipelineDefinition
mkGetPipelineDefinition pipelineId
  = GetPipelineDefinition'{pipelineId, version = Core.Nothing}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdPipelineId :: Lens.Lens' GetPipelineDefinition Types.PipelineId
gpdPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE gpdPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | The version of the pipeline definition to retrieve. Set this parameter to @latest@ (default) to use the last definition saved to the pipeline or @active@ to use the last definition that was activated.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdVersion :: Lens.Lens' GetPipelineDefinition (Core.Maybe Core.Text)
gpdVersion = Lens.field @"version"
{-# INLINEABLE gpdVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery GetPipelineDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPipelineDefinition where
        toHeaders GetPipelineDefinition{..}
          = Core.pure ("X-Amz-Target", "DataPipeline.GetPipelineDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPipelineDefinition where
        toJSON GetPipelineDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineId" Core..= pipelineId),
                  ("version" Core..=) Core.<$> version])

instance Core.AWSRequest GetPipelineDefinition where
        type Rs GetPipelineDefinition = GetPipelineDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPipelineDefinitionResponse' Core.<$>
                   (x Core..:? "parameterObjects") Core.<*>
                     x Core..:? "parameterValues"
                     Core.<*> x Core..:? "pipelineObjects"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of GetPipelineDefinition.
--
-- /See:/ 'mkGetPipelineDefinitionResponse' smart constructor.
data GetPipelineDefinitionResponse = GetPipelineDefinitionResponse'
  { parameterObjects :: Core.Maybe [Types.ParameterObject]
    -- ^ The parameter objects used in the pipeline definition.
  , parameterValues :: Core.Maybe [Types.ParameterValue]
    -- ^ The parameter values used in the pipeline definition.
  , pipelineObjects :: Core.Maybe [Types.PipelineObject]
    -- ^ The objects defined in the pipeline.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPipelineDefinitionResponse' value with any optional fields omitted.
mkGetPipelineDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPipelineDefinitionResponse
mkGetPipelineDefinitionResponse responseStatus
  = GetPipelineDefinitionResponse'{parameterObjects = Core.Nothing,
                                   parameterValues = Core.Nothing, pipelineObjects = Core.Nothing,
                                   responseStatus}

-- | The parameter objects used in the pipeline definition.
--
-- /Note:/ Consider using 'parameterObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrrsParameterObjects :: Lens.Lens' GetPipelineDefinitionResponse (Core.Maybe [Types.ParameterObject])
gpdrrsParameterObjects = Lens.field @"parameterObjects"
{-# INLINEABLE gpdrrsParameterObjects #-}
{-# DEPRECATED parameterObjects "Use generic-lens or generic-optics with 'parameterObjects' instead"  #-}

-- | The parameter values used in the pipeline definition.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrrsParameterValues :: Lens.Lens' GetPipelineDefinitionResponse (Core.Maybe [Types.ParameterValue])
gpdrrsParameterValues = Lens.field @"parameterValues"
{-# INLINEABLE gpdrrsParameterValues #-}
{-# DEPRECATED parameterValues "Use generic-lens or generic-optics with 'parameterValues' instead"  #-}

-- | The objects defined in the pipeline.
--
-- /Note:/ Consider using 'pipelineObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrrsPipelineObjects :: Lens.Lens' GetPipelineDefinitionResponse (Core.Maybe [Types.PipelineObject])
gpdrrsPipelineObjects = Lens.field @"pipelineObjects"
{-# INLINEABLE gpdrrsPipelineObjects #-}
{-# DEPRECATED pipelineObjects "Use generic-lens or generic-optics with 'pipelineObjects' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpdrrsResponseStatus :: Lens.Lens' GetPipelineDefinitionResponse Core.Int
gpdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

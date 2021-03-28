{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.CreatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pipeline.
module Network.AWS.CodePipeline.CreatePipeline
    (
    -- * Creating a request
      CreatePipeline (..)
    , mkCreatePipeline
    -- ** Request lenses
    , cpPipeline
    , cpTags

    -- * Destructuring the response
    , CreatePipelineResponse (..)
    , mkCreatePipelineResponse
    -- ** Response lenses
    , cprrsPipeline
    , cprrsTags
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreatePipeline@ action.
--
-- /See:/ 'mkCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { pipeline :: Types.PipelineDeclaration
    -- ^ Represents the structure of actions and stages to be performed in the pipeline. 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the pipeline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePipeline' value with any optional fields omitted.
mkCreatePipeline
    :: Types.PipelineDeclaration -- ^ 'pipeline'
    -> CreatePipeline
mkCreatePipeline pipeline
  = CreatePipeline'{pipeline, tags = Core.Nothing}

-- | Represents the structure of actions and stages to be performed in the pipeline. 
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPipeline :: Lens.Lens' CreatePipeline Types.PipelineDeclaration
cpPipeline = Lens.field @"pipeline"
{-# INLINEABLE cpPipeline #-}
{-# DEPRECATED pipeline "Use generic-lens or generic-optics with 'pipeline' instead"  #-}

-- | The tags for the pipeline.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePipeline (Core.Maybe [Types.Tag])
cpTags = Lens.field @"tags"
{-# INLINEABLE cpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreatePipeline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePipeline where
        toHeaders CreatePipeline{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.CreatePipeline")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePipeline where
        toJSON CreatePipeline{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipeline" Core..= pipeline),
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreatePipeline where
        type Rs CreatePipeline = CreatePipelineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePipelineResponse' Core.<$>
                   (x Core..:? "pipeline") Core.<*> x Core..:? "tags" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @CreatePipeline@ action.
--
-- /See:/ 'mkCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { pipeline :: Core.Maybe Types.PipelineDeclaration
    -- ^ Represents the structure of actions and stages to be performed in the pipeline. 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Specifies the tags applied to the pipeline.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePipelineResponse' value with any optional fields omitted.
mkCreatePipelineResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePipelineResponse
mkCreatePipelineResponse responseStatus
  = CreatePipelineResponse'{pipeline = Core.Nothing,
                            tags = Core.Nothing, responseStatus}

-- | Represents the structure of actions and stages to be performed in the pipeline. 
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPipeline :: Lens.Lens' CreatePipelineResponse (Core.Maybe Types.PipelineDeclaration)
cprrsPipeline = Lens.field @"pipeline"
{-# INLINEABLE cprrsPipeline #-}
{-# DEPRECATED pipeline "Use generic-lens or generic-optics with 'pipeline' instead"  #-}

-- | Specifies the tags applied to the pipeline.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsTags :: Lens.Lens' CreatePipelineResponse (Core.Maybe [Types.Tag])
cprrsTags = Lens.field @"tags"
{-# INLINEABLE cprrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePipelineResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

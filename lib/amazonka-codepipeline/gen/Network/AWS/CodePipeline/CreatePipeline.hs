{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreatePipeline (..),
    mkCreatePipeline,

    -- ** Request lenses
    cpPipeline,
    cpTags,

    -- * Destructuring the response
    CreatePipelineResponse (..),
    mkCreatePipelineResponse,

    -- ** Response lenses
    cprrsPipeline,
    cprrsTags,
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreatePipeline@ action.
--
-- /See:/ 'mkCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { -- | Represents the structure of actions and stages to be performed in the pipeline.
    pipeline :: Types.PipelineDeclaration,
    -- | The tags for the pipeline.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePipeline' value with any optional fields omitted.
mkCreatePipeline ::
  -- | 'pipeline'
  Types.PipelineDeclaration ->
  CreatePipeline
mkCreatePipeline pipeline =
  CreatePipeline' {pipeline, tags = Core.Nothing}

-- | Represents the structure of actions and stages to be performed in the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPipeline :: Lens.Lens' CreatePipeline Types.PipelineDeclaration
cpPipeline = Lens.field @"pipeline"
{-# DEPRECATED cpPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The tags for the pipeline.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePipeline (Core.Maybe [Types.Tag])
cpTags = Lens.field @"tags"
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreatePipeline where
  toJSON CreatePipeline {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipeline" Core..= pipeline),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreatePipeline where
  type Rs CreatePipeline = CreatePipelineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodePipeline_20150709.CreatePipeline")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePipelineResponse'
            Core.<$> (x Core..:? "pipeline")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @CreatePipeline@ action.
--
-- /See:/ 'mkCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { -- | Represents the structure of actions and stages to be performed in the pipeline.
    pipeline :: Core.Maybe Types.PipelineDeclaration,
    -- | Specifies the tags applied to the pipeline.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePipelineResponse' value with any optional fields omitted.
mkCreatePipelineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePipelineResponse
mkCreatePipelineResponse responseStatus =
  CreatePipelineResponse'
    { pipeline = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | Represents the structure of actions and stages to be performed in the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPipeline :: Lens.Lens' CreatePipelineResponse (Core.Maybe Types.PipelineDeclaration)
cprrsPipeline = Lens.field @"pipeline"
{-# DEPRECATED cprrsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | Specifies the tags applied to the pipeline.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsTags :: Lens.Lens' CreatePipelineResponse (Core.Maybe [Types.Tag])
cprrsTags = Lens.field @"tags"
{-# DEPRECATED cprrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePipelineResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

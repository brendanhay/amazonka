{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetPipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata, structure, stages, and actions of a pipeline. Can be used to return the entire structure of a pipeline in JSON format, which can then be modified and used to update the pipeline structure with 'UpdatePipeline' .
module Network.AWS.CodePipeline.GetPipeline
  ( -- * Creating a request
    GetPipeline (..),
    mkGetPipeline,

    -- ** Request lenses
    gpName,
    gpVersion,

    -- * Destructuring the response
    GetPipelineResponse (..),
    mkGetPipelineResponse,

    -- ** Response lenses
    gprrsMetadata,
    gprrsPipeline,
    gprrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetPipeline@ action.
--
-- /See:/ 'mkGetPipeline' smart constructor.
data GetPipeline = GetPipeline'
  { -- | The name of the pipeline for which you want to get information. Pipeline names must be unique under an AWS user account.
    name :: Types.PipelineName,
    -- | The version number of the pipeline. If you do not specify a version, defaults to the current version.
    version :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPipeline' value with any optional fields omitted.
mkGetPipeline ::
  -- | 'name'
  Types.PipelineName ->
  GetPipeline
mkGetPipeline name = GetPipeline' {name, version = Core.Nothing}

-- | The name of the pipeline for which you want to get information. Pipeline names must be unique under an AWS user account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpName :: Lens.Lens' GetPipeline Types.PipelineName
gpName = Lens.field @"name"
{-# DEPRECATED gpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version number of the pipeline. If you do not specify a version, defaults to the current version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpVersion :: Lens.Lens' GetPipeline (Core.Maybe Core.Natural)
gpVersion = Lens.field @"version"
{-# DEPRECATED gpVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON GetPipeline where
  toJSON GetPipeline {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            ("version" Core..=) Core.<$> version
          ]
      )

instance Core.AWSRequest GetPipeline where
  type Rs GetPipeline = GetPipelineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodePipeline_20150709.GetPipeline")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineResponse'
            Core.<$> (x Core..:? "metadata")
            Core.<*> (x Core..:? "pipeline")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetPipeline@ action.
--
-- /See:/ 'mkGetPipelineResponse' smart constructor.
data GetPipelineResponse = GetPipelineResponse'
  { -- | Represents the pipeline metadata information returned as part of the output of a @GetPipeline@ action.
    metadata :: Core.Maybe Types.PipelineMetadata,
    -- | Represents the structure of actions and stages to be performed in the pipeline.
    pipeline :: Core.Maybe Types.PipelineDeclaration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetPipelineResponse' value with any optional fields omitted.
mkGetPipelineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPipelineResponse
mkGetPipelineResponse responseStatus =
  GetPipelineResponse'
    { metadata = Core.Nothing,
      pipeline = Core.Nothing,
      responseStatus
    }

-- | Represents the pipeline metadata information returned as part of the output of a @GetPipeline@ action.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsMetadata :: Lens.Lens' GetPipelineResponse (Core.Maybe Types.PipelineMetadata)
gprrsMetadata = Lens.field @"metadata"
{-# DEPRECATED gprrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Represents the structure of actions and stages to be performed in the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPipeline :: Lens.Lens' GetPipelineResponse (Core.Maybe Types.PipelineDeclaration)
gprrsPipeline = Lens.field @"pipeline"
{-# DEPRECATED gprrsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetPipelineResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

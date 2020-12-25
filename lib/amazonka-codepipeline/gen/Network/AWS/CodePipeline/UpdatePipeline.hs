{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.UpdatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified pipeline with edits or changes to its structure. Use a JSON file with the pipeline structure and @UpdatePipeline@ to provide the full structure of the pipeline. Updating the pipeline increases the version number of the pipeline by 1.
module Network.AWS.CodePipeline.UpdatePipeline
  ( -- * Creating a request
    UpdatePipeline (..),
    mkUpdatePipeline,

    -- ** Request lenses
    upPipeline,

    -- * Destructuring the response
    UpdatePipelineResponse (..),
    mkUpdatePipelineResponse,

    -- ** Response lenses
    uprrsPipeline,
    uprrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdatePipeline@ action.
--
-- /See:/ 'mkUpdatePipeline' smart constructor.
newtype UpdatePipeline = UpdatePipeline'
  { -- | The name of the pipeline to be updated.
    pipeline :: Types.PipelineDeclaration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePipeline' value with any optional fields omitted.
mkUpdatePipeline ::
  -- | 'pipeline'
  Types.PipelineDeclaration ->
  UpdatePipeline
mkUpdatePipeline pipeline = UpdatePipeline' {pipeline}

-- | The name of the pipeline to be updated.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPipeline :: Lens.Lens' UpdatePipeline Types.PipelineDeclaration
upPipeline = Lens.field @"pipeline"
{-# DEPRECATED upPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

instance Core.FromJSON UpdatePipeline where
  toJSON UpdatePipeline {..} =
    Core.object
      (Core.catMaybes [Core.Just ("pipeline" Core..= pipeline)])

instance Core.AWSRequest UpdatePipeline where
  type Rs UpdatePipeline = UpdatePipelineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodePipeline_20150709.UpdatePipeline")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipelineResponse'
            Core.<$> (x Core..:? "pipeline") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of an @UpdatePipeline@ action.
--
-- /See:/ 'mkUpdatePipelineResponse' smart constructor.
data UpdatePipelineResponse = UpdatePipelineResponse'
  { -- | The structure of the updated pipeline.
    pipeline :: Core.Maybe Types.PipelineDeclaration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePipelineResponse' value with any optional fields omitted.
mkUpdatePipelineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdatePipelineResponse
mkUpdatePipelineResponse responseStatus =
  UpdatePipelineResponse' {pipeline = Core.Nothing, responseStatus}

-- | The structure of the updated pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsPipeline :: Lens.Lens' UpdatePipelineResponse (Core.Maybe Types.PipelineDeclaration)
uprrsPipeline = Lens.field @"pipeline"
{-# DEPRECATED uprrsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdatePipelineResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

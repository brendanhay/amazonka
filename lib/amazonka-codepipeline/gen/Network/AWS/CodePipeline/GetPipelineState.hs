{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetPipelineState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the state of a pipeline, including the stages and actions.
module Network.AWS.CodePipeline.GetPipelineState
  ( -- * Creating a request
    GetPipelineState (..),
    mkGetPipelineState,

    -- ** Request lenses
    gpsName,

    -- * Destructuring the response
    GetPipelineStateResponse (..),
    mkGetPipelineStateResponse,

    -- ** Response lenses
    gpsrrsCreated,
    gpsrrsPipelineName,
    gpsrrsPipelineVersion,
    gpsrrsStageStates,
    gpsrrsUpdated,
    gpsrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetPipelineState@ action.
--
-- /See:/ 'mkGetPipelineState' smart constructor.
newtype GetPipelineState = GetPipelineState'
  { -- | The name of the pipeline about which you want to get information.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPipelineState' value with any optional fields omitted.
mkGetPipelineState ::
  -- | 'name'
  Types.Name ->
  GetPipelineState
mkGetPipelineState name = GetPipelineState' {name}

-- | The name of the pipeline about which you want to get information.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsName :: Lens.Lens' GetPipelineState Types.Name
gpsName = Lens.field @"name"
{-# DEPRECATED gpsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON GetPipelineState where
  toJSON GetPipelineState {..} =
    Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.AWSRequest GetPipelineState where
  type Rs GetPipelineState = GetPipelineStateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.GetPipelineState")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineStateResponse'
            Core.<$> (x Core..:? "created")
            Core.<*> (x Core..:? "pipelineName")
            Core.<*> (x Core..:? "pipelineVersion")
            Core.<*> (x Core..:? "stageStates")
            Core.<*> (x Core..:? "updated")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetPipelineState@ action.
--
-- /See:/ 'mkGetPipelineStateResponse' smart constructor.
data GetPipelineStateResponse = GetPipelineStateResponse'
  { -- | The date and time the pipeline was created, in timestamp format.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the pipeline for which you want to get the state.
    pipelineName :: Core.Maybe Types.PipelineName,
    -- | The version number of the pipeline.
    pipelineVersion :: Core.Maybe Core.Natural,
    -- | A list of the pipeline stage output information, including stage name, state, most recent run details, whether the stage is disabled, and other data.
    stageStates :: Core.Maybe [Types.StageState],
    -- | The date and time the pipeline was last updated, in timestamp format.
    updated :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetPipelineStateResponse' value with any optional fields omitted.
mkGetPipelineStateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPipelineStateResponse
mkGetPipelineStateResponse responseStatus =
  GetPipelineStateResponse'
    { created = Core.Nothing,
      pipelineName = Core.Nothing,
      pipelineVersion = Core.Nothing,
      stageStates = Core.Nothing,
      updated = Core.Nothing,
      responseStatus
    }

-- | The date and time the pipeline was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsCreated :: Lens.Lens' GetPipelineStateResponse (Core.Maybe Core.NominalDiffTime)
gpsrrsCreated = Lens.field @"created"
{-# DEPRECATED gpsrrsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The name of the pipeline for which you want to get the state.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsPipelineName :: Lens.Lens' GetPipelineStateResponse (Core.Maybe Types.PipelineName)
gpsrrsPipelineName = Lens.field @"pipelineName"
{-# DEPRECATED gpsrrsPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The version number of the pipeline.
--
-- /Note:/ Consider using 'pipelineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsPipelineVersion :: Lens.Lens' GetPipelineStateResponse (Core.Maybe Core.Natural)
gpsrrsPipelineVersion = Lens.field @"pipelineVersion"
{-# DEPRECATED gpsrrsPipelineVersion "Use generic-lens or generic-optics with 'pipelineVersion' instead." #-}

-- | A list of the pipeline stage output information, including stage name, state, most recent run details, whether the stage is disabled, and other data.
--
-- /Note:/ Consider using 'stageStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsStageStates :: Lens.Lens' GetPipelineStateResponse (Core.Maybe [Types.StageState])
gpsrrsStageStates = Lens.field @"stageStates"
{-# DEPRECATED gpsrrsStageStates "Use generic-lens or generic-optics with 'stageStates' instead." #-}

-- | The date and time the pipeline was last updated, in timestamp format.
--
-- /Note:/ Consider using 'updated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsUpdated :: Lens.Lens' GetPipelineStateResponse (Core.Maybe Core.NominalDiffTime)
gpsrrsUpdated = Lens.field @"updated"
{-# DEPRECATED gpsrrsUpdated "Use generic-lens or generic-optics with 'updated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpsrrsResponseStatus :: Lens.Lens' GetPipelineStateResponse Core.Int
gpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

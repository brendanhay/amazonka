{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetPipelineExecution (..),
    mkGetPipelineExecution,

    -- ** Request lenses
    gpePipelineName,
    gpePipelineExecutionId,

    -- * Destructuring the response
    GetPipelineExecutionResponse (..),
    mkGetPipelineExecutionResponse,

    -- ** Response lenses
    gperrsPipelineExecution,
    gperrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetPipelineExecution@ action.
--
-- /See:/ 'mkGetPipelineExecution' smart constructor.
data GetPipelineExecution = GetPipelineExecution'
  { -- | The name of the pipeline about which you want to get execution details.
    pipelineName :: Types.PipelineName,
    -- | The ID of the pipeline execution about which you want to get execution details.
    pipelineExecutionId :: Types.PipelineExecutionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPipelineExecution' value with any optional fields omitted.
mkGetPipelineExecution ::
  -- | 'pipelineName'
  Types.PipelineName ->
  -- | 'pipelineExecutionId'
  Types.PipelineExecutionId ->
  GetPipelineExecution
mkGetPipelineExecution pipelineName pipelineExecutionId =
  GetPipelineExecution' {pipelineName, pipelineExecutionId}

-- | The name of the pipeline about which you want to get execution details.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpePipelineName :: Lens.Lens' GetPipelineExecution Types.PipelineName
gpePipelineName = Lens.field @"pipelineName"
{-# DEPRECATED gpePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The ID of the pipeline execution about which you want to get execution details.
--
-- /Note:/ Consider using 'pipelineExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpePipelineExecutionId :: Lens.Lens' GetPipelineExecution Types.PipelineExecutionId
gpePipelineExecutionId = Lens.field @"pipelineExecutionId"
{-# DEPRECATED gpePipelineExecutionId "Use generic-lens or generic-optics with 'pipelineExecutionId' instead." #-}

instance Core.FromJSON GetPipelineExecution where
  toJSON GetPipelineExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            Core.Just ("pipelineExecutionId" Core..= pipelineExecutionId)
          ]
      )

instance Core.AWSRequest GetPipelineExecution where
  type Rs GetPipelineExecution = GetPipelineExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.GetPipelineExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineExecutionResponse'
            Core.<$> (x Core..:? "pipelineExecution")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetPipelineExecution@ action.
--
-- /See:/ 'mkGetPipelineExecutionResponse' smart constructor.
data GetPipelineExecutionResponse = GetPipelineExecutionResponse'
  { -- | Represents information about the execution of a pipeline.
    pipelineExecution :: Core.Maybe Types.PipelineExecution,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetPipelineExecutionResponse' value with any optional fields omitted.
mkGetPipelineExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPipelineExecutionResponse
mkGetPipelineExecutionResponse responseStatus =
  GetPipelineExecutionResponse'
    { pipelineExecution = Core.Nothing,
      responseStatus
    }

-- | Represents information about the execution of a pipeline.
--
-- /Note:/ Consider using 'pipelineExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gperrsPipelineExecution :: Lens.Lens' GetPipelineExecutionResponse (Core.Maybe Types.PipelineExecution)
gperrsPipelineExecution = Lens.field @"pipelineExecution"
{-# DEPRECATED gperrsPipelineExecution "Use generic-lens or generic-optics with 'pipelineExecution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gperrsResponseStatus :: Lens.Lens' GetPipelineExecutionResponse Core.Int
gperrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gperrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

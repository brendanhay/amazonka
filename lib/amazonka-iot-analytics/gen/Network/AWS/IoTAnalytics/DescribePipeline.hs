{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DescribePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a pipeline.
module Network.AWS.IoTAnalytics.DescribePipeline
  ( -- * Creating a request
    DescribePipeline (..),
    mkDescribePipeline,

    -- ** Request lenses
    dpPipelineName,

    -- * Destructuring the response
    DescribePipelineResponse (..),
    mkDescribePipelineResponse,

    -- ** Response lenses
    dprrsPipeline,
    dprrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePipeline' smart constructor.
newtype DescribePipeline = DescribePipeline'
  { -- | The name of the pipeline whose information is retrieved.
    pipelineName :: Types.PipelineName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePipeline' value with any optional fields omitted.
mkDescribePipeline ::
  -- | 'pipelineName'
  Types.PipelineName ->
  DescribePipeline
mkDescribePipeline pipelineName = DescribePipeline' {pipelineName}

-- | The name of the pipeline whose information is retrieved.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPipelineName :: Lens.Lens' DescribePipeline Types.PipelineName
dpPipelineName = Lens.field @"pipelineName"
{-# DEPRECATED dpPipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

instance Core.AWSRequest DescribePipeline where
  type Rs DescribePipeline = DescribePipelineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/pipelines/" Core.<> (Core.toText pipelineName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelineResponse'
            Core.<$> (x Core..:? "pipeline") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribePipelineResponse' smart constructor.
data DescribePipelineResponse = DescribePipelineResponse'
  { -- | A @Pipeline@ object that contains information about the pipeline.
    pipeline :: Core.Maybe Types.Pipeline,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribePipelineResponse' value with any optional fields omitted.
mkDescribePipelineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePipelineResponse
mkDescribePipelineResponse responseStatus =
  DescribePipelineResponse'
    { pipeline = Core.Nothing,
      responseStatus
    }

-- | A @Pipeline@ object that contains information about the pipeline.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsPipeline :: Lens.Lens' DescribePipelineResponse (Core.Maybe Types.Pipeline)
dprrsPipeline = Lens.field @"pipeline"
{-# DEPRECATED dprrsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DescribePipelineResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

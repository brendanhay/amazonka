{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.DescribePipelines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about one or more pipelines. The information retrieved includes the name of the pipeline, the pipeline identifier, its current state, and the user account that owns the pipeline. Using account credentials, you can retrieve metadata about pipelines that you or your IAM users have created. If you are using an IAM user account, you can retrieve metadata about only those pipelines for which you have read permissions.
--
-- To retrieve the full pipeline definition instead of metadata about the pipeline, call 'GetPipelineDefinition' .
module Network.AWS.DataPipeline.DescribePipelines
  ( -- * Creating a request
    DescribePipelines (..),
    mkDescribePipelines,

    -- ** Request lenses
    dpPipelineIds,

    -- * Destructuring the response
    DescribePipelinesResponse (..),
    mkDescribePipelinesResponse,

    -- ** Response lenses
    dprrsPipelineDescriptionList,
    dprrsResponseStatus,
  )
where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribePipelines.
--
-- /See:/ 'mkDescribePipelines' smart constructor.
newtype DescribePipelines = DescribePipelines'
  { -- | The IDs of the pipelines to describe. You can pass as many as 25 identifiers in a single call. To obtain pipeline IDs, call 'ListPipelines' .
    pipelineIds :: [Types.Id]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePipelines' value with any optional fields omitted.
mkDescribePipelines ::
  DescribePipelines
mkDescribePipelines = DescribePipelines' {pipelineIds = Core.mempty}

-- | The IDs of the pipelines to describe. You can pass as many as 25 identifiers in a single call. To obtain pipeline IDs, call 'ListPipelines' .
--
-- /Note:/ Consider using 'pipelineIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPipelineIds :: Lens.Lens' DescribePipelines [Types.Id]
dpPipelineIds = Lens.field @"pipelineIds"
{-# DEPRECATED dpPipelineIds "Use generic-lens or generic-optics with 'pipelineIds' instead." #-}

instance Core.FromJSON DescribePipelines where
  toJSON DescribePipelines {..} =
    Core.object
      (Core.catMaybes [Core.Just ("pipelineIds" Core..= pipelineIds)])

instance Core.AWSRequest DescribePipelines where
  type Rs DescribePipelines = DescribePipelinesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DataPipeline.DescribePipelines")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipelinesResponse'
            Core.<$> (x Core..:? "pipelineDescriptionList" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of DescribePipelines.
--
-- /See:/ 'mkDescribePipelinesResponse' smart constructor.
data DescribePipelinesResponse = DescribePipelinesResponse'
  { -- | An array of descriptions for the specified pipelines.
    pipelineDescriptionList :: [Types.PipelineDescription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePipelinesResponse' value with any optional fields omitted.
mkDescribePipelinesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePipelinesResponse
mkDescribePipelinesResponse responseStatus =
  DescribePipelinesResponse'
    { pipelineDescriptionList = Core.mempty,
      responseStatus
    }

-- | An array of descriptions for the specified pipelines.
--
-- /Note:/ Consider using 'pipelineDescriptionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsPipelineDescriptionList :: Lens.Lens' DescribePipelinesResponse [Types.PipelineDescription]
dprrsPipelineDescriptionList = Lens.field @"pipelineDescriptionList"
{-# DEPRECATED dprrsPipelineDescriptionList "Use generic-lens or generic-optics with 'pipelineDescriptionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DescribePipelinesResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

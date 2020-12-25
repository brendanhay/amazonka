{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ListPipelines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the pipeline identifiers for all active pipelines that you have permission to access.
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.ListPipelines
  ( -- * Creating a request
    ListPipelines (..),
    mkListPipelines,

    -- ** Request lenses
    lpMarker,

    -- * Destructuring the response
    ListPipelinesResponse (..),
    mkListPipelinesResponse,

    -- ** Response lenses
    lprrsPipelineIdList,
    lprrsHasMoreResults,
    lprrsMarker,
    lprrsResponseStatus,
  )
where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ListPipelines.
--
-- /See:/ 'mkListPipelines' smart constructor.
newtype ListPipelines = ListPipelines'
  { -- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @ListPipelines@ with the marker value from the previous call to retrieve the next set of results.
    marker :: Core.Maybe Types.Marker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListPipelines' value with any optional fields omitted.
mkListPipelines ::
  ListPipelines
mkListPipelines = ListPipelines' {marker = Core.Nothing}

-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @ListPipelines@ with the marker value from the previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMarker :: Lens.Lens' ListPipelines (Core.Maybe Types.Marker)
lpMarker = Lens.field @"marker"
{-# DEPRECATED lpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.FromJSON ListPipelines where
  toJSON ListPipelines {..} =
    Core.object (Core.catMaybes [("marker" Core..=) Core.<$> marker])

instance Core.AWSRequest ListPipelines where
  type Rs ListPipelines = ListPipelinesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DataPipeline.ListPipelines")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Core.<$> (x Core..:? "pipelineIdList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "hasMoreResults")
            Core.<*> (x Core..:? "marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPipelines where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"hasMoreResults") =
      Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the output of ListPipelines.
--
-- /See:/ 'mkListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | The pipeline identifiers. If you require additional information about the pipelines, you can use these identifiers to call 'DescribePipelines' and 'GetPipelineDefinition' .
    pipelineIdList :: [Types.PipelineIdName],
    -- | Indicates whether there are more results that can be obtained by a subsequent call.
    hasMoreResults :: Core.Maybe Core.Bool,
    -- | The starting point for the next page of results. To view the next page of results, call @ListPipelinesOutput@ again with this marker value. If the value is null, there are no more results.
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPipelinesResponse' value with any optional fields omitted.
mkListPipelinesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPipelinesResponse
mkListPipelinesResponse responseStatus =
  ListPipelinesResponse'
    { pipelineIdList = Core.mempty,
      hasMoreResults = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | The pipeline identifiers. If you require additional information about the pipelines, you can use these identifiers to call 'DescribePipelines' and 'GetPipelineDefinition' .
--
-- /Note:/ Consider using 'pipelineIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPipelineIdList :: Lens.Lens' ListPipelinesResponse [Types.PipelineIdName]
lprrsPipelineIdList = Lens.field @"pipelineIdList"
{-# DEPRECATED lprrsPipelineIdList "Use generic-lens or generic-optics with 'pipelineIdList' instead." #-}

-- | Indicates whether there are more results that can be obtained by a subsequent call.
--
-- /Note:/ Consider using 'hasMoreResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsHasMoreResults :: Lens.Lens' ListPipelinesResponse (Core.Maybe Core.Bool)
lprrsHasMoreResults = Lens.field @"hasMoreResults"
{-# DEPRECATED lprrsHasMoreResults "Use generic-lens or generic-optics with 'hasMoreResults' instead." #-}

-- | The starting point for the next page of results. To view the next page of results, call @ListPipelinesOutput@ again with this marker value. If the value is null, there are no more results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsMarker :: Lens.Lens' ListPipelinesResponse (Core.Maybe Types.String)
lprrsMarker = Lens.field @"marker"
{-# DEPRECATED lprrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPipelinesResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

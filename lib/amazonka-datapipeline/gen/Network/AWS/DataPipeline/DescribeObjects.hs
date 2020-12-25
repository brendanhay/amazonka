{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.DescribeObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the object definitions for a set of objects associated with the pipeline. Object definitions are composed of a set of fields that define the properties of the object.
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.DescribeObjects
  ( -- * Creating a request
    DescribeObjects (..),
    mkDescribeObjects,

    -- ** Request lenses
    doPipelineId,
    doObjectIds,
    doEvaluateExpressions,
    doMarker,

    -- * Destructuring the response
    DescribeObjectsResponse (..),
    mkDescribeObjectsResponse,

    -- ** Response lenses
    dorrsPipelineObjects,
    dorrsHasMoreResults,
    dorrsMarker,
    dorrsResponseStatus,
  )
where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeObjects.
--
-- /See:/ 'mkDescribeObjects' smart constructor.
data DescribeObjects = DescribeObjects'
  { -- | The ID of the pipeline that contains the object definitions.
    pipelineId :: Types.Id,
    -- | The IDs of the pipeline objects that contain the definitions to be described. You can pass as many as 25 identifiers in a single call to @DescribeObjects@ .
    objectIds :: [Types.Id],
    -- | Indicates whether any expressions in the object should be evaluated when the object descriptions are returned.
    evaluateExpressions :: Core.Maybe Core.Bool,
    -- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @DescribeObjects@ with the marker value from the previous call to retrieve the next set of results.
    marker :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeObjects' value with any optional fields omitted.
mkDescribeObjects ::
  -- | 'pipelineId'
  Types.Id ->
  DescribeObjects
mkDescribeObjects pipelineId =
  DescribeObjects'
    { pipelineId,
      objectIds = Core.mempty,
      evaluateExpressions = Core.Nothing,
      marker = Core.Nothing
    }

-- | The ID of the pipeline that contains the object definitions.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doPipelineId :: Lens.Lens' DescribeObjects Types.Id
doPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED doPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The IDs of the pipeline objects that contain the definitions to be described. You can pass as many as 25 identifiers in a single call to @DescribeObjects@ .
--
-- /Note:/ Consider using 'objectIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doObjectIds :: Lens.Lens' DescribeObjects [Types.Id]
doObjectIds = Lens.field @"objectIds"
{-# DEPRECATED doObjectIds "Use generic-lens or generic-optics with 'objectIds' instead." #-}

-- | Indicates whether any expressions in the object should be evaluated when the object descriptions are returned.
--
-- /Note:/ Consider using 'evaluateExpressions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doEvaluateExpressions :: Lens.Lens' DescribeObjects (Core.Maybe Core.Bool)
doEvaluateExpressions = Lens.field @"evaluateExpressions"
{-# DEPRECATED doEvaluateExpressions "Use generic-lens or generic-optics with 'evaluateExpressions' instead." #-}

-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @DescribeObjects@ with the marker value from the previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doMarker :: Lens.Lens' DescribeObjects (Core.Maybe Types.String)
doMarker = Lens.field @"marker"
{-# DEPRECATED doMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.FromJSON DescribeObjects where
  toJSON DescribeObjects {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineId" Core..= pipelineId),
            Core.Just ("objectIds" Core..= objectIds),
            ("evaluateExpressions" Core..=) Core.<$> evaluateExpressions,
            ("marker" Core..=) Core.<$> marker
          ]
      )

instance Core.AWSRequest DescribeObjects where
  type Rs DescribeObjects = DescribeObjectsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DataPipeline.DescribeObjects")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeObjectsResponse'
            Core.<$> (x Core..:? "pipelineObjects" Core..!= Core.mempty)
            Core.<*> (x Core..:? "hasMoreResults")
            Core.<*> (x Core..:? "marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeObjects where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"hasMoreResults") =
      Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the output of DescribeObjects.
--
-- /See:/ 'mkDescribeObjectsResponse' smart constructor.
data DescribeObjectsResponse = DescribeObjectsResponse'
  { -- | An array of object definitions.
    pipelineObjects :: [Types.PipelineObject],
    -- | Indicates whether there are more results to return.
    hasMoreResults :: Core.Maybe Core.Bool,
    -- | The starting point for the next page of results. To view the next page of results, call @DescribeObjects@ again with this marker value. If the value is null, there are no more results.
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeObjectsResponse' value with any optional fields omitted.
mkDescribeObjectsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeObjectsResponse
mkDescribeObjectsResponse responseStatus =
  DescribeObjectsResponse'
    { pipelineObjects = Core.mempty,
      hasMoreResults = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | An array of object definitions.
--
-- /Note:/ Consider using 'pipelineObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsPipelineObjects :: Lens.Lens' DescribeObjectsResponse [Types.PipelineObject]
dorrsPipelineObjects = Lens.field @"pipelineObjects"
{-# DEPRECATED dorrsPipelineObjects "Use generic-lens or generic-optics with 'pipelineObjects' instead." #-}

-- | Indicates whether there are more results to return.
--
-- /Note:/ Consider using 'hasMoreResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsHasMoreResults :: Lens.Lens' DescribeObjectsResponse (Core.Maybe Core.Bool)
dorrsHasMoreResults = Lens.field @"hasMoreResults"
{-# DEPRECATED dorrsHasMoreResults "Use generic-lens or generic-optics with 'hasMoreResults' instead." #-}

-- | The starting point for the next page of results. To view the next page of results, call @DescribeObjects@ again with this marker value. If the value is null, there are no more results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsMarker :: Lens.Lens' DescribeObjectsResponse (Core.Maybe Types.Marker)
dorrsMarker = Lens.field @"marker"
{-# DEPRECATED dorrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsResponseStatus :: Lens.Lens' DescribeObjectsResponse Core.Int
dorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

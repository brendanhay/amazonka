{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeObjects (..)
    , mkDescribeObjects
    -- ** Request lenses
    , doPipelineId
    , doObjectIds
    , doEvaluateExpressions
    , doMarker

    -- * Destructuring the response
    , DescribeObjectsResponse (..)
    , mkDescribeObjectsResponse
    -- ** Response lenses
    , dorrsPipelineObjects
    , dorrsHasMoreResults
    , dorrsMarker
    , dorrsResponseStatus
    ) where

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
  { pipelineId :: Types.Id
    -- ^ The ID of the pipeline that contains the object definitions.
  , objectIds :: [Types.Id]
    -- ^ The IDs of the pipeline objects that contain the definitions to be described. You can pass as many as 25 identifiers in a single call to @DescribeObjects@ .
  , evaluateExpressions :: Core.Maybe Core.Bool
    -- ^ Indicates whether any expressions in the object should be evaluated when the object descriptions are returned.
  , marker :: Core.Maybe Core.Text
    -- ^ The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @DescribeObjects@ with the marker value from the previous call to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeObjects' value with any optional fields omitted.
mkDescribeObjects
    :: Types.Id -- ^ 'pipelineId'
    -> DescribeObjects
mkDescribeObjects pipelineId
  = DescribeObjects'{pipelineId, objectIds = Core.mempty,
                     evaluateExpressions = Core.Nothing, marker = Core.Nothing}

-- | The ID of the pipeline that contains the object definitions.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doPipelineId :: Lens.Lens' DescribeObjects Types.Id
doPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE doPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | The IDs of the pipeline objects that contain the definitions to be described. You can pass as many as 25 identifiers in a single call to @DescribeObjects@ .
--
-- /Note:/ Consider using 'objectIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doObjectIds :: Lens.Lens' DescribeObjects [Types.Id]
doObjectIds = Lens.field @"objectIds"
{-# INLINEABLE doObjectIds #-}
{-# DEPRECATED objectIds "Use generic-lens or generic-optics with 'objectIds' instead"  #-}

-- | Indicates whether any expressions in the object should be evaluated when the object descriptions are returned.
--
-- /Note:/ Consider using 'evaluateExpressions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doEvaluateExpressions :: Lens.Lens' DescribeObjects (Core.Maybe Core.Bool)
doEvaluateExpressions = Lens.field @"evaluateExpressions"
{-# INLINEABLE doEvaluateExpressions #-}
{-# DEPRECATED evaluateExpressions "Use generic-lens or generic-optics with 'evaluateExpressions' instead"  #-}

-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @DescribeObjects@ with the marker value from the previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doMarker :: Lens.Lens' DescribeObjects (Core.Maybe Core.Text)
doMarker = Lens.field @"marker"
{-# INLINEABLE doMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery DescribeObjects where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeObjects where
        toHeaders DescribeObjects{..}
          = Core.pure ("X-Amz-Target", "DataPipeline.DescribeObjects")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeObjects where
        toJSON DescribeObjects{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineId" Core..= pipelineId),
                  Core.Just ("objectIds" Core..= objectIds),
                  ("evaluateExpressions" Core..=) Core.<$> evaluateExpressions,
                  ("marker" Core..=) Core.<$> marker])

instance Core.AWSRequest DescribeObjects where
        type Rs DescribeObjects = DescribeObjectsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeObjectsResponse' Core.<$>
                   (x Core..:? "pipelineObjects" Core..!= Core.mempty) Core.<*>
                     x Core..:? "hasMoreResults"
                     Core.<*> x Core..:? "marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeObjects where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"hasMoreResults") =
            Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the output of DescribeObjects.
--
-- /See:/ 'mkDescribeObjectsResponse' smart constructor.
data DescribeObjectsResponse = DescribeObjectsResponse'
  { pipelineObjects :: [Types.PipelineObject]
    -- ^ An array of object definitions.
  , hasMoreResults :: Core.Maybe Core.Bool
    -- ^ Indicates whether there are more results to return.
  , marker :: Core.Maybe Core.Text
    -- ^ The starting point for the next page of results. To view the next page of results, call @DescribeObjects@ again with this marker value. If the value is null, there are no more results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeObjectsResponse' value with any optional fields omitted.
mkDescribeObjectsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeObjectsResponse
mkDescribeObjectsResponse responseStatus
  = DescribeObjectsResponse'{pipelineObjects = Core.mempty,
                             hasMoreResults = Core.Nothing, marker = Core.Nothing,
                             responseStatus}

-- | An array of object definitions.
--
-- /Note:/ Consider using 'pipelineObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsPipelineObjects :: Lens.Lens' DescribeObjectsResponse [Types.PipelineObject]
dorrsPipelineObjects = Lens.field @"pipelineObjects"
{-# INLINEABLE dorrsPipelineObjects #-}
{-# DEPRECATED pipelineObjects "Use generic-lens or generic-optics with 'pipelineObjects' instead"  #-}

-- | Indicates whether there are more results to return.
--
-- /Note:/ Consider using 'hasMoreResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsHasMoreResults :: Lens.Lens' DescribeObjectsResponse (Core.Maybe Core.Bool)
dorrsHasMoreResults = Lens.field @"hasMoreResults"
{-# INLINEABLE dorrsHasMoreResults #-}
{-# DEPRECATED hasMoreResults "Use generic-lens or generic-optics with 'hasMoreResults' instead"  #-}

-- | The starting point for the next page of results. To view the next page of results, call @DescribeObjects@ again with this marker value. If the value is null, there are no more results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsMarker :: Lens.Lens' DescribeObjectsResponse (Core.Maybe Core.Text)
dorrsMarker = Lens.field @"marker"
{-# INLINEABLE dorrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsResponseStatus :: Lens.Lens' DescribeObjectsResponse Core.Int
dorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

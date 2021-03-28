{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.QueryObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries the specified pipeline for the names of objects that match the specified set of conditions.
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.QueryObjects
    (
    -- * Creating a request
      QueryObjects (..)
    , mkQueryObjects
    -- ** Request lenses
    , qoPipelineId
    , qoSphere
    , qoLimit
    , qoMarker
    , qoQuery

    -- * Destructuring the response
    , QueryObjectsResponse (..)
    , mkQueryObjectsResponse
    -- ** Response lenses
    , qorrsHasMoreResults
    , qorrsIds
    , qorrsMarker
    , qorrsResponseStatus
    ) where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for QueryObjects.
--
-- /See:/ 'mkQueryObjects' smart constructor.
data QueryObjects = QueryObjects'
  { pipelineId :: Types.PipelineId
    -- ^ The ID of the pipeline.
  , sphere :: Core.Text
    -- ^ Indicates whether the query applies to components or instances. The possible values are: @COMPONENT@ , @INSTANCE@ , and @ATTEMPT@ .
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of object names that @QueryObjects@ will return in a single call. The default value is 100. 
  , marker :: Core.Maybe Core.Text
    -- ^ The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @QueryObjects@ with the marker value from the previous call to retrieve the next set of results.
  , query :: Core.Maybe Types.Query
    -- ^ The query that defines the objects to be returned. The @Query@ object can contain a maximum of ten selectors. The conditions in the query are limited to top-level String fields in the object. These filters can be applied to components, instances, and attempts.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryObjects' value with any optional fields omitted.
mkQueryObjects
    :: Types.PipelineId -- ^ 'pipelineId'
    -> Core.Text -- ^ 'sphere'
    -> QueryObjects
mkQueryObjects pipelineId sphere
  = QueryObjects'{pipelineId, sphere, limit = Core.Nothing,
                  marker = Core.Nothing, query = Core.Nothing}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoPipelineId :: Lens.Lens' QueryObjects Types.PipelineId
qoPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE qoPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | Indicates whether the query applies to components or instances. The possible values are: @COMPONENT@ , @INSTANCE@ , and @ATTEMPT@ .
--
-- /Note:/ Consider using 'sphere' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoSphere :: Lens.Lens' QueryObjects Core.Text
qoSphere = Lens.field @"sphere"
{-# INLINEABLE qoSphere #-}
{-# DEPRECATED sphere "Use generic-lens or generic-optics with 'sphere' instead"  #-}

-- | The maximum number of object names that @QueryObjects@ will return in a single call. The default value is 100. 
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoLimit :: Lens.Lens' QueryObjects (Core.Maybe Core.Int)
qoLimit = Lens.field @"limit"
{-# INLINEABLE qoLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @QueryObjects@ with the marker value from the previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoMarker :: Lens.Lens' QueryObjects (Core.Maybe Core.Text)
qoMarker = Lens.field @"marker"
{-# INLINEABLE qoMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The query that defines the objects to be returned. The @Query@ object can contain a maximum of ten selectors. The conditions in the query are limited to top-level String fields in the object. These filters can be applied to components, instances, and attempts.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoQuery :: Lens.Lens' QueryObjects (Core.Maybe Types.Query)
qoQuery = Lens.field @"query"
{-# INLINEABLE qoQuery #-}
{-# DEPRECATED query "Use generic-lens or generic-optics with 'query' instead"  #-}

instance Core.ToQuery QueryObjects where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders QueryObjects where
        toHeaders QueryObjects{..}
          = Core.pure ("X-Amz-Target", "DataPipeline.QueryObjects") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON QueryObjects where
        toJSON QueryObjects{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineId" Core..= pipelineId),
                  Core.Just ("sphere" Core..= sphere),
                  ("limit" Core..=) Core.<$> limit,
                  ("marker" Core..=) Core.<$> marker,
                  ("query" Core..=) Core.<$> query])

instance Core.AWSRequest QueryObjects where
        type Rs QueryObjects = QueryObjectsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 QueryObjectsResponse' Core.<$>
                   (x Core..:? "hasMoreResults") Core.<*> x Core..:? "ids" Core.<*>
                     x Core..:? "marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager QueryObjects where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"hasMoreResults") =
            Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the output of QueryObjects.
--
-- /See:/ 'mkQueryObjectsResponse' smart constructor.
data QueryObjectsResponse = QueryObjectsResponse'
  { hasMoreResults :: Core.Maybe Core.Bool
    -- ^ Indicates whether there are more results that can be obtained by a subsequent call.
  , ids :: Core.Maybe [Types.Id]
    -- ^ The identifiers that match the query selectors.
  , marker :: Core.Maybe Core.Text
    -- ^ The starting point for the next page of results. To view the next page of results, call @QueryObjects@ again with this marker value. If the value is null, there are no more results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryObjectsResponse' value with any optional fields omitted.
mkQueryObjectsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> QueryObjectsResponse
mkQueryObjectsResponse responseStatus
  = QueryObjectsResponse'{hasMoreResults = Core.Nothing,
                          ids = Core.Nothing, marker = Core.Nothing, responseStatus}

-- | Indicates whether there are more results that can be obtained by a subsequent call.
--
-- /Note:/ Consider using 'hasMoreResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorrsHasMoreResults :: Lens.Lens' QueryObjectsResponse (Core.Maybe Core.Bool)
qorrsHasMoreResults = Lens.field @"hasMoreResults"
{-# INLINEABLE qorrsHasMoreResults #-}
{-# DEPRECATED hasMoreResults "Use generic-lens or generic-optics with 'hasMoreResults' instead"  #-}

-- | The identifiers that match the query selectors.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorrsIds :: Lens.Lens' QueryObjectsResponse (Core.Maybe [Types.Id])
qorrsIds = Lens.field @"ids"
{-# INLINEABLE qorrsIds #-}
{-# DEPRECATED ids "Use generic-lens or generic-optics with 'ids' instead"  #-}

-- | The starting point for the next page of results. To view the next page of results, call @QueryObjects@ again with this marker value. If the value is null, there are no more results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorrsMarker :: Lens.Lens' QueryObjectsResponse (Core.Maybe Core.Text)
qorrsMarker = Lens.field @"marker"
{-# INLINEABLE qorrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorrsResponseStatus :: Lens.Lens' QueryObjectsResponse Core.Int
qorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE qorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

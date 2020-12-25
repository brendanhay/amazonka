{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    QueryObjects (..),
    mkQueryObjects,

    -- ** Request lenses
    qoPipelineId,
    qoSphere,
    qoLimit,
    qoMarker,
    qoQuery,

    -- * Destructuring the response
    QueryObjectsResponse (..),
    mkQueryObjectsResponse,

    -- ** Response lenses
    qorrsHasMoreResults,
    qorrsIds,
    qorrsMarker,
    qorrsResponseStatus,
  )
where

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
  { -- | The ID of the pipeline.
    pipelineId :: Types.PipelineId,
    -- | Indicates whether the query applies to components or instances. The possible values are: @COMPONENT@ , @INSTANCE@ , and @ATTEMPT@ .
    sphere :: Types.String,
    -- | The maximum number of object names that @QueryObjects@ will return in a single call. The default value is 100.
    limit :: Core.Maybe Core.Int,
    -- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @QueryObjects@ with the marker value from the previous call to retrieve the next set of results.
    marker :: Core.Maybe Types.String,
    -- | The query that defines the objects to be returned. The @Query@ object can contain a maximum of ten selectors. The conditions in the query are limited to top-level String fields in the object. These filters can be applied to components, instances, and attempts.
    query :: Core.Maybe Types.Query
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryObjects' value with any optional fields omitted.
mkQueryObjects ::
  -- | 'pipelineId'
  Types.PipelineId ->
  -- | 'sphere'
  Types.String ->
  QueryObjects
mkQueryObjects pipelineId sphere =
  QueryObjects'
    { pipelineId,
      sphere,
      limit = Core.Nothing,
      marker = Core.Nothing,
      query = Core.Nothing
    }

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoPipelineId :: Lens.Lens' QueryObjects Types.PipelineId
qoPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED qoPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | Indicates whether the query applies to components or instances. The possible values are: @COMPONENT@ , @INSTANCE@ , and @ATTEMPT@ .
--
-- /Note:/ Consider using 'sphere' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoSphere :: Lens.Lens' QueryObjects Types.String
qoSphere = Lens.field @"sphere"
{-# DEPRECATED qoSphere "Use generic-lens or generic-optics with 'sphere' instead." #-}

-- | The maximum number of object names that @QueryObjects@ will return in a single call. The default value is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoLimit :: Lens.Lens' QueryObjects (Core.Maybe Core.Int)
qoLimit = Lens.field @"limit"
{-# DEPRECATED qoLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @QueryObjects@ with the marker value from the previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoMarker :: Lens.Lens' QueryObjects (Core.Maybe Types.String)
qoMarker = Lens.field @"marker"
{-# DEPRECATED qoMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The query that defines the objects to be returned. The @Query@ object can contain a maximum of ten selectors. The conditions in the query are limited to top-level String fields in the object. These filters can be applied to components, instances, and attempts.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoQuery :: Lens.Lens' QueryObjects (Core.Maybe Types.Query)
qoQuery = Lens.field @"query"
{-# DEPRECATED qoQuery "Use generic-lens or generic-optics with 'query' instead." #-}

instance Core.FromJSON QueryObjects where
  toJSON QueryObjects {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineId" Core..= pipelineId),
            Core.Just ("sphere" Core..= sphere),
            ("limit" Core..=) Core.<$> limit,
            ("marker" Core..=) Core.<$> marker,
            ("query" Core..=) Core.<$> query
          ]
      )

instance Core.AWSRequest QueryObjects where
  type Rs QueryObjects = QueryObjectsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DataPipeline.QueryObjects")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryObjectsResponse'
            Core.<$> (x Core..:? "hasMoreResults")
            Core.<*> (x Core..:? "ids")
            Core.<*> (x Core..:? "marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager QueryObjects where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"hasMoreResults") =
      Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the output of QueryObjects.
--
-- /See:/ 'mkQueryObjectsResponse' smart constructor.
data QueryObjectsResponse = QueryObjectsResponse'
  { -- | Indicates whether there are more results that can be obtained by a subsequent call.
    hasMoreResults :: Core.Maybe Core.Bool,
    -- | The identifiers that match the query selectors.
    ids :: Core.Maybe [Types.Id],
    -- | The starting point for the next page of results. To view the next page of results, call @QueryObjects@ again with this marker value. If the value is null, there are no more results.
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryObjectsResponse' value with any optional fields omitted.
mkQueryObjectsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  QueryObjectsResponse
mkQueryObjectsResponse responseStatus =
  QueryObjectsResponse'
    { hasMoreResults = Core.Nothing,
      ids = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | Indicates whether there are more results that can be obtained by a subsequent call.
--
-- /Note:/ Consider using 'hasMoreResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorrsHasMoreResults :: Lens.Lens' QueryObjectsResponse (Core.Maybe Core.Bool)
qorrsHasMoreResults = Lens.field @"hasMoreResults"
{-# DEPRECATED qorrsHasMoreResults "Use generic-lens or generic-optics with 'hasMoreResults' instead." #-}

-- | The identifiers that match the query selectors.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorrsIds :: Lens.Lens' QueryObjectsResponse (Core.Maybe [Types.Id])
qorrsIds = Lens.field @"ids"
{-# DEPRECATED qorrsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | The starting point for the next page of results. To view the next page of results, call @QueryObjects@ again with this marker value. If the value is null, there are no more results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorrsMarker :: Lens.Lens' QueryObjectsResponse (Core.Maybe Types.String)
qorrsMarker = Lens.field @"marker"
{-# DEPRECATED qorrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorrsResponseStatus :: Lens.Lens' QueryObjectsResponse Core.Int
qorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED qorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

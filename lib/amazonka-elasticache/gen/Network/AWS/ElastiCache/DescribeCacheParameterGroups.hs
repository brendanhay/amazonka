{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache parameter group descriptions. If a cache parameter group name is specified, the list contains only the descriptions for that group.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheParameterGroups
  ( -- * Creating a request
    DescribeCacheParameterGroups (..),
    mkDescribeCacheParameterGroups,

    -- ** Request lenses
    dcpgCacheParameterGroupName,
    dcpgMarker,
    dcpgMaxRecords,

    -- * Destructuring the response
    DescribeCacheParameterGroupsResponse (..),
    mkDescribeCacheParameterGroupsResponse,

    -- ** Response lenses
    dcpgrrsCacheParameterGroups,
    dcpgrrsMarker,
    dcpgrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheParameterGroups@ operation.
--
-- /See:/ 'mkDescribeCacheParameterGroups' smart constructor.
data DescribeCacheParameterGroups = DescribeCacheParameterGroups'
  { -- | The name of a specific cache parameter group to return details for.
    cacheParameterGroupName :: Core.Maybe Types.CacheParameterGroupName,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheParameterGroups' value with any optional fields omitted.
mkDescribeCacheParameterGroups ::
  DescribeCacheParameterGroups
mkDescribeCacheParameterGroups =
  DescribeCacheParameterGroups'
    { cacheParameterGroupName =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of a specific cache parameter group to return details for.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgCacheParameterGroupName :: Lens.Lens' DescribeCacheParameterGroups (Core.Maybe Types.CacheParameterGroupName)
dcpgCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# DEPRECATED dcpgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgMarker :: Lens.Lens' DescribeCacheParameterGroups (Core.Maybe Types.Marker)
dcpgMarker = Lens.field @"marker"
{-# DEPRECATED dcpgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgMaxRecords :: Lens.Lens' DescribeCacheParameterGroups (Core.Maybe Core.Int)
dcpgMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dcpgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeCacheParameterGroups where
  type
    Rs DescribeCacheParameterGroups =
      DescribeCacheParameterGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeCacheParameterGroups")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue "CacheParameterGroupName"
                            Core.<$> cacheParameterGroupName
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeCacheParameterGroupsResult"
      ( \s h x ->
          DescribeCacheParameterGroupsResponse'
            Core.<$> ( x Core..@? "CacheParameterGroups"
                         Core..<@> Core.parseXMLList "CacheParameterGroup"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeCacheParameterGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"cacheParameterGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Represents the output of a @DescribeCacheParameterGroups@ operation.
--
-- /See:/ 'mkDescribeCacheParameterGroupsResponse' smart constructor.
data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse'
  { -- | A list of cache parameter groups. Each element in the list contains detailed information about one cache parameter group.
    cacheParameterGroups :: Core.Maybe [Types.CacheParameterGroup],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheParameterGroupsResponse' value with any optional fields omitted.
mkDescribeCacheParameterGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCacheParameterGroupsResponse
mkDescribeCacheParameterGroupsResponse responseStatus =
  DescribeCacheParameterGroupsResponse'
    { cacheParameterGroups =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of cache parameter groups. Each element in the list contains detailed information about one cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrrsCacheParameterGroups :: Lens.Lens' DescribeCacheParameterGroupsResponse (Core.Maybe [Types.CacheParameterGroup])
dcpgrrsCacheParameterGroups = Lens.field @"cacheParameterGroups"
{-# DEPRECATED dcpgrrsCacheParameterGroups "Use generic-lens or generic-optics with 'cacheParameterGroups' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrrsMarker :: Lens.Lens' DescribeCacheParameterGroupsResponse (Core.Maybe Types.Marker)
dcpgrrsMarker = Lens.field @"marker"
{-# DEPRECATED dcpgrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrrsResponseStatus :: Lens.Lens' DescribeCacheParameterGroupsResponse Core.Int
dcpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

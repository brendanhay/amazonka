{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache security group descriptions. If a cache security group name is specified, the list contains only the description of that group. This applicable only when you have ElastiCache in Classic setup
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheSecurityGroups
  ( -- * Creating a request
    DescribeCacheSecurityGroups (..),
    mkDescribeCacheSecurityGroups,

    -- ** Request lenses
    dcsgsCacheSecurityGroupName,
    dcsgsMarker,
    dcsgsMaxRecords,

    -- * Destructuring the response
    DescribeCacheSecurityGroupsResponse (..),
    mkDescribeCacheSecurityGroupsResponse,

    -- ** Response lenses
    dcsgrfrsCacheSecurityGroups,
    dcsgrfrsMarker,
    dcsgrfrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheSecurityGroups@ operation.
--
-- /See:/ 'mkDescribeCacheSecurityGroups' smart constructor.
data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups'
  { -- | The name of the cache security group to return details for.
    cacheSecurityGroupName :: Core.Maybe Types.String,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheSecurityGroups' value with any optional fields omitted.
mkDescribeCacheSecurityGroups ::
  DescribeCacheSecurityGroups
mkDescribeCacheSecurityGroups =
  DescribeCacheSecurityGroups'
    { cacheSecurityGroupName =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of the cache security group to return details for.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsCacheSecurityGroupName :: Lens.Lens' DescribeCacheSecurityGroups (Core.Maybe Types.String)
dcsgsCacheSecurityGroupName = Lens.field @"cacheSecurityGroupName"
{-# DEPRECATED dcsgsCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsMarker :: Lens.Lens' DescribeCacheSecurityGroups (Core.Maybe Types.String)
dcsgsMarker = Lens.field @"marker"
{-# DEPRECATED dcsgsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsMaxRecords :: Lens.Lens' DescribeCacheSecurityGroups (Core.Maybe Core.Int)
dcsgsMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dcsgsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeCacheSecurityGroups where
  type
    Rs DescribeCacheSecurityGroups =
      DescribeCacheSecurityGroupsResponse
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
            ( Core.pure ("Action", "DescribeCacheSecurityGroups")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue "CacheSecurityGroupName"
                            Core.<$> cacheSecurityGroupName
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeCacheSecurityGroupsResult"
      ( \s h x ->
          DescribeCacheSecurityGroupsResponse'
            Core.<$> ( x Core..@? "CacheSecurityGroups"
                         Core..<@> Core.parseXMLList "CacheSecurityGroup"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeCacheSecurityGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"cacheSecurityGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Represents the output of a @DescribeCacheSecurityGroups@ operation.
--
-- /See:/ 'mkDescribeCacheSecurityGroupsResponse' smart constructor.
data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse'
  { -- | A list of cache security groups. Each element in the list contains detailed information about one group.
    cacheSecurityGroups :: Core.Maybe [Types.CacheSecurityGroup],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheSecurityGroupsResponse' value with any optional fields omitted.
mkDescribeCacheSecurityGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCacheSecurityGroupsResponse
mkDescribeCacheSecurityGroupsResponse responseStatus =
  DescribeCacheSecurityGroupsResponse'
    { cacheSecurityGroups =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of cache security groups. Each element in the list contains detailed information about one group.
--
-- /Note:/ Consider using 'cacheSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrfrsCacheSecurityGroups :: Lens.Lens' DescribeCacheSecurityGroupsResponse (Core.Maybe [Types.CacheSecurityGroup])
dcsgrfrsCacheSecurityGroups = Lens.field @"cacheSecurityGroups"
{-# DEPRECATED dcsgrfrsCacheSecurityGroups "Use generic-lens or generic-optics with 'cacheSecurityGroups' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrfrsMarker :: Lens.Lens' DescribeCacheSecurityGroupsResponse (Core.Maybe Types.String)
dcsgrfrsMarker = Lens.field @"marker"
{-# DEPRECATED dcsgrfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrfrsResponseStatus :: Lens.Lens' DescribeCacheSecurityGroupsResponse Core.Int
dcsgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcsgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

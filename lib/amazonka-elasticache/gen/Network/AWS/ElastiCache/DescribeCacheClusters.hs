{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all provisioned clusters if no cluster identifier is specified, or about a specific cache cluster if a cluster identifier is supplied.
--
-- By default, abbreviated information about the clusters is returned. You can use the optional /ShowCacheNodeInfo/ flag to retrieve detailed information about the cache nodes associated with the clusters. These details include the DNS address and port for the cache node endpoint.
-- If the cluster is in the /creating/ state, only cluster-level information is displayed until all of the nodes are successfully provisioned.
-- If the cluster is in the /deleting/ state, only cluster-level information is displayed.
-- If cache nodes are currently being added to the cluster, node endpoint information and creation time for the additional nodes are not displayed until they are completely provisioned. When the cluster state is /available/ , the cluster is ready for use.
-- If cache nodes are currently being removed from the cluster, no endpoint information for the removed nodes is displayed.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheClusters
  ( -- * Creating a request
    DescribeCacheClusters (..),
    mkDescribeCacheClusters,

    -- ** Request lenses
    dCacheClusterId,
    dMarker,
    dMaxRecords,
    dShowCacheClustersNotInReplicationGroups,
    dShowCacheNodeInfo,

    -- * Destructuring the response
    DescribeCacheClustersResponse (..),
    mkDescribeCacheClustersResponse,

    -- ** Response lenses
    drsCacheClusters,
    drsMarker,
    drsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheClusters@ operation.
--
-- /See:/ 'mkDescribeCacheClusters' smart constructor.
data DescribeCacheClusters = DescribeCacheClusters'
  { -- | The user-supplied cluster identifier. If this parameter is specified, only information about that specific cluster is returned. This parameter isn't case sensitive.
    cacheClusterId :: Core.Maybe Types.String,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | An optional flag that can be included in the @DescribeCacheCluster@ request to show only nodes (API/CLI: clusters) that are not members of a replication group. In practice, this mean Memcached and single node Redis clusters.
    showCacheClustersNotInReplicationGroups :: Core.Maybe Core.Bool,
    -- | An optional flag that can be included in the @DescribeCacheCluster@ request to retrieve information about the individual cache nodes.
    showCacheNodeInfo :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheClusters' value with any optional fields omitted.
mkDescribeCacheClusters ::
  DescribeCacheClusters
mkDescribeCacheClusters =
  DescribeCacheClusters'
    { cacheClusterId = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      showCacheClustersNotInReplicationGroups = Core.Nothing,
      showCacheNodeInfo = Core.Nothing
    }

-- | The user-supplied cluster identifier. If this parameter is specified, only information about that specific cluster is returned. This parameter isn't case sensitive.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCacheClusterId :: Lens.Lens' DescribeCacheClusters (Core.Maybe Types.String)
dCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED dCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMarker :: Lens.Lens' DescribeCacheClusters (Core.Maybe Types.String)
dMarker = Lens.field @"marker"
{-# DEPRECATED dMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxRecords :: Lens.Lens' DescribeCacheClusters (Core.Maybe Core.Int)
dMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | An optional flag that can be included in the @DescribeCacheCluster@ request to show only nodes (API/CLI: clusters) that are not members of a replication group. In practice, this mean Memcached and single node Redis clusters.
--
-- /Note:/ Consider using 'showCacheClustersNotInReplicationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dShowCacheClustersNotInReplicationGroups :: Lens.Lens' DescribeCacheClusters (Core.Maybe Core.Bool)
dShowCacheClustersNotInReplicationGroups = Lens.field @"showCacheClustersNotInReplicationGroups"
{-# DEPRECATED dShowCacheClustersNotInReplicationGroups "Use generic-lens or generic-optics with 'showCacheClustersNotInReplicationGroups' instead." #-}

-- | An optional flag that can be included in the @DescribeCacheCluster@ request to retrieve information about the individual cache nodes.
--
-- /Note:/ Consider using 'showCacheNodeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dShowCacheNodeInfo :: Lens.Lens' DescribeCacheClusters (Core.Maybe Core.Bool)
dShowCacheNodeInfo = Lens.field @"showCacheNodeInfo"
{-# DEPRECATED dShowCacheNodeInfo "Use generic-lens or generic-optics with 'showCacheNodeInfo' instead." #-}

instance Core.AWSRequest DescribeCacheClusters where
  type Rs DescribeCacheClusters = DescribeCacheClustersResponse
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
            ( Core.pure ("Action", "DescribeCacheClusters")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "CacheClusterId" Core.<$> cacheClusterId)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> ( Core.toQueryValue "ShowCacheClustersNotInReplicationGroups"
                            Core.<$> showCacheClustersNotInReplicationGroups
                        )
                Core.<> ( Core.toQueryValue "ShowCacheNodeInfo"
                            Core.<$> showCacheNodeInfo
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeCacheClustersResult"
      ( \s h x ->
          DescribeCacheClustersResponse'
            Core.<$> ( x Core..@? "CacheClusters"
                         Core..<@> Core.parseXMLList "CacheCluster"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeCacheClusters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"cacheClusters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Represents the output of a @DescribeCacheClusters@ operation.
--
-- /See:/ 'mkDescribeCacheClustersResponse' smart constructor.
data DescribeCacheClustersResponse = DescribeCacheClustersResponse'
  { -- | A list of clusters. Each item in the list contains detailed information about one cluster.
    cacheClusters :: Core.Maybe [Types.CacheCluster],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeCacheClustersResponse' value with any optional fields omitted.
mkDescribeCacheClustersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCacheClustersResponse
mkDescribeCacheClustersResponse responseStatus =
  DescribeCacheClustersResponse'
    { cacheClusters = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of clusters. Each item in the list contains detailed information about one cluster.
--
-- /Note:/ Consider using 'cacheClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCacheClusters :: Lens.Lens' DescribeCacheClustersResponse (Core.Maybe [Types.CacheCluster])
drsCacheClusters = Lens.field @"cacheClusters"
{-# DEPRECATED drsCacheClusters "Use generic-lens or generic-optics with 'cacheClusters' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsMarker :: Lens.Lens' DescribeCacheClustersResponse (Core.Maybe Types.Marker)
drsMarker = Lens.field @"marker"
{-# DEPRECATED drsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCacheClustersResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

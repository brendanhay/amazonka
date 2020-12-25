{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeUpdateActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details of the update actions
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeUpdateActions
  ( -- * Creating a request
    DescribeUpdateActions (..),
    mkDescribeUpdateActions,

    -- ** Request lenses
    duaCacheClusterIds,
    duaEngine,
    duaMarker,
    duaMaxRecords,
    duaReplicationGroupIds,
    duaServiceUpdateName,
    duaServiceUpdateStatus,
    duaServiceUpdateTimeRange,
    duaShowNodeLevelUpdateStatus,
    duaUpdateActionStatus,

    -- * Destructuring the response
    DescribeUpdateActionsResponse (..),
    mkDescribeUpdateActionsResponse,

    -- ** Response lenses
    duarrsMarker,
    duarrsUpdateActions,
    duarrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUpdateActions' smart constructor.
data DescribeUpdateActions = DescribeUpdateActions'
  { -- | The cache cluster IDs
    cacheClusterIds :: Core.Maybe [Types.String],
    -- | The Elasticache engine to which the update applies. Either Redis or Memcached
    engine :: Core.Maybe Types.Engine,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response
    maxRecords :: Core.Maybe Core.Int,
    -- | The replication group IDs
    replicationGroupIds :: Core.Maybe [Types.String],
    -- | The unique ID of the service update
    serviceUpdateName :: Core.Maybe Types.ServiceUpdateName,
    -- | The status of the service update
    serviceUpdateStatus :: Core.Maybe [Types.ServiceUpdateStatus],
    -- | The range of time specified to search for service updates that are in available status
    serviceUpdateTimeRange :: Core.Maybe Types.TimeRangeFilter,
    -- | Dictates whether to include node level update status in the response
    showNodeLevelUpdateStatus :: Core.Maybe Core.Bool,
    -- | The status of the update action.
    updateActionStatus :: Core.Maybe [Types.UpdateActionStatus]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeUpdateActions' value with any optional fields omitted.
mkDescribeUpdateActions ::
  DescribeUpdateActions
mkDescribeUpdateActions =
  DescribeUpdateActions'
    { cacheClusterIds = Core.Nothing,
      engine = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      replicationGroupIds = Core.Nothing,
      serviceUpdateName = Core.Nothing,
      serviceUpdateStatus = Core.Nothing,
      serviceUpdateTimeRange = Core.Nothing,
      showNodeLevelUpdateStatus = Core.Nothing,
      updateActionStatus = Core.Nothing
    }

-- | The cache cluster IDs
--
-- /Note:/ Consider using 'cacheClusterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaCacheClusterIds :: Lens.Lens' DescribeUpdateActions (Core.Maybe [Types.String])
duaCacheClusterIds = Lens.field @"cacheClusterIds"
{-# DEPRECATED duaCacheClusterIds "Use generic-lens or generic-optics with 'cacheClusterIds' instead." #-}

-- | The Elasticache engine to which the update applies. Either Redis or Memcached
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaEngine :: Lens.Lens' DescribeUpdateActions (Core.Maybe Types.Engine)
duaEngine = Lens.field @"engine"
{-# DEPRECATED duaEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaMarker :: Lens.Lens' DescribeUpdateActions (Core.Maybe Types.Marker)
duaMarker = Lens.field @"marker"
{-# DEPRECATED duaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaMaxRecords :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Int)
duaMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED duaMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The replication group IDs
--
-- /Note:/ Consider using 'replicationGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaReplicationGroupIds :: Lens.Lens' DescribeUpdateActions (Core.Maybe [Types.String])
duaReplicationGroupIds = Lens.field @"replicationGroupIds"
{-# DEPRECATED duaReplicationGroupIds "Use generic-lens or generic-optics with 'replicationGroupIds' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaServiceUpdateName :: Lens.Lens' DescribeUpdateActions (Core.Maybe Types.ServiceUpdateName)
duaServiceUpdateName = Lens.field @"serviceUpdateName"
{-# DEPRECATED duaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The status of the service update
--
-- /Note:/ Consider using 'serviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaServiceUpdateStatus :: Lens.Lens' DescribeUpdateActions (Core.Maybe [Types.ServiceUpdateStatus])
duaServiceUpdateStatus = Lens.field @"serviceUpdateStatus"
{-# DEPRECATED duaServiceUpdateStatus "Use generic-lens or generic-optics with 'serviceUpdateStatus' instead." #-}

-- | The range of time specified to search for service updates that are in available status
--
-- /Note:/ Consider using 'serviceUpdateTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaServiceUpdateTimeRange :: Lens.Lens' DescribeUpdateActions (Core.Maybe Types.TimeRangeFilter)
duaServiceUpdateTimeRange = Lens.field @"serviceUpdateTimeRange"
{-# DEPRECATED duaServiceUpdateTimeRange "Use generic-lens or generic-optics with 'serviceUpdateTimeRange' instead." #-}

-- | Dictates whether to include node level update status in the response
--
-- /Note:/ Consider using 'showNodeLevelUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaShowNodeLevelUpdateStatus :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Bool)
duaShowNodeLevelUpdateStatus = Lens.field @"showNodeLevelUpdateStatus"
{-# DEPRECATED duaShowNodeLevelUpdateStatus "Use generic-lens or generic-optics with 'showNodeLevelUpdateStatus' instead." #-}

-- | The status of the update action.
--
-- /Note:/ Consider using 'updateActionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaUpdateActionStatus :: Lens.Lens' DescribeUpdateActions (Core.Maybe [Types.UpdateActionStatus])
duaUpdateActionStatus = Lens.field @"updateActionStatus"
{-# DEPRECATED duaUpdateActionStatus "Use generic-lens or generic-optics with 'updateActionStatus' instead." #-}

instance Core.AWSRequest DescribeUpdateActions where
  type Rs DescribeUpdateActions = DescribeUpdateActionsResponse
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
            ( Core.pure ("Action", "DescribeUpdateActions")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "CacheClusterIds"
                            (Core.toQueryList "member" Core.<$> cacheClusterIds)
                        )
                Core.<> (Core.toQueryValue "Engine" Core.<$> engine)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> ( Core.toQueryValue
                            "ReplicationGroupIds"
                            (Core.toQueryList "member" Core.<$> replicationGroupIds)
                        )
                Core.<> (Core.toQueryValue "ServiceUpdateName" Core.<$> serviceUpdateName)
                Core.<> ( Core.toQueryValue
                            "ServiceUpdateStatus"
                            (Core.toQueryList "member" Core.<$> serviceUpdateStatus)
                        )
                Core.<> ( Core.toQueryValue "ServiceUpdateTimeRange"
                            Core.<$> serviceUpdateTimeRange
                        )
                Core.<> ( Core.toQueryValue "ShowNodeLevelUpdateStatus"
                            Core.<$> showNodeLevelUpdateStatus
                        )
                Core.<> ( Core.toQueryValue
                            "UpdateActionStatus"
                            (Core.toQueryList "member" Core.<$> updateActionStatus)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeUpdateActionsResult"
      ( \s h x ->
          DescribeUpdateActionsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "UpdateActions"
                         Core..<@> Core.parseXMLList "UpdateAction"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeUpdateActions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"updateActions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeUpdateActionsResponse' smart constructor.
data DescribeUpdateActionsResponse = DescribeUpdateActionsResponse'
  { -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | Returns a list of update actions
    updateActions :: Core.Maybe [Types.UpdateAction],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeUpdateActionsResponse' value with any optional fields omitted.
mkDescribeUpdateActionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUpdateActionsResponse
mkDescribeUpdateActionsResponse responseStatus =
  DescribeUpdateActionsResponse'
    { marker = Core.Nothing,
      updateActions = Core.Nothing,
      responseStatus
    }

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarrsMarker :: Lens.Lens' DescribeUpdateActionsResponse (Core.Maybe Types.String)
duarrsMarker = Lens.field @"marker"
{-# DEPRECATED duarrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Returns a list of update actions
--
-- /Note:/ Consider using 'updateActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarrsUpdateActions :: Lens.Lens' DescribeUpdateActionsResponse (Core.Maybe [Types.UpdateAction])
duarrsUpdateActions = Lens.field @"updateActions"
{-# DEPRECATED duarrsUpdateActions "Use generic-lens or generic-optics with 'updateActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarrsResponseStatus :: Lens.Lens' DescribeUpdateActionsResponse Core.Int
duarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED duarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

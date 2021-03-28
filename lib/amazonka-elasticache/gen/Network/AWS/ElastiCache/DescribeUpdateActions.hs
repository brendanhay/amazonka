{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeUpdateActions (..)
    , mkDescribeUpdateActions
    -- ** Request lenses
    , duaCacheClusterIds
    , duaEngine
    , duaMarker
    , duaMaxRecords
    , duaReplicationGroupIds
    , duaServiceUpdateName
    , duaServiceUpdateStatus
    , duaServiceUpdateTimeRange
    , duaShowNodeLevelUpdateStatus
    , duaUpdateActionStatus

    -- * Destructuring the response
    , DescribeUpdateActionsResponse (..)
    , mkDescribeUpdateActionsResponse
    -- ** Response lenses
    , duarrsMarker
    , duarrsUpdateActions
    , duarrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUpdateActions' smart constructor.
data DescribeUpdateActions = DescribeUpdateActions'
  { cacheClusterIds :: Core.Maybe [Core.Text]
    -- ^ The cache cluster IDs
  , engine :: Core.Maybe Core.Text
    -- ^ The Elasticache engine to which the update applies. Either Redis or Memcached 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response
  , replicationGroupIds :: Core.Maybe [Core.Text]
    -- ^ The replication group IDs
  , serviceUpdateName :: Core.Maybe Core.Text
    -- ^ The unique ID of the service update
  , serviceUpdateStatus :: Core.Maybe [Types.ServiceUpdateStatus]
    -- ^ The status of the service update
  , serviceUpdateTimeRange :: Core.Maybe Types.TimeRangeFilter
    -- ^ The range of time specified to search for service updates that are in available status
  , showNodeLevelUpdateStatus :: Core.Maybe Core.Bool
    -- ^ Dictates whether to include node level update status in the response 
  , updateActionStatus :: Core.Maybe [Types.UpdateActionStatus]
    -- ^ The status of the update action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeUpdateActions' value with any optional fields omitted.
mkDescribeUpdateActions
    :: DescribeUpdateActions
mkDescribeUpdateActions
  = DescribeUpdateActions'{cacheClusterIds = Core.Nothing,
                           engine = Core.Nothing, marker = Core.Nothing,
                           maxRecords = Core.Nothing, replicationGroupIds = Core.Nothing,
                           serviceUpdateName = Core.Nothing,
                           serviceUpdateStatus = Core.Nothing,
                           serviceUpdateTimeRange = Core.Nothing,
                           showNodeLevelUpdateStatus = Core.Nothing,
                           updateActionStatus = Core.Nothing}

-- | The cache cluster IDs
--
-- /Note:/ Consider using 'cacheClusterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaCacheClusterIds :: Lens.Lens' DescribeUpdateActions (Core.Maybe [Core.Text])
duaCacheClusterIds = Lens.field @"cacheClusterIds"
{-# INLINEABLE duaCacheClusterIds #-}
{-# DEPRECATED cacheClusterIds "Use generic-lens or generic-optics with 'cacheClusterIds' instead"  #-}

-- | The Elasticache engine to which the update applies. Either Redis or Memcached 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaEngine :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Text)
duaEngine = Lens.field @"engine"
{-# INLINEABLE duaEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaMarker :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Text)
duaMarker = Lens.field @"marker"
{-# INLINEABLE duaMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaMaxRecords :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Int)
duaMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE duaMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The replication group IDs
--
-- /Note:/ Consider using 'replicationGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaReplicationGroupIds :: Lens.Lens' DescribeUpdateActions (Core.Maybe [Core.Text])
duaReplicationGroupIds = Lens.field @"replicationGroupIds"
{-# INLINEABLE duaReplicationGroupIds #-}
{-# DEPRECATED replicationGroupIds "Use generic-lens or generic-optics with 'replicationGroupIds' instead"  #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaServiceUpdateName :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Text)
duaServiceUpdateName = Lens.field @"serviceUpdateName"
{-# INLINEABLE duaServiceUpdateName #-}
{-# DEPRECATED serviceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead"  #-}

-- | The status of the service update
--
-- /Note:/ Consider using 'serviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaServiceUpdateStatus :: Lens.Lens' DescribeUpdateActions (Core.Maybe [Types.ServiceUpdateStatus])
duaServiceUpdateStatus = Lens.field @"serviceUpdateStatus"
{-# INLINEABLE duaServiceUpdateStatus #-}
{-# DEPRECATED serviceUpdateStatus "Use generic-lens or generic-optics with 'serviceUpdateStatus' instead"  #-}

-- | The range of time specified to search for service updates that are in available status
--
-- /Note:/ Consider using 'serviceUpdateTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaServiceUpdateTimeRange :: Lens.Lens' DescribeUpdateActions (Core.Maybe Types.TimeRangeFilter)
duaServiceUpdateTimeRange = Lens.field @"serviceUpdateTimeRange"
{-# INLINEABLE duaServiceUpdateTimeRange #-}
{-# DEPRECATED serviceUpdateTimeRange "Use generic-lens or generic-optics with 'serviceUpdateTimeRange' instead"  #-}

-- | Dictates whether to include node level update status in the response 
--
-- /Note:/ Consider using 'showNodeLevelUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaShowNodeLevelUpdateStatus :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Bool)
duaShowNodeLevelUpdateStatus = Lens.field @"showNodeLevelUpdateStatus"
{-# INLINEABLE duaShowNodeLevelUpdateStatus #-}
{-# DEPRECATED showNodeLevelUpdateStatus "Use generic-lens or generic-optics with 'showNodeLevelUpdateStatus' instead"  #-}

-- | The status of the update action.
--
-- /Note:/ Consider using 'updateActionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaUpdateActionStatus :: Lens.Lens' DescribeUpdateActions (Core.Maybe [Types.UpdateActionStatus])
duaUpdateActionStatus = Lens.field @"updateActionStatus"
{-# INLINEABLE duaUpdateActionStatus #-}
{-# DEPRECATED updateActionStatus "Use generic-lens or generic-optics with 'updateActionStatus' instead"  #-}

instance Core.ToQuery DescribeUpdateActions where
        toQuery DescribeUpdateActions{..}
          = Core.toQueryPair "Action" ("DescribeUpdateActions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheClusterIds"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   cacheClusterIds)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Engine") engine
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.toQueryPair "ReplicationGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   replicationGroupIds)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ServiceUpdateName")
                serviceUpdateName
              Core.<>
              Core.toQueryPair "ServiceUpdateStatus"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   serviceUpdateStatus)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ServiceUpdateTimeRange")
                serviceUpdateTimeRange
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ShowNodeLevelUpdateStatus")
                showNodeLevelUpdateStatus
              Core.<>
              Core.toQueryPair "UpdateActionStatus"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   updateActionStatus)

instance Core.ToHeaders DescribeUpdateActions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeUpdateActions where
        type Rs DescribeUpdateActions = DescribeUpdateActionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeUpdateActionsResult"
              (\ s h x ->
                 DescribeUpdateActionsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "UpdateActions" Core..<@>
                       Core.parseXMLList "UpdateAction"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeUpdateActions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"updateActions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeUpdateActionsResponse' smart constructor.
data DescribeUpdateActionsResponse = DescribeUpdateActionsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , updateActions :: Core.Maybe [Types.UpdateAction]
    -- ^ Returns a list of update actions
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeUpdateActionsResponse' value with any optional fields omitted.
mkDescribeUpdateActionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeUpdateActionsResponse
mkDescribeUpdateActionsResponse responseStatus
  = DescribeUpdateActionsResponse'{marker = Core.Nothing,
                                   updateActions = Core.Nothing, responseStatus}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarrsMarker :: Lens.Lens' DescribeUpdateActionsResponse (Core.Maybe Core.Text)
duarrsMarker = Lens.field @"marker"
{-# INLINEABLE duarrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Returns a list of update actions
--
-- /Note:/ Consider using 'updateActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarrsUpdateActions :: Lens.Lens' DescribeUpdateActionsResponse (Core.Maybe [Types.UpdateAction])
duarrsUpdateActions = Lens.field @"updateActions"
{-# INLINEABLE duarrsUpdateActions #-}
{-# DEPRECATED updateActions "Use generic-lens or generic-optics with 'updateActions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarrsResponseStatus :: Lens.Lens' DescribeUpdateActionsResponse Core.Int
duarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE duarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

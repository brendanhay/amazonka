{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about cluster or replication group snapshots. By default, @DescribeSnapshots@ lists all of your snapshots; it can optionally describe a single snapshot, or just the snapshots associated with a particular cache cluster.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeSnapshots
    (
    -- * Creating a request
      DescribeSnapshots (..)
    , mkDescribeSnapshots
    -- ** Request lenses
    , dsCacheClusterId
    , dsMarker
    , dsMaxRecords
    , dsReplicationGroupId
    , dsShowNodeGroupConfig
    , dsSnapshotName
    , dsSnapshotSource

    -- * Destructuring the response
    , DescribeSnapshotsResponse (..)
    , mkDescribeSnapshotsResponse
    -- ** Response lenses
    , dsrrsMarker
    , dsrrsSnapshots
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeSnapshotsMessage@ operation.
--
-- /See:/ 'mkDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { cacheClusterId :: Core.Maybe Core.Text
    -- ^ A user-supplied cluster identifier. If this parameter is specified, only snapshots associated with that specific cluster are described.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 50
-- Constraints: minimum 20; maximum 50.
  , replicationGroupId :: Core.Maybe Core.Text
    -- ^ A user-supplied replication group identifier. If this parameter is specified, only snapshots associated with that specific replication group are described.
  , showNodeGroupConfig :: Core.Maybe Core.Bool
    -- ^ A Boolean value which if true, the node group (shard) configuration is included in the snapshot description.
  , snapshotName :: Core.Maybe Core.Text
    -- ^ A user-supplied name of the snapshot. If this parameter is specified, only this snapshot are described.
  , snapshotSource :: Core.Maybe Core.Text
    -- ^ If set to @system@ , the output shows snapshots that were automatically created by ElastiCache. If set to @user@ the output shows snapshots that were manually created. If omitted, the output shows both automatically and manually created snapshots.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshots' value with any optional fields omitted.
mkDescribeSnapshots
    :: DescribeSnapshots
mkDescribeSnapshots
  = DescribeSnapshots'{cacheClusterId = Core.Nothing,
                       marker = Core.Nothing, maxRecords = Core.Nothing,
                       replicationGroupId = Core.Nothing,
                       showNodeGroupConfig = Core.Nothing, snapshotName = Core.Nothing,
                       snapshotSource = Core.Nothing}

-- | A user-supplied cluster identifier. If this parameter is specified, only snapshots associated with that specific cluster are described.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCacheClusterId :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
dsCacheClusterId = Lens.field @"cacheClusterId"
{-# INLINEABLE dsCacheClusterId #-}
{-# DEPRECATED cacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead"  #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMarker :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
dsMarker = Lens.field @"marker"
{-# INLINEABLE dsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 50
-- Constraints: minimum 20; maximum 50.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxRecords :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Int)
dsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | A user-supplied replication group identifier. If this parameter is specified, only snapshots associated with that specific replication group are described.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsReplicationGroupId :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
dsReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE dsReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

-- | A Boolean value which if true, the node group (shard) configuration is included in the snapshot description.
--
-- /Note:/ Consider using 'showNodeGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsShowNodeGroupConfig :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Bool)
dsShowNodeGroupConfig = Lens.field @"showNodeGroupConfig"
{-# INLINEABLE dsShowNodeGroupConfig #-}
{-# DEPRECATED showNodeGroupConfig "Use generic-lens or generic-optics with 'showNodeGroupConfig' instead"  #-}

-- | A user-supplied name of the snapshot. If this parameter is specified, only this snapshot are described.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotName :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
dsSnapshotName = Lens.field @"snapshotName"
{-# INLINEABLE dsSnapshotName #-}
{-# DEPRECATED snapshotName "Use generic-lens or generic-optics with 'snapshotName' instead"  #-}

-- | If set to @system@ , the output shows snapshots that were automatically created by ElastiCache. If set to @user@ the output shows snapshots that were manually created. If omitted, the output shows both automatically and manually created snapshots.
--
-- /Note:/ Consider using 'snapshotSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotSource :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
dsSnapshotSource = Lens.field @"snapshotSource"
{-# INLINEABLE dsSnapshotSource #-}
{-# DEPRECATED snapshotSource "Use generic-lens or generic-optics with 'snapshotSource' instead"  #-}

instance Core.ToQuery DescribeSnapshots where
        toQuery DescribeSnapshots{..}
          = Core.toQueryPair "Action" ("DescribeSnapshots" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CacheClusterId")
                cacheClusterId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReplicationGroupId")
                replicationGroupId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ShowNodeGroupConfig")
                showNodeGroupConfig
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotName")
                snapshotName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotSource")
                snapshotSource

instance Core.ToHeaders DescribeSnapshots where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSnapshots where
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
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
          = Response.receiveXMLWrapper "DescribeSnapshotsResult"
              (\ s h x ->
                 DescribeSnapshotsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "Snapshots" Core..<@> Core.parseXMLList "Snapshot"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSnapshots where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"snapshots" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Represents the output of a @DescribeSnapshots@ operation.
--
-- /See:/ 'mkDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , snapshots :: Core.Maybe [Types.Snapshot]
    -- ^ A list of snapshots. Each item in the list contains detailed information about one snapshot.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSnapshotsResponse' value with any optional fields omitted.
mkDescribeSnapshotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSnapshotsResponse
mkDescribeSnapshotsResponse responseStatus
  = DescribeSnapshotsResponse'{marker = Core.Nothing,
                               snapshots = Core.Nothing, responseStatus}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsMarker :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe Core.Text)
dsrrsMarker = Lens.field @"marker"
{-# INLINEABLE dsrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of snapshots. Each item in the list contains detailed information about one snapshot.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSnapshots :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe [Types.Snapshot])
dsrrsSnapshots = Lens.field @"snapshots"
{-# INLINEABLE dsrrsSnapshots #-}
{-# DEPRECATED snapshots "Use generic-lens or generic-optics with 'snapshots' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeSnapshotsResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

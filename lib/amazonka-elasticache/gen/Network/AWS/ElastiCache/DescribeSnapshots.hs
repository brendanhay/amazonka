{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeSnapshots (..),
    mkDescribeSnapshots,

    -- ** Request lenses
    dsCacheClusterId,
    dsMarker,
    dsMaxRecords,
    dsReplicationGroupId,
    dsShowNodeGroupConfig,
    dsSnapshotName,
    dsSnapshotSource,

    -- * Destructuring the response
    DescribeSnapshotsResponse (..),
    mkDescribeSnapshotsResponse,

    -- ** Response lenses
    dsrrsMarker,
    dsrrsSnapshots,
    dsrrsResponseStatus,
  )
where

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
  { -- | A user-supplied cluster identifier. If this parameter is specified, only snapshots associated with that specific cluster are described.
    cacheClusterId :: Core.Maybe Types.String,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 50
    -- Constraints: minimum 20; maximum 50.
    maxRecords :: Core.Maybe Core.Int,
    -- | A user-supplied replication group identifier. If this parameter is specified, only snapshots associated with that specific replication group are described.
    replicationGroupId :: Core.Maybe Types.String,
    -- | A Boolean value which if true, the node group (shard) configuration is included in the snapshot description.
    showNodeGroupConfig :: Core.Maybe Core.Bool,
    -- | A user-supplied name of the snapshot. If this parameter is specified, only this snapshot are described.
    snapshotName :: Core.Maybe Types.String,
    -- | If set to @system@ , the output shows snapshots that were automatically created by ElastiCache. If set to @user@ the output shows snapshots that were manually created. If omitted, the output shows both automatically and manually created snapshots.
    snapshotSource :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshots' value with any optional fields omitted.
mkDescribeSnapshots ::
  DescribeSnapshots
mkDescribeSnapshots =
  DescribeSnapshots'
    { cacheClusterId = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      replicationGroupId = Core.Nothing,
      showNodeGroupConfig = Core.Nothing,
      snapshotName = Core.Nothing,
      snapshotSource = Core.Nothing
    }

-- | A user-supplied cluster identifier. If this parameter is specified, only snapshots associated with that specific cluster are described.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCacheClusterId :: Lens.Lens' DescribeSnapshots (Core.Maybe Types.String)
dsCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED dsCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMarker :: Lens.Lens' DescribeSnapshots (Core.Maybe Types.String)
dsMarker = Lens.field @"marker"
{-# DEPRECATED dsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 50
-- Constraints: minimum 20; maximum 50.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxRecords :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Int)
dsMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A user-supplied replication group identifier. If this parameter is specified, only snapshots associated with that specific replication group are described.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsReplicationGroupId :: Lens.Lens' DescribeSnapshots (Core.Maybe Types.String)
dsReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED dsReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | A Boolean value which if true, the node group (shard) configuration is included in the snapshot description.
--
-- /Note:/ Consider using 'showNodeGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsShowNodeGroupConfig :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Bool)
dsShowNodeGroupConfig = Lens.field @"showNodeGroupConfig"
{-# DEPRECATED dsShowNodeGroupConfig "Use generic-lens or generic-optics with 'showNodeGroupConfig' instead." #-}

-- | A user-supplied name of the snapshot. If this parameter is specified, only this snapshot are described.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotName :: Lens.Lens' DescribeSnapshots (Core.Maybe Types.String)
dsSnapshotName = Lens.field @"snapshotName"
{-# DEPRECATED dsSnapshotName "Use generic-lens or generic-optics with 'snapshotName' instead." #-}

-- | If set to @system@ , the output shows snapshots that were automatically created by ElastiCache. If set to @user@ the output shows snapshots that were manually created. If omitted, the output shows both automatically and manually created snapshots.
--
-- /Note:/ Consider using 'snapshotSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotSource :: Lens.Lens' DescribeSnapshots (Core.Maybe Types.String)
dsSnapshotSource = Lens.field @"snapshotSource"
{-# DEPRECATED dsSnapshotSource "Use generic-lens or generic-optics with 'snapshotSource' instead." #-}

instance Core.AWSRequest DescribeSnapshots where
  type Rs DescribeSnapshots = DescribeSnapshotsResponse
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
            ( Core.pure ("Action", "DescribeSnapshots")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "CacheClusterId" Core.<$> cacheClusterId)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> ( Core.toQueryValue "ReplicationGroupId"
                            Core.<$> replicationGroupId
                        )
                Core.<> ( Core.toQueryValue "ShowNodeGroupConfig"
                            Core.<$> showNodeGroupConfig
                        )
                Core.<> (Core.toQueryValue "SnapshotName" Core.<$> snapshotName)
                Core.<> (Core.toQueryValue "SnapshotSource" Core.<$> snapshotSource)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeSnapshotsResult"
      ( \s h x ->
          DescribeSnapshotsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> (x Core..@? "Snapshots" Core..<@> Core.parseXMLList "Snapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSnapshots where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"snapshots" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Represents the output of a @DescribeSnapshots@ operation.
--
-- /See:/ 'mkDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | A list of snapshots. Each item in the list contains detailed information about one snapshot.
    snapshots :: Core.Maybe [Types.Snapshot],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeSnapshotsResponse' value with any optional fields omitted.
mkDescribeSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSnapshotsResponse
mkDescribeSnapshotsResponse responseStatus =
  DescribeSnapshotsResponse'
    { marker = Core.Nothing,
      snapshots = Core.Nothing,
      responseStatus
    }

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsMarker :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe Types.String)
dsrrsMarker = Lens.field @"marker"
{-# DEPRECATED dsrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of snapshots. Each item in the list contains detailed information about one snapshot.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSnapshots :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe [Types.Snapshot])
dsrrsSnapshots = Lens.field @"snapshots"
{-# DEPRECATED dsrrsSnapshots "Use generic-lens or generic-optics with 'snapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeSnapshotsResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

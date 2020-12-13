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
    dCacheClusterId,
    dMarker,
    dMaxRecords,
    dSnapshotName,
    dShowNodeGroupConfig,
    dReplicationGroupId,
    dSnapshotSource,

    -- * Destructuring the response
    DescribeSnapshotsResponse (..),
    mkDescribeSnapshotsResponse,

    -- ** Response lenses
    dsrsSnapshots,
    dsrsMarker,
    dsrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeSnapshotsMessage@ operation.
--
-- /See:/ 'mkDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { -- | A user-supplied cluster identifier. If this parameter is specified, only snapshots associated with that specific cluster are described.
    cacheClusterId :: Lude.Maybe Lude.Text,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 50
    -- Constraints: minimum 20; maximum 50.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | A user-supplied name of the snapshot. If this parameter is specified, only this snapshot are described.
    snapshotName :: Lude.Maybe Lude.Text,
    -- | A Boolean value which if true, the node group (shard) configuration is included in the snapshot description.
    showNodeGroupConfig :: Lude.Maybe Lude.Bool,
    -- | A user-supplied replication group identifier. If this parameter is specified, only snapshots associated with that specific replication group are described.
    replicationGroupId :: Lude.Maybe Lude.Text,
    -- | If set to @system@ , the output shows snapshots that were automatically created by ElastiCache. If set to @user@ the output shows snapshots that were manually created. If omitted, the output shows both automatically and manually created snapshots.
    snapshotSource :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshots' with the minimum fields required to make a request.
--
-- * 'cacheClusterId' - A user-supplied cluster identifier. If this parameter is specified, only snapshots associated with that specific cluster are described.
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 50
-- Constraints: minimum 20; maximum 50.
-- * 'snapshotName' - A user-supplied name of the snapshot. If this parameter is specified, only this snapshot are described.
-- * 'showNodeGroupConfig' - A Boolean value which if true, the node group (shard) configuration is included in the snapshot description.
-- * 'replicationGroupId' - A user-supplied replication group identifier. If this parameter is specified, only snapshots associated with that specific replication group are described.
-- * 'snapshotSource' - If set to @system@ , the output shows snapshots that were automatically created by ElastiCache. If set to @user@ the output shows snapshots that were manually created. If omitted, the output shows both automatically and manually created snapshots.
mkDescribeSnapshots ::
  DescribeSnapshots
mkDescribeSnapshots =
  DescribeSnapshots'
    { cacheClusterId = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      snapshotName = Lude.Nothing,
      showNodeGroupConfig = Lude.Nothing,
      replicationGroupId = Lude.Nothing,
      snapshotSource = Lude.Nothing
    }

-- | A user-supplied cluster identifier. If this parameter is specified, only snapshots associated with that specific cluster are described.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCacheClusterId :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Text)
dCacheClusterId = Lens.lens (cacheClusterId :: DescribeSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: DescribeSnapshots)
{-# DEPRECATED dCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMarker :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Text)
dMarker = Lens.lens (marker :: DescribeSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSnapshots)
{-# DEPRECATED dMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 50
-- Constraints: minimum 20; maximum 50.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxRecords :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Int)
dMaxRecords = Lens.lens (maxRecords :: DescribeSnapshots -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeSnapshots)
{-# DEPRECATED dMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A user-supplied name of the snapshot. If this parameter is specified, only this snapshot are described.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotName :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Text)
dSnapshotName = Lens.lens (snapshotName :: DescribeSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {snapshotName = a} :: DescribeSnapshots)
{-# DEPRECATED dSnapshotName "Use generic-lens or generic-optics with 'snapshotName' instead." #-}

-- | A Boolean value which if true, the node group (shard) configuration is included in the snapshot description.
--
-- /Note:/ Consider using 'showNodeGroupConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dShowNodeGroupConfig :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Bool)
dShowNodeGroupConfig = Lens.lens (showNodeGroupConfig :: DescribeSnapshots -> Lude.Maybe Lude.Bool) (\s a -> s {showNodeGroupConfig = a} :: DescribeSnapshots)
{-# DEPRECATED dShowNodeGroupConfig "Use generic-lens or generic-optics with 'showNodeGroupConfig' instead." #-}

-- | A user-supplied replication group identifier. If this parameter is specified, only snapshots associated with that specific replication group are described.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dReplicationGroupId :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Text)
dReplicationGroupId = Lens.lens (replicationGroupId :: DescribeSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: DescribeSnapshots)
{-# DEPRECATED dReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | If set to @system@ , the output shows snapshots that were automatically created by ElastiCache. If set to @user@ the output shows snapshots that were manually created. If omitted, the output shows both automatically and manually created snapshots.
--
-- /Note:/ Consider using 'snapshotSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotSource :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Text)
dSnapshotSource = Lens.lens (snapshotSource :: DescribeSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {snapshotSource = a} :: DescribeSnapshots)
{-# DEPRECATED dSnapshotSource "Use generic-lens or generic-optics with 'snapshotSource' instead." #-}

instance Page.AWSPager DescribeSnapshots where
  page rq rs
    | Page.stop (rs Lens.^. dsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dsrsSnapshots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dMarker Lens..~ rs Lens.^. dsrsMarker

instance Lude.AWSRequest DescribeSnapshots where
  type Rs DescribeSnapshots = DescribeSnapshotsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeSnapshotsResult"
      ( \s h x ->
          DescribeSnapshotsResponse'
            Lude.<$> ( x Lude..@? "Snapshots" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Snapshot")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSnapshots where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSnapshots where
  toQuery DescribeSnapshots' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSnapshots" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheClusterId" Lude.=: cacheClusterId,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "SnapshotName" Lude.=: snapshotName,
        "ShowNodeGroupConfig" Lude.=: showNodeGroupConfig,
        "ReplicationGroupId" Lude.=: replicationGroupId,
        "SnapshotSource" Lude.=: snapshotSource
      ]

-- | Represents the output of a @DescribeSnapshots@ operation.
--
-- /See:/ 'mkDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | A list of snapshots. Each item in the list contains detailed information about one snapshot.
    snapshots :: Lude.Maybe [Snapshot],
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'snapshots' - A list of snapshots. Each item in the list contains detailed information about one snapshot.
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSnapshotsResponse
mkDescribeSnapshotsResponse pResponseStatus_ =
  DescribeSnapshotsResponse'
    { snapshots = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of snapshots. Each item in the list contains detailed information about one snapshot.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSnapshots :: Lens.Lens' DescribeSnapshotsResponse (Lude.Maybe [Snapshot])
dsrsSnapshots = Lens.lens (snapshots :: DescribeSnapshotsResponse -> Lude.Maybe [Snapshot]) (\s a -> s {snapshots = a} :: DescribeSnapshotsResponse)
{-# DEPRECATED dsrsSnapshots "Use generic-lens or generic-optics with 'snapshots' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsMarker :: Lens.Lens' DescribeSnapshotsResponse (Lude.Maybe Lude.Text)
dsrsMarker = Lens.lens (marker :: DescribeSnapshotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSnapshotsResponse)
{-# DEPRECATED dsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeSnapshotsResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSnapshotsResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

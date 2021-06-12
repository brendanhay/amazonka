{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about cluster or replication group snapshots. By
-- default, @DescribeSnapshots@ lists all of your snapshots; it can
-- optionally describe a single snapshot, or just the snapshots associated
-- with a particular cache cluster.
--
-- This operation is valid for Redis only.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeSnapshots
  ( -- * Creating a Request
    DescribeSnapshots (..),
    newDescribeSnapshots,

    -- * Request Lenses
    describeSnapshots_replicationGroupId,
    describeSnapshots_cacheClusterId,
    describeSnapshots_snapshotSource,
    describeSnapshots_showNodeGroupConfig,
    describeSnapshots_snapshotName,
    describeSnapshots_marker,
    describeSnapshots_maxRecords,

    -- * Destructuring the Response
    DescribeSnapshotsResponse (..),
    newDescribeSnapshotsResponse,

    -- * Response Lenses
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_marker,
    describeSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeSnapshotsMessage@ operation.
--
-- /See:/ 'newDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { -- | A user-supplied replication group identifier. If this parameter is
    -- specified, only snapshots associated with that specific replication
    -- group are described.
    replicationGroupId :: Core.Maybe Core.Text,
    -- | A user-supplied cluster identifier. If this parameter is specified, only
    -- snapshots associated with that specific cluster are described.
    cacheClusterId :: Core.Maybe Core.Text,
    -- | If set to @system@, the output shows snapshots that were automatically
    -- created by ElastiCache. If set to @user@ the output shows snapshots that
    -- were manually created. If omitted, the output shows both automatically
    -- and manually created snapshots.
    snapshotSource :: Core.Maybe Core.Text,
    -- | A Boolean value which if true, the node group (shard) configuration is
    -- included in the snapshot description.
    showNodeGroupConfig :: Core.Maybe Core.Bool,
    -- | A user-supplied name of the snapshot. If this parameter is specified,
    -- only this snapshot are described.
    snapshotName :: Core.Maybe Core.Text,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a marker is
    -- included in the response so that the remaining results can be retrieved.
    --
    -- Default: 50
    --
    -- Constraints: minimum 20; maximum 50.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroupId', 'describeSnapshots_replicationGroupId' - A user-supplied replication group identifier. If this parameter is
-- specified, only snapshots associated with that specific replication
-- group are described.
--
-- 'cacheClusterId', 'describeSnapshots_cacheClusterId' - A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cluster are described.
--
-- 'snapshotSource', 'describeSnapshots_snapshotSource' - If set to @system@, the output shows snapshots that were automatically
-- created by ElastiCache. If set to @user@ the output shows snapshots that
-- were manually created. If omitted, the output shows both automatically
-- and manually created snapshots.
--
-- 'showNodeGroupConfig', 'describeSnapshots_showNodeGroupConfig' - A Boolean value which if true, the node group (shard) configuration is
-- included in the snapshot description.
--
-- 'snapshotName', 'describeSnapshots_snapshotName' - A user-supplied name of the snapshot. If this parameter is specified,
-- only this snapshot are described.
--
-- 'marker', 'describeSnapshots_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeSnapshots_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 50
--
-- Constraints: minimum 20; maximum 50.
newDescribeSnapshots ::
  DescribeSnapshots
newDescribeSnapshots =
  DescribeSnapshots'
    { replicationGroupId =
        Core.Nothing,
      cacheClusterId = Core.Nothing,
      snapshotSource = Core.Nothing,
      showNodeGroupConfig = Core.Nothing,
      snapshotName = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | A user-supplied replication group identifier. If this parameter is
-- specified, only snapshots associated with that specific replication
-- group are described.
describeSnapshots_replicationGroupId :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
describeSnapshots_replicationGroupId = Lens.lens (\DescribeSnapshots' {replicationGroupId} -> replicationGroupId) (\s@DescribeSnapshots' {} a -> s {replicationGroupId = a} :: DescribeSnapshots)

-- | A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cluster are described.
describeSnapshots_cacheClusterId :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
describeSnapshots_cacheClusterId = Lens.lens (\DescribeSnapshots' {cacheClusterId} -> cacheClusterId) (\s@DescribeSnapshots' {} a -> s {cacheClusterId = a} :: DescribeSnapshots)

-- | If set to @system@, the output shows snapshots that were automatically
-- created by ElastiCache. If set to @user@ the output shows snapshots that
-- were manually created. If omitted, the output shows both automatically
-- and manually created snapshots.
describeSnapshots_snapshotSource :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
describeSnapshots_snapshotSource = Lens.lens (\DescribeSnapshots' {snapshotSource} -> snapshotSource) (\s@DescribeSnapshots' {} a -> s {snapshotSource = a} :: DescribeSnapshots)

-- | A Boolean value which if true, the node group (shard) configuration is
-- included in the snapshot description.
describeSnapshots_showNodeGroupConfig :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Bool)
describeSnapshots_showNodeGroupConfig = Lens.lens (\DescribeSnapshots' {showNodeGroupConfig} -> showNodeGroupConfig) (\s@DescribeSnapshots' {} a -> s {showNodeGroupConfig = a} :: DescribeSnapshots)

-- | A user-supplied name of the snapshot. If this parameter is specified,
-- only this snapshot are described.
describeSnapshots_snapshotName :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
describeSnapshots_snapshotName = Lens.lens (\DescribeSnapshots' {snapshotName} -> snapshotName) (\s@DescribeSnapshots' {} a -> s {snapshotName = a} :: DescribeSnapshots)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeSnapshots_marker :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Text)
describeSnapshots_marker = Lens.lens (\DescribeSnapshots' {marker} -> marker) (\s@DescribeSnapshots' {} a -> s {marker = a} :: DescribeSnapshots)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 50
--
-- Constraints: minimum 20; maximum 50.
describeSnapshots_maxRecords :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Int)
describeSnapshots_maxRecords = Lens.lens (\DescribeSnapshots' {maxRecords} -> maxRecords) (\s@DescribeSnapshots' {} a -> s {maxRecords = a} :: DescribeSnapshots)

instance Core.AWSPager DescribeSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSnapshotsResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSnapshotsResponse_snapshots
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSnapshots_marker
          Lens..~ rs
          Lens.^? describeSnapshotsResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeSnapshots where
  type
    AWSResponse DescribeSnapshots =
      DescribeSnapshotsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeSnapshotsResult"
      ( \s h x ->
          DescribeSnapshotsResponse'
            Core.<$> ( x Core..@? "Snapshots" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "Snapshot")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSnapshots

instance Core.NFData DescribeSnapshots

instance Core.ToHeaders DescribeSnapshots where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSnapshots where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSnapshots where
  toQuery DescribeSnapshots' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeSnapshots" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "ReplicationGroupId" Core.=: replicationGroupId,
        "CacheClusterId" Core.=: cacheClusterId,
        "SnapshotSource" Core.=: snapshotSource,
        "ShowNodeGroupConfig" Core.=: showNodeGroupConfig,
        "SnapshotName" Core.=: snapshotName,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Represents the output of a @DescribeSnapshots@ operation.
--
-- /See:/ 'newDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | A list of snapshots. Each item in the list contains detailed information
    -- about one snapshot.
    snapshots :: Core.Maybe [Snapshot],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshots', 'describeSnapshotsResponse_snapshots' - A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
--
-- 'marker', 'describeSnapshotsResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeSnapshotsResponse_httpStatus' - The response's http status code.
newDescribeSnapshotsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSnapshotsResponse
newDescribeSnapshotsResponse pHttpStatus_ =
  DescribeSnapshotsResponse'
    { snapshots =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
describeSnapshotsResponse_snapshots :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe [Snapshot])
describeSnapshotsResponse_snapshots = Lens.lens (\DescribeSnapshotsResponse' {snapshots} -> snapshots) (\s@DescribeSnapshotsResponse' {} a -> s {snapshots = a} :: DescribeSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeSnapshotsResponse_marker :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe Core.Text)
describeSnapshotsResponse_marker = Lens.lens (\DescribeSnapshotsResponse' {marker} -> marker) (\s@DescribeSnapshotsResponse' {} a -> s {marker = a} :: DescribeSnapshotsResponse)

-- | The response's http status code.
describeSnapshotsResponse_httpStatus :: Lens.Lens' DescribeSnapshotsResponse Core.Int
describeSnapshotsResponse_httpStatus = Lens.lens (\DescribeSnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotsResponse)

instance Core.NFData DescribeSnapshotsResponse

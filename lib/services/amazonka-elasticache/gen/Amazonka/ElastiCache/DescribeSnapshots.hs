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
-- Module      : Amazonka.ElastiCache.DescribeSnapshots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.ElastiCache.DescribeSnapshots
  ( -- * Creating a Request
    DescribeSnapshots (..),
    newDescribeSnapshots,

    -- * Request Lenses
    describeSnapshots_cacheClusterId,
    describeSnapshots_marker,
    describeSnapshots_maxRecords,
    describeSnapshots_replicationGroupId,
    describeSnapshots_showNodeGroupConfig,
    describeSnapshots_snapshotName,
    describeSnapshots_snapshotSource,

    -- * Destructuring the Response
    DescribeSnapshotsResponse (..),
    newDescribeSnapshotsResponse,

    -- * Response Lenses
    describeSnapshotsResponse_marker,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeSnapshotsMessage@ operation.
--
-- /See:/ 'newDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { -- | A user-supplied cluster identifier. If this parameter is specified, only
    -- snapshots associated with that specific cluster are described.
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a marker is
    -- included in the response so that the remaining results can be retrieved.
    --
    -- Default: 50
    --
    -- Constraints: minimum 20; maximum 50.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | A user-supplied replication group identifier. If this parameter is
    -- specified, only snapshots associated with that specific replication
    -- group are described.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value which if true, the node group (shard) configuration is
    -- included in the snapshot description.
    showNodeGroupConfig :: Prelude.Maybe Prelude.Bool,
    -- | A user-supplied name of the snapshot. If this parameter is specified,
    -- only this snapshot are described.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | If set to @system@, the output shows snapshots that were automatically
    -- created by ElastiCache. If set to @user@ the output shows snapshots that
    -- were manually created. If omitted, the output shows both automatically
    -- and manually created snapshots.
    snapshotSource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusterId', 'describeSnapshots_cacheClusterId' - A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cluster are described.
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
--
-- 'replicationGroupId', 'describeSnapshots_replicationGroupId' - A user-supplied replication group identifier. If this parameter is
-- specified, only snapshots associated with that specific replication
-- group are described.
--
-- 'showNodeGroupConfig', 'describeSnapshots_showNodeGroupConfig' - A Boolean value which if true, the node group (shard) configuration is
-- included in the snapshot description.
--
-- 'snapshotName', 'describeSnapshots_snapshotName' - A user-supplied name of the snapshot. If this parameter is specified,
-- only this snapshot are described.
--
-- 'snapshotSource', 'describeSnapshots_snapshotSource' - If set to @system@, the output shows snapshots that were automatically
-- created by ElastiCache. If set to @user@ the output shows snapshots that
-- were manually created. If omitted, the output shows both automatically
-- and manually created snapshots.
newDescribeSnapshots ::
  DescribeSnapshots
newDescribeSnapshots =
  DescribeSnapshots'
    { cacheClusterId =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      showNodeGroupConfig = Prelude.Nothing,
      snapshotName = Prelude.Nothing,
      snapshotSource = Prelude.Nothing
    }

-- | A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cluster are described.
describeSnapshots_cacheClusterId :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_cacheClusterId = Lens.lens (\DescribeSnapshots' {cacheClusterId} -> cacheClusterId) (\s@DescribeSnapshots' {} a -> s {cacheClusterId = a} :: DescribeSnapshots)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeSnapshots_marker :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_marker = Lens.lens (\DescribeSnapshots' {marker} -> marker) (\s@DescribeSnapshots' {} a -> s {marker = a} :: DescribeSnapshots)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 50
--
-- Constraints: minimum 20; maximum 50.
describeSnapshots_maxRecords :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Int)
describeSnapshots_maxRecords = Lens.lens (\DescribeSnapshots' {maxRecords} -> maxRecords) (\s@DescribeSnapshots' {} a -> s {maxRecords = a} :: DescribeSnapshots)

-- | A user-supplied replication group identifier. If this parameter is
-- specified, only snapshots associated with that specific replication
-- group are described.
describeSnapshots_replicationGroupId :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_replicationGroupId = Lens.lens (\DescribeSnapshots' {replicationGroupId} -> replicationGroupId) (\s@DescribeSnapshots' {} a -> s {replicationGroupId = a} :: DescribeSnapshots)

-- | A Boolean value which if true, the node group (shard) configuration is
-- included in the snapshot description.
describeSnapshots_showNodeGroupConfig :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Bool)
describeSnapshots_showNodeGroupConfig = Lens.lens (\DescribeSnapshots' {showNodeGroupConfig} -> showNodeGroupConfig) (\s@DescribeSnapshots' {} a -> s {showNodeGroupConfig = a} :: DescribeSnapshots)

-- | A user-supplied name of the snapshot. If this parameter is specified,
-- only this snapshot are described.
describeSnapshots_snapshotName :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_snapshotName = Lens.lens (\DescribeSnapshots' {snapshotName} -> snapshotName) (\s@DescribeSnapshots' {} a -> s {snapshotName = a} :: DescribeSnapshots)

-- | If set to @system@, the output shows snapshots that were automatically
-- created by ElastiCache. If set to @user@ the output shows snapshots that
-- were manually created. If omitted, the output shows both automatically
-- and manually created snapshots.
describeSnapshots_snapshotSource :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_snapshotSource = Lens.lens (\DescribeSnapshots' {snapshotSource} -> snapshotSource) (\s@DescribeSnapshots' {} a -> s {snapshotSource = a} :: DescribeSnapshots)

instance Core.AWSPager DescribeSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSnapshotsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSnapshotsResponse_snapshots
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeSnapshots_marker
              Lens..~ rs
              Lens.^? describeSnapshotsResponse_marker
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeSnapshots where
  type
    AWSResponse DescribeSnapshots =
      DescribeSnapshotsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeSnapshotsResult"
      ( \s h x ->
          DescribeSnapshotsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "Snapshots" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "Snapshot")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshots where
  hashWithSalt _salt DescribeSnapshots' {..} =
    _salt
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` showNodeGroupConfig
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` snapshotSource

instance Prelude.NFData DescribeSnapshots where
  rnf DescribeSnapshots' {..} =
    Prelude.rnf cacheClusterId `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf maxRecords `Prelude.seq`
          Prelude.rnf replicationGroupId `Prelude.seq`
            Prelude.rnf showNodeGroupConfig `Prelude.seq`
              Prelude.rnf snapshotName `Prelude.seq`
                Prelude.rnf snapshotSource

instance Data.ToHeaders DescribeSnapshots where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSnapshots where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSnapshots where
  toQuery DescribeSnapshots' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeSnapshots" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheClusterId" Data.=: cacheClusterId,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "ReplicationGroupId" Data.=: replicationGroupId,
        "ShowNodeGroupConfig" Data.=: showNodeGroupConfig,
        "SnapshotName" Data.=: snapshotName,
        "SnapshotSource" Data.=: snapshotSource
      ]

-- | Represents the output of a @DescribeSnapshots@ operation.
--
-- /See:/ 'newDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of snapshots. Each item in the list contains detailed information
    -- about one snapshot.
    snapshots :: Prelude.Maybe [Snapshot],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeSnapshotsResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'snapshots', 'describeSnapshotsResponse_snapshots' - A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
--
-- 'httpStatus', 'describeSnapshotsResponse_httpStatus' - The response's http status code.
newDescribeSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSnapshotsResponse
newDescribeSnapshotsResponse pHttpStatus_ =
  DescribeSnapshotsResponse'
    { marker =
        Prelude.Nothing,
      snapshots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeSnapshotsResponse_marker :: Lens.Lens' DescribeSnapshotsResponse (Prelude.Maybe Prelude.Text)
describeSnapshotsResponse_marker = Lens.lens (\DescribeSnapshotsResponse' {marker} -> marker) (\s@DescribeSnapshotsResponse' {} a -> s {marker = a} :: DescribeSnapshotsResponse)

-- | A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
describeSnapshotsResponse_snapshots :: Lens.Lens' DescribeSnapshotsResponse (Prelude.Maybe [Snapshot])
describeSnapshotsResponse_snapshots = Lens.lens (\DescribeSnapshotsResponse' {snapshots} -> snapshots) (\s@DescribeSnapshotsResponse' {} a -> s {snapshots = a} :: DescribeSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSnapshotsResponse_httpStatus :: Lens.Lens' DescribeSnapshotsResponse Prelude.Int
describeSnapshotsResponse_httpStatus = Lens.lens (\DescribeSnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotsResponse)

instance Prelude.NFData DescribeSnapshotsResponse where
  rnf DescribeSnapshotsResponse' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf snapshots `Prelude.seq`
        Prelude.rnf httpStatus

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
-- Module      : Network.AWS.Redshift.DescribeClusterSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns one or more snapshot objects, which contain metadata about your
-- cluster snapshots. By default, this operation returns information about
-- all snapshots of all clusters that are owned by you AWS customer
-- account. No information is returned for snapshots owned by inactive AWS
-- customer accounts.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all snapshots that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- snapshots that have any combination of those values are returned. Only
-- snapshots that you own are returned in the response; shared snapshots
-- are not returned with the tag key and tag value request parameters.
--
-- If both tag keys and values are omitted from the request, snapshots are
-- returned regardless of whether they have tag keys or values associated
-- with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterSnapshots
  ( -- * Creating a Request
    DescribeClusterSnapshots (..),
    newDescribeClusterSnapshots,

    -- * Request Lenses
    describeClusterSnapshots_snapshotIdentifier,
    describeClusterSnapshots_sortingEntities,
    describeClusterSnapshots_tagKeys,
    describeClusterSnapshots_startTime,
    describeClusterSnapshots_endTime,
    describeClusterSnapshots_snapshotType,
    describeClusterSnapshots_clusterIdentifier,
    describeClusterSnapshots_ownerAccount,
    describeClusterSnapshots_clusterExists,
    describeClusterSnapshots_tagValues,
    describeClusterSnapshots_marker,
    describeClusterSnapshots_maxRecords,

    -- * Destructuring the Response
    DescribeClusterSnapshotsResponse (..),
    newDescribeClusterSnapshotsResponse,

    -- * Response Lenses
    describeClusterSnapshotsResponse_snapshots,
    describeClusterSnapshotsResponse_marker,
    describeClusterSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeClusterSnapshots' smart constructor.
data DescribeClusterSnapshots = DescribeClusterSnapshots'
  { -- | The snapshot identifier of the snapshot about which to return
    -- information.
    snapshotIdentifier :: Core.Maybe Core.Text,
    sortingEntities :: Core.Maybe [SnapshotSortingEntity],
    -- | A tag key or keys for which you want to return all matching cluster
    -- snapshots that are associated with the specified key or keys. For
    -- example, suppose that you have snapshots that are tagged with keys
    -- called @owner@ and @environment@. If you specify both of these tag keys
    -- in the request, Amazon Redshift returns a response with the snapshots
    -- that have either or both of these tag keys associated with them.
    tagKeys :: Core.Maybe [Core.Text],
    -- | A value that requests only snapshots created at or after the specified
    -- time. The time value is specified in ISO 8601 format. For more
    -- information about ISO 8601, go to the
    -- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
    --
    -- Example: @2012-07-16T18:00:00Z@
    startTime :: Core.Maybe Core.ISO8601,
    -- | A time value that requests only snapshots created at or before the
    -- specified time. The time value is specified in ISO 8601 format. For more
    -- information about ISO 8601, go to the
    -- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
    --
    -- Example: @2012-07-16T18:00:00Z@
    endTime :: Core.Maybe Core.ISO8601,
    -- | The type of snapshots for which you are requesting information. By
    -- default, snapshots of all types are returned.
    --
    -- Valid Values: @automated@ | @manual@
    snapshotType :: Core.Maybe Core.Text,
    -- | The identifier of the cluster which generated the requested snapshots.
    clusterIdentifier :: Core.Maybe Core.Text,
    -- | The AWS customer account used to create or copy the snapshot. Use this
    -- field to filter the results to snapshots owned by a particular account.
    -- To describe snapshots you own, either specify your AWS customer account,
    -- or do not specify the parameter.
    ownerAccount :: Core.Maybe Core.Text,
    -- | A value that indicates whether to return snapshots only for an existing
    -- cluster. You can perform table-level restore only by using a snapshot of
    -- an existing cluster, that is, a cluster that has not been deleted.
    -- Values for this parameter work as follows:
    --
    -- -   If @ClusterExists@ is set to @true@, @ClusterIdentifier@ is
    --     required.
    --
    -- -   If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ isn\'t
    --     specified, all snapshots associated with deleted clusters (orphaned
    --     snapshots) are returned.
    --
    -- -   If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ is
    --     specified for a deleted cluster, snapshots associated with that
    --     cluster are returned.
    --
    -- -   If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ is
    --     specified for an existing cluster, no snapshots are returned.
    clusterExists :: Core.Maybe Core.Bool,
    -- | A tag value or values for which you want to return all matching cluster
    -- snapshots that are associated with the specified tag value or values.
    -- For example, suppose that you have snapshots that are tagged with values
    -- called @admin@ and @test@. If you specify both of these tag values in
    -- the request, Amazon Redshift returns a response with the snapshots that
    -- have either or both of these tag values associated with them.
    tagValues :: Core.Maybe [Core.Text],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeClusterSnapshots
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClusterSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotIdentifier', 'describeClusterSnapshots_snapshotIdentifier' - The snapshot identifier of the snapshot about which to return
-- information.
--
-- 'sortingEntities', 'describeClusterSnapshots_sortingEntities' -
--
-- 'tagKeys', 'describeClusterSnapshots_tagKeys' - A tag key or keys for which you want to return all matching cluster
-- snapshots that are associated with the specified key or keys. For
-- example, suppose that you have snapshots that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the snapshots
-- that have either or both of these tag keys associated with them.
--
-- 'startTime', 'describeClusterSnapshots_startTime' - A value that requests only snapshots created at or after the specified
-- time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
--
-- 'endTime', 'describeClusterSnapshots_endTime' - A time value that requests only snapshots created at or before the
-- specified time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
--
-- 'snapshotType', 'describeClusterSnapshots_snapshotType' - The type of snapshots for which you are requesting information. By
-- default, snapshots of all types are returned.
--
-- Valid Values: @automated@ | @manual@
--
-- 'clusterIdentifier', 'describeClusterSnapshots_clusterIdentifier' - The identifier of the cluster which generated the requested snapshots.
--
-- 'ownerAccount', 'describeClusterSnapshots_ownerAccount' - The AWS customer account used to create or copy the snapshot. Use this
-- field to filter the results to snapshots owned by a particular account.
-- To describe snapshots you own, either specify your AWS customer account,
-- or do not specify the parameter.
--
-- 'clusterExists', 'describeClusterSnapshots_clusterExists' - A value that indicates whether to return snapshots only for an existing
-- cluster. You can perform table-level restore only by using a snapshot of
-- an existing cluster, that is, a cluster that has not been deleted.
-- Values for this parameter work as follows:
--
-- -   If @ClusterExists@ is set to @true@, @ClusterIdentifier@ is
--     required.
--
-- -   If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ isn\'t
--     specified, all snapshots associated with deleted clusters (orphaned
--     snapshots) are returned.
--
-- -   If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ is
--     specified for a deleted cluster, snapshots associated with that
--     cluster are returned.
--
-- -   If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ is
--     specified for an existing cluster, no snapshots are returned.
--
-- 'tagValues', 'describeClusterSnapshots_tagValues' - A tag value or values for which you want to return all matching cluster
-- snapshots that are associated with the specified tag value or values.
-- For example, suppose that you have snapshots that are tagged with values
-- called @admin@ and @test@. If you specify both of these tag values in
-- the request, Amazon Redshift returns a response with the snapshots that
-- have either or both of these tag values associated with them.
--
-- 'marker', 'describeClusterSnapshots_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSnapshots
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- 'maxRecords', 'describeClusterSnapshots_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeClusterSnapshots ::
  DescribeClusterSnapshots
newDescribeClusterSnapshots =
  DescribeClusterSnapshots'
    { snapshotIdentifier =
        Core.Nothing,
      sortingEntities = Core.Nothing,
      tagKeys = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      snapshotType = Core.Nothing,
      clusterIdentifier = Core.Nothing,
      ownerAccount = Core.Nothing,
      clusterExists = Core.Nothing,
      tagValues = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The snapshot identifier of the snapshot about which to return
-- information.
describeClusterSnapshots_snapshotIdentifier :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.Text)
describeClusterSnapshots_snapshotIdentifier = Lens.lens (\DescribeClusterSnapshots' {snapshotIdentifier} -> snapshotIdentifier) (\s@DescribeClusterSnapshots' {} a -> s {snapshotIdentifier = a} :: DescribeClusterSnapshots)

-- |
describeClusterSnapshots_sortingEntities :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe [SnapshotSortingEntity])
describeClusterSnapshots_sortingEntities = Lens.lens (\DescribeClusterSnapshots' {sortingEntities} -> sortingEntities) (\s@DescribeClusterSnapshots' {} a -> s {sortingEntities = a} :: DescribeClusterSnapshots) Core.. Lens.mapping Lens._Coerce

-- | A tag key or keys for which you want to return all matching cluster
-- snapshots that are associated with the specified key or keys. For
-- example, suppose that you have snapshots that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the snapshots
-- that have either or both of these tag keys associated with them.
describeClusterSnapshots_tagKeys :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe [Core.Text])
describeClusterSnapshots_tagKeys = Lens.lens (\DescribeClusterSnapshots' {tagKeys} -> tagKeys) (\s@DescribeClusterSnapshots' {} a -> s {tagKeys = a} :: DescribeClusterSnapshots) Core.. Lens.mapping Lens._Coerce

-- | A value that requests only snapshots created at or after the specified
-- time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
describeClusterSnapshots_startTime :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.UTCTime)
describeClusterSnapshots_startTime = Lens.lens (\DescribeClusterSnapshots' {startTime} -> startTime) (\s@DescribeClusterSnapshots' {} a -> s {startTime = a} :: DescribeClusterSnapshots) Core.. Lens.mapping Core._Time

-- | A time value that requests only snapshots created at or before the
-- specified time. The time value is specified in ISO 8601 format. For more
-- information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
describeClusterSnapshots_endTime :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.UTCTime)
describeClusterSnapshots_endTime = Lens.lens (\DescribeClusterSnapshots' {endTime} -> endTime) (\s@DescribeClusterSnapshots' {} a -> s {endTime = a} :: DescribeClusterSnapshots) Core.. Lens.mapping Core._Time

-- | The type of snapshots for which you are requesting information. By
-- default, snapshots of all types are returned.
--
-- Valid Values: @automated@ | @manual@
describeClusterSnapshots_snapshotType :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.Text)
describeClusterSnapshots_snapshotType = Lens.lens (\DescribeClusterSnapshots' {snapshotType} -> snapshotType) (\s@DescribeClusterSnapshots' {} a -> s {snapshotType = a} :: DescribeClusterSnapshots)

-- | The identifier of the cluster which generated the requested snapshots.
describeClusterSnapshots_clusterIdentifier :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.Text)
describeClusterSnapshots_clusterIdentifier = Lens.lens (\DescribeClusterSnapshots' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeClusterSnapshots' {} a -> s {clusterIdentifier = a} :: DescribeClusterSnapshots)

-- | The AWS customer account used to create or copy the snapshot. Use this
-- field to filter the results to snapshots owned by a particular account.
-- To describe snapshots you own, either specify your AWS customer account,
-- or do not specify the parameter.
describeClusterSnapshots_ownerAccount :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.Text)
describeClusterSnapshots_ownerAccount = Lens.lens (\DescribeClusterSnapshots' {ownerAccount} -> ownerAccount) (\s@DescribeClusterSnapshots' {} a -> s {ownerAccount = a} :: DescribeClusterSnapshots)

-- | A value that indicates whether to return snapshots only for an existing
-- cluster. You can perform table-level restore only by using a snapshot of
-- an existing cluster, that is, a cluster that has not been deleted.
-- Values for this parameter work as follows:
--
-- -   If @ClusterExists@ is set to @true@, @ClusterIdentifier@ is
--     required.
--
-- -   If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ isn\'t
--     specified, all snapshots associated with deleted clusters (orphaned
--     snapshots) are returned.
--
-- -   If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ is
--     specified for a deleted cluster, snapshots associated with that
--     cluster are returned.
--
-- -   If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ is
--     specified for an existing cluster, no snapshots are returned.
describeClusterSnapshots_clusterExists :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.Bool)
describeClusterSnapshots_clusterExists = Lens.lens (\DescribeClusterSnapshots' {clusterExists} -> clusterExists) (\s@DescribeClusterSnapshots' {} a -> s {clusterExists = a} :: DescribeClusterSnapshots)

-- | A tag value or values for which you want to return all matching cluster
-- snapshots that are associated with the specified tag value or values.
-- For example, suppose that you have snapshots that are tagged with values
-- called @admin@ and @test@. If you specify both of these tag values in
-- the request, Amazon Redshift returns a response with the snapshots that
-- have either or both of these tag values associated with them.
describeClusterSnapshots_tagValues :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe [Core.Text])
describeClusterSnapshots_tagValues = Lens.lens (\DescribeClusterSnapshots' {tagValues} -> tagValues) (\s@DescribeClusterSnapshots' {} a -> s {tagValues = a} :: DescribeClusterSnapshots) Core.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSnapshots
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeClusterSnapshots_marker :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.Text)
describeClusterSnapshots_marker = Lens.lens (\DescribeClusterSnapshots' {marker} -> marker) (\s@DescribeClusterSnapshots' {} a -> s {marker = a} :: DescribeClusterSnapshots)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeClusterSnapshots_maxRecords :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.Int)
describeClusterSnapshots_maxRecords = Lens.lens (\DescribeClusterSnapshots' {maxRecords} -> maxRecords) (\s@DescribeClusterSnapshots' {} a -> s {maxRecords = a} :: DescribeClusterSnapshots)

instance Core.AWSPager DescribeClusterSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClusterSnapshotsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClusterSnapshotsResponse_snapshots
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeClusterSnapshots_marker
          Lens..~ rs
          Lens.^? describeClusterSnapshotsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeClusterSnapshots where
  type
    AWSResponse DescribeClusterSnapshots =
      DescribeClusterSnapshotsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeClusterSnapshotsResult"
      ( \s h x ->
          DescribeClusterSnapshotsResponse'
            Core.<$> ( x Core..@? "Snapshots" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "Snapshot")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeClusterSnapshots

instance Core.NFData DescribeClusterSnapshots

instance Core.ToHeaders DescribeClusterSnapshots where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeClusterSnapshots where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClusterSnapshots where
  toQuery DescribeClusterSnapshots' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeClusterSnapshots" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "SnapshotIdentifier" Core.=: snapshotIdentifier,
        "SortingEntities"
          Core.=: Core.toQuery
            ( Core.toQueryList "SnapshotSortingEntity"
                Core.<$> sortingEntities
            ),
        "TagKeys"
          Core.=: Core.toQuery
            (Core.toQueryList "TagKey" Core.<$> tagKeys),
        "StartTime" Core.=: startTime,
        "EndTime" Core.=: endTime,
        "SnapshotType" Core.=: snapshotType,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "OwnerAccount" Core.=: ownerAccount,
        "ClusterExists" Core.=: clusterExists,
        "TagValues"
          Core.=: Core.toQuery
            (Core.toQueryList "TagValue" Core.<$> tagValues),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the output from the DescribeClusterSnapshots action.
--
-- /See:/ 'newDescribeClusterSnapshotsResponse' smart constructor.
data DescribeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse'
  { -- | A list of Snapshot instances.
    snapshots :: Core.Maybe [Snapshot],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClusterSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshots', 'describeClusterSnapshotsResponse_snapshots' - A list of Snapshot instances.
--
-- 'marker', 'describeClusterSnapshotsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeClusterSnapshotsResponse_httpStatus' - The response's http status code.
newDescribeClusterSnapshotsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeClusterSnapshotsResponse
newDescribeClusterSnapshotsResponse pHttpStatus_ =
  DescribeClusterSnapshotsResponse'
    { snapshots =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Snapshot instances.
describeClusterSnapshotsResponse_snapshots :: Lens.Lens' DescribeClusterSnapshotsResponse (Core.Maybe [Snapshot])
describeClusterSnapshotsResponse_snapshots = Lens.lens (\DescribeClusterSnapshotsResponse' {snapshots} -> snapshots) (\s@DescribeClusterSnapshotsResponse' {} a -> s {snapshots = a} :: DescribeClusterSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeClusterSnapshotsResponse_marker :: Lens.Lens' DescribeClusterSnapshotsResponse (Core.Maybe Core.Text)
describeClusterSnapshotsResponse_marker = Lens.lens (\DescribeClusterSnapshotsResponse' {marker} -> marker) (\s@DescribeClusterSnapshotsResponse' {} a -> s {marker = a} :: DescribeClusterSnapshotsResponse)

-- | The response's http status code.
describeClusterSnapshotsResponse_httpStatus :: Lens.Lens' DescribeClusterSnapshotsResponse Core.Int
describeClusterSnapshotsResponse_httpStatus = Lens.lens (\DescribeClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeClusterSnapshotsResponse)

instance Core.NFData DescribeClusterSnapshotsResponse

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
-- Module      : Network.AWS.Redshift.DescribeSnapshotSchedules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of snapshot schedules.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeSnapshotSchedules
  ( -- * Creating a Request
    DescribeSnapshotSchedules (..),
    newDescribeSnapshotSchedules,

    -- * Request Lenses
    describeSnapshotSchedules_tagKeys,
    describeSnapshotSchedules_scheduleIdentifier,
    describeSnapshotSchedules_clusterIdentifier,
    describeSnapshotSchedules_tagValues,
    describeSnapshotSchedules_marker,
    describeSnapshotSchedules_maxRecords,

    -- * Destructuring the Response
    DescribeSnapshotSchedulesResponse (..),
    newDescribeSnapshotSchedulesResponse,

    -- * Response Lenses
    describeSnapshotSchedulesResponse_snapshotSchedules,
    describeSnapshotSchedulesResponse_marker,
    describeSnapshotSchedulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSnapshotSchedules' smart constructor.
data DescribeSnapshotSchedules = DescribeSnapshotSchedules'
  { -- | The key value for a snapshot schedule tag.
    tagKeys :: Core.Maybe [Core.Text],
    -- | A unique identifier for a snapshot schedule.
    scheduleIdentifier :: Core.Maybe Core.Text,
    -- | The unique identifier for the cluster whose snapshot schedules you want
    -- to view.
    clusterIdentifier :: Core.Maybe Core.Text,
    -- | The value corresponding to the key of the snapshot schedule tag.
    tagValues :: Core.Maybe [Core.Text],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @marker@ parameter and retrying the command. If the
    -- @marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number or response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned @marker@ value.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSnapshotSchedules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeys', 'describeSnapshotSchedules_tagKeys' - The key value for a snapshot schedule tag.
--
-- 'scheduleIdentifier', 'describeSnapshotSchedules_scheduleIdentifier' - A unique identifier for a snapshot schedule.
--
-- 'clusterIdentifier', 'describeSnapshotSchedules_clusterIdentifier' - The unique identifier for the cluster whose snapshot schedules you want
-- to view.
--
-- 'tagValues', 'describeSnapshotSchedules_tagValues' - The value corresponding to the key of the snapshot schedule tag.
--
-- 'marker', 'describeSnapshotSchedules_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'maxRecords', 'describeSnapshotSchedules_maxRecords' - The maximum number or response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned @marker@ value.
newDescribeSnapshotSchedules ::
  DescribeSnapshotSchedules
newDescribeSnapshotSchedules =
  DescribeSnapshotSchedules'
    { tagKeys = Core.Nothing,
      scheduleIdentifier = Core.Nothing,
      clusterIdentifier = Core.Nothing,
      tagValues = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The key value for a snapshot schedule tag.
describeSnapshotSchedules_tagKeys :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe [Core.Text])
describeSnapshotSchedules_tagKeys = Lens.lens (\DescribeSnapshotSchedules' {tagKeys} -> tagKeys) (\s@DescribeSnapshotSchedules' {} a -> s {tagKeys = a} :: DescribeSnapshotSchedules) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for a snapshot schedule.
describeSnapshotSchedules_scheduleIdentifier :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe Core.Text)
describeSnapshotSchedules_scheduleIdentifier = Lens.lens (\DescribeSnapshotSchedules' {scheduleIdentifier} -> scheduleIdentifier) (\s@DescribeSnapshotSchedules' {} a -> s {scheduleIdentifier = a} :: DescribeSnapshotSchedules)

-- | The unique identifier for the cluster whose snapshot schedules you want
-- to view.
describeSnapshotSchedules_clusterIdentifier :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe Core.Text)
describeSnapshotSchedules_clusterIdentifier = Lens.lens (\DescribeSnapshotSchedules' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeSnapshotSchedules' {} a -> s {clusterIdentifier = a} :: DescribeSnapshotSchedules)

-- | The value corresponding to the key of the snapshot schedule tag.
describeSnapshotSchedules_tagValues :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe [Core.Text])
describeSnapshotSchedules_tagValues = Lens.lens (\DescribeSnapshotSchedules' {tagValues} -> tagValues) (\s@DescribeSnapshotSchedules' {} a -> s {tagValues = a} :: DescribeSnapshotSchedules) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
describeSnapshotSchedules_marker :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe Core.Text)
describeSnapshotSchedules_marker = Lens.lens (\DescribeSnapshotSchedules' {marker} -> marker) (\s@DescribeSnapshotSchedules' {} a -> s {marker = a} :: DescribeSnapshotSchedules)

-- | The maximum number or response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned @marker@ value.
describeSnapshotSchedules_maxRecords :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe Core.Int)
describeSnapshotSchedules_maxRecords = Lens.lens (\DescribeSnapshotSchedules' {maxRecords} -> maxRecords) (\s@DescribeSnapshotSchedules' {} a -> s {maxRecords = a} :: DescribeSnapshotSchedules)

instance Core.AWSPager DescribeSnapshotSchedules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSnapshotSchedulesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSnapshotSchedulesResponse_snapshotSchedules
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSnapshotSchedules_marker
          Lens..~ rs
          Lens.^? describeSnapshotSchedulesResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeSnapshotSchedules where
  type
    AWSResponse DescribeSnapshotSchedules =
      DescribeSnapshotSchedulesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeSnapshotSchedulesResult"
      ( \s h x ->
          DescribeSnapshotSchedulesResponse'
            Core.<$> ( x Core..@? "SnapshotSchedules" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "SnapshotSchedule")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSnapshotSchedules

instance Core.NFData DescribeSnapshotSchedules

instance Core.ToHeaders DescribeSnapshotSchedules where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSnapshotSchedules where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSnapshotSchedules where
  toQuery DescribeSnapshotSchedules' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeSnapshotSchedules" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "TagKeys"
          Core.=: Core.toQuery
            (Core.toQueryList "TagKey" Core.<$> tagKeys),
        "ScheduleIdentifier" Core.=: scheduleIdentifier,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "TagValues"
          Core.=: Core.toQuery
            (Core.toQueryList "TagValue" Core.<$> tagValues),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeSnapshotSchedulesResponse' smart constructor.
data DescribeSnapshotSchedulesResponse = DescribeSnapshotSchedulesResponse'
  { -- | A list of SnapshotSchedules.
    snapshotSchedules :: Core.Maybe [SnapshotSchedule],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @marker@ parameter and retrying the command. If the
    -- @marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSnapshotSchedulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotSchedules', 'describeSnapshotSchedulesResponse_snapshotSchedules' - A list of SnapshotSchedules.
--
-- 'marker', 'describeSnapshotSchedulesResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeSnapshotSchedulesResponse_httpStatus' - The response's http status code.
newDescribeSnapshotSchedulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSnapshotSchedulesResponse
newDescribeSnapshotSchedulesResponse pHttpStatus_ =
  DescribeSnapshotSchedulesResponse'
    { snapshotSchedules =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of SnapshotSchedules.
describeSnapshotSchedulesResponse_snapshotSchedules :: Lens.Lens' DescribeSnapshotSchedulesResponse (Core.Maybe [SnapshotSchedule])
describeSnapshotSchedulesResponse_snapshotSchedules = Lens.lens (\DescribeSnapshotSchedulesResponse' {snapshotSchedules} -> snapshotSchedules) (\s@DescribeSnapshotSchedulesResponse' {} a -> s {snapshotSchedules = a} :: DescribeSnapshotSchedulesResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
describeSnapshotSchedulesResponse_marker :: Lens.Lens' DescribeSnapshotSchedulesResponse (Core.Maybe Core.Text)
describeSnapshotSchedulesResponse_marker = Lens.lens (\DescribeSnapshotSchedulesResponse' {marker} -> marker) (\s@DescribeSnapshotSchedulesResponse' {} a -> s {marker = a} :: DescribeSnapshotSchedulesResponse)

-- | The response's http status code.
describeSnapshotSchedulesResponse_httpStatus :: Lens.Lens' DescribeSnapshotSchedulesResponse Core.Int
describeSnapshotSchedulesResponse_httpStatus = Lens.lens (\DescribeSnapshotSchedulesResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotSchedulesResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotSchedulesResponse)

instance
  Core.NFData
    DescribeSnapshotSchedulesResponse

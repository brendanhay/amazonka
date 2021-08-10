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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSnapshotSchedules' smart constructor.
data DescribeSnapshotSchedules = DescribeSnapshotSchedules'
  { -- | The key value for a snapshot schedule tag.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier for a snapshot schedule.
    scheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the cluster whose snapshot schedules you want
    -- to view.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The value corresponding to the key of the snapshot schedule tag.
    tagValues :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @marker@ parameter and retrying the command. If the
    -- @marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number or response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned @marker@ value.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { tagKeys =
        Prelude.Nothing,
      scheduleIdentifier = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      tagValues = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The key value for a snapshot schedule tag.
describeSnapshotSchedules_tagKeys :: Lens.Lens' DescribeSnapshotSchedules (Prelude.Maybe [Prelude.Text])
describeSnapshotSchedules_tagKeys = Lens.lens (\DescribeSnapshotSchedules' {tagKeys} -> tagKeys) (\s@DescribeSnapshotSchedules' {} a -> s {tagKeys = a} :: DescribeSnapshotSchedules) Prelude.. Lens.mapping Lens._Coerce

-- | A unique identifier for a snapshot schedule.
describeSnapshotSchedules_scheduleIdentifier :: Lens.Lens' DescribeSnapshotSchedules (Prelude.Maybe Prelude.Text)
describeSnapshotSchedules_scheduleIdentifier = Lens.lens (\DescribeSnapshotSchedules' {scheduleIdentifier} -> scheduleIdentifier) (\s@DescribeSnapshotSchedules' {} a -> s {scheduleIdentifier = a} :: DescribeSnapshotSchedules)

-- | The unique identifier for the cluster whose snapshot schedules you want
-- to view.
describeSnapshotSchedules_clusterIdentifier :: Lens.Lens' DescribeSnapshotSchedules (Prelude.Maybe Prelude.Text)
describeSnapshotSchedules_clusterIdentifier = Lens.lens (\DescribeSnapshotSchedules' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeSnapshotSchedules' {} a -> s {clusterIdentifier = a} :: DescribeSnapshotSchedules)

-- | The value corresponding to the key of the snapshot schedule tag.
describeSnapshotSchedules_tagValues :: Lens.Lens' DescribeSnapshotSchedules (Prelude.Maybe [Prelude.Text])
describeSnapshotSchedules_tagValues = Lens.lens (\DescribeSnapshotSchedules' {tagValues} -> tagValues) (\s@DescribeSnapshotSchedules' {} a -> s {tagValues = a} :: DescribeSnapshotSchedules) Prelude.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
describeSnapshotSchedules_marker :: Lens.Lens' DescribeSnapshotSchedules (Prelude.Maybe Prelude.Text)
describeSnapshotSchedules_marker = Lens.lens (\DescribeSnapshotSchedules' {marker} -> marker) (\s@DescribeSnapshotSchedules' {} a -> s {marker = a} :: DescribeSnapshotSchedules)

-- | The maximum number or response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned @marker@ value.
describeSnapshotSchedules_maxRecords :: Lens.Lens' DescribeSnapshotSchedules (Prelude.Maybe Prelude.Int)
describeSnapshotSchedules_maxRecords = Lens.lens (\DescribeSnapshotSchedules' {maxRecords} -> maxRecords) (\s@DescribeSnapshotSchedules' {} a -> s {maxRecords = a} :: DescribeSnapshotSchedules)

instance Core.AWSPager DescribeSnapshotSchedules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSnapshotSchedulesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSnapshotSchedulesResponse_snapshotSchedules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSnapshotSchedules_marker
          Lens..~ rs
          Lens.^? describeSnapshotSchedulesResponse_marker
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..@? "SnapshotSchedules"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "SnapshotSchedule")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshotSchedules

instance Prelude.NFData DescribeSnapshotSchedules

instance Core.ToHeaders DescribeSnapshotSchedules where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeSnapshotSchedules where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSnapshotSchedules where
  toQuery DescribeSnapshotSchedules' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeSnapshotSchedules" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "TagKeys"
          Core.=: Core.toQuery
            (Core.toQueryList "TagKey" Prelude.<$> tagKeys),
        "ScheduleIdentifier" Core.=: scheduleIdentifier,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "TagValues"
          Core.=: Core.toQuery
            (Core.toQueryList "TagValue" Prelude.<$> tagValues),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeSnapshotSchedulesResponse' smart constructor.
data DescribeSnapshotSchedulesResponse = DescribeSnapshotSchedulesResponse'
  { -- | A list of SnapshotSchedules.
    snapshotSchedules :: Prelude.Maybe [SnapshotSchedule],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @marker@ parameter and retrying the command. If the
    -- @marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeSnapshotSchedulesResponse
newDescribeSnapshotSchedulesResponse pHttpStatus_ =
  DescribeSnapshotSchedulesResponse'
    { snapshotSchedules =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of SnapshotSchedules.
describeSnapshotSchedulesResponse_snapshotSchedules :: Lens.Lens' DescribeSnapshotSchedulesResponse (Prelude.Maybe [SnapshotSchedule])
describeSnapshotSchedulesResponse_snapshotSchedules = Lens.lens (\DescribeSnapshotSchedulesResponse' {snapshotSchedules} -> snapshotSchedules) (\s@DescribeSnapshotSchedulesResponse' {} a -> s {snapshotSchedules = a} :: DescribeSnapshotSchedulesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
describeSnapshotSchedulesResponse_marker :: Lens.Lens' DescribeSnapshotSchedulesResponse (Prelude.Maybe Prelude.Text)
describeSnapshotSchedulesResponse_marker = Lens.lens (\DescribeSnapshotSchedulesResponse' {marker} -> marker) (\s@DescribeSnapshotSchedulesResponse' {} a -> s {marker = a} :: DescribeSnapshotSchedulesResponse)

-- | The response's http status code.
describeSnapshotSchedulesResponse_httpStatus :: Lens.Lens' DescribeSnapshotSchedulesResponse Prelude.Int
describeSnapshotSchedulesResponse_httpStatus = Lens.lens (\DescribeSnapshotSchedulesResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotSchedulesResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotSchedulesResponse)

instance
  Prelude.NFData
    DescribeSnapshotSchedulesResponse

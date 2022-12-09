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
-- Module      : Amazonka.Redshift.DescribeSnapshotSchedules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of snapshot schedules.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeSnapshotSchedules
  ( -- * Creating a Request
    DescribeSnapshotSchedules (..),
    newDescribeSnapshotSchedules,

    -- * Request Lenses
    describeSnapshotSchedules_clusterIdentifier,
    describeSnapshotSchedules_marker,
    describeSnapshotSchedules_maxRecords,
    describeSnapshotSchedules_scheduleIdentifier,
    describeSnapshotSchedules_tagKeys,
    describeSnapshotSchedules_tagValues,

    -- * Destructuring the Response
    DescribeSnapshotSchedulesResponse (..),
    newDescribeSnapshotSchedulesResponse,

    -- * Response Lenses
    describeSnapshotSchedulesResponse_marker,
    describeSnapshotSchedulesResponse_snapshotSchedules,
    describeSnapshotSchedulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSnapshotSchedules' smart constructor.
data DescribeSnapshotSchedules = DescribeSnapshotSchedules'
  { -- | The unique identifier for the cluster whose snapshot schedules you want
    -- to view.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
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
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | A unique identifier for a snapshot schedule.
    scheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The key value for a snapshot schedule tag.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | The value corresponding to the key of the snapshot schedule tag.
    tagValues :: Prelude.Maybe [Prelude.Text]
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
-- 'clusterIdentifier', 'describeSnapshotSchedules_clusterIdentifier' - The unique identifier for the cluster whose snapshot schedules you want
-- to view.
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
--
-- 'scheduleIdentifier', 'describeSnapshotSchedules_scheduleIdentifier' - A unique identifier for a snapshot schedule.
--
-- 'tagKeys', 'describeSnapshotSchedules_tagKeys' - The key value for a snapshot schedule tag.
--
-- 'tagValues', 'describeSnapshotSchedules_tagValues' - The value corresponding to the key of the snapshot schedule tag.
newDescribeSnapshotSchedules ::
  DescribeSnapshotSchedules
newDescribeSnapshotSchedules =
  DescribeSnapshotSchedules'
    { clusterIdentifier =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      scheduleIdentifier = Prelude.Nothing,
      tagKeys = Prelude.Nothing,
      tagValues = Prelude.Nothing
    }

-- | The unique identifier for the cluster whose snapshot schedules you want
-- to view.
describeSnapshotSchedules_clusterIdentifier :: Lens.Lens' DescribeSnapshotSchedules (Prelude.Maybe Prelude.Text)
describeSnapshotSchedules_clusterIdentifier = Lens.lens (\DescribeSnapshotSchedules' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeSnapshotSchedules' {} a -> s {clusterIdentifier = a} :: DescribeSnapshotSchedules)

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

-- | A unique identifier for a snapshot schedule.
describeSnapshotSchedules_scheduleIdentifier :: Lens.Lens' DescribeSnapshotSchedules (Prelude.Maybe Prelude.Text)
describeSnapshotSchedules_scheduleIdentifier = Lens.lens (\DescribeSnapshotSchedules' {scheduleIdentifier} -> scheduleIdentifier) (\s@DescribeSnapshotSchedules' {} a -> s {scheduleIdentifier = a} :: DescribeSnapshotSchedules)

-- | The key value for a snapshot schedule tag.
describeSnapshotSchedules_tagKeys :: Lens.Lens' DescribeSnapshotSchedules (Prelude.Maybe [Prelude.Text])
describeSnapshotSchedules_tagKeys = Lens.lens (\DescribeSnapshotSchedules' {tagKeys} -> tagKeys) (\s@DescribeSnapshotSchedules' {} a -> s {tagKeys = a} :: DescribeSnapshotSchedules) Prelude.. Lens.mapping Lens.coerced

-- | The value corresponding to the key of the snapshot schedule tag.
describeSnapshotSchedules_tagValues :: Lens.Lens' DescribeSnapshotSchedules (Prelude.Maybe [Prelude.Text])
describeSnapshotSchedules_tagValues = Lens.lens (\DescribeSnapshotSchedules' {tagValues} -> tagValues) (\s@DescribeSnapshotSchedules' {} a -> s {tagValues = a} :: DescribeSnapshotSchedules) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeSnapshotSchedulesResult"
      ( \s h x ->
          DescribeSnapshotSchedulesResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "SnapshotSchedules"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "SnapshotSchedule")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshotSchedules where
  hashWithSalt _salt DescribeSnapshotSchedules' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` scheduleIdentifier
      `Prelude.hashWithSalt` tagKeys
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData DescribeSnapshotSchedules where
  rnf DescribeSnapshotSchedules' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf scheduleIdentifier
      `Prelude.seq` Prelude.rnf tagKeys
      `Prelude.seq` Prelude.rnf tagValues

instance Data.ToHeaders DescribeSnapshotSchedules where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSnapshotSchedules where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSnapshotSchedules where
  toQuery DescribeSnapshotSchedules' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeSnapshotSchedules" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "ScheduleIdentifier" Data.=: scheduleIdentifier,
        "TagKeys"
          Data.=: Data.toQuery
            (Data.toQueryList "TagKey" Prelude.<$> tagKeys),
        "TagValues"
          Data.=: Data.toQuery
            (Data.toQueryList "TagValue" Prelude.<$> tagValues)
      ]

-- | /See:/ 'newDescribeSnapshotSchedulesResponse' smart constructor.
data DescribeSnapshotSchedulesResponse = DescribeSnapshotSchedulesResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @marker@ parameter and retrying the command. If the
    -- @marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of SnapshotSchedules.
    snapshotSchedules :: Prelude.Maybe [SnapshotSchedule],
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
-- 'marker', 'describeSnapshotSchedulesResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'snapshotSchedules', 'describeSnapshotSchedulesResponse_snapshotSchedules' - A list of SnapshotSchedules.
--
-- 'httpStatus', 'describeSnapshotSchedulesResponse_httpStatus' - The response's http status code.
newDescribeSnapshotSchedulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSnapshotSchedulesResponse
newDescribeSnapshotSchedulesResponse pHttpStatus_ =
  DescribeSnapshotSchedulesResponse'
    { marker =
        Prelude.Nothing,
      snapshotSchedules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
describeSnapshotSchedulesResponse_marker :: Lens.Lens' DescribeSnapshotSchedulesResponse (Prelude.Maybe Prelude.Text)
describeSnapshotSchedulesResponse_marker = Lens.lens (\DescribeSnapshotSchedulesResponse' {marker} -> marker) (\s@DescribeSnapshotSchedulesResponse' {} a -> s {marker = a} :: DescribeSnapshotSchedulesResponse)

-- | A list of SnapshotSchedules.
describeSnapshotSchedulesResponse_snapshotSchedules :: Lens.Lens' DescribeSnapshotSchedulesResponse (Prelude.Maybe [SnapshotSchedule])
describeSnapshotSchedulesResponse_snapshotSchedules = Lens.lens (\DescribeSnapshotSchedulesResponse' {snapshotSchedules} -> snapshotSchedules) (\s@DescribeSnapshotSchedulesResponse' {} a -> s {snapshotSchedules = a} :: DescribeSnapshotSchedulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSnapshotSchedulesResponse_httpStatus :: Lens.Lens' DescribeSnapshotSchedulesResponse Prelude.Int
describeSnapshotSchedulesResponse_httpStatus = Lens.lens (\DescribeSnapshotSchedulesResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotSchedulesResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotSchedulesResponse)

instance
  Prelude.NFData
    DescribeSnapshotSchedulesResponse
  where
  rnf DescribeSnapshotSchedulesResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf snapshotSchedules
      `Prelude.seq` Prelude.rnf httpStatus

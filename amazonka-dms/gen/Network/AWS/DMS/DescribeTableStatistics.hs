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
-- Module      : Network.AWS.DMS.DescribeTableStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns table statistics on the database migration task, including table
-- name, rows inserted, rows updated, and rows deleted.
--
-- Note that the \"last updated\" column the DMS console only indicates the
-- time that AWS DMS last updated the table statistics record for a table.
-- It does not indicate the time of the last update to the table.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeTableStatistics
  ( -- * Creating a Request
    DescribeTableStatistics (..),
    newDescribeTableStatistics,

    -- * Request Lenses
    describeTableStatistics_filters,
    describeTableStatistics_marker,
    describeTableStatistics_maxRecords,
    describeTableStatistics_replicationTaskArn,

    -- * Destructuring the Response
    DescribeTableStatisticsResponse (..),
    newDescribeTableStatisticsResponse,

    -- * Response Lenses
    describeTableStatisticsResponse_tableStatistics,
    describeTableStatisticsResponse_replicationTaskArn,
    describeTableStatisticsResponse_marker,
    describeTableStatisticsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeTableStatistics' smart constructor.
data DescribeTableStatistics = DescribeTableStatistics'
  { -- | Filters applied to table statistics.
    --
    -- Valid filter names: schema-name | table-name | table-state
    --
    -- A combination of filters creates an AND condition where each record
    -- matches all specified filters.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 500.
    maxRecords :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTableStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeTableStatistics_filters' - Filters applied to table statistics.
--
-- Valid filter names: schema-name | table-name | table-state
--
-- A combination of filters creates an AND condition where each record
-- matches all specified filters.
--
-- 'marker', 'describeTableStatistics_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeTableStatistics_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 500.
--
-- 'replicationTaskArn', 'describeTableStatistics_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
newDescribeTableStatistics ::
  -- | 'replicationTaskArn'
  Core.Text ->
  DescribeTableStatistics
newDescribeTableStatistics pReplicationTaskArn_ =
  DescribeTableStatistics'
    { filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      replicationTaskArn = pReplicationTaskArn_
    }

-- | Filters applied to table statistics.
--
-- Valid filter names: schema-name | table-name | table-state
--
-- A combination of filters creates an AND condition where each record
-- matches all specified filters.
describeTableStatistics_filters :: Lens.Lens' DescribeTableStatistics (Core.Maybe [Filter])
describeTableStatistics_filters = Lens.lens (\DescribeTableStatistics' {filters} -> filters) (\s@DescribeTableStatistics' {} a -> s {filters = a} :: DescribeTableStatistics) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeTableStatistics_marker :: Lens.Lens' DescribeTableStatistics (Core.Maybe Core.Text)
describeTableStatistics_marker = Lens.lens (\DescribeTableStatistics' {marker} -> marker) (\s@DescribeTableStatistics' {} a -> s {marker = a} :: DescribeTableStatistics)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 500.
describeTableStatistics_maxRecords :: Lens.Lens' DescribeTableStatistics (Core.Maybe Core.Int)
describeTableStatistics_maxRecords = Lens.lens (\DescribeTableStatistics' {maxRecords} -> maxRecords) (\s@DescribeTableStatistics' {} a -> s {maxRecords = a} :: DescribeTableStatistics)

-- | The Amazon Resource Name (ARN) of the replication task.
describeTableStatistics_replicationTaskArn :: Lens.Lens' DescribeTableStatistics Core.Text
describeTableStatistics_replicationTaskArn = Lens.lens (\DescribeTableStatistics' {replicationTaskArn} -> replicationTaskArn) (\s@DescribeTableStatistics' {} a -> s {replicationTaskArn = a} :: DescribeTableStatistics)

instance Core.AWSPager DescribeTableStatistics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTableStatisticsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTableStatisticsResponse_tableStatistics
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTableStatistics_marker
          Lens..~ rs
          Lens.^? describeTableStatisticsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeTableStatistics where
  type
    AWSResponse DescribeTableStatistics =
      DescribeTableStatisticsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTableStatisticsResponse'
            Core.<$> (x Core..?> "TableStatistics" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ReplicationTaskArn")
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTableStatistics

instance Core.NFData DescribeTableStatistics

instance Core.ToHeaders DescribeTableStatistics where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeTableStatistics" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTableStatistics where
  toJSON DescribeTableStatistics' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords,
            Core.Just
              ("ReplicationTaskArn" Core..= replicationTaskArn)
          ]
      )

instance Core.ToPath DescribeTableStatistics where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTableStatistics where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeTableStatisticsResponse' smart constructor.
data DescribeTableStatisticsResponse = DescribeTableStatisticsResponse'
  { -- | The table statistics.
    tableStatistics :: Core.Maybe [TableStatistics],
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Core.Maybe Core.Text,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTableStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableStatistics', 'describeTableStatisticsResponse_tableStatistics' - The table statistics.
--
-- 'replicationTaskArn', 'describeTableStatisticsResponse_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
--
-- 'marker', 'describeTableStatisticsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeTableStatisticsResponse_httpStatus' - The response's http status code.
newDescribeTableStatisticsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTableStatisticsResponse
newDescribeTableStatisticsResponse pHttpStatus_ =
  DescribeTableStatisticsResponse'
    { tableStatistics =
        Core.Nothing,
      replicationTaskArn = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The table statistics.
describeTableStatisticsResponse_tableStatistics :: Lens.Lens' DescribeTableStatisticsResponse (Core.Maybe [TableStatistics])
describeTableStatisticsResponse_tableStatistics = Lens.lens (\DescribeTableStatisticsResponse' {tableStatistics} -> tableStatistics) (\s@DescribeTableStatisticsResponse' {} a -> s {tableStatistics = a} :: DescribeTableStatisticsResponse) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the replication task.
describeTableStatisticsResponse_replicationTaskArn :: Lens.Lens' DescribeTableStatisticsResponse (Core.Maybe Core.Text)
describeTableStatisticsResponse_replicationTaskArn = Lens.lens (\DescribeTableStatisticsResponse' {replicationTaskArn} -> replicationTaskArn) (\s@DescribeTableStatisticsResponse' {} a -> s {replicationTaskArn = a} :: DescribeTableStatisticsResponse)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeTableStatisticsResponse_marker :: Lens.Lens' DescribeTableStatisticsResponse (Core.Maybe Core.Text)
describeTableStatisticsResponse_marker = Lens.lens (\DescribeTableStatisticsResponse' {marker} -> marker) (\s@DescribeTableStatisticsResponse' {} a -> s {marker = a} :: DescribeTableStatisticsResponse)

-- | The response's http status code.
describeTableStatisticsResponse_httpStatus :: Lens.Lens' DescribeTableStatisticsResponse Core.Int
describeTableStatisticsResponse_httpStatus = Lens.lens (\DescribeTableStatisticsResponse' {httpStatus} -> httpStatus) (\s@DescribeTableStatisticsResponse' {} a -> s {httpStatus = a} :: DescribeTableStatisticsResponse)

instance Core.NFData DescribeTableStatisticsResponse

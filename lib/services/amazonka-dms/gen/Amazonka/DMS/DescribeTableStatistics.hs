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
-- Module      : Amazonka.DMS.DescribeTableStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns table statistics on the database migration task, including table
-- name, rows inserted, rows updated, and rows deleted.
--
-- Note that the \"last updated\" column the DMS console only indicates the
-- time that DMS last updated the table statistics record for a table. It
-- does not indicate the time of the last update to the table.
--
-- This operation returns paginated results.
module Amazonka.DMS.DescribeTableStatistics
  ( -- * Creating a Request
    DescribeTableStatistics (..),
    newDescribeTableStatistics,

    -- * Request Lenses
    describeTableStatistics_marker,
    describeTableStatistics_filters,
    describeTableStatistics_maxRecords,
    describeTableStatistics_replicationTaskArn,

    -- * Destructuring the Response
    DescribeTableStatisticsResponse (..),
    newDescribeTableStatisticsResponse,

    -- * Response Lenses
    describeTableStatisticsResponse_marker,
    describeTableStatisticsResponse_tableStatistics,
    describeTableStatisticsResponse_replicationTaskArn,
    describeTableStatisticsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeTableStatistics' smart constructor.
data DescribeTableStatistics = DescribeTableStatistics'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Filters applied to table statistics.
    --
    -- Valid filter names: schema-name | table-name | table-state
    --
    -- A combination of filters creates an AND condition where each record
    -- matches all specified filters.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 500.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeTableStatistics_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'filters', 'describeTableStatistics_filters' - Filters applied to table statistics.
--
-- Valid filter names: schema-name | table-name | table-state
--
-- A combination of filters creates an AND condition where each record
-- matches all specified filters.
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
  Prelude.Text ->
  DescribeTableStatistics
newDescribeTableStatistics pReplicationTaskArn_ =
  DescribeTableStatistics'
    { marker = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      replicationTaskArn = pReplicationTaskArn_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeTableStatistics_marker :: Lens.Lens' DescribeTableStatistics (Prelude.Maybe Prelude.Text)
describeTableStatistics_marker = Lens.lens (\DescribeTableStatistics' {marker} -> marker) (\s@DescribeTableStatistics' {} a -> s {marker = a} :: DescribeTableStatistics)

-- | Filters applied to table statistics.
--
-- Valid filter names: schema-name | table-name | table-state
--
-- A combination of filters creates an AND condition where each record
-- matches all specified filters.
describeTableStatistics_filters :: Lens.Lens' DescribeTableStatistics (Prelude.Maybe [Filter])
describeTableStatistics_filters = Lens.lens (\DescribeTableStatistics' {filters} -> filters) (\s@DescribeTableStatistics' {} a -> s {filters = a} :: DescribeTableStatistics) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 500.
describeTableStatistics_maxRecords :: Lens.Lens' DescribeTableStatistics (Prelude.Maybe Prelude.Int)
describeTableStatistics_maxRecords = Lens.lens (\DescribeTableStatistics' {maxRecords} -> maxRecords) (\s@DescribeTableStatistics' {} a -> s {maxRecords = a} :: DescribeTableStatistics)

-- | The Amazon Resource Name (ARN) of the replication task.
describeTableStatistics_replicationTaskArn :: Lens.Lens' DescribeTableStatistics Prelude.Text
describeTableStatistics_replicationTaskArn = Lens.lens (\DescribeTableStatistics' {replicationTaskArn} -> replicationTaskArn) (\s@DescribeTableStatistics' {} a -> s {replicationTaskArn = a} :: DescribeTableStatistics)

instance Core.AWSPager DescribeTableStatistics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTableStatisticsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTableStatisticsResponse_tableStatistics
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTableStatistics_marker
          Lens..~ rs
          Lens.^? describeTableStatisticsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeTableStatistics where
  type
    AWSResponse DescribeTableStatistics =
      DescribeTableStatisticsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTableStatisticsResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> ( x Core..?> "TableStatistics"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "ReplicationTaskArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTableStatistics where
  hashWithSalt _salt DescribeTableStatistics' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` replicationTaskArn

instance Prelude.NFData DescribeTableStatistics where
  rnf DescribeTableStatistics' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf replicationTaskArn

instance Core.ToHeaders DescribeTableStatistics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeTableStatistics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTableStatistics where
  toJSON DescribeTableStatistics' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords,
            Prelude.Just
              ("ReplicationTaskArn" Core..= replicationTaskArn)
          ]
      )

instance Core.ToPath DescribeTableStatistics where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTableStatistics where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeTableStatisticsResponse' smart constructor.
data DescribeTableStatisticsResponse = DescribeTableStatisticsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The table statistics.
    tableStatistics :: Prelude.Maybe [TableStatistics],
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeTableStatisticsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'tableStatistics', 'describeTableStatisticsResponse_tableStatistics' - The table statistics.
--
-- 'replicationTaskArn', 'describeTableStatisticsResponse_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
--
-- 'httpStatus', 'describeTableStatisticsResponse_httpStatus' - The response's http status code.
newDescribeTableStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTableStatisticsResponse
newDescribeTableStatisticsResponse pHttpStatus_ =
  DescribeTableStatisticsResponse'
    { marker =
        Prelude.Nothing,
      tableStatistics = Prelude.Nothing,
      replicationTaskArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeTableStatisticsResponse_marker :: Lens.Lens' DescribeTableStatisticsResponse (Prelude.Maybe Prelude.Text)
describeTableStatisticsResponse_marker = Lens.lens (\DescribeTableStatisticsResponse' {marker} -> marker) (\s@DescribeTableStatisticsResponse' {} a -> s {marker = a} :: DescribeTableStatisticsResponse)

-- | The table statistics.
describeTableStatisticsResponse_tableStatistics :: Lens.Lens' DescribeTableStatisticsResponse (Prelude.Maybe [TableStatistics])
describeTableStatisticsResponse_tableStatistics = Lens.lens (\DescribeTableStatisticsResponse' {tableStatistics} -> tableStatistics) (\s@DescribeTableStatisticsResponse' {} a -> s {tableStatistics = a} :: DescribeTableStatisticsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the replication task.
describeTableStatisticsResponse_replicationTaskArn :: Lens.Lens' DescribeTableStatisticsResponse (Prelude.Maybe Prelude.Text)
describeTableStatisticsResponse_replicationTaskArn = Lens.lens (\DescribeTableStatisticsResponse' {replicationTaskArn} -> replicationTaskArn) (\s@DescribeTableStatisticsResponse' {} a -> s {replicationTaskArn = a} :: DescribeTableStatisticsResponse)

-- | The response's http status code.
describeTableStatisticsResponse_httpStatus :: Lens.Lens' DescribeTableStatisticsResponse Prelude.Int
describeTableStatisticsResponse_httpStatus = Lens.lens (\DescribeTableStatisticsResponse' {httpStatus} -> httpStatus) (\s@DescribeTableStatisticsResponse' {} a -> s {httpStatus = a} :: DescribeTableStatisticsResponse)

instance
  Prelude.NFData
    DescribeTableStatisticsResponse
  where
  rnf DescribeTableStatisticsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf tableStatistics
      `Prelude.seq` Prelude.rnf replicationTaskArn
      `Prelude.seq` Prelude.rnf httpStatus

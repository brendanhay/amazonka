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
-- Module      : Amazonka.RDS.DescribeExportTasks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a snapshot export to Amazon S3. This API
-- operation supports pagination.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeExportTasks
  ( -- * Creating a Request
    DescribeExportTasks (..),
    newDescribeExportTasks,

    -- * Request Lenses
    describeExportTasks_exportTaskIdentifier,
    describeExportTasks_filters,
    describeExportTasks_marker,
    describeExportTasks_maxRecords,
    describeExportTasks_sourceArn,
    describeExportTasks_sourceType,

    -- * Destructuring the Response
    DescribeExportTasksResponse (..),
    newDescribeExportTasksResponse,

    -- * Response Lenses
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_marker,
    describeExportTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | The identifier of the snapshot export task to be described.
    exportTaskIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Filters specify one or more snapshot exports to describe. The filters
    -- are specified as name-value pairs that define what to include in the
    -- output. Filter names and values are case-sensitive.
    --
    -- Supported filters include the following:
    --
    -- -   @export-task-identifier@ - An identifier for the snapshot export
    --     task.
    --
    -- -   @s3-bucket@ - The Amazon S3 bucket the snapshot is exported to.
    --
    -- -   @source-arn@ - The Amazon Resource Name (ARN) of the snapshot
    --     exported to Amazon S3
    --
    -- -   @status@ - The status of the export task. Must be lowercase. Valid
    --     statuses are the following:
    --
    --     -   @canceled@
    --
    --     -   @canceling@
    --
    --     -   @complete@
    --
    --     -   @failed@
    --
    --     -   @in_progress@
    --
    --     -   @starting@
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeExportTasks@ request. If you specify this parameter, the
    -- response includes only records beyond the marker, up to the value
    -- specified by the @MaxRecords@ parameter.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified value, a pagination token called a
    -- marker is included in the response. You can use the marker in a later
    -- @DescribeExportTasks@ request to retrieve the remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The type of source for the export.
    sourceType :: Prelude.Maybe ExportSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExportTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportTaskIdentifier', 'describeExportTasks_exportTaskIdentifier' - The identifier of the snapshot export task to be described.
--
-- 'filters', 'describeExportTasks_filters' - Filters specify one or more snapshot exports to describe. The filters
-- are specified as name-value pairs that define what to include in the
-- output. Filter names and values are case-sensitive.
--
-- Supported filters include the following:
--
-- -   @export-task-identifier@ - An identifier for the snapshot export
--     task.
--
-- -   @s3-bucket@ - The Amazon S3 bucket the snapshot is exported to.
--
-- -   @source-arn@ - The Amazon Resource Name (ARN) of the snapshot
--     exported to Amazon S3
--
-- -   @status@ - The status of the export task. Must be lowercase. Valid
--     statuses are the following:
--
--     -   @canceled@
--
--     -   @canceling@
--
--     -   @complete@
--
--     -   @failed@
--
--     -   @in_progress@
--
--     -   @starting@
--
-- 'marker', 'describeExportTasks_marker' - An optional pagination token provided by a previous
-- @DescribeExportTasks@ request. If you specify this parameter, the
-- response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
--
-- 'maxRecords', 'describeExportTasks_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified value, a pagination token called a
-- marker is included in the response. You can use the marker in a later
-- @DescribeExportTasks@ request to retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'sourceArn', 'describeExportTasks_sourceArn' - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- 'sourceType', 'describeExportTasks_sourceType' - The type of source for the export.
newDescribeExportTasks ::
  DescribeExportTasks
newDescribeExportTasks =
  DescribeExportTasks'
    { exportTaskIdentifier =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The identifier of the snapshot export task to be described.
describeExportTasks_exportTaskIdentifier :: Lens.Lens' DescribeExportTasks (Prelude.Maybe Prelude.Text)
describeExportTasks_exportTaskIdentifier = Lens.lens (\DescribeExportTasks' {exportTaskIdentifier} -> exportTaskIdentifier) (\s@DescribeExportTasks' {} a -> s {exportTaskIdentifier = a} :: DescribeExportTasks)

-- | Filters specify one or more snapshot exports to describe. The filters
-- are specified as name-value pairs that define what to include in the
-- output. Filter names and values are case-sensitive.
--
-- Supported filters include the following:
--
-- -   @export-task-identifier@ - An identifier for the snapshot export
--     task.
--
-- -   @s3-bucket@ - The Amazon S3 bucket the snapshot is exported to.
--
-- -   @source-arn@ - The Amazon Resource Name (ARN) of the snapshot
--     exported to Amazon S3
--
-- -   @status@ - The status of the export task. Must be lowercase. Valid
--     statuses are the following:
--
--     -   @canceled@
--
--     -   @canceling@
--
--     -   @complete@
--
--     -   @failed@
--
--     -   @in_progress@
--
--     -   @starting@
describeExportTasks_filters :: Lens.Lens' DescribeExportTasks (Prelude.Maybe [Filter])
describeExportTasks_filters = Lens.lens (\DescribeExportTasks' {filters} -> filters) (\s@DescribeExportTasks' {} a -> s {filters = a} :: DescribeExportTasks) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribeExportTasks@ request. If you specify this parameter, the
-- response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
describeExportTasks_marker :: Lens.Lens' DescribeExportTasks (Prelude.Maybe Prelude.Text)
describeExportTasks_marker = Lens.lens (\DescribeExportTasks' {marker} -> marker) (\s@DescribeExportTasks' {} a -> s {marker = a} :: DescribeExportTasks)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified value, a pagination token called a
-- marker is included in the response. You can use the marker in a later
-- @DescribeExportTasks@ request to retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeExportTasks_maxRecords :: Lens.Lens' DescribeExportTasks (Prelude.Maybe Prelude.Natural)
describeExportTasks_maxRecords = Lens.lens (\DescribeExportTasks' {maxRecords} -> maxRecords) (\s@DescribeExportTasks' {} a -> s {maxRecords = a} :: DescribeExportTasks)

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
describeExportTasks_sourceArn :: Lens.Lens' DescribeExportTasks (Prelude.Maybe Prelude.Text)
describeExportTasks_sourceArn = Lens.lens (\DescribeExportTasks' {sourceArn} -> sourceArn) (\s@DescribeExportTasks' {} a -> s {sourceArn = a} :: DescribeExportTasks)

-- | The type of source for the export.
describeExportTasks_sourceType :: Lens.Lens' DescribeExportTasks (Prelude.Maybe ExportSourceType)
describeExportTasks_sourceType = Lens.lens (\DescribeExportTasks' {sourceType} -> sourceType) (\s@DescribeExportTasks' {} a -> s {sourceType = a} :: DescribeExportTasks)

instance Core.AWSPager DescribeExportTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeExportTasksResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeExportTasksResponse_exportTasks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeExportTasks_marker
          Lens..~ rs
          Lens.^? describeExportTasksResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeExportTasks where
  type
    AWSResponse DescribeExportTasks =
      DescribeExportTasksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeExportTasksResult"
      ( \s h x ->
          DescribeExportTasksResponse'
            Prelude.<$> ( x Data..@? "ExportTasks" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "ExportTask")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExportTasks where
  hashWithSalt _salt DescribeExportTasks' {..} =
    _salt `Prelude.hashWithSalt` exportTaskIdentifier
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData DescribeExportTasks where
  rnf DescribeExportTasks' {..} =
    Prelude.rnf exportTaskIdentifier
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf sourceType

instance Data.ToHeaders DescribeExportTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeExportTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeExportTasks where
  toQuery DescribeExportTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeExportTasks" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "ExportTaskIdentifier" Data.=: exportTaskIdentifier,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "SourceArn" Data.=: sourceArn,
        "SourceType" Data.=: sourceType
      ]

-- | /See:/ 'newDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | Information about an export of a snapshot to Amazon S3.
    exportTasks :: Prelude.Maybe [ExportTask],
    -- | A pagination token that can be used in a later @DescribeExportTasks@
    -- request. A marker is used for pagination to identify the location to
    -- begin output for the next response of @DescribeExportTasks@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExportTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportTasks', 'describeExportTasksResponse_exportTasks' - Information about an export of a snapshot to Amazon S3.
--
-- 'marker', 'describeExportTasksResponse_marker' - A pagination token that can be used in a later @DescribeExportTasks@
-- request. A marker is used for pagination to identify the location to
-- begin output for the next response of @DescribeExportTasks@.
--
-- 'httpStatus', 'describeExportTasksResponse_httpStatus' - The response's http status code.
newDescribeExportTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExportTasksResponse
newDescribeExportTasksResponse pHttpStatus_ =
  DescribeExportTasksResponse'
    { exportTasks =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about an export of a snapshot to Amazon S3.
describeExportTasksResponse_exportTasks :: Lens.Lens' DescribeExportTasksResponse (Prelude.Maybe [ExportTask])
describeExportTasksResponse_exportTasks = Lens.lens (\DescribeExportTasksResponse' {exportTasks} -> exportTasks) (\s@DescribeExportTasksResponse' {} a -> s {exportTasks = a} :: DescribeExportTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that can be used in a later @DescribeExportTasks@
-- request. A marker is used for pagination to identify the location to
-- begin output for the next response of @DescribeExportTasks@.
describeExportTasksResponse_marker :: Lens.Lens' DescribeExportTasksResponse (Prelude.Maybe Prelude.Text)
describeExportTasksResponse_marker = Lens.lens (\DescribeExportTasksResponse' {marker} -> marker) (\s@DescribeExportTasksResponse' {} a -> s {marker = a} :: DescribeExportTasksResponse)

-- | The response's http status code.
describeExportTasksResponse_httpStatus :: Lens.Lens' DescribeExportTasksResponse Prelude.Int
describeExportTasksResponse_httpStatus = Lens.lens (\DescribeExportTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportTasksResponse)

instance Prelude.NFData DescribeExportTasksResponse where
  rnf DescribeExportTasksResponse' {..} =
    Prelude.rnf exportTasks
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    describeExportTasks_sourceArn,
    describeExportTasks_filters,
    describeExportTasks_marker,
    describeExportTasks_exportTaskIdentifier,
    describeExportTasks_maxRecords,

    -- * Destructuring the Response
    DescribeExportTasksResponse (..),
    newDescribeExportTasksResponse,

    -- * Response Lenses
    describeExportTasksResponse_marker,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
    sourceArn :: Prelude.Maybe Prelude.Text,
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
    -- -   @status@ - The status of the export task. Must be lowercase, for
    --     example, @complete@.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeExportTasks@ request. If you specify this parameter, the
    -- response includes only records beyond the marker, up to the value
    -- specified by the @MaxRecords@ parameter.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the snapshot export task to be described.
    exportTaskIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified value, a pagination token called a
    -- marker is included in the response. You can use the marker in a later
    -- @DescribeExportTasks@ request to retrieve the remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Natural
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
-- 'sourceArn', 'describeExportTasks_sourceArn' - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
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
-- -   @status@ - The status of the export task. Must be lowercase, for
--     example, @complete@.
--
-- 'marker', 'describeExportTasks_marker' - An optional pagination token provided by a previous
-- @DescribeExportTasks@ request. If you specify this parameter, the
-- response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
--
-- 'exportTaskIdentifier', 'describeExportTasks_exportTaskIdentifier' - The identifier of the snapshot export task to be described.
--
-- 'maxRecords', 'describeExportTasks_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified value, a pagination token called a
-- marker is included in the response. You can use the marker in a later
-- @DescribeExportTasks@ request to retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeExportTasks ::
  DescribeExportTasks
newDescribeExportTasks =
  DescribeExportTasks'
    { sourceArn = Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      exportTaskIdentifier = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
describeExportTasks_sourceArn :: Lens.Lens' DescribeExportTasks (Prelude.Maybe Prelude.Text)
describeExportTasks_sourceArn = Lens.lens (\DescribeExportTasks' {sourceArn} -> sourceArn) (\s@DescribeExportTasks' {} a -> s {sourceArn = a} :: DescribeExportTasks)

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
-- -   @status@ - The status of the export task. Must be lowercase, for
--     example, @complete@.
describeExportTasks_filters :: Lens.Lens' DescribeExportTasks (Prelude.Maybe [Filter])
describeExportTasks_filters = Lens.lens (\DescribeExportTasks' {filters} -> filters) (\s@DescribeExportTasks' {} a -> s {filters = a} :: DescribeExportTasks) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribeExportTasks@ request. If you specify this parameter, the
-- response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
describeExportTasks_marker :: Lens.Lens' DescribeExportTasks (Prelude.Maybe Prelude.Text)
describeExportTasks_marker = Lens.lens (\DescribeExportTasks' {marker} -> marker) (\s@DescribeExportTasks' {} a -> s {marker = a} :: DescribeExportTasks)

-- | The identifier of the snapshot export task to be described.
describeExportTasks_exportTaskIdentifier :: Lens.Lens' DescribeExportTasks (Prelude.Maybe Prelude.Text)
describeExportTasks_exportTaskIdentifier = Lens.lens (\DescribeExportTasks' {exportTaskIdentifier} -> exportTaskIdentifier) (\s@DescribeExportTasks' {} a -> s {exportTaskIdentifier = a} :: DescribeExportTasks)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeExportTasksResult"
      ( \s h x ->
          DescribeExportTasksResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "ExportTasks" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "ExportTask")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExportTasks where
  hashWithSalt salt' DescribeExportTasks' {..} =
    salt' `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` exportTaskIdentifier
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData DescribeExportTasks where
  rnf DescribeExportTasks' {..} =
    Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf exportTaskIdentifier
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters

instance Core.ToHeaders DescribeExportTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeExportTasks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeExportTasks where
  toQuery DescribeExportTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeExportTasks" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "SourceArn" Core.=: sourceArn,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Core.=: marker,
        "ExportTaskIdentifier" Core.=: exportTaskIdentifier,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | A pagination token that can be used in a later @DescribeExportTasks@
    -- request. A marker is used for pagination to identify the location to
    -- begin output for the next response of @DescribeExportTasks@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Information about an export of a snapshot to Amazon S3.
    exportTasks :: Prelude.Maybe [ExportTask],
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
-- 'marker', 'describeExportTasksResponse_marker' - A pagination token that can be used in a later @DescribeExportTasks@
-- request. A marker is used for pagination to identify the location to
-- begin output for the next response of @DescribeExportTasks@.
--
-- 'exportTasks', 'describeExportTasksResponse_exportTasks' - Information about an export of a snapshot to Amazon S3.
--
-- 'httpStatus', 'describeExportTasksResponse_httpStatus' - The response's http status code.
newDescribeExportTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExportTasksResponse
newDescribeExportTasksResponse pHttpStatus_ =
  DescribeExportTasksResponse'
    { marker =
        Prelude.Nothing,
      exportTasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token that can be used in a later @DescribeExportTasks@
-- request. A marker is used for pagination to identify the location to
-- begin output for the next response of @DescribeExportTasks@.
describeExportTasksResponse_marker :: Lens.Lens' DescribeExportTasksResponse (Prelude.Maybe Prelude.Text)
describeExportTasksResponse_marker = Lens.lens (\DescribeExportTasksResponse' {marker} -> marker) (\s@DescribeExportTasksResponse' {} a -> s {marker = a} :: DescribeExportTasksResponse)

-- | Information about an export of a snapshot to Amazon S3.
describeExportTasksResponse_exportTasks :: Lens.Lens' DescribeExportTasksResponse (Prelude.Maybe [ExportTask])
describeExportTasksResponse_exportTasks = Lens.lens (\DescribeExportTasksResponse' {exportTasks} -> exportTasks) (\s@DescribeExportTasksResponse' {} a -> s {exportTasks = a} :: DescribeExportTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeExportTasksResponse_httpStatus :: Lens.Lens' DescribeExportTasksResponse Prelude.Int
describeExportTasksResponse_httpStatus = Lens.lens (\DescribeExportTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportTasksResponse)

instance Prelude.NFData DescribeExportTasksResponse where
  rnf DescribeExportTasksResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf exportTasks

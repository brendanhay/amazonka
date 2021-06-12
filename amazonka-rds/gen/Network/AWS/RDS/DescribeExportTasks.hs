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
-- Module      : Network.AWS.RDS.DescribeExportTasks
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
module Network.AWS.RDS.DescribeExportTasks
  ( -- * Creating a Request
    DescribeExportTasks (..),
    newDescribeExportTasks,

    -- * Request Lenses
    describeExportTasks_filters,
    describeExportTasks_sourceArn,
    describeExportTasks_exportTaskIdentifier,
    describeExportTasks_marker,
    describeExportTasks_maxRecords,

    -- * Destructuring the Response
    DescribeExportTasksResponse (..),
    newDescribeExportTasksResponse,

    -- * Response Lenses
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_marker,
    describeExportTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | Filters specify one or more snapshot exports to describe. The filters
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
    filters :: Core.Maybe [Filter],
    -- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
    sourceArn :: Core.Maybe Core.Text,
    -- | The identifier of the snapshot export task to be described.
    exportTaskIdentifier :: Core.Maybe Core.Text,
    -- | An optional pagination token provided by a previous
    -- @DescribeExportTasks@ request. If you specify this parameter, the
    -- response includes only records beyond the marker, up to the value
    -- specified by the @MaxRecords@ parameter.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified value, a pagination token called a
    -- marker is included in the response. You can use the marker in a later
    -- @DescribeExportTasks@ request to retrieve the remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeExportTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'sourceArn', 'describeExportTasks_sourceArn' - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- 'exportTaskIdentifier', 'describeExportTasks_exportTaskIdentifier' - The identifier of the snapshot export task to be described.
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
newDescribeExportTasks ::
  DescribeExportTasks
newDescribeExportTasks =
  DescribeExportTasks'
    { filters = Core.Nothing,
      sourceArn = Core.Nothing,
      exportTaskIdentifier = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

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
describeExportTasks_filters :: Lens.Lens' DescribeExportTasks (Core.Maybe [Filter])
describeExportTasks_filters = Lens.lens (\DescribeExportTasks' {filters} -> filters) (\s@DescribeExportTasks' {} a -> s {filters = a} :: DescribeExportTasks) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
describeExportTasks_sourceArn :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Text)
describeExportTasks_sourceArn = Lens.lens (\DescribeExportTasks' {sourceArn} -> sourceArn) (\s@DescribeExportTasks' {} a -> s {sourceArn = a} :: DescribeExportTasks)

-- | The identifier of the snapshot export task to be described.
describeExportTasks_exportTaskIdentifier :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Text)
describeExportTasks_exportTaskIdentifier = Lens.lens (\DescribeExportTasks' {exportTaskIdentifier} -> exportTaskIdentifier) (\s@DescribeExportTasks' {} a -> s {exportTaskIdentifier = a} :: DescribeExportTasks)

-- | An optional pagination token provided by a previous
-- @DescribeExportTasks@ request. If you specify this parameter, the
-- response includes only records beyond the marker, up to the value
-- specified by the @MaxRecords@ parameter.
describeExportTasks_marker :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Text)
describeExportTasks_marker = Lens.lens (\DescribeExportTasks' {marker} -> marker) (\s@DescribeExportTasks' {} a -> s {marker = a} :: DescribeExportTasks)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified value, a pagination token called a
-- marker is included in the response. You can use the marker in a later
-- @DescribeExportTasks@ request to retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeExportTasks_maxRecords :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Natural)
describeExportTasks_maxRecords = Lens.lens (\DescribeExportTasks' {maxRecords} -> maxRecords) (\s@DescribeExportTasks' {} a -> s {maxRecords = a} :: DescribeExportTasks)

instance Core.AWSPager DescribeExportTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeExportTasksResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeExportTasksResponse_exportTasks
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeExportTasks_marker
          Lens..~ rs
          Lens.^? describeExportTasksResponse_marker Core.. Lens._Just

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
            Core.<$> ( x Core..@? "ExportTasks" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "ExportTask")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeExportTasks

instance Core.NFData DescribeExportTasks

instance Core.ToHeaders DescribeExportTasks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeExportTasks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeExportTasks where
  toQuery DescribeExportTasks' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeExportTasks" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "SourceArn" Core.=: sourceArn,
        "ExportTaskIdentifier" Core.=: exportTaskIdentifier,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | Information about an export of a snapshot to Amazon S3.
    exportTasks :: Core.Maybe [ExportTask],
    -- | A pagination token that can be used in a later @DescribeExportTasks@
    -- request. A marker is used for pagination to identify the location to
    -- begin output for the next response of @DescribeExportTasks@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeExportTasksResponse
newDescribeExportTasksResponse pHttpStatus_ =
  DescribeExportTasksResponse'
    { exportTasks =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about an export of a snapshot to Amazon S3.
describeExportTasksResponse_exportTasks :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe [ExportTask])
describeExportTasksResponse_exportTasks = Lens.lens (\DescribeExportTasksResponse' {exportTasks} -> exportTasks) (\s@DescribeExportTasksResponse' {} a -> s {exportTasks = a} :: DescribeExportTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | A pagination token that can be used in a later @DescribeExportTasks@
-- request. A marker is used for pagination to identify the location to
-- begin output for the next response of @DescribeExportTasks@.
describeExportTasksResponse_marker :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe Core.Text)
describeExportTasksResponse_marker = Lens.lens (\DescribeExportTasksResponse' {marker} -> marker) (\s@DescribeExportTasksResponse' {} a -> s {marker = a} :: DescribeExportTasksResponse)

-- | The response's http status code.
describeExportTasksResponse_httpStatus :: Lens.Lens' DescribeExportTasksResponse Core.Int
describeExportTasksResponse_httpStatus = Lens.lens (\DescribeExportTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportTasksResponse)

instance Core.NFData DescribeExportTasksResponse

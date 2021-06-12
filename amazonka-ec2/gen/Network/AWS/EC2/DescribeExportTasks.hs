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
-- Module      : Network.AWS.EC2.DescribeExportTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified export instance tasks or all of your export
-- instance tasks.
module Network.AWS.EC2.DescribeExportTasks
  ( -- * Creating a Request
    DescribeExportTasks (..),
    newDescribeExportTasks,

    -- * Request Lenses
    describeExportTasks_exportTaskIds,
    describeExportTasks_filters,

    -- * Destructuring the Response
    DescribeExportTasksResponse (..),
    newDescribeExportTasksResponse,

    -- * Response Lenses
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | The export task IDs.
    exportTaskIds :: Core.Maybe [Core.Text],
    -- | the filters for the export tasks.
    filters :: Core.Maybe [Filter]
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
-- 'exportTaskIds', 'describeExportTasks_exportTaskIds' - The export task IDs.
--
-- 'filters', 'describeExportTasks_filters' - the filters for the export tasks.
newDescribeExportTasks ::
  DescribeExportTasks
newDescribeExportTasks =
  DescribeExportTasks'
    { exportTaskIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The export task IDs.
describeExportTasks_exportTaskIds :: Lens.Lens' DescribeExportTasks (Core.Maybe [Core.Text])
describeExportTasks_exportTaskIds = Lens.lens (\DescribeExportTasks' {exportTaskIds} -> exportTaskIds) (\s@DescribeExportTasks' {} a -> s {exportTaskIds = a} :: DescribeExportTasks) Core.. Lens.mapping Lens._Coerce

-- | the filters for the export tasks.
describeExportTasks_filters :: Lens.Lens' DescribeExportTasks (Core.Maybe [Filter])
describeExportTasks_filters = Lens.lens (\DescribeExportTasks' {filters} -> filters) (\s@DescribeExportTasks' {} a -> s {filters = a} :: DescribeExportTasks) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeExportTasks where
  type
    AWSResponse DescribeExportTasks =
      DescribeExportTasksResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeExportTasksResponse'
            Core.<$> ( x Core..@? "exportTaskSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
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
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "ExportTaskId"
              Core.<$> exportTaskIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | Information about the export tasks.
    exportTasks :: Core.Maybe [ExportTask],
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
-- 'exportTasks', 'describeExportTasksResponse_exportTasks' - Information about the export tasks.
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
      httpStatus = pHttpStatus_
    }

-- | Information about the export tasks.
describeExportTasksResponse_exportTasks :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe [ExportTask])
describeExportTasksResponse_exportTasks = Lens.lens (\DescribeExportTasksResponse' {exportTasks} -> exportTasks) (\s@DescribeExportTasksResponse' {} a -> s {exportTasks = a} :: DescribeExportTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeExportTasksResponse_httpStatus :: Lens.Lens' DescribeExportTasksResponse Core.Int
describeExportTasksResponse_httpStatus = Lens.lens (\DescribeExportTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportTasksResponse)

instance Core.NFData DescribeExportTasksResponse

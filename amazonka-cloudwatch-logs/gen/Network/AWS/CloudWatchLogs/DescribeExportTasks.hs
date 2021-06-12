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
-- Module      : Network.AWS.CloudWatchLogs.DescribeExportTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified export tasks. You can list all your export tasks or
-- filter the results based on task ID or task status.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeExportTasks
  ( -- * Creating a Request
    DescribeExportTasks (..),
    newDescribeExportTasks,

    -- * Request Lenses
    describeExportTasks_nextToken,
    describeExportTasks_taskId,
    describeExportTasks_statusCode,
    describeExportTasks_limit,

    -- * Destructuring the Response
    DescribeExportTasksResponse (..),
    newDescribeExportTasksResponse,

    -- * Response Lenses
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the export task. Specifying a task ID filters the results to
    -- zero or one export tasks.
    taskId :: Core.Maybe Core.Text,
    -- | The status code of the export task. Specifying a status code filters the
    -- results to zero or more export tasks.
    statusCode :: Core.Maybe ExportTaskStatusCode,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default is up to 50 items.
    limit :: Core.Maybe Core.Natural
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
-- 'nextToken', 'describeExportTasks_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'taskId', 'describeExportTasks_taskId' - The ID of the export task. Specifying a task ID filters the results to
-- zero or one export tasks.
--
-- 'statusCode', 'describeExportTasks_statusCode' - The status code of the export task. Specifying a status code filters the
-- results to zero or more export tasks.
--
-- 'limit', 'describeExportTasks_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
newDescribeExportTasks ::
  DescribeExportTasks
newDescribeExportTasks =
  DescribeExportTasks'
    { nextToken = Core.Nothing,
      taskId = Core.Nothing,
      statusCode = Core.Nothing,
      limit = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeExportTasks_nextToken :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Text)
describeExportTasks_nextToken = Lens.lens (\DescribeExportTasks' {nextToken} -> nextToken) (\s@DescribeExportTasks' {} a -> s {nextToken = a} :: DescribeExportTasks)

-- | The ID of the export task. Specifying a task ID filters the results to
-- zero or one export tasks.
describeExportTasks_taskId :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Text)
describeExportTasks_taskId = Lens.lens (\DescribeExportTasks' {taskId} -> taskId) (\s@DescribeExportTasks' {} a -> s {taskId = a} :: DescribeExportTasks)

-- | The status code of the export task. Specifying a status code filters the
-- results to zero or more export tasks.
describeExportTasks_statusCode :: Lens.Lens' DescribeExportTasks (Core.Maybe ExportTaskStatusCode)
describeExportTasks_statusCode = Lens.lens (\DescribeExportTasks' {statusCode} -> statusCode) (\s@DescribeExportTasks' {} a -> s {statusCode = a} :: DescribeExportTasks)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
describeExportTasks_limit :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Natural)
describeExportTasks_limit = Lens.lens (\DescribeExportTasks' {limit} -> limit) (\s@DescribeExportTasks' {} a -> s {limit = a} :: DescribeExportTasks)

instance Core.AWSPager DescribeExportTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeExportTasksResponse_nextToken
              Core.. Lens._Just
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
          Lens.& describeExportTasks_nextToken
          Lens..~ rs
          Lens.^? describeExportTasksResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeExportTasks where
  type
    AWSResponse DescribeExportTasks =
      DescribeExportTasksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExportTasksResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "exportTasks" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeExportTasks

instance Core.NFData DescribeExportTasks

instance Core.ToHeaders DescribeExportTasks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeExportTasks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeExportTasks where
  toJSON DescribeExportTasks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("taskId" Core..=) Core.<$> taskId,
            ("statusCode" Core..=) Core.<$> statusCode,
            ("limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeExportTasks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeExportTasks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The export tasks.
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
-- 'nextToken', 'describeExportTasksResponse_nextToken' - Undocumented member.
--
-- 'exportTasks', 'describeExportTasksResponse_exportTasks' - The export tasks.
--
-- 'httpStatus', 'describeExportTasksResponse_httpStatus' - The response's http status code.
newDescribeExportTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeExportTasksResponse
newDescribeExportTasksResponse pHttpStatus_ =
  DescribeExportTasksResponse'
    { nextToken =
        Core.Nothing,
      exportTasks = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeExportTasksResponse_nextToken :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe Core.Text)
describeExportTasksResponse_nextToken = Lens.lens (\DescribeExportTasksResponse' {nextToken} -> nextToken) (\s@DescribeExportTasksResponse' {} a -> s {nextToken = a} :: DescribeExportTasksResponse)

-- | The export tasks.
describeExportTasksResponse_exportTasks :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe [ExportTask])
describeExportTasksResponse_exportTasks = Lens.lens (\DescribeExportTasksResponse' {exportTasks} -> exportTasks) (\s@DescribeExportTasksResponse' {} a -> s {exportTasks = a} :: DescribeExportTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeExportTasksResponse_httpStatus :: Lens.Lens' DescribeExportTasksResponse Core.Int
describeExportTasksResponse_httpStatus = Lens.lens (\DescribeExportTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportTasksResponse)

instance Core.NFData DescribeExportTasksResponse

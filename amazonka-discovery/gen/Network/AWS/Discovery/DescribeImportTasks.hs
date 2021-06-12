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
-- Module      : Network.AWS.Discovery.DescribeImportTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of import tasks for your account, including status
-- information, times, IDs, the Amazon S3 Object URL for the import file,
-- and more.
module Network.AWS.Discovery.DescribeImportTasks
  ( -- * Creating a Request
    DescribeImportTasks (..),
    newDescribeImportTasks,

    -- * Request Lenses
    describeImportTasks_nextToken,
    describeImportTasks_maxResults,
    describeImportTasks_filters,

    -- * Destructuring the Response
    DescribeImportTasksResponse (..),
    newDescribeImportTasksResponse,

    -- * Response Lenses
    describeImportTasksResponse_nextToken,
    describeImportTasksResponse_tasks,
    describeImportTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeImportTasks' smart constructor.
data DescribeImportTasks = DescribeImportTasks'
  { -- | The token to request a specific page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results that you want this request to return, up
    -- to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | An array of name-value pairs that you provide to filter the results for
    -- the @DescribeImportTask@ request to a specific subset of results.
    -- Currently, wildcard values aren\'t supported for filters.
    filters :: Core.Maybe [ImportTaskFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImportTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImportTasks_nextToken' - The token to request a specific page of results.
--
-- 'maxResults', 'describeImportTasks_maxResults' - The maximum number of results that you want this request to return, up
-- to 100.
--
-- 'filters', 'describeImportTasks_filters' - An array of name-value pairs that you provide to filter the results for
-- the @DescribeImportTask@ request to a specific subset of results.
-- Currently, wildcard values aren\'t supported for filters.
newDescribeImportTasks ::
  DescribeImportTasks
newDescribeImportTasks =
  DescribeImportTasks'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token to request a specific page of results.
describeImportTasks_nextToken :: Lens.Lens' DescribeImportTasks (Core.Maybe Core.Text)
describeImportTasks_nextToken = Lens.lens (\DescribeImportTasks' {nextToken} -> nextToken) (\s@DescribeImportTasks' {} a -> s {nextToken = a} :: DescribeImportTasks)

-- | The maximum number of results that you want this request to return, up
-- to 100.
describeImportTasks_maxResults :: Lens.Lens' DescribeImportTasks (Core.Maybe Core.Natural)
describeImportTasks_maxResults = Lens.lens (\DescribeImportTasks' {maxResults} -> maxResults) (\s@DescribeImportTasks' {} a -> s {maxResults = a} :: DescribeImportTasks)

-- | An array of name-value pairs that you provide to filter the results for
-- the @DescribeImportTask@ request to a specific subset of results.
-- Currently, wildcard values aren\'t supported for filters.
describeImportTasks_filters :: Lens.Lens' DescribeImportTasks (Core.Maybe [ImportTaskFilter])
describeImportTasks_filters = Lens.lens (\DescribeImportTasks' {filters} -> filters) (\s@DescribeImportTasks' {} a -> s {filters = a} :: DescribeImportTasks) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeImportTasks where
  type
    AWSResponse DescribeImportTasks =
      DescribeImportTasksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImportTasksResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "tasks" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeImportTasks

instance Core.NFData DescribeImportTasks

instance Core.ToHeaders DescribeImportTasks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DescribeImportTasks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeImportTasks where
  toJSON DescribeImportTasks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath DescribeImportTasks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeImportTasks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeImportTasksResponse' smart constructor.
data DescribeImportTasksResponse = DescribeImportTasksResponse'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A returned array of import tasks that match any applied filters, up to
    -- the specified number of maximum results.
    tasks :: Core.Maybe [ImportTask],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImportTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImportTasksResponse_nextToken' - The token to request the next page of results.
--
-- 'tasks', 'describeImportTasksResponse_tasks' - A returned array of import tasks that match any applied filters, up to
-- the specified number of maximum results.
--
-- 'httpStatus', 'describeImportTasksResponse_httpStatus' - The response's http status code.
newDescribeImportTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeImportTasksResponse
newDescribeImportTasksResponse pHttpStatus_ =
  DescribeImportTasksResponse'
    { nextToken =
        Core.Nothing,
      tasks = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to request the next page of results.
describeImportTasksResponse_nextToken :: Lens.Lens' DescribeImportTasksResponse (Core.Maybe Core.Text)
describeImportTasksResponse_nextToken = Lens.lens (\DescribeImportTasksResponse' {nextToken} -> nextToken) (\s@DescribeImportTasksResponse' {} a -> s {nextToken = a} :: DescribeImportTasksResponse)

-- | A returned array of import tasks that match any applied filters, up to
-- the specified number of maximum results.
describeImportTasksResponse_tasks :: Lens.Lens' DescribeImportTasksResponse (Core.Maybe [ImportTask])
describeImportTasksResponse_tasks = Lens.lens (\DescribeImportTasksResponse' {tasks} -> tasks) (\s@DescribeImportTasksResponse' {} a -> s {tasks = a} :: DescribeImportTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeImportTasksResponse_httpStatus :: Lens.Lens' DescribeImportTasksResponse Core.Int
describeImportTasksResponse_httpStatus = Lens.lens (\DescribeImportTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeImportTasksResponse' {} a -> s {httpStatus = a} :: DescribeImportTasksResponse)

instance Core.NFData DescribeImportTasksResponse

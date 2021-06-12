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
-- Module      : Network.AWS.Discovery.DescribeExportTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve status of one or more export tasks. You can retrieve the status
-- of up to 100 export tasks.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeExportTasks
  ( -- * Creating a Request
    DescribeExportTasks (..),
    newDescribeExportTasks,

    -- * Request Lenses
    describeExportTasks_nextToken,
    describeExportTasks_maxResults,
    describeExportTasks_exportIds,
    describeExportTasks_filters,

    -- * Destructuring the Response
    DescribeExportTasksResponse (..),
    newDescribeExportTasksResponse,

    -- * Response Lenses
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_exportsInfo,
    describeExportTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeExportTasks@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    -- This value is null when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of volume results returned by @DescribeExportTasks@
    -- in paginated output. When this parameter is used, @DescribeExportTasks@
    -- only returns @maxResults@ results in a single page along with a
    -- @nextToken@ response element.
    maxResults :: Core.Maybe Core.Int,
    -- | One or more unique identifiers used to query the status of an export
    -- request.
    exportIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @AgentId@ - ID of the agent whose collected data will be exported
    filters :: Core.Maybe [ExportFilter]
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
-- 'nextToken', 'describeExportTasks_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeExportTasks@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is null when there are no more results to return.
--
-- 'maxResults', 'describeExportTasks_maxResults' - The maximum number of volume results returned by @DescribeExportTasks@
-- in paginated output. When this parameter is used, @DescribeExportTasks@
-- only returns @maxResults@ results in a single page along with a
-- @nextToken@ response element.
--
-- 'exportIds', 'describeExportTasks_exportIds' - One or more unique identifiers used to query the status of an export
-- request.
--
-- 'filters', 'describeExportTasks_filters' - One or more filters.
--
-- -   @AgentId@ - ID of the agent whose collected data will be exported
newDescribeExportTasks ::
  DescribeExportTasks
newDescribeExportTasks =
  DescribeExportTasks'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      exportIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeExportTasks@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is null when there are no more results to return.
describeExportTasks_nextToken :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Text)
describeExportTasks_nextToken = Lens.lens (\DescribeExportTasks' {nextToken} -> nextToken) (\s@DescribeExportTasks' {} a -> s {nextToken = a} :: DescribeExportTasks)

-- | The maximum number of volume results returned by @DescribeExportTasks@
-- in paginated output. When this parameter is used, @DescribeExportTasks@
-- only returns @maxResults@ results in a single page along with a
-- @nextToken@ response element.
describeExportTasks_maxResults :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Int)
describeExportTasks_maxResults = Lens.lens (\DescribeExportTasks' {maxResults} -> maxResults) (\s@DescribeExportTasks' {} a -> s {maxResults = a} :: DescribeExportTasks)

-- | One or more unique identifiers used to query the status of an export
-- request.
describeExportTasks_exportIds :: Lens.Lens' DescribeExportTasks (Core.Maybe [Core.Text])
describeExportTasks_exportIds = Lens.lens (\DescribeExportTasks' {exportIds} -> exportIds) (\s@DescribeExportTasks' {} a -> s {exportIds = a} :: DescribeExportTasks) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @AgentId@ - ID of the agent whose collected data will be exported
describeExportTasks_filters :: Lens.Lens' DescribeExportTasks (Core.Maybe [ExportFilter])
describeExportTasks_filters = Lens.lens (\DescribeExportTasks' {filters} -> filters) (\s@DescribeExportTasks' {} a -> s {filters = a} :: DescribeExportTasks) Core.. Lens.mapping Lens._Coerce

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
            Lens.^? describeExportTasksResponse_exportsInfo
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
            Core.<*> (x Core..?> "exportsInfo" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeExportTasks

instance Core.NFData DescribeExportTasks

instance Core.ToHeaders DescribeExportTasks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DescribeExportTasks" ::
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
            ("maxResults" Core..=) Core.<$> maxResults,
            ("exportIds" Core..=) Core.<$> exportIds,
            ("filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath DescribeExportTasks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeExportTasks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | The @nextToken@ value to include in a future @DescribeExportTasks@
    -- request. When the results of a @DescribeExportTasks@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is null when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Contains one or more sets of export request details. When the status of
    -- a request is @SUCCEEDED@, the response includes a URL for an Amazon S3
    -- bucket where you can view the data in a CSV file.
    exportsInfo :: Core.Maybe [ExportInfo],
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
-- 'nextToken', 'describeExportTasksResponse_nextToken' - The @nextToken@ value to include in a future @DescribeExportTasks@
-- request. When the results of a @DescribeExportTasks@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is null when there are no more results to return.
--
-- 'exportsInfo', 'describeExportTasksResponse_exportsInfo' - Contains one or more sets of export request details. When the status of
-- a request is @SUCCEEDED@, the response includes a URL for an Amazon S3
-- bucket where you can view the data in a CSV file.
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
      exportsInfo = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeExportTasks@
-- request. When the results of a @DescribeExportTasks@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is null when there are no more results to return.
describeExportTasksResponse_nextToken :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe Core.Text)
describeExportTasksResponse_nextToken = Lens.lens (\DescribeExportTasksResponse' {nextToken} -> nextToken) (\s@DescribeExportTasksResponse' {} a -> s {nextToken = a} :: DescribeExportTasksResponse)

-- | Contains one or more sets of export request details. When the status of
-- a request is @SUCCEEDED@, the response includes a URL for an Amazon S3
-- bucket where you can view the data in a CSV file.
describeExportTasksResponse_exportsInfo :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe [ExportInfo])
describeExportTasksResponse_exportsInfo = Lens.lens (\DescribeExportTasksResponse' {exportsInfo} -> exportsInfo) (\s@DescribeExportTasksResponse' {} a -> s {exportsInfo = a} :: DescribeExportTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeExportTasksResponse_httpStatus :: Lens.Lens' DescribeExportTasksResponse Core.Int
describeExportTasksResponse_httpStatus = Lens.lens (\DescribeExportTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportTasksResponse)

instance Core.NFData DescribeExportTasksResponse

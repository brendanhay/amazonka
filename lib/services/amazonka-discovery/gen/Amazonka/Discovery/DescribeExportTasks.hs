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
-- Module      : Amazonka.Discovery.DescribeExportTasks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve status of one or more export tasks. You can retrieve the status
-- of up to 100 export tasks.
--
-- This operation returns paginated results.
module Amazonka.Discovery.DescribeExportTasks
  ( -- * Creating a Request
    DescribeExportTasks (..),
    newDescribeExportTasks,

    -- * Request Lenses
    describeExportTasks_exportIds,
    describeExportTasks_nextToken,
    describeExportTasks_filters,
    describeExportTasks_maxResults,

    -- * Destructuring the Response
    DescribeExportTasksResponse (..),
    newDescribeExportTasksResponse,

    -- * Response Lenses
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_exportsInfo,
    describeExportTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | One or more unique identifiers used to query the status of an export
    -- request.
    exportIds :: Prelude.Maybe [Prelude.Text],
    -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeExportTasks@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    -- This value is null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters.
    --
    -- -   @AgentId@ - ID of the agent whose collected data will be exported
    filters :: Prelude.Maybe [ExportFilter],
    -- | The maximum number of volume results returned by @DescribeExportTasks@
    -- in paginated output. When this parameter is used, @DescribeExportTasks@
    -- only returns @maxResults@ results in a single page along with a
    -- @nextToken@ response element.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'exportIds', 'describeExportTasks_exportIds' - One or more unique identifiers used to query the status of an export
-- request.
--
-- 'nextToken', 'describeExportTasks_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeExportTasks@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is null when there are no more results to return.
--
-- 'filters', 'describeExportTasks_filters' - One or more filters.
--
-- -   @AgentId@ - ID of the agent whose collected data will be exported
--
-- 'maxResults', 'describeExportTasks_maxResults' - The maximum number of volume results returned by @DescribeExportTasks@
-- in paginated output. When this parameter is used, @DescribeExportTasks@
-- only returns @maxResults@ results in a single page along with a
-- @nextToken@ response element.
newDescribeExportTasks ::
  DescribeExportTasks
newDescribeExportTasks =
  DescribeExportTasks'
    { exportIds = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | One or more unique identifiers used to query the status of an export
-- request.
describeExportTasks_exportIds :: Lens.Lens' DescribeExportTasks (Prelude.Maybe [Prelude.Text])
describeExportTasks_exportIds = Lens.lens (\DescribeExportTasks' {exportIds} -> exportIds) (\s@DescribeExportTasks' {} a -> s {exportIds = a} :: DescribeExportTasks) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeExportTasks@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is null when there are no more results to return.
describeExportTasks_nextToken :: Lens.Lens' DescribeExportTasks (Prelude.Maybe Prelude.Text)
describeExportTasks_nextToken = Lens.lens (\DescribeExportTasks' {nextToken} -> nextToken) (\s@DescribeExportTasks' {} a -> s {nextToken = a} :: DescribeExportTasks)

-- | One or more filters.
--
-- -   @AgentId@ - ID of the agent whose collected data will be exported
describeExportTasks_filters :: Lens.Lens' DescribeExportTasks (Prelude.Maybe [ExportFilter])
describeExportTasks_filters = Lens.lens (\DescribeExportTasks' {filters} -> filters) (\s@DescribeExportTasks' {} a -> s {filters = a} :: DescribeExportTasks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of volume results returned by @DescribeExportTasks@
-- in paginated output. When this parameter is used, @DescribeExportTasks@
-- only returns @maxResults@ results in a single page along with a
-- @nextToken@ response element.
describeExportTasks_maxResults :: Lens.Lens' DescribeExportTasks (Prelude.Maybe Prelude.Int)
describeExportTasks_maxResults = Lens.lens (\DescribeExportTasks' {maxResults} -> maxResults) (\s@DescribeExportTasks' {} a -> s {maxResults = a} :: DescribeExportTasks)

instance Core.AWSPager DescribeExportTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeExportTasksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeExportTasksResponse_exportsInfo
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeExportTasks_nextToken
          Lens..~ rs
          Lens.^? describeExportTasksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeExportTasks where
  type
    AWSResponse DescribeExportTasks =
      DescribeExportTasksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExportTasksResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "exportsInfo" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExportTasks where
  hashWithSalt _salt DescribeExportTasks' {..} =
    _salt `Prelude.hashWithSalt` exportIds
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeExportTasks where
  rnf DescribeExportTasks' {..} =
    Prelude.rnf exportIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeExportTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DescribeExportTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeExportTasks where
  toJSON DescribeExportTasks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("exportIds" Core..=) Prelude.<$> exportIds,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeExportTasks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeExportTasks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | The @nextToken@ value to include in a future @DescribeExportTasks@
    -- request. When the results of a @DescribeExportTasks@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains one or more sets of export request details. When the status of
    -- a request is @SUCCEEDED@, the response includes a URL for an Amazon S3
    -- bucket where you can view the data in a CSV file.
    exportsInfo :: Prelude.Maybe [ExportInfo],
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
  Prelude.Int ->
  DescribeExportTasksResponse
newDescribeExportTasksResponse pHttpStatus_ =
  DescribeExportTasksResponse'
    { nextToken =
        Prelude.Nothing,
      exportsInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeExportTasks@
-- request. When the results of a @DescribeExportTasks@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is null when there are no more results to return.
describeExportTasksResponse_nextToken :: Lens.Lens' DescribeExportTasksResponse (Prelude.Maybe Prelude.Text)
describeExportTasksResponse_nextToken = Lens.lens (\DescribeExportTasksResponse' {nextToken} -> nextToken) (\s@DescribeExportTasksResponse' {} a -> s {nextToken = a} :: DescribeExportTasksResponse)

-- | Contains one or more sets of export request details. When the status of
-- a request is @SUCCEEDED@, the response includes a URL for an Amazon S3
-- bucket where you can view the data in a CSV file.
describeExportTasksResponse_exportsInfo :: Lens.Lens' DescribeExportTasksResponse (Prelude.Maybe [ExportInfo])
describeExportTasksResponse_exportsInfo = Lens.lens (\DescribeExportTasksResponse' {exportsInfo} -> exportsInfo) (\s@DescribeExportTasksResponse' {} a -> s {exportsInfo = a} :: DescribeExportTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeExportTasksResponse_httpStatus :: Lens.Lens' DescribeExportTasksResponse Prelude.Int
describeExportTasksResponse_httpStatus = Lens.lens (\DescribeExportTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportTasksResponse)

instance Prelude.NFData DescribeExportTasksResponse where
  rnf DescribeExportTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf exportsInfo
      `Prelude.seq` Prelude.rnf httpStatus

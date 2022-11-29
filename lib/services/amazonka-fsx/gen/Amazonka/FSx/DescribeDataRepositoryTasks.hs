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
-- Module      : Amazonka.FSx.DescribeDataRepositoryTasks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of specific Amazon FSx for Lustre or Amazon File
-- Cache data repository tasks, if one or more @TaskIds@ values are
-- provided in the request, or if filters are used in the request. You can
-- use filters to narrow the response to include just tasks for specific
-- file systems or caches, or tasks in a specific lifecycle state.
-- Otherwise, it returns all data repository tasks owned by your Amazon Web
-- Services account in the Amazon Web Services Region of the endpoint that
-- you\'re calling.
--
-- When retrieving all tasks, you can paginate the response by using the
-- optional @MaxResults@ parameter to limit the number of tasks returned in
-- a response. If more tasks remain, a @NextToken@ value is returned in the
-- response. In this case, send a later request with the @NextToken@
-- request parameter set to the value of @NextToken@ from the last
-- response.
module Amazonka.FSx.DescribeDataRepositoryTasks
  ( -- * Creating a Request
    DescribeDataRepositoryTasks (..),
    newDescribeDataRepositoryTasks,

    -- * Request Lenses
    describeDataRepositoryTasks_nextToken,
    describeDataRepositoryTasks_filters,
    describeDataRepositoryTasks_taskIds,
    describeDataRepositoryTasks_maxResults,

    -- * Destructuring the Response
    DescribeDataRepositoryTasksResponse (..),
    newDescribeDataRepositoryTasksResponse,

    -- * Response Lenses
    describeDataRepositoryTasksResponse_nextToken,
    describeDataRepositoryTasksResponse_dataRepositoryTasks,
    describeDataRepositoryTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataRepositoryTasks' smart constructor.
data DescribeDataRepositoryTasks = DescribeDataRepositoryTasks'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) You can use filters to narrow the
    -- @DescribeDataRepositoryTasks@ response to include just tasks for
    -- specific file systems, or tasks in a specific lifecycle state.
    filters :: Prelude.Maybe [DataRepositoryTaskFilter],
    -- | (Optional) IDs of the tasks whose descriptions you want to retrieve
    -- (String).
    taskIds :: Prelude.Maybe [Prelude.Text],
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataRepositoryTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDataRepositoryTasks_nextToken' - Undocumented member.
--
-- 'filters', 'describeDataRepositoryTasks_filters' - (Optional) You can use filters to narrow the
-- @DescribeDataRepositoryTasks@ response to include just tasks for
-- specific file systems, or tasks in a specific lifecycle state.
--
-- 'taskIds', 'describeDataRepositoryTasks_taskIds' - (Optional) IDs of the tasks whose descriptions you want to retrieve
-- (String).
--
-- 'maxResults', 'describeDataRepositoryTasks_maxResults' - Undocumented member.
newDescribeDataRepositoryTasks ::
  DescribeDataRepositoryTasks
newDescribeDataRepositoryTasks =
  DescribeDataRepositoryTasks'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      taskIds = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Undocumented member.
describeDataRepositoryTasks_nextToken :: Lens.Lens' DescribeDataRepositoryTasks (Prelude.Maybe Prelude.Text)
describeDataRepositoryTasks_nextToken = Lens.lens (\DescribeDataRepositoryTasks' {nextToken} -> nextToken) (\s@DescribeDataRepositoryTasks' {} a -> s {nextToken = a} :: DescribeDataRepositoryTasks)

-- | (Optional) You can use filters to narrow the
-- @DescribeDataRepositoryTasks@ response to include just tasks for
-- specific file systems, or tasks in a specific lifecycle state.
describeDataRepositoryTasks_filters :: Lens.Lens' DescribeDataRepositoryTasks (Prelude.Maybe [DataRepositoryTaskFilter])
describeDataRepositoryTasks_filters = Lens.lens (\DescribeDataRepositoryTasks' {filters} -> filters) (\s@DescribeDataRepositoryTasks' {} a -> s {filters = a} :: DescribeDataRepositoryTasks) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) IDs of the tasks whose descriptions you want to retrieve
-- (String).
describeDataRepositoryTasks_taskIds :: Lens.Lens' DescribeDataRepositoryTasks (Prelude.Maybe [Prelude.Text])
describeDataRepositoryTasks_taskIds = Lens.lens (\DescribeDataRepositoryTasks' {taskIds} -> taskIds) (\s@DescribeDataRepositoryTasks' {} a -> s {taskIds = a} :: DescribeDataRepositoryTasks) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeDataRepositoryTasks_maxResults :: Lens.Lens' DescribeDataRepositoryTasks (Prelude.Maybe Prelude.Natural)
describeDataRepositoryTasks_maxResults = Lens.lens (\DescribeDataRepositoryTasks' {maxResults} -> maxResults) (\s@DescribeDataRepositoryTasks' {} a -> s {maxResults = a} :: DescribeDataRepositoryTasks)

instance Core.AWSRequest DescribeDataRepositoryTasks where
  type
    AWSResponse DescribeDataRepositoryTasks =
      DescribeDataRepositoryTasksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataRepositoryTasksResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DataRepositoryTasks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataRepositoryTasks where
  hashWithSalt _salt DescribeDataRepositoryTasks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` taskIds
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeDataRepositoryTasks where
  rnf DescribeDataRepositoryTasks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf taskIds
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeDataRepositoryTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSimbaAPIService_v20180301.DescribeDataRepositoryTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDataRepositoryTasks where
  toJSON DescribeDataRepositoryTasks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("TaskIds" Core..=) Prelude.<$> taskIds,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeDataRepositoryTasks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDataRepositoryTasks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataRepositoryTasksResponse' smart constructor.
data DescribeDataRepositoryTasksResponse = DescribeDataRepositoryTasksResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The collection of data repository task descriptions returned.
    dataRepositoryTasks :: Prelude.Maybe [DataRepositoryTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataRepositoryTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDataRepositoryTasksResponse_nextToken' - Undocumented member.
--
-- 'dataRepositoryTasks', 'describeDataRepositoryTasksResponse_dataRepositoryTasks' - The collection of data repository task descriptions returned.
--
-- 'httpStatus', 'describeDataRepositoryTasksResponse_httpStatus' - The response's http status code.
newDescribeDataRepositoryTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDataRepositoryTasksResponse
newDescribeDataRepositoryTasksResponse pHttpStatus_ =
  DescribeDataRepositoryTasksResponse'
    { nextToken =
        Prelude.Nothing,
      dataRepositoryTasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeDataRepositoryTasksResponse_nextToken :: Lens.Lens' DescribeDataRepositoryTasksResponse (Prelude.Maybe Prelude.Text)
describeDataRepositoryTasksResponse_nextToken = Lens.lens (\DescribeDataRepositoryTasksResponse' {nextToken} -> nextToken) (\s@DescribeDataRepositoryTasksResponse' {} a -> s {nextToken = a} :: DescribeDataRepositoryTasksResponse)

-- | The collection of data repository task descriptions returned.
describeDataRepositoryTasksResponse_dataRepositoryTasks :: Lens.Lens' DescribeDataRepositoryTasksResponse (Prelude.Maybe [DataRepositoryTask])
describeDataRepositoryTasksResponse_dataRepositoryTasks = Lens.lens (\DescribeDataRepositoryTasksResponse' {dataRepositoryTasks} -> dataRepositoryTasks) (\s@DescribeDataRepositoryTasksResponse' {} a -> s {dataRepositoryTasks = a} :: DescribeDataRepositoryTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDataRepositoryTasksResponse_httpStatus :: Lens.Lens' DescribeDataRepositoryTasksResponse Prelude.Int
describeDataRepositoryTasksResponse_httpStatus = Lens.lens (\DescribeDataRepositoryTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeDataRepositoryTasksResponse' {} a -> s {httpStatus = a} :: DescribeDataRepositoryTasksResponse)

instance
  Prelude.NFData
    DescribeDataRepositoryTasksResponse
  where
  rnf DescribeDataRepositoryTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf dataRepositoryTasks
      `Prelude.seq` Prelude.rnf httpStatus

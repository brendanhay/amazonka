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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeDataRepositoryTasks_filters,
    describeDataRepositoryTasks_maxResults,
    describeDataRepositoryTasks_nextToken,
    describeDataRepositoryTasks_taskIds,

    -- * Destructuring the Response
    DescribeDataRepositoryTasksResponse (..),
    newDescribeDataRepositoryTasksResponse,

    -- * Response Lenses
    describeDataRepositoryTasksResponse_dataRepositoryTasks,
    describeDataRepositoryTasksResponse_nextToken,
    describeDataRepositoryTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataRepositoryTasks' smart constructor.
data DescribeDataRepositoryTasks = DescribeDataRepositoryTasks'
  { -- | (Optional) You can use filters to narrow the
    -- @DescribeDataRepositoryTasks@ response to include just tasks for
    -- specific file systems, or tasks in a specific lifecycle state.
    filters :: Prelude.Maybe [DataRepositoryTaskFilter],
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) IDs of the tasks whose descriptions you want to retrieve
    -- (String).
    taskIds :: Prelude.Maybe [Prelude.Text]
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
-- 'filters', 'describeDataRepositoryTasks_filters' - (Optional) You can use filters to narrow the
-- @DescribeDataRepositoryTasks@ response to include just tasks for
-- specific file systems, or tasks in a specific lifecycle state.
--
-- 'maxResults', 'describeDataRepositoryTasks_maxResults' - Undocumented member.
--
-- 'nextToken', 'describeDataRepositoryTasks_nextToken' - Undocumented member.
--
-- 'taskIds', 'describeDataRepositoryTasks_taskIds' - (Optional) IDs of the tasks whose descriptions you want to retrieve
-- (String).
newDescribeDataRepositoryTasks ::
  DescribeDataRepositoryTasks
newDescribeDataRepositoryTasks =
  DescribeDataRepositoryTasks'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      taskIds = Prelude.Nothing
    }

-- | (Optional) You can use filters to narrow the
-- @DescribeDataRepositoryTasks@ response to include just tasks for
-- specific file systems, or tasks in a specific lifecycle state.
describeDataRepositoryTasks_filters :: Lens.Lens' DescribeDataRepositoryTasks (Prelude.Maybe [DataRepositoryTaskFilter])
describeDataRepositoryTasks_filters = Lens.lens (\DescribeDataRepositoryTasks' {filters} -> filters) (\s@DescribeDataRepositoryTasks' {} a -> s {filters = a} :: DescribeDataRepositoryTasks) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeDataRepositoryTasks_maxResults :: Lens.Lens' DescribeDataRepositoryTasks (Prelude.Maybe Prelude.Natural)
describeDataRepositoryTasks_maxResults = Lens.lens (\DescribeDataRepositoryTasks' {maxResults} -> maxResults) (\s@DescribeDataRepositoryTasks' {} a -> s {maxResults = a} :: DescribeDataRepositoryTasks)

-- | Undocumented member.
describeDataRepositoryTasks_nextToken :: Lens.Lens' DescribeDataRepositoryTasks (Prelude.Maybe Prelude.Text)
describeDataRepositoryTasks_nextToken = Lens.lens (\DescribeDataRepositoryTasks' {nextToken} -> nextToken) (\s@DescribeDataRepositoryTasks' {} a -> s {nextToken = a} :: DescribeDataRepositoryTasks)

-- | (Optional) IDs of the tasks whose descriptions you want to retrieve
-- (String).
describeDataRepositoryTasks_taskIds :: Lens.Lens' DescribeDataRepositoryTasks (Prelude.Maybe [Prelude.Text])
describeDataRepositoryTasks_taskIds = Lens.lens (\DescribeDataRepositoryTasks' {taskIds} -> taskIds) (\s@DescribeDataRepositoryTasks' {} a -> s {taskIds = a} :: DescribeDataRepositoryTasks) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> ( x
                            Data..?> "DataRepositoryTasks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataRepositoryTasks where
  hashWithSalt _salt DescribeDataRepositoryTasks' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` taskIds

instance Prelude.NFData DescribeDataRepositoryTasks where
  rnf DescribeDataRepositoryTasks' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf taskIds

instance Data.ToHeaders DescribeDataRepositoryTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DescribeDataRepositoryTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDataRepositoryTasks where
  toJSON DescribeDataRepositoryTasks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("TaskIds" Data..=) Prelude.<$> taskIds
          ]
      )

instance Data.ToPath DescribeDataRepositoryTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDataRepositoryTasks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataRepositoryTasksResponse' smart constructor.
data DescribeDataRepositoryTasksResponse = DescribeDataRepositoryTasksResponse'
  { -- | The collection of data repository task descriptions returned.
    dataRepositoryTasks :: Prelude.Maybe [DataRepositoryTask],
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'dataRepositoryTasks', 'describeDataRepositoryTasksResponse_dataRepositoryTasks' - The collection of data repository task descriptions returned.
--
-- 'nextToken', 'describeDataRepositoryTasksResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeDataRepositoryTasksResponse_httpStatus' - The response's http status code.
newDescribeDataRepositoryTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDataRepositoryTasksResponse
newDescribeDataRepositoryTasksResponse pHttpStatus_ =
  DescribeDataRepositoryTasksResponse'
    { dataRepositoryTasks =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The collection of data repository task descriptions returned.
describeDataRepositoryTasksResponse_dataRepositoryTasks :: Lens.Lens' DescribeDataRepositoryTasksResponse (Prelude.Maybe [DataRepositoryTask])
describeDataRepositoryTasksResponse_dataRepositoryTasks = Lens.lens (\DescribeDataRepositoryTasksResponse' {dataRepositoryTasks} -> dataRepositoryTasks) (\s@DescribeDataRepositoryTasksResponse' {} a -> s {dataRepositoryTasks = a} :: DescribeDataRepositoryTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeDataRepositoryTasksResponse_nextToken :: Lens.Lens' DescribeDataRepositoryTasksResponse (Prelude.Maybe Prelude.Text)
describeDataRepositoryTasksResponse_nextToken = Lens.lens (\DescribeDataRepositoryTasksResponse' {nextToken} -> nextToken) (\s@DescribeDataRepositoryTasksResponse' {} a -> s {nextToken = a} :: DescribeDataRepositoryTasksResponse)

-- | The response's http status code.
describeDataRepositoryTasksResponse_httpStatus :: Lens.Lens' DescribeDataRepositoryTasksResponse Prelude.Int
describeDataRepositoryTasksResponse_httpStatus = Lens.lens (\DescribeDataRepositoryTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeDataRepositoryTasksResponse' {} a -> s {httpStatus = a} :: DescribeDataRepositoryTasksResponse)

instance
  Prelude.NFData
    DescribeDataRepositoryTasksResponse
  where
  rnf DescribeDataRepositoryTasksResponse' {..} =
    Prelude.rnf dataRepositoryTasks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

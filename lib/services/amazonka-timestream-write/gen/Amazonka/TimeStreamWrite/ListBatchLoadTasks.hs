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
-- Module      : Amazonka.TimeStreamWrite.ListBatchLoadTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of batch load tasks, along with the name, status, when
-- the task is resumable until, and other details. See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.list-batch-load-tasks.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.ListBatchLoadTasks
  ( -- * Creating a Request
    ListBatchLoadTasks (..),
    newListBatchLoadTasks,

    -- * Request Lenses
    listBatchLoadTasks_maxResults,
    listBatchLoadTasks_nextToken,
    listBatchLoadTasks_taskStatus,

    -- * Destructuring the Response
    ListBatchLoadTasksResponse (..),
    newListBatchLoadTasksResponse,

    -- * Response Lenses
    listBatchLoadTasksResponse_batchLoadTasks,
    listBatchLoadTasksResponse_nextToken,
    listBatchLoadTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newListBatchLoadTasks' smart constructor.
data ListBatchLoadTasks = ListBatchLoadTasks'
  { -- | The total number of items to return in the output. If the total number
    -- of items available is more than the value specified, a NextToken is
    -- provided in the output. To resume pagination, provide the NextToken
    -- value as argument of a subsequent API invocation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Status of the batch load task.
    taskStatus :: Prelude.Maybe BatchLoadStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBatchLoadTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBatchLoadTasks_maxResults' - The total number of items to return in the output. If the total number
-- of items available is more than the value specified, a NextToken is
-- provided in the output. To resume pagination, provide the NextToken
-- value as argument of a subsequent API invocation.
--
-- 'nextToken', 'listBatchLoadTasks_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'taskStatus', 'listBatchLoadTasks_taskStatus' - Status of the batch load task.
newListBatchLoadTasks ::
  ListBatchLoadTasks
newListBatchLoadTasks =
  ListBatchLoadTasks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      taskStatus = Prelude.Nothing
    }

-- | The total number of items to return in the output. If the total number
-- of items available is more than the value specified, a NextToken is
-- provided in the output. To resume pagination, provide the NextToken
-- value as argument of a subsequent API invocation.
listBatchLoadTasks_maxResults :: Lens.Lens' ListBatchLoadTasks (Prelude.Maybe Prelude.Natural)
listBatchLoadTasks_maxResults = Lens.lens (\ListBatchLoadTasks' {maxResults} -> maxResults) (\s@ListBatchLoadTasks' {} a -> s {maxResults = a} :: ListBatchLoadTasks)

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listBatchLoadTasks_nextToken :: Lens.Lens' ListBatchLoadTasks (Prelude.Maybe Prelude.Text)
listBatchLoadTasks_nextToken = Lens.lens (\ListBatchLoadTasks' {nextToken} -> nextToken) (\s@ListBatchLoadTasks' {} a -> s {nextToken = a} :: ListBatchLoadTasks)

-- | Status of the batch load task.
listBatchLoadTasks_taskStatus :: Lens.Lens' ListBatchLoadTasks (Prelude.Maybe BatchLoadStatus)
listBatchLoadTasks_taskStatus = Lens.lens (\ListBatchLoadTasks' {taskStatus} -> taskStatus) (\s@ListBatchLoadTasks' {} a -> s {taskStatus = a} :: ListBatchLoadTasks)

instance Core.AWSRequest ListBatchLoadTasks where
  type
    AWSResponse ListBatchLoadTasks =
      ListBatchLoadTasksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBatchLoadTasksResponse'
            Prelude.<$> (x Data..?> "BatchLoadTasks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBatchLoadTasks where
  hashWithSalt _salt ListBatchLoadTasks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` taskStatus

instance Prelude.NFData ListBatchLoadTasks where
  rnf ListBatchLoadTasks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf taskStatus

instance Data.ToHeaders ListBatchLoadTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.ListBatchLoadTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBatchLoadTasks where
  toJSON ListBatchLoadTasks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("TaskStatus" Data..=) Prelude.<$> taskStatus
          ]
      )

instance Data.ToPath ListBatchLoadTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery ListBatchLoadTasks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBatchLoadTasksResponse' smart constructor.
data ListBatchLoadTasksResponse = ListBatchLoadTasksResponse'
  { -- | A list of batch load task details.
    batchLoadTasks :: Prelude.Maybe [BatchLoadTask],
    -- | A token to specify where to start paginating. Provide the next
    -- ListBatchLoadTasksRequest.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBatchLoadTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchLoadTasks', 'listBatchLoadTasksResponse_batchLoadTasks' - A list of batch load task details.
--
-- 'nextToken', 'listBatchLoadTasksResponse_nextToken' - A token to specify where to start paginating. Provide the next
-- ListBatchLoadTasksRequest.
--
-- 'httpStatus', 'listBatchLoadTasksResponse_httpStatus' - The response's http status code.
newListBatchLoadTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBatchLoadTasksResponse
newListBatchLoadTasksResponse pHttpStatus_ =
  ListBatchLoadTasksResponse'
    { batchLoadTasks =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of batch load task details.
listBatchLoadTasksResponse_batchLoadTasks :: Lens.Lens' ListBatchLoadTasksResponse (Prelude.Maybe [BatchLoadTask])
listBatchLoadTasksResponse_batchLoadTasks = Lens.lens (\ListBatchLoadTasksResponse' {batchLoadTasks} -> batchLoadTasks) (\s@ListBatchLoadTasksResponse' {} a -> s {batchLoadTasks = a} :: ListBatchLoadTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token to specify where to start paginating. Provide the next
-- ListBatchLoadTasksRequest.
listBatchLoadTasksResponse_nextToken :: Lens.Lens' ListBatchLoadTasksResponse (Prelude.Maybe Prelude.Text)
listBatchLoadTasksResponse_nextToken = Lens.lens (\ListBatchLoadTasksResponse' {nextToken} -> nextToken) (\s@ListBatchLoadTasksResponse' {} a -> s {nextToken = a} :: ListBatchLoadTasksResponse)

-- | The response's http status code.
listBatchLoadTasksResponse_httpStatus :: Lens.Lens' ListBatchLoadTasksResponse Prelude.Int
listBatchLoadTasksResponse_httpStatus = Lens.lens (\ListBatchLoadTasksResponse' {httpStatus} -> httpStatus) (\s@ListBatchLoadTasksResponse' {} a -> s {httpStatus = a} :: ListBatchLoadTasksResponse)

instance Prelude.NFData ListBatchLoadTasksResponse where
  rnf ListBatchLoadTasksResponse' {..} =
    Prelude.rnf batchLoadTasks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

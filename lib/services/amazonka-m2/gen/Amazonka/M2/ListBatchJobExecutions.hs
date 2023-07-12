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
-- Module      : Amazonka.M2.ListBatchJobExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists historical, current, and scheduled batch job executions for a
-- specific application.
--
-- This operation returns paginated results.
module Amazonka.M2.ListBatchJobExecutions
  ( -- * Creating a Request
    ListBatchJobExecutions (..),
    newListBatchJobExecutions,

    -- * Request Lenses
    listBatchJobExecutions_executionIds,
    listBatchJobExecutions_jobName,
    listBatchJobExecutions_maxResults,
    listBatchJobExecutions_nextToken,
    listBatchJobExecutions_startedAfter,
    listBatchJobExecutions_startedBefore,
    listBatchJobExecutions_status,
    listBatchJobExecutions_applicationId,

    -- * Destructuring the Response
    ListBatchJobExecutionsResponse (..),
    newListBatchJobExecutionsResponse,

    -- * Response Lenses
    listBatchJobExecutionsResponse_nextToken,
    listBatchJobExecutionsResponse_httpStatus,
    listBatchJobExecutionsResponse_batchJobExecutions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBatchJobExecutions' smart constructor.
data ListBatchJobExecutions = ListBatchJobExecutions'
  { -- | The unique identifier of each batch job execution.
    executionIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of each batch job execution.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of batch job executions to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token to control the number of batch job executions
    -- displayed in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time after which the batch job executions started.
    startedAfter :: Prelude.Maybe Data.POSIX,
    -- | The time before the batch job executions started.
    startedBefore :: Prelude.Maybe Data.POSIX,
    -- | The status of the batch job executions.
    status :: Prelude.Maybe BatchJobExecutionStatus,
    -- | The unique identifier of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBatchJobExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionIds', 'listBatchJobExecutions_executionIds' - The unique identifier of each batch job execution.
--
-- 'jobName', 'listBatchJobExecutions_jobName' - The name of each batch job execution.
--
-- 'maxResults', 'listBatchJobExecutions_maxResults' - The maximum number of batch job executions to return.
--
-- 'nextToken', 'listBatchJobExecutions_nextToken' - A pagination token to control the number of batch job executions
-- displayed in the list.
--
-- 'startedAfter', 'listBatchJobExecutions_startedAfter' - The time after which the batch job executions started.
--
-- 'startedBefore', 'listBatchJobExecutions_startedBefore' - The time before the batch job executions started.
--
-- 'status', 'listBatchJobExecutions_status' - The status of the batch job executions.
--
-- 'applicationId', 'listBatchJobExecutions_applicationId' - The unique identifier of the application.
newListBatchJobExecutions ::
  -- | 'applicationId'
  Prelude.Text ->
  ListBatchJobExecutions
newListBatchJobExecutions pApplicationId_ =
  ListBatchJobExecutions'
    { executionIds =
        Prelude.Nothing,
      jobName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startedAfter = Prelude.Nothing,
      startedBefore = Prelude.Nothing,
      status = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The unique identifier of each batch job execution.
listBatchJobExecutions_executionIds :: Lens.Lens' ListBatchJobExecutions (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listBatchJobExecutions_executionIds = Lens.lens (\ListBatchJobExecutions' {executionIds} -> executionIds) (\s@ListBatchJobExecutions' {} a -> s {executionIds = a} :: ListBatchJobExecutions) Prelude.. Lens.mapping Lens.coerced

-- | The name of each batch job execution.
listBatchJobExecutions_jobName :: Lens.Lens' ListBatchJobExecutions (Prelude.Maybe Prelude.Text)
listBatchJobExecutions_jobName = Lens.lens (\ListBatchJobExecutions' {jobName} -> jobName) (\s@ListBatchJobExecutions' {} a -> s {jobName = a} :: ListBatchJobExecutions)

-- | The maximum number of batch job executions to return.
listBatchJobExecutions_maxResults :: Lens.Lens' ListBatchJobExecutions (Prelude.Maybe Prelude.Natural)
listBatchJobExecutions_maxResults = Lens.lens (\ListBatchJobExecutions' {maxResults} -> maxResults) (\s@ListBatchJobExecutions' {} a -> s {maxResults = a} :: ListBatchJobExecutions)

-- | A pagination token to control the number of batch job executions
-- displayed in the list.
listBatchJobExecutions_nextToken :: Lens.Lens' ListBatchJobExecutions (Prelude.Maybe Prelude.Text)
listBatchJobExecutions_nextToken = Lens.lens (\ListBatchJobExecutions' {nextToken} -> nextToken) (\s@ListBatchJobExecutions' {} a -> s {nextToken = a} :: ListBatchJobExecutions)

-- | The time after which the batch job executions started.
listBatchJobExecutions_startedAfter :: Lens.Lens' ListBatchJobExecutions (Prelude.Maybe Prelude.UTCTime)
listBatchJobExecutions_startedAfter = Lens.lens (\ListBatchJobExecutions' {startedAfter} -> startedAfter) (\s@ListBatchJobExecutions' {} a -> s {startedAfter = a} :: ListBatchJobExecutions) Prelude.. Lens.mapping Data._Time

-- | The time before the batch job executions started.
listBatchJobExecutions_startedBefore :: Lens.Lens' ListBatchJobExecutions (Prelude.Maybe Prelude.UTCTime)
listBatchJobExecutions_startedBefore = Lens.lens (\ListBatchJobExecutions' {startedBefore} -> startedBefore) (\s@ListBatchJobExecutions' {} a -> s {startedBefore = a} :: ListBatchJobExecutions) Prelude.. Lens.mapping Data._Time

-- | The status of the batch job executions.
listBatchJobExecutions_status :: Lens.Lens' ListBatchJobExecutions (Prelude.Maybe BatchJobExecutionStatus)
listBatchJobExecutions_status = Lens.lens (\ListBatchJobExecutions' {status} -> status) (\s@ListBatchJobExecutions' {} a -> s {status = a} :: ListBatchJobExecutions)

-- | The unique identifier of the application.
listBatchJobExecutions_applicationId :: Lens.Lens' ListBatchJobExecutions Prelude.Text
listBatchJobExecutions_applicationId = Lens.lens (\ListBatchJobExecutions' {applicationId} -> applicationId) (\s@ListBatchJobExecutions' {} a -> s {applicationId = a} :: ListBatchJobExecutions)

instance Core.AWSPager ListBatchJobExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBatchJobExecutionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listBatchJobExecutionsResponse_batchJobExecutions
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listBatchJobExecutions_nextToken
          Lens..~ rs
          Lens.^? listBatchJobExecutionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListBatchJobExecutions where
  type
    AWSResponse ListBatchJobExecutions =
      ListBatchJobExecutionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBatchJobExecutionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "batchJobExecutions"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListBatchJobExecutions where
  hashWithSalt _salt ListBatchJobExecutions' {..} =
    _salt
      `Prelude.hashWithSalt` executionIds
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startedAfter
      `Prelude.hashWithSalt` startedBefore
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListBatchJobExecutions where
  rnf ListBatchJobExecutions' {..} =
    Prelude.rnf executionIds
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startedAfter
      `Prelude.seq` Prelude.rnf startedBefore
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders ListBatchJobExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBatchJobExecutions where
  toPath ListBatchJobExecutions' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/batch-job-executions"
      ]

instance Data.ToQuery ListBatchJobExecutions where
  toQuery ListBatchJobExecutions' {..} =
    Prelude.mconcat
      [ "executionIds"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> executionIds),
        "jobName" Data.=: jobName,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "startedAfter" Data.=: startedAfter,
        "startedBefore" Data.=: startedBefore,
        "status" Data.=: status
      ]

-- | /See:/ 'newListBatchJobExecutionsResponse' smart constructor.
data ListBatchJobExecutionsResponse = ListBatchJobExecutionsResponse'
  { -- | A pagination token that\'s returned when the response doesn\'t contain
    -- all batch job executions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns a list of batch job executions for an application.
    batchJobExecutions :: [BatchJobExecutionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBatchJobExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBatchJobExecutionsResponse_nextToken' - A pagination token that\'s returned when the response doesn\'t contain
-- all batch job executions.
--
-- 'httpStatus', 'listBatchJobExecutionsResponse_httpStatus' - The response's http status code.
--
-- 'batchJobExecutions', 'listBatchJobExecutionsResponse_batchJobExecutions' - Returns a list of batch job executions for an application.
newListBatchJobExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBatchJobExecutionsResponse
newListBatchJobExecutionsResponse pHttpStatus_ =
  ListBatchJobExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      batchJobExecutions = Prelude.mempty
    }

-- | A pagination token that\'s returned when the response doesn\'t contain
-- all batch job executions.
listBatchJobExecutionsResponse_nextToken :: Lens.Lens' ListBatchJobExecutionsResponse (Prelude.Maybe Prelude.Text)
listBatchJobExecutionsResponse_nextToken = Lens.lens (\ListBatchJobExecutionsResponse' {nextToken} -> nextToken) (\s@ListBatchJobExecutionsResponse' {} a -> s {nextToken = a} :: ListBatchJobExecutionsResponse)

-- | The response's http status code.
listBatchJobExecutionsResponse_httpStatus :: Lens.Lens' ListBatchJobExecutionsResponse Prelude.Int
listBatchJobExecutionsResponse_httpStatus = Lens.lens (\ListBatchJobExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListBatchJobExecutionsResponse' {} a -> s {httpStatus = a} :: ListBatchJobExecutionsResponse)

-- | Returns a list of batch job executions for an application.
listBatchJobExecutionsResponse_batchJobExecutions :: Lens.Lens' ListBatchJobExecutionsResponse [BatchJobExecutionSummary]
listBatchJobExecutionsResponse_batchJobExecutions = Lens.lens (\ListBatchJobExecutionsResponse' {batchJobExecutions} -> batchJobExecutions) (\s@ListBatchJobExecutionsResponse' {} a -> s {batchJobExecutions = a} :: ListBatchJobExecutionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListBatchJobExecutionsResponse
  where
  rnf ListBatchJobExecutionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf batchJobExecutions

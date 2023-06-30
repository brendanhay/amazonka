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
-- Module      : Amazonka.IoT.ListJobExecutionsForJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for a job.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListJobExecutionsForJob>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListJobExecutionsForJob
  ( -- * Creating a Request
    ListJobExecutionsForJob (..),
    newListJobExecutionsForJob,

    -- * Request Lenses
    listJobExecutionsForJob_maxResults,
    listJobExecutionsForJob_nextToken,
    listJobExecutionsForJob_status,
    listJobExecutionsForJob_jobId,

    -- * Destructuring the Response
    ListJobExecutionsForJobResponse (..),
    newListJobExecutionsForJobResponse,

    -- * Response Lenses
    listJobExecutionsForJobResponse_executionSummaries,
    listJobExecutionsForJobResponse_nextToken,
    listJobExecutionsForJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobExecutionsForJob' smart constructor.
data ListJobExecutionsForJob = ListJobExecutionsForJob'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the job.
    status :: Prelude.Maybe JobExecutionStatus,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobExecutionsForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listJobExecutionsForJob_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listJobExecutionsForJob_nextToken' - The token to retrieve the next set of results.
--
-- 'status', 'listJobExecutionsForJob_status' - The status of the job.
--
-- 'jobId', 'listJobExecutionsForJob_jobId' - The unique identifier you assigned to this job when it was created.
newListJobExecutionsForJob ::
  -- | 'jobId'
  Prelude.Text ->
  ListJobExecutionsForJob
newListJobExecutionsForJob pJobId_ =
  ListJobExecutionsForJob'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      jobId = pJobId_
    }

-- | The maximum number of results to be returned per request.
listJobExecutionsForJob_maxResults :: Lens.Lens' ListJobExecutionsForJob (Prelude.Maybe Prelude.Natural)
listJobExecutionsForJob_maxResults = Lens.lens (\ListJobExecutionsForJob' {maxResults} -> maxResults) (\s@ListJobExecutionsForJob' {} a -> s {maxResults = a} :: ListJobExecutionsForJob)

-- | The token to retrieve the next set of results.
listJobExecutionsForJob_nextToken :: Lens.Lens' ListJobExecutionsForJob (Prelude.Maybe Prelude.Text)
listJobExecutionsForJob_nextToken = Lens.lens (\ListJobExecutionsForJob' {nextToken} -> nextToken) (\s@ListJobExecutionsForJob' {} a -> s {nextToken = a} :: ListJobExecutionsForJob)

-- | The status of the job.
listJobExecutionsForJob_status :: Lens.Lens' ListJobExecutionsForJob (Prelude.Maybe JobExecutionStatus)
listJobExecutionsForJob_status = Lens.lens (\ListJobExecutionsForJob' {status} -> status) (\s@ListJobExecutionsForJob' {} a -> s {status = a} :: ListJobExecutionsForJob)

-- | The unique identifier you assigned to this job when it was created.
listJobExecutionsForJob_jobId :: Lens.Lens' ListJobExecutionsForJob Prelude.Text
listJobExecutionsForJob_jobId = Lens.lens (\ListJobExecutionsForJob' {jobId} -> jobId) (\s@ListJobExecutionsForJob' {} a -> s {jobId = a} :: ListJobExecutionsForJob)

instance Core.AWSPager ListJobExecutionsForJob where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForJobResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForJobResponse_executionSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listJobExecutionsForJob_nextToken
          Lens..~ rs
          Lens.^? listJobExecutionsForJobResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListJobExecutionsForJob where
  type
    AWSResponse ListJobExecutionsForJob =
      ListJobExecutionsForJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobExecutionsForJobResponse'
            Prelude.<$> ( x
                            Data..?> "executionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobExecutionsForJob where
  hashWithSalt _salt ListJobExecutionsForJob' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData ListJobExecutionsForJob where
  rnf ListJobExecutionsForJob' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders ListJobExecutionsForJob where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListJobExecutionsForJob where
  toPath ListJobExecutionsForJob' {..} =
    Prelude.mconcat
      ["/jobs/", Data.toBS jobId, "/things"]

instance Data.ToQuery ListJobExecutionsForJob where
  toQuery ListJobExecutionsForJob' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListJobExecutionsForJobResponse' smart constructor.
data ListJobExecutionsForJobResponse = ListJobExecutionsForJobResponse'
  { -- | A list of job execution summaries.
    executionSummaries :: Prelude.Maybe [JobExecutionSummaryForJob],
    -- | The token for the next set of results, or __null__ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobExecutionsForJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionSummaries', 'listJobExecutionsForJobResponse_executionSummaries' - A list of job execution summaries.
--
-- 'nextToken', 'listJobExecutionsForJobResponse_nextToken' - The token for the next set of results, or __null__ if there are no
-- additional results.
--
-- 'httpStatus', 'listJobExecutionsForJobResponse_httpStatus' - The response's http status code.
newListJobExecutionsForJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobExecutionsForJobResponse
newListJobExecutionsForJobResponse pHttpStatus_ =
  ListJobExecutionsForJobResponse'
    { executionSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of job execution summaries.
listJobExecutionsForJobResponse_executionSummaries :: Lens.Lens' ListJobExecutionsForJobResponse (Prelude.Maybe [JobExecutionSummaryForJob])
listJobExecutionsForJobResponse_executionSummaries = Lens.lens (\ListJobExecutionsForJobResponse' {executionSummaries} -> executionSummaries) (\s@ListJobExecutionsForJobResponse' {} a -> s {executionSummaries = a} :: ListJobExecutionsForJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or __null__ if there are no
-- additional results.
listJobExecutionsForJobResponse_nextToken :: Lens.Lens' ListJobExecutionsForJobResponse (Prelude.Maybe Prelude.Text)
listJobExecutionsForJobResponse_nextToken = Lens.lens (\ListJobExecutionsForJobResponse' {nextToken} -> nextToken) (\s@ListJobExecutionsForJobResponse' {} a -> s {nextToken = a} :: ListJobExecutionsForJobResponse)

-- | The response's http status code.
listJobExecutionsForJobResponse_httpStatus :: Lens.Lens' ListJobExecutionsForJobResponse Prelude.Int
listJobExecutionsForJobResponse_httpStatus = Lens.lens (\ListJobExecutionsForJobResponse' {httpStatus} -> httpStatus) (\s@ListJobExecutionsForJobResponse' {} a -> s {httpStatus = a} :: ListJobExecutionsForJobResponse)

instance
  Prelude.NFData
    ListJobExecutionsForJobResponse
  where
  rnf ListJobExecutionsForJobResponse' {..} =
    Prelude.rnf executionSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

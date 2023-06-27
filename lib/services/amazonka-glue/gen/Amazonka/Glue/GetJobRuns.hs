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
-- Module      : Amazonka.Glue.GetJobRuns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all runs of a given job definition.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetJobRuns
  ( -- * Creating a Request
    GetJobRuns (..),
    newGetJobRuns,

    -- * Request Lenses
    getJobRuns_maxResults,
    getJobRuns_nextToken,
    getJobRuns_jobName,

    -- * Destructuring the Response
    GetJobRunsResponse (..),
    newGetJobRunsResponse,

    -- * Response Lenses
    getJobRunsResponse_jobRuns,
    getJobRunsResponse_nextToken,
    getJobRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJobRuns' smart constructor.
data GetJobRuns = GetJobRuns'
  { -- | The maximum size of the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the job definition for which to retrieve all job runs.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getJobRuns_maxResults' - The maximum size of the response.
--
-- 'nextToken', 'getJobRuns_nextToken' - A continuation token, if this is a continuation call.
--
-- 'jobName', 'getJobRuns_jobName' - The name of the job definition for which to retrieve all job runs.
newGetJobRuns ::
  -- | 'jobName'
  Prelude.Text ->
  GetJobRuns
newGetJobRuns pJobName_ =
  GetJobRuns'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      jobName = pJobName_
    }

-- | The maximum size of the response.
getJobRuns_maxResults :: Lens.Lens' GetJobRuns (Prelude.Maybe Prelude.Natural)
getJobRuns_maxResults = Lens.lens (\GetJobRuns' {maxResults} -> maxResults) (\s@GetJobRuns' {} a -> s {maxResults = a} :: GetJobRuns)

-- | A continuation token, if this is a continuation call.
getJobRuns_nextToken :: Lens.Lens' GetJobRuns (Prelude.Maybe Prelude.Text)
getJobRuns_nextToken = Lens.lens (\GetJobRuns' {nextToken} -> nextToken) (\s@GetJobRuns' {} a -> s {nextToken = a} :: GetJobRuns)

-- | The name of the job definition for which to retrieve all job runs.
getJobRuns_jobName :: Lens.Lens' GetJobRuns Prelude.Text
getJobRuns_jobName = Lens.lens (\GetJobRuns' {jobName} -> jobName) (\s@GetJobRuns' {} a -> s {jobName = a} :: GetJobRuns)

instance Core.AWSPager GetJobRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getJobRunsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getJobRunsResponse_jobRuns
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getJobRuns_nextToken
          Lens..~ rs
          Lens.^? getJobRunsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetJobRuns where
  type AWSResponse GetJobRuns = GetJobRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobRunsResponse'
            Prelude.<$> (x Data..?> "JobRuns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobRuns where
  hashWithSalt _salt GetJobRuns' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` jobName

instance Prelude.NFData GetJobRuns where
  rnf GetJobRuns' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobName

instance Data.ToHeaders GetJobRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetJobRuns" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetJobRuns where
  toJSON GetJobRuns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("JobName" Data..= jobName)
          ]
      )

instance Data.ToPath GetJobRuns where
  toPath = Prelude.const "/"

instance Data.ToQuery GetJobRuns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobRunsResponse' smart constructor.
data GetJobRunsResponse = GetJobRunsResponse'
  { -- | A list of job-run metadata objects.
    jobRuns :: Prelude.Maybe [JobRun],
    -- | A continuation token, if not all requested job runs have been returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobRuns', 'getJobRunsResponse_jobRuns' - A list of job-run metadata objects.
--
-- 'nextToken', 'getJobRunsResponse_nextToken' - A continuation token, if not all requested job runs have been returned.
--
-- 'httpStatus', 'getJobRunsResponse_httpStatus' - The response's http status code.
newGetJobRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobRunsResponse
newGetJobRunsResponse pHttpStatus_ =
  GetJobRunsResponse'
    { jobRuns = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of job-run metadata objects.
getJobRunsResponse_jobRuns :: Lens.Lens' GetJobRunsResponse (Prelude.Maybe [JobRun])
getJobRunsResponse_jobRuns = Lens.lens (\GetJobRunsResponse' {jobRuns} -> jobRuns) (\s@GetJobRunsResponse' {} a -> s {jobRuns = a} :: GetJobRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if not all requested job runs have been returned.
getJobRunsResponse_nextToken :: Lens.Lens' GetJobRunsResponse (Prelude.Maybe Prelude.Text)
getJobRunsResponse_nextToken = Lens.lens (\GetJobRunsResponse' {nextToken} -> nextToken) (\s@GetJobRunsResponse' {} a -> s {nextToken = a} :: GetJobRunsResponse)

-- | The response's http status code.
getJobRunsResponse_httpStatus :: Lens.Lens' GetJobRunsResponse Prelude.Int
getJobRunsResponse_httpStatus = Lens.lens (\GetJobRunsResponse' {httpStatus} -> httpStatus) (\s@GetJobRunsResponse' {} a -> s {httpStatus = a} :: GetJobRunsResponse)

instance Prelude.NFData GetJobRunsResponse where
  rnf GetJobRunsResponse' {..} =
    Prelude.rnf jobRuns
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

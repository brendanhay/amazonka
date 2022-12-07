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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    getJobRuns_nextToken,
    getJobRuns_maxResults,
    getJobRuns_jobName,

    -- * Destructuring the Response
    GetJobRunsResponse (..),
    newGetJobRunsResponse,

    -- * Response Lenses
    getJobRunsResponse_nextToken,
    getJobRunsResponse_jobRuns,
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
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'getJobRuns_nextToken' - A continuation token, if this is a continuation call.
--
-- 'maxResults', 'getJobRuns_maxResults' - The maximum size of the response.
--
-- 'jobName', 'getJobRuns_jobName' - The name of the job definition for which to retrieve all job runs.
newGetJobRuns ::
  -- | 'jobName'
  Prelude.Text ->
  GetJobRuns
newGetJobRuns pJobName_ =
  GetJobRuns'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobName = pJobName_
    }

-- | A continuation token, if this is a continuation call.
getJobRuns_nextToken :: Lens.Lens' GetJobRuns (Prelude.Maybe Prelude.Text)
getJobRuns_nextToken = Lens.lens (\GetJobRuns' {nextToken} -> nextToken) (\s@GetJobRuns' {} a -> s {nextToken = a} :: GetJobRuns)

-- | The maximum size of the response.
getJobRuns_maxResults :: Lens.Lens' GetJobRuns (Prelude.Maybe Prelude.Natural)
getJobRuns_maxResults = Lens.lens (\GetJobRuns' {maxResults} -> maxResults) (\s@GetJobRuns' {} a -> s {maxResults = a} :: GetJobRuns)

-- | The name of the job definition for which to retrieve all job runs.
getJobRuns_jobName :: Lens.Lens' GetJobRuns Prelude.Text
getJobRuns_jobName = Lens.lens (\GetJobRuns' {jobName} -> jobName) (\s@GetJobRuns' {} a -> s {jobName = a} :: GetJobRuns)

instance Core.AWSPager GetJobRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getJobRunsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getJobRunsResponse_jobRuns Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getJobRuns_nextToken
          Lens..~ rs
          Lens.^? getJobRunsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetJobRuns where
  type AWSResponse GetJobRuns = GetJobRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobRunsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "JobRuns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobRuns where
  hashWithSalt _salt GetJobRuns' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` jobName

instance Prelude.NFData GetJobRuns where
  rnf GetJobRuns' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
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
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("JobName" Data..= jobName)
          ]
      )

instance Data.ToPath GetJobRuns where
  toPath = Prelude.const "/"

instance Data.ToQuery GetJobRuns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobRunsResponse' smart constructor.
data GetJobRunsResponse = GetJobRunsResponse'
  { -- | A continuation token, if not all requested job runs have been returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of job-run metadata objects.
    jobRuns :: Prelude.Maybe [JobRun],
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
-- 'nextToken', 'getJobRunsResponse_nextToken' - A continuation token, if not all requested job runs have been returned.
--
-- 'jobRuns', 'getJobRunsResponse_jobRuns' - A list of job-run metadata objects.
--
-- 'httpStatus', 'getJobRunsResponse_httpStatus' - The response's http status code.
newGetJobRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobRunsResponse
newGetJobRunsResponse pHttpStatus_ =
  GetJobRunsResponse'
    { nextToken = Prelude.Nothing,
      jobRuns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all requested job runs have been returned.
getJobRunsResponse_nextToken :: Lens.Lens' GetJobRunsResponse (Prelude.Maybe Prelude.Text)
getJobRunsResponse_nextToken = Lens.lens (\GetJobRunsResponse' {nextToken} -> nextToken) (\s@GetJobRunsResponse' {} a -> s {nextToken = a} :: GetJobRunsResponse)

-- | A list of job-run metadata objects.
getJobRunsResponse_jobRuns :: Lens.Lens' GetJobRunsResponse (Prelude.Maybe [JobRun])
getJobRunsResponse_jobRuns = Lens.lens (\GetJobRunsResponse' {jobRuns} -> jobRuns) (\s@GetJobRunsResponse' {} a -> s {jobRuns = a} :: GetJobRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getJobRunsResponse_httpStatus :: Lens.Lens' GetJobRunsResponse Prelude.Int
getJobRunsResponse_httpStatus = Lens.lens (\GetJobRunsResponse' {httpStatus} -> httpStatus) (\s@GetJobRunsResponse' {} a -> s {httpStatus = a} :: GetJobRunsResponse)

instance Prelude.NFData GetJobRunsResponse where
  rnf GetJobRunsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobRuns
      `Prelude.seq` Prelude.rnf httpStatus

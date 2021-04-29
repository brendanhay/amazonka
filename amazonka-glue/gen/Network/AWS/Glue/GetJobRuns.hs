{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.GetJobRuns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all runs of a given job definition.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetJobRuns
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetJobRuns' smart constructor.
data GetJobRuns = GetJobRuns'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the job definition for which to retrieve all job runs.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager GetJobRuns where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getJobRunsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getJobRunsResponse_jobRuns Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getJobRuns_nextToken
          Lens..~ rs
          Lens.^? getJobRunsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest GetJobRuns where
  type Rs GetJobRuns = GetJobRunsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobRunsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "JobRuns" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobRuns

instance Prelude.NFData GetJobRuns

instance Prelude.ToHeaders GetJobRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.GetJobRuns" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetJobRuns where
  toJSON GetJobRuns' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just ("JobName" Prelude..= jobName)
          ]
      )

instance Prelude.ToPath GetJobRuns where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetJobRuns where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
getJobRunsResponse_jobRuns = Lens.lens (\GetJobRunsResponse' {jobRuns} -> jobRuns) (\s@GetJobRunsResponse' {} a -> s {jobRuns = a} :: GetJobRunsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getJobRunsResponse_httpStatus :: Lens.Lens' GetJobRunsResponse Prelude.Int
getJobRunsResponse_httpStatus = Lens.lens (\GetJobRunsResponse' {httpStatus} -> httpStatus) (\s@GetJobRunsResponse' {} a -> s {httpStatus = a} :: GetJobRunsResponse)

instance Prelude.NFData GetJobRunsResponse

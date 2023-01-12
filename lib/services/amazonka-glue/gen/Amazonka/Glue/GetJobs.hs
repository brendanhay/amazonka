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
-- Module      : Amazonka.Glue.GetJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all current job definitions.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetJobs
  ( -- * Creating a Request
    GetJobs (..),
    newGetJobs,

    -- * Request Lenses
    getJobs_maxResults,
    getJobs_nextToken,

    -- * Destructuring the Response
    GetJobsResponse (..),
    newGetJobsResponse,

    -- * Response Lenses
    getJobsResponse_jobs,
    getJobsResponse_nextToken,
    getJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJobs' smart constructor.
data GetJobs = GetJobs'
  { -- | The maximum size of the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getJobs_maxResults' - The maximum size of the response.
--
-- 'nextToken', 'getJobs_nextToken' - A continuation token, if this is a continuation call.
newGetJobs ::
  GetJobs
newGetJobs =
  GetJobs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum size of the response.
getJobs_maxResults :: Lens.Lens' GetJobs (Prelude.Maybe Prelude.Natural)
getJobs_maxResults = Lens.lens (\GetJobs' {maxResults} -> maxResults) (\s@GetJobs' {} a -> s {maxResults = a} :: GetJobs)

-- | A continuation token, if this is a continuation call.
getJobs_nextToken :: Lens.Lens' GetJobs (Prelude.Maybe Prelude.Text)
getJobs_nextToken = Lens.lens (\GetJobs' {nextToken} -> nextToken) (\s@GetJobs' {} a -> s {nextToken = a} :: GetJobs)

instance Core.AWSPager GetJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getJobsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getJobsResponse_jobs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getJobs_nextToken
          Lens..~ rs
          Lens.^? getJobsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetJobs where
  type AWSResponse GetJobs = GetJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobsResponse'
            Prelude.<$> (x Data..?> "Jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobs where
  hashWithSalt _salt GetJobs' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetJobs where
  rnf GetJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetJobs" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetJobs where
  toJSON GetJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery GetJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobsResponse' smart constructor.
data GetJobsResponse = GetJobsResponse'
  { -- | A list of job definitions.
    jobs :: Prelude.Maybe [Job],
    -- | A continuation token, if not all job definitions have yet been returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobs', 'getJobsResponse_jobs' - A list of job definitions.
--
-- 'nextToken', 'getJobsResponse_nextToken' - A continuation token, if not all job definitions have yet been returned.
--
-- 'httpStatus', 'getJobsResponse_httpStatus' - The response's http status code.
newGetJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobsResponse
newGetJobsResponse pHttpStatus_ =
  GetJobsResponse'
    { jobs = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of job definitions.
getJobsResponse_jobs :: Lens.Lens' GetJobsResponse (Prelude.Maybe [Job])
getJobsResponse_jobs = Lens.lens (\GetJobsResponse' {jobs} -> jobs) (\s@GetJobsResponse' {} a -> s {jobs = a} :: GetJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if not all job definitions have yet been returned.
getJobsResponse_nextToken :: Lens.Lens' GetJobsResponse (Prelude.Maybe Prelude.Text)
getJobsResponse_nextToken = Lens.lens (\GetJobsResponse' {nextToken} -> nextToken) (\s@GetJobsResponse' {} a -> s {nextToken = a} :: GetJobsResponse)

-- | The response's http status code.
getJobsResponse_httpStatus :: Lens.Lens' GetJobsResponse Prelude.Int
getJobsResponse_httpStatus = Lens.lens (\GetJobsResponse' {httpStatus} -> httpStatus) (\s@GetJobsResponse' {} a -> s {httpStatus = a} :: GetJobsResponse)

instance Prelude.NFData GetJobsResponse where
  rnf GetJobsResponse' {..} =
    Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

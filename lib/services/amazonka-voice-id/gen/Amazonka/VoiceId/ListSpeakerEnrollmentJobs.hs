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
-- Module      : Amazonka.VoiceId.ListSpeakerEnrollmentJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the speaker enrollment jobs in the domain with the specified
-- @JobStatus@. If @JobStatus@ is not provided, this lists all jobs with
-- all possible speaker enrollment job statuses.
--
-- This operation returns paginated results.
module Amazonka.VoiceId.ListSpeakerEnrollmentJobs
  ( -- * Creating a Request
    ListSpeakerEnrollmentJobs (..),
    newListSpeakerEnrollmentJobs,

    -- * Request Lenses
    listSpeakerEnrollmentJobs_jobStatus,
    listSpeakerEnrollmentJobs_maxResults,
    listSpeakerEnrollmentJobs_nextToken,
    listSpeakerEnrollmentJobs_domainId,

    -- * Destructuring the Response
    ListSpeakerEnrollmentJobsResponse (..),
    newListSpeakerEnrollmentJobsResponse,

    -- * Response Lenses
    listSpeakerEnrollmentJobsResponse_jobSummaries,
    listSpeakerEnrollmentJobsResponse_nextToken,
    listSpeakerEnrollmentJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newListSpeakerEnrollmentJobs' smart constructor.
data ListSpeakerEnrollmentJobs = ListSpeakerEnrollmentJobs'
  { -- | Provides the status of your speaker enrollment Job.
    jobStatus :: Prelude.Maybe SpeakerEnrollmentJobStatus,
    -- | The maximum number of results that are returned per call. You can use
    -- @NextToken@ to obtain further pages of results. The default is 100; the
    -- maximum allowed page size is also 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the domain containing the speaker enrollment jobs.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSpeakerEnrollmentJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'listSpeakerEnrollmentJobs_jobStatus' - Provides the status of your speaker enrollment Job.
--
-- 'maxResults', 'listSpeakerEnrollmentJobs_maxResults' - The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain further pages of results. The default is 100; the
-- maximum allowed page size is also 100.
--
-- 'nextToken', 'listSpeakerEnrollmentJobs_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'domainId', 'listSpeakerEnrollmentJobs_domainId' - The identifier of the domain containing the speaker enrollment jobs.
newListSpeakerEnrollmentJobs ::
  -- | 'domainId'
  Prelude.Text ->
  ListSpeakerEnrollmentJobs
newListSpeakerEnrollmentJobs pDomainId_ =
  ListSpeakerEnrollmentJobs'
    { jobStatus =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | Provides the status of your speaker enrollment Job.
listSpeakerEnrollmentJobs_jobStatus :: Lens.Lens' ListSpeakerEnrollmentJobs (Prelude.Maybe SpeakerEnrollmentJobStatus)
listSpeakerEnrollmentJobs_jobStatus = Lens.lens (\ListSpeakerEnrollmentJobs' {jobStatus} -> jobStatus) (\s@ListSpeakerEnrollmentJobs' {} a -> s {jobStatus = a} :: ListSpeakerEnrollmentJobs)

-- | The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain further pages of results. The default is 100; the
-- maximum allowed page size is also 100.
listSpeakerEnrollmentJobs_maxResults :: Lens.Lens' ListSpeakerEnrollmentJobs (Prelude.Maybe Prelude.Natural)
listSpeakerEnrollmentJobs_maxResults = Lens.lens (\ListSpeakerEnrollmentJobs' {maxResults} -> maxResults) (\s@ListSpeakerEnrollmentJobs' {} a -> s {maxResults = a} :: ListSpeakerEnrollmentJobs)

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listSpeakerEnrollmentJobs_nextToken :: Lens.Lens' ListSpeakerEnrollmentJobs (Prelude.Maybe Prelude.Text)
listSpeakerEnrollmentJobs_nextToken = Lens.lens (\ListSpeakerEnrollmentJobs' {nextToken} -> nextToken) (\s@ListSpeakerEnrollmentJobs' {} a -> s {nextToken = a} :: ListSpeakerEnrollmentJobs)

-- | The identifier of the domain containing the speaker enrollment jobs.
listSpeakerEnrollmentJobs_domainId :: Lens.Lens' ListSpeakerEnrollmentJobs Prelude.Text
listSpeakerEnrollmentJobs_domainId = Lens.lens (\ListSpeakerEnrollmentJobs' {domainId} -> domainId) (\s@ListSpeakerEnrollmentJobs' {} a -> s {domainId = a} :: ListSpeakerEnrollmentJobs)

instance Core.AWSPager ListSpeakerEnrollmentJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSpeakerEnrollmentJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSpeakerEnrollmentJobsResponse_jobSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSpeakerEnrollmentJobs_nextToken
          Lens..~ rs
          Lens.^? listSpeakerEnrollmentJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSpeakerEnrollmentJobs where
  type
    AWSResponse ListSpeakerEnrollmentJobs =
      ListSpeakerEnrollmentJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSpeakerEnrollmentJobsResponse'
            Prelude.<$> (x Data..?> "JobSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSpeakerEnrollmentJobs where
  hashWithSalt _salt ListSpeakerEnrollmentJobs' {..} =
    _salt
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData ListSpeakerEnrollmentJobs where
  rnf ListSpeakerEnrollmentJobs' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainId

instance Data.ToHeaders ListSpeakerEnrollmentJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VoiceID.ListSpeakerEnrollmentJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSpeakerEnrollmentJobs where
  toJSON ListSpeakerEnrollmentJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobStatus" Data..=) Prelude.<$> jobStatus,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("DomainId" Data..= domainId)
          ]
      )

instance Data.ToPath ListSpeakerEnrollmentJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSpeakerEnrollmentJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSpeakerEnrollmentJobsResponse' smart constructor.
data ListSpeakerEnrollmentJobsResponse = ListSpeakerEnrollmentJobsResponse'
  { -- | A list containing details about each specified speaker enrollment job.
    jobSummaries :: Prelude.Maybe [SpeakerEnrollmentJobSummary],
    -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSpeakerEnrollmentJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobSummaries', 'listSpeakerEnrollmentJobsResponse_jobSummaries' - A list containing details about each specified speaker enrollment job.
--
-- 'nextToken', 'listSpeakerEnrollmentJobsResponse_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'httpStatus', 'listSpeakerEnrollmentJobsResponse_httpStatus' - The response's http status code.
newListSpeakerEnrollmentJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSpeakerEnrollmentJobsResponse
newListSpeakerEnrollmentJobsResponse pHttpStatus_ =
  ListSpeakerEnrollmentJobsResponse'
    { jobSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing details about each specified speaker enrollment job.
listSpeakerEnrollmentJobsResponse_jobSummaries :: Lens.Lens' ListSpeakerEnrollmentJobsResponse (Prelude.Maybe [SpeakerEnrollmentJobSummary])
listSpeakerEnrollmentJobsResponse_jobSummaries = Lens.lens (\ListSpeakerEnrollmentJobsResponse' {jobSummaries} -> jobSummaries) (\s@ListSpeakerEnrollmentJobsResponse' {} a -> s {jobSummaries = a} :: ListSpeakerEnrollmentJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listSpeakerEnrollmentJobsResponse_nextToken :: Lens.Lens' ListSpeakerEnrollmentJobsResponse (Prelude.Maybe Prelude.Text)
listSpeakerEnrollmentJobsResponse_nextToken = Lens.lens (\ListSpeakerEnrollmentJobsResponse' {nextToken} -> nextToken) (\s@ListSpeakerEnrollmentJobsResponse' {} a -> s {nextToken = a} :: ListSpeakerEnrollmentJobsResponse)

-- | The response's http status code.
listSpeakerEnrollmentJobsResponse_httpStatus :: Lens.Lens' ListSpeakerEnrollmentJobsResponse Prelude.Int
listSpeakerEnrollmentJobsResponse_httpStatus = Lens.lens (\ListSpeakerEnrollmentJobsResponse' {httpStatus} -> httpStatus) (\s@ListSpeakerEnrollmentJobsResponse' {} a -> s {httpStatus = a} :: ListSpeakerEnrollmentJobsResponse)

instance
  Prelude.NFData
    ListSpeakerEnrollmentJobsResponse
  where
  rnf ListSpeakerEnrollmentJobsResponse' {..} =
    Prelude.rnf jobSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

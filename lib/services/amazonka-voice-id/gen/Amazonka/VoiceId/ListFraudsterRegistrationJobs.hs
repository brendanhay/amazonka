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
-- Module      : Amazonka.VoiceId.ListFraudsterRegistrationJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the fraudster registration jobs in the domain with the given
-- @JobStatus@. If @JobStatus@ is not provided, this lists all fraudster
-- registration jobs in the given domain.
--
-- This operation returns paginated results.
module Amazonka.VoiceId.ListFraudsterRegistrationJobs
  ( -- * Creating a Request
    ListFraudsterRegistrationJobs (..),
    newListFraudsterRegistrationJobs,

    -- * Request Lenses
    listFraudsterRegistrationJobs_jobStatus,
    listFraudsterRegistrationJobs_maxResults,
    listFraudsterRegistrationJobs_nextToken,
    listFraudsterRegistrationJobs_domainId,

    -- * Destructuring the Response
    ListFraudsterRegistrationJobsResponse (..),
    newListFraudsterRegistrationJobsResponse,

    -- * Response Lenses
    listFraudsterRegistrationJobsResponse_jobSummaries,
    listFraudsterRegistrationJobsResponse_nextToken,
    listFraudsterRegistrationJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newListFraudsterRegistrationJobs' smart constructor.
data ListFraudsterRegistrationJobs = ListFraudsterRegistrationJobs'
  { -- | Provides the status of your fraudster registration job.
    jobStatus :: Prelude.Maybe FraudsterRegistrationJobStatus,
    -- | The maximum number of results that are returned per call. You can use
    -- @NextToken@ to obtain more pages of results. The default is 100; the
    -- maximum allowed page size is also 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the domain that contains the fraudster registration
    -- Jobs.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFraudsterRegistrationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'listFraudsterRegistrationJobs_jobStatus' - Provides the status of your fraudster registration job.
--
-- 'maxResults', 'listFraudsterRegistrationJobs_maxResults' - The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain more pages of results. The default is 100; the
-- maximum allowed page size is also 100.
--
-- 'nextToken', 'listFraudsterRegistrationJobs_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'domainId', 'listFraudsterRegistrationJobs_domainId' - The identifier of the domain that contains the fraudster registration
-- Jobs.
newListFraudsterRegistrationJobs ::
  -- | 'domainId'
  Prelude.Text ->
  ListFraudsterRegistrationJobs
newListFraudsterRegistrationJobs pDomainId_ =
  ListFraudsterRegistrationJobs'
    { jobStatus =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | Provides the status of your fraudster registration job.
listFraudsterRegistrationJobs_jobStatus :: Lens.Lens' ListFraudsterRegistrationJobs (Prelude.Maybe FraudsterRegistrationJobStatus)
listFraudsterRegistrationJobs_jobStatus = Lens.lens (\ListFraudsterRegistrationJobs' {jobStatus} -> jobStatus) (\s@ListFraudsterRegistrationJobs' {} a -> s {jobStatus = a} :: ListFraudsterRegistrationJobs)

-- | The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain more pages of results. The default is 100; the
-- maximum allowed page size is also 100.
listFraudsterRegistrationJobs_maxResults :: Lens.Lens' ListFraudsterRegistrationJobs (Prelude.Maybe Prelude.Natural)
listFraudsterRegistrationJobs_maxResults = Lens.lens (\ListFraudsterRegistrationJobs' {maxResults} -> maxResults) (\s@ListFraudsterRegistrationJobs' {} a -> s {maxResults = a} :: ListFraudsterRegistrationJobs)

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listFraudsterRegistrationJobs_nextToken :: Lens.Lens' ListFraudsterRegistrationJobs (Prelude.Maybe Prelude.Text)
listFraudsterRegistrationJobs_nextToken = Lens.lens (\ListFraudsterRegistrationJobs' {nextToken} -> nextToken) (\s@ListFraudsterRegistrationJobs' {} a -> s {nextToken = a} :: ListFraudsterRegistrationJobs)

-- | The identifier of the domain that contains the fraudster registration
-- Jobs.
listFraudsterRegistrationJobs_domainId :: Lens.Lens' ListFraudsterRegistrationJobs Prelude.Text
listFraudsterRegistrationJobs_domainId = Lens.lens (\ListFraudsterRegistrationJobs' {domainId} -> domainId) (\s@ListFraudsterRegistrationJobs' {} a -> s {domainId = a} :: ListFraudsterRegistrationJobs)

instance Core.AWSPager ListFraudsterRegistrationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFraudsterRegistrationJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFraudsterRegistrationJobsResponse_jobSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFraudsterRegistrationJobs_nextToken
          Lens..~ rs
          Lens.^? listFraudsterRegistrationJobsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListFraudsterRegistrationJobs
  where
  type
    AWSResponse ListFraudsterRegistrationJobs =
      ListFraudsterRegistrationJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFraudsterRegistrationJobsResponse'
            Prelude.<$> (x Data..?> "JobSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFraudsterRegistrationJobs
  where
  hashWithSalt _salt ListFraudsterRegistrationJobs' {..} =
    _salt
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData ListFraudsterRegistrationJobs where
  rnf ListFraudsterRegistrationJobs' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainId

instance Data.ToHeaders ListFraudsterRegistrationJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VoiceID.ListFraudsterRegistrationJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFraudsterRegistrationJobs where
  toJSON ListFraudsterRegistrationJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobStatus" Data..=) Prelude.<$> jobStatus,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("DomainId" Data..= domainId)
          ]
      )

instance Data.ToPath ListFraudsterRegistrationJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFraudsterRegistrationJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFraudsterRegistrationJobsResponse' smart constructor.
data ListFraudsterRegistrationJobsResponse = ListFraudsterRegistrationJobsResponse'
  { -- | A list containing details about each specified fraudster registration
    -- job.
    jobSummaries :: Prelude.Maybe [FraudsterRegistrationJobSummary],
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
-- Create a value of 'ListFraudsterRegistrationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobSummaries', 'listFraudsterRegistrationJobsResponse_jobSummaries' - A list containing details about each specified fraudster registration
-- job.
--
-- 'nextToken', 'listFraudsterRegistrationJobsResponse_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'httpStatus', 'listFraudsterRegistrationJobsResponse_httpStatus' - The response's http status code.
newListFraudsterRegistrationJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFraudsterRegistrationJobsResponse
newListFraudsterRegistrationJobsResponse pHttpStatus_ =
  ListFraudsterRegistrationJobsResponse'
    { jobSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing details about each specified fraudster registration
-- job.
listFraudsterRegistrationJobsResponse_jobSummaries :: Lens.Lens' ListFraudsterRegistrationJobsResponse (Prelude.Maybe [FraudsterRegistrationJobSummary])
listFraudsterRegistrationJobsResponse_jobSummaries = Lens.lens (\ListFraudsterRegistrationJobsResponse' {jobSummaries} -> jobSummaries) (\s@ListFraudsterRegistrationJobsResponse' {} a -> s {jobSummaries = a} :: ListFraudsterRegistrationJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listFraudsterRegistrationJobsResponse_nextToken :: Lens.Lens' ListFraudsterRegistrationJobsResponse (Prelude.Maybe Prelude.Text)
listFraudsterRegistrationJobsResponse_nextToken = Lens.lens (\ListFraudsterRegistrationJobsResponse' {nextToken} -> nextToken) (\s@ListFraudsterRegistrationJobsResponse' {} a -> s {nextToken = a} :: ListFraudsterRegistrationJobsResponse)

-- | The response's http status code.
listFraudsterRegistrationJobsResponse_httpStatus :: Lens.Lens' ListFraudsterRegistrationJobsResponse Prelude.Int
listFraudsterRegistrationJobsResponse_httpStatus = Lens.lens (\ListFraudsterRegistrationJobsResponse' {httpStatus} -> httpStatus) (\s@ListFraudsterRegistrationJobsResponse' {} a -> s {httpStatus = a} :: ListFraudsterRegistrationJobsResponse)

instance
  Prelude.NFData
    ListFraudsterRegistrationJobsResponse
  where
  rnf ListFraudsterRegistrationJobsResponse' {..} =
    Prelude.rnf jobSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

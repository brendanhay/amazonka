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
-- Module      : Amazonka.Signer.ListSigningJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your signing jobs. You can use the @maxResults@ parameter to
-- limit the number of signing jobs that are returned in the response. If
-- additional jobs remain to be listed, code signing returns a @nextToken@
-- value. Use this value in subsequent calls to @ListSigningJobs@ to fetch
-- the remaining values. You can continue calling @ListSigningJobs@ with
-- your @maxResults@ parameter and with new values that code signing
-- returns in the @nextToken@ parameter until all of your signing jobs have
-- been returned.
--
-- This operation returns paginated results.
module Amazonka.Signer.ListSigningJobs
  ( -- * Creating a Request
    ListSigningJobs (..),
    newListSigningJobs,

    -- * Request Lenses
    listSigningJobs_isRevoked,
    listSigningJobs_jobInvoker,
    listSigningJobs_maxResults,
    listSigningJobs_nextToken,
    listSigningJobs_platformId,
    listSigningJobs_requestedBy,
    listSigningJobs_signatureExpiresAfter,
    listSigningJobs_signatureExpiresBefore,
    listSigningJobs_status,

    -- * Destructuring the Response
    ListSigningJobsResponse (..),
    newListSigningJobsResponse,

    -- * Response Lenses
    listSigningJobsResponse_jobs,
    listSigningJobsResponse_nextToken,
    listSigningJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newListSigningJobs' smart constructor.
data ListSigningJobs = ListSigningJobs'
  { -- | Filters results to return only signing jobs with revoked signatures.
    isRevoked :: Prelude.Maybe Prelude.Bool,
    -- | Filters results to return only signing jobs initiated by a specified IAM
    -- entity.
    jobInvoker :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of items to return in the response. Use
    -- this parameter when paginating results. If additional items exist beyond
    -- the number you specify, the @nextToken@ element is set in the response.
    -- Use the @nextToken@ value in a subsequent request to retrieve additional
    -- items.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | String for specifying the next set of paginated results to return. After
    -- you receive a response with truncated results, use this parameter in a
    -- subsequent request. Set it to the value of @nextToken@ from the response
    -- that you just received.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of microcontroller platform that you specified for the
    -- distribution of your code image.
    platformId :: Prelude.Maybe Prelude.Text,
    -- | The IAM principal that requested the signing job.
    requestedBy :: Prelude.Maybe Prelude.Text,
    -- | Filters results to return only signing jobs with signatures expiring
    -- after a specified timestamp.
    signatureExpiresAfter :: Prelude.Maybe Data.POSIX,
    -- | Filters results to return only signing jobs with signatures expiring
    -- before a specified timestamp.
    signatureExpiresBefore :: Prelude.Maybe Data.POSIX,
    -- | A status value with which to filter your results.
    status :: Prelude.Maybe SigningStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSigningJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isRevoked', 'listSigningJobs_isRevoked' - Filters results to return only signing jobs with revoked signatures.
--
-- 'jobInvoker', 'listSigningJobs_jobInvoker' - Filters results to return only signing jobs initiated by a specified IAM
-- entity.
--
-- 'maxResults', 'listSigningJobs_maxResults' - Specifies the maximum number of items to return in the response. Use
-- this parameter when paginating results. If additional items exist beyond
-- the number you specify, the @nextToken@ element is set in the response.
-- Use the @nextToken@ value in a subsequent request to retrieve additional
-- items.
--
-- 'nextToken', 'listSigningJobs_nextToken' - String for specifying the next set of paginated results to return. After
-- you receive a response with truncated results, use this parameter in a
-- subsequent request. Set it to the value of @nextToken@ from the response
-- that you just received.
--
-- 'platformId', 'listSigningJobs_platformId' - The ID of microcontroller platform that you specified for the
-- distribution of your code image.
--
-- 'requestedBy', 'listSigningJobs_requestedBy' - The IAM principal that requested the signing job.
--
-- 'signatureExpiresAfter', 'listSigningJobs_signatureExpiresAfter' - Filters results to return only signing jobs with signatures expiring
-- after a specified timestamp.
--
-- 'signatureExpiresBefore', 'listSigningJobs_signatureExpiresBefore' - Filters results to return only signing jobs with signatures expiring
-- before a specified timestamp.
--
-- 'status', 'listSigningJobs_status' - A status value with which to filter your results.
newListSigningJobs ::
  ListSigningJobs
newListSigningJobs =
  ListSigningJobs'
    { isRevoked = Prelude.Nothing,
      jobInvoker = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      platformId = Prelude.Nothing,
      requestedBy = Prelude.Nothing,
      signatureExpiresAfter = Prelude.Nothing,
      signatureExpiresBefore = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Filters results to return only signing jobs with revoked signatures.
listSigningJobs_isRevoked :: Lens.Lens' ListSigningJobs (Prelude.Maybe Prelude.Bool)
listSigningJobs_isRevoked = Lens.lens (\ListSigningJobs' {isRevoked} -> isRevoked) (\s@ListSigningJobs' {} a -> s {isRevoked = a} :: ListSigningJobs)

-- | Filters results to return only signing jobs initiated by a specified IAM
-- entity.
listSigningJobs_jobInvoker :: Lens.Lens' ListSigningJobs (Prelude.Maybe Prelude.Text)
listSigningJobs_jobInvoker = Lens.lens (\ListSigningJobs' {jobInvoker} -> jobInvoker) (\s@ListSigningJobs' {} a -> s {jobInvoker = a} :: ListSigningJobs)

-- | Specifies the maximum number of items to return in the response. Use
-- this parameter when paginating results. If additional items exist beyond
-- the number you specify, the @nextToken@ element is set in the response.
-- Use the @nextToken@ value in a subsequent request to retrieve additional
-- items.
listSigningJobs_maxResults :: Lens.Lens' ListSigningJobs (Prelude.Maybe Prelude.Natural)
listSigningJobs_maxResults = Lens.lens (\ListSigningJobs' {maxResults} -> maxResults) (\s@ListSigningJobs' {} a -> s {maxResults = a} :: ListSigningJobs)

-- | String for specifying the next set of paginated results to return. After
-- you receive a response with truncated results, use this parameter in a
-- subsequent request. Set it to the value of @nextToken@ from the response
-- that you just received.
listSigningJobs_nextToken :: Lens.Lens' ListSigningJobs (Prelude.Maybe Prelude.Text)
listSigningJobs_nextToken = Lens.lens (\ListSigningJobs' {nextToken} -> nextToken) (\s@ListSigningJobs' {} a -> s {nextToken = a} :: ListSigningJobs)

-- | The ID of microcontroller platform that you specified for the
-- distribution of your code image.
listSigningJobs_platformId :: Lens.Lens' ListSigningJobs (Prelude.Maybe Prelude.Text)
listSigningJobs_platformId = Lens.lens (\ListSigningJobs' {platformId} -> platformId) (\s@ListSigningJobs' {} a -> s {platformId = a} :: ListSigningJobs)

-- | The IAM principal that requested the signing job.
listSigningJobs_requestedBy :: Lens.Lens' ListSigningJobs (Prelude.Maybe Prelude.Text)
listSigningJobs_requestedBy = Lens.lens (\ListSigningJobs' {requestedBy} -> requestedBy) (\s@ListSigningJobs' {} a -> s {requestedBy = a} :: ListSigningJobs)

-- | Filters results to return only signing jobs with signatures expiring
-- after a specified timestamp.
listSigningJobs_signatureExpiresAfter :: Lens.Lens' ListSigningJobs (Prelude.Maybe Prelude.UTCTime)
listSigningJobs_signatureExpiresAfter = Lens.lens (\ListSigningJobs' {signatureExpiresAfter} -> signatureExpiresAfter) (\s@ListSigningJobs' {} a -> s {signatureExpiresAfter = a} :: ListSigningJobs) Prelude.. Lens.mapping Data._Time

-- | Filters results to return only signing jobs with signatures expiring
-- before a specified timestamp.
listSigningJobs_signatureExpiresBefore :: Lens.Lens' ListSigningJobs (Prelude.Maybe Prelude.UTCTime)
listSigningJobs_signatureExpiresBefore = Lens.lens (\ListSigningJobs' {signatureExpiresBefore} -> signatureExpiresBefore) (\s@ListSigningJobs' {} a -> s {signatureExpiresBefore = a} :: ListSigningJobs) Prelude.. Lens.mapping Data._Time

-- | A status value with which to filter your results.
listSigningJobs_status :: Lens.Lens' ListSigningJobs (Prelude.Maybe SigningStatus)
listSigningJobs_status = Lens.lens (\ListSigningJobs' {status} -> status) (\s@ListSigningJobs' {} a -> s {status = a} :: ListSigningJobs)

instance Core.AWSPager ListSigningJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSigningJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSigningJobsResponse_jobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSigningJobs_nextToken
          Lens..~ rs
          Lens.^? listSigningJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSigningJobs where
  type
    AWSResponse ListSigningJobs =
      ListSigningJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSigningJobsResponse'
            Prelude.<$> (x Data..?> "jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSigningJobs where
  hashWithSalt _salt ListSigningJobs' {..} =
    _salt
      `Prelude.hashWithSalt` isRevoked
      `Prelude.hashWithSalt` jobInvoker
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` platformId
      `Prelude.hashWithSalt` requestedBy
      `Prelude.hashWithSalt` signatureExpiresAfter
      `Prelude.hashWithSalt` signatureExpiresBefore
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListSigningJobs where
  rnf ListSigningJobs' {..} =
    Prelude.rnf isRevoked
      `Prelude.seq` Prelude.rnf jobInvoker
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf platformId
      `Prelude.seq` Prelude.rnf requestedBy
      `Prelude.seq` Prelude.rnf signatureExpiresAfter
      `Prelude.seq` Prelude.rnf signatureExpiresBefore
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders ListSigningJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSigningJobs where
  toPath = Prelude.const "/signing-jobs"

instance Data.ToQuery ListSigningJobs where
  toQuery ListSigningJobs' {..} =
    Prelude.mconcat
      [ "isRevoked" Data.=: isRevoked,
        "jobInvoker" Data.=: jobInvoker,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "platformId" Data.=: platformId,
        "requestedBy" Data.=: requestedBy,
        "signatureExpiresAfter"
          Data.=: signatureExpiresAfter,
        "signatureExpiresBefore"
          Data.=: signatureExpiresBefore,
        "status" Data.=: status
      ]

-- | /See:/ 'newListSigningJobsResponse' smart constructor.
data ListSigningJobsResponse = ListSigningJobsResponse'
  { -- | A list of your signing jobs.
    jobs :: Prelude.Maybe [SigningJob],
    -- | String for specifying the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSigningJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobs', 'listSigningJobsResponse_jobs' - A list of your signing jobs.
--
-- 'nextToken', 'listSigningJobsResponse_nextToken' - String for specifying the next set of paginated results.
--
-- 'httpStatus', 'listSigningJobsResponse_httpStatus' - The response's http status code.
newListSigningJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSigningJobsResponse
newListSigningJobsResponse pHttpStatus_ =
  ListSigningJobsResponse'
    { jobs = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of your signing jobs.
listSigningJobsResponse_jobs :: Lens.Lens' ListSigningJobsResponse (Prelude.Maybe [SigningJob])
listSigningJobsResponse_jobs = Lens.lens (\ListSigningJobsResponse' {jobs} -> jobs) (\s@ListSigningJobsResponse' {} a -> s {jobs = a} :: ListSigningJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | String for specifying the next set of paginated results.
listSigningJobsResponse_nextToken :: Lens.Lens' ListSigningJobsResponse (Prelude.Maybe Prelude.Text)
listSigningJobsResponse_nextToken = Lens.lens (\ListSigningJobsResponse' {nextToken} -> nextToken) (\s@ListSigningJobsResponse' {} a -> s {nextToken = a} :: ListSigningJobsResponse)

-- | The response's http status code.
listSigningJobsResponse_httpStatus :: Lens.Lens' ListSigningJobsResponse Prelude.Int
listSigningJobsResponse_httpStatus = Lens.lens (\ListSigningJobsResponse' {httpStatus} -> httpStatus) (\s@ListSigningJobsResponse' {} a -> s {httpStatus = a} :: ListSigningJobsResponse)

instance Prelude.NFData ListSigningJobsResponse where
  rnf ListSigningJobsResponse' {..} =
    Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

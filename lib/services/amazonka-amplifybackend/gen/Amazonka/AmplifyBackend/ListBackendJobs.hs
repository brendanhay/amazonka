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
-- Module      : Amazonka.AmplifyBackend.ListBackendJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the jobs for the backend of an Amplify app.
--
-- This operation returns paginated results.
module Amazonka.AmplifyBackend.ListBackendJobs
  ( -- * Creating a Request
    ListBackendJobs (..),
    newListBackendJobs,

    -- * Request Lenses
    listBackendJobs_jobId,
    listBackendJobs_maxResults,
    listBackendJobs_nextToken,
    listBackendJobs_operation,
    listBackendJobs_status,
    listBackendJobs_appId,
    listBackendJobs_backendEnvironmentName,

    -- * Destructuring the Response
    ListBackendJobsResponse (..),
    newListBackendJobsResponse,

    -- * Response Lenses
    listBackendJobsResponse_jobs,
    listBackendJobsResponse_nextToken,
    listBackendJobsResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for ListBackendJobs.
--
-- /See:/ 'newListBackendJobs' smart constructor.
data ListBackendJobs = ListBackendJobs'
  { -- | The ID for the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that you want in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of response objects to include only those with the
    -- specified operation name.
    operation :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of response objects to include only those with the
    -- specified status.
    status :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Text,
    -- | The name of the backend environment.
    backendEnvironmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackendJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'listBackendJobs_jobId' - The ID for the job.
--
-- 'maxResults', 'listBackendJobs_maxResults' - The maximum number of results that you want in the response.
--
-- 'nextToken', 'listBackendJobs_nextToken' - The token for the next set of results.
--
-- 'operation', 'listBackendJobs_operation' - Filters the list of response objects to include only those with the
-- specified operation name.
--
-- 'status', 'listBackendJobs_status' - Filters the list of response objects to include only those with the
-- specified status.
--
-- 'appId', 'listBackendJobs_appId' - The app ID.
--
-- 'backendEnvironmentName', 'listBackendJobs_backendEnvironmentName' - The name of the backend environment.
newListBackendJobs ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'backendEnvironmentName'
  Prelude.Text ->
  ListBackendJobs
newListBackendJobs pAppId_ pBackendEnvironmentName_ =
  ListBackendJobs'
    { jobId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      operation = Prelude.Nothing,
      status = Prelude.Nothing,
      appId = pAppId_,
      backendEnvironmentName = pBackendEnvironmentName_
    }

-- | The ID for the job.
listBackendJobs_jobId :: Lens.Lens' ListBackendJobs (Prelude.Maybe Prelude.Text)
listBackendJobs_jobId = Lens.lens (\ListBackendJobs' {jobId} -> jobId) (\s@ListBackendJobs' {} a -> s {jobId = a} :: ListBackendJobs)

-- | The maximum number of results that you want in the response.
listBackendJobs_maxResults :: Lens.Lens' ListBackendJobs (Prelude.Maybe Prelude.Natural)
listBackendJobs_maxResults = Lens.lens (\ListBackendJobs' {maxResults} -> maxResults) (\s@ListBackendJobs' {} a -> s {maxResults = a} :: ListBackendJobs)

-- | The token for the next set of results.
listBackendJobs_nextToken :: Lens.Lens' ListBackendJobs (Prelude.Maybe Prelude.Text)
listBackendJobs_nextToken = Lens.lens (\ListBackendJobs' {nextToken} -> nextToken) (\s@ListBackendJobs' {} a -> s {nextToken = a} :: ListBackendJobs)

-- | Filters the list of response objects to include only those with the
-- specified operation name.
listBackendJobs_operation :: Lens.Lens' ListBackendJobs (Prelude.Maybe Prelude.Text)
listBackendJobs_operation = Lens.lens (\ListBackendJobs' {operation} -> operation) (\s@ListBackendJobs' {} a -> s {operation = a} :: ListBackendJobs)

-- | Filters the list of response objects to include only those with the
-- specified status.
listBackendJobs_status :: Lens.Lens' ListBackendJobs (Prelude.Maybe Prelude.Text)
listBackendJobs_status = Lens.lens (\ListBackendJobs' {status} -> status) (\s@ListBackendJobs' {} a -> s {status = a} :: ListBackendJobs)

-- | The app ID.
listBackendJobs_appId :: Lens.Lens' ListBackendJobs Prelude.Text
listBackendJobs_appId = Lens.lens (\ListBackendJobs' {appId} -> appId) (\s@ListBackendJobs' {} a -> s {appId = a} :: ListBackendJobs)

-- | The name of the backend environment.
listBackendJobs_backendEnvironmentName :: Lens.Lens' ListBackendJobs Prelude.Text
listBackendJobs_backendEnvironmentName = Lens.lens (\ListBackendJobs' {backendEnvironmentName} -> backendEnvironmentName) (\s@ListBackendJobs' {} a -> s {backendEnvironmentName = a} :: ListBackendJobs)

instance Core.AWSPager ListBackendJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBackendJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBackendJobsResponse_jobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listBackendJobs_nextToken
          Lens..~ rs
          Lens.^? listBackendJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListBackendJobs where
  type
    AWSResponse ListBackendJobs =
      ListBackendJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBackendJobsResponse'
            Prelude.<$> (x Data..?> "jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBackendJobs where
  hashWithSalt _salt ListBackendJobs' {..} =
    _salt
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` backendEnvironmentName

instance Prelude.NFData ListBackendJobs where
  rnf ListBackendJobs' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf backendEnvironmentName

instance Data.ToHeaders ListBackendJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBackendJobs where
  toJSON ListBackendJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("jobId" Data..=) Prelude.<$> jobId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("operation" Data..=) Prelude.<$> operation,
            ("status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath ListBackendJobs where
  toPath ListBackendJobs' {..} =
    Prelude.mconcat
      [ "/backend/",
        Data.toBS appId,
        "/job/",
        Data.toBS backendEnvironmentName
      ]

instance Data.ToQuery ListBackendJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBackendJobsResponse' smart constructor.
data ListBackendJobsResponse = ListBackendJobsResponse'
  { -- | An array of jobs and their properties.
    jobs :: Prelude.Maybe [BackendJobRespObj],
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackendJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobs', 'listBackendJobsResponse_jobs' - An array of jobs and their properties.
--
-- 'nextToken', 'listBackendJobsResponse_nextToken' - The token for the next set of results.
--
-- 'httpStatus', 'listBackendJobsResponse_httpStatus' - The response's http status code.
newListBackendJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBackendJobsResponse
newListBackendJobsResponse pHttpStatus_ =
  ListBackendJobsResponse'
    { jobs = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of jobs and their properties.
listBackendJobsResponse_jobs :: Lens.Lens' ListBackendJobsResponse (Prelude.Maybe [BackendJobRespObj])
listBackendJobsResponse_jobs = Lens.lens (\ListBackendJobsResponse' {jobs} -> jobs) (\s@ListBackendJobsResponse' {} a -> s {jobs = a} :: ListBackendJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results.
listBackendJobsResponse_nextToken :: Lens.Lens' ListBackendJobsResponse (Prelude.Maybe Prelude.Text)
listBackendJobsResponse_nextToken = Lens.lens (\ListBackendJobsResponse' {nextToken} -> nextToken) (\s@ListBackendJobsResponse' {} a -> s {nextToken = a} :: ListBackendJobsResponse)

-- | The response's http status code.
listBackendJobsResponse_httpStatus :: Lens.Lens' ListBackendJobsResponse Prelude.Int
listBackendJobsResponse_httpStatus = Lens.lens (\ListBackendJobsResponse' {httpStatus} -> httpStatus) (\s@ListBackendJobsResponse' {} a -> s {httpStatus = a} :: ListBackendJobsResponse)

instance Prelude.NFData ListBackendJobsResponse where
  rnf ListBackendJobsResponse' {..} =
    Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

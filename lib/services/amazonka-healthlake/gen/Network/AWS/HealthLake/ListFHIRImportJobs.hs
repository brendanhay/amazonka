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
-- Module      : Network.AWS.HealthLake.ListFHIRImportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all FHIR import jobs associated with an account and their
-- statuses.
module Network.AWS.HealthLake.ListFHIRImportJobs
  ( -- * Creating a Request
    ListFHIRImportJobs (..),
    newListFHIRImportJobs,

    -- * Request Lenses
    listFHIRImportJobs_submittedAfter,
    listFHIRImportJobs_jobName,
    listFHIRImportJobs_submittedBefore,
    listFHIRImportJobs_nextToken,
    listFHIRImportJobs_jobStatus,
    listFHIRImportJobs_maxResults,
    listFHIRImportJobs_datastoreId,

    -- * Destructuring the Response
    ListFHIRImportJobsResponse (..),
    newListFHIRImportJobsResponse,

    -- * Response Lenses
    listFHIRImportJobsResponse_nextToken,
    listFHIRImportJobsResponse_httpStatus,
    listFHIRImportJobsResponse_importJobPropertiesList,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.HealthLake.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFHIRImportJobs' smart constructor.
data ListFHIRImportJobs = ListFHIRImportJobs'
  { -- | This parameter limits the response to FHIR import jobs submitted after a
    -- user specified date.
    submittedAfter :: Prelude.Maybe Core.POSIX,
    -- | This parameter limits the response to the import job with the specified
    -- job name.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | This parameter limits the response to FHIR import jobs submitted before
    -- a user specified date.
    submittedBefore :: Prelude.Maybe Core.POSIX,
    -- | A pagination token used to identify the next page of results to return
    -- for a ListFHIRImportJobs query.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This parameter limits the response to the import job with the specified
    -- job status.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | This parameter limits the number of results returned for a
    -- ListFHIRImportJobs to a maximum quantity specified by the user.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | This parameter limits the response to the import job with the specified
    -- Data Store ID.
    datastoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFHIRImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'submittedAfter', 'listFHIRImportJobs_submittedAfter' - This parameter limits the response to FHIR import jobs submitted after a
-- user specified date.
--
-- 'jobName', 'listFHIRImportJobs_jobName' - This parameter limits the response to the import job with the specified
-- job name.
--
-- 'submittedBefore', 'listFHIRImportJobs_submittedBefore' - This parameter limits the response to FHIR import jobs submitted before
-- a user specified date.
--
-- 'nextToken', 'listFHIRImportJobs_nextToken' - A pagination token used to identify the next page of results to return
-- for a ListFHIRImportJobs query.
--
-- 'jobStatus', 'listFHIRImportJobs_jobStatus' - This parameter limits the response to the import job with the specified
-- job status.
--
-- 'maxResults', 'listFHIRImportJobs_maxResults' - This parameter limits the number of results returned for a
-- ListFHIRImportJobs to a maximum quantity specified by the user.
--
-- 'datastoreId', 'listFHIRImportJobs_datastoreId' - This parameter limits the response to the import job with the specified
-- Data Store ID.
newListFHIRImportJobs ::
  -- | 'datastoreId'
  Prelude.Text ->
  ListFHIRImportJobs
newListFHIRImportJobs pDatastoreId_ =
  ListFHIRImportJobs'
    { submittedAfter =
        Prelude.Nothing,
      jobName = Prelude.Nothing,
      submittedBefore = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      datastoreId = pDatastoreId_
    }

-- | This parameter limits the response to FHIR import jobs submitted after a
-- user specified date.
listFHIRImportJobs_submittedAfter :: Lens.Lens' ListFHIRImportJobs (Prelude.Maybe Prelude.UTCTime)
listFHIRImportJobs_submittedAfter = Lens.lens (\ListFHIRImportJobs' {submittedAfter} -> submittedAfter) (\s@ListFHIRImportJobs' {} a -> s {submittedAfter = a} :: ListFHIRImportJobs) Prelude.. Lens.mapping Core._Time

-- | This parameter limits the response to the import job with the specified
-- job name.
listFHIRImportJobs_jobName :: Lens.Lens' ListFHIRImportJobs (Prelude.Maybe Prelude.Text)
listFHIRImportJobs_jobName = Lens.lens (\ListFHIRImportJobs' {jobName} -> jobName) (\s@ListFHIRImportJobs' {} a -> s {jobName = a} :: ListFHIRImportJobs)

-- | This parameter limits the response to FHIR import jobs submitted before
-- a user specified date.
listFHIRImportJobs_submittedBefore :: Lens.Lens' ListFHIRImportJobs (Prelude.Maybe Prelude.UTCTime)
listFHIRImportJobs_submittedBefore = Lens.lens (\ListFHIRImportJobs' {submittedBefore} -> submittedBefore) (\s@ListFHIRImportJobs' {} a -> s {submittedBefore = a} :: ListFHIRImportJobs) Prelude.. Lens.mapping Core._Time

-- | A pagination token used to identify the next page of results to return
-- for a ListFHIRImportJobs query.
listFHIRImportJobs_nextToken :: Lens.Lens' ListFHIRImportJobs (Prelude.Maybe Prelude.Text)
listFHIRImportJobs_nextToken = Lens.lens (\ListFHIRImportJobs' {nextToken} -> nextToken) (\s@ListFHIRImportJobs' {} a -> s {nextToken = a} :: ListFHIRImportJobs)

-- | This parameter limits the response to the import job with the specified
-- job status.
listFHIRImportJobs_jobStatus :: Lens.Lens' ListFHIRImportJobs (Prelude.Maybe JobStatus)
listFHIRImportJobs_jobStatus = Lens.lens (\ListFHIRImportJobs' {jobStatus} -> jobStatus) (\s@ListFHIRImportJobs' {} a -> s {jobStatus = a} :: ListFHIRImportJobs)

-- | This parameter limits the number of results returned for a
-- ListFHIRImportJobs to a maximum quantity specified by the user.
listFHIRImportJobs_maxResults :: Lens.Lens' ListFHIRImportJobs (Prelude.Maybe Prelude.Natural)
listFHIRImportJobs_maxResults = Lens.lens (\ListFHIRImportJobs' {maxResults} -> maxResults) (\s@ListFHIRImportJobs' {} a -> s {maxResults = a} :: ListFHIRImportJobs)

-- | This parameter limits the response to the import job with the specified
-- Data Store ID.
listFHIRImportJobs_datastoreId :: Lens.Lens' ListFHIRImportJobs Prelude.Text
listFHIRImportJobs_datastoreId = Lens.lens (\ListFHIRImportJobs' {datastoreId} -> datastoreId) (\s@ListFHIRImportJobs' {} a -> s {datastoreId = a} :: ListFHIRImportJobs)

instance Core.AWSRequest ListFHIRImportJobs where
  type
    AWSResponse ListFHIRImportJobs =
      ListFHIRImportJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFHIRImportJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "ImportJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListFHIRImportJobs

instance Prelude.NFData ListFHIRImportJobs

instance Core.ToHeaders ListFHIRImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "HealthLake.ListFHIRImportJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFHIRImportJobs where
  toJSON ListFHIRImportJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SubmittedAfter" Core..=)
              Prelude.<$> submittedAfter,
            ("JobName" Core..=) Prelude.<$> jobName,
            ("SubmittedBefore" Core..=)
              Prelude.<$> submittedBefore,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("JobStatus" Core..=) Prelude.<$> jobStatus,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("DatastoreId" Core..= datastoreId)
          ]
      )

instance Core.ToPath ListFHIRImportJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListFHIRImportJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFHIRImportJobsResponse' smart constructor.
data ListFHIRImportJobsResponse = ListFHIRImportJobsResponse'
  { -- | A pagination token used to identify the next page of results to return
    -- for a ListFHIRImportJobs query.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The properties of a listed FHIR import jobs, including the ID, ARN,
    -- name, and the status of the job.
    importJobPropertiesList :: [ImportJobProperties]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFHIRImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFHIRImportJobsResponse_nextToken' - A pagination token used to identify the next page of results to return
-- for a ListFHIRImportJobs query.
--
-- 'httpStatus', 'listFHIRImportJobsResponse_httpStatus' - The response's http status code.
--
-- 'importJobPropertiesList', 'listFHIRImportJobsResponse_importJobPropertiesList' - The properties of a listed FHIR import jobs, including the ID, ARN,
-- name, and the status of the job.
newListFHIRImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFHIRImportJobsResponse
newListFHIRImportJobsResponse pHttpStatus_ =
  ListFHIRImportJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      importJobPropertiesList = Prelude.mempty
    }

-- | A pagination token used to identify the next page of results to return
-- for a ListFHIRImportJobs query.
listFHIRImportJobsResponse_nextToken :: Lens.Lens' ListFHIRImportJobsResponse (Prelude.Maybe Prelude.Text)
listFHIRImportJobsResponse_nextToken = Lens.lens (\ListFHIRImportJobsResponse' {nextToken} -> nextToken) (\s@ListFHIRImportJobsResponse' {} a -> s {nextToken = a} :: ListFHIRImportJobsResponse)

-- | The response's http status code.
listFHIRImportJobsResponse_httpStatus :: Lens.Lens' ListFHIRImportJobsResponse Prelude.Int
listFHIRImportJobsResponse_httpStatus = Lens.lens (\ListFHIRImportJobsResponse' {httpStatus} -> httpStatus) (\s@ListFHIRImportJobsResponse' {} a -> s {httpStatus = a} :: ListFHIRImportJobsResponse)

-- | The properties of a listed FHIR import jobs, including the ID, ARN,
-- name, and the status of the job.
listFHIRImportJobsResponse_importJobPropertiesList :: Lens.Lens' ListFHIRImportJobsResponse [ImportJobProperties]
listFHIRImportJobsResponse_importJobPropertiesList = Lens.lens (\ListFHIRImportJobsResponse' {importJobPropertiesList} -> importJobPropertiesList) (\s@ListFHIRImportJobsResponse' {} a -> s {importJobPropertiesList = a} :: ListFHIRImportJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFHIRImportJobsResponse

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
-- Module      : Amazonka.HealthLake.ListFHIRExportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all FHIR export jobs associated with an account and their
-- statuses.
module Amazonka.HealthLake.ListFHIRExportJobs
  ( -- * Creating a Request
    ListFHIRExportJobs (..),
    newListFHIRExportJobs,

    -- * Request Lenses
    listFHIRExportJobs_jobName,
    listFHIRExportJobs_jobStatus,
    listFHIRExportJobs_maxResults,
    listFHIRExportJobs_nextToken,
    listFHIRExportJobs_submittedAfter,
    listFHIRExportJobs_submittedBefore,
    listFHIRExportJobs_datastoreId,

    -- * Destructuring the Response
    ListFHIRExportJobsResponse (..),
    newListFHIRExportJobsResponse,

    -- * Response Lenses
    listFHIRExportJobsResponse_nextToken,
    listFHIRExportJobsResponse_httpStatus,
    listFHIRExportJobsResponse_exportJobPropertiesList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFHIRExportJobs' smart constructor.
data ListFHIRExportJobs = ListFHIRExportJobs'
  { -- | This parameter limits the response to the export job with the specified
    -- job name.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | This parameter limits the response to the export jobs with the specified
    -- job status.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | This parameter limits the number of results returned for a
    -- ListFHIRExportJobs to a maximum quantity specified by the user.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token used to identify the next page of results to return
    -- for a ListFHIRExportJobs query.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This parameter limits the response to FHIR export jobs submitted after a
    -- user specified date.
    submittedAfter :: Prelude.Maybe Data.POSIX,
    -- | This parameter limits the response to FHIR export jobs submitted before
    -- a user specified date.
    submittedBefore :: Prelude.Maybe Data.POSIX,
    -- | This parameter limits the response to the export job with the specified
    -- Data Store ID.
    datastoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFHIRExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'listFHIRExportJobs_jobName' - This parameter limits the response to the export job with the specified
-- job name.
--
-- 'jobStatus', 'listFHIRExportJobs_jobStatus' - This parameter limits the response to the export jobs with the specified
-- job status.
--
-- 'maxResults', 'listFHIRExportJobs_maxResults' - This parameter limits the number of results returned for a
-- ListFHIRExportJobs to a maximum quantity specified by the user.
--
-- 'nextToken', 'listFHIRExportJobs_nextToken' - A pagination token used to identify the next page of results to return
-- for a ListFHIRExportJobs query.
--
-- 'submittedAfter', 'listFHIRExportJobs_submittedAfter' - This parameter limits the response to FHIR export jobs submitted after a
-- user specified date.
--
-- 'submittedBefore', 'listFHIRExportJobs_submittedBefore' - This parameter limits the response to FHIR export jobs submitted before
-- a user specified date.
--
-- 'datastoreId', 'listFHIRExportJobs_datastoreId' - This parameter limits the response to the export job with the specified
-- Data Store ID.
newListFHIRExportJobs ::
  -- | 'datastoreId'
  Prelude.Text ->
  ListFHIRExportJobs
newListFHIRExportJobs pDatastoreId_ =
  ListFHIRExportJobs'
    { jobName = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      submittedAfter = Prelude.Nothing,
      submittedBefore = Prelude.Nothing,
      datastoreId = pDatastoreId_
    }

-- | This parameter limits the response to the export job with the specified
-- job name.
listFHIRExportJobs_jobName :: Lens.Lens' ListFHIRExportJobs (Prelude.Maybe Prelude.Text)
listFHIRExportJobs_jobName = Lens.lens (\ListFHIRExportJobs' {jobName} -> jobName) (\s@ListFHIRExportJobs' {} a -> s {jobName = a} :: ListFHIRExportJobs)

-- | This parameter limits the response to the export jobs with the specified
-- job status.
listFHIRExportJobs_jobStatus :: Lens.Lens' ListFHIRExportJobs (Prelude.Maybe JobStatus)
listFHIRExportJobs_jobStatus = Lens.lens (\ListFHIRExportJobs' {jobStatus} -> jobStatus) (\s@ListFHIRExportJobs' {} a -> s {jobStatus = a} :: ListFHIRExportJobs)

-- | This parameter limits the number of results returned for a
-- ListFHIRExportJobs to a maximum quantity specified by the user.
listFHIRExportJobs_maxResults :: Lens.Lens' ListFHIRExportJobs (Prelude.Maybe Prelude.Natural)
listFHIRExportJobs_maxResults = Lens.lens (\ListFHIRExportJobs' {maxResults} -> maxResults) (\s@ListFHIRExportJobs' {} a -> s {maxResults = a} :: ListFHIRExportJobs)

-- | A pagination token used to identify the next page of results to return
-- for a ListFHIRExportJobs query.
listFHIRExportJobs_nextToken :: Lens.Lens' ListFHIRExportJobs (Prelude.Maybe Prelude.Text)
listFHIRExportJobs_nextToken = Lens.lens (\ListFHIRExportJobs' {nextToken} -> nextToken) (\s@ListFHIRExportJobs' {} a -> s {nextToken = a} :: ListFHIRExportJobs)

-- | This parameter limits the response to FHIR export jobs submitted after a
-- user specified date.
listFHIRExportJobs_submittedAfter :: Lens.Lens' ListFHIRExportJobs (Prelude.Maybe Prelude.UTCTime)
listFHIRExportJobs_submittedAfter = Lens.lens (\ListFHIRExportJobs' {submittedAfter} -> submittedAfter) (\s@ListFHIRExportJobs' {} a -> s {submittedAfter = a} :: ListFHIRExportJobs) Prelude.. Lens.mapping Data._Time

-- | This parameter limits the response to FHIR export jobs submitted before
-- a user specified date.
listFHIRExportJobs_submittedBefore :: Lens.Lens' ListFHIRExportJobs (Prelude.Maybe Prelude.UTCTime)
listFHIRExportJobs_submittedBefore = Lens.lens (\ListFHIRExportJobs' {submittedBefore} -> submittedBefore) (\s@ListFHIRExportJobs' {} a -> s {submittedBefore = a} :: ListFHIRExportJobs) Prelude.. Lens.mapping Data._Time

-- | This parameter limits the response to the export job with the specified
-- Data Store ID.
listFHIRExportJobs_datastoreId :: Lens.Lens' ListFHIRExportJobs Prelude.Text
listFHIRExportJobs_datastoreId = Lens.lens (\ListFHIRExportJobs' {datastoreId} -> datastoreId) (\s@ListFHIRExportJobs' {} a -> s {datastoreId = a} :: ListFHIRExportJobs)

instance Core.AWSRequest ListFHIRExportJobs where
  type
    AWSResponse ListFHIRExportJobs =
      ListFHIRExportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFHIRExportJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "ExportJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListFHIRExportJobs where
  hashWithSalt _salt ListFHIRExportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` submittedAfter
      `Prelude.hashWithSalt` submittedBefore
      `Prelude.hashWithSalt` datastoreId

instance Prelude.NFData ListFHIRExportJobs where
  rnf ListFHIRExportJobs' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf submittedAfter
      `Prelude.seq` Prelude.rnf submittedBefore
      `Prelude.seq` Prelude.rnf datastoreId

instance Data.ToHeaders ListFHIRExportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "HealthLake.ListFHIRExportJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFHIRExportJobs where
  toJSON ListFHIRExportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobName" Data..=) Prelude.<$> jobName,
            ("JobStatus" Data..=) Prelude.<$> jobStatus,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SubmittedAfter" Data..=)
              Prelude.<$> submittedAfter,
            ("SubmittedBefore" Data..=)
              Prelude.<$> submittedBefore,
            Prelude.Just ("DatastoreId" Data..= datastoreId)
          ]
      )

instance Data.ToPath ListFHIRExportJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFHIRExportJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFHIRExportJobsResponse' smart constructor.
data ListFHIRExportJobsResponse = ListFHIRExportJobsResponse'
  { -- | A pagination token used to identify the next page of results to return
    -- for a ListFHIRExportJobs query.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The properties of listed FHIR export jobs, including the ID, ARN, name,
    -- and the status of the job.
    exportJobPropertiesList :: [ExportJobProperties]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFHIRExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFHIRExportJobsResponse_nextToken' - A pagination token used to identify the next page of results to return
-- for a ListFHIRExportJobs query.
--
-- 'httpStatus', 'listFHIRExportJobsResponse_httpStatus' - The response's http status code.
--
-- 'exportJobPropertiesList', 'listFHIRExportJobsResponse_exportJobPropertiesList' - The properties of listed FHIR export jobs, including the ID, ARN, name,
-- and the status of the job.
newListFHIRExportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFHIRExportJobsResponse
newListFHIRExportJobsResponse pHttpStatus_ =
  ListFHIRExportJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      exportJobPropertiesList = Prelude.mempty
    }

-- | A pagination token used to identify the next page of results to return
-- for a ListFHIRExportJobs query.
listFHIRExportJobsResponse_nextToken :: Lens.Lens' ListFHIRExportJobsResponse (Prelude.Maybe Prelude.Text)
listFHIRExportJobsResponse_nextToken = Lens.lens (\ListFHIRExportJobsResponse' {nextToken} -> nextToken) (\s@ListFHIRExportJobsResponse' {} a -> s {nextToken = a} :: ListFHIRExportJobsResponse)

-- | The response's http status code.
listFHIRExportJobsResponse_httpStatus :: Lens.Lens' ListFHIRExportJobsResponse Prelude.Int
listFHIRExportJobsResponse_httpStatus = Lens.lens (\ListFHIRExportJobsResponse' {httpStatus} -> httpStatus) (\s@ListFHIRExportJobsResponse' {} a -> s {httpStatus = a} :: ListFHIRExportJobsResponse)

-- | The properties of listed FHIR export jobs, including the ID, ARN, name,
-- and the status of the job.
listFHIRExportJobsResponse_exportJobPropertiesList :: Lens.Lens' ListFHIRExportJobsResponse [ExportJobProperties]
listFHIRExportJobsResponse_exportJobPropertiesList = Lens.lens (\ListFHIRExportJobsResponse' {exportJobPropertiesList} -> exportJobPropertiesList) (\s@ListFHIRExportJobsResponse' {} a -> s {exportJobPropertiesList = a} :: ListFHIRExportJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFHIRExportJobsResponse where
  rnf ListFHIRExportJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf exportJobPropertiesList

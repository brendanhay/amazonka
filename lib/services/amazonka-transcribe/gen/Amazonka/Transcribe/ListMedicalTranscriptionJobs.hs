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
-- Module      : Amazonka.Transcribe.ListMedicalTranscriptionJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists medical transcription jobs with a specified status or substring
-- that matches their names.
module Amazonka.Transcribe.ListMedicalTranscriptionJobs
  ( -- * Creating a Request
    ListMedicalTranscriptionJobs (..),
    newListMedicalTranscriptionJobs,

    -- * Request Lenses
    listMedicalTranscriptionJobs_nextToken,
    listMedicalTranscriptionJobs_status,
    listMedicalTranscriptionJobs_maxResults,
    listMedicalTranscriptionJobs_jobNameContains,

    -- * Destructuring the Response
    ListMedicalTranscriptionJobsResponse (..),
    newListMedicalTranscriptionJobsResponse,

    -- * Response Lenses
    listMedicalTranscriptionJobsResponse_nextToken,
    listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries,
    listMedicalTranscriptionJobsResponse_status,
    listMedicalTranscriptionJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListMedicalTranscriptionJobs' smart constructor.
data ListMedicalTranscriptionJobs = ListMedicalTranscriptionJobs'
  { -- | If you a receive a truncated result in the previous request of
    -- @ListMedicalTranscriptionJobs@, include @NextToken@ to fetch the next
    -- set of jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When specified, returns only medical transcription jobs with the
    -- specified status. Jobs are ordered by creation date, with the newest
    -- jobs returned first. If you don\'t specify a status, Amazon Transcribe
    -- Medical returns all transcription jobs ordered by creation date.
    status :: Prelude.Maybe TranscriptionJobStatus,
    -- | The maximum number of medical transcription jobs to return in each page
    -- of results. If there are fewer results than the value you specify, only
    -- the actual results are returned. If you do not specify a value, the
    -- default of 5 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When specified, the jobs returned in the list are limited to jobs whose
    -- name contains the specified string.
    jobNameContains :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMedicalTranscriptionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMedicalTranscriptionJobs_nextToken' - If you a receive a truncated result in the previous request of
-- @ListMedicalTranscriptionJobs@, include @NextToken@ to fetch the next
-- set of jobs.
--
-- 'status', 'listMedicalTranscriptionJobs_status' - When specified, returns only medical transcription jobs with the
-- specified status. Jobs are ordered by creation date, with the newest
-- jobs returned first. If you don\'t specify a status, Amazon Transcribe
-- Medical returns all transcription jobs ordered by creation date.
--
-- 'maxResults', 'listMedicalTranscriptionJobs_maxResults' - The maximum number of medical transcription jobs to return in each page
-- of results. If there are fewer results than the value you specify, only
-- the actual results are returned. If you do not specify a value, the
-- default of 5 is used.
--
-- 'jobNameContains', 'listMedicalTranscriptionJobs_jobNameContains' - When specified, the jobs returned in the list are limited to jobs whose
-- name contains the specified string.
newListMedicalTranscriptionJobs ::
  ListMedicalTranscriptionJobs
newListMedicalTranscriptionJobs =
  ListMedicalTranscriptionJobs'
    { nextToken =
        Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobNameContains = Prelude.Nothing
    }

-- | If you a receive a truncated result in the previous request of
-- @ListMedicalTranscriptionJobs@, include @NextToken@ to fetch the next
-- set of jobs.
listMedicalTranscriptionJobs_nextToken :: Lens.Lens' ListMedicalTranscriptionJobs (Prelude.Maybe Prelude.Text)
listMedicalTranscriptionJobs_nextToken = Lens.lens (\ListMedicalTranscriptionJobs' {nextToken} -> nextToken) (\s@ListMedicalTranscriptionJobs' {} a -> s {nextToken = a} :: ListMedicalTranscriptionJobs)

-- | When specified, returns only medical transcription jobs with the
-- specified status. Jobs are ordered by creation date, with the newest
-- jobs returned first. If you don\'t specify a status, Amazon Transcribe
-- Medical returns all transcription jobs ordered by creation date.
listMedicalTranscriptionJobs_status :: Lens.Lens' ListMedicalTranscriptionJobs (Prelude.Maybe TranscriptionJobStatus)
listMedicalTranscriptionJobs_status = Lens.lens (\ListMedicalTranscriptionJobs' {status} -> status) (\s@ListMedicalTranscriptionJobs' {} a -> s {status = a} :: ListMedicalTranscriptionJobs)

-- | The maximum number of medical transcription jobs to return in each page
-- of results. If there are fewer results than the value you specify, only
-- the actual results are returned. If you do not specify a value, the
-- default of 5 is used.
listMedicalTranscriptionJobs_maxResults :: Lens.Lens' ListMedicalTranscriptionJobs (Prelude.Maybe Prelude.Natural)
listMedicalTranscriptionJobs_maxResults = Lens.lens (\ListMedicalTranscriptionJobs' {maxResults} -> maxResults) (\s@ListMedicalTranscriptionJobs' {} a -> s {maxResults = a} :: ListMedicalTranscriptionJobs)

-- | When specified, the jobs returned in the list are limited to jobs whose
-- name contains the specified string.
listMedicalTranscriptionJobs_jobNameContains :: Lens.Lens' ListMedicalTranscriptionJobs (Prelude.Maybe Prelude.Text)
listMedicalTranscriptionJobs_jobNameContains = Lens.lens (\ListMedicalTranscriptionJobs' {jobNameContains} -> jobNameContains) (\s@ListMedicalTranscriptionJobs' {} a -> s {jobNameContains = a} :: ListMedicalTranscriptionJobs)

instance Core.AWSRequest ListMedicalTranscriptionJobs where
  type
    AWSResponse ListMedicalTranscriptionJobs =
      ListMedicalTranscriptionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMedicalTranscriptionJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "MedicalTranscriptionJobSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListMedicalTranscriptionJobs
  where
  hashWithSalt _salt ListMedicalTranscriptionJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` jobNameContains

instance Prelude.NFData ListMedicalTranscriptionJobs where
  rnf ListMedicalTranscriptionJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf jobNameContains

instance Core.ToHeaders ListMedicalTranscriptionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListMedicalTranscriptionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListMedicalTranscriptionJobs where
  toJSON ListMedicalTranscriptionJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Status" Core..=) Prelude.<$> status,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("JobNameContains" Core..=)
              Prelude.<$> jobNameContains
          ]
      )

instance Core.ToPath ListMedicalTranscriptionJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMedicalTranscriptionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMedicalTranscriptionJobsResponse' smart constructor.
data ListMedicalTranscriptionJobsResponse = ListMedicalTranscriptionJobsResponse'
  { -- | The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a
    -- time. The maximum size of the page is set by the @MaxResults@ parameter.
    -- If the number of jobs exceeds what can fit on a page, Amazon Transcribe
    -- Medical returns the @NextPage@ token. Include the token in the next
    -- request to the @ListMedicalTranscriptionJobs@ operation to return in the
    -- next page of jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of objects containing summary information for a transcription
    -- job.
    medicalTranscriptionJobSummaries :: Prelude.Maybe [MedicalTranscriptionJobSummary],
    -- | The requested status of the medical transcription jobs returned.
    status :: Prelude.Maybe TranscriptionJobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMedicalTranscriptionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMedicalTranscriptionJobsResponse_nextToken' - The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a
-- time. The maximum size of the page is set by the @MaxResults@ parameter.
-- If the number of jobs exceeds what can fit on a page, Amazon Transcribe
-- Medical returns the @NextPage@ token. Include the token in the next
-- request to the @ListMedicalTranscriptionJobs@ operation to return in the
-- next page of jobs.
--
-- 'medicalTranscriptionJobSummaries', 'listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries' - A list of objects containing summary information for a transcription
-- job.
--
-- 'status', 'listMedicalTranscriptionJobsResponse_status' - The requested status of the medical transcription jobs returned.
--
-- 'httpStatus', 'listMedicalTranscriptionJobsResponse_httpStatus' - The response's http status code.
newListMedicalTranscriptionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMedicalTranscriptionJobsResponse
newListMedicalTranscriptionJobsResponse pHttpStatus_ =
  ListMedicalTranscriptionJobsResponse'
    { nextToken =
        Prelude.Nothing,
      medicalTranscriptionJobSummaries =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a
-- time. The maximum size of the page is set by the @MaxResults@ parameter.
-- If the number of jobs exceeds what can fit on a page, Amazon Transcribe
-- Medical returns the @NextPage@ token. Include the token in the next
-- request to the @ListMedicalTranscriptionJobs@ operation to return in the
-- next page of jobs.
listMedicalTranscriptionJobsResponse_nextToken :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Prelude.Maybe Prelude.Text)
listMedicalTranscriptionJobsResponse_nextToken = Lens.lens (\ListMedicalTranscriptionJobsResponse' {nextToken} -> nextToken) (\s@ListMedicalTranscriptionJobsResponse' {} a -> s {nextToken = a} :: ListMedicalTranscriptionJobsResponse)

-- | A list of objects containing summary information for a transcription
-- job.
listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Prelude.Maybe [MedicalTranscriptionJobSummary])
listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries = Lens.lens (\ListMedicalTranscriptionJobsResponse' {medicalTranscriptionJobSummaries} -> medicalTranscriptionJobSummaries) (\s@ListMedicalTranscriptionJobsResponse' {} a -> s {medicalTranscriptionJobSummaries = a} :: ListMedicalTranscriptionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The requested status of the medical transcription jobs returned.
listMedicalTranscriptionJobsResponse_status :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Prelude.Maybe TranscriptionJobStatus)
listMedicalTranscriptionJobsResponse_status = Lens.lens (\ListMedicalTranscriptionJobsResponse' {status} -> status) (\s@ListMedicalTranscriptionJobsResponse' {} a -> s {status = a} :: ListMedicalTranscriptionJobsResponse)

-- | The response's http status code.
listMedicalTranscriptionJobsResponse_httpStatus :: Lens.Lens' ListMedicalTranscriptionJobsResponse Prelude.Int
listMedicalTranscriptionJobsResponse_httpStatus = Lens.lens (\ListMedicalTranscriptionJobsResponse' {httpStatus} -> httpStatus) (\s@ListMedicalTranscriptionJobsResponse' {} a -> s {httpStatus = a} :: ListMedicalTranscriptionJobsResponse)

instance
  Prelude.NFData
    ListMedicalTranscriptionJobsResponse
  where
  rnf ListMedicalTranscriptionJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf medicalTranscriptionJobSummaries
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of medical transcription jobs that match the specified
-- criteria. If no criteria are specified, all medical transcription jobs
-- are returned.
--
-- To get detailed information about a specific medical transcription job,
-- use the operation.
module Amazonka.Transcribe.ListMedicalTranscriptionJobs
  ( -- * Creating a Request
    ListMedicalTranscriptionJobs (..),
    newListMedicalTranscriptionJobs,

    -- * Request Lenses
    listMedicalTranscriptionJobs_jobNameContains,
    listMedicalTranscriptionJobs_maxResults,
    listMedicalTranscriptionJobs_nextToken,
    listMedicalTranscriptionJobs_status,

    -- * Destructuring the Response
    ListMedicalTranscriptionJobsResponse (..),
    newListMedicalTranscriptionJobsResponse,

    -- * Response Lenses
    listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries,
    listMedicalTranscriptionJobsResponse_nextToken,
    listMedicalTranscriptionJobsResponse_status,
    listMedicalTranscriptionJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListMedicalTranscriptionJobs' smart constructor.
data ListMedicalTranscriptionJobs = ListMedicalTranscriptionJobs'
  { -- | Returns only the medical transcription jobs that contain the specified
    -- string. The search is not case sensitive.
    jobNameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of medical transcription jobs to return in each page
    -- of results. If there are fewer results than the value that you specify,
    -- only the actual results are returned. If you don\'t specify a value, a
    -- default of 5 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your @ListMedicalTranscriptionJobs@ request returns more results than
    -- can be displayed, @NextToken@ is displayed in the response with an
    -- associated string. To get the next page of results, copy this string and
    -- repeat your request, including @NextToken@ with the value of the copied
    -- string. Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns only medical transcription jobs with the specified status. Jobs
    -- are ordered by creation date, with the newest job first. If you don\'t
    -- include @Status@, all medical transcription jobs are returned.
    status :: Prelude.Maybe TranscriptionJobStatus
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
-- 'jobNameContains', 'listMedicalTranscriptionJobs_jobNameContains' - Returns only the medical transcription jobs that contain the specified
-- string. The search is not case sensitive.
--
-- 'maxResults', 'listMedicalTranscriptionJobs_maxResults' - The maximum number of medical transcription jobs to return in each page
-- of results. If there are fewer results than the value that you specify,
-- only the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
--
-- 'nextToken', 'listMedicalTranscriptionJobs_nextToken' - If your @ListMedicalTranscriptionJobs@ request returns more results than
-- can be displayed, @NextToken@ is displayed in the response with an
-- associated string. To get the next page of results, copy this string and
-- repeat your request, including @NextToken@ with the value of the copied
-- string. Repeat as needed to view all your results.
--
-- 'status', 'listMedicalTranscriptionJobs_status' - Returns only medical transcription jobs with the specified status. Jobs
-- are ordered by creation date, with the newest job first. If you don\'t
-- include @Status@, all medical transcription jobs are returned.
newListMedicalTranscriptionJobs ::
  ListMedicalTranscriptionJobs
newListMedicalTranscriptionJobs =
  ListMedicalTranscriptionJobs'
    { jobNameContains =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Returns only the medical transcription jobs that contain the specified
-- string. The search is not case sensitive.
listMedicalTranscriptionJobs_jobNameContains :: Lens.Lens' ListMedicalTranscriptionJobs (Prelude.Maybe Prelude.Text)
listMedicalTranscriptionJobs_jobNameContains = Lens.lens (\ListMedicalTranscriptionJobs' {jobNameContains} -> jobNameContains) (\s@ListMedicalTranscriptionJobs' {} a -> s {jobNameContains = a} :: ListMedicalTranscriptionJobs)

-- | The maximum number of medical transcription jobs to return in each page
-- of results. If there are fewer results than the value that you specify,
-- only the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
listMedicalTranscriptionJobs_maxResults :: Lens.Lens' ListMedicalTranscriptionJobs (Prelude.Maybe Prelude.Natural)
listMedicalTranscriptionJobs_maxResults = Lens.lens (\ListMedicalTranscriptionJobs' {maxResults} -> maxResults) (\s@ListMedicalTranscriptionJobs' {} a -> s {maxResults = a} :: ListMedicalTranscriptionJobs)

-- | If your @ListMedicalTranscriptionJobs@ request returns more results than
-- can be displayed, @NextToken@ is displayed in the response with an
-- associated string. To get the next page of results, copy this string and
-- repeat your request, including @NextToken@ with the value of the copied
-- string. Repeat as needed to view all your results.
listMedicalTranscriptionJobs_nextToken :: Lens.Lens' ListMedicalTranscriptionJobs (Prelude.Maybe Prelude.Text)
listMedicalTranscriptionJobs_nextToken = Lens.lens (\ListMedicalTranscriptionJobs' {nextToken} -> nextToken) (\s@ListMedicalTranscriptionJobs' {} a -> s {nextToken = a} :: ListMedicalTranscriptionJobs)

-- | Returns only medical transcription jobs with the specified status. Jobs
-- are ordered by creation date, with the newest job first. If you don\'t
-- include @Status@, all medical transcription jobs are returned.
listMedicalTranscriptionJobs_status :: Lens.Lens' ListMedicalTranscriptionJobs (Prelude.Maybe TranscriptionJobStatus)
listMedicalTranscriptionJobs_status = Lens.lens (\ListMedicalTranscriptionJobs' {status} -> status) (\s@ListMedicalTranscriptionJobs' {} a -> s {status = a} :: ListMedicalTranscriptionJobs)

instance Core.AWSRequest ListMedicalTranscriptionJobs where
  type
    AWSResponse ListMedicalTranscriptionJobs =
      ListMedicalTranscriptionJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMedicalTranscriptionJobsResponse'
            Prelude.<$> ( x Data..?> "MedicalTranscriptionJobSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListMedicalTranscriptionJobs
  where
  hashWithSalt _salt ListMedicalTranscriptionJobs' {..} =
    _salt `Prelude.hashWithSalt` jobNameContains
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListMedicalTranscriptionJobs where
  rnf ListMedicalTranscriptionJobs' {..} =
    Prelude.rnf jobNameContains
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders ListMedicalTranscriptionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.ListMedicalTranscriptionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMedicalTranscriptionJobs where
  toJSON ListMedicalTranscriptionJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobNameContains" Data..=)
              Prelude.<$> jobNameContains,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath ListMedicalTranscriptionJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMedicalTranscriptionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMedicalTranscriptionJobsResponse' smart constructor.
data ListMedicalTranscriptionJobsResponse = ListMedicalTranscriptionJobsResponse'
  { -- | Provides a summary of information about each result.
    medicalTranscriptionJobSummaries :: Prelude.Maybe [MedicalTranscriptionJobSummary],
    -- | If @NextToken@ is present in your response, it indicates that not all
    -- results are displayed. To view the next set of results, copy the string
    -- associated with the @NextToken@ parameter in your results output, then
    -- run your request again including @NextToken@ with the value of the
    -- copied string. Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists all medical transcription jobs that have the status specified in
    -- your request. Jobs are ordered by creation date, with the newest job
    -- first.
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
-- 'medicalTranscriptionJobSummaries', 'listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries' - Provides a summary of information about each result.
--
-- 'nextToken', 'listMedicalTranscriptionJobsResponse_nextToken' - If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
--
-- 'status', 'listMedicalTranscriptionJobsResponse_status' - Lists all medical transcription jobs that have the status specified in
-- your request. Jobs are ordered by creation date, with the newest job
-- first.
--
-- 'httpStatus', 'listMedicalTranscriptionJobsResponse_httpStatus' - The response's http status code.
newListMedicalTranscriptionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMedicalTranscriptionJobsResponse
newListMedicalTranscriptionJobsResponse pHttpStatus_ =
  ListMedicalTranscriptionJobsResponse'
    { medicalTranscriptionJobSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides a summary of information about each result.
listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Prelude.Maybe [MedicalTranscriptionJobSummary])
listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries = Lens.lens (\ListMedicalTranscriptionJobsResponse' {medicalTranscriptionJobSummaries} -> medicalTranscriptionJobSummaries) (\s@ListMedicalTranscriptionJobsResponse' {} a -> s {medicalTranscriptionJobSummaries = a} :: ListMedicalTranscriptionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
listMedicalTranscriptionJobsResponse_nextToken :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Prelude.Maybe Prelude.Text)
listMedicalTranscriptionJobsResponse_nextToken = Lens.lens (\ListMedicalTranscriptionJobsResponse' {nextToken} -> nextToken) (\s@ListMedicalTranscriptionJobsResponse' {} a -> s {nextToken = a} :: ListMedicalTranscriptionJobsResponse)

-- | Lists all medical transcription jobs that have the status specified in
-- your request. Jobs are ordered by creation date, with the newest job
-- first.
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
    Prelude.rnf medicalTranscriptionJobSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus

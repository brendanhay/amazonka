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
-- Module      : Amazonka.Transcribe.ListTranscriptionJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of transcription jobs that match the specified criteria.
-- If no criteria are specified, all transcription jobs are returned.
--
-- To get detailed information about a specific transcription job, use the
-- operation.
module Amazonka.Transcribe.ListTranscriptionJobs
  ( -- * Creating a Request
    ListTranscriptionJobs (..),
    newListTranscriptionJobs,

    -- * Request Lenses
    listTranscriptionJobs_jobNameContains,
    listTranscriptionJobs_maxResults,
    listTranscriptionJobs_nextToken,
    listTranscriptionJobs_status,

    -- * Destructuring the Response
    ListTranscriptionJobsResponse (..),
    newListTranscriptionJobsResponse,

    -- * Response Lenses
    listTranscriptionJobsResponse_nextToken,
    listTranscriptionJobsResponse_status,
    listTranscriptionJobsResponse_transcriptionJobSummaries,
    listTranscriptionJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListTranscriptionJobs' smart constructor.
data ListTranscriptionJobs = ListTranscriptionJobs'
  { -- | Returns only the transcription jobs that contain the specified string.
    -- The search is not case sensitive.
    jobNameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of transcription jobs to return in each page of
    -- results. If there are fewer results than the value that you specify,
    -- only the actual results are returned. If you don\'t specify a value, a
    -- default of 5 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your @ListTranscriptionJobs@ request returns more results than can be
    -- displayed, @NextToken@ is displayed in the response with an associated
    -- string. To get the next page of results, copy this string and repeat
    -- your request, including @NextToken@ with the value of the copied string.
    -- Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns only transcription jobs with the specified status. Jobs are
    -- ordered by creation date, with the newest job first. If you don\'t
    -- include @Status@, all transcription jobs are returned.
    status :: Prelude.Maybe TranscriptionJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTranscriptionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobNameContains', 'listTranscriptionJobs_jobNameContains' - Returns only the transcription jobs that contain the specified string.
-- The search is not case sensitive.
--
-- 'maxResults', 'listTranscriptionJobs_maxResults' - The maximum number of transcription jobs to return in each page of
-- results. If there are fewer results than the value that you specify,
-- only the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
--
-- 'nextToken', 'listTranscriptionJobs_nextToken' - If your @ListTranscriptionJobs@ request returns more results than can be
-- displayed, @NextToken@ is displayed in the response with an associated
-- string. To get the next page of results, copy this string and repeat
-- your request, including @NextToken@ with the value of the copied string.
-- Repeat as needed to view all your results.
--
-- 'status', 'listTranscriptionJobs_status' - Returns only transcription jobs with the specified status. Jobs are
-- ordered by creation date, with the newest job first. If you don\'t
-- include @Status@, all transcription jobs are returned.
newListTranscriptionJobs ::
  ListTranscriptionJobs
newListTranscriptionJobs =
  ListTranscriptionJobs'
    { jobNameContains =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Returns only the transcription jobs that contain the specified string.
-- The search is not case sensitive.
listTranscriptionJobs_jobNameContains :: Lens.Lens' ListTranscriptionJobs (Prelude.Maybe Prelude.Text)
listTranscriptionJobs_jobNameContains = Lens.lens (\ListTranscriptionJobs' {jobNameContains} -> jobNameContains) (\s@ListTranscriptionJobs' {} a -> s {jobNameContains = a} :: ListTranscriptionJobs)

-- | The maximum number of transcription jobs to return in each page of
-- results. If there are fewer results than the value that you specify,
-- only the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
listTranscriptionJobs_maxResults :: Lens.Lens' ListTranscriptionJobs (Prelude.Maybe Prelude.Natural)
listTranscriptionJobs_maxResults = Lens.lens (\ListTranscriptionJobs' {maxResults} -> maxResults) (\s@ListTranscriptionJobs' {} a -> s {maxResults = a} :: ListTranscriptionJobs)

-- | If your @ListTranscriptionJobs@ request returns more results than can be
-- displayed, @NextToken@ is displayed in the response with an associated
-- string. To get the next page of results, copy this string and repeat
-- your request, including @NextToken@ with the value of the copied string.
-- Repeat as needed to view all your results.
listTranscriptionJobs_nextToken :: Lens.Lens' ListTranscriptionJobs (Prelude.Maybe Prelude.Text)
listTranscriptionJobs_nextToken = Lens.lens (\ListTranscriptionJobs' {nextToken} -> nextToken) (\s@ListTranscriptionJobs' {} a -> s {nextToken = a} :: ListTranscriptionJobs)

-- | Returns only transcription jobs with the specified status. Jobs are
-- ordered by creation date, with the newest job first. If you don\'t
-- include @Status@, all transcription jobs are returned.
listTranscriptionJobs_status :: Lens.Lens' ListTranscriptionJobs (Prelude.Maybe TranscriptionJobStatus)
listTranscriptionJobs_status = Lens.lens (\ListTranscriptionJobs' {status} -> status) (\s@ListTranscriptionJobs' {} a -> s {status = a} :: ListTranscriptionJobs)

instance Core.AWSRequest ListTranscriptionJobs where
  type
    AWSResponse ListTranscriptionJobs =
      ListTranscriptionJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTranscriptionJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> ( x
                            Data..?> "TranscriptionJobSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTranscriptionJobs where
  hashWithSalt _salt ListTranscriptionJobs' {..} =
    _salt
      `Prelude.hashWithSalt` jobNameContains
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListTranscriptionJobs where
  rnf ListTranscriptionJobs' {..} =
    Prelude.rnf jobNameContains `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf status

instance Data.ToHeaders ListTranscriptionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.ListTranscriptionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTranscriptionJobs where
  toJSON ListTranscriptionJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobNameContains" Data..=)
              Prelude.<$> jobNameContains,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath ListTranscriptionJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTranscriptionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTranscriptionJobsResponse' smart constructor.
data ListTranscriptionJobsResponse = ListTranscriptionJobsResponse'
  { -- | If @NextToken@ is present in your response, it indicates that not all
    -- results are displayed. To view the next set of results, copy the string
    -- associated with the @NextToken@ parameter in your results output, then
    -- run your request again including @NextToken@ with the value of the
    -- copied string. Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists all transcription jobs that have the status specified in your
    -- request. Jobs are ordered by creation date, with the newest job first.
    status :: Prelude.Maybe TranscriptionJobStatus,
    -- | Provides a summary of information about each result.
    transcriptionJobSummaries :: Prelude.Maybe [TranscriptionJobSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTranscriptionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTranscriptionJobsResponse_nextToken' - If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
--
-- 'status', 'listTranscriptionJobsResponse_status' - Lists all transcription jobs that have the status specified in your
-- request. Jobs are ordered by creation date, with the newest job first.
--
-- 'transcriptionJobSummaries', 'listTranscriptionJobsResponse_transcriptionJobSummaries' - Provides a summary of information about each result.
--
-- 'httpStatus', 'listTranscriptionJobsResponse_httpStatus' - The response's http status code.
newListTranscriptionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTranscriptionJobsResponse
newListTranscriptionJobsResponse pHttpStatus_ =
  ListTranscriptionJobsResponse'
    { nextToken =
        Prelude.Nothing,
      status = Prelude.Nothing,
      transcriptionJobSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
listTranscriptionJobsResponse_nextToken :: Lens.Lens' ListTranscriptionJobsResponse (Prelude.Maybe Prelude.Text)
listTranscriptionJobsResponse_nextToken = Lens.lens (\ListTranscriptionJobsResponse' {nextToken} -> nextToken) (\s@ListTranscriptionJobsResponse' {} a -> s {nextToken = a} :: ListTranscriptionJobsResponse)

-- | Lists all transcription jobs that have the status specified in your
-- request. Jobs are ordered by creation date, with the newest job first.
listTranscriptionJobsResponse_status :: Lens.Lens' ListTranscriptionJobsResponse (Prelude.Maybe TranscriptionJobStatus)
listTranscriptionJobsResponse_status = Lens.lens (\ListTranscriptionJobsResponse' {status} -> status) (\s@ListTranscriptionJobsResponse' {} a -> s {status = a} :: ListTranscriptionJobsResponse)

-- | Provides a summary of information about each result.
listTranscriptionJobsResponse_transcriptionJobSummaries :: Lens.Lens' ListTranscriptionJobsResponse (Prelude.Maybe [TranscriptionJobSummary])
listTranscriptionJobsResponse_transcriptionJobSummaries = Lens.lens (\ListTranscriptionJobsResponse' {transcriptionJobSummaries} -> transcriptionJobSummaries) (\s@ListTranscriptionJobsResponse' {} a -> s {transcriptionJobSummaries = a} :: ListTranscriptionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTranscriptionJobsResponse_httpStatus :: Lens.Lens' ListTranscriptionJobsResponse Prelude.Int
listTranscriptionJobsResponse_httpStatus = Lens.lens (\ListTranscriptionJobsResponse' {httpStatus} -> httpStatus) (\s@ListTranscriptionJobsResponse' {} a -> s {httpStatus = a} :: ListTranscriptionJobsResponse)

instance Prelude.NFData ListTranscriptionJobsResponse where
  rnf ListTranscriptionJobsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf transcriptionJobSummaries `Prelude.seq`
          Prelude.rnf httpStatus

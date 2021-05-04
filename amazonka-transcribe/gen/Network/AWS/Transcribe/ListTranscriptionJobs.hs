{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Transcribe.ListTranscriptionJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists transcription jobs with the specified status.
module Network.AWS.Transcribe.ListTranscriptionJobs
  ( -- * Creating a Request
    ListTranscriptionJobs (..),
    newListTranscriptionJobs,

    -- * Request Lenses
    listTranscriptionJobs_status,
    listTranscriptionJobs_nextToken,
    listTranscriptionJobs_maxResults,
    listTranscriptionJobs_jobNameContains,

    -- * Destructuring the Response
    ListTranscriptionJobsResponse (..),
    newListTranscriptionJobsResponse,

    -- * Response Lenses
    listTranscriptionJobsResponse_status,
    listTranscriptionJobsResponse_nextToken,
    listTranscriptionJobsResponse_transcriptionJobSummaries,
    listTranscriptionJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newListTranscriptionJobs' smart constructor.
data ListTranscriptionJobs = ListTranscriptionJobs'
  { -- | When specified, returns only transcription jobs with the specified
    -- status. Jobs are ordered by creation date, with the newest jobs returned
    -- first. If you don’t specify a status, Amazon Transcribe returns all
    -- transcription jobs ordered by creation date.
    status :: Prelude.Maybe TranscriptionJobStatus,
    -- | If the result of the previous request to @ListTranscriptionJobs@ was
    -- truncated, include the @NextToken@ to fetch the next set of jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of jobs to return in the response. If there are fewer
    -- results in the list, this response contains only the actual results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When specified, the jobs returned in the list are limited to jobs whose
    -- name contains the specified string.
    jobNameContains :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTranscriptionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listTranscriptionJobs_status' - When specified, returns only transcription jobs with the specified
-- status. Jobs are ordered by creation date, with the newest jobs returned
-- first. If you don’t specify a status, Amazon Transcribe returns all
-- transcription jobs ordered by creation date.
--
-- 'nextToken', 'listTranscriptionJobs_nextToken' - If the result of the previous request to @ListTranscriptionJobs@ was
-- truncated, include the @NextToken@ to fetch the next set of jobs.
--
-- 'maxResults', 'listTranscriptionJobs_maxResults' - The maximum number of jobs to return in the response. If there are fewer
-- results in the list, this response contains only the actual results.
--
-- 'jobNameContains', 'listTranscriptionJobs_jobNameContains' - When specified, the jobs returned in the list are limited to jobs whose
-- name contains the specified string.
newListTranscriptionJobs ::
  ListTranscriptionJobs
newListTranscriptionJobs =
  ListTranscriptionJobs'
    { status = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobNameContains = Prelude.Nothing
    }

-- | When specified, returns only transcription jobs with the specified
-- status. Jobs are ordered by creation date, with the newest jobs returned
-- first. If you don’t specify a status, Amazon Transcribe returns all
-- transcription jobs ordered by creation date.
listTranscriptionJobs_status :: Lens.Lens' ListTranscriptionJobs (Prelude.Maybe TranscriptionJobStatus)
listTranscriptionJobs_status = Lens.lens (\ListTranscriptionJobs' {status} -> status) (\s@ListTranscriptionJobs' {} a -> s {status = a} :: ListTranscriptionJobs)

-- | If the result of the previous request to @ListTranscriptionJobs@ was
-- truncated, include the @NextToken@ to fetch the next set of jobs.
listTranscriptionJobs_nextToken :: Lens.Lens' ListTranscriptionJobs (Prelude.Maybe Prelude.Text)
listTranscriptionJobs_nextToken = Lens.lens (\ListTranscriptionJobs' {nextToken} -> nextToken) (\s@ListTranscriptionJobs' {} a -> s {nextToken = a} :: ListTranscriptionJobs)

-- | The maximum number of jobs to return in the response. If there are fewer
-- results in the list, this response contains only the actual results.
listTranscriptionJobs_maxResults :: Lens.Lens' ListTranscriptionJobs (Prelude.Maybe Prelude.Natural)
listTranscriptionJobs_maxResults = Lens.lens (\ListTranscriptionJobs' {maxResults} -> maxResults) (\s@ListTranscriptionJobs' {} a -> s {maxResults = a} :: ListTranscriptionJobs)

-- | When specified, the jobs returned in the list are limited to jobs whose
-- name contains the specified string.
listTranscriptionJobs_jobNameContains :: Lens.Lens' ListTranscriptionJobs (Prelude.Maybe Prelude.Text)
listTranscriptionJobs_jobNameContains = Lens.lens (\ListTranscriptionJobs' {jobNameContains} -> jobNameContains) (\s@ListTranscriptionJobs' {} a -> s {jobNameContains = a} :: ListTranscriptionJobs)

instance Prelude.AWSRequest ListTranscriptionJobs where
  type
    Rs ListTranscriptionJobs =
      ListTranscriptionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTranscriptionJobsResponse'
            Prelude.<$> (x Prelude..?> "Status")
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "TranscriptionJobSummaries"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTranscriptionJobs

instance Prelude.NFData ListTranscriptionJobs

instance Prelude.ToHeaders ListTranscriptionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Transcribe.ListTranscriptionJobs" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListTranscriptionJobs where
  toJSON ListTranscriptionJobs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Status" Prelude..=) Prelude.<$> status,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("JobNameContains" Prelude..=)
              Prelude.<$> jobNameContains
          ]
      )

instance Prelude.ToPath ListTranscriptionJobs where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListTranscriptionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTranscriptionJobsResponse' smart constructor.
data ListTranscriptionJobsResponse = ListTranscriptionJobsResponse'
  { -- | The requested status of the jobs returned.
    status :: Prelude.Maybe TranscriptionJobStatus,
    -- | The @ListTranscriptionJobs@ operation returns a page of jobs at a time.
    -- The maximum size of the page is set by the @MaxResults@ parameter. If
    -- there are more jobs in the list than the page size, Amazon Transcribe
    -- returns the @NextPage@ token. Include the token in the next request to
    -- the @ListTranscriptionJobs@ operation to return in the next page of
    -- jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of objects containing summary information for a transcription
    -- job.
    transcriptionJobSummaries :: Prelude.Maybe [TranscriptionJobSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTranscriptionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listTranscriptionJobsResponse_status' - The requested status of the jobs returned.
--
-- 'nextToken', 'listTranscriptionJobsResponse_nextToken' - The @ListTranscriptionJobs@ operation returns a page of jobs at a time.
-- The maximum size of the page is set by the @MaxResults@ parameter. If
-- there are more jobs in the list than the page size, Amazon Transcribe
-- returns the @NextPage@ token. Include the token in the next request to
-- the @ListTranscriptionJobs@ operation to return in the next page of
-- jobs.
--
-- 'transcriptionJobSummaries', 'listTranscriptionJobsResponse_transcriptionJobSummaries' - A list of objects containing summary information for a transcription
-- job.
--
-- 'httpStatus', 'listTranscriptionJobsResponse_httpStatus' - The response's http status code.
newListTranscriptionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTranscriptionJobsResponse
newListTranscriptionJobsResponse pHttpStatus_ =
  ListTranscriptionJobsResponse'
    { status =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      transcriptionJobSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested status of the jobs returned.
listTranscriptionJobsResponse_status :: Lens.Lens' ListTranscriptionJobsResponse (Prelude.Maybe TranscriptionJobStatus)
listTranscriptionJobsResponse_status = Lens.lens (\ListTranscriptionJobsResponse' {status} -> status) (\s@ListTranscriptionJobsResponse' {} a -> s {status = a} :: ListTranscriptionJobsResponse)

-- | The @ListTranscriptionJobs@ operation returns a page of jobs at a time.
-- The maximum size of the page is set by the @MaxResults@ parameter. If
-- there are more jobs in the list than the page size, Amazon Transcribe
-- returns the @NextPage@ token. Include the token in the next request to
-- the @ListTranscriptionJobs@ operation to return in the next page of
-- jobs.
listTranscriptionJobsResponse_nextToken :: Lens.Lens' ListTranscriptionJobsResponse (Prelude.Maybe Prelude.Text)
listTranscriptionJobsResponse_nextToken = Lens.lens (\ListTranscriptionJobsResponse' {nextToken} -> nextToken) (\s@ListTranscriptionJobsResponse' {} a -> s {nextToken = a} :: ListTranscriptionJobsResponse)

-- | A list of objects containing summary information for a transcription
-- job.
listTranscriptionJobsResponse_transcriptionJobSummaries :: Lens.Lens' ListTranscriptionJobsResponse (Prelude.Maybe [TranscriptionJobSummary])
listTranscriptionJobsResponse_transcriptionJobSummaries = Lens.lens (\ListTranscriptionJobsResponse' {transcriptionJobSummaries} -> transcriptionJobSummaries) (\s@ListTranscriptionJobsResponse' {} a -> s {transcriptionJobSummaries = a} :: ListTranscriptionJobsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listTranscriptionJobsResponse_httpStatus :: Lens.Lens' ListTranscriptionJobsResponse Prelude.Int
listTranscriptionJobsResponse_httpStatus = Lens.lens (\ListTranscriptionJobsResponse' {httpStatus} -> httpStatus) (\s@ListTranscriptionJobsResponse' {} a -> s {httpStatus = a} :: ListTranscriptionJobsResponse)

instance Prelude.NFData ListTranscriptionJobsResponse

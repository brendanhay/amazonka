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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newListTranscriptionJobs' smart constructor.
data ListTranscriptionJobs = ListTranscriptionJobs'
  { -- | When specified, returns only transcription jobs with the specified
    -- status. Jobs are ordered by creation date, with the newest jobs returned
    -- first. If you don’t specify a status, Amazon Transcribe returns all
    -- transcription jobs ordered by creation date.
    status :: Core.Maybe TranscriptionJobStatus,
    -- | If the result of the previous request to @ListTranscriptionJobs@ was
    -- truncated, include the @NextToken@ to fetch the next set of jobs.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of jobs to return in the response. If there are fewer
    -- results in the list, this response contains only the actual results.
    maxResults :: Core.Maybe Core.Natural,
    -- | When specified, the jobs returned in the list are limited to jobs whose
    -- name contains the specified string.
    jobNameContains :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { status = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      jobNameContains = Core.Nothing
    }

-- | When specified, returns only transcription jobs with the specified
-- status. Jobs are ordered by creation date, with the newest jobs returned
-- first. If you don’t specify a status, Amazon Transcribe returns all
-- transcription jobs ordered by creation date.
listTranscriptionJobs_status :: Lens.Lens' ListTranscriptionJobs (Core.Maybe TranscriptionJobStatus)
listTranscriptionJobs_status = Lens.lens (\ListTranscriptionJobs' {status} -> status) (\s@ListTranscriptionJobs' {} a -> s {status = a} :: ListTranscriptionJobs)

-- | If the result of the previous request to @ListTranscriptionJobs@ was
-- truncated, include the @NextToken@ to fetch the next set of jobs.
listTranscriptionJobs_nextToken :: Lens.Lens' ListTranscriptionJobs (Core.Maybe Core.Text)
listTranscriptionJobs_nextToken = Lens.lens (\ListTranscriptionJobs' {nextToken} -> nextToken) (\s@ListTranscriptionJobs' {} a -> s {nextToken = a} :: ListTranscriptionJobs)

-- | The maximum number of jobs to return in the response. If there are fewer
-- results in the list, this response contains only the actual results.
listTranscriptionJobs_maxResults :: Lens.Lens' ListTranscriptionJobs (Core.Maybe Core.Natural)
listTranscriptionJobs_maxResults = Lens.lens (\ListTranscriptionJobs' {maxResults} -> maxResults) (\s@ListTranscriptionJobs' {} a -> s {maxResults = a} :: ListTranscriptionJobs)

-- | When specified, the jobs returned in the list are limited to jobs whose
-- name contains the specified string.
listTranscriptionJobs_jobNameContains :: Lens.Lens' ListTranscriptionJobs (Core.Maybe Core.Text)
listTranscriptionJobs_jobNameContains = Lens.lens (\ListTranscriptionJobs' {jobNameContains} -> jobNameContains) (\s@ListTranscriptionJobs' {} a -> s {jobNameContains = a} :: ListTranscriptionJobs)

instance Core.AWSRequest ListTranscriptionJobs where
  type
    AWSResponse ListTranscriptionJobs =
      ListTranscriptionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTranscriptionJobsResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "TranscriptionJobSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTranscriptionJobs

instance Core.NFData ListTranscriptionJobs

instance Core.ToHeaders ListTranscriptionJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListTranscriptionJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTranscriptionJobs where
  toJSON ListTranscriptionJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("JobNameContains" Core..=)
              Core.<$> jobNameContains
          ]
      )

instance Core.ToPath ListTranscriptionJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListTranscriptionJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTranscriptionJobsResponse' smart constructor.
data ListTranscriptionJobsResponse = ListTranscriptionJobsResponse'
  { -- | The requested status of the jobs returned.
    status :: Core.Maybe TranscriptionJobStatus,
    -- | The @ListTranscriptionJobs@ operation returns a page of jobs at a time.
    -- The maximum size of the page is set by the @MaxResults@ parameter. If
    -- there are more jobs in the list than the page size, Amazon Transcribe
    -- returns the @NextPage@ token. Include the token in the next request to
    -- the @ListTranscriptionJobs@ operation to return in the next page of
    -- jobs.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of objects containing summary information for a transcription
    -- job.
    transcriptionJobSummaries :: Core.Maybe [TranscriptionJobSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListTranscriptionJobsResponse
newListTranscriptionJobsResponse pHttpStatus_ =
  ListTranscriptionJobsResponse'
    { status =
        Core.Nothing,
      nextToken = Core.Nothing,
      transcriptionJobSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested status of the jobs returned.
listTranscriptionJobsResponse_status :: Lens.Lens' ListTranscriptionJobsResponse (Core.Maybe TranscriptionJobStatus)
listTranscriptionJobsResponse_status = Lens.lens (\ListTranscriptionJobsResponse' {status} -> status) (\s@ListTranscriptionJobsResponse' {} a -> s {status = a} :: ListTranscriptionJobsResponse)

-- | The @ListTranscriptionJobs@ operation returns a page of jobs at a time.
-- The maximum size of the page is set by the @MaxResults@ parameter. If
-- there are more jobs in the list than the page size, Amazon Transcribe
-- returns the @NextPage@ token. Include the token in the next request to
-- the @ListTranscriptionJobs@ operation to return in the next page of
-- jobs.
listTranscriptionJobsResponse_nextToken :: Lens.Lens' ListTranscriptionJobsResponse (Core.Maybe Core.Text)
listTranscriptionJobsResponse_nextToken = Lens.lens (\ListTranscriptionJobsResponse' {nextToken} -> nextToken) (\s@ListTranscriptionJobsResponse' {} a -> s {nextToken = a} :: ListTranscriptionJobsResponse)

-- | A list of objects containing summary information for a transcription
-- job.
listTranscriptionJobsResponse_transcriptionJobSummaries :: Lens.Lens' ListTranscriptionJobsResponse (Core.Maybe [TranscriptionJobSummary])
listTranscriptionJobsResponse_transcriptionJobSummaries = Lens.lens (\ListTranscriptionJobsResponse' {transcriptionJobSummaries} -> transcriptionJobSummaries) (\s@ListTranscriptionJobsResponse' {} a -> s {transcriptionJobSummaries = a} :: ListTranscriptionJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTranscriptionJobsResponse_httpStatus :: Lens.Lens' ListTranscriptionJobsResponse Core.Int
listTranscriptionJobsResponse_httpStatus = Lens.lens (\ListTranscriptionJobsResponse' {httpStatus} -> httpStatus) (\s@ListTranscriptionJobsResponse' {} a -> s {httpStatus = a} :: ListTranscriptionJobsResponse)

instance Core.NFData ListTranscriptionJobsResponse

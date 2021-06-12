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
-- Module      : Network.AWS.Transcribe.ListMedicalTranscriptionJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists medical transcription jobs with a specified status or substring
-- that matches their names.
module Network.AWS.Transcribe.ListMedicalTranscriptionJobs
  ( -- * Creating a Request
    ListMedicalTranscriptionJobs (..),
    newListMedicalTranscriptionJobs,

    -- * Request Lenses
    listMedicalTranscriptionJobs_status,
    listMedicalTranscriptionJobs_nextToken,
    listMedicalTranscriptionJobs_maxResults,
    listMedicalTranscriptionJobs_jobNameContains,

    -- * Destructuring the Response
    ListMedicalTranscriptionJobsResponse (..),
    newListMedicalTranscriptionJobsResponse,

    -- * Response Lenses
    listMedicalTranscriptionJobsResponse_status,
    listMedicalTranscriptionJobsResponse_nextToken,
    listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries,
    listMedicalTranscriptionJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newListMedicalTranscriptionJobs' smart constructor.
data ListMedicalTranscriptionJobs = ListMedicalTranscriptionJobs'
  { -- | When specified, returns only medical transcription jobs with the
    -- specified status. Jobs are ordered by creation date, with the newest
    -- jobs returned first. If you don\'t specify a status, Amazon Transcribe
    -- Medical returns all transcription jobs ordered by creation date.
    status :: Core.Maybe TranscriptionJobStatus,
    -- | If you a receive a truncated result in the previous request of
    -- @ListMedicalTranscriptionJobs@, include @NextToken@ to fetch the next
    -- set of jobs.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of medical transcription jobs to return in the
    -- response. IF there are fewer results in the list, this response contains
    -- only the actual results.
    maxResults :: Core.Maybe Core.Natural,
    -- | When specified, the jobs returned in the list are limited to jobs whose
    -- name contains the specified string.
    jobNameContains :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMedicalTranscriptionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listMedicalTranscriptionJobs_status' - When specified, returns only medical transcription jobs with the
-- specified status. Jobs are ordered by creation date, with the newest
-- jobs returned first. If you don\'t specify a status, Amazon Transcribe
-- Medical returns all transcription jobs ordered by creation date.
--
-- 'nextToken', 'listMedicalTranscriptionJobs_nextToken' - If you a receive a truncated result in the previous request of
-- @ListMedicalTranscriptionJobs@, include @NextToken@ to fetch the next
-- set of jobs.
--
-- 'maxResults', 'listMedicalTranscriptionJobs_maxResults' - The maximum number of medical transcription jobs to return in the
-- response. IF there are fewer results in the list, this response contains
-- only the actual results.
--
-- 'jobNameContains', 'listMedicalTranscriptionJobs_jobNameContains' - When specified, the jobs returned in the list are limited to jobs whose
-- name contains the specified string.
newListMedicalTranscriptionJobs ::
  ListMedicalTranscriptionJobs
newListMedicalTranscriptionJobs =
  ListMedicalTranscriptionJobs'
    { status =
        Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      jobNameContains = Core.Nothing
    }

-- | When specified, returns only medical transcription jobs with the
-- specified status. Jobs are ordered by creation date, with the newest
-- jobs returned first. If you don\'t specify a status, Amazon Transcribe
-- Medical returns all transcription jobs ordered by creation date.
listMedicalTranscriptionJobs_status :: Lens.Lens' ListMedicalTranscriptionJobs (Core.Maybe TranscriptionJobStatus)
listMedicalTranscriptionJobs_status = Lens.lens (\ListMedicalTranscriptionJobs' {status} -> status) (\s@ListMedicalTranscriptionJobs' {} a -> s {status = a} :: ListMedicalTranscriptionJobs)

-- | If you a receive a truncated result in the previous request of
-- @ListMedicalTranscriptionJobs@, include @NextToken@ to fetch the next
-- set of jobs.
listMedicalTranscriptionJobs_nextToken :: Lens.Lens' ListMedicalTranscriptionJobs (Core.Maybe Core.Text)
listMedicalTranscriptionJobs_nextToken = Lens.lens (\ListMedicalTranscriptionJobs' {nextToken} -> nextToken) (\s@ListMedicalTranscriptionJobs' {} a -> s {nextToken = a} :: ListMedicalTranscriptionJobs)

-- | The maximum number of medical transcription jobs to return in the
-- response. IF there are fewer results in the list, this response contains
-- only the actual results.
listMedicalTranscriptionJobs_maxResults :: Lens.Lens' ListMedicalTranscriptionJobs (Core.Maybe Core.Natural)
listMedicalTranscriptionJobs_maxResults = Lens.lens (\ListMedicalTranscriptionJobs' {maxResults} -> maxResults) (\s@ListMedicalTranscriptionJobs' {} a -> s {maxResults = a} :: ListMedicalTranscriptionJobs)

-- | When specified, the jobs returned in the list are limited to jobs whose
-- name contains the specified string.
listMedicalTranscriptionJobs_jobNameContains :: Lens.Lens' ListMedicalTranscriptionJobs (Core.Maybe Core.Text)
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
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "MedicalTranscriptionJobSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListMedicalTranscriptionJobs

instance Core.NFData ListMedicalTranscriptionJobs

instance Core.ToHeaders ListMedicalTranscriptionJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListMedicalTranscriptionJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListMedicalTranscriptionJobs where
  toJSON ListMedicalTranscriptionJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("JobNameContains" Core..=)
              Core.<$> jobNameContains
          ]
      )

instance Core.ToPath ListMedicalTranscriptionJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListMedicalTranscriptionJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListMedicalTranscriptionJobsResponse' smart constructor.
data ListMedicalTranscriptionJobsResponse = ListMedicalTranscriptionJobsResponse'
  { -- | The requested status of the medical transcription jobs returned.
    status :: Core.Maybe TranscriptionJobStatus,
    -- | The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a
    -- time. The maximum size of the page is set by the @MaxResults@ parameter.
    -- If the number of jobs exceeds what can fit on a page, Amazon Transcribe
    -- Medical returns the @NextPage@ token. Include the token in the next
    -- request to the @ListMedicalTranscriptionJobs@ operation to return in the
    -- next page of jobs.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of objects containing summary information for a transcription
    -- job.
    medicalTranscriptionJobSummaries :: Core.Maybe [MedicalTranscriptionJobSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMedicalTranscriptionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listMedicalTranscriptionJobsResponse_status' - The requested status of the medical transcription jobs returned.
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
-- 'httpStatus', 'listMedicalTranscriptionJobsResponse_httpStatus' - The response's http status code.
newListMedicalTranscriptionJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListMedicalTranscriptionJobsResponse
newListMedicalTranscriptionJobsResponse pHttpStatus_ =
  ListMedicalTranscriptionJobsResponse'
    { status =
        Core.Nothing,
      nextToken = Core.Nothing,
      medicalTranscriptionJobSummaries =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested status of the medical transcription jobs returned.
listMedicalTranscriptionJobsResponse_status :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Core.Maybe TranscriptionJobStatus)
listMedicalTranscriptionJobsResponse_status = Lens.lens (\ListMedicalTranscriptionJobsResponse' {status} -> status) (\s@ListMedicalTranscriptionJobsResponse' {} a -> s {status = a} :: ListMedicalTranscriptionJobsResponse)

-- | The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a
-- time. The maximum size of the page is set by the @MaxResults@ parameter.
-- If the number of jobs exceeds what can fit on a page, Amazon Transcribe
-- Medical returns the @NextPage@ token. Include the token in the next
-- request to the @ListMedicalTranscriptionJobs@ operation to return in the
-- next page of jobs.
listMedicalTranscriptionJobsResponse_nextToken :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Core.Maybe Core.Text)
listMedicalTranscriptionJobsResponse_nextToken = Lens.lens (\ListMedicalTranscriptionJobsResponse' {nextToken} -> nextToken) (\s@ListMedicalTranscriptionJobsResponse' {} a -> s {nextToken = a} :: ListMedicalTranscriptionJobsResponse)

-- | A list of objects containing summary information for a transcription
-- job.
listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Core.Maybe [MedicalTranscriptionJobSummary])
listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries = Lens.lens (\ListMedicalTranscriptionJobsResponse' {medicalTranscriptionJobSummaries} -> medicalTranscriptionJobSummaries) (\s@ListMedicalTranscriptionJobsResponse' {} a -> s {medicalTranscriptionJobSummaries = a} :: ListMedicalTranscriptionJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listMedicalTranscriptionJobsResponse_httpStatus :: Lens.Lens' ListMedicalTranscriptionJobsResponse Core.Int
listMedicalTranscriptionJobsResponse_httpStatus = Lens.lens (\ListMedicalTranscriptionJobsResponse' {httpStatus} -> httpStatus) (\s@ListMedicalTranscriptionJobsResponse' {} a -> s {httpStatus = a} :: ListMedicalTranscriptionJobsResponse)

instance
  Core.NFData
    ListMedicalTranscriptionJobsResponse

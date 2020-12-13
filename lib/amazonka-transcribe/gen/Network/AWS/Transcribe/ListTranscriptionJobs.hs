{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListTranscriptionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists transcription jobs with the specified status.
module Network.AWS.Transcribe.ListTranscriptionJobs
  ( -- * Creating a request
    ListTranscriptionJobs (..),
    mkListTranscriptionJobs,

    -- ** Request lenses
    ltjStatus,
    ltjNextToken,
    ltjJobNameContains,
    ltjMaxResults,

    -- * Destructuring the response
    ListTranscriptionJobsResponse (..),
    mkListTranscriptionJobsResponse,

    -- ** Response lenses
    ltjrsStatus,
    ltjrsNextToken,
    ltjrsTranscriptionJobSummaries,
    ltjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkListTranscriptionJobs' smart constructor.
data ListTranscriptionJobs = ListTranscriptionJobs'
  { -- | When specified, returns only transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don’t specify a status, Amazon Transcribe returns all transcription jobs ordered by creation date.
    status :: Lude.Maybe TranscriptionJobStatus,
    -- | If the result of the previous request to @ListTranscriptionJobs@ was truncated, include the @NextToken@ to fetch the next set of jobs.
    nextToken :: Lude.Maybe Lude.Text,
    -- | When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
    jobNameContains :: Lude.Maybe Lude.Text,
    -- | The maximum number of jobs to return in the response. If there are fewer results in the list, this response contains only the actual results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTranscriptionJobs' with the minimum fields required to make a request.
--
-- * 'status' - When specified, returns only transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don’t specify a status, Amazon Transcribe returns all transcription jobs ordered by creation date.
-- * 'nextToken' - If the result of the previous request to @ListTranscriptionJobs@ was truncated, include the @NextToken@ to fetch the next set of jobs.
-- * 'jobNameContains' - When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
-- * 'maxResults' - The maximum number of jobs to return in the response. If there are fewer results in the list, this response contains only the actual results.
mkListTranscriptionJobs ::
  ListTranscriptionJobs
mkListTranscriptionJobs =
  ListTranscriptionJobs'
    { status = Lude.Nothing,
      nextToken = Lude.Nothing,
      jobNameContains = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | When specified, returns only transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don’t specify a status, Amazon Transcribe returns all transcription jobs ordered by creation date.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjStatus :: Lens.Lens' ListTranscriptionJobs (Lude.Maybe TranscriptionJobStatus)
ltjStatus = Lens.lens (status :: ListTranscriptionJobs -> Lude.Maybe TranscriptionJobStatus) (\s a -> s {status = a} :: ListTranscriptionJobs)
{-# DEPRECATED ltjStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | If the result of the previous request to @ListTranscriptionJobs@ was truncated, include the @NextToken@ to fetch the next set of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjNextToken :: Lens.Lens' ListTranscriptionJobs (Lude.Maybe Lude.Text)
ltjNextToken = Lens.lens (nextToken :: ListTranscriptionJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTranscriptionJobs)
{-# DEPRECATED ltjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'jobNameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjJobNameContains :: Lens.Lens' ListTranscriptionJobs (Lude.Maybe Lude.Text)
ltjJobNameContains = Lens.lens (jobNameContains :: ListTranscriptionJobs -> Lude.Maybe Lude.Text) (\s a -> s {jobNameContains = a} :: ListTranscriptionJobs)
{-# DEPRECATED ltjJobNameContains "Use generic-lens or generic-optics with 'jobNameContains' instead." #-}

-- | The maximum number of jobs to return in the response. If there are fewer results in the list, this response contains only the actual results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjMaxResults :: Lens.Lens' ListTranscriptionJobs (Lude.Maybe Lude.Natural)
ltjMaxResults = Lens.lens (maxResults :: ListTranscriptionJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTranscriptionJobs)
{-# DEPRECATED ltjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListTranscriptionJobs where
  type Rs ListTranscriptionJobs = ListTranscriptionJobsResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTranscriptionJobsResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "TranscriptionJobSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTranscriptionJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.ListTranscriptionJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTranscriptionJobs where
  toJSON ListTranscriptionJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("JobNameContains" Lude..=) Lude.<$> jobNameContains,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTranscriptionJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTranscriptionJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTranscriptionJobsResponse' smart constructor.
data ListTranscriptionJobsResponse = ListTranscriptionJobsResponse'
  { -- | The requested status of the jobs returned.
    status :: Lude.Maybe TranscriptionJobStatus,
    -- | The @ListTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListTranscriptionJobs@ operation to return in the next page of jobs.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of objects containing summary information for a transcription job.
    transcriptionJobSummaries :: Lude.Maybe [TranscriptionJobSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTranscriptionJobsResponse' with the minimum fields required to make a request.
--
-- * 'status' - The requested status of the jobs returned.
-- * 'nextToken' - The @ListTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListTranscriptionJobs@ operation to return in the next page of jobs.
-- * 'transcriptionJobSummaries' - A list of objects containing summary information for a transcription job.
-- * 'responseStatus' - The response status code.
mkListTranscriptionJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTranscriptionJobsResponse
mkListTranscriptionJobsResponse pResponseStatus_ =
  ListTranscriptionJobsResponse'
    { status = Lude.Nothing,
      nextToken = Lude.Nothing,
      transcriptionJobSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested status of the jobs returned.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrsStatus :: Lens.Lens' ListTranscriptionJobsResponse (Lude.Maybe TranscriptionJobStatus)
ltjrsStatus = Lens.lens (status :: ListTranscriptionJobsResponse -> Lude.Maybe TranscriptionJobStatus) (\s a -> s {status = a} :: ListTranscriptionJobsResponse)
{-# DEPRECATED ltjrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The @ListTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListTranscriptionJobs@ operation to return in the next page of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrsNextToken :: Lens.Lens' ListTranscriptionJobsResponse (Lude.Maybe Lude.Text)
ltjrsNextToken = Lens.lens (nextToken :: ListTranscriptionJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTranscriptionJobsResponse)
{-# DEPRECATED ltjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of objects containing summary information for a transcription job.
--
-- /Note:/ Consider using 'transcriptionJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrsTranscriptionJobSummaries :: Lens.Lens' ListTranscriptionJobsResponse (Lude.Maybe [TranscriptionJobSummary])
ltjrsTranscriptionJobSummaries = Lens.lens (transcriptionJobSummaries :: ListTranscriptionJobsResponse -> Lude.Maybe [TranscriptionJobSummary]) (\s a -> s {transcriptionJobSummaries = a} :: ListTranscriptionJobsResponse)
{-# DEPRECATED ltjrsTranscriptionJobSummaries "Use generic-lens or generic-optics with 'transcriptionJobSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrsResponseStatus :: Lens.Lens' ListTranscriptionJobsResponse Lude.Int
ltjrsResponseStatus = Lens.lens (responseStatus :: ListTranscriptionJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTranscriptionJobsResponse)
{-# DEPRECATED ltjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

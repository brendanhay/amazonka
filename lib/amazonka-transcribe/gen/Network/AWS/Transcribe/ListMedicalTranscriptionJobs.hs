{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListMedicalTranscriptionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists medical transcription jobs with a specified status or substring that matches their names.
module Network.AWS.Transcribe.ListMedicalTranscriptionJobs
  ( -- * Creating a request
    ListMedicalTranscriptionJobs (..),
    mkListMedicalTranscriptionJobs,

    -- ** Request lenses
    lmtjStatus,
    lmtjNextToken,
    lmtjJobNameContains,
    lmtjMaxResults,

    -- * Destructuring the response
    ListMedicalTranscriptionJobsResponse (..),
    mkListMedicalTranscriptionJobsResponse,

    -- ** Response lenses
    lmtjrsStatus,
    lmtjrsNextToken,
    lmtjrsMedicalTranscriptionJobSummaries,
    lmtjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkListMedicalTranscriptionJobs' smart constructor.
data ListMedicalTranscriptionJobs = ListMedicalTranscriptionJobs'
  { status ::
      Lude.Maybe TranscriptionJobStatus,
    nextToken :: Lude.Maybe Lude.Text,
    jobNameContains ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMedicalTranscriptionJobs' with the minimum fields required to make a request.
--
-- * 'jobNameContains' - When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
-- * 'maxResults' - The maximum number of medical transcription jobs to return in the response. IF there are fewer results in the list, this response contains only the actual results.
-- * 'nextToken' - If you a receive a truncated result in the previous request of @ListMedicalTranscriptionJobs@ , include @NextToken@ to fetch the next set of jobs.
-- * 'status' - When specified, returns only medical transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don't specify a status, Amazon Transcribe Medical returns all transcription jobs ordered by creation date.
mkListMedicalTranscriptionJobs ::
  ListMedicalTranscriptionJobs
mkListMedicalTranscriptionJobs =
  ListMedicalTranscriptionJobs'
    { status = Lude.Nothing,
      nextToken = Lude.Nothing,
      jobNameContains = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | When specified, returns only medical transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don't specify a status, Amazon Transcribe Medical returns all transcription jobs ordered by creation date.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjStatus :: Lens.Lens' ListMedicalTranscriptionJobs (Lude.Maybe TranscriptionJobStatus)
lmtjStatus = Lens.lens (status :: ListMedicalTranscriptionJobs -> Lude.Maybe TranscriptionJobStatus) (\s a -> s {status = a} :: ListMedicalTranscriptionJobs)
{-# DEPRECATED lmtjStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | If you a receive a truncated result in the previous request of @ListMedicalTranscriptionJobs@ , include @NextToken@ to fetch the next set of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjNextToken :: Lens.Lens' ListMedicalTranscriptionJobs (Lude.Maybe Lude.Text)
lmtjNextToken = Lens.lens (nextToken :: ListMedicalTranscriptionJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMedicalTranscriptionJobs)
{-# DEPRECATED lmtjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'jobNameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjJobNameContains :: Lens.Lens' ListMedicalTranscriptionJobs (Lude.Maybe Lude.Text)
lmtjJobNameContains = Lens.lens (jobNameContains :: ListMedicalTranscriptionJobs -> Lude.Maybe Lude.Text) (\s a -> s {jobNameContains = a} :: ListMedicalTranscriptionJobs)
{-# DEPRECATED lmtjJobNameContains "Use generic-lens or generic-optics with 'jobNameContains' instead." #-}

-- | The maximum number of medical transcription jobs to return in the response. IF there are fewer results in the list, this response contains only the actual results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjMaxResults :: Lens.Lens' ListMedicalTranscriptionJobs (Lude.Maybe Lude.Natural)
lmtjMaxResults = Lens.lens (maxResults :: ListMedicalTranscriptionJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMedicalTranscriptionJobs)
{-# DEPRECATED lmtjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListMedicalTranscriptionJobs where
  type
    Rs ListMedicalTranscriptionJobs =
      ListMedicalTranscriptionJobsResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMedicalTranscriptionJobsResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> ( x Lude..?> "MedicalTranscriptionJobSummaries"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMedicalTranscriptionJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.ListMedicalTranscriptionJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListMedicalTranscriptionJobs where
  toJSON ListMedicalTranscriptionJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("JobNameContains" Lude..=) Lude.<$> jobNameContains,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListMedicalTranscriptionJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMedicalTranscriptionJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListMedicalTranscriptionJobsResponse' smart constructor.
data ListMedicalTranscriptionJobsResponse = ListMedicalTranscriptionJobsResponse'
  { status ::
      Lude.Maybe
        TranscriptionJobStatus,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    medicalTranscriptionJobSummaries ::
      Lude.Maybe
        [MedicalTranscriptionJobSummary],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMedicalTranscriptionJobsResponse' with the minimum fields required to make a request.
--
-- * 'medicalTranscriptionJobSummaries' - A list of objects containing summary information for a transcription job.
-- * 'nextToken' - The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If the number of jobs exceeds what can fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. Include the token in the next request to the @ListMedicalTranscriptionJobs@ operation to return in the next page of jobs.
-- * 'responseStatus' - The response status code.
-- * 'status' - The requested status of the medical transcription jobs returned.
mkListMedicalTranscriptionJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMedicalTranscriptionJobsResponse
mkListMedicalTranscriptionJobsResponse pResponseStatus_ =
  ListMedicalTranscriptionJobsResponse'
    { status = Lude.Nothing,
      nextToken = Lude.Nothing,
      medicalTranscriptionJobSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested status of the medical transcription jobs returned.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjrsStatus :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Lude.Maybe TranscriptionJobStatus)
lmtjrsStatus = Lens.lens (status :: ListMedicalTranscriptionJobsResponse -> Lude.Maybe TranscriptionJobStatus) (\s a -> s {status = a} :: ListMedicalTranscriptionJobsResponse)
{-# DEPRECATED lmtjrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If the number of jobs exceeds what can fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. Include the token in the next request to the @ListMedicalTranscriptionJobs@ operation to return in the next page of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjrsNextToken :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Lude.Maybe Lude.Text)
lmtjrsNextToken = Lens.lens (nextToken :: ListMedicalTranscriptionJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMedicalTranscriptionJobsResponse)
{-# DEPRECATED lmtjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of objects containing summary information for a transcription job.
--
-- /Note:/ Consider using 'medicalTranscriptionJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjrsMedicalTranscriptionJobSummaries :: Lens.Lens' ListMedicalTranscriptionJobsResponse (Lude.Maybe [MedicalTranscriptionJobSummary])
lmtjrsMedicalTranscriptionJobSummaries = Lens.lens (medicalTranscriptionJobSummaries :: ListMedicalTranscriptionJobsResponse -> Lude.Maybe [MedicalTranscriptionJobSummary]) (\s a -> s {medicalTranscriptionJobSummaries = a} :: ListMedicalTranscriptionJobsResponse)
{-# DEPRECATED lmtjrsMedicalTranscriptionJobSummaries "Use generic-lens or generic-optics with 'medicalTranscriptionJobSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtjrsResponseStatus :: Lens.Lens' ListMedicalTranscriptionJobsResponse Lude.Int
lmtjrsResponseStatus = Lens.lens (responseStatus :: ListMedicalTranscriptionJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMedicalTranscriptionJobsResponse)
{-# DEPRECATED lmtjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

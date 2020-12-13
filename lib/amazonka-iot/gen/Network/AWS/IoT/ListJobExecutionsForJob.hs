{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListJobExecutionsForJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for a job.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListJobExecutionsForJob
  ( -- * Creating a request
    ListJobExecutionsForJob (..),
    mkListJobExecutionsForJob,

    -- ** Request lenses
    ljefjStatus,
    ljefjJobId,
    ljefjNextToken,
    ljefjMaxResults,

    -- * Destructuring the response
    ListJobExecutionsForJobResponse (..),
    mkListJobExecutionsForJobResponse,

    -- ** Response lenses
    ljefjrsExecutionSummaries,
    ljefjrsNextToken,
    ljefjrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListJobExecutionsForJob' smart constructor.
data ListJobExecutionsForJob = ListJobExecutionsForJob'
  { -- | The status of the job.
    status :: Lude.Maybe JobExecutionStatus,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Lude.Text,
    -- | The token to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobExecutionsForJob' with the minimum fields required to make a request.
--
-- * 'status' - The status of the job.
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
-- * 'nextToken' - The token to retrieve the next set of results.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListJobExecutionsForJob ::
  -- | 'jobId'
  Lude.Text ->
  ListJobExecutionsForJob
mkListJobExecutionsForJob pJobId_ =
  ListJobExecutionsForJob'
    { status = Lude.Nothing,
      jobId = pJobId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The status of the job.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjStatus :: Lens.Lens' ListJobExecutionsForJob (Lude.Maybe JobExecutionStatus)
ljefjStatus = Lens.lens (status :: ListJobExecutionsForJob -> Lude.Maybe JobExecutionStatus) (\s a -> s {status = a} :: ListJobExecutionsForJob)
{-# DEPRECATED ljefjStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjJobId :: Lens.Lens' ListJobExecutionsForJob Lude.Text
ljefjJobId = Lens.lens (jobId :: ListJobExecutionsForJob -> Lude.Text) (\s a -> s {jobId = a} :: ListJobExecutionsForJob)
{-# DEPRECATED ljefjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjNextToken :: Lens.Lens' ListJobExecutionsForJob (Lude.Maybe Lude.Text)
ljefjNextToken = Lens.lens (nextToken :: ListJobExecutionsForJob -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobExecutionsForJob)
{-# DEPRECATED ljefjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjMaxResults :: Lens.Lens' ListJobExecutionsForJob (Lude.Maybe Lude.Natural)
ljefjMaxResults = Lens.lens (maxResults :: ListJobExecutionsForJob -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListJobExecutionsForJob)
{-# DEPRECATED ljefjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListJobExecutionsForJob where
  page rq rs
    | Page.stop (rs Lens.^. ljefjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ljefjrsExecutionSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ljefjNextToken Lens..~ rs Lens.^. ljefjrsNextToken

instance Lude.AWSRequest ListJobExecutionsForJob where
  type Rs ListJobExecutionsForJob = ListJobExecutionsForJobResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobExecutionsForJobResponse'
            Lude.<$> (x Lude..?> "executionSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobExecutionsForJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListJobExecutionsForJob where
  toPath ListJobExecutionsForJob' {..} =
    Lude.mconcat ["/jobs/", Lude.toBS jobId, "/things"]

instance Lude.ToQuery ListJobExecutionsForJob where
  toQuery ListJobExecutionsForJob' {..} =
    Lude.mconcat
      [ "status" Lude.=: status,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListJobExecutionsForJobResponse' smart constructor.
data ListJobExecutionsForJobResponse = ListJobExecutionsForJobResponse'
  { -- | A list of job execution summaries.
    executionSummaries :: Lude.Maybe [JobExecutionSummaryForJob],
    -- | The token for the next set of results, or __null__ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobExecutionsForJobResponse' with the minimum fields required to make a request.
--
-- * 'executionSummaries' - A list of job execution summaries.
-- * 'nextToken' - The token for the next set of results, or __null__ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListJobExecutionsForJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobExecutionsForJobResponse
mkListJobExecutionsForJobResponse pResponseStatus_ =
  ListJobExecutionsForJobResponse'
    { executionSummaries =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of job execution summaries.
--
-- /Note:/ Consider using 'executionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjrsExecutionSummaries :: Lens.Lens' ListJobExecutionsForJobResponse (Lude.Maybe [JobExecutionSummaryForJob])
ljefjrsExecutionSummaries = Lens.lens (executionSummaries :: ListJobExecutionsForJobResponse -> Lude.Maybe [JobExecutionSummaryForJob]) (\s a -> s {executionSummaries = a} :: ListJobExecutionsForJobResponse)
{-# DEPRECATED ljefjrsExecutionSummaries "Use generic-lens or generic-optics with 'executionSummaries' instead." #-}

-- | The token for the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjrsNextToken :: Lens.Lens' ListJobExecutionsForJobResponse (Lude.Maybe Lude.Text)
ljefjrsNextToken = Lens.lens (nextToken :: ListJobExecutionsForJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobExecutionsForJobResponse)
{-# DEPRECATED ljefjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjrsResponseStatus :: Lens.Lens' ListJobExecutionsForJobResponse Lude.Int
ljefjrsResponseStatus = Lens.lens (responseStatus :: ListJobExecutionsForJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobExecutionsForJobResponse)
{-# DEPRECATED ljefjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

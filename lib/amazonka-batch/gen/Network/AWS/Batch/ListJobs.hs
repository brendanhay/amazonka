{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of AWS Batch jobs.
--
-- You must specify only one of the following:
--
--     * a job queue ID to return a list of jobs in that job queue
--
--
--     * a multi-node parallel job ID to return a list of that job's nodes
--
--
--     * an array job ID to return a list of that job's children
--
--
-- You can filter the results by job status with the @jobStatus@ parameter. If you do not specify a status, only @RUNNING@ jobs are returned.
--
-- This operation returns paginated results.
module Network.AWS.Batch.ListJobs
  ( -- * Creating a request
    ListJobs (..),
    mkListJobs,

    -- ** Request lenses
    ljNextToken,
    ljMultiNodeJobId,
    ljJobStatus,
    ljArrayJobId,
    ljJobQueue,
    ljMaxResults,

    -- * Destructuring the response
    ListJobsResponse (..),
    mkListJobsResponse,

    -- ** Response lenses
    ljrsJobSummaryList,
    ljrsNextToken,
    ljrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | The @nextToken@ value returned from a previous paginated @ListJobs@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The job ID for a multi-node parallel job. Specifying a multi-node parallel job ID with this parameter lists all nodes that are associated with the specified job.
    multiNodeJobId :: Lude.Maybe Lude.Text,
    -- | The job status with which to filter jobs in the specified queue. If you do not specify a status, only @RUNNING@ jobs are returned.
    jobStatus :: Lude.Maybe JobStatus,
    -- | The job ID for an array job. Specifying an array job ID with this parameter lists all child jobs from within the specified array.
    arrayJobId :: Lude.Maybe Lude.Text,
    -- | The name or full Amazon Resource Name (ARN) of the job queue with which to list jobs.
    jobQueue :: Lude.Maybe Lude.Text,
    -- | The maximum number of results returned by @ListJobs@ in paginated output. When this parameter is used, @ListJobs@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListJobs@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListJobs@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated @ListJobs@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
-- * 'multiNodeJobId' - The job ID for a multi-node parallel job. Specifying a multi-node parallel job ID with this parameter lists all nodes that are associated with the specified job.
-- * 'jobStatus' - The job status with which to filter jobs in the specified queue. If you do not specify a status, only @RUNNING@ jobs are returned.
-- * 'arrayJobId' - The job ID for an array job. Specifying an array job ID with this parameter lists all child jobs from within the specified array.
-- * 'jobQueue' - The name or full Amazon Resource Name (ARN) of the job queue with which to list jobs.
-- * 'maxResults' - The maximum number of results returned by @ListJobs@ in paginated output. When this parameter is used, @ListJobs@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListJobs@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListJobs@ returns up to 100 results and a @nextToken@ value if applicable.
mkListJobs ::
  ListJobs
mkListJobs =
  ListJobs'
    { nextToken = Lude.Nothing,
      multiNodeJobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      arrayJobId = Lude.Nothing,
      jobQueue = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated @ListJobs@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNextToken :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljNextToken = Lens.lens (nextToken :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobs)
{-# DEPRECATED ljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The job ID for a multi-node parallel job. Specifying a multi-node parallel job ID with this parameter lists all nodes that are associated with the specified job.
--
-- /Note:/ Consider using 'multiNodeJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMultiNodeJobId :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljMultiNodeJobId = Lens.lens (multiNodeJobId :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {multiNodeJobId = a} :: ListJobs)
{-# DEPRECATED ljMultiNodeJobId "Use generic-lens or generic-optics with 'multiNodeJobId' instead." #-}

-- | The job status with which to filter jobs in the specified queue. If you do not specify a status, only @RUNNING@ jobs are returned.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljJobStatus :: Lens.Lens' ListJobs (Lude.Maybe JobStatus)
ljJobStatus = Lens.lens (jobStatus :: ListJobs -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: ListJobs)
{-# DEPRECATED ljJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The job ID for an array job. Specifying an array job ID with this parameter lists all child jobs from within the specified array.
--
-- /Note:/ Consider using 'arrayJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljArrayJobId :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljArrayJobId = Lens.lens (arrayJobId :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {arrayJobId = a} :: ListJobs)
{-# DEPRECATED ljArrayJobId "Use generic-lens or generic-optics with 'arrayJobId' instead." #-}

-- | The name or full Amazon Resource Name (ARN) of the job queue with which to list jobs.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljJobQueue :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljJobQueue = Lens.lens (jobQueue :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {jobQueue = a} :: ListJobs)
{-# DEPRECATED ljJobQueue "Use generic-lens or generic-optics with 'jobQueue' instead." #-}

-- | The maximum number of results returned by @ListJobs@ in paginated output. When this parameter is used, @ListJobs@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListJobs@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListJobs@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxResults :: Lens.Lens' ListJobs (Lude.Maybe Lude.Int)
ljMaxResults = Lens.lens (maxResults :: ListJobs -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListJobs)
{-# DEPRECATED ljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListJobs where
  page rq rs
    | Page.stop (rs Lens.^. ljrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ljrsJobSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ljNextToken Lens..~ rs Lens.^. ljrsNextToken

instance Lude.AWSRequest ListJobs where
  type Rs ListJobs = ListJobsResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Lude.<$> (x Lude..?> "jobSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListJobs where
  toJSON ListJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("multiNodeJobId" Lude..=) Lude.<$> multiNodeJobId,
            ("jobStatus" Lude..=) Lude.<$> jobStatus,
            ("arrayJobId" Lude..=) Lude.<$> arrayJobId,
            ("jobQueue" Lude..=) Lude.<$> jobQueue,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListJobs where
  toPath = Lude.const "/v1/listjobs"

instance Lude.ToQuery ListJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | A list of job summaries that match the request.
    jobSummaryList :: [JobSummary],
    -- | The @nextToken@ value to include in a future @ListJobs@ request. When the results of a @ListJobs@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobSummaryList' - A list of job summaries that match the request.
-- * 'nextToken' - The @nextToken@ value to include in a future @ListJobs@ request. When the results of a @ListJobs@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobsResponse
mkListJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { jobSummaryList = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of job summaries that match the request.
--
-- /Note:/ Consider using 'jobSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsJobSummaryList :: Lens.Lens' ListJobsResponse [JobSummary]
ljrsJobSummaryList = Lens.lens (jobSummaryList :: ListJobsResponse -> [JobSummary]) (\s a -> s {jobSummaryList = a} :: ListJobsResponse)
{-# DEPRECATED ljrsJobSummaryList "Use generic-lens or generic-optics with 'jobSummaryList' instead." #-}

-- | The @nextToken@ value to include in a future @ListJobs@ request. When the results of a @ListJobs@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsNextToken :: Lens.Lens' ListJobsResponse (Lude.Maybe Lude.Text)
ljrsNextToken = Lens.lens (nextToken :: ListJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobsResponse)
{-# DEPRECATED ljrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsResponseStatus :: Lens.Lens' ListJobsResponse Lude.Int
ljrsResponseStatus = Lens.lens (responseStatus :: ListJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobsResponse)
{-# DEPRECATED ljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

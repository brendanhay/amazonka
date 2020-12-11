{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @JobListEntry@ objects of the specified length. Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs. Calling this API action in one of the US regions will return jobs from the list of all jobs associated with this account in all US regions.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListJobs
  ( -- * Creating a request
    ListJobs (..),
    mkListJobs,

    -- ** Request lenses
    ljNextToken,
    ljMaxResults,

    -- * Destructuring the response
    ListJobsResponse (..),
    mkListJobsResponse,

    -- ** Response lenses
    ljrsJobListEntries,
    ljrsNextToken,
    ljrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- * 'maxResults' - The number of @JobListEntry@ objects to return.
-- * 'nextToken' - HTTP requests are stateless. To identify what object comes "next" in the list of @JobListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
mkListJobs ::
  ListJobs
mkListJobs =
  ListJobs' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | HTTP requests are stateless. To identify what object comes "next" in the list of @JobListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNextToken :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljNextToken = Lens.lens (nextToken :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobs)
{-# DEPRECATED ljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of @JobListEntry@ objects to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxResults :: Lens.Lens' ListJobs (Lude.Maybe Lude.Natural)
ljMaxResults = Lens.lens (maxResults :: ListJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListJobs)
{-# DEPRECATED ljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListJobs where
  page rq rs
    | Page.stop (rs Lens.^. ljrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ljrsJobListEntries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ljNextToken Lens..~ rs Lens.^. ljrsNextToken

instance Lude.AWSRequest ListJobs where
  type Rs ListJobs = ListJobsResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Lude.<$> (x Lude..?> "JobListEntries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSIESnowballJobManagementService.ListJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListJobs where
  toJSON ListJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { jobListEntries ::
      Lude.Maybe [JobListEntry],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobListEntries' - Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs.
-- * 'nextToken' - HTTP requests are stateless. If you use this automatically generated @NextToken@ value in your next @ListJobs@ call, your returned @JobListEntry@ objects will start from this point in the array.
-- * 'responseStatus' - The response status code.
mkListJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobsResponse
mkListJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { jobListEntries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs.
--
-- /Note:/ Consider using 'jobListEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsJobListEntries :: Lens.Lens' ListJobsResponse (Lude.Maybe [JobListEntry])
ljrsJobListEntries = Lens.lens (jobListEntries :: ListJobsResponse -> Lude.Maybe [JobListEntry]) (\s a -> s {jobListEntries = a} :: ListJobsResponse)
{-# DEPRECATED ljrsJobListEntries "Use generic-lens or generic-optics with 'jobListEntries' instead." #-}

-- | HTTP requests are stateless. If you use this automatically generated @NextToken@ value in your next @ListJobs@ call, your returned @JobListEntry@ objects will start from this point in the array.
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

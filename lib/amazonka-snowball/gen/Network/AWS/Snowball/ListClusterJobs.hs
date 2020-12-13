{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.ListClusterJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @JobListEntry@ objects of the specified length. Each @JobListEntry@ object is for a job in the specified cluster and contains a job's state, a job's ID, and other information.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListClusterJobs
  ( -- * Creating a request
    ListClusterJobs (..),
    mkListClusterJobs,

    -- ** Request lenses
    lcjNextToken,
    lcjClusterId,
    lcjMaxResults,

    -- * Destructuring the response
    ListClusterJobsResponse (..),
    mkListClusterJobsResponse,

    -- ** Response lenses
    lcjrsJobListEntries,
    lcjrsNextToken,
    lcjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkListClusterJobs' smart constructor.
data ListClusterJobs = ListClusterJobs'
  { -- | HTTP requests are stateless. To identify what object comes "next" in the list of @JobListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
    clusterId :: Lude.Text,
    -- | The number of @JobListEntry@ objects to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListClusterJobs' with the minimum fields required to make a request.
--
-- * 'nextToken' - HTTP requests are stateless. To identify what object comes "next" in the list of @JobListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
-- * 'clusterId' - The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
-- * 'maxResults' - The number of @JobListEntry@ objects to return.
mkListClusterJobs ::
  -- | 'clusterId'
  Lude.Text ->
  ListClusterJobs
mkListClusterJobs pClusterId_ =
  ListClusterJobs'
    { nextToken = Lude.Nothing,
      clusterId = pClusterId_,
      maxResults = Lude.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes "next" in the list of @JobListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjNextToken :: Lens.Lens' ListClusterJobs (Lude.Maybe Lude.Text)
lcjNextToken = Lens.lens (nextToken :: ListClusterJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListClusterJobs)
{-# DEPRECATED lcjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjClusterId :: Lens.Lens' ListClusterJobs Lude.Text
lcjClusterId = Lens.lens (clusterId :: ListClusterJobs -> Lude.Text) (\s a -> s {clusterId = a} :: ListClusterJobs)
{-# DEPRECATED lcjClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The number of @JobListEntry@ objects to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjMaxResults :: Lens.Lens' ListClusterJobs (Lude.Maybe Lude.Natural)
lcjMaxResults = Lens.lens (maxResults :: ListClusterJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListClusterJobs)
{-# DEPRECATED lcjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListClusterJobs where
  page rq rs
    | Page.stop (rs Lens.^. lcjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcjrsJobListEntries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcjNextToken Lens..~ rs Lens.^. lcjrsNextToken

instance Lude.AWSRequest ListClusterJobs where
  type Rs ListClusterJobs = ListClusterJobsResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListClusterJobsResponse'
            Lude.<$> (x Lude..?> "JobListEntries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListClusterJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.ListClusterJobs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListClusterJobs where
  toJSON ListClusterJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ClusterId" Lude..= clusterId),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListClusterJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListClusterJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListClusterJobsResponse' smart constructor.
data ListClusterJobsResponse = ListClusterJobsResponse'
  { -- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs.
    jobListEntries :: Lude.Maybe [JobListEntry],
    -- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ListClusterJobsResult@ call, your list of returned jobs will start from this point in the array.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListClusterJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobListEntries' - Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs.
-- * 'nextToken' - HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ListClusterJobsResult@ call, your list of returned jobs will start from this point in the array.
-- * 'responseStatus' - The response status code.
mkListClusterJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListClusterJobsResponse
mkListClusterJobsResponse pResponseStatus_ =
  ListClusterJobsResponse'
    { jobListEntries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs.
--
-- /Note:/ Consider using 'jobListEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrsJobListEntries :: Lens.Lens' ListClusterJobsResponse (Lude.Maybe [JobListEntry])
lcjrsJobListEntries = Lens.lens (jobListEntries :: ListClusterJobsResponse -> Lude.Maybe [JobListEntry]) (\s a -> s {jobListEntries = a} :: ListClusterJobsResponse)
{-# DEPRECATED lcjrsJobListEntries "Use generic-lens or generic-optics with 'jobListEntries' instead." #-}

-- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ListClusterJobsResult@ call, your list of returned jobs will start from this point in the array.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrsNextToken :: Lens.Lens' ListClusterJobsResponse (Lude.Maybe Lude.Text)
lcjrsNextToken = Lens.lens (nextToken :: ListClusterJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListClusterJobsResponse)
{-# DEPRECATED lcjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrsResponseStatus :: Lens.Lens' ListClusterJobsResponse Lude.Int
lcjrsResponseStatus = Lens.lens (responseStatus :: ListClusterJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListClusterJobsResponse)
{-# DEPRECATED lcjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

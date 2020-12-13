{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your most recently created jobs. This array includes in-process, completed, and errored jobs. This will return the jobs themselves, not just a list of the jobs. To retrieve the twenty next most recent jobs, use the nextToken string returned with the array.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListJobs
  ( -- * Creating a request
    ListJobs (..),
    mkListJobs,

    -- ** Request lenses
    ljStatus,
    ljQueue,
    ljNextToken,
    ljOrder,
    ljMaxResults,

    -- * Destructuring the response
    ListJobsResponse (..),
    mkListJobsResponse,

    -- ** Response lenses
    ljrsNextToken,
    ljrsJobs,
    ljrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | Optional. A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
    status :: Lude.Maybe JobStatus,
    -- | Optional. Provide a queue name to get back only jobs from that queue.
    queue :: Lude.Maybe Lude.Text,
    -- | Optional. Use this string, provided with the response to a previous request, to request the next batch of jobs.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
    order :: Lude.Maybe Order,
    -- | Optional. Number of jobs, up to twenty, that will be returned at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- * 'status' - Optional. A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
-- * 'queue' - Optional. Provide a queue name to get back only jobs from that queue.
-- * 'nextToken' - Optional. Use this string, provided with the response to a previous request, to request the next batch of jobs.
-- * 'order' - Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
-- * 'maxResults' - Optional. Number of jobs, up to twenty, that will be returned at one time.
mkListJobs ::
  ListJobs
mkListJobs =
  ListJobs'
    { status = Lude.Nothing,
      queue = Lude.Nothing,
      nextToken = Lude.Nothing,
      order = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Optional. A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljStatus :: Lens.Lens' ListJobs (Lude.Maybe JobStatus)
ljStatus = Lens.lens (status :: ListJobs -> Lude.Maybe JobStatus) (\s a -> s {status = a} :: ListJobs)
{-# DEPRECATED ljStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Optional. Provide a queue name to get back only jobs from that queue.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljQueue :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljQueue = Lens.lens (queue :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {queue = a} :: ListJobs)
{-# DEPRECATED ljQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | Optional. Use this string, provided with the response to a previous request, to request the next batch of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNextToken :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljNextToken = Lens.lens (nextToken :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobs)
{-# DEPRECATED ljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljOrder :: Lens.Lens' ListJobs (Lude.Maybe Order)
ljOrder = Lens.lens (order :: ListJobs -> Lude.Maybe Order) (\s a -> s {order = a} :: ListJobs)
{-# DEPRECATED ljOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | Optional. Number of jobs, up to twenty, that will be returned at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxResults :: Lens.Lens' ListJobs (Lude.Maybe Lude.Natural)
ljMaxResults = Lens.lens (maxResults :: ListJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListJobs)
{-# DEPRECATED ljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListJobs where
  page rq rs
    | Page.stop (rs Lens.^. ljrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ljrsJobs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ljNextToken Lens..~ rs Lens.^. ljrsNextToken

instance Lude.AWSRequest ListJobs where
  type Rs ListJobs = ListJobsResponse
  request = Req.get mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "jobs" Lude..!@ Lude.mempty)
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

instance Lude.ToPath ListJobs where
  toPath = Lude.const "/2017-08-29/jobs"

instance Lude.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Lude.mconcat
      [ "status" Lude.=: status,
        "queue" Lude.=: queue,
        "nextToken" Lude.=: nextToken,
        "order" Lude.=: order,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | Use this string to request the next batch of jobs.
    nextToken :: Lude.Maybe Lude.Text,
    -- | List of jobs
    jobs :: Lude.Maybe [Job],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Use this string to request the next batch of jobs.
-- * 'jobs' - List of jobs
-- * 'responseStatus' - The response status code.
mkListJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobsResponse
mkListJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { nextToken = Lude.Nothing,
      jobs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Use this string to request the next batch of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsNextToken :: Lens.Lens' ListJobsResponse (Lude.Maybe Lude.Text)
ljrsNextToken = Lens.lens (nextToken :: ListJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobsResponse)
{-# DEPRECATED ljrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of jobs
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsJobs :: Lens.Lens' ListJobsResponse (Lude.Maybe [Job])
ljrsJobs = Lens.lens (jobs :: ListJobsResponse -> Lude.Maybe [Job]) (\s a -> s {jobs = a} :: ListJobsResponse)
{-# DEPRECATED ljrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsResponseStatus :: Lens.Lens' ListJobsResponse Lude.Int
ljrsResponseStatus = Lens.lens (responseStatus :: ListJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobsResponse)
{-# DEPRECATED ljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

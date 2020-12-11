{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListJobsByStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListJobsByStatus operation gets a list of jobs that have a specified status. The response body contains one element for each job that satisfies the search criteria.
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListJobsByStatus
  ( -- * Creating a request
    ListJobsByStatus (..),
    mkListJobsByStatus,

    -- ** Request lenses
    ljbsAscending,
    ljbsPageToken,
    ljbsStatus,

    -- * Destructuring the response
    ListJobsByStatusResponse (..),
    mkListJobsByStatusResponse,

    -- ** Response lenses
    ljbsrsNextPageToken,
    ljbsrsJobs,
    ljbsrsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @ListJobsByStatusRequest@ structure.
--
-- /See:/ 'mkListJobsByStatus' smart constructor.
data ListJobsByStatus = ListJobsByStatus'
  { ascending ::
      Lude.Maybe Lude.Text,
    pageToken :: Lude.Maybe Lude.Text,
    status :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobsByStatus' with the minimum fields required to make a request.
--
-- * 'ascending' - To list jobs in chronological order by the date and time that they were submitted, enter @true@ . To list jobs in reverse chronological order, enter @false@ .
-- * 'pageToken' - When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
-- * 'status' - To get information about all of the jobs associated with the current AWS account that have a given status, specify the following status: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
mkListJobsByStatus ::
  -- | 'status'
  Lude.Text ->
  ListJobsByStatus
mkListJobsByStatus pStatus_ =
  ListJobsByStatus'
    { ascending = Lude.Nothing,
      pageToken = Lude.Nothing,
      status = pStatus_
    }

-- | To list jobs in chronological order by the date and time that they were submitted, enter @true@ . To list jobs in reverse chronological order, enter @false@ .
--
-- /Note:/ Consider using 'ascending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbsAscending :: Lens.Lens' ListJobsByStatus (Lude.Maybe Lude.Text)
ljbsAscending = Lens.lens (ascending :: ListJobsByStatus -> Lude.Maybe Lude.Text) (\s a -> s {ascending = a} :: ListJobsByStatus)
{-# DEPRECATED ljbsAscending "Use generic-lens or generic-optics with 'ascending' instead." #-}

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbsPageToken :: Lens.Lens' ListJobsByStatus (Lude.Maybe Lude.Text)
ljbsPageToken = Lens.lens (pageToken :: ListJobsByStatus -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListJobsByStatus)
{-# DEPRECATED ljbsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | To get information about all of the jobs associated with the current AWS account that have a given status, specify the following status: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbsStatus :: Lens.Lens' ListJobsByStatus Lude.Text
ljbsStatus = Lens.lens (status :: ListJobsByStatus -> Lude.Text) (\s a -> s {status = a} :: ListJobsByStatus)
{-# DEPRECATED ljbsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Page.AWSPager ListJobsByStatus where
  page rq rs
    | Page.stop (rs Lens.^. ljbsrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ljbsrsJobs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ljbsPageToken Lens..~ rs Lens.^. ljbsrsNextPageToken

instance Lude.AWSRequest ListJobsByStatus where
  type Rs ListJobsByStatus = ListJobsByStatusResponse
  request = Req.get elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobsByStatusResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "Jobs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobsByStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListJobsByStatus where
  toPath ListJobsByStatus' {..} =
    Lude.mconcat ["/2012-09-25/jobsByStatus/", Lude.toBS status]

instance Lude.ToQuery ListJobsByStatus where
  toQuery ListJobsByStatus' {..} =
    Lude.mconcat
      ["Ascending" Lude.=: ascending, "PageToken" Lude.=: pageToken]

-- | The @ListJobsByStatusResponse@ structure.
--
-- /See:/ 'mkListJobsByStatusResponse' smart constructor.
data ListJobsByStatusResponse = ListJobsByStatusResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    jobs :: Lude.Maybe [Job'],
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

-- | Creates a value of 'ListJobsByStatusResponse' with the minimum fields required to make a request.
--
-- * 'jobs' - An array of @Job@ objects that have the specified status.
-- * 'nextPageToken' - A value that you use to access the second and subsequent pages of results, if any. When the jobs in the specified pipeline fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
-- * 'responseStatus' - The response status code.
mkListJobsByStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobsByStatusResponse
mkListJobsByStatusResponse pResponseStatus_ =
  ListJobsByStatusResponse'
    { nextPageToken = Lude.Nothing,
      jobs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that you use to access the second and subsequent pages of results, if any. When the jobs in the specified pipeline fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbsrsNextPageToken :: Lens.Lens' ListJobsByStatusResponse (Lude.Maybe Lude.Text)
ljbsrsNextPageToken = Lens.lens (nextPageToken :: ListJobsByStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListJobsByStatusResponse)
{-# DEPRECATED ljbsrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of @Job@ objects that have the specified status.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbsrsJobs :: Lens.Lens' ListJobsByStatusResponse (Lude.Maybe [Job'])
ljbsrsJobs = Lens.lens (jobs :: ListJobsByStatusResponse -> Lude.Maybe [Job']) (\s a -> s {jobs = a} :: ListJobsByStatusResponse)
{-# DEPRECATED ljbsrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljbsrsResponseStatus :: Lens.Lens' ListJobsByStatusResponse Lude.Int
ljbsrsResponseStatus = Lens.lens (responseStatus :: ListJobsByStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobsByStatusResponse)
{-# DEPRECATED ljbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

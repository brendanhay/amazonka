{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListTopicsDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the topic detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListTopicsDetectionJobs
  ( -- * Creating a request
    ListTopicsDetectionJobs (..),
    mkListTopicsDetectionJobs,

    -- ** Request lenses
    ltdjNextToken,
    ltdjFilter,
    ltdjMaxResults,

    -- * Destructuring the response
    ListTopicsDetectionJobsResponse (..),
    mkListTopicsDetectionJobsResponse,

    -- ** Response lenses
    ltdjrsNextToken,
    ltdjrsTopicsDetectionJobPropertiesList,
    ltdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTopicsDetectionJobs' smart constructor.
data ListTopicsDetectionJobs = ListTopicsDetectionJobs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    filter ::
      Lude.Maybe TopicsDetectionJobFilter,
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

-- | Creates a value of 'ListTopicsDetectionJobs' with the minimum fields required to make a request.
--
-- * 'filter' - Filters the jobs that are returned. Jobs can be filtered on their name, status, or the date and time that they were submitted. You can set only one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page. The default is 100.
-- * 'nextToken' - Identifies the next page of results to return.
mkListTopicsDetectionJobs ::
  ListTopicsDetectionJobs
mkListTopicsDetectionJobs =
  ListTopicsDetectionJobs'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdjNextToken :: Lens.Lens' ListTopicsDetectionJobs (Lude.Maybe Lude.Text)
ltdjNextToken = Lens.lens (nextToken :: ListTopicsDetectionJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTopicsDetectionJobs)
{-# DEPRECATED ltdjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the jobs that are returned. Jobs can be filtered on their name, status, or the date and time that they were submitted. You can set only one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdjFilter :: Lens.Lens' ListTopicsDetectionJobs (Lude.Maybe TopicsDetectionJobFilter)
ltdjFilter = Lens.lens (filter :: ListTopicsDetectionJobs -> Lude.Maybe TopicsDetectionJobFilter) (\s a -> s {filter = a} :: ListTopicsDetectionJobs)
{-# DEPRECATED ltdjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdjMaxResults :: Lens.Lens' ListTopicsDetectionJobs (Lude.Maybe Lude.Natural)
ltdjMaxResults = Lens.lens (maxResults :: ListTopicsDetectionJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTopicsDetectionJobs)
{-# DEPRECATED ltdjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTopicsDetectionJobs where
  page rq rs
    | Page.stop (rs Lens.^. ltdjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltdjrsTopicsDetectionJobPropertiesList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltdjNextToken Lens..~ rs Lens.^. ltdjrsNextToken

instance Lude.AWSRequest ListTopicsDetectionJobs where
  type Rs ListTopicsDetectionJobs = ListTopicsDetectionJobsResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTopicsDetectionJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> ( x Lude..?> "TopicsDetectionJobPropertiesList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTopicsDetectionJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.ListTopicsDetectionJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTopicsDetectionJobs where
  toJSON ListTopicsDetectionJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTopicsDetectionJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTopicsDetectionJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTopicsDetectionJobsResponse' smart constructor.
data ListTopicsDetectionJobsResponse = ListTopicsDetectionJobsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    topicsDetectionJobPropertiesList ::
      Lude.Maybe
        [TopicsDetectionJobProperties],
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

-- | Creates a value of 'ListTopicsDetectionJobsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'responseStatus' - The response status code.
-- * 'topicsDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
mkListTopicsDetectionJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTopicsDetectionJobsResponse
mkListTopicsDetectionJobsResponse pResponseStatus_ =
  ListTopicsDetectionJobsResponse'
    { nextToken = Lude.Nothing,
      topicsDetectionJobPropertiesList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdjrsNextToken :: Lens.Lens' ListTopicsDetectionJobsResponse (Lude.Maybe Lude.Text)
ltdjrsNextToken = Lens.lens (nextToken :: ListTopicsDetectionJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTopicsDetectionJobsResponse)
{-# DEPRECATED ltdjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'topicsDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdjrsTopicsDetectionJobPropertiesList :: Lens.Lens' ListTopicsDetectionJobsResponse (Lude.Maybe [TopicsDetectionJobProperties])
ltdjrsTopicsDetectionJobPropertiesList = Lens.lens (topicsDetectionJobPropertiesList :: ListTopicsDetectionJobsResponse -> Lude.Maybe [TopicsDetectionJobProperties]) (\s a -> s {topicsDetectionJobPropertiesList = a} :: ListTopicsDetectionJobsResponse)
{-# DEPRECATED ltdjrsTopicsDetectionJobPropertiesList "Use generic-lens or generic-optics with 'topicsDetectionJobPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdjrsResponseStatus :: Lens.Lens' ListTopicsDetectionJobsResponse Lude.Int
ltdjrsResponseStatus = Lens.lens (responseStatus :: ListTopicsDetectionJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTopicsDetectionJobsResponse)
{-# DEPRECATED ltdjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

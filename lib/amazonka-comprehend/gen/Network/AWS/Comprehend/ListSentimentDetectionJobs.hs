{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListSentimentDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of sentiment detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListSentimentDetectionJobs
  ( -- * Creating a request
    ListSentimentDetectionJobs (..),
    mkListSentimentDetectionJobs,

    -- ** Request lenses
    lsdjNextToken,
    lsdjFilter,
    lsdjMaxResults,

    -- * Destructuring the response
    ListSentimentDetectionJobsResponse (..),
    mkListSentimentDetectionJobsResponse,

    -- ** Response lenses
    lsdjrsNextToken,
    lsdjrsSentimentDetectionJobPropertiesList,
    lsdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSentimentDetectionJobs' smart constructor.
data ListSentimentDetectionJobs = ListSentimentDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
    filter :: Lude.Maybe SentimentDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSentimentDetectionJobs' with the minimum fields required to make a request.
--
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'filter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page. The default is 100.
mkListSentimentDetectionJobs ::
  ListSentimentDetectionJobs
mkListSentimentDetectionJobs =
  ListSentimentDetectionJobs'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjNextToken :: Lens.Lens' ListSentimentDetectionJobs (Lude.Maybe Lude.Text)
lsdjNextToken = Lens.lens (nextToken :: ListSentimentDetectionJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSentimentDetectionJobs)
{-# DEPRECATED lsdjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjFilter :: Lens.Lens' ListSentimentDetectionJobs (Lude.Maybe SentimentDetectionJobFilter)
lsdjFilter = Lens.lens (filter :: ListSentimentDetectionJobs -> Lude.Maybe SentimentDetectionJobFilter) (\s a -> s {filter = a} :: ListSentimentDetectionJobs)
{-# DEPRECATED lsdjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjMaxResults :: Lens.Lens' ListSentimentDetectionJobs (Lude.Maybe Lude.Natural)
lsdjMaxResults = Lens.lens (maxResults :: ListSentimentDetectionJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSentimentDetectionJobs)
{-# DEPRECATED lsdjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSentimentDetectionJobs where
  page rq rs
    | Page.stop (rs Lens.^. lsdjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsdjrsSentimentDetectionJobPropertiesList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsdjNextToken Lens..~ rs Lens.^. lsdjrsNextToken

instance Lude.AWSRequest ListSentimentDetectionJobs where
  type
    Rs ListSentimentDetectionJobs =
      ListSentimentDetectionJobsResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSentimentDetectionJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> ( x Lude..?> "SentimentDetectionJobPropertiesList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSentimentDetectionJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.ListSentimentDetectionJobs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSentimentDetectionJobs where
  toJSON ListSentimentDetectionJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListSentimentDetectionJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSentimentDetectionJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSentimentDetectionJobsResponse' smart constructor.
data ListSentimentDetectionJobsResponse = ListSentimentDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list containing the properties of each job that is returned.
    sentimentDetectionJobPropertiesList :: Lude.Maybe [SentimentDetectionJobProperties],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSentimentDetectionJobsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'sentimentDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
-- * 'responseStatus' - The response status code.
mkListSentimentDetectionJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSentimentDetectionJobsResponse
mkListSentimentDetectionJobsResponse pResponseStatus_ =
  ListSentimentDetectionJobsResponse'
    { nextToken = Lude.Nothing,
      sentimentDetectionJobPropertiesList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjrsNextToken :: Lens.Lens' ListSentimentDetectionJobsResponse (Lude.Maybe Lude.Text)
lsdjrsNextToken = Lens.lens (nextToken :: ListSentimentDetectionJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSentimentDetectionJobsResponse)
{-# DEPRECATED lsdjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'sentimentDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjrsSentimentDetectionJobPropertiesList :: Lens.Lens' ListSentimentDetectionJobsResponse (Lude.Maybe [SentimentDetectionJobProperties])
lsdjrsSentimentDetectionJobPropertiesList = Lens.lens (sentimentDetectionJobPropertiesList :: ListSentimentDetectionJobsResponse -> Lude.Maybe [SentimentDetectionJobProperties]) (\s a -> s {sentimentDetectionJobPropertiesList = a} :: ListSentimentDetectionJobsResponse)
{-# DEPRECATED lsdjrsSentimentDetectionJobPropertiesList "Use generic-lens or generic-optics with 'sentimentDetectionJobPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdjrsResponseStatus :: Lens.Lens' ListSentimentDetectionJobsResponse Lude.Int
lsdjrsResponseStatus = Lens.lens (responseStatus :: ListSentimentDetectionJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSentimentDetectionJobsResponse)
{-# DEPRECATED lsdjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of key phrase detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
  ( -- * Creating a request
    ListKeyPhrasesDetectionJobs (..),
    mkListKeyPhrasesDetectionJobs,

    -- ** Request lenses
    lkpdjNextToken,
    lkpdjFilter,
    lkpdjMaxResults,

    -- * Destructuring the response
    ListKeyPhrasesDetectionJobsResponse (..),
    mkListKeyPhrasesDetectionJobsResponse,

    -- ** Response lenses
    lkpdjrsKeyPhrasesDetectionJobPropertiesList,
    lkpdjrsNextToken,
    lkpdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListKeyPhrasesDetectionJobs' smart constructor.
data ListKeyPhrasesDetectionJobs = ListKeyPhrasesDetectionJobs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    filter ::
      Lude.Maybe
        KeyPhrasesDetectionJobFilter,
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

-- | Creates a value of 'ListKeyPhrasesDetectionJobs' with the minimum fields required to make a request.
--
-- * 'filter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page. The default is 100.
-- * 'nextToken' - Identifies the next page of results to return.
mkListKeyPhrasesDetectionJobs ::
  ListKeyPhrasesDetectionJobs
mkListKeyPhrasesDetectionJobs =
  ListKeyPhrasesDetectionJobs'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjNextToken :: Lens.Lens' ListKeyPhrasesDetectionJobs (Lude.Maybe Lude.Text)
lkpdjNextToken = Lens.lens (nextToken :: ListKeyPhrasesDetectionJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListKeyPhrasesDetectionJobs)
{-# DEPRECATED lkpdjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjFilter :: Lens.Lens' ListKeyPhrasesDetectionJobs (Lude.Maybe KeyPhrasesDetectionJobFilter)
lkpdjFilter = Lens.lens (filter :: ListKeyPhrasesDetectionJobs -> Lude.Maybe KeyPhrasesDetectionJobFilter) (\s a -> s {filter = a} :: ListKeyPhrasesDetectionJobs)
{-# DEPRECATED lkpdjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjMaxResults :: Lens.Lens' ListKeyPhrasesDetectionJobs (Lude.Maybe Lude.Natural)
lkpdjMaxResults = Lens.lens (maxResults :: ListKeyPhrasesDetectionJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListKeyPhrasesDetectionJobs)
{-# DEPRECATED lkpdjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListKeyPhrasesDetectionJobs where
  page rq rs
    | Page.stop (rs Lens.^. lkpdjrsNextToken) = Lude.Nothing
    | Page.stop
        (rs Lens.^. lkpdjrsKeyPhrasesDetectionJobPropertiesList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lkpdjNextToken Lens..~ rs Lens.^. lkpdjrsNextToken

instance Lude.AWSRequest ListKeyPhrasesDetectionJobs where
  type
    Rs ListKeyPhrasesDetectionJobs =
      ListKeyPhrasesDetectionJobsResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListKeyPhrasesDetectionJobsResponse'
            Lude.<$> ( x Lude..?> "KeyPhrasesDetectionJobPropertiesList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListKeyPhrasesDetectionJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.ListKeyPhrasesDetectionJobs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListKeyPhrasesDetectionJobs where
  toJSON ListKeyPhrasesDetectionJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListKeyPhrasesDetectionJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListKeyPhrasesDetectionJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListKeyPhrasesDetectionJobsResponse' smart constructor.
data ListKeyPhrasesDetectionJobsResponse = ListKeyPhrasesDetectionJobsResponse'
  { keyPhrasesDetectionJobPropertiesList ::
      Lude.Maybe
        [KeyPhrasesDetectionJobProperties],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'ListKeyPhrasesDetectionJobsResponse' with the minimum fields required to make a request.
--
-- * 'keyPhrasesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'responseStatus' - The response status code.
mkListKeyPhrasesDetectionJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListKeyPhrasesDetectionJobsResponse
mkListKeyPhrasesDetectionJobsResponse pResponseStatus_ =
  ListKeyPhrasesDetectionJobsResponse'
    { keyPhrasesDetectionJobPropertiesList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'keyPhrasesDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjrsKeyPhrasesDetectionJobPropertiesList :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse (Lude.Maybe [KeyPhrasesDetectionJobProperties])
lkpdjrsKeyPhrasesDetectionJobPropertiesList = Lens.lens (keyPhrasesDetectionJobPropertiesList :: ListKeyPhrasesDetectionJobsResponse -> Lude.Maybe [KeyPhrasesDetectionJobProperties]) (\s a -> s {keyPhrasesDetectionJobPropertiesList = a} :: ListKeyPhrasesDetectionJobsResponse)
{-# DEPRECATED lkpdjrsKeyPhrasesDetectionJobPropertiesList "Use generic-lens or generic-optics with 'keyPhrasesDetectionJobPropertiesList' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjrsNextToken :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse (Lude.Maybe Lude.Text)
lkpdjrsNextToken = Lens.lens (nextToken :: ListKeyPhrasesDetectionJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListKeyPhrasesDetectionJobsResponse)
{-# DEPRECATED lkpdjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpdjrsResponseStatus :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse Lude.Int
lkpdjrsResponseStatus = Lens.lens (responseStatus :: ListKeyPhrasesDetectionJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListKeyPhrasesDetectionJobsResponse)
{-# DEPRECATED lkpdjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

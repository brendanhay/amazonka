{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the dominant language detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
  ( -- * Creating a request
    ListDominantLanguageDetectionJobs (..),
    mkListDominantLanguageDetectionJobs,

    -- ** Request lenses
    ldldjNextToken,
    ldldjFilter,
    ldldjMaxResults,

    -- * Destructuring the response
    ListDominantLanguageDetectionJobsResponse (..),
    mkListDominantLanguageDetectionJobsResponse,

    -- ** Response lenses
    ldldjrsNextToken,
    ldldjrsDominantLanguageDetectionJobPropertiesList,
    ldldjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDominantLanguageDetectionJobs' smart constructor.
data ListDominantLanguageDetectionJobs = ListDominantLanguageDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filters that jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
    filter :: Lude.Maybe DominantLanguageDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDominantLanguageDetectionJobs' with the minimum fields required to make a request.
--
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'filter' - Filters that jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page. The default is 100.
mkListDominantLanguageDetectionJobs ::
  ListDominantLanguageDetectionJobs
mkListDominantLanguageDetectionJobs =
  ListDominantLanguageDetectionJobs'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjNextToken :: Lens.Lens' ListDominantLanguageDetectionJobs (Lude.Maybe Lude.Text)
ldldjNextToken = Lens.lens (nextToken :: ListDominantLanguageDetectionJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDominantLanguageDetectionJobs)
{-# DEPRECATED ldldjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters that jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjFilter :: Lens.Lens' ListDominantLanguageDetectionJobs (Lude.Maybe DominantLanguageDetectionJobFilter)
ldldjFilter = Lens.lens (filter :: ListDominantLanguageDetectionJobs -> Lude.Maybe DominantLanguageDetectionJobFilter) (\s a -> s {filter = a} :: ListDominantLanguageDetectionJobs)
{-# DEPRECATED ldldjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjMaxResults :: Lens.Lens' ListDominantLanguageDetectionJobs (Lude.Maybe Lude.Natural)
ldldjMaxResults = Lens.lens (maxResults :: ListDominantLanguageDetectionJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDominantLanguageDetectionJobs)
{-# DEPRECATED ldldjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDominantLanguageDetectionJobs where
  page rq rs
    | Page.stop (rs Lens.^. ldldjrsNextToken) = Lude.Nothing
    | Page.stop
        (rs Lens.^. ldldjrsDominantLanguageDetectionJobPropertiesList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldldjNextToken Lens..~ rs Lens.^. ldldjrsNextToken

instance Lude.AWSRequest ListDominantLanguageDetectionJobs where
  type
    Rs ListDominantLanguageDetectionJobs =
      ListDominantLanguageDetectionJobsResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDominantLanguageDetectionJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> ( x Lude..?> "DominantLanguageDetectionJobPropertiesList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDominantLanguageDetectionJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.ListDominantLanguageDetectionJobs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDominantLanguageDetectionJobs where
  toJSON ListDominantLanguageDetectionJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDominantLanguageDetectionJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDominantLanguageDetectionJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDominantLanguageDetectionJobsResponse' smart constructor.
data ListDominantLanguageDetectionJobsResponse = ListDominantLanguageDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list containing the properties of each job that is returned.
    dominantLanguageDetectionJobPropertiesList :: Lude.Maybe [DominantLanguageDetectionJobProperties],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDominantLanguageDetectionJobsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'dominantLanguageDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
-- * 'responseStatus' - The response status code.
mkListDominantLanguageDetectionJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDominantLanguageDetectionJobsResponse
mkListDominantLanguageDetectionJobsResponse pResponseStatus_ =
  ListDominantLanguageDetectionJobsResponse'
    { nextToken =
        Lude.Nothing,
      dominantLanguageDetectionJobPropertiesList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjrsNextToken :: Lens.Lens' ListDominantLanguageDetectionJobsResponse (Lude.Maybe Lude.Text)
ldldjrsNextToken = Lens.lens (nextToken :: ListDominantLanguageDetectionJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDominantLanguageDetectionJobsResponse)
{-# DEPRECATED ldldjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'dominantLanguageDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjrsDominantLanguageDetectionJobPropertiesList :: Lens.Lens' ListDominantLanguageDetectionJobsResponse (Lude.Maybe [DominantLanguageDetectionJobProperties])
ldldjrsDominantLanguageDetectionJobPropertiesList = Lens.lens (dominantLanguageDetectionJobPropertiesList :: ListDominantLanguageDetectionJobsResponse -> Lude.Maybe [DominantLanguageDetectionJobProperties]) (\s a -> s {dominantLanguageDetectionJobPropertiesList = a} :: ListDominantLanguageDetectionJobsResponse)
{-# DEPRECATED ldldjrsDominantLanguageDetectionJobPropertiesList "Use generic-lens or generic-optics with 'dominantLanguageDetectionJobPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldldjrsResponseStatus :: Lens.Lens' ListDominantLanguageDetectionJobsResponse Lude.Int
ldldjrsResponseStatus = Lens.lens (responseStatus :: ListDominantLanguageDetectionJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDominantLanguageDetectionJobsResponse)
{-# DEPRECATED ldldjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

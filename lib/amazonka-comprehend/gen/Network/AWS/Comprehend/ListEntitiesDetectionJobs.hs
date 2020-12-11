{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListEntitiesDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the entity detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListEntitiesDetectionJobs
  ( -- * Creating a request
    ListEntitiesDetectionJobs (..),
    mkListEntitiesDetectionJobs,

    -- ** Request lenses
    ledjNextToken,
    ledjFilter,
    ledjMaxResults,

    -- * Destructuring the response
    ListEntitiesDetectionJobsResponse (..),
    mkListEntitiesDetectionJobsResponse,

    -- ** Response lenses
    ledjrsEntitiesDetectionJobPropertiesList,
    ledjrsNextToken,
    ledjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListEntitiesDetectionJobs' smart constructor.
data ListEntitiesDetectionJobs = ListEntitiesDetectionJobs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    filter ::
      Lude.Maybe EntitiesDetectionJobFilter,
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

-- | Creates a value of 'ListEntitiesDetectionJobs' with the minimum fields required to make a request.
--
-- * 'filter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page. The default is 100.
-- * 'nextToken' - Identifies the next page of results to return.
mkListEntitiesDetectionJobs ::
  ListEntitiesDetectionJobs
mkListEntitiesDetectionJobs =
  ListEntitiesDetectionJobs'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjNextToken :: Lens.Lens' ListEntitiesDetectionJobs (Lude.Maybe Lude.Text)
ledjNextToken = Lens.lens (nextToken :: ListEntitiesDetectionJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEntitiesDetectionJobs)
{-# DEPRECATED ledjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjFilter :: Lens.Lens' ListEntitiesDetectionJobs (Lude.Maybe EntitiesDetectionJobFilter)
ledjFilter = Lens.lens (filter :: ListEntitiesDetectionJobs -> Lude.Maybe EntitiesDetectionJobFilter) (\s a -> s {filter = a} :: ListEntitiesDetectionJobs)
{-# DEPRECATED ledjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjMaxResults :: Lens.Lens' ListEntitiesDetectionJobs (Lude.Maybe Lude.Natural)
ledjMaxResults = Lens.lens (maxResults :: ListEntitiesDetectionJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListEntitiesDetectionJobs)
{-# DEPRECATED ledjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListEntitiesDetectionJobs where
  page rq rs
    | Page.stop (rs Lens.^. ledjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ledjrsEntitiesDetectionJobPropertiesList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ledjNextToken Lens..~ rs Lens.^. ledjrsNextToken

instance Lude.AWSRequest ListEntitiesDetectionJobs where
  type
    Rs ListEntitiesDetectionJobs =
      ListEntitiesDetectionJobsResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEntitiesDetectionJobsResponse'
            Lude.<$> ( x Lude..?> "EntitiesDetectionJobPropertiesList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListEntitiesDetectionJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.ListEntitiesDetectionJobs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListEntitiesDetectionJobs where
  toJSON ListEntitiesDetectionJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListEntitiesDetectionJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEntitiesDetectionJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListEntitiesDetectionJobsResponse' smart constructor.
data ListEntitiesDetectionJobsResponse = ListEntitiesDetectionJobsResponse'
  { entitiesDetectionJobPropertiesList ::
      Lude.Maybe
        [EntitiesDetectionJobProperties],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListEntitiesDetectionJobsResponse' with the minimum fields required to make a request.
--
-- * 'entitiesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'responseStatus' - The response status code.
mkListEntitiesDetectionJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEntitiesDetectionJobsResponse
mkListEntitiesDetectionJobsResponse pResponseStatus_ =
  ListEntitiesDetectionJobsResponse'
    { entitiesDetectionJobPropertiesList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'entitiesDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjrsEntitiesDetectionJobPropertiesList :: Lens.Lens' ListEntitiesDetectionJobsResponse (Lude.Maybe [EntitiesDetectionJobProperties])
ledjrsEntitiesDetectionJobPropertiesList = Lens.lens (entitiesDetectionJobPropertiesList :: ListEntitiesDetectionJobsResponse -> Lude.Maybe [EntitiesDetectionJobProperties]) (\s a -> s {entitiesDetectionJobPropertiesList = a} :: ListEntitiesDetectionJobsResponse)
{-# DEPRECATED ledjrsEntitiesDetectionJobPropertiesList "Use generic-lens or generic-optics with 'entitiesDetectionJobPropertiesList' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjrsNextToken :: Lens.Lens' ListEntitiesDetectionJobsResponse (Lude.Maybe Lude.Text)
ledjrsNextToken = Lens.lens (nextToken :: ListEntitiesDetectionJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEntitiesDetectionJobsResponse)
{-# DEPRECATED ledjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ledjrsResponseStatus :: Lens.Lens' ListEntitiesDetectionJobsResponse Lude.Int
ledjrsResponseStatus = Lens.lens (responseStatus :: ListEntitiesDetectionJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEntitiesDetectionJobsResponse)
{-# DEPRECATED ledjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

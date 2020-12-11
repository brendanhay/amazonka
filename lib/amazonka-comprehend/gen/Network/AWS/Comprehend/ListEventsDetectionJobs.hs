{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListEventsDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the events detection jobs that you have submitted.
module Network.AWS.Comprehend.ListEventsDetectionJobs
  ( -- * Creating a request
    ListEventsDetectionJobs (..),
    mkListEventsDetectionJobs,

    -- ** Request lenses
    lNextToken,
    lFilter,
    lMaxResults,

    -- * Destructuring the response
    ListEventsDetectionJobsResponse (..),
    mkListEventsDetectionJobsResponse,

    -- ** Response lenses
    lrsEventsDetectionJobPropertiesList,
    lrsNextToken,
    lrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListEventsDetectionJobs' smart constructor.
data ListEventsDetectionJobs = ListEventsDetectionJobs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    filter ::
      Lude.Maybe EventsDetectionJobFilter,
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

-- | Creates a value of 'ListEventsDetectionJobs' with the minimum fields required to make a request.
--
-- * 'filter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page.
-- * 'nextToken' - Identifies the next page of results to return.
mkListEventsDetectionJobs ::
  ListEventsDetectionJobs
mkListEventsDetectionJobs =
  ListEventsDetectionJobs'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListEventsDetectionJobs (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListEventsDetectionJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEventsDetectionJobs)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lFilter :: Lens.Lens' ListEventsDetectionJobs (Lude.Maybe EventsDetectionJobFilter)
lFilter = Lens.lens (filter :: ListEventsDetectionJobs -> Lude.Maybe EventsDetectionJobFilter) (\s a -> s {filter = a} :: ListEventsDetectionJobs)
{-# DEPRECATED lFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListEventsDetectionJobs (Lude.Maybe Lude.Natural)
lMaxResults = Lens.lens (maxResults :: ListEventsDetectionJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListEventsDetectionJobs)
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListEventsDetectionJobs where
  type Rs ListEventsDetectionJobs = ListEventsDetectionJobsResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEventsDetectionJobsResponse'
            Lude.<$> ( x Lude..?> "EventsDetectionJobPropertiesList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListEventsDetectionJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.ListEventsDetectionJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListEventsDetectionJobs where
  toJSON ListEventsDetectionJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListEventsDetectionJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEventsDetectionJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListEventsDetectionJobsResponse' smart constructor.
data ListEventsDetectionJobsResponse = ListEventsDetectionJobsResponse'
  { eventsDetectionJobPropertiesList ::
      Lude.Maybe
        [EventsDetectionJobProperties],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListEventsDetectionJobsResponse' with the minimum fields required to make a request.
--
-- * 'eventsDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'responseStatus' - The response status code.
mkListEventsDetectionJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEventsDetectionJobsResponse
mkListEventsDetectionJobsResponse pResponseStatus_ =
  ListEventsDetectionJobsResponse'
    { eventsDetectionJobPropertiesList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'eventsDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsEventsDetectionJobPropertiesList :: Lens.Lens' ListEventsDetectionJobsResponse (Lude.Maybe [EventsDetectionJobProperties])
lrsEventsDetectionJobPropertiesList = Lens.lens (eventsDetectionJobPropertiesList :: ListEventsDetectionJobsResponse -> Lude.Maybe [EventsDetectionJobProperties]) (\s a -> s {eventsDetectionJobPropertiesList = a} :: ListEventsDetectionJobsResponse)
{-# DEPRECATED lrsEventsDetectionJobPropertiesList "Use generic-lens or generic-optics with 'eventsDetectionJobPropertiesList' instead." #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListEventsDetectionJobsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListEventsDetectionJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEventsDetectionJobsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListEventsDetectionJobsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListEventsDetectionJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEventsDetectionJobsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

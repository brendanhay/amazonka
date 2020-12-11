{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.ListActivities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing activities.
--
-- If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.ListActivities
  ( -- * Creating a request
    ListActivities (..),
    mkListActivities,

    -- ** Request lenses
    laNextToken,
    laMaxResults,

    -- * Destructuring the response
    ListActivitiesResponse (..),
    mkListActivitiesResponse,

    -- ** Response lenses
    larsNextToken,
    larsResponseStatus,
    larsActivities,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkListActivities' smart constructor.
data ListActivities = ListActivities'
  { nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListActivities' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
-- * 'nextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
mkListActivities ::
  ListActivities
mkListActivities =
  ListActivities'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListActivities (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListActivities -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListActivities)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListActivities (Lude.Maybe Lude.Natural)
laMaxResults = Lens.lens (maxResults :: ListActivities -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListActivities)
{-# DEPRECATED laMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListActivities where
  page rq rs
    | Page.stop (rs Lens.^. larsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larsActivities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. larsNextToken

instance Lude.AWSRequest ListActivities where
  type Rs ListActivities = ListActivitiesResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListActivitiesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "activities" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListActivities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.ListActivities" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListActivities where
  toJSON ListActivities' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListActivities where
  toPath = Lude.const "/"

instance Lude.ToQuery ListActivities where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListActivitiesResponse' smart constructor.
data ListActivitiesResponse = ListActivitiesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    activities :: [ActivityListItem]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListActivitiesResponse' with the minimum fields required to make a request.
--
-- * 'activities' - The list of activities.
-- * 'nextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
-- * 'responseStatus' - The response status code.
mkListActivitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListActivitiesResponse
mkListActivitiesResponse pResponseStatus_ =
  ListActivitiesResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      activities = Lude.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListActivitiesResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListActivitiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListActivitiesResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListActivitiesResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListActivitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListActivitiesResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The list of activities.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsActivities :: Lens.Lens' ListActivitiesResponse [ActivityListItem]
larsActivities = Lens.lens (activities :: ListActivitiesResponse -> [ActivityListItem]) (\s a -> s {activities = a} :: ListActivitiesResponse)
{-# DEPRECATED larsActivities "Use generic-lens or generic-optics with 'activities' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListWorkteams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of private work teams that you have defined in a region. The list may be empty if no work team satisfies the filter specified in the @NameContains@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListWorkteams
  ( -- * Creating a request
    ListWorkteams (..),
    mkListWorkteams,

    -- ** Request lenses
    lwNameContains,
    lwNextToken,
    lwSortOrder,
    lwMaxResults,
    lwSortBy,

    -- * Destructuring the response
    ListWorkteamsResponse (..),
    mkListWorkteamsResponse,

    -- ** Response lenses
    lrsNextToken,
    lrsResponseStatus,
    lrsWorkteams,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListWorkteams' smart constructor.
data ListWorkteams = ListWorkteams'
  { nameContains ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe ListWorkteamsSortByOptions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkteams' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of work teams to return in each page of the response.
-- * 'nameContains' - A string in the work team's name. This filter returns only work teams whose name contains the specified string.
-- * 'nextToken' - If the result of the previous @ListWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
-- * 'sortBy' - The field to sort results by. The default is @CreationTime@ .
-- * 'sortOrder' - The sort order for results. The default is @Ascending@ .
mkListWorkteams ::
  ListWorkteams
mkListWorkteams =
  ListWorkteams'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A string in the work team's name. This filter returns only work teams whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwNameContains :: Lens.Lens' ListWorkteams (Lude.Maybe Lude.Text)
lwNameContains = Lens.lens (nameContains :: ListWorkteams -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListWorkteams)
{-# DEPRECATED lwNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwNextToken :: Lens.Lens' ListWorkteams (Lude.Maybe Lude.Text)
lwNextToken = Lens.lens (nextToken :: ListWorkteams -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkteams)
{-# DEPRECATED lwNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwSortOrder :: Lens.Lens' ListWorkteams (Lude.Maybe SortOrder)
lwSortOrder = Lens.lens (sortOrder :: ListWorkteams -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListWorkteams)
{-# DEPRECATED lwSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum number of work teams to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwMaxResults :: Lens.Lens' ListWorkteams (Lude.Maybe Lude.Natural)
lwMaxResults = Lens.lens (maxResults :: ListWorkteams -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListWorkteams)
{-# DEPRECATED lwMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwSortBy :: Lens.Lens' ListWorkteams (Lude.Maybe ListWorkteamsSortByOptions)
lwSortBy = Lens.lens (sortBy :: ListWorkteams -> Lude.Maybe ListWorkteamsSortByOptions) (\s a -> s {sortBy = a} :: ListWorkteams)
{-# DEPRECATED lwSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListWorkteams where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsWorkteams) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lwNextToken Lens..~ rs Lens.^. lrsNextToken

instance Lude.AWSRequest ListWorkteams where
  type Rs ListWorkteams = ListWorkteamsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListWorkteamsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Workteams" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListWorkteams where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListWorkteams" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListWorkteams where
  toJSON ListWorkteams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListWorkteams where
  toPath = Lude.const "/"

instance Lude.ToQuery ListWorkteams where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListWorkteamsResponse' smart constructor.
data ListWorkteamsResponse = ListWorkteamsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    workteams :: [Workteam]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkteamsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
-- * 'responseStatus' - The response status code.
-- * 'workteams' - An array of @Workteam@ objects, each describing a work team.
mkListWorkteamsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListWorkteamsResponse
mkListWorkteamsResponse pResponseStatus_ =
  ListWorkteamsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      workteams = Lude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListWorkteamsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListWorkteamsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkteamsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListWorkteamsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListWorkteamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListWorkteamsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An array of @Workteam@ objects, each describing a work team.
--
-- /Note:/ Consider using 'workteams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsWorkteams :: Lens.Lens' ListWorkteamsResponse [Workteam]
lrsWorkteams = Lens.lens (workteams :: ListWorkteamsResponse -> [Workteam]) (\s a -> s {workteams = a} :: ListWorkteamsResponse)
{-# DEPRECATED lrsWorkteams "Use generic-lens or generic-optics with 'workteams' instead." #-}

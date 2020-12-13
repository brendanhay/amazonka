{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    lNameContains,
    lNextToken,
    lSortOrder,
    lMaxResults,
    lSortBy,

    -- * Destructuring the response
    ListWorkteamsResponse (..),
    mkListWorkteamsResponse,

    -- ** Response lenses
    lwsrsNextToken,
    lwsrsWorkteams,
    lwsrsResponseStatus,
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
  { -- | A string in the work team's name. This filter returns only work teams whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | If the result of the previous @ListWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | The maximum number of work teams to return in each page of the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The field to sort results by. The default is @CreationTime@ .
    sortBy :: Lude.Maybe ListWorkteamsSortByOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkteams' with the minimum fields required to make a request.
--
-- * 'nameContains' - A string in the work team's name. This filter returns only work teams whose name contains the specified string.
-- * 'nextToken' - If the result of the previous @ListWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
-- * 'sortOrder' - The sort order for results. The default is @Ascending@ .
-- * 'maxResults' - The maximum number of work teams to return in each page of the response.
-- * 'sortBy' - The field to sort results by. The default is @CreationTime@ .
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
lNameContains :: Lens.Lens' ListWorkteams (Lude.Maybe Lude.Text)
lNameContains = Lens.lens (nameContains :: ListWorkteams -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListWorkteams)
{-# DEPRECATED lNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListWorkteams (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListWorkteams -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkteams)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSortOrder :: Lens.Lens' ListWorkteams (Lude.Maybe SortOrder)
lSortOrder = Lens.lens (sortOrder :: ListWorkteams -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListWorkteams)
{-# DEPRECATED lSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum number of work teams to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListWorkteams (Lude.Maybe Lude.Natural)
lMaxResults = Lens.lens (maxResults :: ListWorkteams -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListWorkteams)
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSortBy :: Lens.Lens' ListWorkteams (Lude.Maybe ListWorkteamsSortByOptions)
lSortBy = Lens.lens (sortBy :: ListWorkteams -> Lude.Maybe ListWorkteamsSortByOptions) (\s a -> s {sortBy = a} :: ListWorkteams)
{-# DEPRECATED lSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListWorkteams where
  page rq rs
    | Page.stop (rs Lens.^. lwsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lwsrsWorkteams) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lNextToken Lens..~ rs Lens.^. lwsrsNextToken

instance Lude.AWSRequest ListWorkteams where
  type Rs ListWorkteams = ListWorkteamsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListWorkteamsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Workteams" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of @Workteam@ objects, each describing a work team.
    workteams :: [Workteam],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkteamsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
-- * 'workteams' - An array of @Workteam@ objects, each describing a work team.
-- * 'responseStatus' - The response status code.
mkListWorkteamsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListWorkteamsResponse
mkListWorkteamsResponse pResponseStatus_ =
  ListWorkteamsResponse'
    { nextToken = Lude.Nothing,
      workteams = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsrsNextToken :: Lens.Lens' ListWorkteamsResponse (Lude.Maybe Lude.Text)
lwsrsNextToken = Lens.lens (nextToken :: ListWorkteamsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkteamsResponse)
{-# DEPRECATED lwsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @Workteam@ objects, each describing a work team.
--
-- /Note:/ Consider using 'workteams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsrsWorkteams :: Lens.Lens' ListWorkteamsResponse [Workteam]
lwsrsWorkteams = Lens.lens (workteams :: ListWorkteamsResponse -> [Workteam]) (\s a -> s {workteams = a} :: ListWorkteamsResponse)
{-# DEPRECATED lwsrsWorkteams "Use generic-lens or generic-optics with 'workteams' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsrsResponseStatus :: Lens.Lens' ListWorkteamsResponse Lude.Int
lwsrsResponseStatus = Lens.lens (responseStatus :: ListWorkteamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListWorkteamsResponse)
{-# DEPRECATED lwsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

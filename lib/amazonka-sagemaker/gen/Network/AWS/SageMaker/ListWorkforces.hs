{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListWorkforces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to list all private and vendor workforces in an AWS Region. Note that you can only have one private workforce per AWS Region.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListWorkforces
  ( -- * Creating a request
    ListWorkforces (..),
    mkListWorkforces,

    -- ** Request lenses
    lwsNameContains,
    lwsNextToken,
    lwsSortOrder,
    lwsMaxResults,
    lwsSortBy,

    -- * Destructuring the response
    ListWorkforcesResponse (..),
    mkListWorkforcesResponse,

    -- ** Response lenses
    lwrsNextToken,
    lwrsResponseStatus,
    lwrsWorkforces,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListWorkforces' smart constructor.
data ListWorkforces = ListWorkforces'
  { nameContains ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe ListWorkforcesSortByOptions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkforces' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of workforces returned in the response.
-- * 'nameContains' - A filter you can use to search for workforces using part of the workforce name.
-- * 'nextToken' - A token to resume pagination.
-- * 'sortBy' - Sort workforces using the workforce name or creation date.
-- * 'sortOrder' - Sort workforces in ascending or descending order.
mkListWorkforces ::
  ListWorkforces
mkListWorkforces =
  ListWorkforces'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A filter you can use to search for workforces using part of the workforce name.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsNameContains :: Lens.Lens' ListWorkforces (Lude.Maybe Lude.Text)
lwsNameContains = Lens.lens (nameContains :: ListWorkforces -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListWorkforces)
{-# DEPRECATED lwsNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsNextToken :: Lens.Lens' ListWorkforces (Lude.Maybe Lude.Text)
lwsNextToken = Lens.lens (nextToken :: ListWorkforces -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkforces)
{-# DEPRECATED lwsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Sort workforces in ascending or descending order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsSortOrder :: Lens.Lens' ListWorkforces (Lude.Maybe SortOrder)
lwsSortOrder = Lens.lens (sortOrder :: ListWorkforces -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListWorkforces)
{-# DEPRECATED lwsSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum number of workforces returned in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsMaxResults :: Lens.Lens' ListWorkforces (Lude.Maybe Lude.Natural)
lwsMaxResults = Lens.lens (maxResults :: ListWorkforces -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListWorkforces)
{-# DEPRECATED lwsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sort workforces using the workforce name or creation date.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsSortBy :: Lens.Lens' ListWorkforces (Lude.Maybe ListWorkforcesSortByOptions)
lwsSortBy = Lens.lens (sortBy :: ListWorkforces -> Lude.Maybe ListWorkforcesSortByOptions) (\s a -> s {sortBy = a} :: ListWorkforces)
{-# DEPRECATED lwsSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListWorkforces where
  page rq rs
    | Page.stop (rs Lens.^. lwrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lwrsWorkforces) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lwsNextToken Lens..~ rs Lens.^. lwrsNextToken

instance Lude.AWSRequest ListWorkforces where
  type Rs ListWorkforces = ListWorkforcesResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListWorkforcesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Workforces" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListWorkforces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListWorkforces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListWorkforces where
  toJSON ListWorkforces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListWorkforces where
  toPath = Lude.const "/"

instance Lude.ToQuery ListWorkforces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListWorkforcesResponse' smart constructor.
data ListWorkforcesResponse = ListWorkforcesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    workforces :: [Workforce]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkforcesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token to resume pagination.
-- * 'responseStatus' - The response status code.
-- * 'workforces' - A list containing information about your workforce.
mkListWorkforcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListWorkforcesResponse
mkListWorkforcesResponse pResponseStatus_ =
  ListWorkforcesResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      workforces = Lude.mempty
    }

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsNextToken :: Lens.Lens' ListWorkforcesResponse (Lude.Maybe Lude.Text)
lwrsNextToken = Lens.lens (nextToken :: ListWorkforcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkforcesResponse)
{-# DEPRECATED lwrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsResponseStatus :: Lens.Lens' ListWorkforcesResponse Lude.Int
lwrsResponseStatus = Lens.lens (responseStatus :: ListWorkforcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListWorkforcesResponse)
{-# DEPRECATED lwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list containing information about your workforce.
--
-- /Note:/ Consider using 'workforces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsWorkforces :: Lens.Lens' ListWorkforcesResponse [Workforce]
lwrsWorkforces = Lens.lens (workforces :: ListWorkforcesResponse -> [Workforce]) (\s a -> s {workforces = a} :: ListWorkforcesResponse)
{-# DEPRECATED lwrsWorkforces "Use generic-lens or generic-optics with 'workforces' instead." #-}

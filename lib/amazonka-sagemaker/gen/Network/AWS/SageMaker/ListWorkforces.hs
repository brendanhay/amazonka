{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    lwNameContains,
    lwNextToken,
    lwSortOrder,
    lwMaxResults,
    lwSortBy,

    -- * Destructuring the response
    ListWorkforcesResponse (..),
    mkListWorkforcesResponse,

    -- ** Response lenses
    lwrsNextToken,
    lwrsWorkforces,
    lwrsResponseStatus,
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
  { -- | A filter you can use to search for workforces using part of the workforce name.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A token to resume pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Sort workforces in ascending or descending order.
    sortOrder :: Lude.Maybe SortOrder,
    -- | The maximum number of workforces returned in the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Sort workforces using the workforce name or creation date.
    sortBy :: Lude.Maybe ListWorkforcesSortByOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkforces' with the minimum fields required to make a request.
--
-- * 'nameContains' - A filter you can use to search for workforces using part of the workforce name.
-- * 'nextToken' - A token to resume pagination.
-- * 'sortOrder' - Sort workforces in ascending or descending order.
-- * 'maxResults' - The maximum number of workforces returned in the response.
-- * 'sortBy' - Sort workforces using the workforce name or creation date.
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
lwNameContains :: Lens.Lens' ListWorkforces (Lude.Maybe Lude.Text)
lwNameContains = Lens.lens (nameContains :: ListWorkforces -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListWorkforces)
{-# DEPRECATED lwNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwNextToken :: Lens.Lens' ListWorkforces (Lude.Maybe Lude.Text)
lwNextToken = Lens.lens (nextToken :: ListWorkforces -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkforces)
{-# DEPRECATED lwNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Sort workforces in ascending or descending order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwSortOrder :: Lens.Lens' ListWorkforces (Lude.Maybe SortOrder)
lwSortOrder = Lens.lens (sortOrder :: ListWorkforces -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListWorkforces)
{-# DEPRECATED lwSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum number of workforces returned in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwMaxResults :: Lens.Lens' ListWorkforces (Lude.Maybe Lude.Natural)
lwMaxResults = Lens.lens (maxResults :: ListWorkforces -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListWorkforces)
{-# DEPRECATED lwMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sort workforces using the workforce name or creation date.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwSortBy :: Lens.Lens' ListWorkforces (Lude.Maybe ListWorkforcesSortByOptions)
lwSortBy = Lens.lens (sortBy :: ListWorkforces -> Lude.Maybe ListWorkforcesSortByOptions) (\s a -> s {sortBy = a} :: ListWorkforces)
{-# DEPRECATED lwSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListWorkforces where
  page rq rs
    | Page.stop (rs Lens.^. lwrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lwrsWorkforces) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lwNextToken Lens..~ rs Lens.^. lwrsNextToken

instance Lude.AWSRequest ListWorkforces where
  type Rs ListWorkforces = ListWorkforcesResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListWorkforcesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Workforces" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | A token to resume pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list containing information about your workforce.
    workforces :: [Workforce],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListWorkforcesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token to resume pagination.
-- * 'workforces' - A list containing information about your workforce.
-- * 'responseStatus' - The response status code.
mkListWorkforcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListWorkforcesResponse
mkListWorkforcesResponse pResponseStatus_ =
  ListWorkforcesResponse'
    { nextToken = Lude.Nothing,
      workforces = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsNextToken :: Lens.Lens' ListWorkforcesResponse (Lude.Maybe Lude.Text)
lwrsNextToken = Lens.lens (nextToken :: ListWorkforcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListWorkforcesResponse)
{-# DEPRECATED lwrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list containing information about your workforce.
--
-- /Note:/ Consider using 'workforces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsWorkforces :: Lens.Lens' ListWorkforcesResponse [Workforce]
lwrsWorkforces = Lens.lens (workforces :: ListWorkforcesResponse -> [Workforce]) (\s a -> s {workforces = a} :: ListWorkforcesResponse)
{-# DEPRECATED lwrsWorkforces "Use generic-lens or generic-optics with 'workforces' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrsResponseStatus :: Lens.Lens' ListWorkforcesResponse Lude.Int
lwrsResponseStatus = Lens.lens (responseStatus :: ListWorkforcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListWorkforcesResponse)
{-# DEPRECATED lwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

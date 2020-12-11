{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.ListOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists operations that match the criteria that you specify.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListOperations
  ( -- * Creating a request
    ListOperations (..),
    mkListOperations,

    -- ** Request lenses
    loFilters,
    loNextToken,
    loMaxResults,

    -- * Destructuring the response
    ListOperationsResponse (..),
    mkListOperationsResponse,

    -- ** Response lenses
    lorsNextToken,
    lorsOperations,
    lorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkListOperations' smart constructor.
data ListOperations = ListOperations'
  { filters ::
      Lude.Maybe [OperationFilter],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListOperations' with the minimum fields required to make a request.
--
-- * 'filters' - A complex type that contains specifications for the operations that you want to list, for example, operations that you started between a specified start date and end date.
--
-- If you specify more than one filter, an operation must match all filters to be returned by @ListOperations@ .
-- * 'maxResults' - The maximum number of items that you want AWS Cloud Map to return in the response to a @ListOperations@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 operations.
-- * 'nextToken' - For the first @ListOperations@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
mkListOperations ::
  ListOperations
mkListOperations =
  ListOperations'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A complex type that contains specifications for the operations that you want to list, for example, operations that you started between a specified start date and end date.
--
-- If you specify more than one filter, an operation must match all filters to be returned by @ListOperations@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loFilters :: Lens.Lens' ListOperations (Lude.Maybe [OperationFilter])
loFilters = Lens.lens (filters :: ListOperations -> Lude.Maybe [OperationFilter]) (\s a -> s {filters = a} :: ListOperations)
{-# DEPRECATED loFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | For the first @ListOperations@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loNextToken :: Lens.Lens' ListOperations (Lude.Maybe Lude.Text)
loNextToken = Lens.lens (nextToken :: ListOperations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOperations)
{-# DEPRECATED loNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items that you want AWS Cloud Map to return in the response to a @ListOperations@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 operations.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaxResults :: Lens.Lens' ListOperations (Lude.Maybe Lude.Natural)
loMaxResults = Lens.lens (maxResults :: ListOperations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListOperations)
{-# DEPRECATED loMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListOperations where
  page rq rs
    | Page.stop (rs Lens.^. lorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lorsOperations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loNextToken Lens..~ rs Lens.^. lorsNextToken

instance Lude.AWSRequest ListOperations where
  type Rs ListOperations = ListOperationsResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOperationsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOperations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.ListOperations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListOperations where
  toJSON ListOperations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListOperations where
  toPath = Lude.const "/"

instance Lude.ToQuery ListOperations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    operations :: Lude.Maybe [OperationSummary],
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

-- | Creates a value of 'ListOperationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
-- * 'operations' - Summary information about the operations that match the specified criteria.
-- * 'responseStatus' - The response status code.
mkListOperationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOperationsResponse
mkListOperationsResponse pResponseStatus_ =
  ListOperationsResponse'
    { nextToken = Lude.Nothing,
      operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsNextToken :: Lens.Lens' ListOperationsResponse (Lude.Maybe Lude.Text)
lorsNextToken = Lens.lens (nextToken :: ListOperationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOperationsResponse)
{-# DEPRECATED lorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Summary information about the operations that match the specified criteria.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsOperations :: Lens.Lens' ListOperationsResponse (Lude.Maybe [OperationSummary])
lorsOperations = Lens.lens (operations :: ListOperationsResponse -> Lude.Maybe [OperationSummary]) (\s a -> s {operations = a} :: ListOperationsResponse)
{-# DEPRECATED lorsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsResponseStatus :: Lens.Lens' ListOperationsResponse Lude.Int
lorsResponseStatus = Lens.lens (responseStatus :: ListOperationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOperationsResponse)
{-# DEPRECATED lorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

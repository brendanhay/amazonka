{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.ListItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of metadata entries about folders and objects in the specified folder.
--
-- This operation returns paginated results.
module Network.AWS.MediaStoreData.ListItems
  ( -- * Creating a request
    ListItems (..),
    mkListItems,

    -- ** Request lenses
    liPath,
    liNextToken,
    liMaxResults,

    -- * Destructuring the response
    ListItemsResponse (..),
    mkListItemsResponse,

    -- ** Response lenses
    lirsItems,
    lirsNextToken,
    lirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListItems' smart constructor.
data ListItems = ListItems'
  { path :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListItems' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return per API request. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. Although 2,000 items match your request, the service returns no more than the first 500 items. (The service also returns a @NextToken@ value that you can use to fetch the next batch of results.) The service might return fewer results than the @MaxResults@ value.
--
-- If @MaxResults@ is not included in the request, the service defaults to pagination with a maximum of 1,000 results per page.
-- * 'nextToken' - The token that identifies which batch of results that you want to see. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value. To see the next batch of results, you can submit the @ListItems@ request a second time and specify the @NextToken@ value.
--
-- Tokens expire after 15 minutes.
-- * 'path' - The path in the container from which to retrieve items. Format: <folder name>/<folder name>/<file name>
mkListItems ::
  ListItems
mkListItems =
  ListItems'
    { path = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The path in the container from which to retrieve items. Format: <folder name>/<folder name>/<file name>
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liPath :: Lens.Lens' ListItems (Lude.Maybe Lude.Text)
liPath = Lens.lens (path :: ListItems -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: ListItems)
{-# DEPRECATED liPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The token that identifies which batch of results that you want to see. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value. To see the next batch of results, you can submit the @ListItems@ request a second time and specify the @NextToken@ value.
--
-- Tokens expire after 15 minutes.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListItems (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListItems -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListItems)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return per API request. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. Although 2,000 items match your request, the service returns no more than the first 500 items. (The service also returns a @NextToken@ value that you can use to fetch the next batch of results.) The service might return fewer results than the @MaxResults@ value.
--
-- If @MaxResults@ is not included in the request, the service defaults to pagination with a maximum of 1,000 results per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListItems (Lude.Maybe Lude.Natural)
liMaxResults = Lens.lens (maxResults :: ListItems -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListItems)
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListItems where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListItems where
  type Rs ListItems = ListItemsResponse
  request = Req.get mediaStoreDataService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListItemsResponse'
            Lude.<$> (x Lude..?> "Items" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListItems where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListItems where
  toPath = Lude.const "/"

instance Lude.ToQuery ListItems where
  toQuery ListItems' {..} =
    Lude.mconcat
      [ "Path" Lude.=: path,
        "NextToken" Lude.=: nextToken,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListItemsResponse' smart constructor.
data ListItemsResponse = ListItemsResponse'
  { items ::
      Lude.Maybe [Item],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListItemsResponse' with the minimum fields required to make a request.
--
-- * 'items' - The metadata entries for the folders and objects at the requested path.
-- * 'nextToken' - The token that can be used in a request to view the next set of results. For example, you submit a @ListItems@ request that matches 2,000 items with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value that can be used to fetch the next batch of results.
-- * 'responseStatus' - The response status code.
mkListItemsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListItemsResponse
mkListItemsResponse pResponseStatus_ =
  ListItemsResponse'
    { items = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The metadata entries for the folders and objects at the requested path.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsItems :: Lens.Lens' ListItemsResponse (Lude.Maybe [Item])
lirsItems = Lens.lens (items :: ListItemsResponse -> Lude.Maybe [Item]) (\s a -> s {items = a} :: ListItemsResponse)
{-# DEPRECATED lirsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The token that can be used in a request to view the next set of results. For example, you submit a @ListItems@ request that matches 2,000 items with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value that can be used to fetch the next batch of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListItemsResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListItemsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListItemsResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListItemsResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListItemsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListItemsResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

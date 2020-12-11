{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListBuildBatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the identifiers of your build batches in the current region.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildBatches
  ( -- * Creating a request
    ListBuildBatches (..),
    mkListBuildBatches,

    -- ** Request lenses
    lbbSortOrder,
    lbbNextToken,
    lbbFilter,
    lbbMaxResults,

    -- * Destructuring the response
    ListBuildBatchesResponse (..),
    mkListBuildBatchesResponse,

    -- ** Response lenses
    lbbrsIds,
    lbbrsNextToken,
    lbbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBuildBatches' smart constructor.
data ListBuildBatches = ListBuildBatches'
  { sortOrder ::
      Lude.Maybe SortOrderType,
    nextToken :: Lude.Maybe Lude.Text,
    filter :: Lude.Maybe BuildBatchFilter,
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

-- | Creates a value of 'ListBuildBatches' with the minimum fields required to make a request.
--
-- * 'filter' - A @BuildBatchFilter@ object that specifies the filters for the search.
-- * 'maxResults' - The maximum number of results to return.
-- * 'nextToken' - The @nextToken@ value returned from a previous call to @ListBuildBatches@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
-- * 'sortOrder' - Specifies the sort order of the returned items. Valid values include:
--
--
--     * @ASCENDING@ : List the batch build identifiers in ascending order by identifier.
--
--
--     * @DESCENDING@ : List the batch build identifiers in descending order by identifier.
mkListBuildBatches ::
  ListBuildBatches
mkListBuildBatches =
  ListBuildBatches'
    { sortOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Specifies the sort order of the returned items. Valid values include:
--
--
--     * @ASCENDING@ : List the batch build identifiers in ascending order by identifier.
--
--
--     * @DESCENDING@ : List the batch build identifiers in descending order by identifier.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbSortOrder :: Lens.Lens' ListBuildBatches (Lude.Maybe SortOrderType)
lbbSortOrder = Lens.lens (sortOrder :: ListBuildBatches -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListBuildBatches)
{-# DEPRECATED lbbSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The @nextToken@ value returned from a previous call to @ListBuildBatches@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbNextToken :: Lens.Lens' ListBuildBatches (Lude.Maybe Lude.Text)
lbbNextToken = Lens.lens (nextToken :: ListBuildBatches -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBuildBatches)
{-# DEPRECATED lbbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A @BuildBatchFilter@ object that specifies the filters for the search.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbFilter :: Lens.Lens' ListBuildBatches (Lude.Maybe BuildBatchFilter)
lbbFilter = Lens.lens (filter :: ListBuildBatches -> Lude.Maybe BuildBatchFilter) (\s a -> s {filter = a} :: ListBuildBatches)
{-# DEPRECATED lbbFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbMaxResults :: Lens.Lens' ListBuildBatches (Lude.Maybe Lude.Natural)
lbbMaxResults = Lens.lens (maxResults :: ListBuildBatches -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListBuildBatches)
{-# DEPRECATED lbbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListBuildBatches where
  page rq rs
    | Page.stop (rs Lens.^. lbbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbbrsIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbbNextToken Lens..~ rs Lens.^. lbbrsNextToken

instance Lude.AWSRequest ListBuildBatches where
  type Rs ListBuildBatches = ListBuildBatchesResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBuildBatchesResponse'
            Lude.<$> (x Lude..?> "ids" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBuildBatches where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.ListBuildBatches" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBuildBatches where
  toJSON ListBuildBatches' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListBuildBatches where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBuildBatches where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListBuildBatchesResponse' smart constructor.
data ListBuildBatchesResponse = ListBuildBatchesResponse'
  { ids ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListBuildBatchesResponse' with the minimum fields required to make a request.
--
-- * 'ids' - An array of strings that contains the batch build identifiers.
-- * 'nextToken' - If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatches@ to retrieve the next set of items.
-- * 'responseStatus' - The response status code.
mkListBuildBatchesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBuildBatchesResponse
mkListBuildBatchesResponse pResponseStatus_ =
  ListBuildBatchesResponse'
    { ids = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of strings that contains the batch build identifiers.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbrsIds :: Lens.Lens' ListBuildBatchesResponse (Lude.Maybe [Lude.Text])
lbbrsIds = Lens.lens (ids :: ListBuildBatchesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {ids = a} :: ListBuildBatchesResponse)
{-# DEPRECATED lbbrsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatches@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbrsNextToken :: Lens.Lens' ListBuildBatchesResponse (Lude.Maybe Lude.Text)
lbbrsNextToken = Lens.lens (nextToken :: ListBuildBatchesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBuildBatchesResponse)
{-# DEPRECATED lbbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbrsResponseStatus :: Lens.Lens' ListBuildBatchesResponse Lude.Int
lbbrsResponseStatus = Lens.lens (responseStatus :: ListBuildBatchesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBuildBatchesResponse)
{-# DEPRECATED lbbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

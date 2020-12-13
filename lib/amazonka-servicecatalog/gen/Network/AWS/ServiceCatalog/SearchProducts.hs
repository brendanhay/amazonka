{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.SearchProducts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the products to which the caller has access.
module Network.AWS.ServiceCatalog.SearchProducts
  ( -- * Creating a request
    SearchProducts (..),
    mkSearchProducts,

    -- ** Request lenses
    spFilters,
    spSortOrder,
    spAcceptLanguage,
    spPageToken,
    spPageSize,
    spSortBy,

    -- * Destructuring the response
    SearchProductsResponse (..),
    mkSearchProductsResponse,

    -- ** Response lenses
    sprsNextPageToken,
    sprsProductViewAggregations,
    sprsProductViewSummaries,
    sprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkSearchProducts' smart constructor.
data SearchProducts = SearchProducts'
  { -- | The search filters. If no search filters are specified, the output includes all products to which the caller has access.
    filters :: Lude.Maybe (Lude.HashMap ProductViewFilterBy ([Lude.Text])),
    -- | The sort order. If no value is specified, the results are not sorted.
    sortOrder :: Lude.Maybe SortOrder,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Lude.Maybe Lude.Natural,
    -- | The sort field. If no value is specified, the results are not sorted.
    sortBy :: Lude.Maybe ProductViewSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchProducts' with the minimum fields required to make a request.
--
-- * 'filters' - The search filters. If no search filters are specified, the output includes all products to which the caller has access.
-- * 'sortOrder' - The sort order. If no value is specified, the results are not sorted.
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'pageSize' - The maximum number of items to return with this call.
-- * 'sortBy' - The sort field. If no value is specified, the results are not sorted.
mkSearchProducts ::
  SearchProducts
mkSearchProducts =
  SearchProducts'
    { filters = Lude.Nothing,
      sortOrder = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | The search filters. If no search filters are specified, the output includes all products to which the caller has access.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spFilters :: Lens.Lens' SearchProducts (Lude.Maybe (Lude.HashMap ProductViewFilterBy ([Lude.Text])))
spFilters = Lens.lens (filters :: SearchProducts -> Lude.Maybe (Lude.HashMap ProductViewFilterBy ([Lude.Text]))) (\s a -> s {filters = a} :: SearchProducts)
{-# DEPRECATED spFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSortOrder :: Lens.Lens' SearchProducts (Lude.Maybe SortOrder)
spSortOrder = Lens.lens (sortOrder :: SearchProducts -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: SearchProducts)
{-# DEPRECATED spSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAcceptLanguage :: Lens.Lens' SearchProducts (Lude.Maybe Lude.Text)
spAcceptLanguage = Lens.lens (acceptLanguage :: SearchProducts -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: SearchProducts)
{-# DEPRECATED spAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPageToken :: Lens.Lens' SearchProducts (Lude.Maybe Lude.Text)
spPageToken = Lens.lens (pageToken :: SearchProducts -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: SearchProducts)
{-# DEPRECATED spPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPageSize :: Lens.Lens' SearchProducts (Lude.Maybe Lude.Natural)
spPageSize = Lens.lens (pageSize :: SearchProducts -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: SearchProducts)
{-# DEPRECATED spPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The sort field. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSortBy :: Lens.Lens' SearchProducts (Lude.Maybe ProductViewSortBy)
spSortBy = Lens.lens (sortBy :: SearchProducts -> Lude.Maybe ProductViewSortBy) (\s a -> s {sortBy = a} :: SearchProducts)
{-# DEPRECATED spSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Lude.AWSRequest SearchProducts where
  type Rs SearchProducts = SearchProductsResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchProductsResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "ProductViewAggregations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ProductViewSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchProducts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.SearchProducts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchProducts where
  toJSON SearchProducts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath SearchProducts where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchProducts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchProductsResponse' smart constructor.
data SearchProductsResponse = SearchProductsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The product view aggregations.
    productViewAggregations :: Lude.Maybe (Lude.HashMap Lude.Text ([ProductViewAggregationValue])),
    -- | Information about the product views.
    productViewSummaries :: Lude.Maybe [ProductViewSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchProductsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'productViewAggregations' - The product view aggregations.
-- * 'productViewSummaries' - Information about the product views.
-- * 'responseStatus' - The response status code.
mkSearchProductsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchProductsResponse
mkSearchProductsResponse pResponseStatus_ =
  SearchProductsResponse'
    { nextPageToken = Lude.Nothing,
      productViewAggregations = Lude.Nothing,
      productViewSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprsNextPageToken :: Lens.Lens' SearchProductsResponse (Lude.Maybe Lude.Text)
sprsNextPageToken = Lens.lens (nextPageToken :: SearchProductsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: SearchProductsResponse)
{-# DEPRECATED sprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The product view aggregations.
--
-- /Note:/ Consider using 'productViewAggregations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprsProductViewAggregations :: Lens.Lens' SearchProductsResponse (Lude.Maybe (Lude.HashMap Lude.Text ([ProductViewAggregationValue])))
sprsProductViewAggregations = Lens.lens (productViewAggregations :: SearchProductsResponse -> Lude.Maybe (Lude.HashMap Lude.Text ([ProductViewAggregationValue]))) (\s a -> s {productViewAggregations = a} :: SearchProductsResponse)
{-# DEPRECATED sprsProductViewAggregations "Use generic-lens or generic-optics with 'productViewAggregations' instead." #-}

-- | Information about the product views.
--
-- /Note:/ Consider using 'productViewSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprsProductViewSummaries :: Lens.Lens' SearchProductsResponse (Lude.Maybe [ProductViewSummary])
sprsProductViewSummaries = Lens.lens (productViewSummaries :: SearchProductsResponse -> Lude.Maybe [ProductViewSummary]) (\s a -> s {productViewSummaries = a} :: SearchProductsResponse)
{-# DEPRECATED sprsProductViewSummaries "Use generic-lens or generic-optics with 'productViewSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprsResponseStatus :: Lens.Lens' SearchProductsResponse Lude.Int
sprsResponseStatus = Lens.lens (responseStatus :: SearchProductsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchProductsResponse)
{-# DEPRECATED sprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

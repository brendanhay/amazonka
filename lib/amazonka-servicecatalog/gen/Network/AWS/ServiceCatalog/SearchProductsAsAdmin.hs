{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.SearchProductsAsAdmin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the products for the specified portfolio or all products.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.SearchProductsAsAdmin
  ( -- * Creating a request
    SearchProductsAsAdmin (..),
    mkSearchProductsAsAdmin,

    -- ** Request lenses
    spaaPortfolioId,
    spaaFilters,
    spaaSortOrder,
    spaaAcceptLanguage,
    spaaPageToken,
    spaaPageSize,
    spaaProductSource,
    spaaSortBy,

    -- * Destructuring the response
    SearchProductsAsAdminResponse (..),
    mkSearchProductsAsAdminResponse,

    -- ** Response lenses
    spaarsNextPageToken,
    spaarsProductViewDetails,
    spaarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkSearchProductsAsAdmin' smart constructor.
data SearchProductsAsAdmin = SearchProductsAsAdmin'
  { -- | The portfolio identifier.
    portfolioId :: Lude.Maybe Lude.Text,
    -- | The search filters. If no search filters are specified, the output includes all products to which the administrator has access.
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
    -- | Access level of the source of the product.
    productSource :: Lude.Maybe ProductSource,
    -- | The sort field. If no value is specified, the results are not sorted.
    sortBy :: Lude.Maybe ProductViewSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchProductsAsAdmin' with the minimum fields required to make a request.
--
-- * 'portfolioId' - The portfolio identifier.
-- * 'filters' - The search filters. If no search filters are specified, the output includes all products to which the administrator has access.
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
-- * 'productSource' - Access level of the source of the product.
-- * 'sortBy' - The sort field. If no value is specified, the results are not sorted.
mkSearchProductsAsAdmin ::
  SearchProductsAsAdmin
mkSearchProductsAsAdmin =
  SearchProductsAsAdmin'
    { portfolioId = Lude.Nothing,
      filters = Lude.Nothing,
      sortOrder = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing,
      productSource = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaPortfolioId :: Lens.Lens' SearchProductsAsAdmin (Lude.Maybe Lude.Text)
spaaPortfolioId = Lens.lens (portfolioId :: SearchProductsAsAdmin -> Lude.Maybe Lude.Text) (\s a -> s {portfolioId = a} :: SearchProductsAsAdmin)
{-# DEPRECATED spaaPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The search filters. If no search filters are specified, the output includes all products to which the administrator has access.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaFilters :: Lens.Lens' SearchProductsAsAdmin (Lude.Maybe (Lude.HashMap ProductViewFilterBy ([Lude.Text])))
spaaFilters = Lens.lens (filters :: SearchProductsAsAdmin -> Lude.Maybe (Lude.HashMap ProductViewFilterBy ([Lude.Text]))) (\s a -> s {filters = a} :: SearchProductsAsAdmin)
{-# DEPRECATED spaaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaSortOrder :: Lens.Lens' SearchProductsAsAdmin (Lude.Maybe SortOrder)
spaaSortOrder = Lens.lens (sortOrder :: SearchProductsAsAdmin -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: SearchProductsAsAdmin)
{-# DEPRECATED spaaSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

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
spaaAcceptLanguage :: Lens.Lens' SearchProductsAsAdmin (Lude.Maybe Lude.Text)
spaaAcceptLanguage = Lens.lens (acceptLanguage :: SearchProductsAsAdmin -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: SearchProductsAsAdmin)
{-# DEPRECATED spaaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaPageToken :: Lens.Lens' SearchProductsAsAdmin (Lude.Maybe Lude.Text)
spaaPageToken = Lens.lens (pageToken :: SearchProductsAsAdmin -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: SearchProductsAsAdmin)
{-# DEPRECATED spaaPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaPageSize :: Lens.Lens' SearchProductsAsAdmin (Lude.Maybe Lude.Natural)
spaaPageSize = Lens.lens (pageSize :: SearchProductsAsAdmin -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: SearchProductsAsAdmin)
{-# DEPRECATED spaaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | Access level of the source of the product.
--
-- /Note:/ Consider using 'productSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaProductSource :: Lens.Lens' SearchProductsAsAdmin (Lude.Maybe ProductSource)
spaaProductSource = Lens.lens (productSource :: SearchProductsAsAdmin -> Lude.Maybe ProductSource) (\s a -> s {productSource = a} :: SearchProductsAsAdmin)
{-# DEPRECATED spaaProductSource "Use generic-lens or generic-optics with 'productSource' instead." #-}

-- | The sort field. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaSortBy :: Lens.Lens' SearchProductsAsAdmin (Lude.Maybe ProductViewSortBy)
spaaSortBy = Lens.lens (sortBy :: SearchProductsAsAdmin -> Lude.Maybe ProductViewSortBy) (\s a -> s {sortBy = a} :: SearchProductsAsAdmin)
{-# DEPRECATED spaaSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager SearchProductsAsAdmin where
  page rq rs
    | Page.stop (rs Lens.^. spaarsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. spaarsProductViewDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& spaaPageToken Lens..~ rs Lens.^. spaarsNextPageToken

instance Lude.AWSRequest SearchProductsAsAdmin where
  type Rs SearchProductsAsAdmin = SearchProductsAsAdminResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchProductsAsAdminResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "ProductViewDetails" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchProductsAsAdmin where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.SearchProductsAsAdmin" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchProductsAsAdmin where
  toJSON SearchProductsAsAdmin' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PortfolioId" Lude..=) Lude.<$> portfolioId,
            ("Filters" Lude..=) Lude.<$> filters,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            ("ProductSource" Lude..=) Lude.<$> productSource,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath SearchProductsAsAdmin where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchProductsAsAdmin where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchProductsAsAdminResponse' smart constructor.
data SearchProductsAsAdminResponse = SearchProductsAsAdminResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Information about the product views.
    productViewDetails :: Lude.Maybe [ProductViewDetail],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchProductsAsAdminResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'productViewDetails' - Information about the product views.
-- * 'responseStatus' - The response status code.
mkSearchProductsAsAdminResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchProductsAsAdminResponse
mkSearchProductsAsAdminResponse pResponseStatus_ =
  SearchProductsAsAdminResponse'
    { nextPageToken = Lude.Nothing,
      productViewDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaarsNextPageToken :: Lens.Lens' SearchProductsAsAdminResponse (Lude.Maybe Lude.Text)
spaarsNextPageToken = Lens.lens (nextPageToken :: SearchProductsAsAdminResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: SearchProductsAsAdminResponse)
{-# DEPRECATED spaarsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the product views.
--
-- /Note:/ Consider using 'productViewDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaarsProductViewDetails :: Lens.Lens' SearchProductsAsAdminResponse (Lude.Maybe [ProductViewDetail])
spaarsProductViewDetails = Lens.lens (productViewDetails :: SearchProductsAsAdminResponse -> Lude.Maybe [ProductViewDetail]) (\s a -> s {productViewDetails = a} :: SearchProductsAsAdminResponse)
{-# DEPRECATED spaarsProductViewDetails "Use generic-lens or generic-optics with 'productViewDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaarsResponseStatus :: Lens.Lens' SearchProductsAsAdminResponse Lude.Int
spaarsResponseStatus = Lens.lens (responseStatus :: SearchProductsAsAdminResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchProductsAsAdminResponse)
{-# DEPRECATED spaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

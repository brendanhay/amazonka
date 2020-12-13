{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListPortfoliosForProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios that the specified product is associated with.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPortfoliosForProduct
  ( -- * Creating a request
    ListPortfoliosForProduct (..),
    mkListPortfoliosForProduct,

    -- ** Request lenses
    lpfpfAcceptLanguage,
    lpfpfPageToken,
    lpfpfPageSize,
    lpfpfProductId,

    -- * Destructuring the response
    ListPortfoliosForProductResponse (..),
    mkListPortfoliosForProductResponse,

    -- ** Response lenses
    lpfprsNextPageToken,
    lpfprsPortfolioDetails,
    lpfprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListPortfoliosForProduct' smart constructor.
data ListPortfoliosForProduct = ListPortfoliosForProduct'
  { -- | The language code.
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
    -- | The product identifier.
    productId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPortfoliosForProduct' with the minimum fields required to make a request.
--
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
-- * 'productId' - The product identifier.
mkListPortfoliosForProduct ::
  -- | 'productId'
  Lude.Text ->
  ListPortfoliosForProduct
mkListPortfoliosForProduct pProductId_ =
  ListPortfoliosForProduct'
    { acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing,
      productId = pProductId_
    }

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
lpfpfAcceptLanguage :: Lens.Lens' ListPortfoliosForProduct (Lude.Maybe Lude.Text)
lpfpfAcceptLanguage = Lens.lens (acceptLanguage :: ListPortfoliosForProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListPortfoliosForProduct)
{-# DEPRECATED lpfpfAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpfPageToken :: Lens.Lens' ListPortfoliosForProduct (Lude.Maybe Lude.Text)
lpfpfPageToken = Lens.lens (pageToken :: ListPortfoliosForProduct -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListPortfoliosForProduct)
{-# DEPRECATED lpfpfPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpfPageSize :: Lens.Lens' ListPortfoliosForProduct (Lude.Maybe Lude.Natural)
lpfpfPageSize = Lens.lens (pageSize :: ListPortfoliosForProduct -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListPortfoliosForProduct)
{-# DEPRECATED lpfpfPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpfProductId :: Lens.Lens' ListPortfoliosForProduct Lude.Text
lpfpfProductId = Lens.lens (productId :: ListPortfoliosForProduct -> Lude.Text) (\s a -> s {productId = a} :: ListPortfoliosForProduct)
{-# DEPRECATED lpfpfProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Page.AWSPager ListPortfoliosForProduct where
  page rq rs
    | Page.stop (rs Lens.^. lpfprsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpfprsPortfolioDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpfpfPageToken Lens..~ rs Lens.^. lpfprsNextPageToken

instance Lude.AWSRequest ListPortfoliosForProduct where
  type Rs ListPortfoliosForProduct = ListPortfoliosForProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPortfoliosForProductResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "PortfolioDetails" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPortfoliosForProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListPortfoliosForProduct" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPortfoliosForProduct where
  toJSON ListPortfoliosForProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            Lude.Just ("ProductId" Lude..= productId)
          ]
      )

instance Lude.ToPath ListPortfoliosForProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPortfoliosForProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPortfoliosForProductResponse' smart constructor.
data ListPortfoliosForProductResponse = ListPortfoliosForProductResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Information about the portfolios.
    portfolioDetails :: Lude.Maybe [PortfolioDetail],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPortfoliosForProductResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'portfolioDetails' - Information about the portfolios.
-- * 'responseStatus' - The response status code.
mkListPortfoliosForProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPortfoliosForProductResponse
mkListPortfoliosForProductResponse pResponseStatus_ =
  ListPortfoliosForProductResponse'
    { nextPageToken = Lude.Nothing,
      portfolioDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprsNextPageToken :: Lens.Lens' ListPortfoliosForProductResponse (Lude.Maybe Lude.Text)
lpfprsNextPageToken = Lens.lens (nextPageToken :: ListPortfoliosForProductResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListPortfoliosForProductResponse)
{-# DEPRECATED lpfprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the portfolios.
--
-- /Note:/ Consider using 'portfolioDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprsPortfolioDetails :: Lens.Lens' ListPortfoliosForProductResponse (Lude.Maybe [PortfolioDetail])
lpfprsPortfolioDetails = Lens.lens (portfolioDetails :: ListPortfoliosForProductResponse -> Lude.Maybe [PortfolioDetail]) (\s a -> s {portfolioDetails = a} :: ListPortfoliosForProductResponse)
{-# DEPRECATED lpfprsPortfolioDetails "Use generic-lens or generic-optics with 'portfolioDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprsResponseStatus :: Lens.Lens' ListPortfoliosForProductResponse Lude.Int
lpfprsResponseStatus = Lens.lens (responseStatus :: ListPortfoliosForProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPortfoliosForProductResponse)
{-# DEPRECATED lpfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

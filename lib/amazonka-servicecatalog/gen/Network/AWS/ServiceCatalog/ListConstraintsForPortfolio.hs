{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the constraints for the specified portfolio and product.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
  ( -- * Creating a request
    ListConstraintsForPortfolio (..),
    mkListConstraintsForPortfolio,

    -- ** Request lenses
    lcfpAcceptLanguage,
    lcfpPageToken,
    lcfpPageSize,
    lcfpProductId,
    lcfpPortfolioId,

    -- * Destructuring the response
    ListConstraintsForPortfolioResponse (..),
    mkListConstraintsForPortfolioResponse,

    -- ** Response lenses
    lcfprsNextPageToken,
    lcfprsConstraintDetails,
    lcfprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListConstraintsForPortfolio' smart constructor.
data ListConstraintsForPortfolio = ListConstraintsForPortfolio'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    pageToken :: Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Natural,
    productId :: Lude.Maybe Lude.Text,
    portfolioId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConstraintsForPortfolio' with the minimum fields required to make a request.
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
-- * 'pageSize' - The maximum number of items to return with this call.
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'portfolioId' - The portfolio identifier.
-- * 'productId' - The product identifier.
mkListConstraintsForPortfolio ::
  -- | 'portfolioId'
  Lude.Text ->
  ListConstraintsForPortfolio
mkListConstraintsForPortfolio pPortfolioId_ =
  ListConstraintsForPortfolio'
    { acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing,
      productId = Lude.Nothing,
      portfolioId = pPortfolioId_
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
lcfpAcceptLanguage :: Lens.Lens' ListConstraintsForPortfolio (Lude.Maybe Lude.Text)
lcfpAcceptLanguage = Lens.lens (acceptLanguage :: ListConstraintsForPortfolio -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListConstraintsForPortfolio)
{-# DEPRECATED lcfpAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfpPageToken :: Lens.Lens' ListConstraintsForPortfolio (Lude.Maybe Lude.Text)
lcfpPageToken = Lens.lens (pageToken :: ListConstraintsForPortfolio -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListConstraintsForPortfolio)
{-# DEPRECATED lcfpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfpPageSize :: Lens.Lens' ListConstraintsForPortfolio (Lude.Maybe Lude.Natural)
lcfpPageSize = Lens.lens (pageSize :: ListConstraintsForPortfolio -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListConstraintsForPortfolio)
{-# DEPRECATED lcfpPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfpProductId :: Lens.Lens' ListConstraintsForPortfolio (Lude.Maybe Lude.Text)
lcfpProductId = Lens.lens (productId :: ListConstraintsForPortfolio -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: ListConstraintsForPortfolio)
{-# DEPRECATED lcfpProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfpPortfolioId :: Lens.Lens' ListConstraintsForPortfolio Lude.Text
lcfpPortfolioId = Lens.lens (portfolioId :: ListConstraintsForPortfolio -> Lude.Text) (\s a -> s {portfolioId = a} :: ListConstraintsForPortfolio)
{-# DEPRECATED lcfpPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

instance Page.AWSPager ListConstraintsForPortfolio where
  page rq rs
    | Page.stop (rs Lens.^. lcfprsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcfprsConstraintDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcfpPageToken Lens..~ rs Lens.^. lcfprsNextPageToken

instance Lude.AWSRequest ListConstraintsForPortfolio where
  type
    Rs ListConstraintsForPortfolio =
      ListConstraintsForPortfolioResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListConstraintsForPortfolioResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "ConstraintDetails" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListConstraintsForPortfolio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListConstraintsForPortfolio" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListConstraintsForPortfolio where
  toJSON ListConstraintsForPortfolio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            ("ProductId" Lude..=) Lude.<$> productId,
            Lude.Just ("PortfolioId" Lude..= portfolioId)
          ]
      )

instance Lude.ToPath ListConstraintsForPortfolio where
  toPath = Lude.const "/"

instance Lude.ToQuery ListConstraintsForPortfolio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListConstraintsForPortfolioResponse' smart constructor.
data ListConstraintsForPortfolioResponse = ListConstraintsForPortfolioResponse'
  { nextPageToken ::
      Lude.Maybe
        Lude.Text,
    constraintDetails ::
      Lude.Maybe
        [ConstraintDetail],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConstraintsForPortfolioResponse' with the minimum fields required to make a request.
--
-- * 'constraintDetails' - Information about the constraints.
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'responseStatus' - The response status code.
mkListConstraintsForPortfolioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListConstraintsForPortfolioResponse
mkListConstraintsForPortfolioResponse pResponseStatus_ =
  ListConstraintsForPortfolioResponse'
    { nextPageToken =
        Lude.Nothing,
      constraintDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfprsNextPageToken :: Lens.Lens' ListConstraintsForPortfolioResponse (Lude.Maybe Lude.Text)
lcfprsNextPageToken = Lens.lens (nextPageToken :: ListConstraintsForPortfolioResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListConstraintsForPortfolioResponse)
{-# DEPRECATED lcfprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the constraints.
--
-- /Note:/ Consider using 'constraintDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfprsConstraintDetails :: Lens.Lens' ListConstraintsForPortfolioResponse (Lude.Maybe [ConstraintDetail])
lcfprsConstraintDetails = Lens.lens (constraintDetails :: ListConstraintsForPortfolioResponse -> Lude.Maybe [ConstraintDetail]) (\s a -> s {constraintDetails = a} :: ListConstraintsForPortfolioResponse)
{-# DEPRECATED lcfprsConstraintDetails "Use generic-lens or generic-optics with 'constraintDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfprsResponseStatus :: Lens.Lens' ListConstraintsForPortfolioResponse Lude.Int
lcfprsResponseStatus = Lens.lens (responseStatus :: ListConstraintsForPortfolioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListConstraintsForPortfolioResponse)
{-# DEPRECATED lcfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all principal ARNs associated with the specified portfolio.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
  ( -- * Creating a request
    ListPrincipalsForPortfolio (..),
    mkListPrincipalsForPortfolio,

    -- ** Request lenses
    lpfpPortfolioId,
    lpfpAcceptLanguage,
    lpfpPageToken,
    lpfpPageSize,

    -- * Destructuring the response
    ListPrincipalsForPortfolioResponse (..),
    mkListPrincipalsForPortfolioResponse,

    -- ** Response lenses
    lrsNextPageToken,
    lrsPrincipals,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListPrincipalsForPortfolio' smart constructor.
data ListPrincipalsForPortfolio = ListPrincipalsForPortfolio'
  { -- | The portfolio identifier.
    portfolioId :: Lude.Text,
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
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPrincipalsForPortfolio' with the minimum fields required to make a request.
--
-- * 'portfolioId' - The portfolio identifier.
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
mkListPrincipalsForPortfolio ::
  -- | 'portfolioId'
  Lude.Text ->
  ListPrincipalsForPortfolio
mkListPrincipalsForPortfolio pPortfolioId_ =
  ListPrincipalsForPortfolio'
    { portfolioId = pPortfolioId_,
      acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpPortfolioId :: Lens.Lens' ListPrincipalsForPortfolio Lude.Text
lpfpPortfolioId = Lens.lens (portfolioId :: ListPrincipalsForPortfolio -> Lude.Text) (\s a -> s {portfolioId = a} :: ListPrincipalsForPortfolio)
{-# DEPRECATED lpfpPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

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
lpfpAcceptLanguage :: Lens.Lens' ListPrincipalsForPortfolio (Lude.Maybe Lude.Text)
lpfpAcceptLanguage = Lens.lens (acceptLanguage :: ListPrincipalsForPortfolio -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListPrincipalsForPortfolio)
{-# DEPRECATED lpfpAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpPageToken :: Lens.Lens' ListPrincipalsForPortfolio (Lude.Maybe Lude.Text)
lpfpPageToken = Lens.lens (pageToken :: ListPrincipalsForPortfolio -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListPrincipalsForPortfolio)
{-# DEPRECATED lpfpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpPageSize :: Lens.Lens' ListPrincipalsForPortfolio (Lude.Maybe Lude.Natural)
lpfpPageSize = Lens.lens (pageSize :: ListPrincipalsForPortfolio -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListPrincipalsForPortfolio)
{-# DEPRECATED lpfpPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListPrincipalsForPortfolio where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsPrincipals) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpfpPageToken Lens..~ rs Lens.^. lrsNextPageToken

instance Lude.AWSRequest ListPrincipalsForPortfolio where
  type
    Rs ListPrincipalsForPortfolio =
      ListPrincipalsForPortfolioResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPrincipalsForPortfolioResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "Principals" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPrincipalsForPortfolio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListPrincipalsForPortfolio" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPrincipalsForPortfolio where
  toJSON ListPrincipalsForPortfolio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PortfolioId" Lude..= portfolioId),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath ListPrincipalsForPortfolio where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPrincipalsForPortfolio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPrincipalsForPortfolioResponse' smart constructor.
data ListPrincipalsForPortfolioResponse = ListPrincipalsForPortfolioResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The IAM principals (users or roles) associated with the portfolio.
    principals :: Lude.Maybe [Principal],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPrincipalsForPortfolioResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'principals' - The IAM principals (users or roles) associated with the portfolio.
-- * 'responseStatus' - The response status code.
mkListPrincipalsForPortfolioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPrincipalsForPortfolioResponse
mkListPrincipalsForPortfolioResponse pResponseStatus_ =
  ListPrincipalsForPortfolioResponse'
    { nextPageToken = Lude.Nothing,
      principals = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextPageToken :: Lens.Lens' ListPrincipalsForPortfolioResponse (Lude.Maybe Lude.Text)
lrsNextPageToken = Lens.lens (nextPageToken :: ListPrincipalsForPortfolioResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListPrincipalsForPortfolioResponse)
{-# DEPRECATED lrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The IAM principals (users or roles) associated with the portfolio.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsPrincipals :: Lens.Lens' ListPrincipalsForPortfolioResponse (Lude.Maybe [Principal])
lrsPrincipals = Lens.lens (principals :: ListPrincipalsForPortfolioResponse -> Lude.Maybe [Principal]) (\s a -> s {principals = a} :: ListPrincipalsForPortfolioResponse)
{-# DEPRECATED lrsPrincipals "Use generic-lens or generic-optics with 'principals' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListPrincipalsForPortfolioResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListPrincipalsForPortfolioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPrincipalsForPortfolioResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios for which sharing was accepted by this account.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
  ( -- * Creating a request
    ListAcceptedPortfolioShares (..),
    mkListAcceptedPortfolioShares,

    -- ** Request lenses
    lapsPortfolioShareType,
    lapsAcceptLanguage,
    lapsPageToken,
    lapsPageSize,

    -- * Destructuring the response
    ListAcceptedPortfolioSharesResponse (..),
    mkListAcceptedPortfolioSharesResponse,

    -- ** Response lenses
    lapsrsNextPageToken,
    lapsrsPortfolioDetails,
    lapsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListAcceptedPortfolioShares' smart constructor.
data ListAcceptedPortfolioShares = ListAcceptedPortfolioShares'
  { portfolioShareType ::
      Lude.Maybe PortfolioShareType,
    acceptLanguage ::
      Lude.Maybe Lude.Text,
    pageToken :: Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAcceptedPortfolioShares' with the minimum fields required to make a request.
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
-- * 'portfolioShareType' - The type of shared portfolios to list. The default is to list imported portfolios.
--
--
--     * @AWS_ORGANIZATIONS@ - List portfolios shared by the management account of your organization
--
--
--     * @AWS_SERVICECATALOG@ - List default portfolios
--
--
--     * @IMPORTED@ - List imported portfolios
mkListAcceptedPortfolioShares ::
  ListAcceptedPortfolioShares
mkListAcceptedPortfolioShares =
  ListAcceptedPortfolioShares'
    { portfolioShareType = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The type of shared portfolios to list. The default is to list imported portfolios.
--
--
--     * @AWS_ORGANIZATIONS@ - List portfolios shared by the management account of your organization
--
--
--     * @AWS_SERVICECATALOG@ - List default portfolios
--
--
--     * @IMPORTED@ - List imported portfolios
--
--
--
-- /Note:/ Consider using 'portfolioShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsPortfolioShareType :: Lens.Lens' ListAcceptedPortfolioShares (Lude.Maybe PortfolioShareType)
lapsPortfolioShareType = Lens.lens (portfolioShareType :: ListAcceptedPortfolioShares -> Lude.Maybe PortfolioShareType) (\s a -> s {portfolioShareType = a} :: ListAcceptedPortfolioShares)
{-# DEPRECATED lapsPortfolioShareType "Use generic-lens or generic-optics with 'portfolioShareType' instead." #-}

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
lapsAcceptLanguage :: Lens.Lens' ListAcceptedPortfolioShares (Lude.Maybe Lude.Text)
lapsAcceptLanguage = Lens.lens (acceptLanguage :: ListAcceptedPortfolioShares -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListAcceptedPortfolioShares)
{-# DEPRECATED lapsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsPageToken :: Lens.Lens' ListAcceptedPortfolioShares (Lude.Maybe Lude.Text)
lapsPageToken = Lens.lens (pageToken :: ListAcceptedPortfolioShares -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListAcceptedPortfolioShares)
{-# DEPRECATED lapsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsPageSize :: Lens.Lens' ListAcceptedPortfolioShares (Lude.Maybe Lude.Natural)
lapsPageSize = Lens.lens (pageSize :: ListAcceptedPortfolioShares -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListAcceptedPortfolioShares)
{-# DEPRECATED lapsPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListAcceptedPortfolioShares where
  page rq rs
    | Page.stop (rs Lens.^. lapsrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lapsrsPortfolioDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lapsPageToken Lens..~ rs Lens.^. lapsrsNextPageToken

instance Lude.AWSRequest ListAcceptedPortfolioShares where
  type
    Rs ListAcceptedPortfolioShares =
      ListAcceptedPortfolioSharesResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAcceptedPortfolioSharesResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "PortfolioDetails" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAcceptedPortfolioShares where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListAcceptedPortfolioShares" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAcceptedPortfolioShares where
  toJSON ListAcceptedPortfolioShares' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PortfolioShareType" Lude..=) Lude.<$> portfolioShareType,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath ListAcceptedPortfolioShares where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAcceptedPortfolioShares where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAcceptedPortfolioSharesResponse' smart constructor.
data ListAcceptedPortfolioSharesResponse = ListAcceptedPortfolioSharesResponse'
  { nextPageToken ::
      Lude.Maybe
        Lude.Text,
    portfolioDetails ::
      Lude.Maybe
        [PortfolioDetail],
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

-- | Creates a value of 'ListAcceptedPortfolioSharesResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'portfolioDetails' - Information about the portfolios.
-- * 'responseStatus' - The response status code.
mkListAcceptedPortfolioSharesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAcceptedPortfolioSharesResponse
mkListAcceptedPortfolioSharesResponse pResponseStatus_ =
  ListAcceptedPortfolioSharesResponse'
    { nextPageToken =
        Lude.Nothing,
      portfolioDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsrsNextPageToken :: Lens.Lens' ListAcceptedPortfolioSharesResponse (Lude.Maybe Lude.Text)
lapsrsNextPageToken = Lens.lens (nextPageToken :: ListAcceptedPortfolioSharesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListAcceptedPortfolioSharesResponse)
{-# DEPRECATED lapsrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the portfolios.
--
-- /Note:/ Consider using 'portfolioDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsrsPortfolioDetails :: Lens.Lens' ListAcceptedPortfolioSharesResponse (Lude.Maybe [PortfolioDetail])
lapsrsPortfolioDetails = Lens.lens (portfolioDetails :: ListAcceptedPortfolioSharesResponse -> Lude.Maybe [PortfolioDetail]) (\s a -> s {portfolioDetails = a} :: ListAcceptedPortfolioSharesResponse)
{-# DEPRECATED lapsrsPortfolioDetails "Use generic-lens or generic-optics with 'portfolioDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsrsResponseStatus :: Lens.Lens' ListAcceptedPortfolioSharesResponse Lude.Int
lapsrsResponseStatus = Lens.lens (responseStatus :: ListAcceptedPortfolioSharesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAcceptedPortfolioSharesResponse)
{-# DEPRECATED lapsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

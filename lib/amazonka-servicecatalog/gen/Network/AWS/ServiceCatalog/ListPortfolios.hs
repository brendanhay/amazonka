{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListPortfolios
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios in the catalog.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPortfolios
  ( -- * Creating a request
    ListPortfolios (..),
    mkListPortfolios,

    -- ** Request lenses
    lpAcceptLanguage,
    lpPageToken,
    lpPageSize,

    -- * Destructuring the response
    ListPortfoliosResponse (..),
    mkListPortfoliosResponse,

    -- ** Response lenses
    lprsNextPageToken,
    lprsPortfolioDetails,
    lprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListPortfolios' smart constructor.
data ListPortfolios = ListPortfolios'
  { acceptLanguage ::
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

-- | Creates a value of 'ListPortfolios' with the minimum fields required to make a request.
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
mkListPortfolios ::
  ListPortfolios
mkListPortfolios =
  ListPortfolios'
    { acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing
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
lpAcceptLanguage :: Lens.Lens' ListPortfolios (Lude.Maybe Lude.Text)
lpAcceptLanguage = Lens.lens (acceptLanguage :: ListPortfolios -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListPortfolios)
{-# DEPRECATED lpAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPageToken :: Lens.Lens' ListPortfolios (Lude.Maybe Lude.Text)
lpPageToken = Lens.lens (pageToken :: ListPortfolios -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListPortfolios)
{-# DEPRECATED lpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPageSize :: Lens.Lens' ListPortfolios (Lude.Maybe Lude.Natural)
lpPageSize = Lens.lens (pageSize :: ListPortfolios -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListPortfolios)
{-# DEPRECATED lpPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListPortfolios where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPortfolioDetails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpPageToken Lens..~ rs Lens.^. lprsNextPageToken

instance Lude.AWSRequest ListPortfolios where
  type Rs ListPortfolios = ListPortfoliosResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPortfoliosResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "PortfolioDetails" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPortfolios where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.ListPortfolios" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPortfolios where
  toJSON ListPortfolios' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath ListPortfolios where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPortfolios where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPortfoliosResponse' smart constructor.
data ListPortfoliosResponse = ListPortfoliosResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    portfolioDetails ::
      Lude.Maybe [PortfolioDetail],
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

-- | Creates a value of 'ListPortfoliosResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'portfolioDetails' - Information about the portfolios.
-- * 'responseStatus' - The response status code.
mkListPortfoliosResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPortfoliosResponse
mkListPortfoliosResponse pResponseStatus_ =
  ListPortfoliosResponse'
    { nextPageToken = Lude.Nothing,
      portfolioDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextPageToken :: Lens.Lens' ListPortfoliosResponse (Lude.Maybe Lude.Text)
lprsNextPageToken = Lens.lens (nextPageToken :: ListPortfoliosResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListPortfoliosResponse)
{-# DEPRECATED lprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the portfolios.
--
-- /Note:/ Consider using 'portfolioDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPortfolioDetails :: Lens.Lens' ListPortfoliosResponse (Lude.Maybe [PortfolioDetail])
lprsPortfolioDetails = Lens.lens (portfolioDetails :: ListPortfoliosResponse -> Lude.Maybe [PortfolioDetail]) (\s a -> s {portfolioDetails = a} :: ListPortfoliosResponse)
{-# DEPRECATED lprsPortfolioDetails "Use generic-lens or generic-optics with 'portfolioDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPortfoliosResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPortfoliosResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPortfoliosResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

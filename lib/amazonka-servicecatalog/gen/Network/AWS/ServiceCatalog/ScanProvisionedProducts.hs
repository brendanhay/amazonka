{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ScanProvisionedProducts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the provisioned products that are available (not terminated).
--
-- To use additional filtering, see 'SearchProvisionedProducts' .
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ScanProvisionedProducts
  ( -- * Creating a request
    ScanProvisionedProducts (..),
    mkScanProvisionedProducts,

    -- ** Request lenses
    sAcceptLanguage,
    sAccessLevelFilter,
    sPageToken,
    sPageSize,

    -- * Destructuring the response
    ScanProvisionedProductsResponse (..),
    mkScanProvisionedProductsResponse,

    -- ** Response lenses
    spprsNextPageToken,
    spprsProvisionedProducts,
    spprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkScanProvisionedProducts' smart constructor.
data ScanProvisionedProducts = ScanProvisionedProducts'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    accessLevelFilter ::
      Lude.Maybe AccessLevelFilter,
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

-- | Creates a value of 'ScanProvisionedProducts' with the minimum fields required to make a request.
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
-- * 'accessLevelFilter' - The access level to use to obtain results. The default is @User@ .
-- * 'pageSize' - The maximum number of items to return with this call.
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
mkScanProvisionedProducts ::
  ScanProvisionedProducts
mkScanProvisionedProducts =
  ScanProvisionedProducts'
    { acceptLanguage = Lude.Nothing,
      accessLevelFilter = Lude.Nothing,
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
sAcceptLanguage :: Lens.Lens' ScanProvisionedProducts (Lude.Maybe Lude.Text)
sAcceptLanguage = Lens.lens (acceptLanguage :: ScanProvisionedProducts -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ScanProvisionedProducts)
{-# DEPRECATED sAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The access level to use to obtain results. The default is @User@ .
--
-- /Note:/ Consider using 'accessLevelFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccessLevelFilter :: Lens.Lens' ScanProvisionedProducts (Lude.Maybe AccessLevelFilter)
sAccessLevelFilter = Lens.lens (accessLevelFilter :: ScanProvisionedProducts -> Lude.Maybe AccessLevelFilter) (\s a -> s {accessLevelFilter = a} :: ScanProvisionedProducts)
{-# DEPRECATED sAccessLevelFilter "Use generic-lens or generic-optics with 'accessLevelFilter' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPageToken :: Lens.Lens' ScanProvisionedProducts (Lude.Maybe Lude.Text)
sPageToken = Lens.lens (pageToken :: ScanProvisionedProducts -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ScanProvisionedProducts)
{-# DEPRECATED sPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPageSize :: Lens.Lens' ScanProvisionedProducts (Lude.Maybe Lude.Natural)
sPageSize = Lens.lens (pageSize :: ScanProvisionedProducts -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ScanProvisionedProducts)
{-# DEPRECATED sPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ScanProvisionedProducts where
  page rq rs
    | Page.stop (rs Lens.^. spprsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. spprsProvisionedProducts) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& sPageToken Lens..~ rs Lens.^. spprsNextPageToken

instance Lude.AWSRequest ScanProvisionedProducts where
  type Rs ScanProvisionedProducts = ScanProvisionedProductsResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ScanProvisionedProductsResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "ProvisionedProducts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ScanProvisionedProducts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ScanProvisionedProducts" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ScanProvisionedProducts where
  toJSON ScanProvisionedProducts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("AccessLevelFilter" Lude..=) Lude.<$> accessLevelFilter,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath ScanProvisionedProducts where
  toPath = Lude.const "/"

instance Lude.ToQuery ScanProvisionedProducts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkScanProvisionedProductsResponse' smart constructor.
data ScanProvisionedProductsResponse = ScanProvisionedProductsResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    provisionedProducts ::
      Lude.Maybe
        [ProvisionedProductDetail],
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

-- | Creates a value of 'ScanProvisionedProductsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'provisionedProducts' - Information about the provisioned products.
-- * 'responseStatus' - The response status code.
mkScanProvisionedProductsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ScanProvisionedProductsResponse
mkScanProvisionedProductsResponse pResponseStatus_ =
  ScanProvisionedProductsResponse'
    { nextPageToken = Lude.Nothing,
      provisionedProducts = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsNextPageToken :: Lens.Lens' ScanProvisionedProductsResponse (Lude.Maybe Lude.Text)
spprsNextPageToken = Lens.lens (nextPageToken :: ScanProvisionedProductsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ScanProvisionedProductsResponse)
{-# DEPRECATED spprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the provisioned products.
--
-- /Note:/ Consider using 'provisionedProducts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsProvisionedProducts :: Lens.Lens' ScanProvisionedProductsResponse (Lude.Maybe [ProvisionedProductDetail])
spprsProvisionedProducts = Lens.lens (provisionedProducts :: ScanProvisionedProductsResponse -> Lude.Maybe [ProvisionedProductDetail]) (\s a -> s {provisionedProducts = a} :: ScanProvisionedProductsResponse)
{-# DEPRECATED spprsProvisionedProducts "Use generic-lens or generic-optics with 'provisionedProducts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprsResponseStatus :: Lens.Lens' ScanProvisionedProductsResponse Lude.Int
spprsResponseStatus = Lens.lens (responseStatus :: ScanProvisionedProductsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ScanProvisionedProductsResponse)
{-# DEPRECATED spprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

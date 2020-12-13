{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListLaunchPaths
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the paths to the specified product. A path is how the user has access to a specified product, and is necessary when provisioning a product. A path also determines the constraints put on the product.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListLaunchPaths
  ( -- * Creating a request
    ListLaunchPaths (..),
    mkListLaunchPaths,

    -- ** Request lenses
    llpAcceptLanguage,
    llpPageToken,
    llpPageSize,
    llpProductId,

    -- * Destructuring the response
    ListLaunchPathsResponse (..),
    mkListLaunchPathsResponse,

    -- ** Response lenses
    llprsNextPageToken,
    llprsLaunchPathSummaries,
    llprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListLaunchPaths' smart constructor.
data ListLaunchPaths = ListLaunchPaths'
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

-- | Creates a value of 'ListLaunchPaths' with the minimum fields required to make a request.
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
mkListLaunchPaths ::
  -- | 'productId'
  Lude.Text ->
  ListLaunchPaths
mkListLaunchPaths pProductId_ =
  ListLaunchPaths'
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
llpAcceptLanguage :: Lens.Lens' ListLaunchPaths (Lude.Maybe Lude.Text)
llpAcceptLanguage = Lens.lens (acceptLanguage :: ListLaunchPaths -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListLaunchPaths)
{-# DEPRECATED llpAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llpPageToken :: Lens.Lens' ListLaunchPaths (Lude.Maybe Lude.Text)
llpPageToken = Lens.lens (pageToken :: ListLaunchPaths -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListLaunchPaths)
{-# DEPRECATED llpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llpPageSize :: Lens.Lens' ListLaunchPaths (Lude.Maybe Lude.Natural)
llpPageSize = Lens.lens (pageSize :: ListLaunchPaths -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListLaunchPaths)
{-# DEPRECATED llpPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llpProductId :: Lens.Lens' ListLaunchPaths Lude.Text
llpProductId = Lens.lens (productId :: ListLaunchPaths -> Lude.Text) (\s a -> s {productId = a} :: ListLaunchPaths)
{-# DEPRECATED llpProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Page.AWSPager ListLaunchPaths where
  page rq rs
    | Page.stop (rs Lens.^. llprsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. llprsLaunchPathSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& llpPageToken Lens..~ rs Lens.^. llprsNextPageToken

instance Lude.AWSRequest ListLaunchPaths where
  type Rs ListLaunchPaths = ListLaunchPathsResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLaunchPathsResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "LaunchPathSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLaunchPaths where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.ListLaunchPaths" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListLaunchPaths where
  toJSON ListLaunchPaths' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            Lude.Just ("ProductId" Lude..= productId)
          ]
      )

instance Lude.ToPath ListLaunchPaths where
  toPath = Lude.const "/"

instance Lude.ToQuery ListLaunchPaths where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListLaunchPathsResponse' smart constructor.
data ListLaunchPathsResponse = ListLaunchPathsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Information about the launch path.
    launchPathSummaries :: Lude.Maybe [LaunchPathSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLaunchPathsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'launchPathSummaries' - Information about the launch path.
-- * 'responseStatus' - The response status code.
mkListLaunchPathsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLaunchPathsResponse
mkListLaunchPathsResponse pResponseStatus_ =
  ListLaunchPathsResponse'
    { nextPageToken = Lude.Nothing,
      launchPathSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llprsNextPageToken :: Lens.Lens' ListLaunchPathsResponse (Lude.Maybe Lude.Text)
llprsNextPageToken = Lens.lens (nextPageToken :: ListLaunchPathsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListLaunchPathsResponse)
{-# DEPRECATED llprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the launch path.
--
-- /Note:/ Consider using 'launchPathSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llprsLaunchPathSummaries :: Lens.Lens' ListLaunchPathsResponse (Lude.Maybe [LaunchPathSummary])
llprsLaunchPathSummaries = Lens.lens (launchPathSummaries :: ListLaunchPathsResponse -> Lude.Maybe [LaunchPathSummary]) (\s a -> s {launchPathSummaries = a} :: ListLaunchPathsResponse)
{-# DEPRECATED llprsLaunchPathSummaries "Use generic-lens or generic-optics with 'launchPathSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llprsResponseStatus :: Lens.Lens' ListLaunchPathsResponse Lude.Int
llprsResponseStatus = Lens.lens (responseStatus :: ListLaunchPathsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLaunchPathsResponse)
{-# DEPRECATED llprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListProvisionedProductPlans
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the plans for the specified provisioned product or all plans to which the user has access.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListProvisionedProductPlans
  ( -- * Creating a request
    ListProvisionedProductPlans (..),
    mkListProvisionedProductPlans,

    -- ** Request lenses
    lpppProvisionProductId,
    lpppAcceptLanguage,
    lpppAccessLevelFilter,
    lpppPageToken,
    lpppPageSize,

    -- * Destructuring the response
    ListProvisionedProductPlansResponse (..),
    mkListProvisionedProductPlansResponse,

    -- ** Response lenses
    lppprsNextPageToken,
    lppprsProvisionedProductPlans,
    lppprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkListProvisionedProductPlans' smart constructor.
data ListProvisionedProductPlans = ListProvisionedProductPlans'
  { -- | The product identifier.
    provisionProductId :: Lude.Maybe Lude.Text,
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
    -- | The access level to use to obtain results. The default is @User@ .
    accessLevelFilter :: Lude.Maybe AccessLevelFilter,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProvisionedProductPlans' with the minimum fields required to make a request.
--
-- * 'provisionProductId' - The product identifier.
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
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'pageSize' - The maximum number of items to return with this call.
mkListProvisionedProductPlans ::
  ListProvisionedProductPlans
mkListProvisionedProductPlans =
  ListProvisionedProductPlans'
    { provisionProductId = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      accessLevelFilter = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The product identifier.
--
-- /Note:/ Consider using 'provisionProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpppProvisionProductId :: Lens.Lens' ListProvisionedProductPlans (Lude.Maybe Lude.Text)
lpppProvisionProductId = Lens.lens (provisionProductId :: ListProvisionedProductPlans -> Lude.Maybe Lude.Text) (\s a -> s {provisionProductId = a} :: ListProvisionedProductPlans)
{-# DEPRECATED lpppProvisionProductId "Use generic-lens or generic-optics with 'provisionProductId' instead." #-}

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
lpppAcceptLanguage :: Lens.Lens' ListProvisionedProductPlans (Lude.Maybe Lude.Text)
lpppAcceptLanguage = Lens.lens (acceptLanguage :: ListProvisionedProductPlans -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ListProvisionedProductPlans)
{-# DEPRECATED lpppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The access level to use to obtain results. The default is @User@ .
--
-- /Note:/ Consider using 'accessLevelFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpppAccessLevelFilter :: Lens.Lens' ListProvisionedProductPlans (Lude.Maybe AccessLevelFilter)
lpppAccessLevelFilter = Lens.lens (accessLevelFilter :: ListProvisionedProductPlans -> Lude.Maybe AccessLevelFilter) (\s a -> s {accessLevelFilter = a} :: ListProvisionedProductPlans)
{-# DEPRECATED lpppAccessLevelFilter "Use generic-lens or generic-optics with 'accessLevelFilter' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpppPageToken :: Lens.Lens' ListProvisionedProductPlans (Lude.Maybe Lude.Text)
lpppPageToken = Lens.lens (pageToken :: ListProvisionedProductPlans -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListProvisionedProductPlans)
{-# DEPRECATED lpppPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpppPageSize :: Lens.Lens' ListProvisionedProductPlans (Lude.Maybe Lude.Natural)
lpppPageSize = Lens.lens (pageSize :: ListProvisionedProductPlans -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListProvisionedProductPlans)
{-# DEPRECATED lpppPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListProvisionedProductPlans where
  page rq rs
    | Page.stop (rs Lens.^. lppprsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lppprsProvisionedProductPlans) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpppPageToken Lens..~ rs Lens.^. lppprsNextPageToken

instance Lude.AWSRequest ListProvisionedProductPlans where
  type
    Rs ListProvisionedProductPlans =
      ListProvisionedProductPlansResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProvisionedProductPlansResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "ProvisionedProductPlans" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProvisionedProductPlans where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ListProvisionedProductPlans" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListProvisionedProductPlans where
  toJSON ListProvisionedProductPlans' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionProductId" Lude..=) Lude.<$> provisionProductId,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("AccessLevelFilter" Lude..=) Lude.<$> accessLevelFilter,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath ListProvisionedProductPlans where
  toPath = Lude.const "/"

instance Lude.ToQuery ListProvisionedProductPlans where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListProvisionedProductPlansResponse' smart constructor.
data ListProvisionedProductPlansResponse = ListProvisionedProductPlansResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Information about the plans.
    provisionedProductPlans :: Lude.Maybe [ProvisionedProductPlanSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProvisionedProductPlansResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'provisionedProductPlans' - Information about the plans.
-- * 'responseStatus' - The response status code.
mkListProvisionedProductPlansResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProvisionedProductPlansResponse
mkListProvisionedProductPlansResponse pResponseStatus_ =
  ListProvisionedProductPlansResponse'
    { nextPageToken =
        Lude.Nothing,
      provisionedProductPlans = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lppprsNextPageToken :: Lens.Lens' ListProvisionedProductPlansResponse (Lude.Maybe Lude.Text)
lppprsNextPageToken = Lens.lens (nextPageToken :: ListProvisionedProductPlansResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListProvisionedProductPlansResponse)
{-# DEPRECATED lppprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the plans.
--
-- /Note:/ Consider using 'provisionedProductPlans' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lppprsProvisionedProductPlans :: Lens.Lens' ListProvisionedProductPlansResponse (Lude.Maybe [ProvisionedProductPlanSummary])
lppprsProvisionedProductPlans = Lens.lens (provisionedProductPlans :: ListProvisionedProductPlansResponse -> Lude.Maybe [ProvisionedProductPlanSummary]) (\s a -> s {provisionedProductPlans = a} :: ListProvisionedProductPlansResponse)
{-# DEPRECATED lppprsProvisionedProductPlans "Use generic-lens or generic-optics with 'provisionedProductPlans' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lppprsResponseStatus :: Lens.Lens' ListProvisionedProductPlansResponse Lude.Int
lppprsResponseStatus = Lens.lens (responseStatus :: ListProvisionedProductPlansResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProvisionedProductPlansResponse)
{-# DEPRECATED lppprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

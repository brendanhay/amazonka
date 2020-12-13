{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the resource changes for the specified plan.
module Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan
  ( -- * Creating a request
    DescribeProvisionedProductPlan (..),
    mkDescribeProvisionedProductPlan,

    -- ** Request lenses
    dpppfPlanId,
    dpppfAcceptLanguage,
    dpppfPageToken,
    dpppfPageSize,

    -- * Destructuring the response
    DescribeProvisionedProductPlanResponse (..),
    mkDescribeProvisionedProductPlanResponse,

    -- ** Response lenses
    dpppfrsNextPageToken,
    dpppfrsProvisionedProductPlanDetails,
    dpppfrsResourceChanges,
    dpppfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDescribeProvisionedProductPlan' smart constructor.
data DescribeProvisionedProductPlan = DescribeProvisionedProductPlan'
  { -- | The plan identifier.
    planId :: Lude.Text,
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

-- | Creates a value of 'DescribeProvisionedProductPlan' with the minimum fields required to make a request.
--
-- * 'planId' - The plan identifier.
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
mkDescribeProvisionedProductPlan ::
  -- | 'planId'
  Lude.Text ->
  DescribeProvisionedProductPlan
mkDescribeProvisionedProductPlan pPlanId_ =
  DescribeProvisionedProductPlan'
    { planId = pPlanId_,
      acceptLanguage = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppfPlanId :: Lens.Lens' DescribeProvisionedProductPlan Lude.Text
dpppfPlanId = Lens.lens (planId :: DescribeProvisionedProductPlan -> Lude.Text) (\s a -> s {planId = a} :: DescribeProvisionedProductPlan)
{-# DEPRECATED dpppfPlanId "Use generic-lens or generic-optics with 'planId' instead." #-}

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
dpppfAcceptLanguage :: Lens.Lens' DescribeProvisionedProductPlan (Lude.Maybe Lude.Text)
dpppfAcceptLanguage = Lens.lens (acceptLanguage :: DescribeProvisionedProductPlan -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DescribeProvisionedProductPlan)
{-# DEPRECATED dpppfAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppfPageToken :: Lens.Lens' DescribeProvisionedProductPlan (Lude.Maybe Lude.Text)
dpppfPageToken = Lens.lens (pageToken :: DescribeProvisionedProductPlan -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: DescribeProvisionedProductPlan)
{-# DEPRECATED dpppfPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppfPageSize :: Lens.Lens' DescribeProvisionedProductPlan (Lude.Maybe Lude.Natural)
dpppfPageSize = Lens.lens (pageSize :: DescribeProvisionedProductPlan -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: DescribeProvisionedProductPlan)
{-# DEPRECATED dpppfPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest DescribeProvisionedProductPlan where
  type
    Rs DescribeProvisionedProductPlan =
      DescribeProvisionedProductPlanResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProvisionedProductPlanResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "ProvisionedProductPlanDetails")
            Lude.<*> (x Lude..?> "ResourceChanges" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProvisionedProductPlan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DescribeProvisionedProductPlan" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProvisionedProductPlan where
  toJSON DescribeProvisionedProductPlan' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PlanId" Lude..= planId),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize
          ]
      )

instance Lude.ToPath DescribeProvisionedProductPlan where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProvisionedProductPlan where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProvisionedProductPlanResponse' smart constructor.
data DescribeProvisionedProductPlanResponse = DescribeProvisionedProductPlanResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Information about the plan.
    provisionedProductPlanDetails :: Lude.Maybe ProvisionedProductPlanDetails,
    -- | Information about the resource changes that will occur when the plan is executed.
    resourceChanges :: Lude.Maybe [ResourceChange],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProvisionedProductPlanResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'provisionedProductPlanDetails' - Information about the plan.
-- * 'resourceChanges' - Information about the resource changes that will occur when the plan is executed.
-- * 'responseStatus' - The response status code.
mkDescribeProvisionedProductPlanResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProvisionedProductPlanResponse
mkDescribeProvisionedProductPlanResponse pResponseStatus_ =
  DescribeProvisionedProductPlanResponse'
    { nextPageToken =
        Lude.Nothing,
      provisionedProductPlanDetails = Lude.Nothing,
      resourceChanges = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppfrsNextPageToken :: Lens.Lens' DescribeProvisionedProductPlanResponse (Lude.Maybe Lude.Text)
dpppfrsNextPageToken = Lens.lens (nextPageToken :: DescribeProvisionedProductPlanResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: DescribeProvisionedProductPlanResponse)
{-# DEPRECATED dpppfrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the plan.
--
-- /Note:/ Consider using 'provisionedProductPlanDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppfrsProvisionedProductPlanDetails :: Lens.Lens' DescribeProvisionedProductPlanResponse (Lude.Maybe ProvisionedProductPlanDetails)
dpppfrsProvisionedProductPlanDetails = Lens.lens (provisionedProductPlanDetails :: DescribeProvisionedProductPlanResponse -> Lude.Maybe ProvisionedProductPlanDetails) (\s a -> s {provisionedProductPlanDetails = a} :: DescribeProvisionedProductPlanResponse)
{-# DEPRECATED dpppfrsProvisionedProductPlanDetails "Use generic-lens or generic-optics with 'provisionedProductPlanDetails' instead." #-}

-- | Information about the resource changes that will occur when the plan is executed.
--
-- /Note:/ Consider using 'resourceChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppfrsResourceChanges :: Lens.Lens' DescribeProvisionedProductPlanResponse (Lude.Maybe [ResourceChange])
dpppfrsResourceChanges = Lens.lens (resourceChanges :: DescribeProvisionedProductPlanResponse -> Lude.Maybe [ResourceChange]) (\s a -> s {resourceChanges = a} :: DescribeProvisionedProductPlanResponse)
{-# DEPRECATED dpppfrsResourceChanges "Use generic-lens or generic-optics with 'resourceChanges' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppfrsResponseStatus :: Lens.Lens' DescribeProvisionedProductPlanResponse Lude.Int
dpppfrsResponseStatus = Lens.lens (responseStatus :: DescribeProvisionedProductPlanResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProvisionedProductPlanResponse)
{-# DEPRECATED dpppfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

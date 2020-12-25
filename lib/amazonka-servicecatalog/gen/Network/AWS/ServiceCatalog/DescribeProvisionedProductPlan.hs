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
    dpppPlanId,
    dpppAcceptLanguage,
    dpppPageSize,
    dpppPageToken,

    -- * Destructuring the response
    DescribeProvisionedProductPlanResponse (..),
    mkDescribeProvisionedProductPlanResponse,

    -- ** Response lenses
    dppprrsNextPageToken,
    dppprrsProvisionedProductPlanDetails,
    dppprrsResourceChanges,
    dppprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeProvisionedProductPlan' smart constructor.
data DescribeProvisionedProductPlan = DescribeProvisionedProductPlan'
  { -- | The plan identifier.
    planId :: Types.PlanId,
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
    acceptLanguage :: Core.Maybe Types.AcceptLanguage,
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Core.Maybe Types.PageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProvisionedProductPlan' value with any optional fields omitted.
mkDescribeProvisionedProductPlan ::
  -- | 'planId'
  Types.PlanId ->
  DescribeProvisionedProductPlan
mkDescribeProvisionedProductPlan planId =
  DescribeProvisionedProductPlan'
    { planId,
      acceptLanguage = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing
    }

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppPlanId :: Lens.Lens' DescribeProvisionedProductPlan Types.PlanId
dpppPlanId = Lens.field @"planId"
{-# DEPRECATED dpppPlanId "Use generic-lens or generic-optics with 'planId' instead." #-}

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
dpppAcceptLanguage :: Lens.Lens' DescribeProvisionedProductPlan (Core.Maybe Types.AcceptLanguage)
dpppAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dpppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppPageSize :: Lens.Lens' DescribeProvisionedProductPlan (Core.Maybe Core.Natural)
dpppPageSize = Lens.field @"pageSize"
{-# DEPRECATED dpppPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppPageToken :: Lens.Lens' DescribeProvisionedProductPlan (Core.Maybe Types.PageToken)
dpppPageToken = Lens.field @"pageToken"
{-# DEPRECATED dpppPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON DescribeProvisionedProductPlan where
  toJSON DescribeProvisionedProductPlan {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PlanId" Core..= planId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken
          ]
      )

instance Core.AWSRequest DescribeProvisionedProductPlan where
  type
    Rs DescribeProvisionedProductPlan =
      DescribeProvisionedProductPlanResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DescribeProvisionedProductPlan"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisionedProductPlanResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "ProvisionedProductPlanDetails")
            Core.<*> (x Core..:? "ResourceChanges")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProvisionedProductPlanResponse' smart constructor.
data DescribeProvisionedProductPlanResponse = DescribeProvisionedProductPlanResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | Information about the plan.
    provisionedProductPlanDetails :: Core.Maybe Types.ProvisionedProductPlanDetails,
    -- | Information about the resource changes that will occur when the plan is executed.
    resourceChanges :: Core.Maybe [Types.ResourceChange],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeProvisionedProductPlanResponse' value with any optional fields omitted.
mkDescribeProvisionedProductPlanResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeProvisionedProductPlanResponse
mkDescribeProvisionedProductPlanResponse responseStatus =
  DescribeProvisionedProductPlanResponse'
    { nextPageToken =
        Core.Nothing,
      provisionedProductPlanDetails = Core.Nothing,
      resourceChanges = Core.Nothing,
      responseStatus
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppprrsNextPageToken :: Lens.Lens' DescribeProvisionedProductPlanResponse (Core.Maybe Types.NextPageToken)
dppprrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED dppprrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the plan.
--
-- /Note:/ Consider using 'provisionedProductPlanDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppprrsProvisionedProductPlanDetails :: Lens.Lens' DescribeProvisionedProductPlanResponse (Core.Maybe Types.ProvisionedProductPlanDetails)
dppprrsProvisionedProductPlanDetails = Lens.field @"provisionedProductPlanDetails"
{-# DEPRECATED dppprrsProvisionedProductPlanDetails "Use generic-lens or generic-optics with 'provisionedProductPlanDetails' instead." #-}

-- | Information about the resource changes that will occur when the plan is executed.
--
-- /Note:/ Consider using 'resourceChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppprrsResourceChanges :: Lens.Lens' DescribeProvisionedProductPlanResponse (Core.Maybe [Types.ResourceChange])
dppprrsResourceChanges = Lens.field @"resourceChanges"
{-# DEPRECATED dppprrsResourceChanges "Use generic-lens or generic-optics with 'resourceChanges' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppprrsResponseStatus :: Lens.Lens' DescribeProvisionedProductPlanResponse Core.Int
dppprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dppprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

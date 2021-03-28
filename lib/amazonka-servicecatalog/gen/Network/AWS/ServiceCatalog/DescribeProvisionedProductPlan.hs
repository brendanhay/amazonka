{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeProvisionedProductPlan (..)
    , mkDescribeProvisionedProductPlan
    -- ** Request lenses
    , dpppPlanId
    , dpppAcceptLanguage
    , dpppPageSize
    , dpppPageToken

    -- * Destructuring the response
    , DescribeProvisionedProductPlanResponse (..)
    , mkDescribeProvisionedProductPlanResponse
    -- ** Response lenses
    , dppprrsNextPageToken
    , dppprrsProvisionedProductPlanDetails
    , dppprrsResourceChanges
    , dppprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeProvisionedProductPlan' smart constructor.
data DescribeProvisionedProductPlan = DescribeProvisionedProductPlan'
  { planId :: Types.PlanId
    -- ^ The plan identifier.
  , acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
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
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProvisionedProductPlan' value with any optional fields omitted.
mkDescribeProvisionedProductPlan
    :: Types.PlanId -- ^ 'planId'
    -> DescribeProvisionedProductPlan
mkDescribeProvisionedProductPlan planId
  = DescribeProvisionedProductPlan'{planId,
                                    acceptLanguage = Core.Nothing, pageSize = Core.Nothing,
                                    pageToken = Core.Nothing}

-- | The plan identifier.
--
-- /Note:/ Consider using 'planId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppPlanId :: Lens.Lens' DescribeProvisionedProductPlan Types.PlanId
dpppPlanId = Lens.field @"planId"
{-# INLINEABLE dpppPlanId #-}
{-# DEPRECATED planId "Use generic-lens or generic-optics with 'planId' instead"  #-}

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
{-# INLINEABLE dpppAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppPageSize :: Lens.Lens' DescribeProvisionedProductPlan (Core.Maybe Core.Natural)
dpppPageSize = Lens.field @"pageSize"
{-# INLINEABLE dpppPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpppPageToken :: Lens.Lens' DescribeProvisionedProductPlan (Core.Maybe Types.PageToken)
dpppPageToken = Lens.field @"pageToken"
{-# INLINEABLE dpppPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery DescribeProvisionedProductPlan where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeProvisionedProductPlan where
        toHeaders DescribeProvisionedProductPlan{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DescribeProvisionedProductPlan")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeProvisionedProductPlan where
        toJSON DescribeProvisionedProductPlan{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PlanId" Core..= planId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest DescribeProvisionedProductPlan where
        type Rs DescribeProvisionedProductPlan =
             DescribeProvisionedProductPlanResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeProvisionedProductPlanResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*>
                     x Core..:? "ProvisionedProductPlanDetails"
                     Core.<*> x Core..:? "ResourceChanges"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeProvisionedProductPlanResponse' smart constructor.
data DescribeProvisionedProductPlanResponse = DescribeProvisionedProductPlanResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , provisionedProductPlanDetails :: Core.Maybe Types.ProvisionedProductPlanDetails
    -- ^ Information about the plan.
  , resourceChanges :: Core.Maybe [Types.ResourceChange]
    -- ^ Information about the resource changes that will occur when the plan is executed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeProvisionedProductPlanResponse' value with any optional fields omitted.
mkDescribeProvisionedProductPlanResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeProvisionedProductPlanResponse
mkDescribeProvisionedProductPlanResponse responseStatus
  = DescribeProvisionedProductPlanResponse'{nextPageToken =
                                              Core.Nothing,
                                            provisionedProductPlanDetails = Core.Nothing,
                                            resourceChanges = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppprrsNextPageToken :: Lens.Lens' DescribeProvisionedProductPlanResponse (Core.Maybe Types.NextPageToken)
dppprrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE dppprrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Information about the plan.
--
-- /Note:/ Consider using 'provisionedProductPlanDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppprrsProvisionedProductPlanDetails :: Lens.Lens' DescribeProvisionedProductPlanResponse (Core.Maybe Types.ProvisionedProductPlanDetails)
dppprrsProvisionedProductPlanDetails = Lens.field @"provisionedProductPlanDetails"
{-# INLINEABLE dppprrsProvisionedProductPlanDetails #-}
{-# DEPRECATED provisionedProductPlanDetails "Use generic-lens or generic-optics with 'provisionedProductPlanDetails' instead"  #-}

-- | Information about the resource changes that will occur when the plan is executed.
--
-- /Note:/ Consider using 'resourceChanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppprrsResourceChanges :: Lens.Lens' DescribeProvisionedProductPlanResponse (Core.Maybe [Types.ResourceChange])
dppprrsResourceChanges = Lens.field @"resourceChanges"
{-# INLINEABLE dppprrsResourceChanges #-}
{-# DEPRECATED resourceChanges "Use generic-lens or generic-optics with 'resourceChanges' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppprrsResponseStatus :: Lens.Lens' DescribeProvisionedProductPlanResponse Core.Int
dppprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dppprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

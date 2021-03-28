{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListProvisionedProductPlans (..)
    , mkListProvisionedProductPlans
    -- ** Request lenses
    , lpppAcceptLanguage
    , lpppAccessLevelFilter
    , lpppPageSize
    , lpppPageToken
    , lpppProvisionProductId

    -- * Destructuring the response
    , ListProvisionedProductPlansResponse (..)
    , mkListProvisionedProductPlansResponse
    -- ** Response lenses
    , lppprrsNextPageToken
    , lppprrsProvisionedProductPlans
    , lppprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListProvisionedProductPlans' smart constructor.
data ListProvisionedProductPlans = ListProvisionedProductPlans'
  { acceptLanguage :: Core.Maybe Types.AcceptLanguage
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
  , accessLevelFilter :: Core.Maybe Types.AccessLevelFilter
    -- ^ The access level to use to obtain results. The default is @User@ .
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  , provisionProductId :: Core.Maybe Types.ProvisionProductId
    -- ^ The product identifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProvisionedProductPlans' value with any optional fields omitted.
mkListProvisionedProductPlans
    :: ListProvisionedProductPlans
mkListProvisionedProductPlans
  = ListProvisionedProductPlans'{acceptLanguage = Core.Nothing,
                                 accessLevelFilter = Core.Nothing, pageSize = Core.Nothing,
                                 pageToken = Core.Nothing, provisionProductId = Core.Nothing}

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
lpppAcceptLanguage :: Lens.Lens' ListProvisionedProductPlans (Core.Maybe Types.AcceptLanguage)
lpppAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE lpppAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The access level to use to obtain results. The default is @User@ .
--
-- /Note:/ Consider using 'accessLevelFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpppAccessLevelFilter :: Lens.Lens' ListProvisionedProductPlans (Core.Maybe Types.AccessLevelFilter)
lpppAccessLevelFilter = Lens.field @"accessLevelFilter"
{-# INLINEABLE lpppAccessLevelFilter #-}
{-# DEPRECATED accessLevelFilter "Use generic-lens or generic-optics with 'accessLevelFilter' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpppPageSize :: Lens.Lens' ListProvisionedProductPlans (Core.Maybe Core.Natural)
lpppPageSize = Lens.field @"pageSize"
{-# INLINEABLE lpppPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpppPageToken :: Lens.Lens' ListProvisionedProductPlans (Core.Maybe Types.PageToken)
lpppPageToken = Lens.field @"pageToken"
{-# INLINEABLE lpppPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'provisionProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpppProvisionProductId :: Lens.Lens' ListProvisionedProductPlans (Core.Maybe Types.ProvisionProductId)
lpppProvisionProductId = Lens.field @"provisionProductId"
{-# INLINEABLE lpppProvisionProductId #-}
{-# DEPRECATED provisionProductId "Use generic-lens or generic-optics with 'provisionProductId' instead"  #-}

instance Core.ToQuery ListProvisionedProductPlans where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListProvisionedProductPlans where
        toHeaders ListProvisionedProductPlans{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.ListProvisionedProductPlans")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListProvisionedProductPlans where
        toJSON ListProvisionedProductPlans{..}
          = Core.object
              (Core.catMaybes
                 [("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("AccessLevelFilter" Core..=) Core.<$> accessLevelFilter,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken,
                  ("ProvisionProductId" Core..=) Core.<$> provisionProductId])

instance Core.AWSRequest ListProvisionedProductPlans where
        type Rs ListProvisionedProductPlans =
             ListProvisionedProductPlansResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListProvisionedProductPlansResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*>
                     x Core..:? "ProvisionedProductPlans"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListProvisionedProductPlans where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"provisionedProductPlans" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkListProvisionedProductPlansResponse' smart constructor.
data ListProvisionedProductPlansResponse = ListProvisionedProductPlansResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , provisionedProductPlans :: Core.Maybe [Types.ProvisionedProductPlanSummary]
    -- ^ Information about the plans.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProvisionedProductPlansResponse' value with any optional fields omitted.
mkListProvisionedProductPlansResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListProvisionedProductPlansResponse
mkListProvisionedProductPlansResponse responseStatus
  = ListProvisionedProductPlansResponse'{nextPageToken =
                                           Core.Nothing,
                                         provisionedProductPlans = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lppprrsNextPageToken :: Lens.Lens' ListProvisionedProductPlansResponse (Core.Maybe Types.NextPageToken)
lppprrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lppprrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Information about the plans.
--
-- /Note:/ Consider using 'provisionedProductPlans' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lppprrsProvisionedProductPlans :: Lens.Lens' ListProvisionedProductPlansResponse (Core.Maybe [Types.ProvisionedProductPlanSummary])
lppprrsProvisionedProductPlans = Lens.field @"provisionedProductPlans"
{-# INLINEABLE lppprrsProvisionedProductPlans #-}
{-# DEPRECATED provisionedProductPlans "Use generic-lens or generic-optics with 'provisionedProductPlans' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lppprrsResponseStatus :: Lens.Lens' ListProvisionedProductPlansResponse Core.Int
lppprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lppprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

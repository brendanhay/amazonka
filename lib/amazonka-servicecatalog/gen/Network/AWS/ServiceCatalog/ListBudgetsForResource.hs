{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListBudgetsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the budgets associated to the specified resource.
module Network.AWS.ServiceCatalog.ListBudgetsForResource
    (
    -- * Creating a request
      ListBudgetsForResource (..)
    , mkListBudgetsForResource
    -- ** Request lenses
    , lbfrResourceId
    , lbfrAcceptLanguage
    , lbfrPageSize
    , lbfrPageToken

    -- * Destructuring the response
    , ListBudgetsForResourceResponse (..)
    , mkListBudgetsForResourceResponse
    -- ** Response lenses
    , lbfrrrsBudgets
    , lbfrrrsNextPageToken
    , lbfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListBudgetsForResource' smart constructor.
data ListBudgetsForResource = ListBudgetsForResource'
  { resourceId :: Types.ResourceId
    -- ^ The resource identifier.
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

-- | Creates a 'ListBudgetsForResource' value with any optional fields omitted.
mkListBudgetsForResource
    :: Types.ResourceId -- ^ 'resourceId'
    -> ListBudgetsForResource
mkListBudgetsForResource resourceId
  = ListBudgetsForResource'{resourceId,
                            acceptLanguage = Core.Nothing, pageSize = Core.Nothing,
                            pageToken = Core.Nothing}

-- | The resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrResourceId :: Lens.Lens' ListBudgetsForResource Types.ResourceId
lbfrResourceId = Lens.field @"resourceId"
{-# INLINEABLE lbfrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

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
lbfrAcceptLanguage :: Lens.Lens' ListBudgetsForResource (Core.Maybe Types.AcceptLanguage)
lbfrAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE lbfrAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrPageSize :: Lens.Lens' ListBudgetsForResource (Core.Maybe Core.Natural)
lbfrPageSize = Lens.field @"pageSize"
{-# INLINEABLE lbfrPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrPageToken :: Lens.Lens' ListBudgetsForResource (Core.Maybe Types.PageToken)
lbfrPageToken = Lens.field @"pageToken"
{-# INLINEABLE lbfrPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListBudgetsForResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListBudgetsForResource where
        toHeaders ListBudgetsForResource{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.ListBudgetsForResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListBudgetsForResource where
        toJSON ListBudgetsForResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest ListBudgetsForResource where
        type Rs ListBudgetsForResource = ListBudgetsForResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListBudgetsForResourceResponse' Core.<$>
                   (x Core..:? "Budgets") Core.<*> x Core..:? "NextPageToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListBudgetsForResourceResponse' smart constructor.
data ListBudgetsForResourceResponse = ListBudgetsForResourceResponse'
  { budgets :: Core.Maybe [Types.BudgetDetail]
    -- ^ Information about the associated budgets.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBudgetsForResourceResponse' value with any optional fields omitted.
mkListBudgetsForResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBudgetsForResourceResponse
mkListBudgetsForResourceResponse responseStatus
  = ListBudgetsForResourceResponse'{budgets = Core.Nothing,
                                    nextPageToken = Core.Nothing, responseStatus}

-- | Information about the associated budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrrrsBudgets :: Lens.Lens' ListBudgetsForResourceResponse (Core.Maybe [Types.BudgetDetail])
lbfrrrsBudgets = Lens.field @"budgets"
{-# INLINEABLE lbfrrrsBudgets #-}
{-# DEPRECATED budgets "Use generic-lens or generic-optics with 'budgets' instead"  #-}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrrrsNextPageToken :: Lens.Lens' ListBudgetsForResourceResponse (Core.Maybe Types.NextPageToken)
lbfrrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lbfrrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbfrrrsResponseStatus :: Lens.Lens' ListBudgetsForResourceResponse Core.Int
lbfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

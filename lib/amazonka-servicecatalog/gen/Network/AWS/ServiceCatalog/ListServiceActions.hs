{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListServiceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all self-service actions.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListServiceActions
    (
    -- * Creating a request
      ListServiceActions (..)
    , mkListServiceActions
    -- ** Request lenses
    , lsaAcceptLanguage
    , lsaPageSize
    , lsaPageToken

    -- * Destructuring the response
    , ListServiceActionsResponse (..)
    , mkListServiceActionsResponse
    -- ** Response lenses
    , lsarrsNextPageToken
    , lsarrsServiceActionSummaries
    , lsarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListServiceActions' smart constructor.
data ListServiceActions = ListServiceActions'
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
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListServiceActions' value with any optional fields omitted.
mkListServiceActions
    :: ListServiceActions
mkListServiceActions
  = ListServiceActions'{acceptLanguage = Core.Nothing,
                        pageSize = Core.Nothing, pageToken = Core.Nothing}

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
lsaAcceptLanguage :: Lens.Lens' ListServiceActions (Core.Maybe Types.AcceptLanguage)
lsaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE lsaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsaPageSize :: Lens.Lens' ListServiceActions (Core.Maybe Core.Natural)
lsaPageSize = Lens.field @"pageSize"
{-# INLINEABLE lsaPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsaPageToken :: Lens.Lens' ListServiceActions (Core.Maybe Types.PageToken)
lsaPageToken = Lens.field @"pageToken"
{-# INLINEABLE lsaPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListServiceActions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListServiceActions where
        toHeaders ListServiceActions{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.ListServiceActions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListServiceActions where
        toJSON ListServiceActions{..}
          = Core.object
              (Core.catMaybes
                 [("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest ListServiceActions where
        type Rs ListServiceActions = ListServiceActionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListServiceActionsResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*>
                     x Core..:? "ServiceActionSummaries"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListServiceActions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"serviceActionSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkListServiceActionsResponse' smart constructor.
data ListServiceActionsResponse = ListServiceActionsResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , serviceActionSummaries :: Core.Maybe [Types.ServiceActionSummary]
    -- ^ An object containing information about the service actions associated with the provisioning artifact.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListServiceActionsResponse' value with any optional fields omitted.
mkListServiceActionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListServiceActionsResponse
mkListServiceActionsResponse responseStatus
  = ListServiceActionsResponse'{nextPageToken = Core.Nothing,
                                serviceActionSummaries = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarrsNextPageToken :: Lens.Lens' ListServiceActionsResponse (Core.Maybe Types.NextPageToken)
lsarrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lsarrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | An object containing information about the service actions associated with the provisioning artifact.
--
-- /Note:/ Consider using 'serviceActionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarrsServiceActionSummaries :: Lens.Lens' ListServiceActionsResponse (Core.Maybe [Types.ServiceActionSummary])
lsarrsServiceActionSummaries = Lens.field @"serviceActionSummaries"
{-# INLINEABLE lsarrsServiceActionSummaries #-}
{-# DEPRECATED serviceActionSummaries "Use generic-lens or generic-optics with 'serviceActionSummaries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarrsResponseStatus :: Lens.Lens' ListServiceActionsResponse Core.Int
lsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

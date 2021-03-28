{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListLaunchPaths (..)
    , mkListLaunchPaths
    -- ** Request lenses
    , llpProductId
    , llpAcceptLanguage
    , llpPageSize
    , llpPageToken

    -- * Destructuring the response
    , ListLaunchPathsResponse (..)
    , mkListLaunchPathsResponse
    -- ** Response lenses
    , llprrsLaunchPathSummaries
    , llprrsNextPageToken
    , llprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListLaunchPaths' smart constructor.
data ListLaunchPaths = ListLaunchPaths'
  { productId :: Types.ProductId
    -- ^ The product identifier.
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

-- | Creates a 'ListLaunchPaths' value with any optional fields omitted.
mkListLaunchPaths
    :: Types.ProductId -- ^ 'productId'
    -> ListLaunchPaths
mkListLaunchPaths productId
  = ListLaunchPaths'{productId, acceptLanguage = Core.Nothing,
                     pageSize = Core.Nothing, pageToken = Core.Nothing}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llpProductId :: Lens.Lens' ListLaunchPaths Types.ProductId
llpProductId = Lens.field @"productId"
{-# INLINEABLE llpProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

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
llpAcceptLanguage :: Lens.Lens' ListLaunchPaths (Core.Maybe Types.AcceptLanguage)
llpAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE llpAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llpPageSize :: Lens.Lens' ListLaunchPaths (Core.Maybe Core.Natural)
llpPageSize = Lens.field @"pageSize"
{-# INLINEABLE llpPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llpPageToken :: Lens.Lens' ListLaunchPaths (Core.Maybe Types.PageToken)
llpPageToken = Lens.field @"pageToken"
{-# INLINEABLE llpPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListLaunchPaths where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListLaunchPaths where
        toHeaders ListLaunchPaths{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.ListLaunchPaths")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListLaunchPaths where
        toJSON ListLaunchPaths{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductId" Core..= productId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest ListLaunchPaths where
        type Rs ListLaunchPaths = ListLaunchPathsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListLaunchPathsResponse' Core.<$>
                   (x Core..:? "LaunchPathSummaries") Core.<*>
                     x Core..:? "NextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListLaunchPaths where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"launchPathSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkListLaunchPathsResponse' smart constructor.
data ListLaunchPathsResponse = ListLaunchPathsResponse'
  { launchPathSummaries :: Core.Maybe [Types.LaunchPathSummary]
    -- ^ Information about the launch path.
  , nextPageToken :: Core.Maybe Types.PageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLaunchPathsResponse' value with any optional fields omitted.
mkListLaunchPathsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListLaunchPathsResponse
mkListLaunchPathsResponse responseStatus
  = ListLaunchPathsResponse'{launchPathSummaries = Core.Nothing,
                             nextPageToken = Core.Nothing, responseStatus}

-- | Information about the launch path.
--
-- /Note:/ Consider using 'launchPathSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llprrsLaunchPathSummaries :: Lens.Lens' ListLaunchPathsResponse (Core.Maybe [Types.LaunchPathSummary])
llprrsLaunchPathSummaries = Lens.field @"launchPathSummaries"
{-# INLINEABLE llprrsLaunchPathSummaries #-}
{-# DEPRECATED launchPathSummaries "Use generic-lens or generic-optics with 'launchPathSummaries' instead"  #-}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llprrsNextPageToken :: Lens.Lens' ListLaunchPathsResponse (Core.Maybe Types.PageToken)
llprrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE llprrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llprrsResponseStatus :: Lens.Lens' ListLaunchPathsResponse Core.Int
llprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE llprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

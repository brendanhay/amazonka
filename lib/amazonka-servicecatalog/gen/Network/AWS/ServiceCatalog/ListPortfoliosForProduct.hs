{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListPortfoliosForProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios that the specified product is associated with.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPortfoliosForProduct
    (
    -- * Creating a request
      ListPortfoliosForProduct (..)
    , mkListPortfoliosForProduct
    -- ** Request lenses
    , lpfpfProductId
    , lpfpfAcceptLanguage
    , lpfpfPageSize
    , lpfpfPageToken

    -- * Destructuring the response
    , ListPortfoliosForProductResponse (..)
    , mkListPortfoliosForProductResponse
    -- ** Response lenses
    , lpfprfrsNextPageToken
    , lpfprfrsPortfolioDetails
    , lpfprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListPortfoliosForProduct' smart constructor.
data ListPortfoliosForProduct = ListPortfoliosForProduct'
  { productId :: Types.Id
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

-- | Creates a 'ListPortfoliosForProduct' value with any optional fields omitted.
mkListPortfoliosForProduct
    :: Types.Id -- ^ 'productId'
    -> ListPortfoliosForProduct
mkListPortfoliosForProduct productId
  = ListPortfoliosForProduct'{productId,
                              acceptLanguage = Core.Nothing, pageSize = Core.Nothing,
                              pageToken = Core.Nothing}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpfProductId :: Lens.Lens' ListPortfoliosForProduct Types.Id
lpfpfProductId = Lens.field @"productId"
{-# INLINEABLE lpfpfProductId #-}
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
lpfpfAcceptLanguage :: Lens.Lens' ListPortfoliosForProduct (Core.Maybe Types.AcceptLanguage)
lpfpfAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE lpfpfAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpfPageSize :: Lens.Lens' ListPortfoliosForProduct (Core.Maybe Core.Natural)
lpfpfPageSize = Lens.field @"pageSize"
{-# INLINEABLE lpfpfPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpfPageToken :: Lens.Lens' ListPortfoliosForProduct (Core.Maybe Types.PageToken)
lpfpfPageToken = Lens.field @"pageToken"
{-# INLINEABLE lpfpfPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListPortfoliosForProduct where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPortfoliosForProduct where
        toHeaders ListPortfoliosForProduct{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.ListPortfoliosForProduct")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPortfoliosForProduct where
        toJSON ListPortfoliosForProduct{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductId" Core..= productId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest ListPortfoliosForProduct where
        type Rs ListPortfoliosForProduct = ListPortfoliosForProductResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPortfoliosForProductResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*> x Core..:? "PortfolioDetails"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPortfoliosForProduct where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"portfolioDetails" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkListPortfoliosForProductResponse' smart constructor.
data ListPortfoliosForProductResponse = ListPortfoliosForProductResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , portfolioDetails :: Core.Maybe [Types.PortfolioDetail]
    -- ^ Information about the portfolios.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPortfoliosForProductResponse' value with any optional fields omitted.
mkListPortfoliosForProductResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPortfoliosForProductResponse
mkListPortfoliosForProductResponse responseStatus
  = ListPortfoliosForProductResponse'{nextPageToken = Core.Nothing,
                                      portfolioDetails = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprfrsNextPageToken :: Lens.Lens' ListPortfoliosForProductResponse (Core.Maybe Types.NextPageToken)
lpfprfrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lpfprfrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Information about the portfolios.
--
-- /Note:/ Consider using 'portfolioDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprfrsPortfolioDetails :: Lens.Lens' ListPortfoliosForProductResponse (Core.Maybe [Types.PortfolioDetail])
lpfprfrsPortfolioDetails = Lens.field @"portfolioDetails"
{-# INLINEABLE lpfprfrsPortfolioDetails #-}
{-# DEPRECATED portfolioDetails "Use generic-lens or generic-optics with 'portfolioDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprfrsResponseStatus :: Lens.Lens' ListPortfoliosForProductResponse Core.Int
lpfprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpfprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListPortfolios
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios in the catalog.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPortfolios
    (
    -- * Creating a request
      ListPortfolios (..)
    , mkListPortfolios
    -- ** Request lenses
    , lpAcceptLanguage
    , lpPageSize
    , lpPageToken

    -- * Destructuring the response
    , ListPortfoliosResponse (..)
    , mkListPortfoliosResponse
    -- ** Response lenses
    , lprrsNextPageToken
    , lprrsPortfolioDetails
    , lprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListPortfolios' smart constructor.
data ListPortfolios = ListPortfolios'
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

-- | Creates a 'ListPortfolios' value with any optional fields omitted.
mkListPortfolios
    :: ListPortfolios
mkListPortfolios
  = ListPortfolios'{acceptLanguage = Core.Nothing,
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
lpAcceptLanguage :: Lens.Lens' ListPortfolios (Core.Maybe Types.AcceptLanguage)
lpAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE lpAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPageSize :: Lens.Lens' ListPortfolios (Core.Maybe Core.Natural)
lpPageSize = Lens.field @"pageSize"
{-# INLINEABLE lpPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPageToken :: Lens.Lens' ListPortfolios (Core.Maybe Types.PageToken)
lpPageToken = Lens.field @"pageToken"
{-# INLINEABLE lpPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListPortfolios where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPortfolios where
        toHeaders ListPortfolios{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.ListPortfolios")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPortfolios where
        toJSON ListPortfolios{..}
          = Core.object
              (Core.catMaybes
                 [("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest ListPortfolios where
        type Rs ListPortfolios = ListPortfoliosResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPortfoliosResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*> x Core..:? "PortfolioDetails"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPortfolios where
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

-- | /See:/ 'mkListPortfoliosResponse' smart constructor.
data ListPortfoliosResponse = ListPortfoliosResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , portfolioDetails :: Core.Maybe [Types.PortfolioDetail]
    -- ^ Information about the portfolios.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPortfoliosResponse' value with any optional fields omitted.
mkListPortfoliosResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPortfoliosResponse
mkListPortfoliosResponse responseStatus
  = ListPortfoliosResponse'{nextPageToken = Core.Nothing,
                            portfolioDetails = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextPageToken :: Lens.Lens' ListPortfoliosResponse (Core.Maybe Types.NextPageToken)
lprrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lprrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Information about the portfolios.
--
-- /Note:/ Consider using 'portfolioDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPortfolioDetails :: Lens.Lens' ListPortfoliosResponse (Core.Maybe [Types.PortfolioDetail])
lprrsPortfolioDetails = Lens.field @"portfolioDetails"
{-# INLINEABLE lprrsPortfolioDetails #-}
{-# DEPRECATED portfolioDetails "Use generic-lens or generic-optics with 'portfolioDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPortfoliosResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListPortfolioAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account IDs that have access to the specified portfolio.
--
-- A delegated admin can list the accounts that have access to the shared portfolio. Note that if a delegated admin is de-registered, they can no longer perform this operation.
module Network.AWS.ServiceCatalog.ListPortfolioAccess
    (
    -- * Creating a request
      ListPortfolioAccess (..)
    , mkListPortfolioAccess
    -- ** Request lenses
    , lPortfolioId
    , lAcceptLanguage
    , lOrganizationParentId
    , lPageSize
    , lPageToken

    -- * Destructuring the response
    , ListPortfolioAccessResponse (..)
    , mkListPortfolioAccessResponse
    -- ** Response lenses
    , lrsAccountIds
    , lrsNextPageToken
    , lrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListPortfolioAccess' smart constructor.
data ListPortfolioAccess = ListPortfolioAccess'
  { portfolioId :: Types.PortfolioId
    -- ^ The portfolio identifier.
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
  , organizationParentId :: Core.Maybe Types.OrganizationParentId
    -- ^ The ID of an organization node the portfolio is shared with. All children of this node with an inherited portfolio share will be returned.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPortfolioAccess' value with any optional fields omitted.
mkListPortfolioAccess
    :: Types.PortfolioId -- ^ 'portfolioId'
    -> ListPortfolioAccess
mkListPortfolioAccess portfolioId
  = ListPortfolioAccess'{portfolioId, acceptLanguage = Core.Nothing,
                         organizationParentId = Core.Nothing, pageSize = Core.Nothing,
                         pageToken = Core.Nothing}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPortfolioId :: Lens.Lens' ListPortfolioAccess Types.PortfolioId
lPortfolioId = Lens.field @"portfolioId"
{-# INLINEABLE lPortfolioId #-}
{-# DEPRECATED portfolioId "Use generic-lens or generic-optics with 'portfolioId' instead"  #-}

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
lAcceptLanguage :: Lens.Lens' ListPortfolioAccess (Core.Maybe Types.AcceptLanguage)
lAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE lAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The ID of an organization node the portfolio is shared with. All children of this node with an inherited portfolio share will be returned.
--
-- /Note:/ Consider using 'organizationParentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lOrganizationParentId :: Lens.Lens' ListPortfolioAccess (Core.Maybe Types.OrganizationParentId)
lOrganizationParentId = Lens.field @"organizationParentId"
{-# INLINEABLE lOrganizationParentId #-}
{-# DEPRECATED organizationParentId "Use generic-lens or generic-optics with 'organizationParentId' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPageSize :: Lens.Lens' ListPortfolioAccess (Core.Maybe Core.Natural)
lPageSize = Lens.field @"pageSize"
{-# INLINEABLE lPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPageToken :: Lens.Lens' ListPortfolioAccess (Core.Maybe Types.PageToken)
lPageToken = Lens.field @"pageToken"
{-# INLINEABLE lPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListPortfolioAccess where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPortfolioAccess where
        toHeaders ListPortfolioAccess{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.ListPortfolioAccess")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPortfolioAccess where
        toJSON ListPortfolioAccess{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PortfolioId" Core..= portfolioId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("OrganizationParentId" Core..=) Core.<$> organizationParentId,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest ListPortfolioAccess where
        type Rs ListPortfolioAccess = ListPortfolioAccessResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPortfolioAccessResponse' Core.<$>
                   (x Core..:? "AccountIds") Core.<*> x Core..:? "NextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListPortfolioAccessResponse' smart constructor.
data ListPortfolioAccessResponse = ListPortfolioAccessResponse'
  { accountIds :: Core.Maybe [Types.AccountId]
    -- ^ Information about the AWS accounts with access to the portfolio.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPortfolioAccessResponse' value with any optional fields omitted.
mkListPortfolioAccessResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPortfolioAccessResponse
mkListPortfolioAccessResponse responseStatus
  = ListPortfolioAccessResponse'{accountIds = Core.Nothing,
                                 nextPageToken = Core.Nothing, responseStatus}

-- | Information about the AWS accounts with access to the portfolio.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsAccountIds :: Lens.Lens' ListPortfolioAccessResponse (Core.Maybe [Types.AccountId])
lrsAccountIds = Lens.field @"accountIds"
{-# INLINEABLE lrsAccountIds #-}
{-# DEPRECATED accountIds "Use generic-lens or generic-optics with 'accountIds' instead"  #-}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextPageToken :: Lens.Lens' ListPortfolioAccessResponse (Core.Maybe Types.NextPageToken)
lrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListPortfolioAccessResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

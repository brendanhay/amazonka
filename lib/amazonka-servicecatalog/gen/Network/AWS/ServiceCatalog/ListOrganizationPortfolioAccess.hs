{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListOrganizationPortfolioAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the organization nodes that have access to the specified portfolio. This API can only be called by the management account in the organization or by a delegated admin.
--
-- If a delegated admin is de-registered, they can no longer perform this operation.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListOrganizationPortfolioAccess
    (
    -- * Creating a request
      ListOrganizationPortfolioAccess (..)
    , mkListOrganizationPortfolioAccess
    -- ** Request lenses
    , lopaPortfolioId
    , lopaOrganizationNodeType
    , lopaAcceptLanguage
    , lopaPageSize
    , lopaPageToken

    -- * Destructuring the response
    , ListOrganizationPortfolioAccessResponse (..)
    , mkListOrganizationPortfolioAccessResponse
    -- ** Response lenses
    , loparrsNextPageToken
    , loparrsOrganizationNodes
    , loparrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListOrganizationPortfolioAccess' smart constructor.
data ListOrganizationPortfolioAccess = ListOrganizationPortfolioAccess'
  { portfolioId :: Types.Id
    -- ^ The portfolio identifier. For example, @port-2abcdext3y5fk@ .
  , organizationNodeType :: Types.OrganizationNodeType
    -- ^ The organization node type that will be returned in the output.
--
--
--     * @ORGANIZATION@ - Organization that has access to the portfolio. 
--
--
--     * @ORGANIZATIONAL_UNIT@ - Organizational unit that has access to the portfolio within your organization.
--
--
--     * @ACCOUNT@ - Account that has access to the portfolio within your organization.
--
--
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

-- | Creates a 'ListOrganizationPortfolioAccess' value with any optional fields omitted.
mkListOrganizationPortfolioAccess
    :: Types.Id -- ^ 'portfolioId'
    -> Types.OrganizationNodeType -- ^ 'organizationNodeType'
    -> ListOrganizationPortfolioAccess
mkListOrganizationPortfolioAccess portfolioId organizationNodeType
  = ListOrganizationPortfolioAccess'{portfolioId,
                                     organizationNodeType, acceptLanguage = Core.Nothing,
                                     pageSize = Core.Nothing, pageToken = Core.Nothing}

-- | The portfolio identifier. For example, @port-2abcdext3y5fk@ .
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopaPortfolioId :: Lens.Lens' ListOrganizationPortfolioAccess Types.Id
lopaPortfolioId = Lens.field @"portfolioId"
{-# INLINEABLE lopaPortfolioId #-}
{-# DEPRECATED portfolioId "Use generic-lens or generic-optics with 'portfolioId' instead"  #-}

-- | The organization node type that will be returned in the output.
--
--
--     * @ORGANIZATION@ - Organization that has access to the portfolio. 
--
--
--     * @ORGANIZATIONAL_UNIT@ - Organizational unit that has access to the portfolio within your organization.
--
--
--     * @ACCOUNT@ - Account that has access to the portfolio within your organization.
--
--
--
-- /Note:/ Consider using 'organizationNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopaOrganizationNodeType :: Lens.Lens' ListOrganizationPortfolioAccess Types.OrganizationNodeType
lopaOrganizationNodeType = Lens.field @"organizationNodeType"
{-# INLINEABLE lopaOrganizationNodeType #-}
{-# DEPRECATED organizationNodeType "Use generic-lens or generic-optics with 'organizationNodeType' instead"  #-}

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
lopaAcceptLanguage :: Lens.Lens' ListOrganizationPortfolioAccess (Core.Maybe Types.AcceptLanguage)
lopaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE lopaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopaPageSize :: Lens.Lens' ListOrganizationPortfolioAccess (Core.Maybe Core.Natural)
lopaPageSize = Lens.field @"pageSize"
{-# INLINEABLE lopaPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopaPageToken :: Lens.Lens' ListOrganizationPortfolioAccess (Core.Maybe Types.PageToken)
lopaPageToken = Lens.field @"pageToken"
{-# INLINEABLE lopaPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListOrganizationPortfolioAccess where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListOrganizationPortfolioAccess where
        toHeaders ListOrganizationPortfolioAccess{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.ListOrganizationPortfolioAccess")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListOrganizationPortfolioAccess where
        toJSON ListOrganizationPortfolioAccess{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PortfolioId" Core..= portfolioId),
                  Core.Just ("OrganizationNodeType" Core..= organizationNodeType),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest ListOrganizationPortfolioAccess where
        type Rs ListOrganizationPortfolioAccess =
             ListOrganizationPortfolioAccessResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListOrganizationPortfolioAccessResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*>
                     x Core..:? "OrganizationNodes"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListOrganizationPortfolioAccess where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"organizationNodes" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkListOrganizationPortfolioAccessResponse' smart constructor.
data ListOrganizationPortfolioAccessResponse = ListOrganizationPortfolioAccessResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , organizationNodes :: Core.Maybe [Types.OrganizationNode]
    -- ^ Displays information about the organization nodes.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOrganizationPortfolioAccessResponse' value with any optional fields omitted.
mkListOrganizationPortfolioAccessResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListOrganizationPortfolioAccessResponse
mkListOrganizationPortfolioAccessResponse responseStatus
  = ListOrganizationPortfolioAccessResponse'{nextPageToken =
                                               Core.Nothing,
                                             organizationNodes = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loparrsNextPageToken :: Lens.Lens' ListOrganizationPortfolioAccessResponse (Core.Maybe Types.NextPageToken)
loparrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE loparrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Displays information about the organization nodes.
--
-- /Note:/ Consider using 'organizationNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loparrsOrganizationNodes :: Lens.Lens' ListOrganizationPortfolioAccessResponse (Core.Maybe [Types.OrganizationNode])
loparrsOrganizationNodes = Lens.field @"organizationNodes"
{-# INLINEABLE loparrsOrganizationNodes #-}
{-# DEPRECATED organizationNodes "Use generic-lens or generic-optics with 'organizationNodes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loparrsResponseStatus :: Lens.Lens' ListOrganizationPortfolioAccessResponse Core.Int
loparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE loparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

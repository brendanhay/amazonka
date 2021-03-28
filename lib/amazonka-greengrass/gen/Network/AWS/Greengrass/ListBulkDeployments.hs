{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListBulkDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of bulk deployments.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListBulkDeployments
    (
    -- * Creating a request
      ListBulkDeployments (..)
    , mkListBulkDeployments
    -- ** Request lenses
    , lbdMaxResults
    , lbdNextToken

    -- * Destructuring the response
    , ListBulkDeploymentsResponse (..)
    , mkListBulkDeploymentsResponse
    -- ** Response lenses
    , lbdrrsBulkDeployments
    , lbdrrsNextToken
    , lbdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBulkDeployments' smart constructor.
data ListBulkDeployments = ListBulkDeployments'
  { maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBulkDeployments' value with any optional fields omitted.
mkListBulkDeployments
    :: ListBulkDeployments
mkListBulkDeployments
  = ListBulkDeployments'{maxResults = Core.Nothing,
                         nextToken = Core.Nothing}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdMaxResults :: Lens.Lens' ListBulkDeployments (Core.Maybe Core.Text)
lbdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lbdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdNextToken :: Lens.Lens' ListBulkDeployments (Core.Maybe Core.Text)
lbdNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListBulkDeployments where
        toQuery ListBulkDeployments{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListBulkDeployments where
        toHeaders ListBulkDeployments{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListBulkDeployments where
        type Rs ListBulkDeployments = ListBulkDeploymentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/greengrass/bulk/deployments",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListBulkDeploymentsResponse' Core.<$>
                   (x Core..:? "BulkDeployments") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListBulkDeployments where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"bulkDeployments" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListBulkDeploymentsResponse' smart constructor.
data ListBulkDeploymentsResponse = ListBulkDeploymentsResponse'
  { bulkDeployments :: Core.Maybe [Types.BulkDeployment]
    -- ^ A list of bulk deployments.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBulkDeploymentsResponse' value with any optional fields omitted.
mkListBulkDeploymentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBulkDeploymentsResponse
mkListBulkDeploymentsResponse responseStatus
  = ListBulkDeploymentsResponse'{bulkDeployments = Core.Nothing,
                                 nextToken = Core.Nothing, responseStatus}

-- | A list of bulk deployments.
--
-- /Note:/ Consider using 'bulkDeployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdrrsBulkDeployments :: Lens.Lens' ListBulkDeploymentsResponse (Core.Maybe [Types.BulkDeployment])
lbdrrsBulkDeployments = Lens.field @"bulkDeployments"
{-# INLINEABLE lbdrrsBulkDeployments #-}
{-# DEPRECATED bulkDeployments "Use generic-lens or generic-optics with 'bulkDeployments' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdrrsNextToken :: Lens.Lens' ListBulkDeploymentsResponse (Core.Maybe Core.Text)
lbdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdrrsResponseStatus :: Lens.Lens' ListBulkDeploymentsResponse Core.Int
lbdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

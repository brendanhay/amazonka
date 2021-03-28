{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListBulkDeploymentDetailedReports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of the deployments that have been started in a bulk deployment operation, and their current deployment status.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListBulkDeploymentDetailedReports
    (
    -- * Creating a request
      ListBulkDeploymentDetailedReports (..)
    , mkListBulkDeploymentDetailedReports
    -- ** Request lenses
    , lbddrBulkDeploymentId
    , lbddrMaxResults
    , lbddrNextToken

    -- * Destructuring the response
    , ListBulkDeploymentDetailedReportsResponse (..)
    , mkListBulkDeploymentDetailedReportsResponse
    -- ** Response lenses
    , lbddrrrsDeployments
    , lbddrrrsNextToken
    , lbddrrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBulkDeploymentDetailedReports' smart constructor.
data ListBulkDeploymentDetailedReports = ListBulkDeploymentDetailedReports'
  { bulkDeploymentId :: Core.Text
    -- ^ The ID of the bulk deployment.
  , maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBulkDeploymentDetailedReports' value with any optional fields omitted.
mkListBulkDeploymentDetailedReports
    :: Core.Text -- ^ 'bulkDeploymentId'
    -> ListBulkDeploymentDetailedReports
mkListBulkDeploymentDetailedReports bulkDeploymentId
  = ListBulkDeploymentDetailedReports'{bulkDeploymentId,
                                       maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrBulkDeploymentId :: Lens.Lens' ListBulkDeploymentDetailedReports Core.Text
lbddrBulkDeploymentId = Lens.field @"bulkDeploymentId"
{-# INLINEABLE lbddrBulkDeploymentId #-}
{-# DEPRECATED bulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead"  #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrMaxResults :: Lens.Lens' ListBulkDeploymentDetailedReports (Core.Maybe Core.Text)
lbddrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lbddrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrNextToken :: Lens.Lens' ListBulkDeploymentDetailedReports (Core.Maybe Core.Text)
lbddrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbddrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListBulkDeploymentDetailedReports where
        toQuery ListBulkDeploymentDetailedReports{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListBulkDeploymentDetailedReports where
        toHeaders ListBulkDeploymentDetailedReports{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListBulkDeploymentDetailedReports where
        type Rs ListBulkDeploymentDetailedReports =
             ListBulkDeploymentDetailedReportsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/bulk/deployments/" Core.<>
                             Core.toText bulkDeploymentId
                             Core.<> "/detailed-reports",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListBulkDeploymentDetailedReportsResponse' Core.<$>
                   (x Core..:? "Deployments") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListBulkDeploymentDetailedReports where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"deployments" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListBulkDeploymentDetailedReportsResponse' smart constructor.
data ListBulkDeploymentDetailedReportsResponse = ListBulkDeploymentDetailedReportsResponse'
  { deployments :: Core.Maybe [Types.BulkDeploymentResult]
    -- ^ A list of the individual group deployments in the bulk deployment operation.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBulkDeploymentDetailedReportsResponse' value with any optional fields omitted.
mkListBulkDeploymentDetailedReportsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBulkDeploymentDetailedReportsResponse
mkListBulkDeploymentDetailedReportsResponse responseStatus
  = ListBulkDeploymentDetailedReportsResponse'{deployments =
                                                 Core.Nothing,
                                               nextToken = Core.Nothing, responseStatus}

-- | A list of the individual group deployments in the bulk deployment operation.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrrrsDeployments :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse (Core.Maybe [Types.BulkDeploymentResult])
lbddrrrsDeployments = Lens.field @"deployments"
{-# INLINEABLE lbddrrrsDeployments #-}
{-# DEPRECATED deployments "Use generic-lens or generic-optics with 'deployments' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrrrsNextToken :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse (Core.Maybe Core.Text)
lbddrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbddrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrrrsResponseStatus :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse Core.Int
lbddrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbddrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListBulkDeployments (..),
    mkListBulkDeployments,

    -- ** Request lenses
    lbdMaxResults,
    lbdNextToken,

    -- * Destructuring the response
    ListBulkDeploymentsResponse (..),
    mkListBulkDeploymentsResponse,

    -- ** Response lenses
    lbdrrsBulkDeployments,
    lbdrrsNextToken,
    lbdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBulkDeployments' smart constructor.
data ListBulkDeployments = ListBulkDeployments'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBulkDeployments' value with any optional fields omitted.
mkListBulkDeployments ::
  ListBulkDeployments
mkListBulkDeployments =
  ListBulkDeployments'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdMaxResults :: Lens.Lens' ListBulkDeployments (Core.Maybe Core.Text)
lbdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lbdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdNextToken :: Lens.Lens' ListBulkDeployments (Core.Maybe Core.Text)
lbdNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListBulkDeployments where
  type Rs ListBulkDeployments = ListBulkDeploymentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/greengrass/bulk/deployments",
        Core._rqQuery =
          Core.toQueryValue "MaxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBulkDeploymentsResponse'
            Core.<$> (x Core..:? "BulkDeployments")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListBulkDeployments where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"bulkDeployments" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListBulkDeploymentsResponse' smart constructor.
data ListBulkDeploymentsResponse = ListBulkDeploymentsResponse'
  { -- | A list of bulk deployments.
    bulkDeployments :: Core.Maybe [Types.BulkDeployment],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBulkDeploymentsResponse' value with any optional fields omitted.
mkListBulkDeploymentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListBulkDeploymentsResponse
mkListBulkDeploymentsResponse responseStatus =
  ListBulkDeploymentsResponse'
    { bulkDeployments = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of bulk deployments.
--
-- /Note:/ Consider using 'bulkDeployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdrrsBulkDeployments :: Lens.Lens' ListBulkDeploymentsResponse (Core.Maybe [Types.BulkDeployment])
lbdrrsBulkDeployments = Lens.field @"bulkDeployments"
{-# DEPRECATED lbdrrsBulkDeployments "Use generic-lens or generic-optics with 'bulkDeployments' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdrrsNextToken :: Lens.Lens' ListBulkDeploymentsResponse (Core.Maybe Core.Text)
lbdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdrrsResponseStatus :: Lens.Lens' ListBulkDeploymentsResponse Core.Int
lbdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lbdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

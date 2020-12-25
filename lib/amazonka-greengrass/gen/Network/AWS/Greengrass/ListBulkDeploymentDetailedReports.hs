{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListBulkDeploymentDetailedReports (..),
    mkListBulkDeploymentDetailedReports,

    -- ** Request lenses
    lbddrBulkDeploymentId,
    lbddrMaxResults,
    lbddrNextToken,

    -- * Destructuring the response
    ListBulkDeploymentDetailedReportsResponse (..),
    mkListBulkDeploymentDetailedReportsResponse,

    -- ** Response lenses
    lbddrrrsDeployments,
    lbddrrrsNextToken,
    lbddrrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBulkDeploymentDetailedReports' smart constructor.
data ListBulkDeploymentDetailedReports = ListBulkDeploymentDetailedReports'
  { -- | The ID of the bulk deployment.
    bulkDeploymentId :: Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBulkDeploymentDetailedReports' value with any optional fields omitted.
mkListBulkDeploymentDetailedReports ::
  -- | 'bulkDeploymentId'
  Core.Text ->
  ListBulkDeploymentDetailedReports
mkListBulkDeploymentDetailedReports bulkDeploymentId =
  ListBulkDeploymentDetailedReports'
    { bulkDeploymentId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrBulkDeploymentId :: Lens.Lens' ListBulkDeploymentDetailedReports Core.Text
lbddrBulkDeploymentId = Lens.field @"bulkDeploymentId"
{-# DEPRECATED lbddrBulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrMaxResults :: Lens.Lens' ListBulkDeploymentDetailedReports (Core.Maybe Core.Text)
lbddrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lbddrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrNextToken :: Lens.Lens' ListBulkDeploymentDetailedReports (Core.Maybe Core.Text)
lbddrNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbddrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListBulkDeploymentDetailedReports where
  type
    Rs ListBulkDeploymentDetailedReports =
      ListBulkDeploymentDetailedReportsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/bulk/deployments/"
                Core.<> (Core.toText bulkDeploymentId)
                Core.<> ("/detailed-reports")
            ),
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
          ListBulkDeploymentDetailedReportsResponse'
            Core.<$> (x Core..:? "Deployments")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListBulkDeploymentDetailedReports where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"deployments" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListBulkDeploymentDetailedReportsResponse' smart constructor.
data ListBulkDeploymentDetailedReportsResponse = ListBulkDeploymentDetailedReportsResponse'
  { -- | A list of the individual group deployments in the bulk deployment operation.
    deployments :: Core.Maybe [Types.BulkDeploymentResult],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBulkDeploymentDetailedReportsResponse' value with any optional fields omitted.
mkListBulkDeploymentDetailedReportsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListBulkDeploymentDetailedReportsResponse
mkListBulkDeploymentDetailedReportsResponse responseStatus =
  ListBulkDeploymentDetailedReportsResponse'
    { deployments =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of the individual group deployments in the bulk deployment operation.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrrrsDeployments :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse (Core.Maybe [Types.BulkDeploymentResult])
lbddrrrsDeployments = Lens.field @"deployments"
{-# DEPRECATED lbddrrrsDeployments "Use generic-lens or generic-optics with 'deployments' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrrrsNextToken :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse (Core.Maybe Core.Text)
lbddrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbddrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbddrrrsResponseStatus :: Lens.Lens' ListBulkDeploymentDetailedReportsResponse Core.Int
lbddrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lbddrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

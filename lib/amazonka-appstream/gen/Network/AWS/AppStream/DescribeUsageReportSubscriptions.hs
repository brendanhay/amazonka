{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeUsageReportSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more usage report subscriptions.
module Network.AWS.AppStream.DescribeUsageReportSubscriptions
  ( -- * Creating a request
    DescribeUsageReportSubscriptions (..),
    mkDescribeUsageReportSubscriptions,

    -- ** Request lenses
    dursMaxResults,
    dursNextToken,

    -- * Destructuring the response
    DescribeUsageReportSubscriptionsResponse (..),
    mkDescribeUsageReportSubscriptionsResponse,

    -- ** Response lenses
    dursrrsNextToken,
    dursrrsUsageReportSubscriptions,
    dursrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUsageReportSubscriptions' smart constructor.
data DescribeUsageReportSubscriptions = DescribeUsageReportSubscriptions'
  { -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Int,
    -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUsageReportSubscriptions' value with any optional fields omitted.
mkDescribeUsageReportSubscriptions ::
  DescribeUsageReportSubscriptions
mkDescribeUsageReportSubscriptions =
  DescribeUsageReportSubscriptions'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursMaxResults :: Lens.Lens' DescribeUsageReportSubscriptions (Core.Maybe Core.Int)
dursMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dursMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursNextToken :: Lens.Lens' DescribeUsageReportSubscriptions (Core.Maybe Types.NextToken)
dursNextToken = Lens.field @"nextToken"
{-# DEPRECATED dursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeUsageReportSubscriptions where
  toJSON DescribeUsageReportSubscriptions {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeUsageReportSubscriptions where
  type
    Rs DescribeUsageReportSubscriptions =
      DescribeUsageReportSubscriptionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "PhotonAdminProxyService.DescribeUsageReportSubscriptions"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUsageReportSubscriptionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "UsageReportSubscriptions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeUsageReportSubscriptionsResponse' smart constructor.
data DescribeUsageReportSubscriptionsResponse = DescribeUsageReportSubscriptionsResponse'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the usage report subscription.
    usageReportSubscriptions :: Core.Maybe [Types.UsageReportSubscription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeUsageReportSubscriptionsResponse' value with any optional fields omitted.
mkDescribeUsageReportSubscriptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUsageReportSubscriptionsResponse
mkDescribeUsageReportSubscriptionsResponse responseStatus =
  DescribeUsageReportSubscriptionsResponse'
    { nextToken =
        Core.Nothing,
      usageReportSubscriptions = Core.Nothing,
      responseStatus
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursrrsNextToken :: Lens.Lens' DescribeUsageReportSubscriptionsResponse (Core.Maybe Types.String)
dursrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dursrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the usage report subscription.
--
-- /Note:/ Consider using 'usageReportSubscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursrrsUsageReportSubscriptions :: Lens.Lens' DescribeUsageReportSubscriptionsResponse (Core.Maybe [Types.UsageReportSubscription])
dursrrsUsageReportSubscriptions = Lens.field @"usageReportSubscriptions"
{-# DEPRECATED dursrrsUsageReportSubscriptions "Use generic-lens or generic-optics with 'usageReportSubscriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursrrsResponseStatus :: Lens.Lens' DescribeUsageReportSubscriptionsResponse Core.Int
dursrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dursrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

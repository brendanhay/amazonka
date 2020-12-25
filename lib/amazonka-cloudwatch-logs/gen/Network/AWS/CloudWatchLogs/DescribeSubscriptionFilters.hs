{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the subscription filters for the specified log group. You can list all the subscription filters or filter the results by prefix. The results are ASCII-sorted by filter name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
  ( -- * Creating a request
    DescribeSubscriptionFilters (..),
    mkDescribeSubscriptionFilters,

    -- ** Request lenses
    dsfLogGroupName,
    dsfFilterNamePrefix,
    dsfLimit,
    dsfNextToken,

    -- * Destructuring the response
    DescribeSubscriptionFiltersResponse (..),
    mkDescribeSubscriptionFiltersResponse,

    -- ** Response lenses
    dsfrrsNextToken,
    dsfrrsSubscriptionFilters,
    dsfrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSubscriptionFilters' smart constructor.
data DescribeSubscriptionFilters = DescribeSubscriptionFilters'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    -- | The prefix to match. If you don't specify a value, no prefix filter is applied.
    filterNamePrefix :: Core.Maybe Types.FilterNamePrefix,
    -- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
    limit :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscriptionFilters' value with any optional fields omitted.
mkDescribeSubscriptionFilters ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  DescribeSubscriptionFilters
mkDescribeSubscriptionFilters logGroupName =
  DescribeSubscriptionFilters'
    { logGroupName,
      filterNamePrefix = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfLogGroupName :: Lens.Lens' DescribeSubscriptionFilters Types.LogGroupName
dsfLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED dsfLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The prefix to match. If you don't specify a value, no prefix filter is applied.
--
-- /Note:/ Consider using 'filterNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfFilterNamePrefix :: Lens.Lens' DescribeSubscriptionFilters (Core.Maybe Types.FilterNamePrefix)
dsfFilterNamePrefix = Lens.field @"filterNamePrefix"
{-# DEPRECATED dsfFilterNamePrefix "Use generic-lens or generic-optics with 'filterNamePrefix' instead." #-}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfLimit :: Lens.Lens' DescribeSubscriptionFilters (Core.Maybe Core.Natural)
dsfLimit = Lens.field @"limit"
{-# DEPRECATED dsfLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfNextToken :: Lens.Lens' DescribeSubscriptionFilters (Core.Maybe Types.NextToken)
dsfNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeSubscriptionFilters where
  toJSON DescribeSubscriptionFilters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            ("filterNamePrefix" Core..=) Core.<$> filterNamePrefix,
            ("limit" Core..=) Core.<$> limit,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeSubscriptionFilters where
  type
    Rs DescribeSubscriptionFilters =
      DescribeSubscriptionFiltersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Logs_20140328.DescribeSubscriptionFilters")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscriptionFiltersResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "subscriptionFilters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSubscriptionFilters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"subscriptionFilters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeSubscriptionFiltersResponse' smart constructor.
data DescribeSubscriptionFiltersResponse = DescribeSubscriptionFiltersResponse'
  { nextToken :: Core.Maybe Types.NextToken,
    -- | The subscription filters.
    subscriptionFilters :: Core.Maybe [Types.SubscriptionFilter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscriptionFiltersResponse' value with any optional fields omitted.
mkDescribeSubscriptionFiltersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSubscriptionFiltersResponse
mkDescribeSubscriptionFiltersResponse responseStatus =
  DescribeSubscriptionFiltersResponse'
    { nextToken = Core.Nothing,
      subscriptionFilters = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrrsNextToken :: Lens.Lens' DescribeSubscriptionFiltersResponse (Core.Maybe Types.NextToken)
dsfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The subscription filters.
--
-- /Note:/ Consider using 'subscriptionFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrrsSubscriptionFilters :: Lens.Lens' DescribeSubscriptionFiltersResponse (Core.Maybe [Types.SubscriptionFilter])
dsfrrsSubscriptionFilters = Lens.field @"subscriptionFilters"
{-# DEPRECATED dsfrrsSubscriptionFilters "Use generic-lens or generic-optics with 'subscriptionFilters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrrsResponseStatus :: Lens.Lens' DescribeSubscriptionFiltersResponse Core.Int
dsfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

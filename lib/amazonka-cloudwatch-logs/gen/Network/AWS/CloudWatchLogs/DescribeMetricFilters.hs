{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeMetricFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified metric filters. You can list all of the metric filters or filter the results by log name, prefix, metric name, or metric namespace. The results are ASCII-sorted by filter name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeMetricFilters
  ( -- * Creating a request
    DescribeMetricFilters (..),
    mkDescribeMetricFilters,

    -- ** Request lenses
    dmfFilterNamePrefix,
    dmfLimit,
    dmfLogGroupName,
    dmfMetricName,
    dmfMetricNamespace,
    dmfNextToken,

    -- * Destructuring the response
    DescribeMetricFiltersResponse (..),
    mkDescribeMetricFiltersResponse,

    -- ** Response lenses
    dmfrrsMetricFilters,
    dmfrrsNextToken,
    dmfrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeMetricFilters' smart constructor.
data DescribeMetricFilters = DescribeMetricFilters'
  { -- | The prefix to match. CloudWatch Logs uses the value you set here only if you also include the @logGroupName@ parameter in your request.
    filterNamePrefix :: Core.Maybe Types.FilterName,
    -- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
    limit :: Core.Maybe Core.Natural,
    -- | The name of the log group.
    logGroupName :: Core.Maybe Types.LogGroupName,
    -- | Filters results to include only those with the specified metric name. If you include this parameter in your request, you must also include the @metricNamespace@ parameter.
    metricName :: Core.Maybe Types.MetricName,
    -- | Filters results to include only those in the specified namespace. If you include this parameter in your request, you must also include the @metricName@ parameter.
    metricNamespace :: Core.Maybe Types.MetricNamespace,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMetricFilters' value with any optional fields omitted.
mkDescribeMetricFilters ::
  DescribeMetricFilters
mkDescribeMetricFilters =
  DescribeMetricFilters'
    { filterNamePrefix = Core.Nothing,
      limit = Core.Nothing,
      logGroupName = Core.Nothing,
      metricName = Core.Nothing,
      metricNamespace = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The prefix to match. CloudWatch Logs uses the value you set here only if you also include the @logGroupName@ parameter in your request.
--
-- /Note:/ Consider using 'filterNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfFilterNamePrefix :: Lens.Lens' DescribeMetricFilters (Core.Maybe Types.FilterName)
dmfFilterNamePrefix = Lens.field @"filterNamePrefix"
{-# DEPRECATED dmfFilterNamePrefix "Use generic-lens or generic-optics with 'filterNamePrefix' instead." #-}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfLimit :: Lens.Lens' DescribeMetricFilters (Core.Maybe Core.Natural)
dmfLimit = Lens.field @"limit"
{-# DEPRECATED dmfLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfLogGroupName :: Lens.Lens' DescribeMetricFilters (Core.Maybe Types.LogGroupName)
dmfLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED dmfLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | Filters results to include only those with the specified metric name. If you include this parameter in your request, you must also include the @metricNamespace@ parameter.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfMetricName :: Lens.Lens' DescribeMetricFilters (Core.Maybe Types.MetricName)
dmfMetricName = Lens.field @"metricName"
{-# DEPRECATED dmfMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | Filters results to include only those in the specified namespace. If you include this parameter in your request, you must also include the @metricName@ parameter.
--
-- /Note:/ Consider using 'metricNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfMetricNamespace :: Lens.Lens' DescribeMetricFilters (Core.Maybe Types.MetricNamespace)
dmfMetricNamespace = Lens.field @"metricNamespace"
{-# DEPRECATED dmfMetricNamespace "Use generic-lens or generic-optics with 'metricNamespace' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfNextToken :: Lens.Lens' DescribeMetricFilters (Core.Maybe Types.NextToken)
dmfNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeMetricFilters where
  toJSON DescribeMetricFilters {..} =
    Core.object
      ( Core.catMaybes
          [ ("filterNamePrefix" Core..=) Core.<$> filterNamePrefix,
            ("limit" Core..=) Core.<$> limit,
            ("logGroupName" Core..=) Core.<$> logGroupName,
            ("metricName" Core..=) Core.<$> metricName,
            ("metricNamespace" Core..=) Core.<$> metricNamespace,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeMetricFilters where
  type Rs DescribeMetricFilters = DescribeMetricFiltersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.DescribeMetricFilters")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMetricFiltersResponse'
            Core.<$> (x Core..:? "metricFilters")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeMetricFilters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"metricFilters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeMetricFiltersResponse' smart constructor.
data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse'
  { -- | The metric filters.
    metricFilters :: Core.Maybe [Types.MetricFilter],
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMetricFiltersResponse' value with any optional fields omitted.
mkDescribeMetricFiltersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMetricFiltersResponse
mkDescribeMetricFiltersResponse responseStatus =
  DescribeMetricFiltersResponse'
    { metricFilters = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The metric filters.
--
-- /Note:/ Consider using 'metricFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrrsMetricFilters :: Lens.Lens' DescribeMetricFiltersResponse (Core.Maybe [Types.MetricFilter])
dmfrrsMetricFilters = Lens.field @"metricFilters"
{-# DEPRECATED dmfrrsMetricFilters "Use generic-lens or generic-optics with 'metricFilters' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrrsNextToken :: Lens.Lens' DescribeMetricFiltersResponse (Core.Maybe Types.NextToken)
dmfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrrsResponseStatus :: Lens.Lens' DescribeMetricFiltersResponse Core.Int
dmfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

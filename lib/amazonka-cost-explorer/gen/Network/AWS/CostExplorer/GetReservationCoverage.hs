{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetReservationCoverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the reservation coverage for your account. This enables you to see how much of your Amazon Elastic Compute Cloud, Amazon ElastiCache, Amazon Relational Database Service, or Amazon Redshift usage is covered by a reservation. An organization's management account can see the coverage of the associated member accounts. This supports dimensions, Cost Categories, and nested expressions. For any time period, you can filter data about reservation usage by the following dimensions:
--
--
--     * AZ
--
--
--     * CACHE_ENGINE
--
--
--     * DATABASE_ENGINE
--
--
--     * DEPLOYMENT_OPTION
--
--
--     * INSTANCE_TYPE
--
--
--     * LINKED_ACCOUNT
--
--
--     * OPERATING_SYSTEM
--
--
--     * PLATFORM
--
--
--     * REGION
--
--
--     * SERVICE
--
--
--     * TAG
--
--
--     * TENANCY
--
--
-- To determine valid values for a dimension, use the @GetDimensionValues@ operation.
module Network.AWS.CostExplorer.GetReservationCoverage
  ( -- * Creating a request
    GetReservationCoverage (..),
    mkGetReservationCoverage,

    -- ** Request lenses
    grcTimePeriod,
    grcFilter,
    grcGranularity,
    grcGroupBy,
    grcMetrics,
    grcNextPageToken,

    -- * Destructuring the response
    GetReservationCoverageResponse (..),
    mkGetReservationCoverageResponse,

    -- ** Response lenses
    grcrrsCoveragesByTime,
    grcrrsNextPageToken,
    grcrrsTotal,
    grcrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | You can use the following request parameters to query for how much of your instance usage a reservation covered.
--
-- /See:/ 'mkGetReservationCoverage' smart constructor.
data GetReservationCoverage = GetReservationCoverage'
  { -- | The start and end dates of the period that you want to retrieve data about reservation coverage for. You can retrieve data for a maximum of 13 months: the last 12 months and the current month. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
    timePeriod :: Types.DateInterval,
    -- | Filters utilization data by dimensions. You can filter by the following dimensions:
    --
    --
    --     * AZ
    --
    --
    --     * CACHE_ENGINE
    --
    --
    --     * DATABASE_ENGINE
    --
    --
    --     * DEPLOYMENT_OPTION
    --
    --
    --     * INSTANCE_TYPE
    --
    --
    --     * LINKED_ACCOUNT
    --
    --
    --     * OPERATING_SYSTEM
    --
    --
    --     * PLATFORM
    --
    --
    --     * REGION
    --
    --
    --     * SERVICE
    --
    --
    --     * TAG
    --
    --
    --     * TENANCY
    --
    --
    -- @GetReservationCoverage@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension. You can nest only one level deep. If there are multiple values for a dimension, they are OR'd together.
    -- If you don't provide a @SERVICE@ filter, Cost Explorer defaults to EC2.
    -- Cost category is also supported.
    filter :: Core.Maybe Types.Expression,
    -- | The granularity of the AWS cost data for the reservation. Valid values are @MONTHLY@ and @DAILY@ .
    --
    -- If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ .
    -- The @GetReservationCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
    granularity :: Core.Maybe Types.Granularity,
    -- | You can group the data by the following attributes:
    --
    --
    --     * AZ
    --
    --
    --     * CACHE_ENGINE
    --
    --
    --     * DATABASE_ENGINE
    --
    --
    --     * DEPLOYMENT_OPTION
    --
    --
    --     * INSTANCE_TYPE
    --
    --
    --     * LINKED_ACCOUNT
    --
    --
    --     * OPERATING_SYSTEM
    --
    --
    --     * PLATFORM
    --
    --
    --     * REGION
    --
    --
    --     * TENANCY
    groupBy :: Core.Maybe [Types.GroupDefinition],
    -- | The measurement that you want your reservation coverage reported in.
    --
    -- Valid values are @Hour@ , @Unit@ , and @Cost@ . You can use multiple values in a request.
    metrics :: Core.Maybe [Types.MetricName],
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReservationCoverage' value with any optional fields omitted.
mkGetReservationCoverage ::
  -- | 'timePeriod'
  Types.DateInterval ->
  GetReservationCoverage
mkGetReservationCoverage timePeriod =
  GetReservationCoverage'
    { timePeriod,
      filter = Core.Nothing,
      granularity = Core.Nothing,
      groupBy = Core.Nothing,
      metrics = Core.Nothing,
      nextPageToken = Core.Nothing
    }

-- | The start and end dates of the period that you want to retrieve data about reservation coverage for. You can retrieve data for a maximum of 13 months: the last 12 months and the current month. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcTimePeriod :: Lens.Lens' GetReservationCoverage Types.DateInterval
grcTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED grcTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | Filters utilization data by dimensions. You can filter by the following dimensions:
--
--
--     * AZ
--
--
--     * CACHE_ENGINE
--
--
--     * DATABASE_ENGINE
--
--
--     * DEPLOYMENT_OPTION
--
--
--     * INSTANCE_TYPE
--
--
--     * LINKED_ACCOUNT
--
--
--     * OPERATING_SYSTEM
--
--
--     * PLATFORM
--
--
--     * REGION
--
--
--     * SERVICE
--
--
--     * TAG
--
--
--     * TENANCY
--
--
-- @GetReservationCoverage@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension. You can nest only one level deep. If there are multiple values for a dimension, they are OR'd together.
-- If you don't provide a @SERVICE@ filter, Cost Explorer defaults to EC2.
-- Cost category is also supported.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcFilter :: Lens.Lens' GetReservationCoverage (Core.Maybe Types.Expression)
grcFilter = Lens.field @"filter"
{-# DEPRECATED grcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The granularity of the AWS cost data for the reservation. Valid values are @MONTHLY@ and @DAILY@ .
--
-- If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ .
-- The @GetReservationCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcGranularity :: Lens.Lens' GetReservationCoverage (Core.Maybe Types.Granularity)
grcGranularity = Lens.field @"granularity"
{-# DEPRECATED grcGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | You can group the data by the following attributes:
--
--
--     * AZ
--
--
--     * CACHE_ENGINE
--
--
--     * DATABASE_ENGINE
--
--
--     * DEPLOYMENT_OPTION
--
--
--     * INSTANCE_TYPE
--
--
--     * LINKED_ACCOUNT
--
--
--     * OPERATING_SYSTEM
--
--
--     * PLATFORM
--
--
--     * REGION
--
--
--     * TENANCY
--
--
--
-- /Note:/ Consider using 'groupBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcGroupBy :: Lens.Lens' GetReservationCoverage (Core.Maybe [Types.GroupDefinition])
grcGroupBy = Lens.field @"groupBy"
{-# DEPRECATED grcGroupBy "Use generic-lens or generic-optics with 'groupBy' instead." #-}

-- | The measurement that you want your reservation coverage reported in.
--
-- Valid values are @Hour@ , @Unit@ , and @Cost@ . You can use multiple values in a request.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcMetrics :: Lens.Lens' GetReservationCoverage (Core.Maybe [Types.MetricName])
grcMetrics = Lens.field @"metrics"
{-# DEPRECATED grcMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcNextPageToken :: Lens.Lens' GetReservationCoverage (Core.Maybe Types.NextPageToken)
grcNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED grcNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

instance Core.FromJSON GetReservationCoverage where
  toJSON GetReservationCoverage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TimePeriod" Core..= timePeriod),
            ("Filter" Core..=) Core.<$> filter,
            ("Granularity" Core..=) Core.<$> granularity,
            ("GroupBy" Core..=) Core.<$> groupBy,
            ("Metrics" Core..=) Core.<$> metrics,
            ("NextPageToken" Core..=) Core.<$> nextPageToken
          ]
      )

instance Core.AWSRequest GetReservationCoverage where
  type Rs GetReservationCoverage = GetReservationCoverageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSInsightsIndexService.GetReservationCoverage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReservationCoverageResponse'
            Core.<$> (x Core..:? "CoveragesByTime" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "Total")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetReservationCoverageResponse' smart constructor.
data GetReservationCoverageResponse = GetReservationCoverageResponse'
  { -- | The amount of time that your reservations covered.
    coveragesByTime :: [Types.CoverageByTime],
    -- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The total amount of instance usage that a reservation covered.
    total :: Core.Maybe Types.Coverage,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReservationCoverageResponse' value with any optional fields omitted.
mkGetReservationCoverageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetReservationCoverageResponse
mkGetReservationCoverageResponse responseStatus =
  GetReservationCoverageResponse'
    { coveragesByTime = Core.mempty,
      nextPageToken = Core.Nothing,
      total = Core.Nothing,
      responseStatus
    }

-- | The amount of time that your reservations covered.
--
-- /Note:/ Consider using 'coveragesByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrrsCoveragesByTime :: Lens.Lens' GetReservationCoverageResponse [Types.CoverageByTime]
grcrrsCoveragesByTime = Lens.field @"coveragesByTime"
{-# DEPRECATED grcrrsCoveragesByTime "Use generic-lens or generic-optics with 'coveragesByTime' instead." #-}

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrrsNextPageToken :: Lens.Lens' GetReservationCoverageResponse (Core.Maybe Types.NextPageToken)
grcrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED grcrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The total amount of instance usage that a reservation covered.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrrsTotal :: Lens.Lens' GetReservationCoverageResponse (Core.Maybe Types.Coverage)
grcrrsTotal = Lens.field @"total"
{-# DEPRECATED grcrrsTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrrsResponseStatus :: Lens.Lens' GetReservationCoverageResponse Core.Int
grcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

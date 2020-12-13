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
    grcGroupBy,
    grcNextPageToken,
    grcMetrics,
    grcTimePeriod,
    grcGranularity,
    grcFilter,

    -- * Destructuring the response
    GetReservationCoverageResponse (..),
    mkGetReservationCoverageResponse,

    -- ** Response lenses
    grcrsNextPageToken,
    grcrsCoveragesByTime,
    grcrsTotal,
    grcrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | You can use the following request parameters to query for how much of your instance usage a reservation covered.
--
-- /See:/ 'mkGetReservationCoverage' smart constructor.
data GetReservationCoverage = GetReservationCoverage'
  { -- | You can group the data by the following attributes:
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
    groupBy :: Lude.Maybe [GroupDefinition],
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The measurement that you want your reservation coverage reported in.
    --
    -- Valid values are @Hour@ , @Unit@ , and @Cost@ . You can use multiple values in a request.
    metrics :: Lude.Maybe [Lude.Text],
    -- | The start and end dates of the period that you want to retrieve data about reservation coverage for. You can retrieve data for a maximum of 13 months: the last 12 months and the current month. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
    timePeriod :: DateInterval,
    -- | The granularity of the AWS cost data for the reservation. Valid values are @MONTHLY@ and @DAILY@ .
    --
    -- If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ .
    -- The @GetReservationCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
    granularity :: Lude.Maybe Granularity,
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
    filter :: Lude.Maybe Expression
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReservationCoverage' with the minimum fields required to make a request.
--
-- * 'groupBy' - You can group the data by the following attributes:
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
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'metrics' - The measurement that you want your reservation coverage reported in.
--
-- Valid values are @Hour@ , @Unit@ , and @Cost@ . You can use multiple values in a request.
-- * 'timePeriod' - The start and end dates of the period that you want to retrieve data about reservation coverage for. You can retrieve data for a maximum of 13 months: the last 12 months and the current month. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
-- * 'granularity' - The granularity of the AWS cost data for the reservation. Valid values are @MONTHLY@ and @DAILY@ .
--
-- If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ .
-- The @GetReservationCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
-- * 'filter' - Filters utilization data by dimensions. You can filter by the following dimensions:
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
mkGetReservationCoverage ::
  -- | 'timePeriod'
  DateInterval ->
  GetReservationCoverage
mkGetReservationCoverage pTimePeriod_ =
  GetReservationCoverage'
    { groupBy = Lude.Nothing,
      nextPageToken = Lude.Nothing,
      metrics = Lude.Nothing,
      timePeriod = pTimePeriod_,
      granularity = Lude.Nothing,
      filter = Lude.Nothing
    }

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
grcGroupBy :: Lens.Lens' GetReservationCoverage (Lude.Maybe [GroupDefinition])
grcGroupBy = Lens.lens (groupBy :: GetReservationCoverage -> Lude.Maybe [GroupDefinition]) (\s a -> s {groupBy = a} :: GetReservationCoverage)
{-# DEPRECATED grcGroupBy "Use generic-lens or generic-optics with 'groupBy' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcNextPageToken :: Lens.Lens' GetReservationCoverage (Lude.Maybe Lude.Text)
grcNextPageToken = Lens.lens (nextPageToken :: GetReservationCoverage -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetReservationCoverage)
{-# DEPRECATED grcNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The measurement that you want your reservation coverage reported in.
--
-- Valid values are @Hour@ , @Unit@ , and @Cost@ . You can use multiple values in a request.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcMetrics :: Lens.Lens' GetReservationCoverage (Lude.Maybe [Lude.Text])
grcMetrics = Lens.lens (metrics :: GetReservationCoverage -> Lude.Maybe [Lude.Text]) (\s a -> s {metrics = a} :: GetReservationCoverage)
{-# DEPRECATED grcMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The start and end dates of the period that you want to retrieve data about reservation coverage for. You can retrieve data for a maximum of 13 months: the last 12 months and the current month. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcTimePeriod :: Lens.Lens' GetReservationCoverage DateInterval
grcTimePeriod = Lens.lens (timePeriod :: GetReservationCoverage -> DateInterval) (\s a -> s {timePeriod = a} :: GetReservationCoverage)
{-# DEPRECATED grcTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The granularity of the AWS cost data for the reservation. Valid values are @MONTHLY@ and @DAILY@ .
--
-- If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ .
-- The @GetReservationCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcGranularity :: Lens.Lens' GetReservationCoverage (Lude.Maybe Granularity)
grcGranularity = Lens.lens (granularity :: GetReservationCoverage -> Lude.Maybe Granularity) (\s a -> s {granularity = a} :: GetReservationCoverage)
{-# DEPRECATED grcGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

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
grcFilter :: Lens.Lens' GetReservationCoverage (Lude.Maybe Expression)
grcFilter = Lens.lens (filter :: GetReservationCoverage -> Lude.Maybe Expression) (\s a -> s {filter = a} :: GetReservationCoverage)
{-# DEPRECATED grcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Lude.AWSRequest GetReservationCoverage where
  type Rs GetReservationCoverage = GetReservationCoverageResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetReservationCoverageResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "CoveragesByTime" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Total")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetReservationCoverage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.GetReservationCoverage" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetReservationCoverage where
  toJSON GetReservationCoverage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupBy" Lude..=) Lude.<$> groupBy,
            ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("Metrics" Lude..=) Lude.<$> metrics,
            Lude.Just ("TimePeriod" Lude..= timePeriod),
            ("Granularity" Lude..=) Lude.<$> granularity,
            ("Filter" Lude..=) Lude.<$> filter
          ]
      )

instance Lude.ToPath GetReservationCoverage where
  toPath = Lude.const "/"

instance Lude.ToQuery GetReservationCoverage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetReservationCoverageResponse' smart constructor.
data GetReservationCoverageResponse = GetReservationCoverageResponse'
  { -- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The amount of time that your reservations covered.
    coveragesByTime :: [CoverageByTime],
    -- | The total amount of instance usage that a reservation covered.
    total :: Lude.Maybe Coverage,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReservationCoverageResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'coveragesByTime' - The amount of time that your reservations covered.
-- * 'total' - The total amount of instance usage that a reservation covered.
-- * 'responseStatus' - The response status code.
mkGetReservationCoverageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetReservationCoverageResponse
mkGetReservationCoverageResponse pResponseStatus_ =
  GetReservationCoverageResponse'
    { nextPageToken = Lude.Nothing,
      coveragesByTime = Lude.mempty,
      total = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrsNextPageToken :: Lens.Lens' GetReservationCoverageResponse (Lude.Maybe Lude.Text)
grcrsNextPageToken = Lens.lens (nextPageToken :: GetReservationCoverageResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetReservationCoverageResponse)
{-# DEPRECATED grcrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The amount of time that your reservations covered.
--
-- /Note:/ Consider using 'coveragesByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrsCoveragesByTime :: Lens.Lens' GetReservationCoverageResponse [CoverageByTime]
grcrsCoveragesByTime = Lens.lens (coveragesByTime :: GetReservationCoverageResponse -> [CoverageByTime]) (\s a -> s {coveragesByTime = a} :: GetReservationCoverageResponse)
{-# DEPRECATED grcrsCoveragesByTime "Use generic-lens or generic-optics with 'coveragesByTime' instead." #-}

-- | The total amount of instance usage that a reservation covered.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrsTotal :: Lens.Lens' GetReservationCoverageResponse (Lude.Maybe Coverage)
grcrsTotal = Lens.lens (total :: GetReservationCoverageResponse -> Lude.Maybe Coverage) (\s a -> s {total = a} :: GetReservationCoverageResponse)
{-# DEPRECATED grcrsTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrsResponseStatus :: Lens.Lens' GetReservationCoverageResponse Lude.Int
grcrsResponseStatus = Lens.lens (responseStatus :: GetReservationCoverageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetReservationCoverageResponse)
{-# DEPRECATED grcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

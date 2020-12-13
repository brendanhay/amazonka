{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetReservationUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the reservation utilization for your account. Management account in an organization have access to member accounts. You can filter data by dimensions in a time period. You can use @GetDimensionValues@ to determine the possible dimension values. Currently, you can group only by @SUBSCRIPTION_ID@ .
module Network.AWS.CostExplorer.GetReservationUtilization
  ( -- * Creating a request
    GetReservationUtilization (..),
    mkGetReservationUtilization,

    -- ** Request lenses
    gruGroupBy,
    gruNextPageToken,
    gruTimePeriod,
    gruGranularity,
    gruFilter,

    -- * Destructuring the response
    GetReservationUtilizationResponse (..),
    mkGetReservationUtilizationResponse,

    -- ** Response lenses
    grursNextPageToken,
    grursUtilizationsByTime,
    grursTotal,
    grursResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetReservationUtilization' smart constructor.
data GetReservationUtilization = GetReservationUtilization'
  { -- | Groups only by @SUBSCRIPTION_ID@ . Metadata is included.
    groupBy :: Lude.Maybe [GroupDefinition],
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Sets the start and end dates for retrieving RI utilization. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
    timePeriod :: DateInterval,
    -- | If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ . If both @GroupBy@ and @Granularity@ aren't set, @GetReservationUtilization@ defaults to @DAILY@ .
    --
    -- The @GetReservationUtilization@ operation supports only @DAILY@ and @MONTHLY@ granularities.
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
    --     * SCOPE
    --
    --
    --     * TENANCY
    --
    --
    -- @GetReservationUtilization@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension, and nesting is supported up to only one level deep. If there are multiple values for a dimension, they are OR'd together.
    filter :: Lude.Maybe Expression
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReservationUtilization' with the minimum fields required to make a request.
--
-- * 'groupBy' - Groups only by @SUBSCRIPTION_ID@ . Metadata is included.
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'timePeriod' - Sets the start and end dates for retrieving RI utilization. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
-- * 'granularity' - If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ . If both @GroupBy@ and @Granularity@ aren't set, @GetReservationUtilization@ defaults to @DAILY@ .
--
-- The @GetReservationUtilization@ operation supports only @DAILY@ and @MONTHLY@ granularities.
-- * 'filter' - Filters utilization data by dimensions. You can filter by the following dimensions:
--
--
--     * AZ
--
--
--     * CACHE_ENGINE
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
--     * SCOPE
--
--
--     * TENANCY
--
--
-- @GetReservationUtilization@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension, and nesting is supported up to only one level deep. If there are multiple values for a dimension, they are OR'd together.
mkGetReservationUtilization ::
  -- | 'timePeriod'
  DateInterval ->
  GetReservationUtilization
mkGetReservationUtilization pTimePeriod_ =
  GetReservationUtilization'
    { groupBy = Lude.Nothing,
      nextPageToken = Lude.Nothing,
      timePeriod = pTimePeriod_,
      granularity = Lude.Nothing,
      filter = Lude.Nothing
    }

-- | Groups only by @SUBSCRIPTION_ID@ . Metadata is included.
--
-- /Note:/ Consider using 'groupBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gruGroupBy :: Lens.Lens' GetReservationUtilization (Lude.Maybe [GroupDefinition])
gruGroupBy = Lens.lens (groupBy :: GetReservationUtilization -> Lude.Maybe [GroupDefinition]) (\s a -> s {groupBy = a} :: GetReservationUtilization)
{-# DEPRECATED gruGroupBy "Use generic-lens or generic-optics with 'groupBy' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gruNextPageToken :: Lens.Lens' GetReservationUtilization (Lude.Maybe Lude.Text)
gruNextPageToken = Lens.lens (nextPageToken :: GetReservationUtilization -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetReservationUtilization)
{-# DEPRECATED gruNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Sets the start and end dates for retrieving RI utilization. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gruTimePeriod :: Lens.Lens' GetReservationUtilization DateInterval
gruTimePeriod = Lens.lens (timePeriod :: GetReservationUtilization -> DateInterval) (\s a -> s {timePeriod = a} :: GetReservationUtilization)
{-# DEPRECATED gruTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | If @GroupBy@ is set, @Granularity@ can't be set. If @Granularity@ isn't set, the response object doesn't include @Granularity@ , either @MONTHLY@ or @DAILY@ . If both @GroupBy@ and @Granularity@ aren't set, @GetReservationUtilization@ defaults to @DAILY@ .
--
-- The @GetReservationUtilization@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gruGranularity :: Lens.Lens' GetReservationUtilization (Lude.Maybe Granularity)
gruGranularity = Lens.lens (granularity :: GetReservationUtilization -> Lude.Maybe Granularity) (\s a -> s {granularity = a} :: GetReservationUtilization)
{-# DEPRECATED gruGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | Filters utilization data by dimensions. You can filter by the following dimensions:
--
--
--     * AZ
--
--
--     * CACHE_ENGINE
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
--     * SCOPE
--
--
--     * TENANCY
--
--
-- @GetReservationUtilization@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension, and nesting is supported up to only one level deep. If there are multiple values for a dimension, they are OR'd together.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gruFilter :: Lens.Lens' GetReservationUtilization (Lude.Maybe Expression)
gruFilter = Lens.lens (filter :: GetReservationUtilization -> Lude.Maybe Expression) (\s a -> s {filter = a} :: GetReservationUtilization)
{-# DEPRECATED gruFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Lude.AWSRequest GetReservationUtilization where
  type
    Rs GetReservationUtilization =
      GetReservationUtilizationResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetReservationUtilizationResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "UtilizationsByTime" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Total")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetReservationUtilization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.GetReservationUtilization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetReservationUtilization where
  toJSON GetReservationUtilization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupBy" Lude..=) Lude.<$> groupBy,
            ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            Lude.Just ("TimePeriod" Lude..= timePeriod),
            ("Granularity" Lude..=) Lude.<$> granularity,
            ("Filter" Lude..=) Lude.<$> filter
          ]
      )

instance Lude.ToPath GetReservationUtilization where
  toPath = Lude.const "/"

instance Lude.ToQuery GetReservationUtilization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetReservationUtilizationResponse' smart constructor.
data GetReservationUtilizationResponse = GetReservationUtilizationResponse'
  { -- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The amount of time that you used your RIs.
    utilizationsByTime :: [UtilizationByTime],
    -- | The total amount of time that you used your RIs.
    total :: Lude.Maybe ReservationAggregates,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReservationUtilizationResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'utilizationsByTime' - The amount of time that you used your RIs.
-- * 'total' - The total amount of time that you used your RIs.
-- * 'responseStatus' - The response status code.
mkGetReservationUtilizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetReservationUtilizationResponse
mkGetReservationUtilizationResponse pResponseStatus_ =
  GetReservationUtilizationResponse'
    { nextPageToken = Lude.Nothing,
      utilizationsByTime = Lude.mempty,
      total = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grursNextPageToken :: Lens.Lens' GetReservationUtilizationResponse (Lude.Maybe Lude.Text)
grursNextPageToken = Lens.lens (nextPageToken :: GetReservationUtilizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetReservationUtilizationResponse)
{-# DEPRECATED grursNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The amount of time that you used your RIs.
--
-- /Note:/ Consider using 'utilizationsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grursUtilizationsByTime :: Lens.Lens' GetReservationUtilizationResponse [UtilizationByTime]
grursUtilizationsByTime = Lens.lens (utilizationsByTime :: GetReservationUtilizationResponse -> [UtilizationByTime]) (\s a -> s {utilizationsByTime = a} :: GetReservationUtilizationResponse)
{-# DEPRECATED grursUtilizationsByTime "Use generic-lens or generic-optics with 'utilizationsByTime' instead." #-}

-- | The total amount of time that you used your RIs.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grursTotal :: Lens.Lens' GetReservationUtilizationResponse (Lude.Maybe ReservationAggregates)
grursTotal = Lens.lens (total :: GetReservationUtilizationResponse -> Lude.Maybe ReservationAggregates) (\s a -> s {total = a} :: GetReservationUtilizationResponse)
{-# DEPRECATED grursTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grursResponseStatus :: Lens.Lens' GetReservationUtilizationResponse Lude.Int
grursResponseStatus = Lens.lens (responseStatus :: GetReservationUtilizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetReservationUtilizationResponse)
{-# DEPRECATED grursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

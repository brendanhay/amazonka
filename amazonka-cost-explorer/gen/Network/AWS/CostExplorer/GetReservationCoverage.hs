{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetReservationCoverage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the reservation coverage for your account. This enables you to
-- see how much of your Amazon Elastic Compute Cloud, Amazon ElastiCache,
-- Amazon Relational Database Service, or Amazon Redshift usage is covered
-- by a reservation. An organization\'s management account can see the
-- coverage of the associated member accounts. This supports dimensions,
-- Cost Categories, and nested expressions. For any time period, you can
-- filter data about reservation usage by the following dimensions:
--
-- -   AZ
--
-- -   CACHE_ENGINE
--
-- -   DATABASE_ENGINE
--
-- -   DEPLOYMENT_OPTION
--
-- -   INSTANCE_TYPE
--
-- -   LINKED_ACCOUNT
--
-- -   OPERATING_SYSTEM
--
-- -   PLATFORM
--
-- -   REGION
--
-- -   SERVICE
--
-- -   TAG
--
-- -   TENANCY
--
-- To determine valid values for a dimension, use the @GetDimensionValues@
-- operation.
module Network.AWS.CostExplorer.GetReservationCoverage
  ( -- * Creating a Request
    GetReservationCoverage (..),
    newGetReservationCoverage,

    -- * Request Lenses
    getReservationCoverage_granularity,
    getReservationCoverage_maxResults,
    getReservationCoverage_nextPageToken,
    getReservationCoverage_metrics,
    getReservationCoverage_groupBy,
    getReservationCoverage_sortBy,
    getReservationCoverage_filter,
    getReservationCoverage_timePeriod,

    -- * Destructuring the Response
    GetReservationCoverageResponse (..),
    newGetReservationCoverageResponse,

    -- * Response Lenses
    getReservationCoverageResponse_total,
    getReservationCoverageResponse_nextPageToken,
    getReservationCoverageResponse_httpStatus,
    getReservationCoverageResponse_coveragesByTime,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | You can use the following request parameters to query for how much of
-- your instance usage a reservation covered.
--
-- /See:/ 'newGetReservationCoverage' smart constructor.
data GetReservationCoverage = GetReservationCoverage'
  { -- | The granularity of the AWS cost data for the reservation. Valid values
    -- are @MONTHLY@ and @DAILY@.
    --
    -- If @GroupBy@ is set, @Granularity@ can\'t be set. If @Granularity@
    -- isn\'t set, the response object doesn\'t include @Granularity@, either
    -- @MONTHLY@ or @DAILY@.
    --
    -- The @GetReservationCoverage@ operation supports only @DAILY@ and
    -- @MONTHLY@ granularities.
    granularity :: Prelude.Maybe Granularity,
    -- | The maximum number of objects that you returned for this request. If
    -- more objects are available, in the response, AWS provides a
    -- NextPageToken value that you can use in a subsequent call to get the
    -- next batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The measurement that you want your reservation coverage reported in.
    --
    -- Valid values are @Hour@, @Unit@, and @Cost@. You can use multiple values
    -- in a request.
    metrics :: Prelude.Maybe [Prelude.Text],
    -- | You can group the data by the following attributes:
    --
    -- -   AZ
    --
    -- -   CACHE_ENGINE
    --
    -- -   DATABASE_ENGINE
    --
    -- -   DEPLOYMENT_OPTION
    --
    -- -   INSTANCE_TYPE
    --
    -- -   LINKED_ACCOUNT
    --
    -- -   OPERATING_SYSTEM
    --
    -- -   PLATFORM
    --
    -- -   REGION
    --
    -- -   TENANCY
    groupBy :: Prelude.Maybe [GroupDefinition],
    -- | The value by which you want to sort the data.
    --
    -- The following values are supported for @Key@:
    --
    -- -   @OnDemandCost@
    --
    -- -   @CoverageHoursPercentage@
    --
    -- -   @OnDemandHours@
    --
    -- -   @ReservedHours@
    --
    -- -   @TotalRunningHours@
    --
    -- -   @CoverageNormalizedUnitsPercentage@
    --
    -- -   @OnDemandNormalizedUnits@
    --
    -- -   @ReservedNormalizedUnits@
    --
    -- -   @TotalRunningNormalizedUnits@
    --
    -- -   @Time@
    --
    -- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
    sortBy :: Prelude.Maybe SortDefinition,
    -- | Filters utilization data by dimensions. You can filter by the following
    -- dimensions:
    --
    -- -   AZ
    --
    -- -   CACHE_ENGINE
    --
    -- -   DATABASE_ENGINE
    --
    -- -   DEPLOYMENT_OPTION
    --
    -- -   INSTANCE_TYPE
    --
    -- -   LINKED_ACCOUNT
    --
    -- -   OPERATING_SYSTEM
    --
    -- -   PLATFORM
    --
    -- -   REGION
    --
    -- -   SERVICE
    --
    -- -   TAG
    --
    -- -   TENANCY
    --
    -- @GetReservationCoverage@ uses the same
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
    -- object as the other operations, but only @AND@ is supported among each
    -- dimension. You can nest only one level deep. If there are multiple
    -- values for a dimension, they are OR\'d together.
    --
    -- If you don\'t provide a @SERVICE@ filter, Cost Explorer defaults to EC2.
    --
    -- Cost category is also supported.
    filter' :: Prelude.Maybe Expression,
    -- | The start and end dates of the period that you want to retrieve data
    -- about reservation coverage for. You can retrieve data for a maximum of
    -- 13 months: the last 12 months and the current month. The start date is
    -- inclusive, but the end date is exclusive. For example, if @start@ is
    -- @2017-01-01@ and @end@ is @2017-05-01@, then the cost and usage data is
    -- retrieved from @2017-01-01@ up to and including @2017-04-30@ but not
    -- including @2017-05-01@.
    timePeriod :: DateInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReservationCoverage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'granularity', 'getReservationCoverage_granularity' - The granularity of the AWS cost data for the reservation. Valid values
-- are @MONTHLY@ and @DAILY@.
--
-- If @GroupBy@ is set, @Granularity@ can\'t be set. If @Granularity@
-- isn\'t set, the response object doesn\'t include @Granularity@, either
-- @MONTHLY@ or @DAILY@.
--
-- The @GetReservationCoverage@ operation supports only @DAILY@ and
-- @MONTHLY@ granularities.
--
-- 'maxResults', 'getReservationCoverage_maxResults' - The maximum number of objects that you returned for this request. If
-- more objects are available, in the response, AWS provides a
-- NextPageToken value that you can use in a subsequent call to get the
-- next batch of objects.
--
-- 'nextPageToken', 'getReservationCoverage_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
--
-- 'metrics', 'getReservationCoverage_metrics' - The measurement that you want your reservation coverage reported in.
--
-- Valid values are @Hour@, @Unit@, and @Cost@. You can use multiple values
-- in a request.
--
-- 'groupBy', 'getReservationCoverage_groupBy' - You can group the data by the following attributes:
--
-- -   AZ
--
-- -   CACHE_ENGINE
--
-- -   DATABASE_ENGINE
--
-- -   DEPLOYMENT_OPTION
--
-- -   INSTANCE_TYPE
--
-- -   LINKED_ACCOUNT
--
-- -   OPERATING_SYSTEM
--
-- -   PLATFORM
--
-- -   REGION
--
-- -   TENANCY
--
-- 'sortBy', 'getReservationCoverage_sortBy' - The value by which you want to sort the data.
--
-- The following values are supported for @Key@:
--
-- -   @OnDemandCost@
--
-- -   @CoverageHoursPercentage@
--
-- -   @OnDemandHours@
--
-- -   @ReservedHours@
--
-- -   @TotalRunningHours@
--
-- -   @CoverageNormalizedUnitsPercentage@
--
-- -   @OnDemandNormalizedUnits@
--
-- -   @ReservedNormalizedUnits@
--
-- -   @TotalRunningNormalizedUnits@
--
-- -   @Time@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
--
-- 'filter'', 'getReservationCoverage_filter' - Filters utilization data by dimensions. You can filter by the following
-- dimensions:
--
-- -   AZ
--
-- -   CACHE_ENGINE
--
-- -   DATABASE_ENGINE
--
-- -   DEPLOYMENT_OPTION
--
-- -   INSTANCE_TYPE
--
-- -   LINKED_ACCOUNT
--
-- -   OPERATING_SYSTEM
--
-- -   PLATFORM
--
-- -   REGION
--
-- -   SERVICE
--
-- -   TAG
--
-- -   TENANCY
--
-- @GetReservationCoverage@ uses the same
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object as the other operations, but only @AND@ is supported among each
-- dimension. You can nest only one level deep. If there are multiple
-- values for a dimension, they are OR\'d together.
--
-- If you don\'t provide a @SERVICE@ filter, Cost Explorer defaults to EC2.
--
-- Cost category is also supported.
--
-- 'timePeriod', 'getReservationCoverage_timePeriod' - The start and end dates of the period that you want to retrieve data
-- about reservation coverage for. You can retrieve data for a maximum of
-- 13 months: the last 12 months and the current month. The start date is
-- inclusive, but the end date is exclusive. For example, if @start@ is
-- @2017-01-01@ and @end@ is @2017-05-01@, then the cost and usage data is
-- retrieved from @2017-01-01@ up to and including @2017-04-30@ but not
-- including @2017-05-01@.
newGetReservationCoverage ::
  -- | 'timePeriod'
  DateInterval ->
  GetReservationCoverage
newGetReservationCoverage pTimePeriod_ =
  GetReservationCoverage'
    { granularity =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      metrics = Prelude.Nothing,
      groupBy = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      filter' = Prelude.Nothing,
      timePeriod = pTimePeriod_
    }

-- | The granularity of the AWS cost data for the reservation. Valid values
-- are @MONTHLY@ and @DAILY@.
--
-- If @GroupBy@ is set, @Granularity@ can\'t be set. If @Granularity@
-- isn\'t set, the response object doesn\'t include @Granularity@, either
-- @MONTHLY@ or @DAILY@.
--
-- The @GetReservationCoverage@ operation supports only @DAILY@ and
-- @MONTHLY@ granularities.
getReservationCoverage_granularity :: Lens.Lens' GetReservationCoverage (Prelude.Maybe Granularity)
getReservationCoverage_granularity = Lens.lens (\GetReservationCoverage' {granularity} -> granularity) (\s@GetReservationCoverage' {} a -> s {granularity = a} :: GetReservationCoverage)

-- | The maximum number of objects that you returned for this request. If
-- more objects are available, in the response, AWS provides a
-- NextPageToken value that you can use in a subsequent call to get the
-- next batch of objects.
getReservationCoverage_maxResults :: Lens.Lens' GetReservationCoverage (Prelude.Maybe Prelude.Natural)
getReservationCoverage_maxResults = Lens.lens (\GetReservationCoverage' {maxResults} -> maxResults) (\s@GetReservationCoverage' {} a -> s {maxResults = a} :: GetReservationCoverage)

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getReservationCoverage_nextPageToken :: Lens.Lens' GetReservationCoverage (Prelude.Maybe Prelude.Text)
getReservationCoverage_nextPageToken = Lens.lens (\GetReservationCoverage' {nextPageToken} -> nextPageToken) (\s@GetReservationCoverage' {} a -> s {nextPageToken = a} :: GetReservationCoverage)

-- | The measurement that you want your reservation coverage reported in.
--
-- Valid values are @Hour@, @Unit@, and @Cost@. You can use multiple values
-- in a request.
getReservationCoverage_metrics :: Lens.Lens' GetReservationCoverage (Prelude.Maybe [Prelude.Text])
getReservationCoverage_metrics = Lens.lens (\GetReservationCoverage' {metrics} -> metrics) (\s@GetReservationCoverage' {} a -> s {metrics = a} :: GetReservationCoverage) Prelude.. Lens.mapping Lens._Coerce

-- | You can group the data by the following attributes:
--
-- -   AZ
--
-- -   CACHE_ENGINE
--
-- -   DATABASE_ENGINE
--
-- -   DEPLOYMENT_OPTION
--
-- -   INSTANCE_TYPE
--
-- -   LINKED_ACCOUNT
--
-- -   OPERATING_SYSTEM
--
-- -   PLATFORM
--
-- -   REGION
--
-- -   TENANCY
getReservationCoverage_groupBy :: Lens.Lens' GetReservationCoverage (Prelude.Maybe [GroupDefinition])
getReservationCoverage_groupBy = Lens.lens (\GetReservationCoverage' {groupBy} -> groupBy) (\s@GetReservationCoverage' {} a -> s {groupBy = a} :: GetReservationCoverage) Prelude.. Lens.mapping Lens._Coerce

-- | The value by which you want to sort the data.
--
-- The following values are supported for @Key@:
--
-- -   @OnDemandCost@
--
-- -   @CoverageHoursPercentage@
--
-- -   @OnDemandHours@
--
-- -   @ReservedHours@
--
-- -   @TotalRunningHours@
--
-- -   @CoverageNormalizedUnitsPercentage@
--
-- -   @OnDemandNormalizedUnits@
--
-- -   @ReservedNormalizedUnits@
--
-- -   @TotalRunningNormalizedUnits@
--
-- -   @Time@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
getReservationCoverage_sortBy :: Lens.Lens' GetReservationCoverage (Prelude.Maybe SortDefinition)
getReservationCoverage_sortBy = Lens.lens (\GetReservationCoverage' {sortBy} -> sortBy) (\s@GetReservationCoverage' {} a -> s {sortBy = a} :: GetReservationCoverage)

-- | Filters utilization data by dimensions. You can filter by the following
-- dimensions:
--
-- -   AZ
--
-- -   CACHE_ENGINE
--
-- -   DATABASE_ENGINE
--
-- -   DEPLOYMENT_OPTION
--
-- -   INSTANCE_TYPE
--
-- -   LINKED_ACCOUNT
--
-- -   OPERATING_SYSTEM
--
-- -   PLATFORM
--
-- -   REGION
--
-- -   SERVICE
--
-- -   TAG
--
-- -   TENANCY
--
-- @GetReservationCoverage@ uses the same
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object as the other operations, but only @AND@ is supported among each
-- dimension. You can nest only one level deep. If there are multiple
-- values for a dimension, they are OR\'d together.
--
-- If you don\'t provide a @SERVICE@ filter, Cost Explorer defaults to EC2.
--
-- Cost category is also supported.
getReservationCoverage_filter :: Lens.Lens' GetReservationCoverage (Prelude.Maybe Expression)
getReservationCoverage_filter = Lens.lens (\GetReservationCoverage' {filter'} -> filter') (\s@GetReservationCoverage' {} a -> s {filter' = a} :: GetReservationCoverage)

-- | The start and end dates of the period that you want to retrieve data
-- about reservation coverage for. You can retrieve data for a maximum of
-- 13 months: the last 12 months and the current month. The start date is
-- inclusive, but the end date is exclusive. For example, if @start@ is
-- @2017-01-01@ and @end@ is @2017-05-01@, then the cost and usage data is
-- retrieved from @2017-01-01@ up to and including @2017-04-30@ but not
-- including @2017-05-01@.
getReservationCoverage_timePeriod :: Lens.Lens' GetReservationCoverage DateInterval
getReservationCoverage_timePeriod = Lens.lens (\GetReservationCoverage' {timePeriod} -> timePeriod) (\s@GetReservationCoverage' {} a -> s {timePeriod = a} :: GetReservationCoverage)

instance Core.AWSRequest GetReservationCoverage where
  type
    AWSResponse GetReservationCoverage =
      GetReservationCoverageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReservationCoverageResponse'
            Prelude.<$> (x Core..?> "Total")
            Prelude.<*> (x Core..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "CoveragesByTime"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetReservationCoverage

instance Prelude.NFData GetReservationCoverage

instance Core.ToHeaders GetReservationCoverage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetReservationCoverage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetReservationCoverage where
  toJSON GetReservationCoverage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Granularity" Core..=) Prelude.<$> granularity,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("NextPageToken" Core..=) Prelude.<$> nextPageToken,
            ("Metrics" Core..=) Prelude.<$> metrics,
            ("GroupBy" Core..=) Prelude.<$> groupBy,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("Filter" Core..=) Prelude.<$> filter',
            Prelude.Just ("TimePeriod" Core..= timePeriod)
          ]
      )

instance Core.ToPath GetReservationCoverage where
  toPath = Prelude.const "/"

instance Core.ToQuery GetReservationCoverage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReservationCoverageResponse' smart constructor.
data GetReservationCoverageResponse = GetReservationCoverageResponse'
  { -- | The total amount of instance usage that a reservation covered.
    total :: Prelude.Maybe Coverage,
    -- | The token for the next set of retrievable results. AWS provides the
    -- token when the response from a previous call has more results than the
    -- maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The amount of time that your reservations covered.
    coveragesByTime :: [CoverageByTime]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReservationCoverageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'total', 'getReservationCoverageResponse_total' - The total amount of instance usage that a reservation covered.
--
-- 'nextPageToken', 'getReservationCoverageResponse_nextPageToken' - The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
--
-- 'httpStatus', 'getReservationCoverageResponse_httpStatus' - The response's http status code.
--
-- 'coveragesByTime', 'getReservationCoverageResponse_coveragesByTime' - The amount of time that your reservations covered.
newGetReservationCoverageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReservationCoverageResponse
newGetReservationCoverageResponse pHttpStatus_ =
  GetReservationCoverageResponse'
    { total =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      coveragesByTime = Prelude.mempty
    }

-- | The total amount of instance usage that a reservation covered.
getReservationCoverageResponse_total :: Lens.Lens' GetReservationCoverageResponse (Prelude.Maybe Coverage)
getReservationCoverageResponse_total = Lens.lens (\GetReservationCoverageResponse' {total} -> total) (\s@GetReservationCoverageResponse' {} a -> s {total = a} :: GetReservationCoverageResponse)

-- | The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
getReservationCoverageResponse_nextPageToken :: Lens.Lens' GetReservationCoverageResponse (Prelude.Maybe Prelude.Text)
getReservationCoverageResponse_nextPageToken = Lens.lens (\GetReservationCoverageResponse' {nextPageToken} -> nextPageToken) (\s@GetReservationCoverageResponse' {} a -> s {nextPageToken = a} :: GetReservationCoverageResponse)

-- | The response's http status code.
getReservationCoverageResponse_httpStatus :: Lens.Lens' GetReservationCoverageResponse Prelude.Int
getReservationCoverageResponse_httpStatus = Lens.lens (\GetReservationCoverageResponse' {httpStatus} -> httpStatus) (\s@GetReservationCoverageResponse' {} a -> s {httpStatus = a} :: GetReservationCoverageResponse)

-- | The amount of time that your reservations covered.
getReservationCoverageResponse_coveragesByTime :: Lens.Lens' GetReservationCoverageResponse [CoverageByTime]
getReservationCoverageResponse_coveragesByTime = Lens.lens (\GetReservationCoverageResponse' {coveragesByTime} -> coveragesByTime) (\s@GetReservationCoverageResponse' {} a -> s {coveragesByTime = a} :: GetReservationCoverageResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    GetReservationCoverageResponse

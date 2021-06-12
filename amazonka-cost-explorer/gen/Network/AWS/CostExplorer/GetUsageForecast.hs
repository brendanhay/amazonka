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
-- Module      : Network.AWS.CostExplorer.GetUsageForecast
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a forecast for how much Amazon Web Services predicts that you
-- will use over the forecast time period that you select, based on your
-- past usage.
module Network.AWS.CostExplorer.GetUsageForecast
  ( -- * Creating a Request
    GetUsageForecast (..),
    newGetUsageForecast,

    -- * Request Lenses
    getUsageForecast_predictionIntervalLevel,
    getUsageForecast_filter,
    getUsageForecast_timePeriod,
    getUsageForecast_metric,
    getUsageForecast_granularity,

    -- * Destructuring the Response
    GetUsageForecastResponse (..),
    newGetUsageForecastResponse,

    -- * Response Lenses
    getUsageForecastResponse_forecastResultsByTime,
    getUsageForecastResponse_total,
    getUsageForecastResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetUsageForecast' smart constructor.
data GetUsageForecast = GetUsageForecast'
  { -- | Cost Explorer always returns the mean forecast as a single point. You
    -- can request a prediction interval around the mean by specifying a
    -- confidence level. The higher the confidence level, the more confident
    -- Cost Explorer is about the actual value falling in the prediction
    -- interval. Higher confidence levels result in wider prediction intervals.
    predictionIntervalLevel :: Core.Maybe Core.Natural,
    -- | The filters that you want to use to filter your forecast. The
    -- @GetUsageForecast@ API supports filtering by the following dimensions:
    --
    -- -   @AZ@
    --
    -- -   @INSTANCE_TYPE@
    --
    -- -   @LINKED_ACCOUNT@
    --
    -- -   @LINKED_ACCOUNT_NAME@
    --
    -- -   @OPERATION@
    --
    -- -   @PURCHASE_TYPE@
    --
    -- -   @REGION@
    --
    -- -   @SERVICE@
    --
    -- -   @USAGE_TYPE@
    --
    -- -   @USAGE_TYPE_GROUP@
    --
    -- -   @RECORD_TYPE@
    --
    -- -   @OPERATING_SYSTEM@
    --
    -- -   @TENANCY@
    --
    -- -   @SCOPE@
    --
    -- -   @PLATFORM@
    --
    -- -   @SUBSCRIPTION_ID@
    --
    -- -   @LEGAL_ENTITY_NAME@
    --
    -- -   @DEPLOYMENT_OPTION@
    --
    -- -   @DATABASE_ENGINE@
    --
    -- -   @INSTANCE_TYPE_FAMILY@
    --
    -- -   @BILLING_ENTITY@
    --
    -- -   @RESERVATION_ID@
    --
    -- -   @SAVINGS_PLAN_ARN@
    filter' :: Core.Maybe Expression,
    -- | The start and end dates of the period that you want to retrieve usage
    -- forecast for. The start date is inclusive, but the end date is
    -- exclusive. For example, if @start@ is @2017-01-01@ and @end@ is
    -- @2017-05-01@, then the cost and usage data is retrieved from
    -- @2017-01-01@ up to and including @2017-04-30@ but not including
    -- @2017-05-01@. The start date must be equal to or later than the current
    -- date to avoid a validation error.
    timePeriod :: DateInterval,
    -- | Which metric Cost Explorer uses to create your forecast.
    --
    -- Valid values for a @GetUsageForecast@ call are the following:
    --
    -- -   USAGE_QUANTITY
    --
    -- -   NORMALIZED_USAGE_AMOUNT
    metric :: Metric,
    -- | How granular you want the forecast to be. You can get 3 months of
    -- @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
    --
    -- The @GetUsageForecast@ operation supports only @DAILY@ and @MONTHLY@
    -- granularities.
    granularity :: Granularity
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUsageForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictionIntervalLevel', 'getUsageForecast_predictionIntervalLevel' - Cost Explorer always returns the mean forecast as a single point. You
-- can request a prediction interval around the mean by specifying a
-- confidence level. The higher the confidence level, the more confident
-- Cost Explorer is about the actual value falling in the prediction
-- interval. Higher confidence levels result in wider prediction intervals.
--
-- 'filter'', 'getUsageForecast_filter' - The filters that you want to use to filter your forecast. The
-- @GetUsageForecast@ API supports filtering by the following dimensions:
--
-- -   @AZ@
--
-- -   @INSTANCE_TYPE@
--
-- -   @LINKED_ACCOUNT@
--
-- -   @LINKED_ACCOUNT_NAME@
--
-- -   @OPERATION@
--
-- -   @PURCHASE_TYPE@
--
-- -   @REGION@
--
-- -   @SERVICE@
--
-- -   @USAGE_TYPE@
--
-- -   @USAGE_TYPE_GROUP@
--
-- -   @RECORD_TYPE@
--
-- -   @OPERATING_SYSTEM@
--
-- -   @TENANCY@
--
-- -   @SCOPE@
--
-- -   @PLATFORM@
--
-- -   @SUBSCRIPTION_ID@
--
-- -   @LEGAL_ENTITY_NAME@
--
-- -   @DEPLOYMENT_OPTION@
--
-- -   @DATABASE_ENGINE@
--
-- -   @INSTANCE_TYPE_FAMILY@
--
-- -   @BILLING_ENTITY@
--
-- -   @RESERVATION_ID@
--
-- -   @SAVINGS_PLAN_ARN@
--
-- 'timePeriod', 'getUsageForecast_timePeriod' - The start and end dates of the period that you want to retrieve usage
-- forecast for. The start date is inclusive, but the end date is
-- exclusive. For example, if @start@ is @2017-01-01@ and @end@ is
-- @2017-05-01@, then the cost and usage data is retrieved from
-- @2017-01-01@ up to and including @2017-04-30@ but not including
-- @2017-05-01@. The start date must be equal to or later than the current
-- date to avoid a validation error.
--
-- 'metric', 'getUsageForecast_metric' - Which metric Cost Explorer uses to create your forecast.
--
-- Valid values for a @GetUsageForecast@ call are the following:
--
-- -   USAGE_QUANTITY
--
-- -   NORMALIZED_USAGE_AMOUNT
--
-- 'granularity', 'getUsageForecast_granularity' - How granular you want the forecast to be. You can get 3 months of
-- @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetUsageForecast@ operation supports only @DAILY@ and @MONTHLY@
-- granularities.
newGetUsageForecast ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'metric'
  Metric ->
  -- | 'granularity'
  Granularity ->
  GetUsageForecast
newGetUsageForecast
  pTimePeriod_
  pMetric_
  pGranularity_ =
    GetUsageForecast'
      { predictionIntervalLevel =
          Core.Nothing,
        filter' = Core.Nothing,
        timePeriod = pTimePeriod_,
        metric = pMetric_,
        granularity = pGranularity_
      }

-- | Cost Explorer always returns the mean forecast as a single point. You
-- can request a prediction interval around the mean by specifying a
-- confidence level. The higher the confidence level, the more confident
-- Cost Explorer is about the actual value falling in the prediction
-- interval. Higher confidence levels result in wider prediction intervals.
getUsageForecast_predictionIntervalLevel :: Lens.Lens' GetUsageForecast (Core.Maybe Core.Natural)
getUsageForecast_predictionIntervalLevel = Lens.lens (\GetUsageForecast' {predictionIntervalLevel} -> predictionIntervalLevel) (\s@GetUsageForecast' {} a -> s {predictionIntervalLevel = a} :: GetUsageForecast)

-- | The filters that you want to use to filter your forecast. The
-- @GetUsageForecast@ API supports filtering by the following dimensions:
--
-- -   @AZ@
--
-- -   @INSTANCE_TYPE@
--
-- -   @LINKED_ACCOUNT@
--
-- -   @LINKED_ACCOUNT_NAME@
--
-- -   @OPERATION@
--
-- -   @PURCHASE_TYPE@
--
-- -   @REGION@
--
-- -   @SERVICE@
--
-- -   @USAGE_TYPE@
--
-- -   @USAGE_TYPE_GROUP@
--
-- -   @RECORD_TYPE@
--
-- -   @OPERATING_SYSTEM@
--
-- -   @TENANCY@
--
-- -   @SCOPE@
--
-- -   @PLATFORM@
--
-- -   @SUBSCRIPTION_ID@
--
-- -   @LEGAL_ENTITY_NAME@
--
-- -   @DEPLOYMENT_OPTION@
--
-- -   @DATABASE_ENGINE@
--
-- -   @INSTANCE_TYPE_FAMILY@
--
-- -   @BILLING_ENTITY@
--
-- -   @RESERVATION_ID@
--
-- -   @SAVINGS_PLAN_ARN@
getUsageForecast_filter :: Lens.Lens' GetUsageForecast (Core.Maybe Expression)
getUsageForecast_filter = Lens.lens (\GetUsageForecast' {filter'} -> filter') (\s@GetUsageForecast' {} a -> s {filter' = a} :: GetUsageForecast)

-- | The start and end dates of the period that you want to retrieve usage
-- forecast for. The start date is inclusive, but the end date is
-- exclusive. For example, if @start@ is @2017-01-01@ and @end@ is
-- @2017-05-01@, then the cost and usage data is retrieved from
-- @2017-01-01@ up to and including @2017-04-30@ but not including
-- @2017-05-01@. The start date must be equal to or later than the current
-- date to avoid a validation error.
getUsageForecast_timePeriod :: Lens.Lens' GetUsageForecast DateInterval
getUsageForecast_timePeriod = Lens.lens (\GetUsageForecast' {timePeriod} -> timePeriod) (\s@GetUsageForecast' {} a -> s {timePeriod = a} :: GetUsageForecast)

-- | Which metric Cost Explorer uses to create your forecast.
--
-- Valid values for a @GetUsageForecast@ call are the following:
--
-- -   USAGE_QUANTITY
--
-- -   NORMALIZED_USAGE_AMOUNT
getUsageForecast_metric :: Lens.Lens' GetUsageForecast Metric
getUsageForecast_metric = Lens.lens (\GetUsageForecast' {metric} -> metric) (\s@GetUsageForecast' {} a -> s {metric = a} :: GetUsageForecast)

-- | How granular you want the forecast to be. You can get 3 months of
-- @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetUsageForecast@ operation supports only @DAILY@ and @MONTHLY@
-- granularities.
getUsageForecast_granularity :: Lens.Lens' GetUsageForecast Granularity
getUsageForecast_granularity = Lens.lens (\GetUsageForecast' {granularity} -> granularity) (\s@GetUsageForecast' {} a -> s {granularity = a} :: GetUsageForecast)

instance Core.AWSRequest GetUsageForecast where
  type
    AWSResponse GetUsageForecast =
      GetUsageForecastResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsageForecastResponse'
            Core.<$> ( x Core..?> "ForecastResultsByTime"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Total")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetUsageForecast

instance Core.NFData GetUsageForecast

instance Core.ToHeaders GetUsageForecast where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetUsageForecast" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetUsageForecast where
  toJSON GetUsageForecast' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PredictionIntervalLevel" Core..=)
              Core.<$> predictionIntervalLevel,
            ("Filter" Core..=) Core.<$> filter',
            Core.Just ("TimePeriod" Core..= timePeriod),
            Core.Just ("Metric" Core..= metric),
            Core.Just ("Granularity" Core..= granularity)
          ]
      )

instance Core.ToPath GetUsageForecast where
  toPath = Core.const "/"

instance Core.ToQuery GetUsageForecast where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetUsageForecastResponse' smart constructor.
data GetUsageForecastResponse = GetUsageForecastResponse'
  { -- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a
    -- list of days. For @MONTHLY@ forecasts, this is a list of months.
    forecastResultsByTime :: Core.Maybe [ForecastResult],
    -- | How much you\'re forecasted to use over the forecast period.
    total :: Core.Maybe MetricValue,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUsageForecastResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastResultsByTime', 'getUsageForecastResponse_forecastResultsByTime' - The forecasts for your query, in order. For @DAILY@ forecasts, this is a
-- list of days. For @MONTHLY@ forecasts, this is a list of months.
--
-- 'total', 'getUsageForecastResponse_total' - How much you\'re forecasted to use over the forecast period.
--
-- 'httpStatus', 'getUsageForecastResponse_httpStatus' - The response's http status code.
newGetUsageForecastResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetUsageForecastResponse
newGetUsageForecastResponse pHttpStatus_ =
  GetUsageForecastResponse'
    { forecastResultsByTime =
        Core.Nothing,
      total = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a
-- list of days. For @MONTHLY@ forecasts, this is a list of months.
getUsageForecastResponse_forecastResultsByTime :: Lens.Lens' GetUsageForecastResponse (Core.Maybe [ForecastResult])
getUsageForecastResponse_forecastResultsByTime = Lens.lens (\GetUsageForecastResponse' {forecastResultsByTime} -> forecastResultsByTime) (\s@GetUsageForecastResponse' {} a -> s {forecastResultsByTime = a} :: GetUsageForecastResponse) Core.. Lens.mapping Lens._Coerce

-- | How much you\'re forecasted to use over the forecast period.
getUsageForecastResponse_total :: Lens.Lens' GetUsageForecastResponse (Core.Maybe MetricValue)
getUsageForecastResponse_total = Lens.lens (\GetUsageForecastResponse' {total} -> total) (\s@GetUsageForecastResponse' {} a -> s {total = a} :: GetUsageForecastResponse)

-- | The response's http status code.
getUsageForecastResponse_httpStatus :: Lens.Lens' GetUsageForecastResponse Core.Int
getUsageForecastResponse_httpStatus = Lens.lens (\GetUsageForecastResponse' {httpStatus} -> httpStatus) (\s@GetUsageForecastResponse' {} a -> s {httpStatus = a} :: GetUsageForecastResponse)

instance Core.NFData GetUsageForecastResponse

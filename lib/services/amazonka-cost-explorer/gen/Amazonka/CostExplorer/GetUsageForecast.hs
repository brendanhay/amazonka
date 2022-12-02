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
-- Module      : Amazonka.CostExplorer.GetUsageForecast
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a forecast for how much Amazon Web Services predicts that you
-- will use over the forecast time period that you select, based on your
-- past usage.
module Amazonka.CostExplorer.GetUsageForecast
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUsageForecast' smart constructor.
data GetUsageForecast = GetUsageForecast'
  { -- | Amazon Web Services Cost Explorer always returns the mean forecast as a
    -- single point. You can request a prediction interval around the mean by
    -- specifying a confidence level. The higher the confidence level, the more
    -- confident Cost Explorer is about the actual value falling in the
    -- prediction interval. Higher confidence levels result in wider prediction
    -- intervals.
    predictionIntervalLevel :: Prelude.Maybe Prelude.Natural,
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
    filter' :: Prelude.Maybe Expression,
    -- | The start and end dates of the period that you want to retrieve usage
    -- forecast for. The start date is included in the period, but the end date
    -- isn\'t included in the period. For example, if @start@ is @2017-01-01@
    -- and @end@ is @2017-05-01@, then the cost and usage data is retrieved
    -- from @2017-01-01@ up to and including @2017-04-30@ but not including
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsageForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictionIntervalLevel', 'getUsageForecast_predictionIntervalLevel' - Amazon Web Services Cost Explorer always returns the mean forecast as a
-- single point. You can request a prediction interval around the mean by
-- specifying a confidence level. The higher the confidence level, the more
-- confident Cost Explorer is about the actual value falling in the
-- prediction interval. Higher confidence levels result in wider prediction
-- intervals.
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
-- forecast for. The start date is included in the period, but the end date
-- isn\'t included in the period. For example, if @start@ is @2017-01-01@
-- and @end@ is @2017-05-01@, then the cost and usage data is retrieved
-- from @2017-01-01@ up to and including @2017-04-30@ but not including
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
          Prelude.Nothing,
        filter' = Prelude.Nothing,
        timePeriod = pTimePeriod_,
        metric = pMetric_,
        granularity = pGranularity_
      }

-- | Amazon Web Services Cost Explorer always returns the mean forecast as a
-- single point. You can request a prediction interval around the mean by
-- specifying a confidence level. The higher the confidence level, the more
-- confident Cost Explorer is about the actual value falling in the
-- prediction interval. Higher confidence levels result in wider prediction
-- intervals.
getUsageForecast_predictionIntervalLevel :: Lens.Lens' GetUsageForecast (Prelude.Maybe Prelude.Natural)
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
getUsageForecast_filter :: Lens.Lens' GetUsageForecast (Prelude.Maybe Expression)
getUsageForecast_filter = Lens.lens (\GetUsageForecast' {filter'} -> filter') (\s@GetUsageForecast' {} a -> s {filter' = a} :: GetUsageForecast)

-- | The start and end dates of the period that you want to retrieve usage
-- forecast for. The start date is included in the period, but the end date
-- isn\'t included in the period. For example, if @start@ is @2017-01-01@
-- and @end@ is @2017-05-01@, then the cost and usage data is retrieved
-- from @2017-01-01@ up to and including @2017-04-30@ but not including
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsageForecastResponse'
            Prelude.<$> ( x Data..?> "ForecastResultsByTime"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Total")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUsageForecast where
  hashWithSalt _salt GetUsageForecast' {..} =
    _salt
      `Prelude.hashWithSalt` predictionIntervalLevel
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` granularity

instance Prelude.NFData GetUsageForecast where
  rnf GetUsageForecast' {..} =
    Prelude.rnf predictionIntervalLevel
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf granularity

instance Data.ToHeaders GetUsageForecast where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.GetUsageForecast" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetUsageForecast where
  toJSON GetUsageForecast' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PredictionIntervalLevel" Data..=)
              Prelude.<$> predictionIntervalLevel,
            ("Filter" Data..=) Prelude.<$> filter',
            Prelude.Just ("TimePeriod" Data..= timePeriod),
            Prelude.Just ("Metric" Data..= metric),
            Prelude.Just ("Granularity" Data..= granularity)
          ]
      )

instance Data.ToPath GetUsageForecast where
  toPath = Prelude.const "/"

instance Data.ToQuery GetUsageForecast where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUsageForecastResponse' smart constructor.
data GetUsageForecastResponse = GetUsageForecastResponse'
  { -- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a
    -- list of days. For @MONTHLY@ forecasts, this is a list of months.
    forecastResultsByTime :: Prelude.Maybe [ForecastResult],
    -- | How much you\'re forecasted to use over the forecast period.
    total :: Prelude.Maybe MetricValue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetUsageForecastResponse
newGetUsageForecastResponse pHttpStatus_ =
  GetUsageForecastResponse'
    { forecastResultsByTime =
        Prelude.Nothing,
      total = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a
-- list of days. For @MONTHLY@ forecasts, this is a list of months.
getUsageForecastResponse_forecastResultsByTime :: Lens.Lens' GetUsageForecastResponse (Prelude.Maybe [ForecastResult])
getUsageForecastResponse_forecastResultsByTime = Lens.lens (\GetUsageForecastResponse' {forecastResultsByTime} -> forecastResultsByTime) (\s@GetUsageForecastResponse' {} a -> s {forecastResultsByTime = a} :: GetUsageForecastResponse) Prelude.. Lens.mapping Lens.coerced

-- | How much you\'re forecasted to use over the forecast period.
getUsageForecastResponse_total :: Lens.Lens' GetUsageForecastResponse (Prelude.Maybe MetricValue)
getUsageForecastResponse_total = Lens.lens (\GetUsageForecastResponse' {total} -> total) (\s@GetUsageForecastResponse' {} a -> s {total = a} :: GetUsageForecastResponse)

-- | The response's http status code.
getUsageForecastResponse_httpStatus :: Lens.Lens' GetUsageForecastResponse Prelude.Int
getUsageForecastResponse_httpStatus = Lens.lens (\GetUsageForecastResponse' {httpStatus} -> httpStatus) (\s@GetUsageForecastResponse' {} a -> s {httpStatus = a} :: GetUsageForecastResponse)

instance Prelude.NFData GetUsageForecastResponse where
  rnf GetUsageForecastResponse' {..} =
    Prelude.rnf forecastResultsByTime
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf httpStatus

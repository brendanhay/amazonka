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
-- Module      : Amazonka.CostExplorer.GetCostForecast
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a forecast for how much Amazon Web Services predicts that you
-- will spend over the forecast time period that you select, based on your
-- past costs.
module Amazonka.CostExplorer.GetCostForecast
  ( -- * Creating a Request
    GetCostForecast (..),
    newGetCostForecast,

    -- * Request Lenses
    getCostForecast_filter,
    getCostForecast_predictionIntervalLevel,
    getCostForecast_timePeriod,
    getCostForecast_metric,
    getCostForecast_granularity,

    -- * Destructuring the Response
    GetCostForecastResponse (..),
    newGetCostForecastResponse,

    -- * Response Lenses
    getCostForecastResponse_forecastResultsByTime,
    getCostForecastResponse_total,
    getCostForecastResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCostForecast' smart constructor.
data GetCostForecast = GetCostForecast'
  { -- | The filters that you want to use to filter your forecast. The
    -- @GetCostForecast@ API supports filtering by the following dimensions:
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
    -- | Cost Explorer always returns the mean forecast as a single point. You
    -- can request a prediction interval around the mean by specifying a
    -- confidence level. The higher the confidence level, the more confident
    -- Cost Explorer is about the actual value falling in the prediction
    -- interval. Higher confidence levels result in wider prediction intervals.
    predictionIntervalLevel :: Prelude.Maybe Prelude.Natural,
    -- | The period of time that you want the forecast to cover. The start date
    -- must be equal to or no later than the current date to avoid a validation
    -- error.
    timePeriod :: DateInterval,
    -- | Which metric Cost Explorer uses to create your forecast. For more
    -- information about blended and unblended rates, see
    -- <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the \"blended\" annotation appear on some line items in my bill?>.
    --
    -- Valid values for a @GetCostForecast@ call are the following:
    --
    -- -   AMORTIZED_COST
    --
    -- -   BLENDED_COST
    --
    -- -   NET_AMORTIZED_COST
    --
    -- -   NET_UNBLENDED_COST
    --
    -- -   UNBLENDED_COST
    metric :: Metric,
    -- | How granular you want the forecast to be. You can get 3 months of
    -- @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
    --
    -- The @GetCostForecast@ operation supports only @DAILY@ and @MONTHLY@
    -- granularities.
    granularity :: Granularity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'getCostForecast_filter' - The filters that you want to use to filter your forecast. The
-- @GetCostForecast@ API supports filtering by the following dimensions:
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
-- 'predictionIntervalLevel', 'getCostForecast_predictionIntervalLevel' - Cost Explorer always returns the mean forecast as a single point. You
-- can request a prediction interval around the mean by specifying a
-- confidence level. The higher the confidence level, the more confident
-- Cost Explorer is about the actual value falling in the prediction
-- interval. Higher confidence levels result in wider prediction intervals.
--
-- 'timePeriod', 'getCostForecast_timePeriod' - The period of time that you want the forecast to cover. The start date
-- must be equal to or no later than the current date to avoid a validation
-- error.
--
-- 'metric', 'getCostForecast_metric' - Which metric Cost Explorer uses to create your forecast. For more
-- information about blended and unblended rates, see
-- <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the \"blended\" annotation appear on some line items in my bill?>.
--
-- Valid values for a @GetCostForecast@ call are the following:
--
-- -   AMORTIZED_COST
--
-- -   BLENDED_COST
--
-- -   NET_AMORTIZED_COST
--
-- -   NET_UNBLENDED_COST
--
-- -   UNBLENDED_COST
--
-- 'granularity', 'getCostForecast_granularity' - How granular you want the forecast to be. You can get 3 months of
-- @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetCostForecast@ operation supports only @DAILY@ and @MONTHLY@
-- granularities.
newGetCostForecast ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'metric'
  Metric ->
  -- | 'granularity'
  Granularity ->
  GetCostForecast
newGetCostForecast
  pTimePeriod_
  pMetric_
  pGranularity_ =
    GetCostForecast'
      { filter' = Prelude.Nothing,
        predictionIntervalLevel = Prelude.Nothing,
        timePeriod = pTimePeriod_,
        metric = pMetric_,
        granularity = pGranularity_
      }

-- | The filters that you want to use to filter your forecast. The
-- @GetCostForecast@ API supports filtering by the following dimensions:
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
getCostForecast_filter :: Lens.Lens' GetCostForecast (Prelude.Maybe Expression)
getCostForecast_filter = Lens.lens (\GetCostForecast' {filter'} -> filter') (\s@GetCostForecast' {} a -> s {filter' = a} :: GetCostForecast)

-- | Cost Explorer always returns the mean forecast as a single point. You
-- can request a prediction interval around the mean by specifying a
-- confidence level. The higher the confidence level, the more confident
-- Cost Explorer is about the actual value falling in the prediction
-- interval. Higher confidence levels result in wider prediction intervals.
getCostForecast_predictionIntervalLevel :: Lens.Lens' GetCostForecast (Prelude.Maybe Prelude.Natural)
getCostForecast_predictionIntervalLevel = Lens.lens (\GetCostForecast' {predictionIntervalLevel} -> predictionIntervalLevel) (\s@GetCostForecast' {} a -> s {predictionIntervalLevel = a} :: GetCostForecast)

-- | The period of time that you want the forecast to cover. The start date
-- must be equal to or no later than the current date to avoid a validation
-- error.
getCostForecast_timePeriod :: Lens.Lens' GetCostForecast DateInterval
getCostForecast_timePeriod = Lens.lens (\GetCostForecast' {timePeriod} -> timePeriod) (\s@GetCostForecast' {} a -> s {timePeriod = a} :: GetCostForecast)

-- | Which metric Cost Explorer uses to create your forecast. For more
-- information about blended and unblended rates, see
-- <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the \"blended\" annotation appear on some line items in my bill?>.
--
-- Valid values for a @GetCostForecast@ call are the following:
--
-- -   AMORTIZED_COST
--
-- -   BLENDED_COST
--
-- -   NET_AMORTIZED_COST
--
-- -   NET_UNBLENDED_COST
--
-- -   UNBLENDED_COST
getCostForecast_metric :: Lens.Lens' GetCostForecast Metric
getCostForecast_metric = Lens.lens (\GetCostForecast' {metric} -> metric) (\s@GetCostForecast' {} a -> s {metric = a} :: GetCostForecast)

-- | How granular you want the forecast to be. You can get 3 months of
-- @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetCostForecast@ operation supports only @DAILY@ and @MONTHLY@
-- granularities.
getCostForecast_granularity :: Lens.Lens' GetCostForecast Granularity
getCostForecast_granularity = Lens.lens (\GetCostForecast' {granularity} -> granularity) (\s@GetCostForecast' {} a -> s {granularity = a} :: GetCostForecast)

instance Core.AWSRequest GetCostForecast where
  type
    AWSResponse GetCostForecast =
      GetCostForecastResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostForecastResponse'
            Prelude.<$> ( x
                            Data..?> "ForecastResultsByTime"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Total")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCostForecast where
  hashWithSalt _salt GetCostForecast' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` predictionIntervalLevel
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` granularity

instance Prelude.NFData GetCostForecast where
  rnf GetCostForecast' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf predictionIntervalLevel
      `Prelude.seq` Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf granularity

instance Data.ToHeaders GetCostForecast where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.GetCostForecast" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCostForecast where
  toJSON GetCostForecast' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("PredictionIntervalLevel" Data..=)
              Prelude.<$> predictionIntervalLevel,
            Prelude.Just ("TimePeriod" Data..= timePeriod),
            Prelude.Just ("Metric" Data..= metric),
            Prelude.Just ("Granularity" Data..= granularity)
          ]
      )

instance Data.ToPath GetCostForecast where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCostForecast where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCostForecastResponse' smart constructor.
data GetCostForecastResponse = GetCostForecastResponse'
  { -- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a
    -- list of days. For @MONTHLY@ forecasts, this is a list of months.
    forecastResultsByTime :: Prelude.Maybe [ForecastResult],
    -- | How much you are forecasted to spend over the forecast period, in @USD@.
    total :: Prelude.Maybe MetricValue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostForecastResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastResultsByTime', 'getCostForecastResponse_forecastResultsByTime' - The forecasts for your query, in order. For @DAILY@ forecasts, this is a
-- list of days. For @MONTHLY@ forecasts, this is a list of months.
--
-- 'total', 'getCostForecastResponse_total' - How much you are forecasted to spend over the forecast period, in @USD@.
--
-- 'httpStatus', 'getCostForecastResponse_httpStatus' - The response's http status code.
newGetCostForecastResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCostForecastResponse
newGetCostForecastResponse pHttpStatus_ =
  GetCostForecastResponse'
    { forecastResultsByTime =
        Prelude.Nothing,
      total = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a
-- list of days. For @MONTHLY@ forecasts, this is a list of months.
getCostForecastResponse_forecastResultsByTime :: Lens.Lens' GetCostForecastResponse (Prelude.Maybe [ForecastResult])
getCostForecastResponse_forecastResultsByTime = Lens.lens (\GetCostForecastResponse' {forecastResultsByTime} -> forecastResultsByTime) (\s@GetCostForecastResponse' {} a -> s {forecastResultsByTime = a} :: GetCostForecastResponse) Prelude.. Lens.mapping Lens.coerced

-- | How much you are forecasted to spend over the forecast period, in @USD@.
getCostForecastResponse_total :: Lens.Lens' GetCostForecastResponse (Prelude.Maybe MetricValue)
getCostForecastResponse_total = Lens.lens (\GetCostForecastResponse' {total} -> total) (\s@GetCostForecastResponse' {} a -> s {total = a} :: GetCostForecastResponse)

-- | The response's http status code.
getCostForecastResponse_httpStatus :: Lens.Lens' GetCostForecastResponse Prelude.Int
getCostForecastResponse_httpStatus = Lens.lens (\GetCostForecastResponse' {httpStatus} -> httpStatus) (\s@GetCostForecastResponse' {} a -> s {httpStatus = a} :: GetCostForecastResponse)

instance Prelude.NFData GetCostForecastResponse where
  rnf GetCostForecastResponse' {..} =
    Prelude.rnf forecastResultsByTime
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf httpStatus

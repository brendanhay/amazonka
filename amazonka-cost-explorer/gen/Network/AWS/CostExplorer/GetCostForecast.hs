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
-- Module      : Network.AWS.CostExplorer.GetCostForecast
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a forecast for how much Amazon Web Services predicts that you
-- will spend over the forecast time period that you select, based on your
-- past costs.
module Network.AWS.CostExplorer.GetCostForecast
  ( -- * Creating a Request
    GetCostForecast (..),
    newGetCostForecast,

    -- * Request Lenses
    getCostForecast_predictionIntervalLevel,
    getCostForecast_filter,
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

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCostForecast' smart constructor.
data GetCostForecast = GetCostForecast'
  { -- | Cost Explorer always returns the mean forecast as a single point. You
    -- can request a prediction interval around the mean by specifying a
    -- confidence level. The higher the confidence level, the more confident
    -- Cost Explorer is about the actual value falling in the prediction
    -- interval. Higher confidence levels result in wider prediction intervals.
    predictionIntervalLevel :: Prelude.Maybe Prelude.Natural,
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
    filter' :: Prelude.Maybe Expression,
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
-- 'predictionIntervalLevel', 'getCostForecast_predictionIntervalLevel' - Cost Explorer always returns the mean forecast as a single point. You
-- can request a prediction interval around the mean by specifying a
-- confidence level. The higher the confidence level, the more confident
-- Cost Explorer is about the actual value falling in the prediction
-- interval. Higher confidence levels result in wider prediction intervals.
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
      { predictionIntervalLevel =
          Prelude.Nothing,
        filter' = Prelude.Nothing,
        timePeriod = pTimePeriod_,
        metric = pMetric_,
        granularity = pGranularity_
      }

-- | Cost Explorer always returns the mean forecast as a single point. You
-- can request a prediction interval around the mean by specifying a
-- confidence level. The higher the confidence level, the more confident
-- Cost Explorer is about the actual value falling in the prediction
-- interval. Higher confidence levels result in wider prediction intervals.
getCostForecast_predictionIntervalLevel :: Lens.Lens' GetCostForecast (Prelude.Maybe Prelude.Natural)
getCostForecast_predictionIntervalLevel = Lens.lens (\GetCostForecast' {predictionIntervalLevel} -> predictionIntervalLevel) (\s@GetCostForecast' {} a -> s {predictionIntervalLevel = a} :: GetCostForecast)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostForecastResponse'
            Prelude.<$> ( x Core..?> "ForecastResultsByTime"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Total")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCostForecast

instance Prelude.NFData GetCostForecast

instance Core.ToHeaders GetCostForecast where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetCostForecast" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCostForecast where
  toJSON GetCostForecast' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PredictionIntervalLevel" Core..=)
              Prelude.<$> predictionIntervalLevel,
            ("Filter" Core..=) Prelude.<$> filter',
            Prelude.Just ("TimePeriod" Core..= timePeriod),
            Prelude.Just ("Metric" Core..= metric),
            Prelude.Just ("Granularity" Core..= granularity)
          ]
      )

instance Core.ToPath GetCostForecast where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCostForecast where
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
getCostForecastResponse_forecastResultsByTime = Lens.lens (\GetCostForecastResponse' {forecastResultsByTime} -> forecastResultsByTime) (\s@GetCostForecastResponse' {} a -> s {forecastResultsByTime = a} :: GetCostForecastResponse) Prelude.. Lens.mapping Lens._Coerce

-- | How much you are forecasted to spend over the forecast period, in @USD@.
getCostForecastResponse_total :: Lens.Lens' GetCostForecastResponse (Prelude.Maybe MetricValue)
getCostForecastResponse_total = Lens.lens (\GetCostForecastResponse' {total} -> total) (\s@GetCostForecastResponse' {} a -> s {total = a} :: GetCostForecastResponse)

-- | The response's http status code.
getCostForecastResponse_httpStatus :: Lens.Lens' GetCostForecastResponse Prelude.Int
getCostForecastResponse_httpStatus = Lens.lens (\GetCostForecastResponse' {httpStatus} -> httpStatus) (\s@GetCostForecastResponse' {} a -> s {httpStatus = a} :: GetCostForecastResponse)

instance Prelude.NFData GetCostForecastResponse

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetCostForecast
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a forecast for how much Amazon Web Services predicts that you will spend over the forecast time period that you select, based on your past costs.
module Network.AWS.CostExplorer.GetCostForecast
  ( -- * Creating a request
    GetCostForecast (..),
    mkGetCostForecast,

    -- ** Request lenses
    gcfTimePeriod,
    gcfGranularity,
    gcfPredictionIntervalLevel,
    gcfMetric,
    gcfFilter,

    -- * Destructuring the response
    GetCostForecastResponse (..),
    mkGetCostForecastResponse,

    -- ** Response lenses
    gcfrsForecastResultsByTime,
    gcfrsTotal,
    gcfrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCostForecast' smart constructor.
data GetCostForecast = GetCostForecast'
  { -- | The period of time that you want the forecast to cover. The start date must be equal to or no later than the current date to avoid a validation error.
    timePeriod :: DateInterval,
    -- | How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
    --
    -- The @GetCostForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
    granularity :: Granularity,
    -- | Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
    predictionIntervalLevel :: Lude.Maybe Lude.Natural,
    -- | Which metric Cost Explorer uses to create your forecast. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
    --
    -- Valid values for a @GetCostForecast@ call are the following:
    --
    --     * AMORTIZED_COST
    --
    --
    --     * BLENDED_COST
    --
    --
    --     * NET_AMORTIZED_COST
    --
    --
    --     * NET_UNBLENDED_COST
    --
    --
    --     * UNBLENDED_COST
    metric :: Metric,
    -- | The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
    filter :: Lude.Maybe Expression
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCostForecast' with the minimum fields required to make a request.
--
-- * 'timePeriod' - The period of time that you want the forecast to cover. The start date must be equal to or no later than the current date to avoid a validation error.
-- * 'granularity' - How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetCostForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
-- * 'predictionIntervalLevel' - Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
-- * 'metric' - Which metric Cost Explorer uses to create your forecast. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
--
-- Valid values for a @GetCostForecast@ call are the following:
--
--     * AMORTIZED_COST
--
--
--     * BLENDED_COST
--
--
--     * NET_AMORTIZED_COST
--
--
--     * NET_UNBLENDED_COST
--
--
--     * UNBLENDED_COST
--
--
-- * 'filter' - The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
mkGetCostForecast ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'granularity'
  Granularity ->
  -- | 'metric'
  Metric ->
  GetCostForecast
mkGetCostForecast pTimePeriod_ pGranularity_ pMetric_ =
  GetCostForecast'
    { timePeriod = pTimePeriod_,
      granularity = pGranularity_,
      predictionIntervalLevel = Lude.Nothing,
      metric = pMetric_,
      filter = Lude.Nothing
    }

-- | The period of time that you want the forecast to cover. The start date must be equal to or no later than the current date to avoid a validation error.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfTimePeriod :: Lens.Lens' GetCostForecast DateInterval
gcfTimePeriod = Lens.lens (timePeriod :: GetCostForecast -> DateInterval) (\s a -> s {timePeriod = a} :: GetCostForecast)
{-# DEPRECATED gcfTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetCostForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfGranularity :: Lens.Lens' GetCostForecast Granularity
gcfGranularity = Lens.lens (granularity :: GetCostForecast -> Granularity) (\s a -> s {granularity = a} :: GetCostForecast)
{-# DEPRECATED gcfGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
--
-- /Note:/ Consider using 'predictionIntervalLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfPredictionIntervalLevel :: Lens.Lens' GetCostForecast (Lude.Maybe Lude.Natural)
gcfPredictionIntervalLevel = Lens.lens (predictionIntervalLevel :: GetCostForecast -> Lude.Maybe Lude.Natural) (\s a -> s {predictionIntervalLevel = a} :: GetCostForecast)
{-# DEPRECATED gcfPredictionIntervalLevel "Use generic-lens or generic-optics with 'predictionIntervalLevel' instead." #-}

-- | Which metric Cost Explorer uses to create your forecast. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
--
-- Valid values for a @GetCostForecast@ call are the following:
--
--     * AMORTIZED_COST
--
--
--     * BLENDED_COST
--
--
--     * NET_AMORTIZED_COST
--
--
--     * NET_UNBLENDED_COST
--
--
--     * UNBLENDED_COST
--
--
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfMetric :: Lens.Lens' GetCostForecast Metric
gcfMetric = Lens.lens (metric :: GetCostForecast -> Metric) (\s a -> s {metric = a} :: GetCostForecast)
{-# DEPRECATED gcfMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

-- | The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfFilter :: Lens.Lens' GetCostForecast (Lude.Maybe Expression)
gcfFilter = Lens.lens (filter :: GetCostForecast -> Lude.Maybe Expression) (\s a -> s {filter = a} :: GetCostForecast)
{-# DEPRECATED gcfFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Lude.AWSRequest GetCostForecast where
  type Rs GetCostForecast = GetCostForecastResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCostForecastResponse'
            Lude.<$> (x Lude..?> "ForecastResultsByTime" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Total")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCostForecast where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSInsightsIndexService.GetCostForecast" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCostForecast where
  toJSON GetCostForecast' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TimePeriod" Lude..= timePeriod),
            Lude.Just ("Granularity" Lude..= granularity),
            ("PredictionIntervalLevel" Lude..=)
              Lude.<$> predictionIntervalLevel,
            Lude.Just ("Metric" Lude..= metric),
            ("Filter" Lude..=) Lude.<$> filter
          ]
      )

instance Lude.ToPath GetCostForecast where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCostForecast where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCostForecastResponse' smart constructor.
data GetCostForecastResponse = GetCostForecastResponse'
  { -- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
    forecastResultsByTime :: Lude.Maybe [ForecastResult],
    -- | How much you are forecasted to spend over the forecast period, in @USD@ .
    total :: Lude.Maybe MetricValue,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCostForecastResponse' with the minimum fields required to make a request.
--
-- * 'forecastResultsByTime' - The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
-- * 'total' - How much you are forecasted to spend over the forecast period, in @USD@ .
-- * 'responseStatus' - The response status code.
mkGetCostForecastResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCostForecastResponse
mkGetCostForecastResponse pResponseStatus_ =
  GetCostForecastResponse'
    { forecastResultsByTime = Lude.Nothing,
      total = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
--
-- /Note:/ Consider using 'forecastResultsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfrsForecastResultsByTime :: Lens.Lens' GetCostForecastResponse (Lude.Maybe [ForecastResult])
gcfrsForecastResultsByTime = Lens.lens (forecastResultsByTime :: GetCostForecastResponse -> Lude.Maybe [ForecastResult]) (\s a -> s {forecastResultsByTime = a} :: GetCostForecastResponse)
{-# DEPRECATED gcfrsForecastResultsByTime "Use generic-lens or generic-optics with 'forecastResultsByTime' instead." #-}

-- | How much you are forecasted to spend over the forecast period, in @USD@ .
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfrsTotal :: Lens.Lens' GetCostForecastResponse (Lude.Maybe MetricValue)
gcfrsTotal = Lens.lens (total :: GetCostForecastResponse -> Lude.Maybe MetricValue) (\s a -> s {total = a} :: GetCostForecastResponse)
{-# DEPRECATED gcfrsTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfrsResponseStatus :: Lens.Lens' GetCostForecastResponse Lude.Int
gcfrsResponseStatus = Lens.lens (responseStatus :: GetCostForecastResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCostForecastResponse)
{-# DEPRECATED gcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

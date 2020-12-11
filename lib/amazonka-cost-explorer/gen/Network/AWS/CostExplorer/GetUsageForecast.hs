{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetUsageForecast
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a forecast for how much Amazon Web Services predicts that you will use over the forecast time period that you select, based on your past usage.
module Network.AWS.CostExplorer.GetUsageForecast
  ( -- * Creating a request
    GetUsageForecast (..),
    mkGetUsageForecast,

    -- ** Request lenses
    gufPredictionIntervalLevel,
    gufFilter,
    gufTimePeriod,
    gufMetric,
    gufGranularity,

    -- * Destructuring the response
    GetUsageForecastResponse (..),
    mkGetUsageForecastResponse,

    -- ** Response lenses
    gufrsForecastResultsByTime,
    gufrsTotal,
    gufrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUsageForecast' smart constructor.
data GetUsageForecast = GetUsageForecast'
  { predictionIntervalLevel ::
      Lude.Maybe Lude.Natural,
    filter :: Lude.Maybe Expression,
    timePeriod :: DateInterval,
    metric :: Metric,
    granularity :: Granularity
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsageForecast' with the minimum fields required to make a request.
--
-- * 'filter' - The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
-- * 'granularity' - How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetUsageForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
-- * 'metric' - Which metric Cost Explorer uses to create your forecast.
--
-- Valid values for a @GetUsageForecast@ call are the following:
--
--     * USAGE_QUANTITY
--
--
--     * NORMALIZED_USAGE_AMOUNT
--
--
-- * 'predictionIntervalLevel' - Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
-- * 'timePeriod' - The start and end dates of the period that you want to retrieve usage forecast for. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ . The start date must be equal to or later than the current date to avoid a validation error.
mkGetUsageForecast ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'metric'
  Metric ->
  -- | 'granularity'
  Granularity ->
  GetUsageForecast
mkGetUsageForecast pTimePeriod_ pMetric_ pGranularity_ =
  GetUsageForecast'
    { predictionIntervalLevel = Lude.Nothing,
      filter = Lude.Nothing,
      timePeriod = pTimePeriod_,
      metric = pMetric_,
      granularity = pGranularity_
    }

-- | Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
--
-- /Note:/ Consider using 'predictionIntervalLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufPredictionIntervalLevel :: Lens.Lens' GetUsageForecast (Lude.Maybe Lude.Natural)
gufPredictionIntervalLevel = Lens.lens (predictionIntervalLevel :: GetUsageForecast -> Lude.Maybe Lude.Natural) (\s a -> s {predictionIntervalLevel = a} :: GetUsageForecast)
{-# DEPRECATED gufPredictionIntervalLevel "Use generic-lens or generic-optics with 'predictionIntervalLevel' instead." #-}

-- | The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufFilter :: Lens.Lens' GetUsageForecast (Lude.Maybe Expression)
gufFilter = Lens.lens (filter :: GetUsageForecast -> Lude.Maybe Expression) (\s a -> s {filter = a} :: GetUsageForecast)
{-# DEPRECATED gufFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The start and end dates of the period that you want to retrieve usage forecast for. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ . The start date must be equal to or later than the current date to avoid a validation error.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufTimePeriod :: Lens.Lens' GetUsageForecast DateInterval
gufTimePeriod = Lens.lens (timePeriod :: GetUsageForecast -> DateInterval) (\s a -> s {timePeriod = a} :: GetUsageForecast)
{-# DEPRECATED gufTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | Which metric Cost Explorer uses to create your forecast.
--
-- Valid values for a @GetUsageForecast@ call are the following:
--
--     * USAGE_QUANTITY
--
--
--     * NORMALIZED_USAGE_AMOUNT
--
--
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufMetric :: Lens.Lens' GetUsageForecast Metric
gufMetric = Lens.lens (metric :: GetUsageForecast -> Metric) (\s a -> s {metric = a} :: GetUsageForecast)
{-# DEPRECATED gufMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

-- | How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetUsageForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufGranularity :: Lens.Lens' GetUsageForecast Granularity
gufGranularity = Lens.lens (granularity :: GetUsageForecast -> Granularity) (\s a -> s {granularity = a} :: GetUsageForecast)
{-# DEPRECATED gufGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

instance Lude.AWSRequest GetUsageForecast where
  type Rs GetUsageForecast = GetUsageForecastResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUsageForecastResponse'
            Lude.<$> (x Lude..?> "ForecastResultsByTime" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Total")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUsageForecast where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSInsightsIndexService.GetUsageForecast" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetUsageForecast where
  toJSON GetUsageForecast' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PredictionIntervalLevel" Lude..=)
              Lude.<$> predictionIntervalLevel,
            ("Filter" Lude..=) Lude.<$> filter,
            Lude.Just ("TimePeriod" Lude..= timePeriod),
            Lude.Just ("Metric" Lude..= metric),
            Lude.Just ("Granularity" Lude..= granularity)
          ]
      )

instance Lude.ToPath GetUsageForecast where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUsageForecast where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetUsageForecastResponse' smart constructor.
data GetUsageForecastResponse = GetUsageForecastResponse'
  { forecastResultsByTime ::
      Lude.Maybe [ForecastResult],
    total :: Lude.Maybe MetricValue,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsageForecastResponse' with the minimum fields required to make a request.
--
-- * 'forecastResultsByTime' - The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
-- * 'responseStatus' - The response status code.
-- * 'total' - How much you're forecasted to use over the forecast period.
mkGetUsageForecastResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUsageForecastResponse
mkGetUsageForecastResponse pResponseStatus_ =
  GetUsageForecastResponse'
    { forecastResultsByTime = Lude.Nothing,
      total = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
--
-- /Note:/ Consider using 'forecastResultsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufrsForecastResultsByTime :: Lens.Lens' GetUsageForecastResponse (Lude.Maybe [ForecastResult])
gufrsForecastResultsByTime = Lens.lens (forecastResultsByTime :: GetUsageForecastResponse -> Lude.Maybe [ForecastResult]) (\s a -> s {forecastResultsByTime = a} :: GetUsageForecastResponse)
{-# DEPRECATED gufrsForecastResultsByTime "Use generic-lens or generic-optics with 'forecastResultsByTime' instead." #-}

-- | How much you're forecasted to use over the forecast period.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufrsTotal :: Lens.Lens' GetUsageForecastResponse (Lude.Maybe MetricValue)
gufrsTotal = Lens.lens (total :: GetUsageForecastResponse -> Lude.Maybe MetricValue) (\s a -> s {total = a} :: GetUsageForecastResponse)
{-# DEPRECATED gufrsTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufrsResponseStatus :: Lens.Lens' GetUsageForecastResponse Lude.Int
gufrsResponseStatus = Lens.lens (responseStatus :: GetUsageForecastResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUsageForecastResponse)
{-# DEPRECATED gufrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

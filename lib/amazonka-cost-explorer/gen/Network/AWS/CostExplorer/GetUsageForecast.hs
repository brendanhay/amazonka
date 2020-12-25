{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gufTimePeriod,
    gufMetric,
    gufGranularity,
    gufFilter,
    gufPredictionIntervalLevel,

    -- * Destructuring the response
    GetUsageForecastResponse (..),
    mkGetUsageForecastResponse,

    -- ** Response lenses
    gufrrsForecastResultsByTime,
    gufrrsTotal,
    gufrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUsageForecast' smart constructor.
data GetUsageForecast = GetUsageForecast'
  { -- | The start and end dates of the period that you want to retrieve usage forecast for. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ . The start date must be equal to or later than the current date to avoid a validation error.
    timePeriod :: Types.DateInterval,
    -- | Which metric Cost Explorer uses to create your forecast.
    --
    -- Valid values for a @GetUsageForecast@ call are the following:
    --
    --     * USAGE_QUANTITY
    --
    --
    --     * NORMALIZED_USAGE_AMOUNT
    metric :: Types.Metric,
    -- | How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
    --
    -- The @GetUsageForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
    granularity :: Types.Granularity,
    -- | The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
    filter :: Core.Maybe Types.Expression,
    -- | Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
    predictionIntervalLevel :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsageForecast' value with any optional fields omitted.
mkGetUsageForecast ::
  -- | 'timePeriod'
  Types.DateInterval ->
  -- | 'metric'
  Types.Metric ->
  -- | 'granularity'
  Types.Granularity ->
  GetUsageForecast
mkGetUsageForecast timePeriod metric granularity =
  GetUsageForecast'
    { timePeriod,
      metric,
      granularity,
      filter = Core.Nothing,
      predictionIntervalLevel = Core.Nothing
    }

-- | The start and end dates of the period that you want to retrieve usage forecast for. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ . The start date must be equal to or later than the current date to avoid a validation error.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufTimePeriod :: Lens.Lens' GetUsageForecast Types.DateInterval
gufTimePeriod = Lens.field @"timePeriod"
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
gufMetric :: Lens.Lens' GetUsageForecast Types.Metric
gufMetric = Lens.field @"metric"
{-# DEPRECATED gufMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

-- | How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetUsageForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufGranularity :: Lens.Lens' GetUsageForecast Types.Granularity
gufGranularity = Lens.field @"granularity"
{-# DEPRECATED gufGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufFilter :: Lens.Lens' GetUsageForecast (Core.Maybe Types.Expression)
gufFilter = Lens.field @"filter"
{-# DEPRECATED gufFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
--
-- /Note:/ Consider using 'predictionIntervalLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufPredictionIntervalLevel :: Lens.Lens' GetUsageForecast (Core.Maybe Core.Natural)
gufPredictionIntervalLevel = Lens.field @"predictionIntervalLevel"
{-# DEPRECATED gufPredictionIntervalLevel "Use generic-lens or generic-optics with 'predictionIntervalLevel' instead." #-}

instance Core.FromJSON GetUsageForecast where
  toJSON GetUsageForecast {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TimePeriod" Core..= timePeriod),
            Core.Just ("Metric" Core..= metric),
            Core.Just ("Granularity" Core..= granularity),
            ("Filter" Core..=) Core.<$> filter,
            ("PredictionIntervalLevel" Core..=)
              Core.<$> predictionIntervalLevel
          ]
      )

instance Core.AWSRequest GetUsageForecast where
  type Rs GetUsageForecast = GetUsageForecastResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSInsightsIndexService.GetUsageForecast")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsageForecastResponse'
            Core.<$> (x Core..:? "ForecastResultsByTime")
            Core.<*> (x Core..:? "Total")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetUsageForecastResponse' smart constructor.
data GetUsageForecastResponse = GetUsageForecastResponse'
  { -- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
    forecastResultsByTime :: Core.Maybe [Types.ForecastResult],
    -- | How much you're forecasted to use over the forecast period.
    total :: Core.Maybe Types.MetricValue,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsageForecastResponse' value with any optional fields omitted.
mkGetUsageForecastResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetUsageForecastResponse
mkGetUsageForecastResponse responseStatus =
  GetUsageForecastResponse'
    { forecastResultsByTime = Core.Nothing,
      total = Core.Nothing,
      responseStatus
    }

-- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
--
-- /Note:/ Consider using 'forecastResultsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufrrsForecastResultsByTime :: Lens.Lens' GetUsageForecastResponse (Core.Maybe [Types.ForecastResult])
gufrrsForecastResultsByTime = Lens.field @"forecastResultsByTime"
{-# DEPRECATED gufrrsForecastResultsByTime "Use generic-lens or generic-optics with 'forecastResultsByTime' instead." #-}

-- | How much you're forecasted to use over the forecast period.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufrrsTotal :: Lens.Lens' GetUsageForecastResponse (Core.Maybe Types.MetricValue)
gufrrsTotal = Lens.field @"total"
{-# DEPRECATED gufrrsTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gufrrsResponseStatus :: Lens.Lens' GetUsageForecastResponse Core.Int
gufrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gufrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

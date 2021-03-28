{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetCostForecast (..)
    , mkGetCostForecast
    -- ** Request lenses
    , gcfTimePeriod
    , gcfMetric
    , gcfGranularity
    , gcfFilter
    , gcfPredictionIntervalLevel

    -- * Destructuring the response
    , GetCostForecastResponse (..)
    , mkGetCostForecastResponse
    -- ** Response lenses
    , gcfrrsForecastResultsByTime
    , gcfrrsTotal
    , gcfrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCostForecast' smart constructor.
data GetCostForecast = GetCostForecast'
  { timePeriod :: Types.DateInterval
    -- ^ The period of time that you want the forecast to cover. The start date must be equal to or no later than the current date to avoid a validation error.
  , metric :: Types.Metric
    -- ^ Which metric Cost Explorer uses to create your forecast. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> . 
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
  , granularity :: Types.Granularity
    -- ^ How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetCostForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
  , filter :: Core.Maybe Types.Expression
    -- ^ The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
  , predictionIntervalLevel :: Core.Maybe Core.Natural
    -- ^ Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCostForecast' value with any optional fields omitted.
mkGetCostForecast
    :: Types.DateInterval -- ^ 'timePeriod'
    -> Types.Metric -- ^ 'metric'
    -> Types.Granularity -- ^ 'granularity'
    -> GetCostForecast
mkGetCostForecast timePeriod metric granularity
  = GetCostForecast'{timePeriod, metric, granularity,
                     filter = Core.Nothing, predictionIntervalLevel = Core.Nothing}

-- | The period of time that you want the forecast to cover. The start date must be equal to or no later than the current date to avoid a validation error.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfTimePeriod :: Lens.Lens' GetCostForecast Types.DateInterval
gcfTimePeriod = Lens.field @"timePeriod"
{-# INLINEABLE gcfTimePeriod #-}
{-# DEPRECATED timePeriod "Use generic-lens or generic-optics with 'timePeriod' instead"  #-}

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
gcfMetric :: Lens.Lens' GetCostForecast Types.Metric
gcfMetric = Lens.field @"metric"
{-# INLINEABLE gcfMetric #-}
{-# DEPRECATED metric "Use generic-lens or generic-optics with 'metric' instead"  #-}

-- | How granular you want the forecast to be. You can get 3 months of @DAILY@ forecasts or 12 months of @MONTHLY@ forecasts.
--
-- The @GetCostForecast@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfGranularity :: Lens.Lens' GetCostForecast Types.Granularity
gcfGranularity = Lens.field @"granularity"
{-# INLINEABLE gcfGranularity #-}
{-# DEPRECATED granularity "Use generic-lens or generic-optics with 'granularity' instead"  #-}

-- | The filters that you want to use to filter your forecast. Cost Explorer API supports all of the Cost Explorer filters.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfFilter :: Lens.Lens' GetCostForecast (Core.Maybe Types.Expression)
gcfFilter = Lens.field @"filter"
{-# INLINEABLE gcfFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | Cost Explorer always returns the mean forecast as a single point. You can request a prediction interval around the mean by specifying a confidence level. The higher the confidence level, the more confident Cost Explorer is about the actual value falling in the prediction interval. Higher confidence levels result in wider prediction intervals.
--
-- /Note:/ Consider using 'predictionIntervalLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfPredictionIntervalLevel :: Lens.Lens' GetCostForecast (Core.Maybe Core.Natural)
gcfPredictionIntervalLevel = Lens.field @"predictionIntervalLevel"
{-# INLINEABLE gcfPredictionIntervalLevel #-}
{-# DEPRECATED predictionIntervalLevel "Use generic-lens or generic-optics with 'predictionIntervalLevel' instead"  #-}

instance Core.ToQuery GetCostForecast where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCostForecast where
        toHeaders GetCostForecast{..}
          = Core.pure
              ("X-Amz-Target", "AWSInsightsIndexService.GetCostForecast")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCostForecast where
        toJSON GetCostForecast{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TimePeriod" Core..= timePeriod),
                  Core.Just ("Metric" Core..= metric),
                  Core.Just ("Granularity" Core..= granularity),
                  ("Filter" Core..=) Core.<$> filter,
                  ("PredictionIntervalLevel" Core..=) Core.<$>
                    predictionIntervalLevel])

instance Core.AWSRequest GetCostForecast where
        type Rs GetCostForecast = GetCostForecastResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCostForecastResponse' Core.<$>
                   (x Core..:? "ForecastResultsByTime") Core.<*> x Core..:? "Total"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCostForecastResponse' smart constructor.
data GetCostForecastResponse = GetCostForecastResponse'
  { forecastResultsByTime :: Core.Maybe [Types.ForecastResult]
    -- ^ The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
  , total :: Core.Maybe Types.MetricValue
    -- ^ How much you are forecasted to spend over the forecast period, in @USD@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCostForecastResponse' value with any optional fields omitted.
mkGetCostForecastResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCostForecastResponse
mkGetCostForecastResponse responseStatus
  = GetCostForecastResponse'{forecastResultsByTime = Core.Nothing,
                             total = Core.Nothing, responseStatus}

-- | The forecasts for your query, in order. For @DAILY@ forecasts, this is a list of days. For @MONTHLY@ forecasts, this is a list of months.
--
-- /Note:/ Consider using 'forecastResultsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfrrsForecastResultsByTime :: Lens.Lens' GetCostForecastResponse (Core.Maybe [Types.ForecastResult])
gcfrrsForecastResultsByTime = Lens.field @"forecastResultsByTime"
{-# INLINEABLE gcfrrsForecastResultsByTime #-}
{-# DEPRECATED forecastResultsByTime "Use generic-lens or generic-optics with 'forecastResultsByTime' instead"  #-}

-- | How much you are forecasted to spend over the forecast period, in @USD@ .
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfrrsTotal :: Lens.Lens' GetCostForecastResponse (Core.Maybe Types.MetricValue)
gcfrrsTotal = Lens.field @"total"
{-# INLINEABLE gcfrrsTotal #-}
{-# DEPRECATED total "Use generic-lens or generic-optics with 'total' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfrrsResponseStatus :: Lens.Lens' GetCostForecastResponse Core.Int
gcfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

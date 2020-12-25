{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAlarmsForMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the alarms for the specified metric. To filter the results, specify a statistic, period, or unit.
--
-- This operation retrieves only standard alarms that are based on the specified metric. It does not return alarms based on math expressions that use the specified metric, or composite alarms that use the specified metric.
module Network.AWS.CloudWatch.DescribeAlarmsForMetric
  ( -- * Creating a request
    DescribeAlarmsForMetric (..),
    mkDescribeAlarmsForMetric,

    -- ** Request lenses
    dafmMetricName,
    dafmNamespace,
    dafmDimensions,
    dafmExtendedStatistic,
    dafmPeriod,
    dafmStatistic,
    dafmUnit,

    -- * Destructuring the response
    DescribeAlarmsForMetricResponse (..),
    mkDescribeAlarmsForMetricResponse,

    -- ** Response lenses
    dafmrrsMetricAlarms,
    dafmrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAlarmsForMetric' smart constructor.
data DescribeAlarmsForMetric = DescribeAlarmsForMetric'
  { -- | The name of the metric.
    metricName :: Types.MetricName,
    -- | The namespace of the metric.
    namespace :: Types.Namespace,
    -- | The dimensions associated with the metric. If the metric has any associated dimensions, you must specify them in order for the call to succeed.
    dimensions :: Core.Maybe [Types.Dimension],
    -- | The percentile statistic for the metric. Specify a value between p0.0 and p100.
    extendedStatistic :: Core.Maybe Types.ExtendedStatistic,
    -- | The period, in seconds, over which the statistic is applied.
    period :: Core.Maybe Core.Natural,
    -- | The statistic for the metric, other than percentiles. For percentile statistics, use @ExtendedStatistics@ .
    statistic :: Core.Maybe Types.Statistic,
    -- | The unit for the metric.
    unit :: Core.Maybe Types.StandardUnit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAlarmsForMetric' value with any optional fields omitted.
mkDescribeAlarmsForMetric ::
  -- | 'metricName'
  Types.MetricName ->
  -- | 'namespace'
  Types.Namespace ->
  DescribeAlarmsForMetric
mkDescribeAlarmsForMetric metricName namespace =
  DescribeAlarmsForMetric'
    { metricName,
      namespace,
      dimensions = Core.Nothing,
      extendedStatistic = Core.Nothing,
      period = Core.Nothing,
      statistic = Core.Nothing,
      unit = Core.Nothing
    }

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmMetricName :: Lens.Lens' DescribeAlarmsForMetric Types.MetricName
dafmMetricName = Lens.field @"metricName"
{-# DEPRECATED dafmMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmNamespace :: Lens.Lens' DescribeAlarmsForMetric Types.Namespace
dafmNamespace = Lens.field @"namespace"
{-# DEPRECATED dafmNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The dimensions associated with the metric. If the metric has any associated dimensions, you must specify them in order for the call to succeed.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmDimensions :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe [Types.Dimension])
dafmDimensions = Lens.field @"dimensions"
{-# DEPRECATED dafmDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The percentile statistic for the metric. Specify a value between p0.0 and p100.
--
-- /Note:/ Consider using 'extendedStatistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmExtendedStatistic :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Types.ExtendedStatistic)
dafmExtendedStatistic = Lens.field @"extendedStatistic"
{-# DEPRECATED dafmExtendedStatistic "Use generic-lens or generic-optics with 'extendedStatistic' instead." #-}

-- | The period, in seconds, over which the statistic is applied.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmPeriod :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Core.Natural)
dafmPeriod = Lens.field @"period"
{-# DEPRECATED dafmPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The statistic for the metric, other than percentiles. For percentile statistics, use @ExtendedStatistics@ .
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmStatistic :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Types.Statistic)
dafmStatistic = Lens.field @"statistic"
{-# DEPRECATED dafmStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | The unit for the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmUnit :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Types.StandardUnit)
dafmUnit = Lens.field @"unit"
{-# DEPRECATED dafmUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.AWSRequest DescribeAlarmsForMetric where
  type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeAlarmsForMetric")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> (Core.toQueryValue "MetricName" metricName)
                Core.<> (Core.toQueryValue "Namespace" namespace)
                Core.<> ( Core.toQueryValue
                            "Dimensions"
                            (Core.toQueryList "member" Core.<$> dimensions)
                        )
                Core.<> (Core.toQueryValue "ExtendedStatistic" Core.<$> extendedStatistic)
                Core.<> (Core.toQueryValue "Period" Core.<$> period)
                Core.<> (Core.toQueryValue "Statistic" Core.<$> statistic)
                Core.<> (Core.toQueryValue "Unit" Core.<$> unit)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeAlarmsForMetricResult"
      ( \s h x ->
          DescribeAlarmsForMetricResponse'
            Core.<$> (x Core..@? "MetricAlarms" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAlarmsForMetricResponse' smart constructor.
data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'
  { -- | The information for each alarm with the specified metric.
    metricAlarms :: Core.Maybe [Types.MetricAlarm],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAlarmsForMetricResponse' value with any optional fields omitted.
mkDescribeAlarmsForMetricResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAlarmsForMetricResponse
mkDescribeAlarmsForMetricResponse responseStatus =
  DescribeAlarmsForMetricResponse'
    { metricAlarms = Core.Nothing,
      responseStatus
    }

-- | The information for each alarm with the specified metric.
--
-- /Note:/ Consider using 'metricAlarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmrrsMetricAlarms :: Lens.Lens' DescribeAlarmsForMetricResponse (Core.Maybe [Types.MetricAlarm])
dafmrrsMetricAlarms = Lens.field @"metricAlarms"
{-# DEPRECATED dafmrrsMetricAlarms "Use generic-lens or generic-optics with 'metricAlarms' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmrrsResponseStatus :: Lens.Lens' DescribeAlarmsForMetricResponse Core.Int
dafmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dafmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

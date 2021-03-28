{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeAlarmsForMetric (..)
    , mkDescribeAlarmsForMetric
    -- ** Request lenses
    , dafmMetricName
    , dafmNamespace
    , dafmDimensions
    , dafmExtendedStatistic
    , dafmPeriod
    , dafmStatistic
    , dafmUnit

    -- * Destructuring the response
    , DescribeAlarmsForMetricResponse (..)
    , mkDescribeAlarmsForMetricResponse
    -- ** Response lenses
    , dafmrrsMetricAlarms
    , dafmrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAlarmsForMetric' smart constructor.
data DescribeAlarmsForMetric = DescribeAlarmsForMetric'
  { metricName :: Types.MetricName
    -- ^ The name of the metric.
  , namespace :: Types.Namespace
    -- ^ The namespace of the metric.
  , dimensions :: Core.Maybe [Types.Dimension]
    -- ^ The dimensions associated with the metric. If the metric has any associated dimensions, you must specify them in order for the call to succeed.
  , extendedStatistic :: Core.Maybe Types.ExtendedStatistic
    -- ^ The percentile statistic for the metric. Specify a value between p0.0 and p100.
  , period :: Core.Maybe Core.Natural
    -- ^ The period, in seconds, over which the statistic is applied.
  , statistic :: Core.Maybe Types.Statistic
    -- ^ The statistic for the metric, other than percentiles. For percentile statistics, use @ExtendedStatistics@ .
  , unit :: Core.Maybe Types.StandardUnit
    -- ^ The unit for the metric.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAlarmsForMetric' value with any optional fields omitted.
mkDescribeAlarmsForMetric
    :: Types.MetricName -- ^ 'metricName'
    -> Types.Namespace -- ^ 'namespace'
    -> DescribeAlarmsForMetric
mkDescribeAlarmsForMetric metricName namespace
  = DescribeAlarmsForMetric'{metricName, namespace,
                             dimensions = Core.Nothing, extendedStatistic = Core.Nothing,
                             period = Core.Nothing, statistic = Core.Nothing,
                             unit = Core.Nothing}

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmMetricName :: Lens.Lens' DescribeAlarmsForMetric Types.MetricName
dafmMetricName = Lens.field @"metricName"
{-# INLINEABLE dafmMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmNamespace :: Lens.Lens' DescribeAlarmsForMetric Types.Namespace
dafmNamespace = Lens.field @"namespace"
{-# INLINEABLE dafmNamespace #-}
{-# DEPRECATED namespace "Use generic-lens or generic-optics with 'namespace' instead"  #-}

-- | The dimensions associated with the metric. If the metric has any associated dimensions, you must specify them in order for the call to succeed.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmDimensions :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe [Types.Dimension])
dafmDimensions = Lens.field @"dimensions"
{-# INLINEABLE dafmDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

-- | The percentile statistic for the metric. Specify a value between p0.0 and p100.
--
-- /Note:/ Consider using 'extendedStatistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmExtendedStatistic :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Types.ExtendedStatistic)
dafmExtendedStatistic = Lens.field @"extendedStatistic"
{-# INLINEABLE dafmExtendedStatistic #-}
{-# DEPRECATED extendedStatistic "Use generic-lens or generic-optics with 'extendedStatistic' instead"  #-}

-- | The period, in seconds, over which the statistic is applied.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmPeriod :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Core.Natural)
dafmPeriod = Lens.field @"period"
{-# INLINEABLE dafmPeriod #-}
{-# DEPRECATED period "Use generic-lens or generic-optics with 'period' instead"  #-}

-- | The statistic for the metric, other than percentiles. For percentile statistics, use @ExtendedStatistics@ .
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmStatistic :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Types.Statistic)
dafmStatistic = Lens.field @"statistic"
{-# INLINEABLE dafmStatistic #-}
{-# DEPRECATED statistic "Use generic-lens or generic-optics with 'statistic' instead"  #-}

-- | The unit for the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmUnit :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Types.StandardUnit)
dafmUnit = Lens.field @"unit"
{-# INLINEABLE dafmUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.ToQuery DescribeAlarmsForMetric where
        toQuery DescribeAlarmsForMetric{..}
          = Core.toQueryPair "Action"
              ("DescribeAlarmsForMetric" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<> Core.toQueryPair "MetricName" metricName
              Core.<> Core.toQueryPair "Namespace" namespace
              Core.<>
              Core.toQueryPair "Dimensions"
                (Core.maybe Core.mempty (Core.toQueryList "member") dimensions)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ExtendedStatistic")
                extendedStatistic
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Period") period
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Statistic") statistic
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Unit") unit

instance Core.ToHeaders DescribeAlarmsForMetric where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAlarmsForMetric where
        type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeAlarmsForMetricResult"
              (\ s h x ->
                 DescribeAlarmsForMetricResponse' Core.<$>
                   (x Core..@? "MetricAlarms" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAlarmsForMetricResponse' smart constructor.
data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'
  { metricAlarms :: Core.Maybe [Types.MetricAlarm]
    -- ^ The information for each alarm with the specified metric.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAlarmsForMetricResponse' value with any optional fields omitted.
mkDescribeAlarmsForMetricResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAlarmsForMetricResponse
mkDescribeAlarmsForMetricResponse responseStatus
  = DescribeAlarmsForMetricResponse'{metricAlarms = Core.Nothing,
                                     responseStatus}

-- | The information for each alarm with the specified metric.
--
-- /Note:/ Consider using 'metricAlarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmrrsMetricAlarms :: Lens.Lens' DescribeAlarmsForMetricResponse (Core.Maybe [Types.MetricAlarm])
dafmrrsMetricAlarms = Lens.field @"metricAlarms"
{-# INLINEABLE dafmrrsMetricAlarms #-}
{-# DEPRECATED metricAlarms "Use generic-lens or generic-optics with 'metricAlarms' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmrrsResponseStatus :: Lens.Lens' DescribeAlarmsForMetricResponse Core.Int
dafmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dafmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

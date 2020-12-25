{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricStat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricStat
  ( MetricStat (..),

    -- * Smart constructor
    mkMetricStat,

    -- * Lenses
    msMetric,
    msPeriod,
    msStat,
    msUnit,
  )
where

import qualified Network.AWS.CloudWatch.Types.Metric as Types
import qualified Network.AWS.CloudWatch.Types.StandardUnit as Types
import qualified Network.AWS.CloudWatch.Types.Stat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This structure defines the metric to be returned, along with the statistics, period, and units.
--
-- /See:/ 'mkMetricStat' smart constructor.
data MetricStat = MetricStat'
  { -- | The metric to return, including the metric name, namespace, and dimensions.
    metric :: Types.Metric,
    -- | The granularity, in seconds, of the returned data points. For metrics with regular resolution, a period can be as short as one minute (60 seconds) and must be a multiple of 60. For high-resolution metrics that are collected at intervals of less than one minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those metrics stored by a @PutMetricData@ call that includes a @StorageResolution@ of 1 second.
    --
    -- If the @StartTime@ parameter specifies a time stamp that is greater than 3 hours ago, you must specify the period as follows or no data points in that time range is returned:
    --
    --     * Start time between 3 hours and 15 days ago - Use a multiple of 60 seconds (1 minute).
    --
    --
    --     * Start time between 15 and 63 days ago - Use a multiple of 300 seconds (5 minutes).
    --
    --
    --     * Start time greater than 63 days ago - Use a multiple of 3600 seconds (1 hour).
    period :: Core.Natural,
    -- | The statistic to return. It can include any CloudWatch statistic or extended statistic.
    stat :: Types.Stat,
    -- | When you are using a @Put@ operation, this defines what unit you want to use when storing the metric.
    --
    -- In a @Get@ operation, if you omit @Unit@ then all data that was collected with any unit is returned, along with the corresponding units that were specified when the data was reported to CloudWatch. If you specify a unit, the operation returns only data that was collected with that unit specified. If you specify a unit that does not match the data collected, the results of the operation are null. CloudWatch does not perform unit conversions.
    unit :: Core.Maybe Types.StandardUnit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricStat' value with any optional fields omitted.
mkMetricStat ::
  -- | 'metric'
  Types.Metric ->
  -- | 'period'
  Core.Natural ->
  -- | 'stat'
  Types.Stat ->
  MetricStat
mkMetricStat metric period stat =
  MetricStat' {metric, period, stat, unit = Core.Nothing}

-- | The metric to return, including the metric name, namespace, and dimensions.
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMetric :: Lens.Lens' MetricStat Types.Metric
msMetric = Lens.field @"metric"
{-# DEPRECATED msMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

-- | The granularity, in seconds, of the returned data points. For metrics with regular resolution, a period can be as short as one minute (60 seconds) and must be a multiple of 60. For high-resolution metrics that are collected at intervals of less than one minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those metrics stored by a @PutMetricData@ call that includes a @StorageResolution@ of 1 second.
--
-- If the @StartTime@ parameter specifies a time stamp that is greater than 3 hours ago, you must specify the period as follows or no data points in that time range is returned:
--
--     * Start time between 3 hours and 15 days ago - Use a multiple of 60 seconds (1 minute).
--
--
--     * Start time between 15 and 63 days ago - Use a multiple of 300 seconds (5 minutes).
--
--
--     * Start time greater than 63 days ago - Use a multiple of 3600 seconds (1 hour).
--
--
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msPeriod :: Lens.Lens' MetricStat Core.Natural
msPeriod = Lens.field @"period"
{-# DEPRECATED msPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The statistic to return. It can include any CloudWatch statistic or extended statistic.
--
-- /Note:/ Consider using 'stat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msStat :: Lens.Lens' MetricStat Types.Stat
msStat = Lens.field @"stat"
{-# DEPRECATED msStat "Use generic-lens or generic-optics with 'stat' instead." #-}

-- | When you are using a @Put@ operation, this defines what unit you want to use when storing the metric.
--
-- In a @Get@ operation, if you omit @Unit@ then all data that was collected with any unit is returned, along with the corresponding units that were specified when the data was reported to CloudWatch. If you specify a unit, the operation returns only data that was collected with that unit specified. If you specify a unit that does not match the data collected, the results of the operation are null. CloudWatch does not perform unit conversions.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msUnit :: Lens.Lens' MetricStat (Core.Maybe Types.StandardUnit)
msUnit = Lens.field @"unit"
{-# DEPRECATED msUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.FromXML MetricStat where
  parseXML x =
    MetricStat'
      Core.<$> (x Core..@ "Metric")
      Core.<*> (x Core..@ "Period")
      Core.<*> (x Core..@ "Stat")
      Core.<*> (x Core..@? "Unit")

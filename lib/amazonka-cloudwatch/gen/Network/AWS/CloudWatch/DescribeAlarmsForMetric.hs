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
    dafmPeriod,
    dafmMetricName,
    dafmNamespace,
    dafmDimensions,
    dafmUnit,
    dafmStatistic,
    dafmExtendedStatistic,

    -- * Destructuring the response
    DescribeAlarmsForMetricResponse (..),
    mkDescribeAlarmsForMetricResponse,

    -- ** Response lenses
    dafmrsMetricAlarms,
    dafmrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAlarmsForMetric' smart constructor.
data DescribeAlarmsForMetric = DescribeAlarmsForMetric'
  { -- | The period, in seconds, over which the statistic is applied.
    period :: Lude.Maybe Lude.Natural,
    -- | The name of the metric.
    metricName :: Lude.Text,
    -- | The namespace of the metric.
    namespace :: Lude.Text,
    -- | The dimensions associated with the metric. If the metric has any associated dimensions, you must specify them in order for the call to succeed.
    dimensions :: Lude.Maybe [Dimension],
    -- | The unit for the metric.
    unit :: Lude.Maybe StandardUnit,
    -- | The statistic for the metric, other than percentiles. For percentile statistics, use @ExtendedStatistics@ .
    statistic :: Lude.Maybe Statistic,
    -- | The percentile statistic for the metric. Specify a value between p0.0 and p100.
    extendedStatistic :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlarmsForMetric' with the minimum fields required to make a request.
--
-- * 'period' - The period, in seconds, over which the statistic is applied.
-- * 'metricName' - The name of the metric.
-- * 'namespace' - The namespace of the metric.
-- * 'dimensions' - The dimensions associated with the metric. If the metric has any associated dimensions, you must specify them in order for the call to succeed.
-- * 'unit' - The unit for the metric.
-- * 'statistic' - The statistic for the metric, other than percentiles. For percentile statistics, use @ExtendedStatistics@ .
-- * 'extendedStatistic' - The percentile statistic for the metric. Specify a value between p0.0 and p100.
mkDescribeAlarmsForMetric ::
  -- | 'metricName'
  Lude.Text ->
  -- | 'namespace'
  Lude.Text ->
  DescribeAlarmsForMetric
mkDescribeAlarmsForMetric pMetricName_ pNamespace_ =
  DescribeAlarmsForMetric'
    { period = Lude.Nothing,
      metricName = pMetricName_,
      namespace = pNamespace_,
      dimensions = Lude.Nothing,
      unit = Lude.Nothing,
      statistic = Lude.Nothing,
      extendedStatistic = Lude.Nothing
    }

-- | The period, in seconds, over which the statistic is applied.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmPeriod :: Lens.Lens' DescribeAlarmsForMetric (Lude.Maybe Lude.Natural)
dafmPeriod = Lens.lens (period :: DescribeAlarmsForMetric -> Lude.Maybe Lude.Natural) (\s a -> s {period = a} :: DescribeAlarmsForMetric)
{-# DEPRECATED dafmPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmMetricName :: Lens.Lens' DescribeAlarmsForMetric Lude.Text
dafmMetricName = Lens.lens (metricName :: DescribeAlarmsForMetric -> Lude.Text) (\s a -> s {metricName = a} :: DescribeAlarmsForMetric)
{-# DEPRECATED dafmMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmNamespace :: Lens.Lens' DescribeAlarmsForMetric Lude.Text
dafmNamespace = Lens.lens (namespace :: DescribeAlarmsForMetric -> Lude.Text) (\s a -> s {namespace = a} :: DescribeAlarmsForMetric)
{-# DEPRECATED dafmNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The dimensions associated with the metric. If the metric has any associated dimensions, you must specify them in order for the call to succeed.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmDimensions :: Lens.Lens' DescribeAlarmsForMetric (Lude.Maybe [Dimension])
dafmDimensions = Lens.lens (dimensions :: DescribeAlarmsForMetric -> Lude.Maybe [Dimension]) (\s a -> s {dimensions = a} :: DescribeAlarmsForMetric)
{-# DEPRECATED dafmDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The unit for the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmUnit :: Lens.Lens' DescribeAlarmsForMetric (Lude.Maybe StandardUnit)
dafmUnit = Lens.lens (unit :: DescribeAlarmsForMetric -> Lude.Maybe StandardUnit) (\s a -> s {unit = a} :: DescribeAlarmsForMetric)
{-# DEPRECATED dafmUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The statistic for the metric, other than percentiles. For percentile statistics, use @ExtendedStatistics@ .
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmStatistic :: Lens.Lens' DescribeAlarmsForMetric (Lude.Maybe Statistic)
dafmStatistic = Lens.lens (statistic :: DescribeAlarmsForMetric -> Lude.Maybe Statistic) (\s a -> s {statistic = a} :: DescribeAlarmsForMetric)
{-# DEPRECATED dafmStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | The percentile statistic for the metric. Specify a value between p0.0 and p100.
--
-- /Note:/ Consider using 'extendedStatistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmExtendedStatistic :: Lens.Lens' DescribeAlarmsForMetric (Lude.Maybe Lude.Text)
dafmExtendedStatistic = Lens.lens (extendedStatistic :: DescribeAlarmsForMetric -> Lude.Maybe Lude.Text) (\s a -> s {extendedStatistic = a} :: DescribeAlarmsForMetric)
{-# DEPRECATED dafmExtendedStatistic "Use generic-lens or generic-optics with 'extendedStatistic' instead." #-}

instance Lude.AWSRequest DescribeAlarmsForMetric where
  type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "DescribeAlarmsForMetricResult"
      ( \s h x ->
          DescribeAlarmsForMetricResponse'
            Lude.<$> ( x Lude..@? "MetricAlarms" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAlarmsForMetric where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAlarmsForMetric where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAlarmsForMetric where
  toQuery DescribeAlarmsForMetric' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAlarmsForMetric" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "Period" Lude.=: period,
        "MetricName" Lude.=: metricName,
        "Namespace" Lude.=: namespace,
        "Dimensions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> dimensions),
        "Unit" Lude.=: unit,
        "Statistic" Lude.=: statistic,
        "ExtendedStatistic" Lude.=: extendedStatistic
      ]

-- | /See:/ 'mkDescribeAlarmsForMetricResponse' smart constructor.
data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'
  { -- | The information for each alarm with the specified metric.
    metricAlarms :: Lude.Maybe [MetricAlarm],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlarmsForMetricResponse' with the minimum fields required to make a request.
--
-- * 'metricAlarms' - The information for each alarm with the specified metric.
-- * 'responseStatus' - The response status code.
mkDescribeAlarmsForMetricResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAlarmsForMetricResponse
mkDescribeAlarmsForMetricResponse pResponseStatus_ =
  DescribeAlarmsForMetricResponse'
    { metricAlarms = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The information for each alarm with the specified metric.
--
-- /Note:/ Consider using 'metricAlarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmrsMetricAlarms :: Lens.Lens' DescribeAlarmsForMetricResponse (Lude.Maybe [MetricAlarm])
dafmrsMetricAlarms = Lens.lens (metricAlarms :: DescribeAlarmsForMetricResponse -> Lude.Maybe [MetricAlarm]) (\s a -> s {metricAlarms = a} :: DescribeAlarmsForMetricResponse)
{-# DEPRECATED dafmrsMetricAlarms "Use generic-lens or generic-optics with 'metricAlarms' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafmrsResponseStatus :: Lens.Lens' DescribeAlarmsForMetricResponse Lude.Int
dafmrsResponseStatus = Lens.lens (responseStatus :: DescribeAlarmsForMetricResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAlarmsForMetricResponse)
{-# DEPRECATED dafmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

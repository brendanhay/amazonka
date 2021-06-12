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
-- Module      : Network.AWS.CloudWatch.DescribeAlarmsForMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the alarms for the specified metric. To filter the results,
-- specify a statistic, period, or unit.
--
-- This operation retrieves only standard alarms that are based on the
-- specified metric. It does not return alarms based on math expressions
-- that use the specified metric, or composite alarms that use the
-- specified metric.
module Network.AWS.CloudWatch.DescribeAlarmsForMetric
  ( -- * Creating a Request
    DescribeAlarmsForMetric (..),
    newDescribeAlarmsForMetric,

    -- * Request Lenses
    describeAlarmsForMetric_extendedStatistic,
    describeAlarmsForMetric_unit,
    describeAlarmsForMetric_statistic,
    describeAlarmsForMetric_dimensions,
    describeAlarmsForMetric_period,
    describeAlarmsForMetric_metricName,
    describeAlarmsForMetric_namespace,

    -- * Destructuring the Response
    DescribeAlarmsForMetricResponse (..),
    newDescribeAlarmsForMetricResponse,

    -- * Response Lenses
    describeAlarmsForMetricResponse_metricAlarms,
    describeAlarmsForMetricResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAlarmsForMetric' smart constructor.
data DescribeAlarmsForMetric = DescribeAlarmsForMetric'
  { -- | The percentile statistic for the metric. Specify a value between p0.0
    -- and p100.
    extendedStatistic :: Core.Maybe Core.Text,
    -- | The unit for the metric.
    unit :: Core.Maybe StandardUnit,
    -- | The statistic for the metric, other than percentiles. For percentile
    -- statistics, use @ExtendedStatistics@.
    statistic :: Core.Maybe Statistic,
    -- | The dimensions associated with the metric. If the metric has any
    -- associated dimensions, you must specify them in order for the call to
    -- succeed.
    dimensions :: Core.Maybe [Dimension],
    -- | The period, in seconds, over which the statistic is applied.
    period :: Core.Maybe Core.Natural,
    -- | The name of the metric.
    metricName :: Core.Text,
    -- | The namespace of the metric.
    namespace :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAlarmsForMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extendedStatistic', 'describeAlarmsForMetric_extendedStatistic' - The percentile statistic for the metric. Specify a value between p0.0
-- and p100.
--
-- 'unit', 'describeAlarmsForMetric_unit' - The unit for the metric.
--
-- 'statistic', 'describeAlarmsForMetric_statistic' - The statistic for the metric, other than percentiles. For percentile
-- statistics, use @ExtendedStatistics@.
--
-- 'dimensions', 'describeAlarmsForMetric_dimensions' - The dimensions associated with the metric. If the metric has any
-- associated dimensions, you must specify them in order for the call to
-- succeed.
--
-- 'period', 'describeAlarmsForMetric_period' - The period, in seconds, over which the statistic is applied.
--
-- 'metricName', 'describeAlarmsForMetric_metricName' - The name of the metric.
--
-- 'namespace', 'describeAlarmsForMetric_namespace' - The namespace of the metric.
newDescribeAlarmsForMetric ::
  -- | 'metricName'
  Core.Text ->
  -- | 'namespace'
  Core.Text ->
  DescribeAlarmsForMetric
newDescribeAlarmsForMetric pMetricName_ pNamespace_ =
  DescribeAlarmsForMetric'
    { extendedStatistic =
        Core.Nothing,
      unit = Core.Nothing,
      statistic = Core.Nothing,
      dimensions = Core.Nothing,
      period = Core.Nothing,
      metricName = pMetricName_,
      namespace = pNamespace_
    }

-- | The percentile statistic for the metric. Specify a value between p0.0
-- and p100.
describeAlarmsForMetric_extendedStatistic :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Core.Text)
describeAlarmsForMetric_extendedStatistic = Lens.lens (\DescribeAlarmsForMetric' {extendedStatistic} -> extendedStatistic) (\s@DescribeAlarmsForMetric' {} a -> s {extendedStatistic = a} :: DescribeAlarmsForMetric)

-- | The unit for the metric.
describeAlarmsForMetric_unit :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe StandardUnit)
describeAlarmsForMetric_unit = Lens.lens (\DescribeAlarmsForMetric' {unit} -> unit) (\s@DescribeAlarmsForMetric' {} a -> s {unit = a} :: DescribeAlarmsForMetric)

-- | The statistic for the metric, other than percentiles. For percentile
-- statistics, use @ExtendedStatistics@.
describeAlarmsForMetric_statistic :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Statistic)
describeAlarmsForMetric_statistic = Lens.lens (\DescribeAlarmsForMetric' {statistic} -> statistic) (\s@DescribeAlarmsForMetric' {} a -> s {statistic = a} :: DescribeAlarmsForMetric)

-- | The dimensions associated with the metric. If the metric has any
-- associated dimensions, you must specify them in order for the call to
-- succeed.
describeAlarmsForMetric_dimensions :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe [Dimension])
describeAlarmsForMetric_dimensions = Lens.lens (\DescribeAlarmsForMetric' {dimensions} -> dimensions) (\s@DescribeAlarmsForMetric' {} a -> s {dimensions = a} :: DescribeAlarmsForMetric) Core.. Lens.mapping Lens._Coerce

-- | The period, in seconds, over which the statistic is applied.
describeAlarmsForMetric_period :: Lens.Lens' DescribeAlarmsForMetric (Core.Maybe Core.Natural)
describeAlarmsForMetric_period = Lens.lens (\DescribeAlarmsForMetric' {period} -> period) (\s@DescribeAlarmsForMetric' {} a -> s {period = a} :: DescribeAlarmsForMetric)

-- | The name of the metric.
describeAlarmsForMetric_metricName :: Lens.Lens' DescribeAlarmsForMetric Core.Text
describeAlarmsForMetric_metricName = Lens.lens (\DescribeAlarmsForMetric' {metricName} -> metricName) (\s@DescribeAlarmsForMetric' {} a -> s {metricName = a} :: DescribeAlarmsForMetric)

-- | The namespace of the metric.
describeAlarmsForMetric_namespace :: Lens.Lens' DescribeAlarmsForMetric Core.Text
describeAlarmsForMetric_namespace = Lens.lens (\DescribeAlarmsForMetric' {namespace} -> namespace) (\s@DescribeAlarmsForMetric' {} a -> s {namespace = a} :: DescribeAlarmsForMetric)

instance Core.AWSRequest DescribeAlarmsForMetric where
  type
    AWSResponse DescribeAlarmsForMetric =
      DescribeAlarmsForMetricResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAlarmsForMetricResult"
      ( \s h x ->
          DescribeAlarmsForMetricResponse'
            Core.<$> ( x Core..@? "MetricAlarms" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAlarmsForMetric

instance Core.NFData DescribeAlarmsForMetric

instance Core.ToHeaders DescribeAlarmsForMetric where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeAlarmsForMetric where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAlarmsForMetric where
  toQuery DescribeAlarmsForMetric' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeAlarmsForMetric" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "ExtendedStatistic" Core.=: extendedStatistic,
        "Unit" Core.=: unit,
        "Statistic" Core.=: statistic,
        "Dimensions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> dimensions),
        "Period" Core.=: period,
        "MetricName" Core.=: metricName,
        "Namespace" Core.=: namespace
      ]

-- | /See:/ 'newDescribeAlarmsForMetricResponse' smart constructor.
data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'
  { -- | The information for each alarm with the specified metric.
    metricAlarms :: Core.Maybe [MetricAlarm],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAlarmsForMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricAlarms', 'describeAlarmsForMetricResponse_metricAlarms' - The information for each alarm with the specified metric.
--
-- 'httpStatus', 'describeAlarmsForMetricResponse_httpStatus' - The response's http status code.
newDescribeAlarmsForMetricResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAlarmsForMetricResponse
newDescribeAlarmsForMetricResponse pHttpStatus_ =
  DescribeAlarmsForMetricResponse'
    { metricAlarms =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The information for each alarm with the specified metric.
describeAlarmsForMetricResponse_metricAlarms :: Lens.Lens' DescribeAlarmsForMetricResponse (Core.Maybe [MetricAlarm])
describeAlarmsForMetricResponse_metricAlarms = Lens.lens (\DescribeAlarmsForMetricResponse' {metricAlarms} -> metricAlarms) (\s@DescribeAlarmsForMetricResponse' {} a -> s {metricAlarms = a} :: DescribeAlarmsForMetricResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAlarmsForMetricResponse_httpStatus :: Lens.Lens' DescribeAlarmsForMetricResponse Core.Int
describeAlarmsForMetricResponse_httpStatus = Lens.lens (\DescribeAlarmsForMetricResponse' {httpStatus} -> httpStatus) (\s@DescribeAlarmsForMetricResponse' {} a -> s {httpStatus = a} :: DescribeAlarmsForMetricResponse)

instance Core.NFData DescribeAlarmsForMetricResponse

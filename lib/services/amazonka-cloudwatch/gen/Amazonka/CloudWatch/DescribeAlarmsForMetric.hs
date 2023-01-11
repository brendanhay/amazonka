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
-- Module      : Amazonka.CloudWatch.DescribeAlarmsForMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CloudWatch.DescribeAlarmsForMetric
  ( -- * Creating a Request
    DescribeAlarmsForMetric (..),
    newDescribeAlarmsForMetric,

    -- * Request Lenses
    describeAlarmsForMetric_dimensions,
    describeAlarmsForMetric_extendedStatistic,
    describeAlarmsForMetric_period,
    describeAlarmsForMetric_statistic,
    describeAlarmsForMetric_unit,
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

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAlarmsForMetric' smart constructor.
data DescribeAlarmsForMetric = DescribeAlarmsForMetric'
  { -- | The dimensions associated with the metric. If the metric has any
    -- associated dimensions, you must specify them in order for the call to
    -- succeed.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The percentile statistic for the metric. Specify a value between p0.0
    -- and p100.
    extendedStatistic :: Prelude.Maybe Prelude.Text,
    -- | The period, in seconds, over which the statistic is applied.
    period :: Prelude.Maybe Prelude.Natural,
    -- | The statistic for the metric, other than percentiles. For percentile
    -- statistics, use @ExtendedStatistics@.
    statistic :: Prelude.Maybe Statistic,
    -- | The unit for the metric.
    unit :: Prelude.Maybe StandardUnit,
    -- | The name of the metric.
    metricName :: Prelude.Text,
    -- | The namespace of the metric.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAlarmsForMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'describeAlarmsForMetric_dimensions' - The dimensions associated with the metric. If the metric has any
-- associated dimensions, you must specify them in order for the call to
-- succeed.
--
-- 'extendedStatistic', 'describeAlarmsForMetric_extendedStatistic' - The percentile statistic for the metric. Specify a value between p0.0
-- and p100.
--
-- 'period', 'describeAlarmsForMetric_period' - The period, in seconds, over which the statistic is applied.
--
-- 'statistic', 'describeAlarmsForMetric_statistic' - The statistic for the metric, other than percentiles. For percentile
-- statistics, use @ExtendedStatistics@.
--
-- 'unit', 'describeAlarmsForMetric_unit' - The unit for the metric.
--
-- 'metricName', 'describeAlarmsForMetric_metricName' - The name of the metric.
--
-- 'namespace', 'describeAlarmsForMetric_namespace' - The namespace of the metric.
newDescribeAlarmsForMetric ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  DescribeAlarmsForMetric
newDescribeAlarmsForMetric pMetricName_ pNamespace_ =
  DescribeAlarmsForMetric'
    { dimensions =
        Prelude.Nothing,
      extendedStatistic = Prelude.Nothing,
      period = Prelude.Nothing,
      statistic = Prelude.Nothing,
      unit = Prelude.Nothing,
      metricName = pMetricName_,
      namespace = pNamespace_
    }

-- | The dimensions associated with the metric. If the metric has any
-- associated dimensions, you must specify them in order for the call to
-- succeed.
describeAlarmsForMetric_dimensions :: Lens.Lens' DescribeAlarmsForMetric (Prelude.Maybe [Dimension])
describeAlarmsForMetric_dimensions = Lens.lens (\DescribeAlarmsForMetric' {dimensions} -> dimensions) (\s@DescribeAlarmsForMetric' {} a -> s {dimensions = a} :: DescribeAlarmsForMetric) Prelude.. Lens.mapping Lens.coerced

-- | The percentile statistic for the metric. Specify a value between p0.0
-- and p100.
describeAlarmsForMetric_extendedStatistic :: Lens.Lens' DescribeAlarmsForMetric (Prelude.Maybe Prelude.Text)
describeAlarmsForMetric_extendedStatistic = Lens.lens (\DescribeAlarmsForMetric' {extendedStatistic} -> extendedStatistic) (\s@DescribeAlarmsForMetric' {} a -> s {extendedStatistic = a} :: DescribeAlarmsForMetric)

-- | The period, in seconds, over which the statistic is applied.
describeAlarmsForMetric_period :: Lens.Lens' DescribeAlarmsForMetric (Prelude.Maybe Prelude.Natural)
describeAlarmsForMetric_period = Lens.lens (\DescribeAlarmsForMetric' {period} -> period) (\s@DescribeAlarmsForMetric' {} a -> s {period = a} :: DescribeAlarmsForMetric)

-- | The statistic for the metric, other than percentiles. For percentile
-- statistics, use @ExtendedStatistics@.
describeAlarmsForMetric_statistic :: Lens.Lens' DescribeAlarmsForMetric (Prelude.Maybe Statistic)
describeAlarmsForMetric_statistic = Lens.lens (\DescribeAlarmsForMetric' {statistic} -> statistic) (\s@DescribeAlarmsForMetric' {} a -> s {statistic = a} :: DescribeAlarmsForMetric)

-- | The unit for the metric.
describeAlarmsForMetric_unit :: Lens.Lens' DescribeAlarmsForMetric (Prelude.Maybe StandardUnit)
describeAlarmsForMetric_unit = Lens.lens (\DescribeAlarmsForMetric' {unit} -> unit) (\s@DescribeAlarmsForMetric' {} a -> s {unit = a} :: DescribeAlarmsForMetric)

-- | The name of the metric.
describeAlarmsForMetric_metricName :: Lens.Lens' DescribeAlarmsForMetric Prelude.Text
describeAlarmsForMetric_metricName = Lens.lens (\DescribeAlarmsForMetric' {metricName} -> metricName) (\s@DescribeAlarmsForMetric' {} a -> s {metricName = a} :: DescribeAlarmsForMetric)

-- | The namespace of the metric.
describeAlarmsForMetric_namespace :: Lens.Lens' DescribeAlarmsForMetric Prelude.Text
describeAlarmsForMetric_namespace = Lens.lens (\DescribeAlarmsForMetric' {namespace} -> namespace) (\s@DescribeAlarmsForMetric' {} a -> s {namespace = a} :: DescribeAlarmsForMetric)

instance Core.AWSRequest DescribeAlarmsForMetric where
  type
    AWSResponse DescribeAlarmsForMetric =
      DescribeAlarmsForMetricResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAlarmsForMetricResult"
      ( \s h x ->
          DescribeAlarmsForMetricResponse'
            Prelude.<$> ( x Data..@? "MetricAlarms" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAlarmsForMetric where
  hashWithSalt _salt DescribeAlarmsForMetric' {..} =
    _salt `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` extendedStatistic
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` statistic
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DescribeAlarmsForMetric where
  rnf DescribeAlarmsForMetric' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf extendedStatistic
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf statistic
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders DescribeAlarmsForMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAlarmsForMetric where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAlarmsForMetric where
  toQuery DescribeAlarmsForMetric' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeAlarmsForMetric" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "Dimensions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> dimensions),
        "ExtendedStatistic" Data.=: extendedStatistic,
        "Period" Data.=: period,
        "Statistic" Data.=: statistic,
        "Unit" Data.=: unit,
        "MetricName" Data.=: metricName,
        "Namespace" Data.=: namespace
      ]

-- | /See:/ 'newDescribeAlarmsForMetricResponse' smart constructor.
data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse'
  { -- | The information for each alarm with the specified metric.
    metricAlarms :: Prelude.Maybe [MetricAlarm],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeAlarmsForMetricResponse
newDescribeAlarmsForMetricResponse pHttpStatus_ =
  DescribeAlarmsForMetricResponse'
    { metricAlarms =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The information for each alarm with the specified metric.
describeAlarmsForMetricResponse_metricAlarms :: Lens.Lens' DescribeAlarmsForMetricResponse (Prelude.Maybe [MetricAlarm])
describeAlarmsForMetricResponse_metricAlarms = Lens.lens (\DescribeAlarmsForMetricResponse' {metricAlarms} -> metricAlarms) (\s@DescribeAlarmsForMetricResponse' {} a -> s {metricAlarms = a} :: DescribeAlarmsForMetricResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAlarmsForMetricResponse_httpStatus :: Lens.Lens' DescribeAlarmsForMetricResponse Prelude.Int
describeAlarmsForMetricResponse_httpStatus = Lens.lens (\DescribeAlarmsForMetricResponse' {httpStatus} -> httpStatus) (\s@DescribeAlarmsForMetricResponse' {} a -> s {httpStatus = a} :: DescribeAlarmsForMetricResponse)

instance
  Prelude.NFData
    DescribeAlarmsForMetricResponse
  where
  rnf DescribeAlarmsForMetricResponse' {..} =
    Prelude.rnf metricAlarms
      `Prelude.seq` Prelude.rnf httpStatus

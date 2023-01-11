{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AutoScaling.Types.MetricStat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.MetricStat where

import Amazonka.AutoScaling.Types.Metric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure defines the CloudWatch metric to return, along with the
-- statistic, period, and unit.
--
-- For more information about the CloudWatch terminology below, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch concepts>
-- in the /Amazon CloudWatch User Guide/.
--
-- /See:/ 'newMetricStat' smart constructor.
data MetricStat = MetricStat'
  { -- | The unit to use for the returned data points. For a complete list of the
    -- units that CloudWatch supports, see the
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html MetricDatum>
    -- data type in the /Amazon CloudWatch API Reference/.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The CloudWatch metric to return, including the metric name, namespace,
    -- and dimensions. To get the exact metric name, namespace, and dimensions,
    -- inspect the
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Metric.html Metric>
    -- object that is returned by a call to
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html ListMetrics>.
    metric :: Metric,
    -- | The statistic to return. It can include any CloudWatch statistic or
    -- extended statistic. For a list of valid values, see the table in
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
    -- in the /Amazon CloudWatch User Guide/.
    --
    -- The most commonly used metrics for predictive scaling are @Average@ and
    -- @Sum@.
    stat :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricStat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'metricStat_unit' - The unit to use for the returned data points. For a complete list of the
-- units that CloudWatch supports, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html MetricDatum>
-- data type in the /Amazon CloudWatch API Reference/.
--
-- 'metric', 'metricStat_metric' - The CloudWatch metric to return, including the metric name, namespace,
-- and dimensions. To get the exact metric name, namespace, and dimensions,
-- inspect the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Metric.html Metric>
-- object that is returned by a call to
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html ListMetrics>.
--
-- 'stat', 'metricStat_stat' - The statistic to return. It can include any CloudWatch statistic or
-- extended statistic. For a list of valid values, see the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch User Guide/.
--
-- The most commonly used metrics for predictive scaling are @Average@ and
-- @Sum@.
newMetricStat ::
  -- | 'metric'
  Metric ->
  -- | 'stat'
  Prelude.Text ->
  MetricStat
newMetricStat pMetric_ pStat_ =
  MetricStat'
    { unit = Prelude.Nothing,
      metric = pMetric_,
      stat = pStat_
    }

-- | The unit to use for the returned data points. For a complete list of the
-- units that CloudWatch supports, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html MetricDatum>
-- data type in the /Amazon CloudWatch API Reference/.
metricStat_unit :: Lens.Lens' MetricStat (Prelude.Maybe Prelude.Text)
metricStat_unit = Lens.lens (\MetricStat' {unit} -> unit) (\s@MetricStat' {} a -> s {unit = a} :: MetricStat)

-- | The CloudWatch metric to return, including the metric name, namespace,
-- and dimensions. To get the exact metric name, namespace, and dimensions,
-- inspect the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Metric.html Metric>
-- object that is returned by a call to
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html ListMetrics>.
metricStat_metric :: Lens.Lens' MetricStat Metric
metricStat_metric = Lens.lens (\MetricStat' {metric} -> metric) (\s@MetricStat' {} a -> s {metric = a} :: MetricStat)

-- | The statistic to return. It can include any CloudWatch statistic or
-- extended statistic. For a list of valid values, see the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch User Guide/.
--
-- The most commonly used metrics for predictive scaling are @Average@ and
-- @Sum@.
metricStat_stat :: Lens.Lens' MetricStat Prelude.Text
metricStat_stat = Lens.lens (\MetricStat' {stat} -> stat) (\s@MetricStat' {} a -> s {stat = a} :: MetricStat)

instance Data.FromXML MetricStat where
  parseXML x =
    MetricStat'
      Prelude.<$> (x Data..@? "Unit")
      Prelude.<*> (x Data..@ "Metric")
      Prelude.<*> (x Data..@ "Stat")

instance Prelude.Hashable MetricStat where
  hashWithSalt _salt MetricStat' {..} =
    _salt `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` stat

instance Prelude.NFData MetricStat where
  rnf MetricStat' {..} =
    Prelude.rnf unit
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf stat

instance Data.ToQuery MetricStat where
  toQuery MetricStat' {..} =
    Prelude.mconcat
      [ "Unit" Data.=: unit,
        "Metric" Data.=: metric,
        "Stat" Data.=: stat
      ]

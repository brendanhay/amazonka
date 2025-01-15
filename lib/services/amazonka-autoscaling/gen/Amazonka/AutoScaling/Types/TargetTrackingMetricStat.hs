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
-- Module      : Amazonka.AutoScaling.Types.TargetTrackingMetricStat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.TargetTrackingMetricStat where

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
-- /See:/ 'newTargetTrackingMetricStat' smart constructor.
data TargetTrackingMetricStat = TargetTrackingMetricStat'
  { -- | The unit to use for the returned data points. For a complete list of the
    -- units that CloudWatch supports, see the
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html MetricDatum>
    -- data type in the /Amazon CloudWatch API Reference/.
    unit :: Prelude.Maybe Prelude.Text,
    metric :: Metric,
    -- | The statistic to return. It can include any CloudWatch statistic or
    -- extended statistic. For a list of valid values, see the table in
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
    -- in the /Amazon CloudWatch User Guide/.
    --
    -- The most commonly used metrics for scaling is @Average@
    stat :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetTrackingMetricStat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'targetTrackingMetricStat_unit' - The unit to use for the returned data points. For a complete list of the
-- units that CloudWatch supports, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html MetricDatum>
-- data type in the /Amazon CloudWatch API Reference/.
--
-- 'metric', 'targetTrackingMetricStat_metric' - Undocumented member.
--
-- 'stat', 'targetTrackingMetricStat_stat' - The statistic to return. It can include any CloudWatch statistic or
-- extended statistic. For a list of valid values, see the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch User Guide/.
--
-- The most commonly used metrics for scaling is @Average@
newTargetTrackingMetricStat ::
  -- | 'metric'
  Metric ->
  -- | 'stat'
  Prelude.Text ->
  TargetTrackingMetricStat
newTargetTrackingMetricStat pMetric_ pStat_ =
  TargetTrackingMetricStat'
    { unit = Prelude.Nothing,
      metric = pMetric_,
      stat = pStat_
    }

-- | The unit to use for the returned data points. For a complete list of the
-- units that CloudWatch supports, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html MetricDatum>
-- data type in the /Amazon CloudWatch API Reference/.
targetTrackingMetricStat_unit :: Lens.Lens' TargetTrackingMetricStat (Prelude.Maybe Prelude.Text)
targetTrackingMetricStat_unit = Lens.lens (\TargetTrackingMetricStat' {unit} -> unit) (\s@TargetTrackingMetricStat' {} a -> s {unit = a} :: TargetTrackingMetricStat)

-- | Undocumented member.
targetTrackingMetricStat_metric :: Lens.Lens' TargetTrackingMetricStat Metric
targetTrackingMetricStat_metric = Lens.lens (\TargetTrackingMetricStat' {metric} -> metric) (\s@TargetTrackingMetricStat' {} a -> s {metric = a} :: TargetTrackingMetricStat)

-- | The statistic to return. It can include any CloudWatch statistic or
-- extended statistic. For a list of valid values, see the table in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch User Guide/.
--
-- The most commonly used metrics for scaling is @Average@
targetTrackingMetricStat_stat :: Lens.Lens' TargetTrackingMetricStat Prelude.Text
targetTrackingMetricStat_stat = Lens.lens (\TargetTrackingMetricStat' {stat} -> stat) (\s@TargetTrackingMetricStat' {} a -> s {stat = a} :: TargetTrackingMetricStat)

instance Data.FromXML TargetTrackingMetricStat where
  parseXML x =
    TargetTrackingMetricStat'
      Prelude.<$> (x Data..@? "Unit")
      Prelude.<*> (x Data..@ "Metric")
      Prelude.<*> (x Data..@ "Stat")

instance Prelude.Hashable TargetTrackingMetricStat where
  hashWithSalt _salt TargetTrackingMetricStat' {..} =
    _salt
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` stat

instance Prelude.NFData TargetTrackingMetricStat where
  rnf TargetTrackingMetricStat' {..} =
    Prelude.rnf unit `Prelude.seq`
      Prelude.rnf metric `Prelude.seq`
        Prelude.rnf stat

instance Data.ToQuery TargetTrackingMetricStat where
  toQuery TargetTrackingMetricStat' {..} =
    Prelude.mconcat
      [ "Unit" Data.=: unit,
        "Metric" Data.=: metric,
        "Stat" Data.=: stat
      ]

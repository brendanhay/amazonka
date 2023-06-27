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
-- Module      : Amazonka.EMR.Types.CloudWatchAlarmDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.CloudWatchAlarmDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.ComparisonOperator
import Amazonka.EMR.Types.MetricDimension
import Amazonka.EMR.Types.Statistic
import Amazonka.EMR.Types.Unit
import qualified Amazonka.Prelude as Prelude

-- | The definition of a CloudWatch metric alarm, which determines when an
-- automatic scaling activity is triggered. When the defined alarm
-- conditions are satisfied, scaling activity begins.
--
-- /See:/ 'newCloudWatchAlarmDefinition' smart constructor.
data CloudWatchAlarmDefinition = CloudWatchAlarmDefinition'
  { -- | A CloudWatch metric dimension.
    dimensions :: Prelude.Maybe [MetricDimension],
    -- | The number of periods, in five-minute increments, during which the alarm
    -- condition must exist before the alarm triggers automatic scaling
    -- activity. The default value is @1@.
    evaluationPeriods :: Prelude.Maybe Prelude.Int,
    -- | The namespace for the CloudWatch metric. The default is
    -- @AWS\/ElasticMapReduce@.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The statistic to apply to the metric associated with the alarm. The
    -- default is @AVERAGE@.
    statistic :: Prelude.Maybe Statistic,
    -- | The unit of measure associated with the CloudWatch metric being watched.
    -- The value specified for @Unit@ must correspond to the units specified in
    -- the CloudWatch metric.
    unit :: Prelude.Maybe Unit,
    -- | Determines how the metric specified by @MetricName@ is compared to the
    -- value specified by @Threshold@.
    comparisonOperator :: ComparisonOperator,
    -- | The name of the CloudWatch metric that is watched to determine an alarm
    -- condition.
    metricName :: Prelude.Text,
    -- | The period, in seconds, over which the statistic is applied. CloudWatch
    -- metrics for Amazon EMR are emitted every five minutes (300 seconds), so
    -- if you specify a CloudWatch metric, specify @300@.
    period :: Prelude.Int,
    -- | The value against which the specified statistic is compared.
    threshold :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchAlarmDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'cloudWatchAlarmDefinition_dimensions' - A CloudWatch metric dimension.
--
-- 'evaluationPeriods', 'cloudWatchAlarmDefinition_evaluationPeriods' - The number of periods, in five-minute increments, during which the alarm
-- condition must exist before the alarm triggers automatic scaling
-- activity. The default value is @1@.
--
-- 'namespace', 'cloudWatchAlarmDefinition_namespace' - The namespace for the CloudWatch metric. The default is
-- @AWS\/ElasticMapReduce@.
--
-- 'statistic', 'cloudWatchAlarmDefinition_statistic' - The statistic to apply to the metric associated with the alarm. The
-- default is @AVERAGE@.
--
-- 'unit', 'cloudWatchAlarmDefinition_unit' - The unit of measure associated with the CloudWatch metric being watched.
-- The value specified for @Unit@ must correspond to the units specified in
-- the CloudWatch metric.
--
-- 'comparisonOperator', 'cloudWatchAlarmDefinition_comparisonOperator' - Determines how the metric specified by @MetricName@ is compared to the
-- value specified by @Threshold@.
--
-- 'metricName', 'cloudWatchAlarmDefinition_metricName' - The name of the CloudWatch metric that is watched to determine an alarm
-- condition.
--
-- 'period', 'cloudWatchAlarmDefinition_period' - The period, in seconds, over which the statistic is applied. CloudWatch
-- metrics for Amazon EMR are emitted every five minutes (300 seconds), so
-- if you specify a CloudWatch metric, specify @300@.
--
-- 'threshold', 'cloudWatchAlarmDefinition_threshold' - The value against which the specified statistic is compared.
newCloudWatchAlarmDefinition ::
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'period'
  Prelude.Int ->
  -- | 'threshold'
  Prelude.Double ->
  CloudWatchAlarmDefinition
newCloudWatchAlarmDefinition
  pComparisonOperator_
  pMetricName_
  pPeriod_
  pThreshold_ =
    CloudWatchAlarmDefinition'
      { dimensions =
          Prelude.Nothing,
        evaluationPeriods = Prelude.Nothing,
        namespace = Prelude.Nothing,
        statistic = Prelude.Nothing,
        unit = Prelude.Nothing,
        comparisonOperator = pComparisonOperator_,
        metricName = pMetricName_,
        period = pPeriod_,
        threshold = pThreshold_
      }

-- | A CloudWatch metric dimension.
cloudWatchAlarmDefinition_dimensions :: Lens.Lens' CloudWatchAlarmDefinition (Prelude.Maybe [MetricDimension])
cloudWatchAlarmDefinition_dimensions = Lens.lens (\CloudWatchAlarmDefinition' {dimensions} -> dimensions) (\s@CloudWatchAlarmDefinition' {} a -> s {dimensions = a} :: CloudWatchAlarmDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The number of periods, in five-minute increments, during which the alarm
-- condition must exist before the alarm triggers automatic scaling
-- activity. The default value is @1@.
cloudWatchAlarmDefinition_evaluationPeriods :: Lens.Lens' CloudWatchAlarmDefinition (Prelude.Maybe Prelude.Int)
cloudWatchAlarmDefinition_evaluationPeriods = Lens.lens (\CloudWatchAlarmDefinition' {evaluationPeriods} -> evaluationPeriods) (\s@CloudWatchAlarmDefinition' {} a -> s {evaluationPeriods = a} :: CloudWatchAlarmDefinition)

-- | The namespace for the CloudWatch metric. The default is
-- @AWS\/ElasticMapReduce@.
cloudWatchAlarmDefinition_namespace :: Lens.Lens' CloudWatchAlarmDefinition (Prelude.Maybe Prelude.Text)
cloudWatchAlarmDefinition_namespace = Lens.lens (\CloudWatchAlarmDefinition' {namespace} -> namespace) (\s@CloudWatchAlarmDefinition' {} a -> s {namespace = a} :: CloudWatchAlarmDefinition)

-- | The statistic to apply to the metric associated with the alarm. The
-- default is @AVERAGE@.
cloudWatchAlarmDefinition_statistic :: Lens.Lens' CloudWatchAlarmDefinition (Prelude.Maybe Statistic)
cloudWatchAlarmDefinition_statistic = Lens.lens (\CloudWatchAlarmDefinition' {statistic} -> statistic) (\s@CloudWatchAlarmDefinition' {} a -> s {statistic = a} :: CloudWatchAlarmDefinition)

-- | The unit of measure associated with the CloudWatch metric being watched.
-- The value specified for @Unit@ must correspond to the units specified in
-- the CloudWatch metric.
cloudWatchAlarmDefinition_unit :: Lens.Lens' CloudWatchAlarmDefinition (Prelude.Maybe Unit)
cloudWatchAlarmDefinition_unit = Lens.lens (\CloudWatchAlarmDefinition' {unit} -> unit) (\s@CloudWatchAlarmDefinition' {} a -> s {unit = a} :: CloudWatchAlarmDefinition)

-- | Determines how the metric specified by @MetricName@ is compared to the
-- value specified by @Threshold@.
cloudWatchAlarmDefinition_comparisonOperator :: Lens.Lens' CloudWatchAlarmDefinition ComparisonOperator
cloudWatchAlarmDefinition_comparisonOperator = Lens.lens (\CloudWatchAlarmDefinition' {comparisonOperator} -> comparisonOperator) (\s@CloudWatchAlarmDefinition' {} a -> s {comparisonOperator = a} :: CloudWatchAlarmDefinition)

-- | The name of the CloudWatch metric that is watched to determine an alarm
-- condition.
cloudWatchAlarmDefinition_metricName :: Lens.Lens' CloudWatchAlarmDefinition Prelude.Text
cloudWatchAlarmDefinition_metricName = Lens.lens (\CloudWatchAlarmDefinition' {metricName} -> metricName) (\s@CloudWatchAlarmDefinition' {} a -> s {metricName = a} :: CloudWatchAlarmDefinition)

-- | The period, in seconds, over which the statistic is applied. CloudWatch
-- metrics for Amazon EMR are emitted every five minutes (300 seconds), so
-- if you specify a CloudWatch metric, specify @300@.
cloudWatchAlarmDefinition_period :: Lens.Lens' CloudWatchAlarmDefinition Prelude.Int
cloudWatchAlarmDefinition_period = Lens.lens (\CloudWatchAlarmDefinition' {period} -> period) (\s@CloudWatchAlarmDefinition' {} a -> s {period = a} :: CloudWatchAlarmDefinition)

-- | The value against which the specified statistic is compared.
cloudWatchAlarmDefinition_threshold :: Lens.Lens' CloudWatchAlarmDefinition Prelude.Double
cloudWatchAlarmDefinition_threshold = Lens.lens (\CloudWatchAlarmDefinition' {threshold} -> threshold) (\s@CloudWatchAlarmDefinition' {} a -> s {threshold = a} :: CloudWatchAlarmDefinition)

instance Data.FromJSON CloudWatchAlarmDefinition where
  parseJSON =
    Data.withObject
      "CloudWatchAlarmDefinition"
      ( \x ->
          CloudWatchAlarmDefinition'
            Prelude.<$> (x Data..:? "Dimensions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EvaluationPeriods")
            Prelude.<*> (x Data..:? "Namespace")
            Prelude.<*> (x Data..:? "Statistic")
            Prelude.<*> (x Data..:? "Unit")
            Prelude.<*> (x Data..: "ComparisonOperator")
            Prelude.<*> (x Data..: "MetricName")
            Prelude.<*> (x Data..: "Period")
            Prelude.<*> (x Data..: "Threshold")
      )

instance Prelude.Hashable CloudWatchAlarmDefinition where
  hashWithSalt _salt CloudWatchAlarmDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` evaluationPeriods
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` statistic
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` threshold

instance Prelude.NFData CloudWatchAlarmDefinition where
  rnf CloudWatchAlarmDefinition' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf evaluationPeriods
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf statistic
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf threshold

instance Data.ToJSON CloudWatchAlarmDefinition where
  toJSON CloudWatchAlarmDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Dimensions" Data..=) Prelude.<$> dimensions,
            ("EvaluationPeriods" Data..=)
              Prelude.<$> evaluationPeriods,
            ("Namespace" Data..=) Prelude.<$> namespace,
            ("Statistic" Data..=) Prelude.<$> statistic,
            ("Unit" Data..=) Prelude.<$> unit,
            Prelude.Just
              ("ComparisonOperator" Data..= comparisonOperator),
            Prelude.Just ("MetricName" Data..= metricName),
            Prelude.Just ("Period" Data..= period),
            Prelude.Just ("Threshold" Data..= threshold)
          ]
      )

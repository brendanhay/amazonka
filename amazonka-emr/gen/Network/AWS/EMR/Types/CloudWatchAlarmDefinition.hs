{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.Types.CloudWatchAlarmDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.CloudWatchAlarmDefinition where

import Network.AWS.EMR.Types.ComparisonOperator
import Network.AWS.EMR.Types.MetricDimension
import Network.AWS.EMR.Types.Statistic
import Network.AWS.EMR.Types.Unit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The definition of a CloudWatch metric alarm, which determines when an
-- automatic scaling activity is triggered. When the defined alarm
-- conditions are satisfied, scaling activity begins.
--
-- /See:/ 'newCloudWatchAlarmDefinition' smart constructor.
data CloudWatchAlarmDefinition = CloudWatchAlarmDefinition'
  { -- | The unit of measure associated with the CloudWatch metric being watched.
    -- The value specified for @Unit@ must correspond to the units specified in
    -- the CloudWatch metric.
    unit :: Prelude.Maybe Unit,
    -- | The statistic to apply to the metric associated with the alarm. The
    -- default is @AVERAGE@.
    statistic :: Prelude.Maybe Statistic,
    -- | A CloudWatch metric dimension.
    dimensions :: Prelude.Maybe [MetricDimension],
    -- | The namespace for the CloudWatch metric. The default is
    -- @AWS\/ElasticMapReduce@.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The number of periods, in five-minute increments, during which the alarm
    -- condition must exist before the alarm triggers automatic scaling
    -- activity. The default value is @1@.
    evaluationPeriods :: Prelude.Maybe Prelude.Int,
    -- | Determines how the metric specified by @MetricName@ is compared to the
    -- value specified by @Threshold@.
    comparisonOperator :: ComparisonOperator,
    -- | The name of the CloudWatch metric that is watched to determine an alarm
    -- condition.
    metricName :: Prelude.Text,
    -- | The period, in seconds, over which the statistic is applied. EMR
    -- CloudWatch metrics are emitted every five minutes (300 seconds), so if
    -- an EMR CloudWatch metric is specified, specify @300@.
    period :: Prelude.Int,
    -- | The value against which the specified statistic is compared.
    threshold :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchAlarmDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'cloudWatchAlarmDefinition_unit' - The unit of measure associated with the CloudWatch metric being watched.
-- The value specified for @Unit@ must correspond to the units specified in
-- the CloudWatch metric.
--
-- 'statistic', 'cloudWatchAlarmDefinition_statistic' - The statistic to apply to the metric associated with the alarm. The
-- default is @AVERAGE@.
--
-- 'dimensions', 'cloudWatchAlarmDefinition_dimensions' - A CloudWatch metric dimension.
--
-- 'namespace', 'cloudWatchAlarmDefinition_namespace' - The namespace for the CloudWatch metric. The default is
-- @AWS\/ElasticMapReduce@.
--
-- 'evaluationPeriods', 'cloudWatchAlarmDefinition_evaluationPeriods' - The number of periods, in five-minute increments, during which the alarm
-- condition must exist before the alarm triggers automatic scaling
-- activity. The default value is @1@.
--
-- 'comparisonOperator', 'cloudWatchAlarmDefinition_comparisonOperator' - Determines how the metric specified by @MetricName@ is compared to the
-- value specified by @Threshold@.
--
-- 'metricName', 'cloudWatchAlarmDefinition_metricName' - The name of the CloudWatch metric that is watched to determine an alarm
-- condition.
--
-- 'period', 'cloudWatchAlarmDefinition_period' - The period, in seconds, over which the statistic is applied. EMR
-- CloudWatch metrics are emitted every five minutes (300 seconds), so if
-- an EMR CloudWatch metric is specified, specify @300@.
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
      { unit = Prelude.Nothing,
        statistic = Prelude.Nothing,
        dimensions = Prelude.Nothing,
        namespace = Prelude.Nothing,
        evaluationPeriods = Prelude.Nothing,
        comparisonOperator = pComparisonOperator_,
        metricName = pMetricName_,
        period = pPeriod_,
        threshold = pThreshold_
      }

-- | The unit of measure associated with the CloudWatch metric being watched.
-- The value specified for @Unit@ must correspond to the units specified in
-- the CloudWatch metric.
cloudWatchAlarmDefinition_unit :: Lens.Lens' CloudWatchAlarmDefinition (Prelude.Maybe Unit)
cloudWatchAlarmDefinition_unit = Lens.lens (\CloudWatchAlarmDefinition' {unit} -> unit) (\s@CloudWatchAlarmDefinition' {} a -> s {unit = a} :: CloudWatchAlarmDefinition)

-- | The statistic to apply to the metric associated with the alarm. The
-- default is @AVERAGE@.
cloudWatchAlarmDefinition_statistic :: Lens.Lens' CloudWatchAlarmDefinition (Prelude.Maybe Statistic)
cloudWatchAlarmDefinition_statistic = Lens.lens (\CloudWatchAlarmDefinition' {statistic} -> statistic) (\s@CloudWatchAlarmDefinition' {} a -> s {statistic = a} :: CloudWatchAlarmDefinition)

-- | A CloudWatch metric dimension.
cloudWatchAlarmDefinition_dimensions :: Lens.Lens' CloudWatchAlarmDefinition (Prelude.Maybe [MetricDimension])
cloudWatchAlarmDefinition_dimensions = Lens.lens (\CloudWatchAlarmDefinition' {dimensions} -> dimensions) (\s@CloudWatchAlarmDefinition' {} a -> s {dimensions = a} :: CloudWatchAlarmDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | The namespace for the CloudWatch metric. The default is
-- @AWS\/ElasticMapReduce@.
cloudWatchAlarmDefinition_namespace :: Lens.Lens' CloudWatchAlarmDefinition (Prelude.Maybe Prelude.Text)
cloudWatchAlarmDefinition_namespace = Lens.lens (\CloudWatchAlarmDefinition' {namespace} -> namespace) (\s@CloudWatchAlarmDefinition' {} a -> s {namespace = a} :: CloudWatchAlarmDefinition)

-- | The number of periods, in five-minute increments, during which the alarm
-- condition must exist before the alarm triggers automatic scaling
-- activity. The default value is @1@.
cloudWatchAlarmDefinition_evaluationPeriods :: Lens.Lens' CloudWatchAlarmDefinition (Prelude.Maybe Prelude.Int)
cloudWatchAlarmDefinition_evaluationPeriods = Lens.lens (\CloudWatchAlarmDefinition' {evaluationPeriods} -> evaluationPeriods) (\s@CloudWatchAlarmDefinition' {} a -> s {evaluationPeriods = a} :: CloudWatchAlarmDefinition)

-- | Determines how the metric specified by @MetricName@ is compared to the
-- value specified by @Threshold@.
cloudWatchAlarmDefinition_comparisonOperator :: Lens.Lens' CloudWatchAlarmDefinition ComparisonOperator
cloudWatchAlarmDefinition_comparisonOperator = Lens.lens (\CloudWatchAlarmDefinition' {comparisonOperator} -> comparisonOperator) (\s@CloudWatchAlarmDefinition' {} a -> s {comparisonOperator = a} :: CloudWatchAlarmDefinition)

-- | The name of the CloudWatch metric that is watched to determine an alarm
-- condition.
cloudWatchAlarmDefinition_metricName :: Lens.Lens' CloudWatchAlarmDefinition Prelude.Text
cloudWatchAlarmDefinition_metricName = Lens.lens (\CloudWatchAlarmDefinition' {metricName} -> metricName) (\s@CloudWatchAlarmDefinition' {} a -> s {metricName = a} :: CloudWatchAlarmDefinition)

-- | The period, in seconds, over which the statistic is applied. EMR
-- CloudWatch metrics are emitted every five minutes (300 seconds), so if
-- an EMR CloudWatch metric is specified, specify @300@.
cloudWatchAlarmDefinition_period :: Lens.Lens' CloudWatchAlarmDefinition Prelude.Int
cloudWatchAlarmDefinition_period = Lens.lens (\CloudWatchAlarmDefinition' {period} -> period) (\s@CloudWatchAlarmDefinition' {} a -> s {period = a} :: CloudWatchAlarmDefinition)

-- | The value against which the specified statistic is compared.
cloudWatchAlarmDefinition_threshold :: Lens.Lens' CloudWatchAlarmDefinition Prelude.Double
cloudWatchAlarmDefinition_threshold = Lens.lens (\CloudWatchAlarmDefinition' {threshold} -> threshold) (\s@CloudWatchAlarmDefinition' {} a -> s {threshold = a} :: CloudWatchAlarmDefinition)

instance Prelude.FromJSON CloudWatchAlarmDefinition where
  parseJSON =
    Prelude.withObject
      "CloudWatchAlarmDefinition"
      ( \x ->
          CloudWatchAlarmDefinition'
            Prelude.<$> (x Prelude..:? "Unit")
            Prelude.<*> (x Prelude..:? "Statistic")
            Prelude.<*> ( x Prelude..:? "Dimensions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Namespace")
            Prelude.<*> (x Prelude..:? "EvaluationPeriods")
            Prelude.<*> (x Prelude..: "ComparisonOperator")
            Prelude.<*> (x Prelude..: "MetricName")
            Prelude.<*> (x Prelude..: "Period")
            Prelude.<*> (x Prelude..: "Threshold")
      )

instance Prelude.Hashable CloudWatchAlarmDefinition

instance Prelude.NFData CloudWatchAlarmDefinition

instance Prelude.ToJSON CloudWatchAlarmDefinition where
  toJSON CloudWatchAlarmDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Unit" Prelude..=) Prelude.<$> unit,
            ("Statistic" Prelude..=) Prelude.<$> statistic,
            ("Dimensions" Prelude..=) Prelude.<$> dimensions,
            ("Namespace" Prelude..=) Prelude.<$> namespace,
            ("EvaluationPeriods" Prelude..=)
              Prelude.<$> evaluationPeriods,
            Prelude.Just
              ("ComparisonOperator" Prelude..= comparisonOperator),
            Prelude.Just ("MetricName" Prelude..= metricName),
            Prelude.Just ("Period" Prelude..= period),
            Prelude.Just ("Threshold" Prelude..= threshold)
          ]
      )

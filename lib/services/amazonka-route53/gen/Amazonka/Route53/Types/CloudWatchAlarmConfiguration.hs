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
-- Module      : Amazonka.Route53.Types.CloudWatchAlarmConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.CloudWatchAlarmConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.ComparisonOperator
import Amazonka.Route53.Types.Dimension
import Amazonka.Route53.Types.Statistic

-- | A complex type that contains information about the CloudWatch alarm that
-- Amazon Route 53 is monitoring for this health check.
--
-- /See:/ 'newCloudWatchAlarmConfiguration' smart constructor.
data CloudWatchAlarmConfiguration = CloudWatchAlarmConfiguration'
  { -- | For the metric that the CloudWatch alarm is associated with, a complex
    -- type that contains information about the dimensions for the metric. For
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference>
    -- in the /Amazon CloudWatch User Guide/.
    dimensions :: Prelude.Maybe [Dimension],
    -- | For the metric that the CloudWatch alarm is associated with, the number
    -- of periods that the metric is compared to the threshold.
    evaluationPeriods :: Prelude.Natural,
    -- | For the metric that the CloudWatch alarm is associated with, the value
    -- the metric is compared with.
    threshold :: Prelude.Double,
    -- | For the metric that the CloudWatch alarm is associated with, the
    -- arithmetic operation that is used for the comparison.
    comparisonOperator :: ComparisonOperator,
    -- | For the metric that the CloudWatch alarm is associated with, the
    -- duration of one evaluation period in seconds.
    period :: Prelude.Natural,
    -- | The name of the CloudWatch metric that the alarm is associated with.
    metricName :: Prelude.Text,
    -- | The namespace of the metric that the alarm is associated with. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference>
    -- in the /Amazon CloudWatch User Guide/.
    namespace :: Prelude.Text,
    -- | For the metric that the CloudWatch alarm is associated with, the
    -- statistic that is applied to the metric.
    statistic :: Statistic
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchAlarmConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'cloudWatchAlarmConfiguration_dimensions' - For the metric that the CloudWatch alarm is associated with, a complex
-- type that contains information about the dimensions for the metric. For
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference>
-- in the /Amazon CloudWatch User Guide/.
--
-- 'evaluationPeriods', 'cloudWatchAlarmConfiguration_evaluationPeriods' - For the metric that the CloudWatch alarm is associated with, the number
-- of periods that the metric is compared to the threshold.
--
-- 'threshold', 'cloudWatchAlarmConfiguration_threshold' - For the metric that the CloudWatch alarm is associated with, the value
-- the metric is compared with.
--
-- 'comparisonOperator', 'cloudWatchAlarmConfiguration_comparisonOperator' - For the metric that the CloudWatch alarm is associated with, the
-- arithmetic operation that is used for the comparison.
--
-- 'period', 'cloudWatchAlarmConfiguration_period' - For the metric that the CloudWatch alarm is associated with, the
-- duration of one evaluation period in seconds.
--
-- 'metricName', 'cloudWatchAlarmConfiguration_metricName' - The name of the CloudWatch metric that the alarm is associated with.
--
-- 'namespace', 'cloudWatchAlarmConfiguration_namespace' - The namespace of the metric that the alarm is associated with. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference>
-- in the /Amazon CloudWatch User Guide/.
--
-- 'statistic', 'cloudWatchAlarmConfiguration_statistic' - For the metric that the CloudWatch alarm is associated with, the
-- statistic that is applied to the metric.
newCloudWatchAlarmConfiguration ::
  -- | 'evaluationPeriods'
  Prelude.Natural ->
  -- | 'threshold'
  Prelude.Double ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'period'
  Prelude.Natural ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  -- | 'statistic'
  Statistic ->
  CloudWatchAlarmConfiguration
newCloudWatchAlarmConfiguration
  pEvaluationPeriods_
  pThreshold_
  pComparisonOperator_
  pPeriod_
  pMetricName_
  pNamespace_
  pStatistic_ =
    CloudWatchAlarmConfiguration'
      { dimensions =
          Prelude.Nothing,
        evaluationPeriods = pEvaluationPeriods_,
        threshold = pThreshold_,
        comparisonOperator = pComparisonOperator_,
        period = pPeriod_,
        metricName = pMetricName_,
        namespace = pNamespace_,
        statistic = pStatistic_
      }

-- | For the metric that the CloudWatch alarm is associated with, a complex
-- type that contains information about the dimensions for the metric. For
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference>
-- in the /Amazon CloudWatch User Guide/.
cloudWatchAlarmConfiguration_dimensions :: Lens.Lens' CloudWatchAlarmConfiguration (Prelude.Maybe [Dimension])
cloudWatchAlarmConfiguration_dimensions = Lens.lens (\CloudWatchAlarmConfiguration' {dimensions} -> dimensions) (\s@CloudWatchAlarmConfiguration' {} a -> s {dimensions = a} :: CloudWatchAlarmConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | For the metric that the CloudWatch alarm is associated with, the number
-- of periods that the metric is compared to the threshold.
cloudWatchAlarmConfiguration_evaluationPeriods :: Lens.Lens' CloudWatchAlarmConfiguration Prelude.Natural
cloudWatchAlarmConfiguration_evaluationPeriods = Lens.lens (\CloudWatchAlarmConfiguration' {evaluationPeriods} -> evaluationPeriods) (\s@CloudWatchAlarmConfiguration' {} a -> s {evaluationPeriods = a} :: CloudWatchAlarmConfiguration)

-- | For the metric that the CloudWatch alarm is associated with, the value
-- the metric is compared with.
cloudWatchAlarmConfiguration_threshold :: Lens.Lens' CloudWatchAlarmConfiguration Prelude.Double
cloudWatchAlarmConfiguration_threshold = Lens.lens (\CloudWatchAlarmConfiguration' {threshold} -> threshold) (\s@CloudWatchAlarmConfiguration' {} a -> s {threshold = a} :: CloudWatchAlarmConfiguration)

-- | For the metric that the CloudWatch alarm is associated with, the
-- arithmetic operation that is used for the comparison.
cloudWatchAlarmConfiguration_comparisonOperator :: Lens.Lens' CloudWatchAlarmConfiguration ComparisonOperator
cloudWatchAlarmConfiguration_comparisonOperator = Lens.lens (\CloudWatchAlarmConfiguration' {comparisonOperator} -> comparisonOperator) (\s@CloudWatchAlarmConfiguration' {} a -> s {comparisonOperator = a} :: CloudWatchAlarmConfiguration)

-- | For the metric that the CloudWatch alarm is associated with, the
-- duration of one evaluation period in seconds.
cloudWatchAlarmConfiguration_period :: Lens.Lens' CloudWatchAlarmConfiguration Prelude.Natural
cloudWatchAlarmConfiguration_period = Lens.lens (\CloudWatchAlarmConfiguration' {period} -> period) (\s@CloudWatchAlarmConfiguration' {} a -> s {period = a} :: CloudWatchAlarmConfiguration)

-- | The name of the CloudWatch metric that the alarm is associated with.
cloudWatchAlarmConfiguration_metricName :: Lens.Lens' CloudWatchAlarmConfiguration Prelude.Text
cloudWatchAlarmConfiguration_metricName = Lens.lens (\CloudWatchAlarmConfiguration' {metricName} -> metricName) (\s@CloudWatchAlarmConfiguration' {} a -> s {metricName = a} :: CloudWatchAlarmConfiguration)

-- | The namespace of the metric that the alarm is associated with. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference>
-- in the /Amazon CloudWatch User Guide/.
cloudWatchAlarmConfiguration_namespace :: Lens.Lens' CloudWatchAlarmConfiguration Prelude.Text
cloudWatchAlarmConfiguration_namespace = Lens.lens (\CloudWatchAlarmConfiguration' {namespace} -> namespace) (\s@CloudWatchAlarmConfiguration' {} a -> s {namespace = a} :: CloudWatchAlarmConfiguration)

-- | For the metric that the CloudWatch alarm is associated with, the
-- statistic that is applied to the metric.
cloudWatchAlarmConfiguration_statistic :: Lens.Lens' CloudWatchAlarmConfiguration Statistic
cloudWatchAlarmConfiguration_statistic = Lens.lens (\CloudWatchAlarmConfiguration' {statistic} -> statistic) (\s@CloudWatchAlarmConfiguration' {} a -> s {statistic = a} :: CloudWatchAlarmConfiguration)

instance Data.FromXML CloudWatchAlarmConfiguration where
  parseXML x =
    CloudWatchAlarmConfiguration'
      Prelude.<$> ( x Data..@? "Dimensions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Dimension")
                  )
      Prelude.<*> (x Data..@ "EvaluationPeriods")
      Prelude.<*> (x Data..@ "Threshold")
      Prelude.<*> (x Data..@ "ComparisonOperator")
      Prelude.<*> (x Data..@ "Period")
      Prelude.<*> (x Data..@ "MetricName")
      Prelude.<*> (x Data..@ "Namespace")
      Prelude.<*> (x Data..@ "Statistic")

instance
  Prelude.Hashable
    CloudWatchAlarmConfiguration
  where
  hashWithSalt _salt CloudWatchAlarmConfiguration' {..} =
    _salt `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` evaluationPeriods
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` statistic

instance Prelude.NFData CloudWatchAlarmConfiguration where
  rnf CloudWatchAlarmConfiguration' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf evaluationPeriods
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf statistic

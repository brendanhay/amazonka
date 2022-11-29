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
-- Module      : Amazonka.AutoScaling.Types.PredictiveScalingMetricSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredictiveScalingMetricSpecification where

import Amazonka.AutoScaling.Types.PredictiveScalingCustomizedCapacityMetric
import Amazonka.AutoScaling.Types.PredictiveScalingCustomizedLoadMetric
import Amazonka.AutoScaling.Types.PredictiveScalingCustomizedScalingMetric
import Amazonka.AutoScaling.Types.PredictiveScalingPredefinedLoadMetric
import Amazonka.AutoScaling.Types.PredictiveScalingPredefinedMetricPair
import Amazonka.AutoScaling.Types.PredictiveScalingPredefinedScalingMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | This structure specifies the metrics and target utilization settings for
-- a predictive scaling policy.
--
-- You must specify either a metric pair, or a load metric and a scaling
-- metric individually. Specifying a metric pair instead of individual
-- metrics provides a simpler way to configure metrics for a scaling
-- policy. You choose the metric pair, and the policy automatically knows
-- the correct sum and average statistics to use for the load metric and
-- the scaling metric.
--
-- Example
--
-- -   You create a predictive scaling policy and specify @ALBRequestCount@
--     as the value for the metric pair and @1000.0@ as the target value.
--     For this type of metric, you must provide the metric dimension for
--     the corresponding target group, so you also provide a resource label
--     for the Application Load Balancer target group that is attached to
--     your Auto Scaling group.
--
-- -   The number of requests the target group receives per minute provides
--     the load metric, and the request count averaged between the members
--     of the target group provides the scaling metric. In CloudWatch, this
--     refers to the @RequestCount@ and @RequestCountPerTarget@ metrics,
--     respectively.
--
-- -   For optimal use of predictive scaling, you adhere to the best
--     practice of using a dynamic scaling policy to automatically scale
--     between the minimum capacity and maximum capacity in response to
--     real-time changes in resource utilization.
--
-- -   Amazon EC2 Auto Scaling consumes data points for the load metric
--     over the last 14 days and creates an hourly load forecast for
--     predictive scaling. (A minimum of 24 hours of data is required.)
--
-- -   After creating the load forecast, Amazon EC2 Auto Scaling determines
--     when to reduce or increase the capacity of your Auto Scaling group
--     in each hour of the forecast period so that the average number of
--     requests received by each instance is as close to 1000 requests per
--     minute as possible at all times.
--
-- For information about using custom metrics with predictive scaling, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/predictive-scaling-customized-metric-specification.html Advanced predictive scaling policy configurations using custom metrics>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newPredictiveScalingMetricSpecification' smart constructor.
data PredictiveScalingMetricSpecification = PredictiveScalingMetricSpecification'
  { -- | The customized scaling metric specification.
    customizedScalingMetricSpecification :: Prelude.Maybe PredictiveScalingCustomizedScalingMetric,
    -- | The predefined load metric specification.
    predefinedLoadMetricSpecification :: Prelude.Maybe PredictiveScalingPredefinedLoadMetric,
    -- | The customized load metric specification.
    customizedLoadMetricSpecification :: Prelude.Maybe PredictiveScalingCustomizedLoadMetric,
    -- | The customized capacity metric specification.
    customizedCapacityMetricSpecification :: Prelude.Maybe PredictiveScalingCustomizedCapacityMetric,
    -- | The predefined scaling metric specification.
    predefinedScalingMetricSpecification :: Prelude.Maybe PredictiveScalingPredefinedScalingMetric,
    -- | The predefined metric pair specification from which Amazon EC2 Auto
    -- Scaling determines the appropriate scaling metric and load metric to
    -- use.
    predefinedMetricPairSpecification :: Prelude.Maybe PredictiveScalingPredefinedMetricPair,
    -- | Specifies the target utilization.
    --
    -- Some metrics are based on a count instead of a percentage, such as the
    -- request count for an Application Load Balancer or the number of messages
    -- in an SQS queue. If the scaling policy specifies one of these metrics,
    -- specify the target utilization as the optimal average request or message
    -- count per instance during any one-minute interval.
    targetValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictiveScalingMetricSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customizedScalingMetricSpecification', 'predictiveScalingMetricSpecification_customizedScalingMetricSpecification' - The customized scaling metric specification.
--
-- 'predefinedLoadMetricSpecification', 'predictiveScalingMetricSpecification_predefinedLoadMetricSpecification' - The predefined load metric specification.
--
-- 'customizedLoadMetricSpecification', 'predictiveScalingMetricSpecification_customizedLoadMetricSpecification' - The customized load metric specification.
--
-- 'customizedCapacityMetricSpecification', 'predictiveScalingMetricSpecification_customizedCapacityMetricSpecification' - The customized capacity metric specification.
--
-- 'predefinedScalingMetricSpecification', 'predictiveScalingMetricSpecification_predefinedScalingMetricSpecification' - The predefined scaling metric specification.
--
-- 'predefinedMetricPairSpecification', 'predictiveScalingMetricSpecification_predefinedMetricPairSpecification' - The predefined metric pair specification from which Amazon EC2 Auto
-- Scaling determines the appropriate scaling metric and load metric to
-- use.
--
-- 'targetValue', 'predictiveScalingMetricSpecification_targetValue' - Specifies the target utilization.
--
-- Some metrics are based on a count instead of a percentage, such as the
-- request count for an Application Load Balancer or the number of messages
-- in an SQS queue. If the scaling policy specifies one of these metrics,
-- specify the target utilization as the optimal average request or message
-- count per instance during any one-minute interval.
newPredictiveScalingMetricSpecification ::
  -- | 'targetValue'
  Prelude.Double ->
  PredictiveScalingMetricSpecification
newPredictiveScalingMetricSpecification pTargetValue_ =
  PredictiveScalingMetricSpecification'
    { customizedScalingMetricSpecification =
        Prelude.Nothing,
      predefinedLoadMetricSpecification =
        Prelude.Nothing,
      customizedLoadMetricSpecification =
        Prelude.Nothing,
      customizedCapacityMetricSpecification =
        Prelude.Nothing,
      predefinedScalingMetricSpecification =
        Prelude.Nothing,
      predefinedMetricPairSpecification =
        Prelude.Nothing,
      targetValue = pTargetValue_
    }

-- | The customized scaling metric specification.
predictiveScalingMetricSpecification_customizedScalingMetricSpecification :: Lens.Lens' PredictiveScalingMetricSpecification (Prelude.Maybe PredictiveScalingCustomizedScalingMetric)
predictiveScalingMetricSpecification_customizedScalingMetricSpecification = Lens.lens (\PredictiveScalingMetricSpecification' {customizedScalingMetricSpecification} -> customizedScalingMetricSpecification) (\s@PredictiveScalingMetricSpecification' {} a -> s {customizedScalingMetricSpecification = a} :: PredictiveScalingMetricSpecification)

-- | The predefined load metric specification.
predictiveScalingMetricSpecification_predefinedLoadMetricSpecification :: Lens.Lens' PredictiveScalingMetricSpecification (Prelude.Maybe PredictiveScalingPredefinedLoadMetric)
predictiveScalingMetricSpecification_predefinedLoadMetricSpecification = Lens.lens (\PredictiveScalingMetricSpecification' {predefinedLoadMetricSpecification} -> predefinedLoadMetricSpecification) (\s@PredictiveScalingMetricSpecification' {} a -> s {predefinedLoadMetricSpecification = a} :: PredictiveScalingMetricSpecification)

-- | The customized load metric specification.
predictiveScalingMetricSpecification_customizedLoadMetricSpecification :: Lens.Lens' PredictiveScalingMetricSpecification (Prelude.Maybe PredictiveScalingCustomizedLoadMetric)
predictiveScalingMetricSpecification_customizedLoadMetricSpecification = Lens.lens (\PredictiveScalingMetricSpecification' {customizedLoadMetricSpecification} -> customizedLoadMetricSpecification) (\s@PredictiveScalingMetricSpecification' {} a -> s {customizedLoadMetricSpecification = a} :: PredictiveScalingMetricSpecification)

-- | The customized capacity metric specification.
predictiveScalingMetricSpecification_customizedCapacityMetricSpecification :: Lens.Lens' PredictiveScalingMetricSpecification (Prelude.Maybe PredictiveScalingCustomizedCapacityMetric)
predictiveScalingMetricSpecification_customizedCapacityMetricSpecification = Lens.lens (\PredictiveScalingMetricSpecification' {customizedCapacityMetricSpecification} -> customizedCapacityMetricSpecification) (\s@PredictiveScalingMetricSpecification' {} a -> s {customizedCapacityMetricSpecification = a} :: PredictiveScalingMetricSpecification)

-- | The predefined scaling metric specification.
predictiveScalingMetricSpecification_predefinedScalingMetricSpecification :: Lens.Lens' PredictiveScalingMetricSpecification (Prelude.Maybe PredictiveScalingPredefinedScalingMetric)
predictiveScalingMetricSpecification_predefinedScalingMetricSpecification = Lens.lens (\PredictiveScalingMetricSpecification' {predefinedScalingMetricSpecification} -> predefinedScalingMetricSpecification) (\s@PredictiveScalingMetricSpecification' {} a -> s {predefinedScalingMetricSpecification = a} :: PredictiveScalingMetricSpecification)

-- | The predefined metric pair specification from which Amazon EC2 Auto
-- Scaling determines the appropriate scaling metric and load metric to
-- use.
predictiveScalingMetricSpecification_predefinedMetricPairSpecification :: Lens.Lens' PredictiveScalingMetricSpecification (Prelude.Maybe PredictiveScalingPredefinedMetricPair)
predictiveScalingMetricSpecification_predefinedMetricPairSpecification = Lens.lens (\PredictiveScalingMetricSpecification' {predefinedMetricPairSpecification} -> predefinedMetricPairSpecification) (\s@PredictiveScalingMetricSpecification' {} a -> s {predefinedMetricPairSpecification = a} :: PredictiveScalingMetricSpecification)

-- | Specifies the target utilization.
--
-- Some metrics are based on a count instead of a percentage, such as the
-- request count for an Application Load Balancer or the number of messages
-- in an SQS queue. If the scaling policy specifies one of these metrics,
-- specify the target utilization as the optimal average request or message
-- count per instance during any one-minute interval.
predictiveScalingMetricSpecification_targetValue :: Lens.Lens' PredictiveScalingMetricSpecification Prelude.Double
predictiveScalingMetricSpecification_targetValue = Lens.lens (\PredictiveScalingMetricSpecification' {targetValue} -> targetValue) (\s@PredictiveScalingMetricSpecification' {} a -> s {targetValue = a} :: PredictiveScalingMetricSpecification)

instance
  Core.FromXML
    PredictiveScalingMetricSpecification
  where
  parseXML x =
    PredictiveScalingMetricSpecification'
      Prelude.<$> (x Core..@? "CustomizedScalingMetricSpecification")
      Prelude.<*> (x Core..@? "PredefinedLoadMetricSpecification")
      Prelude.<*> (x Core..@? "CustomizedLoadMetricSpecification")
      Prelude.<*> (x Core..@? "CustomizedCapacityMetricSpecification")
      Prelude.<*> (x Core..@? "PredefinedScalingMetricSpecification")
      Prelude.<*> (x Core..@? "PredefinedMetricPairSpecification")
      Prelude.<*> (x Core..@ "TargetValue")

instance
  Prelude.Hashable
    PredictiveScalingMetricSpecification
  where
  hashWithSalt
    _salt
    PredictiveScalingMetricSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` customizedScalingMetricSpecification
        `Prelude.hashWithSalt` predefinedLoadMetricSpecification
        `Prelude.hashWithSalt` customizedLoadMetricSpecification
        `Prelude.hashWithSalt` customizedCapacityMetricSpecification
        `Prelude.hashWithSalt` predefinedScalingMetricSpecification
        `Prelude.hashWithSalt` predefinedMetricPairSpecification
        `Prelude.hashWithSalt` targetValue

instance
  Prelude.NFData
    PredictiveScalingMetricSpecification
  where
  rnf PredictiveScalingMetricSpecification' {..} =
    Prelude.rnf customizedScalingMetricSpecification
      `Prelude.seq` Prelude.rnf predefinedLoadMetricSpecification
      `Prelude.seq` Prelude.rnf customizedLoadMetricSpecification
      `Prelude.seq` Prelude.rnf customizedCapacityMetricSpecification
      `Prelude.seq` Prelude.rnf predefinedScalingMetricSpecification
      `Prelude.seq` Prelude.rnf predefinedMetricPairSpecification
      `Prelude.seq` Prelude.rnf targetValue

instance
  Core.ToQuery
    PredictiveScalingMetricSpecification
  where
  toQuery PredictiveScalingMetricSpecification' {..} =
    Prelude.mconcat
      [ "CustomizedScalingMetricSpecification"
          Core.=: customizedScalingMetricSpecification,
        "PredefinedLoadMetricSpecification"
          Core.=: predefinedLoadMetricSpecification,
        "CustomizedLoadMetricSpecification"
          Core.=: customizedLoadMetricSpecification,
        "CustomizedCapacityMetricSpecification"
          Core.=: customizedCapacityMetricSpecification,
        "PredefinedScalingMetricSpecification"
          Core.=: predefinedScalingMetricSpecification,
        "PredefinedMetricPairSpecification"
          Core.=: predefinedMetricPairSpecification,
        "TargetValue" Core.=: targetValue
      ]

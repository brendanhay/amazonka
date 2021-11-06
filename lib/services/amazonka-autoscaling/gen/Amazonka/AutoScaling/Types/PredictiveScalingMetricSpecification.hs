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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredictiveScalingMetricSpecification where

import Amazonka.AutoScaling.Types.PredictiveScalingPredefinedLoadMetric
import Amazonka.AutoScaling.Types.PredictiveScalingPredefinedMetricPair
import Amazonka.AutoScaling.Types.PredictiveScalingPredefinedScalingMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
-- /See:/ 'newPredictiveScalingMetricSpecification' smart constructor.
data PredictiveScalingMetricSpecification = PredictiveScalingMetricSpecification'
  { -- | The scaling metric specification.
    predefinedScalingMetricSpecification :: Prelude.Maybe PredictiveScalingPredefinedScalingMetric,
    -- | The metric pair specification from which Amazon EC2 Auto Scaling
    -- determines the appropriate scaling metric and load metric to use.
    predefinedMetricPairSpecification :: Prelude.Maybe PredictiveScalingPredefinedMetricPair,
    -- | The load metric specification.
    predefinedLoadMetricSpecification :: Prelude.Maybe PredictiveScalingPredefinedLoadMetric,
    -- | Specifies the target utilization.
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
-- 'predefinedScalingMetricSpecification', 'predictiveScalingMetricSpecification_predefinedScalingMetricSpecification' - The scaling metric specification.
--
-- 'predefinedMetricPairSpecification', 'predictiveScalingMetricSpecification_predefinedMetricPairSpecification' - The metric pair specification from which Amazon EC2 Auto Scaling
-- determines the appropriate scaling metric and load metric to use.
--
-- 'predefinedLoadMetricSpecification', 'predictiveScalingMetricSpecification_predefinedLoadMetricSpecification' - The load metric specification.
--
-- 'targetValue', 'predictiveScalingMetricSpecification_targetValue' - Specifies the target utilization.
newPredictiveScalingMetricSpecification ::
  -- | 'targetValue'
  Prelude.Double ->
  PredictiveScalingMetricSpecification
newPredictiveScalingMetricSpecification pTargetValue_ =
  PredictiveScalingMetricSpecification'
    { predefinedScalingMetricSpecification =
        Prelude.Nothing,
      predefinedMetricPairSpecification =
        Prelude.Nothing,
      predefinedLoadMetricSpecification =
        Prelude.Nothing,
      targetValue = pTargetValue_
    }

-- | The scaling metric specification.
predictiveScalingMetricSpecification_predefinedScalingMetricSpecification :: Lens.Lens' PredictiveScalingMetricSpecification (Prelude.Maybe PredictiveScalingPredefinedScalingMetric)
predictiveScalingMetricSpecification_predefinedScalingMetricSpecification = Lens.lens (\PredictiveScalingMetricSpecification' {predefinedScalingMetricSpecification} -> predefinedScalingMetricSpecification) (\s@PredictiveScalingMetricSpecification' {} a -> s {predefinedScalingMetricSpecification = a} :: PredictiveScalingMetricSpecification)

-- | The metric pair specification from which Amazon EC2 Auto Scaling
-- determines the appropriate scaling metric and load metric to use.
predictiveScalingMetricSpecification_predefinedMetricPairSpecification :: Lens.Lens' PredictiveScalingMetricSpecification (Prelude.Maybe PredictiveScalingPredefinedMetricPair)
predictiveScalingMetricSpecification_predefinedMetricPairSpecification = Lens.lens (\PredictiveScalingMetricSpecification' {predefinedMetricPairSpecification} -> predefinedMetricPairSpecification) (\s@PredictiveScalingMetricSpecification' {} a -> s {predefinedMetricPairSpecification = a} :: PredictiveScalingMetricSpecification)

-- | The load metric specification.
predictiveScalingMetricSpecification_predefinedLoadMetricSpecification :: Lens.Lens' PredictiveScalingMetricSpecification (Prelude.Maybe PredictiveScalingPredefinedLoadMetric)
predictiveScalingMetricSpecification_predefinedLoadMetricSpecification = Lens.lens (\PredictiveScalingMetricSpecification' {predefinedLoadMetricSpecification} -> predefinedLoadMetricSpecification) (\s@PredictiveScalingMetricSpecification' {} a -> s {predefinedLoadMetricSpecification = a} :: PredictiveScalingMetricSpecification)

-- | Specifies the target utilization.
predictiveScalingMetricSpecification_targetValue :: Lens.Lens' PredictiveScalingMetricSpecification Prelude.Double
predictiveScalingMetricSpecification_targetValue = Lens.lens (\PredictiveScalingMetricSpecification' {targetValue} -> targetValue) (\s@PredictiveScalingMetricSpecification' {} a -> s {targetValue = a} :: PredictiveScalingMetricSpecification)

instance
  Core.FromXML
    PredictiveScalingMetricSpecification
  where
  parseXML x =
    PredictiveScalingMetricSpecification'
      Prelude.<$> (x Core..@? "PredefinedScalingMetricSpecification")
      Prelude.<*> (x Core..@? "PredefinedMetricPairSpecification")
      Prelude.<*> (x Core..@? "PredefinedLoadMetricSpecification")
      Prelude.<*> (x Core..@ "TargetValue")

instance
  Prelude.Hashable
    PredictiveScalingMetricSpecification

instance
  Prelude.NFData
    PredictiveScalingMetricSpecification

instance
  Core.ToQuery
    PredictiveScalingMetricSpecification
  where
  toQuery PredictiveScalingMetricSpecification' {..} =
    Prelude.mconcat
      [ "PredefinedScalingMetricSpecification"
          Core.=: predefinedScalingMetricSpecification,
        "PredefinedMetricPairSpecification"
          Core.=: predefinedMetricPairSpecification,
        "PredefinedLoadMetricSpecification"
          Core.=: predefinedLoadMetricSpecification,
        "TargetValue" Core.=: targetValue
      ]

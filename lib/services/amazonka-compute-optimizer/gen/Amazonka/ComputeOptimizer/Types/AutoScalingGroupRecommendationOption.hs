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
-- Module      : Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendationOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendationOption where

import Amazonka.ComputeOptimizer.Types.AutoScalingGroupConfiguration
import Amazonka.ComputeOptimizer.Types.UtilizationMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a recommendation option for an Auto Scaling group.
--
-- /See:/ 'newAutoScalingGroupRecommendationOption' smart constructor.
data AutoScalingGroupRecommendationOption = AutoScalingGroupRecommendationOption'
  { -- | The performance risk of the Auto Scaling group configuration
    -- recommendation.
    --
    -- Performance risk indicates the likelihood of the recommended instance
    -- type not meeting the resource needs of your workload. Compute Optimizer
    -- calculates an individual performance risk score for each specification
    -- of the recommended instance, including CPU, memory, EBS throughput, EBS
    -- IOPS, disk throughput, disk IOPS, network throughput, and network PPS.
    -- The performance risk of the recommended instance is calculated as the
    -- maximum performance risk score across the analyzed resource
    -- specifications.
    --
    -- The value ranges from @0@ - @4@, with @0@ meaning that the recommended
    -- resource is predicted to always provide enough hardware capability. The
    -- higher the performance risk is, the more likely you should validate
    -- whether the recommendation will meet the performance requirements of
    -- your workload before migrating your resource.
    performanceRisk :: Prelude.Maybe Prelude.Double,
    -- | An array of objects that describe the projected utilization metrics of
    -- the Auto Scaling group recommendation option.
    --
    -- The @Cpu@ and @Memory@ metrics are the only projected utilization
    -- metrics returned. Additionally, the @Memory@ metric is returned only for
    -- resources that have the unified CloudWatch agent installed on them. For
    -- more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
    projectedUtilizationMetrics :: Prelude.Maybe [UtilizationMetric],
    -- | An array of objects that describe an Auto Scaling group configuration.
    configuration :: Prelude.Maybe AutoScalingGroupConfiguration,
    -- | The rank of the Auto Scaling group recommendation option.
    --
    -- The top recommendation option is ranked as @1@.
    rank :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingGroupRecommendationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'performanceRisk', 'autoScalingGroupRecommendationOption_performanceRisk' - The performance risk of the Auto Scaling group configuration
-- recommendation.
--
-- Performance risk indicates the likelihood of the recommended instance
-- type not meeting the resource needs of your workload. Compute Optimizer
-- calculates an individual performance risk score for each specification
-- of the recommended instance, including CPU, memory, EBS throughput, EBS
-- IOPS, disk throughput, disk IOPS, network throughput, and network PPS.
-- The performance risk of the recommended instance is calculated as the
-- maximum performance risk score across the analyzed resource
-- specifications.
--
-- The value ranges from @0@ - @4@, with @0@ meaning that the recommended
-- resource is predicted to always provide enough hardware capability. The
-- higher the performance risk is, the more likely you should validate
-- whether the recommendation will meet the performance requirements of
-- your workload before migrating your resource.
--
-- 'projectedUtilizationMetrics', 'autoScalingGroupRecommendationOption_projectedUtilizationMetrics' - An array of objects that describe the projected utilization metrics of
-- the Auto Scaling group recommendation option.
--
-- The @Cpu@ and @Memory@ metrics are the only projected utilization
-- metrics returned. Additionally, the @Memory@ metric is returned only for
-- resources that have the unified CloudWatch agent installed on them. For
-- more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
--
-- 'configuration', 'autoScalingGroupRecommendationOption_configuration' - An array of objects that describe an Auto Scaling group configuration.
--
-- 'rank', 'autoScalingGroupRecommendationOption_rank' - The rank of the Auto Scaling group recommendation option.
--
-- The top recommendation option is ranked as @1@.
newAutoScalingGroupRecommendationOption ::
  AutoScalingGroupRecommendationOption
newAutoScalingGroupRecommendationOption =
  AutoScalingGroupRecommendationOption'
    { performanceRisk =
        Prelude.Nothing,
      projectedUtilizationMetrics =
        Prelude.Nothing,
      configuration = Prelude.Nothing,
      rank = Prelude.Nothing
    }

-- | The performance risk of the Auto Scaling group configuration
-- recommendation.
--
-- Performance risk indicates the likelihood of the recommended instance
-- type not meeting the resource needs of your workload. Compute Optimizer
-- calculates an individual performance risk score for each specification
-- of the recommended instance, including CPU, memory, EBS throughput, EBS
-- IOPS, disk throughput, disk IOPS, network throughput, and network PPS.
-- The performance risk of the recommended instance is calculated as the
-- maximum performance risk score across the analyzed resource
-- specifications.
--
-- The value ranges from @0@ - @4@, with @0@ meaning that the recommended
-- resource is predicted to always provide enough hardware capability. The
-- higher the performance risk is, the more likely you should validate
-- whether the recommendation will meet the performance requirements of
-- your workload before migrating your resource.
autoScalingGroupRecommendationOption_performanceRisk :: Lens.Lens' AutoScalingGroupRecommendationOption (Prelude.Maybe Prelude.Double)
autoScalingGroupRecommendationOption_performanceRisk = Lens.lens (\AutoScalingGroupRecommendationOption' {performanceRisk} -> performanceRisk) (\s@AutoScalingGroupRecommendationOption' {} a -> s {performanceRisk = a} :: AutoScalingGroupRecommendationOption)

-- | An array of objects that describe the projected utilization metrics of
-- the Auto Scaling group recommendation option.
--
-- The @Cpu@ and @Memory@ metrics are the only projected utilization
-- metrics returned. Additionally, the @Memory@ metric is returned only for
-- resources that have the unified CloudWatch agent installed on them. For
-- more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
autoScalingGroupRecommendationOption_projectedUtilizationMetrics :: Lens.Lens' AutoScalingGroupRecommendationOption (Prelude.Maybe [UtilizationMetric])
autoScalingGroupRecommendationOption_projectedUtilizationMetrics = Lens.lens (\AutoScalingGroupRecommendationOption' {projectedUtilizationMetrics} -> projectedUtilizationMetrics) (\s@AutoScalingGroupRecommendationOption' {} a -> s {projectedUtilizationMetrics = a} :: AutoScalingGroupRecommendationOption) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that describe an Auto Scaling group configuration.
autoScalingGroupRecommendationOption_configuration :: Lens.Lens' AutoScalingGroupRecommendationOption (Prelude.Maybe AutoScalingGroupConfiguration)
autoScalingGroupRecommendationOption_configuration = Lens.lens (\AutoScalingGroupRecommendationOption' {configuration} -> configuration) (\s@AutoScalingGroupRecommendationOption' {} a -> s {configuration = a} :: AutoScalingGroupRecommendationOption)

-- | The rank of the Auto Scaling group recommendation option.
--
-- The top recommendation option is ranked as @1@.
autoScalingGroupRecommendationOption_rank :: Lens.Lens' AutoScalingGroupRecommendationOption (Prelude.Maybe Prelude.Int)
autoScalingGroupRecommendationOption_rank = Lens.lens (\AutoScalingGroupRecommendationOption' {rank} -> rank) (\s@AutoScalingGroupRecommendationOption' {} a -> s {rank = a} :: AutoScalingGroupRecommendationOption)

instance
  Core.FromJSON
    AutoScalingGroupRecommendationOption
  where
  parseJSON =
    Core.withObject
      "AutoScalingGroupRecommendationOption"
      ( \x ->
          AutoScalingGroupRecommendationOption'
            Prelude.<$> (x Core..:? "performanceRisk")
            Prelude.<*> ( x Core..:? "projectedUtilizationMetrics"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "configuration")
            Prelude.<*> (x Core..:? "rank")
      )

instance
  Prelude.Hashable
    AutoScalingGroupRecommendationOption

instance
  Prelude.NFData
    AutoScalingGroupRecommendationOption

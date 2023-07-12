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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendationOption where

import Amazonka.ComputeOptimizer.Types.AutoScalingGroupConfiguration
import Amazonka.ComputeOptimizer.Types.MigrationEffort
import Amazonka.ComputeOptimizer.Types.SavingsOpportunity
import Amazonka.ComputeOptimizer.Types.UtilizationMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a recommendation option for an Auto Scaling group.
--
-- /See:/ 'newAutoScalingGroupRecommendationOption' smart constructor.
data AutoScalingGroupRecommendationOption = AutoScalingGroupRecommendationOption'
  { -- | An array of objects that describe an Auto Scaling group configuration.
    configuration :: Prelude.Maybe AutoScalingGroupConfiguration,
    -- | The level of effort required to migrate from the current instance type
    -- to the recommended instance type.
    --
    -- For example, the migration effort is @Low@ if Amazon EMR is the inferred
    -- workload type and an Amazon Web Services Graviton instance type is
    -- recommended. The migration effort is @Medium@ if a workload type
    -- couldn\'t be inferred but an Amazon Web Services Graviton instance type
    -- is recommended. The migration effort is @VeryLow@ if both the current
    -- and recommended instance types are of the same CPU architecture.
    migrationEffort :: Prelude.Maybe MigrationEffort,
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
    -- | The rank of the Auto Scaling group recommendation option.
    --
    -- The top recommendation option is ranked as @1@.
    rank :: Prelude.Maybe Prelude.Int,
    -- | An object that describes the savings opportunity for the Auto Scaling
    -- group recommendation option. Savings opportunity includes the estimated
    -- monthly savings amount and percentage.
    savingsOpportunity :: Prelude.Maybe SavingsOpportunity
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
-- 'configuration', 'autoScalingGroupRecommendationOption_configuration' - An array of objects that describe an Auto Scaling group configuration.
--
-- 'migrationEffort', 'autoScalingGroupRecommendationOption_migrationEffort' - The level of effort required to migrate from the current instance type
-- to the recommended instance type.
--
-- For example, the migration effort is @Low@ if Amazon EMR is the inferred
-- workload type and an Amazon Web Services Graviton instance type is
-- recommended. The migration effort is @Medium@ if a workload type
-- couldn\'t be inferred but an Amazon Web Services Graviton instance type
-- is recommended. The migration effort is @VeryLow@ if both the current
-- and recommended instance types are of the same CPU architecture.
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
-- 'rank', 'autoScalingGroupRecommendationOption_rank' - The rank of the Auto Scaling group recommendation option.
--
-- The top recommendation option is ranked as @1@.
--
-- 'savingsOpportunity', 'autoScalingGroupRecommendationOption_savingsOpportunity' - An object that describes the savings opportunity for the Auto Scaling
-- group recommendation option. Savings opportunity includes the estimated
-- monthly savings amount and percentage.
newAutoScalingGroupRecommendationOption ::
  AutoScalingGroupRecommendationOption
newAutoScalingGroupRecommendationOption =
  AutoScalingGroupRecommendationOption'
    { configuration =
        Prelude.Nothing,
      migrationEffort = Prelude.Nothing,
      performanceRisk = Prelude.Nothing,
      projectedUtilizationMetrics =
        Prelude.Nothing,
      rank = Prelude.Nothing,
      savingsOpportunity = Prelude.Nothing
    }

-- | An array of objects that describe an Auto Scaling group configuration.
autoScalingGroupRecommendationOption_configuration :: Lens.Lens' AutoScalingGroupRecommendationOption (Prelude.Maybe AutoScalingGroupConfiguration)
autoScalingGroupRecommendationOption_configuration = Lens.lens (\AutoScalingGroupRecommendationOption' {configuration} -> configuration) (\s@AutoScalingGroupRecommendationOption' {} a -> s {configuration = a} :: AutoScalingGroupRecommendationOption)

-- | The level of effort required to migrate from the current instance type
-- to the recommended instance type.
--
-- For example, the migration effort is @Low@ if Amazon EMR is the inferred
-- workload type and an Amazon Web Services Graviton instance type is
-- recommended. The migration effort is @Medium@ if a workload type
-- couldn\'t be inferred but an Amazon Web Services Graviton instance type
-- is recommended. The migration effort is @VeryLow@ if both the current
-- and recommended instance types are of the same CPU architecture.
autoScalingGroupRecommendationOption_migrationEffort :: Lens.Lens' AutoScalingGroupRecommendationOption (Prelude.Maybe MigrationEffort)
autoScalingGroupRecommendationOption_migrationEffort = Lens.lens (\AutoScalingGroupRecommendationOption' {migrationEffort} -> migrationEffort) (\s@AutoScalingGroupRecommendationOption' {} a -> s {migrationEffort = a} :: AutoScalingGroupRecommendationOption)

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

-- | The rank of the Auto Scaling group recommendation option.
--
-- The top recommendation option is ranked as @1@.
autoScalingGroupRecommendationOption_rank :: Lens.Lens' AutoScalingGroupRecommendationOption (Prelude.Maybe Prelude.Int)
autoScalingGroupRecommendationOption_rank = Lens.lens (\AutoScalingGroupRecommendationOption' {rank} -> rank) (\s@AutoScalingGroupRecommendationOption' {} a -> s {rank = a} :: AutoScalingGroupRecommendationOption)

-- | An object that describes the savings opportunity for the Auto Scaling
-- group recommendation option. Savings opportunity includes the estimated
-- monthly savings amount and percentage.
autoScalingGroupRecommendationOption_savingsOpportunity :: Lens.Lens' AutoScalingGroupRecommendationOption (Prelude.Maybe SavingsOpportunity)
autoScalingGroupRecommendationOption_savingsOpportunity = Lens.lens (\AutoScalingGroupRecommendationOption' {savingsOpportunity} -> savingsOpportunity) (\s@AutoScalingGroupRecommendationOption' {} a -> s {savingsOpportunity = a} :: AutoScalingGroupRecommendationOption)

instance
  Data.FromJSON
    AutoScalingGroupRecommendationOption
  where
  parseJSON =
    Data.withObject
      "AutoScalingGroupRecommendationOption"
      ( \x ->
          AutoScalingGroupRecommendationOption'
            Prelude.<$> (x Data..:? "configuration")
            Prelude.<*> (x Data..:? "migrationEffort")
            Prelude.<*> (x Data..:? "performanceRisk")
            Prelude.<*> ( x
                            Data..:? "projectedUtilizationMetrics"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "rank")
            Prelude.<*> (x Data..:? "savingsOpportunity")
      )

instance
  Prelude.Hashable
    AutoScalingGroupRecommendationOption
  where
  hashWithSalt
    _salt
    AutoScalingGroupRecommendationOption' {..} =
      _salt
        `Prelude.hashWithSalt` configuration
        `Prelude.hashWithSalt` migrationEffort
        `Prelude.hashWithSalt` performanceRisk
        `Prelude.hashWithSalt` projectedUtilizationMetrics
        `Prelude.hashWithSalt` rank
        `Prelude.hashWithSalt` savingsOpportunity

instance
  Prelude.NFData
    AutoScalingGroupRecommendationOption
  where
  rnf AutoScalingGroupRecommendationOption' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf migrationEffort
      `Prelude.seq` Prelude.rnf performanceRisk
      `Prelude.seq` Prelude.rnf projectedUtilizationMetrics
      `Prelude.seq` Prelude.rnf rank
      `Prelude.seq` Prelude.rnf savingsOpportunity

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
-- Module      : Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendation where

import Amazonka.ComputeOptimizer.Types.AutoScalingGroupConfiguration
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendationOption
import Amazonka.ComputeOptimizer.Types.Finding
import Amazonka.ComputeOptimizer.Types.UtilizationMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an Auto Scaling group recommendation.
--
-- /See:/ 'newAutoScalingGroupRecommendation' smart constructor.
data AutoScalingGroupRecommendation = AutoScalingGroupRecommendation'
  { -- | The finding classification of the Auto Scaling group.
    --
    -- Findings for Auto Scaling groups include:
    --
    -- -   __@NotOptimized@__ —An Auto Scaling group is considered not
    --     optimized when Compute Optimizer identifies a recommendation that
    --     can provide better performance for your workload.
    --
    -- -   __@Optimized@__ —An Auto Scaling group is considered optimized when
    --     Compute Optimizer determines that the group is correctly provisioned
    --     to run your workload based on the chosen instance type. For
    --     optimized resources, Compute Optimizer might recommend a new
    --     generation instance type.
    finding :: Prelude.Maybe Finding,
    -- | The timestamp of when the Auto Scaling group recommendation was last
    -- refreshed.
    lastRefreshTimestamp :: Prelude.Maybe Core.POSIX,
    -- | An array of objects that describe the current configuration of the Auto
    -- Scaling group.
    currentConfiguration :: Prelude.Maybe AutoScalingGroupConfiguration,
    -- | The Amazon Web Services account ID of the Auto Scaling group.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the utilization metrics of the Auto
    -- Scaling group.
    utilizationMetrics :: Prelude.Maybe [UtilizationMetric],
    -- | The Amazon Resource Name (ARN) of the Auto Scaling group.
    autoScalingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the recommendation options for the
    -- Auto Scaling group.
    recommendationOptions :: Prelude.Maybe [AutoScalingGroupRecommendationOption],
    -- | The number of days for which utilization metrics were analyzed for the
    -- Auto Scaling group.
    lookBackPeriodInDays :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingGroupRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finding', 'autoScalingGroupRecommendation_finding' - The finding classification of the Auto Scaling group.
--
-- Findings for Auto Scaling groups include:
--
-- -   __@NotOptimized@__ —An Auto Scaling group is considered not
--     optimized when Compute Optimizer identifies a recommendation that
--     can provide better performance for your workload.
--
-- -   __@Optimized@__ —An Auto Scaling group is considered optimized when
--     Compute Optimizer determines that the group is correctly provisioned
--     to run your workload based on the chosen instance type. For
--     optimized resources, Compute Optimizer might recommend a new
--     generation instance type.
--
-- 'lastRefreshTimestamp', 'autoScalingGroupRecommendation_lastRefreshTimestamp' - The timestamp of when the Auto Scaling group recommendation was last
-- refreshed.
--
-- 'currentConfiguration', 'autoScalingGroupRecommendation_currentConfiguration' - An array of objects that describe the current configuration of the Auto
-- Scaling group.
--
-- 'accountId', 'autoScalingGroupRecommendation_accountId' - The Amazon Web Services account ID of the Auto Scaling group.
--
-- 'autoScalingGroupName', 'autoScalingGroupRecommendation_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'utilizationMetrics', 'autoScalingGroupRecommendation_utilizationMetrics' - An array of objects that describe the utilization metrics of the Auto
-- Scaling group.
--
-- 'autoScalingGroupArn', 'autoScalingGroupRecommendation_autoScalingGroupArn' - The Amazon Resource Name (ARN) of the Auto Scaling group.
--
-- 'recommendationOptions', 'autoScalingGroupRecommendation_recommendationOptions' - An array of objects that describe the recommendation options for the
-- Auto Scaling group.
--
-- 'lookBackPeriodInDays', 'autoScalingGroupRecommendation_lookBackPeriodInDays' - The number of days for which utilization metrics were analyzed for the
-- Auto Scaling group.
newAutoScalingGroupRecommendation ::
  AutoScalingGroupRecommendation
newAutoScalingGroupRecommendation =
  AutoScalingGroupRecommendation'
    { finding =
        Prelude.Nothing,
      lastRefreshTimestamp = Prelude.Nothing,
      currentConfiguration = Prelude.Nothing,
      accountId = Prelude.Nothing,
      autoScalingGroupName = Prelude.Nothing,
      utilizationMetrics = Prelude.Nothing,
      autoScalingGroupArn = Prelude.Nothing,
      recommendationOptions = Prelude.Nothing,
      lookBackPeriodInDays = Prelude.Nothing
    }

-- | The finding classification of the Auto Scaling group.
--
-- Findings for Auto Scaling groups include:
--
-- -   __@NotOptimized@__ —An Auto Scaling group is considered not
--     optimized when Compute Optimizer identifies a recommendation that
--     can provide better performance for your workload.
--
-- -   __@Optimized@__ —An Auto Scaling group is considered optimized when
--     Compute Optimizer determines that the group is correctly provisioned
--     to run your workload based on the chosen instance type. For
--     optimized resources, Compute Optimizer might recommend a new
--     generation instance type.
autoScalingGroupRecommendation_finding :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Finding)
autoScalingGroupRecommendation_finding = Lens.lens (\AutoScalingGroupRecommendation' {finding} -> finding) (\s@AutoScalingGroupRecommendation' {} a -> s {finding = a} :: AutoScalingGroupRecommendation)

-- | The timestamp of when the Auto Scaling group recommendation was last
-- refreshed.
autoScalingGroupRecommendation_lastRefreshTimestamp :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Prelude.UTCTime)
autoScalingGroupRecommendation_lastRefreshTimestamp = Lens.lens (\AutoScalingGroupRecommendation' {lastRefreshTimestamp} -> lastRefreshTimestamp) (\s@AutoScalingGroupRecommendation' {} a -> s {lastRefreshTimestamp = a} :: AutoScalingGroupRecommendation) Prelude.. Lens.mapping Core._Time

-- | An array of objects that describe the current configuration of the Auto
-- Scaling group.
autoScalingGroupRecommendation_currentConfiguration :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe AutoScalingGroupConfiguration)
autoScalingGroupRecommendation_currentConfiguration = Lens.lens (\AutoScalingGroupRecommendation' {currentConfiguration} -> currentConfiguration) (\s@AutoScalingGroupRecommendation' {} a -> s {currentConfiguration = a} :: AutoScalingGroupRecommendation)

-- | The Amazon Web Services account ID of the Auto Scaling group.
autoScalingGroupRecommendation_accountId :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Prelude.Text)
autoScalingGroupRecommendation_accountId = Lens.lens (\AutoScalingGroupRecommendation' {accountId} -> accountId) (\s@AutoScalingGroupRecommendation' {} a -> s {accountId = a} :: AutoScalingGroupRecommendation)

-- | The name of the Auto Scaling group.
autoScalingGroupRecommendation_autoScalingGroupName :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Prelude.Text)
autoScalingGroupRecommendation_autoScalingGroupName = Lens.lens (\AutoScalingGroupRecommendation' {autoScalingGroupName} -> autoScalingGroupName) (\s@AutoScalingGroupRecommendation' {} a -> s {autoScalingGroupName = a} :: AutoScalingGroupRecommendation)

-- | An array of objects that describe the utilization metrics of the Auto
-- Scaling group.
autoScalingGroupRecommendation_utilizationMetrics :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe [UtilizationMetric])
autoScalingGroupRecommendation_utilizationMetrics = Lens.lens (\AutoScalingGroupRecommendation' {utilizationMetrics} -> utilizationMetrics) (\s@AutoScalingGroupRecommendation' {} a -> s {utilizationMetrics = a} :: AutoScalingGroupRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
autoScalingGroupRecommendation_autoScalingGroupArn :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Prelude.Text)
autoScalingGroupRecommendation_autoScalingGroupArn = Lens.lens (\AutoScalingGroupRecommendation' {autoScalingGroupArn} -> autoScalingGroupArn) (\s@AutoScalingGroupRecommendation' {} a -> s {autoScalingGroupArn = a} :: AutoScalingGroupRecommendation)

-- | An array of objects that describe the recommendation options for the
-- Auto Scaling group.
autoScalingGroupRecommendation_recommendationOptions :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe [AutoScalingGroupRecommendationOption])
autoScalingGroupRecommendation_recommendationOptions = Lens.lens (\AutoScalingGroupRecommendation' {recommendationOptions} -> recommendationOptions) (\s@AutoScalingGroupRecommendation' {} a -> s {recommendationOptions = a} :: AutoScalingGroupRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The number of days for which utilization metrics were analyzed for the
-- Auto Scaling group.
autoScalingGroupRecommendation_lookBackPeriodInDays :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Prelude.Double)
autoScalingGroupRecommendation_lookBackPeriodInDays = Lens.lens (\AutoScalingGroupRecommendation' {lookBackPeriodInDays} -> lookBackPeriodInDays) (\s@AutoScalingGroupRecommendation' {} a -> s {lookBackPeriodInDays = a} :: AutoScalingGroupRecommendation)

instance Core.FromJSON AutoScalingGroupRecommendation where
  parseJSON =
    Core.withObject
      "AutoScalingGroupRecommendation"
      ( \x ->
          AutoScalingGroupRecommendation'
            Prelude.<$> (x Core..:? "finding")
            Prelude.<*> (x Core..:? "lastRefreshTimestamp")
            Prelude.<*> (x Core..:? "currentConfiguration")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "autoScalingGroupName")
            Prelude.<*> ( x Core..:? "utilizationMetrics"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "autoScalingGroupArn")
            Prelude.<*> ( x Core..:? "recommendationOptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "lookBackPeriodInDays")
      )

instance
  Prelude.Hashable
    AutoScalingGroupRecommendation
  where
  hashWithSalt
    salt'
    AutoScalingGroupRecommendation' {..} =
      salt' `Prelude.hashWithSalt` lookBackPeriodInDays
        `Prelude.hashWithSalt` recommendationOptions
        `Prelude.hashWithSalt` autoScalingGroupArn
        `Prelude.hashWithSalt` utilizationMetrics
        `Prelude.hashWithSalt` autoScalingGroupName
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` currentConfiguration
        `Prelude.hashWithSalt` lastRefreshTimestamp
        `Prelude.hashWithSalt` finding

instance
  Prelude.NFData
    AutoScalingGroupRecommendation
  where
  rnf AutoScalingGroupRecommendation' {..} =
    Prelude.rnf finding
      `Prelude.seq` Prelude.rnf lookBackPeriodInDays
      `Prelude.seq` Prelude.rnf recommendationOptions
      `Prelude.seq` Prelude.rnf autoScalingGroupArn
      `Prelude.seq` Prelude.rnf utilizationMetrics
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf currentConfiguration
      `Prelude.seq` Prelude.rnf lastRefreshTimestamp

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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendation where

import Amazonka.ComputeOptimizer.Types.AutoScalingGroupConfiguration
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendationOption
import Amazonka.ComputeOptimizer.Types.CurrentPerformanceRisk
import Amazonka.ComputeOptimizer.Types.EffectiveRecommendationPreferences
import Amazonka.ComputeOptimizer.Types.Finding
import Amazonka.ComputeOptimizer.Types.InferredWorkloadType
import Amazonka.ComputeOptimizer.Types.UtilizationMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Auto Scaling group recommendation.
--
-- /See:/ 'newAutoScalingGroupRecommendation' smart constructor.
data AutoScalingGroupRecommendation = AutoScalingGroupRecommendation'
  { -- | The Amazon Web Services account ID of the Auto Scaling group.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Auto Scaling group.
    autoScalingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the current configuration of the Auto
    -- Scaling group.
    currentConfiguration :: Prelude.Maybe AutoScalingGroupConfiguration,
    -- | The risk of the current Auto Scaling group not meeting the performance
    -- needs of its workloads. The higher the risk, the more likely the current
    -- Auto Scaling group configuration has insufficient capacity and cannot
    -- meet workload requirements.
    currentPerformanceRisk :: Prelude.Maybe CurrentPerformanceRisk,
    -- | An object that describes the effective recommendation preferences for
    -- the Auto Scaling group.
    effectiveRecommendationPreferences :: Prelude.Maybe EffectiveRecommendationPreferences,
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
    finding :: Prelude.Maybe Finding,
    -- | The applications that might be running on the instances in the Auto
    -- Scaling group as inferred by Compute Optimizer.
    --
    -- Compute Optimizer can infer if one of the following applications might
    -- be running on the instances:
    --
    -- -   @AmazonEmr@ - Infers that Amazon EMR might be running on the
    --     instances.
    --
    -- -   @ApacheCassandra@ - Infers that Apache Cassandra might be running on
    --     the instances.
    --
    -- -   @ApacheHadoop@ - Infers that Apache Hadoop might be running on the
    --     instances.
    --
    -- -   @Memcached@ - Infers that Memcached might be running on the
    --     instances.
    --
    -- -   @NGINX@ - Infers that NGINX might be running on the instances.
    --
    -- -   @PostgreSql@ - Infers that PostgreSQL might be running on the
    --     instances.
    --
    -- -   @Redis@ - Infers that Redis might be running on the instances.
    inferredWorkloadTypes :: Prelude.Maybe [InferredWorkloadType],
    -- | The timestamp of when the Auto Scaling group recommendation was last
    -- generated.
    lastRefreshTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The number of days for which utilization metrics were analyzed for the
    -- Auto Scaling group.
    lookBackPeriodInDays :: Prelude.Maybe Prelude.Double,
    -- | An array of objects that describe the recommendation options for the
    -- Auto Scaling group.
    recommendationOptions :: Prelude.Maybe [AutoScalingGroupRecommendationOption],
    -- | An array of objects that describe the utilization metrics of the Auto
    -- Scaling group.
    utilizationMetrics :: Prelude.Maybe [UtilizationMetric]
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
-- 'accountId', 'autoScalingGroupRecommendation_accountId' - The Amazon Web Services account ID of the Auto Scaling group.
--
-- 'autoScalingGroupArn', 'autoScalingGroupRecommendation_autoScalingGroupArn' - The Amazon Resource Name (ARN) of the Auto Scaling group.
--
-- 'autoScalingGroupName', 'autoScalingGroupRecommendation_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'currentConfiguration', 'autoScalingGroupRecommendation_currentConfiguration' - An array of objects that describe the current configuration of the Auto
-- Scaling group.
--
-- 'currentPerformanceRisk', 'autoScalingGroupRecommendation_currentPerformanceRisk' - The risk of the current Auto Scaling group not meeting the performance
-- needs of its workloads. The higher the risk, the more likely the current
-- Auto Scaling group configuration has insufficient capacity and cannot
-- meet workload requirements.
--
-- 'effectiveRecommendationPreferences', 'autoScalingGroupRecommendation_effectiveRecommendationPreferences' - An object that describes the effective recommendation preferences for
-- the Auto Scaling group.
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
-- 'inferredWorkloadTypes', 'autoScalingGroupRecommendation_inferredWorkloadTypes' - The applications that might be running on the instances in the Auto
-- Scaling group as inferred by Compute Optimizer.
--
-- Compute Optimizer can infer if one of the following applications might
-- be running on the instances:
--
-- -   @AmazonEmr@ - Infers that Amazon EMR might be running on the
--     instances.
--
-- -   @ApacheCassandra@ - Infers that Apache Cassandra might be running on
--     the instances.
--
-- -   @ApacheHadoop@ - Infers that Apache Hadoop might be running on the
--     instances.
--
-- -   @Memcached@ - Infers that Memcached might be running on the
--     instances.
--
-- -   @NGINX@ - Infers that NGINX might be running on the instances.
--
-- -   @PostgreSql@ - Infers that PostgreSQL might be running on the
--     instances.
--
-- -   @Redis@ - Infers that Redis might be running on the instances.
--
-- 'lastRefreshTimestamp', 'autoScalingGroupRecommendation_lastRefreshTimestamp' - The timestamp of when the Auto Scaling group recommendation was last
-- generated.
--
-- 'lookBackPeriodInDays', 'autoScalingGroupRecommendation_lookBackPeriodInDays' - The number of days for which utilization metrics were analyzed for the
-- Auto Scaling group.
--
-- 'recommendationOptions', 'autoScalingGroupRecommendation_recommendationOptions' - An array of objects that describe the recommendation options for the
-- Auto Scaling group.
--
-- 'utilizationMetrics', 'autoScalingGroupRecommendation_utilizationMetrics' - An array of objects that describe the utilization metrics of the Auto
-- Scaling group.
newAutoScalingGroupRecommendation ::
  AutoScalingGroupRecommendation
newAutoScalingGroupRecommendation =
  AutoScalingGroupRecommendation'
    { accountId =
        Prelude.Nothing,
      autoScalingGroupArn = Prelude.Nothing,
      autoScalingGroupName = Prelude.Nothing,
      currentConfiguration = Prelude.Nothing,
      currentPerformanceRisk = Prelude.Nothing,
      effectiveRecommendationPreferences =
        Prelude.Nothing,
      finding = Prelude.Nothing,
      inferredWorkloadTypes = Prelude.Nothing,
      lastRefreshTimestamp = Prelude.Nothing,
      lookBackPeriodInDays = Prelude.Nothing,
      recommendationOptions = Prelude.Nothing,
      utilizationMetrics = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the Auto Scaling group.
autoScalingGroupRecommendation_accountId :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Prelude.Text)
autoScalingGroupRecommendation_accountId = Lens.lens (\AutoScalingGroupRecommendation' {accountId} -> accountId) (\s@AutoScalingGroupRecommendation' {} a -> s {accountId = a} :: AutoScalingGroupRecommendation)

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
autoScalingGroupRecommendation_autoScalingGroupArn :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Prelude.Text)
autoScalingGroupRecommendation_autoScalingGroupArn = Lens.lens (\AutoScalingGroupRecommendation' {autoScalingGroupArn} -> autoScalingGroupArn) (\s@AutoScalingGroupRecommendation' {} a -> s {autoScalingGroupArn = a} :: AutoScalingGroupRecommendation)

-- | The name of the Auto Scaling group.
autoScalingGroupRecommendation_autoScalingGroupName :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Prelude.Text)
autoScalingGroupRecommendation_autoScalingGroupName = Lens.lens (\AutoScalingGroupRecommendation' {autoScalingGroupName} -> autoScalingGroupName) (\s@AutoScalingGroupRecommendation' {} a -> s {autoScalingGroupName = a} :: AutoScalingGroupRecommendation)

-- | An array of objects that describe the current configuration of the Auto
-- Scaling group.
autoScalingGroupRecommendation_currentConfiguration :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe AutoScalingGroupConfiguration)
autoScalingGroupRecommendation_currentConfiguration = Lens.lens (\AutoScalingGroupRecommendation' {currentConfiguration} -> currentConfiguration) (\s@AutoScalingGroupRecommendation' {} a -> s {currentConfiguration = a} :: AutoScalingGroupRecommendation)

-- | The risk of the current Auto Scaling group not meeting the performance
-- needs of its workloads. The higher the risk, the more likely the current
-- Auto Scaling group configuration has insufficient capacity and cannot
-- meet workload requirements.
autoScalingGroupRecommendation_currentPerformanceRisk :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe CurrentPerformanceRisk)
autoScalingGroupRecommendation_currentPerformanceRisk = Lens.lens (\AutoScalingGroupRecommendation' {currentPerformanceRisk} -> currentPerformanceRisk) (\s@AutoScalingGroupRecommendation' {} a -> s {currentPerformanceRisk = a} :: AutoScalingGroupRecommendation)

-- | An object that describes the effective recommendation preferences for
-- the Auto Scaling group.
autoScalingGroupRecommendation_effectiveRecommendationPreferences :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe EffectiveRecommendationPreferences)
autoScalingGroupRecommendation_effectiveRecommendationPreferences = Lens.lens (\AutoScalingGroupRecommendation' {effectiveRecommendationPreferences} -> effectiveRecommendationPreferences) (\s@AutoScalingGroupRecommendation' {} a -> s {effectiveRecommendationPreferences = a} :: AutoScalingGroupRecommendation)

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

-- | The applications that might be running on the instances in the Auto
-- Scaling group as inferred by Compute Optimizer.
--
-- Compute Optimizer can infer if one of the following applications might
-- be running on the instances:
--
-- -   @AmazonEmr@ - Infers that Amazon EMR might be running on the
--     instances.
--
-- -   @ApacheCassandra@ - Infers that Apache Cassandra might be running on
--     the instances.
--
-- -   @ApacheHadoop@ - Infers that Apache Hadoop might be running on the
--     instances.
--
-- -   @Memcached@ - Infers that Memcached might be running on the
--     instances.
--
-- -   @NGINX@ - Infers that NGINX might be running on the instances.
--
-- -   @PostgreSql@ - Infers that PostgreSQL might be running on the
--     instances.
--
-- -   @Redis@ - Infers that Redis might be running on the instances.
autoScalingGroupRecommendation_inferredWorkloadTypes :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe [InferredWorkloadType])
autoScalingGroupRecommendation_inferredWorkloadTypes = Lens.lens (\AutoScalingGroupRecommendation' {inferredWorkloadTypes} -> inferredWorkloadTypes) (\s@AutoScalingGroupRecommendation' {} a -> s {inferredWorkloadTypes = a} :: AutoScalingGroupRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of when the Auto Scaling group recommendation was last
-- generated.
autoScalingGroupRecommendation_lastRefreshTimestamp :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Prelude.UTCTime)
autoScalingGroupRecommendation_lastRefreshTimestamp = Lens.lens (\AutoScalingGroupRecommendation' {lastRefreshTimestamp} -> lastRefreshTimestamp) (\s@AutoScalingGroupRecommendation' {} a -> s {lastRefreshTimestamp = a} :: AutoScalingGroupRecommendation) Prelude.. Lens.mapping Data._Time

-- | The number of days for which utilization metrics were analyzed for the
-- Auto Scaling group.
autoScalingGroupRecommendation_lookBackPeriodInDays :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe Prelude.Double)
autoScalingGroupRecommendation_lookBackPeriodInDays = Lens.lens (\AutoScalingGroupRecommendation' {lookBackPeriodInDays} -> lookBackPeriodInDays) (\s@AutoScalingGroupRecommendation' {} a -> s {lookBackPeriodInDays = a} :: AutoScalingGroupRecommendation)

-- | An array of objects that describe the recommendation options for the
-- Auto Scaling group.
autoScalingGroupRecommendation_recommendationOptions :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe [AutoScalingGroupRecommendationOption])
autoScalingGroupRecommendation_recommendationOptions = Lens.lens (\AutoScalingGroupRecommendation' {recommendationOptions} -> recommendationOptions) (\s@AutoScalingGroupRecommendation' {} a -> s {recommendationOptions = a} :: AutoScalingGroupRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that describe the utilization metrics of the Auto
-- Scaling group.
autoScalingGroupRecommendation_utilizationMetrics :: Lens.Lens' AutoScalingGroupRecommendation (Prelude.Maybe [UtilizationMetric])
autoScalingGroupRecommendation_utilizationMetrics = Lens.lens (\AutoScalingGroupRecommendation' {utilizationMetrics} -> utilizationMetrics) (\s@AutoScalingGroupRecommendation' {} a -> s {utilizationMetrics = a} :: AutoScalingGroupRecommendation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AutoScalingGroupRecommendation where
  parseJSON =
    Data.withObject
      "AutoScalingGroupRecommendation"
      ( \x ->
          AutoScalingGroupRecommendation'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "autoScalingGroupArn")
            Prelude.<*> (x Data..:? "autoScalingGroupName")
            Prelude.<*> (x Data..:? "currentConfiguration")
            Prelude.<*> (x Data..:? "currentPerformanceRisk")
            Prelude.<*> (x Data..:? "effectiveRecommendationPreferences")
            Prelude.<*> (x Data..:? "finding")
            Prelude.<*> ( x
                            Data..:? "inferredWorkloadTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "lastRefreshTimestamp")
            Prelude.<*> (x Data..:? "lookBackPeriodInDays")
            Prelude.<*> ( x
                            Data..:? "recommendationOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "utilizationMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AutoScalingGroupRecommendation
  where
  hashWithSalt
    _salt
    AutoScalingGroupRecommendation' {..} =
      _salt
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` autoScalingGroupArn
        `Prelude.hashWithSalt` autoScalingGroupName
        `Prelude.hashWithSalt` currentConfiguration
        `Prelude.hashWithSalt` currentPerformanceRisk
        `Prelude.hashWithSalt` effectiveRecommendationPreferences
        `Prelude.hashWithSalt` finding
        `Prelude.hashWithSalt` inferredWorkloadTypes
        `Prelude.hashWithSalt` lastRefreshTimestamp
        `Prelude.hashWithSalt` lookBackPeriodInDays
        `Prelude.hashWithSalt` recommendationOptions
        `Prelude.hashWithSalt` utilizationMetrics

instance
  Prelude.NFData
    AutoScalingGroupRecommendation
  where
  rnf AutoScalingGroupRecommendation' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf autoScalingGroupArn
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf currentConfiguration
      `Prelude.seq` Prelude.rnf currentPerformanceRisk
      `Prelude.seq` Prelude.rnf effectiveRecommendationPreferences
      `Prelude.seq` Prelude.rnf finding
      `Prelude.seq` Prelude.rnf inferredWorkloadTypes
      `Prelude.seq` Prelude.rnf lastRefreshTimestamp
      `Prelude.seq` Prelude.rnf lookBackPeriodInDays
      `Prelude.seq` Prelude.rnf recommendationOptions
      `Prelude.seq` Prelude.rnf utilizationMetrics

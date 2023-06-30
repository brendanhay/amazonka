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
-- Module      : Amazonka.ComputeOptimizer.Types.ECSServiceRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ECSServiceRecommendation where

import Amazonka.ComputeOptimizer.Types.CurrentPerformanceRisk
import Amazonka.ComputeOptimizer.Types.ECSServiceLaunchType
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFinding
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFindingReasonCode
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationOption
import Amazonka.ComputeOptimizer.Types.ECSServiceUtilizationMetric
import Amazonka.ComputeOptimizer.Types.ServiceConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon ECS service recommendation.
--
-- /See:/ 'newECSServiceRecommendation' smart constructor.
data ECSServiceRecommendation = ECSServiceRecommendation'
  { -- | The Amazon Web Services account ID of the ECS service.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The risk of the current ECS service not meeting the performance needs of
    -- its workloads. The higher the risk, the more likely the current service
    -- can\'t meet the performance requirements of its workload.
    currentPerformanceRisk :: Prelude.Maybe CurrentPerformanceRisk,
    -- | The configuration of the current ECS service.
    currentServiceConfiguration :: Prelude.Maybe ServiceConfiguration,
    -- | The finding classification of an ECS service.
    --
    -- Findings for ECS services include:
    --
    -- -   __@Underprovisioned@__ — When Compute Optimizer detects that there’s
    --     not enough memory or CPU, an ECS service is considered
    --     under-provisioned. An under-provisioned ECS service might result in
    --     poor application performance.
    --
    -- -   __@Overprovisioned@__ — When Compute Optimizer detects that there’s
    --     excessive memory or CPU, an ECS service is considered
    --     over-provisioned. An over-provisioned ECS service might result in
    --     additional infrastructure costs.
    --
    -- -   __@Optimized@__ — When both the CPU and memory of your ECS service
    --     meet the performance requirements of your workload, the service is
    --     considered optimized.
    finding :: Prelude.Maybe ECSServiceRecommendationFinding,
    -- | The reason for the finding classification of an ECS service.
    --
    -- Finding reason codes for ECS services include:
    --
    -- -   __@CPUUnderprovisioned@__ — The ECS service CPU configuration can be
    --     sized up to enhance the performance of your workload. This is
    --     identified by analyzing the @CPUUtilization@ metric of the current
    --     service during the look-back period.
    --
    -- -   __@CPUOverprovisioned@__ — The ECS service CPU configuration can be
    --     sized down while still meeting the performance requirements of your
    --     workload. This is identified by analyzing the @CPUUtilization@
    --     metric of the current service during the look-back period.
    --
    -- -   __@MemoryUnderprovisioned@__ — The ECS service memory configuration
    --     can be sized up to enhance the performance of your workload. This is
    --     identified by analyzing the @MemoryUtilization@ metric of the
    --     current service during the look-back period.
    --
    -- -   __@MemoryOverprovisioned@__ — The ECS service memory configuration
    --     can be sized down while still meeting the performance requirements
    --     of your workload. This is identified by analyzing the
    --     @MemoryUtilization@ metric of the current service during the
    --     look-back period.
    findingReasonCodes :: Prelude.Maybe [ECSServiceRecommendationFindingReasonCode],
    -- | The timestamp of when the ECS service recommendation was last generated.
    lastRefreshTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The launch type the ECS service is using.
    --
    -- Compute Optimizer only supports the Fargate launch type.
    launchType :: Prelude.Maybe ECSServiceLaunchType,
    -- | The number of days the ECS service utilization metrics were analyzed.
    lookbackPeriodInDays :: Prelude.Maybe Prelude.Double,
    -- | The Amazon Resource Name (ARN) of the current ECS service.
    --
    -- The following is the format of the ARN:
    --
    -- @arn:aws:ecs:region:aws_account_id:service\/cluster-name\/service-name@
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the recommendation options for the ECS
    -- service.
    serviceRecommendationOptions :: Prelude.Maybe [ECSServiceRecommendationOption],
    -- | An array of objects that describe the utilization metrics of the ECS
    -- service.
    utilizationMetrics :: Prelude.Maybe [ECSServiceUtilizationMetric]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ECSServiceRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'eCSServiceRecommendation_accountId' - The Amazon Web Services account ID of the ECS service.
--
-- 'currentPerformanceRisk', 'eCSServiceRecommendation_currentPerformanceRisk' - The risk of the current ECS service not meeting the performance needs of
-- its workloads. The higher the risk, the more likely the current service
-- can\'t meet the performance requirements of its workload.
--
-- 'currentServiceConfiguration', 'eCSServiceRecommendation_currentServiceConfiguration' - The configuration of the current ECS service.
--
-- 'finding', 'eCSServiceRecommendation_finding' - The finding classification of an ECS service.
--
-- Findings for ECS services include:
--
-- -   __@Underprovisioned@__ — When Compute Optimizer detects that there’s
--     not enough memory or CPU, an ECS service is considered
--     under-provisioned. An under-provisioned ECS service might result in
--     poor application performance.
--
-- -   __@Overprovisioned@__ — When Compute Optimizer detects that there’s
--     excessive memory or CPU, an ECS service is considered
--     over-provisioned. An over-provisioned ECS service might result in
--     additional infrastructure costs.
--
-- -   __@Optimized@__ — When both the CPU and memory of your ECS service
--     meet the performance requirements of your workload, the service is
--     considered optimized.
--
-- 'findingReasonCodes', 'eCSServiceRecommendation_findingReasonCodes' - The reason for the finding classification of an ECS service.
--
-- Finding reason codes for ECS services include:
--
-- -   __@CPUUnderprovisioned@__ — The ECS service CPU configuration can be
--     sized up to enhance the performance of your workload. This is
--     identified by analyzing the @CPUUtilization@ metric of the current
--     service during the look-back period.
--
-- -   __@CPUOverprovisioned@__ — The ECS service CPU configuration can be
--     sized down while still meeting the performance requirements of your
--     workload. This is identified by analyzing the @CPUUtilization@
--     metric of the current service during the look-back period.
--
-- -   __@MemoryUnderprovisioned@__ — The ECS service memory configuration
--     can be sized up to enhance the performance of your workload. This is
--     identified by analyzing the @MemoryUtilization@ metric of the
--     current service during the look-back period.
--
-- -   __@MemoryOverprovisioned@__ — The ECS service memory configuration
--     can be sized down while still meeting the performance requirements
--     of your workload. This is identified by analyzing the
--     @MemoryUtilization@ metric of the current service during the
--     look-back period.
--
-- 'lastRefreshTimestamp', 'eCSServiceRecommendation_lastRefreshTimestamp' - The timestamp of when the ECS service recommendation was last generated.
--
-- 'launchType', 'eCSServiceRecommendation_launchType' - The launch type the ECS service is using.
--
-- Compute Optimizer only supports the Fargate launch type.
--
-- 'lookbackPeriodInDays', 'eCSServiceRecommendation_lookbackPeriodInDays' - The number of days the ECS service utilization metrics were analyzed.
--
-- 'serviceArn', 'eCSServiceRecommendation_serviceArn' - The Amazon Resource Name (ARN) of the current ECS service.
--
-- The following is the format of the ARN:
--
-- @arn:aws:ecs:region:aws_account_id:service\/cluster-name\/service-name@
--
-- 'serviceRecommendationOptions', 'eCSServiceRecommendation_serviceRecommendationOptions' - An array of objects that describe the recommendation options for the ECS
-- service.
--
-- 'utilizationMetrics', 'eCSServiceRecommendation_utilizationMetrics' - An array of objects that describe the utilization metrics of the ECS
-- service.
newECSServiceRecommendation ::
  ECSServiceRecommendation
newECSServiceRecommendation =
  ECSServiceRecommendation'
    { accountId =
        Prelude.Nothing,
      currentPerformanceRisk = Prelude.Nothing,
      currentServiceConfiguration = Prelude.Nothing,
      finding = Prelude.Nothing,
      findingReasonCodes = Prelude.Nothing,
      lastRefreshTimestamp = Prelude.Nothing,
      launchType = Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      serviceRecommendationOptions = Prelude.Nothing,
      utilizationMetrics = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the ECS service.
eCSServiceRecommendation_accountId :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe Prelude.Text)
eCSServiceRecommendation_accountId = Lens.lens (\ECSServiceRecommendation' {accountId} -> accountId) (\s@ECSServiceRecommendation' {} a -> s {accountId = a} :: ECSServiceRecommendation)

-- | The risk of the current ECS service not meeting the performance needs of
-- its workloads. The higher the risk, the more likely the current service
-- can\'t meet the performance requirements of its workload.
eCSServiceRecommendation_currentPerformanceRisk :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe CurrentPerformanceRisk)
eCSServiceRecommendation_currentPerformanceRisk = Lens.lens (\ECSServiceRecommendation' {currentPerformanceRisk} -> currentPerformanceRisk) (\s@ECSServiceRecommendation' {} a -> s {currentPerformanceRisk = a} :: ECSServiceRecommendation)

-- | The configuration of the current ECS service.
eCSServiceRecommendation_currentServiceConfiguration :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe ServiceConfiguration)
eCSServiceRecommendation_currentServiceConfiguration = Lens.lens (\ECSServiceRecommendation' {currentServiceConfiguration} -> currentServiceConfiguration) (\s@ECSServiceRecommendation' {} a -> s {currentServiceConfiguration = a} :: ECSServiceRecommendation)

-- | The finding classification of an ECS service.
--
-- Findings for ECS services include:
--
-- -   __@Underprovisioned@__ — When Compute Optimizer detects that there’s
--     not enough memory or CPU, an ECS service is considered
--     under-provisioned. An under-provisioned ECS service might result in
--     poor application performance.
--
-- -   __@Overprovisioned@__ — When Compute Optimizer detects that there’s
--     excessive memory or CPU, an ECS service is considered
--     over-provisioned. An over-provisioned ECS service might result in
--     additional infrastructure costs.
--
-- -   __@Optimized@__ — When both the CPU and memory of your ECS service
--     meet the performance requirements of your workload, the service is
--     considered optimized.
eCSServiceRecommendation_finding :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe ECSServiceRecommendationFinding)
eCSServiceRecommendation_finding = Lens.lens (\ECSServiceRecommendation' {finding} -> finding) (\s@ECSServiceRecommendation' {} a -> s {finding = a} :: ECSServiceRecommendation)

-- | The reason for the finding classification of an ECS service.
--
-- Finding reason codes for ECS services include:
--
-- -   __@CPUUnderprovisioned@__ — The ECS service CPU configuration can be
--     sized up to enhance the performance of your workload. This is
--     identified by analyzing the @CPUUtilization@ metric of the current
--     service during the look-back period.
--
-- -   __@CPUOverprovisioned@__ — The ECS service CPU configuration can be
--     sized down while still meeting the performance requirements of your
--     workload. This is identified by analyzing the @CPUUtilization@
--     metric of the current service during the look-back period.
--
-- -   __@MemoryUnderprovisioned@__ — The ECS service memory configuration
--     can be sized up to enhance the performance of your workload. This is
--     identified by analyzing the @MemoryUtilization@ metric of the
--     current service during the look-back period.
--
-- -   __@MemoryOverprovisioned@__ — The ECS service memory configuration
--     can be sized down while still meeting the performance requirements
--     of your workload. This is identified by analyzing the
--     @MemoryUtilization@ metric of the current service during the
--     look-back period.
eCSServiceRecommendation_findingReasonCodes :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe [ECSServiceRecommendationFindingReasonCode])
eCSServiceRecommendation_findingReasonCodes = Lens.lens (\ECSServiceRecommendation' {findingReasonCodes} -> findingReasonCodes) (\s@ECSServiceRecommendation' {} a -> s {findingReasonCodes = a} :: ECSServiceRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of when the ECS service recommendation was last generated.
eCSServiceRecommendation_lastRefreshTimestamp :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe Prelude.UTCTime)
eCSServiceRecommendation_lastRefreshTimestamp = Lens.lens (\ECSServiceRecommendation' {lastRefreshTimestamp} -> lastRefreshTimestamp) (\s@ECSServiceRecommendation' {} a -> s {lastRefreshTimestamp = a} :: ECSServiceRecommendation) Prelude.. Lens.mapping Data._Time

-- | The launch type the ECS service is using.
--
-- Compute Optimizer only supports the Fargate launch type.
eCSServiceRecommendation_launchType :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe ECSServiceLaunchType)
eCSServiceRecommendation_launchType = Lens.lens (\ECSServiceRecommendation' {launchType} -> launchType) (\s@ECSServiceRecommendation' {} a -> s {launchType = a} :: ECSServiceRecommendation)

-- | The number of days the ECS service utilization metrics were analyzed.
eCSServiceRecommendation_lookbackPeriodInDays :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe Prelude.Double)
eCSServiceRecommendation_lookbackPeriodInDays = Lens.lens (\ECSServiceRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@ECSServiceRecommendation' {} a -> s {lookbackPeriodInDays = a} :: ECSServiceRecommendation)

-- | The Amazon Resource Name (ARN) of the current ECS service.
--
-- The following is the format of the ARN:
--
-- @arn:aws:ecs:region:aws_account_id:service\/cluster-name\/service-name@
eCSServiceRecommendation_serviceArn :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe Prelude.Text)
eCSServiceRecommendation_serviceArn = Lens.lens (\ECSServiceRecommendation' {serviceArn} -> serviceArn) (\s@ECSServiceRecommendation' {} a -> s {serviceArn = a} :: ECSServiceRecommendation)

-- | An array of objects that describe the recommendation options for the ECS
-- service.
eCSServiceRecommendation_serviceRecommendationOptions :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe [ECSServiceRecommendationOption])
eCSServiceRecommendation_serviceRecommendationOptions = Lens.lens (\ECSServiceRecommendation' {serviceRecommendationOptions} -> serviceRecommendationOptions) (\s@ECSServiceRecommendation' {} a -> s {serviceRecommendationOptions = a} :: ECSServiceRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that describe the utilization metrics of the ECS
-- service.
eCSServiceRecommendation_utilizationMetrics :: Lens.Lens' ECSServiceRecommendation (Prelude.Maybe [ECSServiceUtilizationMetric])
eCSServiceRecommendation_utilizationMetrics = Lens.lens (\ECSServiceRecommendation' {utilizationMetrics} -> utilizationMetrics) (\s@ECSServiceRecommendation' {} a -> s {utilizationMetrics = a} :: ECSServiceRecommendation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ECSServiceRecommendation where
  parseJSON =
    Data.withObject
      "ECSServiceRecommendation"
      ( \x ->
          ECSServiceRecommendation'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "currentPerformanceRisk")
            Prelude.<*> (x Data..:? "currentServiceConfiguration")
            Prelude.<*> (x Data..:? "finding")
            Prelude.<*> ( x
                            Data..:? "findingReasonCodes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "lastRefreshTimestamp")
            Prelude.<*> (x Data..:? "launchType")
            Prelude.<*> (x Data..:? "lookbackPeriodInDays")
            Prelude.<*> (x Data..:? "serviceArn")
            Prelude.<*> ( x
                            Data..:? "serviceRecommendationOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "utilizationMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ECSServiceRecommendation where
  hashWithSalt _salt ECSServiceRecommendation' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` currentPerformanceRisk
      `Prelude.hashWithSalt` currentServiceConfiguration
      `Prelude.hashWithSalt` finding
      `Prelude.hashWithSalt` findingReasonCodes
      `Prelude.hashWithSalt` lastRefreshTimestamp
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` lookbackPeriodInDays
      `Prelude.hashWithSalt` serviceArn
      `Prelude.hashWithSalt` serviceRecommendationOptions
      `Prelude.hashWithSalt` utilizationMetrics

instance Prelude.NFData ECSServiceRecommendation where
  rnf ECSServiceRecommendation' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf currentPerformanceRisk
      `Prelude.seq` Prelude.rnf currentServiceConfiguration
      `Prelude.seq` Prelude.rnf finding
      `Prelude.seq` Prelude.rnf findingReasonCodes
      `Prelude.seq` Prelude.rnf lastRefreshTimestamp
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf lookbackPeriodInDays
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf serviceRecommendationOptions
      `Prelude.seq` Prelude.rnf utilizationMetrics

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
-- Module      : Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendation where

import Amazonka.ComputeOptimizer.Types.CurrentPerformanceRisk
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryRecommendationOption
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFinding
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFindingReasonCode
import Amazonka.ComputeOptimizer.Types.LambdaFunctionUtilizationMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Lambda function recommendation.
--
-- /See:/ 'newLambdaFunctionRecommendation' smart constructor.
data LambdaFunctionRecommendation = LambdaFunctionRecommendation'
  { -- | The Amazon Web Services account ID of the function.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The amount of memory, in MB, that\'s allocated to the current function.
    currentMemorySize :: Prelude.Maybe Prelude.Int,
    -- | The risk of the current Lambda function not meeting the performance
    -- needs of its workloads. The higher the risk, the more likely the current
    -- Lambda function requires more memory.
    currentPerformanceRisk :: Prelude.Maybe CurrentPerformanceRisk,
    -- | The finding classification of the function.
    --
    -- Findings for functions include:
    --
    -- -   __@Optimized@__ — The function is correctly provisioned to run your
    --     workload based on its current configuration and its utilization
    --     history. This finding classification does not include finding reason
    --     codes.
    --
    -- -   __@NotOptimized@__ — The function is performing at a higher level
    --     (over-provisioned) or at a lower level (under-provisioned) than
    --     required for your workload because its current configuration is not
    --     optimal. Over-provisioned resources might lead to unnecessary
    --     infrastructure cost, and under-provisioned resources might lead to
    --     poor application performance. This finding classification can
    --     include the @MemoryUnderprovisioned@ and @MemoryUnderprovisioned@
    --     finding reason codes.
    --
    -- -   __@Unavailable@__ — Compute Optimizer was unable to generate a
    --     recommendation for the function. This could be because the function
    --     has not accumulated sufficient metric data, or the function does not
    --     qualify for a recommendation. This finding classification can
    --     include the @InsufficientData@ and @Inconclusive@ finding reason
    --     codes.
    --
    --     Functions with a finding of unavailable are not returned unless you
    --     specify the @filter@ parameter with a value of @Unavailable@ in your
    --     @GetLambdaFunctionRecommendations@ request.
    finding :: Prelude.Maybe LambdaFunctionRecommendationFinding,
    -- | The reason for the finding classification of the function.
    --
    -- Functions that have a finding classification of @Optimized@ don\'t have
    -- a finding reason code.
    --
    -- Finding reason codes for functions include:
    --
    -- -   __@MemoryOverprovisioned@__ — The function is over-provisioned when
    --     its memory configuration can be sized down while still meeting the
    --     performance requirements of your workload. An over-provisioned
    --     function might lead to unnecessary infrastructure cost. This finding
    --     reason code is part of the @NotOptimized@ finding classification.
    --
    -- -   __@MemoryUnderprovisioned@__ — The function is under-provisioned
    --     when its memory configuration doesn\'t meet the performance
    --     requirements of the workload. An under-provisioned function might
    --     lead to poor application performance. This finding reason code is
    --     part of the @NotOptimized@ finding classification.
    --
    -- -   __@InsufficientData@__ — The function does not have sufficient
    --     metric data for Compute Optimizer to generate a recommendation. For
    --     more information, see the
    --     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/requirements.html Supported resources and requirements>
    --     in the /Compute Optimizer User Guide/. This finding reason code is
    --     part of the @Unavailable@ finding classification.
    --
    -- -   __@Inconclusive@__ — The function does not qualify for a
    --     recommendation because Compute Optimizer cannot generate a
    --     recommendation with a high degree of confidence. This finding reason
    --     code is part of the @Unavailable@ finding classification.
    findingReasonCodes :: Prelude.Maybe [LambdaFunctionRecommendationFindingReasonCode],
    -- | The Amazon Resource Name (ARN) of the current function.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The version number of the current function.
    functionVersion :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the function recommendation was last generated.
    lastRefreshTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The number of days for which utilization metrics were analyzed for the
    -- function.
    lookbackPeriodInDays :: Prelude.Maybe Prelude.Double,
    -- | An array of objects that describe the memory configuration
    -- recommendation options for the function.
    memorySizeRecommendationOptions :: Prelude.Maybe [LambdaFunctionMemoryRecommendationOption],
    -- | The number of times your function code was applied during the look-back
    -- period.
    numberOfInvocations :: Prelude.Maybe Prelude.Integer,
    -- | An array of objects that describe the utilization metrics of the
    -- function.
    utilizationMetrics :: Prelude.Maybe [LambdaFunctionUtilizationMetric]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'lambdaFunctionRecommendation_accountId' - The Amazon Web Services account ID of the function.
--
-- 'currentMemorySize', 'lambdaFunctionRecommendation_currentMemorySize' - The amount of memory, in MB, that\'s allocated to the current function.
--
-- 'currentPerformanceRisk', 'lambdaFunctionRecommendation_currentPerformanceRisk' - The risk of the current Lambda function not meeting the performance
-- needs of its workloads. The higher the risk, the more likely the current
-- Lambda function requires more memory.
--
-- 'finding', 'lambdaFunctionRecommendation_finding' - The finding classification of the function.
--
-- Findings for functions include:
--
-- -   __@Optimized@__ — The function is correctly provisioned to run your
--     workload based on its current configuration and its utilization
--     history. This finding classification does not include finding reason
--     codes.
--
-- -   __@NotOptimized@__ — The function is performing at a higher level
--     (over-provisioned) or at a lower level (under-provisioned) than
--     required for your workload because its current configuration is not
--     optimal. Over-provisioned resources might lead to unnecessary
--     infrastructure cost, and under-provisioned resources might lead to
--     poor application performance. This finding classification can
--     include the @MemoryUnderprovisioned@ and @MemoryUnderprovisioned@
--     finding reason codes.
--
-- -   __@Unavailable@__ — Compute Optimizer was unable to generate a
--     recommendation for the function. This could be because the function
--     has not accumulated sufficient metric data, or the function does not
--     qualify for a recommendation. This finding classification can
--     include the @InsufficientData@ and @Inconclusive@ finding reason
--     codes.
--
--     Functions with a finding of unavailable are not returned unless you
--     specify the @filter@ parameter with a value of @Unavailable@ in your
--     @GetLambdaFunctionRecommendations@ request.
--
-- 'findingReasonCodes', 'lambdaFunctionRecommendation_findingReasonCodes' - The reason for the finding classification of the function.
--
-- Functions that have a finding classification of @Optimized@ don\'t have
-- a finding reason code.
--
-- Finding reason codes for functions include:
--
-- -   __@MemoryOverprovisioned@__ — The function is over-provisioned when
--     its memory configuration can be sized down while still meeting the
--     performance requirements of your workload. An over-provisioned
--     function might lead to unnecessary infrastructure cost. This finding
--     reason code is part of the @NotOptimized@ finding classification.
--
-- -   __@MemoryUnderprovisioned@__ — The function is under-provisioned
--     when its memory configuration doesn\'t meet the performance
--     requirements of the workload. An under-provisioned function might
--     lead to poor application performance. This finding reason code is
--     part of the @NotOptimized@ finding classification.
--
-- -   __@InsufficientData@__ — The function does not have sufficient
--     metric data for Compute Optimizer to generate a recommendation. For
--     more information, see the
--     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/requirements.html Supported resources and requirements>
--     in the /Compute Optimizer User Guide/. This finding reason code is
--     part of the @Unavailable@ finding classification.
--
-- -   __@Inconclusive@__ — The function does not qualify for a
--     recommendation because Compute Optimizer cannot generate a
--     recommendation with a high degree of confidence. This finding reason
--     code is part of the @Unavailable@ finding classification.
--
-- 'functionArn', 'lambdaFunctionRecommendation_functionArn' - The Amazon Resource Name (ARN) of the current function.
--
-- 'functionVersion', 'lambdaFunctionRecommendation_functionVersion' - The version number of the current function.
--
-- 'lastRefreshTimestamp', 'lambdaFunctionRecommendation_lastRefreshTimestamp' - The timestamp of when the function recommendation was last generated.
--
-- 'lookbackPeriodInDays', 'lambdaFunctionRecommendation_lookbackPeriodInDays' - The number of days for which utilization metrics were analyzed for the
-- function.
--
-- 'memorySizeRecommendationOptions', 'lambdaFunctionRecommendation_memorySizeRecommendationOptions' - An array of objects that describe the memory configuration
-- recommendation options for the function.
--
-- 'numberOfInvocations', 'lambdaFunctionRecommendation_numberOfInvocations' - The number of times your function code was applied during the look-back
-- period.
--
-- 'utilizationMetrics', 'lambdaFunctionRecommendation_utilizationMetrics' - An array of objects that describe the utilization metrics of the
-- function.
newLambdaFunctionRecommendation ::
  LambdaFunctionRecommendation
newLambdaFunctionRecommendation =
  LambdaFunctionRecommendation'
    { accountId =
        Prelude.Nothing,
      currentMemorySize = Prelude.Nothing,
      currentPerformanceRisk = Prelude.Nothing,
      finding = Prelude.Nothing,
      findingReasonCodes = Prelude.Nothing,
      functionArn = Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      lastRefreshTimestamp = Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing,
      memorySizeRecommendationOptions =
        Prelude.Nothing,
      numberOfInvocations = Prelude.Nothing,
      utilizationMetrics = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the function.
lambdaFunctionRecommendation_accountId :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Text)
lambdaFunctionRecommendation_accountId = Lens.lens (\LambdaFunctionRecommendation' {accountId} -> accountId) (\s@LambdaFunctionRecommendation' {} a -> s {accountId = a} :: LambdaFunctionRecommendation)

-- | The amount of memory, in MB, that\'s allocated to the current function.
lambdaFunctionRecommendation_currentMemorySize :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Int)
lambdaFunctionRecommendation_currentMemorySize = Lens.lens (\LambdaFunctionRecommendation' {currentMemorySize} -> currentMemorySize) (\s@LambdaFunctionRecommendation' {} a -> s {currentMemorySize = a} :: LambdaFunctionRecommendation)

-- | The risk of the current Lambda function not meeting the performance
-- needs of its workloads. The higher the risk, the more likely the current
-- Lambda function requires more memory.
lambdaFunctionRecommendation_currentPerformanceRisk :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe CurrentPerformanceRisk)
lambdaFunctionRecommendation_currentPerformanceRisk = Lens.lens (\LambdaFunctionRecommendation' {currentPerformanceRisk} -> currentPerformanceRisk) (\s@LambdaFunctionRecommendation' {} a -> s {currentPerformanceRisk = a} :: LambdaFunctionRecommendation)

-- | The finding classification of the function.
--
-- Findings for functions include:
--
-- -   __@Optimized@__ — The function is correctly provisioned to run your
--     workload based on its current configuration and its utilization
--     history. This finding classification does not include finding reason
--     codes.
--
-- -   __@NotOptimized@__ — The function is performing at a higher level
--     (over-provisioned) or at a lower level (under-provisioned) than
--     required for your workload because its current configuration is not
--     optimal. Over-provisioned resources might lead to unnecessary
--     infrastructure cost, and under-provisioned resources might lead to
--     poor application performance. This finding classification can
--     include the @MemoryUnderprovisioned@ and @MemoryUnderprovisioned@
--     finding reason codes.
--
-- -   __@Unavailable@__ — Compute Optimizer was unable to generate a
--     recommendation for the function. This could be because the function
--     has not accumulated sufficient metric data, or the function does not
--     qualify for a recommendation. This finding classification can
--     include the @InsufficientData@ and @Inconclusive@ finding reason
--     codes.
--
--     Functions with a finding of unavailable are not returned unless you
--     specify the @filter@ parameter with a value of @Unavailable@ in your
--     @GetLambdaFunctionRecommendations@ request.
lambdaFunctionRecommendation_finding :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe LambdaFunctionRecommendationFinding)
lambdaFunctionRecommendation_finding = Lens.lens (\LambdaFunctionRecommendation' {finding} -> finding) (\s@LambdaFunctionRecommendation' {} a -> s {finding = a} :: LambdaFunctionRecommendation)

-- | The reason for the finding classification of the function.
--
-- Functions that have a finding classification of @Optimized@ don\'t have
-- a finding reason code.
--
-- Finding reason codes for functions include:
--
-- -   __@MemoryOverprovisioned@__ — The function is over-provisioned when
--     its memory configuration can be sized down while still meeting the
--     performance requirements of your workload. An over-provisioned
--     function might lead to unnecessary infrastructure cost. This finding
--     reason code is part of the @NotOptimized@ finding classification.
--
-- -   __@MemoryUnderprovisioned@__ — The function is under-provisioned
--     when its memory configuration doesn\'t meet the performance
--     requirements of the workload. An under-provisioned function might
--     lead to poor application performance. This finding reason code is
--     part of the @NotOptimized@ finding classification.
--
-- -   __@InsufficientData@__ — The function does not have sufficient
--     metric data for Compute Optimizer to generate a recommendation. For
--     more information, see the
--     <https://docs.aws.amazon.com/compute-optimizer/latest/ug/requirements.html Supported resources and requirements>
--     in the /Compute Optimizer User Guide/. This finding reason code is
--     part of the @Unavailable@ finding classification.
--
-- -   __@Inconclusive@__ — The function does not qualify for a
--     recommendation because Compute Optimizer cannot generate a
--     recommendation with a high degree of confidence. This finding reason
--     code is part of the @Unavailable@ finding classification.
lambdaFunctionRecommendation_findingReasonCodes :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe [LambdaFunctionRecommendationFindingReasonCode])
lambdaFunctionRecommendation_findingReasonCodes = Lens.lens (\LambdaFunctionRecommendation' {findingReasonCodes} -> findingReasonCodes) (\s@LambdaFunctionRecommendation' {} a -> s {findingReasonCodes = a} :: LambdaFunctionRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the current function.
lambdaFunctionRecommendation_functionArn :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Text)
lambdaFunctionRecommendation_functionArn = Lens.lens (\LambdaFunctionRecommendation' {functionArn} -> functionArn) (\s@LambdaFunctionRecommendation' {} a -> s {functionArn = a} :: LambdaFunctionRecommendation)

-- | The version number of the current function.
lambdaFunctionRecommendation_functionVersion :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Text)
lambdaFunctionRecommendation_functionVersion = Lens.lens (\LambdaFunctionRecommendation' {functionVersion} -> functionVersion) (\s@LambdaFunctionRecommendation' {} a -> s {functionVersion = a} :: LambdaFunctionRecommendation)

-- | The timestamp of when the function recommendation was last generated.
lambdaFunctionRecommendation_lastRefreshTimestamp :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.UTCTime)
lambdaFunctionRecommendation_lastRefreshTimestamp = Lens.lens (\LambdaFunctionRecommendation' {lastRefreshTimestamp} -> lastRefreshTimestamp) (\s@LambdaFunctionRecommendation' {} a -> s {lastRefreshTimestamp = a} :: LambdaFunctionRecommendation) Prelude.. Lens.mapping Data._Time

-- | The number of days for which utilization metrics were analyzed for the
-- function.
lambdaFunctionRecommendation_lookbackPeriodInDays :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Double)
lambdaFunctionRecommendation_lookbackPeriodInDays = Lens.lens (\LambdaFunctionRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@LambdaFunctionRecommendation' {} a -> s {lookbackPeriodInDays = a} :: LambdaFunctionRecommendation)

-- | An array of objects that describe the memory configuration
-- recommendation options for the function.
lambdaFunctionRecommendation_memorySizeRecommendationOptions :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe [LambdaFunctionMemoryRecommendationOption])
lambdaFunctionRecommendation_memorySizeRecommendationOptions = Lens.lens (\LambdaFunctionRecommendation' {memorySizeRecommendationOptions} -> memorySizeRecommendationOptions) (\s@LambdaFunctionRecommendation' {} a -> s {memorySizeRecommendationOptions = a} :: LambdaFunctionRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The number of times your function code was applied during the look-back
-- period.
lambdaFunctionRecommendation_numberOfInvocations :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Integer)
lambdaFunctionRecommendation_numberOfInvocations = Lens.lens (\LambdaFunctionRecommendation' {numberOfInvocations} -> numberOfInvocations) (\s@LambdaFunctionRecommendation' {} a -> s {numberOfInvocations = a} :: LambdaFunctionRecommendation)

-- | An array of objects that describe the utilization metrics of the
-- function.
lambdaFunctionRecommendation_utilizationMetrics :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe [LambdaFunctionUtilizationMetric])
lambdaFunctionRecommendation_utilizationMetrics = Lens.lens (\LambdaFunctionRecommendation' {utilizationMetrics} -> utilizationMetrics) (\s@LambdaFunctionRecommendation' {} a -> s {utilizationMetrics = a} :: LambdaFunctionRecommendation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LambdaFunctionRecommendation where
  parseJSON =
    Data.withObject
      "LambdaFunctionRecommendation"
      ( \x ->
          LambdaFunctionRecommendation'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "currentMemorySize")
            Prelude.<*> (x Data..:? "currentPerformanceRisk")
            Prelude.<*> (x Data..:? "finding")
            Prelude.<*> ( x Data..:? "findingReasonCodes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "functionArn")
            Prelude.<*> (x Data..:? "functionVersion")
            Prelude.<*> (x Data..:? "lastRefreshTimestamp")
            Prelude.<*> (x Data..:? "lookbackPeriodInDays")
            Prelude.<*> ( x Data..:? "memorySizeRecommendationOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "numberOfInvocations")
            Prelude.<*> ( x Data..:? "utilizationMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    LambdaFunctionRecommendation
  where
  hashWithSalt _salt LambdaFunctionRecommendation' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` currentMemorySize
      `Prelude.hashWithSalt` currentPerformanceRisk
      `Prelude.hashWithSalt` finding
      `Prelude.hashWithSalt` findingReasonCodes
      `Prelude.hashWithSalt` functionArn
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` lastRefreshTimestamp
      `Prelude.hashWithSalt` lookbackPeriodInDays
      `Prelude.hashWithSalt` memorySizeRecommendationOptions
      `Prelude.hashWithSalt` numberOfInvocations
      `Prelude.hashWithSalt` utilizationMetrics

instance Prelude.NFData LambdaFunctionRecommendation where
  rnf LambdaFunctionRecommendation' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf currentMemorySize
      `Prelude.seq` Prelude.rnf currentPerformanceRisk
      `Prelude.seq` Prelude.rnf finding
      `Prelude.seq` Prelude.rnf findingReasonCodes
      `Prelude.seq` Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf lastRefreshTimestamp
      `Prelude.seq` Prelude.rnf lookbackPeriodInDays
      `Prelude.seq` Prelude.rnf memorySizeRecommendationOptions
      `Prelude.seq` Prelude.rnf numberOfInvocations
      `Prelude.seq` Prelude.rnf utilizationMetrics

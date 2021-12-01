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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendation where

import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryRecommendationOption
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFinding
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFindingReasonCode
import Amazonka.ComputeOptimizer.Types.LambdaFunctionUtilizationMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an Lambda function recommendation.
--
-- /See:/ 'newLambdaFunctionRecommendation' smart constructor.
data LambdaFunctionRecommendation = LambdaFunctionRecommendation'
  { -- | The Amazon Resource Name (ARN) of the current function.
    functionArn :: Prelude.Maybe Prelude.Text,
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
    -- | The amount of memory, in MB, that\'s allocated to the current function.
    currentMemorySize :: Prelude.Maybe Prelude.Int,
    -- | The timestamp of when the function recommendation was last refreshed.
    lastRefreshTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services account ID of the function.
    accountId :: Prelude.Maybe Prelude.Text,
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
    -- | An array of objects that describe the utilization metrics of the
    -- function.
    utilizationMetrics :: Prelude.Maybe [LambdaFunctionUtilizationMetric],
    -- | An array of objects that describe the memory configuration
    -- recommendation options for the function.
    memorySizeRecommendationOptions :: Prelude.Maybe [LambdaFunctionMemoryRecommendationOption],
    -- | The version number of the current function.
    functionVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of times your function code was applied during the look-back
    -- period.
    numberOfInvocations :: Prelude.Maybe Prelude.Integer,
    -- | The number of days for which utilization metrics were analyzed for the
    -- function.
    lookbackPeriodInDays :: Prelude.Maybe Prelude.Double
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
-- 'functionArn', 'lambdaFunctionRecommendation_functionArn' - The Amazon Resource Name (ARN) of the current function.
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
-- 'currentMemorySize', 'lambdaFunctionRecommendation_currentMemorySize' - The amount of memory, in MB, that\'s allocated to the current function.
--
-- 'lastRefreshTimestamp', 'lambdaFunctionRecommendation_lastRefreshTimestamp' - The timestamp of when the function recommendation was last refreshed.
--
-- 'accountId', 'lambdaFunctionRecommendation_accountId' - The Amazon Web Services account ID of the function.
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
-- 'utilizationMetrics', 'lambdaFunctionRecommendation_utilizationMetrics' - An array of objects that describe the utilization metrics of the
-- function.
--
-- 'memorySizeRecommendationOptions', 'lambdaFunctionRecommendation_memorySizeRecommendationOptions' - An array of objects that describe the memory configuration
-- recommendation options for the function.
--
-- 'functionVersion', 'lambdaFunctionRecommendation_functionVersion' - The version number of the current function.
--
-- 'numberOfInvocations', 'lambdaFunctionRecommendation_numberOfInvocations' - The number of times your function code was applied during the look-back
-- period.
--
-- 'lookbackPeriodInDays', 'lambdaFunctionRecommendation_lookbackPeriodInDays' - The number of days for which utilization metrics were analyzed for the
-- function.
newLambdaFunctionRecommendation ::
  LambdaFunctionRecommendation
newLambdaFunctionRecommendation =
  LambdaFunctionRecommendation'
    { functionArn =
        Prelude.Nothing,
      finding = Prelude.Nothing,
      currentMemorySize = Prelude.Nothing,
      lastRefreshTimestamp = Prelude.Nothing,
      accountId = Prelude.Nothing,
      findingReasonCodes = Prelude.Nothing,
      utilizationMetrics = Prelude.Nothing,
      memorySizeRecommendationOptions =
        Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      numberOfInvocations = Prelude.Nothing,
      lookbackPeriodInDays = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the current function.
lambdaFunctionRecommendation_functionArn :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Text)
lambdaFunctionRecommendation_functionArn = Lens.lens (\LambdaFunctionRecommendation' {functionArn} -> functionArn) (\s@LambdaFunctionRecommendation' {} a -> s {functionArn = a} :: LambdaFunctionRecommendation)

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

-- | The amount of memory, in MB, that\'s allocated to the current function.
lambdaFunctionRecommendation_currentMemorySize :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Int)
lambdaFunctionRecommendation_currentMemorySize = Lens.lens (\LambdaFunctionRecommendation' {currentMemorySize} -> currentMemorySize) (\s@LambdaFunctionRecommendation' {} a -> s {currentMemorySize = a} :: LambdaFunctionRecommendation)

-- | The timestamp of when the function recommendation was last refreshed.
lambdaFunctionRecommendation_lastRefreshTimestamp :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.UTCTime)
lambdaFunctionRecommendation_lastRefreshTimestamp = Lens.lens (\LambdaFunctionRecommendation' {lastRefreshTimestamp} -> lastRefreshTimestamp) (\s@LambdaFunctionRecommendation' {} a -> s {lastRefreshTimestamp = a} :: LambdaFunctionRecommendation) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services account ID of the function.
lambdaFunctionRecommendation_accountId :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Text)
lambdaFunctionRecommendation_accountId = Lens.lens (\LambdaFunctionRecommendation' {accountId} -> accountId) (\s@LambdaFunctionRecommendation' {} a -> s {accountId = a} :: LambdaFunctionRecommendation)

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

-- | An array of objects that describe the utilization metrics of the
-- function.
lambdaFunctionRecommendation_utilizationMetrics :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe [LambdaFunctionUtilizationMetric])
lambdaFunctionRecommendation_utilizationMetrics = Lens.lens (\LambdaFunctionRecommendation' {utilizationMetrics} -> utilizationMetrics) (\s@LambdaFunctionRecommendation' {} a -> s {utilizationMetrics = a} :: LambdaFunctionRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that describe the memory configuration
-- recommendation options for the function.
lambdaFunctionRecommendation_memorySizeRecommendationOptions :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe [LambdaFunctionMemoryRecommendationOption])
lambdaFunctionRecommendation_memorySizeRecommendationOptions = Lens.lens (\LambdaFunctionRecommendation' {memorySizeRecommendationOptions} -> memorySizeRecommendationOptions) (\s@LambdaFunctionRecommendation' {} a -> s {memorySizeRecommendationOptions = a} :: LambdaFunctionRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The version number of the current function.
lambdaFunctionRecommendation_functionVersion :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Text)
lambdaFunctionRecommendation_functionVersion = Lens.lens (\LambdaFunctionRecommendation' {functionVersion} -> functionVersion) (\s@LambdaFunctionRecommendation' {} a -> s {functionVersion = a} :: LambdaFunctionRecommendation)

-- | The number of times your function code was applied during the look-back
-- period.
lambdaFunctionRecommendation_numberOfInvocations :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Integer)
lambdaFunctionRecommendation_numberOfInvocations = Lens.lens (\LambdaFunctionRecommendation' {numberOfInvocations} -> numberOfInvocations) (\s@LambdaFunctionRecommendation' {} a -> s {numberOfInvocations = a} :: LambdaFunctionRecommendation)

-- | The number of days for which utilization metrics were analyzed for the
-- function.
lambdaFunctionRecommendation_lookbackPeriodInDays :: Lens.Lens' LambdaFunctionRecommendation (Prelude.Maybe Prelude.Double)
lambdaFunctionRecommendation_lookbackPeriodInDays = Lens.lens (\LambdaFunctionRecommendation' {lookbackPeriodInDays} -> lookbackPeriodInDays) (\s@LambdaFunctionRecommendation' {} a -> s {lookbackPeriodInDays = a} :: LambdaFunctionRecommendation)

instance Core.FromJSON LambdaFunctionRecommendation where
  parseJSON =
    Core.withObject
      "LambdaFunctionRecommendation"
      ( \x ->
          LambdaFunctionRecommendation'
            Prelude.<$> (x Core..:? "functionArn")
            Prelude.<*> (x Core..:? "finding")
            Prelude.<*> (x Core..:? "currentMemorySize")
            Prelude.<*> (x Core..:? "lastRefreshTimestamp")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> ( x Core..:? "findingReasonCodes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "utilizationMetrics"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "memorySizeRecommendationOptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "functionVersion")
            Prelude.<*> (x Core..:? "numberOfInvocations")
            Prelude.<*> (x Core..:? "lookbackPeriodInDays")
      )

instance
  Prelude.Hashable
    LambdaFunctionRecommendation
  where
  hashWithSalt salt' LambdaFunctionRecommendation' {..} =
    salt' `Prelude.hashWithSalt` lookbackPeriodInDays
      `Prelude.hashWithSalt` numberOfInvocations
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` memorySizeRecommendationOptions
      `Prelude.hashWithSalt` utilizationMetrics
      `Prelude.hashWithSalt` findingReasonCodes
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` lastRefreshTimestamp
      `Prelude.hashWithSalt` currentMemorySize
      `Prelude.hashWithSalt` finding
      `Prelude.hashWithSalt` functionArn

instance Prelude.NFData LambdaFunctionRecommendation where
  rnf LambdaFunctionRecommendation' {..} =
    Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf lookbackPeriodInDays
      `Prelude.seq` Prelude.rnf numberOfInvocations
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf memorySizeRecommendationOptions
      `Prelude.seq` Prelude.rnf utilizationMetrics
      `Prelude.seq` Prelude.rnf findingReasonCodes
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf lastRefreshTimestamp
      `Prelude.seq` Prelude.rnf currentMemorySize
      `Prelude.seq` Prelude.rnf finding

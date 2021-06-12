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
-- Module      : Network.AWS.Config.Types.AggregateEvaluationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateEvaluationResult where

import Network.AWS.Config.Types.ComplianceType
import Network.AWS.Config.Types.EvaluationResultIdentifier
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The details of an AWS Config evaluation for an account ID and region in
-- an aggregator. Provides the AWS resource that was evaluated, the
-- compliance of the resource, related time stamps, and supplementary
-- information.
--
-- /See:/ 'newAggregateEvaluationResult' smart constructor.
data AggregateEvaluationResult = AggregateEvaluationResult'
  { -- | Supplementary information about how the agrregate evaluation determined
    -- the compliance.
    annotation :: Core.Maybe Core.Text,
    -- | Uniquely identifies the evaluation result.
    evaluationResultIdentifier :: Core.Maybe EvaluationResultIdentifier,
    -- | The 12-digit account ID of the source account.
    accountId :: Core.Maybe Core.Text,
    -- | The time when AWS Config recorded the aggregate evaluation result.
    resultRecordedTime :: Core.Maybe Core.POSIX,
    -- | The resource compliance status.
    --
    -- For the @AggregationEvaluationResult@ data type, AWS Config supports
    -- only the @COMPLIANT@ and @NON_COMPLIANT@. AWS Config does not support
    -- the @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ value.
    complianceType :: Core.Maybe ComplianceType,
    -- | The time when the AWS Config rule evaluated the AWS resource.
    configRuleInvokedTime :: Core.Maybe Core.POSIX,
    -- | The source region from where the data is aggregated.
    awsRegion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AggregateEvaluationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotation', 'aggregateEvaluationResult_annotation' - Supplementary information about how the agrregate evaluation determined
-- the compliance.
--
-- 'evaluationResultIdentifier', 'aggregateEvaluationResult_evaluationResultIdentifier' - Uniquely identifies the evaluation result.
--
-- 'accountId', 'aggregateEvaluationResult_accountId' - The 12-digit account ID of the source account.
--
-- 'resultRecordedTime', 'aggregateEvaluationResult_resultRecordedTime' - The time when AWS Config recorded the aggregate evaluation result.
--
-- 'complianceType', 'aggregateEvaluationResult_complianceType' - The resource compliance status.
--
-- For the @AggregationEvaluationResult@ data type, AWS Config supports
-- only the @COMPLIANT@ and @NON_COMPLIANT@. AWS Config does not support
-- the @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ value.
--
-- 'configRuleInvokedTime', 'aggregateEvaluationResult_configRuleInvokedTime' - The time when the AWS Config rule evaluated the AWS resource.
--
-- 'awsRegion', 'aggregateEvaluationResult_awsRegion' - The source region from where the data is aggregated.
newAggregateEvaluationResult ::
  AggregateEvaluationResult
newAggregateEvaluationResult =
  AggregateEvaluationResult'
    { annotation =
        Core.Nothing,
      evaluationResultIdentifier = Core.Nothing,
      accountId = Core.Nothing,
      resultRecordedTime = Core.Nothing,
      complianceType = Core.Nothing,
      configRuleInvokedTime = Core.Nothing,
      awsRegion = Core.Nothing
    }

-- | Supplementary information about how the agrregate evaluation determined
-- the compliance.
aggregateEvaluationResult_annotation :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Core.Text)
aggregateEvaluationResult_annotation = Lens.lens (\AggregateEvaluationResult' {annotation} -> annotation) (\s@AggregateEvaluationResult' {} a -> s {annotation = a} :: AggregateEvaluationResult)

-- | Uniquely identifies the evaluation result.
aggregateEvaluationResult_evaluationResultIdentifier :: Lens.Lens' AggregateEvaluationResult (Core.Maybe EvaluationResultIdentifier)
aggregateEvaluationResult_evaluationResultIdentifier = Lens.lens (\AggregateEvaluationResult' {evaluationResultIdentifier} -> evaluationResultIdentifier) (\s@AggregateEvaluationResult' {} a -> s {evaluationResultIdentifier = a} :: AggregateEvaluationResult)

-- | The 12-digit account ID of the source account.
aggregateEvaluationResult_accountId :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Core.Text)
aggregateEvaluationResult_accountId = Lens.lens (\AggregateEvaluationResult' {accountId} -> accountId) (\s@AggregateEvaluationResult' {} a -> s {accountId = a} :: AggregateEvaluationResult)

-- | The time when AWS Config recorded the aggregate evaluation result.
aggregateEvaluationResult_resultRecordedTime :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Core.UTCTime)
aggregateEvaluationResult_resultRecordedTime = Lens.lens (\AggregateEvaluationResult' {resultRecordedTime} -> resultRecordedTime) (\s@AggregateEvaluationResult' {} a -> s {resultRecordedTime = a} :: AggregateEvaluationResult) Core.. Lens.mapping Core._Time

-- | The resource compliance status.
--
-- For the @AggregationEvaluationResult@ data type, AWS Config supports
-- only the @COMPLIANT@ and @NON_COMPLIANT@. AWS Config does not support
-- the @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ value.
aggregateEvaluationResult_complianceType :: Lens.Lens' AggregateEvaluationResult (Core.Maybe ComplianceType)
aggregateEvaluationResult_complianceType = Lens.lens (\AggregateEvaluationResult' {complianceType} -> complianceType) (\s@AggregateEvaluationResult' {} a -> s {complianceType = a} :: AggregateEvaluationResult)

-- | The time when the AWS Config rule evaluated the AWS resource.
aggregateEvaluationResult_configRuleInvokedTime :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Core.UTCTime)
aggregateEvaluationResult_configRuleInvokedTime = Lens.lens (\AggregateEvaluationResult' {configRuleInvokedTime} -> configRuleInvokedTime) (\s@AggregateEvaluationResult' {} a -> s {configRuleInvokedTime = a} :: AggregateEvaluationResult) Core.. Lens.mapping Core._Time

-- | The source region from where the data is aggregated.
aggregateEvaluationResult_awsRegion :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Core.Text)
aggregateEvaluationResult_awsRegion = Lens.lens (\AggregateEvaluationResult' {awsRegion} -> awsRegion) (\s@AggregateEvaluationResult' {} a -> s {awsRegion = a} :: AggregateEvaluationResult)

instance Core.FromJSON AggregateEvaluationResult where
  parseJSON =
    Core.withObject
      "AggregateEvaluationResult"
      ( \x ->
          AggregateEvaluationResult'
            Core.<$> (x Core..:? "Annotation")
            Core.<*> (x Core..:? "EvaluationResultIdentifier")
            Core.<*> (x Core..:? "AccountId")
            Core.<*> (x Core..:? "ResultRecordedTime")
            Core.<*> (x Core..:? "ComplianceType")
            Core.<*> (x Core..:? "ConfigRuleInvokedTime")
            Core.<*> (x Core..:? "AwsRegion")
      )

instance Core.Hashable AggregateEvaluationResult

instance Core.NFData AggregateEvaluationResult

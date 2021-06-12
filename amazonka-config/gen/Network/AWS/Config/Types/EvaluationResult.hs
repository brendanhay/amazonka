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
-- Module      : Network.AWS.Config.Types.EvaluationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EvaluationResult where

import Network.AWS.Config.Types.ComplianceType
import Network.AWS.Config.Types.EvaluationResultIdentifier
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The details of an AWS Config evaluation. Provides the AWS resource that
-- was evaluated, the compliance of the resource, related time stamps, and
-- supplementary information.
--
-- /See:/ 'newEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | Supplementary information about how the evaluation determined the
    -- compliance.
    annotation :: Core.Maybe Core.Text,
    -- | Uniquely identifies the evaluation result.
    evaluationResultIdentifier :: Core.Maybe EvaluationResultIdentifier,
    -- | The time when AWS Config recorded the evaluation result.
    resultRecordedTime :: Core.Maybe Core.POSIX,
    -- | Indicates whether the AWS resource complies with the AWS Config rule
    -- that evaluated it.
    --
    -- For the @EvaluationResult@ data type, AWS Config supports only the
    -- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. AWS Config
    -- does not support the @INSUFFICIENT_DATA@ value for the
    -- @EvaluationResult@ data type.
    complianceType :: Core.Maybe ComplianceType,
    -- | The time when the AWS Config rule evaluated the AWS resource.
    configRuleInvokedTime :: Core.Maybe Core.POSIX,
    -- | An encrypted token that associates an evaluation with an AWS Config
    -- rule. The token identifies the rule, the AWS resource being evaluated,
    -- and the event that triggered the evaluation.
    resultToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EvaluationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotation', 'evaluationResult_annotation' - Supplementary information about how the evaluation determined the
-- compliance.
--
-- 'evaluationResultIdentifier', 'evaluationResult_evaluationResultIdentifier' - Uniquely identifies the evaluation result.
--
-- 'resultRecordedTime', 'evaluationResult_resultRecordedTime' - The time when AWS Config recorded the evaluation result.
--
-- 'complianceType', 'evaluationResult_complianceType' - Indicates whether the AWS resource complies with the AWS Config rule
-- that evaluated it.
--
-- For the @EvaluationResult@ data type, AWS Config supports only the
-- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. AWS Config
-- does not support the @INSUFFICIENT_DATA@ value for the
-- @EvaluationResult@ data type.
--
-- 'configRuleInvokedTime', 'evaluationResult_configRuleInvokedTime' - The time when the AWS Config rule evaluated the AWS resource.
--
-- 'resultToken', 'evaluationResult_resultToken' - An encrypted token that associates an evaluation with an AWS Config
-- rule. The token identifies the rule, the AWS resource being evaluated,
-- and the event that triggered the evaluation.
newEvaluationResult ::
  EvaluationResult
newEvaluationResult =
  EvaluationResult'
    { annotation = Core.Nothing,
      evaluationResultIdentifier = Core.Nothing,
      resultRecordedTime = Core.Nothing,
      complianceType = Core.Nothing,
      configRuleInvokedTime = Core.Nothing,
      resultToken = Core.Nothing
    }

-- | Supplementary information about how the evaluation determined the
-- compliance.
evaluationResult_annotation :: Lens.Lens' EvaluationResult (Core.Maybe Core.Text)
evaluationResult_annotation = Lens.lens (\EvaluationResult' {annotation} -> annotation) (\s@EvaluationResult' {} a -> s {annotation = a} :: EvaluationResult)

-- | Uniquely identifies the evaluation result.
evaluationResult_evaluationResultIdentifier :: Lens.Lens' EvaluationResult (Core.Maybe EvaluationResultIdentifier)
evaluationResult_evaluationResultIdentifier = Lens.lens (\EvaluationResult' {evaluationResultIdentifier} -> evaluationResultIdentifier) (\s@EvaluationResult' {} a -> s {evaluationResultIdentifier = a} :: EvaluationResult)

-- | The time when AWS Config recorded the evaluation result.
evaluationResult_resultRecordedTime :: Lens.Lens' EvaluationResult (Core.Maybe Core.UTCTime)
evaluationResult_resultRecordedTime = Lens.lens (\EvaluationResult' {resultRecordedTime} -> resultRecordedTime) (\s@EvaluationResult' {} a -> s {resultRecordedTime = a} :: EvaluationResult) Core.. Lens.mapping Core._Time

-- | Indicates whether the AWS resource complies with the AWS Config rule
-- that evaluated it.
--
-- For the @EvaluationResult@ data type, AWS Config supports only the
-- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. AWS Config
-- does not support the @INSUFFICIENT_DATA@ value for the
-- @EvaluationResult@ data type.
evaluationResult_complianceType :: Lens.Lens' EvaluationResult (Core.Maybe ComplianceType)
evaluationResult_complianceType = Lens.lens (\EvaluationResult' {complianceType} -> complianceType) (\s@EvaluationResult' {} a -> s {complianceType = a} :: EvaluationResult)

-- | The time when the AWS Config rule evaluated the AWS resource.
evaluationResult_configRuleInvokedTime :: Lens.Lens' EvaluationResult (Core.Maybe Core.UTCTime)
evaluationResult_configRuleInvokedTime = Lens.lens (\EvaluationResult' {configRuleInvokedTime} -> configRuleInvokedTime) (\s@EvaluationResult' {} a -> s {configRuleInvokedTime = a} :: EvaluationResult) Core.. Lens.mapping Core._Time

-- | An encrypted token that associates an evaluation with an AWS Config
-- rule. The token identifies the rule, the AWS resource being evaluated,
-- and the event that triggered the evaluation.
evaluationResult_resultToken :: Lens.Lens' EvaluationResult (Core.Maybe Core.Text)
evaluationResult_resultToken = Lens.lens (\EvaluationResult' {resultToken} -> resultToken) (\s@EvaluationResult' {} a -> s {resultToken = a} :: EvaluationResult)

instance Core.FromJSON EvaluationResult where
  parseJSON =
    Core.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Core.<$> (x Core..:? "Annotation")
            Core.<*> (x Core..:? "EvaluationResultIdentifier")
            Core.<*> (x Core..:? "ResultRecordedTime")
            Core.<*> (x Core..:? "ComplianceType")
            Core.<*> (x Core..:? "ConfigRuleInvokedTime")
            Core.<*> (x Core..:? "ResultToken")
      )

instance Core.Hashable EvaluationResult

instance Core.NFData EvaluationResult

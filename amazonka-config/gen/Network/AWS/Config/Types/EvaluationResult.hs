{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of an AWS Config evaluation. Provides the AWS resource that
-- was evaluated, the compliance of the resource, related time stamps, and
-- supplementary information.
--
-- /See:/ 'newEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | Supplementary information about how the evaluation determined the
    -- compliance.
    annotation :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies the evaluation result.
    evaluationResultIdentifier :: Prelude.Maybe EvaluationResultIdentifier,
    -- | The time when AWS Config recorded the evaluation result.
    resultRecordedTime :: Prelude.Maybe Prelude.POSIX,
    -- | Indicates whether the AWS resource complies with the AWS Config rule
    -- that evaluated it.
    --
    -- For the @EvaluationResult@ data type, AWS Config supports only the
    -- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. AWS Config
    -- does not support the @INSUFFICIENT_DATA@ value for the
    -- @EvaluationResult@ data type.
    complianceType :: Prelude.Maybe ComplianceType,
    -- | The time when the AWS Config rule evaluated the AWS resource.
    configRuleInvokedTime :: Prelude.Maybe Prelude.POSIX,
    -- | An encrypted token that associates an evaluation with an AWS Config
    -- rule. The token identifies the rule, the AWS resource being evaluated,
    -- and the event that triggered the evaluation.
    resultToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { annotation = Prelude.Nothing,
      evaluationResultIdentifier = Prelude.Nothing,
      resultRecordedTime = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      configRuleInvokedTime = Prelude.Nothing,
      resultToken = Prelude.Nothing
    }

-- | Supplementary information about how the evaluation determined the
-- compliance.
evaluationResult_annotation :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Text)
evaluationResult_annotation = Lens.lens (\EvaluationResult' {annotation} -> annotation) (\s@EvaluationResult' {} a -> s {annotation = a} :: EvaluationResult)

-- | Uniquely identifies the evaluation result.
evaluationResult_evaluationResultIdentifier :: Lens.Lens' EvaluationResult (Prelude.Maybe EvaluationResultIdentifier)
evaluationResult_evaluationResultIdentifier = Lens.lens (\EvaluationResult' {evaluationResultIdentifier} -> evaluationResultIdentifier) (\s@EvaluationResult' {} a -> s {evaluationResultIdentifier = a} :: EvaluationResult)

-- | The time when AWS Config recorded the evaluation result.
evaluationResult_resultRecordedTime :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.UTCTime)
evaluationResult_resultRecordedTime = Lens.lens (\EvaluationResult' {resultRecordedTime} -> resultRecordedTime) (\s@EvaluationResult' {} a -> s {resultRecordedTime = a} :: EvaluationResult) Prelude.. Lens.mapping Prelude._Time

-- | Indicates whether the AWS resource complies with the AWS Config rule
-- that evaluated it.
--
-- For the @EvaluationResult@ data type, AWS Config supports only the
-- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. AWS Config
-- does not support the @INSUFFICIENT_DATA@ value for the
-- @EvaluationResult@ data type.
evaluationResult_complianceType :: Lens.Lens' EvaluationResult (Prelude.Maybe ComplianceType)
evaluationResult_complianceType = Lens.lens (\EvaluationResult' {complianceType} -> complianceType) (\s@EvaluationResult' {} a -> s {complianceType = a} :: EvaluationResult)

-- | The time when the AWS Config rule evaluated the AWS resource.
evaluationResult_configRuleInvokedTime :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.UTCTime)
evaluationResult_configRuleInvokedTime = Lens.lens (\EvaluationResult' {configRuleInvokedTime} -> configRuleInvokedTime) (\s@EvaluationResult' {} a -> s {configRuleInvokedTime = a} :: EvaluationResult) Prelude.. Lens.mapping Prelude._Time

-- | An encrypted token that associates an evaluation with an AWS Config
-- rule. The token identifies the rule, the AWS resource being evaluated,
-- and the event that triggered the evaluation.
evaluationResult_resultToken :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Text)
evaluationResult_resultToken = Lens.lens (\EvaluationResult' {resultToken} -> resultToken) (\s@EvaluationResult' {} a -> s {resultToken = a} :: EvaluationResult)

instance Prelude.FromJSON EvaluationResult where
  parseJSON =
    Prelude.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Prelude.<$> (x Prelude..:? "Annotation")
            Prelude.<*> (x Prelude..:? "EvaluationResultIdentifier")
            Prelude.<*> (x Prelude..:? "ResultRecordedTime")
            Prelude.<*> (x Prelude..:? "ComplianceType")
            Prelude.<*> (x Prelude..:? "ConfigRuleInvokedTime")
            Prelude.<*> (x Prelude..:? "ResultToken")
      )

instance Prelude.Hashable EvaluationResult

instance Prelude.NFData EvaluationResult

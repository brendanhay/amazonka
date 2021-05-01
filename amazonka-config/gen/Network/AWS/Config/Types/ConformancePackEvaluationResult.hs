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
-- Module      : Network.AWS.Config.Types.ConformancePackEvaluationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackEvaluationResult where

import Network.AWS.Config.Types.ConformancePackComplianceType
import Network.AWS.Config.Types.EvaluationResultIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of a conformance pack evaluation. Provides AWS Config rule
-- and AWS resource type that was evaluated, the compliance of the
-- conformance pack, related time stamps, and supplementary information.
--
-- /See:/ 'newConformancePackEvaluationResult' smart constructor.
data ConformancePackEvaluationResult = ConformancePackEvaluationResult'
  { -- | Supplementary information about how the evaluation determined the
    -- compliance.
    annotation :: Prelude.Maybe Prelude.Text,
    -- | The compliance type. The allowed values are @COMPLIANT@ and
    -- @NON_COMPLIANT@.
    complianceType :: ConformancePackComplianceType,
    evaluationResultIdentifier :: EvaluationResultIdentifier,
    -- | The time when AWS Config rule evaluated AWS resource.
    configRuleInvokedTime :: Prelude.POSIX,
    -- | The time when AWS Config recorded the evaluation result.
    resultRecordedTime :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConformancePackEvaluationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotation', 'conformancePackEvaluationResult_annotation' - Supplementary information about how the evaluation determined the
-- compliance.
--
-- 'complianceType', 'conformancePackEvaluationResult_complianceType' - The compliance type. The allowed values are @COMPLIANT@ and
-- @NON_COMPLIANT@.
--
-- 'evaluationResultIdentifier', 'conformancePackEvaluationResult_evaluationResultIdentifier' - Undocumented member.
--
-- 'configRuleInvokedTime', 'conformancePackEvaluationResult_configRuleInvokedTime' - The time when AWS Config rule evaluated AWS resource.
--
-- 'resultRecordedTime', 'conformancePackEvaluationResult_resultRecordedTime' - The time when AWS Config recorded the evaluation result.
newConformancePackEvaluationResult ::
  -- | 'complianceType'
  ConformancePackComplianceType ->
  -- | 'evaluationResultIdentifier'
  EvaluationResultIdentifier ->
  -- | 'configRuleInvokedTime'
  Prelude.UTCTime ->
  -- | 'resultRecordedTime'
  Prelude.UTCTime ->
  ConformancePackEvaluationResult
newConformancePackEvaluationResult
  pComplianceType_
  pEvaluationResultIdentifier_
  pConfigRuleInvokedTime_
  pResultRecordedTime_ =
    ConformancePackEvaluationResult'
      { annotation =
          Prelude.Nothing,
        complianceType = pComplianceType_,
        evaluationResultIdentifier =
          pEvaluationResultIdentifier_,
        configRuleInvokedTime =
          Prelude._Time
            Lens.# pConfigRuleInvokedTime_,
        resultRecordedTime =
          Prelude._Time
            Lens.# pResultRecordedTime_
      }

-- | Supplementary information about how the evaluation determined the
-- compliance.
conformancePackEvaluationResult_annotation :: Lens.Lens' ConformancePackEvaluationResult (Prelude.Maybe Prelude.Text)
conformancePackEvaluationResult_annotation = Lens.lens (\ConformancePackEvaluationResult' {annotation} -> annotation) (\s@ConformancePackEvaluationResult' {} a -> s {annotation = a} :: ConformancePackEvaluationResult)

-- | The compliance type. The allowed values are @COMPLIANT@ and
-- @NON_COMPLIANT@.
conformancePackEvaluationResult_complianceType :: Lens.Lens' ConformancePackEvaluationResult ConformancePackComplianceType
conformancePackEvaluationResult_complianceType = Lens.lens (\ConformancePackEvaluationResult' {complianceType} -> complianceType) (\s@ConformancePackEvaluationResult' {} a -> s {complianceType = a} :: ConformancePackEvaluationResult)

-- | Undocumented member.
conformancePackEvaluationResult_evaluationResultIdentifier :: Lens.Lens' ConformancePackEvaluationResult EvaluationResultIdentifier
conformancePackEvaluationResult_evaluationResultIdentifier = Lens.lens (\ConformancePackEvaluationResult' {evaluationResultIdentifier} -> evaluationResultIdentifier) (\s@ConformancePackEvaluationResult' {} a -> s {evaluationResultIdentifier = a} :: ConformancePackEvaluationResult)

-- | The time when AWS Config rule evaluated AWS resource.
conformancePackEvaluationResult_configRuleInvokedTime :: Lens.Lens' ConformancePackEvaluationResult Prelude.UTCTime
conformancePackEvaluationResult_configRuleInvokedTime = Lens.lens (\ConformancePackEvaluationResult' {configRuleInvokedTime} -> configRuleInvokedTime) (\s@ConformancePackEvaluationResult' {} a -> s {configRuleInvokedTime = a} :: ConformancePackEvaluationResult) Prelude.. Prelude._Time

-- | The time when AWS Config recorded the evaluation result.
conformancePackEvaluationResult_resultRecordedTime :: Lens.Lens' ConformancePackEvaluationResult Prelude.UTCTime
conformancePackEvaluationResult_resultRecordedTime = Lens.lens (\ConformancePackEvaluationResult' {resultRecordedTime} -> resultRecordedTime) (\s@ConformancePackEvaluationResult' {} a -> s {resultRecordedTime = a} :: ConformancePackEvaluationResult) Prelude.. Prelude._Time

instance
  Prelude.FromJSON
    ConformancePackEvaluationResult
  where
  parseJSON =
    Prelude.withObject
      "ConformancePackEvaluationResult"
      ( \x ->
          ConformancePackEvaluationResult'
            Prelude.<$> (x Prelude..:? "Annotation")
            Prelude.<*> (x Prelude..: "ComplianceType")
            Prelude.<*> (x Prelude..: "EvaluationResultIdentifier")
            Prelude.<*> (x Prelude..: "ConfigRuleInvokedTime")
            Prelude.<*> (x Prelude..: "ResultRecordedTime")
      )

instance
  Prelude.Hashable
    ConformancePackEvaluationResult

instance
  Prelude.NFData
    ConformancePackEvaluationResult

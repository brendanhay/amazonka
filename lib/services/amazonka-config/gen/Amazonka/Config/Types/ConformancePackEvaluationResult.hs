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
-- Module      : Amazonka.Config.Types.ConformancePackEvaluationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConformancePackEvaluationResult where

import Amazonka.Config.Types.ConformancePackComplianceType
import Amazonka.Config.Types.EvaluationResultIdentifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a conformance pack evaluation. Provides Config rule and
-- Amazon Web Services resource type that was evaluated, the compliance of
-- the conformance pack, related time stamps, and supplementary
-- information.
--
-- /See:/ 'newConformancePackEvaluationResult' smart constructor.
data ConformancePackEvaluationResult = ConformancePackEvaluationResult'
  { -- | Supplementary information about how the evaluation determined the
    -- compliance.
    annotation :: Prelude.Maybe Prelude.Text,
    -- | The compliance type. The allowed values are @COMPLIANT@ and
    -- @NON_COMPLIANT@. @INSUFFICIENT_DATA@ is not supported.
    complianceType :: ConformancePackComplianceType,
    evaluationResultIdentifier :: EvaluationResultIdentifier,
    -- | The time when Config rule evaluated Amazon Web Services resource.
    configRuleInvokedTime :: Data.POSIX,
    -- | The time when Config recorded the evaluation result.
    resultRecordedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- @NON_COMPLIANT@. @INSUFFICIENT_DATA@ is not supported.
--
-- 'evaluationResultIdentifier', 'conformancePackEvaluationResult_evaluationResultIdentifier' - Undocumented member.
--
-- 'configRuleInvokedTime', 'conformancePackEvaluationResult_configRuleInvokedTime' - The time when Config rule evaluated Amazon Web Services resource.
--
-- 'resultRecordedTime', 'conformancePackEvaluationResult_resultRecordedTime' - The time when Config recorded the evaluation result.
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
          Data._Time
            Lens.# pConfigRuleInvokedTime_,
        resultRecordedTime =
          Data._Time Lens.# pResultRecordedTime_
      }

-- | Supplementary information about how the evaluation determined the
-- compliance.
conformancePackEvaluationResult_annotation :: Lens.Lens' ConformancePackEvaluationResult (Prelude.Maybe Prelude.Text)
conformancePackEvaluationResult_annotation = Lens.lens (\ConformancePackEvaluationResult' {annotation} -> annotation) (\s@ConformancePackEvaluationResult' {} a -> s {annotation = a} :: ConformancePackEvaluationResult)

-- | The compliance type. The allowed values are @COMPLIANT@ and
-- @NON_COMPLIANT@. @INSUFFICIENT_DATA@ is not supported.
conformancePackEvaluationResult_complianceType :: Lens.Lens' ConformancePackEvaluationResult ConformancePackComplianceType
conformancePackEvaluationResult_complianceType = Lens.lens (\ConformancePackEvaluationResult' {complianceType} -> complianceType) (\s@ConformancePackEvaluationResult' {} a -> s {complianceType = a} :: ConformancePackEvaluationResult)

-- | Undocumented member.
conformancePackEvaluationResult_evaluationResultIdentifier :: Lens.Lens' ConformancePackEvaluationResult EvaluationResultIdentifier
conformancePackEvaluationResult_evaluationResultIdentifier = Lens.lens (\ConformancePackEvaluationResult' {evaluationResultIdentifier} -> evaluationResultIdentifier) (\s@ConformancePackEvaluationResult' {} a -> s {evaluationResultIdentifier = a} :: ConformancePackEvaluationResult)

-- | The time when Config rule evaluated Amazon Web Services resource.
conformancePackEvaluationResult_configRuleInvokedTime :: Lens.Lens' ConformancePackEvaluationResult Prelude.UTCTime
conformancePackEvaluationResult_configRuleInvokedTime = Lens.lens (\ConformancePackEvaluationResult' {configRuleInvokedTime} -> configRuleInvokedTime) (\s@ConformancePackEvaluationResult' {} a -> s {configRuleInvokedTime = a} :: ConformancePackEvaluationResult) Prelude.. Data._Time

-- | The time when Config recorded the evaluation result.
conformancePackEvaluationResult_resultRecordedTime :: Lens.Lens' ConformancePackEvaluationResult Prelude.UTCTime
conformancePackEvaluationResult_resultRecordedTime = Lens.lens (\ConformancePackEvaluationResult' {resultRecordedTime} -> resultRecordedTime) (\s@ConformancePackEvaluationResult' {} a -> s {resultRecordedTime = a} :: ConformancePackEvaluationResult) Prelude.. Data._Time

instance
  Data.FromJSON
    ConformancePackEvaluationResult
  where
  parseJSON =
    Data.withObject
      "ConformancePackEvaluationResult"
      ( \x ->
          ConformancePackEvaluationResult'
            Prelude.<$> (x Data..:? "Annotation")
            Prelude.<*> (x Data..: "ComplianceType")
            Prelude.<*> (x Data..: "EvaluationResultIdentifier")
            Prelude.<*> (x Data..: "ConfigRuleInvokedTime")
            Prelude.<*> (x Data..: "ResultRecordedTime")
      )

instance
  Prelude.Hashable
    ConformancePackEvaluationResult
  where
  hashWithSalt
    _salt
    ConformancePackEvaluationResult' {..} =
      _salt
        `Prelude.hashWithSalt` annotation
        `Prelude.hashWithSalt` complianceType
        `Prelude.hashWithSalt` evaluationResultIdentifier
        `Prelude.hashWithSalt` configRuleInvokedTime
        `Prelude.hashWithSalt` resultRecordedTime

instance
  Prelude.NFData
    ConformancePackEvaluationResult
  where
  rnf ConformancePackEvaluationResult' {..} =
    Prelude.rnf annotation
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf evaluationResultIdentifier
      `Prelude.seq` Prelude.rnf configRuleInvokedTime
      `Prelude.seq` Prelude.rnf resultRecordedTime

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
-- Module      : Amazonka.Config.Types.AggregateEvaluationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregateEvaluationResult where

import Amazonka.Config.Types.ComplianceType
import Amazonka.Config.Types.EvaluationResultIdentifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of an Config evaluation for an account ID and region in an
-- aggregator. Provides the Amazon Web Services resource that was
-- evaluated, the compliance of the resource, related time stamps, and
-- supplementary information.
--
-- /See:/ 'newAggregateEvaluationResult' smart constructor.
data AggregateEvaluationResult = AggregateEvaluationResult'
  { -- | The 12-digit account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Supplementary information about how the agrregate evaluation determined
    -- the compliance.
    annotation :: Prelude.Maybe Prelude.Text,
    -- | The source region from where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The resource compliance status.
    --
    -- For the @AggregationEvaluationResult@ data type, Config supports only
    -- the @COMPLIANT@ and @NON_COMPLIANT@. Config does not support the
    -- @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ value.
    complianceType :: Prelude.Maybe ComplianceType,
    -- | The time when the Config rule evaluated the Amazon Web Services
    -- resource.
    configRuleInvokedTime :: Prelude.Maybe Data.POSIX,
    -- | Uniquely identifies the evaluation result.
    evaluationResultIdentifier :: Prelude.Maybe EvaluationResultIdentifier,
    -- | The time when Config recorded the aggregate evaluation result.
    resultRecordedTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateEvaluationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'aggregateEvaluationResult_accountId' - The 12-digit account ID of the source account.
--
-- 'annotation', 'aggregateEvaluationResult_annotation' - Supplementary information about how the agrregate evaluation determined
-- the compliance.
--
-- 'awsRegion', 'aggregateEvaluationResult_awsRegion' - The source region from where the data is aggregated.
--
-- 'complianceType', 'aggregateEvaluationResult_complianceType' - The resource compliance status.
--
-- For the @AggregationEvaluationResult@ data type, Config supports only
-- the @COMPLIANT@ and @NON_COMPLIANT@. Config does not support the
-- @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ value.
--
-- 'configRuleInvokedTime', 'aggregateEvaluationResult_configRuleInvokedTime' - The time when the Config rule evaluated the Amazon Web Services
-- resource.
--
-- 'evaluationResultIdentifier', 'aggregateEvaluationResult_evaluationResultIdentifier' - Uniquely identifies the evaluation result.
--
-- 'resultRecordedTime', 'aggregateEvaluationResult_resultRecordedTime' - The time when Config recorded the aggregate evaluation result.
newAggregateEvaluationResult ::
  AggregateEvaluationResult
newAggregateEvaluationResult =
  AggregateEvaluationResult'
    { accountId =
        Prelude.Nothing,
      annotation = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      configRuleInvokedTime = Prelude.Nothing,
      evaluationResultIdentifier = Prelude.Nothing,
      resultRecordedTime = Prelude.Nothing
    }

-- | The 12-digit account ID of the source account.
aggregateEvaluationResult_accountId :: Lens.Lens' AggregateEvaluationResult (Prelude.Maybe Prelude.Text)
aggregateEvaluationResult_accountId = Lens.lens (\AggregateEvaluationResult' {accountId} -> accountId) (\s@AggregateEvaluationResult' {} a -> s {accountId = a} :: AggregateEvaluationResult)

-- | Supplementary information about how the agrregate evaluation determined
-- the compliance.
aggregateEvaluationResult_annotation :: Lens.Lens' AggregateEvaluationResult (Prelude.Maybe Prelude.Text)
aggregateEvaluationResult_annotation = Lens.lens (\AggregateEvaluationResult' {annotation} -> annotation) (\s@AggregateEvaluationResult' {} a -> s {annotation = a} :: AggregateEvaluationResult)

-- | The source region from where the data is aggregated.
aggregateEvaluationResult_awsRegion :: Lens.Lens' AggregateEvaluationResult (Prelude.Maybe Prelude.Text)
aggregateEvaluationResult_awsRegion = Lens.lens (\AggregateEvaluationResult' {awsRegion} -> awsRegion) (\s@AggregateEvaluationResult' {} a -> s {awsRegion = a} :: AggregateEvaluationResult)

-- | The resource compliance status.
--
-- For the @AggregationEvaluationResult@ data type, Config supports only
-- the @COMPLIANT@ and @NON_COMPLIANT@. Config does not support the
-- @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ value.
aggregateEvaluationResult_complianceType :: Lens.Lens' AggregateEvaluationResult (Prelude.Maybe ComplianceType)
aggregateEvaluationResult_complianceType = Lens.lens (\AggregateEvaluationResult' {complianceType} -> complianceType) (\s@AggregateEvaluationResult' {} a -> s {complianceType = a} :: AggregateEvaluationResult)

-- | The time when the Config rule evaluated the Amazon Web Services
-- resource.
aggregateEvaluationResult_configRuleInvokedTime :: Lens.Lens' AggregateEvaluationResult (Prelude.Maybe Prelude.UTCTime)
aggregateEvaluationResult_configRuleInvokedTime = Lens.lens (\AggregateEvaluationResult' {configRuleInvokedTime} -> configRuleInvokedTime) (\s@AggregateEvaluationResult' {} a -> s {configRuleInvokedTime = a} :: AggregateEvaluationResult) Prelude.. Lens.mapping Data._Time

-- | Uniquely identifies the evaluation result.
aggregateEvaluationResult_evaluationResultIdentifier :: Lens.Lens' AggregateEvaluationResult (Prelude.Maybe EvaluationResultIdentifier)
aggregateEvaluationResult_evaluationResultIdentifier = Lens.lens (\AggregateEvaluationResult' {evaluationResultIdentifier} -> evaluationResultIdentifier) (\s@AggregateEvaluationResult' {} a -> s {evaluationResultIdentifier = a} :: AggregateEvaluationResult)

-- | The time when Config recorded the aggregate evaluation result.
aggregateEvaluationResult_resultRecordedTime :: Lens.Lens' AggregateEvaluationResult (Prelude.Maybe Prelude.UTCTime)
aggregateEvaluationResult_resultRecordedTime = Lens.lens (\AggregateEvaluationResult' {resultRecordedTime} -> resultRecordedTime) (\s@AggregateEvaluationResult' {} a -> s {resultRecordedTime = a} :: AggregateEvaluationResult) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AggregateEvaluationResult where
  parseJSON =
    Data.withObject
      "AggregateEvaluationResult"
      ( \x ->
          AggregateEvaluationResult'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "Annotation")
            Prelude.<*> (x Data..:? "AwsRegion")
            Prelude.<*> (x Data..:? "ComplianceType")
            Prelude.<*> (x Data..:? "ConfigRuleInvokedTime")
            Prelude.<*> (x Data..:? "EvaluationResultIdentifier")
            Prelude.<*> (x Data..:? "ResultRecordedTime")
      )

instance Prelude.Hashable AggregateEvaluationResult where
  hashWithSalt _salt AggregateEvaluationResult' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` annotation
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` configRuleInvokedTime
      `Prelude.hashWithSalt` evaluationResultIdentifier
      `Prelude.hashWithSalt` resultRecordedTime

instance Prelude.NFData AggregateEvaluationResult where
  rnf AggregateEvaluationResult' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf annotation `Prelude.seq`
        Prelude.rnf awsRegion `Prelude.seq`
          Prelude.rnf complianceType `Prelude.seq`
            Prelude.rnf configRuleInvokedTime `Prelude.seq`
              Prelude.rnf evaluationResultIdentifier `Prelude.seq`
                Prelude.rnf resultRecordedTime

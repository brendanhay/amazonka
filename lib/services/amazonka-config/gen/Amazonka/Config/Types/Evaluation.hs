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
-- Module      : Amazonka.Config.Types.Evaluation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.Evaluation where

import Amazonka.Config.Types.ComplianceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Identifies an Amazon Web Services resource and indicates whether it
-- complies with the Config rule that it was evaluated against.
--
-- /See:/ 'newEvaluation' smart constructor.
data Evaluation = Evaluation'
  { -- | Supplementary information about how the evaluation determined the
    -- compliance.
    annotation :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource that was evaluated.
    complianceResourceType :: Prelude.Text,
    -- | The ID of the Amazon Web Services resource that was evaluated.
    complianceResourceId :: Prelude.Text,
    -- | Indicates whether the Amazon Web Services resource complies with the
    -- Config rule that it was evaluated against.
    --
    -- For the @Evaluation@ data type, Config supports only the @COMPLIANT@,
    -- @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. Config does not support
    -- the @INSUFFICIENT_DATA@ value for this data type.
    --
    -- Similarly, Config does not accept @INSUFFICIENT_DATA@ as the value for
    -- @ComplianceType@ from a @PutEvaluations@ request. For example, an Lambda
    -- function for a custom Config rule cannot pass an @INSUFFICIENT_DATA@
    -- value to Config.
    complianceType :: ComplianceType,
    -- | The time of the event in Config that triggered the evaluation. For
    -- event-based evaluations, the time indicates when Config created the
    -- configuration item that triggered the evaluation. For periodic
    -- evaluations, the time indicates when Config triggered the evaluation at
    -- the frequency that you specified (for example, every 24 hours).
    orderingTimestamp :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Evaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotation', 'evaluation_annotation' - Supplementary information about how the evaluation determined the
-- compliance.
--
-- 'complianceResourceType', 'evaluation_complianceResourceType' - The type of Amazon Web Services resource that was evaluated.
--
-- 'complianceResourceId', 'evaluation_complianceResourceId' - The ID of the Amazon Web Services resource that was evaluated.
--
-- 'complianceType', 'evaluation_complianceType' - Indicates whether the Amazon Web Services resource complies with the
-- Config rule that it was evaluated against.
--
-- For the @Evaluation@ data type, Config supports only the @COMPLIANT@,
-- @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. Config does not support
-- the @INSUFFICIENT_DATA@ value for this data type.
--
-- Similarly, Config does not accept @INSUFFICIENT_DATA@ as the value for
-- @ComplianceType@ from a @PutEvaluations@ request. For example, an Lambda
-- function for a custom Config rule cannot pass an @INSUFFICIENT_DATA@
-- value to Config.
--
-- 'orderingTimestamp', 'evaluation_orderingTimestamp' - The time of the event in Config that triggered the evaluation. For
-- event-based evaluations, the time indicates when Config created the
-- configuration item that triggered the evaluation. For periodic
-- evaluations, the time indicates when Config triggered the evaluation at
-- the frequency that you specified (for example, every 24 hours).
newEvaluation ::
  -- | 'complianceResourceType'
  Prelude.Text ->
  -- | 'complianceResourceId'
  Prelude.Text ->
  -- | 'complianceType'
  ComplianceType ->
  -- | 'orderingTimestamp'
  Prelude.UTCTime ->
  Evaluation
newEvaluation
  pComplianceResourceType_
  pComplianceResourceId_
  pComplianceType_
  pOrderingTimestamp_ =
    Evaluation'
      { annotation = Prelude.Nothing,
        complianceResourceType = pComplianceResourceType_,
        complianceResourceId = pComplianceResourceId_,
        complianceType = pComplianceType_,
        orderingTimestamp =
          Core._Time Lens.# pOrderingTimestamp_
      }

-- | Supplementary information about how the evaluation determined the
-- compliance.
evaluation_annotation :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Text)
evaluation_annotation = Lens.lens (\Evaluation' {annotation} -> annotation) (\s@Evaluation' {} a -> s {annotation = a} :: Evaluation)

-- | The type of Amazon Web Services resource that was evaluated.
evaluation_complianceResourceType :: Lens.Lens' Evaluation Prelude.Text
evaluation_complianceResourceType = Lens.lens (\Evaluation' {complianceResourceType} -> complianceResourceType) (\s@Evaluation' {} a -> s {complianceResourceType = a} :: Evaluation)

-- | The ID of the Amazon Web Services resource that was evaluated.
evaluation_complianceResourceId :: Lens.Lens' Evaluation Prelude.Text
evaluation_complianceResourceId = Lens.lens (\Evaluation' {complianceResourceId} -> complianceResourceId) (\s@Evaluation' {} a -> s {complianceResourceId = a} :: Evaluation)

-- | Indicates whether the Amazon Web Services resource complies with the
-- Config rule that it was evaluated against.
--
-- For the @Evaluation@ data type, Config supports only the @COMPLIANT@,
-- @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. Config does not support
-- the @INSUFFICIENT_DATA@ value for this data type.
--
-- Similarly, Config does not accept @INSUFFICIENT_DATA@ as the value for
-- @ComplianceType@ from a @PutEvaluations@ request. For example, an Lambda
-- function for a custom Config rule cannot pass an @INSUFFICIENT_DATA@
-- value to Config.
evaluation_complianceType :: Lens.Lens' Evaluation ComplianceType
evaluation_complianceType = Lens.lens (\Evaluation' {complianceType} -> complianceType) (\s@Evaluation' {} a -> s {complianceType = a} :: Evaluation)

-- | The time of the event in Config that triggered the evaluation. For
-- event-based evaluations, the time indicates when Config created the
-- configuration item that triggered the evaluation. For periodic
-- evaluations, the time indicates when Config triggered the evaluation at
-- the frequency that you specified (for example, every 24 hours).
evaluation_orderingTimestamp :: Lens.Lens' Evaluation Prelude.UTCTime
evaluation_orderingTimestamp = Lens.lens (\Evaluation' {orderingTimestamp} -> orderingTimestamp) (\s@Evaluation' {} a -> s {orderingTimestamp = a} :: Evaluation) Prelude.. Core._Time

instance Core.FromJSON Evaluation where
  parseJSON =
    Core.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Prelude.<$> (x Core..:? "Annotation")
            Prelude.<*> (x Core..: "ComplianceResourceType")
            Prelude.<*> (x Core..: "ComplianceResourceId")
            Prelude.<*> (x Core..: "ComplianceType")
            Prelude.<*> (x Core..: "OrderingTimestamp")
      )

instance Prelude.Hashable Evaluation where
  hashWithSalt _salt Evaluation' {..} =
    _salt `Prelude.hashWithSalt` annotation
      `Prelude.hashWithSalt` complianceResourceType
      `Prelude.hashWithSalt` complianceResourceId
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` orderingTimestamp

instance Prelude.NFData Evaluation where
  rnf Evaluation' {..} =
    Prelude.rnf annotation
      `Prelude.seq` Prelude.rnf complianceResourceType
      `Prelude.seq` Prelude.rnf complianceResourceId
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf orderingTimestamp

instance Core.ToJSON Evaluation where
  toJSON Evaluation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Annotation" Core..=) Prelude.<$> annotation,
            Prelude.Just
              ( "ComplianceResourceType"
                  Core..= complianceResourceType
              ),
            Prelude.Just
              ( "ComplianceResourceId"
                  Core..= complianceResourceId
              ),
            Prelude.Just
              ("ComplianceType" Core..= complianceType),
            Prelude.Just
              ("OrderingTimestamp" Core..= orderingTimestamp)
          ]
      )

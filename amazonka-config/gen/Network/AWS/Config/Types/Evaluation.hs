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
-- Module      : Network.AWS.Config.Types.Evaluation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Evaluation where

import Network.AWS.Config.Types.ComplianceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identifies an AWS resource and indicates whether it complies with the
-- AWS Config rule that it was evaluated against.
--
-- /See:/ 'newEvaluation' smart constructor.
data Evaluation = Evaluation'
  { -- | Supplementary information about how the evaluation determined the
    -- compliance.
    annotation :: Core.Maybe Core.Text,
    -- | The type of AWS resource that was evaluated.
    complianceResourceType :: Core.Text,
    -- | The ID of the AWS resource that was evaluated.
    complianceResourceId :: Core.Text,
    -- | Indicates whether the AWS resource complies with the AWS Config rule
    -- that it was evaluated against.
    --
    -- For the @Evaluation@ data type, AWS Config supports only the
    -- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. AWS Config
    -- does not support the @INSUFFICIENT_DATA@ value for this data type.
    --
    -- Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value
    -- for @ComplianceType@ from a @PutEvaluations@ request. For example, an
    -- AWS Lambda function for a custom AWS Config rule cannot pass an
    -- @INSUFFICIENT_DATA@ value to AWS Config.
    complianceType :: ComplianceType,
    -- | The time of the event in AWS Config that triggered the evaluation. For
    -- event-based evaluations, the time indicates when AWS Config created the
    -- configuration item that triggered the evaluation. For periodic
    -- evaluations, the time indicates when AWS Config triggered the evaluation
    -- at the frequency that you specified (for example, every 24 hours).
    orderingTimestamp :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'complianceResourceType', 'evaluation_complianceResourceType' - The type of AWS resource that was evaluated.
--
-- 'complianceResourceId', 'evaluation_complianceResourceId' - The ID of the AWS resource that was evaluated.
--
-- 'complianceType', 'evaluation_complianceType' - Indicates whether the AWS resource complies with the AWS Config rule
-- that it was evaluated against.
--
-- For the @Evaluation@ data type, AWS Config supports only the
-- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. AWS Config
-- does not support the @INSUFFICIENT_DATA@ value for this data type.
--
-- Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value
-- for @ComplianceType@ from a @PutEvaluations@ request. For example, an
-- AWS Lambda function for a custom AWS Config rule cannot pass an
-- @INSUFFICIENT_DATA@ value to AWS Config.
--
-- 'orderingTimestamp', 'evaluation_orderingTimestamp' - The time of the event in AWS Config that triggered the evaluation. For
-- event-based evaluations, the time indicates when AWS Config created the
-- configuration item that triggered the evaluation. For periodic
-- evaluations, the time indicates when AWS Config triggered the evaluation
-- at the frequency that you specified (for example, every 24 hours).
newEvaluation ::
  -- | 'complianceResourceType'
  Core.Text ->
  -- | 'complianceResourceId'
  Core.Text ->
  -- | 'complianceType'
  ComplianceType ->
  -- | 'orderingTimestamp'
  Core.UTCTime ->
  Evaluation
newEvaluation
  pComplianceResourceType_
  pComplianceResourceId_
  pComplianceType_
  pOrderingTimestamp_ =
    Evaluation'
      { annotation = Core.Nothing,
        complianceResourceType = pComplianceResourceType_,
        complianceResourceId = pComplianceResourceId_,
        complianceType = pComplianceType_,
        orderingTimestamp =
          Core._Time Lens.# pOrderingTimestamp_
      }

-- | Supplementary information about how the evaluation determined the
-- compliance.
evaluation_annotation :: Lens.Lens' Evaluation (Core.Maybe Core.Text)
evaluation_annotation = Lens.lens (\Evaluation' {annotation} -> annotation) (\s@Evaluation' {} a -> s {annotation = a} :: Evaluation)

-- | The type of AWS resource that was evaluated.
evaluation_complianceResourceType :: Lens.Lens' Evaluation Core.Text
evaluation_complianceResourceType = Lens.lens (\Evaluation' {complianceResourceType} -> complianceResourceType) (\s@Evaluation' {} a -> s {complianceResourceType = a} :: Evaluation)

-- | The ID of the AWS resource that was evaluated.
evaluation_complianceResourceId :: Lens.Lens' Evaluation Core.Text
evaluation_complianceResourceId = Lens.lens (\Evaluation' {complianceResourceId} -> complianceResourceId) (\s@Evaluation' {} a -> s {complianceResourceId = a} :: Evaluation)

-- | Indicates whether the AWS resource complies with the AWS Config rule
-- that it was evaluated against.
--
-- For the @Evaluation@ data type, AWS Config supports only the
-- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. AWS Config
-- does not support the @INSUFFICIENT_DATA@ value for this data type.
--
-- Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value
-- for @ComplianceType@ from a @PutEvaluations@ request. For example, an
-- AWS Lambda function for a custom AWS Config rule cannot pass an
-- @INSUFFICIENT_DATA@ value to AWS Config.
evaluation_complianceType :: Lens.Lens' Evaluation ComplianceType
evaluation_complianceType = Lens.lens (\Evaluation' {complianceType} -> complianceType) (\s@Evaluation' {} a -> s {complianceType = a} :: Evaluation)

-- | The time of the event in AWS Config that triggered the evaluation. For
-- event-based evaluations, the time indicates when AWS Config created the
-- configuration item that triggered the evaluation. For periodic
-- evaluations, the time indicates when AWS Config triggered the evaluation
-- at the frequency that you specified (for example, every 24 hours).
evaluation_orderingTimestamp :: Lens.Lens' Evaluation Core.UTCTime
evaluation_orderingTimestamp = Lens.lens (\Evaluation' {orderingTimestamp} -> orderingTimestamp) (\s@Evaluation' {} a -> s {orderingTimestamp = a} :: Evaluation) Core.. Core._Time

instance Core.FromJSON Evaluation where
  parseJSON =
    Core.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Core.<$> (x Core..:? "Annotation")
            Core.<*> (x Core..: "ComplianceResourceType")
            Core.<*> (x Core..: "ComplianceResourceId")
            Core.<*> (x Core..: "ComplianceType")
            Core.<*> (x Core..: "OrderingTimestamp")
      )

instance Core.Hashable Evaluation

instance Core.NFData Evaluation

instance Core.ToJSON Evaluation where
  toJSON Evaluation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Annotation" Core..=) Core.<$> annotation,
            Core.Just
              ( "ComplianceResourceType"
                  Core..= complianceResourceType
              ),
            Core.Just
              ( "ComplianceResourceId"
                  Core..= complianceResourceId
              ),
            Core.Just ("ComplianceType" Core..= complianceType),
            Core.Just
              ("OrderingTimestamp" Core..= orderingTimestamp)
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Evaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Evaluation
  ( Evaluation (..),

    -- * Smart constructor
    mkEvaluation,

    -- * Lenses
    eAnnotation,
    eComplianceResourceType,
    eComplianceResourceId,
    eOrderingTimestamp,
    eComplianceType,
  )
where

import Network.AWS.Config.Types.ComplianceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies an AWS resource and indicates whether it complies with the AWS Config rule that it was evaluated against.
--
-- /See:/ 'mkEvaluation' smart constructor.
data Evaluation = Evaluation'
  { -- | Supplementary information about how the evaluation determined the compliance.
    annotation :: Lude.Maybe Lude.Text,
    -- | The type of AWS resource that was evaluated.
    complianceResourceType :: Lude.Text,
    -- | The ID of the AWS resource that was evaluated.
    complianceResourceId :: Lude.Text,
    -- | The time of the event in AWS Config that triggered the evaluation. For event-based evaluations, the time indicates when AWS Config created the configuration item that triggered the evaluation. For periodic evaluations, the time indicates when AWS Config triggered the evaluation at the frequency that you specified (for example, every 24 hours).
    orderingTimestamp :: Lude.Timestamp,
    -- | Indicates whether the AWS resource complies with the AWS Config rule that it was evaluated against.
    --
    -- For the @Evaluation@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for this data type.
    -- Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value for @ComplianceType@ from a @PutEvaluations@ request. For example, an AWS Lambda function for a custom AWS Config rule cannot pass an @INSUFFICIENT_DATA@ value to AWS Config.
    complianceType :: ComplianceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Evaluation' with the minimum fields required to make a request.
--
-- * 'annotation' - Supplementary information about how the evaluation determined the compliance.
-- * 'complianceResourceType' - The type of AWS resource that was evaluated.
-- * 'complianceResourceId' - The ID of the AWS resource that was evaluated.
-- * 'orderingTimestamp' - The time of the event in AWS Config that triggered the evaluation. For event-based evaluations, the time indicates when AWS Config created the configuration item that triggered the evaluation. For periodic evaluations, the time indicates when AWS Config triggered the evaluation at the frequency that you specified (for example, every 24 hours).
-- * 'complianceType' - Indicates whether the AWS resource complies with the AWS Config rule that it was evaluated against.
--
-- For the @Evaluation@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for this data type.
-- Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value for @ComplianceType@ from a @PutEvaluations@ request. For example, an AWS Lambda function for a custom AWS Config rule cannot pass an @INSUFFICIENT_DATA@ value to AWS Config.
mkEvaluation ::
  -- | 'complianceResourceType'
  Lude.Text ->
  -- | 'complianceResourceId'
  Lude.Text ->
  -- | 'orderingTimestamp'
  Lude.Timestamp ->
  -- | 'complianceType'
  ComplianceType ->
  Evaluation
mkEvaluation
  pComplianceResourceType_
  pComplianceResourceId_
  pOrderingTimestamp_
  pComplianceType_ =
    Evaluation'
      { annotation = Lude.Nothing,
        complianceResourceType = pComplianceResourceType_,
        complianceResourceId = pComplianceResourceId_,
        orderingTimestamp = pOrderingTimestamp_,
        complianceType = pComplianceType_
      }

-- | Supplementary information about how the evaluation determined the compliance.
--
-- /Note:/ Consider using 'annotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAnnotation :: Lens.Lens' Evaluation (Lude.Maybe Lude.Text)
eAnnotation = Lens.lens (annotation :: Evaluation -> Lude.Maybe Lude.Text) (\s a -> s {annotation = a} :: Evaluation)
{-# DEPRECATED eAnnotation "Use generic-lens or generic-optics with 'annotation' instead." #-}

-- | The type of AWS resource that was evaluated.
--
-- /Note:/ Consider using 'complianceResourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eComplianceResourceType :: Lens.Lens' Evaluation Lude.Text
eComplianceResourceType = Lens.lens (complianceResourceType :: Evaluation -> Lude.Text) (\s a -> s {complianceResourceType = a} :: Evaluation)
{-# DEPRECATED eComplianceResourceType "Use generic-lens or generic-optics with 'complianceResourceType' instead." #-}

-- | The ID of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'complianceResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eComplianceResourceId :: Lens.Lens' Evaluation Lude.Text
eComplianceResourceId = Lens.lens (complianceResourceId :: Evaluation -> Lude.Text) (\s a -> s {complianceResourceId = a} :: Evaluation)
{-# DEPRECATED eComplianceResourceId "Use generic-lens or generic-optics with 'complianceResourceId' instead." #-}

-- | The time of the event in AWS Config that triggered the evaluation. For event-based evaluations, the time indicates when AWS Config created the configuration item that triggered the evaluation. For periodic evaluations, the time indicates when AWS Config triggered the evaluation at the frequency that you specified (for example, every 24 hours).
--
-- /Note:/ Consider using 'orderingTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOrderingTimestamp :: Lens.Lens' Evaluation Lude.Timestamp
eOrderingTimestamp = Lens.lens (orderingTimestamp :: Evaluation -> Lude.Timestamp) (\s a -> s {orderingTimestamp = a} :: Evaluation)
{-# DEPRECATED eOrderingTimestamp "Use generic-lens or generic-optics with 'orderingTimestamp' instead." #-}

-- | Indicates whether the AWS resource complies with the AWS Config rule that it was evaluated against.
--
-- For the @Evaluation@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for this data type.
-- Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value for @ComplianceType@ from a @PutEvaluations@ request. For example, an AWS Lambda function for a custom AWS Config rule cannot pass an @INSUFFICIENT_DATA@ value to AWS Config.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eComplianceType :: Lens.Lens' Evaluation ComplianceType
eComplianceType = Lens.lens (complianceType :: Evaluation -> ComplianceType) (\s a -> s {complianceType = a} :: Evaluation)
{-# DEPRECATED eComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

instance Lude.FromJSON Evaluation where
  parseJSON =
    Lude.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Lude.<$> (x Lude..:? "Annotation")
            Lude.<*> (x Lude..: "ComplianceResourceType")
            Lude.<*> (x Lude..: "ComplianceResourceId")
            Lude.<*> (x Lude..: "OrderingTimestamp")
            Lude.<*> (x Lude..: "ComplianceType")
      )

instance Lude.ToJSON Evaluation where
  toJSON Evaluation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Annotation" Lude..=) Lude.<$> annotation,
            Lude.Just
              ("ComplianceResourceType" Lude..= complianceResourceType),
            Lude.Just ("ComplianceResourceId" Lude..= complianceResourceId),
            Lude.Just ("OrderingTimestamp" Lude..= orderingTimestamp),
            Lude.Just ("ComplianceType" Lude..= complianceType)
          ]
      )

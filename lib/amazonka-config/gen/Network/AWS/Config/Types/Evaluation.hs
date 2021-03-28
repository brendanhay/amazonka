{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Evaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.Evaluation
  ( Evaluation (..)
  -- * Smart constructor
  , mkEvaluation
  -- * Lenses
  , eComplianceResourceType
  , eComplianceResourceId
  , eComplianceType
  , eOrderingTimestamp
  , eAnnotation
  ) where

import qualified Network.AWS.Config.Types.BaseResourceId as Types
import qualified Network.AWS.Config.Types.ComplianceType as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies an AWS resource and indicates whether it complies with the AWS Config rule that it was evaluated against.
--
-- /See:/ 'mkEvaluation' smart constructor.
data Evaluation = Evaluation'
  { complianceResourceType :: Types.StringWithCharLimit256
    -- ^ The type of AWS resource that was evaluated.
  , complianceResourceId :: Types.BaseResourceId
    -- ^ The ID of the AWS resource that was evaluated.
  , complianceType :: Types.ComplianceType
    -- ^ Indicates whether the AWS resource complies with the AWS Config rule that it was evaluated against.
--
-- For the @Evaluation@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for this data type.
-- Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value for @ComplianceType@ from a @PutEvaluations@ request. For example, an AWS Lambda function for a custom AWS Config rule cannot pass an @INSUFFICIENT_DATA@ value to AWS Config.
  , orderingTimestamp :: Core.NominalDiffTime
    -- ^ The time of the event in AWS Config that triggered the evaluation. For event-based evaluations, the time indicates when AWS Config created the configuration item that triggered the evaluation. For periodic evaluations, the time indicates when AWS Config triggered the evaluation at the frequency that you specified (for example, every 24 hours).
  , annotation :: Core.Maybe Types.StringWithCharLimit256
    -- ^ Supplementary information about how the evaluation determined the compliance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Evaluation' value with any optional fields omitted.
mkEvaluation
    :: Types.StringWithCharLimit256 -- ^ 'complianceResourceType'
    -> Types.BaseResourceId -- ^ 'complianceResourceId'
    -> Types.ComplianceType -- ^ 'complianceType'
    -> Core.NominalDiffTime -- ^ 'orderingTimestamp'
    -> Evaluation
mkEvaluation complianceResourceType complianceResourceId
  complianceType orderingTimestamp
  = Evaluation'{complianceResourceType, complianceResourceId,
                complianceType, orderingTimestamp, annotation = Core.Nothing}

-- | The type of AWS resource that was evaluated.
--
-- /Note:/ Consider using 'complianceResourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eComplianceResourceType :: Lens.Lens' Evaluation Types.StringWithCharLimit256
eComplianceResourceType = Lens.field @"complianceResourceType"
{-# INLINEABLE eComplianceResourceType #-}
{-# DEPRECATED complianceResourceType "Use generic-lens or generic-optics with 'complianceResourceType' instead"  #-}

-- | The ID of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'complianceResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eComplianceResourceId :: Lens.Lens' Evaluation Types.BaseResourceId
eComplianceResourceId = Lens.field @"complianceResourceId"
{-# INLINEABLE eComplianceResourceId #-}
{-# DEPRECATED complianceResourceId "Use generic-lens or generic-optics with 'complianceResourceId' instead"  #-}

-- | Indicates whether the AWS resource complies with the AWS Config rule that it was evaluated against.
--
-- For the @Evaluation@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for this data type.
-- Similarly, AWS Config does not accept @INSUFFICIENT_DATA@ as the value for @ComplianceType@ from a @PutEvaluations@ request. For example, an AWS Lambda function for a custom AWS Config rule cannot pass an @INSUFFICIENT_DATA@ value to AWS Config.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eComplianceType :: Lens.Lens' Evaluation Types.ComplianceType
eComplianceType = Lens.field @"complianceType"
{-# INLINEABLE eComplianceType #-}
{-# DEPRECATED complianceType "Use generic-lens or generic-optics with 'complianceType' instead"  #-}

-- | The time of the event in AWS Config that triggered the evaluation. For event-based evaluations, the time indicates when AWS Config created the configuration item that triggered the evaluation. For periodic evaluations, the time indicates when AWS Config triggered the evaluation at the frequency that you specified (for example, every 24 hours).
--
-- /Note:/ Consider using 'orderingTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOrderingTimestamp :: Lens.Lens' Evaluation Core.NominalDiffTime
eOrderingTimestamp = Lens.field @"orderingTimestamp"
{-# INLINEABLE eOrderingTimestamp #-}
{-# DEPRECATED orderingTimestamp "Use generic-lens or generic-optics with 'orderingTimestamp' instead"  #-}

-- | Supplementary information about how the evaluation determined the compliance.
--
-- /Note:/ Consider using 'annotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAnnotation :: Lens.Lens' Evaluation (Core.Maybe Types.StringWithCharLimit256)
eAnnotation = Lens.field @"annotation"
{-# INLINEABLE eAnnotation #-}
{-# DEPRECATED annotation "Use generic-lens or generic-optics with 'annotation' instead"  #-}

instance Core.FromJSON Evaluation where
        toJSON Evaluation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ComplianceResourceType" Core..= complianceResourceType),
                  Core.Just ("ComplianceResourceId" Core..= complianceResourceId),
                  Core.Just ("ComplianceType" Core..= complianceType),
                  Core.Just ("OrderingTimestamp" Core..= orderingTimestamp),
                  ("Annotation" Core..=) Core.<$> annotation])

instance Core.FromJSON Evaluation where
        parseJSON
          = Core.withObject "Evaluation" Core.$
              \ x ->
                Evaluation' Core.<$>
                  (x Core..: "ComplianceResourceType") Core.<*>
                    x Core..: "ComplianceResourceId"
                    Core.<*> x Core..: "ComplianceType"
                    Core.<*> x Core..: "OrderingTimestamp"
                    Core.<*> x Core..:? "Annotation"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.EvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EvaluationResult
  ( EvaluationResult (..),

    -- * Smart constructor
    mkEvaluationResult,

    -- * Lenses
    erAnnotation,
    erComplianceType,
    erConfigRuleInvokedTime,
    erEvaluationResultIdentifier,
    erResultRecordedTime,
    erResultToken,
  )
where

import qualified Network.AWS.Config.Types.Annotation as Types
import qualified Network.AWS.Config.Types.ComplianceType as Types
import qualified Network.AWS.Config.Types.EvaluationResultIdentifier as Types
import qualified Network.AWS.Config.Types.ResultToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of an AWS Config evaluation. Provides the AWS resource that was evaluated, the compliance of the resource, related time stamps, and supplementary information.
--
-- /See:/ 'mkEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | Supplementary information about how the evaluation determined the compliance.
    annotation :: Core.Maybe Types.Annotation,
    -- | Indicates whether the AWS resource complies with the AWS Config rule that evaluated it.
    --
    -- For the @EvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@ data type.
    complianceType :: Core.Maybe Types.ComplianceType,
    -- | The time when the AWS Config rule evaluated the AWS resource.
    configRuleInvokedTime :: Core.Maybe Core.NominalDiffTime,
    -- | Uniquely identifies the evaluation result.
    evaluationResultIdentifier :: Core.Maybe Types.EvaluationResultIdentifier,
    -- | The time when AWS Config recorded the evaluation result.
    resultRecordedTime :: Core.Maybe Core.NominalDiffTime,
    -- | An encrypted token that associates an evaluation with an AWS Config rule. The token identifies the rule, the AWS resource being evaluated, and the event that triggered the evaluation.
    resultToken :: Core.Maybe Types.ResultToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EvaluationResult' value with any optional fields omitted.
mkEvaluationResult ::
  EvaluationResult
mkEvaluationResult =
  EvaluationResult'
    { annotation = Core.Nothing,
      complianceType = Core.Nothing,
      configRuleInvokedTime = Core.Nothing,
      evaluationResultIdentifier = Core.Nothing,
      resultRecordedTime = Core.Nothing,
      resultToken = Core.Nothing
    }

-- | Supplementary information about how the evaluation determined the compliance.
--
-- /Note:/ Consider using 'annotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erAnnotation :: Lens.Lens' EvaluationResult (Core.Maybe Types.Annotation)
erAnnotation = Lens.field @"annotation"
{-# DEPRECATED erAnnotation "Use generic-lens or generic-optics with 'annotation' instead." #-}

-- | Indicates whether the AWS resource complies with the AWS Config rule that evaluated it.
--
-- For the @EvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@ data type.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erComplianceType :: Lens.Lens' EvaluationResult (Core.Maybe Types.ComplianceType)
erComplianceType = Lens.field @"complianceType"
{-# DEPRECATED erComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | The time when the AWS Config rule evaluated the AWS resource.
--
-- /Note:/ Consider using 'configRuleInvokedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erConfigRuleInvokedTime :: Lens.Lens' EvaluationResult (Core.Maybe Core.NominalDiffTime)
erConfigRuleInvokedTime = Lens.field @"configRuleInvokedTime"
{-# DEPRECATED erConfigRuleInvokedTime "Use generic-lens or generic-optics with 'configRuleInvokedTime' instead." #-}

-- | Uniquely identifies the evaluation result.
--
-- /Note:/ Consider using 'evaluationResultIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvaluationResultIdentifier :: Lens.Lens' EvaluationResult (Core.Maybe Types.EvaluationResultIdentifier)
erEvaluationResultIdentifier = Lens.field @"evaluationResultIdentifier"
{-# DEPRECATED erEvaluationResultIdentifier "Use generic-lens or generic-optics with 'evaluationResultIdentifier' instead." #-}

-- | The time when AWS Config recorded the evaluation result.
--
-- /Note:/ Consider using 'resultRecordedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erResultRecordedTime :: Lens.Lens' EvaluationResult (Core.Maybe Core.NominalDiffTime)
erResultRecordedTime = Lens.field @"resultRecordedTime"
{-# DEPRECATED erResultRecordedTime "Use generic-lens or generic-optics with 'resultRecordedTime' instead." #-}

-- | An encrypted token that associates an evaluation with an AWS Config rule. The token identifies the rule, the AWS resource being evaluated, and the event that triggered the evaluation.
--
-- /Note:/ Consider using 'resultToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erResultToken :: Lens.Lens' EvaluationResult (Core.Maybe Types.ResultToken)
erResultToken = Lens.field @"resultToken"
{-# DEPRECATED erResultToken "Use generic-lens or generic-optics with 'resultToken' instead." #-}

instance Core.FromJSON EvaluationResult where
  parseJSON =
    Core.withObject "EvaluationResult" Core.$
      \x ->
        EvaluationResult'
          Core.<$> (x Core..:? "Annotation")
          Core.<*> (x Core..:? "ComplianceType")
          Core.<*> (x Core..:? "ConfigRuleInvokedTime")
          Core.<*> (x Core..:? "EvaluationResultIdentifier")
          Core.<*> (x Core..:? "ResultRecordedTime")
          Core.<*> (x Core..:? "ResultToken")

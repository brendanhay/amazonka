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
    erEvaluationResultIdentifier,
    erAnnotation,
    erConfigRuleInvokedTime,
    erResultRecordedTime,
    erResultToken,
    erComplianceType,
  )
where

import Network.AWS.Config.Types.ComplianceType
import Network.AWS.Config.Types.EvaluationResultIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of an AWS Config evaluation. Provides the AWS resource that was evaluated, the compliance of the resource, related time stamps, and supplementary information.
--
-- /See:/ 'mkEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | Uniquely identifies the evaluation result.
    evaluationResultIdentifier :: Lude.Maybe EvaluationResultIdentifier,
    -- | Supplementary information about how the evaluation determined the compliance.
    annotation :: Lude.Maybe Lude.Text,
    -- | The time when the AWS Config rule evaluated the AWS resource.
    configRuleInvokedTime :: Lude.Maybe Lude.Timestamp,
    -- | The time when AWS Config recorded the evaluation result.
    resultRecordedTime :: Lude.Maybe Lude.Timestamp,
    -- | An encrypted token that associates an evaluation with an AWS Config rule. The token identifies the rule, the AWS resource being evaluated, and the event that triggered the evaluation.
    resultToken :: Lude.Maybe Lude.Text,
    -- | Indicates whether the AWS resource complies with the AWS Config rule that evaluated it.
    --
    -- For the @EvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@ data type.
    complianceType :: Lude.Maybe ComplianceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- * 'evaluationResultIdentifier' - Uniquely identifies the evaluation result.
-- * 'annotation' - Supplementary information about how the evaluation determined the compliance.
-- * 'configRuleInvokedTime' - The time when the AWS Config rule evaluated the AWS resource.
-- * 'resultRecordedTime' - The time when AWS Config recorded the evaluation result.
-- * 'resultToken' - An encrypted token that associates an evaluation with an AWS Config rule. The token identifies the rule, the AWS resource being evaluated, and the event that triggered the evaluation.
-- * 'complianceType' - Indicates whether the AWS resource complies with the AWS Config rule that evaluated it.
--
-- For the @EvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@ data type.
mkEvaluationResult ::
  EvaluationResult
mkEvaluationResult =
  EvaluationResult'
    { evaluationResultIdentifier = Lude.Nothing,
      annotation = Lude.Nothing,
      configRuleInvokedTime = Lude.Nothing,
      resultRecordedTime = Lude.Nothing,
      resultToken = Lude.Nothing,
      complianceType = Lude.Nothing
    }

-- | Uniquely identifies the evaluation result.
--
-- /Note:/ Consider using 'evaluationResultIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvaluationResultIdentifier :: Lens.Lens' EvaluationResult (Lude.Maybe EvaluationResultIdentifier)
erEvaluationResultIdentifier = Lens.lens (evaluationResultIdentifier :: EvaluationResult -> Lude.Maybe EvaluationResultIdentifier) (\s a -> s {evaluationResultIdentifier = a} :: EvaluationResult)
{-# DEPRECATED erEvaluationResultIdentifier "Use generic-lens or generic-optics with 'evaluationResultIdentifier' instead." #-}

-- | Supplementary information about how the evaluation determined the compliance.
--
-- /Note:/ Consider using 'annotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erAnnotation :: Lens.Lens' EvaluationResult (Lude.Maybe Lude.Text)
erAnnotation = Lens.lens (annotation :: EvaluationResult -> Lude.Maybe Lude.Text) (\s a -> s {annotation = a} :: EvaluationResult)
{-# DEPRECATED erAnnotation "Use generic-lens or generic-optics with 'annotation' instead." #-}

-- | The time when the AWS Config rule evaluated the AWS resource.
--
-- /Note:/ Consider using 'configRuleInvokedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erConfigRuleInvokedTime :: Lens.Lens' EvaluationResult (Lude.Maybe Lude.Timestamp)
erConfigRuleInvokedTime = Lens.lens (configRuleInvokedTime :: EvaluationResult -> Lude.Maybe Lude.Timestamp) (\s a -> s {configRuleInvokedTime = a} :: EvaluationResult)
{-# DEPRECATED erConfigRuleInvokedTime "Use generic-lens or generic-optics with 'configRuleInvokedTime' instead." #-}

-- | The time when AWS Config recorded the evaluation result.
--
-- /Note:/ Consider using 'resultRecordedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erResultRecordedTime :: Lens.Lens' EvaluationResult (Lude.Maybe Lude.Timestamp)
erResultRecordedTime = Lens.lens (resultRecordedTime :: EvaluationResult -> Lude.Maybe Lude.Timestamp) (\s a -> s {resultRecordedTime = a} :: EvaluationResult)
{-# DEPRECATED erResultRecordedTime "Use generic-lens or generic-optics with 'resultRecordedTime' instead." #-}

-- | An encrypted token that associates an evaluation with an AWS Config rule. The token identifies the rule, the AWS resource being evaluated, and the event that triggered the evaluation.
--
-- /Note:/ Consider using 'resultToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erResultToken :: Lens.Lens' EvaluationResult (Lude.Maybe Lude.Text)
erResultToken = Lens.lens (resultToken :: EvaluationResult -> Lude.Maybe Lude.Text) (\s a -> s {resultToken = a} :: EvaluationResult)
{-# DEPRECATED erResultToken "Use generic-lens or generic-optics with 'resultToken' instead." #-}

-- | Indicates whether the AWS resource complies with the AWS Config rule that evaluated it.
--
-- For the @EvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ values. AWS Config does not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@ data type.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erComplianceType :: Lens.Lens' EvaluationResult (Lude.Maybe ComplianceType)
erComplianceType = Lens.lens (complianceType :: EvaluationResult -> Lude.Maybe ComplianceType) (\s a -> s {complianceType = a} :: EvaluationResult)
{-# DEPRECATED erComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

instance Lude.FromJSON EvaluationResult where
  parseJSON =
    Lude.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Lude.<$> (x Lude..:? "EvaluationResultIdentifier")
            Lude.<*> (x Lude..:? "Annotation")
            Lude.<*> (x Lude..:? "ConfigRuleInvokedTime")
            Lude.<*> (x Lude..:? "ResultRecordedTime")
            Lude.<*> (x Lude..:? "ResultToken")
            Lude.<*> (x Lude..:? "ComplianceType")
      )

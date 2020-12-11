-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackEvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackEvaluationResult
  ( ConformancePackEvaluationResult (..),

    -- * Smart constructor
    mkConformancePackEvaluationResult,

    -- * Lenses
    cperAnnotation,
    cperComplianceType,
    cperEvaluationResultIdentifier,
    cperConfigRuleInvokedTime,
    cperResultRecordedTime,
  )
where

import Network.AWS.Config.Types.ConformancePackComplianceType
import Network.AWS.Config.Types.EvaluationResultIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of a conformance pack evaluation. Provides AWS Config rule and AWS resource type that was evaluated, the compliance of the conformance pack, related time stamps, and supplementary information.
--
-- /See:/ 'mkConformancePackEvaluationResult' smart constructor.
data ConformancePackEvaluationResult = ConformancePackEvaluationResult'
  { annotation ::
      Lude.Maybe Lude.Text,
    complianceType ::
      ConformancePackComplianceType,
    evaluationResultIdentifier ::
      EvaluationResultIdentifier,
    configRuleInvokedTime ::
      Lude.Timestamp,
    resultRecordedTime ::
      Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConformancePackEvaluationResult' with the minimum fields required to make a request.
--
-- * 'annotation' - Supplementary information about how the evaluation determined the compliance.
-- * 'complianceType' - The compliance type. The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
-- * 'configRuleInvokedTime' - The time when AWS Config rule evaluated AWS resource.
-- * 'evaluationResultIdentifier' - Undocumented field.
-- * 'resultRecordedTime' - The time when AWS Config recorded the evaluation result.
mkConformancePackEvaluationResult ::
  -- | 'complianceType'
  ConformancePackComplianceType ->
  -- | 'evaluationResultIdentifier'
  EvaluationResultIdentifier ->
  -- | 'configRuleInvokedTime'
  Lude.Timestamp ->
  -- | 'resultRecordedTime'
  Lude.Timestamp ->
  ConformancePackEvaluationResult
mkConformancePackEvaluationResult
  pComplianceType_
  pEvaluationResultIdentifier_
  pConfigRuleInvokedTime_
  pResultRecordedTime_ =
    ConformancePackEvaluationResult'
      { annotation = Lude.Nothing,
        complianceType = pComplianceType_,
        evaluationResultIdentifier = pEvaluationResultIdentifier_,
        configRuleInvokedTime = pConfigRuleInvokedTime_,
        resultRecordedTime = pResultRecordedTime_
      }

-- | Supplementary information about how the evaluation determined the compliance.
--
-- /Note:/ Consider using 'annotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperAnnotation :: Lens.Lens' ConformancePackEvaluationResult (Lude.Maybe Lude.Text)
cperAnnotation = Lens.lens (annotation :: ConformancePackEvaluationResult -> Lude.Maybe Lude.Text) (\s a -> s {annotation = a} :: ConformancePackEvaluationResult)
{-# DEPRECATED cperAnnotation "Use generic-lens or generic-optics with 'annotation' instead." #-}

-- | The compliance type. The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperComplianceType :: Lens.Lens' ConformancePackEvaluationResult ConformancePackComplianceType
cperComplianceType = Lens.lens (complianceType :: ConformancePackEvaluationResult -> ConformancePackComplianceType) (\s a -> s {complianceType = a} :: ConformancePackEvaluationResult)
{-# DEPRECATED cperComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'evaluationResultIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperEvaluationResultIdentifier :: Lens.Lens' ConformancePackEvaluationResult EvaluationResultIdentifier
cperEvaluationResultIdentifier = Lens.lens (evaluationResultIdentifier :: ConformancePackEvaluationResult -> EvaluationResultIdentifier) (\s a -> s {evaluationResultIdentifier = a} :: ConformancePackEvaluationResult)
{-# DEPRECATED cperEvaluationResultIdentifier "Use generic-lens or generic-optics with 'evaluationResultIdentifier' instead." #-}

-- | The time when AWS Config rule evaluated AWS resource.
--
-- /Note:/ Consider using 'configRuleInvokedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperConfigRuleInvokedTime :: Lens.Lens' ConformancePackEvaluationResult Lude.Timestamp
cperConfigRuleInvokedTime = Lens.lens (configRuleInvokedTime :: ConformancePackEvaluationResult -> Lude.Timestamp) (\s a -> s {configRuleInvokedTime = a} :: ConformancePackEvaluationResult)
{-# DEPRECATED cperConfigRuleInvokedTime "Use generic-lens or generic-optics with 'configRuleInvokedTime' instead." #-}

-- | The time when AWS Config recorded the evaluation result.
--
-- /Note:/ Consider using 'resultRecordedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperResultRecordedTime :: Lens.Lens' ConformancePackEvaluationResult Lude.Timestamp
cperResultRecordedTime = Lens.lens (resultRecordedTime :: ConformancePackEvaluationResult -> Lude.Timestamp) (\s a -> s {resultRecordedTime = a} :: ConformancePackEvaluationResult)
{-# DEPRECATED cperResultRecordedTime "Use generic-lens or generic-optics with 'resultRecordedTime' instead." #-}

instance Lude.FromJSON ConformancePackEvaluationResult where
  parseJSON =
    Lude.withObject
      "ConformancePackEvaluationResult"
      ( \x ->
          ConformancePackEvaluationResult'
            Lude.<$> (x Lude..:? "Annotation")
            Lude.<*> (x Lude..: "ComplianceType")
            Lude.<*> (x Lude..: "EvaluationResultIdentifier")
            Lude.<*> (x Lude..: "ConfigRuleInvokedTime")
            Lude.<*> (x Lude..: "ResultRecordedTime")
      )

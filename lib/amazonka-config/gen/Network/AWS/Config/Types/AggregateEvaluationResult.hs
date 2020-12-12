{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregateEvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateEvaluationResult
  ( AggregateEvaluationResult (..),

    -- * Smart constructor
    mkAggregateEvaluationResult,

    -- * Lenses
    aerEvaluationResultIdentifier,
    aerAnnotation,
    aerConfigRuleInvokedTime,
    aerResultRecordedTime,
    aerAccountId,
    aerComplianceType,
    aerAWSRegion,
  )
where

import Network.AWS.Config.Types.ComplianceType
import Network.AWS.Config.Types.EvaluationResultIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of an AWS Config evaluation for an account ID and region in an aggregator. Provides the AWS resource that was evaluated, the compliance of the resource, related time stamps, and supplementary information.
--
-- /See:/ 'mkAggregateEvaluationResult' smart constructor.
data AggregateEvaluationResult = AggregateEvaluationResult'
  { evaluationResultIdentifier ::
      Lude.Maybe EvaluationResultIdentifier,
    annotation :: Lude.Maybe Lude.Text,
    configRuleInvokedTime ::
      Lude.Maybe Lude.Timestamp,
    resultRecordedTime ::
      Lude.Maybe Lude.Timestamp,
    accountId :: Lude.Maybe Lude.Text,
    complianceType ::
      Lude.Maybe ComplianceType,
    awsRegion :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AggregateEvaluationResult' with the minimum fields required to make a request.
--
-- * 'accountId' - The 12-digit account ID of the source account.
-- * 'annotation' - Supplementary information about how the agrregate evaluation determined the compliance.
-- * 'awsRegion' - The source region from where the data is aggregated.
-- * 'complianceType' - The resource compliance status.
--
-- For the @AggregationEvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ and @NON_COMPLIANT@ . AWS Config does not support the @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ value.
-- * 'configRuleInvokedTime' - The time when the AWS Config rule evaluated the AWS resource.
-- * 'evaluationResultIdentifier' - Uniquely identifies the evaluation result.
-- * 'resultRecordedTime' - The time when AWS Config recorded the aggregate evaluation result.
mkAggregateEvaluationResult ::
  AggregateEvaluationResult
mkAggregateEvaluationResult =
  AggregateEvaluationResult'
    { evaluationResultIdentifier =
        Lude.Nothing,
      annotation = Lude.Nothing,
      configRuleInvokedTime = Lude.Nothing,
      resultRecordedTime = Lude.Nothing,
      accountId = Lude.Nothing,
      complianceType = Lude.Nothing,
      awsRegion = Lude.Nothing
    }

-- | Uniquely identifies the evaluation result.
--
-- /Note:/ Consider using 'evaluationResultIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerEvaluationResultIdentifier :: Lens.Lens' AggregateEvaluationResult (Lude.Maybe EvaluationResultIdentifier)
aerEvaluationResultIdentifier = Lens.lens (evaluationResultIdentifier :: AggregateEvaluationResult -> Lude.Maybe EvaluationResultIdentifier) (\s a -> s {evaluationResultIdentifier = a} :: AggregateEvaluationResult)
{-# DEPRECATED aerEvaluationResultIdentifier "Use generic-lens or generic-optics with 'evaluationResultIdentifier' instead." #-}

-- | Supplementary information about how the agrregate evaluation determined the compliance.
--
-- /Note:/ Consider using 'annotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerAnnotation :: Lens.Lens' AggregateEvaluationResult (Lude.Maybe Lude.Text)
aerAnnotation = Lens.lens (annotation :: AggregateEvaluationResult -> Lude.Maybe Lude.Text) (\s a -> s {annotation = a} :: AggregateEvaluationResult)
{-# DEPRECATED aerAnnotation "Use generic-lens or generic-optics with 'annotation' instead." #-}

-- | The time when the AWS Config rule evaluated the AWS resource.
--
-- /Note:/ Consider using 'configRuleInvokedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerConfigRuleInvokedTime :: Lens.Lens' AggregateEvaluationResult (Lude.Maybe Lude.Timestamp)
aerConfigRuleInvokedTime = Lens.lens (configRuleInvokedTime :: AggregateEvaluationResult -> Lude.Maybe Lude.Timestamp) (\s a -> s {configRuleInvokedTime = a} :: AggregateEvaluationResult)
{-# DEPRECATED aerConfigRuleInvokedTime "Use generic-lens or generic-optics with 'configRuleInvokedTime' instead." #-}

-- | The time when AWS Config recorded the aggregate evaluation result.
--
-- /Note:/ Consider using 'resultRecordedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerResultRecordedTime :: Lens.Lens' AggregateEvaluationResult (Lude.Maybe Lude.Timestamp)
aerResultRecordedTime = Lens.lens (resultRecordedTime :: AggregateEvaluationResult -> Lude.Maybe Lude.Timestamp) (\s a -> s {resultRecordedTime = a} :: AggregateEvaluationResult)
{-# DEPRECATED aerResultRecordedTime "Use generic-lens or generic-optics with 'resultRecordedTime' instead." #-}

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerAccountId :: Lens.Lens' AggregateEvaluationResult (Lude.Maybe Lude.Text)
aerAccountId = Lens.lens (accountId :: AggregateEvaluationResult -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: AggregateEvaluationResult)
{-# DEPRECATED aerAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The resource compliance status.
--
-- For the @AggregationEvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ and @NON_COMPLIANT@ . AWS Config does not support the @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ value.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerComplianceType :: Lens.Lens' AggregateEvaluationResult (Lude.Maybe ComplianceType)
aerComplianceType = Lens.lens (complianceType :: AggregateEvaluationResult -> Lude.Maybe ComplianceType) (\s a -> s {complianceType = a} :: AggregateEvaluationResult)
{-# DEPRECATED aerComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | The source region from where the data is aggregated.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerAWSRegion :: Lens.Lens' AggregateEvaluationResult (Lude.Maybe Lude.Text)
aerAWSRegion = Lens.lens (awsRegion :: AggregateEvaluationResult -> Lude.Maybe Lude.Text) (\s a -> s {awsRegion = a} :: AggregateEvaluationResult)
{-# DEPRECATED aerAWSRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

instance Lude.FromJSON AggregateEvaluationResult where
  parseJSON =
    Lude.withObject
      "AggregateEvaluationResult"
      ( \x ->
          AggregateEvaluationResult'
            Lude.<$> (x Lude..:? "EvaluationResultIdentifier")
            Lude.<*> (x Lude..:? "Annotation")
            Lude.<*> (x Lude..:? "ConfigRuleInvokedTime")
            Lude.<*> (x Lude..:? "ResultRecordedTime")
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "ComplianceType")
            Lude.<*> (x Lude..:? "AwsRegion")
      )

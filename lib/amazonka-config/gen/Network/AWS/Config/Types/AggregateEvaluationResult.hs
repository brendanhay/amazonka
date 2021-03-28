{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregateEvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.AggregateEvaluationResult
  ( AggregateEvaluationResult (..)
  -- * Smart constructor
  , mkAggregateEvaluationResult
  -- * Lenses
  , aerAccountId
  , aerAnnotation
  , aerAwsRegion
  , aerComplianceType
  , aerConfigRuleInvokedTime
  , aerEvaluationResultIdentifier
  , aerResultRecordedTime
  ) where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.AwsRegion as Types
import qualified Network.AWS.Config.Types.ComplianceType as Types
import qualified Network.AWS.Config.Types.EvaluationResultIdentifier as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of an AWS Config evaluation for an account ID and region in an aggregator. Provides the AWS resource that was evaluated, the compliance of the resource, related time stamps, and supplementary information. 
--
-- /See:/ 'mkAggregateEvaluationResult' smart constructor.
data AggregateEvaluationResult = AggregateEvaluationResult'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The 12-digit account ID of the source account.
  , annotation :: Core.Maybe Types.StringWithCharLimit256
    -- ^ Supplementary information about how the agrregate evaluation determined the compliance.
  , awsRegion :: Core.Maybe Types.AwsRegion
    -- ^ The source region from where the data is aggregated.
  , complianceType :: Core.Maybe Types.ComplianceType
    -- ^ The resource compliance status.
--
-- For the @AggregationEvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ and @NON_COMPLIANT@ . AWS Config does not support the @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ value.
  , configRuleInvokedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the AWS Config rule evaluated the AWS resource.
  , evaluationResultIdentifier :: Core.Maybe Types.EvaluationResultIdentifier
    -- ^ Uniquely identifies the evaluation result.
  , resultRecordedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when AWS Config recorded the aggregate evaluation result.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AggregateEvaluationResult' value with any optional fields omitted.
mkAggregateEvaluationResult
    :: AggregateEvaluationResult
mkAggregateEvaluationResult
  = AggregateEvaluationResult'{accountId = Core.Nothing,
                               annotation = Core.Nothing, awsRegion = Core.Nothing,
                               complianceType = Core.Nothing,
                               configRuleInvokedTime = Core.Nothing,
                               evaluationResultIdentifier = Core.Nothing,
                               resultRecordedTime = Core.Nothing}

-- | The 12-digit account ID of the source account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerAccountId :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Types.AccountId)
aerAccountId = Lens.field @"accountId"
{-# INLINEABLE aerAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Supplementary information about how the agrregate evaluation determined the compliance.
--
-- /Note:/ Consider using 'annotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerAnnotation :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Types.StringWithCharLimit256)
aerAnnotation = Lens.field @"annotation"
{-# INLINEABLE aerAnnotation #-}
{-# DEPRECATED annotation "Use generic-lens or generic-optics with 'annotation' instead"  #-}

-- | The source region from where the data is aggregated.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerAwsRegion :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Types.AwsRegion)
aerAwsRegion = Lens.field @"awsRegion"
{-# INLINEABLE aerAwsRegion #-}
{-# DEPRECATED awsRegion "Use generic-lens or generic-optics with 'awsRegion' instead"  #-}

-- | The resource compliance status.
--
-- For the @AggregationEvaluationResult@ data type, AWS Config supports only the @COMPLIANT@ and @NON_COMPLIANT@ . AWS Config does not support the @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ value.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerComplianceType :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Types.ComplianceType)
aerComplianceType = Lens.field @"complianceType"
{-# INLINEABLE aerComplianceType #-}
{-# DEPRECATED complianceType "Use generic-lens or generic-optics with 'complianceType' instead"  #-}

-- | The time when the AWS Config rule evaluated the AWS resource.
--
-- /Note:/ Consider using 'configRuleInvokedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerConfigRuleInvokedTime :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Core.NominalDiffTime)
aerConfigRuleInvokedTime = Lens.field @"configRuleInvokedTime"
{-# INLINEABLE aerConfigRuleInvokedTime #-}
{-# DEPRECATED configRuleInvokedTime "Use generic-lens or generic-optics with 'configRuleInvokedTime' instead"  #-}

-- | Uniquely identifies the evaluation result.
--
-- /Note:/ Consider using 'evaluationResultIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerEvaluationResultIdentifier :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Types.EvaluationResultIdentifier)
aerEvaluationResultIdentifier = Lens.field @"evaluationResultIdentifier"
{-# INLINEABLE aerEvaluationResultIdentifier #-}
{-# DEPRECATED evaluationResultIdentifier "Use generic-lens or generic-optics with 'evaluationResultIdentifier' instead"  #-}

-- | The time when AWS Config recorded the aggregate evaluation result.
--
-- /Note:/ Consider using 'resultRecordedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerResultRecordedTime :: Lens.Lens' AggregateEvaluationResult (Core.Maybe Core.NominalDiffTime)
aerResultRecordedTime = Lens.field @"resultRecordedTime"
{-# INLINEABLE aerResultRecordedTime #-}
{-# DEPRECATED resultRecordedTime "Use generic-lens or generic-optics with 'resultRecordedTime' instead"  #-}

instance Core.FromJSON AggregateEvaluationResult where
        parseJSON
          = Core.withObject "AggregateEvaluationResult" Core.$
              \ x ->
                AggregateEvaluationResult' Core.<$>
                  (x Core..:? "AccountId") Core.<*> x Core..:? "Annotation" Core.<*>
                    x Core..:? "AwsRegion"
                    Core.<*> x Core..:? "ComplianceType"
                    Core.<*> x Core..:? "ConfigRuleInvokedTime"
                    Core.<*> x Core..:? "EvaluationResultIdentifier"
                    Core.<*> x Core..:? "ResultRecordedTime"

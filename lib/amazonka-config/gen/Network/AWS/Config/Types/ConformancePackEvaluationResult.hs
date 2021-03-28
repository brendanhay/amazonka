{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackEvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConformancePackEvaluationResult
  ( ConformancePackEvaluationResult (..)
  -- * Smart constructor
  , mkConformancePackEvaluationResult
  -- * Lenses
  , cperComplianceType
  , cperEvaluationResultIdentifier
  , cperConfigRuleInvokedTime
  , cperResultRecordedTime
  , cperAnnotation
  ) where

import qualified Network.AWS.Config.Types.Annotation as Types
import qualified Network.AWS.Config.Types.ConformancePackComplianceType as Types
import qualified Network.AWS.Config.Types.EvaluationResultIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of a conformance pack evaluation. Provides AWS Config rule and AWS resource type that was evaluated, the compliance of the conformance pack, related time stamps, and supplementary information. 
--
-- /See:/ 'mkConformancePackEvaluationResult' smart constructor.
data ConformancePackEvaluationResult = ConformancePackEvaluationResult'
  { complianceType :: Types.ConformancePackComplianceType
    -- ^ The compliance type. The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ . 
  , evaluationResultIdentifier :: Types.EvaluationResultIdentifier
  , configRuleInvokedTime :: Core.NominalDiffTime
    -- ^ The time when AWS Config rule evaluated AWS resource.
  , resultRecordedTime :: Core.NominalDiffTime
    -- ^ The time when AWS Config recorded the evaluation result. 
  , annotation :: Core.Maybe Types.Annotation
    -- ^ Supplementary information about how the evaluation determined the compliance. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ConformancePackEvaluationResult' value with any optional fields omitted.
mkConformancePackEvaluationResult
    :: Types.ConformancePackComplianceType -- ^ 'complianceType'
    -> Types.EvaluationResultIdentifier -- ^ 'evaluationResultIdentifier'
    -> Core.NominalDiffTime -- ^ 'configRuleInvokedTime'
    -> Core.NominalDiffTime -- ^ 'resultRecordedTime'
    -> ConformancePackEvaluationResult
mkConformancePackEvaluationResult complianceType
  evaluationResultIdentifier configRuleInvokedTime resultRecordedTime
  = ConformancePackEvaluationResult'{complianceType,
                                     evaluationResultIdentifier, configRuleInvokedTime,
                                     resultRecordedTime, annotation = Core.Nothing}

-- | The compliance type. The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ . 
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperComplianceType :: Lens.Lens' ConformancePackEvaluationResult Types.ConformancePackComplianceType
cperComplianceType = Lens.field @"complianceType"
{-# INLINEABLE cperComplianceType #-}
{-# DEPRECATED complianceType "Use generic-lens or generic-optics with 'complianceType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'evaluationResultIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperEvaluationResultIdentifier :: Lens.Lens' ConformancePackEvaluationResult Types.EvaluationResultIdentifier
cperEvaluationResultIdentifier = Lens.field @"evaluationResultIdentifier"
{-# INLINEABLE cperEvaluationResultIdentifier #-}
{-# DEPRECATED evaluationResultIdentifier "Use generic-lens or generic-optics with 'evaluationResultIdentifier' instead"  #-}

-- | The time when AWS Config rule evaluated AWS resource.
--
-- /Note:/ Consider using 'configRuleInvokedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperConfigRuleInvokedTime :: Lens.Lens' ConformancePackEvaluationResult Core.NominalDiffTime
cperConfigRuleInvokedTime = Lens.field @"configRuleInvokedTime"
{-# INLINEABLE cperConfigRuleInvokedTime #-}
{-# DEPRECATED configRuleInvokedTime "Use generic-lens or generic-optics with 'configRuleInvokedTime' instead"  #-}

-- | The time when AWS Config recorded the evaluation result. 
--
-- /Note:/ Consider using 'resultRecordedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperResultRecordedTime :: Lens.Lens' ConformancePackEvaluationResult Core.NominalDiffTime
cperResultRecordedTime = Lens.field @"resultRecordedTime"
{-# INLINEABLE cperResultRecordedTime #-}
{-# DEPRECATED resultRecordedTime "Use generic-lens or generic-optics with 'resultRecordedTime' instead"  #-}

-- | Supplementary information about how the evaluation determined the compliance. 
--
-- /Note:/ Consider using 'annotation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperAnnotation :: Lens.Lens' ConformancePackEvaluationResult (Core.Maybe Types.Annotation)
cperAnnotation = Lens.field @"annotation"
{-# INLINEABLE cperAnnotation #-}
{-# DEPRECATED annotation "Use generic-lens or generic-optics with 'annotation' instead"  #-}

instance Core.FromJSON ConformancePackEvaluationResult where
        parseJSON
          = Core.withObject "ConformancePackEvaluationResult" Core.$
              \ x ->
                ConformancePackEvaluationResult' Core.<$>
                  (x Core..: "ComplianceType") Core.<*>
                    x Core..: "EvaluationResultIdentifier"
                    Core.<*> x Core..: "ConfigRuleInvokedTime"
                    Core.<*> x Core..: "ResultRecordedTime"
                    Core.<*> x Core..:? "Annotation"

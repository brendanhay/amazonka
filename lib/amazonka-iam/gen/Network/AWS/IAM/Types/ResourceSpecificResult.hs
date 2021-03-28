{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ResourceSpecificResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.ResourceSpecificResult
  ( ResourceSpecificResult (..)
  -- * Smart constructor
  , mkResourceSpecificResult
  -- * Lenses
  , rsrEvalResourceName
  , rsrEvalResourceDecision
  , rsrEvalDecisionDetails
  , rsrMatchedStatements
  , rsrMissingContextValues
  , rsrPermissionsBoundaryDecisionDetail
  ) where

import qualified Network.AWS.IAM.Types.ContextKeyNameType as Types
import qualified Network.AWS.IAM.Types.EvalDecisionSourceType as Types
import qualified Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail as Types
import qualified Network.AWS.IAM.Types.PolicyEvaluationDecisionType as Types
import qualified Network.AWS.IAM.Types.ResourceNameType as Types
import qualified Network.AWS.IAM.Types.Statement as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the result of the simulation of a single API operation call on a single resource.
--
-- This data type is used by a member of the 'EvaluationResult' data type.
--
-- /See:/ 'mkResourceSpecificResult' smart constructor.
data ResourceSpecificResult = ResourceSpecificResult'
  { evalResourceName :: Types.ResourceNameType
    -- ^ The name of the simulated resource, in Amazon Resource Name (ARN) format.
  , evalResourceDecision :: Types.PolicyEvaluationDecisionType
    -- ^ The result of the simulation of the simulated API operation on the resource specified in @EvalResourceName@ .
  , evalDecisionDetails :: Core.Maybe (Core.HashMap Types.EvalDecisionSourceType Types.PolicyEvaluationDecisionType)
    -- ^ Additional details about the results of the evaluation decision on a single resource. This parameter is returned only for cross-account simulations. This parameter explains how each policy type contributes to the resource-specific evaluation decision.
  , matchedStatements :: Core.Maybe [Types.Statement]
    -- ^ A list of the statements in the input policies that determine the result for this part of the simulation. Remember that even if multiple statements allow the operation on the resource, if /any/ statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
  , missingContextValues :: Core.Maybe [Types.ContextKeyNameType]
    -- ^ A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when a list of ARNs is included in the @ResourceArns@ parameter instead of "*". If you do not specify individual resources, by setting @ResourceArns@ to "*" or by not including the @ResourceArns@ parameter, then any missing context values are instead included under the @EvaluationResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
  , permissionsBoundaryDecisionDetail :: Core.Maybe Types.PermissionsBoundaryDecisionDetail
    -- ^ Contains information about the effect that a permissions boundary has on a policy simulation when that boundary is applied to an IAM entity.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceSpecificResult' value with any optional fields omitted.
mkResourceSpecificResult
    :: Types.ResourceNameType -- ^ 'evalResourceName'
    -> Types.PolicyEvaluationDecisionType -- ^ 'evalResourceDecision'
    -> ResourceSpecificResult
mkResourceSpecificResult evalResourceName evalResourceDecision
  = ResourceSpecificResult'{evalResourceName, evalResourceDecision,
                            evalDecisionDetails = Core.Nothing,
                            matchedStatements = Core.Nothing,
                            missingContextValues = Core.Nothing,
                            permissionsBoundaryDecisionDetail = Core.Nothing}

-- | The name of the simulated resource, in Amazon Resource Name (ARN) format.
--
-- /Note:/ Consider using 'evalResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrEvalResourceName :: Lens.Lens' ResourceSpecificResult Types.ResourceNameType
rsrEvalResourceName = Lens.field @"evalResourceName"
{-# INLINEABLE rsrEvalResourceName #-}
{-# DEPRECATED evalResourceName "Use generic-lens or generic-optics with 'evalResourceName' instead"  #-}

-- | The result of the simulation of the simulated API operation on the resource specified in @EvalResourceName@ .
--
-- /Note:/ Consider using 'evalResourceDecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrEvalResourceDecision :: Lens.Lens' ResourceSpecificResult Types.PolicyEvaluationDecisionType
rsrEvalResourceDecision = Lens.field @"evalResourceDecision"
{-# INLINEABLE rsrEvalResourceDecision #-}
{-# DEPRECATED evalResourceDecision "Use generic-lens or generic-optics with 'evalResourceDecision' instead"  #-}

-- | Additional details about the results of the evaluation decision on a single resource. This parameter is returned only for cross-account simulations. This parameter explains how each policy type contributes to the resource-specific evaluation decision.
--
-- /Note:/ Consider using 'evalDecisionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrEvalDecisionDetails :: Lens.Lens' ResourceSpecificResult (Core.Maybe (Core.HashMap Types.EvalDecisionSourceType Types.PolicyEvaluationDecisionType))
rsrEvalDecisionDetails = Lens.field @"evalDecisionDetails"
{-# INLINEABLE rsrEvalDecisionDetails #-}
{-# DEPRECATED evalDecisionDetails "Use generic-lens or generic-optics with 'evalDecisionDetails' instead"  #-}

-- | A list of the statements in the input policies that determine the result for this part of the simulation. Remember that even if multiple statements allow the operation on the resource, if /any/ statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
--
-- /Note:/ Consider using 'matchedStatements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrMatchedStatements :: Lens.Lens' ResourceSpecificResult (Core.Maybe [Types.Statement])
rsrMatchedStatements = Lens.field @"matchedStatements"
{-# INLINEABLE rsrMatchedStatements #-}
{-# DEPRECATED matchedStatements "Use generic-lens or generic-optics with 'matchedStatements' instead"  #-}

-- | A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when a list of ARNs is included in the @ResourceArns@ parameter instead of "*". If you do not specify individual resources, by setting @ResourceArns@ to "*" or by not including the @ResourceArns@ parameter, then any missing context values are instead included under the @EvaluationResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
--
-- /Note:/ Consider using 'missingContextValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrMissingContextValues :: Lens.Lens' ResourceSpecificResult (Core.Maybe [Types.ContextKeyNameType])
rsrMissingContextValues = Lens.field @"missingContextValues"
{-# INLINEABLE rsrMissingContextValues #-}
{-# DEPRECATED missingContextValues "Use generic-lens or generic-optics with 'missingContextValues' instead"  #-}

-- | Contains information about the effect that a permissions boundary has on a policy simulation when that boundary is applied to an IAM entity.
--
-- /Note:/ Consider using 'permissionsBoundaryDecisionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrPermissionsBoundaryDecisionDetail :: Lens.Lens' ResourceSpecificResult (Core.Maybe Types.PermissionsBoundaryDecisionDetail)
rsrPermissionsBoundaryDecisionDetail = Lens.field @"permissionsBoundaryDecisionDetail"
{-# INLINEABLE rsrPermissionsBoundaryDecisionDetail #-}
{-# DEPRECATED permissionsBoundaryDecisionDetail "Use generic-lens or generic-optics with 'permissionsBoundaryDecisionDetail' instead"  #-}

instance Core.FromXML ResourceSpecificResult where
        parseXML x
          = ResourceSpecificResult' Core.<$>
              (x Core..@ "EvalResourceName") Core.<*>
                x Core..@ "EvalResourceDecision"
                Core.<*>
                x Core..@? "EvalDecisionDetails" Core..<@>
                  Core.parseXMLMap "entry" "key" "value"
                Core.<*>
                x Core..@? "MatchedStatements" Core..<@> Core.parseXMLList "member"
                Core.<*>
                x Core..@? "MissingContextValues" Core..<@>
                  Core.parseXMLList "member"
                Core.<*> x Core..@? "PermissionsBoundaryDecisionDetail"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.EvaluationResult
  ( EvaluationResult (..),

    -- * Smart constructor
    mkEvaluationResult,

    -- * Lenses
    erEvalActionName,
    erEvalDecision,
    erEvalDecisionDetails,
    erEvalResourceName,
    erMatchedStatements,
    erMissingContextValues,
    erOrganizationsDecisionDetail,
    erPermissionsBoundaryDecisionDetail,
    erResourceSpecificResults,
  )
where

import qualified Network.AWS.IAM.Types.ContextKeyNameType as Types
import qualified Network.AWS.IAM.Types.EvalActionName as Types
import qualified Network.AWS.IAM.Types.EvalDecisionSourceType as Types
import qualified Network.AWS.IAM.Types.EvalResourceName as Types
import qualified Network.AWS.IAM.Types.OrganizationsDecisionDetail as Types
import qualified Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail as Types
import qualified Network.AWS.IAM.Types.PolicyEvaluationDecisionType as Types
import qualified Network.AWS.IAM.Types.ResourceSpecificResult as Types
import qualified Network.AWS.IAM.Types.Statement as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the results of a simulation.
--
-- This data type is used by the return parameter of @'SimulateCustomPolicy' @ and @'SimulatePrincipalPolicy' @ .
--
-- /See:/ 'mkEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | The name of the API operation tested on the indicated resource.
    evalActionName :: Types.EvalActionName,
    -- | The result of the simulation.
    evalDecision :: Types.PolicyEvaluationDecisionType,
    -- | Additional details about the results of the cross-account evaluation decision. This parameter is populated for only cross-account simulations. It contains a brief summary of how each policy type contributes to the final evaluation decision.
    --
    -- If the simulation evaluates policies within the same account and includes a resource ARN, then the parameter is present but the response is empty. If the simulation evaluates policies within the same account and specifies all resources (@*@ ), then the parameter is not returned.
    -- When you make a cross-account request, AWS evaluates the request in the trusting account and the trusted account. The request is allowed only if both evaluations return @true@ . For more information about how policies are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating Policies Within a Single Account> .
    -- If an AWS Organizations SCP included in the evaluation denies access, the simulation ends. In this case, policy evaluation does not proceed any further and this parameter is not returned.
    evalDecisionDetails :: Core.Maybe (Core.HashMap Types.EvalDecisionSourceType Types.PolicyEvaluationDecisionType),
    -- | The ARN of the resource that the indicated API operation was tested on.
    evalResourceName :: Core.Maybe Types.EvalResourceName,
    -- | A list of the statements in the input policies that determine the result for this scenario. Remember that even if multiple statements allow the operation on the resource, if only one statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
    matchedStatements :: Core.Maybe [Types.Statement],
    -- | A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when the resource in a simulation is "*", either explicitly, or when the @ResourceArns@ parameter blank. If you include a list of resources, then any missing context values are instead included under the @ResourceSpecificResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
    missingContextValues :: Core.Maybe [Types.ContextKeyNameType],
    -- | A structure that details how Organizations and its service control policies affect the results of the simulation. Only applies if the simulated user's account is part of an organization.
    organizationsDecisionDetail :: Core.Maybe Types.OrganizationsDecisionDetail,
    -- | Contains information about the effect that a permissions boundary has on a policy simulation when the boundary is applied to an IAM entity.
    permissionsBoundaryDecisionDetail :: Core.Maybe Types.PermissionsBoundaryDecisionDetail,
    -- | The individual results of the simulation of the API operation specified in EvalActionName on each resource.
    resourceSpecificResults :: Core.Maybe [Types.ResourceSpecificResult]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EvaluationResult' value with any optional fields omitted.
mkEvaluationResult ::
  -- | 'evalActionName'
  Types.EvalActionName ->
  -- | 'evalDecision'
  Types.PolicyEvaluationDecisionType ->
  EvaluationResult
mkEvaluationResult evalActionName evalDecision =
  EvaluationResult'
    { evalActionName,
      evalDecision,
      evalDecisionDetails = Core.Nothing,
      evalResourceName = Core.Nothing,
      matchedStatements = Core.Nothing,
      missingContextValues = Core.Nothing,
      organizationsDecisionDetail = Core.Nothing,
      permissionsBoundaryDecisionDetail = Core.Nothing,
      resourceSpecificResults = Core.Nothing
    }

-- | The name of the API operation tested on the indicated resource.
--
-- /Note:/ Consider using 'evalActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvalActionName :: Lens.Lens' EvaluationResult Types.EvalActionName
erEvalActionName = Lens.field @"evalActionName"
{-# DEPRECATED erEvalActionName "Use generic-lens or generic-optics with 'evalActionName' instead." #-}

-- | The result of the simulation.
--
-- /Note:/ Consider using 'evalDecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvalDecision :: Lens.Lens' EvaluationResult Types.PolicyEvaluationDecisionType
erEvalDecision = Lens.field @"evalDecision"
{-# DEPRECATED erEvalDecision "Use generic-lens or generic-optics with 'evalDecision' instead." #-}

-- | Additional details about the results of the cross-account evaluation decision. This parameter is populated for only cross-account simulations. It contains a brief summary of how each policy type contributes to the final evaluation decision.
--
-- If the simulation evaluates policies within the same account and includes a resource ARN, then the parameter is present but the response is empty. If the simulation evaluates policies within the same account and specifies all resources (@*@ ), then the parameter is not returned.
-- When you make a cross-account request, AWS evaluates the request in the trusting account and the trusted account. The request is allowed only if both evaluations return @true@ . For more information about how policies are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating Policies Within a Single Account> .
-- If an AWS Organizations SCP included in the evaluation denies access, the simulation ends. In this case, policy evaluation does not proceed any further and this parameter is not returned.
--
-- /Note:/ Consider using 'evalDecisionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvalDecisionDetails :: Lens.Lens' EvaluationResult (Core.Maybe (Core.HashMap Types.EvalDecisionSourceType Types.PolicyEvaluationDecisionType))
erEvalDecisionDetails = Lens.field @"evalDecisionDetails"
{-# DEPRECATED erEvalDecisionDetails "Use generic-lens or generic-optics with 'evalDecisionDetails' instead." #-}

-- | The ARN of the resource that the indicated API operation was tested on.
--
-- /Note:/ Consider using 'evalResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvalResourceName :: Lens.Lens' EvaluationResult (Core.Maybe Types.EvalResourceName)
erEvalResourceName = Lens.field @"evalResourceName"
{-# DEPRECATED erEvalResourceName "Use generic-lens or generic-optics with 'evalResourceName' instead." #-}

-- | A list of the statements in the input policies that determine the result for this scenario. Remember that even if multiple statements allow the operation on the resource, if only one statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
--
-- /Note:/ Consider using 'matchedStatements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erMatchedStatements :: Lens.Lens' EvaluationResult (Core.Maybe [Types.Statement])
erMatchedStatements = Lens.field @"matchedStatements"
{-# DEPRECATED erMatchedStatements "Use generic-lens or generic-optics with 'matchedStatements' instead." #-}

-- | A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when the resource in a simulation is "*", either explicitly, or when the @ResourceArns@ parameter blank. If you include a list of resources, then any missing context values are instead included under the @ResourceSpecificResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
--
-- /Note:/ Consider using 'missingContextValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erMissingContextValues :: Lens.Lens' EvaluationResult (Core.Maybe [Types.ContextKeyNameType])
erMissingContextValues = Lens.field @"missingContextValues"
{-# DEPRECATED erMissingContextValues "Use generic-lens or generic-optics with 'missingContextValues' instead." #-}

-- | A structure that details how Organizations and its service control policies affect the results of the simulation. Only applies if the simulated user's account is part of an organization.
--
-- /Note:/ Consider using 'organizationsDecisionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erOrganizationsDecisionDetail :: Lens.Lens' EvaluationResult (Core.Maybe Types.OrganizationsDecisionDetail)
erOrganizationsDecisionDetail = Lens.field @"organizationsDecisionDetail"
{-# DEPRECATED erOrganizationsDecisionDetail "Use generic-lens or generic-optics with 'organizationsDecisionDetail' instead." #-}

-- | Contains information about the effect that a permissions boundary has on a policy simulation when the boundary is applied to an IAM entity.
--
-- /Note:/ Consider using 'permissionsBoundaryDecisionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erPermissionsBoundaryDecisionDetail :: Lens.Lens' EvaluationResult (Core.Maybe Types.PermissionsBoundaryDecisionDetail)
erPermissionsBoundaryDecisionDetail = Lens.field @"permissionsBoundaryDecisionDetail"
{-# DEPRECATED erPermissionsBoundaryDecisionDetail "Use generic-lens or generic-optics with 'permissionsBoundaryDecisionDetail' instead." #-}

-- | The individual results of the simulation of the API operation specified in EvalActionName on each resource.
--
-- /Note:/ Consider using 'resourceSpecificResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erResourceSpecificResults :: Lens.Lens' EvaluationResult (Core.Maybe [Types.ResourceSpecificResult])
erResourceSpecificResults = Lens.field @"resourceSpecificResults"
{-# DEPRECATED erResourceSpecificResults "Use generic-lens or generic-optics with 'resourceSpecificResults' instead." #-}

instance Core.FromXML EvaluationResult where
  parseXML x =
    EvaluationResult'
      Core.<$> (x Core..@ "EvalActionName")
      Core.<*> (x Core..@ "EvalDecision")
      Core.<*> ( x Core..@? "EvalDecisionDetails"
                   Core..<@> Core.parseXMLMap "entry" "key" "value"
               )
      Core.<*> (x Core..@? "EvalResourceName")
      Core.<*> ( x Core..@? "MatchedStatements"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> ( x Core..@? "MissingContextValues"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "OrganizationsDecisionDetail")
      Core.<*> (x Core..@? "PermissionsBoundaryDecisionDetail")
      Core.<*> ( x Core..@? "ResourceSpecificResults"
                   Core..<@> Core.parseXMLList "member"
               )

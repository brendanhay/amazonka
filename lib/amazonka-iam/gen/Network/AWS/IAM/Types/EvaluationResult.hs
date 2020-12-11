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
    erMatchedStatements,
    erEvalDecisionDetails,
    erResourceSpecificResults,
    erEvalResourceName,
    erMissingContextValues,
    erPermissionsBoundaryDecisionDetail,
    erOrganizationsDecisionDetail,
    erEvalActionName,
    erEvalDecision,
  )
where

import Network.AWS.IAM.Types.OrganizationsDecisionDetail
import Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
import Network.AWS.IAM.Types.PolicyEvaluationDecisionType
import Network.AWS.IAM.Types.ResourceSpecificResult
import Network.AWS.IAM.Types.Statement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the results of a simulation.
--
-- This data type is used by the return parameter of @'SimulateCustomPolicy' @ and @'SimulatePrincipalPolicy' @ .
--
-- /See:/ 'mkEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { matchedStatements ::
      Lude.Maybe [Statement],
    evalDecisionDetails ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (PolicyEvaluationDecisionType)),
    resourceSpecificResults ::
      Lude.Maybe [ResourceSpecificResult],
    evalResourceName :: Lude.Maybe Lude.Text,
    missingContextValues :: Lude.Maybe [Lude.Text],
    permissionsBoundaryDecisionDetail ::
      Lude.Maybe PermissionsBoundaryDecisionDetail,
    organizationsDecisionDetail ::
      Lude.Maybe OrganizationsDecisionDetail,
    evalActionName :: Lude.Text,
    evalDecision :: PolicyEvaluationDecisionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- * 'evalActionName' - The name of the API operation tested on the indicated resource.
-- * 'evalDecision' - The result of the simulation.
-- * 'evalDecisionDetails' - Additional details about the results of the cross-account evaluation decision. This parameter is populated for only cross-account simulations. It contains a brief summary of how each policy type contributes to the final evaluation decision.
--
-- If the simulation evaluates policies within the same account and includes a resource ARN, then the parameter is present but the response is empty. If the simulation evaluates policies within the same account and specifies all resources (@*@ ), then the parameter is not returned.
-- When you make a cross-account request, AWS evaluates the request in the trusting account and the trusted account. The request is allowed only if both evaluations return @true@ . For more information about how policies are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating Policies Within a Single Account> .
-- If an AWS Organizations SCP included in the evaluation denies access, the simulation ends. In this case, policy evaluation does not proceed any further and this parameter is not returned.
-- * 'evalResourceName' - The ARN of the resource that the indicated API operation was tested on.
-- * 'matchedStatements' - A list of the statements in the input policies that determine the result for this scenario. Remember that even if multiple statements allow the operation on the resource, if only one statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
-- * 'missingContextValues' - A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when the resource in a simulation is "*", either explicitly, or when the @ResourceArns@ parameter blank. If you include a list of resources, then any missing context values are instead included under the @ResourceSpecificResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
-- * 'organizationsDecisionDetail' - A structure that details how Organizations and its service control policies affect the results of the simulation. Only applies if the simulated user's account is part of an organization.
-- * 'permissionsBoundaryDecisionDetail' - Contains information about the effect that a permissions boundary has on a policy simulation when the boundary is applied to an IAM entity.
-- * 'resourceSpecificResults' - The individual results of the simulation of the API operation specified in EvalActionName on each resource.
mkEvaluationResult ::
  -- | 'evalActionName'
  Lude.Text ->
  -- | 'evalDecision'
  PolicyEvaluationDecisionType ->
  EvaluationResult
mkEvaluationResult pEvalActionName_ pEvalDecision_ =
  EvaluationResult'
    { matchedStatements = Lude.Nothing,
      evalDecisionDetails = Lude.Nothing,
      resourceSpecificResults = Lude.Nothing,
      evalResourceName = Lude.Nothing,
      missingContextValues = Lude.Nothing,
      permissionsBoundaryDecisionDetail = Lude.Nothing,
      organizationsDecisionDetail = Lude.Nothing,
      evalActionName = pEvalActionName_,
      evalDecision = pEvalDecision_
    }

-- | A list of the statements in the input policies that determine the result for this scenario. Remember that even if multiple statements allow the operation on the resource, if only one statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
--
-- /Note:/ Consider using 'matchedStatements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erMatchedStatements :: Lens.Lens' EvaluationResult (Lude.Maybe [Statement])
erMatchedStatements = Lens.lens (matchedStatements :: EvaluationResult -> Lude.Maybe [Statement]) (\s a -> s {matchedStatements = a} :: EvaluationResult)
{-# DEPRECATED erMatchedStatements "Use generic-lens or generic-optics with 'matchedStatements' instead." #-}

-- | Additional details about the results of the cross-account evaluation decision. This parameter is populated for only cross-account simulations. It contains a brief summary of how each policy type contributes to the final evaluation decision.
--
-- If the simulation evaluates policies within the same account and includes a resource ARN, then the parameter is present but the response is empty. If the simulation evaluates policies within the same account and specifies all resources (@*@ ), then the parameter is not returned.
-- When you make a cross-account request, AWS evaluates the request in the trusting account and the trusted account. The request is allowed only if both evaluations return @true@ . For more information about how policies are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating Policies Within a Single Account> .
-- If an AWS Organizations SCP included in the evaluation denies access, the simulation ends. In this case, policy evaluation does not proceed any further and this parameter is not returned.
--
-- /Note:/ Consider using 'evalDecisionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvalDecisionDetails :: Lens.Lens' EvaluationResult (Lude.Maybe (Lude.HashMap Lude.Text (PolicyEvaluationDecisionType)))
erEvalDecisionDetails = Lens.lens (evalDecisionDetails :: EvaluationResult -> Lude.Maybe (Lude.HashMap Lude.Text (PolicyEvaluationDecisionType))) (\s a -> s {evalDecisionDetails = a} :: EvaluationResult)
{-# DEPRECATED erEvalDecisionDetails "Use generic-lens or generic-optics with 'evalDecisionDetails' instead." #-}

-- | The individual results of the simulation of the API operation specified in EvalActionName on each resource.
--
-- /Note:/ Consider using 'resourceSpecificResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erResourceSpecificResults :: Lens.Lens' EvaluationResult (Lude.Maybe [ResourceSpecificResult])
erResourceSpecificResults = Lens.lens (resourceSpecificResults :: EvaluationResult -> Lude.Maybe [ResourceSpecificResult]) (\s a -> s {resourceSpecificResults = a} :: EvaluationResult)
{-# DEPRECATED erResourceSpecificResults "Use generic-lens or generic-optics with 'resourceSpecificResults' instead." #-}

-- | The ARN of the resource that the indicated API operation was tested on.
--
-- /Note:/ Consider using 'evalResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvalResourceName :: Lens.Lens' EvaluationResult (Lude.Maybe Lude.Text)
erEvalResourceName = Lens.lens (evalResourceName :: EvaluationResult -> Lude.Maybe Lude.Text) (\s a -> s {evalResourceName = a} :: EvaluationResult)
{-# DEPRECATED erEvalResourceName "Use generic-lens or generic-optics with 'evalResourceName' instead." #-}

-- | A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when the resource in a simulation is "*", either explicitly, or when the @ResourceArns@ parameter blank. If you include a list of resources, then any missing context values are instead included under the @ResourceSpecificResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
--
-- /Note:/ Consider using 'missingContextValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erMissingContextValues :: Lens.Lens' EvaluationResult (Lude.Maybe [Lude.Text])
erMissingContextValues = Lens.lens (missingContextValues :: EvaluationResult -> Lude.Maybe [Lude.Text]) (\s a -> s {missingContextValues = a} :: EvaluationResult)
{-# DEPRECATED erMissingContextValues "Use generic-lens or generic-optics with 'missingContextValues' instead." #-}

-- | Contains information about the effect that a permissions boundary has on a policy simulation when the boundary is applied to an IAM entity.
--
-- /Note:/ Consider using 'permissionsBoundaryDecisionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erPermissionsBoundaryDecisionDetail :: Lens.Lens' EvaluationResult (Lude.Maybe PermissionsBoundaryDecisionDetail)
erPermissionsBoundaryDecisionDetail = Lens.lens (permissionsBoundaryDecisionDetail :: EvaluationResult -> Lude.Maybe PermissionsBoundaryDecisionDetail) (\s a -> s {permissionsBoundaryDecisionDetail = a} :: EvaluationResult)
{-# DEPRECATED erPermissionsBoundaryDecisionDetail "Use generic-lens or generic-optics with 'permissionsBoundaryDecisionDetail' instead." #-}

-- | A structure that details how Organizations and its service control policies affect the results of the simulation. Only applies if the simulated user's account is part of an organization.
--
-- /Note:/ Consider using 'organizationsDecisionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erOrganizationsDecisionDetail :: Lens.Lens' EvaluationResult (Lude.Maybe OrganizationsDecisionDetail)
erOrganizationsDecisionDetail = Lens.lens (organizationsDecisionDetail :: EvaluationResult -> Lude.Maybe OrganizationsDecisionDetail) (\s a -> s {organizationsDecisionDetail = a} :: EvaluationResult)
{-# DEPRECATED erOrganizationsDecisionDetail "Use generic-lens or generic-optics with 'organizationsDecisionDetail' instead." #-}

-- | The name of the API operation tested on the indicated resource.
--
-- /Note:/ Consider using 'evalActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvalActionName :: Lens.Lens' EvaluationResult Lude.Text
erEvalActionName = Lens.lens (evalActionName :: EvaluationResult -> Lude.Text) (\s a -> s {evalActionName = a} :: EvaluationResult)
{-# DEPRECATED erEvalActionName "Use generic-lens or generic-optics with 'evalActionName' instead." #-}

-- | The result of the simulation.
--
-- /Note:/ Consider using 'evalDecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erEvalDecision :: Lens.Lens' EvaluationResult PolicyEvaluationDecisionType
erEvalDecision = Lens.lens (evalDecision :: EvaluationResult -> PolicyEvaluationDecisionType) (\s a -> s {evalDecision = a} :: EvaluationResult)
{-# DEPRECATED erEvalDecision "Use generic-lens or generic-optics with 'evalDecision' instead." #-}

instance Lude.FromXML EvaluationResult where
  parseXML x =
    EvaluationResult'
      Lude.<$> ( x Lude..@? "MatchedStatements" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "EvalDecisionDetails" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
               )
      Lude.<*> ( x Lude..@? "ResourceSpecificResults" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "EvalResourceName")
      Lude.<*> ( x Lude..@? "MissingContextValues" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "PermissionsBoundaryDecisionDetail")
      Lude.<*> (x Lude..@? "OrganizationsDecisionDetail")
      Lude.<*> (x Lude..@ "EvalActionName")
      Lude.<*> (x Lude..@ "EvalDecision")

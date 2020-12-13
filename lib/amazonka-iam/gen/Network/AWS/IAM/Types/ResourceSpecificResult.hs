{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ResourceSpecificResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ResourceSpecificResult
  ( ResourceSpecificResult (..),

    -- * Smart constructor
    mkResourceSpecificResult,

    -- * Lenses
    rsrMatchedStatements,
    rsrEvalDecisionDetails,
    rsrEvalResourceDecision,
    rsrEvalResourceName,
    rsrMissingContextValues,
    rsrPermissionsBoundaryDecisionDetail,
  )
where

import Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
import Network.AWS.IAM.Types.PolicyEvaluationDecisionType
import Network.AWS.IAM.Types.Statement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the result of the simulation of a single API operation call on a single resource.
--
-- This data type is used by a member of the 'EvaluationResult' data type.
--
-- /See:/ 'mkResourceSpecificResult' smart constructor.
data ResourceSpecificResult = ResourceSpecificResult'
  { -- | A list of the statements in the input policies that determine the result for this part of the simulation. Remember that even if multiple statements allow the operation on the resource, if /any/ statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
    matchedStatements :: Lude.Maybe [Statement],
    -- | Additional details about the results of the evaluation decision on a single resource. This parameter is returned only for cross-account simulations. This parameter explains how each policy type contributes to the resource-specific evaluation decision.
    evalDecisionDetails :: Lude.Maybe (Lude.HashMap Lude.Text (PolicyEvaluationDecisionType)),
    -- | The result of the simulation of the simulated API operation on the resource specified in @EvalResourceName@ .
    evalResourceDecision :: PolicyEvaluationDecisionType,
    -- | The name of the simulated resource, in Amazon Resource Name (ARN) format.
    evalResourceName :: Lude.Text,
    -- | A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when a list of ARNs is included in the @ResourceArns@ parameter instead of "*". If you do not specify individual resources, by setting @ResourceArns@ to "*" or by not including the @ResourceArns@ parameter, then any missing context values are instead included under the @EvaluationResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
    missingContextValues :: Lude.Maybe [Lude.Text],
    -- | Contains information about the effect that a permissions boundary has on a policy simulation when that boundary is applied to an IAM entity.
    permissionsBoundaryDecisionDetail :: Lude.Maybe PermissionsBoundaryDecisionDetail
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceSpecificResult' with the minimum fields required to make a request.
--
-- * 'matchedStatements' - A list of the statements in the input policies that determine the result for this part of the simulation. Remember that even if multiple statements allow the operation on the resource, if /any/ statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
-- * 'evalDecisionDetails' - Additional details about the results of the evaluation decision on a single resource. This parameter is returned only for cross-account simulations. This parameter explains how each policy type contributes to the resource-specific evaluation decision.
-- * 'evalResourceDecision' - The result of the simulation of the simulated API operation on the resource specified in @EvalResourceName@ .
-- * 'evalResourceName' - The name of the simulated resource, in Amazon Resource Name (ARN) format.
-- * 'missingContextValues' - A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when a list of ARNs is included in the @ResourceArns@ parameter instead of "*". If you do not specify individual resources, by setting @ResourceArns@ to "*" or by not including the @ResourceArns@ parameter, then any missing context values are instead included under the @EvaluationResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
-- * 'permissionsBoundaryDecisionDetail' - Contains information about the effect that a permissions boundary has on a policy simulation when that boundary is applied to an IAM entity.
mkResourceSpecificResult ::
  -- | 'evalResourceDecision'
  PolicyEvaluationDecisionType ->
  -- | 'evalResourceName'
  Lude.Text ->
  ResourceSpecificResult
mkResourceSpecificResult pEvalResourceDecision_ pEvalResourceName_ =
  ResourceSpecificResult'
    { matchedStatements = Lude.Nothing,
      evalDecisionDetails = Lude.Nothing,
      evalResourceDecision = pEvalResourceDecision_,
      evalResourceName = pEvalResourceName_,
      missingContextValues = Lude.Nothing,
      permissionsBoundaryDecisionDetail = Lude.Nothing
    }

-- | A list of the statements in the input policies that determine the result for this part of the simulation. Remember that even if multiple statements allow the operation on the resource, if /any/ statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
--
-- /Note:/ Consider using 'matchedStatements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrMatchedStatements :: Lens.Lens' ResourceSpecificResult (Lude.Maybe [Statement])
rsrMatchedStatements = Lens.lens (matchedStatements :: ResourceSpecificResult -> Lude.Maybe [Statement]) (\s a -> s {matchedStatements = a} :: ResourceSpecificResult)
{-# DEPRECATED rsrMatchedStatements "Use generic-lens or generic-optics with 'matchedStatements' instead." #-}

-- | Additional details about the results of the evaluation decision on a single resource. This parameter is returned only for cross-account simulations. This parameter explains how each policy type contributes to the resource-specific evaluation decision.
--
-- /Note:/ Consider using 'evalDecisionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrEvalDecisionDetails :: Lens.Lens' ResourceSpecificResult (Lude.Maybe (Lude.HashMap Lude.Text (PolicyEvaluationDecisionType)))
rsrEvalDecisionDetails = Lens.lens (evalDecisionDetails :: ResourceSpecificResult -> Lude.Maybe (Lude.HashMap Lude.Text (PolicyEvaluationDecisionType))) (\s a -> s {evalDecisionDetails = a} :: ResourceSpecificResult)
{-# DEPRECATED rsrEvalDecisionDetails "Use generic-lens or generic-optics with 'evalDecisionDetails' instead." #-}

-- | The result of the simulation of the simulated API operation on the resource specified in @EvalResourceName@ .
--
-- /Note:/ Consider using 'evalResourceDecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrEvalResourceDecision :: Lens.Lens' ResourceSpecificResult PolicyEvaluationDecisionType
rsrEvalResourceDecision = Lens.lens (evalResourceDecision :: ResourceSpecificResult -> PolicyEvaluationDecisionType) (\s a -> s {evalResourceDecision = a} :: ResourceSpecificResult)
{-# DEPRECATED rsrEvalResourceDecision "Use generic-lens or generic-optics with 'evalResourceDecision' instead." #-}

-- | The name of the simulated resource, in Amazon Resource Name (ARN) format.
--
-- /Note:/ Consider using 'evalResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrEvalResourceName :: Lens.Lens' ResourceSpecificResult Lude.Text
rsrEvalResourceName = Lens.lens (evalResourceName :: ResourceSpecificResult -> Lude.Text) (\s a -> s {evalResourceName = a} :: ResourceSpecificResult)
{-# DEPRECATED rsrEvalResourceName "Use generic-lens or generic-optics with 'evalResourceName' instead." #-}

-- | A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when a list of ARNs is included in the @ResourceArns@ parameter instead of "*". If you do not specify individual resources, by setting @ResourceArns@ to "*" or by not including the @ResourceArns@ parameter, then any missing context values are instead included under the @EvaluationResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
--
-- /Note:/ Consider using 'missingContextValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrMissingContextValues :: Lens.Lens' ResourceSpecificResult (Lude.Maybe [Lude.Text])
rsrMissingContextValues = Lens.lens (missingContextValues :: ResourceSpecificResult -> Lude.Maybe [Lude.Text]) (\s a -> s {missingContextValues = a} :: ResourceSpecificResult)
{-# DEPRECATED rsrMissingContextValues "Use generic-lens or generic-optics with 'missingContextValues' instead." #-}

-- | Contains information about the effect that a permissions boundary has on a policy simulation when that boundary is applied to an IAM entity.
--
-- /Note:/ Consider using 'permissionsBoundaryDecisionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrPermissionsBoundaryDecisionDetail :: Lens.Lens' ResourceSpecificResult (Lude.Maybe PermissionsBoundaryDecisionDetail)
rsrPermissionsBoundaryDecisionDetail = Lens.lens (permissionsBoundaryDecisionDetail :: ResourceSpecificResult -> Lude.Maybe PermissionsBoundaryDecisionDetail) (\s a -> s {permissionsBoundaryDecisionDetail = a} :: ResourceSpecificResult)
{-# DEPRECATED rsrPermissionsBoundaryDecisionDetail "Use generic-lens or generic-optics with 'permissionsBoundaryDecisionDetail' instead." #-}

instance Lude.FromXML ResourceSpecificResult where
  parseXML x =
    ResourceSpecificResult'
      Lude.<$> ( x Lude..@? "MatchedStatements" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "EvalDecisionDetails" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
               )
      Lude.<*> (x Lude..@ "EvalResourceDecision")
      Lude.<*> (x Lude..@ "EvalResourceName")
      Lude.<*> ( x Lude..@? "MissingContextValues" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "PermissionsBoundaryDecisionDetail")

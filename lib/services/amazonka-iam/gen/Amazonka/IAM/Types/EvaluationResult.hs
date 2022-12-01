{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IAM.Types.EvaluationResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.EvaluationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types.OrganizationsDecisionDetail
import Amazonka.IAM.Types.PermissionsBoundaryDecisionDetail
import Amazonka.IAM.Types.PolicyEvaluationDecisionType
import Amazonka.IAM.Types.ResourceSpecificResult
import Amazonka.IAM.Types.Statement
import qualified Amazonka.Prelude as Prelude

-- | Contains the results of a simulation.
--
-- This data type is used by the return parameter of
-- @ SimulateCustomPolicy @ and @ SimulatePrincipalPolicy @.
--
-- /See:/ 'newEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | The ARN of the resource that the indicated API operation was tested on.
    evalResourceName :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the effect that a permissions boundary has on
    -- a policy simulation when the boundary is applied to an IAM entity.
    permissionsBoundaryDecisionDetail :: Prelude.Maybe PermissionsBoundaryDecisionDetail,
    -- | A list of context keys that are required by the included input policies
    -- but that were not provided by one of the input parameters. This list is
    -- used when the resource in a simulation is \"*\", either explicitly, or
    -- when the @ResourceArns@ parameter blank. If you include a list of
    -- resources, then any missing context values are instead included under
    -- the @ResourceSpecificResults@ section. To discover the context keys used
    -- by a set of policies, you can call GetContextKeysForCustomPolicy or
    -- GetContextKeysForPrincipalPolicy.
    missingContextValues :: Prelude.Maybe [Prelude.Text],
    -- | The individual results of the simulation of the API operation specified
    -- in EvalActionName on each resource.
    resourceSpecificResults :: Prelude.Maybe [ResourceSpecificResult],
    -- | Additional details about the results of the cross-account evaluation
    -- decision. This parameter is populated for only cross-account
    -- simulations. It contains a brief summary of how each policy type
    -- contributes to the final evaluation decision.
    --
    -- If the simulation evaluates policies within the same account and
    -- includes a resource ARN, then the parameter is present but the response
    -- is empty. If the simulation evaluates policies within the same account
    -- and specifies all resources (@*@), then the parameter is not returned.
    --
    -- When you make a cross-account request, Amazon Web Services evaluates the
    -- request in the trusting account and the trusted account. The request is
    -- allowed only if both evaluations return @true@. For more information
    -- about how policies are evaluated, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating policies within a single account>.
    --
    -- If an Organizations SCP included in the evaluation denies access, the
    -- simulation ends. In this case, policy evaluation does not proceed any
    -- further and this parameter is not returned.
    evalDecisionDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text PolicyEvaluationDecisionType),
    -- | A list of the statements in the input policies that determine the result
    -- for this scenario. Remember that even if multiple statements allow the
    -- operation on the resource, if only one statement denies that operation,
    -- then the explicit deny overrides any allow. In addition, the deny
    -- statement is the only entry included in the result.
    matchedStatements :: Prelude.Maybe [Statement],
    -- | A structure that details how Organizations and its service control
    -- policies affect the results of the simulation. Only applies if the
    -- simulated user\'s account is part of an organization.
    organizationsDecisionDetail :: Prelude.Maybe OrganizationsDecisionDetail,
    -- | The name of the API operation tested on the indicated resource.
    evalActionName :: Prelude.Text,
    -- | The result of the simulation.
    evalDecision :: PolicyEvaluationDecisionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evalResourceName', 'evaluationResult_evalResourceName' - The ARN of the resource that the indicated API operation was tested on.
--
-- 'permissionsBoundaryDecisionDetail', 'evaluationResult_permissionsBoundaryDecisionDetail' - Contains information about the effect that a permissions boundary has on
-- a policy simulation when the boundary is applied to an IAM entity.
--
-- 'missingContextValues', 'evaluationResult_missingContextValues' - A list of context keys that are required by the included input policies
-- but that were not provided by one of the input parameters. This list is
-- used when the resource in a simulation is \"*\", either explicitly, or
-- when the @ResourceArns@ parameter blank. If you include a list of
-- resources, then any missing context values are instead included under
-- the @ResourceSpecificResults@ section. To discover the context keys used
-- by a set of policies, you can call GetContextKeysForCustomPolicy or
-- GetContextKeysForPrincipalPolicy.
--
-- 'resourceSpecificResults', 'evaluationResult_resourceSpecificResults' - The individual results of the simulation of the API operation specified
-- in EvalActionName on each resource.
--
-- 'evalDecisionDetails', 'evaluationResult_evalDecisionDetails' - Additional details about the results of the cross-account evaluation
-- decision. This parameter is populated for only cross-account
-- simulations. It contains a brief summary of how each policy type
-- contributes to the final evaluation decision.
--
-- If the simulation evaluates policies within the same account and
-- includes a resource ARN, then the parameter is present but the response
-- is empty. If the simulation evaluates policies within the same account
-- and specifies all resources (@*@), then the parameter is not returned.
--
-- When you make a cross-account request, Amazon Web Services evaluates the
-- request in the trusting account and the trusted account. The request is
-- allowed only if both evaluations return @true@. For more information
-- about how policies are evaluated, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating policies within a single account>.
--
-- If an Organizations SCP included in the evaluation denies access, the
-- simulation ends. In this case, policy evaluation does not proceed any
-- further and this parameter is not returned.
--
-- 'matchedStatements', 'evaluationResult_matchedStatements' - A list of the statements in the input policies that determine the result
-- for this scenario. Remember that even if multiple statements allow the
-- operation on the resource, if only one statement denies that operation,
-- then the explicit deny overrides any allow. In addition, the deny
-- statement is the only entry included in the result.
--
-- 'organizationsDecisionDetail', 'evaluationResult_organizationsDecisionDetail' - A structure that details how Organizations and its service control
-- policies affect the results of the simulation. Only applies if the
-- simulated user\'s account is part of an organization.
--
-- 'evalActionName', 'evaluationResult_evalActionName' - The name of the API operation tested on the indicated resource.
--
-- 'evalDecision', 'evaluationResult_evalDecision' - The result of the simulation.
newEvaluationResult ::
  -- | 'evalActionName'
  Prelude.Text ->
  -- | 'evalDecision'
  PolicyEvaluationDecisionType ->
  EvaluationResult
newEvaluationResult pEvalActionName_ pEvalDecision_ =
  EvaluationResult'
    { evalResourceName =
        Prelude.Nothing,
      permissionsBoundaryDecisionDetail = Prelude.Nothing,
      missingContextValues = Prelude.Nothing,
      resourceSpecificResults = Prelude.Nothing,
      evalDecisionDetails = Prelude.Nothing,
      matchedStatements = Prelude.Nothing,
      organizationsDecisionDetail = Prelude.Nothing,
      evalActionName = pEvalActionName_,
      evalDecision = pEvalDecision_
    }

-- | The ARN of the resource that the indicated API operation was tested on.
evaluationResult_evalResourceName :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Text)
evaluationResult_evalResourceName = Lens.lens (\EvaluationResult' {evalResourceName} -> evalResourceName) (\s@EvaluationResult' {} a -> s {evalResourceName = a} :: EvaluationResult)

-- | Contains information about the effect that a permissions boundary has on
-- a policy simulation when the boundary is applied to an IAM entity.
evaluationResult_permissionsBoundaryDecisionDetail :: Lens.Lens' EvaluationResult (Prelude.Maybe PermissionsBoundaryDecisionDetail)
evaluationResult_permissionsBoundaryDecisionDetail = Lens.lens (\EvaluationResult' {permissionsBoundaryDecisionDetail} -> permissionsBoundaryDecisionDetail) (\s@EvaluationResult' {} a -> s {permissionsBoundaryDecisionDetail = a} :: EvaluationResult)

-- | A list of context keys that are required by the included input policies
-- but that were not provided by one of the input parameters. This list is
-- used when the resource in a simulation is \"*\", either explicitly, or
-- when the @ResourceArns@ parameter blank. If you include a list of
-- resources, then any missing context values are instead included under
-- the @ResourceSpecificResults@ section. To discover the context keys used
-- by a set of policies, you can call GetContextKeysForCustomPolicy or
-- GetContextKeysForPrincipalPolicy.
evaluationResult_missingContextValues :: Lens.Lens' EvaluationResult (Prelude.Maybe [Prelude.Text])
evaluationResult_missingContextValues = Lens.lens (\EvaluationResult' {missingContextValues} -> missingContextValues) (\s@EvaluationResult' {} a -> s {missingContextValues = a} :: EvaluationResult) Prelude.. Lens.mapping Lens.coerced

-- | The individual results of the simulation of the API operation specified
-- in EvalActionName on each resource.
evaluationResult_resourceSpecificResults :: Lens.Lens' EvaluationResult (Prelude.Maybe [ResourceSpecificResult])
evaluationResult_resourceSpecificResults = Lens.lens (\EvaluationResult' {resourceSpecificResults} -> resourceSpecificResults) (\s@EvaluationResult' {} a -> s {resourceSpecificResults = a} :: EvaluationResult) Prelude.. Lens.mapping Lens.coerced

-- | Additional details about the results of the cross-account evaluation
-- decision. This parameter is populated for only cross-account
-- simulations. It contains a brief summary of how each policy type
-- contributes to the final evaluation decision.
--
-- If the simulation evaluates policies within the same account and
-- includes a resource ARN, then the parameter is present but the response
-- is empty. If the simulation evaluates policies within the same account
-- and specifies all resources (@*@), then the parameter is not returned.
--
-- When you make a cross-account request, Amazon Web Services evaluates the
-- request in the trusting account and the trusted account. The request is
-- allowed only if both evaluations return @true@. For more information
-- about how policies are evaluated, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating policies within a single account>.
--
-- If an Organizations SCP included in the evaluation denies access, the
-- simulation ends. In this case, policy evaluation does not proceed any
-- further and this parameter is not returned.
evaluationResult_evalDecisionDetails :: Lens.Lens' EvaluationResult (Prelude.Maybe (Prelude.HashMap Prelude.Text PolicyEvaluationDecisionType))
evaluationResult_evalDecisionDetails = Lens.lens (\EvaluationResult' {evalDecisionDetails} -> evalDecisionDetails) (\s@EvaluationResult' {} a -> s {evalDecisionDetails = a} :: EvaluationResult) Prelude.. Lens.mapping Lens.coerced

-- | A list of the statements in the input policies that determine the result
-- for this scenario. Remember that even if multiple statements allow the
-- operation on the resource, if only one statement denies that operation,
-- then the explicit deny overrides any allow. In addition, the deny
-- statement is the only entry included in the result.
evaluationResult_matchedStatements :: Lens.Lens' EvaluationResult (Prelude.Maybe [Statement])
evaluationResult_matchedStatements = Lens.lens (\EvaluationResult' {matchedStatements} -> matchedStatements) (\s@EvaluationResult' {} a -> s {matchedStatements = a} :: EvaluationResult) Prelude.. Lens.mapping Lens.coerced

-- | A structure that details how Organizations and its service control
-- policies affect the results of the simulation. Only applies if the
-- simulated user\'s account is part of an organization.
evaluationResult_organizationsDecisionDetail :: Lens.Lens' EvaluationResult (Prelude.Maybe OrganizationsDecisionDetail)
evaluationResult_organizationsDecisionDetail = Lens.lens (\EvaluationResult' {organizationsDecisionDetail} -> organizationsDecisionDetail) (\s@EvaluationResult' {} a -> s {organizationsDecisionDetail = a} :: EvaluationResult)

-- | The name of the API operation tested on the indicated resource.
evaluationResult_evalActionName :: Lens.Lens' EvaluationResult Prelude.Text
evaluationResult_evalActionName = Lens.lens (\EvaluationResult' {evalActionName} -> evalActionName) (\s@EvaluationResult' {} a -> s {evalActionName = a} :: EvaluationResult)

-- | The result of the simulation.
evaluationResult_evalDecision :: Lens.Lens' EvaluationResult PolicyEvaluationDecisionType
evaluationResult_evalDecision = Lens.lens (\EvaluationResult' {evalDecision} -> evalDecision) (\s@EvaluationResult' {} a -> s {evalDecision = a} :: EvaluationResult)

instance Core.FromXML EvaluationResult where
  parseXML x =
    EvaluationResult'
      Prelude.<$> (x Core..@? "EvalResourceName")
      Prelude.<*> (x Core..@? "PermissionsBoundaryDecisionDetail")
      Prelude.<*> ( x Core..@? "MissingContextValues"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "ResourceSpecificResults"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "EvalDecisionDetails"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                  )
      Prelude.<*> ( x Core..@? "MatchedStatements"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "OrganizationsDecisionDetail")
      Prelude.<*> (x Core..@ "EvalActionName")
      Prelude.<*> (x Core..@ "EvalDecision")

instance Prelude.Hashable EvaluationResult where
  hashWithSalt _salt EvaluationResult' {..} =
    _salt `Prelude.hashWithSalt` evalResourceName
      `Prelude.hashWithSalt` permissionsBoundaryDecisionDetail
      `Prelude.hashWithSalt` missingContextValues
      `Prelude.hashWithSalt` resourceSpecificResults
      `Prelude.hashWithSalt` evalDecisionDetails
      `Prelude.hashWithSalt` matchedStatements
      `Prelude.hashWithSalt` organizationsDecisionDetail
      `Prelude.hashWithSalt` evalActionName
      `Prelude.hashWithSalt` evalDecision

instance Prelude.NFData EvaluationResult where
  rnf EvaluationResult' {..} =
    Prelude.rnf evalResourceName
      `Prelude.seq` Prelude.rnf permissionsBoundaryDecisionDetail
      `Prelude.seq` Prelude.rnf missingContextValues
      `Prelude.seq` Prelude.rnf resourceSpecificResults
      `Prelude.seq` Prelude.rnf evalDecisionDetails
      `Prelude.seq` Prelude.rnf matchedStatements
      `Prelude.seq` Prelude.rnf organizationsDecisionDetail
      `Prelude.seq` Prelude.rnf evalActionName
      `Prelude.seq` Prelude.rnf evalDecision

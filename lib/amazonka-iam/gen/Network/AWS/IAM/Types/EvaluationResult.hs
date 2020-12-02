{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.EvaluationResult where

import Network.AWS.IAM.Types.OrganizationsDecisionDetail
import Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
import Network.AWS.IAM.Types.PolicyEvaluationDecisionType
import Network.AWS.IAM.Types.ResourceSpecificResult
import Network.AWS.IAM.Types.Statement
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the results of a simulation.
--
--
-- This data type is used by the return parameter of @'SimulateCustomPolicy' @ and @'SimulatePrincipalPolicy' @ .
--
--
-- /See:/ 'evaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { _erMatchedStatements ::
      !(Maybe [Statement]),
    _erEvalDecisionDetails ::
      !(Maybe (Map Text (PolicyEvaluationDecisionType))),
    _erResourceSpecificResults ::
      !(Maybe [ResourceSpecificResult]),
    _erEvalResourceName :: !(Maybe Text),
    _erMissingContextValues :: !(Maybe [Text]),
    _erPermissionsBoundaryDecisionDetail ::
      !(Maybe PermissionsBoundaryDecisionDetail),
    _erOrganizationsDecisionDetail ::
      !(Maybe OrganizationsDecisionDetail),
    _erEvalActionName :: !Text,
    _erEvalDecision :: !PolicyEvaluationDecisionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erMatchedStatements' - A list of the statements in the input policies that determine the result for this scenario. Remember that even if multiple statements allow the operation on the resource, if only one statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
--
-- * 'erEvalDecisionDetails' - Additional details about the results of the cross-account evaluation decision. This parameter is populated for only cross-account simulations. It contains a brief summary of how each policy type contributes to the final evaluation decision. If the simulation evaluates policies within the same account and includes a resource ARN, then the parameter is present but the response is empty. If the simulation evaluates policies within the same account and specifies all resources (@*@ ), then the parameter is not returned. When you make a cross-account request, AWS evaluates the request in the trusting account and the trusted account. The request is allowed only if both evaluations return @true@ . For more information about how policies are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating Policies Within a Single Account> . If an AWS Organizations SCP included in the evaluation denies access, the simulation ends. In this case, policy evaluation does not proceed any further and this parameter is not returned.
--
-- * 'erResourceSpecificResults' - The individual results of the simulation of the API operation specified in EvalActionName on each resource.
--
-- * 'erEvalResourceName' - The ARN of the resource that the indicated API operation was tested on.
--
-- * 'erMissingContextValues' - A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when the resource in a simulation is "*", either explicitly, or when the @ResourceArns@ parameter blank. If you include a list of resources, then any missing context values are instead included under the @ResourceSpecificResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
--
-- * 'erPermissionsBoundaryDecisionDetail' - Contains information about the effect that a permissions boundary has on a policy simulation when the boundary is applied to an IAM entity.
--
-- * 'erOrganizationsDecisionDetail' - A structure that details how Organizations and its service control policies affect the results of the simulation. Only applies if the simulated user's account is part of an organization.
--
-- * 'erEvalActionName' - The name of the API operation tested on the indicated resource.
--
-- * 'erEvalDecision' - The result of the simulation.
evaluationResult ::
  -- | 'erEvalActionName'
  Text ->
  -- | 'erEvalDecision'
  PolicyEvaluationDecisionType ->
  EvaluationResult
evaluationResult pEvalActionName_ pEvalDecision_ =
  EvaluationResult'
    { _erMatchedStatements = Nothing,
      _erEvalDecisionDetails = Nothing,
      _erResourceSpecificResults = Nothing,
      _erEvalResourceName = Nothing,
      _erMissingContextValues = Nothing,
      _erPermissionsBoundaryDecisionDetail = Nothing,
      _erOrganizationsDecisionDetail = Nothing,
      _erEvalActionName = pEvalActionName_,
      _erEvalDecision = pEvalDecision_
    }

-- | A list of the statements in the input policies that determine the result for this scenario. Remember that even if multiple statements allow the operation on the resource, if only one statement denies that operation, then the explicit deny overrides any allow. In addition, the deny statement is the only entry included in the result.
erMatchedStatements :: Lens' EvaluationResult [Statement]
erMatchedStatements = lens _erMatchedStatements (\s a -> s {_erMatchedStatements = a}) . _Default . _Coerce

-- | Additional details about the results of the cross-account evaluation decision. This parameter is populated for only cross-account simulations. It contains a brief summary of how each policy type contributes to the final evaluation decision. If the simulation evaluates policies within the same account and includes a resource ARN, then the parameter is present but the response is empty. If the simulation evaluates policies within the same account and specifies all resources (@*@ ), then the parameter is not returned. When you make a cross-account request, AWS evaluates the request in the trusting account and the trusted account. The request is allowed only if both evaluations return @true@ . For more information about how policies are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating Policies Within a Single Account> . If an AWS Organizations SCP included in the evaluation denies access, the simulation ends. In this case, policy evaluation does not proceed any further and this parameter is not returned.
erEvalDecisionDetails :: Lens' EvaluationResult (HashMap Text (PolicyEvaluationDecisionType))
erEvalDecisionDetails = lens _erEvalDecisionDetails (\s a -> s {_erEvalDecisionDetails = a}) . _Default . _Map

-- | The individual results of the simulation of the API operation specified in EvalActionName on each resource.
erResourceSpecificResults :: Lens' EvaluationResult [ResourceSpecificResult]
erResourceSpecificResults = lens _erResourceSpecificResults (\s a -> s {_erResourceSpecificResults = a}) . _Default . _Coerce

-- | The ARN of the resource that the indicated API operation was tested on.
erEvalResourceName :: Lens' EvaluationResult (Maybe Text)
erEvalResourceName = lens _erEvalResourceName (\s a -> s {_erEvalResourceName = a})

-- | A list of context keys that are required by the included input policies but that were not provided by one of the input parameters. This list is used when the resource in a simulation is "*", either explicitly, or when the @ResourceArns@ parameter blank. If you include a list of resources, then any missing context values are instead included under the @ResourceSpecificResults@ section. To discover the context keys used by a set of policies, you can call 'GetContextKeysForCustomPolicy' or 'GetContextKeysForPrincipalPolicy' .
erMissingContextValues :: Lens' EvaluationResult [Text]
erMissingContextValues = lens _erMissingContextValues (\s a -> s {_erMissingContextValues = a}) . _Default . _Coerce

-- | Contains information about the effect that a permissions boundary has on a policy simulation when the boundary is applied to an IAM entity.
erPermissionsBoundaryDecisionDetail :: Lens' EvaluationResult (Maybe PermissionsBoundaryDecisionDetail)
erPermissionsBoundaryDecisionDetail = lens _erPermissionsBoundaryDecisionDetail (\s a -> s {_erPermissionsBoundaryDecisionDetail = a})

-- | A structure that details how Organizations and its service control policies affect the results of the simulation. Only applies if the simulated user's account is part of an organization.
erOrganizationsDecisionDetail :: Lens' EvaluationResult (Maybe OrganizationsDecisionDetail)
erOrganizationsDecisionDetail = lens _erOrganizationsDecisionDetail (\s a -> s {_erOrganizationsDecisionDetail = a})

-- | The name of the API operation tested on the indicated resource.
erEvalActionName :: Lens' EvaluationResult Text
erEvalActionName = lens _erEvalActionName (\s a -> s {_erEvalActionName = a})

-- | The result of the simulation.
erEvalDecision :: Lens' EvaluationResult PolicyEvaluationDecisionType
erEvalDecision = lens _erEvalDecision (\s a -> s {_erEvalDecision = a})

instance FromXML EvaluationResult where
  parseXML x =
    EvaluationResult'
      <$> ( x .@? "MatchedStatements" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> ( x .@? "EvalDecisionDetails" .!@ mempty
              >>= may (parseXMLMap "entry" "key" "value")
          )
      <*> ( x .@? "ResourceSpecificResults" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "EvalResourceName")
      <*> ( x .@? "MissingContextValues" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "PermissionsBoundaryDecisionDetail")
      <*> (x .@? "OrganizationsDecisionDetail")
      <*> (x .@ "EvalActionName")
      <*> (x .@ "EvalDecision")

instance Hashable EvaluationResult

instance NFData EvaluationResult

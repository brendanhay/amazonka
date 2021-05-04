{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Lens
  ( -- * Operations

    -- ** ListAccountsForParent
    listAccountsForParent_nextToken,
    listAccountsForParent_maxResults,
    listAccountsForParent_parentId,
    listAccountsForParentResponse_nextToken,
    listAccountsForParentResponse_accounts,
    listAccountsForParentResponse_httpStatus,

    -- ** CreateOrganization
    createOrganization_featureSet,
    createOrganizationResponse_organization,
    createOrganizationResponse_httpStatus,

    -- ** CreatePolicy
    createPolicy_tags,
    createPolicy_content,
    createPolicy_description,
    createPolicy_name,
    createPolicy_type,
    createPolicyResponse_policy,
    createPolicyResponse_httpStatus,

    -- ** DisablePolicyType
    disablePolicyType_rootId,
    disablePolicyType_policyType,
    disablePolicyTypeResponse_root,
    disablePolicyTypeResponse_httpStatus,

    -- ** DescribeCreateAccountStatus
    describeCreateAccountStatus_createAccountRequestId,
    describeCreateAccountStatusResponse_createAccountStatus,
    describeCreateAccountStatusResponse_httpStatus,

    -- ** ListPolicies
    listPolicies_nextToken,
    listPolicies_maxResults,
    listPolicies_filter,
    listPoliciesResponse_nextToken,
    listPoliciesResponse_policies,
    listPoliciesResponse_httpStatus,

    -- ** ListHandshakesForAccount
    listHandshakesForAccount_nextToken,
    listHandshakesForAccount_maxResults,
    listHandshakesForAccount_filter,
    listHandshakesForAccountResponse_handshakes,
    listHandshakesForAccountResponse_nextToken,
    listHandshakesForAccountResponse_httpStatus,

    -- ** ListChildren
    listChildren_nextToken,
    listChildren_maxResults,
    listChildren_parentId,
    listChildren_childType,
    listChildrenResponse_nextToken,
    listChildrenResponse_children,
    listChildrenResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicy_policyId,

    -- ** EnablePolicyType
    enablePolicyType_rootId,
    enablePolicyType_policyType,
    enablePolicyTypeResponse_root,
    enablePolicyTypeResponse_httpStatus,

    -- ** UpdatePolicy
    updatePolicy_name,
    updatePolicy_content,
    updatePolicy_description,
    updatePolicy_policyId,
    updatePolicyResponse_policy,
    updatePolicyResponse_httpStatus,

    -- ** ListAWSServiceAccessForOrganization
    listAWSServiceAccessForOrganization_nextToken,
    listAWSServiceAccessForOrganization_maxResults,
    listAWSServiceAccessForOrganizationResponse_nextToken,
    listAWSServiceAccessForOrganizationResponse_enabledServicePrincipals,
    listAWSServiceAccessForOrganizationResponse_httpStatus,

    -- ** DescribeOrganization
    describeOrganizationResponse_organization,
    describeOrganizationResponse_httpStatus,

    -- ** ListCreateAccountStatus
    listCreateAccountStatus_nextToken,
    listCreateAccountStatus_states,
    listCreateAccountStatus_maxResults,
    listCreateAccountStatusResponse_nextToken,
    listCreateAccountStatusResponse_createAccountStatuses,
    listCreateAccountStatusResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceId,
    untagResource_tagKeys,

    -- ** ListAccounts
    listAccounts_nextToken,
    listAccounts_maxResults,
    listAccountsResponse_nextToken,
    listAccountsResponse_accounts,
    listAccountsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceId,
    tagResource_tags,

    -- ** EnableAWSServiceAccess
    enableAWSServiceAccess_servicePrincipal,

    -- ** DescribeOrganizationalUnit
    describeOrganizationalUnit_organizationalUnitId,
    describeOrganizationalUnitResponse_organizationalUnit,
    describeOrganizationalUnitResponse_httpStatus,

    -- ** ListDelegatedServicesForAccount
    listDelegatedServicesForAccount_nextToken,
    listDelegatedServicesForAccount_maxResults,
    listDelegatedServicesForAccount_accountId,
    listDelegatedServicesForAccountResponse_nextToken,
    listDelegatedServicesForAccountResponse_delegatedServices,
    listDelegatedServicesForAccountResponse_httpStatus,

    -- ** RemoveAccountFromOrganization
    removeAccountFromOrganization_accountId,

    -- ** CreateGovCloudAccount
    createGovCloudAccount_roleName,
    createGovCloudAccount_iamUserAccessToBilling,
    createGovCloudAccount_tags,
    createGovCloudAccount_email,
    createGovCloudAccount_accountName,
    createGovCloudAccountResponse_createAccountStatus,
    createGovCloudAccountResponse_httpStatus,

    -- ** DeleteOrganization

    -- ** ListRoots
    listRoots_nextToken,
    listRoots_maxResults,
    listRootsResponse_nextToken,
    listRootsResponse_roots,
    listRootsResponse_httpStatus,

    -- ** EnableAllFeatures
    enableAllFeaturesResponse_handshake,
    enableAllFeaturesResponse_httpStatus,

    -- ** AcceptHandshake
    acceptHandshake_handshakeId,
    acceptHandshakeResponse_handshake,
    acceptHandshakeResponse_httpStatus,

    -- ** DetachPolicy
    detachPolicy_policyId,
    detachPolicy_targetId,

    -- ** CreateOrganizationalUnit
    createOrganizationalUnit_tags,
    createOrganizationalUnit_parentId,
    createOrganizationalUnit_name,
    createOrganizationalUnitResponse_organizationalUnit,
    createOrganizationalUnitResponse_httpStatus,

    -- ** DescribeAccount
    describeAccount_accountId,
    describeAccountResponse_account,
    describeAccountResponse_httpStatus,

    -- ** ListDelegatedAdministrators
    listDelegatedAdministrators_servicePrincipal,
    listDelegatedAdministrators_nextToken,
    listDelegatedAdministrators_maxResults,
    listDelegatedAdministratorsResponse_nextToken,
    listDelegatedAdministratorsResponse_delegatedAdministrators,
    listDelegatedAdministratorsResponse_httpStatus,

    -- ** UpdateOrganizationalUnit
    updateOrganizationalUnit_name,
    updateOrganizationalUnit_organizationalUnitId,
    updateOrganizationalUnitResponse_organizationalUnit,
    updateOrganizationalUnitResponse_httpStatus,

    -- ** DeleteOrganizationalUnit
    deleteOrganizationalUnit_organizationalUnitId,

    -- ** CancelHandshake
    cancelHandshake_handshakeId,
    cancelHandshakeResponse_handshake,
    cancelHandshakeResponse_httpStatus,

    -- ** RegisterDelegatedAdministrator
    registerDelegatedAdministrator_accountId,
    registerDelegatedAdministrator_servicePrincipal,

    -- ** ListHandshakesForOrganization
    listHandshakesForOrganization_nextToken,
    listHandshakesForOrganization_maxResults,
    listHandshakesForOrganization_filter,
    listHandshakesForOrganizationResponse_handshakes,
    listHandshakesForOrganizationResponse_nextToken,
    listHandshakesForOrganizationResponse_httpStatus,

    -- ** ListPoliciesForTarget
    listPoliciesForTarget_nextToken,
    listPoliciesForTarget_maxResults,
    listPoliciesForTarget_targetId,
    listPoliciesForTarget_filter,
    listPoliciesForTargetResponse_nextToken,
    listPoliciesForTargetResponse_policies,
    listPoliciesForTargetResponse_httpStatus,

    -- ** ListOrganizationalUnitsForParent
    listOrganizationalUnitsForParent_nextToken,
    listOrganizationalUnitsForParent_maxResults,
    listOrganizationalUnitsForParent_parentId,
    listOrganizationalUnitsForParentResponse_nextToken,
    listOrganizationalUnitsForParentResponse_organizationalUnits,
    listOrganizationalUnitsForParentResponse_httpStatus,

    -- ** ListTargetsForPolicy
    listTargetsForPolicy_nextToken,
    listTargetsForPolicy_maxResults,
    listTargetsForPolicy_policyId,
    listTargetsForPolicyResponse_nextToken,
    listTargetsForPolicyResponse_targets,
    listTargetsForPolicyResponse_httpStatus,

    -- ** AttachPolicy
    attachPolicy_policyId,
    attachPolicy_targetId,

    -- ** DeclineHandshake
    declineHandshake_handshakeId,
    declineHandshakeResponse_handshake,
    declineHandshakeResponse_httpStatus,

    -- ** DescribeEffectivePolicy
    describeEffectivePolicy_targetId,
    describeEffectivePolicy_policyType,
    describeEffectivePolicyResponse_effectivePolicy,
    describeEffectivePolicyResponse_httpStatus,

    -- ** DeregisterDelegatedAdministrator
    deregisterDelegatedAdministrator_accountId,
    deregisterDelegatedAdministrator_servicePrincipal,

    -- ** MoveAccount
    moveAccount_accountId,
    moveAccount_sourceParentId,
    moveAccount_destinationParentId,

    -- ** InviteAccountToOrganization
    inviteAccountToOrganization_notes,
    inviteAccountToOrganization_tags,
    inviteAccountToOrganization_target,
    inviteAccountToOrganizationResponse_handshake,
    inviteAccountToOrganizationResponse_httpStatus,

    -- ** LeaveOrganization

    -- ** DisableAWSServiceAccess
    disableAWSServiceAccess_servicePrincipal,

    -- ** ListParents
    listParents_nextToken,
    listParents_maxResults,
    listParents_childId,
    listParentsResponse_parents,
    listParentsResponse_nextToken,
    listParentsResponse_httpStatus,

    -- ** DescribePolicy
    describePolicy_policyId,
    describePolicyResponse_policy,
    describePolicyResponse_httpStatus,

    -- ** CreateAccount
    createAccount_roleName,
    createAccount_iamUserAccessToBilling,
    createAccount_tags,
    createAccount_email,
    createAccount_accountName,
    createAccountResponse_createAccountStatus,
    createAccountResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeHandshake
    describeHandshake_handshakeId,
    describeHandshakeResponse_handshake,
    describeHandshakeResponse_httpStatus,

    -- * Types

    -- ** Account
    account_status,
    account_joinedMethod,
    account_arn,
    account_joinedTimestamp,
    account_id,
    account_name,
    account_email,

    -- ** Child
    child_id,
    child_type,

    -- ** CreateAccountStatus
    createAccountStatus_accountId,
    createAccountStatus_requestedTimestamp,
    createAccountStatus_accountName,
    createAccountStatus_govCloudAccountId,
    createAccountStatus_completedTimestamp,
    createAccountStatus_id,
    createAccountStatus_state,
    createAccountStatus_failureReason,

    -- ** DelegatedAdministrator
    delegatedAdministrator_status,
    delegatedAdministrator_joinedMethod,
    delegatedAdministrator_arn,
    delegatedAdministrator_joinedTimestamp,
    delegatedAdministrator_id,
    delegatedAdministrator_name,
    delegatedAdministrator_email,
    delegatedAdministrator_delegationEnabledDate,

    -- ** DelegatedService
    delegatedService_servicePrincipal,
    delegatedService_delegationEnabledDate,

    -- ** EffectivePolicy
    effectivePolicy_targetId,
    effectivePolicy_policyContent,
    effectivePolicy_policyType,
    effectivePolicy_lastUpdatedTimestamp,

    -- ** EnabledServicePrincipal
    enabledServicePrincipal_servicePrincipal,
    enabledServicePrincipal_dateEnabled,

    -- ** Handshake
    handshake_requestedTimestamp,
    handshake_parties,
    handshake_arn,
    handshake_id,
    handshake_state,
    handshake_resources,
    handshake_action,
    handshake_expirationTimestamp,

    -- ** HandshakeFilter
    handshakeFilter_actionType,
    handshakeFilter_parentHandshakeId,

    -- ** HandshakeParty
    handshakeParty_id,
    handshakeParty_type,

    -- ** HandshakeResource
    handshakeResource_resources,
    handshakeResource_value,
    handshakeResource_type,

    -- ** Organization
    organization_masterAccountEmail,
    organization_featureSet,
    organization_masterAccountArn,
    organization_arn,
    organization_id,
    organization_masterAccountId,
    organization_availablePolicyTypes,

    -- ** OrganizationalUnit
    organizationalUnit_arn,
    organizationalUnit_id,
    organizationalUnit_name,

    -- ** Parent
    parent_id,
    parent_type,

    -- ** Policy
    policy_policySummary,
    policy_content,

    -- ** PolicySummary
    policySummary_arn,
    policySummary_id,
    policySummary_name,
    policySummary_description,
    policySummary_type,
    policySummary_awsManaged,

    -- ** PolicyTargetSummary
    policyTargetSummary_targetId,
    policyTargetSummary_arn,
    policyTargetSummary_name,
    policyTargetSummary_type,

    -- ** PolicyTypeSummary
    policyTypeSummary_status,
    policyTypeSummary_type,

    -- ** Root
    root_policyTypes,
    root_arn,
    root_id,
    root_name,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.Organizations.AcceptHandshake
import Network.AWS.Organizations.AttachPolicy
import Network.AWS.Organizations.CancelHandshake
import Network.AWS.Organizations.CreateAccount
import Network.AWS.Organizations.CreateGovCloudAccount
import Network.AWS.Organizations.CreateOrganization
import Network.AWS.Organizations.CreateOrganizationalUnit
import Network.AWS.Organizations.CreatePolicy
import Network.AWS.Organizations.DeclineHandshake
import Network.AWS.Organizations.DeleteOrganization
import Network.AWS.Organizations.DeleteOrganizationalUnit
import Network.AWS.Organizations.DeletePolicy
import Network.AWS.Organizations.DeregisterDelegatedAdministrator
import Network.AWS.Organizations.DescribeAccount
import Network.AWS.Organizations.DescribeCreateAccountStatus
import Network.AWS.Organizations.DescribeEffectivePolicy
import Network.AWS.Organizations.DescribeHandshake
import Network.AWS.Organizations.DescribeOrganization
import Network.AWS.Organizations.DescribeOrganizationalUnit
import Network.AWS.Organizations.DescribePolicy
import Network.AWS.Organizations.DetachPolicy
import Network.AWS.Organizations.DisableAWSServiceAccess
import Network.AWS.Organizations.DisablePolicyType
import Network.AWS.Organizations.EnableAWSServiceAccess
import Network.AWS.Organizations.EnableAllFeatures
import Network.AWS.Organizations.EnablePolicyType
import Network.AWS.Organizations.InviteAccountToOrganization
import Network.AWS.Organizations.LeaveOrganization
import Network.AWS.Organizations.ListAWSServiceAccessForOrganization
import Network.AWS.Organizations.ListAccounts
import Network.AWS.Organizations.ListAccountsForParent
import Network.AWS.Organizations.ListChildren
import Network.AWS.Organizations.ListCreateAccountStatus
import Network.AWS.Organizations.ListDelegatedAdministrators
import Network.AWS.Organizations.ListDelegatedServicesForAccount
import Network.AWS.Organizations.ListHandshakesForAccount
import Network.AWS.Organizations.ListHandshakesForOrganization
import Network.AWS.Organizations.ListOrganizationalUnitsForParent
import Network.AWS.Organizations.ListParents
import Network.AWS.Organizations.ListPolicies
import Network.AWS.Organizations.ListPoliciesForTarget
import Network.AWS.Organizations.ListRoots
import Network.AWS.Organizations.ListTagsForResource
import Network.AWS.Organizations.ListTargetsForPolicy
import Network.AWS.Organizations.MoveAccount
import Network.AWS.Organizations.RegisterDelegatedAdministrator
import Network.AWS.Organizations.RemoveAccountFromOrganization
import Network.AWS.Organizations.TagResource
import Network.AWS.Organizations.Types.Account
import Network.AWS.Organizations.Types.Child
import Network.AWS.Organizations.Types.CreateAccountStatus
import Network.AWS.Organizations.Types.DelegatedAdministrator
import Network.AWS.Organizations.Types.DelegatedService
import Network.AWS.Organizations.Types.EffectivePolicy
import Network.AWS.Organizations.Types.EnabledServicePrincipal
import Network.AWS.Organizations.Types.Handshake
import Network.AWS.Organizations.Types.HandshakeFilter
import Network.AWS.Organizations.Types.HandshakeParty
import Network.AWS.Organizations.Types.HandshakeResource
import Network.AWS.Organizations.Types.Organization
import Network.AWS.Organizations.Types.OrganizationalUnit
import Network.AWS.Organizations.Types.Parent
import Network.AWS.Organizations.Types.Policy
import Network.AWS.Organizations.Types.PolicySummary
import Network.AWS.Organizations.Types.PolicyTargetSummary
import Network.AWS.Organizations.Types.PolicyTypeSummary
import Network.AWS.Organizations.Types.Root
import Network.AWS.Organizations.Types.Tag
import Network.AWS.Organizations.UntagResource
import Network.AWS.Organizations.UpdateOrganizationalUnit
import Network.AWS.Organizations.UpdatePolicy

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

    -- ** ListHandshakesForAccount
    listHandshakesForAccount_nextToken,
    listHandshakesForAccount_filter,
    listHandshakesForAccount_maxResults,
    listHandshakesForAccountResponse_handshakes,
    listHandshakesForAccountResponse_nextToken,
    listHandshakesForAccountResponse_httpStatus,

    -- ** DescribeAccount
    describeAccount_accountId,
    describeAccountResponse_account,
    describeAccountResponse_httpStatus,

    -- ** ListPolicies
    listPolicies_nextToken,
    listPolicies_maxResults,
    listPolicies_filter,
    listPoliciesResponse_nextToken,
    listPoliciesResponse_policies,
    listPoliciesResponse_httpStatus,

    -- ** CreatePolicy
    createPolicy_tags,
    createPolicy_content,
    createPolicy_description,
    createPolicy_name,
    createPolicy_type,
    createPolicyResponse_policy,
    createPolicyResponse_httpStatus,

    -- ** ListRoots
    listRoots_nextToken,
    listRoots_maxResults,
    listRootsResponse_roots,
    listRootsResponse_nextToken,
    listRootsResponse_httpStatus,

    -- ** AcceptHandshake
    acceptHandshake_handshakeId,
    acceptHandshakeResponse_handshake,
    acceptHandshakeResponse_httpStatus,

    -- ** CreateOrganization
    createOrganization_featureSet,
    createOrganizationResponse_organization,
    createOrganizationResponse_httpStatus,

    -- ** EnableAllFeatures
    enableAllFeaturesResponse_handshake,
    enableAllFeaturesResponse_httpStatus,

    -- ** DeleteOrganization

    -- ** DescribeHandshake
    describeHandshake_handshakeId,
    describeHandshakeResponse_handshake,
    describeHandshakeResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribePolicy
    describePolicy_policyId,
    describePolicyResponse_policy,
    describePolicyResponse_httpStatus,

    -- ** ListDelegatedServicesForAccount
    listDelegatedServicesForAccount_nextToken,
    listDelegatedServicesForAccount_maxResults,
    listDelegatedServicesForAccount_accountId,
    listDelegatedServicesForAccountResponse_delegatedServices,
    listDelegatedServicesForAccountResponse_nextToken,
    listDelegatedServicesForAccountResponse_httpStatus,

    -- ** DisableAWSServiceAccess
    disableAWSServiceAccess_servicePrincipal,

    -- ** DescribeEffectivePolicy
    describeEffectivePolicy_targetId,
    describeEffectivePolicy_policyType,
    describeEffectivePolicyResponse_effectivePolicy,
    describeEffectivePolicyResponse_httpStatus,

    -- ** LeaveOrganization

    -- ** MoveAccount
    moveAccount_accountId,
    moveAccount_sourceParentId,
    moveAccount_destinationParentId,

    -- ** ListAccounts
    listAccounts_nextToken,
    listAccounts_maxResults,
    listAccountsResponse_accounts,
    listAccountsResponse_nextToken,
    listAccountsResponse_httpStatus,

    -- ** InviteAccountToOrganization
    inviteAccountToOrganization_notes,
    inviteAccountToOrganization_tags,
    inviteAccountToOrganization_target,
    inviteAccountToOrganizationResponse_handshake,
    inviteAccountToOrganizationResponse_httpStatus,

    -- ** ListAWSServiceAccessForOrganization
    listAWSServiceAccessForOrganization_nextToken,
    listAWSServiceAccessForOrganization_maxResults,
    listAWSServiceAccessForOrganizationResponse_nextToken,
    listAWSServiceAccessForOrganizationResponse_enabledServicePrincipals,
    listAWSServiceAccessForOrganizationResponse_httpStatus,

    -- ** ListOrganizationalUnitsForParent
    listOrganizationalUnitsForParent_nextToken,
    listOrganizationalUnitsForParent_maxResults,
    listOrganizationalUnitsForParent_parentId,
    listOrganizationalUnitsForParentResponse_nextToken,
    listOrganizationalUnitsForParentResponse_organizationalUnits,
    listOrganizationalUnitsForParentResponse_httpStatus,

    -- ** CancelHandshake
    cancelHandshake_handshakeId,
    cancelHandshakeResponse_handshake,
    cancelHandshakeResponse_httpStatus,

    -- ** ListChildren
    listChildren_nextToken,
    listChildren_maxResults,
    listChildren_parentId,
    listChildren_childType,
    listChildrenResponse_children,
    listChildrenResponse_nextToken,
    listChildrenResponse_httpStatus,

    -- ** ListDelegatedAdministrators
    listDelegatedAdministrators_servicePrincipal,
    listDelegatedAdministrators_nextToken,
    listDelegatedAdministrators_maxResults,
    listDelegatedAdministratorsResponse_delegatedAdministrators,
    listDelegatedAdministratorsResponse_nextToken,
    listDelegatedAdministratorsResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicy_policyId,

    -- ** UpdatePolicy
    updatePolicy_content,
    updatePolicy_name,
    updatePolicy_description,
    updatePolicy_policyId,
    updatePolicyResponse_policy,
    updatePolicyResponse_httpStatus,

    -- ** EnablePolicyType
    enablePolicyType_rootId,
    enablePolicyType_policyType,
    enablePolicyTypeResponse_root,
    enablePolicyTypeResponse_httpStatus,

    -- ** DisablePolicyType
    disablePolicyType_rootId,
    disablePolicyType_policyType,
    disablePolicyTypeResponse_root,
    disablePolicyTypeResponse_httpStatus,

    -- ** DescribeCreateAccountStatus
    describeCreateAccountStatus_createAccountRequestId,
    describeCreateAccountStatusResponse_createAccountStatus,
    describeCreateAccountStatusResponse_httpStatus,

    -- ** CreateOrganizationalUnit
    createOrganizationalUnit_tags,
    createOrganizationalUnit_parentId,
    createOrganizationalUnit_name,
    createOrganizationalUnitResponse_organizationalUnit,
    createOrganizationalUnitResponse_httpStatus,

    -- ** ListAccountsForParent
    listAccountsForParent_nextToken,
    listAccountsForParent_maxResults,
    listAccountsForParent_parentId,
    listAccountsForParentResponse_accounts,
    listAccountsForParentResponse_nextToken,
    listAccountsForParentResponse_httpStatus,

    -- ** DetachPolicy
    detachPolicy_policyId,
    detachPolicy_targetId,

    -- ** RemoveAccountFromOrganization
    removeAccountFromOrganization_accountId,

    -- ** CreateGovCloudAccount
    createGovCloudAccount_iamUserAccessToBilling,
    createGovCloudAccount_roleName,
    createGovCloudAccount_tags,
    createGovCloudAccount_email,
    createGovCloudAccount_accountName,
    createGovCloudAccountResponse_createAccountStatus,
    createGovCloudAccountResponse_httpStatus,

    -- ** EnableAWSServiceAccess
    enableAWSServiceAccess_servicePrincipal,

    -- ** DescribeOrganizationalUnit
    describeOrganizationalUnit_organizationalUnitId,
    describeOrganizationalUnitResponse_organizationalUnit,
    describeOrganizationalUnitResponse_httpStatus,

    -- ** ListParents
    listParents_nextToken,
    listParents_maxResults,
    listParents_childId,
    listParentsResponse_nextToken,
    listParentsResponse_parents,
    listParentsResponse_httpStatus,

    -- ** CreateAccount
    createAccount_iamUserAccessToBilling,
    createAccount_roleName,
    createAccount_tags,
    createAccount_email,
    createAccount_accountName,
    createAccountResponse_createAccountStatus,
    createAccountResponse_httpStatus,

    -- ** DeregisterDelegatedAdministrator
    deregisterDelegatedAdministrator_accountId,
    deregisterDelegatedAdministrator_servicePrincipal,

    -- ** TagResource
    tagResource_resourceId,
    tagResource_tags,

    -- ** ListCreateAccountStatus
    listCreateAccountStatus_states,
    listCreateAccountStatus_nextToken,
    listCreateAccountStatus_maxResults,
    listCreateAccountStatusResponse_createAccountStatuses,
    listCreateAccountStatusResponse_nextToken,
    listCreateAccountStatusResponse_httpStatus,

    -- ** ListTargetsForPolicy
    listTargetsForPolicy_nextToken,
    listTargetsForPolicy_maxResults,
    listTargetsForPolicy_policyId,
    listTargetsForPolicyResponse_nextToken,
    listTargetsForPolicyResponse_targets,
    listTargetsForPolicyResponse_httpStatus,

    -- ** DeclineHandshake
    declineHandshake_handshakeId,
    declineHandshakeResponse_handshake,
    declineHandshakeResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceId,
    untagResource_tagKeys,

    -- ** AttachPolicy
    attachPolicy_policyId,
    attachPolicy_targetId,

    -- ** ListPoliciesForTarget
    listPoliciesForTarget_nextToken,
    listPoliciesForTarget_maxResults,
    listPoliciesForTarget_targetId,
    listPoliciesForTarget_filter,
    listPoliciesForTargetResponse_nextToken,
    listPoliciesForTargetResponse_policies,
    listPoliciesForTargetResponse_httpStatus,

    -- ** DescribeOrganization
    describeOrganizationResponse_organization,
    describeOrganizationResponse_httpStatus,

    -- ** ListHandshakesForOrganization
    listHandshakesForOrganization_nextToken,
    listHandshakesForOrganization_filter,
    listHandshakesForOrganization_maxResults,
    listHandshakesForOrganizationResponse_handshakes,
    listHandshakesForOrganizationResponse_nextToken,
    listHandshakesForOrganizationResponse_httpStatus,

    -- ** RegisterDelegatedAdministrator
    registerDelegatedAdministrator_accountId,
    registerDelegatedAdministrator_servicePrincipal,

    -- ** DeleteOrganizationalUnit
    deleteOrganizationalUnit_organizationalUnitId,

    -- ** UpdateOrganizationalUnit
    updateOrganizationalUnit_name,
    updateOrganizationalUnit_organizationalUnitId,
    updateOrganizationalUnitResponse_organizationalUnit,
    updateOrganizationalUnitResponse_httpStatus,

    -- * Types

    -- ** Account
    account_status,
    account_joinedMethod,
    account_email,
    account_arn,
    account_joinedTimestamp,
    account_name,
    account_id,

    -- ** Child
    child_id,
    child_type,

    -- ** CreateAccountStatus
    createAccountStatus_failureReason,
    createAccountStatus_state,
    createAccountStatus_completedTimestamp,
    createAccountStatus_accountName,
    createAccountStatus_accountId,
    createAccountStatus_id,
    createAccountStatus_govCloudAccountId,
    createAccountStatus_requestedTimestamp,

    -- ** DelegatedAdministrator
    delegatedAdministrator_status,
    delegatedAdministrator_joinedMethod,
    delegatedAdministrator_email,
    delegatedAdministrator_arn,
    delegatedAdministrator_joinedTimestamp,
    delegatedAdministrator_delegationEnabledDate,
    delegatedAdministrator_name,
    delegatedAdministrator_id,

    -- ** DelegatedService
    delegatedService_servicePrincipal,
    delegatedService_delegationEnabledDate,

    -- ** EffectivePolicy
    effectivePolicy_targetId,
    effectivePolicy_policyType,
    effectivePolicy_lastUpdatedTimestamp,
    effectivePolicy_policyContent,

    -- ** EnabledServicePrincipal
    enabledServicePrincipal_servicePrincipal,
    enabledServicePrincipal_dateEnabled,

    -- ** Handshake
    handshake_state,
    handshake_arn,
    handshake_action,
    handshake_resources,
    handshake_id,
    handshake_expirationTimestamp,
    handshake_parties,
    handshake_requestedTimestamp,

    -- ** HandshakeFilter
    handshakeFilter_parentHandshakeId,
    handshakeFilter_actionType,

    -- ** HandshakeParty
    handshakeParty_id,
    handshakeParty_type,

    -- ** HandshakeResource
    handshakeResource_value,
    handshakeResource_resources,
    handshakeResource_type,

    -- ** Organization
    organization_arn,
    organization_masterAccountId,
    organization_masterAccountArn,
    organization_masterAccountEmail,
    organization_availablePolicyTypes,
    organization_id,
    organization_featureSet,

    -- ** OrganizationalUnit
    organizationalUnit_arn,
    organizationalUnit_name,
    organizationalUnit_id,

    -- ** Parent
    parent_id,
    parent_type,

    -- ** Policy
    policy_content,
    policy_policySummary,

    -- ** PolicySummary
    policySummary_arn,
    policySummary_name,
    policySummary_id,
    policySummary_awsManaged,
    policySummary_type,
    policySummary_description,

    -- ** PolicyTargetSummary
    policyTargetSummary_targetId,
    policyTargetSummary_arn,
    policyTargetSummary_name,
    policyTargetSummary_type,

    -- ** PolicyTypeSummary
    policyTypeSummary_status,
    policyTypeSummary_type,

    -- ** Root
    root_arn,
    root_name,
    root_id,
    root_policyTypes,

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

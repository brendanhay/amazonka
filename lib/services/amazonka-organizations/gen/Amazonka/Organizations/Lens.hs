{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Organizations.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Lens
  ( -- * Operations

    -- ** AcceptHandshake
    acceptHandshake_handshakeId,
    acceptHandshakeResponse_handshake,
    acceptHandshakeResponse_httpStatus,

    -- ** AttachPolicy
    attachPolicy_policyId,
    attachPolicy_targetId,

    -- ** CancelHandshake
    cancelHandshake_handshakeId,
    cancelHandshakeResponse_handshake,
    cancelHandshakeResponse_httpStatus,

    -- ** CloseAccount
    closeAccount_accountId,

    -- ** CreateAccount
    createAccount_iamUserAccessToBilling,
    createAccount_roleName,
    createAccount_tags,
    createAccount_email,
    createAccount_accountName,
    createAccountResponse_createAccountStatus,
    createAccountResponse_httpStatus,

    -- ** CreateGovCloudAccount
    createGovCloudAccount_iamUserAccessToBilling,
    createGovCloudAccount_roleName,
    createGovCloudAccount_tags,
    createGovCloudAccount_email,
    createGovCloudAccount_accountName,
    createGovCloudAccountResponse_createAccountStatus,
    createGovCloudAccountResponse_httpStatus,

    -- ** CreateOrganization
    createOrganization_featureSet,
    createOrganizationResponse_organization,
    createOrganizationResponse_httpStatus,

    -- ** CreateOrganizationalUnit
    createOrganizationalUnit_tags,
    createOrganizationalUnit_parentId,
    createOrganizationalUnit_name,
    createOrganizationalUnitResponse_organizationalUnit,
    createOrganizationalUnitResponse_httpStatus,

    -- ** CreatePolicy
    createPolicy_tags,
    createPolicy_content,
    createPolicy_description,
    createPolicy_name,
    createPolicy_type,
    createPolicyResponse_policy,
    createPolicyResponse_httpStatus,

    -- ** DeclineHandshake
    declineHandshake_handshakeId,
    declineHandshakeResponse_handshake,
    declineHandshakeResponse_httpStatus,

    -- ** DeleteOrganization

    -- ** DeleteOrganizationalUnit
    deleteOrganizationalUnit_organizationalUnitId,

    -- ** DeletePolicy
    deletePolicy_policyId,

    -- ** DeleteResourcePolicy

    -- ** DeregisterDelegatedAdministrator
    deregisterDelegatedAdministrator_accountId,
    deregisterDelegatedAdministrator_servicePrincipal,

    -- ** DescribeAccount
    describeAccount_accountId,
    describeAccountResponse_account,
    describeAccountResponse_httpStatus,

    -- ** DescribeCreateAccountStatus
    describeCreateAccountStatus_createAccountRequestId,
    describeCreateAccountStatusResponse_createAccountStatus,
    describeCreateAccountStatusResponse_httpStatus,

    -- ** DescribeEffectivePolicy
    describeEffectivePolicy_targetId,
    describeEffectivePolicy_policyType,
    describeEffectivePolicyResponse_effectivePolicy,
    describeEffectivePolicyResponse_httpStatus,

    -- ** DescribeHandshake
    describeHandshake_handshakeId,
    describeHandshakeResponse_handshake,
    describeHandshakeResponse_httpStatus,

    -- ** DescribeOrganization
    describeOrganizationResponse_organization,
    describeOrganizationResponse_httpStatus,

    -- ** DescribeOrganizationalUnit
    describeOrganizationalUnit_organizationalUnitId,
    describeOrganizationalUnitResponse_organizationalUnit,
    describeOrganizationalUnitResponse_httpStatus,

    -- ** DescribePolicy
    describePolicy_policyId,
    describePolicyResponse_policy,
    describePolicyResponse_httpStatus,

    -- ** DescribeResourcePolicy
    describeResourcePolicyResponse_resourcePolicy,
    describeResourcePolicyResponse_httpStatus,

    -- ** DetachPolicy
    detachPolicy_policyId,
    detachPolicy_targetId,

    -- ** DisableAWSServiceAccess
    disableAWSServiceAccess_servicePrincipal,

    -- ** DisablePolicyType
    disablePolicyType_rootId,
    disablePolicyType_policyType,
    disablePolicyTypeResponse_root,
    disablePolicyTypeResponse_httpStatus,

    -- ** EnableAWSServiceAccess
    enableAWSServiceAccess_servicePrincipal,

    -- ** EnableAllFeatures
    enableAllFeaturesResponse_handshake,
    enableAllFeaturesResponse_httpStatus,

    -- ** EnablePolicyType
    enablePolicyType_rootId,
    enablePolicyType_policyType,
    enablePolicyTypeResponse_root,
    enablePolicyTypeResponse_httpStatus,

    -- ** InviteAccountToOrganization
    inviteAccountToOrganization_notes,
    inviteAccountToOrganization_tags,
    inviteAccountToOrganization_target,
    inviteAccountToOrganizationResponse_handshake,
    inviteAccountToOrganizationResponse_httpStatus,

    -- ** LeaveOrganization

    -- ** ListAWSServiceAccessForOrganization
    listAWSServiceAccessForOrganization_maxResults,
    listAWSServiceAccessForOrganization_nextToken,
    listAWSServiceAccessForOrganizationResponse_enabledServicePrincipals,
    listAWSServiceAccessForOrganizationResponse_nextToken,
    listAWSServiceAccessForOrganizationResponse_httpStatus,

    -- ** ListAccounts
    listAccounts_maxResults,
    listAccounts_nextToken,
    listAccountsResponse_accounts,
    listAccountsResponse_nextToken,
    listAccountsResponse_httpStatus,

    -- ** ListAccountsForParent
    listAccountsForParent_maxResults,
    listAccountsForParent_nextToken,
    listAccountsForParent_parentId,
    listAccountsForParentResponse_accounts,
    listAccountsForParentResponse_nextToken,
    listAccountsForParentResponse_httpStatus,

    -- ** ListChildren
    listChildren_maxResults,
    listChildren_nextToken,
    listChildren_parentId,
    listChildren_childType,
    listChildrenResponse_children,
    listChildrenResponse_nextToken,
    listChildrenResponse_httpStatus,

    -- ** ListCreateAccountStatus
    listCreateAccountStatus_maxResults,
    listCreateAccountStatus_nextToken,
    listCreateAccountStatus_states,
    listCreateAccountStatusResponse_createAccountStatuses,
    listCreateAccountStatusResponse_nextToken,
    listCreateAccountStatusResponse_httpStatus,

    -- ** ListDelegatedAdministrators
    listDelegatedAdministrators_maxResults,
    listDelegatedAdministrators_nextToken,
    listDelegatedAdministrators_servicePrincipal,
    listDelegatedAdministratorsResponse_delegatedAdministrators,
    listDelegatedAdministratorsResponse_nextToken,
    listDelegatedAdministratorsResponse_httpStatus,

    -- ** ListDelegatedServicesForAccount
    listDelegatedServicesForAccount_maxResults,
    listDelegatedServicesForAccount_nextToken,
    listDelegatedServicesForAccount_accountId,
    listDelegatedServicesForAccountResponse_delegatedServices,
    listDelegatedServicesForAccountResponse_nextToken,
    listDelegatedServicesForAccountResponse_httpStatus,

    -- ** ListHandshakesForAccount
    listHandshakesForAccount_filter,
    listHandshakesForAccount_maxResults,
    listHandshakesForAccount_nextToken,
    listHandshakesForAccountResponse_handshakes,
    listHandshakesForAccountResponse_nextToken,
    listHandshakesForAccountResponse_httpStatus,

    -- ** ListHandshakesForOrganization
    listHandshakesForOrganization_filter,
    listHandshakesForOrganization_maxResults,
    listHandshakesForOrganization_nextToken,
    listHandshakesForOrganizationResponse_handshakes,
    listHandshakesForOrganizationResponse_nextToken,
    listHandshakesForOrganizationResponse_httpStatus,

    -- ** ListOrganizationalUnitsForParent
    listOrganizationalUnitsForParent_maxResults,
    listOrganizationalUnitsForParent_nextToken,
    listOrganizationalUnitsForParent_parentId,
    listOrganizationalUnitsForParentResponse_nextToken,
    listOrganizationalUnitsForParentResponse_organizationalUnits,
    listOrganizationalUnitsForParentResponse_httpStatus,

    -- ** ListParents
    listParents_maxResults,
    listParents_nextToken,
    listParents_childId,
    listParentsResponse_nextToken,
    listParentsResponse_parents,
    listParentsResponse_httpStatus,

    -- ** ListPolicies
    listPolicies_maxResults,
    listPolicies_nextToken,
    listPolicies_filter,
    listPoliciesResponse_nextToken,
    listPoliciesResponse_policies,
    listPoliciesResponse_httpStatus,

    -- ** ListPoliciesForTarget
    listPoliciesForTarget_maxResults,
    listPoliciesForTarget_nextToken,
    listPoliciesForTarget_targetId,
    listPoliciesForTarget_filter,
    listPoliciesForTargetResponse_nextToken,
    listPoliciesForTargetResponse_policies,
    listPoliciesForTargetResponse_httpStatus,

    -- ** ListRoots
    listRoots_maxResults,
    listRoots_nextToken,
    listRootsResponse_nextToken,
    listRootsResponse_roots,
    listRootsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTargetsForPolicy
    listTargetsForPolicy_maxResults,
    listTargetsForPolicy_nextToken,
    listTargetsForPolicy_policyId,
    listTargetsForPolicyResponse_nextToken,
    listTargetsForPolicyResponse_targets,
    listTargetsForPolicyResponse_httpStatus,

    -- ** MoveAccount
    moveAccount_accountId,
    moveAccount_sourceParentId,
    moveAccount_destinationParentId,

    -- ** PutResourcePolicy
    putResourcePolicy_tags,
    putResourcePolicy_content,
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,

    -- ** RegisterDelegatedAdministrator
    registerDelegatedAdministrator_accountId,
    registerDelegatedAdministrator_servicePrincipal,

    -- ** RemoveAccountFromOrganization
    removeAccountFromOrganization_accountId,

    -- ** TagResource
    tagResource_resourceId,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceId,
    untagResource_tagKeys,

    -- ** UpdateOrganizationalUnit
    updateOrganizationalUnit_name,
    updateOrganizationalUnit_organizationalUnitId,
    updateOrganizationalUnitResponse_organizationalUnit,
    updateOrganizationalUnitResponse_httpStatus,

    -- ** UpdatePolicy
    updatePolicy_content,
    updatePolicy_description,
    updatePolicy_name,
    updatePolicy_policyId,
    updatePolicyResponse_policy,
    updatePolicyResponse_httpStatus,

    -- * Types

    -- ** Account
    account_arn,
    account_email,
    account_id,
    account_joinedMethod,
    account_joinedTimestamp,
    account_name,
    account_status,

    -- ** Child
    child_id,
    child_type,

    -- ** CreateAccountStatus
    createAccountStatus_accountId,
    createAccountStatus_accountName,
    createAccountStatus_completedTimestamp,
    createAccountStatus_failureReason,
    createAccountStatus_govCloudAccountId,
    createAccountStatus_id,
    createAccountStatus_requestedTimestamp,
    createAccountStatus_state,

    -- ** DelegatedAdministrator
    delegatedAdministrator_arn,
    delegatedAdministrator_delegationEnabledDate,
    delegatedAdministrator_email,
    delegatedAdministrator_id,
    delegatedAdministrator_joinedMethod,
    delegatedAdministrator_joinedTimestamp,
    delegatedAdministrator_name,
    delegatedAdministrator_status,

    -- ** DelegatedService
    delegatedService_delegationEnabledDate,
    delegatedService_servicePrincipal,

    -- ** EffectivePolicy
    effectivePolicy_lastUpdatedTimestamp,
    effectivePolicy_policyContent,
    effectivePolicy_policyType,
    effectivePolicy_targetId,

    -- ** EnabledServicePrincipal
    enabledServicePrincipal_dateEnabled,
    enabledServicePrincipal_servicePrincipal,

    -- ** Handshake
    handshake_action,
    handshake_arn,
    handshake_expirationTimestamp,
    handshake_id,
    handshake_parties,
    handshake_requestedTimestamp,
    handshake_resources,
    handshake_state,

    -- ** HandshakeFilter
    handshakeFilter_actionType,
    handshakeFilter_parentHandshakeId,

    -- ** HandshakeParty
    handshakeParty_id,
    handshakeParty_type,

    -- ** HandshakeResource
    handshakeResource_resources,
    handshakeResource_type,
    handshakeResource_value,

    -- ** Organization
    organization_arn,
    organization_availablePolicyTypes,
    organization_featureSet,
    organization_id,
    organization_masterAccountArn,
    organization_masterAccountEmail,
    organization_masterAccountId,

    -- ** OrganizationalUnit
    organizationalUnit_arn,
    organizationalUnit_id,
    organizationalUnit_name,

    -- ** Parent
    parent_id,
    parent_type,

    -- ** Policy
    policy_content,
    policy_policySummary,

    -- ** PolicySummary
    policySummary_arn,
    policySummary_awsManaged,
    policySummary_description,
    policySummary_id,
    policySummary_name,
    policySummary_type,

    -- ** PolicyTargetSummary
    policyTargetSummary_arn,
    policyTargetSummary_name,
    policyTargetSummary_targetId,
    policyTargetSummary_type,

    -- ** PolicyTypeSummary
    policyTypeSummary_status,
    policyTypeSummary_type,

    -- ** ResourcePolicy
    resourcePolicy_content,
    resourcePolicy_resourcePolicySummary,

    -- ** ResourcePolicySummary
    resourcePolicySummary_arn,
    resourcePolicySummary_id,

    -- ** Root
    root_arn,
    root_id,
    root_name,
    root_policyTypes,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.Organizations.AcceptHandshake
import Amazonka.Organizations.AttachPolicy
import Amazonka.Organizations.CancelHandshake
import Amazonka.Organizations.CloseAccount
import Amazonka.Organizations.CreateAccount
import Amazonka.Organizations.CreateGovCloudAccount
import Amazonka.Organizations.CreateOrganization
import Amazonka.Organizations.CreateOrganizationalUnit
import Amazonka.Organizations.CreatePolicy
import Amazonka.Organizations.DeclineHandshake
import Amazonka.Organizations.DeleteOrganization
import Amazonka.Organizations.DeleteOrganizationalUnit
import Amazonka.Organizations.DeletePolicy
import Amazonka.Organizations.DeleteResourcePolicy
import Amazonka.Organizations.DeregisterDelegatedAdministrator
import Amazonka.Organizations.DescribeAccount
import Amazonka.Organizations.DescribeCreateAccountStatus
import Amazonka.Organizations.DescribeEffectivePolicy
import Amazonka.Organizations.DescribeHandshake
import Amazonka.Organizations.DescribeOrganization
import Amazonka.Organizations.DescribeOrganizationalUnit
import Amazonka.Organizations.DescribePolicy
import Amazonka.Organizations.DescribeResourcePolicy
import Amazonka.Organizations.DetachPolicy
import Amazonka.Organizations.DisableAWSServiceAccess
import Amazonka.Organizations.DisablePolicyType
import Amazonka.Organizations.EnableAWSServiceAccess
import Amazonka.Organizations.EnableAllFeatures
import Amazonka.Organizations.EnablePolicyType
import Amazonka.Organizations.InviteAccountToOrganization
import Amazonka.Organizations.LeaveOrganization
import Amazonka.Organizations.ListAWSServiceAccessForOrganization
import Amazonka.Organizations.ListAccounts
import Amazonka.Organizations.ListAccountsForParent
import Amazonka.Organizations.ListChildren
import Amazonka.Organizations.ListCreateAccountStatus
import Amazonka.Organizations.ListDelegatedAdministrators
import Amazonka.Organizations.ListDelegatedServicesForAccount
import Amazonka.Organizations.ListHandshakesForAccount
import Amazonka.Organizations.ListHandshakesForOrganization
import Amazonka.Organizations.ListOrganizationalUnitsForParent
import Amazonka.Organizations.ListParents
import Amazonka.Organizations.ListPolicies
import Amazonka.Organizations.ListPoliciesForTarget
import Amazonka.Organizations.ListRoots
import Amazonka.Organizations.ListTagsForResource
import Amazonka.Organizations.ListTargetsForPolicy
import Amazonka.Organizations.MoveAccount
import Amazonka.Organizations.PutResourcePolicy
import Amazonka.Organizations.RegisterDelegatedAdministrator
import Amazonka.Organizations.RemoveAccountFromOrganization
import Amazonka.Organizations.TagResource
import Amazonka.Organizations.Types.Account
import Amazonka.Organizations.Types.Child
import Amazonka.Organizations.Types.CreateAccountStatus
import Amazonka.Organizations.Types.DelegatedAdministrator
import Amazonka.Organizations.Types.DelegatedService
import Amazonka.Organizations.Types.EffectivePolicy
import Amazonka.Organizations.Types.EnabledServicePrincipal
import Amazonka.Organizations.Types.Handshake
import Amazonka.Organizations.Types.HandshakeFilter
import Amazonka.Organizations.Types.HandshakeParty
import Amazonka.Organizations.Types.HandshakeResource
import Amazonka.Organizations.Types.Organization
import Amazonka.Organizations.Types.OrganizationalUnit
import Amazonka.Organizations.Types.Parent
import Amazonka.Organizations.Types.Policy
import Amazonka.Organizations.Types.PolicySummary
import Amazonka.Organizations.Types.PolicyTargetSummary
import Amazonka.Organizations.Types.PolicyTypeSummary
import Amazonka.Organizations.Types.ResourcePolicy
import Amazonka.Organizations.Types.ResourcePolicySummary
import Amazonka.Organizations.Types.Root
import Amazonka.Organizations.Types.Tag
import Amazonka.Organizations.UntagResource
import Amazonka.Organizations.UpdateOrganizationalUnit
import Amazonka.Organizations.UpdatePolicy

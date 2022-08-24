{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Organizations.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    createAccount_tags,
    createAccount_roleName,
    createAccount_iamUserAccessToBilling,
    createAccount_email,
    createAccount_accountName,
    createAccountResponse_createAccountStatus,
    createAccountResponse_httpStatus,

    -- ** CreateGovCloudAccount
    createGovCloudAccount_tags,
    createGovCloudAccount_roleName,
    createGovCloudAccount_iamUserAccessToBilling,
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
    inviteAccountToOrganization_tags,
    inviteAccountToOrganization_notes,
    inviteAccountToOrganization_target,
    inviteAccountToOrganizationResponse_handshake,
    inviteAccountToOrganizationResponse_httpStatus,

    -- ** LeaveOrganization

    -- ** ListAWSServiceAccessForOrganization
    listAWSServiceAccessForOrganization_nextToken,
    listAWSServiceAccessForOrganization_maxResults,
    listAWSServiceAccessForOrganizationResponse_nextToken,
    listAWSServiceAccessForOrganizationResponse_enabledServicePrincipals,
    listAWSServiceAccessForOrganizationResponse_httpStatus,

    -- ** ListAccounts
    listAccounts_nextToken,
    listAccounts_maxResults,
    listAccountsResponse_nextToken,
    listAccountsResponse_accounts,
    listAccountsResponse_httpStatus,

    -- ** ListAccountsForParent
    listAccountsForParent_nextToken,
    listAccountsForParent_maxResults,
    listAccountsForParent_parentId,
    listAccountsForParentResponse_nextToken,
    listAccountsForParentResponse_accounts,
    listAccountsForParentResponse_httpStatus,

    -- ** ListChildren
    listChildren_nextToken,
    listChildren_maxResults,
    listChildren_parentId,
    listChildren_childType,
    listChildrenResponse_nextToken,
    listChildrenResponse_children,
    listChildrenResponse_httpStatus,

    -- ** ListCreateAccountStatus
    listCreateAccountStatus_nextToken,
    listCreateAccountStatus_maxResults,
    listCreateAccountStatus_states,
    listCreateAccountStatusResponse_nextToken,
    listCreateAccountStatusResponse_createAccountStatuses,
    listCreateAccountStatusResponse_httpStatus,

    -- ** ListDelegatedAdministrators
    listDelegatedAdministrators_nextToken,
    listDelegatedAdministrators_maxResults,
    listDelegatedAdministrators_servicePrincipal,
    listDelegatedAdministratorsResponse_nextToken,
    listDelegatedAdministratorsResponse_delegatedAdministrators,
    listDelegatedAdministratorsResponse_httpStatus,

    -- ** ListDelegatedServicesForAccount
    listDelegatedServicesForAccount_nextToken,
    listDelegatedServicesForAccount_maxResults,
    listDelegatedServicesForAccount_accountId,
    listDelegatedServicesForAccountResponse_nextToken,
    listDelegatedServicesForAccountResponse_delegatedServices,
    listDelegatedServicesForAccountResponse_httpStatus,

    -- ** ListHandshakesForAccount
    listHandshakesForAccount_nextToken,
    listHandshakesForAccount_filter,
    listHandshakesForAccount_maxResults,
    listHandshakesForAccountResponse_handshakes,
    listHandshakesForAccountResponse_nextToken,
    listHandshakesForAccountResponse_httpStatus,

    -- ** ListHandshakesForOrganization
    listHandshakesForOrganization_nextToken,
    listHandshakesForOrganization_filter,
    listHandshakesForOrganization_maxResults,
    listHandshakesForOrganizationResponse_handshakes,
    listHandshakesForOrganizationResponse_nextToken,
    listHandshakesForOrganizationResponse_httpStatus,

    -- ** ListOrganizationalUnitsForParent
    listOrganizationalUnitsForParent_nextToken,
    listOrganizationalUnitsForParent_maxResults,
    listOrganizationalUnitsForParent_parentId,
    listOrganizationalUnitsForParentResponse_nextToken,
    listOrganizationalUnitsForParentResponse_organizationalUnits,
    listOrganizationalUnitsForParentResponse_httpStatus,

    -- ** ListParents
    listParents_nextToken,
    listParents_maxResults,
    listParents_childId,
    listParentsResponse_nextToken,
    listParentsResponse_parents,
    listParentsResponse_httpStatus,

    -- ** ListPolicies
    listPolicies_nextToken,
    listPolicies_maxResults,
    listPolicies_filter,
    listPoliciesResponse_nextToken,
    listPoliciesResponse_policies,
    listPoliciesResponse_httpStatus,

    -- ** ListPoliciesForTarget
    listPoliciesForTarget_nextToken,
    listPoliciesForTarget_maxResults,
    listPoliciesForTarget_targetId,
    listPoliciesForTarget_filter,
    listPoliciesForTargetResponse_nextToken,
    listPoliciesForTargetResponse_policies,
    listPoliciesForTargetResponse_httpStatus,

    -- ** ListRoots
    listRoots_nextToken,
    listRoots_maxResults,
    listRootsResponse_nextToken,
    listRootsResponse_roots,
    listRootsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceId,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTargetsForPolicy
    listTargetsForPolicy_nextToken,
    listTargetsForPolicy_maxResults,
    listTargetsForPolicy_policyId,
    listTargetsForPolicyResponse_nextToken,
    listTargetsForPolicyResponse_targets,
    listTargetsForPolicyResponse_httpStatus,

    -- ** MoveAccount
    moveAccount_accountId,
    moveAccount_sourceParentId,
    moveAccount_destinationParentId,

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
    updatePolicy_name,
    updatePolicy_description,
    updatePolicy_content,
    updatePolicy_policyId,
    updatePolicyResponse_policy,
    updatePolicyResponse_httpStatus,

    -- * Types

    -- ** Account
    account_name,
    account_email,
    account_arn,
    account_status,
    account_id,
    account_joinedTimestamp,
    account_joinedMethod,

    -- ** Child
    child_type,
    child_id,

    -- ** CreateAccountStatus
    createAccountStatus_requestedTimestamp,
    createAccountStatus_govCloudAccountId,
    createAccountStatus_state,
    createAccountStatus_id,
    createAccountStatus_accountId,
    createAccountStatus_accountName,
    createAccountStatus_completedTimestamp,
    createAccountStatus_failureReason,

    -- ** DelegatedAdministrator
    delegatedAdministrator_name,
    delegatedAdministrator_email,
    delegatedAdministrator_arn,
    delegatedAdministrator_status,
    delegatedAdministrator_id,
    delegatedAdministrator_joinedTimestamp,
    delegatedAdministrator_joinedMethod,
    delegatedAdministrator_delegationEnabledDate,

    -- ** DelegatedService
    delegatedService_servicePrincipal,
    delegatedService_delegationEnabledDate,

    -- ** EffectivePolicy
    effectivePolicy_lastUpdatedTimestamp,
    effectivePolicy_targetId,
    effectivePolicy_policyType,
    effectivePolicy_policyContent,

    -- ** EnabledServicePrincipal
    enabledServicePrincipal_servicePrincipal,
    enabledServicePrincipal_dateEnabled,

    -- ** Handshake
    handshake_arn,
    handshake_requestedTimestamp,
    handshake_state,
    handshake_id,
    handshake_parties,
    handshake_expirationTimestamp,
    handshake_action,
    handshake_resources,

    -- ** HandshakeFilter
    handshakeFilter_actionType,
    handshakeFilter_parentHandshakeId,

    -- ** HandshakeParty
    handshakeParty_id,
    handshakeParty_type,

    -- ** HandshakeResource
    handshakeResource_type,
    handshakeResource_resources,
    handshakeResource_value,

    -- ** Organization
    organization_arn,
    organization_id,
    organization_masterAccountId,
    organization_availablePolicyTypes,
    organization_featureSet,
    organization_masterAccountEmail,
    organization_masterAccountArn,

    -- ** OrganizationalUnit
    organizationalUnit_name,
    organizationalUnit_arn,
    organizationalUnit_id,

    -- ** Parent
    parent_type,
    parent_id,

    -- ** Policy
    policy_policySummary,
    policy_content,

    -- ** PolicySummary
    policySummary_name,
    policySummary_type,
    policySummary_arn,
    policySummary_awsManaged,
    policySummary_id,
    policySummary_description,

    -- ** PolicyTargetSummary
    policyTargetSummary_name,
    policyTargetSummary_type,
    policyTargetSummary_targetId,
    policyTargetSummary_arn,

    -- ** PolicyTypeSummary
    policyTypeSummary_type,
    policyTypeSummary_status,

    -- ** Root
    root_name,
    root_arn,
    root_policyTypes,
    root_id,

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
import Amazonka.Organizations.DeregisterDelegatedAdministrator
import Amazonka.Organizations.DescribeAccount
import Amazonka.Organizations.DescribeCreateAccountStatus
import Amazonka.Organizations.DescribeEffectivePolicy
import Amazonka.Organizations.DescribeHandshake
import Amazonka.Organizations.DescribeOrganization
import Amazonka.Organizations.DescribeOrganizationalUnit
import Amazonka.Organizations.DescribePolicy
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
import Amazonka.Organizations.Types.Root
import Amazonka.Organizations.Types.Tag
import Amazonka.Organizations.UntagResource
import Amazonka.Organizations.UpdateOrganizationalUnit
import Amazonka.Organizations.UpdatePolicy

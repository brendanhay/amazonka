{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSOAdmin.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Lens
  ( -- * Operations

    -- ** AttachCustomerManagedPolicyReferenceToPermissionSet
    attachCustomerManagedPolicyReferenceToPermissionSet_instanceArn,
    attachCustomerManagedPolicyReferenceToPermissionSet_permissionSetArn,
    attachCustomerManagedPolicyReferenceToPermissionSet_customerManagedPolicyReference,
    attachCustomerManagedPolicyReferenceToPermissionSetResponse_httpStatus,

    -- ** AttachManagedPolicyToPermissionSet
    attachManagedPolicyToPermissionSet_instanceArn,
    attachManagedPolicyToPermissionSet_permissionSetArn,
    attachManagedPolicyToPermissionSet_managedPolicyArn,
    attachManagedPolicyToPermissionSetResponse_httpStatus,

    -- ** CreateAccountAssignment
    createAccountAssignment_instanceArn,
    createAccountAssignment_targetId,
    createAccountAssignment_targetType,
    createAccountAssignment_permissionSetArn,
    createAccountAssignment_principalType,
    createAccountAssignment_principalId,
    createAccountAssignmentResponse_accountAssignmentCreationStatus,
    createAccountAssignmentResponse_httpStatus,

    -- ** CreateInstanceAccessControlAttributeConfiguration
    createInstanceAccessControlAttributeConfiguration_instanceArn,
    createInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration,
    createInstanceAccessControlAttributeConfigurationResponse_httpStatus,

    -- ** CreatePermissionSet
    createPermissionSet_description,
    createPermissionSet_relayState,
    createPermissionSet_sessionDuration,
    createPermissionSet_tags,
    createPermissionSet_name,
    createPermissionSet_instanceArn,
    createPermissionSetResponse_permissionSet,
    createPermissionSetResponse_httpStatus,

    -- ** DeleteAccountAssignment
    deleteAccountAssignment_instanceArn,
    deleteAccountAssignment_targetId,
    deleteAccountAssignment_targetType,
    deleteAccountAssignment_permissionSetArn,
    deleteAccountAssignment_principalType,
    deleteAccountAssignment_principalId,
    deleteAccountAssignmentResponse_accountAssignmentDeletionStatus,
    deleteAccountAssignmentResponse_httpStatus,

    -- ** DeleteInlinePolicyFromPermissionSet
    deleteInlinePolicyFromPermissionSet_instanceArn,
    deleteInlinePolicyFromPermissionSet_permissionSetArn,
    deleteInlinePolicyFromPermissionSetResponse_httpStatus,

    -- ** DeleteInstanceAccessControlAttributeConfiguration
    deleteInstanceAccessControlAttributeConfiguration_instanceArn,
    deleteInstanceAccessControlAttributeConfigurationResponse_httpStatus,

    -- ** DeletePermissionSet
    deletePermissionSet_instanceArn,
    deletePermissionSet_permissionSetArn,
    deletePermissionSetResponse_httpStatus,

    -- ** DeletePermissionsBoundaryFromPermissionSet
    deletePermissionsBoundaryFromPermissionSet_instanceArn,
    deletePermissionsBoundaryFromPermissionSet_permissionSetArn,
    deletePermissionsBoundaryFromPermissionSetResponse_httpStatus,

    -- ** DescribeAccountAssignmentCreationStatus
    describeAccountAssignmentCreationStatus_instanceArn,
    describeAccountAssignmentCreationStatus_accountAssignmentCreationRequestId,
    describeAccountAssignmentCreationStatusResponse_accountAssignmentCreationStatus,
    describeAccountAssignmentCreationStatusResponse_httpStatus,

    -- ** DescribeAccountAssignmentDeletionStatus
    describeAccountAssignmentDeletionStatus_instanceArn,
    describeAccountAssignmentDeletionStatus_accountAssignmentDeletionRequestId,
    describeAccountAssignmentDeletionStatusResponse_accountAssignmentDeletionStatus,
    describeAccountAssignmentDeletionStatusResponse_httpStatus,

    -- ** DescribeInstanceAccessControlAttributeConfiguration
    describeInstanceAccessControlAttributeConfiguration_instanceArn,
    describeInstanceAccessControlAttributeConfigurationResponse_instanceAccessControlAttributeConfiguration,
    describeInstanceAccessControlAttributeConfigurationResponse_status,
    describeInstanceAccessControlAttributeConfigurationResponse_statusReason,
    describeInstanceAccessControlAttributeConfigurationResponse_httpStatus,

    -- ** DescribePermissionSet
    describePermissionSet_instanceArn,
    describePermissionSet_permissionSetArn,
    describePermissionSetResponse_permissionSet,
    describePermissionSetResponse_httpStatus,

    -- ** DescribePermissionSetProvisioningStatus
    describePermissionSetProvisioningStatus_instanceArn,
    describePermissionSetProvisioningStatus_provisionPermissionSetRequestId,
    describePermissionSetProvisioningStatusResponse_permissionSetProvisioningStatus,
    describePermissionSetProvisioningStatusResponse_httpStatus,

    -- ** DetachCustomerManagedPolicyReferenceFromPermissionSet
    detachCustomerManagedPolicyReferenceFromPermissionSet_instanceArn,
    detachCustomerManagedPolicyReferenceFromPermissionSet_permissionSetArn,
    detachCustomerManagedPolicyReferenceFromPermissionSet_customerManagedPolicyReference,
    detachCustomerManagedPolicyReferenceFromPermissionSetResponse_httpStatus,

    -- ** DetachManagedPolicyFromPermissionSet
    detachManagedPolicyFromPermissionSet_instanceArn,
    detachManagedPolicyFromPermissionSet_permissionSetArn,
    detachManagedPolicyFromPermissionSet_managedPolicyArn,
    detachManagedPolicyFromPermissionSetResponse_httpStatus,

    -- ** GetInlinePolicyForPermissionSet
    getInlinePolicyForPermissionSet_instanceArn,
    getInlinePolicyForPermissionSet_permissionSetArn,
    getInlinePolicyForPermissionSetResponse_inlinePolicy,
    getInlinePolicyForPermissionSetResponse_httpStatus,

    -- ** GetPermissionsBoundaryForPermissionSet
    getPermissionsBoundaryForPermissionSet_instanceArn,
    getPermissionsBoundaryForPermissionSet_permissionSetArn,
    getPermissionsBoundaryForPermissionSetResponse_permissionsBoundary,
    getPermissionsBoundaryForPermissionSetResponse_httpStatus,

    -- ** ListAccountAssignmentCreationStatus
    listAccountAssignmentCreationStatus_filter,
    listAccountAssignmentCreationStatus_maxResults,
    listAccountAssignmentCreationStatus_nextToken,
    listAccountAssignmentCreationStatus_instanceArn,
    listAccountAssignmentCreationStatusResponse_accountAssignmentsCreationStatus,
    listAccountAssignmentCreationStatusResponse_nextToken,
    listAccountAssignmentCreationStatusResponse_httpStatus,

    -- ** ListAccountAssignmentDeletionStatus
    listAccountAssignmentDeletionStatus_filter,
    listAccountAssignmentDeletionStatus_maxResults,
    listAccountAssignmentDeletionStatus_nextToken,
    listAccountAssignmentDeletionStatus_instanceArn,
    listAccountAssignmentDeletionStatusResponse_accountAssignmentsDeletionStatus,
    listAccountAssignmentDeletionStatusResponse_nextToken,
    listAccountAssignmentDeletionStatusResponse_httpStatus,

    -- ** ListAccountAssignments
    listAccountAssignments_maxResults,
    listAccountAssignments_nextToken,
    listAccountAssignments_instanceArn,
    listAccountAssignments_accountId,
    listAccountAssignments_permissionSetArn,
    listAccountAssignmentsResponse_accountAssignments,
    listAccountAssignmentsResponse_nextToken,
    listAccountAssignmentsResponse_httpStatus,

    -- ** ListAccountsForProvisionedPermissionSet
    listAccountsForProvisionedPermissionSet_maxResults,
    listAccountsForProvisionedPermissionSet_nextToken,
    listAccountsForProvisionedPermissionSet_provisioningStatus,
    listAccountsForProvisionedPermissionSet_instanceArn,
    listAccountsForProvisionedPermissionSet_permissionSetArn,
    listAccountsForProvisionedPermissionSetResponse_accountIds,
    listAccountsForProvisionedPermissionSetResponse_nextToken,
    listAccountsForProvisionedPermissionSetResponse_httpStatus,

    -- ** ListCustomerManagedPolicyReferencesInPermissionSet
    listCustomerManagedPolicyReferencesInPermissionSet_maxResults,
    listCustomerManagedPolicyReferencesInPermissionSet_nextToken,
    listCustomerManagedPolicyReferencesInPermissionSet_instanceArn,
    listCustomerManagedPolicyReferencesInPermissionSet_permissionSetArn,
    listCustomerManagedPolicyReferencesInPermissionSetResponse_customerManagedPolicyReferences,
    listCustomerManagedPolicyReferencesInPermissionSetResponse_nextToken,
    listCustomerManagedPolicyReferencesInPermissionSetResponse_httpStatus,

    -- ** ListInstances
    listInstances_maxResults,
    listInstances_nextToken,
    listInstancesResponse_instances,
    listInstancesResponse_nextToken,
    listInstancesResponse_httpStatus,

    -- ** ListManagedPoliciesInPermissionSet
    listManagedPoliciesInPermissionSet_maxResults,
    listManagedPoliciesInPermissionSet_nextToken,
    listManagedPoliciesInPermissionSet_instanceArn,
    listManagedPoliciesInPermissionSet_permissionSetArn,
    listManagedPoliciesInPermissionSetResponse_attachedManagedPolicies,
    listManagedPoliciesInPermissionSetResponse_nextToken,
    listManagedPoliciesInPermissionSetResponse_httpStatus,

    -- ** ListPermissionSetProvisioningStatus
    listPermissionSetProvisioningStatus_filter,
    listPermissionSetProvisioningStatus_maxResults,
    listPermissionSetProvisioningStatus_nextToken,
    listPermissionSetProvisioningStatus_instanceArn,
    listPermissionSetProvisioningStatusResponse_nextToken,
    listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus,
    listPermissionSetProvisioningStatusResponse_httpStatus,

    -- ** ListPermissionSets
    listPermissionSets_maxResults,
    listPermissionSets_nextToken,
    listPermissionSets_instanceArn,
    listPermissionSetsResponse_nextToken,
    listPermissionSetsResponse_permissionSets,
    listPermissionSetsResponse_httpStatus,

    -- ** ListPermissionSetsProvisionedToAccount
    listPermissionSetsProvisionedToAccount_maxResults,
    listPermissionSetsProvisionedToAccount_nextToken,
    listPermissionSetsProvisionedToAccount_provisioningStatus,
    listPermissionSetsProvisionedToAccount_instanceArn,
    listPermissionSetsProvisionedToAccount_accountId,
    listPermissionSetsProvisionedToAccountResponse_nextToken,
    listPermissionSetsProvisionedToAccountResponse_permissionSets,
    listPermissionSetsProvisionedToAccountResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_instanceArn,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ProvisionPermissionSet
    provisionPermissionSet_targetId,
    provisionPermissionSet_instanceArn,
    provisionPermissionSet_permissionSetArn,
    provisionPermissionSet_targetType,
    provisionPermissionSetResponse_permissionSetProvisioningStatus,
    provisionPermissionSetResponse_httpStatus,

    -- ** PutInlinePolicyToPermissionSet
    putInlinePolicyToPermissionSet_instanceArn,
    putInlinePolicyToPermissionSet_permissionSetArn,
    putInlinePolicyToPermissionSet_inlinePolicy,
    putInlinePolicyToPermissionSetResponse_httpStatus,

    -- ** PutPermissionsBoundaryToPermissionSet
    putPermissionsBoundaryToPermissionSet_instanceArn,
    putPermissionsBoundaryToPermissionSet_permissionSetArn,
    putPermissionsBoundaryToPermissionSet_permissionsBoundary,
    putPermissionsBoundaryToPermissionSetResponse_httpStatus,

    -- ** TagResource
    tagResource_instanceArn,
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_instanceArn,
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateInstanceAccessControlAttributeConfiguration
    updateInstanceAccessControlAttributeConfiguration_instanceArn,
    updateInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration,
    updateInstanceAccessControlAttributeConfigurationResponse_httpStatus,

    -- ** UpdatePermissionSet
    updatePermissionSet_description,
    updatePermissionSet_relayState,
    updatePermissionSet_sessionDuration,
    updatePermissionSet_instanceArn,
    updatePermissionSet_permissionSetArn,
    updatePermissionSetResponse_httpStatus,

    -- * Types

    -- ** AccessControlAttribute
    accessControlAttribute_key,
    accessControlAttribute_value,

    -- ** AccessControlAttributeValue
    accessControlAttributeValue_source,

    -- ** AccountAssignment
    accountAssignment_accountId,
    accountAssignment_permissionSetArn,
    accountAssignment_principalId,
    accountAssignment_principalType,

    -- ** AccountAssignmentOperationStatus
    accountAssignmentOperationStatus_createdDate,
    accountAssignmentOperationStatus_failureReason,
    accountAssignmentOperationStatus_permissionSetArn,
    accountAssignmentOperationStatus_principalId,
    accountAssignmentOperationStatus_principalType,
    accountAssignmentOperationStatus_requestId,
    accountAssignmentOperationStatus_status,
    accountAssignmentOperationStatus_targetId,
    accountAssignmentOperationStatus_targetType,

    -- ** AccountAssignmentOperationStatusMetadata
    accountAssignmentOperationStatusMetadata_createdDate,
    accountAssignmentOperationStatusMetadata_requestId,
    accountAssignmentOperationStatusMetadata_status,

    -- ** AttachedManagedPolicy
    attachedManagedPolicy_arn,
    attachedManagedPolicy_name,

    -- ** CustomerManagedPolicyReference
    customerManagedPolicyReference_path,
    customerManagedPolicyReference_name,

    -- ** InstanceAccessControlAttributeConfiguration
    instanceAccessControlAttributeConfiguration_accessControlAttributes,

    -- ** InstanceMetadata
    instanceMetadata_identityStoreId,
    instanceMetadata_instanceArn,

    -- ** OperationStatusFilter
    operationStatusFilter_status,

    -- ** PermissionSet
    permissionSet_createdDate,
    permissionSet_description,
    permissionSet_name,
    permissionSet_permissionSetArn,
    permissionSet_relayState,
    permissionSet_sessionDuration,

    -- ** PermissionSetProvisioningStatus
    permissionSetProvisioningStatus_accountId,
    permissionSetProvisioningStatus_createdDate,
    permissionSetProvisioningStatus_failureReason,
    permissionSetProvisioningStatus_permissionSetArn,
    permissionSetProvisioningStatus_requestId,
    permissionSetProvisioningStatus_status,

    -- ** PermissionSetProvisioningStatusMetadata
    permissionSetProvisioningStatusMetadata_createdDate,
    permissionSetProvisioningStatusMetadata_requestId,
    permissionSetProvisioningStatusMetadata_status,

    -- ** PermissionsBoundary
    permissionsBoundary_customerManagedPolicyReference,
    permissionsBoundary_managedPolicyArn,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.SSOAdmin.AttachCustomerManagedPolicyReferenceToPermissionSet
import Amazonka.SSOAdmin.AttachManagedPolicyToPermissionSet
import Amazonka.SSOAdmin.CreateAccountAssignment
import Amazonka.SSOAdmin.CreateInstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.CreatePermissionSet
import Amazonka.SSOAdmin.DeleteAccountAssignment
import Amazonka.SSOAdmin.DeleteInlinePolicyFromPermissionSet
import Amazonka.SSOAdmin.DeleteInstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.DeletePermissionSet
import Amazonka.SSOAdmin.DeletePermissionsBoundaryFromPermissionSet
import Amazonka.SSOAdmin.DescribeAccountAssignmentCreationStatus
import Amazonka.SSOAdmin.DescribeAccountAssignmentDeletionStatus
import Amazonka.SSOAdmin.DescribeInstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.DescribePermissionSet
import Amazonka.SSOAdmin.DescribePermissionSetProvisioningStatus
import Amazonka.SSOAdmin.DetachCustomerManagedPolicyReferenceFromPermissionSet
import Amazonka.SSOAdmin.DetachManagedPolicyFromPermissionSet
import Amazonka.SSOAdmin.GetInlinePolicyForPermissionSet
import Amazonka.SSOAdmin.GetPermissionsBoundaryForPermissionSet
import Amazonka.SSOAdmin.ListAccountAssignmentCreationStatus
import Amazonka.SSOAdmin.ListAccountAssignmentDeletionStatus
import Amazonka.SSOAdmin.ListAccountAssignments
import Amazonka.SSOAdmin.ListAccountsForProvisionedPermissionSet
import Amazonka.SSOAdmin.ListCustomerManagedPolicyReferencesInPermissionSet
import Amazonka.SSOAdmin.ListInstances
import Amazonka.SSOAdmin.ListManagedPoliciesInPermissionSet
import Amazonka.SSOAdmin.ListPermissionSetProvisioningStatus
import Amazonka.SSOAdmin.ListPermissionSets
import Amazonka.SSOAdmin.ListPermissionSetsProvisionedToAccount
import Amazonka.SSOAdmin.ListTagsForResource
import Amazonka.SSOAdmin.ProvisionPermissionSet
import Amazonka.SSOAdmin.PutInlinePolicyToPermissionSet
import Amazonka.SSOAdmin.PutPermissionsBoundaryToPermissionSet
import Amazonka.SSOAdmin.TagResource
import Amazonka.SSOAdmin.Types.AccessControlAttribute
import Amazonka.SSOAdmin.Types.AccessControlAttributeValue
import Amazonka.SSOAdmin.Types.AccountAssignment
import Amazonka.SSOAdmin.Types.AccountAssignmentOperationStatus
import Amazonka.SSOAdmin.Types.AccountAssignmentOperationStatusMetadata
import Amazonka.SSOAdmin.Types.AttachedManagedPolicy
import Amazonka.SSOAdmin.Types.CustomerManagedPolicyReference
import Amazonka.SSOAdmin.Types.InstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.Types.InstanceMetadata
import Amazonka.SSOAdmin.Types.OperationStatusFilter
import Amazonka.SSOAdmin.Types.PermissionSet
import Amazonka.SSOAdmin.Types.PermissionSetProvisioningStatus
import Amazonka.SSOAdmin.Types.PermissionSetProvisioningStatusMetadata
import Amazonka.SSOAdmin.Types.PermissionsBoundary
import Amazonka.SSOAdmin.Types.Tag
import Amazonka.SSOAdmin.UntagResource
import Amazonka.SSOAdmin.UpdateInstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.UpdatePermissionSet

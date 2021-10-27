{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSOAdmin.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSOAdmin.Lens
  ( -- * Operations

    -- ** DescribePermissionSet
    describePermissionSet_instanceArn,
    describePermissionSet_permissionSetArn,
    describePermissionSetResponse_permissionSet,
    describePermissionSetResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_instanceArn,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateInstanceAccessControlAttributeConfiguration
    createInstanceAccessControlAttributeConfiguration_instanceArn,
    createInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration,
    createInstanceAccessControlAttributeConfigurationResponse_httpStatus,

    -- ** GetInlinePolicyForPermissionSet
    getInlinePolicyForPermissionSet_instanceArn,
    getInlinePolicyForPermissionSet_permissionSetArn,
    getInlinePolicyForPermissionSetResponse_inlinePolicy,
    getInlinePolicyForPermissionSetResponse_httpStatus,

    -- ** ListPermissionSets
    listPermissionSets_nextToken,
    listPermissionSets_maxResults,
    listPermissionSets_instanceArn,
    listPermissionSetsResponse_permissionSets,
    listPermissionSetsResponse_nextToken,
    listPermissionSetsResponse_httpStatus,

    -- ** DeletePermissionSet
    deletePermissionSet_instanceArn,
    deletePermissionSet_permissionSetArn,
    deletePermissionSetResponse_httpStatus,

    -- ** UpdatePermissionSet
    updatePermissionSet_relayState,
    updatePermissionSet_sessionDuration,
    updatePermissionSet_description,
    updatePermissionSet_instanceArn,
    updatePermissionSet_permissionSetArn,
    updatePermissionSetResponse_httpStatus,

    -- ** ProvisionPermissionSet
    provisionPermissionSet_targetId,
    provisionPermissionSet_instanceArn,
    provisionPermissionSet_permissionSetArn,
    provisionPermissionSet_targetType,
    provisionPermissionSetResponse_permissionSetProvisioningStatus,
    provisionPermissionSetResponse_httpStatus,

    -- ** ListAccountAssignments
    listAccountAssignments_nextToken,
    listAccountAssignments_maxResults,
    listAccountAssignments_instanceArn,
    listAccountAssignments_accountId,
    listAccountAssignments_permissionSetArn,
    listAccountAssignmentsResponse_accountAssignments,
    listAccountAssignmentsResponse_nextToken,
    listAccountAssignmentsResponse_httpStatus,

    -- ** DescribePermissionSetProvisioningStatus
    describePermissionSetProvisioningStatus_instanceArn,
    describePermissionSetProvisioningStatus_provisionPermissionSetRequestId,
    describePermissionSetProvisioningStatusResponse_permissionSetProvisioningStatus,
    describePermissionSetProvisioningStatusResponse_httpStatus,

    -- ** AttachManagedPolicyToPermissionSet
    attachManagedPolicyToPermissionSet_instanceArn,
    attachManagedPolicyToPermissionSet_permissionSetArn,
    attachManagedPolicyToPermissionSet_managedPolicyArn,
    attachManagedPolicyToPermissionSetResponse_httpStatus,

    -- ** ListManagedPoliciesInPermissionSet
    listManagedPoliciesInPermissionSet_nextToken,
    listManagedPoliciesInPermissionSet_maxResults,
    listManagedPoliciesInPermissionSet_instanceArn,
    listManagedPoliciesInPermissionSet_permissionSetArn,
    listManagedPoliciesInPermissionSetResponse_nextToken,
    listManagedPoliciesInPermissionSetResponse_attachedManagedPolicies,
    listManagedPoliciesInPermissionSetResponse_httpStatus,

    -- ** DeleteInlinePolicyFromPermissionSet
    deleteInlinePolicyFromPermissionSet_instanceArn,
    deleteInlinePolicyFromPermissionSet_permissionSetArn,
    deleteInlinePolicyFromPermissionSetResponse_httpStatus,

    -- ** DeleteInstanceAccessControlAttributeConfiguration
    deleteInstanceAccessControlAttributeConfiguration_instanceArn,
    deleteInstanceAccessControlAttributeConfigurationResponse_httpStatus,

    -- ** UpdateInstanceAccessControlAttributeConfiguration
    updateInstanceAccessControlAttributeConfiguration_instanceArn,
    updateInstanceAccessControlAttributeConfiguration_instanceAccessControlAttributeConfiguration,
    updateInstanceAccessControlAttributeConfigurationResponse_httpStatus,

    -- ** DescribeAccountAssignmentDeletionStatus
    describeAccountAssignmentDeletionStatus_instanceArn,
    describeAccountAssignmentDeletionStatus_accountAssignmentDeletionRequestId,
    describeAccountAssignmentDeletionStatusResponse_accountAssignmentDeletionStatus,
    describeAccountAssignmentDeletionStatusResponse_httpStatus,

    -- ** DescribeAccountAssignmentCreationStatus
    describeAccountAssignmentCreationStatus_instanceArn,
    describeAccountAssignmentCreationStatus_accountAssignmentCreationRequestId,
    describeAccountAssignmentCreationStatusResponse_accountAssignmentCreationStatus,
    describeAccountAssignmentCreationStatusResponse_httpStatus,

    -- ** PutInlinePolicyToPermissionSet
    putInlinePolicyToPermissionSet_instanceArn,
    putInlinePolicyToPermissionSet_permissionSetArn,
    putInlinePolicyToPermissionSet_inlinePolicy,
    putInlinePolicyToPermissionSetResponse_httpStatus,

    -- ** ListAccountsForProvisionedPermissionSet
    listAccountsForProvisionedPermissionSet_provisioningStatus,
    listAccountsForProvisionedPermissionSet_nextToken,
    listAccountsForProvisionedPermissionSet_maxResults,
    listAccountsForProvisionedPermissionSet_instanceArn,
    listAccountsForProvisionedPermissionSet_permissionSetArn,
    listAccountsForProvisionedPermissionSetResponse_accountIds,
    listAccountsForProvisionedPermissionSetResponse_nextToken,
    listAccountsForProvisionedPermissionSetResponse_httpStatus,

    -- ** ListPermissionSetsProvisionedToAccount
    listPermissionSetsProvisionedToAccount_provisioningStatus,
    listPermissionSetsProvisionedToAccount_nextToken,
    listPermissionSetsProvisionedToAccount_maxResults,
    listPermissionSetsProvisionedToAccount_instanceArn,
    listPermissionSetsProvisionedToAccount_accountId,
    listPermissionSetsProvisionedToAccountResponse_permissionSets,
    listPermissionSetsProvisionedToAccountResponse_nextToken,
    listPermissionSetsProvisionedToAccountResponse_httpStatus,

    -- ** DetachManagedPolicyFromPermissionSet
    detachManagedPolicyFromPermissionSet_instanceArn,
    detachManagedPolicyFromPermissionSet_permissionSetArn,
    detachManagedPolicyFromPermissionSet_managedPolicyArn,
    detachManagedPolicyFromPermissionSetResponse_httpStatus,

    -- ** ListAccountAssignmentCreationStatus
    listAccountAssignmentCreationStatus_nextToken,
    listAccountAssignmentCreationStatus_filter,
    listAccountAssignmentCreationStatus_maxResults,
    listAccountAssignmentCreationStatus_instanceArn,
    listAccountAssignmentCreationStatusResponse_accountAssignmentsCreationStatus,
    listAccountAssignmentCreationStatusResponse_nextToken,
    listAccountAssignmentCreationStatusResponse_httpStatus,

    -- ** CreatePermissionSet
    createPermissionSet_relayState,
    createPermissionSet_sessionDuration,
    createPermissionSet_description,
    createPermissionSet_tags,
    createPermissionSet_name,
    createPermissionSet_instanceArn,
    createPermissionSetResponse_permissionSet,
    createPermissionSetResponse_httpStatus,

    -- ** ListAccountAssignmentDeletionStatus
    listAccountAssignmentDeletionStatus_nextToken,
    listAccountAssignmentDeletionStatus_filter,
    listAccountAssignmentDeletionStatus_maxResults,
    listAccountAssignmentDeletionStatus_instanceArn,
    listAccountAssignmentDeletionStatusResponse_accountAssignmentsDeletionStatus,
    listAccountAssignmentDeletionStatusResponse_nextToken,
    listAccountAssignmentDeletionStatusResponse_httpStatus,

    -- ** TagResource
    tagResource_instanceArn,
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListInstances
    listInstances_nextToken,
    listInstances_maxResults,
    listInstancesResponse_nextToken,
    listInstancesResponse_instances,
    listInstancesResponse_httpStatus,

    -- ** DescribeInstanceAccessControlAttributeConfiguration
    describeInstanceAccessControlAttributeConfiguration_instanceArn,
    describeInstanceAccessControlAttributeConfigurationResponse_status,
    describeInstanceAccessControlAttributeConfigurationResponse_instanceAccessControlAttributeConfiguration,
    describeInstanceAccessControlAttributeConfigurationResponse_statusReason,
    describeInstanceAccessControlAttributeConfigurationResponse_httpStatus,

    -- ** UntagResource
    untagResource_instanceArn,
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteAccountAssignment
    deleteAccountAssignment_instanceArn,
    deleteAccountAssignment_targetId,
    deleteAccountAssignment_targetType,
    deleteAccountAssignment_permissionSetArn,
    deleteAccountAssignment_principalType,
    deleteAccountAssignment_principalId,
    deleteAccountAssignmentResponse_accountAssignmentDeletionStatus,
    deleteAccountAssignmentResponse_httpStatus,

    -- ** ListPermissionSetProvisioningStatus
    listPermissionSetProvisioningStatus_nextToken,
    listPermissionSetProvisioningStatus_filter,
    listPermissionSetProvisioningStatus_maxResults,
    listPermissionSetProvisioningStatus_instanceArn,
    listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus,
    listPermissionSetProvisioningStatusResponse_nextToken,
    listPermissionSetProvisioningStatusResponse_httpStatus,

    -- ** CreateAccountAssignment
    createAccountAssignment_instanceArn,
    createAccountAssignment_targetId,
    createAccountAssignment_targetType,
    createAccountAssignment_permissionSetArn,
    createAccountAssignment_principalType,
    createAccountAssignment_principalId,
    createAccountAssignmentResponse_accountAssignmentCreationStatus,
    createAccountAssignmentResponse_httpStatus,

    -- * Types

    -- ** AccessControlAttribute
    accessControlAttribute_key,
    accessControlAttribute_value,

    -- ** AccessControlAttributeValue
    accessControlAttributeValue_source,

    -- ** AccountAssignment
    accountAssignment_principalId,
    accountAssignment_principalType,
    accountAssignment_accountId,
    accountAssignment_permissionSetArn,

    -- ** AccountAssignmentOperationStatus
    accountAssignmentOperationStatus_requestId,
    accountAssignmentOperationStatus_status,
    accountAssignmentOperationStatus_failureReason,
    accountAssignmentOperationStatus_targetId,
    accountAssignmentOperationStatus_principalId,
    accountAssignmentOperationStatus_principalType,
    accountAssignmentOperationStatus_targetType,
    accountAssignmentOperationStatus_createdDate,
    accountAssignmentOperationStatus_permissionSetArn,

    -- ** AccountAssignmentOperationStatusMetadata
    accountAssignmentOperationStatusMetadata_requestId,
    accountAssignmentOperationStatusMetadata_status,
    accountAssignmentOperationStatusMetadata_createdDate,

    -- ** AttachedManagedPolicy
    attachedManagedPolicy_arn,
    attachedManagedPolicy_name,

    -- ** InstanceAccessControlAttributeConfiguration
    instanceAccessControlAttributeConfiguration_accessControlAttributes,

    -- ** InstanceMetadata
    instanceMetadata_identityStoreId,
    instanceMetadata_instanceArn,

    -- ** OperationStatusFilter
    operationStatusFilter_status,

    -- ** PermissionSet
    permissionSet_relayState,
    permissionSet_sessionDuration,
    permissionSet_createdDate,
    permissionSet_permissionSetArn,
    permissionSet_name,
    permissionSet_description,

    -- ** PermissionSetProvisioningStatus
    permissionSetProvisioningStatus_requestId,
    permissionSetProvisioningStatus_status,
    permissionSetProvisioningStatus_failureReason,
    permissionSetProvisioningStatus_accountId,
    permissionSetProvisioningStatus_createdDate,
    permissionSetProvisioningStatus_permissionSetArn,

    -- ** PermissionSetProvisioningStatusMetadata
    permissionSetProvisioningStatusMetadata_requestId,
    permissionSetProvisioningStatusMetadata_status,
    permissionSetProvisioningStatusMetadata_createdDate,

    -- ** Tag
    tag_value,
    tag_key,
  )
where

import Network.AWS.SSOAdmin.AttachManagedPolicyToPermissionSet
import Network.AWS.SSOAdmin.CreateAccountAssignment
import Network.AWS.SSOAdmin.CreateInstanceAccessControlAttributeConfiguration
import Network.AWS.SSOAdmin.CreatePermissionSet
import Network.AWS.SSOAdmin.DeleteAccountAssignment
import Network.AWS.SSOAdmin.DeleteInlinePolicyFromPermissionSet
import Network.AWS.SSOAdmin.DeleteInstanceAccessControlAttributeConfiguration
import Network.AWS.SSOAdmin.DeletePermissionSet
import Network.AWS.SSOAdmin.DescribeAccountAssignmentCreationStatus
import Network.AWS.SSOAdmin.DescribeAccountAssignmentDeletionStatus
import Network.AWS.SSOAdmin.DescribeInstanceAccessControlAttributeConfiguration
import Network.AWS.SSOAdmin.DescribePermissionSet
import Network.AWS.SSOAdmin.DescribePermissionSetProvisioningStatus
import Network.AWS.SSOAdmin.DetachManagedPolicyFromPermissionSet
import Network.AWS.SSOAdmin.GetInlinePolicyForPermissionSet
import Network.AWS.SSOAdmin.ListAccountAssignmentCreationStatus
import Network.AWS.SSOAdmin.ListAccountAssignmentDeletionStatus
import Network.AWS.SSOAdmin.ListAccountAssignments
import Network.AWS.SSOAdmin.ListAccountsForProvisionedPermissionSet
import Network.AWS.SSOAdmin.ListInstances
import Network.AWS.SSOAdmin.ListManagedPoliciesInPermissionSet
import Network.AWS.SSOAdmin.ListPermissionSetProvisioningStatus
import Network.AWS.SSOAdmin.ListPermissionSets
import Network.AWS.SSOAdmin.ListPermissionSetsProvisionedToAccount
import Network.AWS.SSOAdmin.ListTagsForResource
import Network.AWS.SSOAdmin.ProvisionPermissionSet
import Network.AWS.SSOAdmin.PutInlinePolicyToPermissionSet
import Network.AWS.SSOAdmin.TagResource
import Network.AWS.SSOAdmin.Types.AccessControlAttribute
import Network.AWS.SSOAdmin.Types.AccessControlAttributeValue
import Network.AWS.SSOAdmin.Types.AccountAssignment
import Network.AWS.SSOAdmin.Types.AccountAssignmentOperationStatus
import Network.AWS.SSOAdmin.Types.AccountAssignmentOperationStatusMetadata
import Network.AWS.SSOAdmin.Types.AttachedManagedPolicy
import Network.AWS.SSOAdmin.Types.InstanceAccessControlAttributeConfiguration
import Network.AWS.SSOAdmin.Types.InstanceMetadata
import Network.AWS.SSOAdmin.Types.OperationStatusFilter
import Network.AWS.SSOAdmin.Types.PermissionSet
import Network.AWS.SSOAdmin.Types.PermissionSetProvisioningStatus
import Network.AWS.SSOAdmin.Types.PermissionSetProvisioningStatusMetadata
import Network.AWS.SSOAdmin.Types.Tag
import Network.AWS.SSOAdmin.UntagResource
import Network.AWS.SSOAdmin.UpdateInstanceAccessControlAttributeConfiguration
import Network.AWS.SSOAdmin.UpdatePermissionSet

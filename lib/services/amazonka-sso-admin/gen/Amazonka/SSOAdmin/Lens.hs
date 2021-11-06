{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSOAdmin.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Lens
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

import Amazonka.SSOAdmin.AttachManagedPolicyToPermissionSet
import Amazonka.SSOAdmin.CreateAccountAssignment
import Amazonka.SSOAdmin.CreateInstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.CreatePermissionSet
import Amazonka.SSOAdmin.DeleteAccountAssignment
import Amazonka.SSOAdmin.DeleteInlinePolicyFromPermissionSet
import Amazonka.SSOAdmin.DeleteInstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.DeletePermissionSet
import Amazonka.SSOAdmin.DescribeAccountAssignmentCreationStatus
import Amazonka.SSOAdmin.DescribeAccountAssignmentDeletionStatus
import Amazonka.SSOAdmin.DescribeInstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.DescribePermissionSet
import Amazonka.SSOAdmin.DescribePermissionSetProvisioningStatus
import Amazonka.SSOAdmin.DetachManagedPolicyFromPermissionSet
import Amazonka.SSOAdmin.GetInlinePolicyForPermissionSet
import Amazonka.SSOAdmin.ListAccountAssignmentCreationStatus
import Amazonka.SSOAdmin.ListAccountAssignmentDeletionStatus
import Amazonka.SSOAdmin.ListAccountAssignments
import Amazonka.SSOAdmin.ListAccountsForProvisionedPermissionSet
import Amazonka.SSOAdmin.ListInstances
import Amazonka.SSOAdmin.ListManagedPoliciesInPermissionSet
import Amazonka.SSOAdmin.ListPermissionSetProvisioningStatus
import Amazonka.SSOAdmin.ListPermissionSets
import Amazonka.SSOAdmin.ListPermissionSetsProvisionedToAccount
import Amazonka.SSOAdmin.ListTagsForResource
import Amazonka.SSOAdmin.ProvisionPermissionSet
import Amazonka.SSOAdmin.PutInlinePolicyToPermissionSet
import Amazonka.SSOAdmin.TagResource
import Amazonka.SSOAdmin.Types.AccessControlAttribute
import Amazonka.SSOAdmin.Types.AccessControlAttributeValue
import Amazonka.SSOAdmin.Types.AccountAssignment
import Amazonka.SSOAdmin.Types.AccountAssignmentOperationStatus
import Amazonka.SSOAdmin.Types.AccountAssignmentOperationStatusMetadata
import Amazonka.SSOAdmin.Types.AttachedManagedPolicy
import Amazonka.SSOAdmin.Types.InstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.Types.InstanceMetadata
import Amazonka.SSOAdmin.Types.OperationStatusFilter
import Amazonka.SSOAdmin.Types.PermissionSet
import Amazonka.SSOAdmin.Types.PermissionSetProvisioningStatus
import Amazonka.SSOAdmin.Types.PermissionSetProvisioningStatusMetadata
import Amazonka.SSOAdmin.Types.Tag
import Amazonka.SSOAdmin.UntagResource
import Amazonka.SSOAdmin.UpdateInstanceAccessControlAttributeConfiguration
import Amazonka.SSOAdmin.UpdatePermissionSet

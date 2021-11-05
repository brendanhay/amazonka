{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.SSOAdmin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-07-20@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Single Sign On (SSO) is a cloud SSO service that
-- makes it easy to centrally manage SSO access to multiple Amazon Web
-- Services accounts and business applications. This guide provides
-- information on SSO operations which could be used for access management
-- of Amazon Web Services accounts. For information about Amazon Web
-- Services SSO features, see the
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/what-is.html Amazon Web Services Single Sign-On User Guide>.
--
-- Many operations in the SSO APIs rely on identifiers for users and
-- groups, known as principals. For more information about how to work with
-- principals and principal IDs in Amazon Web Services SSO, see the
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/welcome.html Amazon Web Services SSO Identity Store API Reference>.
module Network.AWS.SSOAdmin
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribePermissionSet
    DescribePermissionSet (DescribePermissionSet'),
    newDescribePermissionSet,
    DescribePermissionSetResponse (DescribePermissionSetResponse'),
    newDescribePermissionSetResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateInstanceAccessControlAttributeConfiguration
    CreateInstanceAccessControlAttributeConfiguration (CreateInstanceAccessControlAttributeConfiguration'),
    newCreateInstanceAccessControlAttributeConfiguration,
    CreateInstanceAccessControlAttributeConfigurationResponse (CreateInstanceAccessControlAttributeConfigurationResponse'),
    newCreateInstanceAccessControlAttributeConfigurationResponse,

    -- ** GetInlinePolicyForPermissionSet
    GetInlinePolicyForPermissionSet (GetInlinePolicyForPermissionSet'),
    newGetInlinePolicyForPermissionSet,
    GetInlinePolicyForPermissionSetResponse (GetInlinePolicyForPermissionSetResponse'),
    newGetInlinePolicyForPermissionSetResponse,

    -- ** ListPermissionSets (Paginated)
    ListPermissionSets (ListPermissionSets'),
    newListPermissionSets,
    ListPermissionSetsResponse (ListPermissionSetsResponse'),
    newListPermissionSetsResponse,

    -- ** DeletePermissionSet
    DeletePermissionSet (DeletePermissionSet'),
    newDeletePermissionSet,
    DeletePermissionSetResponse (DeletePermissionSetResponse'),
    newDeletePermissionSetResponse,

    -- ** UpdatePermissionSet
    UpdatePermissionSet (UpdatePermissionSet'),
    newUpdatePermissionSet,
    UpdatePermissionSetResponse (UpdatePermissionSetResponse'),
    newUpdatePermissionSetResponse,

    -- ** ProvisionPermissionSet
    ProvisionPermissionSet (ProvisionPermissionSet'),
    newProvisionPermissionSet,
    ProvisionPermissionSetResponse (ProvisionPermissionSetResponse'),
    newProvisionPermissionSetResponse,

    -- ** ListAccountAssignments (Paginated)
    ListAccountAssignments (ListAccountAssignments'),
    newListAccountAssignments,
    ListAccountAssignmentsResponse (ListAccountAssignmentsResponse'),
    newListAccountAssignmentsResponse,

    -- ** DescribePermissionSetProvisioningStatus
    DescribePermissionSetProvisioningStatus (DescribePermissionSetProvisioningStatus'),
    newDescribePermissionSetProvisioningStatus,
    DescribePermissionSetProvisioningStatusResponse (DescribePermissionSetProvisioningStatusResponse'),
    newDescribePermissionSetProvisioningStatusResponse,

    -- ** AttachManagedPolicyToPermissionSet
    AttachManagedPolicyToPermissionSet (AttachManagedPolicyToPermissionSet'),
    newAttachManagedPolicyToPermissionSet,
    AttachManagedPolicyToPermissionSetResponse (AttachManagedPolicyToPermissionSetResponse'),
    newAttachManagedPolicyToPermissionSetResponse,

    -- ** ListManagedPoliciesInPermissionSet (Paginated)
    ListManagedPoliciesInPermissionSet (ListManagedPoliciesInPermissionSet'),
    newListManagedPoliciesInPermissionSet,
    ListManagedPoliciesInPermissionSetResponse (ListManagedPoliciesInPermissionSetResponse'),
    newListManagedPoliciesInPermissionSetResponse,

    -- ** DeleteInlinePolicyFromPermissionSet
    DeleteInlinePolicyFromPermissionSet (DeleteInlinePolicyFromPermissionSet'),
    newDeleteInlinePolicyFromPermissionSet,
    DeleteInlinePolicyFromPermissionSetResponse (DeleteInlinePolicyFromPermissionSetResponse'),
    newDeleteInlinePolicyFromPermissionSetResponse,

    -- ** DeleteInstanceAccessControlAttributeConfiguration
    DeleteInstanceAccessControlAttributeConfiguration (DeleteInstanceAccessControlAttributeConfiguration'),
    newDeleteInstanceAccessControlAttributeConfiguration,
    DeleteInstanceAccessControlAttributeConfigurationResponse (DeleteInstanceAccessControlAttributeConfigurationResponse'),
    newDeleteInstanceAccessControlAttributeConfigurationResponse,

    -- ** UpdateInstanceAccessControlAttributeConfiguration
    UpdateInstanceAccessControlAttributeConfiguration (UpdateInstanceAccessControlAttributeConfiguration'),
    newUpdateInstanceAccessControlAttributeConfiguration,
    UpdateInstanceAccessControlAttributeConfigurationResponse (UpdateInstanceAccessControlAttributeConfigurationResponse'),
    newUpdateInstanceAccessControlAttributeConfigurationResponse,

    -- ** DescribeAccountAssignmentDeletionStatus
    DescribeAccountAssignmentDeletionStatus (DescribeAccountAssignmentDeletionStatus'),
    newDescribeAccountAssignmentDeletionStatus,
    DescribeAccountAssignmentDeletionStatusResponse (DescribeAccountAssignmentDeletionStatusResponse'),
    newDescribeAccountAssignmentDeletionStatusResponse,

    -- ** DescribeAccountAssignmentCreationStatus
    DescribeAccountAssignmentCreationStatus (DescribeAccountAssignmentCreationStatus'),
    newDescribeAccountAssignmentCreationStatus,
    DescribeAccountAssignmentCreationStatusResponse (DescribeAccountAssignmentCreationStatusResponse'),
    newDescribeAccountAssignmentCreationStatusResponse,

    -- ** PutInlinePolicyToPermissionSet
    PutInlinePolicyToPermissionSet (PutInlinePolicyToPermissionSet'),
    newPutInlinePolicyToPermissionSet,
    PutInlinePolicyToPermissionSetResponse (PutInlinePolicyToPermissionSetResponse'),
    newPutInlinePolicyToPermissionSetResponse,

    -- ** ListAccountsForProvisionedPermissionSet (Paginated)
    ListAccountsForProvisionedPermissionSet (ListAccountsForProvisionedPermissionSet'),
    newListAccountsForProvisionedPermissionSet,
    ListAccountsForProvisionedPermissionSetResponse (ListAccountsForProvisionedPermissionSetResponse'),
    newListAccountsForProvisionedPermissionSetResponse,

    -- ** ListPermissionSetsProvisionedToAccount (Paginated)
    ListPermissionSetsProvisionedToAccount (ListPermissionSetsProvisionedToAccount'),
    newListPermissionSetsProvisionedToAccount,
    ListPermissionSetsProvisionedToAccountResponse (ListPermissionSetsProvisionedToAccountResponse'),
    newListPermissionSetsProvisionedToAccountResponse,

    -- ** DetachManagedPolicyFromPermissionSet
    DetachManagedPolicyFromPermissionSet (DetachManagedPolicyFromPermissionSet'),
    newDetachManagedPolicyFromPermissionSet,
    DetachManagedPolicyFromPermissionSetResponse (DetachManagedPolicyFromPermissionSetResponse'),
    newDetachManagedPolicyFromPermissionSetResponse,

    -- ** ListAccountAssignmentCreationStatus (Paginated)
    ListAccountAssignmentCreationStatus (ListAccountAssignmentCreationStatus'),
    newListAccountAssignmentCreationStatus,
    ListAccountAssignmentCreationStatusResponse (ListAccountAssignmentCreationStatusResponse'),
    newListAccountAssignmentCreationStatusResponse,

    -- ** CreatePermissionSet
    CreatePermissionSet (CreatePermissionSet'),
    newCreatePermissionSet,
    CreatePermissionSetResponse (CreatePermissionSetResponse'),
    newCreatePermissionSetResponse,

    -- ** ListAccountAssignmentDeletionStatus (Paginated)
    ListAccountAssignmentDeletionStatus (ListAccountAssignmentDeletionStatus'),
    newListAccountAssignmentDeletionStatus,
    ListAccountAssignmentDeletionStatusResponse (ListAccountAssignmentDeletionStatusResponse'),
    newListAccountAssignmentDeletionStatusResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListInstances (Paginated)
    ListInstances (ListInstances'),
    newListInstances,
    ListInstancesResponse (ListInstancesResponse'),
    newListInstancesResponse,

    -- ** DescribeInstanceAccessControlAttributeConfiguration
    DescribeInstanceAccessControlAttributeConfiguration (DescribeInstanceAccessControlAttributeConfiguration'),
    newDescribeInstanceAccessControlAttributeConfiguration,
    DescribeInstanceAccessControlAttributeConfigurationResponse (DescribeInstanceAccessControlAttributeConfigurationResponse'),
    newDescribeInstanceAccessControlAttributeConfigurationResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteAccountAssignment
    DeleteAccountAssignment (DeleteAccountAssignment'),
    newDeleteAccountAssignment,
    DeleteAccountAssignmentResponse (DeleteAccountAssignmentResponse'),
    newDeleteAccountAssignmentResponse,

    -- ** ListPermissionSetProvisioningStatus (Paginated)
    ListPermissionSetProvisioningStatus (ListPermissionSetProvisioningStatus'),
    newListPermissionSetProvisioningStatus,
    ListPermissionSetProvisioningStatusResponse (ListPermissionSetProvisioningStatusResponse'),
    newListPermissionSetProvisioningStatusResponse,

    -- ** CreateAccountAssignment
    CreateAccountAssignment (CreateAccountAssignment'),
    newCreateAccountAssignment,
    CreateAccountAssignmentResponse (CreateAccountAssignmentResponse'),
    newCreateAccountAssignmentResponse,

    -- * Types

    -- ** InstanceAccessControlAttributeConfigurationStatus
    InstanceAccessControlAttributeConfigurationStatus (..),

    -- ** PrincipalType
    PrincipalType (..),

    -- ** ProvisionTargetType
    ProvisionTargetType (..),

    -- ** ProvisioningStatus
    ProvisioningStatus (..),

    -- ** StatusValues
    StatusValues (..),

    -- ** TargetType
    TargetType (..),

    -- ** AccessControlAttribute
    AccessControlAttribute (AccessControlAttribute'),
    newAccessControlAttribute,

    -- ** AccessControlAttributeValue
    AccessControlAttributeValue (AccessControlAttributeValue'),
    newAccessControlAttributeValue,

    -- ** AccountAssignment
    AccountAssignment (AccountAssignment'),
    newAccountAssignment,

    -- ** AccountAssignmentOperationStatus
    AccountAssignmentOperationStatus (AccountAssignmentOperationStatus'),
    newAccountAssignmentOperationStatus,

    -- ** AccountAssignmentOperationStatusMetadata
    AccountAssignmentOperationStatusMetadata (AccountAssignmentOperationStatusMetadata'),
    newAccountAssignmentOperationStatusMetadata,

    -- ** AttachedManagedPolicy
    AttachedManagedPolicy (AttachedManagedPolicy'),
    newAttachedManagedPolicy,

    -- ** InstanceAccessControlAttributeConfiguration
    InstanceAccessControlAttributeConfiguration (InstanceAccessControlAttributeConfiguration'),
    newInstanceAccessControlAttributeConfiguration,

    -- ** InstanceMetadata
    InstanceMetadata (InstanceMetadata'),
    newInstanceMetadata,

    -- ** OperationStatusFilter
    OperationStatusFilter (OperationStatusFilter'),
    newOperationStatusFilter,

    -- ** PermissionSet
    PermissionSet (PermissionSet'),
    newPermissionSet,

    -- ** PermissionSetProvisioningStatus
    PermissionSetProvisioningStatus (PermissionSetProvisioningStatus'),
    newPermissionSetProvisioningStatus,

    -- ** PermissionSetProvisioningStatusMetadata
    PermissionSetProvisioningStatusMetadata (PermissionSetProvisioningStatusMetadata'),
    newPermissionSetProvisioningStatusMetadata,

    -- ** Tag
    Tag (Tag'),
    newTag,
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
import Network.AWS.SSOAdmin.Lens
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
import Network.AWS.SSOAdmin.Types
import Network.AWS.SSOAdmin.UntagResource
import Network.AWS.SSOAdmin.UpdateInstanceAccessControlAttributeConfiguration
import Network.AWS.SSOAdmin.UpdatePermissionSet
import Network.AWS.SSOAdmin.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SSOAdmin'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

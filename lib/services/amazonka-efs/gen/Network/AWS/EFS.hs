{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.EFS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-02-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Elastic File System
--
-- Amazon Elastic File System (Amazon EFS) provides simple, scalable file
-- storage for use with Amazon EC2 instances in the Amazon Web Services
-- Cloud. With Amazon EFS, storage capacity is elastic, growing and
-- shrinking automatically as you add and remove files, so your
-- applications have the storage they need, when they need it. For more
-- information, see the
-- <https://docs.aws.amazon.com/efs/latest/ug/api-reference.html Amazon Elastic File System API Reference>
-- and the
-- <https://docs.aws.amazon.com/efs/latest/ug/whatisefs.html Amazon Elastic File System User Guide>.
module Network.AWS.EFS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** MountTargetNotFound
    _MountTargetNotFound,

    -- ** SecurityGroupLimitExceeded
    _SecurityGroupLimitExceeded,

    -- ** SecurityGroupNotFound
    _SecurityGroupNotFound,

    -- ** MountTargetConflict
    _MountTargetConflict,

    -- ** UnsupportedAvailabilityZone
    _UnsupportedAvailabilityZone,

    -- ** FileSystemLimitExceeded
    _FileSystemLimitExceeded,

    -- ** TooManyRequests
    _TooManyRequests,

    -- ** NetworkInterfaceLimitExceeded
    _NetworkInterfaceLimitExceeded,

    -- ** FileSystemAlreadyExists
    _FileSystemAlreadyExists,

    -- ** SubnetNotFound
    _SubnetNotFound,

    -- ** AvailabilityZonesMismatch
    _AvailabilityZonesMismatch,

    -- ** FileSystemNotFound
    _FileSystemNotFound,

    -- ** IncorrectFileSystemLifeCycleState
    _IncorrectFileSystemLifeCycleState,

    -- ** BadRequest
    _BadRequest,

    -- ** NoFreeAddressesInSubnet
    _NoFreeAddressesInSubnet,

    -- ** ThroughputLimitExceeded
    _ThroughputLimitExceeded,

    -- ** DependencyTimeout
    _DependencyTimeout,

    -- ** FileSystemInUse
    _FileSystemInUse,

    -- ** IncorrectMountTargetState
    _IncorrectMountTargetState,

    -- ** InternalServerError
    _InternalServerError,

    -- ** IpAddressInUse
    _IpAddressInUse,

    -- ** PolicyNotFound
    _PolicyNotFound,

    -- ** AccessPointNotFound
    _AccessPointNotFound,

    -- ** InsufficientThroughputCapacity
    _InsufficientThroughputCapacity,

    -- ** InvalidPolicyException
    _InvalidPolicyException,

    -- ** AccessPointAlreadyExists
    _AccessPointAlreadyExists,

    -- ** AccessPointLimitExceeded
    _AccessPointLimitExceeded,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAccessPoint
    CreateAccessPoint (CreateAccessPoint'),
    newCreateAccessPoint,
    AccessPointDescription (AccessPointDescription'),
    newAccessPointDescription,

    -- ** DescribeAccountPreferences
    DescribeAccountPreferences (DescribeAccountPreferences'),
    newDescribeAccountPreferences,
    DescribeAccountPreferencesResponse (DescribeAccountPreferencesResponse'),
    newDescribeAccountPreferencesResponse,

    -- ** DescribeMountTargets (Paginated)
    DescribeMountTargets (DescribeMountTargets'),
    newDescribeMountTargets,
    DescribeMountTargetsResponse (DescribeMountTargetsResponse'),
    newDescribeMountTargetsResponse,

    -- ** DeleteFileSystemPolicy
    DeleteFileSystemPolicy (DeleteFileSystemPolicy'),
    newDeleteFileSystemPolicy,
    DeleteFileSystemPolicyResponse (DeleteFileSystemPolicyResponse'),
    newDeleteFileSystemPolicyResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutFileSystemPolicy
    PutFileSystemPolicy (PutFileSystemPolicy'),
    newPutFileSystemPolicy,
    FileSystemPolicyDescription (FileSystemPolicyDescription'),
    newFileSystemPolicyDescription,

    -- ** DescribeFileSystems (Paginated)
    DescribeFileSystems (DescribeFileSystems'),
    newDescribeFileSystems,
    DescribeFileSystemsResponse (DescribeFileSystemsResponse'),
    newDescribeFileSystemsResponse,

    -- ** DeleteMountTarget
    DeleteMountTarget (DeleteMountTarget'),
    newDeleteMountTarget,
    DeleteMountTargetResponse (DeleteMountTargetResponse'),
    newDeleteMountTargetResponse,

    -- ** PutAccountPreferences
    PutAccountPreferences (PutAccountPreferences'),
    newPutAccountPreferences,
    PutAccountPreferencesResponse (PutAccountPreferencesResponse'),
    newPutAccountPreferencesResponse,

    -- ** DescribeMountTargetSecurityGroups
    DescribeMountTargetSecurityGroups (DescribeMountTargetSecurityGroups'),
    newDescribeMountTargetSecurityGroups,
    DescribeMountTargetSecurityGroupsResponse (DescribeMountTargetSecurityGroupsResponse'),
    newDescribeMountTargetSecurityGroupsResponse,

    -- ** DescribeAccessPoints
    DescribeAccessPoints (DescribeAccessPoints'),
    newDescribeAccessPoints,
    DescribeAccessPointsResponse (DescribeAccessPointsResponse'),
    newDescribeAccessPointsResponse,

    -- ** ModifyMountTargetSecurityGroups
    ModifyMountTargetSecurityGroups (ModifyMountTargetSecurityGroups'),
    newModifyMountTargetSecurityGroups,
    ModifyMountTargetSecurityGroupsResponse (ModifyMountTargetSecurityGroupsResponse'),
    newModifyMountTargetSecurityGroupsResponse,

    -- ** CreateFileSystem
    CreateFileSystem (CreateFileSystem'),
    newCreateFileSystem,
    FileSystemDescription (FileSystemDescription'),
    newFileSystemDescription,

    -- ** PutLifecycleConfiguration
    PutLifecycleConfiguration (PutLifecycleConfiguration'),
    newPutLifecycleConfiguration,
    LifecycleConfigurationDescription (LifecycleConfigurationDescription'),
    newLifecycleConfigurationDescription,

    -- ** PutBackupPolicy
    PutBackupPolicy (PutBackupPolicy'),
    newPutBackupPolicy,
    BackupPolicyDescription (BackupPolicyDescription'),
    newBackupPolicyDescription,

    -- ** DeleteFileSystem
    DeleteFileSystem (DeleteFileSystem'),
    newDeleteFileSystem,
    DeleteFileSystemResponse (DeleteFileSystemResponse'),
    newDeleteFileSystemResponse,

    -- ** UpdateFileSystem
    UpdateFileSystem (UpdateFileSystem'),
    newUpdateFileSystem,
    FileSystemDescription (FileSystemDescription'),
    newFileSystemDescription,

    -- ** CreateMountTarget
    CreateMountTarget (CreateMountTarget'),
    newCreateMountTarget,
    MountTargetDescription (MountTargetDescription'),
    newMountTargetDescription,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeBackupPolicy
    DescribeBackupPolicy (DescribeBackupPolicy'),
    newDescribeBackupPolicy,
    BackupPolicyDescription (BackupPolicyDescription'),
    newBackupPolicyDescription,

    -- ** DescribeLifecycleConfiguration
    DescribeLifecycleConfiguration (DescribeLifecycleConfiguration'),
    newDescribeLifecycleConfiguration,
    LifecycleConfigurationDescription (LifecycleConfigurationDescription'),
    newLifecycleConfigurationDescription,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeFileSystemPolicy
    DescribeFileSystemPolicy (DescribeFileSystemPolicy'),
    newDescribeFileSystemPolicy,
    FileSystemPolicyDescription (FileSystemPolicyDescription'),
    newFileSystemPolicyDescription,

    -- ** DeleteAccessPoint
    DeleteAccessPoint (DeleteAccessPoint'),
    newDeleteAccessPoint,
    DeleteAccessPointResponse (DeleteAccessPointResponse'),
    newDeleteAccessPointResponse,

    -- * Types

    -- ** BackupStatus
    BackupStatus (..),

    -- ** LifeCycleState
    LifeCycleState (..),

    -- ** PerformanceMode
    PerformanceMode (..),

    -- ** Resource
    Resource (..),

    -- ** ResourceIdType
    ResourceIdType (..),

    -- ** ThroughputMode
    ThroughputMode (..),

    -- ** TransitionToIARules
    TransitionToIARules (..),

    -- ** TransitionToPrimaryStorageClassRules
    TransitionToPrimaryStorageClassRules (..),

    -- ** AccessPointDescription
    AccessPointDescription (AccessPointDescription'),
    newAccessPointDescription,

    -- ** BackupPolicy
    BackupPolicy (BackupPolicy'),
    newBackupPolicy,

    -- ** BackupPolicyDescription
    BackupPolicyDescription (BackupPolicyDescription'),
    newBackupPolicyDescription,

    -- ** CreationInfo
    CreationInfo (CreationInfo'),
    newCreationInfo,

    -- ** FileSystemDescription
    FileSystemDescription (FileSystemDescription'),
    newFileSystemDescription,

    -- ** FileSystemPolicyDescription
    FileSystemPolicyDescription (FileSystemPolicyDescription'),
    newFileSystemPolicyDescription,

    -- ** FileSystemSize
    FileSystemSize (FileSystemSize'),
    newFileSystemSize,

    -- ** LifecycleConfigurationDescription
    LifecycleConfigurationDescription (LifecycleConfigurationDescription'),
    newLifecycleConfigurationDescription,

    -- ** LifecyclePolicy
    LifecyclePolicy (LifecyclePolicy'),
    newLifecyclePolicy,

    -- ** MountTargetDescription
    MountTargetDescription (MountTargetDescription'),
    newMountTargetDescription,

    -- ** PosixUser
    PosixUser (PosixUser'),
    newPosixUser,

    -- ** ResourceIdPreference
    ResourceIdPreference (ResourceIdPreference'),
    newResourceIdPreference,

    -- ** RootDirectory
    RootDirectory (RootDirectory'),
    newRootDirectory,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.EFS.CreateAccessPoint
import Network.AWS.EFS.CreateFileSystem
import Network.AWS.EFS.CreateMountTarget
import Network.AWS.EFS.DeleteAccessPoint
import Network.AWS.EFS.DeleteFileSystem
import Network.AWS.EFS.DeleteFileSystemPolicy
import Network.AWS.EFS.DeleteMountTarget
import Network.AWS.EFS.DescribeAccessPoints
import Network.AWS.EFS.DescribeAccountPreferences
import Network.AWS.EFS.DescribeBackupPolicy
import Network.AWS.EFS.DescribeFileSystemPolicy
import Network.AWS.EFS.DescribeFileSystems
import Network.AWS.EFS.DescribeLifecycleConfiguration
import Network.AWS.EFS.DescribeMountTargetSecurityGroups
import Network.AWS.EFS.DescribeMountTargets
import Network.AWS.EFS.Lens
import Network.AWS.EFS.ListTagsForResource
import Network.AWS.EFS.ModifyMountTargetSecurityGroups
import Network.AWS.EFS.PutAccountPreferences
import Network.AWS.EFS.PutBackupPolicy
import Network.AWS.EFS.PutFileSystemPolicy
import Network.AWS.EFS.PutLifecycleConfiguration
import Network.AWS.EFS.TagResource
import Network.AWS.EFS.Types
import Network.AWS.EFS.UntagResource
import Network.AWS.EFS.UpdateFileSystem
import Network.AWS.EFS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'EFS'.

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

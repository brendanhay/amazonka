{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.EFS
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- storage for use with Amazon EC2 Linux and Mac instances in the Amazon
-- Web Services Cloud. With Amazon EFS, storage capacity is elastic,
-- growing and shrinking automatically as you add and remove files, so that
-- your applications have the storage they need, when they need it. For
-- more information, see the
-- <https://docs.aws.amazon.com/efs/latest/ug/api-reference.html Amazon Elastic File System API Reference>
-- and the
-- <https://docs.aws.amazon.com/efs/latest/ug/whatisefs.html Amazon Elastic File System User Guide>.
module Amazonka.EFS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessPointAlreadyExists
    _AccessPointAlreadyExists,

    -- ** AccessPointLimitExceeded
    _AccessPointLimitExceeded,

    -- ** AccessPointNotFound
    _AccessPointNotFound,

    -- ** AvailabilityZonesMismatch
    _AvailabilityZonesMismatch,

    -- ** BadRequest
    _BadRequest,

    -- ** DependencyTimeout
    _DependencyTimeout,

    -- ** FileSystemAlreadyExists
    _FileSystemAlreadyExists,

    -- ** FileSystemInUse
    _FileSystemInUse,

    -- ** FileSystemLimitExceeded
    _FileSystemLimitExceeded,

    -- ** FileSystemNotFound
    _FileSystemNotFound,

    -- ** IncorrectFileSystemLifeCycleState
    _IncorrectFileSystemLifeCycleState,

    -- ** IncorrectMountTargetState
    _IncorrectMountTargetState,

    -- ** InsufficientThroughputCapacity
    _InsufficientThroughputCapacity,

    -- ** InternalServerError
    _InternalServerError,

    -- ** InvalidPolicyException
    _InvalidPolicyException,

    -- ** IpAddressInUse
    _IpAddressInUse,

    -- ** MountTargetConflict
    _MountTargetConflict,

    -- ** MountTargetNotFound
    _MountTargetNotFound,

    -- ** NetworkInterfaceLimitExceeded
    _NetworkInterfaceLimitExceeded,

    -- ** NoFreeAddressesInSubnet
    _NoFreeAddressesInSubnet,

    -- ** PolicyNotFound
    _PolicyNotFound,

    -- ** ReplicationNotFound
    _ReplicationNotFound,

    -- ** SecurityGroupLimitExceeded
    _SecurityGroupLimitExceeded,

    -- ** SecurityGroupNotFound
    _SecurityGroupNotFound,

    -- ** SubnetNotFound
    _SubnetNotFound,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ThroughputLimitExceeded
    _ThroughputLimitExceeded,

    -- ** TooManyRequests
    _TooManyRequests,

    -- ** UnsupportedAvailabilityZone
    _UnsupportedAvailabilityZone,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAccessPoint
    CreateAccessPoint (CreateAccessPoint'),
    newCreateAccessPoint,
    AccessPointDescription (AccessPointDescription'),
    newAccessPointDescription,

    -- ** CreateFileSystem
    CreateFileSystem (CreateFileSystem'),
    newCreateFileSystem,
    FileSystemDescription (FileSystemDescription'),
    newFileSystemDescription,

    -- ** CreateMountTarget
    CreateMountTarget (CreateMountTarget'),
    newCreateMountTarget,
    MountTargetDescription (MountTargetDescription'),
    newMountTargetDescription,

    -- ** CreateReplicationConfiguration
    CreateReplicationConfiguration (CreateReplicationConfiguration'),
    newCreateReplicationConfiguration,
    ReplicationConfigurationDescription (ReplicationConfigurationDescription'),
    newReplicationConfigurationDescription,

    -- ** DeleteAccessPoint
    DeleteAccessPoint (DeleteAccessPoint'),
    newDeleteAccessPoint,
    DeleteAccessPointResponse (DeleteAccessPointResponse'),
    newDeleteAccessPointResponse,

    -- ** DeleteFileSystem
    DeleteFileSystem (DeleteFileSystem'),
    newDeleteFileSystem,
    DeleteFileSystemResponse (DeleteFileSystemResponse'),
    newDeleteFileSystemResponse,

    -- ** DeleteFileSystemPolicy
    DeleteFileSystemPolicy (DeleteFileSystemPolicy'),
    newDeleteFileSystemPolicy,
    DeleteFileSystemPolicyResponse (DeleteFileSystemPolicyResponse'),
    newDeleteFileSystemPolicyResponse,

    -- ** DeleteMountTarget
    DeleteMountTarget (DeleteMountTarget'),
    newDeleteMountTarget,
    DeleteMountTargetResponse (DeleteMountTargetResponse'),
    newDeleteMountTargetResponse,

    -- ** DeleteReplicationConfiguration
    DeleteReplicationConfiguration (DeleteReplicationConfiguration'),
    newDeleteReplicationConfiguration,
    DeleteReplicationConfigurationResponse (DeleteReplicationConfigurationResponse'),
    newDeleteReplicationConfigurationResponse,

    -- ** DescribeAccessPoints
    DescribeAccessPoints (DescribeAccessPoints'),
    newDescribeAccessPoints,
    DescribeAccessPointsResponse (DescribeAccessPointsResponse'),
    newDescribeAccessPointsResponse,

    -- ** DescribeAccountPreferences
    DescribeAccountPreferences (DescribeAccountPreferences'),
    newDescribeAccountPreferences,
    DescribeAccountPreferencesResponse (DescribeAccountPreferencesResponse'),
    newDescribeAccountPreferencesResponse,

    -- ** DescribeBackupPolicy
    DescribeBackupPolicy (DescribeBackupPolicy'),
    newDescribeBackupPolicy,
    BackupPolicyDescription (BackupPolicyDescription'),
    newBackupPolicyDescription,

    -- ** DescribeFileSystemPolicy
    DescribeFileSystemPolicy (DescribeFileSystemPolicy'),
    newDescribeFileSystemPolicy,
    FileSystemPolicyDescription (FileSystemPolicyDescription'),
    newFileSystemPolicyDescription,

    -- ** DescribeFileSystems (Paginated)
    DescribeFileSystems (DescribeFileSystems'),
    newDescribeFileSystems,
    DescribeFileSystemsResponse (DescribeFileSystemsResponse'),
    newDescribeFileSystemsResponse,

    -- ** DescribeLifecycleConfiguration
    DescribeLifecycleConfiguration (DescribeLifecycleConfiguration'),
    newDescribeLifecycleConfiguration,
    LifecycleConfigurationDescription (LifecycleConfigurationDescription'),
    newLifecycleConfigurationDescription,

    -- ** DescribeMountTargetSecurityGroups
    DescribeMountTargetSecurityGroups (DescribeMountTargetSecurityGroups'),
    newDescribeMountTargetSecurityGroups,
    DescribeMountTargetSecurityGroupsResponse (DescribeMountTargetSecurityGroupsResponse'),
    newDescribeMountTargetSecurityGroupsResponse,

    -- ** DescribeMountTargets (Paginated)
    DescribeMountTargets (DescribeMountTargets'),
    newDescribeMountTargets,
    DescribeMountTargetsResponse (DescribeMountTargetsResponse'),
    newDescribeMountTargetsResponse,

    -- ** DescribeReplicationConfigurations
    DescribeReplicationConfigurations (DescribeReplicationConfigurations'),
    newDescribeReplicationConfigurations,
    DescribeReplicationConfigurationsResponse (DescribeReplicationConfigurationsResponse'),
    newDescribeReplicationConfigurationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ModifyMountTargetSecurityGroups
    ModifyMountTargetSecurityGroups (ModifyMountTargetSecurityGroups'),
    newModifyMountTargetSecurityGroups,
    ModifyMountTargetSecurityGroupsResponse (ModifyMountTargetSecurityGroupsResponse'),
    newModifyMountTargetSecurityGroupsResponse,

    -- ** PutAccountPreferences
    PutAccountPreferences (PutAccountPreferences'),
    newPutAccountPreferences,
    PutAccountPreferencesResponse (PutAccountPreferencesResponse'),
    newPutAccountPreferencesResponse,

    -- ** PutBackupPolicy
    PutBackupPolicy (PutBackupPolicy'),
    newPutBackupPolicy,
    BackupPolicyDescription (BackupPolicyDescription'),
    newBackupPolicyDescription,

    -- ** PutFileSystemPolicy
    PutFileSystemPolicy (PutFileSystemPolicy'),
    newPutFileSystemPolicy,
    FileSystemPolicyDescription (FileSystemPolicyDescription'),
    newFileSystemPolicyDescription,

    -- ** PutLifecycleConfiguration
    PutLifecycleConfiguration (PutLifecycleConfiguration'),
    newPutLifecycleConfiguration,
    LifecycleConfigurationDescription (LifecycleConfigurationDescription'),
    newLifecycleConfigurationDescription,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateFileSystem
    UpdateFileSystem (UpdateFileSystem'),
    newUpdateFileSystem,
    FileSystemDescription (FileSystemDescription'),
    newFileSystemDescription,

    -- * Types

    -- ** BackupStatus
    BackupStatus (..),

    -- ** LifeCycleState
    LifeCycleState (..),

    -- ** PerformanceMode
    PerformanceMode (..),

    -- ** ReplicationStatus
    ReplicationStatus (..),

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

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** DestinationToCreate
    DestinationToCreate (DestinationToCreate'),
    newDestinationToCreate,

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

    -- ** ReplicationConfigurationDescription
    ReplicationConfigurationDescription (ReplicationConfigurationDescription'),
    newReplicationConfigurationDescription,

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

import Amazonka.EFS.CreateAccessPoint
import Amazonka.EFS.CreateFileSystem
import Amazonka.EFS.CreateMountTarget
import Amazonka.EFS.CreateReplicationConfiguration
import Amazonka.EFS.DeleteAccessPoint
import Amazonka.EFS.DeleteFileSystem
import Amazonka.EFS.DeleteFileSystemPolicy
import Amazonka.EFS.DeleteMountTarget
import Amazonka.EFS.DeleteReplicationConfiguration
import Amazonka.EFS.DescribeAccessPoints
import Amazonka.EFS.DescribeAccountPreferences
import Amazonka.EFS.DescribeBackupPolicy
import Amazonka.EFS.DescribeFileSystemPolicy
import Amazonka.EFS.DescribeFileSystems
import Amazonka.EFS.DescribeLifecycleConfiguration
import Amazonka.EFS.DescribeMountTargetSecurityGroups
import Amazonka.EFS.DescribeMountTargets
import Amazonka.EFS.DescribeReplicationConfigurations
import Amazonka.EFS.Lens
import Amazonka.EFS.ListTagsForResource
import Amazonka.EFS.ModifyMountTargetSecurityGroups
import Amazonka.EFS.PutAccountPreferences
import Amazonka.EFS.PutBackupPolicy
import Amazonka.EFS.PutFileSystemPolicy
import Amazonka.EFS.PutLifecycleConfiguration
import Amazonka.EFS.TagResource
import Amazonka.EFS.Types
import Amazonka.EFS.UntagResource
import Amazonka.EFS.UpdateFileSystem
import Amazonka.EFS.Waiters

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

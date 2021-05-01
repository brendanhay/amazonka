{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Elastic File System
--
-- Amazon Elastic File System (Amazon EFS) provides simple, scalable file
-- storage for use with Amazon EC2 instances in the AWS Cloud. With Amazon
-- EFS, storage capacity is elastic, growing and shrinking automatically as
-- you add and remove files, so your applications have the storage they
-- need, when they need it. For more information, see the
-- <https://docs.aws.amazon.com/efs/latest/ug/api-reference.html User Guide>.
module Network.AWS.EFS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** SecurityGroupLimitExceeded
    _SecurityGroupLimitExceeded,

    -- ** FileSystemInUse
    _FileSystemInUse,

    -- ** DependencyTimeout
    _DependencyTimeout,

    -- ** ThroughputLimitExceeded
    _ThroughputLimitExceeded,

    -- ** AccessPointAlreadyExists
    _AccessPointAlreadyExists,

    -- ** AccessPointLimitExceeded
    _AccessPointLimitExceeded,

    -- ** InvalidPolicyException
    _InvalidPolicyException,

    -- ** AccessPointNotFound
    _AccessPointNotFound,

    -- ** FileSystemNotFound
    _FileSystemNotFound,

    -- ** SubnetNotFound
    _SubnetNotFound,

    -- ** NetworkInterfaceLimitExceeded
    _NetworkInterfaceLimitExceeded,

    -- ** FileSystemAlreadyExists
    _FileSystemAlreadyExists,

    -- ** TooManyRequests
    _TooManyRequests,

    -- ** FileSystemLimitExceeded
    _FileSystemLimitExceeded,

    -- ** IncorrectMountTargetState
    _IncorrectMountTargetState,

    -- ** InternalServerError
    _InternalServerError,

    -- ** MountTargetNotFound
    _MountTargetNotFound,

    -- ** ValidationException
    _ValidationException,

    -- ** NoFreeAddressesInSubnet
    _NoFreeAddressesInSubnet,

    -- ** BadRequest
    _BadRequest,

    -- ** InsufficientThroughputCapacity
    _InsufficientThroughputCapacity,

    -- ** PolicyNotFound
    _PolicyNotFound,

    -- ** IncorrectFileSystemLifeCycleState
    _IncorrectFileSystemLifeCycleState,

    -- ** IpAddressInUse
    _IpAddressInUse,

    -- ** MountTargetConflict
    _MountTargetConflict,

    -- ** SecurityGroupNotFound
    _SecurityGroupNotFound,

    -- ** UnsupportedAvailabilityZone
    _UnsupportedAvailabilityZone,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutBackupPolicy
    PutBackupPolicy (PutBackupPolicy'),
    newPutBackupPolicy,
    BackupPolicyDescription (BackupPolicyDescription'),
    newBackupPolicyDescription,

    -- ** PutLifecycleConfiguration
    PutLifecycleConfiguration (PutLifecycleConfiguration'),
    newPutLifecycleConfiguration,
    LifecycleConfigurationDescription (LifecycleConfigurationDescription'),
    newLifecycleConfigurationDescription,

    -- ** DeleteAccessPoint
    DeleteAccessPoint (DeleteAccessPoint'),
    newDeleteAccessPoint,
    DeleteAccessPointResponse (DeleteAccessPointResponse'),
    newDeleteAccessPointResponse,

    -- ** ModifyMountTargetSecurityGroups
    ModifyMountTargetSecurityGroups (ModifyMountTargetSecurityGroups'),
    newModifyMountTargetSecurityGroups,
    ModifyMountTargetSecurityGroupsResponse (ModifyMountTargetSecurityGroupsResponse'),
    newModifyMountTargetSecurityGroupsResponse,

    -- ** DescribeFileSystemPolicy
    DescribeFileSystemPolicy (DescribeFileSystemPolicy'),
    newDescribeFileSystemPolicy,
    FileSystemPolicyDescription (FileSystemPolicyDescription'),
    newFileSystemPolicyDescription,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateMountTarget
    CreateMountTarget (CreateMountTarget'),
    newCreateMountTarget,
    MountTargetDescription (MountTargetDescription'),
    newMountTargetDescription,

    -- ** DeleteMountTarget
    DeleteMountTarget (DeleteMountTarget'),
    newDeleteMountTarget,
    DeleteMountTargetResponse (DeleteMountTargetResponse'),
    newDeleteMountTargetResponse,

    -- ** DescribeFileSystems (Paginated)
    DescribeFileSystems (DescribeFileSystems'),
    newDescribeFileSystems,
    DescribeFileSystemsResponse (DescribeFileSystemsResponse'),
    newDescribeFileSystemsResponse,

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

    -- ** CreateFileSystem
    CreateFileSystem (CreateFileSystem'),
    newCreateFileSystem,
    FileSystemDescription (FileSystemDescription'),
    newFileSystemDescription,

    -- ** CreateAccessPoint
    CreateAccessPoint (CreateAccessPoint'),
    newCreateAccessPoint,
    AccessPointDescription (AccessPointDescription'),
    newAccessPointDescription,

    -- ** DescribeAccessPoints
    DescribeAccessPoints (DescribeAccessPoints'),
    newDescribeAccessPoints,
    DescribeAccessPointsResponse (DescribeAccessPointsResponse'),
    newDescribeAccessPointsResponse,

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

    -- ** DescribeBackupPolicy
    DescribeBackupPolicy (DescribeBackupPolicy'),
    newDescribeBackupPolicy,
    BackupPolicyDescription (BackupPolicyDescription'),
    newBackupPolicyDescription,

    -- ** UpdateFileSystem
    UpdateFileSystem (UpdateFileSystem'),
    newUpdateFileSystem,
    FileSystemDescription (FileSystemDescription'),
    newFileSystemDescription,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteFileSystem
    DeleteFileSystem (DeleteFileSystem'),
    newDeleteFileSystem,
    DeleteFileSystemResponse (DeleteFileSystemResponse'),
    newDeleteFileSystemResponse,

    -- ** PutFileSystemPolicy
    PutFileSystemPolicy (PutFileSystemPolicy'),
    newPutFileSystemPolicy,
    FileSystemPolicyDescription (FileSystemPolicyDescription'),
    newFileSystemPolicyDescription,

    -- * Types

    -- ** BackupStatus
    BackupStatus (..),

    -- ** LifeCycleState
    LifeCycleState (..),

    -- ** PerformanceMode
    PerformanceMode (..),

    -- ** ThroughputMode
    ThroughputMode (..),

    -- ** TransitionToIARules
    TransitionToIARules (..),

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
import Network.AWS.EFS.DescribeBackupPolicy
import Network.AWS.EFS.DescribeFileSystemPolicy
import Network.AWS.EFS.DescribeFileSystems
import Network.AWS.EFS.DescribeLifecycleConfiguration
import Network.AWS.EFS.DescribeMountTargetSecurityGroups
import Network.AWS.EFS.DescribeMountTargets
import Network.AWS.EFS.Lens
import Network.AWS.EFS.ListTagsForResource
import Network.AWS.EFS.ModifyMountTargetSecurityGroups
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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Elastic File System__
--
-- Amazon Elastic File System (Amazon EFS) provides simple, scalable file storage for use with Amazon EC2 instances in the AWS Cloud. With Amazon EFS, storage capacity is elastic, growing and shrinking automatically as you add and remove files, so your applications have the storage they need, when they need it. For more information, see the <https://docs.aws.amazon.com/efs/latest/ug/api-reference.html User Guide> .
module Network.AWS.EFS
  ( -- * Service configuration
    mkServiceConfig,

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
    module Network.AWS.EFS.CreateAccessPoint,

    -- ** DescribeMountTargets (Paginated)
    module Network.AWS.EFS.DescribeMountTargets,

    -- ** DeleteFileSystemPolicy
    module Network.AWS.EFS.DeleteFileSystemPolicy,

    -- ** ListTagsForResource
    module Network.AWS.EFS.ListTagsForResource,

    -- ** PutFileSystemPolicy
    module Network.AWS.EFS.PutFileSystemPolicy,

    -- ** DescribeFileSystems (Paginated)
    module Network.AWS.EFS.DescribeFileSystems,

    -- ** DeleteMountTarget
    module Network.AWS.EFS.DeleteMountTarget,

    -- ** DescribeMountTargetSecurityGroups
    module Network.AWS.EFS.DescribeMountTargetSecurityGroups,

    -- ** DescribeAccessPoints
    module Network.AWS.EFS.DescribeAccessPoints,

    -- ** ModifyMountTargetSecurityGroups
    module Network.AWS.EFS.ModifyMountTargetSecurityGroups,

    -- ** CreateFileSystem
    module Network.AWS.EFS.CreateFileSystem,

    -- ** PutLifecycleConfiguration
    module Network.AWS.EFS.PutLifecycleConfiguration,

    -- ** PutBackupPolicy
    module Network.AWS.EFS.PutBackupPolicy,

    -- ** DeleteFileSystem
    module Network.AWS.EFS.DeleteFileSystem,

    -- ** UpdateFileSystem
    module Network.AWS.EFS.UpdateFileSystem,

    -- ** CreateMountTarget
    module Network.AWS.EFS.CreateMountTarget,

    -- ** TagResource
    module Network.AWS.EFS.TagResource,

    -- ** DescribeBackupPolicy
    module Network.AWS.EFS.DescribeBackupPolicy,

    -- ** DescribeLifecycleConfiguration
    module Network.AWS.EFS.DescribeLifecycleConfiguration,

    -- ** UntagResource
    module Network.AWS.EFS.UntagResource,

    -- ** DescribeFileSystemPolicy
    module Network.AWS.EFS.DescribeFileSystemPolicy,

    -- ** DeleteAccessPoint
    module Network.AWS.EFS.DeleteAccessPoint,

    -- * Types

    -- ** PosixUser
    PosixUser (..),
    mkPosixUser,
    puUid,
    puGid,
    puSecondaryGids,

    -- ** Status
    Status (..),

    -- ** RootDirectory
    RootDirectory (..),
    mkRootDirectory,
    rdCreationInfo,
    rdPath,

    -- ** CreationInfo
    CreationInfo (..),
    mkCreationInfo,
    ciOwnerUid,
    ciOwnerGid,
    ciPermissions,

    -- ** IpAddress
    IpAddress (..),

    -- ** ResourceId
    ResourceId (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** MountTargetDescription
    MountTargetDescription (..),
    mkMountTargetDescription,
    mtdMountTargetId,
    mtdFileSystemId,
    mtdSubnetId,
    mtdLifeCycleState,
    mtdAvailabilityZoneId,
    mtdAvailabilityZoneName,
    mtdIpAddress,
    mtdNetworkInterfaceId,
    mtdOwnerId,
    mtdVpcId,

    -- ** ClientToken
    ClientToken (..),

    -- ** AvailabilityZoneId
    AvailabilityZoneId (..),

    -- ** FileSystemPolicyDescription
    FileSystemPolicyDescription (..),
    mkFileSystemPolicyDescription,
    fspdFileSystemId,
    fspdPolicy,

    -- ** Path
    Path (..),

    -- ** AccessPointId
    AccessPointId (..),

    -- ** VpcId
    VpcId (..),

    -- ** LifecycleConfigurationDescription
    LifecycleConfigurationDescription (..),
    mkLifecycleConfigurationDescription,
    lcdLifecyclePolicies,

    -- ** AvailabilityZoneName
    AvailabilityZoneName (..),

    -- ** BackupPolicyDescription
    BackupPolicyDescription (..),
    mkBackupPolicyDescription,
    bpdBackupPolicy,

    -- ** Token
    Token (..),

    -- ** PerformanceMode
    PerformanceMode (..),

    -- ** NetworkInterfaceId
    NetworkInterfaceId (..),

    -- ** FileSystemId
    FileSystemId (..),

    -- ** FileSystemArn
    FileSystemArn (..),

    -- ** AccessPointDescription
    AccessPointDescription (..),
    mkAccessPointDescription,
    apdAccessPointArn,
    apdAccessPointId,
    apdClientToken,
    apdFileSystemId,
    apdLifeCycleState,
    apdName,
    apdOwnerId,
    apdPosixUser,
    apdRootDirectory,
    apdTags,

    -- ** SubnetId
    SubnetId (..),

    -- ** TagValue
    TagValue (..),

    -- ** AwsAccountId
    AwsAccountId (..),

    -- ** ThroughputMode
    ThroughputMode (..),

    -- ** TransitionToIARules
    TransitionToIARules (..),

    -- ** SecurityGroup
    SecurityGroup (..),

    -- ** KmsKeyId
    KmsKeyId (..),

    -- ** Name
    Name (..),

    -- ** CreationToken
    CreationToken (..),

    -- ** Marker
    Marker (..),

    -- ** FileSystemDescription
    FileSystemDescription (..),
    mkFileSystemDescription,
    fsdOwnerId,
    fsdCreationToken,
    fsdFileSystemId,
    fsdCreationTime,
    fsdLifeCycleState,
    fsdNumberOfMountTargets,
    fsdSizeInBytes,
    fsdPerformanceMode,
    fsdTags,
    fsdEncrypted,
    fsdFileSystemArn,
    fsdKmsKeyId,
    fsdName,
    fsdProvisionedThroughputInMibps,
    fsdThroughputMode,

    -- ** AccessPointArn
    AccessPointArn (..),

    -- ** TagKey
    TagKey (..),

    -- ** Policy
    Policy (..),

    -- ** Permissions
    Permissions (..),

    -- ** FileSystemSize
    FileSystemSize (..),
    mkFileSystemSize,
    fssValue,
    fssTimestamp,
    fssValueInIA,
    fssValueInStandard,

    -- ** BackupPolicy
    BackupPolicy (..),
    mkBackupPolicy,
    bpStatus,

    -- ** LifecyclePolicy
    LifecyclePolicy (..),
    mkLifecyclePolicy,
    lpTransitionToIA,

    -- ** LifeCycleState
    LifeCycleState (..),

    -- ** MountTargetId
    MountTargetId (..),

    -- ** NextMarker
    NextMarker (..),

    -- ** NextToken
    NextToken (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** OwnerId
    OwnerId (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import qualified Network.AWS.Prelude as Lude

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

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ValidationException,
    _MountTargetNotFound,
    _SecurityGroupLimitExceeded,
    _SecurityGroupNotFound,
    _MountTargetConflict,
    _UnsupportedAvailabilityZone,
    _FileSystemLimitExceeded,
    _TooManyRequests,
    _NetworkInterfaceLimitExceeded,
    _FileSystemAlreadyExists,
    _SubnetNotFound,
    _FileSystemNotFound,
    _IncorrectFileSystemLifeCycleState,
    _BadRequest,
    _NoFreeAddressesInSubnet,
    _ThroughputLimitExceeded,
    _DependencyTimeout,
    _FileSystemInUse,
    _IncorrectMountTargetState,
    _InternalServerError,
    _IpAddressInUse,
    _PolicyNotFound,
    _AccessPointNotFound,
    _InsufficientThroughputCapacity,
    _InvalidPolicyException,
    _AccessPointAlreadyExists,
    _AccessPointLimitExceeded,

    -- * PosixUser
    PosixUser (..),
    mkPosixUser,
    puUid,
    puGid,
    puSecondaryGids,

    -- * Status
    Status (..),

    -- * RootDirectory
    RootDirectory (..),
    mkRootDirectory,
    rdCreationInfo,
    rdPath,

    -- * CreationInfo
    CreationInfo (..),
    mkCreationInfo,
    ciOwnerUid,
    ciOwnerGid,
    ciPermissions,

    -- * IpAddress
    IpAddress (..),

    -- * ResourceId
    ResourceId (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * MountTargetDescription
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

    -- * ClientToken
    ClientToken (..),

    -- * AvailabilityZoneId
    AvailabilityZoneId (..),

    -- * FileSystemPolicyDescription
    FileSystemPolicyDescription (..),
    mkFileSystemPolicyDescription,
    fspdFileSystemId,
    fspdPolicy,

    -- * Path
    Path (..),

    -- * AccessPointId
    AccessPointId (..),

    -- * VpcId
    VpcId (..),

    -- * LifecycleConfigurationDescription
    LifecycleConfigurationDescription (..),
    mkLifecycleConfigurationDescription,
    lcdLifecyclePolicies,

    -- * AvailabilityZoneName
    AvailabilityZoneName (..),

    -- * BackupPolicyDescription
    BackupPolicyDescription (..),
    mkBackupPolicyDescription,
    bpdBackupPolicy,

    -- * Token
    Token (..),

    -- * PerformanceMode
    PerformanceMode (..),

    -- * NetworkInterfaceId
    NetworkInterfaceId (..),

    -- * FileSystemId
    FileSystemId (..),

    -- * FileSystemArn
    FileSystemArn (..),

    -- * AccessPointDescription
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

    -- * SubnetId
    SubnetId (..),

    -- * TagValue
    TagValue (..),

    -- * AwsAccountId
    AwsAccountId (..),

    -- * ThroughputMode
    ThroughputMode (..),

    -- * TransitionToIARules
    TransitionToIARules (..),

    -- * SecurityGroup
    SecurityGroup (..),

    -- * KmsKeyId
    KmsKeyId (..),

    -- * Name
    Name (..),

    -- * CreationToken
    CreationToken (..),

    -- * Marker
    Marker (..),

    -- * FileSystemDescription
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

    -- * AccessPointArn
    AccessPointArn (..),

    -- * TagKey
    TagKey (..),

    -- * Policy
    Policy (..),

    -- * Permissions
    Permissions (..),

    -- * FileSystemSize
    FileSystemSize (..),
    mkFileSystemSize,
    fssValue,
    fssTimestamp,
    fssValueInIA,
    fssValueInStandard,

    -- * BackupPolicy
    BackupPolicy (..),
    mkBackupPolicy,
    bpStatus,

    -- * LifecyclePolicy
    LifecyclePolicy (..),
    mkLifecyclePolicy,
    lpTransitionToIA,

    -- * LifeCycleState
    LifeCycleState (..),

    -- * MountTargetId
    MountTargetId (..),

    -- * NextMarker
    NextMarker (..),

    -- * NextToken
    NextToken (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * OwnerId
    OwnerId (..),
  )
where

import Network.AWS.EFS.Types.AccessPointArn
import Network.AWS.EFS.Types.AccessPointDescription
import Network.AWS.EFS.Types.AccessPointId
import Network.AWS.EFS.Types.AvailabilityZoneId
import Network.AWS.EFS.Types.AvailabilityZoneName
import Network.AWS.EFS.Types.AwsAccountId
import Network.AWS.EFS.Types.BackupPolicy
import Network.AWS.EFS.Types.BackupPolicyDescription
import Network.AWS.EFS.Types.ClientToken
import Network.AWS.EFS.Types.CreationInfo
import Network.AWS.EFS.Types.CreationToken
import Network.AWS.EFS.Types.FileSystemArn
import Network.AWS.EFS.Types.FileSystemDescription
import Network.AWS.EFS.Types.FileSystemId
import Network.AWS.EFS.Types.FileSystemPolicyDescription
import Network.AWS.EFS.Types.FileSystemSize
import Network.AWS.EFS.Types.IpAddress
import Network.AWS.EFS.Types.Key
import Network.AWS.EFS.Types.KmsKeyId
import Network.AWS.EFS.Types.LifeCycleState
import Network.AWS.EFS.Types.LifecycleConfigurationDescription
import Network.AWS.EFS.Types.LifecyclePolicy
import Network.AWS.EFS.Types.Marker
import Network.AWS.EFS.Types.MountTargetDescription
import Network.AWS.EFS.Types.MountTargetId
import Network.AWS.EFS.Types.Name
import Network.AWS.EFS.Types.NetworkInterfaceId
import Network.AWS.EFS.Types.NextMarker
import Network.AWS.EFS.Types.NextToken
import Network.AWS.EFS.Types.OwnerId
import Network.AWS.EFS.Types.Path
import Network.AWS.EFS.Types.PerformanceMode
import Network.AWS.EFS.Types.Permissions
import Network.AWS.EFS.Types.Policy
import Network.AWS.EFS.Types.PosixUser
import Network.AWS.EFS.Types.ResourceId
import Network.AWS.EFS.Types.RootDirectory
import Network.AWS.EFS.Types.SecurityGroup
import Network.AWS.EFS.Types.Status
import Network.AWS.EFS.Types.SubnetId
import Network.AWS.EFS.Types.Tag
import Network.AWS.EFS.Types.TagKey
import Network.AWS.EFS.Types.TagValue
import Network.AWS.EFS.Types.ThroughputMode
import Network.AWS.EFS.Types.Token
import Network.AWS.EFS.Types.TransitionToIARules
import Network.AWS.EFS.Types.Value
import Network.AWS.EFS.Types.VpcId
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-02-01@ of the Amazon Elastic File System SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "EFS",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "elasticfilesystem",
      Core._svcVersion = "2015-02-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "EFS",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | Returned if the AWS Backup service is not available in the region that the request was made.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError mkServiceConfig "ValidationException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead." #-}

-- | Returned if there is no mount target with the specified ID found in the caller's account.
_MountTargetNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MountTargetNotFound =
  Core._MatchServiceError mkServiceConfig "MountTargetNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _MountTargetNotFound "Use generic-lens or generic-optics instead." #-}

-- | Returned if the size of @SecurityGroups@ specified in the request is greater than five.
_SecurityGroupLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SecurityGroupLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "SecurityGroupLimitExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SecurityGroupLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | Returned if one of the specified security groups doesn't exist in the subnet's VPC.
_SecurityGroupNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SecurityGroupNotFound =
  Core._MatchServiceError mkServiceConfig "SecurityGroupNotFound"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SecurityGroupNotFound "Use generic-lens or generic-optics instead." #-}

-- | Returned if the mount target would violate one of the specified restrictions based on the file system's existing mount targets.
_MountTargetConflict :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MountTargetConflict =
  Core._MatchServiceError mkServiceConfig "MountTargetConflict"
    Core.. Core.hasStatues 409
{-# DEPRECATED _MountTargetConflict "Use generic-lens or generic-optics instead." #-}

-- |
_UnsupportedAvailabilityZone :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedAvailabilityZone =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedAvailabilityZone"
    Core.. Core.hasStatues 400
{-# DEPRECATED _UnsupportedAvailabilityZone "Use generic-lens or generic-optics instead." #-}

-- | Returned if the AWS account has already created the maximum number of file systems allowed per account.
_FileSystemLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileSystemLimitExceeded =
  Core._MatchServiceError mkServiceConfig "FileSystemLimitExceeded"
    Core.. Core.hasStatues 403
{-# DEPRECATED _FileSystemLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | Returned if you don’t wait at least 24 hours before changing the throughput mode, or decreasing the Provisioned Throughput value.
_TooManyRequests :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequests =
  Core._MatchServiceError mkServiceConfig "TooManyRequests"
    Core.. Core.hasStatues 429
{-# DEPRECATED _TooManyRequests "Use generic-lens or generic-optics instead." #-}

-- | The calling account has reached the limit for elastic network interfaces for the specific AWS Region. The client should try to delete some elastic network interfaces or get the account limit raised. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Appendix_Limits.html Amazon VPC Limits> in the /Amazon VPC User Guide / (see the Network interfaces per VPC entry in the table).
_NetworkInterfaceLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NetworkInterfaceLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "NetworkInterfaceLimitExceeded"
    Core.. Core.hasStatues 409
{-# DEPRECATED _NetworkInterfaceLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | Returned if the file system you are trying to create already exists, with the creation token you provided.
_FileSystemAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileSystemAlreadyExists =
  Core._MatchServiceError mkServiceConfig "FileSystemAlreadyExists"
    Core.. Core.hasStatues 409
{-# DEPRECATED _FileSystemAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | Returned if there is no subnet with ID @SubnetId@ provided in the request.
_SubnetNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetNotFound =
  Core._MatchServiceError mkServiceConfig "SubnetNotFound"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SubnetNotFound "Use generic-lens or generic-optics instead." #-}

-- | Returned if the specified @FileSystemId@ value doesn't exist in the requester's AWS account.
_FileSystemNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileSystemNotFound =
  Core._MatchServiceError mkServiceConfig "FileSystemNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _FileSystemNotFound "Use generic-lens or generic-optics instead." #-}

-- | Returned if the file system's lifecycle state is not "available".
_IncorrectFileSystemLifeCycleState :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncorrectFileSystemLifeCycleState =
  Core._MatchServiceError
    mkServiceConfig
    "IncorrectFileSystemLifeCycleState"
    Core.. Core.hasStatues 409
{-# DEPRECATED _IncorrectFileSystemLifeCycleState "Use generic-lens or generic-optics instead." #-}

-- | Returned if the request is malformed or contains an error such as an invalid parameter value or a missing required parameter.
_BadRequest :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequest =
  Core._MatchServiceError mkServiceConfig "BadRequest"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BadRequest "Use generic-lens or generic-optics instead." #-}

-- | Returned if @IpAddress@ was not specified in the request and there are no free IP addresses in the subnet.
_NoFreeAddressesInSubnet :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoFreeAddressesInSubnet =
  Core._MatchServiceError mkServiceConfig "NoFreeAddressesInSubnet"
    Core.. Core.hasStatues 409
{-# DEPRECATED _NoFreeAddressesInSubnet "Use generic-lens or generic-optics instead." #-}

-- | Returned if the throughput mode or amount of provisioned throughput can't be changed because the throughput limit of 1024 MiB/s has been reached.
_ThroughputLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThroughputLimitExceeded =
  Core._MatchServiceError mkServiceConfig "ThroughputLimitExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ThroughputLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | The service timed out trying to fulfill the request, and the client should try the call again.
_DependencyTimeout :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DependencyTimeout =
  Core._MatchServiceError mkServiceConfig "DependencyTimeout"
    Core.. Core.hasStatues 504
{-# DEPRECATED _DependencyTimeout "Use generic-lens or generic-optics instead." #-}

-- | Returned if a file system has mount targets.
_FileSystemInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileSystemInUse =
  Core._MatchServiceError mkServiceConfig "FileSystemInUse"
    Core.. Core.hasStatues 409
{-# DEPRECATED _FileSystemInUse "Use generic-lens or generic-optics instead." #-}

-- | Returned if the mount target is not in the correct state for the operation.
_IncorrectMountTargetState :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncorrectMountTargetState =
  Core._MatchServiceError
    mkServiceConfig
    "IncorrectMountTargetState"
    Core.. Core.hasStatues 409
{-# DEPRECATED _IncorrectMountTargetState "Use generic-lens or generic-optics instead." #-}

-- | Returned if an error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError mkServiceConfig "InternalServerError"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead." #-}

-- | Returned if the request specified an @IpAddress@ that is already in use in the subnet.
_IpAddressInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IpAddressInUse =
  Core._MatchServiceError mkServiceConfig "IpAddressInUse"
    Core.. Core.hasStatues 409
{-# DEPRECATED _IpAddressInUse "Use generic-lens or generic-optics instead." #-}

-- | Returned if the default file system policy is in effect for the EFS file system specified.
_PolicyNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyNotFound =
  Core._MatchServiceError mkServiceConfig "PolicyNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _PolicyNotFound "Use generic-lens or generic-optics instead." #-}

-- | Returned if the specified @AccessPointId@ value doesn't exist in the requester's AWS account.
_AccessPointNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessPointNotFound =
  Core._MatchServiceError mkServiceConfig "AccessPointNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _AccessPointNotFound "Use generic-lens or generic-optics instead." #-}

-- | Returned if there's not enough capacity to provision additional throughput. This value might be returned when you try to create a file system in provisioned throughput mode, when you attempt to increase the provisioned throughput of an existing file system, or when you attempt to change an existing file system from bursting to provisioned throughput mode.
_InsufficientThroughputCapacity :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientThroughputCapacity =
  Core._MatchServiceError
    mkServiceConfig
    "InsufficientThroughputCapacity"
    Core.. Core.hasStatues 503
{-# DEPRECATED _InsufficientThroughputCapacity "Use generic-lens or generic-optics instead." #-}

-- | Returned if the @FileSystemPolicy@ is is malformed or contains an error such as an invalid parameter value or a missing required parameter. Returned in the case of a policy lockout safety check error.
_InvalidPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyException =
  Core._MatchServiceError mkServiceConfig "InvalidPolicyException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidPolicyException "Use generic-lens or generic-optics instead." #-}

-- | Returned if the access point you are trying to create already exists, with the creation token you provided in the request.
_AccessPointAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessPointAlreadyExists =
  Core._MatchServiceError
    mkServiceConfig
    "AccessPointAlreadyExists"
    Core.. Core.hasStatues 409
{-# DEPRECATED _AccessPointAlreadyExists "Use generic-lens or generic-optics instead." #-}

-- | Returned if the AWS account has already created the maximum number of access points allowed per file system.
_AccessPointLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessPointLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "AccessPointLimitExceeded"
    Core.. Core.hasStatues 403
{-# DEPRECATED _AccessPointLimitExceeded "Use generic-lens or generic-optics instead." #-}

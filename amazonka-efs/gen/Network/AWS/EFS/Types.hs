{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _SecurityGroupLimitExceeded,
    _FileSystemInUse,
    _ThroughputLimitExceeded,
    _DependencyTimeout,
    _AccessPointAlreadyExists,
    _InvalidPolicyException,
    _AccessPointLimitExceeded,
    _AccessPointNotFound,
    _FileSystemNotFound,
    _SubnetNotFound,
    _TooManyRequests,
    _NetworkInterfaceLimitExceeded,
    _FileSystemAlreadyExists,
    _FileSystemLimitExceeded,
    _InternalServerError,
    _IncorrectMountTargetState,
    _MountTargetNotFound,
    _ValidationException,
    _NoFreeAddressesInSubnet,
    _BadRequest,
    _IncorrectFileSystemLifeCycleState,
    _InsufficientThroughputCapacity,
    _PolicyNotFound,
    _IpAddressInUse,
    _AvailabilityZonesMismatch,
    _SecurityGroupNotFound,
    _MountTargetConflict,
    _UnsupportedAvailabilityZone,

    -- * BackupStatus
    BackupStatus (..),

    -- * LifeCycleState
    LifeCycleState (..),

    -- * PerformanceMode
    PerformanceMode (..),

    -- * Resource
    Resource (..),

    -- * ResourceIdType
    ResourceIdType (..),

    -- * ThroughputMode
    ThroughputMode (..),

    -- * TransitionToIARules
    TransitionToIARules (..),

    -- * TransitionToPrimaryStorageClassRules
    TransitionToPrimaryStorageClassRules (..),

    -- * AccessPointDescription
    AccessPointDescription (..),
    newAccessPointDescription,
    accessPointDescription_ownerId,
    accessPointDescription_accessPointId,
    accessPointDescription_accessPointArn,
    accessPointDescription_name,
    accessPointDescription_rootDirectory,
    accessPointDescription_posixUser,
    accessPointDescription_tags,
    accessPointDescription_lifeCycleState,
    accessPointDescription_fileSystemId,
    accessPointDescription_clientToken,

    -- * BackupPolicy
    BackupPolicy (..),
    newBackupPolicy,
    backupPolicy_status,

    -- * BackupPolicyDescription
    BackupPolicyDescription (..),
    newBackupPolicyDescription,
    backupPolicyDescription_backupPolicy,

    -- * CreationInfo
    CreationInfo (..),
    newCreationInfo,
    creationInfo_ownerUid,
    creationInfo_ownerGid,
    creationInfo_permissions,

    -- * FileSystemDescription
    FileSystemDescription (..),
    newFileSystemDescription,
    fileSystemDescription_throughputMode,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_availabilityZoneName,
    fileSystemDescription_availabilityZoneId,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
    fileSystemDescription_ownerId,
    fileSystemDescription_creationToken,
    fileSystemDescription_fileSystemId,
    fileSystemDescription_creationTime,
    fileSystemDescription_lifeCycleState,
    fileSystemDescription_numberOfMountTargets,
    fileSystemDescription_sizeInBytes,
    fileSystemDescription_performanceMode,
    fileSystemDescription_tags,

    -- * FileSystemPolicyDescription
    FileSystemPolicyDescription (..),
    newFileSystemPolicyDescription,
    fileSystemPolicyDescription_fileSystemId,
    fileSystemPolicyDescription_policy,

    -- * FileSystemSize
    FileSystemSize (..),
    newFileSystemSize,
    fileSystemSize_valueInStandard,
    fileSystemSize_valueInIA,
    fileSystemSize_timestamp,
    fileSystemSize_value,

    -- * LifecycleConfigurationDescription
    LifecycleConfigurationDescription (..),
    newLifecycleConfigurationDescription,
    lifecycleConfigurationDescription_lifecyclePolicies,

    -- * LifecyclePolicy
    LifecyclePolicy (..),
    newLifecyclePolicy,
    lifecyclePolicy_transitionToIA,
    lifecyclePolicy_transitionToPrimaryStorageClass,

    -- * MountTargetDescription
    MountTargetDescription (..),
    newMountTargetDescription,
    mountTargetDescription_ownerId,
    mountTargetDescription_availabilityZoneName,
    mountTargetDescription_availabilityZoneId,
    mountTargetDescription_ipAddress,
    mountTargetDescription_networkInterfaceId,
    mountTargetDescription_vpcId,
    mountTargetDescription_mountTargetId,
    mountTargetDescription_fileSystemId,
    mountTargetDescription_subnetId,
    mountTargetDescription_lifeCycleState,

    -- * PosixUser
    PosixUser (..),
    newPosixUser,
    posixUser_secondaryGids,
    posixUser_uid,
    posixUser_gid,

    -- * ResourceIdPreference
    ResourceIdPreference (..),
    newResourceIdPreference,
    resourceIdPreference_resourceIdType,
    resourceIdPreference_resources,

    -- * RootDirectory
    RootDirectory (..),
    newRootDirectory,
    rootDirectory_creationInfo,
    rootDirectory_path,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types.AccessPointDescription
import Network.AWS.EFS.Types.BackupPolicy
import Network.AWS.EFS.Types.BackupPolicyDescription
import Network.AWS.EFS.Types.BackupStatus
import Network.AWS.EFS.Types.CreationInfo
import Network.AWS.EFS.Types.FileSystemDescription
import Network.AWS.EFS.Types.FileSystemPolicyDescription
import Network.AWS.EFS.Types.FileSystemSize
import Network.AWS.EFS.Types.LifeCycleState
import Network.AWS.EFS.Types.LifecycleConfigurationDescription
import Network.AWS.EFS.Types.LifecyclePolicy
import Network.AWS.EFS.Types.MountTargetDescription
import Network.AWS.EFS.Types.PerformanceMode
import Network.AWS.EFS.Types.PosixUser
import Network.AWS.EFS.Types.Resource
import Network.AWS.EFS.Types.ResourceIdPreference
import Network.AWS.EFS.Types.ResourceIdType
import Network.AWS.EFS.Types.RootDirectory
import Network.AWS.EFS.Types.Tag
import Network.AWS.EFS.Types.ThroughputMode
import Network.AWS.EFS.Types.TransitionToIARules
import Network.AWS.EFS.Types.TransitionToPrimaryStorageClassRules
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-02-01@ of the Amazon Elastic File System SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "EFS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "elasticfilesystem",
      Core._serviceSigningName = "elasticfilesystem",
      Core._serviceVersion = "2015-02-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "EFS",
      Core._serviceRetry = retry
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Returned if the size of @SecurityGroups@ specified in the request is
-- greater than five.
_SecurityGroupLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SecurityGroupLimitExceeded =
  Core._MatchServiceError
    defaultService
    "SecurityGroupLimitExceeded"
    Prelude.. Core.hasStatus 400

-- | Returned if a file system has mount targets.
_FileSystemInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileSystemInUse =
  Core._MatchServiceError
    defaultService
    "FileSystemInUse"
    Prelude.. Core.hasStatus 409

-- | Returned if the throughput mode or amount of provisioned throughput
-- can\'t be changed because the throughput limit of 1024 MiB\/s has been
-- reached.
_ThroughputLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThroughputLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ThroughputLimitExceeded"
    Prelude.. Core.hasStatus 400

-- | The service timed out trying to fulfill the request, and the client
-- should try the call again.
_DependencyTimeout :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DependencyTimeout =
  Core._MatchServiceError
    defaultService
    "DependencyTimeout"
    Prelude.. Core.hasStatus 504

-- | Returned if the access point you are trying to create already exists,
-- with the creation token you provided in the request.
_AccessPointAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessPointAlreadyExists =
  Core._MatchServiceError
    defaultService
    "AccessPointAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | Returned if the @FileSystemPolicy@ is is malformed or contains an error
-- such as an invalid parameter value or a missing required parameter.
-- Returned in the case of a policy lockout safety check error.
_InvalidPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyException"
    Prelude.. Core.hasStatus 400

-- | Returned if the Amazon Web Services account has already created the
-- maximum number of access points allowed per file system.
_AccessPointLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessPointLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AccessPointLimitExceeded"
    Prelude.. Core.hasStatus 403

-- | Returned if the specified @AccessPointId@ value doesn\'t exist in the
-- requester\'s Amazon Web Services account.
_AccessPointNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessPointNotFound =
  Core._MatchServiceError
    defaultService
    "AccessPointNotFound"
    Prelude.. Core.hasStatus 404

-- | Returned if the specified @FileSystemId@ value doesn\'t exist in the
-- requester\'s Amazon Web Services account.
_FileSystemNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileSystemNotFound =
  Core._MatchServiceError
    defaultService
    "FileSystemNotFound"
    Prelude.. Core.hasStatus 404

-- | Returned if there is no subnet with ID @SubnetId@ provided in the
-- request.
_SubnetNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetNotFound =
  Core._MatchServiceError
    defaultService
    "SubnetNotFound"
    Prelude.. Core.hasStatus 400

-- | Returned if you don’t wait at least 24 hours before changing the
-- throughput mode, or decreasing the Provisioned Throughput value.
_TooManyRequests :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequests =
  Core._MatchServiceError
    defaultService
    "TooManyRequests"
    Prelude.. Core.hasStatus 429

-- | The calling account has reached the limit for elastic network interfaces
-- for the specific Amazon Web Services Region. The client should try to
-- delete some elastic network interfaces or get the account limit raised.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Appendix_Limits.html Amazon VPC Limits>
-- in the /Amazon VPC User Guide/ (see the Network interfaces per VPC entry
-- in the table).
_NetworkInterfaceLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NetworkInterfaceLimitExceeded =
  Core._MatchServiceError
    defaultService
    "NetworkInterfaceLimitExceeded"
    Prelude.. Core.hasStatus 409

-- | Returned if the file system you are trying to create already exists,
-- with the creation token you provided.
_FileSystemAlreadyExists :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileSystemAlreadyExists =
  Core._MatchServiceError
    defaultService
    "FileSystemAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | Returned if the Amazon Web Services account has already created the
-- maximum number of file systems allowed per account.
_FileSystemLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileSystemLimitExceeded =
  Core._MatchServiceError
    defaultService
    "FileSystemLimitExceeded"
    Prelude.. Core.hasStatus 403

-- | Returned if an error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"
    Prelude.. Core.hasStatus 500

-- | Returned if the mount target is not in the correct state for the
-- operation.
_IncorrectMountTargetState :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncorrectMountTargetState =
  Core._MatchServiceError
    defaultService
    "IncorrectMountTargetState"
    Prelude.. Core.hasStatus 409

-- | Returned if there is no mount target with the specified ID found in the
-- caller\'s Amazon Web Services account.
_MountTargetNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MountTargetNotFound =
  Core._MatchServiceError
    defaultService
    "MountTargetNotFound"
    Prelude.. Core.hasStatus 404

-- | Returned if the Backup service is not available in the Amazon Web
-- Services Region in which the request was made.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Returned if @IpAddress@ was not specified in the request and there are
-- no free IP addresses in the subnet.
_NoFreeAddressesInSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoFreeAddressesInSubnet =
  Core._MatchServiceError
    defaultService
    "NoFreeAddressesInSubnet"
    Prelude.. Core.hasStatus 409

-- | Returned if the request is malformed or contains an error such as an
-- invalid parameter value or a missing required parameter.
_BadRequest :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequest =
  Core._MatchServiceError defaultService "BadRequest"
    Prelude.. Core.hasStatus 400

-- | Returned if the file system\'s lifecycle state is not \"available\".
_IncorrectFileSystemLifeCycleState :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncorrectFileSystemLifeCycleState =
  Core._MatchServiceError
    defaultService
    "IncorrectFileSystemLifeCycleState"
    Prelude.. Core.hasStatus 409

-- | Returned if there\'s not enough capacity to provision additional
-- throughput. This value might be returned when you try to create a file
-- system in provisioned throughput mode, when you attempt to increase the
-- provisioned throughput of an existing file system, or when you attempt
-- to change an existing file system from bursting to provisioned
-- throughput mode. Try again later.
_InsufficientThroughputCapacity :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientThroughputCapacity =
  Core._MatchServiceError
    defaultService
    "InsufficientThroughputCapacity"
    Prelude.. Core.hasStatus 503

-- | Returned if the default file system policy is in effect for the EFS file
-- system specified.
_PolicyNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PolicyNotFound =
  Core._MatchServiceError
    defaultService
    "PolicyNotFound"
    Prelude.. Core.hasStatus 404

-- | Returned if the request specified an @IpAddress@ that is already in use
-- in the subnet.
_IpAddressInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IpAddressInUse =
  Core._MatchServiceError
    defaultService
    "IpAddressInUse"
    Prelude.. Core.hasStatus 409

-- | Returned if the Availability Zone that was specified for a mount target
-- is different from the Availability Zone that was specified for One Zone
-- storage classes. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/availability-durability.html Regional and One Zone storage redundancy>.
_AvailabilityZonesMismatch :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AvailabilityZonesMismatch =
  Core._MatchServiceError
    defaultService
    "AvailabilityZonesMismatch"
    Prelude.. Core.hasStatus 400

-- | Returned if one of the specified security groups doesn\'t exist in the
-- subnet\'s VPC.
_SecurityGroupNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SecurityGroupNotFound =
  Core._MatchServiceError
    defaultService
    "SecurityGroupNotFound"
    Prelude.. Core.hasStatus 400

-- | Returned if the mount target would violate one of the specified
-- restrictions based on the file system\'s existing mount targets.
_MountTargetConflict :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MountTargetConflict =
  Core._MatchServiceError
    defaultService
    "MountTargetConflict"
    Prelude.. Core.hasStatus 409

-- | Returned if the requested Amazon EFS functionality is not available in
-- the specified Availability Zone.
_UnsupportedAvailabilityZone :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedAvailabilityZone =
  Core._MatchServiceError
    defaultService
    "UnsupportedAvailabilityZone"
    Prelude.. Core.hasStatus 400

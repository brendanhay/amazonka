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
    _DependencyTimeout,
    _ThroughputLimitExceeded,
    _AccessPointAlreadyExists,
    _AccessPointLimitExceeded,
    _InvalidPolicyException,
    _AccessPointNotFound,
    _FileSystemNotFound,
    _SubnetNotFound,
    _NetworkInterfaceLimitExceeded,
    _FileSystemAlreadyExists,
    _TooManyRequests,
    _FileSystemLimitExceeded,
    _IncorrectMountTargetState,
    _InternalServerError,
    _MountTargetNotFound,
    _ValidationException,
    _NoFreeAddressesInSubnet,
    _BadRequest,
    _InsufficientThroughputCapacity,
    _PolicyNotFound,
    _IncorrectFileSystemLifeCycleState,
    _IpAddressInUse,
    _MountTargetConflict,
    _SecurityGroupNotFound,
    _UnsupportedAvailabilityZone,

    -- * BackupStatus
    BackupStatus (..),

    -- * LifeCycleState
    LifeCycleState (..),

    -- * PerformanceMode
    PerformanceMode (..),

    -- * ThroughputMode
    ThroughputMode (..),

    -- * TransitionToIARules
    TransitionToIARules (..),

    -- * AccessPointDescription
    AccessPointDescription (..),
    newAccessPointDescription,
    accessPointDescription_ownerId,
    accessPointDescription_accessPointArn,
    accessPointDescription_accessPointId,
    accessPointDescription_rootDirectory,
    accessPointDescription_name,
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
import Network.AWS.EFS.Types.RootDirectory
import Network.AWS.EFS.Types.Tag
import Network.AWS.EFS.Types.ThroughputMode
import Network.AWS.EFS.Types.TransitionToIARules
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-02-01@ of the Amazon Elastic File System SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "EFS",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "elasticfilesystem",
      Prelude._svcVersion = "2015-02-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "EFS",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Returned if the size of @SecurityGroups@ specified in the request is
-- greater than five.
_SecurityGroupLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SecurityGroupLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "SecurityGroupLimitExceeded"
    Prelude.. Prelude.hasStatus 400

-- | Returned if a file system has mount targets.
_FileSystemInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileSystemInUse =
  Prelude._MatchServiceError
    defaultService
    "FileSystemInUse"
    Prelude.. Prelude.hasStatus 409

-- | The service timed out trying to fulfill the request, and the client
-- should try the call again.
_DependencyTimeout :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DependencyTimeout =
  Prelude._MatchServiceError
    defaultService
    "DependencyTimeout"
    Prelude.. Prelude.hasStatus 504

-- | Returned if the throughput mode or amount of provisioned throughput
-- can\'t be changed because the throughput limit of 1024 MiB\/s has been
-- reached.
_ThroughputLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ThroughputLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "ThroughputLimitExceeded"
    Prelude.. Prelude.hasStatus 400

-- | Returned if the access point you are trying to create already exists,
-- with the creation token you provided in the request.
_AccessPointAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessPointAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "AccessPointAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | Returned if the AWS account has already created the maximum number of
-- access points allowed per file system.
_AccessPointLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessPointLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "AccessPointLimitExceeded"
    Prelude.. Prelude.hasStatus 403

-- | Returned if the @FileSystemPolicy@ is is malformed or contains an error
-- such as an invalid parameter value or a missing required parameter.
-- Returned in the case of a policy lockout safety check error.
_InvalidPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPolicyException =
  Prelude._MatchServiceError
    defaultService
    "InvalidPolicyException"
    Prelude.. Prelude.hasStatus 400

-- | Returned if the specified @AccessPointId@ value doesn\'t exist in the
-- requester\'s AWS account.
_AccessPointNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessPointNotFound =
  Prelude._MatchServiceError
    defaultService
    "AccessPointNotFound"
    Prelude.. Prelude.hasStatus 404

-- | Returned if the specified @FileSystemId@ value doesn\'t exist in the
-- requester\'s AWS account.
_FileSystemNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileSystemNotFound =
  Prelude._MatchServiceError
    defaultService
    "FileSystemNotFound"
    Prelude.. Prelude.hasStatus 404

-- | Returned if there is no subnet with ID @SubnetId@ provided in the
-- request.
_SubnetNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubnetNotFound =
  Prelude._MatchServiceError
    defaultService
    "SubnetNotFound"
    Prelude.. Prelude.hasStatus 400

-- | The calling account has reached the limit for elastic network interfaces
-- for the specific AWS Region. The client should try to delete some
-- elastic network interfaces or get the account limit raised. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Appendix_Limits.html Amazon VPC Limits>
-- in the /Amazon VPC User Guide/ (see the Network interfaces per VPC entry
-- in the table).
_NetworkInterfaceLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NetworkInterfaceLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "NetworkInterfaceLimitExceeded"
    Prelude.. Prelude.hasStatus 409

-- | Returned if the file system you are trying to create already exists,
-- with the creation token you provided.
_FileSystemAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileSystemAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "FileSystemAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | Returned if you don’t wait at least 24 hours before changing the
-- throughput mode, or decreasing the Provisioned Throughput value.
_TooManyRequests :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequests =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequests"
    Prelude.. Prelude.hasStatus 429

-- | Returned if the AWS account has already created the maximum number of
-- file systems allowed per account.
_FileSystemLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileSystemLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "FileSystemLimitExceeded"
    Prelude.. Prelude.hasStatus 403

-- | Returned if the mount target is not in the correct state for the
-- operation.
_IncorrectMountTargetState :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IncorrectMountTargetState =
  Prelude._MatchServiceError
    defaultService
    "IncorrectMountTargetState"
    Prelude.. Prelude.hasStatus 409

-- | Returned if an error occurred on the server side.
_InternalServerError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerError =
  Prelude._MatchServiceError
    defaultService
    "InternalServerError"
    Prelude.. Prelude.hasStatus 500

-- | Returned if there is no mount target with the specified ID found in the
-- caller\'s account.
_MountTargetNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MountTargetNotFound =
  Prelude._MatchServiceError
    defaultService
    "MountTargetNotFound"
    Prelude.. Prelude.hasStatus 404

-- | Returned if the AWS Backup service is not available in the region that
-- the request was made.
_ValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ValidationException =
  Prelude._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Prelude.hasStatus 400

-- | Returned if @IpAddress@ was not specified in the request and there are
-- no free IP addresses in the subnet.
_NoFreeAddressesInSubnet :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoFreeAddressesInSubnet =
  Prelude._MatchServiceError
    defaultService
    "NoFreeAddressesInSubnet"
    Prelude.. Prelude.hasStatus 409

-- | Returned if the request is malformed or contains an error such as an
-- invalid parameter value or a missing required parameter.
_BadRequest :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BadRequest =
  Prelude._MatchServiceError
    defaultService
    "BadRequest"
    Prelude.. Prelude.hasStatus 400

-- | Returned if there\'s not enough capacity to provision additional
-- throughput. This value might be returned when you try to create a file
-- system in provisioned throughput mode, when you attempt to increase the
-- provisioned throughput of an existing file system, or when you attempt
-- to change an existing file system from bursting to provisioned
-- throughput mode.
_InsufficientThroughputCapacity :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsufficientThroughputCapacity =
  Prelude._MatchServiceError
    defaultService
    "InsufficientThroughputCapacity"
    Prelude.. Prelude.hasStatus 503

-- | Returned if the default file system policy is in effect for the EFS file
-- system specified.
_PolicyNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PolicyNotFound =
  Prelude._MatchServiceError
    defaultService
    "PolicyNotFound"
    Prelude.. Prelude.hasStatus 404

-- | Returned if the file system\'s lifecycle state is not \"available\".
_IncorrectFileSystemLifeCycleState :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IncorrectFileSystemLifeCycleState =
  Prelude._MatchServiceError
    defaultService
    "IncorrectFileSystemLifeCycleState"
    Prelude.. Prelude.hasStatus 409

-- | Returned if the request specified an @IpAddress@ that is already in use
-- in the subnet.
_IpAddressInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IpAddressInUse =
  Prelude._MatchServiceError
    defaultService
    "IpAddressInUse"
    Prelude.. Prelude.hasStatus 409

-- | Returned if the mount target would violate one of the specified
-- restrictions based on the file system\'s existing mount targets.
_MountTargetConflict :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MountTargetConflict =
  Prelude._MatchServiceError
    defaultService
    "MountTargetConflict"
    Prelude.. Prelude.hasStatus 409

-- | Returned if one of the specified security groups doesn\'t exist in the
-- subnet\'s VPC.
_SecurityGroupNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SecurityGroupNotFound =
  Prelude._MatchServiceError
    defaultService
    "SecurityGroupNotFound"
    Prelude.. Prelude.hasStatus 400

-- |
_UnsupportedAvailabilityZone :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedAvailabilityZone =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedAvailabilityZone"
    Prelude.. Prelude.hasStatus 400

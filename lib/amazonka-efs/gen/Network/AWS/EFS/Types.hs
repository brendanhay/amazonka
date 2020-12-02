{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types
  ( -- * Service Configuration
    efs,

    -- * Errors

    -- * LifeCycleState
    LifeCycleState (..),

    -- * PerformanceMode
    PerformanceMode (..),

    -- * Status
    Status (..),

    -- * ThroughputMode
    ThroughputMode (..),

    -- * TransitionToIARules
    TransitionToIARules (..),

    -- * AccessPointDescription
    AccessPointDescription,
    accessPointDescription,
    apdPosixUser,
    apdRootDirectory,
    apdClientToken,
    apdAccessPointId,
    apdFileSystemId,
    apdOwnerId,
    apdName,
    apdAccessPointARN,
    apdLifeCycleState,
    apdTags,

    -- * BackupPolicy
    BackupPolicy,
    backupPolicy,
    bpStatus,

    -- * BackupPolicyDescription
    BackupPolicyDescription,
    backupPolicyDescription,
    bpdBackupPolicy,

    -- * CreationInfo
    CreationInfo,
    creationInfo,
    ciOwnerUid,
    ciOwnerGid,
    ciPermissions,

    -- * FileSystemDescription
    FileSystemDescription,
    fileSystemDescription,
    fsdProvisionedThroughputInMibps,
    fsdFileSystemARN,
    fsdEncrypted,
    fsdThroughputMode,
    fsdKMSKeyId,
    fsdName,
    fsdOwnerId,
    fsdCreationToken,
    fsdFileSystemId,
    fsdCreationTime,
    fsdLifeCycleState,
    fsdNumberOfMountTargets,
    fsdSizeInBytes,
    fsdPerformanceMode,
    fsdTags,

    -- * FileSystemPolicyDescription
    FileSystemPolicyDescription,
    fileSystemPolicyDescription,
    fspdFileSystemId,
    fspdPolicy,

    -- * FileSystemSize
    FileSystemSize,
    fileSystemSize,
    fssValueInIA,
    fssValueInStandard,
    fssTimestamp,
    fssValue,

    -- * LifecycleConfigurationDescription
    LifecycleConfigurationDescription,
    lifecycleConfigurationDescription,
    lcdLifecyclePolicies,

    -- * LifecyclePolicy
    LifecyclePolicy,
    lifecyclePolicy,
    lpTransitionToIA,

    -- * MountTargetDescription
    MountTargetDescription,
    mountTargetDescription,
    mtdIPAddress,
    mtdAvailabilityZoneId,
    mtdVPCId,
    mtdAvailabilityZoneName,
    mtdNetworkInterfaceId,
    mtdOwnerId,
    mtdMountTargetId,
    mtdFileSystemId,
    mtdSubnetId,
    mtdLifeCycleState,

    -- * PosixUser
    PosixUser,
    posixUser,
    puSecondaryGids,
    puUid,
    puGid,

    -- * RootDirectory
    RootDirectory,
    rootDirectory,
    rdCreationInfo,
    rdPath,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,
  )
where

import Network.AWS.EFS.Types.AccessPointDescription
import Network.AWS.EFS.Types.BackupPolicy
import Network.AWS.EFS.Types.BackupPolicyDescription
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
import Network.AWS.EFS.Types.Status
import Network.AWS.EFS.Types.Tag
import Network.AWS.EFS.Types.ThroughputMode
import Network.AWS.EFS.Types.TransitionToIARules
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-02-01@ of the Amazon Elastic File System SDK configuration.
efs :: Service
efs =
  Service
    { _svcAbbrev = "EFS",
      _svcSigner = v4,
      _svcPrefix = "elasticfilesystem",
      _svcVersion = "2015-02-01",
      _svcEndpoint = defaultEndpoint efs,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "EFS",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

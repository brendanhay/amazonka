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
    efsService,

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
    AccessPointDescription (..),
    mkAccessPointDescription,
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
    BackupPolicy (..),
    mkBackupPolicy,
    bpStatus,

    -- * BackupPolicyDescription
    BackupPolicyDescription (..),
    mkBackupPolicyDescription,
    bpdBackupPolicy,

    -- * CreationInfo
    CreationInfo (..),
    mkCreationInfo,
    ciOwnerGid,
    ciPermissions,
    ciOwnerUid,

    -- * FileSystemDescription
    FileSystemDescription (..),
    mkFileSystemDescription,
    fsdCreationTime,
    fsdNumberOfMountTargets,
    fsdProvisionedThroughputInMibps,
    fsdPerformanceMode,
    fsdSizeInBytes,
    fsdFileSystemId,
    fsdFileSystemARN,
    fsdEncrypted,
    fsdThroughputMode,
    fsdOwnerId,
    fsdKMSKeyId,
    fsdName,
    fsdCreationToken,
    fsdLifeCycleState,
    fsdTags,

    -- * FileSystemPolicyDescription
    FileSystemPolicyDescription (..),
    mkFileSystemPolicyDescription,
    fspdFileSystemId,
    fspdPolicy,

    -- * FileSystemSize
    FileSystemSize (..),
    mkFileSystemSize,
    fssValue,
    fssValueInIA,
    fssValueInStandard,
    fssTimestamp,

    -- * LifecycleConfigurationDescription
    LifecycleConfigurationDescription (..),
    mkLifecycleConfigurationDescription,
    lcdLifecyclePolicies,

    -- * LifecyclePolicy
    LifecyclePolicy (..),
    mkLifecyclePolicy,
    lpTransitionToIA,

    -- * MountTargetDescription
    MountTargetDescription (..),
    mkMountTargetDescription,
    mtdIPAddress,
    mtdAvailabilityZoneId,
    mtdVPCId,
    mtdAvailabilityZoneName,
    mtdNetworkInterfaceId,
    mtdFileSystemId,
    mtdSubnetId,
    mtdOwnerId,
    mtdLifeCycleState,
    mtdMountTargetId,

    -- * PosixUser
    PosixUser (..),
    mkPosixUser,
    puUid,
    puSecondaryGids,
    puGid,

    -- * RootDirectory
    RootDirectory (..),
    mkRootDirectory,
    rdCreationInfo,
    rdPath,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-02-01@ of the Amazon Elastic File System SDK configuration.
efsService :: Lude.Service
efsService =
  Lude.Service
    { Lude._svcAbbrev = "EFS",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "elasticfilesystem",
      Lude._svcVersion = "2015-02-01",
      Lude._svcEndpoint = Lude.defaultEndpoint efsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "EFS",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

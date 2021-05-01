{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServiceUnavailableError,
    _InternalServerError,
    _InvalidGatewayRequestException,

    -- * ActiveDirectoryStatus
    ActiveDirectoryStatus (..),

    -- * AvailabilityMonitorTestStatus
    AvailabilityMonitorTestStatus (..),

    -- * CaseSensitivity
    CaseSensitivity (..),

    -- * FileShareType
    FileShareType (..),

    -- * HostEnvironment
    HostEnvironment (..),

    -- * ObjectACL
    ObjectACL (..),

    -- * PoolStatus
    PoolStatus (..),

    -- * RetentionLockType
    RetentionLockType (..),

    -- * SMBSecurityStrategy
    SMBSecurityStrategy (..),

    -- * TapeStorageClass
    TapeStorageClass (..),

    -- * AutomaticTapeCreationPolicyInfo
    AutomaticTapeCreationPolicyInfo (..),
    newAutomaticTapeCreationPolicyInfo,
    automaticTapeCreationPolicyInfo_automaticTapeCreationRules,
    automaticTapeCreationPolicyInfo_gatewayARN,

    -- * AutomaticTapeCreationRule
    AutomaticTapeCreationRule (..),
    newAutomaticTapeCreationRule,
    automaticTapeCreationRule_worm,
    automaticTapeCreationRule_tapeBarcodePrefix,
    automaticTapeCreationRule_poolId,
    automaticTapeCreationRule_tapeSizeInBytes,
    automaticTapeCreationRule_minimumNumTapes,

    -- * BandwidthRateLimitInterval
    BandwidthRateLimitInterval (..),
    newBandwidthRateLimitInterval,
    bandwidthRateLimitInterval_averageUploadRateLimitInBitsPerSec,
    bandwidthRateLimitInterval_averageDownloadRateLimitInBitsPerSec,
    bandwidthRateLimitInterval_startHourOfDay,
    bandwidthRateLimitInterval_startMinuteOfHour,
    bandwidthRateLimitInterval_endHourOfDay,
    bandwidthRateLimitInterval_endMinuteOfHour,
    bandwidthRateLimitInterval_daysOfWeek,

    -- * CacheAttributes
    CacheAttributes (..),
    newCacheAttributes,
    cacheAttributes_cacheStaleTimeoutInSeconds,

    -- * CachediSCSIVolume
    CachediSCSIVolume (..),
    newCachediSCSIVolume,
    cachediSCSIVolume_sourceSnapshotId,
    cachediSCSIVolume_volumeStatus,
    cachediSCSIVolume_createdDate,
    cachediSCSIVolume_targetName,
    cachediSCSIVolume_volumeARN,
    cachediSCSIVolume_volumeId,
    cachediSCSIVolume_kmsKey,
    cachediSCSIVolume_volumeiSCSIAttributes,
    cachediSCSIVolume_volumeUsedInBytes,
    cachediSCSIVolume_volumeSizeInBytes,
    cachediSCSIVolume_volumeType,
    cachediSCSIVolume_volumeAttachmentStatus,
    cachediSCSIVolume_volumeProgress,

    -- * ChapInfo
    ChapInfo (..),
    newChapInfo,
    chapInfo_initiatorName,
    chapInfo_targetARN,
    chapInfo_secretToAuthenticateTarget,
    chapInfo_secretToAuthenticateInitiator,

    -- * DeviceiSCSIAttributes
    DeviceiSCSIAttributes (..),
    newDeviceiSCSIAttributes,
    deviceiSCSIAttributes_chapEnabled,
    deviceiSCSIAttributes_targetARN,
    deviceiSCSIAttributes_networkInterfaceId,
    deviceiSCSIAttributes_networkInterfacePort,

    -- * Disk
    Disk (..),
    newDisk,
    disk_diskAllocationResource,
    disk_diskStatus,
    disk_diskSizeInBytes,
    disk_diskAttributeList,
    disk_diskPath,
    disk_diskId,
    disk_diskAllocationType,
    disk_diskNode,

    -- * FileShareInfo
    FileShareInfo (..),
    newFileShareInfo,
    fileShareInfo_fileShareId,
    fileShareInfo_fileShareType,
    fileShareInfo_fileShareStatus,
    fileShareInfo_fileShareARN,
    fileShareInfo_gatewayARN,

    -- * GatewayInfo
    GatewayInfo (..),
    newGatewayInfo,
    gatewayInfo_gatewayOperationalState,
    gatewayInfo_gatewayName,
    gatewayInfo_gatewayType,
    gatewayInfo_ec2InstanceRegion,
    gatewayInfo_ec2InstanceId,
    gatewayInfo_gatewayARN,
    gatewayInfo_gatewayId,

    -- * NFSFileShareDefaults
    NFSFileShareDefaults (..),
    newNFSFileShareDefaults,
    nFSFileShareDefaults_ownerId,
    nFSFileShareDefaults_groupId,
    nFSFileShareDefaults_directoryMode,
    nFSFileShareDefaults_fileMode,

    -- * NFSFileShareInfo
    NFSFileShareInfo (..),
    newNFSFileShareInfo,
    nFSFileShareInfo_defaultStorageClass,
    nFSFileShareInfo_fileShareName,
    nFSFileShareInfo_guessMIMETypeEnabled,
    nFSFileShareInfo_readOnly,
    nFSFileShareInfo_fileShareId,
    nFSFileShareInfo_kmsEncrypted,
    nFSFileShareInfo_locationARN,
    nFSFileShareInfo_squash,
    nFSFileShareInfo_notificationPolicy,
    nFSFileShareInfo_kmsKey,
    nFSFileShareInfo_fileShareStatus,
    nFSFileShareInfo_role,
    nFSFileShareInfo_tags,
    nFSFileShareInfo_fileShareARN,
    nFSFileShareInfo_cacheAttributes,
    nFSFileShareInfo_clientList,
    nFSFileShareInfo_objectACL,
    nFSFileShareInfo_nFSFileShareDefaults,
    nFSFileShareInfo_path,
    nFSFileShareInfo_gatewayARN,
    nFSFileShareInfo_requesterPays,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_macAddress,
    networkInterface_ipv6Address,
    networkInterface_ipv4Address,

    -- * PoolInfo
    PoolInfo (..),
    newPoolInfo,
    poolInfo_poolARN,
    poolInfo_poolName,
    poolInfo_storageClass,
    poolInfo_retentionLockType,
    poolInfo_poolStatus,
    poolInfo_retentionLockTimeInDays,

    -- * SMBFileShareInfo
    SMBFileShareInfo (..),
    newSMBFileShareInfo,
    sMBFileShareInfo_sMBACLEnabled,
    sMBFileShareInfo_accessBasedEnumeration,
    sMBFileShareInfo_defaultStorageClass,
    sMBFileShareInfo_fileShareName,
    sMBFileShareInfo_caseSensitivity,
    sMBFileShareInfo_guessMIMETypeEnabled,
    sMBFileShareInfo_readOnly,
    sMBFileShareInfo_fileShareId,
    sMBFileShareInfo_kmsEncrypted,
    sMBFileShareInfo_authentication,
    sMBFileShareInfo_locationARN,
    sMBFileShareInfo_notificationPolicy,
    sMBFileShareInfo_validUserList,
    sMBFileShareInfo_kmsKey,
    sMBFileShareInfo_fileShareStatus,
    sMBFileShareInfo_adminUserList,
    sMBFileShareInfo_auditDestinationARN,
    sMBFileShareInfo_role,
    sMBFileShareInfo_tags,
    sMBFileShareInfo_fileShareARN,
    sMBFileShareInfo_cacheAttributes,
    sMBFileShareInfo_objectACL,
    sMBFileShareInfo_path,
    sMBFileShareInfo_gatewayARN,
    sMBFileShareInfo_requesterPays,
    sMBFileShareInfo_invalidUserList,

    -- * StorediSCSIVolume
    StorediSCSIVolume (..),
    newStorediSCSIVolume,
    storediSCSIVolume_sourceSnapshotId,
    storediSCSIVolume_volumeStatus,
    storediSCSIVolume_createdDate,
    storediSCSIVolume_targetName,
    storediSCSIVolume_volumeARN,
    storediSCSIVolume_volumeId,
    storediSCSIVolume_kmsKey,
    storediSCSIVolume_preservedExistingData,
    storediSCSIVolume_volumeiSCSIAttributes,
    storediSCSIVolume_volumeUsedInBytes,
    storediSCSIVolume_volumeSizeInBytes,
    storediSCSIVolume_volumeDiskId,
    storediSCSIVolume_volumeType,
    storediSCSIVolume_volumeAttachmentStatus,
    storediSCSIVolume_volumeProgress,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Tape
    Tape (..),
    newTape,
    tape_poolEntryDate,
    tape_tapeStatus,
    tape_tapeCreatedDate,
    tape_poolId,
    tape_vTLDevice,
    tape_tapeARN,
    tape_kmsKey,
    tape_worm,
    tape_tapeBarcode,
    tape_tapeUsedInBytes,
    tape_tapeSizeInBytes,
    tape_retentionStartDate,
    tape_progress,

    -- * TapeArchive
    TapeArchive (..),
    newTapeArchive,
    tapeArchive_poolEntryDate,
    tapeArchive_tapeStatus,
    tapeArchive_tapeCreatedDate,
    tapeArchive_poolId,
    tapeArchive_completionTime,
    tapeArchive_retrievedTo,
    tapeArchive_tapeARN,
    tapeArchive_kmsKey,
    tapeArchive_worm,
    tapeArchive_tapeBarcode,
    tapeArchive_tapeUsedInBytes,
    tapeArchive_tapeSizeInBytes,
    tapeArchive_retentionStartDate,

    -- * TapeInfo
    TapeInfo (..),
    newTapeInfo,
    tapeInfo_poolEntryDate,
    tapeInfo_tapeStatus,
    tapeInfo_poolId,
    tapeInfo_tapeARN,
    tapeInfo_tapeBarcode,
    tapeInfo_tapeSizeInBytes,
    tapeInfo_retentionStartDate,
    tapeInfo_gatewayARN,

    -- * TapeRecoveryPointInfo
    TapeRecoveryPointInfo (..),
    newTapeRecoveryPointInfo,
    tapeRecoveryPointInfo_tapeStatus,
    tapeRecoveryPointInfo_tapeARN,
    tapeRecoveryPointInfo_tapeSizeInBytes,
    tapeRecoveryPointInfo_tapeRecoveryPointTime,

    -- * VTLDevice
    VTLDevice (..),
    newVTLDevice,
    vTLDevice_vTLDeviceProductIdentifier,
    vTLDevice_vTLDeviceVendor,
    vTLDevice_deviceiSCSIAttributes,
    vTLDevice_vTLDeviceType,
    vTLDevice_vTLDeviceARN,

    -- * VolumeInfo
    VolumeInfo (..),
    newVolumeInfo,
    volumeInfo_volumeARN,
    volumeInfo_volumeId,
    volumeInfo_volumeSizeInBytes,
    volumeInfo_volumeType,
    volumeInfo_gatewayARN,
    volumeInfo_volumeAttachmentStatus,
    volumeInfo_gatewayId,

    -- * VolumeRecoveryPointInfo
    VolumeRecoveryPointInfo (..),
    newVolumeRecoveryPointInfo,
    volumeRecoveryPointInfo_volumeARN,
    volumeRecoveryPointInfo_volumeSizeInBytes,
    volumeRecoveryPointInfo_volumeUsageInBytes,
    volumeRecoveryPointInfo_volumeRecoveryPointTime,

    -- * VolumeiSCSIAttributes
    VolumeiSCSIAttributes (..),
    newVolumeiSCSIAttributes,
    volumeiSCSIAttributes_chapEnabled,
    volumeiSCSIAttributes_lunNumber,
    volumeiSCSIAttributes_targetARN,
    volumeiSCSIAttributes_networkInterfaceId,
    volumeiSCSIAttributes_networkInterfacePort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.StorageGateway.Types.ActiveDirectoryStatus
import Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
import Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule
import Network.AWS.StorageGateway.Types.AvailabilityMonitorTestStatus
import Network.AWS.StorageGateway.Types.BandwidthRateLimitInterval
import Network.AWS.StorageGateway.Types.CacheAttributes
import Network.AWS.StorageGateway.Types.CachediSCSIVolume
import Network.AWS.StorageGateway.Types.CaseSensitivity
import Network.AWS.StorageGateway.Types.ChapInfo
import Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes
import Network.AWS.StorageGateway.Types.Disk
import Network.AWS.StorageGateway.Types.FileShareInfo
import Network.AWS.StorageGateway.Types.FileShareType
import Network.AWS.StorageGateway.Types.GatewayInfo
import Network.AWS.StorageGateway.Types.HostEnvironment
import Network.AWS.StorageGateway.Types.NFSFileShareDefaults
import Network.AWS.StorageGateway.Types.NFSFileShareInfo
import Network.AWS.StorageGateway.Types.NetworkInterface
import Network.AWS.StorageGateway.Types.ObjectACL
import Network.AWS.StorageGateway.Types.PoolInfo
import Network.AWS.StorageGateway.Types.PoolStatus
import Network.AWS.StorageGateway.Types.RetentionLockType
import Network.AWS.StorageGateway.Types.SMBFileShareInfo
import Network.AWS.StorageGateway.Types.SMBSecurityStrategy
import Network.AWS.StorageGateway.Types.StorediSCSIVolume
import Network.AWS.StorageGateway.Types.Tag
import Network.AWS.StorageGateway.Types.Tape
import Network.AWS.StorageGateway.Types.TapeArchive
import Network.AWS.StorageGateway.Types.TapeInfo
import Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo
import Network.AWS.StorageGateway.Types.TapeStorageClass
import Network.AWS.StorageGateway.Types.VTLDevice
import Network.AWS.StorageGateway.Types.VolumeInfo
import Network.AWS.StorageGateway.Types.VolumeRecoveryPointInfo
import Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes

-- | API version @2013-06-30@ of the Amazon Storage Gateway SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "StorageGateway",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "storagegateway",
      Prelude._svcVersion = "2013-06-30",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "StorageGateway",
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

-- | An internal server error has occurred because the service is
-- unavailable. For more information, see the error and message fields.
_ServiceUnavailableError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceUnavailableError =
  Prelude._MatchServiceError
    defaultService
    "ServiceUnavailableError"

-- | An internal server error has occurred during the request. For more
-- information, see the error and message fields.
_InternalServerError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerError =
  Prelude._MatchServiceError
    defaultService
    "InternalServerError"

-- | An exception occurred because an invalid gateway request was issued to
-- the service. For more information, see the error and message fields.
_InvalidGatewayRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidGatewayRequestException =
  Prelude._MatchServiceError
    defaultService
    "InvalidGatewayRequestException"

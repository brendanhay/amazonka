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
    _InvalidGatewayRequestException,
    _ServiceUnavailableError,
    _InternalServerError,

    -- * ActiveDirectoryStatus
    ActiveDirectoryStatus (..),

    -- * AvailabilityMonitorTestStatus
    AvailabilityMonitorTestStatus (..),

    -- * CaseSensitivity
    CaseSensitivity (..),

    -- * FileShareType
    FileShareType (..),

    -- * GatewayCapacity
    GatewayCapacity (..),

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
    automaticTapeCreationPolicyInfo_gatewayARN,
    automaticTapeCreationPolicyInfo_automaticTapeCreationRules,

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
    cachediSCSIVolume_volumeiSCSIAttributes,
    cachediSCSIVolume_volumeStatus,
    cachediSCSIVolume_sourceSnapshotId,
    cachediSCSIVolume_kmsKey,
    cachediSCSIVolume_volumeAttachmentStatus,
    cachediSCSIVolume_volumeARN,
    cachediSCSIVolume_volumeProgress,
    cachediSCSIVolume_volumeSizeInBytes,
    cachediSCSIVolume_volumeUsedInBytes,
    cachediSCSIVolume_createdDate,
    cachediSCSIVolume_volumeId,
    cachediSCSIVolume_volumeType,
    cachediSCSIVolume_targetName,

    -- * ChapInfo
    ChapInfo (..),
    newChapInfo,
    chapInfo_targetARN,
    chapInfo_secretToAuthenticateInitiator,
    chapInfo_initiatorName,
    chapInfo_secretToAuthenticateTarget,

    -- * DeviceiSCSIAttributes
    DeviceiSCSIAttributes (..),
    newDeviceiSCSIAttributes,
    deviceiSCSIAttributes_targetARN,
    deviceiSCSIAttributes_chapEnabled,
    deviceiSCSIAttributes_networkInterfaceId,
    deviceiSCSIAttributes_networkInterfacePort,

    -- * Disk
    Disk (..),
    newDisk,
    disk_diskAllocationResource,
    disk_diskAllocationType,
    disk_diskNode,
    disk_diskPath,
    disk_diskSizeInBytes,
    disk_diskStatus,
    disk_diskId,
    disk_diskAttributeList,

    -- * EndpointNetworkConfiguration
    EndpointNetworkConfiguration (..),
    newEndpointNetworkConfiguration,
    endpointNetworkConfiguration_ipAddresses,

    -- * FileShareInfo
    FileShareInfo (..),
    newFileShareInfo,
    fileShareInfo_fileShareStatus,
    fileShareInfo_gatewayARN,
    fileShareInfo_fileShareId,
    fileShareInfo_fileShareARN,
    fileShareInfo_fileShareType,

    -- * FileSystemAssociationInfo
    FileSystemAssociationInfo (..),
    newFileSystemAssociationInfo,
    fileSystemAssociationInfo_auditDestinationARN,
    fileSystemAssociationInfo_fileSystemAssociationARN,
    fileSystemAssociationInfo_gatewayARN,
    fileSystemAssociationInfo_cacheAttributes,
    fileSystemAssociationInfo_endpointNetworkConfiguration,
    fileSystemAssociationInfo_locationARN,
    fileSystemAssociationInfo_fileSystemAssociationStatus,
    fileSystemAssociationInfo_tags,

    -- * FileSystemAssociationSummary
    FileSystemAssociationSummary (..),
    newFileSystemAssociationSummary,
    fileSystemAssociationSummary_fileSystemAssociationARN,
    fileSystemAssociationSummary_gatewayARN,
    fileSystemAssociationSummary_fileSystemAssociationId,
    fileSystemAssociationSummary_fileSystemAssociationStatus,

    -- * GatewayInfo
    GatewayInfo (..),
    newGatewayInfo,
    gatewayInfo_ec2InstanceRegion,
    gatewayInfo_gatewayARN,
    gatewayInfo_ec2InstanceId,
    gatewayInfo_gatewayOperationalState,
    gatewayInfo_gatewayName,
    gatewayInfo_gatewayId,
    gatewayInfo_gatewayType,

    -- * NFSFileShareDefaults
    NFSFileShareDefaults (..),
    newNFSFileShareDefaults,
    nFSFileShareDefaults_fileMode,
    nFSFileShareDefaults_ownerId,
    nFSFileShareDefaults_directoryMode,
    nFSFileShareDefaults_groupId,

    -- * NFSFileShareInfo
    NFSFileShareInfo (..),
    newNFSFileShareInfo,
    nFSFileShareInfo_fileShareStatus,
    nFSFileShareInfo_kmsKey,
    nFSFileShareInfo_gatewayARN,
    nFSFileShareInfo_path,
    nFSFileShareInfo_vPCEndpointDNSName,
    nFSFileShareInfo_cacheAttributes,
    nFSFileShareInfo_objectACL,
    nFSFileShareInfo_kmsEncrypted,
    nFSFileShareInfo_fileShareId,
    nFSFileShareInfo_fileShareARN,
    nFSFileShareInfo_defaultStorageClass,
    nFSFileShareInfo_fileShareName,
    nFSFileShareInfo_role,
    nFSFileShareInfo_notificationPolicy,
    nFSFileShareInfo_squash,
    nFSFileShareInfo_requesterPays,
    nFSFileShareInfo_nFSFileShareDefaults,
    nFSFileShareInfo_locationARN,
    nFSFileShareInfo_clientList,
    nFSFileShareInfo_guessMIMETypeEnabled,
    nFSFileShareInfo_readOnly,
    nFSFileShareInfo_bucketRegion,
    nFSFileShareInfo_tags,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_ipv6Address,
    networkInterface_macAddress,
    networkInterface_ipv4Address,

    -- * PoolInfo
    PoolInfo (..),
    newPoolInfo,
    poolInfo_retentionLockType,
    poolInfo_retentionLockTimeInDays,
    poolInfo_poolName,
    poolInfo_storageClass,
    poolInfo_poolStatus,
    poolInfo_poolARN,

    -- * SMBFileShareInfo
    SMBFileShareInfo (..),
    newSMBFileShareInfo,
    sMBFileShareInfo_accessBasedEnumeration,
    sMBFileShareInfo_adminUserList,
    sMBFileShareInfo_auditDestinationARN,
    sMBFileShareInfo_fileShareStatus,
    sMBFileShareInfo_invalidUserList,
    sMBFileShareInfo_kmsKey,
    sMBFileShareInfo_validUserList,
    sMBFileShareInfo_gatewayARN,
    sMBFileShareInfo_path,
    sMBFileShareInfo_vPCEndpointDNSName,
    sMBFileShareInfo_authentication,
    sMBFileShareInfo_cacheAttributes,
    sMBFileShareInfo_objectACL,
    sMBFileShareInfo_kmsEncrypted,
    sMBFileShareInfo_fileShareId,
    sMBFileShareInfo_fileShareARN,
    sMBFileShareInfo_defaultStorageClass,
    sMBFileShareInfo_fileShareName,
    sMBFileShareInfo_role,
    sMBFileShareInfo_sMBACLEnabled,
    sMBFileShareInfo_oplocksEnabled,
    sMBFileShareInfo_notificationPolicy,
    sMBFileShareInfo_requesterPays,
    sMBFileShareInfo_locationARN,
    sMBFileShareInfo_guessMIMETypeEnabled,
    sMBFileShareInfo_readOnly,
    sMBFileShareInfo_bucketRegion,
    sMBFileShareInfo_caseSensitivity,
    sMBFileShareInfo_tags,

    -- * StorediSCSIVolume
    StorediSCSIVolume (..),
    newStorediSCSIVolume,
    storediSCSIVolume_volumeiSCSIAttributes,
    storediSCSIVolume_volumeStatus,
    storediSCSIVolume_sourceSnapshotId,
    storediSCSIVolume_preservedExistingData,
    storediSCSIVolume_kmsKey,
    storediSCSIVolume_volumeAttachmentStatus,
    storediSCSIVolume_volumeARN,
    storediSCSIVolume_volumeProgress,
    storediSCSIVolume_volumeSizeInBytes,
    storediSCSIVolume_volumeUsedInBytes,
    storediSCSIVolume_createdDate,
    storediSCSIVolume_volumeId,
    storediSCSIVolume_volumeDiskId,
    storediSCSIVolume_volumeType,
    storediSCSIVolume_targetName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Tape
    Tape (..),
    newTape,
    tape_tapeBarcode,
    tape_tapeStatus,
    tape_kmsKey,
    tape_tapeARN,
    tape_progress,
    tape_tapeSizeInBytes,
    tape_vTLDevice,
    tape_poolId,
    tape_tapeUsedInBytes,
    tape_tapeCreatedDate,
    tape_poolEntryDate,
    tape_worm,
    tape_retentionStartDate,

    -- * TapeArchive
    TapeArchive (..),
    newTapeArchive,
    tapeArchive_tapeBarcode,
    tapeArchive_tapeStatus,
    tapeArchive_kmsKey,
    tapeArchive_tapeARN,
    tapeArchive_tapeSizeInBytes,
    tapeArchive_completionTime,
    tapeArchive_poolId,
    tapeArchive_tapeUsedInBytes,
    tapeArchive_tapeCreatedDate,
    tapeArchive_poolEntryDate,
    tapeArchive_worm,
    tapeArchive_retentionStartDate,
    tapeArchive_retrievedTo,

    -- * TapeInfo
    TapeInfo (..),
    newTapeInfo,
    tapeInfo_tapeBarcode,
    tapeInfo_tapeStatus,
    tapeInfo_tapeARN,
    tapeInfo_gatewayARN,
    tapeInfo_tapeSizeInBytes,
    tapeInfo_poolId,
    tapeInfo_poolEntryDate,
    tapeInfo_retentionStartDate,

    -- * TapeRecoveryPointInfo
    TapeRecoveryPointInfo (..),
    newTapeRecoveryPointInfo,
    tapeRecoveryPointInfo_tapeStatus,
    tapeRecoveryPointInfo_tapeRecoveryPointTime,
    tapeRecoveryPointInfo_tapeARN,
    tapeRecoveryPointInfo_tapeSizeInBytes,

    -- * VTLDevice
    VTLDevice (..),
    newVTLDevice,
    vTLDevice_deviceiSCSIAttributes,
    vTLDevice_vTLDeviceVendor,
    vTLDevice_vTLDeviceARN,
    vTLDevice_vTLDeviceType,
    vTLDevice_vTLDeviceProductIdentifier,

    -- * VolumeInfo
    VolumeInfo (..),
    newVolumeInfo,
    volumeInfo_gatewayARN,
    volumeInfo_volumeAttachmentStatus,
    volumeInfo_volumeARN,
    volumeInfo_volumeSizeInBytes,
    volumeInfo_volumeId,
    volumeInfo_gatewayId,
    volumeInfo_volumeType,

    -- * VolumeRecoveryPointInfo
    VolumeRecoveryPointInfo (..),
    newVolumeRecoveryPointInfo,
    volumeRecoveryPointInfo_volumeRecoveryPointTime,
    volumeRecoveryPointInfo_volumeARN,
    volumeRecoveryPointInfo_volumeSizeInBytes,
    volumeRecoveryPointInfo_volumeUsageInBytes,

    -- * VolumeiSCSIAttributes
    VolumeiSCSIAttributes (..),
    newVolumeiSCSIAttributes,
    volumeiSCSIAttributes_lunNumber,
    volumeiSCSIAttributes_targetARN,
    volumeiSCSIAttributes_chapEnabled,
    volumeiSCSIAttributes_networkInterfaceId,
    volumeiSCSIAttributes_networkInterfacePort,
  )
where

import qualified Network.AWS.Core as Core
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
import Network.AWS.StorageGateway.Types.EndpointNetworkConfiguration
import Network.AWS.StorageGateway.Types.FileShareInfo
import Network.AWS.StorageGateway.Types.FileShareType
import Network.AWS.StorageGateway.Types.FileSystemAssociationInfo
import Network.AWS.StorageGateway.Types.FileSystemAssociationSummary
import Network.AWS.StorageGateway.Types.GatewayCapacity
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
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "StorageGateway",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "storagegateway",
      Core._serviceSigningName = "storagegateway",
      Core._serviceVersion = "2013-06-30",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "StorageGateway",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | An exception occurred because an invalid gateway request was issued to
-- the service. For more information, see the error and message fields.
_InvalidGatewayRequestException :: (Core.AsError a) => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGatewayRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidGatewayRequestException"

-- | An internal server error has occurred because the service is
-- unavailable. For more information, see the error and message fields.
_ServiceUnavailableError :: (Core.AsError a) => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableError =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableError"

-- | An internal server error has occurred during the request. For more
-- information, see the error and message fields.
_InternalServerError :: (Core.AsError a) => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.StorageGateway.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerError,
    _InvalidGatewayRequestException,
    _ServiceUnavailableError,

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
    bandwidthRateLimitInterval_averageDownloadRateLimitInBitsPerSec,
    bandwidthRateLimitInterval_averageUploadRateLimitInBitsPerSec,
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
    cachediSCSIVolume_createdDate,
    cachediSCSIVolume_kmsKey,
    cachediSCSIVolume_sourceSnapshotId,
    cachediSCSIVolume_targetName,
    cachediSCSIVolume_volumeARN,
    cachediSCSIVolume_volumeAttachmentStatus,
    cachediSCSIVolume_volumeId,
    cachediSCSIVolume_volumeProgress,
    cachediSCSIVolume_volumeSizeInBytes,
    cachediSCSIVolume_volumeStatus,
    cachediSCSIVolume_volumeType,
    cachediSCSIVolume_volumeUsedInBytes,
    cachediSCSIVolume_volumeiSCSIAttributes,

    -- * ChapInfo
    ChapInfo (..),
    newChapInfo,
    chapInfo_initiatorName,
    chapInfo_secretToAuthenticateInitiator,
    chapInfo_secretToAuthenticateTarget,
    chapInfo_targetARN,

    -- * DeviceiSCSIAttributes
    DeviceiSCSIAttributes (..),
    newDeviceiSCSIAttributes,
    deviceiSCSIAttributes_chapEnabled,
    deviceiSCSIAttributes_networkInterfaceId,
    deviceiSCSIAttributes_networkInterfacePort,
    deviceiSCSIAttributes_targetARN,

    -- * Disk
    Disk (..),
    newDisk,
    disk_diskAllocationResource,
    disk_diskAllocationType,
    disk_diskAttributeList,
    disk_diskId,
    disk_diskNode,
    disk_diskPath,
    disk_diskSizeInBytes,
    disk_diskStatus,

    -- * EndpointNetworkConfiguration
    EndpointNetworkConfiguration (..),
    newEndpointNetworkConfiguration,
    endpointNetworkConfiguration_ipAddresses,

    -- * FileShareInfo
    FileShareInfo (..),
    newFileShareInfo,
    fileShareInfo_fileShareARN,
    fileShareInfo_fileShareId,
    fileShareInfo_fileShareStatus,
    fileShareInfo_fileShareType,
    fileShareInfo_gatewayARN,

    -- * FileSystemAssociationInfo
    FileSystemAssociationInfo (..),
    newFileSystemAssociationInfo,
    fileSystemAssociationInfo_auditDestinationARN,
    fileSystemAssociationInfo_cacheAttributes,
    fileSystemAssociationInfo_endpointNetworkConfiguration,
    fileSystemAssociationInfo_fileSystemAssociationARN,
    fileSystemAssociationInfo_fileSystemAssociationStatus,
    fileSystemAssociationInfo_fileSystemAssociationStatusDetails,
    fileSystemAssociationInfo_gatewayARN,
    fileSystemAssociationInfo_locationARN,
    fileSystemAssociationInfo_tags,

    -- * FileSystemAssociationStatusDetail
    FileSystemAssociationStatusDetail (..),
    newFileSystemAssociationStatusDetail,
    fileSystemAssociationStatusDetail_errorCode,

    -- * FileSystemAssociationSummary
    FileSystemAssociationSummary (..),
    newFileSystemAssociationSummary,
    fileSystemAssociationSummary_fileSystemAssociationARN,
    fileSystemAssociationSummary_fileSystemAssociationId,
    fileSystemAssociationSummary_fileSystemAssociationStatus,
    fileSystemAssociationSummary_gatewayARN,

    -- * GatewayInfo
    GatewayInfo (..),
    newGatewayInfo,
    gatewayInfo_ec2InstanceId,
    gatewayInfo_ec2InstanceRegion,
    gatewayInfo_gatewayARN,
    gatewayInfo_gatewayId,
    gatewayInfo_gatewayName,
    gatewayInfo_gatewayOperationalState,
    gatewayInfo_gatewayType,
    gatewayInfo_hostEnvironment,
    gatewayInfo_hostEnvironmentId,

    -- * NFSFileShareDefaults
    NFSFileShareDefaults (..),
    newNFSFileShareDefaults,
    nFSFileShareDefaults_directoryMode,
    nFSFileShareDefaults_fileMode,
    nFSFileShareDefaults_groupId,
    nFSFileShareDefaults_ownerId,

    -- * NFSFileShareInfo
    NFSFileShareInfo (..),
    newNFSFileShareInfo,
    nFSFileShareInfo_auditDestinationARN,
    nFSFileShareInfo_bucketRegion,
    nFSFileShareInfo_cacheAttributes,
    nFSFileShareInfo_clientList,
    nFSFileShareInfo_defaultStorageClass,
    nFSFileShareInfo_fileShareARN,
    nFSFileShareInfo_fileShareId,
    nFSFileShareInfo_fileShareName,
    nFSFileShareInfo_fileShareStatus,
    nFSFileShareInfo_gatewayARN,
    nFSFileShareInfo_guessMIMETypeEnabled,
    nFSFileShareInfo_kmsEncrypted,
    nFSFileShareInfo_kmsKey,
    nFSFileShareInfo_locationARN,
    nFSFileShareInfo_nFSFileShareDefaults,
    nFSFileShareInfo_notificationPolicy,
    nFSFileShareInfo_objectACL,
    nFSFileShareInfo_path,
    nFSFileShareInfo_readOnly,
    nFSFileShareInfo_requesterPays,
    nFSFileShareInfo_role,
    nFSFileShareInfo_squash,
    nFSFileShareInfo_tags,
    nFSFileShareInfo_vPCEndpointDNSName,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_ipv4Address,
    networkInterface_ipv6Address,
    networkInterface_macAddress,

    -- * PoolInfo
    PoolInfo (..),
    newPoolInfo,
    poolInfo_poolARN,
    poolInfo_poolName,
    poolInfo_poolStatus,
    poolInfo_retentionLockTimeInDays,
    poolInfo_retentionLockType,
    poolInfo_storageClass,

    -- * SMBFileShareInfo
    SMBFileShareInfo (..),
    newSMBFileShareInfo,
    sMBFileShareInfo_accessBasedEnumeration,
    sMBFileShareInfo_adminUserList,
    sMBFileShareInfo_auditDestinationARN,
    sMBFileShareInfo_authentication,
    sMBFileShareInfo_bucketRegion,
    sMBFileShareInfo_cacheAttributes,
    sMBFileShareInfo_caseSensitivity,
    sMBFileShareInfo_defaultStorageClass,
    sMBFileShareInfo_fileShareARN,
    sMBFileShareInfo_fileShareId,
    sMBFileShareInfo_fileShareName,
    sMBFileShareInfo_fileShareStatus,
    sMBFileShareInfo_gatewayARN,
    sMBFileShareInfo_guessMIMETypeEnabled,
    sMBFileShareInfo_invalidUserList,
    sMBFileShareInfo_kmsEncrypted,
    sMBFileShareInfo_kmsKey,
    sMBFileShareInfo_locationARN,
    sMBFileShareInfo_notificationPolicy,
    sMBFileShareInfo_objectACL,
    sMBFileShareInfo_oplocksEnabled,
    sMBFileShareInfo_path,
    sMBFileShareInfo_readOnly,
    sMBFileShareInfo_requesterPays,
    sMBFileShareInfo_role,
    sMBFileShareInfo_sMBACLEnabled,
    sMBFileShareInfo_tags,
    sMBFileShareInfo_vPCEndpointDNSName,
    sMBFileShareInfo_validUserList,

    -- * SMBLocalGroups
    SMBLocalGroups (..),
    newSMBLocalGroups,
    sMBLocalGroups_gatewayAdmins,

    -- * StorediSCSIVolume
    StorediSCSIVolume (..),
    newStorediSCSIVolume,
    storediSCSIVolume_createdDate,
    storediSCSIVolume_kmsKey,
    storediSCSIVolume_preservedExistingData,
    storediSCSIVolume_sourceSnapshotId,
    storediSCSIVolume_targetName,
    storediSCSIVolume_volumeARN,
    storediSCSIVolume_volumeAttachmentStatus,
    storediSCSIVolume_volumeDiskId,
    storediSCSIVolume_volumeId,
    storediSCSIVolume_volumeProgress,
    storediSCSIVolume_volumeSizeInBytes,
    storediSCSIVolume_volumeStatus,
    storediSCSIVolume_volumeType,
    storediSCSIVolume_volumeUsedInBytes,
    storediSCSIVolume_volumeiSCSIAttributes,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Tape
    Tape (..),
    newTape,
    tape_kmsKey,
    tape_poolEntryDate,
    tape_poolId,
    tape_progress,
    tape_retentionStartDate,
    tape_tapeARN,
    tape_tapeBarcode,
    tape_tapeCreatedDate,
    tape_tapeSizeInBytes,
    tape_tapeStatus,
    tape_tapeUsedInBytes,
    tape_vTLDevice,
    tape_worm,

    -- * TapeArchive
    TapeArchive (..),
    newTapeArchive,
    tapeArchive_completionTime,
    tapeArchive_kmsKey,
    tapeArchive_poolEntryDate,
    tapeArchive_poolId,
    tapeArchive_retentionStartDate,
    tapeArchive_retrievedTo,
    tapeArchive_tapeARN,
    tapeArchive_tapeBarcode,
    tapeArchive_tapeCreatedDate,
    tapeArchive_tapeSizeInBytes,
    tapeArchive_tapeStatus,
    tapeArchive_tapeUsedInBytes,
    tapeArchive_worm,

    -- * TapeInfo
    TapeInfo (..),
    newTapeInfo,
    tapeInfo_gatewayARN,
    tapeInfo_poolEntryDate,
    tapeInfo_poolId,
    tapeInfo_retentionStartDate,
    tapeInfo_tapeARN,
    tapeInfo_tapeBarcode,
    tapeInfo_tapeSizeInBytes,
    tapeInfo_tapeStatus,

    -- * TapeRecoveryPointInfo
    TapeRecoveryPointInfo (..),
    newTapeRecoveryPointInfo,
    tapeRecoveryPointInfo_tapeARN,
    tapeRecoveryPointInfo_tapeRecoveryPointTime,
    tapeRecoveryPointInfo_tapeSizeInBytes,
    tapeRecoveryPointInfo_tapeStatus,

    -- * VTLDevice
    VTLDevice (..),
    newVTLDevice,
    vTLDevice_deviceiSCSIAttributes,
    vTLDevice_vTLDeviceARN,
    vTLDevice_vTLDeviceProductIdentifier,
    vTLDevice_vTLDeviceType,
    vTLDevice_vTLDeviceVendor,

    -- * VolumeInfo
    VolumeInfo (..),
    newVolumeInfo,
    volumeInfo_gatewayARN,
    volumeInfo_gatewayId,
    volumeInfo_volumeARN,
    volumeInfo_volumeAttachmentStatus,
    volumeInfo_volumeId,
    volumeInfo_volumeSizeInBytes,
    volumeInfo_volumeType,

    -- * VolumeRecoveryPointInfo
    VolumeRecoveryPointInfo (..),
    newVolumeRecoveryPointInfo,
    volumeRecoveryPointInfo_volumeARN,
    volumeRecoveryPointInfo_volumeRecoveryPointTime,
    volumeRecoveryPointInfo_volumeSizeInBytes,
    volumeRecoveryPointInfo_volumeUsageInBytes,

    -- * VolumeiSCSIAttributes
    VolumeiSCSIAttributes (..),
    newVolumeiSCSIAttributes,
    volumeiSCSIAttributes_chapEnabled,
    volumeiSCSIAttributes_lunNumber,
    volumeiSCSIAttributes_networkInterfaceId,
    volumeiSCSIAttributes_networkInterfacePort,
    volumeiSCSIAttributes_targetARN,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.StorageGateway.Types.ActiveDirectoryStatus
import Amazonka.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
import Amazonka.StorageGateway.Types.AutomaticTapeCreationRule
import Amazonka.StorageGateway.Types.AvailabilityMonitorTestStatus
import Amazonka.StorageGateway.Types.BandwidthRateLimitInterval
import Amazonka.StorageGateway.Types.CacheAttributes
import Amazonka.StorageGateway.Types.CachediSCSIVolume
import Amazonka.StorageGateway.Types.CaseSensitivity
import Amazonka.StorageGateway.Types.ChapInfo
import Amazonka.StorageGateway.Types.DeviceiSCSIAttributes
import Amazonka.StorageGateway.Types.Disk
import Amazonka.StorageGateway.Types.EndpointNetworkConfiguration
import Amazonka.StorageGateway.Types.FileShareInfo
import Amazonka.StorageGateway.Types.FileShareType
import Amazonka.StorageGateway.Types.FileSystemAssociationInfo
import Amazonka.StorageGateway.Types.FileSystemAssociationStatusDetail
import Amazonka.StorageGateway.Types.FileSystemAssociationSummary
import Amazonka.StorageGateway.Types.GatewayCapacity
import Amazonka.StorageGateway.Types.GatewayInfo
import Amazonka.StorageGateway.Types.HostEnvironment
import Amazonka.StorageGateway.Types.NFSFileShareDefaults
import Amazonka.StorageGateway.Types.NFSFileShareInfo
import Amazonka.StorageGateway.Types.NetworkInterface
import Amazonka.StorageGateway.Types.ObjectACL
import Amazonka.StorageGateway.Types.PoolInfo
import Amazonka.StorageGateway.Types.PoolStatus
import Amazonka.StorageGateway.Types.RetentionLockType
import Amazonka.StorageGateway.Types.SMBFileShareInfo
import Amazonka.StorageGateway.Types.SMBLocalGroups
import Amazonka.StorageGateway.Types.SMBSecurityStrategy
import Amazonka.StorageGateway.Types.StorediSCSIVolume
import Amazonka.StorageGateway.Types.Tag
import Amazonka.StorageGateway.Types.Tape
import Amazonka.StorageGateway.Types.TapeArchive
import Amazonka.StorageGateway.Types.TapeInfo
import Amazonka.StorageGateway.Types.TapeRecoveryPointInfo
import Amazonka.StorageGateway.Types.TapeStorageClass
import Amazonka.StorageGateway.Types.VTLDevice
import Amazonka.StorageGateway.Types.VolumeInfo
import Amazonka.StorageGateway.Types.VolumeRecoveryPointInfo
import Amazonka.StorageGateway.Types.VolumeiSCSIAttributes

-- | API version @2013-06-30@ of the Amazon Storage Gateway SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "StorageGateway",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "storagegateway",
      Core.signingName = "storagegateway",
      Core.version = "2013-06-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "StorageGateway",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An internal server error has occurred during the request. For more
-- information, see the error and message fields.
_InternalServerError :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | An exception occurred because an invalid gateway request was issued to
-- the service. For more information, see the error and message fields.
_InvalidGatewayRequestException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidGatewayRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidGatewayRequestException"

-- | An internal server error has occurred because the service is
-- unavailable. For more information, see the error and message fields.
_ServiceUnavailableError :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceUnavailableError =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableError"

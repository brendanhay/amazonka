{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.StorageGateway.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidGatewayRequestException,
    _InternalServerError,
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
    cachediSCSIVolume_volumeUsedInBytes,
    cachediSCSIVolume_volumeProgress,
    cachediSCSIVolume_volumeSizeInBytes,
    cachediSCSIVolume_targetName,
    cachediSCSIVolume_volumeiSCSIAttributes,
    cachediSCSIVolume_volumeStatus,
    cachediSCSIVolume_volumeAttachmentStatus,
    cachediSCSIVolume_sourceSnapshotId,
    cachediSCSIVolume_kmsKey,
    cachediSCSIVolume_volumeType,
    cachediSCSIVolume_volumeARN,
    cachediSCSIVolume_createdDate,
    cachediSCSIVolume_volumeId,

    -- * ChapInfo
    ChapInfo (..),
    newChapInfo,
    chapInfo_initiatorName,
    chapInfo_secretToAuthenticateTarget,
    chapInfo_targetARN,
    chapInfo_secretToAuthenticateInitiator,

    -- * DeviceiSCSIAttributes
    DeviceiSCSIAttributes (..),
    newDeviceiSCSIAttributes,
    deviceiSCSIAttributes_networkInterfacePort,
    deviceiSCSIAttributes_targetARN,
    deviceiSCSIAttributes_chapEnabled,
    deviceiSCSIAttributes_networkInterfaceId,

    -- * Disk
    Disk (..),
    newDisk,
    disk_diskStatus,
    disk_diskNode,
    disk_diskId,
    disk_diskAllocationType,
    disk_diskPath,
    disk_diskSizeInBytes,
    disk_diskAttributeList,
    disk_diskAllocationResource,

    -- * EndpointNetworkConfiguration
    EndpointNetworkConfiguration (..),
    newEndpointNetworkConfiguration,
    endpointNetworkConfiguration_ipAddresses,

    -- * FileShareInfo
    FileShareInfo (..),
    newFileShareInfo,
    fileShareInfo_fileShareStatus,
    fileShareInfo_fileShareId,
    fileShareInfo_fileShareARN,
    fileShareInfo_gatewayARN,
    fileShareInfo_fileShareType,

    -- * FileSystemAssociationInfo
    FileSystemAssociationInfo (..),
    newFileSystemAssociationInfo,
    fileSystemAssociationInfo_tags,
    fileSystemAssociationInfo_fileSystemAssociationStatusDetails,
    fileSystemAssociationInfo_endpointNetworkConfiguration,
    fileSystemAssociationInfo_locationARN,
    fileSystemAssociationInfo_gatewayARN,
    fileSystemAssociationInfo_fileSystemAssociationStatus,
    fileSystemAssociationInfo_cacheAttributes,
    fileSystemAssociationInfo_auditDestinationARN,
    fileSystemAssociationInfo_fileSystemAssociationARN,

    -- * FileSystemAssociationStatusDetail
    FileSystemAssociationStatusDetail (..),
    newFileSystemAssociationStatusDetail,
    fileSystemAssociationStatusDetail_errorCode,

    -- * FileSystemAssociationSummary
    FileSystemAssociationSummary (..),
    newFileSystemAssociationSummary,
    fileSystemAssociationSummary_fileSystemAssociationId,
    fileSystemAssociationSummary_gatewayARN,
    fileSystemAssociationSummary_fileSystemAssociationStatus,
    fileSystemAssociationSummary_fileSystemAssociationARN,

    -- * GatewayInfo
    GatewayInfo (..),
    newGatewayInfo,
    gatewayInfo_ec2InstanceId,
    gatewayInfo_gatewayName,
    gatewayInfo_gatewayType,
    gatewayInfo_gatewayARN,
    gatewayInfo_gatewayOperationalState,
    gatewayInfo_gatewayId,
    gatewayInfo_ec2InstanceRegion,
    gatewayInfo_hostEnvironmentId,
    gatewayInfo_hostEnvironment,

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
    nFSFileShareInfo_tags,
    nFSFileShareInfo_squash,
    nFSFileShareInfo_nFSFileShareDefaults,
    nFSFileShareInfo_fileShareStatus,
    nFSFileShareInfo_fileShareId,
    nFSFileShareInfo_fileShareName,
    nFSFileShareInfo_requesterPays,
    nFSFileShareInfo_objectACL,
    nFSFileShareInfo_kmsKey,
    nFSFileShareInfo_locationARN,
    nFSFileShareInfo_path,
    nFSFileShareInfo_fileShareARN,
    nFSFileShareInfo_vPCEndpointDNSName,
    nFSFileShareInfo_gatewayARN,
    nFSFileShareInfo_kmsEncrypted,
    nFSFileShareInfo_defaultStorageClass,
    nFSFileShareInfo_cacheAttributes,
    nFSFileShareInfo_readOnly,
    nFSFileShareInfo_bucketRegion,
    nFSFileShareInfo_role,
    nFSFileShareInfo_auditDestinationARN,
    nFSFileShareInfo_clientList,
    nFSFileShareInfo_guessMIMETypeEnabled,
    nFSFileShareInfo_notificationPolicy,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_ipv6Address,
    networkInterface_macAddress,
    networkInterface_ipv4Address,

    -- * PoolInfo
    PoolInfo (..),
    newPoolInfo,
    poolInfo_poolARN,
    poolInfo_retentionLockTimeInDays,
    poolInfo_retentionLockType,
    poolInfo_poolStatus,
    poolInfo_poolName,
    poolInfo_storageClass,

    -- * SMBFileShareInfo
    SMBFileShareInfo (..),
    newSMBFileShareInfo,
    sMBFileShareInfo_tags,
    sMBFileShareInfo_validUserList,
    sMBFileShareInfo_authentication,
    sMBFileShareInfo_fileShareStatus,
    sMBFileShareInfo_fileShareId,
    sMBFileShareInfo_fileShareName,
    sMBFileShareInfo_requesterPays,
    sMBFileShareInfo_objectACL,
    sMBFileShareInfo_caseSensitivity,
    sMBFileShareInfo_kmsKey,
    sMBFileShareInfo_sMBACLEnabled,
    sMBFileShareInfo_oplocksEnabled,
    sMBFileShareInfo_locationARN,
    sMBFileShareInfo_path,
    sMBFileShareInfo_fileShareARN,
    sMBFileShareInfo_vPCEndpointDNSName,
    sMBFileShareInfo_accessBasedEnumeration,
    sMBFileShareInfo_gatewayARN,
    sMBFileShareInfo_invalidUserList,
    sMBFileShareInfo_adminUserList,
    sMBFileShareInfo_kmsEncrypted,
    sMBFileShareInfo_defaultStorageClass,
    sMBFileShareInfo_cacheAttributes,
    sMBFileShareInfo_readOnly,
    sMBFileShareInfo_bucketRegion,
    sMBFileShareInfo_role,
    sMBFileShareInfo_auditDestinationARN,
    sMBFileShareInfo_guessMIMETypeEnabled,
    sMBFileShareInfo_notificationPolicy,

    -- * SMBLocalGroups
    SMBLocalGroups (..),
    newSMBLocalGroups,
    sMBLocalGroups_gatewayAdmins,

    -- * StorediSCSIVolume
    StorediSCSIVolume (..),
    newStorediSCSIVolume,
    storediSCSIVolume_volumeUsedInBytes,
    storediSCSIVolume_volumeProgress,
    storediSCSIVolume_volumeSizeInBytes,
    storediSCSIVolume_targetName,
    storediSCSIVolume_volumeiSCSIAttributes,
    storediSCSIVolume_volumeStatus,
    storediSCSIVolume_volumeAttachmentStatus,
    storediSCSIVolume_preservedExistingData,
    storediSCSIVolume_sourceSnapshotId,
    storediSCSIVolume_volumeDiskId,
    storediSCSIVolume_kmsKey,
    storediSCSIVolume_volumeType,
    storediSCSIVolume_volumeARN,
    storediSCSIVolume_createdDate,
    storediSCSIVolume_volumeId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Tape
    Tape (..),
    newTape,
    tape_tapeBarcode,
    tape_progress,
    tape_tapeStatus,
    tape_worm,
    tape_tapeUsedInBytes,
    tape_vTLDevice,
    tape_retentionStartDate,
    tape_poolEntryDate,
    tape_kmsKey,
    tape_tapeSizeInBytes,
    tape_poolId,
    tape_tapeCreatedDate,
    tape_tapeARN,

    -- * TapeArchive
    TapeArchive (..),
    newTapeArchive,
    tapeArchive_tapeBarcode,
    tapeArchive_tapeStatus,
    tapeArchive_worm,
    tapeArchive_tapeUsedInBytes,
    tapeArchive_retentionStartDate,
    tapeArchive_poolEntryDate,
    tapeArchive_kmsKey,
    tapeArchive_completionTime,
    tapeArchive_tapeSizeInBytes,
    tapeArchive_poolId,
    tapeArchive_tapeCreatedDate,
    tapeArchive_retrievedTo,
    tapeArchive_tapeARN,

    -- * TapeInfo
    TapeInfo (..),
    newTapeInfo,
    tapeInfo_tapeBarcode,
    tapeInfo_tapeStatus,
    tapeInfo_retentionStartDate,
    tapeInfo_poolEntryDate,
    tapeInfo_gatewayARN,
    tapeInfo_tapeSizeInBytes,
    tapeInfo_poolId,
    tapeInfo_tapeARN,

    -- * TapeRecoveryPointInfo
    TapeRecoveryPointInfo (..),
    newTapeRecoveryPointInfo,
    tapeRecoveryPointInfo_tapeStatus,
    tapeRecoveryPointInfo_tapeSizeInBytes,
    tapeRecoveryPointInfo_tapeRecoveryPointTime,
    tapeRecoveryPointInfo_tapeARN,

    -- * VTLDevice
    VTLDevice (..),
    newVTLDevice,
    vTLDevice_vTLDeviceVendor,
    vTLDevice_vTLDeviceType,
    vTLDevice_vTLDeviceProductIdentifier,
    vTLDevice_deviceiSCSIAttributes,
    vTLDevice_vTLDeviceARN,

    -- * VolumeInfo
    VolumeInfo (..),
    newVolumeInfo,
    volumeInfo_volumeSizeInBytes,
    volumeInfo_volumeAttachmentStatus,
    volumeInfo_volumeType,
    volumeInfo_volumeARN,
    volumeInfo_gatewayARN,
    volumeInfo_gatewayId,
    volumeInfo_volumeId,

    -- * VolumeRecoveryPointInfo
    VolumeRecoveryPointInfo (..),
    newVolumeRecoveryPointInfo,
    volumeRecoveryPointInfo_volumeSizeInBytes,
    volumeRecoveryPointInfo_volumeUsageInBytes,
    volumeRecoveryPointInfo_volumeARN,
    volumeRecoveryPointInfo_volumeRecoveryPointTime,

    -- * VolumeiSCSIAttributes
    VolumeiSCSIAttributes (..),
    newVolumeiSCSIAttributes,
    volumeiSCSIAttributes_networkInterfacePort,
    volumeiSCSIAttributes_lunNumber,
    volumeiSCSIAttributes_targetARN,
    volumeiSCSIAttributes_chapEnabled,
    volumeiSCSIAttributes_networkInterfaceId,
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | An exception occurred because an invalid gateway request was issued to
-- the service. For more information, see the error and message fields.
_InvalidGatewayRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGatewayRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidGatewayRequestException"

-- | An internal server error has occurred during the request. For more
-- information, see the error and message fields.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | An internal server error has occurred because the service is
-- unavailable. For more information, see the error and message fields.
_ServiceUnavailableError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableError =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableError"

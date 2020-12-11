-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types
  ( -- * Service configuration
    storageGatewayService,

    -- * Errors

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
    mkAutomaticTapeCreationPolicyInfo,
    atcpiGatewayARN,
    atcpiAutomaticTapeCreationRules,

    -- * AutomaticTapeCreationRule
    AutomaticTapeCreationRule (..),
    mkAutomaticTapeCreationRule,
    atcrWorm,
    atcrTapeBarcodePrefix,
    atcrPoolId,
    atcrTapeSizeInBytes,
    atcrMinimumNumTapes,

    -- * BandwidthRateLimitInterval
    BandwidthRateLimitInterval (..),
    mkBandwidthRateLimitInterval,
    brliAverageUploadRateLimitInBitsPerSec,
    brliAverageDownloadRateLimitInBitsPerSec,
    brliStartHourOfDay,
    brliStartMinuteOfHour,
    brliEndHourOfDay,
    brliEndMinuteOfHour,
    brliDaysOfWeek,

    -- * CacheAttributes
    CacheAttributes (..),
    mkCacheAttributes,
    caCacheStaleTimeoutInSeconds,

    -- * CachediSCSIVolume
    CachediSCSIVolume (..),
    mkCachediSCSIVolume,
    cscsivVolumeiSCSIAttributes,
    cscsivVolumeStatus,
    cscsivSourceSnapshotId,
    cscsivKMSKey,
    cscsivVolumeAttachmentStatus,
    cscsivVolumeARN,
    cscsivVolumeProgress,
    cscsivVolumeSizeInBytes,
    cscsivVolumeUsedInBytes,
    cscsivCreatedDate,
    cscsivVolumeId,
    cscsivVolumeType,
    cscsivTargetName,

    -- * ChapInfo
    ChapInfo (..),
    mkChapInfo,
    ciTargetARN,
    ciSecretToAuthenticateInitiator,
    ciInitiatorName,
    ciSecretToAuthenticateTarget,

    -- * DeviceiSCSIAttributes
    DeviceiSCSIAttributes (..),
    mkDeviceiSCSIAttributes,
    dscsiaTargetARN,
    dscsiaChapEnabled,
    dscsiaNetworkInterfaceId,
    dscsiaNetworkInterfacePort,

    -- * Disk
    Disk (..),
    mkDisk,
    dDiskAllocationResource,
    dDiskAllocationType,
    dDiskNode,
    dDiskPath,
    dDiskSizeInBytes,
    dDiskStatus,
    dDiskId,
    dDiskAttributeList,

    -- * FileShareInfo
    FileShareInfo (..),
    mkFileShareInfo,
    fsiFileShareStatus,
    fsiGatewayARN,
    fsiFileShareId,
    fsiFileShareARN,
    fsiFileShareType,

    -- * GatewayInfo
    GatewayInfo (..),
    mkGatewayInfo,
    giEC2InstanceRegion,
    giGatewayARN,
    giEC2InstanceId,
    giGatewayOperationalState,
    giGatewayName,
    giGatewayId,
    giGatewayType,

    -- * NFSFileShareDefaults
    NFSFileShareDefaults (..),
    mkNFSFileShareDefaults,
    nfsfsdFileMode,
    nfsfsdOwnerId,
    nfsfsdDirectoryMode,
    nfsfsdGroupId,

    -- * NFSFileShareInfo
    NFSFileShareInfo (..),
    mkNFSFileShareInfo,
    nfsfsiFileShareStatus,
    nfsfsiKMSKey,
    nfsfsiGatewayARN,
    nfsfsiPath,
    nfsfsiCacheAttributes,
    nfsfsiObjectACL,
    nfsfsiKMSEncrypted,
    nfsfsiFileShareId,
    nfsfsiFileShareARN,
    nfsfsiDefaultStorageClass,
    nfsfsiFileShareName,
    nfsfsiRole,
    nfsfsiNotificationPolicy,
    nfsfsiSquash,
    nfsfsiRequesterPays,
    nfsfsiNFSFileShareDefaults,
    nfsfsiLocationARN,
    nfsfsiClientList,
    nfsfsiGuessMIMETypeEnabled,
    nfsfsiReadOnly,
    nfsfsiTags,

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niIPv6Address,
    niMACAddress,
    niIPv4Address,

    -- * PoolInfo
    PoolInfo (..),
    mkPoolInfo,
    piRetentionLockType,
    piRetentionLockTimeInDays,
    piPoolName,
    piStorageClass,
    piPoolStatus,
    piPoolARN,

    -- * SMBFileShareInfo
    SMBFileShareInfo (..),
    mkSMBFileShareInfo,
    smbfsiAccessBasedEnumeration,
    smbfsiAdminUserList,
    smbfsiAuditDestinationARN,
    smbfsiFileShareStatus,
    smbfsiInvalidUserList,
    smbfsiKMSKey,
    smbfsiValidUserList,
    smbfsiGatewayARN,
    smbfsiPath,
    smbfsiAuthentication,
    smbfsiCacheAttributes,
    smbfsiObjectACL,
    smbfsiKMSEncrypted,
    smbfsiFileShareId,
    smbfsiFileShareARN,
    smbfsiDefaultStorageClass,
    smbfsiFileShareName,
    smbfsiRole,
    smbfsiSMBACLEnabled,
    smbfsiNotificationPolicy,
    smbfsiRequesterPays,
    smbfsiLocationARN,
    smbfsiGuessMIMETypeEnabled,
    smbfsiReadOnly,
    smbfsiCaseSensitivity,
    smbfsiTags,

    -- * StorediSCSIVolume
    StorediSCSIVolume (..),
    mkStorediSCSIVolume,
    sscsivVolumeiSCSIAttributes,
    sscsivVolumeStatus,
    sscsivSourceSnapshotId,
    sscsivPreservedExistingData,
    sscsivKMSKey,
    sscsivVolumeAttachmentStatus,
    sscsivVolumeARN,
    sscsivVolumeProgress,
    sscsivVolumeSizeInBytes,
    sscsivVolumeUsedInBytes,
    sscsivCreatedDate,
    sscsivVolumeId,
    sscsivVolumeDiskId,
    sscsivVolumeType,
    sscsivTargetName,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * Tape
    Tape (..),
    mkTape,
    tTapeBarcode,
    tTapeStatus,
    tKMSKey,
    tTapeARN,
    tProgress,
    tTapeSizeInBytes,
    tVTLDevice,
    tPoolId,
    tTapeUsedInBytes,
    tTapeCreatedDate,
    tPoolEntryDate,
    tWorm,
    tRetentionStartDate,

    -- * TapeArchive
    TapeArchive (..),
    mkTapeArchive,
    taTapeBarcode,
    taTapeStatus,
    taKMSKey,
    taTapeARN,
    taTapeSizeInBytes,
    taCompletionTime,
    taPoolId,
    taTapeUsedInBytes,
    taTapeCreatedDate,
    taPoolEntryDate,
    taWorm,
    taRetentionStartDate,
    taRetrievedTo,

    -- * TapeInfo
    TapeInfo (..),
    mkTapeInfo,
    tiTapeBarcode,
    tiTapeStatus,
    tiTapeARN,
    tiGatewayARN,
    tiTapeSizeInBytes,
    tiPoolId,
    tiPoolEntryDate,
    tiRetentionStartDate,

    -- * TapeRecoveryPointInfo
    TapeRecoveryPointInfo (..),
    mkTapeRecoveryPointInfo,
    trpiTapeStatus,
    trpiTapeRecoveryPointTime,
    trpiTapeARN,
    trpiTapeSizeInBytes,

    -- * VTLDevice
    VTLDevice (..),
    mkVTLDevice,
    vtldDeviceiSCSIAttributes,
    vtldVTLDeviceVendor,
    vtldVTLDeviceARN,
    vtldVTLDeviceType,
    vtldVTLDeviceProductIdentifier,

    -- * VolumeInfo
    VolumeInfo (..),
    mkVolumeInfo,
    viGatewayARN,
    viVolumeAttachmentStatus,
    viVolumeARN,
    viVolumeSizeInBytes,
    viVolumeId,
    viGatewayId,
    viVolumeType,

    -- * VolumeRecoveryPointInfo
    VolumeRecoveryPointInfo (..),
    mkVolumeRecoveryPointInfo,
    vrpiVolumeRecoveryPointTime,
    vrpiVolumeARN,
    vrpiVolumeSizeInBytes,
    vrpiVolumeUsageInBytes,

    -- * VolumeiSCSIAttributes
    VolumeiSCSIAttributes (..),
    mkVolumeiSCSIAttributes,
    vscsiaLunNumber,
    vscsiaTargetARN,
    vscsiaChapEnabled,
    vscsiaNetworkInterfaceId,
    vscsiaNetworkInterfacePort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
storageGatewayService :: Lude.Service
storageGatewayService =
  Lude.Service
    { Lude._svcAbbrev = "StorageGateway",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "storagegateway",
      Lude._svcVersion = "2013-06-30",
      Lude._svcEndpoint = Lude.defaultEndpoint storageGatewayService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "StorageGateway",
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

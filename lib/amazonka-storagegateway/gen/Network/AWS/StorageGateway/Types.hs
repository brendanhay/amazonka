{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types
  ( -- * Service Configuration
    storageGateway,

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
    AutomaticTapeCreationPolicyInfo,
    automaticTapeCreationPolicyInfo,
    atcpiGatewayARN,
    atcpiAutomaticTapeCreationRules,

    -- * AutomaticTapeCreationRule
    AutomaticTapeCreationRule,
    automaticTapeCreationRule,
    atcrWorm,
    atcrTapeBarcodePrefix,
    atcrPoolId,
    atcrTapeSizeInBytes,
    atcrMinimumNumTapes,

    -- * BandwidthRateLimitInterval
    BandwidthRateLimitInterval,
    bandwidthRateLimitInterval,
    brliAverageUploadRateLimitInBitsPerSec,
    brliAverageDownloadRateLimitInBitsPerSec,
    brliStartHourOfDay,
    brliStartMinuteOfHour,
    brliEndHourOfDay,
    brliEndMinuteOfHour,
    brliDaysOfWeek,

    -- * CacheAttributes
    CacheAttributes,
    cacheAttributes,
    caCacheStaleTimeoutInSeconds,

    -- * CachediSCSIVolume
    CachediSCSIVolume,
    cachediSCSIVolume,
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
    ChapInfo,
    chapInfo,
    ciTargetARN,
    ciSecretToAuthenticateInitiator,
    ciInitiatorName,
    ciSecretToAuthenticateTarget,

    -- * DeviceiSCSIAttributes
    DeviceiSCSIAttributes,
    deviceiSCSIAttributes,
    dscsiaTargetARN,
    dscsiaChapEnabled,
    dscsiaNetworkInterfaceId,
    dscsiaNetworkInterfacePort,

    -- * Disk
    Disk,
    disk,
    dDiskAllocationResource,
    dDiskAllocationType,
    dDiskNode,
    dDiskPath,
    dDiskSizeInBytes,
    dDiskStatus,
    dDiskId,
    dDiskAttributeList,

    -- * FileShareInfo
    FileShareInfo,
    fileShareInfo,
    fsiFileShareStatus,
    fsiGatewayARN,
    fsiFileShareId,
    fsiFileShareARN,
    fsiFileShareType,

    -- * GatewayInfo
    GatewayInfo,
    gatewayInfo,
    giEC2InstanceRegion,
    giGatewayARN,
    giEC2InstanceId,
    giGatewayOperationalState,
    giGatewayName,
    giGatewayId,
    giGatewayType,

    -- * NFSFileShareDefaults
    NFSFileShareDefaults,
    nFSFileShareDefaults,
    nfsfsdFileMode,
    nfsfsdOwnerId,
    nfsfsdDirectoryMode,
    nfsfsdGroupId,

    -- * NFSFileShareInfo
    NFSFileShareInfo,
    nFSFileShareInfo,
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
    NetworkInterface,
    networkInterface,
    niIPv6Address,
    niMACAddress,
    niIPv4Address,

    -- * PoolInfo
    PoolInfo,
    poolInfo,
    piRetentionLockType,
    piRetentionLockTimeInDays,
    piPoolName,
    piStorageClass,
    piPoolStatus,
    piPoolARN,

    -- * SMBFileShareInfo
    SMBFileShareInfo,
    sMBFileShareInfo,
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
    StorediSCSIVolume,
    storediSCSIVolume,
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
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * Tape
    Tape,
    tape,
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
    TapeArchive,
    tapeArchive,
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
    TapeInfo,
    tapeInfo,
    tiTapeBarcode,
    tiTapeStatus,
    tiTapeARN,
    tiGatewayARN,
    tiTapeSizeInBytes,
    tiPoolId,
    tiPoolEntryDate,
    tiRetentionStartDate,

    -- * TapeRecoveryPointInfo
    TapeRecoveryPointInfo,
    tapeRecoveryPointInfo,
    trpiTapeStatus,
    trpiTapeRecoveryPointTime,
    trpiTapeARN,
    trpiTapeSizeInBytes,

    -- * VTLDevice
    VTLDevice,
    vTLDevice,
    vtldDeviceiSCSIAttributes,
    vtldVTLDeviceVendor,
    vtldVTLDeviceARN,
    vtldVTLDeviceType,
    vtldVTLDeviceProductIdentifier,

    -- * VolumeInfo
    VolumeInfo,
    volumeInfo,
    viGatewayARN,
    viVolumeAttachmentStatus,
    viVolumeARN,
    viVolumeSizeInBytes,
    viVolumeId,
    viGatewayId,
    viVolumeType,

    -- * VolumeRecoveryPointInfo
    VolumeRecoveryPointInfo,
    volumeRecoveryPointInfo,
    vrpiVolumeRecoveryPointTime,
    vrpiVolumeARN,
    vrpiVolumeSizeInBytes,
    vrpiVolumeUsageInBytes,

    -- * VolumeiSCSIAttributes
    VolumeiSCSIAttributes,
    volumeiSCSIAttributes,
    vscsiaLunNumber,
    vscsiaTargetARN,
    vscsiaChapEnabled,
    vscsiaNetworkInterfaceId,
    vscsiaNetworkInterfacePort,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
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
storageGateway :: Service
storageGateway =
  Service
    { _svcAbbrev = "StorageGateway",
      _svcSigner = v4,
      _svcPrefix = "storagegateway",
      _svcVersion = "2013-06-30",
      _svcEndpoint = defaultEndpoint storageGateway,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "StorageGateway",
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

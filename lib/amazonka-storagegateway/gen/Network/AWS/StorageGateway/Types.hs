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
    mkServiceConfig,

    -- * Errors
    _InvalidGatewayRequestException,
    _ServiceUnavailableError,
    _InternalServerError,

    -- * TapeBarcodePrefix
    TapeBarcodePrefix (..),

    -- * ChapInfo
    ChapInfo (..),
    mkChapInfo,
    ciInitiatorName,
    ciSecretToAuthenticateInitiator,
    ciSecretToAuthenticateTarget,
    ciTargetARN,

    -- * VolumeiSCSIAttributes
    VolumeiSCSIAttributes (..),
    mkVolumeiSCSIAttributes,
    vscsiaChapEnabled,
    vscsiaLunNumber,
    vscsiaNetworkInterfaceId,
    vscsiaNetworkInterfacePort,
    vscsiaTargetARN,

    -- * TapeBarcode
    TapeBarcode (..),

    -- * DeviceiSCSIAttributes
    DeviceiSCSIAttributes (..),
    mkDeviceiSCSIAttributes,
    dscsiaChapEnabled,
    dscsiaNetworkInterfaceId,
    dscsiaNetworkInterfacePort,
    dscsiaTargetARN,

    -- * TargetARN
    TargetARN (..),

    -- * VolumeStatus
    VolumeStatus (..),

    -- * TapeStatus
    TapeStatus (..),

    -- * AuditDestinationARN
    AuditDestinationARN (..),

    -- * FileShareStatus
    FileShareStatus (..),

    -- * RetentionLockType
    RetentionLockType (..),

    -- * BandwidthType
    BandwidthType (..),

    -- * GatewayState
    GatewayState (..),

    -- * SnapshotDescription
    SnapshotDescription (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * FileShareUser
    FileShareUser (..),

    -- * KMSKey
    KMSKey (..),

    -- * ClientToken
    ClientToken (..),

    -- * TapeARN
    TapeARN (..),

    -- * Disk
    Disk (..),
    mkDisk,
    dDiskAllocationResource,
    dDiskAllocationType,
    dDiskAttributeList,
    dDiskId,
    dDiskNode,
    dDiskPath,
    dDiskSizeInBytes,
    dDiskStatus,

    -- * Ec2InstanceRegion
    Ec2InstanceRegion (..),

    -- * RegionId
    RegionId (..),

    -- * LocalConsolePassword
    LocalConsolePassword (..),

    -- * FileShareInfo
    FileShareInfo (..),
    mkFileShareInfo,
    fsiFileShareARN,
    fsiFileShareId,
    fsiFileShareStatus,
    fsiFileShareType,
    fsiGatewayARN,

    -- * GatewayARN
    GatewayARN (..),

    -- * DiskAllocationType
    DiskAllocationType (..),

    -- * Path
    Path (..),

    -- * VolumeAttachmentStatus
    VolumeAttachmentStatus (..),

    -- * VolumeARN
    VolumeARN (..),

    -- * String
    String (..),

    -- * OrganizationalUnit
    OrganizationalUnit (..),

    -- * Ec2InstanceId
    Ec2InstanceId (..),

    -- * Authentication
    Authentication (..),

    -- * Tape
    Tape (..),
    mkTape,
    tKMSKey,
    tPoolEntryDate,
    tPoolId,
    tProgress,
    tRetentionStartDate,
    tTapeARN,
    tTapeBarcode,
    tTapeCreatedDate,
    tTapeSizeInBytes,
    tTapeStatus,
    tTapeUsedInBytes,
    tVTLDevice,
    tWorm,

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niIpv4Address,
    niIpv6Address,
    niMacAddress,

    -- * PoolName
    PoolName (..),

    -- * ActivationKey
    ActivationKey (..),

    -- * CacheAttributes
    CacheAttributes (..),
    mkCacheAttributes,
    caCacheStaleTimeoutInSeconds,

    -- * ObjectACL
    ObjectACL (..),

    -- * NextUpdateAvailabilityDate
    NextUpdateAvailabilityDate (..),

    -- * TapeRecoveryPointStatus
    TapeRecoveryPointStatus (..),

    -- * EndpointType
    EndpointType (..),

    -- * Initiator
    Initiator (..),

    -- * VTLDevice
    VTLDevice (..),
    mkVTLDevice,
    vtldDeviceiSCSIAttributes,
    vtldVTLDeviceARN,
    vtldVTLDeviceProductIdentifier,
    vtldVTLDeviceType,
    vtldVTLDeviceVendor,

    -- * NFSFileShareInfo
    NFSFileShareInfo (..),
    mkNFSFileShareInfo,
    nfsfsiCacheAttributes,
    nfsfsiClientList,
    nfsfsiDefaultStorageClass,
    nfsfsiFileShareARN,
    nfsfsiFileShareId,
    nfsfsiFileShareName,
    nfsfsiFileShareStatus,
    nfsfsiGatewayARN,
    nfsfsiGuessMIMETypeEnabled,
    nfsfsiKMSEncrypted,
    nfsfsiKMSKey,
    nfsfsiLocationARN,
    nfsfsiNFSFileShareDefaults,
    nfsfsiNotificationPolicy,
    nfsfsiObjectACL,
    nfsfsiPath,
    nfsfsiReadOnly,
    nfsfsiRequesterPays,
    nfsfsiRole,
    nfsfsiSquash,
    nfsfsiTags,

    -- * NetworkInterfaceId
    NetworkInterfaceId (..),

    -- * DomainUserPassword
    DomainUserPassword (..),

    -- * FileShareId
    FileShareId (..),

    -- * TapeArchiveStatus
    TapeArchiveStatus (..),

    -- * FileShareARN
    FileShareARN (..),

    -- * DomainUserName
    DomainUserName (..),

    -- * Folder
    Folder (..),

    -- * TapeRecoveryPointInfo
    TapeRecoveryPointInfo (..),
    mkTapeRecoveryPointInfo,
    trpiTapeARN,
    trpiTapeRecoveryPointTime,
    trpiTapeSizeInBytes,
    trpiTapeStatus,

    -- * IPV4AddressCIDR
    IPV4AddressCIDR (..),

    -- * IqnName
    IqnName (..),

    -- * ActiveDirectoryStatus
    ActiveDirectoryStatus (..),

    -- * PoolId
    PoolId (..),

    -- * VolumeRecoveryPointInfo
    VolumeRecoveryPointInfo (..),
    mkVolumeRecoveryPointInfo,
    vrpiVolumeARN,
    vrpiVolumeRecoveryPointTime,
    vrpiVolumeSizeInBytes,
    vrpiVolumeUsageInBytes,

    -- * MediumChangerType
    MediumChangerType (..),

    -- * TapeDriveType
    TapeDriveType (..),

    -- * DeprecationDate
    DeprecationDate (..),

    -- * LastSoftwareUpdate
    LastSoftwareUpdate (..),

    -- * FileShareName
    FileShareName (..),

    -- * Role
    Role (..),

    -- * TapeStorageClass
    TapeStorageClass (..),

    -- * GatewayOperationalState
    GatewayOperationalState (..),

    -- * ResourceARN
    ResourceARN (..),

    -- * BandwidthRateLimitInterval
    BandwidthRateLimitInterval (..),
    mkBandwidthRateLimitInterval,
    brliStartHourOfDay,
    brliStartMinuteOfHour,
    brliEndHourOfDay,
    brliEndMinuteOfHour,
    brliDaysOfWeek,
    brliAverageDownloadRateLimitInBitsPerSec,
    brliAverageUploadRateLimitInBitsPerSec,

    -- * AvailabilityMonitorTestStatus
    AvailabilityMonitorTestStatus (..),

    -- * DomainName
    DomainName (..),

    -- * GatewayName
    GatewayName (..),

    -- * StorageClass
    StorageClass (..),

    -- * AutomaticTapeCreationPolicyInfo
    AutomaticTapeCreationPolicyInfo (..),
    mkAutomaticTapeCreationPolicyInfo,
    atcpiAutomaticTapeCreationRules,
    atcpiGatewayARN,

    -- * Marker
    Marker (..),

    -- * TapeArchive
    TapeArchive (..),
    mkTapeArchive,
    taCompletionTime,
    taKMSKey,
    taPoolEntryDate,
    taPoolId,
    taRetentionStartDate,
    taRetrievedTo,
    taTapeARN,
    taTapeBarcode,
    taTapeCreatedDate,
    taTapeSizeInBytes,
    taTapeStatus,
    taTapeUsedInBytes,
    taWorm,

    -- * VTLDeviceVendor
    VTLDeviceVendor (..),

    -- * PoolInfo
    PoolInfo (..),
    mkPoolInfo,
    piPoolARN,
    piPoolName,
    piPoolStatus,
    piRetentionLockTimeInDays,
    piRetentionLockType,
    piStorageClass,

    -- * AutomaticTapeCreationRule
    AutomaticTapeCreationRule (..),
    mkAutomaticTapeCreationRule,
    atcrTapeBarcodePrefix,
    atcrPoolId,
    atcrTapeSizeInBytes,
    atcrMinimumNumTapes,
    atcrWorm,

    -- * NotificationPolicy
    NotificationPolicy (..),

    -- * VolumeId
    VolumeId (..),

    -- * Squash
    Squash (..),

    -- * GatewayId
    GatewayId (..),

    -- * HostEnvironment
    HostEnvironment (..),

    -- * TagKey
    TagKey (..),

    -- * DeviceType
    DeviceType (..),

    -- * StorediSCSIVolume
    StorediSCSIVolume (..),
    mkStorediSCSIVolume,
    sscsivCreatedDate,
    sscsivKMSKey,
    sscsivPreservedExistingData,
    sscsivSourceSnapshotId,
    sscsivTargetName,
    sscsivVolumeARN,
    sscsivVolumeAttachmentStatus,
    sscsivVolumeDiskId,
    sscsivVolumeId,
    sscsivVolumeProgress,
    sscsivVolumeSizeInBytes,
    sscsivVolumeStatus,
    sscsivVolumeType,
    sscsivVolumeUsedInBytes,
    sscsivVolumeiSCSIAttributes,

    -- * PoolStatus
    PoolStatus (..),

    -- * NFSFileShareDefaults
    NFSFileShareDefaults (..),
    mkNFSFileShareDefaults,
    nfsfsdDirectoryMode,
    nfsfsdFileMode,
    nfsfsdGroupId,
    nfsfsdOwnerId,

    -- * LocationARN
    LocationARN (..),

    -- * Host
    Host (..),

    -- * DiskId
    DiskId (..),

    -- * VolumeType
    VolumeType (..),

    -- * NotificationId
    NotificationId (..),

    -- * GatewayType
    GatewayType (..),

    -- * CachediSCSIVolume
    CachediSCSIVolume (..),
    mkCachediSCSIVolume,
    cscsivCreatedDate,
    cscsivKMSKey,
    cscsivSourceSnapshotId,
    cscsivTargetName,
    cscsivVolumeARN,
    cscsivVolumeAttachmentStatus,
    cscsivVolumeId,
    cscsivVolumeProgress,
    cscsivVolumeSizeInBytes,
    cscsivVolumeStatus,
    cscsivVolumeType,
    cscsivVolumeUsedInBytes,
    cscsivVolumeiSCSIAttributes,

    -- * FileShareType
    FileShareType (..),

    -- * GatewayTimezone
    GatewayTimezone (..),

    -- * VTLDeviceARN
    VTLDeviceARN (..),

    -- * VTLDeviceType
    VTLDeviceType (..),

    -- * VolumeInfo
    VolumeInfo (..),
    mkVolumeInfo,
    viGatewayARN,
    viGatewayId,
    viVolumeARN,
    viVolumeAttachmentStatus,
    viVolumeId,
    viVolumeSizeInBytes,
    viVolumeType,

    -- * SoftwareUpdatesEndDate
    SoftwareUpdatesEndDate (..),

    -- * Description
    Description (..),

    -- * TapeInfo
    TapeInfo (..),
    mkTapeInfo,
    tiGatewayARN,
    tiPoolEntryDate,
    tiPoolId,
    tiRetentionStartDate,
    tiTapeARN,
    tiTapeBarcode,
    tiTapeSizeInBytes,
    tiTapeStatus,

    -- * CloudWatchLogGroupARN
    CloudWatchLogGroupARN (..),

    -- * GatewayInfo
    GatewayInfo (..),
    mkGatewayInfo,
    giEc2InstanceId,
    giEc2InstanceRegion,
    giGatewayARN,
    giGatewayId,
    giGatewayName,
    giGatewayOperationalState,
    giGatewayType,

    -- * TargetName
    TargetName (..),

    -- * VTLDeviceProductIdentifier
    VTLDeviceProductIdentifier (..),

    -- * CaseSensitivity
    CaseSensitivity (..),

    -- * SMBFileShareInfo
    SMBFileShareInfo (..),
    mkSMBFileShareInfo,
    smbfsiAccessBasedEnumeration,
    smbfsiAdminUserList,
    smbfsiAuditDestinationARN,
    smbfsiAuthentication,
    smbfsiCacheAttributes,
    smbfsiCaseSensitivity,
    smbfsiDefaultStorageClass,
    smbfsiFileShareARN,
    smbfsiFileShareId,
    smbfsiFileShareName,
    smbfsiFileShareStatus,
    smbfsiGatewayARN,
    smbfsiGuessMIMETypeEnabled,
    smbfsiInvalidUserList,
    smbfsiKMSEncrypted,
    smbfsiKMSKey,
    smbfsiLocationARN,
    smbfsiNotificationPolicy,
    smbfsiObjectACL,
    smbfsiPath,
    smbfsiReadOnly,
    smbfsiRequesterPays,
    smbfsiRole,
    smbfsiSMBACLEnabled,
    smbfsiTags,
    smbfsiValidUserList,

    -- * DiskAttribute
    DiskAttribute (..),

    -- * PoolARN
    PoolARN (..),

    -- * SnapshotId
    SnapshotId (..),

    -- * SMBSecurityStrategy
    SMBSecurityStrategy (..),

    -- * InitiatorName
    InitiatorName (..),

    -- * SecretToAuthenticateInitiator
    SecretToAuthenticateInitiator (..),

    -- * SecretToAuthenticateTarget
    SecretToAuthenticateTarget (..),

    -- * DefaultStorageClass
    DefaultStorageClass (..),

    -- * Password
    Password (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * DiskAllocationResource
    DiskAllocationResource (..),

    -- * DiskNode
    DiskNode (..),

    -- * DiskPath
    DiskPath (..),

    -- * DiskStatus
    DiskStatus (..),

    -- * Timezone
    Timezone (..),

    -- * SourceSnapshotId
    SourceSnapshotId (..),

    -- * VolumeDiskId
    VolumeDiskId (..),

    -- * DirectoryMode
    DirectoryMode (..),

    -- * FileMode
    FileMode (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.StorageGateway.Types.ActivationKey
import Network.AWS.StorageGateway.Types.ActiveDirectoryStatus
import Network.AWS.StorageGateway.Types.AuditDestinationARN
import Network.AWS.StorageGateway.Types.Authentication
import Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
import Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule
import Network.AWS.StorageGateway.Types.AvailabilityMonitorTestStatus
import Network.AWS.StorageGateway.Types.BandwidthRateLimitInterval
import Network.AWS.StorageGateway.Types.BandwidthType
import Network.AWS.StorageGateway.Types.CacheAttributes
import Network.AWS.StorageGateway.Types.CachediSCSIVolume
import Network.AWS.StorageGateway.Types.CaseSensitivity
import Network.AWS.StorageGateway.Types.ChapInfo
import Network.AWS.StorageGateway.Types.ClientToken
import Network.AWS.StorageGateway.Types.CloudWatchLogGroupARN
import Network.AWS.StorageGateway.Types.DefaultStorageClass
import Network.AWS.StorageGateway.Types.DeprecationDate
import Network.AWS.StorageGateway.Types.Description
import Network.AWS.StorageGateway.Types.DeviceType
import Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes
import Network.AWS.StorageGateway.Types.DirectoryMode
import Network.AWS.StorageGateway.Types.Disk
import Network.AWS.StorageGateway.Types.DiskAllocationResource
import Network.AWS.StorageGateway.Types.DiskAllocationType
import Network.AWS.StorageGateway.Types.DiskAttribute
import Network.AWS.StorageGateway.Types.DiskId
import Network.AWS.StorageGateway.Types.DiskNode
import Network.AWS.StorageGateway.Types.DiskPath
import Network.AWS.StorageGateway.Types.DiskStatus
import Network.AWS.StorageGateway.Types.DomainName
import Network.AWS.StorageGateway.Types.DomainUserName
import Network.AWS.StorageGateway.Types.DomainUserPassword
import Network.AWS.StorageGateway.Types.Ec2InstanceId
import Network.AWS.StorageGateway.Types.Ec2InstanceRegion
import Network.AWS.StorageGateway.Types.EndpointType
import Network.AWS.StorageGateway.Types.FileMode
import Network.AWS.StorageGateway.Types.FileShareARN
import Network.AWS.StorageGateway.Types.FileShareId
import Network.AWS.StorageGateway.Types.FileShareInfo
import Network.AWS.StorageGateway.Types.FileShareName
import Network.AWS.StorageGateway.Types.FileShareStatus
import Network.AWS.StorageGateway.Types.FileShareType
import Network.AWS.StorageGateway.Types.FileShareUser
import Network.AWS.StorageGateway.Types.Folder
import Network.AWS.StorageGateway.Types.GatewayARN
import Network.AWS.StorageGateway.Types.GatewayId
import Network.AWS.StorageGateway.Types.GatewayInfo
import Network.AWS.StorageGateway.Types.GatewayName
import Network.AWS.StorageGateway.Types.GatewayOperationalState
import Network.AWS.StorageGateway.Types.GatewayState
import Network.AWS.StorageGateway.Types.GatewayTimezone
import Network.AWS.StorageGateway.Types.GatewayType
import Network.AWS.StorageGateway.Types.Host
import Network.AWS.StorageGateway.Types.HostEnvironment
import Network.AWS.StorageGateway.Types.IPV4AddressCIDR
import Network.AWS.StorageGateway.Types.Initiator
import Network.AWS.StorageGateway.Types.InitiatorName
import Network.AWS.StorageGateway.Types.IqnName
import Network.AWS.StorageGateway.Types.KMSKey
import Network.AWS.StorageGateway.Types.Key
import Network.AWS.StorageGateway.Types.LastSoftwareUpdate
import Network.AWS.StorageGateway.Types.LocalConsolePassword
import Network.AWS.StorageGateway.Types.LocationARN
import Network.AWS.StorageGateway.Types.Marker
import Network.AWS.StorageGateway.Types.MediumChangerType
import Network.AWS.StorageGateway.Types.NFSFileShareDefaults
import Network.AWS.StorageGateway.Types.NFSFileShareInfo
import Network.AWS.StorageGateway.Types.NetworkInterface
import Network.AWS.StorageGateway.Types.NetworkInterfaceId
import Network.AWS.StorageGateway.Types.NextUpdateAvailabilityDate
import Network.AWS.StorageGateway.Types.NotificationId
import Network.AWS.StorageGateway.Types.NotificationPolicy
import Network.AWS.StorageGateway.Types.ObjectACL
import Network.AWS.StorageGateway.Types.OrganizationalUnit
import Network.AWS.StorageGateway.Types.Password
import Network.AWS.StorageGateway.Types.Path
import Network.AWS.StorageGateway.Types.PoolARN
import Network.AWS.StorageGateway.Types.PoolId
import Network.AWS.StorageGateway.Types.PoolInfo
import Network.AWS.StorageGateway.Types.PoolName
import Network.AWS.StorageGateway.Types.PoolStatus
import Network.AWS.StorageGateway.Types.RegionId
import Network.AWS.StorageGateway.Types.ResourceARN
import Network.AWS.StorageGateway.Types.RetentionLockType
import Network.AWS.StorageGateway.Types.Role
import Network.AWS.StorageGateway.Types.SMBFileShareInfo
import Network.AWS.StorageGateway.Types.SMBSecurityStrategy
import Network.AWS.StorageGateway.Types.SecretToAuthenticateInitiator
import Network.AWS.StorageGateway.Types.SecretToAuthenticateTarget
import Network.AWS.StorageGateway.Types.SnapshotDescription
import Network.AWS.StorageGateway.Types.SnapshotId
import Network.AWS.StorageGateway.Types.SoftwareUpdatesEndDate
import Network.AWS.StorageGateway.Types.SourceSnapshotId
import Network.AWS.StorageGateway.Types.Squash
import Network.AWS.StorageGateway.Types.StorageClass
import Network.AWS.StorageGateway.Types.StorediSCSIVolume
import Network.AWS.StorageGateway.Types.String
import Network.AWS.StorageGateway.Types.Tag
import Network.AWS.StorageGateway.Types.TagKey
import Network.AWS.StorageGateway.Types.Tape
import Network.AWS.StorageGateway.Types.TapeARN
import Network.AWS.StorageGateway.Types.TapeArchive
import Network.AWS.StorageGateway.Types.TapeArchiveStatus
import Network.AWS.StorageGateway.Types.TapeBarcode
import Network.AWS.StorageGateway.Types.TapeBarcodePrefix
import Network.AWS.StorageGateway.Types.TapeDriveType
import Network.AWS.StorageGateway.Types.TapeInfo
import Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo
import Network.AWS.StorageGateway.Types.TapeRecoveryPointStatus
import Network.AWS.StorageGateway.Types.TapeStatus
import Network.AWS.StorageGateway.Types.TapeStorageClass
import Network.AWS.StorageGateway.Types.TargetARN
import Network.AWS.StorageGateway.Types.TargetName
import Network.AWS.StorageGateway.Types.Timezone
import Network.AWS.StorageGateway.Types.VTLDevice
import Network.AWS.StorageGateway.Types.VTLDeviceARN
import Network.AWS.StorageGateway.Types.VTLDeviceProductIdentifier
import Network.AWS.StorageGateway.Types.VTLDeviceType
import Network.AWS.StorageGateway.Types.VTLDeviceVendor
import Network.AWS.StorageGateway.Types.Value
import Network.AWS.StorageGateway.Types.VolumeARN
import Network.AWS.StorageGateway.Types.VolumeAttachmentStatus
import Network.AWS.StorageGateway.Types.VolumeDiskId
import Network.AWS.StorageGateway.Types.VolumeId
import Network.AWS.StorageGateway.Types.VolumeInfo
import Network.AWS.StorageGateway.Types.VolumeRecoveryPointInfo
import Network.AWS.StorageGateway.Types.VolumeStatus
import Network.AWS.StorageGateway.Types.VolumeType
import Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes

-- | API version @2013-06-30@ of the Amazon Storage Gateway SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "StorageGateway",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "storagegateway",
      Core._svcVersion = "2013-06-30",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "StorageGateway",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | An exception occurred because an invalid gateway request was issued to the service. For more information, see the error and message fields.
_InvalidGatewayRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGatewayRequestException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidGatewayRequestException"
{-# DEPRECATED _InvalidGatewayRequestException "Use generic-lens or generic-optics instead." #-}

-- | An internal server error has occurred because the service is unavailable. For more information, see the error and message fields.
_ServiceUnavailableError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableError =
  Core._MatchServiceError mkServiceConfig "ServiceUnavailableError"
{-# DEPRECATED _ServiceUnavailableError "Use generic-lens or generic-optics instead." #-}

-- | An internal server error has occurred during the request. For more information, see the error and message fields.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError mkServiceConfig "InternalServerError"
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead." #-}

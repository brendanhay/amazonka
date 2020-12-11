{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Storage Gateway Service__
--
-- AWS Storage Gateway is the service that connects an on-premises software appliance with cloud-based storage to provide seamless and secure integration between an organization's on-premises IT environment and the AWS storage infrastructure. The service enables you to securely upload data to the AWS Cloud for cost effective backup and rapid disaster recovery.
-- Use the following links to get started using the /AWS Storage Gateway Service API Reference/ :
--
--     * <https://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewayHTTPRequestsHeaders AWS Storage Gateway required request headers> : Describes the required headers that you must send with every POST request to AWS Storage Gateway.
--
--
--     * <https://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewaySigningRequests Signing requests> : AWS Storage Gateway requires that you authenticate every request you send; this topic describes how sign such a request.
--
--
--     * <https://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#APIErrorResponses Error responses> : Provides reference information about AWS Storage Gateway errors.
--
--
--     * <https://docs.aws.amazon.com/storagegateway/latest/APIReference/API_Operations.html Operations in AWS Storage Gateway> : Contains detailed descriptions of all AWS Storage Gateway operations, their request parameters, response elements, possible errors, and examples of requests and responses.
--
--
--     * <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas> : Provides a list of each AWS Region and the endpoints available for use with AWS Storage Gateway.
--
--
-- /Important:/ IDs for Storage Gateway volumes and Amazon EBS snapshots created from gateway volumes are changing to a longer format. Starting in December 2016, all new volumes and snapshots will be created with a 17-character string. Starting in April 2016, you will be able to use these longer IDs so you can test your systems with the new format. For more information, see <http://aws.amazon.com/ec2/faqs/#longer-ids Longer EC2 and EBS resource IDs> .
-- For example, a volume Amazon Resource Name (ARN) with the longer volume ID format looks like the following:
-- @arn:aws:storagegateway:us-west-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABBCCDDEEFFG@ .
-- A snapshot ID with the longer ID format looks like the following: @snap-78e226633445566ee@ .
-- For more information, see <http://forums.aws.amazon.com/ann.jspa?annID=3557 Announcement: Heads-up – Longer AWS Storage Gateway volume and snapshot IDs coming in 2016> .
module Network.AWS.StorageGateway
  ( -- * Service configuration
    storageGatewayService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelArchival
    module Network.AWS.StorageGateway.CancelArchival,

    -- ** CreateStorediSCSIVolume
    module Network.AWS.StorageGateway.CreateStorediSCSIVolume,

    -- ** CreateNFSFileShare
    module Network.AWS.StorageGateway.CreateNFSFileShare,

    -- ** DetachVolume
    module Network.AWS.StorageGateway.DetachVolume,

    -- ** DescribeChapCredentials
    module Network.AWS.StorageGateway.DescribeChapCredentials,

    -- ** SetLocalConsolePassword
    module Network.AWS.StorageGateway.SetLocalConsolePassword,

    -- ** CreateTapes
    module Network.AWS.StorageGateway.CreateTapes,

    -- ** UpdateVTLDeviceType
    module Network.AWS.StorageGateway.UpdateVTLDeviceType,

    -- ** CreateCachediSCSIVolume
    module Network.AWS.StorageGateway.CreateCachediSCSIVolume,

    -- ** ListFileShares (Paginated)
    module Network.AWS.StorageGateway.ListFileShares,

    -- ** JoinDomain
    module Network.AWS.StorageGateway.JoinDomain,

    -- ** DeleteFileShare
    module Network.AWS.StorageGateway.DeleteFileShare,

    -- ** ListVolumeInitiators
    module Network.AWS.StorageGateway.ListVolumeInitiators,

    -- ** AddUploadBuffer
    module Network.AWS.StorageGateway.AddUploadBuffer,

    -- ** ListTagsForResource (Paginated)
    module Network.AWS.StorageGateway.ListTagsForResource,

    -- ** NotifyWhenUploaded
    module Network.AWS.StorageGateway.NotifyWhenUploaded,

    -- ** ListTapePools (Paginated)
    module Network.AWS.StorageGateway.ListTapePools,

    -- ** DeleteTapePool
    module Network.AWS.StorageGateway.DeleteTapePool,

    -- ** UpdateGatewayInformation
    module Network.AWS.StorageGateway.UpdateGatewayInformation,

    -- ** DescribeMaintenanceStartTime
    module Network.AWS.StorageGateway.DescribeMaintenanceStartTime,

    -- ** AssignTapePool
    module Network.AWS.StorageGateway.AssignTapePool,

    -- ** DescribeWorkingStorage
    module Network.AWS.StorageGateway.DescribeWorkingStorage,

    -- ** DescribeCachediSCSIVolumes
    module Network.AWS.StorageGateway.DescribeCachediSCSIVolumes,

    -- ** AddCache
    module Network.AWS.StorageGateway.AddCache,

    -- ** CreateTapePool
    module Network.AWS.StorageGateway.CreateTapePool,

    -- ** StartGateway
    module Network.AWS.StorageGateway.StartGateway,

    -- ** ShutdownGateway
    module Network.AWS.StorageGateway.ShutdownGateway,

    -- ** ListAutomaticTapeCreationPolicies
    module Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies,

    -- ** UpdateGatewaySoftwareNow
    module Network.AWS.StorageGateway.UpdateGatewaySoftwareNow,

    -- ** RemoveTagsFromResource
    module Network.AWS.StorageGateway.RemoveTagsFromResource,

    -- ** CreateSMBFileShare
    module Network.AWS.StorageGateway.CreateSMBFileShare,

    -- ** DeleteChapCredentials
    module Network.AWS.StorageGateway.DeleteChapCredentials,

    -- ** UpdateChapCredentials
    module Network.AWS.StorageGateway.UpdateChapCredentials,

    -- ** AttachVolume
    module Network.AWS.StorageGateway.AttachVolume,

    -- ** DescribeAvailabilityMonitorTest
    module Network.AWS.StorageGateway.DescribeAvailabilityMonitorTest,

    -- ** DescribeUploadBuffer
    module Network.AWS.StorageGateway.DescribeUploadBuffer,

    -- ** DescribeTapes (Paginated)
    module Network.AWS.StorageGateway.DescribeTapes,

    -- ** DescribeStorediSCSIVolumes
    module Network.AWS.StorageGateway.DescribeStorediSCSIVolumes,

    -- ** SetSMBGuestPassword
    module Network.AWS.StorageGateway.SetSMBGuestPassword,

    -- ** CreateSnapshotFromVolumeRecoveryPoint
    module Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint,

    -- ** RetrieveTapeRecoveryPoint
    module Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint,

    -- ** AddTagsToResource
    module Network.AWS.StorageGateway.AddTagsToResource,

    -- ** DeleteGateway
    module Network.AWS.StorageGateway.DeleteGateway,

    -- ** UpdateMaintenanceStartTime
    module Network.AWS.StorageGateway.UpdateMaintenanceStartTime,

    -- ** DescribeGatewayInformation
    module Network.AWS.StorageGateway.DescribeGatewayInformation,

    -- ** RefreshCache
    module Network.AWS.StorageGateway.RefreshCache,

    -- ** UpdateNFSFileShare
    module Network.AWS.StorageGateway.UpdateNFSFileShare,

    -- ** RetrieveTapeArchive
    module Network.AWS.StorageGateway.RetrieveTapeArchive,

    -- ** DescribeTapeArchives (Paginated)
    module Network.AWS.StorageGateway.DescribeTapeArchives,

    -- ** UpdateBandwidthRateLimitSchedule
    module Network.AWS.StorageGateway.UpdateBandwidthRateLimitSchedule,

    -- ** DisableGateway
    module Network.AWS.StorageGateway.DisableGateway,

    -- ** DescribeSMBSettings
    module Network.AWS.StorageGateway.DescribeSMBSettings,

    -- ** DescribeSnapshotSchedule
    module Network.AWS.StorageGateway.DescribeSnapshotSchedule,

    -- ** CreateTapeWithBarcode
    module Network.AWS.StorageGateway.CreateTapeWithBarcode,

    -- ** DescribeBandwidthRateLimit
    module Network.AWS.StorageGateway.DescribeBandwidthRateLimit,

    -- ** DeleteAutomaticTapeCreationPolicy
    module Network.AWS.StorageGateway.DeleteAutomaticTapeCreationPolicy,

    -- ** UpdateAutomaticTapeCreationPolicy
    module Network.AWS.StorageGateway.UpdateAutomaticTapeCreationPolicy,

    -- ** UpdateSMBFileShareVisibility
    module Network.AWS.StorageGateway.UpdateSMBFileShareVisibility,

    -- ** DeleteSnapshotSchedule
    module Network.AWS.StorageGateway.DeleteSnapshotSchedule,

    -- ** UpdateSnapshotSchedule
    module Network.AWS.StorageGateway.UpdateSnapshotSchedule,

    -- ** DescribeBandwidthRateLimitSchedule
    module Network.AWS.StorageGateway.DescribeBandwidthRateLimitSchedule,

    -- ** CreateSnapshot
    module Network.AWS.StorageGateway.CreateSnapshot,

    -- ** UpdateSMBSecurityStrategy
    module Network.AWS.StorageGateway.UpdateSMBSecurityStrategy,

    -- ** CancelRetrieval
    module Network.AWS.StorageGateway.CancelRetrieval,

    -- ** DescribeVTLDevices (Paginated)
    module Network.AWS.StorageGateway.DescribeVTLDevices,

    -- ** StartAvailabilityMonitorTest
    module Network.AWS.StorageGateway.StartAvailabilityMonitorTest,

    -- ** DeleteTapeArchive
    module Network.AWS.StorageGateway.DeleteTapeArchive,

    -- ** UpdateSMBFileShare
    module Network.AWS.StorageGateway.UpdateSMBFileShare,

    -- ** DescribeNFSFileShares
    module Network.AWS.StorageGateway.DescribeNFSFileShares,

    -- ** ListVolumeRecoveryPoints
    module Network.AWS.StorageGateway.ListVolumeRecoveryPoints,

    -- ** ListTapes (Paginated)
    module Network.AWS.StorageGateway.ListTapes,

    -- ** ResetCache
    module Network.AWS.StorageGateway.ResetCache,

    -- ** DescribeSMBFileShares
    module Network.AWS.StorageGateway.DescribeSMBFileShares,

    -- ** ListGateways (Paginated)
    module Network.AWS.StorageGateway.ListGateways,

    -- ** DeleteTape
    module Network.AWS.StorageGateway.DeleteTape,

    -- ** ListLocalDisks
    module Network.AWS.StorageGateway.ListLocalDisks,

    -- ** ListVolumes (Paginated)
    module Network.AWS.StorageGateway.ListVolumes,

    -- ** UpdateBandwidthRateLimit
    module Network.AWS.StorageGateway.UpdateBandwidthRateLimit,

    -- ** AddWorkingStorage
    module Network.AWS.StorageGateway.AddWorkingStorage,

    -- ** DescribeTapeRecoveryPoints (Paginated)
    module Network.AWS.StorageGateway.DescribeTapeRecoveryPoints,

    -- ** DeleteBandwidthRateLimit
    module Network.AWS.StorageGateway.DeleteBandwidthRateLimit,

    -- ** ActivateGateway
    module Network.AWS.StorageGateway.ActivateGateway,

    -- ** DescribeCache
    module Network.AWS.StorageGateway.DescribeCache,

    -- ** DeleteVolume
    module Network.AWS.StorageGateway.DeleteVolume,

    -- * Types

    -- ** ActiveDirectoryStatus
    ActiveDirectoryStatus (..),

    -- ** AvailabilityMonitorTestStatus
    AvailabilityMonitorTestStatus (..),

    -- ** CaseSensitivity
    CaseSensitivity (..),

    -- ** FileShareType
    FileShareType (..),

    -- ** HostEnvironment
    HostEnvironment (..),

    -- ** ObjectACL
    ObjectACL (..),

    -- ** PoolStatus
    PoolStatus (..),

    -- ** RetentionLockType
    RetentionLockType (..),

    -- ** SMBSecurityStrategy
    SMBSecurityStrategy (..),

    -- ** TapeStorageClass
    TapeStorageClass (..),

    -- ** AutomaticTapeCreationPolicyInfo
    AutomaticTapeCreationPolicyInfo (..),
    mkAutomaticTapeCreationPolicyInfo,
    atcpiGatewayARN,
    atcpiAutomaticTapeCreationRules,

    -- ** AutomaticTapeCreationRule
    AutomaticTapeCreationRule (..),
    mkAutomaticTapeCreationRule,
    atcrWorm,
    atcrTapeBarcodePrefix,
    atcrPoolId,
    atcrTapeSizeInBytes,
    atcrMinimumNumTapes,

    -- ** BandwidthRateLimitInterval
    BandwidthRateLimitInterval (..),
    mkBandwidthRateLimitInterval,
    brliAverageUploadRateLimitInBitsPerSec,
    brliAverageDownloadRateLimitInBitsPerSec,
    brliStartHourOfDay,
    brliStartMinuteOfHour,
    brliEndHourOfDay,
    brliEndMinuteOfHour,
    brliDaysOfWeek,

    -- ** CacheAttributes
    CacheAttributes (..),
    mkCacheAttributes,
    caCacheStaleTimeoutInSeconds,

    -- ** CachediSCSIVolume
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

    -- ** ChapInfo
    ChapInfo (..),
    mkChapInfo,
    ciTargetARN,
    ciSecretToAuthenticateInitiator,
    ciInitiatorName,
    ciSecretToAuthenticateTarget,

    -- ** DeviceiSCSIAttributes
    DeviceiSCSIAttributes (..),
    mkDeviceiSCSIAttributes,
    dscsiaTargetARN,
    dscsiaChapEnabled,
    dscsiaNetworkInterfaceId,
    dscsiaNetworkInterfacePort,

    -- ** Disk
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

    -- ** FileShareInfo
    FileShareInfo (..),
    mkFileShareInfo,
    fsiFileShareStatus,
    fsiGatewayARN,
    fsiFileShareId,
    fsiFileShareARN,
    fsiFileShareType,

    -- ** GatewayInfo
    GatewayInfo (..),
    mkGatewayInfo,
    giEC2InstanceRegion,
    giGatewayARN,
    giEC2InstanceId,
    giGatewayOperationalState,
    giGatewayName,
    giGatewayId,
    giGatewayType,

    -- ** NFSFileShareDefaults
    NFSFileShareDefaults (..),
    mkNFSFileShareDefaults,
    nfsfsdFileMode,
    nfsfsdOwnerId,
    nfsfsdDirectoryMode,
    nfsfsdGroupId,

    -- ** NFSFileShareInfo
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

    -- ** NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niIPv6Address,
    niMACAddress,
    niIPv4Address,

    -- ** PoolInfo
    PoolInfo (..),
    mkPoolInfo,
    piRetentionLockType,
    piRetentionLockTimeInDays,
    piPoolName,
    piStorageClass,
    piPoolStatus,
    piPoolARN,

    -- ** SMBFileShareInfo
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

    -- ** StorediSCSIVolume
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

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** Tape
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

    -- ** TapeArchive
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

    -- ** TapeInfo
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

    -- ** TapeRecoveryPointInfo
    TapeRecoveryPointInfo (..),
    mkTapeRecoveryPointInfo,
    trpiTapeStatus,
    trpiTapeRecoveryPointTime,
    trpiTapeARN,
    trpiTapeSizeInBytes,

    -- ** VTLDevice
    VTLDevice (..),
    mkVTLDevice,
    vtldDeviceiSCSIAttributes,
    vtldVTLDeviceVendor,
    vtldVTLDeviceARN,
    vtldVTLDeviceType,
    vtldVTLDeviceProductIdentifier,

    -- ** VolumeInfo
    VolumeInfo (..),
    mkVolumeInfo,
    viGatewayARN,
    viVolumeAttachmentStatus,
    viVolumeARN,
    viVolumeSizeInBytes,
    viVolumeId,
    viGatewayId,
    viVolumeType,

    -- ** VolumeRecoveryPointInfo
    VolumeRecoveryPointInfo (..),
    mkVolumeRecoveryPointInfo,
    vrpiVolumeRecoveryPointTime,
    vrpiVolumeARN,
    vrpiVolumeSizeInBytes,
    vrpiVolumeUsageInBytes,

    -- ** VolumeiSCSIAttributes
    VolumeiSCSIAttributes (..),
    mkVolumeiSCSIAttributes,
    vscsiaLunNumber,
    vscsiaTargetARN,
    vscsiaChapEnabled,
    vscsiaNetworkInterfaceId,
    vscsiaNetworkInterfacePort,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.StorageGateway.ActivateGateway
import Network.AWS.StorageGateway.AddCache
import Network.AWS.StorageGateway.AddTagsToResource
import Network.AWS.StorageGateway.AddUploadBuffer
import Network.AWS.StorageGateway.AddWorkingStorage
import Network.AWS.StorageGateway.AssignTapePool
import Network.AWS.StorageGateway.AttachVolume
import Network.AWS.StorageGateway.CancelArchival
import Network.AWS.StorageGateway.CancelRetrieval
import Network.AWS.StorageGateway.CreateCachediSCSIVolume
import Network.AWS.StorageGateway.CreateNFSFileShare
import Network.AWS.StorageGateway.CreateSMBFileShare
import Network.AWS.StorageGateway.CreateSnapshot
import Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
import Network.AWS.StorageGateway.CreateStorediSCSIVolume
import Network.AWS.StorageGateway.CreateTapePool
import Network.AWS.StorageGateway.CreateTapeWithBarcode
import Network.AWS.StorageGateway.CreateTapes
import Network.AWS.StorageGateway.DeleteAutomaticTapeCreationPolicy
import Network.AWS.StorageGateway.DeleteBandwidthRateLimit
import Network.AWS.StorageGateway.DeleteChapCredentials
import Network.AWS.StorageGateway.DeleteFileShare
import Network.AWS.StorageGateway.DeleteGateway
import Network.AWS.StorageGateway.DeleteSnapshotSchedule
import Network.AWS.StorageGateway.DeleteTape
import Network.AWS.StorageGateway.DeleteTapeArchive
import Network.AWS.StorageGateway.DeleteTapePool
import Network.AWS.StorageGateway.DeleteVolume
import Network.AWS.StorageGateway.DescribeAvailabilityMonitorTest
import Network.AWS.StorageGateway.DescribeBandwidthRateLimit
import Network.AWS.StorageGateway.DescribeBandwidthRateLimitSchedule
import Network.AWS.StorageGateway.DescribeCache
import Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
import Network.AWS.StorageGateway.DescribeChapCredentials
import Network.AWS.StorageGateway.DescribeGatewayInformation
import Network.AWS.StorageGateway.DescribeMaintenanceStartTime
import Network.AWS.StorageGateway.DescribeNFSFileShares
import Network.AWS.StorageGateway.DescribeSMBFileShares
import Network.AWS.StorageGateway.DescribeSMBSettings
import Network.AWS.StorageGateway.DescribeSnapshotSchedule
import Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
import Network.AWS.StorageGateway.DescribeTapeArchives
import Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
import Network.AWS.StorageGateway.DescribeTapes
import Network.AWS.StorageGateway.DescribeUploadBuffer
import Network.AWS.StorageGateway.DescribeVTLDevices
import Network.AWS.StorageGateway.DescribeWorkingStorage
import Network.AWS.StorageGateway.DetachVolume
import Network.AWS.StorageGateway.DisableGateway
import Network.AWS.StorageGateway.JoinDomain
import Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies
import Network.AWS.StorageGateway.ListFileShares
import Network.AWS.StorageGateway.ListGateways
import Network.AWS.StorageGateway.ListLocalDisks
import Network.AWS.StorageGateway.ListTagsForResource
import Network.AWS.StorageGateway.ListTapePools
import Network.AWS.StorageGateway.ListTapes
import Network.AWS.StorageGateway.ListVolumeInitiators
import Network.AWS.StorageGateway.ListVolumeRecoveryPoints
import Network.AWS.StorageGateway.ListVolumes
import Network.AWS.StorageGateway.NotifyWhenUploaded
import Network.AWS.StorageGateway.RefreshCache
import Network.AWS.StorageGateway.RemoveTagsFromResource
import Network.AWS.StorageGateway.ResetCache
import Network.AWS.StorageGateway.RetrieveTapeArchive
import Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
import Network.AWS.StorageGateway.SetLocalConsolePassword
import Network.AWS.StorageGateway.SetSMBGuestPassword
import Network.AWS.StorageGateway.ShutdownGateway
import Network.AWS.StorageGateway.StartAvailabilityMonitorTest
import Network.AWS.StorageGateway.StartGateway
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.UpdateAutomaticTapeCreationPolicy
import Network.AWS.StorageGateway.UpdateBandwidthRateLimit
import Network.AWS.StorageGateway.UpdateBandwidthRateLimitSchedule
import Network.AWS.StorageGateway.UpdateChapCredentials
import Network.AWS.StorageGateway.UpdateGatewayInformation
import Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
import Network.AWS.StorageGateway.UpdateMaintenanceStartTime
import Network.AWS.StorageGateway.UpdateNFSFileShare
import Network.AWS.StorageGateway.UpdateSMBFileShare
import Network.AWS.StorageGateway.UpdateSMBFileShareVisibility
import Network.AWS.StorageGateway.UpdateSMBSecurityStrategy
import Network.AWS.StorageGateway.UpdateSnapshotSchedule
import Network.AWS.StorageGateway.UpdateVTLDeviceType
import Network.AWS.StorageGateway.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'StorageGateway'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

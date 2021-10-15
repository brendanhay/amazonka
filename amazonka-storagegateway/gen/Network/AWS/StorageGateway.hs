{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.StorageGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2013-06-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Storage Gateway Service
--
-- Storage Gateway is the service that connects an on-premises software
-- appliance with cloud-based storage to provide seamless and secure
-- integration between an organization\'s on-premises IT environment and
-- the Amazon Web Services storage infrastructure. The service enables you
-- to securely upload data to the Cloud for cost effective backup and rapid
-- disaster recovery.
--
-- Use the following links to get started using the /Storage Gateway
-- Service API Reference/:
--
-- -   <https://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewayHTTPRequestsHeaders Storage Gateway required request headers>:
--     Describes the required headers that you must send with every POST
--     request to Storage Gateway.
--
-- -   <https://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewaySigningRequests Signing requests>:
--     Storage Gateway requires that you authenticate every request you
--     send; this topic describes how sign such a request.
--
-- -   <https://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#APIErrorResponses Error responses>:
--     Provides reference information about Storage Gateway errors.
--
-- -   <https://docs.aws.amazon.com/storagegateway/latest/APIReference/API_Operations.html Operations in Storage Gateway>:
--     Contains detailed descriptions of all Storage Gateway operations,
--     their request parameters, response elements, possible errors, and
--     examples of requests and responses.
--
-- -   <https://docs.aws.amazon.com/general/latest/gr/sg.html Storage Gateway endpoints and quotas>:
--     Provides a list of each Region and the endpoints available for use
--     with Storage Gateway.
--
-- Storage Gateway resource IDs are in uppercase. When you use these
-- resource IDs with the Amazon EC2 API, EC2 expects resource IDs in
-- lowercase. You must change your resource ID to lowercase to use it with
-- the EC2 API. For example, in Storage Gateway the ID for a volume might
-- be @vol-AA22BB012345DAF670@. When you use this ID with the EC2 API, you
-- must change it to @vol-aa22bb012345daf670@. Otherwise, the EC2 API might
-- not behave as expected.
--
-- IDs for Storage Gateway volumes and Amazon EBS snapshots created from
-- gateway volumes are changing to a longer format. Starting in December
-- 2016, all new volumes and snapshots will be created with a 17-character
-- string. Starting in April 2016, you will be able to use these longer IDs
-- so you can test your systems with the new format. For more information,
-- see
-- <http://aws.amazon.com/ec2/faqs/#longer-ids Longer EC2 and EBS resource IDs>.
--
-- For example, a volume Amazon Resource Name (ARN) with the longer volume
-- ID format looks like the following:
--
-- @arn:aws:storagegateway:us-west-2:111122223333:gateway\/sgw-12A3456B\/volume\/vol-1122AABBCCDDEEFFG@.
--
-- A snapshot ID with the longer ID format looks like the following:
-- @snap-78e226633445566ee@.
--
-- For more information, see
-- <http://forums.aws.amazon.com/ann.jspa?annID=3557 Announcement: Heads-up – Longer Storage Gateway volume and snapshot IDs coming in 2016>.
module Network.AWS.StorageGateway
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidGatewayRequestException
    _InvalidGatewayRequestException,

    -- ** ServiceUnavailableError
    _ServiceUnavailableError,

    -- ** InternalServerError
    _InternalServerError,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelArchival
    CancelArchival (CancelArchival'),
    newCancelArchival,
    CancelArchivalResponse (CancelArchivalResponse'),
    newCancelArchivalResponse,

    -- ** CreateStorediSCSIVolume
    CreateStorediSCSIVolume (CreateStorediSCSIVolume'),
    newCreateStorediSCSIVolume,
    CreateStorediSCSIVolumeResponse (CreateStorediSCSIVolumeResponse'),
    newCreateStorediSCSIVolumeResponse,

    -- ** DescribeFileSystemAssociations
    DescribeFileSystemAssociations (DescribeFileSystemAssociations'),
    newDescribeFileSystemAssociations,
    DescribeFileSystemAssociationsResponse (DescribeFileSystemAssociationsResponse'),
    newDescribeFileSystemAssociationsResponse,

    -- ** CreateNFSFileShare
    CreateNFSFileShare (CreateNFSFileShare'),
    newCreateNFSFileShare,
    CreateNFSFileShareResponse (CreateNFSFileShareResponse'),
    newCreateNFSFileShareResponse,

    -- ** AssociateFileSystem
    AssociateFileSystem (AssociateFileSystem'),
    newAssociateFileSystem,
    AssociateFileSystemResponse (AssociateFileSystemResponse'),
    newAssociateFileSystemResponse,

    -- ** DetachVolume
    DetachVolume (DetachVolume'),
    newDetachVolume,
    DetachVolumeResponse (DetachVolumeResponse'),
    newDetachVolumeResponse,

    -- ** DescribeChapCredentials
    DescribeChapCredentials (DescribeChapCredentials'),
    newDescribeChapCredentials,
    DescribeChapCredentialsResponse (DescribeChapCredentialsResponse'),
    newDescribeChapCredentialsResponse,

    -- ** SetLocalConsolePassword
    SetLocalConsolePassword (SetLocalConsolePassword'),
    newSetLocalConsolePassword,
    SetLocalConsolePasswordResponse (SetLocalConsolePasswordResponse'),
    newSetLocalConsolePasswordResponse,

    -- ** CreateTapes
    CreateTapes (CreateTapes'),
    newCreateTapes,
    CreateTapesResponse (CreateTapesResponse'),
    newCreateTapesResponse,

    -- ** UpdateVTLDeviceType
    UpdateVTLDeviceType (UpdateVTLDeviceType'),
    newUpdateVTLDeviceType,
    UpdateVTLDeviceTypeResponse (UpdateVTLDeviceTypeResponse'),
    newUpdateVTLDeviceTypeResponse,

    -- ** CreateCachediSCSIVolume
    CreateCachediSCSIVolume (CreateCachediSCSIVolume'),
    newCreateCachediSCSIVolume,
    CreateCachediSCSIVolumeResponse (CreateCachediSCSIVolumeResponse'),
    newCreateCachediSCSIVolumeResponse,

    -- ** ListFileShares (Paginated)
    ListFileShares (ListFileShares'),
    newListFileShares,
    ListFileSharesResponse (ListFileSharesResponse'),
    newListFileSharesResponse,

    -- ** JoinDomain
    JoinDomain (JoinDomain'),
    newJoinDomain,
    JoinDomainResponse (JoinDomainResponse'),
    newJoinDomainResponse,

    -- ** DeleteFileShare
    DeleteFileShare (DeleteFileShare'),
    newDeleteFileShare,
    DeleteFileShareResponse (DeleteFileShareResponse'),
    newDeleteFileShareResponse,

    -- ** ListVolumeInitiators
    ListVolumeInitiators (ListVolumeInitiators'),
    newListVolumeInitiators,
    ListVolumeInitiatorsResponse (ListVolumeInitiatorsResponse'),
    newListVolumeInitiatorsResponse,

    -- ** AddUploadBuffer
    AddUploadBuffer (AddUploadBuffer'),
    newAddUploadBuffer,
    AddUploadBufferResponse (AddUploadBufferResponse'),
    newAddUploadBufferResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** NotifyWhenUploaded
    NotifyWhenUploaded (NotifyWhenUploaded'),
    newNotifyWhenUploaded,
    NotifyWhenUploadedResponse (NotifyWhenUploadedResponse'),
    newNotifyWhenUploadedResponse,

    -- ** ListTapePools (Paginated)
    ListTapePools (ListTapePools'),
    newListTapePools,
    ListTapePoolsResponse (ListTapePoolsResponse'),
    newListTapePoolsResponse,

    -- ** DeleteTapePool
    DeleteTapePool (DeleteTapePool'),
    newDeleteTapePool,
    DeleteTapePoolResponse (DeleteTapePoolResponse'),
    newDeleteTapePoolResponse,

    -- ** UpdateGatewayInformation
    UpdateGatewayInformation (UpdateGatewayInformation'),
    newUpdateGatewayInformation,
    UpdateGatewayInformationResponse (UpdateGatewayInformationResponse'),
    newUpdateGatewayInformationResponse,

    -- ** DescribeMaintenanceStartTime
    DescribeMaintenanceStartTime (DescribeMaintenanceStartTime'),
    newDescribeMaintenanceStartTime,
    DescribeMaintenanceStartTimeResponse (DescribeMaintenanceStartTimeResponse'),
    newDescribeMaintenanceStartTimeResponse,

    -- ** AssignTapePool
    AssignTapePool (AssignTapePool'),
    newAssignTapePool,
    AssignTapePoolResponse (AssignTapePoolResponse'),
    newAssignTapePoolResponse,

    -- ** DescribeWorkingStorage
    DescribeWorkingStorage (DescribeWorkingStorage'),
    newDescribeWorkingStorage,
    DescribeWorkingStorageResponse (DescribeWorkingStorageResponse'),
    newDescribeWorkingStorageResponse,

    -- ** DescribeCachediSCSIVolumes
    DescribeCachediSCSIVolumes (DescribeCachediSCSIVolumes'),
    newDescribeCachediSCSIVolumes,
    DescribeCachediSCSIVolumesResponse (DescribeCachediSCSIVolumesResponse'),
    newDescribeCachediSCSIVolumesResponse,

    -- ** AddCache
    AddCache (AddCache'),
    newAddCache,
    AddCacheResponse (AddCacheResponse'),
    newAddCacheResponse,

    -- ** CreateTapePool
    CreateTapePool (CreateTapePool'),
    newCreateTapePool,
    CreateTapePoolResponse (CreateTapePoolResponse'),
    newCreateTapePoolResponse,

    -- ** StartGateway
    StartGateway (StartGateway'),
    newStartGateway,
    StartGatewayResponse (StartGatewayResponse'),
    newStartGatewayResponse,

    -- ** ShutdownGateway
    ShutdownGateway (ShutdownGateway'),
    newShutdownGateway,
    ShutdownGatewayResponse (ShutdownGatewayResponse'),
    newShutdownGatewayResponse,

    -- ** ListAutomaticTapeCreationPolicies
    ListAutomaticTapeCreationPolicies (ListAutomaticTapeCreationPolicies'),
    newListAutomaticTapeCreationPolicies,
    ListAutomaticTapeCreationPoliciesResponse (ListAutomaticTapeCreationPoliciesResponse'),
    newListAutomaticTapeCreationPoliciesResponse,

    -- ** UpdateGatewaySoftwareNow
    UpdateGatewaySoftwareNow (UpdateGatewaySoftwareNow'),
    newUpdateGatewaySoftwareNow,
    UpdateGatewaySoftwareNowResponse (UpdateGatewaySoftwareNowResponse'),
    newUpdateGatewaySoftwareNowResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** UpdateFileSystemAssociation
    UpdateFileSystemAssociation (UpdateFileSystemAssociation'),
    newUpdateFileSystemAssociation,
    UpdateFileSystemAssociationResponse (UpdateFileSystemAssociationResponse'),
    newUpdateFileSystemAssociationResponse,

    -- ** CreateSMBFileShare
    CreateSMBFileShare (CreateSMBFileShare'),
    newCreateSMBFileShare,
    CreateSMBFileShareResponse (CreateSMBFileShareResponse'),
    newCreateSMBFileShareResponse,

    -- ** DeleteChapCredentials
    DeleteChapCredentials (DeleteChapCredentials'),
    newDeleteChapCredentials,
    DeleteChapCredentialsResponse (DeleteChapCredentialsResponse'),
    newDeleteChapCredentialsResponse,

    -- ** UpdateChapCredentials
    UpdateChapCredentials (UpdateChapCredentials'),
    newUpdateChapCredentials,
    UpdateChapCredentialsResponse (UpdateChapCredentialsResponse'),
    newUpdateChapCredentialsResponse,

    -- ** AttachVolume
    AttachVolume (AttachVolume'),
    newAttachVolume,
    AttachVolumeResponse (AttachVolumeResponse'),
    newAttachVolumeResponse,

    -- ** DescribeAvailabilityMonitorTest
    DescribeAvailabilityMonitorTest (DescribeAvailabilityMonitorTest'),
    newDescribeAvailabilityMonitorTest,
    DescribeAvailabilityMonitorTestResponse (DescribeAvailabilityMonitorTestResponse'),
    newDescribeAvailabilityMonitorTestResponse,

    -- ** DescribeUploadBuffer
    DescribeUploadBuffer (DescribeUploadBuffer'),
    newDescribeUploadBuffer,
    DescribeUploadBufferResponse (DescribeUploadBufferResponse'),
    newDescribeUploadBufferResponse,

    -- ** DescribeTapes (Paginated)
    DescribeTapes (DescribeTapes'),
    newDescribeTapes,
    DescribeTapesResponse (DescribeTapesResponse'),
    newDescribeTapesResponse,

    -- ** DescribeStorediSCSIVolumes
    DescribeStorediSCSIVolumes (DescribeStorediSCSIVolumes'),
    newDescribeStorediSCSIVolumes,
    DescribeStorediSCSIVolumesResponse (DescribeStorediSCSIVolumesResponse'),
    newDescribeStorediSCSIVolumesResponse,

    -- ** DisassociateFileSystem
    DisassociateFileSystem (DisassociateFileSystem'),
    newDisassociateFileSystem,
    DisassociateFileSystemResponse (DisassociateFileSystemResponse'),
    newDisassociateFileSystemResponse,

    -- ** SetSMBGuestPassword
    SetSMBGuestPassword (SetSMBGuestPassword'),
    newSetSMBGuestPassword,
    SetSMBGuestPasswordResponse (SetSMBGuestPasswordResponse'),
    newSetSMBGuestPasswordResponse,

    -- ** CreateSnapshotFromVolumeRecoveryPoint
    CreateSnapshotFromVolumeRecoveryPoint (CreateSnapshotFromVolumeRecoveryPoint'),
    newCreateSnapshotFromVolumeRecoveryPoint,
    CreateSnapshotFromVolumeRecoveryPointResponse (CreateSnapshotFromVolumeRecoveryPointResponse'),
    newCreateSnapshotFromVolumeRecoveryPointResponse,

    -- ** RetrieveTapeRecoveryPoint
    RetrieveTapeRecoveryPoint (RetrieveTapeRecoveryPoint'),
    newRetrieveTapeRecoveryPoint,
    RetrieveTapeRecoveryPointResponse (RetrieveTapeRecoveryPointResponse'),
    newRetrieveTapeRecoveryPointResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** DeleteGateway
    DeleteGateway (DeleteGateway'),
    newDeleteGateway,
    DeleteGatewayResponse (DeleteGatewayResponse'),
    newDeleteGatewayResponse,

    -- ** UpdateMaintenanceStartTime
    UpdateMaintenanceStartTime (UpdateMaintenanceStartTime'),
    newUpdateMaintenanceStartTime,
    UpdateMaintenanceStartTimeResponse (UpdateMaintenanceStartTimeResponse'),
    newUpdateMaintenanceStartTimeResponse,

    -- ** DescribeGatewayInformation
    DescribeGatewayInformation (DescribeGatewayInformation'),
    newDescribeGatewayInformation,
    DescribeGatewayInformationResponse (DescribeGatewayInformationResponse'),
    newDescribeGatewayInformationResponse,

    -- ** RefreshCache
    RefreshCache (RefreshCache'),
    newRefreshCache,
    RefreshCacheResponse (RefreshCacheResponse'),
    newRefreshCacheResponse,

    -- ** UpdateNFSFileShare
    UpdateNFSFileShare (UpdateNFSFileShare'),
    newUpdateNFSFileShare,
    UpdateNFSFileShareResponse (UpdateNFSFileShareResponse'),
    newUpdateNFSFileShareResponse,

    -- ** RetrieveTapeArchive
    RetrieveTapeArchive (RetrieveTapeArchive'),
    newRetrieveTapeArchive,
    RetrieveTapeArchiveResponse (RetrieveTapeArchiveResponse'),
    newRetrieveTapeArchiveResponse,

    -- ** DescribeTapeArchives (Paginated)
    DescribeTapeArchives (DescribeTapeArchives'),
    newDescribeTapeArchives,
    DescribeTapeArchivesResponse (DescribeTapeArchivesResponse'),
    newDescribeTapeArchivesResponse,

    -- ** UpdateBandwidthRateLimitSchedule
    UpdateBandwidthRateLimitSchedule (UpdateBandwidthRateLimitSchedule'),
    newUpdateBandwidthRateLimitSchedule,
    UpdateBandwidthRateLimitScheduleResponse (UpdateBandwidthRateLimitScheduleResponse'),
    newUpdateBandwidthRateLimitScheduleResponse,

    -- ** DisableGateway
    DisableGateway (DisableGateway'),
    newDisableGateway,
    DisableGatewayResponse (DisableGatewayResponse'),
    newDisableGatewayResponse,

    -- ** DescribeSMBSettings
    DescribeSMBSettings (DescribeSMBSettings'),
    newDescribeSMBSettings,
    DescribeSMBSettingsResponse (DescribeSMBSettingsResponse'),
    newDescribeSMBSettingsResponse,

    -- ** DescribeSnapshotSchedule
    DescribeSnapshotSchedule (DescribeSnapshotSchedule'),
    newDescribeSnapshotSchedule,
    DescribeSnapshotScheduleResponse (DescribeSnapshotScheduleResponse'),
    newDescribeSnapshotScheduleResponse,

    -- ** CreateTapeWithBarcode
    CreateTapeWithBarcode (CreateTapeWithBarcode'),
    newCreateTapeWithBarcode,
    CreateTapeWithBarcodeResponse (CreateTapeWithBarcodeResponse'),
    newCreateTapeWithBarcodeResponse,

    -- ** DescribeBandwidthRateLimit
    DescribeBandwidthRateLimit (DescribeBandwidthRateLimit'),
    newDescribeBandwidthRateLimit,
    DescribeBandwidthRateLimitResponse (DescribeBandwidthRateLimitResponse'),
    newDescribeBandwidthRateLimitResponse,

    -- ** DeleteAutomaticTapeCreationPolicy
    DeleteAutomaticTapeCreationPolicy (DeleteAutomaticTapeCreationPolicy'),
    newDeleteAutomaticTapeCreationPolicy,
    DeleteAutomaticTapeCreationPolicyResponse (DeleteAutomaticTapeCreationPolicyResponse'),
    newDeleteAutomaticTapeCreationPolicyResponse,

    -- ** UpdateAutomaticTapeCreationPolicy
    UpdateAutomaticTapeCreationPolicy (UpdateAutomaticTapeCreationPolicy'),
    newUpdateAutomaticTapeCreationPolicy,
    UpdateAutomaticTapeCreationPolicyResponse (UpdateAutomaticTapeCreationPolicyResponse'),
    newUpdateAutomaticTapeCreationPolicyResponse,

    -- ** UpdateSMBFileShareVisibility
    UpdateSMBFileShareVisibility (UpdateSMBFileShareVisibility'),
    newUpdateSMBFileShareVisibility,
    UpdateSMBFileShareVisibilityResponse (UpdateSMBFileShareVisibilityResponse'),
    newUpdateSMBFileShareVisibilityResponse,

    -- ** DeleteSnapshotSchedule
    DeleteSnapshotSchedule (DeleteSnapshotSchedule'),
    newDeleteSnapshotSchedule,
    DeleteSnapshotScheduleResponse (DeleteSnapshotScheduleResponse'),
    newDeleteSnapshotScheduleResponse,

    -- ** UpdateSnapshotSchedule
    UpdateSnapshotSchedule (UpdateSnapshotSchedule'),
    newUpdateSnapshotSchedule,
    UpdateSnapshotScheduleResponse (UpdateSnapshotScheduleResponse'),
    newUpdateSnapshotScheduleResponse,

    -- ** DescribeBandwidthRateLimitSchedule
    DescribeBandwidthRateLimitSchedule (DescribeBandwidthRateLimitSchedule'),
    newDescribeBandwidthRateLimitSchedule,
    DescribeBandwidthRateLimitScheduleResponse (DescribeBandwidthRateLimitScheduleResponse'),
    newDescribeBandwidthRateLimitScheduleResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** UpdateSMBSecurityStrategy
    UpdateSMBSecurityStrategy (UpdateSMBSecurityStrategy'),
    newUpdateSMBSecurityStrategy,
    UpdateSMBSecurityStrategyResponse (UpdateSMBSecurityStrategyResponse'),
    newUpdateSMBSecurityStrategyResponse,

    -- ** CancelRetrieval
    CancelRetrieval (CancelRetrieval'),
    newCancelRetrieval,
    CancelRetrievalResponse (CancelRetrievalResponse'),
    newCancelRetrievalResponse,

    -- ** DescribeVTLDevices (Paginated)
    DescribeVTLDevices (DescribeVTLDevices'),
    newDescribeVTLDevices,
    DescribeVTLDevicesResponse (DescribeVTLDevicesResponse'),
    newDescribeVTLDevicesResponse,

    -- ** StartAvailabilityMonitorTest
    StartAvailabilityMonitorTest (StartAvailabilityMonitorTest'),
    newStartAvailabilityMonitorTest,
    StartAvailabilityMonitorTestResponse (StartAvailabilityMonitorTestResponse'),
    newStartAvailabilityMonitorTestResponse,

    -- ** DeleteTapeArchive
    DeleteTapeArchive (DeleteTapeArchive'),
    newDeleteTapeArchive,
    DeleteTapeArchiveResponse (DeleteTapeArchiveResponse'),
    newDeleteTapeArchiveResponse,

    -- ** ListFileSystemAssociations (Paginated)
    ListFileSystemAssociations (ListFileSystemAssociations'),
    newListFileSystemAssociations,
    ListFileSystemAssociationsResponse (ListFileSystemAssociationsResponse'),
    newListFileSystemAssociationsResponse,

    -- ** UpdateSMBFileShare
    UpdateSMBFileShare (UpdateSMBFileShare'),
    newUpdateSMBFileShare,
    UpdateSMBFileShareResponse (UpdateSMBFileShareResponse'),
    newUpdateSMBFileShareResponse,

    -- ** DescribeNFSFileShares
    DescribeNFSFileShares (DescribeNFSFileShares'),
    newDescribeNFSFileShares,
    DescribeNFSFileSharesResponse (DescribeNFSFileSharesResponse'),
    newDescribeNFSFileSharesResponse,

    -- ** ListVolumeRecoveryPoints
    ListVolumeRecoveryPoints (ListVolumeRecoveryPoints'),
    newListVolumeRecoveryPoints,
    ListVolumeRecoveryPointsResponse (ListVolumeRecoveryPointsResponse'),
    newListVolumeRecoveryPointsResponse,

    -- ** ListTapes (Paginated)
    ListTapes (ListTapes'),
    newListTapes,
    ListTapesResponse (ListTapesResponse'),
    newListTapesResponse,

    -- ** ResetCache
    ResetCache (ResetCache'),
    newResetCache,
    ResetCacheResponse (ResetCacheResponse'),
    newResetCacheResponse,

    -- ** DescribeSMBFileShares
    DescribeSMBFileShares (DescribeSMBFileShares'),
    newDescribeSMBFileShares,
    DescribeSMBFileSharesResponse (DescribeSMBFileSharesResponse'),
    newDescribeSMBFileSharesResponse,

    -- ** ListGateways (Paginated)
    ListGateways (ListGateways'),
    newListGateways,
    ListGatewaysResponse (ListGatewaysResponse'),
    newListGatewaysResponse,

    -- ** DeleteTape
    DeleteTape (DeleteTape'),
    newDeleteTape,
    DeleteTapeResponse (DeleteTapeResponse'),
    newDeleteTapeResponse,

    -- ** ListLocalDisks
    ListLocalDisks (ListLocalDisks'),
    newListLocalDisks,
    ListLocalDisksResponse (ListLocalDisksResponse'),
    newListLocalDisksResponse,

    -- ** ListVolumes (Paginated)
    ListVolumes (ListVolumes'),
    newListVolumes,
    ListVolumesResponse (ListVolumesResponse'),
    newListVolumesResponse,

    -- ** UpdateBandwidthRateLimit
    UpdateBandwidthRateLimit (UpdateBandwidthRateLimit'),
    newUpdateBandwidthRateLimit,
    UpdateBandwidthRateLimitResponse (UpdateBandwidthRateLimitResponse'),
    newUpdateBandwidthRateLimitResponse,

    -- ** AddWorkingStorage
    AddWorkingStorage (AddWorkingStorage'),
    newAddWorkingStorage,
    AddWorkingStorageResponse (AddWorkingStorageResponse'),
    newAddWorkingStorageResponse,

    -- ** DescribeTapeRecoveryPoints (Paginated)
    DescribeTapeRecoveryPoints (DescribeTapeRecoveryPoints'),
    newDescribeTapeRecoveryPoints,
    DescribeTapeRecoveryPointsResponse (DescribeTapeRecoveryPointsResponse'),
    newDescribeTapeRecoveryPointsResponse,

    -- ** DeleteBandwidthRateLimit
    DeleteBandwidthRateLimit (DeleteBandwidthRateLimit'),
    newDeleteBandwidthRateLimit,
    DeleteBandwidthRateLimitResponse (DeleteBandwidthRateLimitResponse'),
    newDeleteBandwidthRateLimitResponse,

    -- ** ActivateGateway
    ActivateGateway (ActivateGateway'),
    newActivateGateway,
    ActivateGatewayResponse (ActivateGatewayResponse'),
    newActivateGatewayResponse,

    -- ** DescribeCache
    DescribeCache (DescribeCache'),
    newDescribeCache,
    DescribeCacheResponse (DescribeCacheResponse'),
    newDescribeCacheResponse,

    -- ** DeleteVolume
    DeleteVolume (DeleteVolume'),
    newDeleteVolume,
    DeleteVolumeResponse (DeleteVolumeResponse'),
    newDeleteVolumeResponse,

    -- * Types

    -- ** ActiveDirectoryStatus
    ActiveDirectoryStatus (..),

    -- ** AvailabilityMonitorTestStatus
    AvailabilityMonitorTestStatus (..),

    -- ** CaseSensitivity
    CaseSensitivity (..),

    -- ** FileShareType
    FileShareType (..),

    -- ** GatewayCapacity
    GatewayCapacity (..),

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
    AutomaticTapeCreationPolicyInfo (AutomaticTapeCreationPolicyInfo'),
    newAutomaticTapeCreationPolicyInfo,

    -- ** AutomaticTapeCreationRule
    AutomaticTapeCreationRule (AutomaticTapeCreationRule'),
    newAutomaticTapeCreationRule,

    -- ** BandwidthRateLimitInterval
    BandwidthRateLimitInterval (BandwidthRateLimitInterval'),
    newBandwidthRateLimitInterval,

    -- ** CacheAttributes
    CacheAttributes (CacheAttributes'),
    newCacheAttributes,

    -- ** CachediSCSIVolume
    CachediSCSIVolume (CachediSCSIVolume'),
    newCachediSCSIVolume,

    -- ** ChapInfo
    ChapInfo (ChapInfo'),
    newChapInfo,

    -- ** DeviceiSCSIAttributes
    DeviceiSCSIAttributes (DeviceiSCSIAttributes'),
    newDeviceiSCSIAttributes,

    -- ** Disk
    Disk (Disk'),
    newDisk,

    -- ** EndpointNetworkConfiguration
    EndpointNetworkConfiguration (EndpointNetworkConfiguration'),
    newEndpointNetworkConfiguration,

    -- ** FileShareInfo
    FileShareInfo (FileShareInfo'),
    newFileShareInfo,

    -- ** FileSystemAssociationInfo
    FileSystemAssociationInfo (FileSystemAssociationInfo'),
    newFileSystemAssociationInfo,

    -- ** FileSystemAssociationSummary
    FileSystemAssociationSummary (FileSystemAssociationSummary'),
    newFileSystemAssociationSummary,

    -- ** GatewayInfo
    GatewayInfo (GatewayInfo'),
    newGatewayInfo,

    -- ** NFSFileShareDefaults
    NFSFileShareDefaults (NFSFileShareDefaults'),
    newNFSFileShareDefaults,

    -- ** NFSFileShareInfo
    NFSFileShareInfo (NFSFileShareInfo'),
    newNFSFileShareInfo,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** PoolInfo
    PoolInfo (PoolInfo'),
    newPoolInfo,

    -- ** SMBFileShareInfo
    SMBFileShareInfo (SMBFileShareInfo'),
    newSMBFileShareInfo,

    -- ** StorediSCSIVolume
    StorediSCSIVolume (StorediSCSIVolume'),
    newStorediSCSIVolume,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Tape
    Tape (Tape'),
    newTape,

    -- ** TapeArchive
    TapeArchive (TapeArchive'),
    newTapeArchive,

    -- ** TapeInfo
    TapeInfo (TapeInfo'),
    newTapeInfo,

    -- ** TapeRecoveryPointInfo
    TapeRecoveryPointInfo (TapeRecoveryPointInfo'),
    newTapeRecoveryPointInfo,

    -- ** VTLDevice
    VTLDevice (VTLDevice'),
    newVTLDevice,

    -- ** VolumeInfo
    VolumeInfo (VolumeInfo'),
    newVolumeInfo,

    -- ** VolumeRecoveryPointInfo
    VolumeRecoveryPointInfo (VolumeRecoveryPointInfo'),
    newVolumeRecoveryPointInfo,

    -- ** VolumeiSCSIAttributes
    VolumeiSCSIAttributes (VolumeiSCSIAttributes'),
    newVolumeiSCSIAttributes,
  )
where

import Network.AWS.StorageGateway.ActivateGateway
import Network.AWS.StorageGateway.AddCache
import Network.AWS.StorageGateway.AddTagsToResource
import Network.AWS.StorageGateway.AddUploadBuffer
import Network.AWS.StorageGateway.AddWorkingStorage
import Network.AWS.StorageGateway.AssignTapePool
import Network.AWS.StorageGateway.AssociateFileSystem
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
import Network.AWS.StorageGateway.DescribeFileSystemAssociations
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
import Network.AWS.StorageGateway.DisassociateFileSystem
import Network.AWS.StorageGateway.JoinDomain
import Network.AWS.StorageGateway.Lens
import Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies
import Network.AWS.StorageGateway.ListFileShares
import Network.AWS.StorageGateway.ListFileSystemAssociations
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
import Network.AWS.StorageGateway.UpdateFileSystemAssociation
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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.StorageGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- to securely upload data to the Amazon Web Services Cloud for cost
-- effective backup and rapid disaster recovery.
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
--     Provides a list of each Amazon Web Services Region and the endpoints
--     available for use with Storage Gateway.
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
module Amazonka.StorageGateway
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerError
    _InternalServerError,

    -- ** InvalidGatewayRequestException
    _InvalidGatewayRequestException,

    -- ** ServiceUnavailableError
    _ServiceUnavailableError,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ActivateGateway
    ActivateGateway (ActivateGateway'),
    newActivateGateway,
    ActivateGatewayResponse (ActivateGatewayResponse'),
    newActivateGatewayResponse,

    -- ** AddCache
    AddCache (AddCache'),
    newAddCache,
    AddCacheResponse (AddCacheResponse'),
    newAddCacheResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** AddUploadBuffer
    AddUploadBuffer (AddUploadBuffer'),
    newAddUploadBuffer,
    AddUploadBufferResponse (AddUploadBufferResponse'),
    newAddUploadBufferResponse,

    -- ** AddWorkingStorage
    AddWorkingStorage (AddWorkingStorage'),
    newAddWorkingStorage,
    AddWorkingStorageResponse (AddWorkingStorageResponse'),
    newAddWorkingStorageResponse,

    -- ** AssignTapePool
    AssignTapePool (AssignTapePool'),
    newAssignTapePool,
    AssignTapePoolResponse (AssignTapePoolResponse'),
    newAssignTapePoolResponse,

    -- ** AssociateFileSystem
    AssociateFileSystem (AssociateFileSystem'),
    newAssociateFileSystem,
    AssociateFileSystemResponse (AssociateFileSystemResponse'),
    newAssociateFileSystemResponse,

    -- ** AttachVolume
    AttachVolume (AttachVolume'),
    newAttachVolume,
    AttachVolumeResponse (AttachVolumeResponse'),
    newAttachVolumeResponse,

    -- ** CancelArchival
    CancelArchival (CancelArchival'),
    newCancelArchival,
    CancelArchivalResponse (CancelArchivalResponse'),
    newCancelArchivalResponse,

    -- ** CancelRetrieval
    CancelRetrieval (CancelRetrieval'),
    newCancelRetrieval,
    CancelRetrievalResponse (CancelRetrievalResponse'),
    newCancelRetrievalResponse,

    -- ** CreateCachediSCSIVolume
    CreateCachediSCSIVolume (CreateCachediSCSIVolume'),
    newCreateCachediSCSIVolume,
    CreateCachediSCSIVolumeResponse (CreateCachediSCSIVolumeResponse'),
    newCreateCachediSCSIVolumeResponse,

    -- ** CreateNFSFileShare
    CreateNFSFileShare (CreateNFSFileShare'),
    newCreateNFSFileShare,
    CreateNFSFileShareResponse (CreateNFSFileShareResponse'),
    newCreateNFSFileShareResponse,

    -- ** CreateSMBFileShare
    CreateSMBFileShare (CreateSMBFileShare'),
    newCreateSMBFileShare,
    CreateSMBFileShareResponse (CreateSMBFileShareResponse'),
    newCreateSMBFileShareResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** CreateSnapshotFromVolumeRecoveryPoint
    CreateSnapshotFromVolumeRecoveryPoint (CreateSnapshotFromVolumeRecoveryPoint'),
    newCreateSnapshotFromVolumeRecoveryPoint,
    CreateSnapshotFromVolumeRecoveryPointResponse (CreateSnapshotFromVolumeRecoveryPointResponse'),
    newCreateSnapshotFromVolumeRecoveryPointResponse,

    -- ** CreateStorediSCSIVolume
    CreateStorediSCSIVolume (CreateStorediSCSIVolume'),
    newCreateStorediSCSIVolume,
    CreateStorediSCSIVolumeResponse (CreateStorediSCSIVolumeResponse'),
    newCreateStorediSCSIVolumeResponse,

    -- ** CreateTapePool
    CreateTapePool (CreateTapePool'),
    newCreateTapePool,
    CreateTapePoolResponse (CreateTapePoolResponse'),
    newCreateTapePoolResponse,

    -- ** CreateTapeWithBarcode
    CreateTapeWithBarcode (CreateTapeWithBarcode'),
    newCreateTapeWithBarcode,
    CreateTapeWithBarcodeResponse (CreateTapeWithBarcodeResponse'),
    newCreateTapeWithBarcodeResponse,

    -- ** CreateTapes
    CreateTapes (CreateTapes'),
    newCreateTapes,
    CreateTapesResponse (CreateTapesResponse'),
    newCreateTapesResponse,

    -- ** DeleteAutomaticTapeCreationPolicy
    DeleteAutomaticTapeCreationPolicy (DeleteAutomaticTapeCreationPolicy'),
    newDeleteAutomaticTapeCreationPolicy,
    DeleteAutomaticTapeCreationPolicyResponse (DeleteAutomaticTapeCreationPolicyResponse'),
    newDeleteAutomaticTapeCreationPolicyResponse,

    -- ** DeleteBandwidthRateLimit
    DeleteBandwidthRateLimit (DeleteBandwidthRateLimit'),
    newDeleteBandwidthRateLimit,
    DeleteBandwidthRateLimitResponse (DeleteBandwidthRateLimitResponse'),
    newDeleteBandwidthRateLimitResponse,

    -- ** DeleteChapCredentials
    DeleteChapCredentials (DeleteChapCredentials'),
    newDeleteChapCredentials,
    DeleteChapCredentialsResponse (DeleteChapCredentialsResponse'),
    newDeleteChapCredentialsResponse,

    -- ** DeleteFileShare
    DeleteFileShare (DeleteFileShare'),
    newDeleteFileShare,
    DeleteFileShareResponse (DeleteFileShareResponse'),
    newDeleteFileShareResponse,

    -- ** DeleteGateway
    DeleteGateway (DeleteGateway'),
    newDeleteGateway,
    DeleteGatewayResponse (DeleteGatewayResponse'),
    newDeleteGatewayResponse,

    -- ** DeleteSnapshotSchedule
    DeleteSnapshotSchedule (DeleteSnapshotSchedule'),
    newDeleteSnapshotSchedule,
    DeleteSnapshotScheduleResponse (DeleteSnapshotScheduleResponse'),
    newDeleteSnapshotScheduleResponse,

    -- ** DeleteTape
    DeleteTape (DeleteTape'),
    newDeleteTape,
    DeleteTapeResponse (DeleteTapeResponse'),
    newDeleteTapeResponse,

    -- ** DeleteTapeArchive
    DeleteTapeArchive (DeleteTapeArchive'),
    newDeleteTapeArchive,
    DeleteTapeArchiveResponse (DeleteTapeArchiveResponse'),
    newDeleteTapeArchiveResponse,

    -- ** DeleteTapePool
    DeleteTapePool (DeleteTapePool'),
    newDeleteTapePool,
    DeleteTapePoolResponse (DeleteTapePoolResponse'),
    newDeleteTapePoolResponse,

    -- ** DeleteVolume
    DeleteVolume (DeleteVolume'),
    newDeleteVolume,
    DeleteVolumeResponse (DeleteVolumeResponse'),
    newDeleteVolumeResponse,

    -- ** DescribeAvailabilityMonitorTest
    DescribeAvailabilityMonitorTest (DescribeAvailabilityMonitorTest'),
    newDescribeAvailabilityMonitorTest,
    DescribeAvailabilityMonitorTestResponse (DescribeAvailabilityMonitorTestResponse'),
    newDescribeAvailabilityMonitorTestResponse,

    -- ** DescribeBandwidthRateLimit
    DescribeBandwidthRateLimit (DescribeBandwidthRateLimit'),
    newDescribeBandwidthRateLimit,
    DescribeBandwidthRateLimitResponse (DescribeBandwidthRateLimitResponse'),
    newDescribeBandwidthRateLimitResponse,

    -- ** DescribeBandwidthRateLimitSchedule
    DescribeBandwidthRateLimitSchedule (DescribeBandwidthRateLimitSchedule'),
    newDescribeBandwidthRateLimitSchedule,
    DescribeBandwidthRateLimitScheduleResponse (DescribeBandwidthRateLimitScheduleResponse'),
    newDescribeBandwidthRateLimitScheduleResponse,

    -- ** DescribeCache
    DescribeCache (DescribeCache'),
    newDescribeCache,
    DescribeCacheResponse (DescribeCacheResponse'),
    newDescribeCacheResponse,

    -- ** DescribeCachediSCSIVolumes
    DescribeCachediSCSIVolumes (DescribeCachediSCSIVolumes'),
    newDescribeCachediSCSIVolumes,
    DescribeCachediSCSIVolumesResponse (DescribeCachediSCSIVolumesResponse'),
    newDescribeCachediSCSIVolumesResponse,

    -- ** DescribeChapCredentials
    DescribeChapCredentials (DescribeChapCredentials'),
    newDescribeChapCredentials,
    DescribeChapCredentialsResponse (DescribeChapCredentialsResponse'),
    newDescribeChapCredentialsResponse,

    -- ** DescribeFileSystemAssociations
    DescribeFileSystemAssociations (DescribeFileSystemAssociations'),
    newDescribeFileSystemAssociations,
    DescribeFileSystemAssociationsResponse (DescribeFileSystemAssociationsResponse'),
    newDescribeFileSystemAssociationsResponse,

    -- ** DescribeGatewayInformation
    DescribeGatewayInformation (DescribeGatewayInformation'),
    newDescribeGatewayInformation,
    DescribeGatewayInformationResponse (DescribeGatewayInformationResponse'),
    newDescribeGatewayInformationResponse,

    -- ** DescribeMaintenanceStartTime
    DescribeMaintenanceStartTime (DescribeMaintenanceStartTime'),
    newDescribeMaintenanceStartTime,
    DescribeMaintenanceStartTimeResponse (DescribeMaintenanceStartTimeResponse'),
    newDescribeMaintenanceStartTimeResponse,

    -- ** DescribeNFSFileShares
    DescribeNFSFileShares (DescribeNFSFileShares'),
    newDescribeNFSFileShares,
    DescribeNFSFileSharesResponse (DescribeNFSFileSharesResponse'),
    newDescribeNFSFileSharesResponse,

    -- ** DescribeSMBFileShares
    DescribeSMBFileShares (DescribeSMBFileShares'),
    newDescribeSMBFileShares,
    DescribeSMBFileSharesResponse (DescribeSMBFileSharesResponse'),
    newDescribeSMBFileSharesResponse,

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

    -- ** DescribeStorediSCSIVolumes
    DescribeStorediSCSIVolumes (DescribeStorediSCSIVolumes'),
    newDescribeStorediSCSIVolumes,
    DescribeStorediSCSIVolumesResponse (DescribeStorediSCSIVolumesResponse'),
    newDescribeStorediSCSIVolumesResponse,

    -- ** DescribeTapeArchives (Paginated)
    DescribeTapeArchives (DescribeTapeArchives'),
    newDescribeTapeArchives,
    DescribeTapeArchivesResponse (DescribeTapeArchivesResponse'),
    newDescribeTapeArchivesResponse,

    -- ** DescribeTapeRecoveryPoints (Paginated)
    DescribeTapeRecoveryPoints (DescribeTapeRecoveryPoints'),
    newDescribeTapeRecoveryPoints,
    DescribeTapeRecoveryPointsResponse (DescribeTapeRecoveryPointsResponse'),
    newDescribeTapeRecoveryPointsResponse,

    -- ** DescribeTapes (Paginated)
    DescribeTapes (DescribeTapes'),
    newDescribeTapes,
    DescribeTapesResponse (DescribeTapesResponse'),
    newDescribeTapesResponse,

    -- ** DescribeUploadBuffer
    DescribeUploadBuffer (DescribeUploadBuffer'),
    newDescribeUploadBuffer,
    DescribeUploadBufferResponse (DescribeUploadBufferResponse'),
    newDescribeUploadBufferResponse,

    -- ** DescribeVTLDevices (Paginated)
    DescribeVTLDevices (DescribeVTLDevices'),
    newDescribeVTLDevices,
    DescribeVTLDevicesResponse (DescribeVTLDevicesResponse'),
    newDescribeVTLDevicesResponse,

    -- ** DescribeWorkingStorage
    DescribeWorkingStorage (DescribeWorkingStorage'),
    newDescribeWorkingStorage,
    DescribeWorkingStorageResponse (DescribeWorkingStorageResponse'),
    newDescribeWorkingStorageResponse,

    -- ** DetachVolume
    DetachVolume (DetachVolume'),
    newDetachVolume,
    DetachVolumeResponse (DetachVolumeResponse'),
    newDetachVolumeResponse,

    -- ** DisableGateway
    DisableGateway (DisableGateway'),
    newDisableGateway,
    DisableGatewayResponse (DisableGatewayResponse'),
    newDisableGatewayResponse,

    -- ** DisassociateFileSystem
    DisassociateFileSystem (DisassociateFileSystem'),
    newDisassociateFileSystem,
    DisassociateFileSystemResponse (DisassociateFileSystemResponse'),
    newDisassociateFileSystemResponse,

    -- ** JoinDomain
    JoinDomain (JoinDomain'),
    newJoinDomain,
    JoinDomainResponse (JoinDomainResponse'),
    newJoinDomainResponse,

    -- ** ListAutomaticTapeCreationPolicies
    ListAutomaticTapeCreationPolicies (ListAutomaticTapeCreationPolicies'),
    newListAutomaticTapeCreationPolicies,
    ListAutomaticTapeCreationPoliciesResponse (ListAutomaticTapeCreationPoliciesResponse'),
    newListAutomaticTapeCreationPoliciesResponse,

    -- ** ListFileShares (Paginated)
    ListFileShares (ListFileShares'),
    newListFileShares,
    ListFileSharesResponse (ListFileSharesResponse'),
    newListFileSharesResponse,

    -- ** ListFileSystemAssociations (Paginated)
    ListFileSystemAssociations (ListFileSystemAssociations'),
    newListFileSystemAssociations,
    ListFileSystemAssociationsResponse (ListFileSystemAssociationsResponse'),
    newListFileSystemAssociationsResponse,

    -- ** ListGateways (Paginated)
    ListGateways (ListGateways'),
    newListGateways,
    ListGatewaysResponse (ListGatewaysResponse'),
    newListGatewaysResponse,

    -- ** ListLocalDisks
    ListLocalDisks (ListLocalDisks'),
    newListLocalDisks,
    ListLocalDisksResponse (ListLocalDisksResponse'),
    newListLocalDisksResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTapePools (Paginated)
    ListTapePools (ListTapePools'),
    newListTapePools,
    ListTapePoolsResponse (ListTapePoolsResponse'),
    newListTapePoolsResponse,

    -- ** ListTapes (Paginated)
    ListTapes (ListTapes'),
    newListTapes,
    ListTapesResponse (ListTapesResponse'),
    newListTapesResponse,

    -- ** ListVolumeInitiators
    ListVolumeInitiators (ListVolumeInitiators'),
    newListVolumeInitiators,
    ListVolumeInitiatorsResponse (ListVolumeInitiatorsResponse'),
    newListVolumeInitiatorsResponse,

    -- ** ListVolumeRecoveryPoints
    ListVolumeRecoveryPoints (ListVolumeRecoveryPoints'),
    newListVolumeRecoveryPoints,
    ListVolumeRecoveryPointsResponse (ListVolumeRecoveryPointsResponse'),
    newListVolumeRecoveryPointsResponse,

    -- ** ListVolumes (Paginated)
    ListVolumes (ListVolumes'),
    newListVolumes,
    ListVolumesResponse (ListVolumesResponse'),
    newListVolumesResponse,

    -- ** NotifyWhenUploaded
    NotifyWhenUploaded (NotifyWhenUploaded'),
    newNotifyWhenUploaded,
    NotifyWhenUploadedResponse (NotifyWhenUploadedResponse'),
    newNotifyWhenUploadedResponse,

    -- ** RefreshCache
    RefreshCache (RefreshCache'),
    newRefreshCache,
    RefreshCacheResponse (RefreshCacheResponse'),
    newRefreshCacheResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** ResetCache
    ResetCache (ResetCache'),
    newResetCache,
    ResetCacheResponse (ResetCacheResponse'),
    newResetCacheResponse,

    -- ** RetrieveTapeArchive
    RetrieveTapeArchive (RetrieveTapeArchive'),
    newRetrieveTapeArchive,
    RetrieveTapeArchiveResponse (RetrieveTapeArchiveResponse'),
    newRetrieveTapeArchiveResponse,

    -- ** RetrieveTapeRecoveryPoint
    RetrieveTapeRecoveryPoint (RetrieveTapeRecoveryPoint'),
    newRetrieveTapeRecoveryPoint,
    RetrieveTapeRecoveryPointResponse (RetrieveTapeRecoveryPointResponse'),
    newRetrieveTapeRecoveryPointResponse,

    -- ** SetLocalConsolePassword
    SetLocalConsolePassword (SetLocalConsolePassword'),
    newSetLocalConsolePassword,
    SetLocalConsolePasswordResponse (SetLocalConsolePasswordResponse'),
    newSetLocalConsolePasswordResponse,

    -- ** SetSMBGuestPassword
    SetSMBGuestPassword (SetSMBGuestPassword'),
    newSetSMBGuestPassword,
    SetSMBGuestPasswordResponse (SetSMBGuestPasswordResponse'),
    newSetSMBGuestPasswordResponse,

    -- ** ShutdownGateway
    ShutdownGateway (ShutdownGateway'),
    newShutdownGateway,
    ShutdownGatewayResponse (ShutdownGatewayResponse'),
    newShutdownGatewayResponse,

    -- ** StartAvailabilityMonitorTest
    StartAvailabilityMonitorTest (StartAvailabilityMonitorTest'),
    newStartAvailabilityMonitorTest,
    StartAvailabilityMonitorTestResponse (StartAvailabilityMonitorTestResponse'),
    newStartAvailabilityMonitorTestResponse,

    -- ** StartGateway
    StartGateway (StartGateway'),
    newStartGateway,
    StartGatewayResponse (StartGatewayResponse'),
    newStartGatewayResponse,

    -- ** UpdateAutomaticTapeCreationPolicy
    UpdateAutomaticTapeCreationPolicy (UpdateAutomaticTapeCreationPolicy'),
    newUpdateAutomaticTapeCreationPolicy,
    UpdateAutomaticTapeCreationPolicyResponse (UpdateAutomaticTapeCreationPolicyResponse'),
    newUpdateAutomaticTapeCreationPolicyResponse,

    -- ** UpdateBandwidthRateLimit
    UpdateBandwidthRateLimit (UpdateBandwidthRateLimit'),
    newUpdateBandwidthRateLimit,
    UpdateBandwidthRateLimitResponse (UpdateBandwidthRateLimitResponse'),
    newUpdateBandwidthRateLimitResponse,

    -- ** UpdateBandwidthRateLimitSchedule
    UpdateBandwidthRateLimitSchedule (UpdateBandwidthRateLimitSchedule'),
    newUpdateBandwidthRateLimitSchedule,
    UpdateBandwidthRateLimitScheduleResponse (UpdateBandwidthRateLimitScheduleResponse'),
    newUpdateBandwidthRateLimitScheduleResponse,

    -- ** UpdateChapCredentials
    UpdateChapCredentials (UpdateChapCredentials'),
    newUpdateChapCredentials,
    UpdateChapCredentialsResponse (UpdateChapCredentialsResponse'),
    newUpdateChapCredentialsResponse,

    -- ** UpdateFileSystemAssociation
    UpdateFileSystemAssociation (UpdateFileSystemAssociation'),
    newUpdateFileSystemAssociation,
    UpdateFileSystemAssociationResponse (UpdateFileSystemAssociationResponse'),
    newUpdateFileSystemAssociationResponse,

    -- ** UpdateGatewayInformation
    UpdateGatewayInformation (UpdateGatewayInformation'),
    newUpdateGatewayInformation,
    UpdateGatewayInformationResponse (UpdateGatewayInformationResponse'),
    newUpdateGatewayInformationResponse,

    -- ** UpdateGatewaySoftwareNow
    UpdateGatewaySoftwareNow (UpdateGatewaySoftwareNow'),
    newUpdateGatewaySoftwareNow,
    UpdateGatewaySoftwareNowResponse (UpdateGatewaySoftwareNowResponse'),
    newUpdateGatewaySoftwareNowResponse,

    -- ** UpdateMaintenanceStartTime
    UpdateMaintenanceStartTime (UpdateMaintenanceStartTime'),
    newUpdateMaintenanceStartTime,
    UpdateMaintenanceStartTimeResponse (UpdateMaintenanceStartTimeResponse'),
    newUpdateMaintenanceStartTimeResponse,

    -- ** UpdateNFSFileShare
    UpdateNFSFileShare (UpdateNFSFileShare'),
    newUpdateNFSFileShare,
    UpdateNFSFileShareResponse (UpdateNFSFileShareResponse'),
    newUpdateNFSFileShareResponse,

    -- ** UpdateSMBFileShare
    UpdateSMBFileShare (UpdateSMBFileShare'),
    newUpdateSMBFileShare,
    UpdateSMBFileShareResponse (UpdateSMBFileShareResponse'),
    newUpdateSMBFileShareResponse,

    -- ** UpdateSMBFileShareVisibility
    UpdateSMBFileShareVisibility (UpdateSMBFileShareVisibility'),
    newUpdateSMBFileShareVisibility,
    UpdateSMBFileShareVisibilityResponse (UpdateSMBFileShareVisibilityResponse'),
    newUpdateSMBFileShareVisibilityResponse,

    -- ** UpdateSMBLocalGroups
    UpdateSMBLocalGroups (UpdateSMBLocalGroups'),
    newUpdateSMBLocalGroups,
    UpdateSMBLocalGroupsResponse (UpdateSMBLocalGroupsResponse'),
    newUpdateSMBLocalGroupsResponse,

    -- ** UpdateSMBSecurityStrategy
    UpdateSMBSecurityStrategy (UpdateSMBSecurityStrategy'),
    newUpdateSMBSecurityStrategy,
    UpdateSMBSecurityStrategyResponse (UpdateSMBSecurityStrategyResponse'),
    newUpdateSMBSecurityStrategyResponse,

    -- ** UpdateSnapshotSchedule
    UpdateSnapshotSchedule (UpdateSnapshotSchedule'),
    newUpdateSnapshotSchedule,
    UpdateSnapshotScheduleResponse (UpdateSnapshotScheduleResponse'),
    newUpdateSnapshotScheduleResponse,

    -- ** UpdateVTLDeviceType
    UpdateVTLDeviceType (UpdateVTLDeviceType'),
    newUpdateVTLDeviceType,
    UpdateVTLDeviceTypeResponse (UpdateVTLDeviceTypeResponse'),
    newUpdateVTLDeviceTypeResponse,

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

    -- ** FileSystemAssociationStatusDetail
    FileSystemAssociationStatusDetail (FileSystemAssociationStatusDetail'),
    newFileSystemAssociationStatusDetail,

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

    -- ** SMBLocalGroups
    SMBLocalGroups (SMBLocalGroups'),
    newSMBLocalGroups,

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

import Amazonka.StorageGateway.ActivateGateway
import Amazonka.StorageGateway.AddCache
import Amazonka.StorageGateway.AddTagsToResource
import Amazonka.StorageGateway.AddUploadBuffer
import Amazonka.StorageGateway.AddWorkingStorage
import Amazonka.StorageGateway.AssignTapePool
import Amazonka.StorageGateway.AssociateFileSystem
import Amazonka.StorageGateway.AttachVolume
import Amazonka.StorageGateway.CancelArchival
import Amazonka.StorageGateway.CancelRetrieval
import Amazonka.StorageGateway.CreateCachediSCSIVolume
import Amazonka.StorageGateway.CreateNFSFileShare
import Amazonka.StorageGateway.CreateSMBFileShare
import Amazonka.StorageGateway.CreateSnapshot
import Amazonka.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
import Amazonka.StorageGateway.CreateStorediSCSIVolume
import Amazonka.StorageGateway.CreateTapePool
import Amazonka.StorageGateway.CreateTapeWithBarcode
import Amazonka.StorageGateway.CreateTapes
import Amazonka.StorageGateway.DeleteAutomaticTapeCreationPolicy
import Amazonka.StorageGateway.DeleteBandwidthRateLimit
import Amazonka.StorageGateway.DeleteChapCredentials
import Amazonka.StorageGateway.DeleteFileShare
import Amazonka.StorageGateway.DeleteGateway
import Amazonka.StorageGateway.DeleteSnapshotSchedule
import Amazonka.StorageGateway.DeleteTape
import Amazonka.StorageGateway.DeleteTapeArchive
import Amazonka.StorageGateway.DeleteTapePool
import Amazonka.StorageGateway.DeleteVolume
import Amazonka.StorageGateway.DescribeAvailabilityMonitorTest
import Amazonka.StorageGateway.DescribeBandwidthRateLimit
import Amazonka.StorageGateway.DescribeBandwidthRateLimitSchedule
import Amazonka.StorageGateway.DescribeCache
import Amazonka.StorageGateway.DescribeCachediSCSIVolumes
import Amazonka.StorageGateway.DescribeChapCredentials
import Amazonka.StorageGateway.DescribeFileSystemAssociations
import Amazonka.StorageGateway.DescribeGatewayInformation
import Amazonka.StorageGateway.DescribeMaintenanceStartTime
import Amazonka.StorageGateway.DescribeNFSFileShares
import Amazonka.StorageGateway.DescribeSMBFileShares
import Amazonka.StorageGateway.DescribeSMBSettings
import Amazonka.StorageGateway.DescribeSnapshotSchedule
import Amazonka.StorageGateway.DescribeStorediSCSIVolumes
import Amazonka.StorageGateway.DescribeTapeArchives
import Amazonka.StorageGateway.DescribeTapeRecoveryPoints
import Amazonka.StorageGateway.DescribeTapes
import Amazonka.StorageGateway.DescribeUploadBuffer
import Amazonka.StorageGateway.DescribeVTLDevices
import Amazonka.StorageGateway.DescribeWorkingStorage
import Amazonka.StorageGateway.DetachVolume
import Amazonka.StorageGateway.DisableGateway
import Amazonka.StorageGateway.DisassociateFileSystem
import Amazonka.StorageGateway.JoinDomain
import Amazonka.StorageGateway.Lens
import Amazonka.StorageGateway.ListAutomaticTapeCreationPolicies
import Amazonka.StorageGateway.ListFileShares
import Amazonka.StorageGateway.ListFileSystemAssociations
import Amazonka.StorageGateway.ListGateways
import Amazonka.StorageGateway.ListLocalDisks
import Amazonka.StorageGateway.ListTagsForResource
import Amazonka.StorageGateway.ListTapePools
import Amazonka.StorageGateway.ListTapes
import Amazonka.StorageGateway.ListVolumeInitiators
import Amazonka.StorageGateway.ListVolumeRecoveryPoints
import Amazonka.StorageGateway.ListVolumes
import Amazonka.StorageGateway.NotifyWhenUploaded
import Amazonka.StorageGateway.RefreshCache
import Amazonka.StorageGateway.RemoveTagsFromResource
import Amazonka.StorageGateway.ResetCache
import Amazonka.StorageGateway.RetrieveTapeArchive
import Amazonka.StorageGateway.RetrieveTapeRecoveryPoint
import Amazonka.StorageGateway.SetLocalConsolePassword
import Amazonka.StorageGateway.SetSMBGuestPassword
import Amazonka.StorageGateway.ShutdownGateway
import Amazonka.StorageGateway.StartAvailabilityMonitorTest
import Amazonka.StorageGateway.StartGateway
import Amazonka.StorageGateway.Types
import Amazonka.StorageGateway.UpdateAutomaticTapeCreationPolicy
import Amazonka.StorageGateway.UpdateBandwidthRateLimit
import Amazonka.StorageGateway.UpdateBandwidthRateLimitSchedule
import Amazonka.StorageGateway.UpdateChapCredentials
import Amazonka.StorageGateway.UpdateFileSystemAssociation
import Amazonka.StorageGateway.UpdateGatewayInformation
import Amazonka.StorageGateway.UpdateGatewaySoftwareNow
import Amazonka.StorageGateway.UpdateMaintenanceStartTime
import Amazonka.StorageGateway.UpdateNFSFileShare
import Amazonka.StorageGateway.UpdateSMBFileShare
import Amazonka.StorageGateway.UpdateSMBFileShareVisibility
import Amazonka.StorageGateway.UpdateSMBLocalGroups
import Amazonka.StorageGateway.UpdateSMBSecurityStrategy
import Amazonka.StorageGateway.UpdateSnapshotSchedule
import Amazonka.StorageGateway.UpdateVTLDeviceType
import Amazonka.StorageGateway.Waiters

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

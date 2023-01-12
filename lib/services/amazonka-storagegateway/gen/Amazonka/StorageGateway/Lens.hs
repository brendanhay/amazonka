{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.StorageGateway.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Lens
  ( -- * Operations

    -- ** ActivateGateway
    activateGateway_gatewayType,
    activateGateway_mediumChangerType,
    activateGateway_tags,
    activateGateway_tapeDriveType,
    activateGateway_activationKey,
    activateGateway_gatewayName,
    activateGateway_gatewayTimezone,
    activateGateway_gatewayRegion,
    activateGatewayResponse_gatewayARN,
    activateGatewayResponse_httpStatus,

    -- ** AddCache
    addCache_gatewayARN,
    addCache_diskIds,
    addCacheResponse_gatewayARN,
    addCacheResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceARN,
    addTagsToResource_tags,
    addTagsToResourceResponse_resourceARN,
    addTagsToResourceResponse_httpStatus,

    -- ** AddUploadBuffer
    addUploadBuffer_gatewayARN,
    addUploadBuffer_diskIds,
    addUploadBufferResponse_gatewayARN,
    addUploadBufferResponse_httpStatus,

    -- ** AddWorkingStorage
    addWorkingStorage_gatewayARN,
    addWorkingStorage_diskIds,
    addWorkingStorageResponse_gatewayARN,
    addWorkingStorageResponse_httpStatus,

    -- ** AssignTapePool
    assignTapePool_bypassGovernanceRetention,
    assignTapePool_tapeARN,
    assignTapePool_poolId,
    assignTapePoolResponse_tapeARN,
    assignTapePoolResponse_httpStatus,

    -- ** AssociateFileSystem
    associateFileSystem_auditDestinationARN,
    associateFileSystem_cacheAttributes,
    associateFileSystem_endpointNetworkConfiguration,
    associateFileSystem_tags,
    associateFileSystem_userName,
    associateFileSystem_password,
    associateFileSystem_clientToken,
    associateFileSystem_gatewayARN,
    associateFileSystem_locationARN,
    associateFileSystemResponse_fileSystemAssociationARN,
    associateFileSystemResponse_httpStatus,

    -- ** AttachVolume
    attachVolume_diskId,
    attachVolume_targetName,
    attachVolume_gatewayARN,
    attachVolume_volumeARN,
    attachVolume_networkInterfaceId,
    attachVolumeResponse_targetARN,
    attachVolumeResponse_volumeARN,
    attachVolumeResponse_httpStatus,

    -- ** CancelArchival
    cancelArchival_gatewayARN,
    cancelArchival_tapeARN,
    cancelArchivalResponse_tapeARN,
    cancelArchivalResponse_httpStatus,

    -- ** CancelRetrieval
    cancelRetrieval_gatewayARN,
    cancelRetrieval_tapeARN,
    cancelRetrievalResponse_tapeARN,
    cancelRetrievalResponse_httpStatus,

    -- ** CreateCachediSCSIVolume
    createCachediSCSIVolume_kmsEncrypted,
    createCachediSCSIVolume_kmsKey,
    createCachediSCSIVolume_snapshotId,
    createCachediSCSIVolume_sourceVolumeARN,
    createCachediSCSIVolume_tags,
    createCachediSCSIVolume_gatewayARN,
    createCachediSCSIVolume_volumeSizeInBytes,
    createCachediSCSIVolume_targetName,
    createCachediSCSIVolume_networkInterfaceId,
    createCachediSCSIVolume_clientToken,
    createCachediSCSIVolumeResponse_targetARN,
    createCachediSCSIVolumeResponse_volumeARN,
    createCachediSCSIVolumeResponse_httpStatus,

    -- ** CreateNFSFileShare
    createNFSFileShare_auditDestinationARN,
    createNFSFileShare_bucketRegion,
    createNFSFileShare_cacheAttributes,
    createNFSFileShare_clientList,
    createNFSFileShare_defaultStorageClass,
    createNFSFileShare_fileShareName,
    createNFSFileShare_guessMIMETypeEnabled,
    createNFSFileShare_kmsEncrypted,
    createNFSFileShare_kmsKey,
    createNFSFileShare_nFSFileShareDefaults,
    createNFSFileShare_notificationPolicy,
    createNFSFileShare_objectACL,
    createNFSFileShare_readOnly,
    createNFSFileShare_requesterPays,
    createNFSFileShare_squash,
    createNFSFileShare_tags,
    createNFSFileShare_vPCEndpointDNSName,
    createNFSFileShare_clientToken,
    createNFSFileShare_gatewayARN,
    createNFSFileShare_role,
    createNFSFileShare_locationARN,
    createNFSFileShareResponse_fileShareARN,
    createNFSFileShareResponse_httpStatus,

    -- ** CreateSMBFileShare
    createSMBFileShare_accessBasedEnumeration,
    createSMBFileShare_adminUserList,
    createSMBFileShare_auditDestinationARN,
    createSMBFileShare_authentication,
    createSMBFileShare_bucketRegion,
    createSMBFileShare_cacheAttributes,
    createSMBFileShare_caseSensitivity,
    createSMBFileShare_defaultStorageClass,
    createSMBFileShare_fileShareName,
    createSMBFileShare_guessMIMETypeEnabled,
    createSMBFileShare_invalidUserList,
    createSMBFileShare_kmsEncrypted,
    createSMBFileShare_kmsKey,
    createSMBFileShare_notificationPolicy,
    createSMBFileShare_objectACL,
    createSMBFileShare_oplocksEnabled,
    createSMBFileShare_readOnly,
    createSMBFileShare_requesterPays,
    createSMBFileShare_sMBACLEnabled,
    createSMBFileShare_tags,
    createSMBFileShare_vPCEndpointDNSName,
    createSMBFileShare_validUserList,
    createSMBFileShare_clientToken,
    createSMBFileShare_gatewayARN,
    createSMBFileShare_role,
    createSMBFileShare_locationARN,
    createSMBFileShareResponse_fileShareARN,
    createSMBFileShareResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_tags,
    createSnapshot_volumeARN,
    createSnapshot_snapshotDescription,
    createSnapshotResponse_snapshotId,
    createSnapshotResponse_volumeARN,
    createSnapshotResponse_httpStatus,

    -- ** CreateSnapshotFromVolumeRecoveryPoint
    createSnapshotFromVolumeRecoveryPoint_tags,
    createSnapshotFromVolumeRecoveryPoint_volumeARN,
    createSnapshotFromVolumeRecoveryPoint_snapshotDescription,
    createSnapshotFromVolumeRecoveryPointResponse_snapshotId,
    createSnapshotFromVolumeRecoveryPointResponse_volumeARN,
    createSnapshotFromVolumeRecoveryPointResponse_volumeRecoveryPointTime,
    createSnapshotFromVolumeRecoveryPointResponse_httpStatus,

    -- ** CreateStorediSCSIVolume
    createStorediSCSIVolume_kmsEncrypted,
    createStorediSCSIVolume_kmsKey,
    createStorediSCSIVolume_snapshotId,
    createStorediSCSIVolume_tags,
    createStorediSCSIVolume_gatewayARN,
    createStorediSCSIVolume_diskId,
    createStorediSCSIVolume_preserveExistingData,
    createStorediSCSIVolume_targetName,
    createStorediSCSIVolume_networkInterfaceId,
    createStorediSCSIVolumeResponse_targetARN,
    createStorediSCSIVolumeResponse_volumeARN,
    createStorediSCSIVolumeResponse_volumeSizeInBytes,
    createStorediSCSIVolumeResponse_httpStatus,

    -- ** CreateTapePool
    createTapePool_retentionLockTimeInDays,
    createTapePool_retentionLockType,
    createTapePool_tags,
    createTapePool_poolName,
    createTapePool_storageClass,
    createTapePoolResponse_poolARN,
    createTapePoolResponse_httpStatus,

    -- ** CreateTapeWithBarcode
    createTapeWithBarcode_kmsEncrypted,
    createTapeWithBarcode_kmsKey,
    createTapeWithBarcode_poolId,
    createTapeWithBarcode_tags,
    createTapeWithBarcode_worm,
    createTapeWithBarcode_gatewayARN,
    createTapeWithBarcode_tapeSizeInBytes,
    createTapeWithBarcode_tapeBarcode,
    createTapeWithBarcodeResponse_tapeARN,
    createTapeWithBarcodeResponse_httpStatus,

    -- ** CreateTapes
    createTapes_kmsEncrypted,
    createTapes_kmsKey,
    createTapes_poolId,
    createTapes_tags,
    createTapes_worm,
    createTapes_gatewayARN,
    createTapes_tapeSizeInBytes,
    createTapes_clientToken,
    createTapes_numTapesToCreate,
    createTapes_tapeBarcodePrefix,
    createTapesResponse_tapeARNs,
    createTapesResponse_httpStatus,

    -- ** DeleteAutomaticTapeCreationPolicy
    deleteAutomaticTapeCreationPolicy_gatewayARN,
    deleteAutomaticTapeCreationPolicyResponse_gatewayARN,
    deleteAutomaticTapeCreationPolicyResponse_httpStatus,

    -- ** DeleteBandwidthRateLimit
    deleteBandwidthRateLimit_gatewayARN,
    deleteBandwidthRateLimit_bandwidthType,
    deleteBandwidthRateLimitResponse_gatewayARN,
    deleteBandwidthRateLimitResponse_httpStatus,

    -- ** DeleteChapCredentials
    deleteChapCredentials_targetARN,
    deleteChapCredentials_initiatorName,
    deleteChapCredentialsResponse_initiatorName,
    deleteChapCredentialsResponse_targetARN,
    deleteChapCredentialsResponse_httpStatus,

    -- ** DeleteFileShare
    deleteFileShare_forceDelete,
    deleteFileShare_fileShareARN,
    deleteFileShareResponse_fileShareARN,
    deleteFileShareResponse_httpStatus,

    -- ** DeleteGateway
    deleteGateway_gatewayARN,
    deleteGatewayResponse_gatewayARN,
    deleteGatewayResponse_httpStatus,

    -- ** DeleteSnapshotSchedule
    deleteSnapshotSchedule_volumeARN,
    deleteSnapshotScheduleResponse_volumeARN,
    deleteSnapshotScheduleResponse_httpStatus,

    -- ** DeleteTape
    deleteTape_bypassGovernanceRetention,
    deleteTape_gatewayARN,
    deleteTape_tapeARN,
    deleteTapeResponse_tapeARN,
    deleteTapeResponse_httpStatus,

    -- ** DeleteTapeArchive
    deleteTapeArchive_bypassGovernanceRetention,
    deleteTapeArchive_tapeARN,
    deleteTapeArchiveResponse_tapeARN,
    deleteTapeArchiveResponse_httpStatus,

    -- ** DeleteTapePool
    deleteTapePool_poolARN,
    deleteTapePoolResponse_poolARN,
    deleteTapePoolResponse_httpStatus,

    -- ** DeleteVolume
    deleteVolume_volumeARN,
    deleteVolumeResponse_volumeARN,
    deleteVolumeResponse_httpStatus,

    -- ** DescribeAvailabilityMonitorTest
    describeAvailabilityMonitorTest_gatewayARN,
    describeAvailabilityMonitorTestResponse_gatewayARN,
    describeAvailabilityMonitorTestResponse_startTime,
    describeAvailabilityMonitorTestResponse_status,
    describeAvailabilityMonitorTestResponse_httpStatus,

    -- ** DescribeBandwidthRateLimit
    describeBandwidthRateLimit_gatewayARN,
    describeBandwidthRateLimitResponse_averageDownloadRateLimitInBitsPerSec,
    describeBandwidthRateLimitResponse_averageUploadRateLimitInBitsPerSec,
    describeBandwidthRateLimitResponse_gatewayARN,
    describeBandwidthRateLimitResponse_httpStatus,

    -- ** DescribeBandwidthRateLimitSchedule
    describeBandwidthRateLimitSchedule_gatewayARN,
    describeBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals,
    describeBandwidthRateLimitScheduleResponse_gatewayARN,
    describeBandwidthRateLimitScheduleResponse_httpStatus,

    -- ** DescribeCache
    describeCache_gatewayARN,
    describeCacheResponse_cacheAllocatedInBytes,
    describeCacheResponse_cacheDirtyPercentage,
    describeCacheResponse_cacheHitPercentage,
    describeCacheResponse_cacheMissPercentage,
    describeCacheResponse_cacheUsedPercentage,
    describeCacheResponse_diskIds,
    describeCacheResponse_gatewayARN,
    describeCacheResponse_httpStatus,

    -- ** DescribeCachediSCSIVolumes
    describeCachediSCSIVolumes_volumeARNs,
    describeCachediSCSIVolumesResponse_cachediSCSIVolumes,
    describeCachediSCSIVolumesResponse_httpStatus,

    -- ** DescribeChapCredentials
    describeChapCredentials_targetARN,
    describeChapCredentialsResponse_chapCredentials,
    describeChapCredentialsResponse_httpStatus,

    -- ** DescribeFileSystemAssociations
    describeFileSystemAssociations_fileSystemAssociationARNList,
    describeFileSystemAssociationsResponse_fileSystemAssociationInfoList,
    describeFileSystemAssociationsResponse_httpStatus,

    -- ** DescribeGatewayInformation
    describeGatewayInformation_gatewayARN,
    describeGatewayInformationResponse_cloudWatchLogGroupARN,
    describeGatewayInformationResponse_deprecationDate,
    describeGatewayInformationResponse_ec2InstanceId,
    describeGatewayInformationResponse_ec2InstanceRegion,
    describeGatewayInformationResponse_endpointType,
    describeGatewayInformationResponse_gatewayARN,
    describeGatewayInformationResponse_gatewayCapacity,
    describeGatewayInformationResponse_gatewayId,
    describeGatewayInformationResponse_gatewayName,
    describeGatewayInformationResponse_gatewayNetworkInterfaces,
    describeGatewayInformationResponse_gatewayState,
    describeGatewayInformationResponse_gatewayTimezone,
    describeGatewayInformationResponse_gatewayType,
    describeGatewayInformationResponse_hostEnvironment,
    describeGatewayInformationResponse_hostEnvironmentId,
    describeGatewayInformationResponse_lastSoftwareUpdate,
    describeGatewayInformationResponse_nextUpdateAvailabilityDate,
    describeGatewayInformationResponse_softwareUpdatesEndDate,
    describeGatewayInformationResponse_supportedGatewayCapacities,
    describeGatewayInformationResponse_tags,
    describeGatewayInformationResponse_vPCEndpoint,
    describeGatewayInformationResponse_httpStatus,

    -- ** DescribeMaintenanceStartTime
    describeMaintenanceStartTime_gatewayARN,
    describeMaintenanceStartTimeResponse_dayOfMonth,
    describeMaintenanceStartTimeResponse_dayOfWeek,
    describeMaintenanceStartTimeResponse_gatewayARN,
    describeMaintenanceStartTimeResponse_hourOfDay,
    describeMaintenanceStartTimeResponse_minuteOfHour,
    describeMaintenanceStartTimeResponse_timezone,
    describeMaintenanceStartTimeResponse_httpStatus,

    -- ** DescribeNFSFileShares
    describeNFSFileShares_fileShareARNList,
    describeNFSFileSharesResponse_nFSFileShareInfoList,
    describeNFSFileSharesResponse_httpStatus,

    -- ** DescribeSMBFileShares
    describeSMBFileShares_fileShareARNList,
    describeSMBFileSharesResponse_sMBFileShareInfoList,
    describeSMBFileSharesResponse_httpStatus,

    -- ** DescribeSMBSettings
    describeSMBSettings_gatewayARN,
    describeSMBSettingsResponse_activeDirectoryStatus,
    describeSMBSettingsResponse_domainName,
    describeSMBSettingsResponse_fileSharesVisible,
    describeSMBSettingsResponse_gatewayARN,
    describeSMBSettingsResponse_sMBGuestPasswordSet,
    describeSMBSettingsResponse_sMBLocalGroups,
    describeSMBSettingsResponse_sMBSecurityStrategy,
    describeSMBSettingsResponse_httpStatus,

    -- ** DescribeSnapshotSchedule
    describeSnapshotSchedule_volumeARN,
    describeSnapshotScheduleResponse_description,
    describeSnapshotScheduleResponse_recurrenceInHours,
    describeSnapshotScheduleResponse_startAt,
    describeSnapshotScheduleResponse_tags,
    describeSnapshotScheduleResponse_timezone,
    describeSnapshotScheduleResponse_volumeARN,
    describeSnapshotScheduleResponse_httpStatus,

    -- ** DescribeStorediSCSIVolumes
    describeStorediSCSIVolumes_volumeARNs,
    describeStorediSCSIVolumesResponse_storediSCSIVolumes,
    describeStorediSCSIVolumesResponse_httpStatus,

    -- ** DescribeTapeArchives
    describeTapeArchives_limit,
    describeTapeArchives_marker,
    describeTapeArchives_tapeARNs,
    describeTapeArchivesResponse_marker,
    describeTapeArchivesResponse_tapeArchives,
    describeTapeArchivesResponse_httpStatus,

    -- ** DescribeTapeRecoveryPoints
    describeTapeRecoveryPoints_limit,
    describeTapeRecoveryPoints_marker,
    describeTapeRecoveryPoints_gatewayARN,
    describeTapeRecoveryPointsResponse_gatewayARN,
    describeTapeRecoveryPointsResponse_marker,
    describeTapeRecoveryPointsResponse_tapeRecoveryPointInfos,
    describeTapeRecoveryPointsResponse_httpStatus,

    -- ** DescribeTapes
    describeTapes_limit,
    describeTapes_marker,
    describeTapes_tapeARNs,
    describeTapes_gatewayARN,
    describeTapesResponse_marker,
    describeTapesResponse_tapes,
    describeTapesResponse_httpStatus,

    -- ** DescribeUploadBuffer
    describeUploadBuffer_gatewayARN,
    describeUploadBufferResponse_diskIds,
    describeUploadBufferResponse_gatewayARN,
    describeUploadBufferResponse_uploadBufferAllocatedInBytes,
    describeUploadBufferResponse_uploadBufferUsedInBytes,
    describeUploadBufferResponse_httpStatus,

    -- ** DescribeVTLDevices
    describeVTLDevices_limit,
    describeVTLDevices_marker,
    describeVTLDevices_vTLDeviceARNs,
    describeVTLDevices_gatewayARN,
    describeVTLDevicesResponse_gatewayARN,
    describeVTLDevicesResponse_marker,
    describeVTLDevicesResponse_vTLDevices,
    describeVTLDevicesResponse_httpStatus,

    -- ** DescribeWorkingStorage
    describeWorkingStorage_gatewayARN,
    describeWorkingStorageResponse_diskIds,
    describeWorkingStorageResponse_gatewayARN,
    describeWorkingStorageResponse_workingStorageAllocatedInBytes,
    describeWorkingStorageResponse_workingStorageUsedInBytes,
    describeWorkingStorageResponse_httpStatus,

    -- ** DetachVolume
    detachVolume_forceDetach,
    detachVolume_volumeARN,
    detachVolumeResponse_volumeARN,
    detachVolumeResponse_httpStatus,

    -- ** DisableGateway
    disableGateway_gatewayARN,
    disableGatewayResponse_gatewayARN,
    disableGatewayResponse_httpStatus,

    -- ** DisassociateFileSystem
    disassociateFileSystem_forceDelete,
    disassociateFileSystem_fileSystemAssociationARN,
    disassociateFileSystemResponse_fileSystemAssociationARN,
    disassociateFileSystemResponse_httpStatus,

    -- ** JoinDomain
    joinDomain_domainControllers,
    joinDomain_organizationalUnit,
    joinDomain_timeoutInSeconds,
    joinDomain_gatewayARN,
    joinDomain_domainName,
    joinDomain_userName,
    joinDomain_password,
    joinDomainResponse_activeDirectoryStatus,
    joinDomainResponse_gatewayARN,
    joinDomainResponse_httpStatus,

    -- ** ListAutomaticTapeCreationPolicies
    listAutomaticTapeCreationPolicies_gatewayARN,
    listAutomaticTapeCreationPoliciesResponse_automaticTapeCreationPolicyInfos,
    listAutomaticTapeCreationPoliciesResponse_httpStatus,

    -- ** ListFileShares
    listFileShares_gatewayARN,
    listFileShares_limit,
    listFileShares_marker,
    listFileSharesResponse_fileShareInfoList,
    listFileSharesResponse_marker,
    listFileSharesResponse_nextMarker,
    listFileSharesResponse_httpStatus,

    -- ** ListFileSystemAssociations
    listFileSystemAssociations_gatewayARN,
    listFileSystemAssociations_limit,
    listFileSystemAssociations_marker,
    listFileSystemAssociationsResponse_fileSystemAssociationSummaryList,
    listFileSystemAssociationsResponse_marker,
    listFileSystemAssociationsResponse_nextMarker,
    listFileSystemAssociationsResponse_httpStatus,

    -- ** ListGateways
    listGateways_limit,
    listGateways_marker,
    listGatewaysResponse_gateways,
    listGatewaysResponse_marker,
    listGatewaysResponse_httpStatus,

    -- ** ListLocalDisks
    listLocalDisks_gatewayARN,
    listLocalDisksResponse_disks,
    listLocalDisksResponse_gatewayARN,
    listLocalDisksResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_limit,
    listTagsForResource_marker,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_marker,
    listTagsForResourceResponse_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTapePools
    listTapePools_limit,
    listTapePools_marker,
    listTapePools_poolARNs,
    listTapePoolsResponse_marker,
    listTapePoolsResponse_poolInfos,
    listTapePoolsResponse_httpStatus,

    -- ** ListTapes
    listTapes_limit,
    listTapes_marker,
    listTapes_tapeARNs,
    listTapesResponse_marker,
    listTapesResponse_tapeInfos,
    listTapesResponse_httpStatus,

    -- ** ListVolumeInitiators
    listVolumeInitiators_volumeARN,
    listVolumeInitiatorsResponse_initiators,
    listVolumeInitiatorsResponse_httpStatus,

    -- ** ListVolumeRecoveryPoints
    listVolumeRecoveryPoints_gatewayARN,
    listVolumeRecoveryPointsResponse_gatewayARN,
    listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos,
    listVolumeRecoveryPointsResponse_httpStatus,

    -- ** ListVolumes
    listVolumes_gatewayARN,
    listVolumes_limit,
    listVolumes_marker,
    listVolumesResponse_gatewayARN,
    listVolumesResponse_marker,
    listVolumesResponse_volumeInfos,
    listVolumesResponse_httpStatus,

    -- ** NotifyWhenUploaded
    notifyWhenUploaded_fileShareARN,
    notifyWhenUploadedResponse_fileShareARN,
    notifyWhenUploadedResponse_notificationId,
    notifyWhenUploadedResponse_httpStatus,

    -- ** RefreshCache
    refreshCache_folderList,
    refreshCache_recursive,
    refreshCache_fileShareARN,
    refreshCacheResponse_fileShareARN,
    refreshCacheResponse_notificationId,
    refreshCacheResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceARN,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_resourceARN,
    removeTagsFromResourceResponse_httpStatus,

    -- ** ResetCache
    resetCache_gatewayARN,
    resetCacheResponse_gatewayARN,
    resetCacheResponse_httpStatus,

    -- ** RetrieveTapeArchive
    retrieveTapeArchive_tapeARN,
    retrieveTapeArchive_gatewayARN,
    retrieveTapeArchiveResponse_tapeARN,
    retrieveTapeArchiveResponse_httpStatus,

    -- ** RetrieveTapeRecoveryPoint
    retrieveTapeRecoveryPoint_tapeARN,
    retrieveTapeRecoveryPoint_gatewayARN,
    retrieveTapeRecoveryPointResponse_tapeARN,
    retrieveTapeRecoveryPointResponse_httpStatus,

    -- ** SetLocalConsolePassword
    setLocalConsolePassword_gatewayARN,
    setLocalConsolePassword_localConsolePassword,
    setLocalConsolePasswordResponse_gatewayARN,
    setLocalConsolePasswordResponse_httpStatus,

    -- ** SetSMBGuestPassword
    setSMBGuestPassword_gatewayARN,
    setSMBGuestPassword_password,
    setSMBGuestPasswordResponse_gatewayARN,
    setSMBGuestPasswordResponse_httpStatus,

    -- ** ShutdownGateway
    shutdownGateway_gatewayARN,
    shutdownGatewayResponse_gatewayARN,
    shutdownGatewayResponse_httpStatus,

    -- ** StartAvailabilityMonitorTest
    startAvailabilityMonitorTest_gatewayARN,
    startAvailabilityMonitorTestResponse_gatewayARN,
    startAvailabilityMonitorTestResponse_httpStatus,

    -- ** StartGateway
    startGateway_gatewayARN,
    startGatewayResponse_gatewayARN,
    startGatewayResponse_httpStatus,

    -- ** UpdateAutomaticTapeCreationPolicy
    updateAutomaticTapeCreationPolicy_automaticTapeCreationRules,
    updateAutomaticTapeCreationPolicy_gatewayARN,
    updateAutomaticTapeCreationPolicyResponse_gatewayARN,
    updateAutomaticTapeCreationPolicyResponse_httpStatus,

    -- ** UpdateBandwidthRateLimit
    updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_gatewayARN,
    updateBandwidthRateLimitResponse_gatewayARN,
    updateBandwidthRateLimitResponse_httpStatus,

    -- ** UpdateBandwidthRateLimitSchedule
    updateBandwidthRateLimitSchedule_gatewayARN,
    updateBandwidthRateLimitSchedule_bandwidthRateLimitIntervals,
    updateBandwidthRateLimitScheduleResponse_gatewayARN,
    updateBandwidthRateLimitScheduleResponse_httpStatus,

    -- ** UpdateChapCredentials
    updateChapCredentials_secretToAuthenticateTarget,
    updateChapCredentials_targetARN,
    updateChapCredentials_secretToAuthenticateInitiator,
    updateChapCredentials_initiatorName,
    updateChapCredentialsResponse_initiatorName,
    updateChapCredentialsResponse_targetARN,
    updateChapCredentialsResponse_httpStatus,

    -- ** UpdateFileSystemAssociation
    updateFileSystemAssociation_auditDestinationARN,
    updateFileSystemAssociation_cacheAttributes,
    updateFileSystemAssociation_password,
    updateFileSystemAssociation_userName,
    updateFileSystemAssociation_fileSystemAssociationARN,
    updateFileSystemAssociationResponse_fileSystemAssociationARN,
    updateFileSystemAssociationResponse_httpStatus,

    -- ** UpdateGatewayInformation
    updateGatewayInformation_cloudWatchLogGroupARN,
    updateGatewayInformation_gatewayCapacity,
    updateGatewayInformation_gatewayName,
    updateGatewayInformation_gatewayTimezone,
    updateGatewayInformation_gatewayARN,
    updateGatewayInformationResponse_gatewayARN,
    updateGatewayInformationResponse_gatewayName,
    updateGatewayInformationResponse_httpStatus,

    -- ** UpdateGatewaySoftwareNow
    updateGatewaySoftwareNow_gatewayARN,
    updateGatewaySoftwareNowResponse_gatewayARN,
    updateGatewaySoftwareNowResponse_httpStatus,

    -- ** UpdateMaintenanceStartTime
    updateMaintenanceStartTime_dayOfMonth,
    updateMaintenanceStartTime_dayOfWeek,
    updateMaintenanceStartTime_gatewayARN,
    updateMaintenanceStartTime_hourOfDay,
    updateMaintenanceStartTime_minuteOfHour,
    updateMaintenanceStartTimeResponse_gatewayARN,
    updateMaintenanceStartTimeResponse_httpStatus,

    -- ** UpdateNFSFileShare
    updateNFSFileShare_auditDestinationARN,
    updateNFSFileShare_cacheAttributes,
    updateNFSFileShare_clientList,
    updateNFSFileShare_defaultStorageClass,
    updateNFSFileShare_fileShareName,
    updateNFSFileShare_guessMIMETypeEnabled,
    updateNFSFileShare_kmsEncrypted,
    updateNFSFileShare_kmsKey,
    updateNFSFileShare_nFSFileShareDefaults,
    updateNFSFileShare_notificationPolicy,
    updateNFSFileShare_objectACL,
    updateNFSFileShare_readOnly,
    updateNFSFileShare_requesterPays,
    updateNFSFileShare_squash,
    updateNFSFileShare_fileShareARN,
    updateNFSFileShareResponse_fileShareARN,
    updateNFSFileShareResponse_httpStatus,

    -- ** UpdateSMBFileShare
    updateSMBFileShare_accessBasedEnumeration,
    updateSMBFileShare_adminUserList,
    updateSMBFileShare_auditDestinationARN,
    updateSMBFileShare_cacheAttributes,
    updateSMBFileShare_caseSensitivity,
    updateSMBFileShare_defaultStorageClass,
    updateSMBFileShare_fileShareName,
    updateSMBFileShare_guessMIMETypeEnabled,
    updateSMBFileShare_invalidUserList,
    updateSMBFileShare_kmsEncrypted,
    updateSMBFileShare_kmsKey,
    updateSMBFileShare_notificationPolicy,
    updateSMBFileShare_objectACL,
    updateSMBFileShare_oplocksEnabled,
    updateSMBFileShare_readOnly,
    updateSMBFileShare_requesterPays,
    updateSMBFileShare_sMBACLEnabled,
    updateSMBFileShare_validUserList,
    updateSMBFileShare_fileShareARN,
    updateSMBFileShareResponse_fileShareARN,
    updateSMBFileShareResponse_httpStatus,

    -- ** UpdateSMBFileShareVisibility
    updateSMBFileShareVisibility_gatewayARN,
    updateSMBFileShareVisibility_fileSharesVisible,
    updateSMBFileShareVisibilityResponse_gatewayARN,
    updateSMBFileShareVisibilityResponse_httpStatus,

    -- ** UpdateSMBLocalGroups
    updateSMBLocalGroups_gatewayARN,
    updateSMBLocalGroups_sMBLocalGroups,
    updateSMBLocalGroupsResponse_gatewayARN,
    updateSMBLocalGroupsResponse_httpStatus,

    -- ** UpdateSMBSecurityStrategy
    updateSMBSecurityStrategy_gatewayARN,
    updateSMBSecurityStrategy_sMBSecurityStrategy,
    updateSMBSecurityStrategyResponse_gatewayARN,
    updateSMBSecurityStrategyResponse_httpStatus,

    -- ** UpdateSnapshotSchedule
    updateSnapshotSchedule_description,
    updateSnapshotSchedule_tags,
    updateSnapshotSchedule_volumeARN,
    updateSnapshotSchedule_startAt,
    updateSnapshotSchedule_recurrenceInHours,
    updateSnapshotScheduleResponse_volumeARN,
    updateSnapshotScheduleResponse_httpStatus,

    -- ** UpdateVTLDeviceType
    updateVTLDeviceType_vTLDeviceARN,
    updateVTLDeviceType_deviceType,
    updateVTLDeviceTypeResponse_vTLDeviceARN,
    updateVTLDeviceTypeResponse_httpStatus,

    -- * Types

    -- ** AutomaticTapeCreationPolicyInfo
    automaticTapeCreationPolicyInfo_automaticTapeCreationRules,
    automaticTapeCreationPolicyInfo_gatewayARN,

    -- ** AutomaticTapeCreationRule
    automaticTapeCreationRule_worm,
    automaticTapeCreationRule_tapeBarcodePrefix,
    automaticTapeCreationRule_poolId,
    automaticTapeCreationRule_tapeSizeInBytes,
    automaticTapeCreationRule_minimumNumTapes,

    -- ** BandwidthRateLimitInterval
    bandwidthRateLimitInterval_averageDownloadRateLimitInBitsPerSec,
    bandwidthRateLimitInterval_averageUploadRateLimitInBitsPerSec,
    bandwidthRateLimitInterval_startHourOfDay,
    bandwidthRateLimitInterval_startMinuteOfHour,
    bandwidthRateLimitInterval_endHourOfDay,
    bandwidthRateLimitInterval_endMinuteOfHour,
    bandwidthRateLimitInterval_daysOfWeek,

    -- ** CacheAttributes
    cacheAttributes_cacheStaleTimeoutInSeconds,

    -- ** CachediSCSIVolume
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

    -- ** ChapInfo
    chapInfo_initiatorName,
    chapInfo_secretToAuthenticateInitiator,
    chapInfo_secretToAuthenticateTarget,
    chapInfo_targetARN,

    -- ** DeviceiSCSIAttributes
    deviceiSCSIAttributes_chapEnabled,
    deviceiSCSIAttributes_networkInterfaceId,
    deviceiSCSIAttributes_networkInterfacePort,
    deviceiSCSIAttributes_targetARN,

    -- ** Disk
    disk_diskAllocationResource,
    disk_diskAllocationType,
    disk_diskAttributeList,
    disk_diskId,
    disk_diskNode,
    disk_diskPath,
    disk_diskSizeInBytes,
    disk_diskStatus,

    -- ** EndpointNetworkConfiguration
    endpointNetworkConfiguration_ipAddresses,

    -- ** FileShareInfo
    fileShareInfo_fileShareARN,
    fileShareInfo_fileShareId,
    fileShareInfo_fileShareStatus,
    fileShareInfo_fileShareType,
    fileShareInfo_gatewayARN,

    -- ** FileSystemAssociationInfo
    fileSystemAssociationInfo_auditDestinationARN,
    fileSystemAssociationInfo_cacheAttributes,
    fileSystemAssociationInfo_endpointNetworkConfiguration,
    fileSystemAssociationInfo_fileSystemAssociationARN,
    fileSystemAssociationInfo_fileSystemAssociationStatus,
    fileSystemAssociationInfo_fileSystemAssociationStatusDetails,
    fileSystemAssociationInfo_gatewayARN,
    fileSystemAssociationInfo_locationARN,
    fileSystemAssociationInfo_tags,

    -- ** FileSystemAssociationStatusDetail
    fileSystemAssociationStatusDetail_errorCode,

    -- ** FileSystemAssociationSummary
    fileSystemAssociationSummary_fileSystemAssociationARN,
    fileSystemAssociationSummary_fileSystemAssociationId,
    fileSystemAssociationSummary_fileSystemAssociationStatus,
    fileSystemAssociationSummary_gatewayARN,

    -- ** GatewayInfo
    gatewayInfo_ec2InstanceId,
    gatewayInfo_ec2InstanceRegion,
    gatewayInfo_gatewayARN,
    gatewayInfo_gatewayId,
    gatewayInfo_gatewayName,
    gatewayInfo_gatewayOperationalState,
    gatewayInfo_gatewayType,
    gatewayInfo_hostEnvironment,
    gatewayInfo_hostEnvironmentId,

    -- ** NFSFileShareDefaults
    nFSFileShareDefaults_directoryMode,
    nFSFileShareDefaults_fileMode,
    nFSFileShareDefaults_groupId,
    nFSFileShareDefaults_ownerId,

    -- ** NFSFileShareInfo
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

    -- ** NetworkInterface
    networkInterface_ipv4Address,
    networkInterface_ipv6Address,
    networkInterface_macAddress,

    -- ** PoolInfo
    poolInfo_poolARN,
    poolInfo_poolName,
    poolInfo_poolStatus,
    poolInfo_retentionLockTimeInDays,
    poolInfo_retentionLockType,
    poolInfo_storageClass,

    -- ** SMBFileShareInfo
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

    -- ** SMBLocalGroups
    sMBLocalGroups_gatewayAdmins,

    -- ** StorediSCSIVolume
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

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Tape
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

    -- ** TapeArchive
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

    -- ** TapeInfo
    tapeInfo_gatewayARN,
    tapeInfo_poolEntryDate,
    tapeInfo_poolId,
    tapeInfo_retentionStartDate,
    tapeInfo_tapeARN,
    tapeInfo_tapeBarcode,
    tapeInfo_tapeSizeInBytes,
    tapeInfo_tapeStatus,

    -- ** TapeRecoveryPointInfo
    tapeRecoveryPointInfo_tapeARN,
    tapeRecoveryPointInfo_tapeRecoveryPointTime,
    tapeRecoveryPointInfo_tapeSizeInBytes,
    tapeRecoveryPointInfo_tapeStatus,

    -- ** VTLDevice
    vTLDevice_deviceiSCSIAttributes,
    vTLDevice_vTLDeviceARN,
    vTLDevice_vTLDeviceProductIdentifier,
    vTLDevice_vTLDeviceType,
    vTLDevice_vTLDeviceVendor,

    -- ** VolumeInfo
    volumeInfo_gatewayARN,
    volumeInfo_gatewayId,
    volumeInfo_volumeARN,
    volumeInfo_volumeAttachmentStatus,
    volumeInfo_volumeId,
    volumeInfo_volumeSizeInBytes,
    volumeInfo_volumeType,

    -- ** VolumeRecoveryPointInfo
    volumeRecoveryPointInfo_volumeARN,
    volumeRecoveryPointInfo_volumeRecoveryPointTime,
    volumeRecoveryPointInfo_volumeSizeInBytes,
    volumeRecoveryPointInfo_volumeUsageInBytes,

    -- ** VolumeiSCSIAttributes
    volumeiSCSIAttributes_chapEnabled,
    volumeiSCSIAttributes_lunNumber,
    volumeiSCSIAttributes_networkInterfaceId,
    volumeiSCSIAttributes_networkInterfacePort,
    volumeiSCSIAttributes_targetARN,
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
import Amazonka.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
import Amazonka.StorageGateway.Types.AutomaticTapeCreationRule
import Amazonka.StorageGateway.Types.BandwidthRateLimitInterval
import Amazonka.StorageGateway.Types.CacheAttributes
import Amazonka.StorageGateway.Types.CachediSCSIVolume
import Amazonka.StorageGateway.Types.ChapInfo
import Amazonka.StorageGateway.Types.DeviceiSCSIAttributes
import Amazonka.StorageGateway.Types.Disk
import Amazonka.StorageGateway.Types.EndpointNetworkConfiguration
import Amazonka.StorageGateway.Types.FileShareInfo
import Amazonka.StorageGateway.Types.FileSystemAssociationInfo
import Amazonka.StorageGateway.Types.FileSystemAssociationStatusDetail
import Amazonka.StorageGateway.Types.FileSystemAssociationSummary
import Amazonka.StorageGateway.Types.GatewayInfo
import Amazonka.StorageGateway.Types.NFSFileShareDefaults
import Amazonka.StorageGateway.Types.NFSFileShareInfo
import Amazonka.StorageGateway.Types.NetworkInterface
import Amazonka.StorageGateway.Types.PoolInfo
import Amazonka.StorageGateway.Types.SMBFileShareInfo
import Amazonka.StorageGateway.Types.SMBLocalGroups
import Amazonka.StorageGateway.Types.StorediSCSIVolume
import Amazonka.StorageGateway.Types.Tag
import Amazonka.StorageGateway.Types.Tape
import Amazonka.StorageGateway.Types.TapeArchive
import Amazonka.StorageGateway.Types.TapeInfo
import Amazonka.StorageGateway.Types.TapeRecoveryPointInfo
import Amazonka.StorageGateway.Types.VTLDevice
import Amazonka.StorageGateway.Types.VolumeInfo
import Amazonka.StorageGateway.Types.VolumeRecoveryPointInfo
import Amazonka.StorageGateway.Types.VolumeiSCSIAttributes
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

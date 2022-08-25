{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.StorageGateway.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Lens
  ( -- * Operations

    -- ** ActivateGateway
    activateGateway_tags,
    activateGateway_gatewayType,
    activateGateway_mediumChangerType,
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
    associateFileSystem_tags,
    associateFileSystem_endpointNetworkConfiguration,
    associateFileSystem_cacheAttributes,
    associateFileSystem_auditDestinationARN,
    associateFileSystem_userName,
    associateFileSystem_password,
    associateFileSystem_clientToken,
    associateFileSystem_gatewayARN,
    associateFileSystem_locationARN,
    associateFileSystemResponse_fileSystemAssociationARN,
    associateFileSystemResponse_httpStatus,

    -- ** AttachVolume
    attachVolume_targetName,
    attachVolume_diskId,
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
    createCachediSCSIVolume_tags,
    createCachediSCSIVolume_snapshotId,
    createCachediSCSIVolume_kmsKey,
    createCachediSCSIVolume_kmsEncrypted,
    createCachediSCSIVolume_sourceVolumeARN,
    createCachediSCSIVolume_gatewayARN,
    createCachediSCSIVolume_volumeSizeInBytes,
    createCachediSCSIVolume_targetName,
    createCachediSCSIVolume_networkInterfaceId,
    createCachediSCSIVolume_clientToken,
    createCachediSCSIVolumeResponse_targetARN,
    createCachediSCSIVolumeResponse_volumeARN,
    createCachediSCSIVolumeResponse_httpStatus,

    -- ** CreateNFSFileShare
    createNFSFileShare_tags,
    createNFSFileShare_squash,
    createNFSFileShare_nFSFileShareDefaults,
    createNFSFileShare_fileShareName,
    createNFSFileShare_requesterPays,
    createNFSFileShare_objectACL,
    createNFSFileShare_kmsKey,
    createNFSFileShare_vPCEndpointDNSName,
    createNFSFileShare_kmsEncrypted,
    createNFSFileShare_defaultStorageClass,
    createNFSFileShare_cacheAttributes,
    createNFSFileShare_readOnly,
    createNFSFileShare_bucketRegion,
    createNFSFileShare_auditDestinationARN,
    createNFSFileShare_clientList,
    createNFSFileShare_guessMIMETypeEnabled,
    createNFSFileShare_notificationPolicy,
    createNFSFileShare_clientToken,
    createNFSFileShare_gatewayARN,
    createNFSFileShare_role,
    createNFSFileShare_locationARN,
    createNFSFileShareResponse_fileShareARN,
    createNFSFileShareResponse_httpStatus,

    -- ** CreateSMBFileShare
    createSMBFileShare_tags,
    createSMBFileShare_validUserList,
    createSMBFileShare_authentication,
    createSMBFileShare_fileShareName,
    createSMBFileShare_requesterPays,
    createSMBFileShare_objectACL,
    createSMBFileShare_caseSensitivity,
    createSMBFileShare_kmsKey,
    createSMBFileShare_sMBACLEnabled,
    createSMBFileShare_oplocksEnabled,
    createSMBFileShare_vPCEndpointDNSName,
    createSMBFileShare_accessBasedEnumeration,
    createSMBFileShare_invalidUserList,
    createSMBFileShare_adminUserList,
    createSMBFileShare_kmsEncrypted,
    createSMBFileShare_defaultStorageClass,
    createSMBFileShare_cacheAttributes,
    createSMBFileShare_readOnly,
    createSMBFileShare_bucketRegion,
    createSMBFileShare_auditDestinationARN,
    createSMBFileShare_guessMIMETypeEnabled,
    createSMBFileShare_notificationPolicy,
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
    createStorediSCSIVolume_tags,
    createStorediSCSIVolume_snapshotId,
    createStorediSCSIVolume_kmsKey,
    createStorediSCSIVolume_kmsEncrypted,
    createStorediSCSIVolume_gatewayARN,
    createStorediSCSIVolume_diskId,
    createStorediSCSIVolume_preserveExistingData,
    createStorediSCSIVolume_targetName,
    createStorediSCSIVolume_networkInterfaceId,
    createStorediSCSIVolumeResponse_volumeSizeInBytes,
    createStorediSCSIVolumeResponse_targetARN,
    createStorediSCSIVolumeResponse_volumeARN,
    createStorediSCSIVolumeResponse_httpStatus,

    -- ** CreateTapePool
    createTapePool_tags,
    createTapePool_retentionLockTimeInDays,
    createTapePool_retentionLockType,
    createTapePool_poolName,
    createTapePool_storageClass,
    createTapePoolResponse_poolARN,
    createTapePoolResponse_httpStatus,

    -- ** CreateTapeWithBarcode
    createTapeWithBarcode_tags,
    createTapeWithBarcode_worm,
    createTapeWithBarcode_kmsKey,
    createTapeWithBarcode_kmsEncrypted,
    createTapeWithBarcode_poolId,
    createTapeWithBarcode_gatewayARN,
    createTapeWithBarcode_tapeSizeInBytes,
    createTapeWithBarcode_tapeBarcode,
    createTapeWithBarcodeResponse_tapeARN,
    createTapeWithBarcodeResponse_httpStatus,

    -- ** CreateTapes
    createTapes_tags,
    createTapes_worm,
    createTapes_kmsKey,
    createTapes_kmsEncrypted,
    createTapes_poolId,
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
    describeAvailabilityMonitorTestResponse_status,
    describeAvailabilityMonitorTestResponse_gatewayARN,
    describeAvailabilityMonitorTestResponse_startTime,
    describeAvailabilityMonitorTestResponse_httpStatus,

    -- ** DescribeBandwidthRateLimit
    describeBandwidthRateLimit_gatewayARN,
    describeBandwidthRateLimitResponse_averageUploadRateLimitInBitsPerSec,
    describeBandwidthRateLimitResponse_gatewayARN,
    describeBandwidthRateLimitResponse_averageDownloadRateLimitInBitsPerSec,
    describeBandwidthRateLimitResponse_httpStatus,

    -- ** DescribeBandwidthRateLimitSchedule
    describeBandwidthRateLimitSchedule_gatewayARN,
    describeBandwidthRateLimitScheduleResponse_gatewayARN,
    describeBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals,
    describeBandwidthRateLimitScheduleResponse_httpStatus,

    -- ** DescribeCache
    describeCache_gatewayARN,
    describeCacheResponse_cacheMissPercentage,
    describeCacheResponse_diskIds,
    describeCacheResponse_cacheHitPercentage,
    describeCacheResponse_cacheUsedPercentage,
    describeCacheResponse_cacheDirtyPercentage,
    describeCacheResponse_gatewayARN,
    describeCacheResponse_cacheAllocatedInBytes,
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
    describeGatewayInformationResponse_tags,
    describeGatewayInformationResponse_ec2InstanceId,
    describeGatewayInformationResponse_nextUpdateAvailabilityDate,
    describeGatewayInformationResponse_cloudWatchLogGroupARN,
    describeGatewayInformationResponse_gatewayName,
    describeGatewayInformationResponse_deprecationDate,
    describeGatewayInformationResponse_gatewayCapacity,
    describeGatewayInformationResponse_gatewayType,
    describeGatewayInformationResponse_gatewayARN,
    describeGatewayInformationResponse_endpointType,
    describeGatewayInformationResponse_softwareUpdatesEndDate,
    describeGatewayInformationResponse_lastSoftwareUpdate,
    describeGatewayInformationResponse_gatewayState,
    describeGatewayInformationResponse_gatewayId,
    describeGatewayInformationResponse_ec2InstanceRegion,
    describeGatewayInformationResponse_gatewayNetworkInterfaces,
    describeGatewayInformationResponse_supportedGatewayCapacities,
    describeGatewayInformationResponse_gatewayTimezone,
    describeGatewayInformationResponse_hostEnvironmentId,
    describeGatewayInformationResponse_vPCEndpoint,
    describeGatewayInformationResponse_hostEnvironment,
    describeGatewayInformationResponse_httpStatus,

    -- ** DescribeMaintenanceStartTime
    describeMaintenanceStartTime_gatewayARN,
    describeMaintenanceStartTimeResponse_minuteOfHour,
    describeMaintenanceStartTimeResponse_timezone,
    describeMaintenanceStartTimeResponse_gatewayARN,
    describeMaintenanceStartTimeResponse_dayOfWeek,
    describeMaintenanceStartTimeResponse_hourOfDay,
    describeMaintenanceStartTimeResponse_dayOfMonth,
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
    describeSMBSettingsResponse_sMBSecurityStrategy,
    describeSMBSettingsResponse_domainName,
    describeSMBSettingsResponse_sMBLocalGroups,
    describeSMBSettingsResponse_gatewayARN,
    describeSMBSettingsResponse_sMBGuestPasswordSet,
    describeSMBSettingsResponse_fileSharesVisible,
    describeSMBSettingsResponse_activeDirectoryStatus,
    describeSMBSettingsResponse_httpStatus,

    -- ** DescribeSnapshotSchedule
    describeSnapshotSchedule_volumeARN,
    describeSnapshotScheduleResponse_tags,
    describeSnapshotScheduleResponse_timezone,
    describeSnapshotScheduleResponse_volumeARN,
    describeSnapshotScheduleResponse_description,
    describeSnapshotScheduleResponse_recurrenceInHours,
    describeSnapshotScheduleResponse_startAt,
    describeSnapshotScheduleResponse_httpStatus,

    -- ** DescribeStorediSCSIVolumes
    describeStorediSCSIVolumes_volumeARNs,
    describeStorediSCSIVolumesResponse_storediSCSIVolumes,
    describeStorediSCSIVolumesResponse_httpStatus,

    -- ** DescribeTapeArchives
    describeTapeArchives_tapeARNs,
    describeTapeArchives_marker,
    describeTapeArchives_limit,
    describeTapeArchivesResponse_tapeArchives,
    describeTapeArchivesResponse_marker,
    describeTapeArchivesResponse_httpStatus,

    -- ** DescribeTapeRecoveryPoints
    describeTapeRecoveryPoints_marker,
    describeTapeRecoveryPoints_limit,
    describeTapeRecoveryPoints_gatewayARN,
    describeTapeRecoveryPointsResponse_marker,
    describeTapeRecoveryPointsResponse_tapeRecoveryPointInfos,
    describeTapeRecoveryPointsResponse_gatewayARN,
    describeTapeRecoveryPointsResponse_httpStatus,

    -- ** DescribeTapes
    describeTapes_tapeARNs,
    describeTapes_marker,
    describeTapes_limit,
    describeTapes_gatewayARN,
    describeTapesResponse_marker,
    describeTapesResponse_tapes,
    describeTapesResponse_httpStatus,

    -- ** DescribeUploadBuffer
    describeUploadBuffer_gatewayARN,
    describeUploadBufferResponse_uploadBufferAllocatedInBytes,
    describeUploadBufferResponse_diskIds,
    describeUploadBufferResponse_gatewayARN,
    describeUploadBufferResponse_uploadBufferUsedInBytes,
    describeUploadBufferResponse_httpStatus,

    -- ** DescribeVTLDevices
    describeVTLDevices_marker,
    describeVTLDevices_limit,
    describeVTLDevices_vTLDeviceARNs,
    describeVTLDevices_gatewayARN,
    describeVTLDevicesResponse_marker,
    describeVTLDevicesResponse_gatewayARN,
    describeVTLDevicesResponse_vTLDevices,
    describeVTLDevicesResponse_httpStatus,

    -- ** DescribeWorkingStorage
    describeWorkingStorage_gatewayARN,
    describeWorkingStorageResponse_workingStorageUsedInBytes,
    describeWorkingStorageResponse_diskIds,
    describeWorkingStorageResponse_workingStorageAllocatedInBytes,
    describeWorkingStorageResponse_gatewayARN,
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
    joinDomain_organizationalUnit,
    joinDomain_timeoutInSeconds,
    joinDomain_domainControllers,
    joinDomain_gatewayARN,
    joinDomain_domainName,
    joinDomain_userName,
    joinDomain_password,
    joinDomainResponse_gatewayARN,
    joinDomainResponse_activeDirectoryStatus,
    joinDomainResponse_httpStatus,

    -- ** ListAutomaticTapeCreationPolicies
    listAutomaticTapeCreationPolicies_gatewayARN,
    listAutomaticTapeCreationPoliciesResponse_automaticTapeCreationPolicyInfos,
    listAutomaticTapeCreationPoliciesResponse_httpStatus,

    -- ** ListFileShares
    listFileShares_marker,
    listFileShares_gatewayARN,
    listFileShares_limit,
    listFileSharesResponse_marker,
    listFileSharesResponse_fileShareInfoList,
    listFileSharesResponse_nextMarker,
    listFileSharesResponse_httpStatus,

    -- ** ListFileSystemAssociations
    listFileSystemAssociations_marker,
    listFileSystemAssociations_gatewayARN,
    listFileSystemAssociations_limit,
    listFileSystemAssociationsResponse_marker,
    listFileSystemAssociationsResponse_fileSystemAssociationSummaryList,
    listFileSystemAssociationsResponse_nextMarker,
    listFileSystemAssociationsResponse_httpStatus,

    -- ** ListGateways
    listGateways_marker,
    listGateways_limit,
    listGatewaysResponse_marker,
    listGatewaysResponse_gateways,
    listGatewaysResponse_httpStatus,

    -- ** ListLocalDisks
    listLocalDisks_gatewayARN,
    listLocalDisksResponse_disks,
    listLocalDisksResponse_gatewayARN,
    listLocalDisksResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_marker,
    listTagsForResource_limit,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_marker,
    listTagsForResourceResponse_resourceARN,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTapePools
    listTapePools_marker,
    listTapePools_limit,
    listTapePools_poolARNs,
    listTapePoolsResponse_marker,
    listTapePoolsResponse_poolInfos,
    listTapePoolsResponse_httpStatus,

    -- ** ListTapes
    listTapes_tapeARNs,
    listTapes_marker,
    listTapes_limit,
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
    listVolumes_marker,
    listVolumes_gatewayARN,
    listVolumes_limit,
    listVolumesResponse_marker,
    listVolumesResponse_volumeInfos,
    listVolumesResponse_gatewayARN,
    listVolumesResponse_httpStatus,

    -- ** NotifyWhenUploaded
    notifyWhenUploaded_fileShareARN,
    notifyWhenUploadedResponse_fileShareARN,
    notifyWhenUploadedResponse_notificationId,
    notifyWhenUploadedResponse_httpStatus,

    -- ** RefreshCache
    refreshCache_recursive,
    refreshCache_folderList,
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
    updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec,
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
    updateFileSystemAssociation_password,
    updateFileSystemAssociation_userName,
    updateFileSystemAssociation_cacheAttributes,
    updateFileSystemAssociation_auditDestinationARN,
    updateFileSystemAssociation_fileSystemAssociationARN,
    updateFileSystemAssociationResponse_fileSystemAssociationARN,
    updateFileSystemAssociationResponse_httpStatus,

    -- ** UpdateGatewayInformation
    updateGatewayInformation_cloudWatchLogGroupARN,
    updateGatewayInformation_gatewayName,
    updateGatewayInformation_gatewayCapacity,
    updateGatewayInformation_gatewayTimezone,
    updateGatewayInformation_gatewayARN,
    updateGatewayInformationResponse_gatewayName,
    updateGatewayInformationResponse_gatewayARN,
    updateGatewayInformationResponse_httpStatus,

    -- ** UpdateGatewaySoftwareNow
    updateGatewaySoftwareNow_gatewayARN,
    updateGatewaySoftwareNowResponse_gatewayARN,
    updateGatewaySoftwareNowResponse_httpStatus,

    -- ** UpdateMaintenanceStartTime
    updateMaintenanceStartTime_dayOfWeek,
    updateMaintenanceStartTime_dayOfMonth,
    updateMaintenanceStartTime_gatewayARN,
    updateMaintenanceStartTime_hourOfDay,
    updateMaintenanceStartTime_minuteOfHour,
    updateMaintenanceStartTimeResponse_gatewayARN,
    updateMaintenanceStartTimeResponse_httpStatus,

    -- ** UpdateNFSFileShare
    updateNFSFileShare_squash,
    updateNFSFileShare_nFSFileShareDefaults,
    updateNFSFileShare_fileShareName,
    updateNFSFileShare_requesterPays,
    updateNFSFileShare_objectACL,
    updateNFSFileShare_kmsKey,
    updateNFSFileShare_kmsEncrypted,
    updateNFSFileShare_defaultStorageClass,
    updateNFSFileShare_cacheAttributes,
    updateNFSFileShare_readOnly,
    updateNFSFileShare_auditDestinationARN,
    updateNFSFileShare_clientList,
    updateNFSFileShare_guessMIMETypeEnabled,
    updateNFSFileShare_notificationPolicy,
    updateNFSFileShare_fileShareARN,
    updateNFSFileShareResponse_fileShareARN,
    updateNFSFileShareResponse_httpStatus,

    -- ** UpdateSMBFileShare
    updateSMBFileShare_validUserList,
    updateSMBFileShare_fileShareName,
    updateSMBFileShare_requesterPays,
    updateSMBFileShare_objectACL,
    updateSMBFileShare_caseSensitivity,
    updateSMBFileShare_kmsKey,
    updateSMBFileShare_sMBACLEnabled,
    updateSMBFileShare_oplocksEnabled,
    updateSMBFileShare_accessBasedEnumeration,
    updateSMBFileShare_invalidUserList,
    updateSMBFileShare_adminUserList,
    updateSMBFileShare_kmsEncrypted,
    updateSMBFileShare_defaultStorageClass,
    updateSMBFileShare_cacheAttributes,
    updateSMBFileShare_readOnly,
    updateSMBFileShare_auditDestinationARN,
    updateSMBFileShare_guessMIMETypeEnabled,
    updateSMBFileShare_notificationPolicy,
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
    updateSnapshotSchedule_tags,
    updateSnapshotSchedule_description,
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
    bandwidthRateLimitInterval_averageUploadRateLimitInBitsPerSec,
    bandwidthRateLimitInterval_averageDownloadRateLimitInBitsPerSec,
    bandwidthRateLimitInterval_startHourOfDay,
    bandwidthRateLimitInterval_startMinuteOfHour,
    bandwidthRateLimitInterval_endHourOfDay,
    bandwidthRateLimitInterval_endMinuteOfHour,
    bandwidthRateLimitInterval_daysOfWeek,

    -- ** CacheAttributes
    cacheAttributes_cacheStaleTimeoutInSeconds,

    -- ** CachediSCSIVolume
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

    -- ** ChapInfo
    chapInfo_initiatorName,
    chapInfo_secretToAuthenticateTarget,
    chapInfo_targetARN,
    chapInfo_secretToAuthenticateInitiator,

    -- ** DeviceiSCSIAttributes
    deviceiSCSIAttributes_networkInterfacePort,
    deviceiSCSIAttributes_targetARN,
    deviceiSCSIAttributes_chapEnabled,
    deviceiSCSIAttributes_networkInterfaceId,

    -- ** Disk
    disk_diskStatus,
    disk_diskNode,
    disk_diskId,
    disk_diskAllocationType,
    disk_diskPath,
    disk_diskSizeInBytes,
    disk_diskAttributeList,
    disk_diskAllocationResource,

    -- ** EndpointNetworkConfiguration
    endpointNetworkConfiguration_ipAddresses,

    -- ** FileShareInfo
    fileShareInfo_fileShareStatus,
    fileShareInfo_fileShareId,
    fileShareInfo_fileShareARN,
    fileShareInfo_gatewayARN,
    fileShareInfo_fileShareType,

    -- ** FileSystemAssociationInfo
    fileSystemAssociationInfo_tags,
    fileSystemAssociationInfo_fileSystemAssociationStatusDetails,
    fileSystemAssociationInfo_endpointNetworkConfiguration,
    fileSystemAssociationInfo_locationARN,
    fileSystemAssociationInfo_gatewayARN,
    fileSystemAssociationInfo_fileSystemAssociationStatus,
    fileSystemAssociationInfo_cacheAttributes,
    fileSystemAssociationInfo_auditDestinationARN,
    fileSystemAssociationInfo_fileSystemAssociationARN,

    -- ** FileSystemAssociationStatusDetail
    fileSystemAssociationStatusDetail_errorCode,

    -- ** FileSystemAssociationSummary
    fileSystemAssociationSummary_fileSystemAssociationId,
    fileSystemAssociationSummary_gatewayARN,
    fileSystemAssociationSummary_fileSystemAssociationStatus,
    fileSystemAssociationSummary_fileSystemAssociationARN,

    -- ** GatewayInfo
    gatewayInfo_ec2InstanceId,
    gatewayInfo_gatewayName,
    gatewayInfo_gatewayType,
    gatewayInfo_gatewayARN,
    gatewayInfo_gatewayOperationalState,
    gatewayInfo_gatewayId,
    gatewayInfo_ec2InstanceRegion,
    gatewayInfo_hostEnvironmentId,
    gatewayInfo_hostEnvironment,

    -- ** NFSFileShareDefaults
    nFSFileShareDefaults_fileMode,
    nFSFileShareDefaults_ownerId,
    nFSFileShareDefaults_directoryMode,
    nFSFileShareDefaults_groupId,

    -- ** NFSFileShareInfo
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

    -- ** NetworkInterface
    networkInterface_ipv6Address,
    networkInterface_macAddress,
    networkInterface_ipv4Address,

    -- ** PoolInfo
    poolInfo_poolARN,
    poolInfo_retentionLockTimeInDays,
    poolInfo_retentionLockType,
    poolInfo_poolStatus,
    poolInfo_poolName,
    poolInfo_storageClass,

    -- ** SMBFileShareInfo
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

    -- ** SMBLocalGroups
    sMBLocalGroups_gatewayAdmins,

    -- ** StorediSCSIVolume
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

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Tape
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

    -- ** TapeArchive
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

    -- ** TapeInfo
    tapeInfo_tapeBarcode,
    tapeInfo_tapeStatus,
    tapeInfo_retentionStartDate,
    tapeInfo_poolEntryDate,
    tapeInfo_gatewayARN,
    tapeInfo_tapeSizeInBytes,
    tapeInfo_poolId,
    tapeInfo_tapeARN,

    -- ** TapeRecoveryPointInfo
    tapeRecoveryPointInfo_tapeStatus,
    tapeRecoveryPointInfo_tapeSizeInBytes,
    tapeRecoveryPointInfo_tapeRecoveryPointTime,
    tapeRecoveryPointInfo_tapeARN,

    -- ** VTLDevice
    vTLDevice_vTLDeviceVendor,
    vTLDevice_vTLDeviceType,
    vTLDevice_vTLDeviceProductIdentifier,
    vTLDevice_deviceiSCSIAttributes,
    vTLDevice_vTLDeviceARN,

    -- ** VolumeInfo
    volumeInfo_volumeSizeInBytes,
    volumeInfo_volumeAttachmentStatus,
    volumeInfo_volumeType,
    volumeInfo_volumeARN,
    volumeInfo_gatewayARN,
    volumeInfo_gatewayId,
    volumeInfo_volumeId,

    -- ** VolumeRecoveryPointInfo
    volumeRecoveryPointInfo_volumeSizeInBytes,
    volumeRecoveryPointInfo_volumeUsageInBytes,
    volumeRecoveryPointInfo_volumeARN,
    volumeRecoveryPointInfo_volumeRecoveryPointTime,

    -- ** VolumeiSCSIAttributes
    volumeiSCSIAttributes_networkInterfacePort,
    volumeiSCSIAttributes_lunNumber,
    volumeiSCSIAttributes_targetARN,
    volumeiSCSIAttributes_chapEnabled,
    volumeiSCSIAttributes_networkInterfaceId,
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

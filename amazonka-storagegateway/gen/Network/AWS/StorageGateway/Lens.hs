{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Lens
  ( -- * Operations

    -- ** DetachVolume
    detachVolume_forceDetach,
    detachVolume_volumeARN,
    detachVolumeResponse_volumeARN,
    detachVolumeResponse_httpStatus,

    -- ** DescribeSMBSettings
    describeSMBSettings_gatewayARN,
    describeSMBSettingsResponse_sMBSecurityStrategy,
    describeSMBSettingsResponse_sMBGuestPasswordSet,
    describeSMBSettingsResponse_fileSharesVisible,
    describeSMBSettingsResponse_domainName,
    describeSMBSettingsResponse_activeDirectoryStatus,
    describeSMBSettingsResponse_gatewayARN,
    describeSMBSettingsResponse_httpStatus,

    -- ** RetrieveTapeArchive
    retrieveTapeArchive_tapeARN,
    retrieveTapeArchive_gatewayARN,
    retrieveTapeArchiveResponse_tapeARN,
    retrieveTapeArchiveResponse_httpStatus,

    -- ** CancelArchival
    cancelArchival_gatewayARN,
    cancelArchival_tapeARN,
    cancelArchivalResponse_tapeARN,
    cancelArchivalResponse_httpStatus,

    -- ** DescribeFileSystemAssociations
    describeFileSystemAssociations_fileSystemAssociationARNList,
    describeFileSystemAssociationsResponse_fileSystemAssociationInfoList,
    describeFileSystemAssociationsResponse_httpStatus,

    -- ** CreateNFSFileShare
    createNFSFileShare_defaultStorageClass,
    createNFSFileShare_fileShareName,
    createNFSFileShare_readOnly,
    createNFSFileShare_bucketRegion,
    createNFSFileShare_guessMIMETypeEnabled,
    createNFSFileShare_kmsEncrypted,
    createNFSFileShare_vPCEndpointDNSName,
    createNFSFileShare_squash,
    createNFSFileShare_notificationPolicy,
    createNFSFileShare_kmsKey,
    createNFSFileShare_tags,
    createNFSFileShare_clientList,
    createNFSFileShare_objectACL,
    createNFSFileShare_cacheAttributes,
    createNFSFileShare_nFSFileShareDefaults,
    createNFSFileShare_requesterPays,
    createNFSFileShare_clientToken,
    createNFSFileShare_gatewayARN,
    createNFSFileShare_role,
    createNFSFileShare_locationARN,
    createNFSFileShareResponse_fileShareARN,
    createNFSFileShareResponse_httpStatus,

    -- ** DisassociateFileSystem
    disassociateFileSystem_forceDelete,
    disassociateFileSystem_fileSystemAssociationARN,
    disassociateFileSystemResponse_fileSystemAssociationARN,
    disassociateFileSystemResponse_httpStatus,

    -- ** DescribeCache
    describeCache_gatewayARN,
    describeCacheResponse_cacheHitPercentage,
    describeCacheResponse_cacheDirtyPercentage,
    describeCacheResponse_cacheAllocatedInBytes,
    describeCacheResponse_cacheMissPercentage,
    describeCacheResponse_cacheUsedPercentage,
    describeCacheResponse_diskIds,
    describeCacheResponse_gatewayARN,
    describeCacheResponse_httpStatus,

    -- ** RefreshCache
    refreshCache_recursive,
    refreshCache_folderList,
    refreshCache_fileShareARN,
    refreshCacheResponse_fileShareARN,
    refreshCacheResponse_notificationId,
    refreshCacheResponse_httpStatus,

    -- ** ListTapes
    listTapes_tapeARNs,
    listTapes_limit,
    listTapes_marker,
    listTapesResponse_tapeInfos,
    listTapesResponse_marker,
    listTapesResponse_httpStatus,

    -- ** DescribeSMBFileShares
    describeSMBFileShares_fileShareARNList,
    describeSMBFileSharesResponse_sMBFileShareInfoList,
    describeSMBFileSharesResponse_httpStatus,

    -- ** UpdateNFSFileShare
    updateNFSFileShare_defaultStorageClass,
    updateNFSFileShare_fileShareName,
    updateNFSFileShare_readOnly,
    updateNFSFileShare_guessMIMETypeEnabled,
    updateNFSFileShare_kmsEncrypted,
    updateNFSFileShare_squash,
    updateNFSFileShare_notificationPolicy,
    updateNFSFileShare_kmsKey,
    updateNFSFileShare_clientList,
    updateNFSFileShare_objectACL,
    updateNFSFileShare_cacheAttributes,
    updateNFSFileShare_nFSFileShareDefaults,
    updateNFSFileShare_requesterPays,
    updateNFSFileShare_fileShareARN,
    updateNFSFileShareResponse_fileShareARN,
    updateNFSFileShareResponse_httpStatus,

    -- ** DescribeGatewayInformation
    describeGatewayInformation_gatewayARN,
    describeGatewayInformationResponse_gatewayName,
    describeGatewayInformationResponse_gatewayState,
    describeGatewayInformationResponse_deprecationDate,
    describeGatewayInformationResponse_gatewayCapacity,
    describeGatewayInformationResponse_softwareUpdatesEndDate,
    describeGatewayInformationResponse_nextUpdateAvailabilityDate,
    describeGatewayInformationResponse_gatewayTimezone,
    describeGatewayInformationResponse_endpointType,
    describeGatewayInformationResponse_gatewayType,
    describeGatewayInformationResponse_gatewayNetworkInterfaces,
    describeGatewayInformationResponse_ec2InstanceRegion,
    describeGatewayInformationResponse_lastSoftwareUpdate,
    describeGatewayInformationResponse_supportedGatewayCapacities,
    describeGatewayInformationResponse_tags,
    describeGatewayInformationResponse_vPCEndpoint,
    describeGatewayInformationResponse_cloudWatchLogGroupARN,
    describeGatewayInformationResponse_ec2InstanceId,
    describeGatewayInformationResponse_gatewayARN,
    describeGatewayInformationResponse_gatewayId,
    describeGatewayInformationResponse_hostEnvironment,
    describeGatewayInformationResponse_httpStatus,

    -- ** ListVolumes
    listVolumes_gatewayARN,
    listVolumes_limit,
    listVolumes_marker,
    listVolumesResponse_gatewayARN,
    listVolumesResponse_volumeInfos,
    listVolumesResponse_marker,
    listVolumesResponse_httpStatus,

    -- ** AddWorkingStorage
    addWorkingStorage_gatewayARN,
    addWorkingStorage_diskIds,
    addWorkingStorageResponse_gatewayARN,
    addWorkingStorageResponse_httpStatus,

    -- ** UpdateMaintenanceStartTime
    updateMaintenanceStartTime_dayOfWeek,
    updateMaintenanceStartTime_dayOfMonth,
    updateMaintenanceStartTime_gatewayARN,
    updateMaintenanceStartTime_hourOfDay,
    updateMaintenanceStartTime_minuteOfHour,
    updateMaintenanceStartTimeResponse_gatewayARN,
    updateMaintenanceStartTimeResponse_httpStatus,

    -- ** ActivateGateway
    activateGateway_tapeDriveType,
    activateGateway_gatewayType,
    activateGateway_tags,
    activateGateway_mediumChangerType,
    activateGateway_activationKey,
    activateGateway_gatewayName,
    activateGateway_gatewayTimezone,
    activateGateway_gatewayRegion,
    activateGatewayResponse_gatewayARN,
    activateGatewayResponse_httpStatus,

    -- ** ListVolumeRecoveryPoints
    listVolumeRecoveryPoints_gatewayARN,
    listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos,
    listVolumeRecoveryPointsResponse_gatewayARN,
    listVolumeRecoveryPointsResponse_httpStatus,

    -- ** DescribeAvailabilityMonitorTest
    describeAvailabilityMonitorTest_gatewayARN,
    describeAvailabilityMonitorTestResponse_status,
    describeAvailabilityMonitorTestResponse_startTime,
    describeAvailabilityMonitorTestResponse_gatewayARN,
    describeAvailabilityMonitorTestResponse_httpStatus,

    -- ** UpdateSMBFileShare
    updateSMBFileShare_sMBACLEnabled,
    updateSMBFileShare_defaultStorageClass,
    updateSMBFileShare_accessBasedEnumeration,
    updateSMBFileShare_fileShareName,
    updateSMBFileShare_caseSensitivity,
    updateSMBFileShare_readOnly,
    updateSMBFileShare_guessMIMETypeEnabled,
    updateSMBFileShare_kmsEncrypted,
    updateSMBFileShare_validUserList,
    updateSMBFileShare_notificationPolicy,
    updateSMBFileShare_kmsKey,
    updateSMBFileShare_auditDestinationARN,
    updateSMBFileShare_adminUserList,
    updateSMBFileShare_objectACL,
    updateSMBFileShare_cacheAttributes,
    updateSMBFileShare_requesterPays,
    updateSMBFileShare_oplocksEnabled,
    updateSMBFileShare_invalidUserList,
    updateSMBFileShare_fileShareARN,
    updateSMBFileShareResponse_fileShareARN,
    updateSMBFileShareResponse_httpStatus,

    -- ** DescribeTapes
    describeTapes_tapeARNs,
    describeTapes_limit,
    describeTapes_marker,
    describeTapes_gatewayARN,
    describeTapesResponse_tapes,
    describeTapesResponse_marker,
    describeTapesResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceARN,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_resourceARN,
    removeTagsFromResourceResponse_httpStatus,

    -- ** DeleteTapeArchive
    deleteTapeArchive_bypassGovernanceRetention,
    deleteTapeArchive_tapeARN,
    deleteTapeArchiveResponse_tapeARN,
    deleteTapeArchiveResponse_httpStatus,

    -- ** AttachVolume
    attachVolume_targetName,
    attachVolume_diskId,
    attachVolume_gatewayARN,
    attachVolume_volumeARN,
    attachVolume_networkInterfaceId,
    attachVolumeResponse_volumeARN,
    attachVolumeResponse_targetARN,
    attachVolumeResponse_httpStatus,

    -- ** CreateSMBFileShare
    createSMBFileShare_sMBACLEnabled,
    createSMBFileShare_defaultStorageClass,
    createSMBFileShare_accessBasedEnumeration,
    createSMBFileShare_fileShareName,
    createSMBFileShare_caseSensitivity,
    createSMBFileShare_readOnly,
    createSMBFileShare_bucketRegion,
    createSMBFileShare_guessMIMETypeEnabled,
    createSMBFileShare_kmsEncrypted,
    createSMBFileShare_vPCEndpointDNSName,
    createSMBFileShare_authentication,
    createSMBFileShare_validUserList,
    createSMBFileShare_notificationPolicy,
    createSMBFileShare_kmsKey,
    createSMBFileShare_auditDestinationARN,
    createSMBFileShare_adminUserList,
    createSMBFileShare_tags,
    createSMBFileShare_objectACL,
    createSMBFileShare_cacheAttributes,
    createSMBFileShare_requesterPays,
    createSMBFileShare_oplocksEnabled,
    createSMBFileShare_invalidUserList,
    createSMBFileShare_clientToken,
    createSMBFileShare_gatewayARN,
    createSMBFileShare_role,
    createSMBFileShare_locationARN,
    createSMBFileShareResponse_fileShareARN,
    createSMBFileShareResponse_httpStatus,

    -- ** UpdateSMBSecurityStrategy
    updateSMBSecurityStrategy_gatewayARN,
    updateSMBSecurityStrategy_sMBSecurityStrategy,
    updateSMBSecurityStrategyResponse_gatewayARN,
    updateSMBSecurityStrategyResponse_httpStatus,

    -- ** StartGateway
    startGateway_gatewayARN,
    startGatewayResponse_gatewayARN,
    startGatewayResponse_httpStatus,

    -- ** UpdateGatewaySoftwareNow
    updateGatewaySoftwareNow_gatewayARN,
    updateGatewaySoftwareNowResponse_gatewayARN,
    updateGatewaySoftwareNowResponse_httpStatus,

    -- ** CancelRetrieval
    cancelRetrieval_gatewayARN,
    cancelRetrieval_tapeARN,
    cancelRetrievalResponse_tapeARN,
    cancelRetrievalResponse_httpStatus,

    -- ** CreateTapePool
    createTapePool_retentionLockType,
    createTapePool_tags,
    createTapePool_retentionLockTimeInDays,
    createTapePool_poolName,
    createTapePool_storageClass,
    createTapePoolResponse_poolARN,
    createTapePoolResponse_httpStatus,

    -- ** ListTapePools
    listTapePools_limit,
    listTapePools_poolARNs,
    listTapePools_marker,
    listTapePoolsResponse_poolInfos,
    listTapePoolsResponse_marker,
    listTapePoolsResponse_httpStatus,

    -- ** DescribeBandwidthRateLimit
    describeBandwidthRateLimit_gatewayARN,
    describeBandwidthRateLimitResponse_averageUploadRateLimitInBitsPerSec,
    describeBandwidthRateLimitResponse_averageDownloadRateLimitInBitsPerSec,
    describeBandwidthRateLimitResponse_gatewayARN,
    describeBandwidthRateLimitResponse_httpStatus,

    -- ** CreateTapeWithBarcode
    createTapeWithBarcode_poolId,
    createTapeWithBarcode_kmsEncrypted,
    createTapeWithBarcode_kmsKey,
    createTapeWithBarcode_worm,
    createTapeWithBarcode_tags,
    createTapeWithBarcode_gatewayARN,
    createTapeWithBarcode_tapeSizeInBytes,
    createTapeWithBarcode_tapeBarcode,
    createTapeWithBarcodeResponse_tapeARN,
    createTapeWithBarcodeResponse_httpStatus,

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

    -- ** ListVolumeInitiators
    listVolumeInitiators_volumeARN,
    listVolumeInitiatorsResponse_initiators,
    listVolumeInitiatorsResponse_httpStatus,

    -- ** ListFileShares
    listFileShares_gatewayARN,
    listFileShares_limit,
    listFileShares_marker,
    listFileSharesResponse_nextMarker,
    listFileSharesResponse_fileShareInfoList,
    listFileSharesResponse_marker,
    listFileSharesResponse_httpStatus,

    -- ** CreateTapes
    createTapes_poolId,
    createTapes_kmsEncrypted,
    createTapes_kmsKey,
    createTapes_worm,
    createTapes_tags,
    createTapes_gatewayARN,
    createTapes_tapeSizeInBytes,
    createTapes_clientToken,
    createTapes_numTapesToCreate,
    createTapes_tapeBarcodePrefix,
    createTapesResponse_tapeARNs,
    createTapesResponse_httpStatus,

    -- ** UpdateVTLDeviceType
    updateVTLDeviceType_vTLDeviceARN,
    updateVTLDeviceType_deviceType,
    updateVTLDeviceTypeResponse_vTLDeviceARN,
    updateVTLDeviceTypeResponse_httpStatus,

    -- ** CreateCachediSCSIVolume
    createCachediSCSIVolume_kmsEncrypted,
    createCachediSCSIVolume_kmsKey,
    createCachediSCSIVolume_tags,
    createCachediSCSIVolume_snapshotId,
    createCachediSCSIVolume_sourceVolumeARN,
    createCachediSCSIVolume_gatewayARN,
    createCachediSCSIVolume_volumeSizeInBytes,
    createCachediSCSIVolume_targetName,
    createCachediSCSIVolume_networkInterfaceId,
    createCachediSCSIVolume_clientToken,
    createCachediSCSIVolumeResponse_volumeARN,
    createCachediSCSIVolumeResponse_targetARN,
    createCachediSCSIVolumeResponse_httpStatus,

    -- ** SetLocalConsolePassword
    setLocalConsolePassword_gatewayARN,
    setLocalConsolePassword_localConsolePassword,
    setLocalConsolePasswordResponse_gatewayARN,
    setLocalConsolePasswordResponse_httpStatus,

    -- ** AssociateFileSystem
    associateFileSystem_endpointNetworkConfiguration,
    associateFileSystem_auditDestinationARN,
    associateFileSystem_tags,
    associateFileSystem_cacheAttributes,
    associateFileSystem_userName,
    associateFileSystem_password,
    associateFileSystem_clientToken,
    associateFileSystem_gatewayARN,
    associateFileSystem_locationARN,
    associateFileSystemResponse_fileSystemAssociationARN,
    associateFileSystemResponse_httpStatus,

    -- ** DescribeChapCredentials
    describeChapCredentials_targetARN,
    describeChapCredentialsResponse_chapCredentials,
    describeChapCredentialsResponse_httpStatus,

    -- ** UpdateBandwidthRateLimitSchedule
    updateBandwidthRateLimitSchedule_gatewayARN,
    updateBandwidthRateLimitSchedule_bandwidthRateLimitIntervals,
    updateBandwidthRateLimitScheduleResponse_gatewayARN,
    updateBandwidthRateLimitScheduleResponse_httpStatus,

    -- ** DisableGateway
    disableGateway_gatewayARN,
    disableGatewayResponse_gatewayARN,
    disableGatewayResponse_httpStatus,

    -- ** DescribeSnapshotSchedule
    describeSnapshotSchedule_volumeARN,
    describeSnapshotScheduleResponse_recurrenceInHours,
    describeSnapshotScheduleResponse_volumeARN,
    describeSnapshotScheduleResponse_startAt,
    describeSnapshotScheduleResponse_tags,
    describeSnapshotScheduleResponse_description,
    describeSnapshotScheduleResponse_timezone,
    describeSnapshotScheduleResponse_httpStatus,

    -- ** DescribeTapeArchives
    describeTapeArchives_tapeARNs,
    describeTapeArchives_limit,
    describeTapeArchives_marker,
    describeTapeArchivesResponse_tapeArchives,
    describeTapeArchivesResponse_marker,
    describeTapeArchivesResponse_httpStatus,

    -- ** CreateStorediSCSIVolume
    createStorediSCSIVolume_kmsEncrypted,
    createStorediSCSIVolume_kmsKey,
    createStorediSCSIVolume_tags,
    createStorediSCSIVolume_snapshotId,
    createStorediSCSIVolume_gatewayARN,
    createStorediSCSIVolume_diskId,
    createStorediSCSIVolume_preserveExistingData,
    createStorediSCSIVolume_targetName,
    createStorediSCSIVolume_networkInterfaceId,
    createStorediSCSIVolumeResponse_volumeARN,
    createStorediSCSIVolumeResponse_targetARN,
    createStorediSCSIVolumeResponse_volumeSizeInBytes,
    createStorediSCSIVolumeResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceARN,
    addTagsToResource_tags,
    addTagsToResourceResponse_resourceARN,
    addTagsToResourceResponse_httpStatus,

    -- ** DeleteGateway
    deleteGateway_gatewayARN,
    deleteGatewayResponse_gatewayARN,
    deleteGatewayResponse_httpStatus,

    -- ** CreateSnapshotFromVolumeRecoveryPoint
    createSnapshotFromVolumeRecoveryPoint_tags,
    createSnapshotFromVolumeRecoveryPoint_volumeARN,
    createSnapshotFromVolumeRecoveryPoint_snapshotDescription,
    createSnapshotFromVolumeRecoveryPointResponse_volumeARN,
    createSnapshotFromVolumeRecoveryPointResponse_snapshotId,
    createSnapshotFromVolumeRecoveryPointResponse_volumeRecoveryPointTime,
    createSnapshotFromVolumeRecoveryPointResponse_httpStatus,

    -- ** DeleteBandwidthRateLimit
    deleteBandwidthRateLimit_gatewayARN,
    deleteBandwidthRateLimit_bandwidthType,
    deleteBandwidthRateLimitResponse_gatewayARN,
    deleteBandwidthRateLimitResponse_httpStatus,

    -- ** DescribeTapeRecoveryPoints
    describeTapeRecoveryPoints_limit,
    describeTapeRecoveryPoints_marker,
    describeTapeRecoveryPoints_gatewayARN,
    describeTapeRecoveryPointsResponse_tapeRecoveryPointInfos,
    describeTapeRecoveryPointsResponse_gatewayARN,
    describeTapeRecoveryPointsResponse_marker,
    describeTapeRecoveryPointsResponse_httpStatus,

    -- ** SetSMBGuestPassword
    setSMBGuestPassword_gatewayARN,
    setSMBGuestPassword_password,
    setSMBGuestPasswordResponse_gatewayARN,
    setSMBGuestPasswordResponse_httpStatus,

    -- ** UpdateBandwidthRateLimit
    updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_gatewayARN,
    updateBandwidthRateLimitResponse_gatewayARN,
    updateBandwidthRateLimitResponse_httpStatus,

    -- ** RetrieveTapeRecoveryPoint
    retrieveTapeRecoveryPoint_tapeARN,
    retrieveTapeRecoveryPoint_gatewayARN,
    retrieveTapeRecoveryPointResponse_tapeARN,
    retrieveTapeRecoveryPointResponse_httpStatus,

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

    -- ** DeleteVolume
    deleteVolume_volumeARN,
    deleteVolumeResponse_volumeARN,
    deleteVolumeResponse_httpStatus,

    -- ** DeleteTape
    deleteTape_bypassGovernanceRetention,
    deleteTape_gatewayARN,
    deleteTape_tapeARN,
    deleteTapeResponse_tapeARN,
    deleteTapeResponse_httpStatus,

    -- ** ResetCache
    resetCache_gatewayARN,
    resetCacheResponse_gatewayARN,
    resetCacheResponse_httpStatus,

    -- ** DescribeUploadBuffer
    describeUploadBuffer_gatewayARN,
    describeUploadBufferResponse_uploadBufferAllocatedInBytes,
    describeUploadBufferResponse_uploadBufferUsedInBytes,
    describeUploadBufferResponse_diskIds,
    describeUploadBufferResponse_gatewayARN,
    describeUploadBufferResponse_httpStatus,

    -- ** DescribeNFSFileShares
    describeNFSFileShares_fileShareARNList,
    describeNFSFileSharesResponse_nFSFileShareInfoList,
    describeNFSFileSharesResponse_httpStatus,

    -- ** DescribeStorediSCSIVolumes
    describeStorediSCSIVolumes_volumeARNs,
    describeStorediSCSIVolumesResponse_storediSCSIVolumes,
    describeStorediSCSIVolumesResponse_httpStatus,

    -- ** DeleteChapCredentials
    deleteChapCredentials_targetARN,
    deleteChapCredentials_initiatorName,
    deleteChapCredentialsResponse_initiatorName,
    deleteChapCredentialsResponse_targetARN,
    deleteChapCredentialsResponse_httpStatus,

    -- ** ListFileSystemAssociations
    listFileSystemAssociations_gatewayARN,
    listFileSystemAssociations_limit,
    listFileSystemAssociations_marker,
    listFileSystemAssociationsResponse_fileSystemAssociationSummaryList,
    listFileSystemAssociationsResponse_nextMarker,
    listFileSystemAssociationsResponse_marker,
    listFileSystemAssociationsResponse_httpStatus,

    -- ** UpdateFileSystemAssociation
    updateFileSystemAssociation_password,
    updateFileSystemAssociation_auditDestinationARN,
    updateFileSystemAssociation_userName,
    updateFileSystemAssociation_cacheAttributes,
    updateFileSystemAssociation_fileSystemAssociationARN,
    updateFileSystemAssociationResponse_fileSystemAssociationARN,
    updateFileSystemAssociationResponse_httpStatus,

    -- ** UpdateChapCredentials
    updateChapCredentials_secretToAuthenticateTarget,
    updateChapCredentials_targetARN,
    updateChapCredentials_secretToAuthenticateInitiator,
    updateChapCredentials_initiatorName,
    updateChapCredentialsResponse_initiatorName,
    updateChapCredentialsResponse_targetARN,
    updateChapCredentialsResponse_httpStatus,

    -- ** StartAvailabilityMonitorTest
    startAvailabilityMonitorTest_gatewayARN,
    startAvailabilityMonitorTestResponse_gatewayARN,
    startAvailabilityMonitorTestResponse_httpStatus,

    -- ** UpdateSnapshotSchedule
    updateSnapshotSchedule_tags,
    updateSnapshotSchedule_description,
    updateSnapshotSchedule_volumeARN,
    updateSnapshotSchedule_startAt,
    updateSnapshotSchedule_recurrenceInHours,
    updateSnapshotScheduleResponse_volumeARN,
    updateSnapshotScheduleResponse_httpStatus,

    -- ** DescribeVTLDevices
    describeVTLDevices_vTLDeviceARNs,
    describeVTLDevices_limit,
    describeVTLDevices_marker,
    describeVTLDevices_gatewayARN,
    describeVTLDevicesResponse_gatewayARN,
    describeVTLDevicesResponse_vTLDevices,
    describeVTLDevicesResponse_marker,
    describeVTLDevicesResponse_httpStatus,

    -- ** DeleteSnapshotSchedule
    deleteSnapshotSchedule_volumeARN,
    deleteSnapshotScheduleResponse_volumeARN,
    deleteSnapshotScheduleResponse_httpStatus,

    -- ** ListAutomaticTapeCreationPolicies
    listAutomaticTapeCreationPolicies_gatewayARN,
    listAutomaticTapeCreationPoliciesResponse_automaticTapeCreationPolicyInfos,
    listAutomaticTapeCreationPoliciesResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_tags,
    createSnapshot_volumeARN,
    createSnapshot_snapshotDescription,
    createSnapshotResponse_volumeARN,
    createSnapshotResponse_snapshotId,
    createSnapshotResponse_httpStatus,

    -- ** ShutdownGateway
    shutdownGateway_gatewayARN,
    shutdownGatewayResponse_gatewayARN,
    shutdownGatewayResponse_httpStatus,

    -- ** UpdateSMBFileShareVisibility
    updateSMBFileShareVisibility_gatewayARN,
    updateSMBFileShareVisibility_fileSharesVisible,
    updateSMBFileShareVisibilityResponse_gatewayARN,
    updateSMBFileShareVisibilityResponse_httpStatus,

    -- ** DescribeBandwidthRateLimitSchedule
    describeBandwidthRateLimitSchedule_gatewayARN,
    describeBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals,
    describeBandwidthRateLimitScheduleResponse_gatewayARN,
    describeBandwidthRateLimitScheduleResponse_httpStatus,

    -- ** AssignTapePool
    assignTapePool_bypassGovernanceRetention,
    assignTapePool_tapeARN,
    assignTapePool_poolId,
    assignTapePoolResponse_tapeARN,
    assignTapePoolResponse_httpStatus,

    -- ** DescribeCachediSCSIVolumes
    describeCachediSCSIVolumes_volumeARNs,
    describeCachediSCSIVolumesResponse_cachediSCSIVolumes,
    describeCachediSCSIVolumesResponse_httpStatus,

    -- ** NotifyWhenUploaded
    notifyWhenUploaded_fileShareARN,
    notifyWhenUploadedResponse_fileShareARN,
    notifyWhenUploadedResponse_notificationId,
    notifyWhenUploadedResponse_httpStatus,

    -- ** DescribeMaintenanceStartTime
    describeMaintenanceStartTime_gatewayARN,
    describeMaintenanceStartTimeResponse_dayOfWeek,
    describeMaintenanceStartTimeResponse_dayOfMonth,
    describeMaintenanceStartTimeResponse_minuteOfHour,
    describeMaintenanceStartTimeResponse_timezone,
    describeMaintenanceStartTimeResponse_gatewayARN,
    describeMaintenanceStartTimeResponse_hourOfDay,
    describeMaintenanceStartTimeResponse_httpStatus,

    -- ** UpdateGatewayInformation
    updateGatewayInformation_gatewayName,
    updateGatewayInformation_gatewayCapacity,
    updateGatewayInformation_gatewayTimezone,
    updateGatewayInformation_cloudWatchLogGroupARN,
    updateGatewayInformation_gatewayARN,
    updateGatewayInformationResponse_gatewayName,
    updateGatewayInformationResponse_gatewayARN,
    updateGatewayInformationResponse_httpStatus,

    -- ** DeleteAutomaticTapeCreationPolicy
    deleteAutomaticTapeCreationPolicy_gatewayARN,
    deleteAutomaticTapeCreationPolicyResponse_gatewayARN,
    deleteAutomaticTapeCreationPolicyResponse_httpStatus,

    -- ** DescribeWorkingStorage
    describeWorkingStorage_gatewayARN,
    describeWorkingStorageResponse_workingStorageUsedInBytes,
    describeWorkingStorageResponse_workingStorageAllocatedInBytes,
    describeWorkingStorageResponse_diskIds,
    describeWorkingStorageResponse_gatewayARN,
    describeWorkingStorageResponse_httpStatus,

    -- ** DeleteTapePool
    deleteTapePool_poolARN,
    deleteTapePoolResponse_poolARN,
    deleteTapePoolResponse_httpStatus,

    -- ** UpdateAutomaticTapeCreationPolicy
    updateAutomaticTapeCreationPolicy_automaticTapeCreationRules,
    updateAutomaticTapeCreationPolicy_gatewayARN,
    updateAutomaticTapeCreationPolicyResponse_gatewayARN,
    updateAutomaticTapeCreationPolicyResponse_httpStatus,

    -- ** AddCache
    addCache_gatewayARN,
    addCache_diskIds,
    addCacheResponse_gatewayARN,
    addCacheResponse_httpStatus,

    -- ** AddUploadBuffer
    addUploadBuffer_gatewayARN,
    addUploadBuffer_diskIds,
    addUploadBufferResponse_gatewayARN,
    addUploadBufferResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_limit,
    listTagsForResource_marker,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_marker,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteFileShare
    deleteFileShare_forceDelete,
    deleteFileShare_fileShareARN,
    deleteFileShareResponse_fileShareARN,
    deleteFileShareResponse_httpStatus,

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
    cachediSCSIVolume_createdDate,
    cachediSCSIVolume_sourceSnapshotId,
    cachediSCSIVolume_volumeStatus,
    cachediSCSIVolume_targetName,
    cachediSCSIVolume_volumeARN,
    cachediSCSIVolume_volumeId,
    cachediSCSIVolume_kmsKey,
    cachediSCSIVolume_volumeiSCSIAttributes,
    cachediSCSIVolume_volumeUsedInBytes,
    cachediSCSIVolume_volumeSizeInBytes,
    cachediSCSIVolume_volumeType,
    cachediSCSIVolume_volumeProgress,
    cachediSCSIVolume_volumeAttachmentStatus,

    -- ** ChapInfo
    chapInfo_initiatorName,
    chapInfo_targetARN,
    chapInfo_secretToAuthenticateTarget,
    chapInfo_secretToAuthenticateInitiator,

    -- ** DeviceiSCSIAttributes
    deviceiSCSIAttributes_chapEnabled,
    deviceiSCSIAttributes_targetARN,
    deviceiSCSIAttributes_networkInterfaceId,
    deviceiSCSIAttributes_networkInterfacePort,

    -- ** Disk
    disk_diskAllocationResource,
    disk_diskStatus,
    disk_diskSizeInBytes,
    disk_diskAttributeList,
    disk_diskPath,
    disk_diskId,
    disk_diskNode,
    disk_diskAllocationType,

    -- ** EndpointNetworkConfiguration
    endpointNetworkConfiguration_ipAddresses,

    -- ** FileShareInfo
    fileShareInfo_fileShareId,
    fileShareInfo_fileShareType,
    fileShareInfo_fileShareStatus,
    fileShareInfo_fileShareARN,
    fileShareInfo_gatewayARN,

    -- ** FileSystemAssociationInfo
    fileSystemAssociationInfo_fileSystemAssociationARN,
    fileSystemAssociationInfo_locationARN,
    fileSystemAssociationInfo_endpointNetworkConfiguration,
    fileSystemAssociationInfo_auditDestinationARN,
    fileSystemAssociationInfo_tags,
    fileSystemAssociationInfo_fileSystemAssociationStatus,
    fileSystemAssociationInfo_cacheAttributes,
    fileSystemAssociationInfo_gatewayARN,

    -- ** FileSystemAssociationSummary
    fileSystemAssociationSummary_fileSystemAssociationARN,
    fileSystemAssociationSummary_fileSystemAssociationId,
    fileSystemAssociationSummary_fileSystemAssociationStatus,
    fileSystemAssociationSummary_gatewayARN,

    -- ** GatewayInfo
    gatewayInfo_gatewayName,
    gatewayInfo_gatewayOperationalState,
    gatewayInfo_gatewayType,
    gatewayInfo_ec2InstanceRegion,
    gatewayInfo_ec2InstanceId,
    gatewayInfo_gatewayARN,
    gatewayInfo_gatewayId,

    -- ** NFSFileShareDefaults
    nFSFileShareDefaults_ownerId,
    nFSFileShareDefaults_groupId,
    nFSFileShareDefaults_directoryMode,
    nFSFileShareDefaults_fileMode,

    -- ** NFSFileShareInfo
    nFSFileShareInfo_defaultStorageClass,
    nFSFileShareInfo_fileShareName,
    nFSFileShareInfo_readOnly,
    nFSFileShareInfo_bucketRegion,
    nFSFileShareInfo_guessMIMETypeEnabled,
    nFSFileShareInfo_fileShareId,
    nFSFileShareInfo_kmsEncrypted,
    nFSFileShareInfo_locationARN,
    nFSFileShareInfo_vPCEndpointDNSName,
    nFSFileShareInfo_squash,
    nFSFileShareInfo_notificationPolicy,
    nFSFileShareInfo_kmsKey,
    nFSFileShareInfo_fileShareStatus,
    nFSFileShareInfo_role,
    nFSFileShareInfo_tags,
    nFSFileShareInfo_fileShareARN,
    nFSFileShareInfo_clientList,
    nFSFileShareInfo_objectACL,
    nFSFileShareInfo_cacheAttributes,
    nFSFileShareInfo_nFSFileShareDefaults,
    nFSFileShareInfo_gatewayARN,
    nFSFileShareInfo_requesterPays,
    nFSFileShareInfo_path,

    -- ** NetworkInterface
    networkInterface_macAddress,
    networkInterface_ipv6Address,
    networkInterface_ipv4Address,

    -- ** PoolInfo
    poolInfo_poolARN,
    poolInfo_poolName,
    poolInfo_storageClass,
    poolInfo_retentionLockType,
    poolInfo_poolStatus,
    poolInfo_retentionLockTimeInDays,

    -- ** SMBFileShareInfo
    sMBFileShareInfo_sMBACLEnabled,
    sMBFileShareInfo_defaultStorageClass,
    sMBFileShareInfo_accessBasedEnumeration,
    sMBFileShareInfo_fileShareName,
    sMBFileShareInfo_caseSensitivity,
    sMBFileShareInfo_readOnly,
    sMBFileShareInfo_bucketRegion,
    sMBFileShareInfo_guessMIMETypeEnabled,
    sMBFileShareInfo_fileShareId,
    sMBFileShareInfo_kmsEncrypted,
    sMBFileShareInfo_locationARN,
    sMBFileShareInfo_vPCEndpointDNSName,
    sMBFileShareInfo_authentication,
    sMBFileShareInfo_validUserList,
    sMBFileShareInfo_notificationPolicy,
    sMBFileShareInfo_kmsKey,
    sMBFileShareInfo_fileShareStatus,
    sMBFileShareInfo_auditDestinationARN,
    sMBFileShareInfo_adminUserList,
    sMBFileShareInfo_role,
    sMBFileShareInfo_tags,
    sMBFileShareInfo_fileShareARN,
    sMBFileShareInfo_objectACL,
    sMBFileShareInfo_cacheAttributes,
    sMBFileShareInfo_gatewayARN,
    sMBFileShareInfo_requesterPays,
    sMBFileShareInfo_path,
    sMBFileShareInfo_oplocksEnabled,
    sMBFileShareInfo_invalidUserList,

    -- ** StorediSCSIVolume
    storediSCSIVolume_createdDate,
    storediSCSIVolume_sourceSnapshotId,
    storediSCSIVolume_volumeStatus,
    storediSCSIVolume_targetName,
    storediSCSIVolume_volumeARN,
    storediSCSIVolume_volumeId,
    storediSCSIVolume_kmsKey,
    storediSCSIVolume_preservedExistingData,
    storediSCSIVolume_volumeiSCSIAttributes,
    storediSCSIVolume_volumeUsedInBytes,
    storediSCSIVolume_volumeSizeInBytes,
    storediSCSIVolume_volumeDiskId,
    storediSCSIVolume_volumeType,
    storediSCSIVolume_volumeProgress,
    storediSCSIVolume_volumeAttachmentStatus,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Tape
    tape_poolEntryDate,
    tape_tapeCreatedDate,
    tape_tapeStatus,
    tape_poolId,
    tape_vTLDevice,
    tape_tapeARN,
    tape_kmsKey,
    tape_worm,
    tape_tapeBarcode,
    tape_tapeUsedInBytes,
    tape_tapeSizeInBytes,
    tape_retentionStartDate,
    tape_progress,

    -- ** TapeArchive
    tapeArchive_poolEntryDate,
    tapeArchive_tapeCreatedDate,
    tapeArchive_tapeStatus,
    tapeArchive_poolId,
    tapeArchive_completionTime,
    tapeArchive_retrievedTo,
    tapeArchive_tapeARN,
    tapeArchive_kmsKey,
    tapeArchive_worm,
    tapeArchive_tapeBarcode,
    tapeArchive_tapeUsedInBytes,
    tapeArchive_tapeSizeInBytes,
    tapeArchive_retentionStartDate,

    -- ** TapeInfo
    tapeInfo_poolEntryDate,
    tapeInfo_tapeStatus,
    tapeInfo_poolId,
    tapeInfo_tapeARN,
    tapeInfo_tapeBarcode,
    tapeInfo_tapeSizeInBytes,
    tapeInfo_retentionStartDate,
    tapeInfo_gatewayARN,

    -- ** TapeRecoveryPointInfo
    tapeRecoveryPointInfo_tapeStatus,
    tapeRecoveryPointInfo_tapeARN,
    tapeRecoveryPointInfo_tapeSizeInBytes,
    tapeRecoveryPointInfo_tapeRecoveryPointTime,

    -- ** VTLDevice
    vTLDevice_vTLDeviceProductIdentifier,
    vTLDevice_vTLDeviceVendor,
    vTLDevice_deviceiSCSIAttributes,
    vTLDevice_vTLDeviceType,
    vTLDevice_vTLDeviceARN,

    -- ** VolumeInfo
    volumeInfo_volumeARN,
    volumeInfo_volumeId,
    volumeInfo_volumeSizeInBytes,
    volumeInfo_volumeType,
    volumeInfo_gatewayARN,
    volumeInfo_gatewayId,
    volumeInfo_volumeAttachmentStatus,

    -- ** VolumeRecoveryPointInfo
    volumeRecoveryPointInfo_volumeARN,
    volumeRecoveryPointInfo_volumeUsageInBytes,
    volumeRecoveryPointInfo_volumeSizeInBytes,
    volumeRecoveryPointInfo_volumeRecoveryPointTime,

    -- ** VolumeiSCSIAttributes
    volumeiSCSIAttributes_chapEnabled,
    volumeiSCSIAttributes_targetARN,
    volumeiSCSIAttributes_lunNumber,
    volumeiSCSIAttributes_networkInterfaceId,
    volumeiSCSIAttributes_networkInterfacePort,
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
import Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
import Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule
import Network.AWS.StorageGateway.Types.BandwidthRateLimitInterval
import Network.AWS.StorageGateway.Types.CacheAttributes
import Network.AWS.StorageGateway.Types.CachediSCSIVolume
import Network.AWS.StorageGateway.Types.ChapInfo
import Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes
import Network.AWS.StorageGateway.Types.Disk
import Network.AWS.StorageGateway.Types.EndpointNetworkConfiguration
import Network.AWS.StorageGateway.Types.FileShareInfo
import Network.AWS.StorageGateway.Types.FileSystemAssociationInfo
import Network.AWS.StorageGateway.Types.FileSystemAssociationSummary
import Network.AWS.StorageGateway.Types.GatewayInfo
import Network.AWS.StorageGateway.Types.NFSFileShareDefaults
import Network.AWS.StorageGateway.Types.NFSFileShareInfo
import Network.AWS.StorageGateway.Types.NetworkInterface
import Network.AWS.StorageGateway.Types.PoolInfo
import Network.AWS.StorageGateway.Types.SMBFileShareInfo
import Network.AWS.StorageGateway.Types.StorediSCSIVolume
import Network.AWS.StorageGateway.Types.Tag
import Network.AWS.StorageGateway.Types.Tape
import Network.AWS.StorageGateway.Types.TapeArchive
import Network.AWS.StorageGateway.Types.TapeInfo
import Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo
import Network.AWS.StorageGateway.Types.VTLDevice
import Network.AWS.StorageGateway.Types.VolumeInfo
import Network.AWS.StorageGateway.Types.VolumeRecoveryPointInfo
import Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes
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

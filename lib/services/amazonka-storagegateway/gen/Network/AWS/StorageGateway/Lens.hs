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

    -- ** CancelArchival
    cancelArchival_gatewayARN,
    cancelArchival_tapeARN,
    cancelArchivalResponse_tapeARN,
    cancelArchivalResponse_httpStatus,

    -- ** CreateStorediSCSIVolume
    createStorediSCSIVolume_kmsKey,
    createStorediSCSIVolume_kmsEncrypted,
    createStorediSCSIVolume_tags,
    createStorediSCSIVolume_snapshotId,
    createStorediSCSIVolume_gatewayARN,
    createStorediSCSIVolume_diskId,
    createStorediSCSIVolume_preserveExistingData,
    createStorediSCSIVolume_targetName,
    createStorediSCSIVolume_networkInterfaceId,
    createStorediSCSIVolumeResponse_targetARN,
    createStorediSCSIVolumeResponse_volumeARN,
    createStorediSCSIVolumeResponse_volumeSizeInBytes,
    createStorediSCSIVolumeResponse_httpStatus,

    -- ** DescribeFileSystemAssociations
    describeFileSystemAssociations_fileSystemAssociationARNList,
    describeFileSystemAssociationsResponse_fileSystemAssociationInfoList,
    describeFileSystemAssociationsResponse_httpStatus,

    -- ** CreateNFSFileShare
    createNFSFileShare_kmsKey,
    createNFSFileShare_vPCEndpointDNSName,
    createNFSFileShare_cacheAttributes,
    createNFSFileShare_objectACL,
    createNFSFileShare_kmsEncrypted,
    createNFSFileShare_defaultStorageClass,
    createNFSFileShare_fileShareName,
    createNFSFileShare_notificationPolicy,
    createNFSFileShare_squash,
    createNFSFileShare_requesterPays,
    createNFSFileShare_nFSFileShareDefaults,
    createNFSFileShare_clientList,
    createNFSFileShare_guessMIMETypeEnabled,
    createNFSFileShare_readOnly,
    createNFSFileShare_bucketRegion,
    createNFSFileShare_tags,
    createNFSFileShare_clientToken,
    createNFSFileShare_gatewayARN,
    createNFSFileShare_role,
    createNFSFileShare_locationARN,
    createNFSFileShareResponse_fileShareARN,
    createNFSFileShareResponse_httpStatus,

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

    -- ** DetachVolume
    detachVolume_forceDetach,
    detachVolume_volumeARN,
    detachVolumeResponse_volumeARN,
    detachVolumeResponse_httpStatus,

    -- ** DescribeChapCredentials
    describeChapCredentials_targetARN,
    describeChapCredentialsResponse_chapCredentials,
    describeChapCredentialsResponse_httpStatus,

    -- ** SetLocalConsolePassword
    setLocalConsolePassword_gatewayARN,
    setLocalConsolePassword_localConsolePassword,
    setLocalConsolePasswordResponse_gatewayARN,
    setLocalConsolePasswordResponse_httpStatus,

    -- ** CreateTapes
    createTapes_kmsKey,
    createTapes_kmsEncrypted,
    createTapes_poolId,
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
    createCachediSCSIVolume_kmsKey,
    createCachediSCSIVolume_sourceVolumeARN,
    createCachediSCSIVolume_kmsEncrypted,
    createCachediSCSIVolume_tags,
    createCachediSCSIVolume_snapshotId,
    createCachediSCSIVolume_gatewayARN,
    createCachediSCSIVolume_volumeSizeInBytes,
    createCachediSCSIVolume_targetName,
    createCachediSCSIVolume_networkInterfaceId,
    createCachediSCSIVolume_clientToken,
    createCachediSCSIVolumeResponse_targetARN,
    createCachediSCSIVolumeResponse_volumeARN,
    createCachediSCSIVolumeResponse_httpStatus,

    -- ** ListFileShares
    listFileShares_gatewayARN,
    listFileShares_marker,
    listFileShares_limit,
    listFileSharesResponse_fileShareInfoList,
    listFileSharesResponse_marker,
    listFileSharesResponse_nextMarker,
    listFileSharesResponse_httpStatus,

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

    -- ** DeleteFileShare
    deleteFileShare_forceDelete,
    deleteFileShare_fileShareARN,
    deleteFileShareResponse_fileShareARN,
    deleteFileShareResponse_httpStatus,

    -- ** ListVolumeInitiators
    listVolumeInitiators_volumeARN,
    listVolumeInitiatorsResponse_initiators,
    listVolumeInitiatorsResponse_httpStatus,

    -- ** AddUploadBuffer
    addUploadBuffer_gatewayARN,
    addUploadBuffer_diskIds,
    addUploadBufferResponse_gatewayARN,
    addUploadBufferResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_marker,
    listTagsForResource_limit,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_resourceARN,
    listTagsForResourceResponse_marker,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** NotifyWhenUploaded
    notifyWhenUploaded_fileShareARN,
    notifyWhenUploadedResponse_fileShareARN,
    notifyWhenUploadedResponse_notificationId,
    notifyWhenUploadedResponse_httpStatus,

    -- ** ListTapePools
    listTapePools_poolARNs,
    listTapePools_marker,
    listTapePools_limit,
    listTapePoolsResponse_poolInfos,
    listTapePoolsResponse_marker,
    listTapePoolsResponse_httpStatus,

    -- ** DeleteTapePool
    deleteTapePool_poolARN,
    deleteTapePoolResponse_poolARN,
    deleteTapePoolResponse_httpStatus,

    -- ** UpdateGatewayInformation
    updateGatewayInformation_gatewayCapacity,
    updateGatewayInformation_gatewayName,
    updateGatewayInformation_gatewayTimezone,
    updateGatewayInformation_cloudWatchLogGroupARN,
    updateGatewayInformation_gatewayARN,
    updateGatewayInformationResponse_gatewayARN,
    updateGatewayInformationResponse_gatewayName,
    updateGatewayInformationResponse_httpStatus,

    -- ** DescribeMaintenanceStartTime
    describeMaintenanceStartTime_gatewayARN,
    describeMaintenanceStartTimeResponse_gatewayARN,
    describeMaintenanceStartTimeResponse_minuteOfHour,
    describeMaintenanceStartTimeResponse_dayOfMonth,
    describeMaintenanceStartTimeResponse_hourOfDay,
    describeMaintenanceStartTimeResponse_timezone,
    describeMaintenanceStartTimeResponse_dayOfWeek,
    describeMaintenanceStartTimeResponse_httpStatus,

    -- ** AssignTapePool
    assignTapePool_bypassGovernanceRetention,
    assignTapePool_tapeARN,
    assignTapePool_poolId,
    assignTapePoolResponse_tapeARN,
    assignTapePoolResponse_httpStatus,

    -- ** DescribeWorkingStorage
    describeWorkingStorage_gatewayARN,
    describeWorkingStorageResponse_gatewayARN,
    describeWorkingStorageResponse_diskIds,
    describeWorkingStorageResponse_workingStorageAllocatedInBytes,
    describeWorkingStorageResponse_workingStorageUsedInBytes,
    describeWorkingStorageResponse_httpStatus,

    -- ** DescribeCachediSCSIVolumes
    describeCachediSCSIVolumes_volumeARNs,
    describeCachediSCSIVolumesResponse_cachediSCSIVolumes,
    describeCachediSCSIVolumesResponse_httpStatus,

    -- ** AddCache
    addCache_gatewayARN,
    addCache_diskIds,
    addCacheResponse_gatewayARN,
    addCacheResponse_httpStatus,

    -- ** CreateTapePool
    createTapePool_retentionLockType,
    createTapePool_retentionLockTimeInDays,
    createTapePool_tags,
    createTapePool_poolName,
    createTapePool_storageClass,
    createTapePoolResponse_poolARN,
    createTapePoolResponse_httpStatus,

    -- ** StartGateway
    startGateway_gatewayARN,
    startGatewayResponse_gatewayARN,
    startGatewayResponse_httpStatus,

    -- ** ShutdownGateway
    shutdownGateway_gatewayARN,
    shutdownGatewayResponse_gatewayARN,
    shutdownGatewayResponse_httpStatus,

    -- ** ListAutomaticTapeCreationPolicies
    listAutomaticTapeCreationPolicies_gatewayARN,
    listAutomaticTapeCreationPoliciesResponse_automaticTapeCreationPolicyInfos,
    listAutomaticTapeCreationPoliciesResponse_httpStatus,

    -- ** UpdateGatewaySoftwareNow
    updateGatewaySoftwareNow_gatewayARN,
    updateGatewaySoftwareNowResponse_gatewayARN,
    updateGatewaySoftwareNowResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceARN,
    removeTagsFromResource_tagKeys,
    removeTagsFromResourceResponse_resourceARN,
    removeTagsFromResourceResponse_httpStatus,

    -- ** UpdateFileSystemAssociation
    updateFileSystemAssociation_auditDestinationARN,
    updateFileSystemAssociation_cacheAttributes,
    updateFileSystemAssociation_userName,
    updateFileSystemAssociation_password,
    updateFileSystemAssociation_fileSystemAssociationARN,
    updateFileSystemAssociationResponse_fileSystemAssociationARN,
    updateFileSystemAssociationResponse_httpStatus,

    -- ** CreateSMBFileShare
    createSMBFileShare_accessBasedEnumeration,
    createSMBFileShare_adminUserList,
    createSMBFileShare_auditDestinationARN,
    createSMBFileShare_invalidUserList,
    createSMBFileShare_kmsKey,
    createSMBFileShare_validUserList,
    createSMBFileShare_vPCEndpointDNSName,
    createSMBFileShare_authentication,
    createSMBFileShare_cacheAttributes,
    createSMBFileShare_objectACL,
    createSMBFileShare_kmsEncrypted,
    createSMBFileShare_defaultStorageClass,
    createSMBFileShare_fileShareName,
    createSMBFileShare_sMBACLEnabled,
    createSMBFileShare_oplocksEnabled,
    createSMBFileShare_notificationPolicy,
    createSMBFileShare_requesterPays,
    createSMBFileShare_guessMIMETypeEnabled,
    createSMBFileShare_readOnly,
    createSMBFileShare_bucketRegion,
    createSMBFileShare_caseSensitivity,
    createSMBFileShare_tags,
    createSMBFileShare_clientToken,
    createSMBFileShare_gatewayARN,
    createSMBFileShare_role,
    createSMBFileShare_locationARN,
    createSMBFileShareResponse_fileShareARN,
    createSMBFileShareResponse_httpStatus,

    -- ** DeleteChapCredentials
    deleteChapCredentials_targetARN,
    deleteChapCredentials_initiatorName,
    deleteChapCredentialsResponse_targetARN,
    deleteChapCredentialsResponse_initiatorName,
    deleteChapCredentialsResponse_httpStatus,

    -- ** UpdateChapCredentials
    updateChapCredentials_secretToAuthenticateTarget,
    updateChapCredentials_targetARN,
    updateChapCredentials_secretToAuthenticateInitiator,
    updateChapCredentials_initiatorName,
    updateChapCredentialsResponse_targetARN,
    updateChapCredentialsResponse_initiatorName,
    updateChapCredentialsResponse_httpStatus,

    -- ** AttachVolume
    attachVolume_diskId,
    attachVolume_targetName,
    attachVolume_gatewayARN,
    attachVolume_volumeARN,
    attachVolume_networkInterfaceId,
    attachVolumeResponse_targetARN,
    attachVolumeResponse_volumeARN,
    attachVolumeResponse_httpStatus,

    -- ** DescribeAvailabilityMonitorTest
    describeAvailabilityMonitorTest_gatewayARN,
    describeAvailabilityMonitorTestResponse_status,
    describeAvailabilityMonitorTestResponse_startTime,
    describeAvailabilityMonitorTestResponse_gatewayARN,
    describeAvailabilityMonitorTestResponse_httpStatus,

    -- ** DescribeUploadBuffer
    describeUploadBuffer_gatewayARN,
    describeUploadBufferResponse_uploadBufferAllocatedInBytes,
    describeUploadBufferResponse_gatewayARN,
    describeUploadBufferResponse_diskIds,
    describeUploadBufferResponse_uploadBufferUsedInBytes,
    describeUploadBufferResponse_httpStatus,

    -- ** DescribeTapes
    describeTapes_marker,
    describeTapes_limit,
    describeTapes_tapeARNs,
    describeTapes_gatewayARN,
    describeTapesResponse_marker,
    describeTapesResponse_tapes,
    describeTapesResponse_httpStatus,

    -- ** DescribeStorediSCSIVolumes
    describeStorediSCSIVolumes_volumeARNs,
    describeStorediSCSIVolumesResponse_storediSCSIVolumes,
    describeStorediSCSIVolumesResponse_httpStatus,

    -- ** DisassociateFileSystem
    disassociateFileSystem_forceDelete,
    disassociateFileSystem_fileSystemAssociationARN,
    disassociateFileSystemResponse_fileSystemAssociationARN,
    disassociateFileSystemResponse_httpStatus,

    -- ** SetSMBGuestPassword
    setSMBGuestPassword_gatewayARN,
    setSMBGuestPassword_password,
    setSMBGuestPasswordResponse_gatewayARN,
    setSMBGuestPasswordResponse_httpStatus,

    -- ** CreateSnapshotFromVolumeRecoveryPoint
    createSnapshotFromVolumeRecoveryPoint_tags,
    createSnapshotFromVolumeRecoveryPoint_volumeARN,
    createSnapshotFromVolumeRecoveryPoint_snapshotDescription,
    createSnapshotFromVolumeRecoveryPointResponse_volumeRecoveryPointTime,
    createSnapshotFromVolumeRecoveryPointResponse_volumeARN,
    createSnapshotFromVolumeRecoveryPointResponse_snapshotId,
    createSnapshotFromVolumeRecoveryPointResponse_httpStatus,

    -- ** RetrieveTapeRecoveryPoint
    retrieveTapeRecoveryPoint_tapeARN,
    retrieveTapeRecoveryPoint_gatewayARN,
    retrieveTapeRecoveryPointResponse_tapeARN,
    retrieveTapeRecoveryPointResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceARN,
    addTagsToResource_tags,
    addTagsToResourceResponse_resourceARN,
    addTagsToResourceResponse_httpStatus,

    -- ** DeleteGateway
    deleteGateway_gatewayARN,
    deleteGatewayResponse_gatewayARN,
    deleteGatewayResponse_httpStatus,

    -- ** UpdateMaintenanceStartTime
    updateMaintenanceStartTime_dayOfMonth,
    updateMaintenanceStartTime_dayOfWeek,
    updateMaintenanceStartTime_gatewayARN,
    updateMaintenanceStartTime_hourOfDay,
    updateMaintenanceStartTime_minuteOfHour,
    updateMaintenanceStartTimeResponse_gatewayARN,
    updateMaintenanceStartTimeResponse_httpStatus,

    -- ** DescribeGatewayInformation
    describeGatewayInformation_gatewayARN,
    describeGatewayInformationResponse_gatewayState,
    describeGatewayInformationResponse_ec2InstanceRegion,
    describeGatewayInformationResponse_gatewayARN,
    describeGatewayInformationResponse_gatewayNetworkInterfaces,
    describeGatewayInformationResponse_ec2InstanceId,
    describeGatewayInformationResponse_nextUpdateAvailabilityDate,
    describeGatewayInformationResponse_endpointType,
    describeGatewayInformationResponse_deprecationDate,
    describeGatewayInformationResponse_lastSoftwareUpdate,
    describeGatewayInformationResponse_gatewayCapacity,
    describeGatewayInformationResponse_supportedGatewayCapacities,
    describeGatewayInformationResponse_gatewayName,
    describeGatewayInformationResponse_gatewayId,
    describeGatewayInformationResponse_hostEnvironment,
    describeGatewayInformationResponse_gatewayType,
    describeGatewayInformationResponse_gatewayTimezone,
    describeGatewayInformationResponse_softwareUpdatesEndDate,
    describeGatewayInformationResponse_cloudWatchLogGroupARN,
    describeGatewayInformationResponse_vPCEndpoint,
    describeGatewayInformationResponse_tags,
    describeGatewayInformationResponse_httpStatus,

    -- ** RefreshCache
    refreshCache_folderList,
    refreshCache_recursive,
    refreshCache_fileShareARN,
    refreshCacheResponse_fileShareARN,
    refreshCacheResponse_notificationId,
    refreshCacheResponse_httpStatus,

    -- ** UpdateNFSFileShare
    updateNFSFileShare_kmsKey,
    updateNFSFileShare_cacheAttributes,
    updateNFSFileShare_objectACL,
    updateNFSFileShare_kmsEncrypted,
    updateNFSFileShare_defaultStorageClass,
    updateNFSFileShare_fileShareName,
    updateNFSFileShare_notificationPolicy,
    updateNFSFileShare_squash,
    updateNFSFileShare_requesterPays,
    updateNFSFileShare_nFSFileShareDefaults,
    updateNFSFileShare_clientList,
    updateNFSFileShare_guessMIMETypeEnabled,
    updateNFSFileShare_readOnly,
    updateNFSFileShare_fileShareARN,
    updateNFSFileShareResponse_fileShareARN,
    updateNFSFileShareResponse_httpStatus,

    -- ** RetrieveTapeArchive
    retrieveTapeArchive_tapeARN,
    retrieveTapeArchive_gatewayARN,
    retrieveTapeArchiveResponse_tapeARN,
    retrieveTapeArchiveResponse_httpStatus,

    -- ** DescribeTapeArchives
    describeTapeArchives_marker,
    describeTapeArchives_limit,
    describeTapeArchives_tapeARNs,
    describeTapeArchivesResponse_tapeArchives,
    describeTapeArchivesResponse_marker,
    describeTapeArchivesResponse_httpStatus,

    -- ** UpdateBandwidthRateLimitSchedule
    updateBandwidthRateLimitSchedule_gatewayARN,
    updateBandwidthRateLimitSchedule_bandwidthRateLimitIntervals,
    updateBandwidthRateLimitScheduleResponse_gatewayARN,
    updateBandwidthRateLimitScheduleResponse_httpStatus,

    -- ** DisableGateway
    disableGateway_gatewayARN,
    disableGatewayResponse_gatewayARN,
    disableGatewayResponse_httpStatus,

    -- ** DescribeSMBSettings
    describeSMBSettings_gatewayARN,
    describeSMBSettingsResponse_gatewayARN,
    describeSMBSettingsResponse_fileSharesVisible,
    describeSMBSettingsResponse_activeDirectoryStatus,
    describeSMBSettingsResponse_domainName,
    describeSMBSettingsResponse_sMBGuestPasswordSet,
    describeSMBSettingsResponse_sMBSecurityStrategy,
    describeSMBSettingsResponse_httpStatus,

    -- ** DescribeSnapshotSchedule
    describeSnapshotSchedule_volumeARN,
    describeSnapshotScheduleResponse_startAt,
    describeSnapshotScheduleResponse_volumeARN,
    describeSnapshotScheduleResponse_recurrenceInHours,
    describeSnapshotScheduleResponse_timezone,
    describeSnapshotScheduleResponse_description,
    describeSnapshotScheduleResponse_tags,
    describeSnapshotScheduleResponse_httpStatus,

    -- ** CreateTapeWithBarcode
    createTapeWithBarcode_kmsKey,
    createTapeWithBarcode_kmsEncrypted,
    createTapeWithBarcode_poolId,
    createTapeWithBarcode_worm,
    createTapeWithBarcode_tags,
    createTapeWithBarcode_gatewayARN,
    createTapeWithBarcode_tapeSizeInBytes,
    createTapeWithBarcode_tapeBarcode,
    createTapeWithBarcodeResponse_tapeARN,
    createTapeWithBarcodeResponse_httpStatus,

    -- ** DescribeBandwidthRateLimit
    describeBandwidthRateLimit_gatewayARN,
    describeBandwidthRateLimitResponse_gatewayARN,
    describeBandwidthRateLimitResponse_averageUploadRateLimitInBitsPerSec,
    describeBandwidthRateLimitResponse_averageDownloadRateLimitInBitsPerSec,
    describeBandwidthRateLimitResponse_httpStatus,

    -- ** DeleteAutomaticTapeCreationPolicy
    deleteAutomaticTapeCreationPolicy_gatewayARN,
    deleteAutomaticTapeCreationPolicyResponse_gatewayARN,
    deleteAutomaticTapeCreationPolicyResponse_httpStatus,

    -- ** UpdateAutomaticTapeCreationPolicy
    updateAutomaticTapeCreationPolicy_automaticTapeCreationRules,
    updateAutomaticTapeCreationPolicy_gatewayARN,
    updateAutomaticTapeCreationPolicyResponse_gatewayARN,
    updateAutomaticTapeCreationPolicyResponse_httpStatus,

    -- ** UpdateSMBFileShareVisibility
    updateSMBFileShareVisibility_gatewayARN,
    updateSMBFileShareVisibility_fileSharesVisible,
    updateSMBFileShareVisibilityResponse_gatewayARN,
    updateSMBFileShareVisibilityResponse_httpStatus,

    -- ** DeleteSnapshotSchedule
    deleteSnapshotSchedule_volumeARN,
    deleteSnapshotScheduleResponse_volumeARN,
    deleteSnapshotScheduleResponse_httpStatus,

    -- ** UpdateSnapshotSchedule
    updateSnapshotSchedule_description,
    updateSnapshotSchedule_tags,
    updateSnapshotSchedule_volumeARN,
    updateSnapshotSchedule_startAt,
    updateSnapshotSchedule_recurrenceInHours,
    updateSnapshotScheduleResponse_volumeARN,
    updateSnapshotScheduleResponse_httpStatus,

    -- ** DescribeBandwidthRateLimitSchedule
    describeBandwidthRateLimitSchedule_gatewayARN,
    describeBandwidthRateLimitScheduleResponse_gatewayARN,
    describeBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals,
    describeBandwidthRateLimitScheduleResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_tags,
    createSnapshot_volumeARN,
    createSnapshot_snapshotDescription,
    createSnapshotResponse_volumeARN,
    createSnapshotResponse_snapshotId,
    createSnapshotResponse_httpStatus,

    -- ** UpdateSMBSecurityStrategy
    updateSMBSecurityStrategy_gatewayARN,
    updateSMBSecurityStrategy_sMBSecurityStrategy,
    updateSMBSecurityStrategyResponse_gatewayARN,
    updateSMBSecurityStrategyResponse_httpStatus,

    -- ** CancelRetrieval
    cancelRetrieval_gatewayARN,
    cancelRetrieval_tapeARN,
    cancelRetrievalResponse_tapeARN,
    cancelRetrievalResponse_httpStatus,

    -- ** DescribeVTLDevices
    describeVTLDevices_marker,
    describeVTLDevices_limit,
    describeVTLDevices_vTLDeviceARNs,
    describeVTLDevices_gatewayARN,
    describeVTLDevicesResponse_vTLDevices,
    describeVTLDevicesResponse_gatewayARN,
    describeVTLDevicesResponse_marker,
    describeVTLDevicesResponse_httpStatus,

    -- ** StartAvailabilityMonitorTest
    startAvailabilityMonitorTest_gatewayARN,
    startAvailabilityMonitorTestResponse_gatewayARN,
    startAvailabilityMonitorTestResponse_httpStatus,

    -- ** DeleteTapeArchive
    deleteTapeArchive_bypassGovernanceRetention,
    deleteTapeArchive_tapeARN,
    deleteTapeArchiveResponse_tapeARN,
    deleteTapeArchiveResponse_httpStatus,

    -- ** ListFileSystemAssociations
    listFileSystemAssociations_gatewayARN,
    listFileSystemAssociations_marker,
    listFileSystemAssociations_limit,
    listFileSystemAssociationsResponse_fileSystemAssociationSummaryList,
    listFileSystemAssociationsResponse_marker,
    listFileSystemAssociationsResponse_nextMarker,
    listFileSystemAssociationsResponse_httpStatus,

    -- ** UpdateSMBFileShare
    updateSMBFileShare_accessBasedEnumeration,
    updateSMBFileShare_adminUserList,
    updateSMBFileShare_auditDestinationARN,
    updateSMBFileShare_invalidUserList,
    updateSMBFileShare_kmsKey,
    updateSMBFileShare_validUserList,
    updateSMBFileShare_cacheAttributes,
    updateSMBFileShare_objectACL,
    updateSMBFileShare_kmsEncrypted,
    updateSMBFileShare_defaultStorageClass,
    updateSMBFileShare_fileShareName,
    updateSMBFileShare_sMBACLEnabled,
    updateSMBFileShare_oplocksEnabled,
    updateSMBFileShare_notificationPolicy,
    updateSMBFileShare_requesterPays,
    updateSMBFileShare_guessMIMETypeEnabled,
    updateSMBFileShare_readOnly,
    updateSMBFileShare_caseSensitivity,
    updateSMBFileShare_fileShareARN,
    updateSMBFileShareResponse_fileShareARN,
    updateSMBFileShareResponse_httpStatus,

    -- ** DescribeNFSFileShares
    describeNFSFileShares_fileShareARNList,
    describeNFSFileSharesResponse_nFSFileShareInfoList,
    describeNFSFileSharesResponse_httpStatus,

    -- ** ListVolumeRecoveryPoints
    listVolumeRecoveryPoints_gatewayARN,
    listVolumeRecoveryPointsResponse_volumeRecoveryPointInfos,
    listVolumeRecoveryPointsResponse_gatewayARN,
    listVolumeRecoveryPointsResponse_httpStatus,

    -- ** ListTapes
    listTapes_marker,
    listTapes_limit,
    listTapes_tapeARNs,
    listTapesResponse_marker,
    listTapesResponse_tapeInfos,
    listTapesResponse_httpStatus,

    -- ** ResetCache
    resetCache_gatewayARN,
    resetCacheResponse_gatewayARN,
    resetCacheResponse_httpStatus,

    -- ** DescribeSMBFileShares
    describeSMBFileShares_fileShareARNList,
    describeSMBFileSharesResponse_sMBFileShareInfoList,
    describeSMBFileSharesResponse_httpStatus,

    -- ** ListGateways
    listGateways_marker,
    listGateways_limit,
    listGatewaysResponse_marker,
    listGatewaysResponse_gateways,
    listGatewaysResponse_httpStatus,

    -- ** DeleteTape
    deleteTape_bypassGovernanceRetention,
    deleteTape_gatewayARN,
    deleteTape_tapeARN,
    deleteTapeResponse_tapeARN,
    deleteTapeResponse_httpStatus,

    -- ** ListLocalDisks
    listLocalDisks_gatewayARN,
    listLocalDisksResponse_gatewayARN,
    listLocalDisksResponse_disks,
    listLocalDisksResponse_httpStatus,

    -- ** ListVolumes
    listVolumes_gatewayARN,
    listVolumes_marker,
    listVolumes_limit,
    listVolumesResponse_gatewayARN,
    listVolumesResponse_marker,
    listVolumesResponse_volumeInfos,
    listVolumesResponse_httpStatus,

    -- ** UpdateBandwidthRateLimit
    updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_gatewayARN,
    updateBandwidthRateLimitResponse_gatewayARN,
    updateBandwidthRateLimitResponse_httpStatus,

    -- ** AddWorkingStorage
    addWorkingStorage_gatewayARN,
    addWorkingStorage_diskIds,
    addWorkingStorageResponse_gatewayARN,
    addWorkingStorageResponse_httpStatus,

    -- ** DescribeTapeRecoveryPoints
    describeTapeRecoveryPoints_marker,
    describeTapeRecoveryPoints_limit,
    describeTapeRecoveryPoints_gatewayARN,
    describeTapeRecoveryPointsResponse_tapeRecoveryPointInfos,
    describeTapeRecoveryPointsResponse_gatewayARN,
    describeTapeRecoveryPointsResponse_marker,
    describeTapeRecoveryPointsResponse_httpStatus,

    -- ** DeleteBandwidthRateLimit
    deleteBandwidthRateLimit_gatewayARN,
    deleteBandwidthRateLimit_bandwidthType,
    deleteBandwidthRateLimitResponse_gatewayARN,
    deleteBandwidthRateLimitResponse_httpStatus,

    -- ** ActivateGateway
    activateGateway_mediumChangerType,
    activateGateway_tapeDriveType,
    activateGateway_gatewayType,
    activateGateway_tags,
    activateGateway_activationKey,
    activateGateway_gatewayName,
    activateGateway_gatewayTimezone,
    activateGateway_gatewayRegion,
    activateGatewayResponse_gatewayARN,
    activateGatewayResponse_httpStatus,

    -- ** DescribeCache
    describeCache_gatewayARN,
    describeCacheResponse_gatewayARN,
    describeCacheResponse_diskIds,
    describeCacheResponse_cacheUsedPercentage,
    describeCacheResponse_cacheHitPercentage,
    describeCacheResponse_cacheMissPercentage,
    describeCacheResponse_cacheAllocatedInBytes,
    describeCacheResponse_cacheDirtyPercentage,
    describeCacheResponse_httpStatus,

    -- ** DeleteVolume
    deleteVolume_volumeARN,
    deleteVolumeResponse_volumeARN,
    deleteVolumeResponse_httpStatus,

    -- * Types

    -- ** AutomaticTapeCreationPolicyInfo
    automaticTapeCreationPolicyInfo_gatewayARN,
    automaticTapeCreationPolicyInfo_automaticTapeCreationRules,

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
    cachediSCSIVolume_volumeiSCSIAttributes,
    cachediSCSIVolume_volumeStatus,
    cachediSCSIVolume_sourceSnapshotId,
    cachediSCSIVolume_kmsKey,
    cachediSCSIVolume_volumeAttachmentStatus,
    cachediSCSIVolume_volumeARN,
    cachediSCSIVolume_volumeProgress,
    cachediSCSIVolume_volumeSizeInBytes,
    cachediSCSIVolume_volumeUsedInBytes,
    cachediSCSIVolume_createdDate,
    cachediSCSIVolume_volumeId,
    cachediSCSIVolume_volumeType,
    cachediSCSIVolume_targetName,

    -- ** ChapInfo
    chapInfo_targetARN,
    chapInfo_secretToAuthenticateInitiator,
    chapInfo_initiatorName,
    chapInfo_secretToAuthenticateTarget,

    -- ** DeviceiSCSIAttributes
    deviceiSCSIAttributes_targetARN,
    deviceiSCSIAttributes_chapEnabled,
    deviceiSCSIAttributes_networkInterfaceId,
    deviceiSCSIAttributes_networkInterfacePort,

    -- ** Disk
    disk_diskAllocationResource,
    disk_diskAllocationType,
    disk_diskNode,
    disk_diskPath,
    disk_diskSizeInBytes,
    disk_diskStatus,
    disk_diskId,
    disk_diskAttributeList,

    -- ** EndpointNetworkConfiguration
    endpointNetworkConfiguration_ipAddresses,

    -- ** FileShareInfo
    fileShareInfo_fileShareStatus,
    fileShareInfo_gatewayARN,
    fileShareInfo_fileShareId,
    fileShareInfo_fileShareARN,
    fileShareInfo_fileShareType,

    -- ** FileSystemAssociationInfo
    fileSystemAssociationInfo_auditDestinationARN,
    fileSystemAssociationInfo_fileSystemAssociationARN,
    fileSystemAssociationInfo_gatewayARN,
    fileSystemAssociationInfo_cacheAttributes,
    fileSystemAssociationInfo_endpointNetworkConfiguration,
    fileSystemAssociationInfo_locationARN,
    fileSystemAssociationInfo_fileSystemAssociationStatus,
    fileSystemAssociationInfo_tags,

    -- ** FileSystemAssociationSummary
    fileSystemAssociationSummary_fileSystemAssociationARN,
    fileSystemAssociationSummary_gatewayARN,
    fileSystemAssociationSummary_fileSystemAssociationId,
    fileSystemAssociationSummary_fileSystemAssociationStatus,

    -- ** GatewayInfo
    gatewayInfo_ec2InstanceRegion,
    gatewayInfo_gatewayARN,
    gatewayInfo_ec2InstanceId,
    gatewayInfo_gatewayOperationalState,
    gatewayInfo_gatewayName,
    gatewayInfo_gatewayId,
    gatewayInfo_gatewayType,

    -- ** NFSFileShareDefaults
    nFSFileShareDefaults_fileMode,
    nFSFileShareDefaults_ownerId,
    nFSFileShareDefaults_directoryMode,
    nFSFileShareDefaults_groupId,

    -- ** NFSFileShareInfo
    nFSFileShareInfo_fileShareStatus,
    nFSFileShareInfo_kmsKey,
    nFSFileShareInfo_gatewayARN,
    nFSFileShareInfo_path,
    nFSFileShareInfo_vPCEndpointDNSName,
    nFSFileShareInfo_cacheAttributes,
    nFSFileShareInfo_objectACL,
    nFSFileShareInfo_kmsEncrypted,
    nFSFileShareInfo_fileShareId,
    nFSFileShareInfo_fileShareARN,
    nFSFileShareInfo_defaultStorageClass,
    nFSFileShareInfo_fileShareName,
    nFSFileShareInfo_role,
    nFSFileShareInfo_notificationPolicy,
    nFSFileShareInfo_squash,
    nFSFileShareInfo_requesterPays,
    nFSFileShareInfo_nFSFileShareDefaults,
    nFSFileShareInfo_locationARN,
    nFSFileShareInfo_clientList,
    nFSFileShareInfo_guessMIMETypeEnabled,
    nFSFileShareInfo_readOnly,
    nFSFileShareInfo_bucketRegion,
    nFSFileShareInfo_tags,

    -- ** NetworkInterface
    networkInterface_ipv6Address,
    networkInterface_macAddress,
    networkInterface_ipv4Address,

    -- ** PoolInfo
    poolInfo_retentionLockType,
    poolInfo_retentionLockTimeInDays,
    poolInfo_poolName,
    poolInfo_storageClass,
    poolInfo_poolStatus,
    poolInfo_poolARN,

    -- ** SMBFileShareInfo
    sMBFileShareInfo_accessBasedEnumeration,
    sMBFileShareInfo_adminUserList,
    sMBFileShareInfo_auditDestinationARN,
    sMBFileShareInfo_fileShareStatus,
    sMBFileShareInfo_invalidUserList,
    sMBFileShareInfo_kmsKey,
    sMBFileShareInfo_validUserList,
    sMBFileShareInfo_gatewayARN,
    sMBFileShareInfo_path,
    sMBFileShareInfo_vPCEndpointDNSName,
    sMBFileShareInfo_authentication,
    sMBFileShareInfo_cacheAttributes,
    sMBFileShareInfo_objectACL,
    sMBFileShareInfo_kmsEncrypted,
    sMBFileShareInfo_fileShareId,
    sMBFileShareInfo_fileShareARN,
    sMBFileShareInfo_defaultStorageClass,
    sMBFileShareInfo_fileShareName,
    sMBFileShareInfo_role,
    sMBFileShareInfo_sMBACLEnabled,
    sMBFileShareInfo_oplocksEnabled,
    sMBFileShareInfo_notificationPolicy,
    sMBFileShareInfo_requesterPays,
    sMBFileShareInfo_locationARN,
    sMBFileShareInfo_guessMIMETypeEnabled,
    sMBFileShareInfo_readOnly,
    sMBFileShareInfo_bucketRegion,
    sMBFileShareInfo_caseSensitivity,
    sMBFileShareInfo_tags,

    -- ** StorediSCSIVolume
    storediSCSIVolume_volumeiSCSIAttributes,
    storediSCSIVolume_volumeStatus,
    storediSCSIVolume_sourceSnapshotId,
    storediSCSIVolume_preservedExistingData,
    storediSCSIVolume_kmsKey,
    storediSCSIVolume_volumeAttachmentStatus,
    storediSCSIVolume_volumeARN,
    storediSCSIVolume_volumeProgress,
    storediSCSIVolume_volumeSizeInBytes,
    storediSCSIVolume_volumeUsedInBytes,
    storediSCSIVolume_createdDate,
    storediSCSIVolume_volumeId,
    storediSCSIVolume_volumeDiskId,
    storediSCSIVolume_volumeType,
    storediSCSIVolume_targetName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Tape
    tape_tapeBarcode,
    tape_tapeStatus,
    tape_kmsKey,
    tape_tapeARN,
    tape_progress,
    tape_tapeSizeInBytes,
    tape_vTLDevice,
    tape_poolId,
    tape_tapeUsedInBytes,
    tape_tapeCreatedDate,
    tape_poolEntryDate,
    tape_worm,
    tape_retentionStartDate,

    -- ** TapeArchive
    tapeArchive_tapeBarcode,
    tapeArchive_tapeStatus,
    tapeArchive_kmsKey,
    tapeArchive_tapeARN,
    tapeArchive_tapeSizeInBytes,
    tapeArchive_completionTime,
    tapeArchive_poolId,
    tapeArchive_tapeUsedInBytes,
    tapeArchive_tapeCreatedDate,
    tapeArchive_poolEntryDate,
    tapeArchive_worm,
    tapeArchive_retentionStartDate,
    tapeArchive_retrievedTo,

    -- ** TapeInfo
    tapeInfo_tapeBarcode,
    tapeInfo_tapeStatus,
    tapeInfo_tapeARN,
    tapeInfo_gatewayARN,
    tapeInfo_tapeSizeInBytes,
    tapeInfo_poolId,
    tapeInfo_poolEntryDate,
    tapeInfo_retentionStartDate,

    -- ** TapeRecoveryPointInfo
    tapeRecoveryPointInfo_tapeStatus,
    tapeRecoveryPointInfo_tapeRecoveryPointTime,
    tapeRecoveryPointInfo_tapeARN,
    tapeRecoveryPointInfo_tapeSizeInBytes,

    -- ** VTLDevice
    vTLDevice_deviceiSCSIAttributes,
    vTLDevice_vTLDeviceVendor,
    vTLDevice_vTLDeviceARN,
    vTLDevice_vTLDeviceType,
    vTLDevice_vTLDeviceProductIdentifier,

    -- ** VolumeInfo
    volumeInfo_gatewayARN,
    volumeInfo_volumeAttachmentStatus,
    volumeInfo_volumeARN,
    volumeInfo_volumeSizeInBytes,
    volumeInfo_volumeId,
    volumeInfo_gatewayId,
    volumeInfo_volumeType,

    -- ** VolumeRecoveryPointInfo
    volumeRecoveryPointInfo_volumeRecoveryPointTime,
    volumeRecoveryPointInfo_volumeARN,
    volumeRecoveryPointInfo_volumeSizeInBytes,
    volumeRecoveryPointInfo_volumeUsageInBytes,

    -- ** VolumeiSCSIAttributes
    volumeiSCSIAttributes_lunNumber,
    volumeiSCSIAttributes_targetARN,
    volumeiSCSIAttributes_chapEnabled,
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

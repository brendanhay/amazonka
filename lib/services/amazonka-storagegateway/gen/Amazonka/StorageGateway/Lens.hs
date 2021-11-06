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

    -- ** UpdateSMBLocalGroups
    updateSMBLocalGroups_gatewayARN,
    updateSMBLocalGroups_sMBLocalGroups,
    updateSMBLocalGroupsResponse_gatewayARN,
    updateSMBLocalGroupsResponse_httpStatus,

    -- ** CreateNFSFileShare
    createNFSFileShare_auditDestinationARN,
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
    updateNFSFileShare_auditDestinationARN,
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
    describeSMBSettingsResponse_sMBLocalGroups,
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
    fileSystemAssociationInfo_fileSystemAssociationStatusDetails,
    fileSystemAssociationInfo_endpointNetworkConfiguration,
    fileSystemAssociationInfo_locationARN,
    fileSystemAssociationInfo_fileSystemAssociationStatus,
    fileSystemAssociationInfo_tags,

    -- ** FileSystemAssociationStatusDetail
    fileSystemAssociationStatusDetail_errorCode,

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
    nFSFileShareInfo_auditDestinationARN,
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

    -- ** SMBLocalGroups
    sMBLocalGroups_gatewayAdmins,

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

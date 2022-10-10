{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FSx.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Lens
  ( -- * Operations

    -- ** AssociateFileSystemAliases
    associateFileSystemAliases_clientRequestToken,
    associateFileSystemAliases_fileSystemId,
    associateFileSystemAliases_aliases,
    associateFileSystemAliasesResponse_aliases,
    associateFileSystemAliasesResponse_httpStatus,

    -- ** CancelDataRepositoryTask
    cancelDataRepositoryTask_taskId,
    cancelDataRepositoryTaskResponse_lifecycle,
    cancelDataRepositoryTaskResponse_taskId,
    cancelDataRepositoryTaskResponse_httpStatus,

    -- ** CopyBackup
    copyBackup_tags,
    copyBackup_sourceRegion,
    copyBackup_clientRequestToken,
    copyBackup_copyTags,
    copyBackup_kmsKeyId,
    copyBackup_sourceBackupId,
    copyBackupResponse_backup,
    copyBackupResponse_httpStatus,

    -- ** CreateBackup
    createBackup_tags,
    createBackup_clientRequestToken,
    createBackup_fileSystemId,
    createBackup_volumeId,
    createBackupResponse_backup,
    createBackupResponse_httpStatus,

    -- ** CreateDataRepositoryAssociation
    createDataRepositoryAssociation_tags,
    createDataRepositoryAssociation_clientRequestToken,
    createDataRepositoryAssociation_s3,
    createDataRepositoryAssociation_importedFileChunkSize,
    createDataRepositoryAssociation_fileSystemPath,
    createDataRepositoryAssociation_batchImportMetaDataOnCreate,
    createDataRepositoryAssociation_fileSystemId,
    createDataRepositoryAssociation_dataRepositoryPath,
    createDataRepositoryAssociationResponse_association,
    createDataRepositoryAssociationResponse_httpStatus,

    -- ** CreateDataRepositoryTask
    createDataRepositoryTask_tags,
    createDataRepositoryTask_clientRequestToken,
    createDataRepositoryTask_capacityToRelease,
    createDataRepositoryTask_paths,
    createDataRepositoryTask_type,
    createDataRepositoryTask_fileSystemId,
    createDataRepositoryTask_report,
    createDataRepositoryTaskResponse_dataRepositoryTask,
    createDataRepositoryTaskResponse_httpStatus,

    -- ** CreateFileCache
    createFileCache_tags,
    createFileCache_clientRequestToken,
    createFileCache_securityGroupIds,
    createFileCache_copyTagsToDataRepositoryAssociations,
    createFileCache_kmsKeyId,
    createFileCache_dataRepositoryAssociations,
    createFileCache_lustreConfiguration,
    createFileCache_fileCacheType,
    createFileCache_fileCacheTypeVersion,
    createFileCache_storageCapacity,
    createFileCache_subnetIds,
    createFileCacheResponse_fileCache,
    createFileCacheResponse_httpStatus,

    -- ** CreateFileSystem
    createFileSystem_tags,
    createFileSystem_clientRequestToken,
    createFileSystem_fileSystemTypeVersion,
    createFileSystem_securityGroupIds,
    createFileSystem_openZFSConfiguration,
    createFileSystem_storageType,
    createFileSystem_ontapConfiguration,
    createFileSystem_windowsConfiguration,
    createFileSystem_kmsKeyId,
    createFileSystem_lustreConfiguration,
    createFileSystem_fileSystemType,
    createFileSystem_storageCapacity,
    createFileSystem_subnetIds,
    createFileSystemResponse_fileSystem,
    createFileSystemResponse_httpStatus,

    -- ** CreateFileSystemFromBackup
    createFileSystemFromBackup_tags,
    createFileSystemFromBackup_clientRequestToken,
    createFileSystemFromBackup_fileSystemTypeVersion,
    createFileSystemFromBackup_securityGroupIds,
    createFileSystemFromBackup_openZFSConfiguration,
    createFileSystemFromBackup_storageCapacity,
    createFileSystemFromBackup_storageType,
    createFileSystemFromBackup_windowsConfiguration,
    createFileSystemFromBackup_kmsKeyId,
    createFileSystemFromBackup_lustreConfiguration,
    createFileSystemFromBackup_backupId,
    createFileSystemFromBackup_subnetIds,
    createFileSystemFromBackupResponse_fileSystem,
    createFileSystemFromBackupResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_tags,
    createSnapshot_clientRequestToken,
    createSnapshot_name,
    createSnapshot_volumeId,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** CreateStorageVirtualMachine
    createStorageVirtualMachine_tags,
    createStorageVirtualMachine_activeDirectoryConfiguration,
    createStorageVirtualMachine_clientRequestToken,
    createStorageVirtualMachine_rootVolumeSecurityStyle,
    createStorageVirtualMachine_svmAdminPassword,
    createStorageVirtualMachine_fileSystemId,
    createStorageVirtualMachine_name,
    createStorageVirtualMachineResponse_storageVirtualMachine,
    createStorageVirtualMachineResponse_httpStatus,

    -- ** CreateVolume
    createVolume_tags,
    createVolume_clientRequestToken,
    createVolume_openZFSConfiguration,
    createVolume_ontapConfiguration,
    createVolume_volumeType,
    createVolume_name,
    createVolumeResponse_volume,
    createVolumeResponse_httpStatus,

    -- ** CreateVolumeFromBackup
    createVolumeFromBackup_tags,
    createVolumeFromBackup_clientRequestToken,
    createVolumeFromBackup_ontapConfiguration,
    createVolumeFromBackup_backupId,
    createVolumeFromBackup_name,
    createVolumeFromBackupResponse_volume,
    createVolumeFromBackupResponse_httpStatus,

    -- ** DeleteBackup
    deleteBackup_clientRequestToken,
    deleteBackup_backupId,
    deleteBackupResponse_backupId,
    deleteBackupResponse_lifecycle,
    deleteBackupResponse_httpStatus,

    -- ** DeleteDataRepositoryAssociation
    deleteDataRepositoryAssociation_clientRequestToken,
    deleteDataRepositoryAssociation_deleteDataInFileSystem,
    deleteDataRepositoryAssociation_associationId,
    deleteDataRepositoryAssociationResponse_lifecycle,
    deleteDataRepositoryAssociationResponse_deleteDataInFileSystem,
    deleteDataRepositoryAssociationResponse_associationId,
    deleteDataRepositoryAssociationResponse_httpStatus,

    -- ** DeleteFileCache
    deleteFileCache_clientRequestToken,
    deleteFileCache_fileCacheId,
    deleteFileCacheResponse_lifecycle,
    deleteFileCacheResponse_fileCacheId,
    deleteFileCacheResponse_httpStatus,

    -- ** DeleteFileSystem
    deleteFileSystem_clientRequestToken,
    deleteFileSystem_openZFSConfiguration,
    deleteFileSystem_windowsConfiguration,
    deleteFileSystem_lustreConfiguration,
    deleteFileSystem_fileSystemId,
    deleteFileSystemResponse_openZFSResponse,
    deleteFileSystemResponse_lifecycle,
    deleteFileSystemResponse_windowsResponse,
    deleteFileSystemResponse_fileSystemId,
    deleteFileSystemResponse_lustreResponse,
    deleteFileSystemResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_clientRequestToken,
    deleteSnapshot_snapshotId,
    deleteSnapshotResponse_lifecycle,
    deleteSnapshotResponse_snapshotId,
    deleteSnapshotResponse_httpStatus,

    -- ** DeleteStorageVirtualMachine
    deleteStorageVirtualMachine_clientRequestToken,
    deleteStorageVirtualMachine_storageVirtualMachineId,
    deleteStorageVirtualMachineResponse_lifecycle,
    deleteStorageVirtualMachineResponse_storageVirtualMachineId,
    deleteStorageVirtualMachineResponse_httpStatus,

    -- ** DeleteVolume
    deleteVolume_clientRequestToken,
    deleteVolume_openZFSConfiguration,
    deleteVolume_ontapConfiguration,
    deleteVolume_volumeId,
    deleteVolumeResponse_ontapResponse,
    deleteVolumeResponse_lifecycle,
    deleteVolumeResponse_volumeId,
    deleteVolumeResponse_httpStatus,

    -- ** DescribeBackups
    describeBackups_nextToken,
    describeBackups_filters,
    describeBackups_maxResults,
    describeBackups_backupIds,
    describeBackupsResponse_nextToken,
    describeBackupsResponse_backups,
    describeBackupsResponse_httpStatus,

    -- ** DescribeDataRepositoryAssociations
    describeDataRepositoryAssociations_nextToken,
    describeDataRepositoryAssociations_filters,
    describeDataRepositoryAssociations_maxResults,
    describeDataRepositoryAssociations_associationIds,
    describeDataRepositoryAssociationsResponse_nextToken,
    describeDataRepositoryAssociationsResponse_associations,
    describeDataRepositoryAssociationsResponse_httpStatus,

    -- ** DescribeDataRepositoryTasks
    describeDataRepositoryTasks_nextToken,
    describeDataRepositoryTasks_filters,
    describeDataRepositoryTasks_taskIds,
    describeDataRepositoryTasks_maxResults,
    describeDataRepositoryTasksResponse_nextToken,
    describeDataRepositoryTasksResponse_dataRepositoryTasks,
    describeDataRepositoryTasksResponse_httpStatus,

    -- ** DescribeFileCaches
    describeFileCaches_nextToken,
    describeFileCaches_fileCacheIds,
    describeFileCaches_maxResults,
    describeFileCachesResponse_nextToken,
    describeFileCachesResponse_fileCaches,
    describeFileCachesResponse_httpStatus,

    -- ** DescribeFileSystemAliases
    describeFileSystemAliases_nextToken,
    describeFileSystemAliases_clientRequestToken,
    describeFileSystemAliases_maxResults,
    describeFileSystemAliases_fileSystemId,
    describeFileSystemAliasesResponse_nextToken,
    describeFileSystemAliasesResponse_aliases,
    describeFileSystemAliasesResponse_httpStatus,

    -- ** DescribeFileSystems
    describeFileSystems_nextToken,
    describeFileSystems_fileSystemIds,
    describeFileSystems_maxResults,
    describeFileSystemsResponse_nextToken,
    describeFileSystemsResponse_fileSystems,
    describeFileSystemsResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_nextToken,
    describeSnapshots_filters,
    describeSnapshots_snapshotIds,
    describeSnapshots_maxResults,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** DescribeStorageVirtualMachines
    describeStorageVirtualMachines_nextToken,
    describeStorageVirtualMachines_filters,
    describeStorageVirtualMachines_storageVirtualMachineIds,
    describeStorageVirtualMachines_maxResults,
    describeStorageVirtualMachinesResponse_nextToken,
    describeStorageVirtualMachinesResponse_storageVirtualMachines,
    describeStorageVirtualMachinesResponse_httpStatus,

    -- ** DescribeVolumes
    describeVolumes_nextToken,
    describeVolumes_volumeIds,
    describeVolumes_filters,
    describeVolumes_maxResults,
    describeVolumesResponse_nextToken,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,

    -- ** DisassociateFileSystemAliases
    disassociateFileSystemAliases_clientRequestToken,
    disassociateFileSystemAliases_fileSystemId,
    disassociateFileSystemAliases_aliases,
    disassociateFileSystemAliasesResponse_aliases,
    disassociateFileSystemAliasesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** ReleaseFileSystemNfsV3Locks
    releaseFileSystemNfsV3Locks_clientRequestToken,
    releaseFileSystemNfsV3Locks_fileSystemId,
    releaseFileSystemNfsV3LocksResponse_fileSystem,
    releaseFileSystemNfsV3LocksResponse_httpStatus,

    -- ** RestoreVolumeFromSnapshot
    restoreVolumeFromSnapshot_clientRequestToken,
    restoreVolumeFromSnapshot_options,
    restoreVolumeFromSnapshot_volumeId,
    restoreVolumeFromSnapshot_snapshotId,
    restoreVolumeFromSnapshotResponse_lifecycle,
    restoreVolumeFromSnapshotResponse_volumeId,
    restoreVolumeFromSnapshotResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDataRepositoryAssociation
    updateDataRepositoryAssociation_clientRequestToken,
    updateDataRepositoryAssociation_s3,
    updateDataRepositoryAssociation_importedFileChunkSize,
    updateDataRepositoryAssociation_associationId,
    updateDataRepositoryAssociationResponse_association,
    updateDataRepositoryAssociationResponse_httpStatus,

    -- ** UpdateFileCache
    updateFileCache_clientRequestToken,
    updateFileCache_lustreConfiguration,
    updateFileCache_fileCacheId,
    updateFileCacheResponse_fileCache,
    updateFileCacheResponse_httpStatus,

    -- ** UpdateFileSystem
    updateFileSystem_clientRequestToken,
    updateFileSystem_openZFSConfiguration,
    updateFileSystem_storageCapacity,
    updateFileSystem_ontapConfiguration,
    updateFileSystem_windowsConfiguration,
    updateFileSystem_lustreConfiguration,
    updateFileSystem_fileSystemId,
    updateFileSystemResponse_fileSystem,
    updateFileSystemResponse_httpStatus,

    -- ** UpdateSnapshot
    updateSnapshot_clientRequestToken,
    updateSnapshot_name,
    updateSnapshot_snapshotId,
    updateSnapshotResponse_snapshot,
    updateSnapshotResponse_httpStatus,

    -- ** UpdateStorageVirtualMachine
    updateStorageVirtualMachine_activeDirectoryConfiguration,
    updateStorageVirtualMachine_clientRequestToken,
    updateStorageVirtualMachine_svmAdminPassword,
    updateStorageVirtualMachine_storageVirtualMachineId,
    updateStorageVirtualMachineResponse_storageVirtualMachine,
    updateStorageVirtualMachineResponse_httpStatus,

    -- ** UpdateVolume
    updateVolume_name,
    updateVolume_clientRequestToken,
    updateVolume_openZFSConfiguration,
    updateVolume_ontapConfiguration,
    updateVolume_volumeId,
    updateVolumeResponse_volume,
    updateVolumeResponse_httpStatus,

    -- * Types

    -- ** ActiveDirectoryBackupAttributes
    activeDirectoryBackupAttributes_activeDirectoryId,
    activeDirectoryBackupAttributes_domainName,
    activeDirectoryBackupAttributes_resourceARN,

    -- ** AdministrativeAction
    administrativeAction_targetFileSystemValues,
    administrativeAction_requestTime,
    administrativeAction_status,
    administrativeAction_targetVolumeValues,
    administrativeAction_failureDetails,
    administrativeAction_targetSnapshotValues,
    administrativeAction_progressPercent,
    administrativeAction_administrativeActionType,

    -- ** AdministrativeActionFailureDetails
    administrativeActionFailureDetails_message,

    -- ** Alias
    alias_name,
    alias_lifecycle,

    -- ** AutoExportPolicy
    autoExportPolicy_events,

    -- ** AutoImportPolicy
    autoImportPolicy_events,

    -- ** Backup
    backup_tags,
    backup_resourceType,
    backup_ownerId,
    backup_sourceBackupRegion,
    backup_failureDetails,
    backup_directoryInformation,
    backup_kmsKeyId,
    backup_volume,
    backup_resourceARN,
    backup_progressPercent,
    backup_sourceBackupId,
    backup_backupId,
    backup_lifecycle,
    backup_type,
    backup_creationTime,
    backup_fileSystem,

    -- ** BackupFailureDetails
    backupFailureDetails_message,

    -- ** CompletionReport
    completionReport_format,
    completionReport_path,
    completionReport_scope,
    completionReport_enabled,

    -- ** CreateFileCacheLustreConfiguration
    createFileCacheLustreConfiguration_weeklyMaintenanceStartTime,
    createFileCacheLustreConfiguration_perUnitStorageThroughput,
    createFileCacheLustreConfiguration_deploymentType,
    createFileCacheLustreConfiguration_metadataConfiguration,

    -- ** CreateFileSystemLustreConfiguration
    createFileSystemLustreConfiguration_copyTagsToBackups,
    createFileSystemLustreConfiguration_driveCacheType,
    createFileSystemLustreConfiguration_weeklyMaintenanceStartTime,
    createFileSystemLustreConfiguration_logConfiguration,
    createFileSystemLustreConfiguration_automaticBackupRetentionDays,
    createFileSystemLustreConfiguration_importedFileChunkSize,
    createFileSystemLustreConfiguration_importPath,
    createFileSystemLustreConfiguration_autoImportPolicy,
    createFileSystemLustreConfiguration_deploymentType,
    createFileSystemLustreConfiguration_exportPath,
    createFileSystemLustreConfiguration_dailyAutomaticBackupStartTime,
    createFileSystemLustreConfiguration_perUnitStorageThroughput,
    createFileSystemLustreConfiguration_dataCompressionType,
    createFileSystemLustreConfiguration_rootSquashConfiguration,

    -- ** CreateFileSystemOntapConfiguration
    createFileSystemOntapConfiguration_weeklyMaintenanceStartTime,
    createFileSystemOntapConfiguration_automaticBackupRetentionDays,
    createFileSystemOntapConfiguration_routeTableIds,
    createFileSystemOntapConfiguration_fsxAdminPassword,
    createFileSystemOntapConfiguration_endpointIpAddressRange,
    createFileSystemOntapConfiguration_diskIopsConfiguration,
    createFileSystemOntapConfiguration_dailyAutomaticBackupStartTime,
    createFileSystemOntapConfiguration_preferredSubnetId,
    createFileSystemOntapConfiguration_deploymentType,
    createFileSystemOntapConfiguration_throughputCapacity,

    -- ** CreateFileSystemOpenZFSConfiguration
    createFileSystemOpenZFSConfiguration_copyTagsToBackups,
    createFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime,
    createFileSystemOpenZFSConfiguration_automaticBackupRetentionDays,
    createFileSystemOpenZFSConfiguration_diskIopsConfiguration,
    createFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime,
    createFileSystemOpenZFSConfiguration_copyTagsToVolumes,
    createFileSystemOpenZFSConfiguration_rootVolumeConfiguration,
    createFileSystemOpenZFSConfiguration_deploymentType,
    createFileSystemOpenZFSConfiguration_throughputCapacity,

    -- ** CreateFileSystemWindowsConfiguration
    createFileSystemWindowsConfiguration_copyTagsToBackups,
    createFileSystemWindowsConfiguration_weeklyMaintenanceStartTime,
    createFileSystemWindowsConfiguration_automaticBackupRetentionDays,
    createFileSystemWindowsConfiguration_activeDirectoryId,
    createFileSystemWindowsConfiguration_aliases,
    createFileSystemWindowsConfiguration_deploymentType,
    createFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime,
    createFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration,
    createFileSystemWindowsConfiguration_auditLogConfiguration,
    createFileSystemWindowsConfiguration_preferredSubnetId,
    createFileSystemWindowsConfiguration_throughputCapacity,

    -- ** CreateOntapVolumeConfiguration
    createOntapVolumeConfiguration_tieringPolicy,
    createOntapVolumeConfiguration_securityStyle,
    createOntapVolumeConfiguration_junctionPath,
    createOntapVolumeConfiguration_sizeInMegabytes,
    createOntapVolumeConfiguration_storageEfficiencyEnabled,
    createOntapVolumeConfiguration_storageVirtualMachineId,

    -- ** CreateOpenZFSOriginSnapshotConfiguration
    createOpenZFSOriginSnapshotConfiguration_snapshotARN,
    createOpenZFSOriginSnapshotConfiguration_copyStrategy,

    -- ** CreateOpenZFSVolumeConfiguration
    createOpenZFSVolumeConfiguration_originSnapshot,
    createOpenZFSVolumeConfiguration_recordSizeKiB,
    createOpenZFSVolumeConfiguration_storageCapacityReservationGiB,
    createOpenZFSVolumeConfiguration_storageCapacityQuotaGiB,
    createOpenZFSVolumeConfiguration_readOnly,
    createOpenZFSVolumeConfiguration_nfsExports,
    createOpenZFSVolumeConfiguration_copyTagsToSnapshots,
    createOpenZFSVolumeConfiguration_dataCompressionType,
    createOpenZFSVolumeConfiguration_userAndGroupQuotas,
    createOpenZFSVolumeConfiguration_parentVolumeId,

    -- ** CreateSvmActiveDirectoryConfiguration
    createSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration,
    createSvmActiveDirectoryConfiguration_netBiosName,

    -- ** DataRepositoryAssociation
    dataRepositoryAssociation_tags,
    dataRepositoryAssociation_dataRepositorySubdirectories,
    dataRepositoryAssociation_lifecycle,
    dataRepositoryAssociation_s3,
    dataRepositoryAssociation_importedFileChunkSize,
    dataRepositoryAssociation_fileSystemPath,
    dataRepositoryAssociation_batchImportMetaDataOnCreate,
    dataRepositoryAssociation_dataRepositoryPath,
    dataRepositoryAssociation_fileCachePath,
    dataRepositoryAssociation_nfs,
    dataRepositoryAssociation_fileSystemId,
    dataRepositoryAssociation_fileCacheId,
    dataRepositoryAssociation_failureDetails,
    dataRepositoryAssociation_creationTime,
    dataRepositoryAssociation_resourceARN,
    dataRepositoryAssociation_associationId,

    -- ** DataRepositoryConfiguration
    dataRepositoryConfiguration_lifecycle,
    dataRepositoryConfiguration_importedFileChunkSize,
    dataRepositoryConfiguration_importPath,
    dataRepositoryConfiguration_autoImportPolicy,
    dataRepositoryConfiguration_exportPath,
    dataRepositoryConfiguration_failureDetails,

    -- ** DataRepositoryFailureDetails
    dataRepositoryFailureDetails_message,

    -- ** DataRepositoryTask
    dataRepositoryTask_tags,
    dataRepositoryTask_capacityToRelease,
    dataRepositoryTask_fileSystemId,
    dataRepositoryTask_status,
    dataRepositoryTask_endTime,
    dataRepositoryTask_fileCacheId,
    dataRepositoryTask_failureDetails,
    dataRepositoryTask_report,
    dataRepositoryTask_paths,
    dataRepositoryTask_resourceARN,
    dataRepositoryTask_startTime,
    dataRepositoryTask_taskId,
    dataRepositoryTask_lifecycle,
    dataRepositoryTask_type,
    dataRepositoryTask_creationTime,

    -- ** DataRepositoryTaskFailureDetails
    dataRepositoryTaskFailureDetails_message,

    -- ** DataRepositoryTaskFilter
    dataRepositoryTaskFilter_name,
    dataRepositoryTaskFilter_values,

    -- ** DataRepositoryTaskStatus
    dataRepositoryTaskStatus_succeededCount,
    dataRepositoryTaskStatus_failedCount,
    dataRepositoryTaskStatus_lastUpdatedTime,
    dataRepositoryTaskStatus_releasedCapacity,
    dataRepositoryTaskStatus_totalCount,

    -- ** DeleteFileSystemLustreConfiguration
    deleteFileSystemLustreConfiguration_skipFinalBackup,
    deleteFileSystemLustreConfiguration_finalBackupTags,

    -- ** DeleteFileSystemLustreResponse
    deleteFileSystemLustreResponse_finalBackupTags,
    deleteFileSystemLustreResponse_finalBackupId,

    -- ** DeleteFileSystemOpenZFSConfiguration
    deleteFileSystemOpenZFSConfiguration_skipFinalBackup,
    deleteFileSystemOpenZFSConfiguration_options,
    deleteFileSystemOpenZFSConfiguration_finalBackupTags,

    -- ** DeleteFileSystemOpenZFSResponse
    deleteFileSystemOpenZFSResponse_finalBackupTags,
    deleteFileSystemOpenZFSResponse_finalBackupId,

    -- ** DeleteFileSystemWindowsConfiguration
    deleteFileSystemWindowsConfiguration_skipFinalBackup,
    deleteFileSystemWindowsConfiguration_finalBackupTags,

    -- ** DeleteFileSystemWindowsResponse
    deleteFileSystemWindowsResponse_finalBackupTags,
    deleteFileSystemWindowsResponse_finalBackupId,

    -- ** DeleteVolumeOntapConfiguration
    deleteVolumeOntapConfiguration_skipFinalBackup,
    deleteVolumeOntapConfiguration_finalBackupTags,

    -- ** DeleteVolumeOntapResponse
    deleteVolumeOntapResponse_finalBackupTags,
    deleteVolumeOntapResponse_finalBackupId,

    -- ** DeleteVolumeOpenZFSConfiguration
    deleteVolumeOpenZFSConfiguration_options,

    -- ** DiskIopsConfiguration
    diskIopsConfiguration_mode,
    diskIopsConfiguration_iops,

    -- ** FileCache
    fileCache_ownerId,
    fileCache_lifecycle,
    fileCache_dataRepositoryAssociationIds,
    fileCache_storageCapacity,
    fileCache_fileCacheId,
    fileCache_failureDetails,
    fileCache_fileCacheTypeVersion,
    fileCache_fileCacheType,
    fileCache_kmsKeyId,
    fileCache_creationTime,
    fileCache_resourceARN,
    fileCache_dNSName,
    fileCache_vpcId,
    fileCache_networkInterfaceIds,
    fileCache_subnetIds,
    fileCache_lustreConfiguration,

    -- ** FileCacheCreating
    fileCacheCreating_tags,
    fileCacheCreating_ownerId,
    fileCacheCreating_lifecycle,
    fileCacheCreating_copyTagsToDataRepositoryAssociations,
    fileCacheCreating_dataRepositoryAssociationIds,
    fileCacheCreating_storageCapacity,
    fileCacheCreating_fileCacheId,
    fileCacheCreating_failureDetails,
    fileCacheCreating_fileCacheTypeVersion,
    fileCacheCreating_fileCacheType,
    fileCacheCreating_kmsKeyId,
    fileCacheCreating_creationTime,
    fileCacheCreating_resourceARN,
    fileCacheCreating_dNSName,
    fileCacheCreating_vpcId,
    fileCacheCreating_networkInterfaceIds,
    fileCacheCreating_subnetIds,
    fileCacheCreating_lustreConfiguration,

    -- ** FileCacheDataRepositoryAssociation
    fileCacheDataRepositoryAssociation_dataRepositorySubdirectories,
    fileCacheDataRepositoryAssociation_nfs,
    fileCacheDataRepositoryAssociation_fileCachePath,
    fileCacheDataRepositoryAssociation_dataRepositoryPath,

    -- ** FileCacheFailureDetails
    fileCacheFailureDetails_message,

    -- ** FileCacheLustreConfiguration
    fileCacheLustreConfiguration_metadataConfiguration,
    fileCacheLustreConfiguration_weeklyMaintenanceStartTime,
    fileCacheLustreConfiguration_logConfiguration,
    fileCacheLustreConfiguration_deploymentType,
    fileCacheLustreConfiguration_perUnitStorageThroughput,
    fileCacheLustreConfiguration_mountName,

    -- ** FileCacheLustreMetadataConfiguration
    fileCacheLustreMetadataConfiguration_storageCapacity,

    -- ** FileCacheNFSConfiguration
    fileCacheNFSConfiguration_dnsIps,
    fileCacheNFSConfiguration_version,

    -- ** FileSystem
    fileSystem_tags,
    fileSystem_ownerId,
    fileSystem_lifecycle,
    fileSystem_fileSystemTypeVersion,
    fileSystem_administrativeActions,
    fileSystem_openZFSConfiguration,
    fileSystem_fileSystemId,
    fileSystem_storageCapacity,
    fileSystem_storageType,
    fileSystem_ontapConfiguration,
    fileSystem_windowsConfiguration,
    fileSystem_failureDetails,
    fileSystem_kmsKeyId,
    fileSystem_creationTime,
    fileSystem_fileSystemType,
    fileSystem_resourceARN,
    fileSystem_dNSName,
    fileSystem_vpcId,
    fileSystem_networkInterfaceIds,
    fileSystem_subnetIds,
    fileSystem_lustreConfiguration,

    -- ** FileSystemEndpoint
    fileSystemEndpoint_dNSName,
    fileSystemEndpoint_ipAddresses,

    -- ** FileSystemEndpoints
    fileSystemEndpoints_intercluster,
    fileSystemEndpoints_management,

    -- ** FileSystemFailureDetails
    fileSystemFailureDetails_message,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** LifecycleTransitionReason
    lifecycleTransitionReason_message,

    -- ** LustreFileSystemConfiguration
    lustreFileSystemConfiguration_copyTagsToBackups,
    lustreFileSystemConfiguration_driveCacheType,
    lustreFileSystemConfiguration_weeklyMaintenanceStartTime,
    lustreFileSystemConfiguration_logConfiguration,
    lustreFileSystemConfiguration_automaticBackupRetentionDays,
    lustreFileSystemConfiguration_deploymentType,
    lustreFileSystemConfiguration_dailyAutomaticBackupStartTime,
    lustreFileSystemConfiguration_perUnitStorageThroughput,
    lustreFileSystemConfiguration_dataCompressionType,
    lustreFileSystemConfiguration_mountName,
    lustreFileSystemConfiguration_rootSquashConfiguration,
    lustreFileSystemConfiguration_dataRepositoryConfiguration,

    -- ** LustreLogConfiguration
    lustreLogConfiguration_destination,
    lustreLogConfiguration_level,

    -- ** LustreLogCreateConfiguration
    lustreLogCreateConfiguration_destination,
    lustreLogCreateConfiguration_level,

    -- ** LustreRootSquashConfiguration
    lustreRootSquashConfiguration_rootSquash,
    lustreRootSquashConfiguration_noSquashNids,

    -- ** NFSDataRepositoryConfiguration
    nFSDataRepositoryConfiguration_autoExportPolicy,
    nFSDataRepositoryConfiguration_dnsIps,
    nFSDataRepositoryConfiguration_version,

    -- ** OntapFileSystemConfiguration
    ontapFileSystemConfiguration_weeklyMaintenanceStartTime,
    ontapFileSystemConfiguration_throughputCapacity,
    ontapFileSystemConfiguration_automaticBackupRetentionDays,
    ontapFileSystemConfiguration_routeTableIds,
    ontapFileSystemConfiguration_deploymentType,
    ontapFileSystemConfiguration_endpoints,
    ontapFileSystemConfiguration_endpointIpAddressRange,
    ontapFileSystemConfiguration_diskIopsConfiguration,
    ontapFileSystemConfiguration_dailyAutomaticBackupStartTime,
    ontapFileSystemConfiguration_preferredSubnetId,

    -- ** OntapVolumeConfiguration
    ontapVolumeConfiguration_storageEfficiencyEnabled,
    ontapVolumeConfiguration_tieringPolicy,
    ontapVolumeConfiguration_securityStyle,
    ontapVolumeConfiguration_storageVirtualMachineId,
    ontapVolumeConfiguration_storageVirtualMachineRoot,
    ontapVolumeConfiguration_uuid,
    ontapVolumeConfiguration_junctionPath,
    ontapVolumeConfiguration_flexCacheEndpointType,
    ontapVolumeConfiguration_ontapVolumeType,
    ontapVolumeConfiguration_sizeInMegabytes,

    -- ** OpenZFSClientConfiguration
    openZFSClientConfiguration_clients,
    openZFSClientConfiguration_options,

    -- ** OpenZFSCreateRootVolumeConfiguration
    openZFSCreateRootVolumeConfiguration_recordSizeKiB,
    openZFSCreateRootVolumeConfiguration_readOnly,
    openZFSCreateRootVolumeConfiguration_nfsExports,
    openZFSCreateRootVolumeConfiguration_copyTagsToSnapshots,
    openZFSCreateRootVolumeConfiguration_dataCompressionType,
    openZFSCreateRootVolumeConfiguration_userAndGroupQuotas,

    -- ** OpenZFSFileSystemConfiguration
    openZFSFileSystemConfiguration_copyTagsToBackups,
    openZFSFileSystemConfiguration_weeklyMaintenanceStartTime,
    openZFSFileSystemConfiguration_throughputCapacity,
    openZFSFileSystemConfiguration_automaticBackupRetentionDays,
    openZFSFileSystemConfiguration_deploymentType,
    openZFSFileSystemConfiguration_rootVolumeId,
    openZFSFileSystemConfiguration_diskIopsConfiguration,
    openZFSFileSystemConfiguration_dailyAutomaticBackupStartTime,
    openZFSFileSystemConfiguration_copyTagsToVolumes,

    -- ** OpenZFSNfsExport
    openZFSNfsExport_clientConfigurations,

    -- ** OpenZFSOriginSnapshotConfiguration
    openZFSOriginSnapshotConfiguration_copyStrategy,
    openZFSOriginSnapshotConfiguration_snapshotARN,

    -- ** OpenZFSUserOrGroupQuota
    openZFSUserOrGroupQuota_type,
    openZFSUserOrGroupQuota_id,
    openZFSUserOrGroupQuota_storageCapacityQuotaGiB,

    -- ** OpenZFSVolumeConfiguration
    openZFSVolumeConfiguration_originSnapshot,
    openZFSVolumeConfiguration_recordSizeKiB,
    openZFSVolumeConfiguration_storageCapacityReservationGiB,
    openZFSVolumeConfiguration_parentVolumeId,
    openZFSVolumeConfiguration_storageCapacityQuotaGiB,
    openZFSVolumeConfiguration_readOnly,
    openZFSVolumeConfiguration_nfsExports,
    openZFSVolumeConfiguration_copyTagsToSnapshots,
    openZFSVolumeConfiguration_dataCompressionType,
    openZFSVolumeConfiguration_userAndGroupQuotas,
    openZFSVolumeConfiguration_volumePath,

    -- ** S3DataRepositoryConfiguration
    s3DataRepositoryConfiguration_autoExportPolicy,
    s3DataRepositoryConfiguration_autoImportPolicy,

    -- ** SelfManagedActiveDirectoryAttributes
    selfManagedActiveDirectoryAttributes_domainName,
    selfManagedActiveDirectoryAttributes_dnsIps,
    selfManagedActiveDirectoryAttributes_userName,
    selfManagedActiveDirectoryAttributes_organizationalUnitDistinguishedName,
    selfManagedActiveDirectoryAttributes_fileSystemAdministratorsGroup,

    -- ** SelfManagedActiveDirectoryConfiguration
    selfManagedActiveDirectoryConfiguration_organizationalUnitDistinguishedName,
    selfManagedActiveDirectoryConfiguration_fileSystemAdministratorsGroup,
    selfManagedActiveDirectoryConfiguration_domainName,
    selfManagedActiveDirectoryConfiguration_userName,
    selfManagedActiveDirectoryConfiguration_password,
    selfManagedActiveDirectoryConfiguration_dnsIps,

    -- ** SelfManagedActiveDirectoryConfigurationUpdates
    selfManagedActiveDirectoryConfigurationUpdates_password,
    selfManagedActiveDirectoryConfigurationUpdates_dnsIps,
    selfManagedActiveDirectoryConfigurationUpdates_userName,

    -- ** Snapshot
    snapshot_tags,
    snapshot_name,
    snapshot_lifecycle,
    snapshot_administrativeActions,
    snapshot_snapshotId,
    snapshot_creationTime,
    snapshot_volumeId,
    snapshot_resourceARN,
    snapshot_lifecycleTransitionReason,

    -- ** SnapshotFilter
    snapshotFilter_name,
    snapshotFilter_values,

    -- ** StorageVirtualMachine
    storageVirtualMachine_tags,
    storageVirtualMachine_name,
    storageVirtualMachine_activeDirectoryConfiguration,
    storageVirtualMachine_lifecycle,
    storageVirtualMachine_storageVirtualMachineId,
    storageVirtualMachine_uuid,
    storageVirtualMachine_fileSystemId,
    storageVirtualMachine_endpoints,
    storageVirtualMachine_subtype,
    storageVirtualMachine_rootVolumeSecurityStyle,
    storageVirtualMachine_creationTime,
    storageVirtualMachine_resourceARN,
    storageVirtualMachine_lifecycleTransitionReason,

    -- ** StorageVirtualMachineFilter
    storageVirtualMachineFilter_name,
    storageVirtualMachineFilter_values,

    -- ** SvmActiveDirectoryConfiguration
    svmActiveDirectoryConfiguration_netBiosName,
    svmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration,

    -- ** SvmEndpoint
    svmEndpoint_dNSName,
    svmEndpoint_ipAddresses,

    -- ** SvmEndpoints
    svmEndpoints_smb,
    svmEndpoints_iscsi,
    svmEndpoints_nfs,
    svmEndpoints_management,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TieringPolicy
    tieringPolicy_name,
    tieringPolicy_coolingPeriod,

    -- ** UpdateFileCacheLustreConfiguration
    updateFileCacheLustreConfiguration_weeklyMaintenanceStartTime,

    -- ** UpdateFileSystemLustreConfiguration
    updateFileSystemLustreConfiguration_weeklyMaintenanceStartTime,
    updateFileSystemLustreConfiguration_logConfiguration,
    updateFileSystemLustreConfiguration_automaticBackupRetentionDays,
    updateFileSystemLustreConfiguration_autoImportPolicy,
    updateFileSystemLustreConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemLustreConfiguration_dataCompressionType,
    updateFileSystemLustreConfiguration_rootSquashConfiguration,

    -- ** UpdateFileSystemOntapConfiguration
    updateFileSystemOntapConfiguration_weeklyMaintenanceStartTime,
    updateFileSystemOntapConfiguration_throughputCapacity,
    updateFileSystemOntapConfiguration_automaticBackupRetentionDays,
    updateFileSystemOntapConfiguration_fsxAdminPassword,
    updateFileSystemOntapConfiguration_diskIopsConfiguration,
    updateFileSystemOntapConfiguration_dailyAutomaticBackupStartTime,

    -- ** UpdateFileSystemOpenZFSConfiguration
    updateFileSystemOpenZFSConfiguration_copyTagsToBackups,
    updateFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime,
    updateFileSystemOpenZFSConfiguration_throughputCapacity,
    updateFileSystemOpenZFSConfiguration_automaticBackupRetentionDays,
    updateFileSystemOpenZFSConfiguration_diskIopsConfiguration,
    updateFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemOpenZFSConfiguration_copyTagsToVolumes,

    -- ** UpdateFileSystemWindowsConfiguration
    updateFileSystemWindowsConfiguration_weeklyMaintenanceStartTime,
    updateFileSystemWindowsConfiguration_throughputCapacity,
    updateFileSystemWindowsConfiguration_automaticBackupRetentionDays,
    updateFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration,
    updateFileSystemWindowsConfiguration_auditLogConfiguration,

    -- ** UpdateOntapVolumeConfiguration
    updateOntapVolumeConfiguration_storageEfficiencyEnabled,
    updateOntapVolumeConfiguration_tieringPolicy,
    updateOntapVolumeConfiguration_securityStyle,
    updateOntapVolumeConfiguration_junctionPath,
    updateOntapVolumeConfiguration_sizeInMegabytes,

    -- ** UpdateOpenZFSVolumeConfiguration
    updateOpenZFSVolumeConfiguration_recordSizeKiB,
    updateOpenZFSVolumeConfiguration_storageCapacityReservationGiB,
    updateOpenZFSVolumeConfiguration_storageCapacityQuotaGiB,
    updateOpenZFSVolumeConfiguration_readOnly,
    updateOpenZFSVolumeConfiguration_nfsExports,
    updateOpenZFSVolumeConfiguration_dataCompressionType,
    updateOpenZFSVolumeConfiguration_userAndGroupQuotas,

    -- ** UpdateSvmActiveDirectoryConfiguration
    updateSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration,

    -- ** Volume
    volume_tags,
    volume_name,
    volume_lifecycle,
    volume_administrativeActions,
    volume_openZFSConfiguration,
    volume_volumeType,
    volume_fileSystemId,
    volume_ontapConfiguration,
    volume_creationTime,
    volume_volumeId,
    volume_resourceARN,
    volume_lifecycleTransitionReason,

    -- ** VolumeFilter
    volumeFilter_name,
    volumeFilter_values,

    -- ** WindowsAuditLogConfiguration
    windowsAuditLogConfiguration_auditLogDestination,
    windowsAuditLogConfiguration_fileAccessAuditLogLevel,
    windowsAuditLogConfiguration_fileShareAccessAuditLogLevel,

    -- ** WindowsAuditLogCreateConfiguration
    windowsAuditLogCreateConfiguration_auditLogDestination,
    windowsAuditLogCreateConfiguration_fileAccessAuditLogLevel,
    windowsAuditLogCreateConfiguration_fileShareAccessAuditLogLevel,

    -- ** WindowsFileSystemConfiguration
    windowsFileSystemConfiguration_copyTagsToBackups,
    windowsFileSystemConfiguration_weeklyMaintenanceStartTime,
    windowsFileSystemConfiguration_throughputCapacity,
    windowsFileSystemConfiguration_automaticBackupRetentionDays,
    windowsFileSystemConfiguration_activeDirectoryId,
    windowsFileSystemConfiguration_aliases,
    windowsFileSystemConfiguration_maintenanceOperationsInProgress,
    windowsFileSystemConfiguration_deploymentType,
    windowsFileSystemConfiguration_remoteAdministrationEndpoint,
    windowsFileSystemConfiguration_preferredFileServerIp,
    windowsFileSystemConfiguration_dailyAutomaticBackupStartTime,
    windowsFileSystemConfiguration_selfManagedActiveDirectoryConfiguration,
    windowsFileSystemConfiguration_auditLogConfiguration,
    windowsFileSystemConfiguration_preferredSubnetId,
  )
where

import Amazonka.FSx.AssociateFileSystemAliases
import Amazonka.FSx.CancelDataRepositoryTask
import Amazonka.FSx.CopyBackup
import Amazonka.FSx.CreateBackup
import Amazonka.FSx.CreateDataRepositoryAssociation
import Amazonka.FSx.CreateDataRepositoryTask
import Amazonka.FSx.CreateFileCache
import Amazonka.FSx.CreateFileSystem
import Amazonka.FSx.CreateFileSystemFromBackup
import Amazonka.FSx.CreateSnapshot
import Amazonka.FSx.CreateStorageVirtualMachine
import Amazonka.FSx.CreateVolume
import Amazonka.FSx.CreateVolumeFromBackup
import Amazonka.FSx.DeleteBackup
import Amazonka.FSx.DeleteDataRepositoryAssociation
import Amazonka.FSx.DeleteFileCache
import Amazonka.FSx.DeleteFileSystem
import Amazonka.FSx.DeleteSnapshot
import Amazonka.FSx.DeleteStorageVirtualMachine
import Amazonka.FSx.DeleteVolume
import Amazonka.FSx.DescribeBackups
import Amazonka.FSx.DescribeDataRepositoryAssociations
import Amazonka.FSx.DescribeDataRepositoryTasks
import Amazonka.FSx.DescribeFileCaches
import Amazonka.FSx.DescribeFileSystemAliases
import Amazonka.FSx.DescribeFileSystems
import Amazonka.FSx.DescribeSnapshots
import Amazonka.FSx.DescribeStorageVirtualMachines
import Amazonka.FSx.DescribeVolumes
import Amazonka.FSx.DisassociateFileSystemAliases
import Amazonka.FSx.ListTagsForResource
import Amazonka.FSx.ReleaseFileSystemNfsV3Locks
import Amazonka.FSx.RestoreVolumeFromSnapshot
import Amazonka.FSx.TagResource
import Amazonka.FSx.Types.ActiveDirectoryBackupAttributes
import Amazonka.FSx.Types.AdministrativeAction
import Amazonka.FSx.Types.AdministrativeActionFailureDetails
import Amazonka.FSx.Types.Alias
import Amazonka.FSx.Types.AutoExportPolicy
import Amazonka.FSx.Types.AutoImportPolicy
import Amazonka.FSx.Types.Backup
import Amazonka.FSx.Types.BackupFailureDetails
import Amazonka.FSx.Types.CompletionReport
import Amazonka.FSx.Types.CreateFileCacheLustreConfiguration
import Amazonka.FSx.Types.CreateFileSystemLustreConfiguration
import Amazonka.FSx.Types.CreateFileSystemOntapConfiguration
import Amazonka.FSx.Types.CreateFileSystemOpenZFSConfiguration
import Amazonka.FSx.Types.CreateFileSystemWindowsConfiguration
import Amazonka.FSx.Types.CreateOntapVolumeConfiguration
import Amazonka.FSx.Types.CreateOpenZFSOriginSnapshotConfiguration
import Amazonka.FSx.Types.CreateOpenZFSVolumeConfiguration
import Amazonka.FSx.Types.CreateSvmActiveDirectoryConfiguration
import Amazonka.FSx.Types.DataRepositoryAssociation
import Amazonka.FSx.Types.DataRepositoryConfiguration
import Amazonka.FSx.Types.DataRepositoryFailureDetails
import Amazonka.FSx.Types.DataRepositoryTask
import Amazonka.FSx.Types.DataRepositoryTaskFailureDetails
import Amazonka.FSx.Types.DataRepositoryTaskFilter
import Amazonka.FSx.Types.DataRepositoryTaskStatus
import Amazonka.FSx.Types.DeleteFileSystemLustreConfiguration
import Amazonka.FSx.Types.DeleteFileSystemLustreResponse
import Amazonka.FSx.Types.DeleteFileSystemOpenZFSConfiguration
import Amazonka.FSx.Types.DeleteFileSystemOpenZFSResponse
import Amazonka.FSx.Types.DeleteFileSystemWindowsConfiguration
import Amazonka.FSx.Types.DeleteFileSystemWindowsResponse
import Amazonka.FSx.Types.DeleteVolumeOntapConfiguration
import Amazonka.FSx.Types.DeleteVolumeOntapResponse
import Amazonka.FSx.Types.DeleteVolumeOpenZFSConfiguration
import Amazonka.FSx.Types.DiskIopsConfiguration
import Amazonka.FSx.Types.FileCache
import Amazonka.FSx.Types.FileCacheCreating
import Amazonka.FSx.Types.FileCacheDataRepositoryAssociation
import Amazonka.FSx.Types.FileCacheFailureDetails
import Amazonka.FSx.Types.FileCacheLustreConfiguration
import Amazonka.FSx.Types.FileCacheLustreMetadataConfiguration
import Amazonka.FSx.Types.FileCacheNFSConfiguration
import Amazonka.FSx.Types.FileSystem
import Amazonka.FSx.Types.FileSystemEndpoint
import Amazonka.FSx.Types.FileSystemEndpoints
import Amazonka.FSx.Types.FileSystemFailureDetails
import Amazonka.FSx.Types.Filter
import Amazonka.FSx.Types.LifecycleTransitionReason
import Amazonka.FSx.Types.LustreFileSystemConfiguration
import Amazonka.FSx.Types.LustreLogConfiguration
import Amazonka.FSx.Types.LustreLogCreateConfiguration
import Amazonka.FSx.Types.LustreRootSquashConfiguration
import Amazonka.FSx.Types.NFSDataRepositoryConfiguration
import Amazonka.FSx.Types.OntapFileSystemConfiguration
import Amazonka.FSx.Types.OntapVolumeConfiguration
import Amazonka.FSx.Types.OpenZFSClientConfiguration
import Amazonka.FSx.Types.OpenZFSCreateRootVolumeConfiguration
import Amazonka.FSx.Types.OpenZFSFileSystemConfiguration
import Amazonka.FSx.Types.OpenZFSNfsExport
import Amazonka.FSx.Types.OpenZFSOriginSnapshotConfiguration
import Amazonka.FSx.Types.OpenZFSUserOrGroupQuota
import Amazonka.FSx.Types.OpenZFSVolumeConfiguration
import Amazonka.FSx.Types.S3DataRepositoryConfiguration
import Amazonka.FSx.Types.SelfManagedActiveDirectoryAttributes
import Amazonka.FSx.Types.SelfManagedActiveDirectoryConfiguration
import Amazonka.FSx.Types.SelfManagedActiveDirectoryConfigurationUpdates
import Amazonka.FSx.Types.Snapshot
import Amazonka.FSx.Types.SnapshotFilter
import Amazonka.FSx.Types.StorageVirtualMachine
import Amazonka.FSx.Types.StorageVirtualMachineFilter
import Amazonka.FSx.Types.SvmActiveDirectoryConfiguration
import Amazonka.FSx.Types.SvmEndpoint
import Amazonka.FSx.Types.SvmEndpoints
import Amazonka.FSx.Types.Tag
import Amazonka.FSx.Types.TieringPolicy
import Amazonka.FSx.Types.UpdateFileCacheLustreConfiguration
import Amazonka.FSx.Types.UpdateFileSystemLustreConfiguration
import Amazonka.FSx.Types.UpdateFileSystemOntapConfiguration
import Amazonka.FSx.Types.UpdateFileSystemOpenZFSConfiguration
import Amazonka.FSx.Types.UpdateFileSystemWindowsConfiguration
import Amazonka.FSx.Types.UpdateOntapVolumeConfiguration
import Amazonka.FSx.Types.UpdateOpenZFSVolumeConfiguration
import Amazonka.FSx.Types.UpdateSvmActiveDirectoryConfiguration
import Amazonka.FSx.Types.Volume
import Amazonka.FSx.Types.VolumeFilter
import Amazonka.FSx.Types.WindowsAuditLogConfiguration
import Amazonka.FSx.Types.WindowsAuditLogCreateConfiguration
import Amazonka.FSx.Types.WindowsFileSystemConfiguration
import Amazonka.FSx.UntagResource
import Amazonka.FSx.UpdateDataRepositoryAssociation
import Amazonka.FSx.UpdateFileCache
import Amazonka.FSx.UpdateFileSystem
import Amazonka.FSx.UpdateSnapshot
import Amazonka.FSx.UpdateStorageVirtualMachine
import Amazonka.FSx.UpdateVolume

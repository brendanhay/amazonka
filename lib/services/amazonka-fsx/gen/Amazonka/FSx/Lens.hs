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
    copyBackup_clientRequestToken,
    copyBackup_copyTags,
    copyBackup_kmsKeyId,
    copyBackup_sourceRegion,
    copyBackup_tags,
    copyBackup_sourceBackupId,
    copyBackupResponse_backup,
    copyBackupResponse_httpStatus,

    -- ** CreateBackup
    createBackup_clientRequestToken,
    createBackup_fileSystemId,
    createBackup_tags,
    createBackup_volumeId,
    createBackupResponse_backup,
    createBackupResponse_httpStatus,

    -- ** CreateDataRepositoryAssociation
    createDataRepositoryAssociation_batchImportMetaDataOnCreate,
    createDataRepositoryAssociation_clientRequestToken,
    createDataRepositoryAssociation_fileSystemPath,
    createDataRepositoryAssociation_importedFileChunkSize,
    createDataRepositoryAssociation_s3,
    createDataRepositoryAssociation_tags,
    createDataRepositoryAssociation_fileSystemId,
    createDataRepositoryAssociation_dataRepositoryPath,
    createDataRepositoryAssociationResponse_association,
    createDataRepositoryAssociationResponse_httpStatus,

    -- ** CreateDataRepositoryTask
    createDataRepositoryTask_capacityToRelease,
    createDataRepositoryTask_clientRequestToken,
    createDataRepositoryTask_paths,
    createDataRepositoryTask_tags,
    createDataRepositoryTask_type,
    createDataRepositoryTask_fileSystemId,
    createDataRepositoryTask_report,
    createDataRepositoryTaskResponse_dataRepositoryTask,
    createDataRepositoryTaskResponse_httpStatus,

    -- ** CreateFileCache
    createFileCache_clientRequestToken,
    createFileCache_copyTagsToDataRepositoryAssociations,
    createFileCache_dataRepositoryAssociations,
    createFileCache_kmsKeyId,
    createFileCache_lustreConfiguration,
    createFileCache_securityGroupIds,
    createFileCache_tags,
    createFileCache_fileCacheType,
    createFileCache_fileCacheTypeVersion,
    createFileCache_storageCapacity,
    createFileCache_subnetIds,
    createFileCacheResponse_fileCache,
    createFileCacheResponse_httpStatus,

    -- ** CreateFileSystem
    createFileSystem_clientRequestToken,
    createFileSystem_fileSystemTypeVersion,
    createFileSystem_kmsKeyId,
    createFileSystem_lustreConfiguration,
    createFileSystem_ontapConfiguration,
    createFileSystem_openZFSConfiguration,
    createFileSystem_securityGroupIds,
    createFileSystem_storageType,
    createFileSystem_tags,
    createFileSystem_windowsConfiguration,
    createFileSystem_fileSystemType,
    createFileSystem_storageCapacity,
    createFileSystem_subnetIds,
    createFileSystemResponse_fileSystem,
    createFileSystemResponse_httpStatus,

    -- ** CreateFileSystemFromBackup
    createFileSystemFromBackup_clientRequestToken,
    createFileSystemFromBackup_fileSystemTypeVersion,
    createFileSystemFromBackup_kmsKeyId,
    createFileSystemFromBackup_lustreConfiguration,
    createFileSystemFromBackup_openZFSConfiguration,
    createFileSystemFromBackup_securityGroupIds,
    createFileSystemFromBackup_storageCapacity,
    createFileSystemFromBackup_storageType,
    createFileSystemFromBackup_tags,
    createFileSystemFromBackup_windowsConfiguration,
    createFileSystemFromBackup_backupId,
    createFileSystemFromBackup_subnetIds,
    createFileSystemFromBackupResponse_fileSystem,
    createFileSystemFromBackupResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_clientRequestToken,
    createSnapshot_tags,
    createSnapshot_name,
    createSnapshot_volumeId,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** CreateStorageVirtualMachine
    createStorageVirtualMachine_activeDirectoryConfiguration,
    createStorageVirtualMachine_clientRequestToken,
    createStorageVirtualMachine_rootVolumeSecurityStyle,
    createStorageVirtualMachine_svmAdminPassword,
    createStorageVirtualMachine_tags,
    createStorageVirtualMachine_fileSystemId,
    createStorageVirtualMachine_name,
    createStorageVirtualMachineResponse_storageVirtualMachine,
    createStorageVirtualMachineResponse_httpStatus,

    -- ** CreateVolume
    createVolume_clientRequestToken,
    createVolume_ontapConfiguration,
    createVolume_openZFSConfiguration,
    createVolume_tags,
    createVolume_volumeType,
    createVolume_name,
    createVolumeResponse_volume,
    createVolumeResponse_httpStatus,

    -- ** CreateVolumeFromBackup
    createVolumeFromBackup_clientRequestToken,
    createVolumeFromBackup_ontapConfiguration,
    createVolumeFromBackup_tags,
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
    deleteDataRepositoryAssociationResponse_associationId,
    deleteDataRepositoryAssociationResponse_deleteDataInFileSystem,
    deleteDataRepositoryAssociationResponse_lifecycle,
    deleteDataRepositoryAssociationResponse_httpStatus,

    -- ** DeleteFileCache
    deleteFileCache_clientRequestToken,
    deleteFileCache_fileCacheId,
    deleteFileCacheResponse_fileCacheId,
    deleteFileCacheResponse_lifecycle,
    deleteFileCacheResponse_httpStatus,

    -- ** DeleteFileSystem
    deleteFileSystem_clientRequestToken,
    deleteFileSystem_lustreConfiguration,
    deleteFileSystem_openZFSConfiguration,
    deleteFileSystem_windowsConfiguration,
    deleteFileSystem_fileSystemId,
    deleteFileSystemResponse_fileSystemId,
    deleteFileSystemResponse_lifecycle,
    deleteFileSystemResponse_lustreResponse,
    deleteFileSystemResponse_openZFSResponse,
    deleteFileSystemResponse_windowsResponse,
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
    deleteVolume_ontapConfiguration,
    deleteVolume_openZFSConfiguration,
    deleteVolume_volumeId,
    deleteVolumeResponse_lifecycle,
    deleteVolumeResponse_ontapResponse,
    deleteVolumeResponse_volumeId,
    deleteVolumeResponse_httpStatus,

    -- ** DescribeBackups
    describeBackups_backupIds,
    describeBackups_filters,
    describeBackups_maxResults,
    describeBackups_nextToken,
    describeBackupsResponse_backups,
    describeBackupsResponse_nextToken,
    describeBackupsResponse_httpStatus,

    -- ** DescribeDataRepositoryAssociations
    describeDataRepositoryAssociations_associationIds,
    describeDataRepositoryAssociations_filters,
    describeDataRepositoryAssociations_maxResults,
    describeDataRepositoryAssociations_nextToken,
    describeDataRepositoryAssociationsResponse_associations,
    describeDataRepositoryAssociationsResponse_nextToken,
    describeDataRepositoryAssociationsResponse_httpStatus,

    -- ** DescribeDataRepositoryTasks
    describeDataRepositoryTasks_filters,
    describeDataRepositoryTasks_maxResults,
    describeDataRepositoryTasks_nextToken,
    describeDataRepositoryTasks_taskIds,
    describeDataRepositoryTasksResponse_dataRepositoryTasks,
    describeDataRepositoryTasksResponse_nextToken,
    describeDataRepositoryTasksResponse_httpStatus,

    -- ** DescribeFileCaches
    describeFileCaches_fileCacheIds,
    describeFileCaches_maxResults,
    describeFileCaches_nextToken,
    describeFileCachesResponse_fileCaches,
    describeFileCachesResponse_nextToken,
    describeFileCachesResponse_httpStatus,

    -- ** DescribeFileSystemAliases
    describeFileSystemAliases_clientRequestToken,
    describeFileSystemAliases_maxResults,
    describeFileSystemAliases_nextToken,
    describeFileSystemAliases_fileSystemId,
    describeFileSystemAliasesResponse_aliases,
    describeFileSystemAliasesResponse_nextToken,
    describeFileSystemAliasesResponse_httpStatus,

    -- ** DescribeFileSystems
    describeFileSystems_fileSystemIds,
    describeFileSystems_maxResults,
    describeFileSystems_nextToken,
    describeFileSystemsResponse_fileSystems,
    describeFileSystemsResponse_nextToken,
    describeFileSystemsResponse_httpStatus,

    -- ** DescribeSnapshots
    describeSnapshots_filters,
    describeSnapshots_maxResults,
    describeSnapshots_nextToken,
    describeSnapshots_snapshotIds,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,

    -- ** DescribeStorageVirtualMachines
    describeStorageVirtualMachines_filters,
    describeStorageVirtualMachines_maxResults,
    describeStorageVirtualMachines_nextToken,
    describeStorageVirtualMachines_storageVirtualMachineIds,
    describeStorageVirtualMachinesResponse_nextToken,
    describeStorageVirtualMachinesResponse_storageVirtualMachines,
    describeStorageVirtualMachinesResponse_httpStatus,

    -- ** DescribeVolumes
    describeVolumes_filters,
    describeVolumes_maxResults,
    describeVolumes_nextToken,
    describeVolumes_volumeIds,
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
    listTagsForResource_maxResults,
    listTagsForResource_nextToken,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
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
    restoreVolumeFromSnapshotResponse_administrativeActions,
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
    updateDataRepositoryAssociation_importedFileChunkSize,
    updateDataRepositoryAssociation_s3,
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
    updateFileSystem_lustreConfiguration,
    updateFileSystem_ontapConfiguration,
    updateFileSystem_openZFSConfiguration,
    updateFileSystem_storageCapacity,
    updateFileSystem_windowsConfiguration,
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
    updateVolume_clientRequestToken,
    updateVolume_name,
    updateVolume_ontapConfiguration,
    updateVolume_openZFSConfiguration,
    updateVolume_volumeId,
    updateVolumeResponse_volume,
    updateVolumeResponse_httpStatus,

    -- * Types

    -- ** ActiveDirectoryBackupAttributes
    activeDirectoryBackupAttributes_activeDirectoryId,
    activeDirectoryBackupAttributes_domainName,
    activeDirectoryBackupAttributes_resourceARN,

    -- ** AdministrativeAction
    administrativeAction_administrativeActionType,
    administrativeAction_failureDetails,
    administrativeAction_progressPercent,
    administrativeAction_requestTime,
    administrativeAction_status,
    administrativeAction_targetFileSystemValues,
    administrativeAction_targetSnapshotValues,
    administrativeAction_targetVolumeValues,

    -- ** AdministrativeActionFailureDetails
    administrativeActionFailureDetails_message,

    -- ** Alias
    alias_lifecycle,
    alias_name,

    -- ** AutoExportPolicy
    autoExportPolicy_events,

    -- ** AutoImportPolicy
    autoImportPolicy_events,

    -- ** Backup
    backup_directoryInformation,
    backup_failureDetails,
    backup_kmsKeyId,
    backup_ownerId,
    backup_progressPercent,
    backup_resourceARN,
    backup_resourceType,
    backup_sourceBackupId,
    backup_sourceBackupRegion,
    backup_tags,
    backup_volume,
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
    createFileSystemLustreConfiguration_autoImportPolicy,
    createFileSystemLustreConfiguration_automaticBackupRetentionDays,
    createFileSystemLustreConfiguration_copyTagsToBackups,
    createFileSystemLustreConfiguration_dailyAutomaticBackupStartTime,
    createFileSystemLustreConfiguration_dataCompressionType,
    createFileSystemLustreConfiguration_deploymentType,
    createFileSystemLustreConfiguration_driveCacheType,
    createFileSystemLustreConfiguration_exportPath,
    createFileSystemLustreConfiguration_importPath,
    createFileSystemLustreConfiguration_importedFileChunkSize,
    createFileSystemLustreConfiguration_logConfiguration,
    createFileSystemLustreConfiguration_perUnitStorageThroughput,
    createFileSystemLustreConfiguration_rootSquashConfiguration,
    createFileSystemLustreConfiguration_weeklyMaintenanceStartTime,

    -- ** CreateFileSystemOntapConfiguration
    createFileSystemOntapConfiguration_automaticBackupRetentionDays,
    createFileSystemOntapConfiguration_dailyAutomaticBackupStartTime,
    createFileSystemOntapConfiguration_diskIopsConfiguration,
    createFileSystemOntapConfiguration_endpointIpAddressRange,
    createFileSystemOntapConfiguration_fsxAdminPassword,
    createFileSystemOntapConfiguration_preferredSubnetId,
    createFileSystemOntapConfiguration_routeTableIds,
    createFileSystemOntapConfiguration_weeklyMaintenanceStartTime,
    createFileSystemOntapConfiguration_deploymentType,
    createFileSystemOntapConfiguration_throughputCapacity,

    -- ** CreateFileSystemOpenZFSConfiguration
    createFileSystemOpenZFSConfiguration_automaticBackupRetentionDays,
    createFileSystemOpenZFSConfiguration_copyTagsToBackups,
    createFileSystemOpenZFSConfiguration_copyTagsToVolumes,
    createFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime,
    createFileSystemOpenZFSConfiguration_diskIopsConfiguration,
    createFileSystemOpenZFSConfiguration_rootVolumeConfiguration,
    createFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime,
    createFileSystemOpenZFSConfiguration_deploymentType,
    createFileSystemOpenZFSConfiguration_throughputCapacity,

    -- ** CreateFileSystemWindowsConfiguration
    createFileSystemWindowsConfiguration_activeDirectoryId,
    createFileSystemWindowsConfiguration_aliases,
    createFileSystemWindowsConfiguration_auditLogConfiguration,
    createFileSystemWindowsConfiguration_automaticBackupRetentionDays,
    createFileSystemWindowsConfiguration_copyTagsToBackups,
    createFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime,
    createFileSystemWindowsConfiguration_deploymentType,
    createFileSystemWindowsConfiguration_preferredSubnetId,
    createFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration,
    createFileSystemWindowsConfiguration_weeklyMaintenanceStartTime,
    createFileSystemWindowsConfiguration_throughputCapacity,

    -- ** CreateOntapVolumeConfiguration
    createOntapVolumeConfiguration_copyTagsToBackups,
    createOntapVolumeConfiguration_junctionPath,
    createOntapVolumeConfiguration_ontapVolumeType,
    createOntapVolumeConfiguration_securityStyle,
    createOntapVolumeConfiguration_snapshotPolicy,
    createOntapVolumeConfiguration_storageEfficiencyEnabled,
    createOntapVolumeConfiguration_tieringPolicy,
    createOntapVolumeConfiguration_sizeInMegabytes,
    createOntapVolumeConfiguration_storageVirtualMachineId,

    -- ** CreateOpenZFSOriginSnapshotConfiguration
    createOpenZFSOriginSnapshotConfiguration_snapshotARN,
    createOpenZFSOriginSnapshotConfiguration_copyStrategy,

    -- ** CreateOpenZFSVolumeConfiguration
    createOpenZFSVolumeConfiguration_copyTagsToSnapshots,
    createOpenZFSVolumeConfiguration_dataCompressionType,
    createOpenZFSVolumeConfiguration_nfsExports,
    createOpenZFSVolumeConfiguration_originSnapshot,
    createOpenZFSVolumeConfiguration_readOnly,
    createOpenZFSVolumeConfiguration_recordSizeKiB,
    createOpenZFSVolumeConfiguration_storageCapacityQuotaGiB,
    createOpenZFSVolumeConfiguration_storageCapacityReservationGiB,
    createOpenZFSVolumeConfiguration_userAndGroupQuotas,
    createOpenZFSVolumeConfiguration_parentVolumeId,

    -- ** CreateSvmActiveDirectoryConfiguration
    createSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration,
    createSvmActiveDirectoryConfiguration_netBiosName,

    -- ** DataRepositoryAssociation
    dataRepositoryAssociation_associationId,
    dataRepositoryAssociation_batchImportMetaDataOnCreate,
    dataRepositoryAssociation_creationTime,
    dataRepositoryAssociation_dataRepositoryPath,
    dataRepositoryAssociation_dataRepositorySubdirectories,
    dataRepositoryAssociation_failureDetails,
    dataRepositoryAssociation_fileCacheId,
    dataRepositoryAssociation_fileCachePath,
    dataRepositoryAssociation_fileSystemId,
    dataRepositoryAssociation_fileSystemPath,
    dataRepositoryAssociation_importedFileChunkSize,
    dataRepositoryAssociation_lifecycle,
    dataRepositoryAssociation_nfs,
    dataRepositoryAssociation_resourceARN,
    dataRepositoryAssociation_s3,
    dataRepositoryAssociation_tags,

    -- ** DataRepositoryConfiguration
    dataRepositoryConfiguration_autoImportPolicy,
    dataRepositoryConfiguration_exportPath,
    dataRepositoryConfiguration_failureDetails,
    dataRepositoryConfiguration_importPath,
    dataRepositoryConfiguration_importedFileChunkSize,
    dataRepositoryConfiguration_lifecycle,

    -- ** DataRepositoryFailureDetails
    dataRepositoryFailureDetails_message,

    -- ** DataRepositoryTask
    dataRepositoryTask_capacityToRelease,
    dataRepositoryTask_endTime,
    dataRepositoryTask_failureDetails,
    dataRepositoryTask_fileCacheId,
    dataRepositoryTask_fileSystemId,
    dataRepositoryTask_paths,
    dataRepositoryTask_report,
    dataRepositoryTask_resourceARN,
    dataRepositoryTask_startTime,
    dataRepositoryTask_status,
    dataRepositoryTask_tags,
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
    dataRepositoryTaskStatus_failedCount,
    dataRepositoryTaskStatus_lastUpdatedTime,
    dataRepositoryTaskStatus_releasedCapacity,
    dataRepositoryTaskStatus_succeededCount,
    dataRepositoryTaskStatus_totalCount,

    -- ** DeleteFileSystemLustreConfiguration
    deleteFileSystemLustreConfiguration_finalBackupTags,
    deleteFileSystemLustreConfiguration_skipFinalBackup,

    -- ** DeleteFileSystemLustreResponse
    deleteFileSystemLustreResponse_finalBackupId,
    deleteFileSystemLustreResponse_finalBackupTags,

    -- ** DeleteFileSystemOpenZFSConfiguration
    deleteFileSystemOpenZFSConfiguration_finalBackupTags,
    deleteFileSystemOpenZFSConfiguration_options,
    deleteFileSystemOpenZFSConfiguration_skipFinalBackup,

    -- ** DeleteFileSystemOpenZFSResponse
    deleteFileSystemOpenZFSResponse_finalBackupId,
    deleteFileSystemOpenZFSResponse_finalBackupTags,

    -- ** DeleteFileSystemWindowsConfiguration
    deleteFileSystemWindowsConfiguration_finalBackupTags,
    deleteFileSystemWindowsConfiguration_skipFinalBackup,

    -- ** DeleteFileSystemWindowsResponse
    deleteFileSystemWindowsResponse_finalBackupId,
    deleteFileSystemWindowsResponse_finalBackupTags,

    -- ** DeleteVolumeOntapConfiguration
    deleteVolumeOntapConfiguration_finalBackupTags,
    deleteVolumeOntapConfiguration_skipFinalBackup,

    -- ** DeleteVolumeOntapResponse
    deleteVolumeOntapResponse_finalBackupId,
    deleteVolumeOntapResponse_finalBackupTags,

    -- ** DeleteVolumeOpenZFSConfiguration
    deleteVolumeOpenZFSConfiguration_options,

    -- ** DiskIopsConfiguration
    diskIopsConfiguration_iops,
    diskIopsConfiguration_mode,

    -- ** FileCache
    fileCache_creationTime,
    fileCache_dNSName,
    fileCache_dataRepositoryAssociationIds,
    fileCache_failureDetails,
    fileCache_fileCacheId,
    fileCache_fileCacheType,
    fileCache_fileCacheTypeVersion,
    fileCache_kmsKeyId,
    fileCache_lifecycle,
    fileCache_lustreConfiguration,
    fileCache_networkInterfaceIds,
    fileCache_ownerId,
    fileCache_resourceARN,
    fileCache_storageCapacity,
    fileCache_subnetIds,
    fileCache_vpcId,

    -- ** FileCacheCreating
    fileCacheCreating_copyTagsToDataRepositoryAssociations,
    fileCacheCreating_creationTime,
    fileCacheCreating_dNSName,
    fileCacheCreating_dataRepositoryAssociationIds,
    fileCacheCreating_failureDetails,
    fileCacheCreating_fileCacheId,
    fileCacheCreating_fileCacheType,
    fileCacheCreating_fileCacheTypeVersion,
    fileCacheCreating_kmsKeyId,
    fileCacheCreating_lifecycle,
    fileCacheCreating_lustreConfiguration,
    fileCacheCreating_networkInterfaceIds,
    fileCacheCreating_ownerId,
    fileCacheCreating_resourceARN,
    fileCacheCreating_storageCapacity,
    fileCacheCreating_subnetIds,
    fileCacheCreating_tags,
    fileCacheCreating_vpcId,

    -- ** FileCacheDataRepositoryAssociation
    fileCacheDataRepositoryAssociation_dataRepositorySubdirectories,
    fileCacheDataRepositoryAssociation_nfs,
    fileCacheDataRepositoryAssociation_fileCachePath,
    fileCacheDataRepositoryAssociation_dataRepositoryPath,

    -- ** FileCacheFailureDetails
    fileCacheFailureDetails_message,

    -- ** FileCacheLustreConfiguration
    fileCacheLustreConfiguration_deploymentType,
    fileCacheLustreConfiguration_logConfiguration,
    fileCacheLustreConfiguration_metadataConfiguration,
    fileCacheLustreConfiguration_mountName,
    fileCacheLustreConfiguration_perUnitStorageThroughput,
    fileCacheLustreConfiguration_weeklyMaintenanceStartTime,

    -- ** FileCacheLustreMetadataConfiguration
    fileCacheLustreMetadataConfiguration_storageCapacity,

    -- ** FileCacheNFSConfiguration
    fileCacheNFSConfiguration_dnsIps,
    fileCacheNFSConfiguration_version,

    -- ** FileSystem
    fileSystem_administrativeActions,
    fileSystem_creationTime,
    fileSystem_dNSName,
    fileSystem_failureDetails,
    fileSystem_fileSystemId,
    fileSystem_fileSystemType,
    fileSystem_fileSystemTypeVersion,
    fileSystem_kmsKeyId,
    fileSystem_lifecycle,
    fileSystem_lustreConfiguration,
    fileSystem_networkInterfaceIds,
    fileSystem_ontapConfiguration,
    fileSystem_openZFSConfiguration,
    fileSystem_ownerId,
    fileSystem_resourceARN,
    fileSystem_storageCapacity,
    fileSystem_storageType,
    fileSystem_subnetIds,
    fileSystem_tags,
    fileSystem_vpcId,
    fileSystem_windowsConfiguration,

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
    lustreFileSystemConfiguration_automaticBackupRetentionDays,
    lustreFileSystemConfiguration_copyTagsToBackups,
    lustreFileSystemConfiguration_dailyAutomaticBackupStartTime,
    lustreFileSystemConfiguration_dataCompressionType,
    lustreFileSystemConfiguration_dataRepositoryConfiguration,
    lustreFileSystemConfiguration_deploymentType,
    lustreFileSystemConfiguration_driveCacheType,
    lustreFileSystemConfiguration_logConfiguration,
    lustreFileSystemConfiguration_mountName,
    lustreFileSystemConfiguration_perUnitStorageThroughput,
    lustreFileSystemConfiguration_rootSquashConfiguration,
    lustreFileSystemConfiguration_weeklyMaintenanceStartTime,

    -- ** LustreLogConfiguration
    lustreLogConfiguration_destination,
    lustreLogConfiguration_level,

    -- ** LustreLogCreateConfiguration
    lustreLogCreateConfiguration_destination,
    lustreLogCreateConfiguration_level,

    -- ** LustreRootSquashConfiguration
    lustreRootSquashConfiguration_noSquashNids,
    lustreRootSquashConfiguration_rootSquash,

    -- ** NFSDataRepositoryConfiguration
    nFSDataRepositoryConfiguration_autoExportPolicy,
    nFSDataRepositoryConfiguration_dnsIps,
    nFSDataRepositoryConfiguration_version,

    -- ** OntapFileSystemConfiguration
    ontapFileSystemConfiguration_automaticBackupRetentionDays,
    ontapFileSystemConfiguration_dailyAutomaticBackupStartTime,
    ontapFileSystemConfiguration_deploymentType,
    ontapFileSystemConfiguration_diskIopsConfiguration,
    ontapFileSystemConfiguration_endpointIpAddressRange,
    ontapFileSystemConfiguration_endpoints,
    ontapFileSystemConfiguration_preferredSubnetId,
    ontapFileSystemConfiguration_routeTableIds,
    ontapFileSystemConfiguration_throughputCapacity,
    ontapFileSystemConfiguration_weeklyMaintenanceStartTime,

    -- ** OntapVolumeConfiguration
    ontapVolumeConfiguration_copyTagsToBackups,
    ontapVolumeConfiguration_flexCacheEndpointType,
    ontapVolumeConfiguration_junctionPath,
    ontapVolumeConfiguration_ontapVolumeType,
    ontapVolumeConfiguration_securityStyle,
    ontapVolumeConfiguration_sizeInMegabytes,
    ontapVolumeConfiguration_snapshotPolicy,
    ontapVolumeConfiguration_storageEfficiencyEnabled,
    ontapVolumeConfiguration_storageVirtualMachineId,
    ontapVolumeConfiguration_storageVirtualMachineRoot,
    ontapVolumeConfiguration_tieringPolicy,
    ontapVolumeConfiguration_uuid,

    -- ** OpenZFSClientConfiguration
    openZFSClientConfiguration_clients,
    openZFSClientConfiguration_options,

    -- ** OpenZFSCreateRootVolumeConfiguration
    openZFSCreateRootVolumeConfiguration_copyTagsToSnapshots,
    openZFSCreateRootVolumeConfiguration_dataCompressionType,
    openZFSCreateRootVolumeConfiguration_nfsExports,
    openZFSCreateRootVolumeConfiguration_readOnly,
    openZFSCreateRootVolumeConfiguration_recordSizeKiB,
    openZFSCreateRootVolumeConfiguration_userAndGroupQuotas,

    -- ** OpenZFSFileSystemConfiguration
    openZFSFileSystemConfiguration_automaticBackupRetentionDays,
    openZFSFileSystemConfiguration_copyTagsToBackups,
    openZFSFileSystemConfiguration_copyTagsToVolumes,
    openZFSFileSystemConfiguration_dailyAutomaticBackupStartTime,
    openZFSFileSystemConfiguration_deploymentType,
    openZFSFileSystemConfiguration_diskIopsConfiguration,
    openZFSFileSystemConfiguration_rootVolumeId,
    openZFSFileSystemConfiguration_throughputCapacity,
    openZFSFileSystemConfiguration_weeklyMaintenanceStartTime,

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
    openZFSVolumeConfiguration_copyTagsToSnapshots,
    openZFSVolumeConfiguration_dataCompressionType,
    openZFSVolumeConfiguration_deleteClonedVolumes,
    openZFSVolumeConfiguration_deleteIntermediateSnaphots,
    openZFSVolumeConfiguration_nfsExports,
    openZFSVolumeConfiguration_originSnapshot,
    openZFSVolumeConfiguration_parentVolumeId,
    openZFSVolumeConfiguration_readOnly,
    openZFSVolumeConfiguration_recordSizeKiB,
    openZFSVolumeConfiguration_restoreToSnapshot,
    openZFSVolumeConfiguration_storageCapacityQuotaGiB,
    openZFSVolumeConfiguration_storageCapacityReservationGiB,
    openZFSVolumeConfiguration_userAndGroupQuotas,
    openZFSVolumeConfiguration_volumePath,

    -- ** S3DataRepositoryConfiguration
    s3DataRepositoryConfiguration_autoExportPolicy,
    s3DataRepositoryConfiguration_autoImportPolicy,

    -- ** SelfManagedActiveDirectoryAttributes
    selfManagedActiveDirectoryAttributes_dnsIps,
    selfManagedActiveDirectoryAttributes_domainName,
    selfManagedActiveDirectoryAttributes_fileSystemAdministratorsGroup,
    selfManagedActiveDirectoryAttributes_organizationalUnitDistinguishedName,
    selfManagedActiveDirectoryAttributes_userName,

    -- ** SelfManagedActiveDirectoryConfiguration
    selfManagedActiveDirectoryConfiguration_fileSystemAdministratorsGroup,
    selfManagedActiveDirectoryConfiguration_organizationalUnitDistinguishedName,
    selfManagedActiveDirectoryConfiguration_domainName,
    selfManagedActiveDirectoryConfiguration_userName,
    selfManagedActiveDirectoryConfiguration_password,
    selfManagedActiveDirectoryConfiguration_dnsIps,

    -- ** SelfManagedActiveDirectoryConfigurationUpdates
    selfManagedActiveDirectoryConfigurationUpdates_dnsIps,
    selfManagedActiveDirectoryConfigurationUpdates_password,
    selfManagedActiveDirectoryConfigurationUpdates_userName,

    -- ** Snapshot
    snapshot_administrativeActions,
    snapshot_creationTime,
    snapshot_lifecycle,
    snapshot_lifecycleTransitionReason,
    snapshot_name,
    snapshot_resourceARN,
    snapshot_snapshotId,
    snapshot_tags,
    snapshot_volumeId,

    -- ** SnapshotFilter
    snapshotFilter_name,
    snapshotFilter_values,

    -- ** StorageVirtualMachine
    storageVirtualMachine_activeDirectoryConfiguration,
    storageVirtualMachine_creationTime,
    storageVirtualMachine_endpoints,
    storageVirtualMachine_fileSystemId,
    storageVirtualMachine_lifecycle,
    storageVirtualMachine_lifecycleTransitionReason,
    storageVirtualMachine_name,
    storageVirtualMachine_resourceARN,
    storageVirtualMachine_rootVolumeSecurityStyle,
    storageVirtualMachine_storageVirtualMachineId,
    storageVirtualMachine_tags,
    storageVirtualMachine_uuid,

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
    svmEndpoints_iscsi,
    svmEndpoints_management,
    svmEndpoints_nfs,
    svmEndpoints_smb,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TieringPolicy
    tieringPolicy_coolingPeriod,
    tieringPolicy_name,

    -- ** UpdateFileCacheLustreConfiguration
    updateFileCacheLustreConfiguration_weeklyMaintenanceStartTime,

    -- ** UpdateFileSystemLustreConfiguration
    updateFileSystemLustreConfiguration_autoImportPolicy,
    updateFileSystemLustreConfiguration_automaticBackupRetentionDays,
    updateFileSystemLustreConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemLustreConfiguration_dataCompressionType,
    updateFileSystemLustreConfiguration_logConfiguration,
    updateFileSystemLustreConfiguration_rootSquashConfiguration,
    updateFileSystemLustreConfiguration_weeklyMaintenanceStartTime,

    -- ** UpdateFileSystemOntapConfiguration
    updateFileSystemOntapConfiguration_addRouteTableIds,
    updateFileSystemOntapConfiguration_automaticBackupRetentionDays,
    updateFileSystemOntapConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemOntapConfiguration_diskIopsConfiguration,
    updateFileSystemOntapConfiguration_fsxAdminPassword,
    updateFileSystemOntapConfiguration_removeRouteTableIds,
    updateFileSystemOntapConfiguration_throughputCapacity,
    updateFileSystemOntapConfiguration_weeklyMaintenanceStartTime,

    -- ** UpdateFileSystemOpenZFSConfiguration
    updateFileSystemOpenZFSConfiguration_automaticBackupRetentionDays,
    updateFileSystemOpenZFSConfiguration_copyTagsToBackups,
    updateFileSystemOpenZFSConfiguration_copyTagsToVolumes,
    updateFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemOpenZFSConfiguration_diskIopsConfiguration,
    updateFileSystemOpenZFSConfiguration_throughputCapacity,
    updateFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime,

    -- ** UpdateFileSystemWindowsConfiguration
    updateFileSystemWindowsConfiguration_auditLogConfiguration,
    updateFileSystemWindowsConfiguration_automaticBackupRetentionDays,
    updateFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration,
    updateFileSystemWindowsConfiguration_throughputCapacity,
    updateFileSystemWindowsConfiguration_weeklyMaintenanceStartTime,

    -- ** UpdateOntapVolumeConfiguration
    updateOntapVolumeConfiguration_copyTagsToBackups,
    updateOntapVolumeConfiguration_junctionPath,
    updateOntapVolumeConfiguration_securityStyle,
    updateOntapVolumeConfiguration_sizeInMegabytes,
    updateOntapVolumeConfiguration_snapshotPolicy,
    updateOntapVolumeConfiguration_storageEfficiencyEnabled,
    updateOntapVolumeConfiguration_tieringPolicy,

    -- ** UpdateOpenZFSVolumeConfiguration
    updateOpenZFSVolumeConfiguration_dataCompressionType,
    updateOpenZFSVolumeConfiguration_nfsExports,
    updateOpenZFSVolumeConfiguration_readOnly,
    updateOpenZFSVolumeConfiguration_recordSizeKiB,
    updateOpenZFSVolumeConfiguration_storageCapacityQuotaGiB,
    updateOpenZFSVolumeConfiguration_storageCapacityReservationGiB,
    updateOpenZFSVolumeConfiguration_userAndGroupQuotas,

    -- ** UpdateSvmActiveDirectoryConfiguration
    updateSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration,

    -- ** Volume
    volume_administrativeActions,
    volume_creationTime,
    volume_fileSystemId,
    volume_lifecycle,
    volume_lifecycleTransitionReason,
    volume_name,
    volume_ontapConfiguration,
    volume_openZFSConfiguration,
    volume_resourceARN,
    volume_tags,
    volume_volumeId,
    volume_volumeType,

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
    windowsFileSystemConfiguration_activeDirectoryId,
    windowsFileSystemConfiguration_aliases,
    windowsFileSystemConfiguration_auditLogConfiguration,
    windowsFileSystemConfiguration_automaticBackupRetentionDays,
    windowsFileSystemConfiguration_copyTagsToBackups,
    windowsFileSystemConfiguration_dailyAutomaticBackupStartTime,
    windowsFileSystemConfiguration_deploymentType,
    windowsFileSystemConfiguration_maintenanceOperationsInProgress,
    windowsFileSystemConfiguration_preferredFileServerIp,
    windowsFileSystemConfiguration_preferredSubnetId,
    windowsFileSystemConfiguration_remoteAdministrationEndpoint,
    windowsFileSystemConfiguration_selfManagedActiveDirectoryConfiguration,
    windowsFileSystemConfiguration_throughputCapacity,
    windowsFileSystemConfiguration_weeklyMaintenanceStartTime,
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

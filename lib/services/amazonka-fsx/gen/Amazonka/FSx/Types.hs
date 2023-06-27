{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FSx.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ActiveDirectoryError,
    _BackupBeingCopied,
    _BackupInProgress,
    _BackupNotFound,
    _BackupRestoring,
    _BadRequest,
    _DataRepositoryAssociationNotFound,
    _DataRepositoryTaskEnded,
    _DataRepositoryTaskExecuting,
    _DataRepositoryTaskNotFound,
    _FileCacheNotFound,
    _FileSystemNotFound,
    _IncompatibleParameterError,
    _IncompatibleRegionForMultiAZ,
    _InternalServerError,
    _InvalidDataRepositoryType,
    _InvalidDestinationKmsKey,
    _InvalidExportPath,
    _InvalidImportPath,
    _InvalidNetworkSettings,
    _InvalidPerUnitStorageThroughput,
    _InvalidRegion,
    _InvalidSourceKmsKey,
    _MissingFileCacheConfiguration,
    _MissingFileSystemConfiguration,
    _MissingVolumeConfiguration,
    _NotServiceResourceError,
    _ResourceDoesNotSupportTagging,
    _ResourceNotFound,
    _ServiceLimitExceeded,
    _SnapshotNotFound,
    _SourceBackupUnavailable,
    _StorageVirtualMachineNotFound,
    _UnsupportedOperation,
    _VolumeNotFound,

    -- * AdministrativeActionType
    AdministrativeActionType (..),

    -- * AliasLifecycle
    AliasLifecycle (..),

    -- * AutoImportPolicyType
    AutoImportPolicyType (..),

    -- * BackupLifecycle
    BackupLifecycle (..),

    -- * BackupType
    BackupType (..),

    -- * DataCompressionType
    DataCompressionType (..),

    -- * DataRepositoryLifecycle
    DataRepositoryLifecycle (..),

    -- * DataRepositoryTaskFilterName
    DataRepositoryTaskFilterName (..),

    -- * DataRepositoryTaskLifecycle
    DataRepositoryTaskLifecycle (..),

    -- * DataRepositoryTaskType
    DataRepositoryTaskType (..),

    -- * DeleteFileSystemOpenZFSOption
    DeleteFileSystemOpenZFSOption (..),

    -- * DeleteOpenZFSVolumeOption
    DeleteOpenZFSVolumeOption (..),

    -- * DiskIopsConfigurationMode
    DiskIopsConfigurationMode (..),

    -- * DriveCacheType
    DriveCacheType (..),

    -- * EventType
    EventType (..),

    -- * FileCacheLifecycle
    FileCacheLifecycle (..),

    -- * FileCacheLustreDeploymentType
    FileCacheLustreDeploymentType (..),

    -- * FileCacheType
    FileCacheType (..),

    -- * FileSystemLifecycle
    FileSystemLifecycle (..),

    -- * FileSystemMaintenanceOperation
    FileSystemMaintenanceOperation (..),

    -- * FileSystemType
    FileSystemType (..),

    -- * FilterName
    FilterName (..),

    -- * FlexCacheEndpointType
    FlexCacheEndpointType (..),

    -- * InputOntapVolumeType
    InputOntapVolumeType (..),

    -- * LustreAccessAuditLogLevel
    LustreAccessAuditLogLevel (..),

    -- * LustreDeploymentType
    LustreDeploymentType (..),

    -- * NfsVersion
    NfsVersion (..),

    -- * OntapDeploymentType
    OntapDeploymentType (..),

    -- * OntapVolumeType
    OntapVolumeType (..),

    -- * OpenZFSCopyStrategy
    OpenZFSCopyStrategy (..),

    -- * OpenZFSDataCompressionType
    OpenZFSDataCompressionType (..),

    -- * OpenZFSDeploymentType
    OpenZFSDeploymentType (..),

    -- * OpenZFSQuotaType
    OpenZFSQuotaType (..),

    -- * ReportFormat
    ReportFormat (..),

    -- * ReportScope
    ReportScope (..),

    -- * ResourceType
    ResourceType (..),

    -- * RestoreOpenZFSVolumeOption
    RestoreOpenZFSVolumeOption (..),

    -- * SecurityStyle
    SecurityStyle (..),

    -- * SnapshotFilterName
    SnapshotFilterName (..),

    -- * SnapshotLifecycle
    SnapshotLifecycle (..),

    -- * Status
    Status (..),

    -- * StorageType
    StorageType (..),

    -- * StorageVirtualMachineFilterName
    StorageVirtualMachineFilterName (..),

    -- * StorageVirtualMachineLifecycle
    StorageVirtualMachineLifecycle (..),

    -- * StorageVirtualMachineRootVolumeSecurityStyle
    StorageVirtualMachineRootVolumeSecurityStyle (..),

    -- * StorageVirtualMachineSubtype
    StorageVirtualMachineSubtype (..),

    -- * TieringPolicyName
    TieringPolicyName (..),

    -- * VolumeFilterName
    VolumeFilterName (..),

    -- * VolumeLifecycle
    VolumeLifecycle (..),

    -- * VolumeType
    VolumeType (..),

    -- * WindowsAccessAuditLogLevel
    WindowsAccessAuditLogLevel (..),

    -- * WindowsDeploymentType
    WindowsDeploymentType (..),

    -- * ActiveDirectoryBackupAttributes
    ActiveDirectoryBackupAttributes (..),
    newActiveDirectoryBackupAttributes,
    activeDirectoryBackupAttributes_activeDirectoryId,
    activeDirectoryBackupAttributes_domainName,
    activeDirectoryBackupAttributes_resourceARN,

    -- * AdministrativeAction
    AdministrativeAction (..),
    newAdministrativeAction,
    administrativeAction_administrativeActionType,
    administrativeAction_failureDetails,
    administrativeAction_progressPercent,
    administrativeAction_requestTime,
    administrativeAction_status,
    administrativeAction_targetFileSystemValues,
    administrativeAction_targetSnapshotValues,
    administrativeAction_targetVolumeValues,

    -- * AdministrativeActionFailureDetails
    AdministrativeActionFailureDetails (..),
    newAdministrativeActionFailureDetails,
    administrativeActionFailureDetails_message,

    -- * Alias
    Alias (..),
    newAlias,
    alias_lifecycle,
    alias_name,

    -- * AutoExportPolicy
    AutoExportPolicy (..),
    newAutoExportPolicy,
    autoExportPolicy_events,

    -- * AutoImportPolicy
    AutoImportPolicy (..),
    newAutoImportPolicy,
    autoImportPolicy_events,

    -- * Backup
    Backup (..),
    newBackup,
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

    -- * BackupFailureDetails
    BackupFailureDetails (..),
    newBackupFailureDetails,
    backupFailureDetails_message,

    -- * CompletionReport
    CompletionReport (..),
    newCompletionReport,
    completionReport_format,
    completionReport_path,
    completionReport_scope,
    completionReport_enabled,

    -- * CreateFileCacheLustreConfiguration
    CreateFileCacheLustreConfiguration (..),
    newCreateFileCacheLustreConfiguration,
    createFileCacheLustreConfiguration_weeklyMaintenanceStartTime,
    createFileCacheLustreConfiguration_perUnitStorageThroughput,
    createFileCacheLustreConfiguration_deploymentType,
    createFileCacheLustreConfiguration_metadataConfiguration,

    -- * CreateFileSystemLustreConfiguration
    CreateFileSystemLustreConfiguration (..),
    newCreateFileSystemLustreConfiguration,
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

    -- * CreateFileSystemOntapConfiguration
    CreateFileSystemOntapConfiguration (..),
    newCreateFileSystemOntapConfiguration,
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

    -- * CreateFileSystemOpenZFSConfiguration
    CreateFileSystemOpenZFSConfiguration (..),
    newCreateFileSystemOpenZFSConfiguration,
    createFileSystemOpenZFSConfiguration_automaticBackupRetentionDays,
    createFileSystemOpenZFSConfiguration_copyTagsToBackups,
    createFileSystemOpenZFSConfiguration_copyTagsToVolumes,
    createFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime,
    createFileSystemOpenZFSConfiguration_diskIopsConfiguration,
    createFileSystemOpenZFSConfiguration_rootVolumeConfiguration,
    createFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime,
    createFileSystemOpenZFSConfiguration_deploymentType,
    createFileSystemOpenZFSConfiguration_throughputCapacity,

    -- * CreateFileSystemWindowsConfiguration
    CreateFileSystemWindowsConfiguration (..),
    newCreateFileSystemWindowsConfiguration,
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

    -- * CreateOntapVolumeConfiguration
    CreateOntapVolumeConfiguration (..),
    newCreateOntapVolumeConfiguration,
    createOntapVolumeConfiguration_copyTagsToBackups,
    createOntapVolumeConfiguration_junctionPath,
    createOntapVolumeConfiguration_ontapVolumeType,
    createOntapVolumeConfiguration_securityStyle,
    createOntapVolumeConfiguration_snapshotPolicy,
    createOntapVolumeConfiguration_storageEfficiencyEnabled,
    createOntapVolumeConfiguration_tieringPolicy,
    createOntapVolumeConfiguration_sizeInMegabytes,
    createOntapVolumeConfiguration_storageVirtualMachineId,

    -- * CreateOpenZFSOriginSnapshotConfiguration
    CreateOpenZFSOriginSnapshotConfiguration (..),
    newCreateOpenZFSOriginSnapshotConfiguration,
    createOpenZFSOriginSnapshotConfiguration_snapshotARN,
    createOpenZFSOriginSnapshotConfiguration_copyStrategy,

    -- * CreateOpenZFSVolumeConfiguration
    CreateOpenZFSVolumeConfiguration (..),
    newCreateOpenZFSVolumeConfiguration,
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

    -- * CreateSvmActiveDirectoryConfiguration
    CreateSvmActiveDirectoryConfiguration (..),
    newCreateSvmActiveDirectoryConfiguration,
    createSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration,
    createSvmActiveDirectoryConfiguration_netBiosName,

    -- * DataRepositoryAssociation
    DataRepositoryAssociation (..),
    newDataRepositoryAssociation,
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

    -- * DataRepositoryConfiguration
    DataRepositoryConfiguration (..),
    newDataRepositoryConfiguration,
    dataRepositoryConfiguration_autoImportPolicy,
    dataRepositoryConfiguration_exportPath,
    dataRepositoryConfiguration_failureDetails,
    dataRepositoryConfiguration_importPath,
    dataRepositoryConfiguration_importedFileChunkSize,
    dataRepositoryConfiguration_lifecycle,

    -- * DataRepositoryFailureDetails
    DataRepositoryFailureDetails (..),
    newDataRepositoryFailureDetails,
    dataRepositoryFailureDetails_message,

    -- * DataRepositoryTask
    DataRepositoryTask (..),
    newDataRepositoryTask,
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

    -- * DataRepositoryTaskFailureDetails
    DataRepositoryTaskFailureDetails (..),
    newDataRepositoryTaskFailureDetails,
    dataRepositoryTaskFailureDetails_message,

    -- * DataRepositoryTaskFilter
    DataRepositoryTaskFilter (..),
    newDataRepositoryTaskFilter,
    dataRepositoryTaskFilter_name,
    dataRepositoryTaskFilter_values,

    -- * DataRepositoryTaskStatus
    DataRepositoryTaskStatus (..),
    newDataRepositoryTaskStatus,
    dataRepositoryTaskStatus_failedCount,
    dataRepositoryTaskStatus_lastUpdatedTime,
    dataRepositoryTaskStatus_releasedCapacity,
    dataRepositoryTaskStatus_succeededCount,
    dataRepositoryTaskStatus_totalCount,

    -- * DeleteFileSystemLustreConfiguration
    DeleteFileSystemLustreConfiguration (..),
    newDeleteFileSystemLustreConfiguration,
    deleteFileSystemLustreConfiguration_finalBackupTags,
    deleteFileSystemLustreConfiguration_skipFinalBackup,

    -- * DeleteFileSystemLustreResponse
    DeleteFileSystemLustreResponse (..),
    newDeleteFileSystemLustreResponse,
    deleteFileSystemLustreResponse_finalBackupId,
    deleteFileSystemLustreResponse_finalBackupTags,

    -- * DeleteFileSystemOpenZFSConfiguration
    DeleteFileSystemOpenZFSConfiguration (..),
    newDeleteFileSystemOpenZFSConfiguration,
    deleteFileSystemOpenZFSConfiguration_finalBackupTags,
    deleteFileSystemOpenZFSConfiguration_options,
    deleteFileSystemOpenZFSConfiguration_skipFinalBackup,

    -- * DeleteFileSystemOpenZFSResponse
    DeleteFileSystemOpenZFSResponse (..),
    newDeleteFileSystemOpenZFSResponse,
    deleteFileSystemOpenZFSResponse_finalBackupId,
    deleteFileSystemOpenZFSResponse_finalBackupTags,

    -- * DeleteFileSystemWindowsConfiguration
    DeleteFileSystemWindowsConfiguration (..),
    newDeleteFileSystemWindowsConfiguration,
    deleteFileSystemWindowsConfiguration_finalBackupTags,
    deleteFileSystemWindowsConfiguration_skipFinalBackup,

    -- * DeleteFileSystemWindowsResponse
    DeleteFileSystemWindowsResponse (..),
    newDeleteFileSystemWindowsResponse,
    deleteFileSystemWindowsResponse_finalBackupId,
    deleteFileSystemWindowsResponse_finalBackupTags,

    -- * DeleteVolumeOntapConfiguration
    DeleteVolumeOntapConfiguration (..),
    newDeleteVolumeOntapConfiguration,
    deleteVolumeOntapConfiguration_finalBackupTags,
    deleteVolumeOntapConfiguration_skipFinalBackup,

    -- * DeleteVolumeOntapResponse
    DeleteVolumeOntapResponse (..),
    newDeleteVolumeOntapResponse,
    deleteVolumeOntapResponse_finalBackupId,
    deleteVolumeOntapResponse_finalBackupTags,

    -- * DeleteVolumeOpenZFSConfiguration
    DeleteVolumeOpenZFSConfiguration (..),
    newDeleteVolumeOpenZFSConfiguration,
    deleteVolumeOpenZFSConfiguration_options,

    -- * DiskIopsConfiguration
    DiskIopsConfiguration (..),
    newDiskIopsConfiguration,
    diskIopsConfiguration_iops,
    diskIopsConfiguration_mode,

    -- * FileCache
    FileCache (..),
    newFileCache,
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

    -- * FileCacheCreating
    FileCacheCreating (..),
    newFileCacheCreating,
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

    -- * FileCacheDataRepositoryAssociation
    FileCacheDataRepositoryAssociation (..),
    newFileCacheDataRepositoryAssociation,
    fileCacheDataRepositoryAssociation_dataRepositorySubdirectories,
    fileCacheDataRepositoryAssociation_nfs,
    fileCacheDataRepositoryAssociation_fileCachePath,
    fileCacheDataRepositoryAssociation_dataRepositoryPath,

    -- * FileCacheFailureDetails
    FileCacheFailureDetails (..),
    newFileCacheFailureDetails,
    fileCacheFailureDetails_message,

    -- * FileCacheLustreConfiguration
    FileCacheLustreConfiguration (..),
    newFileCacheLustreConfiguration,
    fileCacheLustreConfiguration_deploymentType,
    fileCacheLustreConfiguration_logConfiguration,
    fileCacheLustreConfiguration_metadataConfiguration,
    fileCacheLustreConfiguration_mountName,
    fileCacheLustreConfiguration_perUnitStorageThroughput,
    fileCacheLustreConfiguration_weeklyMaintenanceStartTime,

    -- * FileCacheLustreMetadataConfiguration
    FileCacheLustreMetadataConfiguration (..),
    newFileCacheLustreMetadataConfiguration,
    fileCacheLustreMetadataConfiguration_storageCapacity,

    -- * FileCacheNFSConfiguration
    FileCacheNFSConfiguration (..),
    newFileCacheNFSConfiguration,
    fileCacheNFSConfiguration_dnsIps,
    fileCacheNFSConfiguration_version,

    -- * FileSystem
    FileSystem (..),
    newFileSystem,
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

    -- * FileSystemEndpoint
    FileSystemEndpoint (..),
    newFileSystemEndpoint,
    fileSystemEndpoint_dNSName,
    fileSystemEndpoint_ipAddresses,

    -- * FileSystemEndpoints
    FileSystemEndpoints (..),
    newFileSystemEndpoints,
    fileSystemEndpoints_intercluster,
    fileSystemEndpoints_management,

    -- * FileSystemFailureDetails
    FileSystemFailureDetails (..),
    newFileSystemFailureDetails,
    fileSystemFailureDetails_message,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * LifecycleTransitionReason
    LifecycleTransitionReason (..),
    newLifecycleTransitionReason,
    lifecycleTransitionReason_message,

    -- * LustreFileSystemConfiguration
    LustreFileSystemConfiguration (..),
    newLustreFileSystemConfiguration,
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

    -- * LustreLogConfiguration
    LustreLogConfiguration (..),
    newLustreLogConfiguration,
    lustreLogConfiguration_destination,
    lustreLogConfiguration_level,

    -- * LustreLogCreateConfiguration
    LustreLogCreateConfiguration (..),
    newLustreLogCreateConfiguration,
    lustreLogCreateConfiguration_destination,
    lustreLogCreateConfiguration_level,

    -- * LustreRootSquashConfiguration
    LustreRootSquashConfiguration (..),
    newLustreRootSquashConfiguration,
    lustreRootSquashConfiguration_noSquashNids,
    lustreRootSquashConfiguration_rootSquash,

    -- * NFSDataRepositoryConfiguration
    NFSDataRepositoryConfiguration (..),
    newNFSDataRepositoryConfiguration,
    nFSDataRepositoryConfiguration_autoExportPolicy,
    nFSDataRepositoryConfiguration_dnsIps,
    nFSDataRepositoryConfiguration_version,

    -- * OntapFileSystemConfiguration
    OntapFileSystemConfiguration (..),
    newOntapFileSystemConfiguration,
    ontapFileSystemConfiguration_automaticBackupRetentionDays,
    ontapFileSystemConfiguration_dailyAutomaticBackupStartTime,
    ontapFileSystemConfiguration_deploymentType,
    ontapFileSystemConfiguration_diskIopsConfiguration,
    ontapFileSystemConfiguration_endpointIpAddressRange,
    ontapFileSystemConfiguration_endpoints,
    ontapFileSystemConfiguration_fsxAdminPassword,
    ontapFileSystemConfiguration_preferredSubnetId,
    ontapFileSystemConfiguration_routeTableIds,
    ontapFileSystemConfiguration_throughputCapacity,
    ontapFileSystemConfiguration_weeklyMaintenanceStartTime,

    -- * OntapVolumeConfiguration
    OntapVolumeConfiguration (..),
    newOntapVolumeConfiguration,
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

    -- * OpenZFSClientConfiguration
    OpenZFSClientConfiguration (..),
    newOpenZFSClientConfiguration,
    openZFSClientConfiguration_clients,
    openZFSClientConfiguration_options,

    -- * OpenZFSCreateRootVolumeConfiguration
    OpenZFSCreateRootVolumeConfiguration (..),
    newOpenZFSCreateRootVolumeConfiguration,
    openZFSCreateRootVolumeConfiguration_copyTagsToSnapshots,
    openZFSCreateRootVolumeConfiguration_dataCompressionType,
    openZFSCreateRootVolumeConfiguration_nfsExports,
    openZFSCreateRootVolumeConfiguration_readOnly,
    openZFSCreateRootVolumeConfiguration_recordSizeKiB,
    openZFSCreateRootVolumeConfiguration_userAndGroupQuotas,

    -- * OpenZFSFileSystemConfiguration
    OpenZFSFileSystemConfiguration (..),
    newOpenZFSFileSystemConfiguration,
    openZFSFileSystemConfiguration_automaticBackupRetentionDays,
    openZFSFileSystemConfiguration_copyTagsToBackups,
    openZFSFileSystemConfiguration_copyTagsToVolumes,
    openZFSFileSystemConfiguration_dailyAutomaticBackupStartTime,
    openZFSFileSystemConfiguration_deploymentType,
    openZFSFileSystemConfiguration_diskIopsConfiguration,
    openZFSFileSystemConfiguration_rootVolumeId,
    openZFSFileSystemConfiguration_throughputCapacity,
    openZFSFileSystemConfiguration_weeklyMaintenanceStartTime,

    -- * OpenZFSNfsExport
    OpenZFSNfsExport (..),
    newOpenZFSNfsExport,
    openZFSNfsExport_clientConfigurations,

    -- * OpenZFSOriginSnapshotConfiguration
    OpenZFSOriginSnapshotConfiguration (..),
    newOpenZFSOriginSnapshotConfiguration,
    openZFSOriginSnapshotConfiguration_copyStrategy,
    openZFSOriginSnapshotConfiguration_snapshotARN,

    -- * OpenZFSUserOrGroupQuota
    OpenZFSUserOrGroupQuota (..),
    newOpenZFSUserOrGroupQuota,
    openZFSUserOrGroupQuota_type,
    openZFSUserOrGroupQuota_id,
    openZFSUserOrGroupQuota_storageCapacityQuotaGiB,

    -- * OpenZFSVolumeConfiguration
    OpenZFSVolumeConfiguration (..),
    newOpenZFSVolumeConfiguration,
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

    -- * S3DataRepositoryConfiguration
    S3DataRepositoryConfiguration (..),
    newS3DataRepositoryConfiguration,
    s3DataRepositoryConfiguration_autoExportPolicy,
    s3DataRepositoryConfiguration_autoImportPolicy,

    -- * SelfManagedActiveDirectoryAttributes
    SelfManagedActiveDirectoryAttributes (..),
    newSelfManagedActiveDirectoryAttributes,
    selfManagedActiveDirectoryAttributes_dnsIps,
    selfManagedActiveDirectoryAttributes_domainName,
    selfManagedActiveDirectoryAttributes_fileSystemAdministratorsGroup,
    selfManagedActiveDirectoryAttributes_organizationalUnitDistinguishedName,
    selfManagedActiveDirectoryAttributes_userName,

    -- * SelfManagedActiveDirectoryConfiguration
    SelfManagedActiveDirectoryConfiguration (..),
    newSelfManagedActiveDirectoryConfiguration,
    selfManagedActiveDirectoryConfiguration_fileSystemAdministratorsGroup,
    selfManagedActiveDirectoryConfiguration_organizationalUnitDistinguishedName,
    selfManagedActiveDirectoryConfiguration_domainName,
    selfManagedActiveDirectoryConfiguration_userName,
    selfManagedActiveDirectoryConfiguration_password,
    selfManagedActiveDirectoryConfiguration_dnsIps,

    -- * SelfManagedActiveDirectoryConfigurationUpdates
    SelfManagedActiveDirectoryConfigurationUpdates (..),
    newSelfManagedActiveDirectoryConfigurationUpdates,
    selfManagedActiveDirectoryConfigurationUpdates_dnsIps,
    selfManagedActiveDirectoryConfigurationUpdates_domainName,
    selfManagedActiveDirectoryConfigurationUpdates_fileSystemAdministratorsGroup,
    selfManagedActiveDirectoryConfigurationUpdates_organizationalUnitDistinguishedName,
    selfManagedActiveDirectoryConfigurationUpdates_password,
    selfManagedActiveDirectoryConfigurationUpdates_userName,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_administrativeActions,
    snapshot_creationTime,
    snapshot_lifecycle,
    snapshot_lifecycleTransitionReason,
    snapshot_name,
    snapshot_resourceARN,
    snapshot_snapshotId,
    snapshot_tags,
    snapshot_volumeId,

    -- * SnapshotFilter
    SnapshotFilter (..),
    newSnapshotFilter,
    snapshotFilter_name,
    snapshotFilter_values,

    -- * StorageVirtualMachine
    StorageVirtualMachine (..),
    newStorageVirtualMachine,
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
    storageVirtualMachine_subtype,
    storageVirtualMachine_tags,
    storageVirtualMachine_uuid,

    -- * StorageVirtualMachineFilter
    StorageVirtualMachineFilter (..),
    newStorageVirtualMachineFilter,
    storageVirtualMachineFilter_name,
    storageVirtualMachineFilter_values,

    -- * SvmActiveDirectoryConfiguration
    SvmActiveDirectoryConfiguration (..),
    newSvmActiveDirectoryConfiguration,
    svmActiveDirectoryConfiguration_netBiosName,
    svmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration,

    -- * SvmEndpoint
    SvmEndpoint (..),
    newSvmEndpoint,
    svmEndpoint_dNSName,
    svmEndpoint_ipAddresses,

    -- * SvmEndpoints
    SvmEndpoints (..),
    newSvmEndpoints,
    svmEndpoints_iscsi,
    svmEndpoints_management,
    svmEndpoints_nfs,
    svmEndpoints_smb,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TieringPolicy
    TieringPolicy (..),
    newTieringPolicy,
    tieringPolicy_coolingPeriod,
    tieringPolicy_name,

    -- * UpdateFileCacheLustreConfiguration
    UpdateFileCacheLustreConfiguration (..),
    newUpdateFileCacheLustreConfiguration,
    updateFileCacheLustreConfiguration_weeklyMaintenanceStartTime,

    -- * UpdateFileSystemLustreConfiguration
    UpdateFileSystemLustreConfiguration (..),
    newUpdateFileSystemLustreConfiguration,
    updateFileSystemLustreConfiguration_autoImportPolicy,
    updateFileSystemLustreConfiguration_automaticBackupRetentionDays,
    updateFileSystemLustreConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemLustreConfiguration_dataCompressionType,
    updateFileSystemLustreConfiguration_logConfiguration,
    updateFileSystemLustreConfiguration_rootSquashConfiguration,
    updateFileSystemLustreConfiguration_weeklyMaintenanceStartTime,

    -- * UpdateFileSystemOntapConfiguration
    UpdateFileSystemOntapConfiguration (..),
    newUpdateFileSystemOntapConfiguration,
    updateFileSystemOntapConfiguration_addRouteTableIds,
    updateFileSystemOntapConfiguration_automaticBackupRetentionDays,
    updateFileSystemOntapConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemOntapConfiguration_diskIopsConfiguration,
    updateFileSystemOntapConfiguration_fsxAdminPassword,
    updateFileSystemOntapConfiguration_removeRouteTableIds,
    updateFileSystemOntapConfiguration_throughputCapacity,
    updateFileSystemOntapConfiguration_weeklyMaintenanceStartTime,

    -- * UpdateFileSystemOpenZFSConfiguration
    UpdateFileSystemOpenZFSConfiguration (..),
    newUpdateFileSystemOpenZFSConfiguration,
    updateFileSystemOpenZFSConfiguration_automaticBackupRetentionDays,
    updateFileSystemOpenZFSConfiguration_copyTagsToBackups,
    updateFileSystemOpenZFSConfiguration_copyTagsToVolumes,
    updateFileSystemOpenZFSConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemOpenZFSConfiguration_diskIopsConfiguration,
    updateFileSystemOpenZFSConfiguration_throughputCapacity,
    updateFileSystemOpenZFSConfiguration_weeklyMaintenanceStartTime,

    -- * UpdateFileSystemWindowsConfiguration
    UpdateFileSystemWindowsConfiguration (..),
    newUpdateFileSystemWindowsConfiguration,
    updateFileSystemWindowsConfiguration_auditLogConfiguration,
    updateFileSystemWindowsConfiguration_automaticBackupRetentionDays,
    updateFileSystemWindowsConfiguration_dailyAutomaticBackupStartTime,
    updateFileSystemWindowsConfiguration_selfManagedActiveDirectoryConfiguration,
    updateFileSystemWindowsConfiguration_throughputCapacity,
    updateFileSystemWindowsConfiguration_weeklyMaintenanceStartTime,

    -- * UpdateOntapVolumeConfiguration
    UpdateOntapVolumeConfiguration (..),
    newUpdateOntapVolumeConfiguration,
    updateOntapVolumeConfiguration_copyTagsToBackups,
    updateOntapVolumeConfiguration_junctionPath,
    updateOntapVolumeConfiguration_securityStyle,
    updateOntapVolumeConfiguration_sizeInMegabytes,
    updateOntapVolumeConfiguration_snapshotPolicy,
    updateOntapVolumeConfiguration_storageEfficiencyEnabled,
    updateOntapVolumeConfiguration_tieringPolicy,

    -- * UpdateOpenZFSVolumeConfiguration
    UpdateOpenZFSVolumeConfiguration (..),
    newUpdateOpenZFSVolumeConfiguration,
    updateOpenZFSVolumeConfiguration_dataCompressionType,
    updateOpenZFSVolumeConfiguration_nfsExports,
    updateOpenZFSVolumeConfiguration_readOnly,
    updateOpenZFSVolumeConfiguration_recordSizeKiB,
    updateOpenZFSVolumeConfiguration_storageCapacityQuotaGiB,
    updateOpenZFSVolumeConfiguration_storageCapacityReservationGiB,
    updateOpenZFSVolumeConfiguration_userAndGroupQuotas,

    -- * UpdateSvmActiveDirectoryConfiguration
    UpdateSvmActiveDirectoryConfiguration (..),
    newUpdateSvmActiveDirectoryConfiguration,
    updateSvmActiveDirectoryConfiguration_netBiosName,
    updateSvmActiveDirectoryConfiguration_selfManagedActiveDirectoryConfiguration,

    -- * Volume
    Volume (..),
    newVolume,
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

    -- * VolumeFilter
    VolumeFilter (..),
    newVolumeFilter,
    volumeFilter_name,
    volumeFilter_values,

    -- * WindowsAuditLogConfiguration
    WindowsAuditLogConfiguration (..),
    newWindowsAuditLogConfiguration,
    windowsAuditLogConfiguration_auditLogDestination,
    windowsAuditLogConfiguration_fileAccessAuditLogLevel,
    windowsAuditLogConfiguration_fileShareAccessAuditLogLevel,

    -- * WindowsAuditLogCreateConfiguration
    WindowsAuditLogCreateConfiguration (..),
    newWindowsAuditLogCreateConfiguration,
    windowsAuditLogCreateConfiguration_auditLogDestination,
    windowsAuditLogCreateConfiguration_fileAccessAuditLogLevel,
    windowsAuditLogCreateConfiguration_fileShareAccessAuditLogLevel,

    -- * WindowsFileSystemConfiguration
    WindowsFileSystemConfiguration (..),
    newWindowsFileSystemConfiguration,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.ActiveDirectoryBackupAttributes
import Amazonka.FSx.Types.AdministrativeAction
import Amazonka.FSx.Types.AdministrativeActionFailureDetails
import Amazonka.FSx.Types.AdministrativeActionType
import Amazonka.FSx.Types.Alias
import Amazonka.FSx.Types.AliasLifecycle
import Amazonka.FSx.Types.AutoExportPolicy
import Amazonka.FSx.Types.AutoImportPolicy
import Amazonka.FSx.Types.AutoImportPolicyType
import Amazonka.FSx.Types.Backup
import Amazonka.FSx.Types.BackupFailureDetails
import Amazonka.FSx.Types.BackupLifecycle
import Amazonka.FSx.Types.BackupType
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
import Amazonka.FSx.Types.DataCompressionType
import Amazonka.FSx.Types.DataRepositoryAssociation
import Amazonka.FSx.Types.DataRepositoryConfiguration
import Amazonka.FSx.Types.DataRepositoryFailureDetails
import Amazonka.FSx.Types.DataRepositoryLifecycle
import Amazonka.FSx.Types.DataRepositoryTask
import Amazonka.FSx.Types.DataRepositoryTaskFailureDetails
import Amazonka.FSx.Types.DataRepositoryTaskFilter
import Amazonka.FSx.Types.DataRepositoryTaskFilterName
import Amazonka.FSx.Types.DataRepositoryTaskLifecycle
import Amazonka.FSx.Types.DataRepositoryTaskStatus
import Amazonka.FSx.Types.DataRepositoryTaskType
import Amazonka.FSx.Types.DeleteFileSystemLustreConfiguration
import Amazonka.FSx.Types.DeleteFileSystemLustreResponse
import Amazonka.FSx.Types.DeleteFileSystemOpenZFSConfiguration
import Amazonka.FSx.Types.DeleteFileSystemOpenZFSOption
import Amazonka.FSx.Types.DeleteFileSystemOpenZFSResponse
import Amazonka.FSx.Types.DeleteFileSystemWindowsConfiguration
import Amazonka.FSx.Types.DeleteFileSystemWindowsResponse
import Amazonka.FSx.Types.DeleteOpenZFSVolumeOption
import Amazonka.FSx.Types.DeleteVolumeOntapConfiguration
import Amazonka.FSx.Types.DeleteVolumeOntapResponse
import Amazonka.FSx.Types.DeleteVolumeOpenZFSConfiguration
import Amazonka.FSx.Types.DiskIopsConfiguration
import Amazonka.FSx.Types.DiskIopsConfigurationMode
import Amazonka.FSx.Types.DriveCacheType
import Amazonka.FSx.Types.EventType
import Amazonka.FSx.Types.FileCache
import Amazonka.FSx.Types.FileCacheCreating
import Amazonka.FSx.Types.FileCacheDataRepositoryAssociation
import Amazonka.FSx.Types.FileCacheFailureDetails
import Amazonka.FSx.Types.FileCacheLifecycle
import Amazonka.FSx.Types.FileCacheLustreConfiguration
import Amazonka.FSx.Types.FileCacheLustreDeploymentType
import Amazonka.FSx.Types.FileCacheLustreMetadataConfiguration
import Amazonka.FSx.Types.FileCacheNFSConfiguration
import Amazonka.FSx.Types.FileCacheType
import Amazonka.FSx.Types.FileSystem
import Amazonka.FSx.Types.FileSystemEndpoint
import Amazonka.FSx.Types.FileSystemEndpoints
import Amazonka.FSx.Types.FileSystemFailureDetails
import Amazonka.FSx.Types.FileSystemLifecycle
import Amazonka.FSx.Types.FileSystemMaintenanceOperation
import Amazonka.FSx.Types.FileSystemType
import Amazonka.FSx.Types.Filter
import Amazonka.FSx.Types.FilterName
import Amazonka.FSx.Types.FlexCacheEndpointType
import Amazonka.FSx.Types.InputOntapVolumeType
import Amazonka.FSx.Types.LifecycleTransitionReason
import Amazonka.FSx.Types.LustreAccessAuditLogLevel
import Amazonka.FSx.Types.LustreDeploymentType
import Amazonka.FSx.Types.LustreFileSystemConfiguration
import Amazonka.FSx.Types.LustreLogConfiguration
import Amazonka.FSx.Types.LustreLogCreateConfiguration
import Amazonka.FSx.Types.LustreRootSquashConfiguration
import Amazonka.FSx.Types.NFSDataRepositoryConfiguration
import Amazonka.FSx.Types.NfsVersion
import Amazonka.FSx.Types.OntapDeploymentType
import Amazonka.FSx.Types.OntapFileSystemConfiguration
import Amazonka.FSx.Types.OntapVolumeConfiguration
import Amazonka.FSx.Types.OntapVolumeType
import Amazonka.FSx.Types.OpenZFSClientConfiguration
import Amazonka.FSx.Types.OpenZFSCopyStrategy
import Amazonka.FSx.Types.OpenZFSCreateRootVolumeConfiguration
import Amazonka.FSx.Types.OpenZFSDataCompressionType
import Amazonka.FSx.Types.OpenZFSDeploymentType
import Amazonka.FSx.Types.OpenZFSFileSystemConfiguration
import Amazonka.FSx.Types.OpenZFSNfsExport
import Amazonka.FSx.Types.OpenZFSOriginSnapshotConfiguration
import Amazonka.FSx.Types.OpenZFSQuotaType
import Amazonka.FSx.Types.OpenZFSUserOrGroupQuota
import Amazonka.FSx.Types.OpenZFSVolumeConfiguration
import Amazonka.FSx.Types.ReportFormat
import Amazonka.FSx.Types.ReportScope
import Amazonka.FSx.Types.ResourceType
import Amazonka.FSx.Types.RestoreOpenZFSVolumeOption
import Amazonka.FSx.Types.S3DataRepositoryConfiguration
import Amazonka.FSx.Types.SecurityStyle
import Amazonka.FSx.Types.SelfManagedActiveDirectoryAttributes
import Amazonka.FSx.Types.SelfManagedActiveDirectoryConfiguration
import Amazonka.FSx.Types.SelfManagedActiveDirectoryConfigurationUpdates
import Amazonka.FSx.Types.Snapshot
import Amazonka.FSx.Types.SnapshotFilter
import Amazonka.FSx.Types.SnapshotFilterName
import Amazonka.FSx.Types.SnapshotLifecycle
import Amazonka.FSx.Types.Status
import Amazonka.FSx.Types.StorageType
import Amazonka.FSx.Types.StorageVirtualMachine
import Amazonka.FSx.Types.StorageVirtualMachineFilter
import Amazonka.FSx.Types.StorageVirtualMachineFilterName
import Amazonka.FSx.Types.StorageVirtualMachineLifecycle
import Amazonka.FSx.Types.StorageVirtualMachineRootVolumeSecurityStyle
import Amazonka.FSx.Types.StorageVirtualMachineSubtype
import Amazonka.FSx.Types.SvmActiveDirectoryConfiguration
import Amazonka.FSx.Types.SvmEndpoint
import Amazonka.FSx.Types.SvmEndpoints
import Amazonka.FSx.Types.Tag
import Amazonka.FSx.Types.TieringPolicy
import Amazonka.FSx.Types.TieringPolicyName
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
import Amazonka.FSx.Types.VolumeFilterName
import Amazonka.FSx.Types.VolumeLifecycle
import Amazonka.FSx.Types.VolumeType
import Amazonka.FSx.Types.WindowsAccessAuditLogLevel
import Amazonka.FSx.Types.WindowsAuditLogConfiguration
import Amazonka.FSx.Types.WindowsAuditLogCreateConfiguration
import Amazonka.FSx.Types.WindowsDeploymentType
import Amazonka.FSx.Types.WindowsFileSystemConfiguration
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-03-01@ of the Amazon FSx SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "FSx",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "fsx",
      Core.signingName = "fsx",
      Core.version = "2018-03-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "FSx",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An Active Directory error.
_ActiveDirectoryError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ActiveDirectoryError =
  Core._MatchServiceError
    defaultService
    "ActiveDirectoryError"

-- | You can\'t delete a backup while it\'s being copied.
_BackupBeingCopied :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BackupBeingCopied =
  Core._MatchServiceError
    defaultService
    "BackupBeingCopied"

-- | Another backup is already under way. Wait for completion before
-- initiating additional backups of this file system.
_BackupInProgress :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BackupInProgress =
  Core._MatchServiceError
    defaultService
    "BackupInProgress"

-- | No Amazon FSx backups were found based upon the supplied parameters.
_BackupNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BackupNotFound =
  Core._MatchServiceError
    defaultService
    "BackupNotFound"

-- | You can\'t delete a backup while it\'s being used to restore a file
-- system.
_BackupRestoring :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BackupRestoring =
  Core._MatchServiceError
    defaultService
    "BackupRestoring"

-- | A generic error indicating a failure with a client request.
_BadRequest :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequest =
  Core._MatchServiceError defaultService "BadRequest"

-- | No data repository associations were found based upon the supplied
-- parameters.
_DataRepositoryAssociationNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DataRepositoryAssociationNotFound =
  Core._MatchServiceError
    defaultService
    "DataRepositoryAssociationNotFound"

-- | The data repository task could not be canceled because the task has
-- already ended.
_DataRepositoryTaskEnded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DataRepositoryTaskEnded =
  Core._MatchServiceError
    defaultService
    "DataRepositoryTaskEnded"

-- | An existing data repository task is currently executing on the file
-- system. Wait until the existing task has completed, then create the new
-- task.
_DataRepositoryTaskExecuting :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DataRepositoryTaskExecuting =
  Core._MatchServiceError
    defaultService
    "DataRepositoryTaskExecuting"

-- | The data repository task or tasks you specified could not be found.
_DataRepositoryTaskNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DataRepositoryTaskNotFound =
  Core._MatchServiceError
    defaultService
    "DataRepositoryTaskNotFound"

-- | No caches were found based upon supplied parameters.
_FileCacheNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FileCacheNotFound =
  Core._MatchServiceError
    defaultService
    "FileCacheNotFound"

-- | No Amazon FSx file systems were found based upon supplied parameters.
_FileSystemNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FileSystemNotFound =
  Core._MatchServiceError
    defaultService
    "FileSystemNotFound"

-- | The error returned when a second request is received with the same
-- client request token but different parameters settings. A client request
-- token should always uniquely identify a single request.
_IncompatibleParameterError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IncompatibleParameterError =
  Core._MatchServiceError
    defaultService
    "IncompatibleParameterError"

-- | Amazon FSx doesn\'t support Multi-AZ Windows File Server copy backup in
-- the destination Region, so the copied backup can\'t be restored.
_IncompatibleRegionForMultiAZ :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IncompatibleRegionForMultiAZ =
  Core._MatchServiceError
    defaultService
    "IncompatibleRegionForMultiAZ"

-- | A generic error indicating a server-side failure.
_InternalServerError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | You have filtered the response to a data repository type that is not
-- supported.
_InvalidDataRepositoryType :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDataRepositoryType =
  Core._MatchServiceError
    defaultService
    "InvalidDataRepositoryType"

-- | The Key Management Service (KMS) key of the destination backup is not
-- valid.
_InvalidDestinationKmsKey :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDestinationKmsKey =
  Core._MatchServiceError
    defaultService
    "InvalidDestinationKmsKey"

-- | The path provided for data repository export isn\'t valid.
_InvalidExportPath :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidExportPath =
  Core._MatchServiceError
    defaultService
    "InvalidExportPath"

-- | The path provided for data repository import isn\'t valid.
_InvalidImportPath :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidImportPath =
  Core._MatchServiceError
    defaultService
    "InvalidImportPath"

-- | One or more network settings specified in the request are invalid.
_InvalidNetworkSettings :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNetworkSettings =
  Core._MatchServiceError
    defaultService
    "InvalidNetworkSettings"

-- | An invalid value for @PerUnitStorageThroughput@ was provided. Please
-- create your file system again, using a valid value.
_InvalidPerUnitStorageThroughput :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPerUnitStorageThroughput =
  Core._MatchServiceError
    defaultService
    "InvalidPerUnitStorageThroughput"

-- | The Region provided for @SourceRegion@ is not valid or is in a different
-- Amazon Web Services partition.
_InvalidRegion :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRegion =
  Core._MatchServiceError
    defaultService
    "InvalidRegion"

-- | The Key Management Service (KMS) key of the source backup is not valid.
_InvalidSourceKmsKey :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSourceKmsKey =
  Core._MatchServiceError
    defaultService
    "InvalidSourceKmsKey"

-- | A cache configuration is required for this operation.
_MissingFileCacheConfiguration :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MissingFileCacheConfiguration =
  Core._MatchServiceError
    defaultService
    "MissingFileCacheConfiguration"

-- | A file system configuration is required for this operation.
_MissingFileSystemConfiguration :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MissingFileSystemConfiguration =
  Core._MatchServiceError
    defaultService
    "MissingFileSystemConfiguration"

-- | A volume configuration is required for this operation.
_MissingVolumeConfiguration :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MissingVolumeConfiguration =
  Core._MatchServiceError
    defaultService
    "MissingVolumeConfiguration"

-- | The resource specified for the tagging operation is not a resource type
-- owned by Amazon FSx. Use the API of the relevant service to perform the
-- operation.
_NotServiceResourceError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotServiceResourceError =
  Core._MatchServiceError
    defaultService
    "NotServiceResourceError"

-- | The resource specified does not support tagging.
_ResourceDoesNotSupportTagging :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceDoesNotSupportTagging =
  Core._MatchServiceError
    defaultService
    "ResourceDoesNotSupportTagging"

-- | The resource specified by the Amazon Resource Name (ARN) can\'t be
-- found.
_ResourceNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFound =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"

-- | An error indicating that a particular service limit was exceeded. You
-- can increase some service limits by contacting Amazon Web Services
-- Support.
_ServiceLimitExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ServiceLimitExceeded"

-- | No Amazon FSx snapshots were found based on the supplied parameters.
_SnapshotNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotNotFound =
  Core._MatchServiceError
    defaultService
    "SnapshotNotFound"

-- | The request was rejected because the lifecycle status of the source
-- backup isn\'t @AVAILABLE@.
_SourceBackupUnavailable :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SourceBackupUnavailable =
  Core._MatchServiceError
    defaultService
    "SourceBackupUnavailable"

-- | No FSx for ONTAP SVMs were found based upon the supplied parameters.
_StorageVirtualMachineNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StorageVirtualMachineNotFound =
  Core._MatchServiceError
    defaultService
    "StorageVirtualMachineNotFound"

-- | The requested operation is not supported for this resource or API.
_UnsupportedOperation :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedOperation =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperation"

-- | No Amazon FSx volumes were found based upon the supplied parameters.
_VolumeNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_VolumeNotFound =
  Core._MatchServiceError
    defaultService
    "VolumeNotFound"

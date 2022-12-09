{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.FSx
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-03-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon FSx is a fully managed service that makes it easy for storage and
-- application administrators to launch and use shared file storage.
module Amazonka.FSx
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ActiveDirectoryError
    _ActiveDirectoryError,

    -- ** BackupBeingCopied
    _BackupBeingCopied,

    -- ** BackupInProgress
    _BackupInProgress,

    -- ** BackupNotFound
    _BackupNotFound,

    -- ** BackupRestoring
    _BackupRestoring,

    -- ** BadRequest
    _BadRequest,

    -- ** DataRepositoryAssociationNotFound
    _DataRepositoryAssociationNotFound,

    -- ** DataRepositoryTaskEnded
    _DataRepositoryTaskEnded,

    -- ** DataRepositoryTaskExecuting
    _DataRepositoryTaskExecuting,

    -- ** DataRepositoryTaskNotFound
    _DataRepositoryTaskNotFound,

    -- ** FileCacheNotFound
    _FileCacheNotFound,

    -- ** FileSystemNotFound
    _FileSystemNotFound,

    -- ** IncompatibleParameterError
    _IncompatibleParameterError,

    -- ** IncompatibleRegionForMultiAZ
    _IncompatibleRegionForMultiAZ,

    -- ** InternalServerError
    _InternalServerError,

    -- ** InvalidDataRepositoryType
    _InvalidDataRepositoryType,

    -- ** InvalidDestinationKmsKey
    _InvalidDestinationKmsKey,

    -- ** InvalidExportPath
    _InvalidExportPath,

    -- ** InvalidImportPath
    _InvalidImportPath,

    -- ** InvalidNetworkSettings
    _InvalidNetworkSettings,

    -- ** InvalidPerUnitStorageThroughput
    _InvalidPerUnitStorageThroughput,

    -- ** InvalidRegion
    _InvalidRegion,

    -- ** InvalidSourceKmsKey
    _InvalidSourceKmsKey,

    -- ** MissingFileCacheConfiguration
    _MissingFileCacheConfiguration,

    -- ** MissingFileSystemConfiguration
    _MissingFileSystemConfiguration,

    -- ** MissingVolumeConfiguration
    _MissingVolumeConfiguration,

    -- ** NotServiceResourceError
    _NotServiceResourceError,

    -- ** ResourceDoesNotSupportTagging
    _ResourceDoesNotSupportTagging,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- ** ServiceLimitExceeded
    _ServiceLimitExceeded,

    -- ** SnapshotNotFound
    _SnapshotNotFound,

    -- ** SourceBackupUnavailable
    _SourceBackupUnavailable,

    -- ** StorageVirtualMachineNotFound
    _StorageVirtualMachineNotFound,

    -- ** UnsupportedOperation
    _UnsupportedOperation,

    -- ** VolumeNotFound
    _VolumeNotFound,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateFileSystemAliases
    AssociateFileSystemAliases (AssociateFileSystemAliases'),
    newAssociateFileSystemAliases,
    AssociateFileSystemAliasesResponse (AssociateFileSystemAliasesResponse'),
    newAssociateFileSystemAliasesResponse,

    -- ** CancelDataRepositoryTask
    CancelDataRepositoryTask (CancelDataRepositoryTask'),
    newCancelDataRepositoryTask,
    CancelDataRepositoryTaskResponse (CancelDataRepositoryTaskResponse'),
    newCancelDataRepositoryTaskResponse,

    -- ** CopyBackup
    CopyBackup (CopyBackup'),
    newCopyBackup,
    CopyBackupResponse (CopyBackupResponse'),
    newCopyBackupResponse,

    -- ** CreateBackup
    CreateBackup (CreateBackup'),
    newCreateBackup,
    CreateBackupResponse (CreateBackupResponse'),
    newCreateBackupResponse,

    -- ** CreateDataRepositoryAssociation
    CreateDataRepositoryAssociation (CreateDataRepositoryAssociation'),
    newCreateDataRepositoryAssociation,
    CreateDataRepositoryAssociationResponse (CreateDataRepositoryAssociationResponse'),
    newCreateDataRepositoryAssociationResponse,

    -- ** CreateDataRepositoryTask
    CreateDataRepositoryTask (CreateDataRepositoryTask'),
    newCreateDataRepositoryTask,
    CreateDataRepositoryTaskResponse (CreateDataRepositoryTaskResponse'),
    newCreateDataRepositoryTaskResponse,

    -- ** CreateFileCache
    CreateFileCache (CreateFileCache'),
    newCreateFileCache,
    CreateFileCacheResponse (CreateFileCacheResponse'),
    newCreateFileCacheResponse,

    -- ** CreateFileSystem
    CreateFileSystem (CreateFileSystem'),
    newCreateFileSystem,
    CreateFileSystemResponse (CreateFileSystemResponse'),
    newCreateFileSystemResponse,

    -- ** CreateFileSystemFromBackup
    CreateFileSystemFromBackup (CreateFileSystemFromBackup'),
    newCreateFileSystemFromBackup,
    CreateFileSystemFromBackupResponse (CreateFileSystemFromBackupResponse'),
    newCreateFileSystemFromBackupResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** CreateStorageVirtualMachine
    CreateStorageVirtualMachine (CreateStorageVirtualMachine'),
    newCreateStorageVirtualMachine,
    CreateStorageVirtualMachineResponse (CreateStorageVirtualMachineResponse'),
    newCreateStorageVirtualMachineResponse,

    -- ** CreateVolume
    CreateVolume (CreateVolume'),
    newCreateVolume,
    CreateVolumeResponse (CreateVolumeResponse'),
    newCreateVolumeResponse,

    -- ** CreateVolumeFromBackup
    CreateVolumeFromBackup (CreateVolumeFromBackup'),
    newCreateVolumeFromBackup,
    CreateVolumeFromBackupResponse (CreateVolumeFromBackupResponse'),
    newCreateVolumeFromBackupResponse,

    -- ** DeleteBackup
    DeleteBackup (DeleteBackup'),
    newDeleteBackup,
    DeleteBackupResponse (DeleteBackupResponse'),
    newDeleteBackupResponse,

    -- ** DeleteDataRepositoryAssociation
    DeleteDataRepositoryAssociation (DeleteDataRepositoryAssociation'),
    newDeleteDataRepositoryAssociation,
    DeleteDataRepositoryAssociationResponse (DeleteDataRepositoryAssociationResponse'),
    newDeleteDataRepositoryAssociationResponse,

    -- ** DeleteFileCache
    DeleteFileCache (DeleteFileCache'),
    newDeleteFileCache,
    DeleteFileCacheResponse (DeleteFileCacheResponse'),
    newDeleteFileCacheResponse,

    -- ** DeleteFileSystem
    DeleteFileSystem (DeleteFileSystem'),
    newDeleteFileSystem,
    DeleteFileSystemResponse (DeleteFileSystemResponse'),
    newDeleteFileSystemResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** DeleteStorageVirtualMachine
    DeleteStorageVirtualMachine (DeleteStorageVirtualMachine'),
    newDeleteStorageVirtualMachine,
    DeleteStorageVirtualMachineResponse (DeleteStorageVirtualMachineResponse'),
    newDeleteStorageVirtualMachineResponse,

    -- ** DeleteVolume
    DeleteVolume (DeleteVolume'),
    newDeleteVolume,
    DeleteVolumeResponse (DeleteVolumeResponse'),
    newDeleteVolumeResponse,

    -- ** DescribeBackups (Paginated)
    DescribeBackups (DescribeBackups'),
    newDescribeBackups,
    DescribeBackupsResponse (DescribeBackupsResponse'),
    newDescribeBackupsResponse,

    -- ** DescribeDataRepositoryAssociations
    DescribeDataRepositoryAssociations (DescribeDataRepositoryAssociations'),
    newDescribeDataRepositoryAssociations,
    DescribeDataRepositoryAssociationsResponse (DescribeDataRepositoryAssociationsResponse'),
    newDescribeDataRepositoryAssociationsResponse,

    -- ** DescribeDataRepositoryTasks
    DescribeDataRepositoryTasks (DescribeDataRepositoryTasks'),
    newDescribeDataRepositoryTasks,
    DescribeDataRepositoryTasksResponse (DescribeDataRepositoryTasksResponse'),
    newDescribeDataRepositoryTasksResponse,

    -- ** DescribeFileCaches
    DescribeFileCaches (DescribeFileCaches'),
    newDescribeFileCaches,
    DescribeFileCachesResponse (DescribeFileCachesResponse'),
    newDescribeFileCachesResponse,

    -- ** DescribeFileSystemAliases
    DescribeFileSystemAliases (DescribeFileSystemAliases'),
    newDescribeFileSystemAliases,
    DescribeFileSystemAliasesResponse (DescribeFileSystemAliasesResponse'),
    newDescribeFileSystemAliasesResponse,

    -- ** DescribeFileSystems (Paginated)
    DescribeFileSystems (DescribeFileSystems'),
    newDescribeFileSystems,
    DescribeFileSystemsResponse (DescribeFileSystemsResponse'),
    newDescribeFileSystemsResponse,

    -- ** DescribeSnapshots
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** DescribeStorageVirtualMachines (Paginated)
    DescribeStorageVirtualMachines (DescribeStorageVirtualMachines'),
    newDescribeStorageVirtualMachines,
    DescribeStorageVirtualMachinesResponse (DescribeStorageVirtualMachinesResponse'),
    newDescribeStorageVirtualMachinesResponse,

    -- ** DescribeVolumes (Paginated)
    DescribeVolumes (DescribeVolumes'),
    newDescribeVolumes,
    DescribeVolumesResponse (DescribeVolumesResponse'),
    newDescribeVolumesResponse,

    -- ** DisassociateFileSystemAliases
    DisassociateFileSystemAliases (DisassociateFileSystemAliases'),
    newDisassociateFileSystemAliases,
    DisassociateFileSystemAliasesResponse (DisassociateFileSystemAliasesResponse'),
    newDisassociateFileSystemAliasesResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ReleaseFileSystemNfsV3Locks
    ReleaseFileSystemNfsV3Locks (ReleaseFileSystemNfsV3Locks'),
    newReleaseFileSystemNfsV3Locks,
    ReleaseFileSystemNfsV3LocksResponse (ReleaseFileSystemNfsV3LocksResponse'),
    newReleaseFileSystemNfsV3LocksResponse,

    -- ** RestoreVolumeFromSnapshot
    RestoreVolumeFromSnapshot (RestoreVolumeFromSnapshot'),
    newRestoreVolumeFromSnapshot,
    RestoreVolumeFromSnapshotResponse (RestoreVolumeFromSnapshotResponse'),
    newRestoreVolumeFromSnapshotResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateDataRepositoryAssociation
    UpdateDataRepositoryAssociation (UpdateDataRepositoryAssociation'),
    newUpdateDataRepositoryAssociation,
    UpdateDataRepositoryAssociationResponse (UpdateDataRepositoryAssociationResponse'),
    newUpdateDataRepositoryAssociationResponse,

    -- ** UpdateFileCache
    UpdateFileCache (UpdateFileCache'),
    newUpdateFileCache,
    UpdateFileCacheResponse (UpdateFileCacheResponse'),
    newUpdateFileCacheResponse,

    -- ** UpdateFileSystem
    UpdateFileSystem (UpdateFileSystem'),
    newUpdateFileSystem,
    UpdateFileSystemResponse (UpdateFileSystemResponse'),
    newUpdateFileSystemResponse,

    -- ** UpdateSnapshot
    UpdateSnapshot (UpdateSnapshot'),
    newUpdateSnapshot,
    UpdateSnapshotResponse (UpdateSnapshotResponse'),
    newUpdateSnapshotResponse,

    -- ** UpdateStorageVirtualMachine
    UpdateStorageVirtualMachine (UpdateStorageVirtualMachine'),
    newUpdateStorageVirtualMachine,
    UpdateStorageVirtualMachineResponse (UpdateStorageVirtualMachineResponse'),
    newUpdateStorageVirtualMachineResponse,

    -- ** UpdateVolume
    UpdateVolume (UpdateVolume'),
    newUpdateVolume,
    UpdateVolumeResponse (UpdateVolumeResponse'),
    newUpdateVolumeResponse,

    -- * Types

    -- ** AdministrativeActionType
    AdministrativeActionType (..),

    -- ** AliasLifecycle
    AliasLifecycle (..),

    -- ** AutoImportPolicyType
    AutoImportPolicyType (..),

    -- ** BackupLifecycle
    BackupLifecycle (..),

    -- ** BackupType
    BackupType (..),

    -- ** DataCompressionType
    DataCompressionType (..),

    -- ** DataRepositoryLifecycle
    DataRepositoryLifecycle (..),

    -- ** DataRepositoryTaskFilterName
    DataRepositoryTaskFilterName (..),

    -- ** DataRepositoryTaskLifecycle
    DataRepositoryTaskLifecycle (..),

    -- ** DataRepositoryTaskType
    DataRepositoryTaskType (..),

    -- ** DeleteFileSystemOpenZFSOption
    DeleteFileSystemOpenZFSOption (..),

    -- ** DeleteOpenZFSVolumeOption
    DeleteOpenZFSVolumeOption (..),

    -- ** DiskIopsConfigurationMode
    DiskIopsConfigurationMode (..),

    -- ** DriveCacheType
    DriveCacheType (..),

    -- ** EventType
    EventType (..),

    -- ** FileCacheLifecycle
    FileCacheLifecycle (..),

    -- ** FileCacheLustreDeploymentType
    FileCacheLustreDeploymentType (..),

    -- ** FileCacheType
    FileCacheType (..),

    -- ** FileSystemLifecycle
    FileSystemLifecycle (..),

    -- ** FileSystemMaintenanceOperation
    FileSystemMaintenanceOperation (..),

    -- ** FileSystemType
    FileSystemType (..),

    -- ** FilterName
    FilterName (..),

    -- ** FlexCacheEndpointType
    FlexCacheEndpointType (..),

    -- ** InputOntapVolumeType
    InputOntapVolumeType (..),

    -- ** LustreAccessAuditLogLevel
    LustreAccessAuditLogLevel (..),

    -- ** LustreDeploymentType
    LustreDeploymentType (..),

    -- ** NfsVersion
    NfsVersion (..),

    -- ** OntapDeploymentType
    OntapDeploymentType (..),

    -- ** OntapVolumeType
    OntapVolumeType (..),

    -- ** OpenZFSCopyStrategy
    OpenZFSCopyStrategy (..),

    -- ** OpenZFSDataCompressionType
    OpenZFSDataCompressionType (..),

    -- ** OpenZFSDeploymentType
    OpenZFSDeploymentType (..),

    -- ** OpenZFSQuotaType
    OpenZFSQuotaType (..),

    -- ** ReportFormat
    ReportFormat (..),

    -- ** ReportScope
    ReportScope (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** RestoreOpenZFSVolumeOption
    RestoreOpenZFSVolumeOption (..),

    -- ** SecurityStyle
    SecurityStyle (..),

    -- ** SnapshotFilterName
    SnapshotFilterName (..),

    -- ** SnapshotLifecycle
    SnapshotLifecycle (..),

    -- ** Status
    Status (..),

    -- ** StorageType
    StorageType (..),

    -- ** StorageVirtualMachineFilterName
    StorageVirtualMachineFilterName (..),

    -- ** StorageVirtualMachineLifecycle
    StorageVirtualMachineLifecycle (..),

    -- ** StorageVirtualMachineRootVolumeSecurityStyle
    StorageVirtualMachineRootVolumeSecurityStyle (..),

    -- ** TieringPolicyName
    TieringPolicyName (..),

    -- ** VolumeFilterName
    VolumeFilterName (..),

    -- ** VolumeLifecycle
    VolumeLifecycle (..),

    -- ** VolumeType
    VolumeType (..),

    -- ** WindowsAccessAuditLogLevel
    WindowsAccessAuditLogLevel (..),

    -- ** WindowsDeploymentType
    WindowsDeploymentType (..),

    -- ** ActiveDirectoryBackupAttributes
    ActiveDirectoryBackupAttributes (ActiveDirectoryBackupAttributes'),
    newActiveDirectoryBackupAttributes,

    -- ** AdministrativeAction
    AdministrativeAction (AdministrativeAction'),
    newAdministrativeAction,

    -- ** AdministrativeActionFailureDetails
    AdministrativeActionFailureDetails (AdministrativeActionFailureDetails'),
    newAdministrativeActionFailureDetails,

    -- ** Alias
    Alias (Alias'),
    newAlias,

    -- ** AutoExportPolicy
    AutoExportPolicy (AutoExportPolicy'),
    newAutoExportPolicy,

    -- ** AutoImportPolicy
    AutoImportPolicy (AutoImportPolicy'),
    newAutoImportPolicy,

    -- ** Backup
    Backup (Backup'),
    newBackup,

    -- ** BackupFailureDetails
    BackupFailureDetails (BackupFailureDetails'),
    newBackupFailureDetails,

    -- ** CompletionReport
    CompletionReport (CompletionReport'),
    newCompletionReport,

    -- ** CreateFileCacheLustreConfiguration
    CreateFileCacheLustreConfiguration (CreateFileCacheLustreConfiguration'),
    newCreateFileCacheLustreConfiguration,

    -- ** CreateFileSystemLustreConfiguration
    CreateFileSystemLustreConfiguration (CreateFileSystemLustreConfiguration'),
    newCreateFileSystemLustreConfiguration,

    -- ** CreateFileSystemOntapConfiguration
    CreateFileSystemOntapConfiguration (CreateFileSystemOntapConfiguration'),
    newCreateFileSystemOntapConfiguration,

    -- ** CreateFileSystemOpenZFSConfiguration
    CreateFileSystemOpenZFSConfiguration (CreateFileSystemOpenZFSConfiguration'),
    newCreateFileSystemOpenZFSConfiguration,

    -- ** CreateFileSystemWindowsConfiguration
    CreateFileSystemWindowsConfiguration (CreateFileSystemWindowsConfiguration'),
    newCreateFileSystemWindowsConfiguration,

    -- ** CreateOntapVolumeConfiguration
    CreateOntapVolumeConfiguration (CreateOntapVolumeConfiguration'),
    newCreateOntapVolumeConfiguration,

    -- ** CreateOpenZFSOriginSnapshotConfiguration
    CreateOpenZFSOriginSnapshotConfiguration (CreateOpenZFSOriginSnapshotConfiguration'),
    newCreateOpenZFSOriginSnapshotConfiguration,

    -- ** CreateOpenZFSVolumeConfiguration
    CreateOpenZFSVolumeConfiguration (CreateOpenZFSVolumeConfiguration'),
    newCreateOpenZFSVolumeConfiguration,

    -- ** CreateSvmActiveDirectoryConfiguration
    CreateSvmActiveDirectoryConfiguration (CreateSvmActiveDirectoryConfiguration'),
    newCreateSvmActiveDirectoryConfiguration,

    -- ** DataRepositoryAssociation
    DataRepositoryAssociation (DataRepositoryAssociation'),
    newDataRepositoryAssociation,

    -- ** DataRepositoryConfiguration
    DataRepositoryConfiguration (DataRepositoryConfiguration'),
    newDataRepositoryConfiguration,

    -- ** DataRepositoryFailureDetails
    DataRepositoryFailureDetails (DataRepositoryFailureDetails'),
    newDataRepositoryFailureDetails,

    -- ** DataRepositoryTask
    DataRepositoryTask (DataRepositoryTask'),
    newDataRepositoryTask,

    -- ** DataRepositoryTaskFailureDetails
    DataRepositoryTaskFailureDetails (DataRepositoryTaskFailureDetails'),
    newDataRepositoryTaskFailureDetails,

    -- ** DataRepositoryTaskFilter
    DataRepositoryTaskFilter (DataRepositoryTaskFilter'),
    newDataRepositoryTaskFilter,

    -- ** DataRepositoryTaskStatus
    DataRepositoryTaskStatus (DataRepositoryTaskStatus'),
    newDataRepositoryTaskStatus,

    -- ** DeleteFileSystemLustreConfiguration
    DeleteFileSystemLustreConfiguration (DeleteFileSystemLustreConfiguration'),
    newDeleteFileSystemLustreConfiguration,

    -- ** DeleteFileSystemLustreResponse
    DeleteFileSystemLustreResponse (DeleteFileSystemLustreResponse'),
    newDeleteFileSystemLustreResponse,

    -- ** DeleteFileSystemOpenZFSConfiguration
    DeleteFileSystemOpenZFSConfiguration (DeleteFileSystemOpenZFSConfiguration'),
    newDeleteFileSystemOpenZFSConfiguration,

    -- ** DeleteFileSystemOpenZFSResponse
    DeleteFileSystemOpenZFSResponse (DeleteFileSystemOpenZFSResponse'),
    newDeleteFileSystemOpenZFSResponse,

    -- ** DeleteFileSystemWindowsConfiguration
    DeleteFileSystemWindowsConfiguration (DeleteFileSystemWindowsConfiguration'),
    newDeleteFileSystemWindowsConfiguration,

    -- ** DeleteFileSystemWindowsResponse
    DeleteFileSystemWindowsResponse (DeleteFileSystemWindowsResponse'),
    newDeleteFileSystemWindowsResponse,

    -- ** DeleteVolumeOntapConfiguration
    DeleteVolumeOntapConfiguration (DeleteVolumeOntapConfiguration'),
    newDeleteVolumeOntapConfiguration,

    -- ** DeleteVolumeOntapResponse
    DeleteVolumeOntapResponse (DeleteVolumeOntapResponse'),
    newDeleteVolumeOntapResponse,

    -- ** DeleteVolumeOpenZFSConfiguration
    DeleteVolumeOpenZFSConfiguration (DeleteVolumeOpenZFSConfiguration'),
    newDeleteVolumeOpenZFSConfiguration,

    -- ** DiskIopsConfiguration
    DiskIopsConfiguration (DiskIopsConfiguration'),
    newDiskIopsConfiguration,

    -- ** FileCache
    FileCache (FileCache'),
    newFileCache,

    -- ** FileCacheCreating
    FileCacheCreating (FileCacheCreating'),
    newFileCacheCreating,

    -- ** FileCacheDataRepositoryAssociation
    FileCacheDataRepositoryAssociation (FileCacheDataRepositoryAssociation'),
    newFileCacheDataRepositoryAssociation,

    -- ** FileCacheFailureDetails
    FileCacheFailureDetails (FileCacheFailureDetails'),
    newFileCacheFailureDetails,

    -- ** FileCacheLustreConfiguration
    FileCacheLustreConfiguration (FileCacheLustreConfiguration'),
    newFileCacheLustreConfiguration,

    -- ** FileCacheLustreMetadataConfiguration
    FileCacheLustreMetadataConfiguration (FileCacheLustreMetadataConfiguration'),
    newFileCacheLustreMetadataConfiguration,

    -- ** FileCacheNFSConfiguration
    FileCacheNFSConfiguration (FileCacheNFSConfiguration'),
    newFileCacheNFSConfiguration,

    -- ** FileSystem
    FileSystem (FileSystem'),
    newFileSystem,

    -- ** FileSystemEndpoint
    FileSystemEndpoint (FileSystemEndpoint'),
    newFileSystemEndpoint,

    -- ** FileSystemEndpoints
    FileSystemEndpoints (FileSystemEndpoints'),
    newFileSystemEndpoints,

    -- ** FileSystemFailureDetails
    FileSystemFailureDetails (FileSystemFailureDetails'),
    newFileSystemFailureDetails,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** LifecycleTransitionReason
    LifecycleTransitionReason (LifecycleTransitionReason'),
    newLifecycleTransitionReason,

    -- ** LustreFileSystemConfiguration
    LustreFileSystemConfiguration (LustreFileSystemConfiguration'),
    newLustreFileSystemConfiguration,

    -- ** LustreLogConfiguration
    LustreLogConfiguration (LustreLogConfiguration'),
    newLustreLogConfiguration,

    -- ** LustreLogCreateConfiguration
    LustreLogCreateConfiguration (LustreLogCreateConfiguration'),
    newLustreLogCreateConfiguration,

    -- ** LustreRootSquashConfiguration
    LustreRootSquashConfiguration (LustreRootSquashConfiguration'),
    newLustreRootSquashConfiguration,

    -- ** NFSDataRepositoryConfiguration
    NFSDataRepositoryConfiguration (NFSDataRepositoryConfiguration'),
    newNFSDataRepositoryConfiguration,

    -- ** OntapFileSystemConfiguration
    OntapFileSystemConfiguration (OntapFileSystemConfiguration'),
    newOntapFileSystemConfiguration,

    -- ** OntapVolumeConfiguration
    OntapVolumeConfiguration (OntapVolumeConfiguration'),
    newOntapVolumeConfiguration,

    -- ** OpenZFSClientConfiguration
    OpenZFSClientConfiguration (OpenZFSClientConfiguration'),
    newOpenZFSClientConfiguration,

    -- ** OpenZFSCreateRootVolumeConfiguration
    OpenZFSCreateRootVolumeConfiguration (OpenZFSCreateRootVolumeConfiguration'),
    newOpenZFSCreateRootVolumeConfiguration,

    -- ** OpenZFSFileSystemConfiguration
    OpenZFSFileSystemConfiguration (OpenZFSFileSystemConfiguration'),
    newOpenZFSFileSystemConfiguration,

    -- ** OpenZFSNfsExport
    OpenZFSNfsExport (OpenZFSNfsExport'),
    newOpenZFSNfsExport,

    -- ** OpenZFSOriginSnapshotConfiguration
    OpenZFSOriginSnapshotConfiguration (OpenZFSOriginSnapshotConfiguration'),
    newOpenZFSOriginSnapshotConfiguration,

    -- ** OpenZFSUserOrGroupQuota
    OpenZFSUserOrGroupQuota (OpenZFSUserOrGroupQuota'),
    newOpenZFSUserOrGroupQuota,

    -- ** OpenZFSVolumeConfiguration
    OpenZFSVolumeConfiguration (OpenZFSVolumeConfiguration'),
    newOpenZFSVolumeConfiguration,

    -- ** S3DataRepositoryConfiguration
    S3DataRepositoryConfiguration (S3DataRepositoryConfiguration'),
    newS3DataRepositoryConfiguration,

    -- ** SelfManagedActiveDirectoryAttributes
    SelfManagedActiveDirectoryAttributes (SelfManagedActiveDirectoryAttributes'),
    newSelfManagedActiveDirectoryAttributes,

    -- ** SelfManagedActiveDirectoryConfiguration
    SelfManagedActiveDirectoryConfiguration (SelfManagedActiveDirectoryConfiguration'),
    newSelfManagedActiveDirectoryConfiguration,

    -- ** SelfManagedActiveDirectoryConfigurationUpdates
    SelfManagedActiveDirectoryConfigurationUpdates (SelfManagedActiveDirectoryConfigurationUpdates'),
    newSelfManagedActiveDirectoryConfigurationUpdates,

    -- ** Snapshot
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** SnapshotFilter
    SnapshotFilter (SnapshotFilter'),
    newSnapshotFilter,

    -- ** StorageVirtualMachine
    StorageVirtualMachine (StorageVirtualMachine'),
    newStorageVirtualMachine,

    -- ** StorageVirtualMachineFilter
    StorageVirtualMachineFilter (StorageVirtualMachineFilter'),
    newStorageVirtualMachineFilter,

    -- ** SvmActiveDirectoryConfiguration
    SvmActiveDirectoryConfiguration (SvmActiveDirectoryConfiguration'),
    newSvmActiveDirectoryConfiguration,

    -- ** SvmEndpoint
    SvmEndpoint (SvmEndpoint'),
    newSvmEndpoint,

    -- ** SvmEndpoints
    SvmEndpoints (SvmEndpoints'),
    newSvmEndpoints,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TieringPolicy
    TieringPolicy (TieringPolicy'),
    newTieringPolicy,

    -- ** UpdateFileCacheLustreConfiguration
    UpdateFileCacheLustreConfiguration (UpdateFileCacheLustreConfiguration'),
    newUpdateFileCacheLustreConfiguration,

    -- ** UpdateFileSystemLustreConfiguration
    UpdateFileSystemLustreConfiguration (UpdateFileSystemLustreConfiguration'),
    newUpdateFileSystemLustreConfiguration,

    -- ** UpdateFileSystemOntapConfiguration
    UpdateFileSystemOntapConfiguration (UpdateFileSystemOntapConfiguration'),
    newUpdateFileSystemOntapConfiguration,

    -- ** UpdateFileSystemOpenZFSConfiguration
    UpdateFileSystemOpenZFSConfiguration (UpdateFileSystemOpenZFSConfiguration'),
    newUpdateFileSystemOpenZFSConfiguration,

    -- ** UpdateFileSystemWindowsConfiguration
    UpdateFileSystemWindowsConfiguration (UpdateFileSystemWindowsConfiguration'),
    newUpdateFileSystemWindowsConfiguration,

    -- ** UpdateOntapVolumeConfiguration
    UpdateOntapVolumeConfiguration (UpdateOntapVolumeConfiguration'),
    newUpdateOntapVolumeConfiguration,

    -- ** UpdateOpenZFSVolumeConfiguration
    UpdateOpenZFSVolumeConfiguration (UpdateOpenZFSVolumeConfiguration'),
    newUpdateOpenZFSVolumeConfiguration,

    -- ** UpdateSvmActiveDirectoryConfiguration
    UpdateSvmActiveDirectoryConfiguration (UpdateSvmActiveDirectoryConfiguration'),
    newUpdateSvmActiveDirectoryConfiguration,

    -- ** Volume
    Volume (Volume'),
    newVolume,

    -- ** VolumeFilter
    VolumeFilter (VolumeFilter'),
    newVolumeFilter,

    -- ** WindowsAuditLogConfiguration
    WindowsAuditLogConfiguration (WindowsAuditLogConfiguration'),
    newWindowsAuditLogConfiguration,

    -- ** WindowsAuditLogCreateConfiguration
    WindowsAuditLogCreateConfiguration (WindowsAuditLogCreateConfiguration'),
    newWindowsAuditLogCreateConfiguration,

    -- ** WindowsFileSystemConfiguration
    WindowsFileSystemConfiguration (WindowsFileSystemConfiguration'),
    newWindowsFileSystemConfiguration,
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
import Amazonka.FSx.Lens
import Amazonka.FSx.ListTagsForResource
import Amazonka.FSx.ReleaseFileSystemNfsV3Locks
import Amazonka.FSx.RestoreVolumeFromSnapshot
import Amazonka.FSx.TagResource
import Amazonka.FSx.Types
import Amazonka.FSx.UntagResource
import Amazonka.FSx.UpdateDataRepositoryAssociation
import Amazonka.FSx.UpdateFileCache
import Amazonka.FSx.UpdateFileSystem
import Amazonka.FSx.UpdateSnapshot
import Amazonka.FSx.UpdateStorageVirtualMachine
import Amazonka.FSx.UpdateVolume
import Amazonka.FSx.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'FSx'.

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

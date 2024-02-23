{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.M2.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * ApplicationDeploymentLifecycle
    ApplicationDeploymentLifecycle (..),

    -- * ApplicationLifecycle
    ApplicationLifecycle (..),

    -- * ApplicationVersionLifecycle
    ApplicationVersionLifecycle (..),

    -- * BatchJobExecutionStatus
    BatchJobExecutionStatus (..),

    -- * BatchJobType
    BatchJobType (..),

    -- * DataSetTaskLifecycle
    DataSetTaskLifecycle (..),

    -- * DeploymentLifecycle
    DeploymentLifecycle (..),

    -- * EngineType
    EngineType (..),

    -- * EnvironmentLifecycle
    EnvironmentLifecycle (..),

    -- * AlternateKey
    AlternateKey (..),
    newAlternateKey,
    alternateKey_allowDuplicates,
    alternateKey_name,
    alternateKey_length,
    alternateKey_offset,

    -- * ApplicationSummary
    ApplicationSummary (..),
    newApplicationSummary,
    applicationSummary_deploymentStatus,
    applicationSummary_description,
    applicationSummary_environmentId,
    applicationSummary_lastStartTime,
    applicationSummary_versionStatus,
    applicationSummary_applicationArn,
    applicationSummary_applicationId,
    applicationSummary_applicationVersion,
    applicationSummary_creationTime,
    applicationSummary_engineType,
    applicationSummary_name,
    applicationSummary_status,

    -- * ApplicationVersionSummary
    ApplicationVersionSummary (..),
    newApplicationVersionSummary,
    applicationVersionSummary_statusReason,
    applicationVersionSummary_applicationVersion,
    applicationVersionSummary_creationTime,
    applicationVersionSummary_status,

    -- * BatchJobDefinition
    BatchJobDefinition (..),
    newBatchJobDefinition,
    batchJobDefinition_fileBatchJobDefinition,
    batchJobDefinition_scriptBatchJobDefinition,

    -- * BatchJobExecutionSummary
    BatchJobExecutionSummary (..),
    newBatchJobExecutionSummary,
    batchJobExecutionSummary_endTime,
    batchJobExecutionSummary_jobId,
    batchJobExecutionSummary_jobName,
    batchJobExecutionSummary_jobType,
    batchJobExecutionSummary_applicationId,
    batchJobExecutionSummary_executionId,
    batchJobExecutionSummary_startTime,
    batchJobExecutionSummary_status,

    -- * BatchJobIdentifier
    BatchJobIdentifier (..),
    newBatchJobIdentifier,
    batchJobIdentifier_fileBatchJobIdentifier,
    batchJobIdentifier_scriptBatchJobIdentifier,

    -- * DataSet
    DataSet (..),
    newDataSet,
    dataSet_relativePath,
    dataSet_storageType,
    dataSet_datasetName,
    dataSet_datasetOrg,
    dataSet_recordLength,

    -- * DataSetImportConfig
    DataSetImportConfig (..),
    newDataSetImportConfig,
    dataSetImportConfig_dataSets,
    dataSetImportConfig_s3Location,

    -- * DataSetImportItem
    DataSetImportItem (..),
    newDataSetImportItem,
    dataSetImportItem_dataSet,
    dataSetImportItem_externalLocation,

    -- * DataSetImportSummary
    DataSetImportSummary (..),
    newDataSetImportSummary,
    dataSetImportSummary_failed,
    dataSetImportSummary_inProgress,
    dataSetImportSummary_pending,
    dataSetImportSummary_succeeded,
    dataSetImportSummary_total,

    -- * DataSetImportTask
    DataSetImportTask (..),
    newDataSetImportTask,
    dataSetImportTask_status,
    dataSetImportTask_summary,
    dataSetImportTask_taskId,

    -- * DataSetSummary
    DataSetSummary (..),
    newDataSetSummary,
    dataSetSummary_creationTime,
    dataSetSummary_dataSetOrg,
    dataSetSummary_format,
    dataSetSummary_lastReferencedTime,
    dataSetSummary_lastUpdatedTime,
    dataSetSummary_dataSetName,

    -- * DatasetDetailOrgAttributes
    DatasetDetailOrgAttributes (..),
    newDatasetDetailOrgAttributes,
    datasetDetailOrgAttributes_gdg,
    datasetDetailOrgAttributes_vsam,

    -- * DatasetOrgAttributes
    DatasetOrgAttributes (..),
    newDatasetOrgAttributes,
    datasetOrgAttributes_gdg,
    datasetOrgAttributes_vsam,

    -- * Definition
    Definition (..),
    newDefinition,
    definition_content,
    definition_s3Location,

    -- * DeployedVersionSummary
    DeployedVersionSummary (..),
    newDeployedVersionSummary,
    deployedVersionSummary_statusReason,
    deployedVersionSummary_applicationVersion,
    deployedVersionSummary_status,

    -- * DeploymentSummary
    DeploymentSummary (..),
    newDeploymentSummary,
    deploymentSummary_statusReason,
    deploymentSummary_applicationId,
    deploymentSummary_applicationVersion,
    deploymentSummary_creationTime,
    deploymentSummary_deploymentId,
    deploymentSummary_environmentId,
    deploymentSummary_status,

    -- * EfsStorageConfiguration
    EfsStorageConfiguration (..),
    newEfsStorageConfiguration,
    efsStorageConfiguration_fileSystemId,
    efsStorageConfiguration_mountPoint,

    -- * EngineVersionsSummary
    EngineVersionsSummary (..),
    newEngineVersionsSummary,
    engineVersionsSummary_engineType,
    engineVersionsSummary_engineVersion,

    -- * EnvironmentSummary
    EnvironmentSummary (..),
    newEnvironmentSummary,
    environmentSummary_creationTime,
    environmentSummary_engineType,
    environmentSummary_engineVersion,
    environmentSummary_environmentArn,
    environmentSummary_environmentId,
    environmentSummary_instanceType,
    environmentSummary_name,
    environmentSummary_status,

    -- * ExternalLocation
    ExternalLocation (..),
    newExternalLocation,
    externalLocation_s3Location,

    -- * FileBatchJobDefinition
    FileBatchJobDefinition (..),
    newFileBatchJobDefinition,
    fileBatchJobDefinition_folderPath,
    fileBatchJobDefinition_fileName,

    -- * FileBatchJobIdentifier
    FileBatchJobIdentifier (..),
    newFileBatchJobIdentifier,
    fileBatchJobIdentifier_folderPath,
    fileBatchJobIdentifier_fileName,

    -- * FsxStorageConfiguration
    FsxStorageConfiguration (..),
    newFsxStorageConfiguration,
    fsxStorageConfiguration_fileSystemId,
    fsxStorageConfiguration_mountPoint,

    -- * GdgAttributes
    GdgAttributes (..),
    newGdgAttributes,
    gdgAttributes_limit,
    gdgAttributes_rollDisposition,

    -- * GdgDetailAttributes
    GdgDetailAttributes (..),
    newGdgDetailAttributes,
    gdgDetailAttributes_limit,
    gdgDetailAttributes_rollDisposition,

    -- * HighAvailabilityConfig
    HighAvailabilityConfig (..),
    newHighAvailabilityConfig,
    highAvailabilityConfig_desiredCapacity,

    -- * LogGroupSummary
    LogGroupSummary (..),
    newLogGroupSummary,
    logGroupSummary_logGroupName,
    logGroupSummary_logType,

    -- * MaintenanceSchedule
    MaintenanceSchedule (..),
    newMaintenanceSchedule,
    maintenanceSchedule_endTime,
    maintenanceSchedule_startTime,

    -- * PendingMaintenance
    PendingMaintenance (..),
    newPendingMaintenance,
    pendingMaintenance_engineVersion,
    pendingMaintenance_schedule,

    -- * PrimaryKey
    PrimaryKey (..),
    newPrimaryKey,
    primaryKey_name,
    primaryKey_length,
    primaryKey_offset,

    -- * RecordLength
    RecordLength (..),
    newRecordLength,
    recordLength_max,
    recordLength_min,

    -- * ScriptBatchJobDefinition
    ScriptBatchJobDefinition (..),
    newScriptBatchJobDefinition,
    scriptBatchJobDefinition_scriptName,

    -- * ScriptBatchJobIdentifier
    ScriptBatchJobIdentifier (..),
    newScriptBatchJobIdentifier,
    scriptBatchJobIdentifier_scriptName,

    -- * StorageConfiguration
    StorageConfiguration (..),
    newStorageConfiguration,
    storageConfiguration_efs,
    storageConfiguration_fsx,

    -- * VsamAttributes
    VsamAttributes (..),
    newVsamAttributes,
    vsamAttributes_alternateKeys,
    vsamAttributes_compressed,
    vsamAttributes_encoding,
    vsamAttributes_primaryKey,
    vsamAttributes_format,

    -- * VsamDetailAttributes
    VsamDetailAttributes (..),
    newVsamDetailAttributes,
    vsamDetailAttributes_alternateKeys,
    vsamDetailAttributes_cacheAtStartup,
    vsamDetailAttributes_compressed,
    vsamDetailAttributes_encoding,
    vsamDetailAttributes_primaryKey,
    vsamDetailAttributes_recordFormat,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.M2.Types.AlternateKey
import Amazonka.M2.Types.ApplicationDeploymentLifecycle
import Amazonka.M2.Types.ApplicationLifecycle
import Amazonka.M2.Types.ApplicationSummary
import Amazonka.M2.Types.ApplicationVersionLifecycle
import Amazonka.M2.Types.ApplicationVersionSummary
import Amazonka.M2.Types.BatchJobDefinition
import Amazonka.M2.Types.BatchJobExecutionStatus
import Amazonka.M2.Types.BatchJobExecutionSummary
import Amazonka.M2.Types.BatchJobIdentifier
import Amazonka.M2.Types.BatchJobType
import Amazonka.M2.Types.DataSet
import Amazonka.M2.Types.DataSetImportConfig
import Amazonka.M2.Types.DataSetImportItem
import Amazonka.M2.Types.DataSetImportSummary
import Amazonka.M2.Types.DataSetImportTask
import Amazonka.M2.Types.DataSetSummary
import Amazonka.M2.Types.DataSetTaskLifecycle
import Amazonka.M2.Types.DatasetDetailOrgAttributes
import Amazonka.M2.Types.DatasetOrgAttributes
import Amazonka.M2.Types.Definition
import Amazonka.M2.Types.DeployedVersionSummary
import Amazonka.M2.Types.DeploymentLifecycle
import Amazonka.M2.Types.DeploymentSummary
import Amazonka.M2.Types.EfsStorageConfiguration
import Amazonka.M2.Types.EngineType
import Amazonka.M2.Types.EngineVersionsSummary
import Amazonka.M2.Types.EnvironmentLifecycle
import Amazonka.M2.Types.EnvironmentSummary
import Amazonka.M2.Types.ExternalLocation
import Amazonka.M2.Types.FileBatchJobDefinition
import Amazonka.M2.Types.FileBatchJobIdentifier
import Amazonka.M2.Types.FsxStorageConfiguration
import Amazonka.M2.Types.GdgAttributes
import Amazonka.M2.Types.GdgDetailAttributes
import Amazonka.M2.Types.HighAvailabilityConfig
import Amazonka.M2.Types.LogGroupSummary
import Amazonka.M2.Types.MaintenanceSchedule
import Amazonka.M2.Types.PendingMaintenance
import Amazonka.M2.Types.PrimaryKey
import Amazonka.M2.Types.RecordLength
import Amazonka.M2.Types.ScriptBatchJobDefinition
import Amazonka.M2.Types.ScriptBatchJobIdentifier
import Amazonka.M2.Types.StorageConfiguration
import Amazonka.M2.Types.VsamAttributes
import Amazonka.M2.Types.VsamDetailAttributes
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-04-28@ of the Amazon MainframeModernization SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "M2",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "m2",
      Core.signingName = "m2",
      Core.version = "2021-04-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "M2",
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

-- | The account or role doesn\'t have the right permissions to make the
-- request.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The parameters provided in the request conflict with existing resources.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An unexpected error occurred during the processing of the request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource was not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | One or more quotas for Amazon Web Services Mainframe Modernization
-- exceeds the limit.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The number of requests made exceeds the limit.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | One or more parameters provided in the request is not valid.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.M2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-04-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Mainframe Modernization provides tools and resources
-- to help you plan and implement migration and modernization from
-- mainframes to Amazon Web Services managed runtime environments. It
-- provides tools for analyzing existing mainframe applications, developing
-- or updating mainframe applications using COBOL or PL\/I, and
-- implementing an automated pipeline for continuous integration and
-- continuous delivery (CI\/CD) of the applications.
module Amazonka.M2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelBatchJobExecution
    CancelBatchJobExecution (CancelBatchJobExecution'),
    newCancelBatchJobExecution,
    CancelBatchJobExecutionResponse (CancelBatchJobExecutionResponse'),
    newCancelBatchJobExecutionResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** CreateDataSetImportTask
    CreateDataSetImportTask (CreateDataSetImportTask'),
    newCreateDataSetImportTask,
    CreateDataSetImportTaskResponse (CreateDataSetImportTaskResponse'),
    newCreateDataSetImportTaskResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** CreateEnvironment
    CreateEnvironment (CreateEnvironment'),
    newCreateEnvironment,
    CreateEnvironmentResponse (CreateEnvironmentResponse'),
    newCreateEnvironmentResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** DeleteApplicationFromEnvironment
    DeleteApplicationFromEnvironment (DeleteApplicationFromEnvironment'),
    newDeleteApplicationFromEnvironment,
    DeleteApplicationFromEnvironmentResponse (DeleteApplicationFromEnvironmentResponse'),
    newDeleteApplicationFromEnvironmentResponse,

    -- ** DeleteEnvironment
    DeleteEnvironment (DeleteEnvironment'),
    newDeleteEnvironment,
    DeleteEnvironmentResponse (DeleteEnvironmentResponse'),
    newDeleteEnvironmentResponse,

    -- ** GetApplication
    GetApplication (GetApplication'),
    newGetApplication,
    GetApplicationResponse (GetApplicationResponse'),
    newGetApplicationResponse,

    -- ** GetApplicationVersion
    GetApplicationVersion (GetApplicationVersion'),
    newGetApplicationVersion,
    GetApplicationVersionResponse (GetApplicationVersionResponse'),
    newGetApplicationVersionResponse,

    -- ** GetBatchJobExecution
    GetBatchJobExecution (GetBatchJobExecution'),
    newGetBatchJobExecution,
    GetBatchJobExecutionResponse (GetBatchJobExecutionResponse'),
    newGetBatchJobExecutionResponse,

    -- ** GetDataSetDetails
    GetDataSetDetails (GetDataSetDetails'),
    newGetDataSetDetails,
    GetDataSetDetailsResponse (GetDataSetDetailsResponse'),
    newGetDataSetDetailsResponse,

    -- ** GetDataSetImportTask
    GetDataSetImportTask (GetDataSetImportTask'),
    newGetDataSetImportTask,
    GetDataSetImportTaskResponse (GetDataSetImportTaskResponse'),
    newGetDataSetImportTaskResponse,

    -- ** GetDeployment
    GetDeployment (GetDeployment'),
    newGetDeployment,
    GetDeploymentResponse (GetDeploymentResponse'),
    newGetDeploymentResponse,

    -- ** GetEnvironment
    GetEnvironment (GetEnvironment'),
    newGetEnvironment,
    GetEnvironmentResponse (GetEnvironmentResponse'),
    newGetEnvironmentResponse,

    -- ** ListApplicationVersions (Paginated)
    ListApplicationVersions (ListApplicationVersions'),
    newListApplicationVersions,
    ListApplicationVersionsResponse (ListApplicationVersionsResponse'),
    newListApplicationVersionsResponse,

    -- ** ListApplications (Paginated)
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** ListBatchJobDefinitions (Paginated)
    ListBatchJobDefinitions (ListBatchJobDefinitions'),
    newListBatchJobDefinitions,
    ListBatchJobDefinitionsResponse (ListBatchJobDefinitionsResponse'),
    newListBatchJobDefinitionsResponse,

    -- ** ListBatchJobExecutions (Paginated)
    ListBatchJobExecutions (ListBatchJobExecutions'),
    newListBatchJobExecutions,
    ListBatchJobExecutionsResponse (ListBatchJobExecutionsResponse'),
    newListBatchJobExecutionsResponse,

    -- ** ListDataSetImportHistory (Paginated)
    ListDataSetImportHistory (ListDataSetImportHistory'),
    newListDataSetImportHistory,
    ListDataSetImportHistoryResponse (ListDataSetImportHistoryResponse'),
    newListDataSetImportHistoryResponse,

    -- ** ListDataSets (Paginated)
    ListDataSets (ListDataSets'),
    newListDataSets,
    ListDataSetsResponse (ListDataSetsResponse'),
    newListDataSetsResponse,

    -- ** ListDeployments (Paginated)
    ListDeployments (ListDeployments'),
    newListDeployments,
    ListDeploymentsResponse (ListDeploymentsResponse'),
    newListDeploymentsResponse,

    -- ** ListEngineVersions (Paginated)
    ListEngineVersions (ListEngineVersions'),
    newListEngineVersions,
    ListEngineVersionsResponse (ListEngineVersionsResponse'),
    newListEngineVersionsResponse,

    -- ** ListEnvironments (Paginated)
    ListEnvironments (ListEnvironments'),
    newListEnvironments,
    ListEnvironmentsResponse (ListEnvironmentsResponse'),
    newListEnvironmentsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartApplication
    StartApplication (StartApplication'),
    newStartApplication,
    StartApplicationResponse (StartApplicationResponse'),
    newStartApplicationResponse,

    -- ** StartBatchJob
    StartBatchJob (StartBatchJob'),
    newStartBatchJob,
    StartBatchJobResponse (StartBatchJobResponse'),
    newStartBatchJobResponse,

    -- ** StopApplication
    StopApplication (StopApplication'),
    newStopApplication,
    StopApplicationResponse (StopApplicationResponse'),
    newStopApplicationResponse,

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

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    UpdateApplicationResponse (UpdateApplicationResponse'),
    newUpdateApplicationResponse,

    -- ** UpdateEnvironment
    UpdateEnvironment (UpdateEnvironment'),
    newUpdateEnvironment,
    UpdateEnvironmentResponse (UpdateEnvironmentResponse'),
    newUpdateEnvironmentResponse,

    -- * Types

    -- ** ApplicationDeploymentLifecycle
    ApplicationDeploymentLifecycle (..),

    -- ** ApplicationLifecycle
    ApplicationLifecycle (..),

    -- ** ApplicationVersionLifecycle
    ApplicationVersionLifecycle (..),

    -- ** BatchJobExecutionStatus
    BatchJobExecutionStatus (..),

    -- ** BatchJobType
    BatchJobType (..),

    -- ** DataSetTaskLifecycle
    DataSetTaskLifecycle (..),

    -- ** DeploymentLifecycle
    DeploymentLifecycle (..),

    -- ** EngineType
    EngineType (..),

    -- ** EnvironmentLifecycle
    EnvironmentLifecycle (..),

    -- ** AlternateKey
    AlternateKey (AlternateKey'),
    newAlternateKey,

    -- ** ApplicationSummary
    ApplicationSummary (ApplicationSummary'),
    newApplicationSummary,

    -- ** ApplicationVersionSummary
    ApplicationVersionSummary (ApplicationVersionSummary'),
    newApplicationVersionSummary,

    -- ** BatchJobDefinition
    BatchJobDefinition (BatchJobDefinition'),
    newBatchJobDefinition,

    -- ** BatchJobExecutionSummary
    BatchJobExecutionSummary (BatchJobExecutionSummary'),
    newBatchJobExecutionSummary,

    -- ** BatchJobIdentifier
    BatchJobIdentifier (BatchJobIdentifier'),
    newBatchJobIdentifier,

    -- ** DataSet
    DataSet (DataSet'),
    newDataSet,

    -- ** DataSetImportConfig
    DataSetImportConfig (DataSetImportConfig'),
    newDataSetImportConfig,

    -- ** DataSetImportItem
    DataSetImportItem (DataSetImportItem'),
    newDataSetImportItem,

    -- ** DataSetImportSummary
    DataSetImportSummary (DataSetImportSummary'),
    newDataSetImportSummary,

    -- ** DataSetImportTask
    DataSetImportTask (DataSetImportTask'),
    newDataSetImportTask,

    -- ** DataSetSummary
    DataSetSummary (DataSetSummary'),
    newDataSetSummary,

    -- ** DatasetDetailOrgAttributes
    DatasetDetailOrgAttributes (DatasetDetailOrgAttributes'),
    newDatasetDetailOrgAttributes,

    -- ** DatasetOrgAttributes
    DatasetOrgAttributes (DatasetOrgAttributes'),
    newDatasetOrgAttributes,

    -- ** Definition
    Definition (Definition'),
    newDefinition,

    -- ** DeployedVersionSummary
    DeployedVersionSummary (DeployedVersionSummary'),
    newDeployedVersionSummary,

    -- ** DeploymentSummary
    DeploymentSummary (DeploymentSummary'),
    newDeploymentSummary,

    -- ** EfsStorageConfiguration
    EfsStorageConfiguration (EfsStorageConfiguration'),
    newEfsStorageConfiguration,

    -- ** EngineVersionsSummary
    EngineVersionsSummary (EngineVersionsSummary'),
    newEngineVersionsSummary,

    -- ** EnvironmentSummary
    EnvironmentSummary (EnvironmentSummary'),
    newEnvironmentSummary,

    -- ** ExternalLocation
    ExternalLocation (ExternalLocation'),
    newExternalLocation,

    -- ** FileBatchJobDefinition
    FileBatchJobDefinition (FileBatchJobDefinition'),
    newFileBatchJobDefinition,

    -- ** FileBatchJobIdentifier
    FileBatchJobIdentifier (FileBatchJobIdentifier'),
    newFileBatchJobIdentifier,

    -- ** FsxStorageConfiguration
    FsxStorageConfiguration (FsxStorageConfiguration'),
    newFsxStorageConfiguration,

    -- ** GdgAttributes
    GdgAttributes (GdgAttributes'),
    newGdgAttributes,

    -- ** GdgDetailAttributes
    GdgDetailAttributes (GdgDetailAttributes'),
    newGdgDetailAttributes,

    -- ** HighAvailabilityConfig
    HighAvailabilityConfig (HighAvailabilityConfig'),
    newHighAvailabilityConfig,

    -- ** LogGroupSummary
    LogGroupSummary (LogGroupSummary'),
    newLogGroupSummary,

    -- ** MaintenanceSchedule
    MaintenanceSchedule (MaintenanceSchedule'),
    newMaintenanceSchedule,

    -- ** PendingMaintenance
    PendingMaintenance (PendingMaintenance'),
    newPendingMaintenance,

    -- ** PoAttributes
    PoAttributes (PoAttributes'),
    newPoAttributes,

    -- ** PoDetailAttributes
    PoDetailAttributes (PoDetailAttributes'),
    newPoDetailAttributes,

    -- ** PrimaryKey
    PrimaryKey (PrimaryKey'),
    newPrimaryKey,

    -- ** PsAttributes
    PsAttributes (PsAttributes'),
    newPsAttributes,

    -- ** PsDetailAttributes
    PsDetailAttributes (PsDetailAttributes'),
    newPsDetailAttributes,

    -- ** RecordLength
    RecordLength (RecordLength'),
    newRecordLength,

    -- ** ScriptBatchJobDefinition
    ScriptBatchJobDefinition (ScriptBatchJobDefinition'),
    newScriptBatchJobDefinition,

    -- ** ScriptBatchJobIdentifier
    ScriptBatchJobIdentifier (ScriptBatchJobIdentifier'),
    newScriptBatchJobIdentifier,

    -- ** StorageConfiguration
    StorageConfiguration (StorageConfiguration'),
    newStorageConfiguration,

    -- ** VsamAttributes
    VsamAttributes (VsamAttributes'),
    newVsamAttributes,

    -- ** VsamDetailAttributes
    VsamDetailAttributes (VsamDetailAttributes'),
    newVsamDetailAttributes,
  )
where

import Amazonka.M2.CancelBatchJobExecution
import Amazonka.M2.CreateApplication
import Amazonka.M2.CreateDataSetImportTask
import Amazonka.M2.CreateDeployment
import Amazonka.M2.CreateEnvironment
import Amazonka.M2.DeleteApplication
import Amazonka.M2.DeleteApplicationFromEnvironment
import Amazonka.M2.DeleteEnvironment
import Amazonka.M2.GetApplication
import Amazonka.M2.GetApplicationVersion
import Amazonka.M2.GetBatchJobExecution
import Amazonka.M2.GetDataSetDetails
import Amazonka.M2.GetDataSetImportTask
import Amazonka.M2.GetDeployment
import Amazonka.M2.GetEnvironment
import Amazonka.M2.Lens
import Amazonka.M2.ListApplicationVersions
import Amazonka.M2.ListApplications
import Amazonka.M2.ListBatchJobDefinitions
import Amazonka.M2.ListBatchJobExecutions
import Amazonka.M2.ListDataSetImportHistory
import Amazonka.M2.ListDataSets
import Amazonka.M2.ListDeployments
import Amazonka.M2.ListEngineVersions
import Amazonka.M2.ListEnvironments
import Amazonka.M2.ListTagsForResource
import Amazonka.M2.StartApplication
import Amazonka.M2.StartBatchJob
import Amazonka.M2.StopApplication
import Amazonka.M2.TagResource
import Amazonka.M2.Types
import Amazonka.M2.UntagResource
import Amazonka.M2.UpdateApplication
import Amazonka.M2.UpdateEnvironment
import Amazonka.M2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'M2'.

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

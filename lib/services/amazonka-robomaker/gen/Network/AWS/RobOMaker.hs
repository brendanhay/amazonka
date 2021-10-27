{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.RobOMaker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-06-29@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This section provides documentation for the AWS RoboMaker API
-- operations.
module Network.AWS.RobOMaker
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ConcurrentDeploymentException
    _ConcurrentDeploymentException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeWorldExportJob
    DescribeWorldExportJob (DescribeWorldExportJob'),
    newDescribeWorldExportJob,
    DescribeWorldExportJobResponse (DescribeWorldExportJobResponse'),
    newDescribeWorldExportJobResponse,

    -- ** BatchDeleteWorlds
    BatchDeleteWorlds (BatchDeleteWorlds'),
    newBatchDeleteWorlds,
    BatchDeleteWorldsResponse (BatchDeleteWorldsResponse'),
    newBatchDeleteWorldsResponse,

    -- ** GetWorldTemplateBody
    GetWorldTemplateBody (GetWorldTemplateBody'),
    newGetWorldTemplateBody,
    GetWorldTemplateBodyResponse (GetWorldTemplateBodyResponse'),
    newGetWorldTemplateBodyResponse,

    -- ** DeleteFleet
    DeleteFleet (DeleteFleet'),
    newDeleteFleet,
    DeleteFleetResponse (DeleteFleetResponse'),
    newDeleteFleetResponse,

    -- ** CreateWorldGenerationJob
    CreateWorldGenerationJob (CreateWorldGenerationJob'),
    newCreateWorldGenerationJob,
    CreateWorldGenerationJobResponse (CreateWorldGenerationJobResponse'),
    newCreateWorldGenerationJobResponse,

    -- ** ListRobotApplications (Paginated)
    ListRobotApplications (ListRobotApplications'),
    newListRobotApplications,
    ListRobotApplicationsResponse (ListRobotApplicationsResponse'),
    newListRobotApplicationsResponse,

    -- ** UpdateRobotApplication
    UpdateRobotApplication (UpdateRobotApplication'),
    newUpdateRobotApplication,
    UpdateRobotApplicationResponse (UpdateRobotApplicationResponse'),
    newUpdateRobotApplicationResponse,

    -- ** DeleteRobotApplication
    DeleteRobotApplication (DeleteRobotApplication'),
    newDeleteRobotApplication,
    DeleteRobotApplicationResponse (DeleteRobotApplicationResponse'),
    newDeleteRobotApplicationResponse,

    -- ** CreateSimulationApplicationVersion
    CreateSimulationApplicationVersion (CreateSimulationApplicationVersion'),
    newCreateSimulationApplicationVersion,
    CreateSimulationApplicationVersionResponse (CreateSimulationApplicationVersionResponse'),
    newCreateSimulationApplicationVersionResponse,

    -- ** ListDeploymentJobs (Paginated)
    ListDeploymentJobs (ListDeploymentJobs'),
    newListDeploymentJobs,
    ListDeploymentJobsResponse (ListDeploymentJobsResponse'),
    newListDeploymentJobsResponse,

    -- ** DescribeWorld
    DescribeWorld (DescribeWorld'),
    newDescribeWorld,
    DescribeWorldResponse (DescribeWorldResponse'),
    newDescribeWorldResponse,

    -- ** CancelSimulationJob
    CancelSimulationJob (CancelSimulationJob'),
    newCancelSimulationJob,
    CancelSimulationJobResponse (CancelSimulationJobResponse'),
    newCancelSimulationJobResponse,

    -- ** CreateRobotApplication
    CreateRobotApplication (CreateRobotApplication'),
    newCreateRobotApplication,
    CreateRobotApplicationResponse (CreateRobotApplicationResponse'),
    newCreateRobotApplicationResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateDeploymentJob
    CreateDeploymentJob (CreateDeploymentJob'),
    newCreateDeploymentJob,
    CreateDeploymentJobResponse (CreateDeploymentJobResponse'),
    newCreateDeploymentJobResponse,

    -- ** RegisterRobot
    RegisterRobot (RegisterRobot'),
    newRegisterRobot,
    RegisterRobotResponse (RegisterRobotResponse'),
    newRegisterRobotResponse,

    -- ** ListRobots (Paginated)
    ListRobots (ListRobots'),
    newListRobots,
    ListRobotsResponse (ListRobotsResponse'),
    newListRobotsResponse,

    -- ** CreateWorldExportJob
    CreateWorldExportJob (CreateWorldExportJob'),
    newCreateWorldExportJob,
    CreateWorldExportJobResponse (CreateWorldExportJobResponse'),
    newCreateWorldExportJobResponse,

    -- ** BatchDescribeSimulationJob
    BatchDescribeSimulationJob (BatchDescribeSimulationJob'),
    newBatchDescribeSimulationJob,
    BatchDescribeSimulationJobResponse (BatchDescribeSimulationJobResponse'),
    newBatchDescribeSimulationJobResponse,

    -- ** CreateSimulationApplication
    CreateSimulationApplication (CreateSimulationApplication'),
    newCreateSimulationApplication,
    CreateSimulationApplicationResponse (CreateSimulationApplicationResponse'),
    newCreateSimulationApplicationResponse,

    -- ** StartSimulationJobBatch
    StartSimulationJobBatch (StartSimulationJobBatch'),
    newStartSimulationJobBatch,
    StartSimulationJobBatchResponse (StartSimulationJobBatchResponse'),
    newStartSimulationJobBatchResponse,

    -- ** CreateRobot
    CreateRobot (CreateRobot'),
    newCreateRobot,
    CreateRobotResponse (CreateRobotResponse'),
    newCreateRobotResponse,

    -- ** DescribeFleet
    DescribeFleet (DescribeFleet'),
    newDescribeFleet,
    DescribeFleetResponse (DescribeFleetResponse'),
    newDescribeFleetResponse,

    -- ** ListWorldTemplates (Paginated)
    ListWorldTemplates (ListWorldTemplates'),
    newListWorldTemplates,
    ListWorldTemplatesResponse (ListWorldTemplatesResponse'),
    newListWorldTemplatesResponse,

    -- ** DescribeRobotApplication
    DescribeRobotApplication (DescribeRobotApplication'),
    newDescribeRobotApplication,
    DescribeRobotApplicationResponse (DescribeRobotApplicationResponse'),
    newDescribeRobotApplicationResponse,

    -- ** RestartSimulationJob
    RestartSimulationJob (RestartSimulationJob'),
    newRestartSimulationJob,
    RestartSimulationJobResponse (RestartSimulationJobResponse'),
    newRestartSimulationJobResponse,

    -- ** DescribeSimulationJob
    DescribeSimulationJob (DescribeSimulationJob'),
    newDescribeSimulationJob,
    DescribeSimulationJobResponse (DescribeSimulationJobResponse'),
    newDescribeSimulationJobResponse,

    -- ** DeregisterRobot
    DeregisterRobot (DeregisterRobot'),
    newDeregisterRobot,
    DeregisterRobotResponse (DeregisterRobotResponse'),
    newDeregisterRobotResponse,

    -- ** DescribeSimulationApplication
    DescribeSimulationApplication (DescribeSimulationApplication'),
    newDescribeSimulationApplication,
    DescribeSimulationApplicationResponse (DescribeSimulationApplicationResponse'),
    newDescribeSimulationApplicationResponse,

    -- ** ListSimulationJobBatches (Paginated)
    ListSimulationJobBatches (ListSimulationJobBatches'),
    newListSimulationJobBatches,
    ListSimulationJobBatchesResponse (ListSimulationJobBatchesResponse'),
    newListSimulationJobBatchesResponse,

    -- ** ListFleets (Paginated)
    ListFleets (ListFleets'),
    newListFleets,
    ListFleetsResponse (ListFleetsResponse'),
    newListFleetsResponse,

    -- ** DescribeWorldTemplate
    DescribeWorldTemplate (DescribeWorldTemplate'),
    newDescribeWorldTemplate,
    DescribeWorldTemplateResponse (DescribeWorldTemplateResponse'),
    newDescribeWorldTemplateResponse,

    -- ** CancelWorldExportJob
    CancelWorldExportJob (CancelWorldExportJob'),
    newCancelWorldExportJob,
    CancelWorldExportJobResponse (CancelWorldExportJobResponse'),
    newCancelWorldExportJobResponse,

    -- ** ListWorldGenerationJobs (Paginated)
    ListWorldGenerationJobs (ListWorldGenerationJobs'),
    newListWorldGenerationJobs,
    ListWorldGenerationJobsResponse (ListWorldGenerationJobsResponse'),
    newListWorldGenerationJobsResponse,

    -- ** CreateFleet
    CreateFleet (CreateFleet'),
    newCreateFleet,
    CreateFleetResponse (CreateFleetResponse'),
    newCreateFleetResponse,

    -- ** CancelWorldGenerationJob
    CancelWorldGenerationJob (CancelWorldGenerationJob'),
    newCancelWorldGenerationJob,
    CancelWorldGenerationJobResponse (CancelWorldGenerationJobResponse'),
    newCancelWorldGenerationJobResponse,

    -- ** DescribeSimulationJobBatch
    DescribeSimulationJobBatch (DescribeSimulationJobBatch'),
    newDescribeSimulationJobBatch,
    DescribeSimulationJobBatchResponse (DescribeSimulationJobBatchResponse'),
    newDescribeSimulationJobBatchResponse,

    -- ** ListSimulationJobs (Paginated)
    ListSimulationJobs (ListSimulationJobs'),
    newListSimulationJobs,
    ListSimulationJobsResponse (ListSimulationJobsResponse'),
    newListSimulationJobsResponse,

    -- ** DeleteRobot
    DeleteRobot (DeleteRobot'),
    newDeleteRobot,
    DeleteRobotResponse (DeleteRobotResponse'),
    newDeleteRobotResponse,

    -- ** DeleteSimulationApplication
    DeleteSimulationApplication (DeleteSimulationApplication'),
    newDeleteSimulationApplication,
    DeleteSimulationApplicationResponse (DeleteSimulationApplicationResponse'),
    newDeleteSimulationApplicationResponse,

    -- ** UpdateSimulationApplication
    UpdateSimulationApplication (UpdateSimulationApplication'),
    newUpdateSimulationApplication,
    UpdateSimulationApplicationResponse (UpdateSimulationApplicationResponse'),
    newUpdateSimulationApplicationResponse,

    -- ** CreateSimulationJob
    CreateSimulationJob (CreateSimulationJob'),
    newCreateSimulationJob,
    CreateSimulationJobResponse (CreateSimulationJobResponse'),
    newCreateSimulationJobResponse,

    -- ** ListWorldExportJobs (Paginated)
    ListWorldExportJobs (ListWorldExportJobs'),
    newListWorldExportJobs,
    ListWorldExportJobsResponse (ListWorldExportJobsResponse'),
    newListWorldExportJobsResponse,

    -- ** ListSimulationApplications (Paginated)
    ListSimulationApplications (ListSimulationApplications'),
    newListSimulationApplications,
    ListSimulationApplicationsResponse (ListSimulationApplicationsResponse'),
    newListSimulationApplicationsResponse,

    -- ** CreateRobotApplicationVersion
    CreateRobotApplicationVersion (CreateRobotApplicationVersion'),
    newCreateRobotApplicationVersion,
    CreateRobotApplicationVersionResponse (CreateRobotApplicationVersionResponse'),
    newCreateRobotApplicationVersionResponse,

    -- ** CancelDeploymentJob
    CancelDeploymentJob (CancelDeploymentJob'),
    newCancelDeploymentJob,
    CancelDeploymentJobResponse (CancelDeploymentJobResponse'),
    newCancelDeploymentJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListWorlds (Paginated)
    ListWorlds (ListWorlds'),
    newListWorlds,
    ListWorldsResponse (ListWorldsResponse'),
    newListWorldsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeDeploymentJob
    DescribeDeploymentJob (DescribeDeploymentJob'),
    newDescribeDeploymentJob,
    DescribeDeploymentJobResponse (DescribeDeploymentJobResponse'),
    newDescribeDeploymentJobResponse,

    -- ** DeleteWorldTemplate
    DeleteWorldTemplate (DeleteWorldTemplate'),
    newDeleteWorldTemplate,
    DeleteWorldTemplateResponse (DeleteWorldTemplateResponse'),
    newDeleteWorldTemplateResponse,

    -- ** UpdateWorldTemplate
    UpdateWorldTemplate (UpdateWorldTemplate'),
    newUpdateWorldTemplate,
    UpdateWorldTemplateResponse (UpdateWorldTemplateResponse'),
    newUpdateWorldTemplateResponse,

    -- ** DescribeWorldGenerationJob
    DescribeWorldGenerationJob (DescribeWorldGenerationJob'),
    newDescribeWorldGenerationJob,
    DescribeWorldGenerationJobResponse (DescribeWorldGenerationJobResponse'),
    newDescribeWorldGenerationJobResponse,

    -- ** CreateWorldTemplate
    CreateWorldTemplate (CreateWorldTemplate'),
    newCreateWorldTemplate,
    CreateWorldTemplateResponse (CreateWorldTemplateResponse'),
    newCreateWorldTemplateResponse,

    -- ** CancelSimulationJobBatch
    CancelSimulationJobBatch (CancelSimulationJobBatch'),
    newCancelSimulationJobBatch,
    CancelSimulationJobBatchResponse (CancelSimulationJobBatchResponse'),
    newCancelSimulationJobBatchResponse,

    -- ** DescribeRobot
    DescribeRobot (DescribeRobot'),
    newDescribeRobot,
    DescribeRobotResponse (DescribeRobotResponse'),
    newDescribeRobotResponse,

    -- ** SyncDeploymentJob
    SyncDeploymentJob (SyncDeploymentJob'),
    newSyncDeploymentJob,
    SyncDeploymentJobResponse (SyncDeploymentJobResponse'),
    newSyncDeploymentJobResponse,

    -- * Types

    -- ** Architecture
    Architecture (..),

    -- ** ComputeType
    ComputeType (..),

    -- ** DataSourceType
    DataSourceType (..),

    -- ** DeploymentJobErrorCode
    DeploymentJobErrorCode (..),

    -- ** DeploymentStatus
    DeploymentStatus (..),

    -- ** ExitBehavior
    ExitBehavior (..),

    -- ** FailureBehavior
    FailureBehavior (..),

    -- ** RenderingEngineType
    RenderingEngineType (..),

    -- ** RobotDeploymentStep
    RobotDeploymentStep (..),

    -- ** RobotSoftwareSuiteType
    RobotSoftwareSuiteType (..),

    -- ** RobotSoftwareSuiteVersionType
    RobotSoftwareSuiteVersionType (..),

    -- ** RobotStatus
    RobotStatus (..),

    -- ** SimulationJobBatchErrorCode
    SimulationJobBatchErrorCode (..),

    -- ** SimulationJobBatchStatus
    SimulationJobBatchStatus (..),

    -- ** SimulationJobErrorCode
    SimulationJobErrorCode (..),

    -- ** SimulationJobStatus
    SimulationJobStatus (..),

    -- ** SimulationSoftwareSuiteType
    SimulationSoftwareSuiteType (..),

    -- ** UploadBehavior
    UploadBehavior (..),

    -- ** WorldExportJobErrorCode
    WorldExportJobErrorCode (..),

    -- ** WorldExportJobStatus
    WorldExportJobStatus (..),

    -- ** WorldGenerationJobErrorCode
    WorldGenerationJobErrorCode (..),

    -- ** WorldGenerationJobStatus
    WorldGenerationJobStatus (..),

    -- ** BatchPolicy
    BatchPolicy (BatchPolicy'),
    newBatchPolicy,

    -- ** Compute
    Compute (Compute'),
    newCompute,

    -- ** ComputeResponse
    ComputeResponse (ComputeResponse'),
    newComputeResponse,

    -- ** DataSource
    DataSource (DataSource'),
    newDataSource,

    -- ** DataSourceConfig
    DataSourceConfig (DataSourceConfig'),
    newDataSourceConfig,

    -- ** DeploymentApplicationConfig
    DeploymentApplicationConfig (DeploymentApplicationConfig'),
    newDeploymentApplicationConfig,

    -- ** DeploymentConfig
    DeploymentConfig (DeploymentConfig'),
    newDeploymentConfig,

    -- ** DeploymentJob
    DeploymentJob (DeploymentJob'),
    newDeploymentJob,

    -- ** DeploymentLaunchConfig
    DeploymentLaunchConfig (DeploymentLaunchConfig'),
    newDeploymentLaunchConfig,

    -- ** Environment
    Environment (Environment'),
    newEnvironment,

    -- ** FailedCreateSimulationJobRequest
    FailedCreateSimulationJobRequest (FailedCreateSimulationJobRequest'),
    newFailedCreateSimulationJobRequest,

    -- ** FailureSummary
    FailureSummary (FailureSummary'),
    newFailureSummary,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** FinishedWorldsSummary
    FinishedWorldsSummary (FinishedWorldsSummary'),
    newFinishedWorldsSummary,

    -- ** Fleet
    Fleet (Fleet'),
    newFleet,

    -- ** LaunchConfig
    LaunchConfig (LaunchConfig'),
    newLaunchConfig,

    -- ** LoggingConfig
    LoggingConfig (LoggingConfig'),
    newLoggingConfig,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** OutputLocation
    OutputLocation (OutputLocation'),
    newOutputLocation,

    -- ** PortForwardingConfig
    PortForwardingConfig (PortForwardingConfig'),
    newPortForwardingConfig,

    -- ** PortMapping
    PortMapping (PortMapping'),
    newPortMapping,

    -- ** ProgressDetail
    ProgressDetail (ProgressDetail'),
    newProgressDetail,

    -- ** RenderingEngine
    RenderingEngine (RenderingEngine'),
    newRenderingEngine,

    -- ** Robot
    Robot (Robot'),
    newRobot,

    -- ** RobotApplicationConfig
    RobotApplicationConfig (RobotApplicationConfig'),
    newRobotApplicationConfig,

    -- ** RobotApplicationSummary
    RobotApplicationSummary (RobotApplicationSummary'),
    newRobotApplicationSummary,

    -- ** RobotDeployment
    RobotDeployment (RobotDeployment'),
    newRobotDeployment,

    -- ** RobotSoftwareSuite
    RobotSoftwareSuite (RobotSoftwareSuite'),
    newRobotSoftwareSuite,

    -- ** S3KeyOutput
    S3KeyOutput (S3KeyOutput'),
    newS3KeyOutput,

    -- ** S3Object
    S3Object (S3Object'),
    newS3Object,

    -- ** SimulationApplicationConfig
    SimulationApplicationConfig (SimulationApplicationConfig'),
    newSimulationApplicationConfig,

    -- ** SimulationApplicationSummary
    SimulationApplicationSummary (SimulationApplicationSummary'),
    newSimulationApplicationSummary,

    -- ** SimulationJob
    SimulationJob (SimulationJob'),
    newSimulationJob,

    -- ** SimulationJobBatchSummary
    SimulationJobBatchSummary (SimulationJobBatchSummary'),
    newSimulationJobBatchSummary,

    -- ** SimulationJobRequest
    SimulationJobRequest (SimulationJobRequest'),
    newSimulationJobRequest,

    -- ** SimulationJobSummary
    SimulationJobSummary (SimulationJobSummary'),
    newSimulationJobSummary,

    -- ** SimulationSoftwareSuite
    SimulationSoftwareSuite (SimulationSoftwareSuite'),
    newSimulationSoftwareSuite,

    -- ** Source
    Source (Source'),
    newSource,

    -- ** SourceConfig
    SourceConfig (SourceConfig'),
    newSourceConfig,

    -- ** TemplateLocation
    TemplateLocation (TemplateLocation'),
    newTemplateLocation,

    -- ** TemplateSummary
    TemplateSummary (TemplateSummary'),
    newTemplateSummary,

    -- ** Tool
    Tool (Tool'),
    newTool,

    -- ** UploadConfiguration
    UploadConfiguration (UploadConfiguration'),
    newUploadConfiguration,

    -- ** VPCConfig
    VPCConfig (VPCConfig'),
    newVPCConfig,

    -- ** VPCConfigResponse
    VPCConfigResponse (VPCConfigResponse'),
    newVPCConfigResponse,

    -- ** WorldConfig
    WorldConfig (WorldConfig'),
    newWorldConfig,

    -- ** WorldCount
    WorldCount (WorldCount'),
    newWorldCount,

    -- ** WorldExportJobSummary
    WorldExportJobSummary (WorldExportJobSummary'),
    newWorldExportJobSummary,

    -- ** WorldFailure
    WorldFailure (WorldFailure'),
    newWorldFailure,

    -- ** WorldGenerationJobSummary
    WorldGenerationJobSummary (WorldGenerationJobSummary'),
    newWorldGenerationJobSummary,

    -- ** WorldSummary
    WorldSummary (WorldSummary'),
    newWorldSummary,
  )
where

import Network.AWS.RobOMaker.BatchDeleteWorlds
import Network.AWS.RobOMaker.BatchDescribeSimulationJob
import Network.AWS.RobOMaker.CancelDeploymentJob
import Network.AWS.RobOMaker.CancelSimulationJob
import Network.AWS.RobOMaker.CancelSimulationJobBatch
import Network.AWS.RobOMaker.CancelWorldExportJob
import Network.AWS.RobOMaker.CancelWorldGenerationJob
import Network.AWS.RobOMaker.CreateDeploymentJob
import Network.AWS.RobOMaker.CreateFleet
import Network.AWS.RobOMaker.CreateRobot
import Network.AWS.RobOMaker.CreateRobotApplication
import Network.AWS.RobOMaker.CreateRobotApplicationVersion
import Network.AWS.RobOMaker.CreateSimulationApplication
import Network.AWS.RobOMaker.CreateSimulationApplicationVersion
import Network.AWS.RobOMaker.CreateSimulationJob
import Network.AWS.RobOMaker.CreateWorldExportJob
import Network.AWS.RobOMaker.CreateWorldGenerationJob
import Network.AWS.RobOMaker.CreateWorldTemplate
import Network.AWS.RobOMaker.DeleteFleet
import Network.AWS.RobOMaker.DeleteRobot
import Network.AWS.RobOMaker.DeleteRobotApplication
import Network.AWS.RobOMaker.DeleteSimulationApplication
import Network.AWS.RobOMaker.DeleteWorldTemplate
import Network.AWS.RobOMaker.DeregisterRobot
import Network.AWS.RobOMaker.DescribeDeploymentJob
import Network.AWS.RobOMaker.DescribeFleet
import Network.AWS.RobOMaker.DescribeRobot
import Network.AWS.RobOMaker.DescribeRobotApplication
import Network.AWS.RobOMaker.DescribeSimulationApplication
import Network.AWS.RobOMaker.DescribeSimulationJob
import Network.AWS.RobOMaker.DescribeSimulationJobBatch
import Network.AWS.RobOMaker.DescribeWorld
import Network.AWS.RobOMaker.DescribeWorldExportJob
import Network.AWS.RobOMaker.DescribeWorldGenerationJob
import Network.AWS.RobOMaker.DescribeWorldTemplate
import Network.AWS.RobOMaker.GetWorldTemplateBody
import Network.AWS.RobOMaker.Lens
import Network.AWS.RobOMaker.ListDeploymentJobs
import Network.AWS.RobOMaker.ListFleets
import Network.AWS.RobOMaker.ListRobotApplications
import Network.AWS.RobOMaker.ListRobots
import Network.AWS.RobOMaker.ListSimulationApplications
import Network.AWS.RobOMaker.ListSimulationJobBatches
import Network.AWS.RobOMaker.ListSimulationJobs
import Network.AWS.RobOMaker.ListTagsForResource
import Network.AWS.RobOMaker.ListWorldExportJobs
import Network.AWS.RobOMaker.ListWorldGenerationJobs
import Network.AWS.RobOMaker.ListWorldTemplates
import Network.AWS.RobOMaker.ListWorlds
import Network.AWS.RobOMaker.RegisterRobot
import Network.AWS.RobOMaker.RestartSimulationJob
import Network.AWS.RobOMaker.StartSimulationJobBatch
import Network.AWS.RobOMaker.SyncDeploymentJob
import Network.AWS.RobOMaker.TagResource
import Network.AWS.RobOMaker.Types
import Network.AWS.RobOMaker.UntagResource
import Network.AWS.RobOMaker.UpdateRobotApplication
import Network.AWS.RobOMaker.UpdateSimulationApplication
import Network.AWS.RobOMaker.UpdateWorldTemplate
import Network.AWS.RobOMaker.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'RobOMaker'.

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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.EMRServerless
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-07-13@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon EMR Serverless is a new deployment option for Amazon EMR. EMR
-- Serverless provides a serverless runtime environment that simplifies
-- running analytics applications using the latest open source frameworks
-- such as Apache Spark and Apache Hive. With EMR Serverless, you don’t
-- have to configure, optimize, secure, or operate clusters to run
-- applications with these frameworks.
--
-- The API reference to Amazon EMR Serverless is @emr-serverless@. The
-- @emr-serverless@ prefix is used in the following scenarios:
--
-- -   It is the prefix in the CLI commands for Amazon EMR Serverless. For
--     example, @aws emr-serverless start-job-run@.
--
-- -   It is the prefix before IAM policy actions for Amazon EMR
--     Serverless. For example,
--     @\"Action\": [\"emr-serverless:StartJobRun\"]@. For more
--     information, see
--     <https://docs.aws.amazon.com/emr/latest/EMR-Serverless-UserGuide/security_iam_service-with-iam.html#security_iam_service-with-iam-id-based-policies-actions Policy actions for Amazon EMR Serverless>.
--
-- -   It is the prefix used in Amazon EMR Serverless service endpoints.
--     For example, @emr-serverless.us-east-2.amazonaws.com@.
module Amazonka.EMRServerless
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerException
    _InternalServerException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ConflictException
    _ConflictException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelJobRun
    CancelJobRun (CancelJobRun'),
    newCancelJobRun,
    CancelJobRunResponse (CancelJobRunResponse'),
    newCancelJobRunResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** GetApplication
    GetApplication (GetApplication'),
    newGetApplication,
    GetApplicationResponse (GetApplicationResponse'),
    newGetApplicationResponse,

    -- ** GetDashboardForJobRun
    GetDashboardForJobRun (GetDashboardForJobRun'),
    newGetDashboardForJobRun,
    GetDashboardForJobRunResponse (GetDashboardForJobRunResponse'),
    newGetDashboardForJobRunResponse,

    -- ** GetJobRun
    GetJobRun (GetJobRun'),
    newGetJobRun,
    GetJobRunResponse (GetJobRunResponse'),
    newGetJobRunResponse,

    -- ** ListApplications (Paginated)
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** ListJobRuns (Paginated)
    ListJobRuns (ListJobRuns'),
    newListJobRuns,
    ListJobRunsResponse (ListJobRunsResponse'),
    newListJobRunsResponse,

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

    -- ** StartJobRun
    StartJobRun (StartJobRun'),
    newStartJobRun,
    StartJobRunResponse (StartJobRunResponse'),
    newStartJobRunResponse,

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

    -- * Types

    -- ** ApplicationState
    ApplicationState (..),

    -- ** Architecture
    Architecture (..),

    -- ** JobRunState
    JobRunState (..),

    -- ** Application
    Application (Application'),
    newApplication,

    -- ** ApplicationSummary
    ApplicationSummary (ApplicationSummary'),
    newApplicationSummary,

    -- ** AutoStartConfig
    AutoStartConfig (AutoStartConfig'),
    newAutoStartConfig,

    -- ** AutoStopConfig
    AutoStopConfig (AutoStopConfig'),
    newAutoStopConfig,

    -- ** Configuration
    Configuration (Configuration'),
    newConfiguration,

    -- ** ConfigurationOverrides
    ConfigurationOverrides (ConfigurationOverrides'),
    newConfigurationOverrides,

    -- ** Hive
    Hive (Hive'),
    newHive,

    -- ** InitialCapacityConfig
    InitialCapacityConfig (InitialCapacityConfig'),
    newInitialCapacityConfig,

    -- ** JobDriver
    JobDriver (JobDriver'),
    newJobDriver,

    -- ** JobRun
    JobRun (JobRun'),
    newJobRun,

    -- ** JobRunSummary
    JobRunSummary (JobRunSummary'),
    newJobRunSummary,

    -- ** ManagedPersistenceMonitoringConfiguration
    ManagedPersistenceMonitoringConfiguration (ManagedPersistenceMonitoringConfiguration'),
    newManagedPersistenceMonitoringConfiguration,

    -- ** MaximumAllowedResources
    MaximumAllowedResources (MaximumAllowedResources'),
    newMaximumAllowedResources,

    -- ** MonitoringConfiguration
    MonitoringConfiguration (MonitoringConfiguration'),
    newMonitoringConfiguration,

    -- ** NetworkConfiguration
    NetworkConfiguration (NetworkConfiguration'),
    newNetworkConfiguration,

    -- ** S3MonitoringConfiguration
    S3MonitoringConfiguration (S3MonitoringConfiguration'),
    newS3MonitoringConfiguration,

    -- ** SparkSubmit
    SparkSubmit (SparkSubmit'),
    newSparkSubmit,

    -- ** TotalResourceUtilization
    TotalResourceUtilization (TotalResourceUtilization'),
    newTotalResourceUtilization,

    -- ** WorkerResourceConfig
    WorkerResourceConfig (WorkerResourceConfig'),
    newWorkerResourceConfig,
  )
where

import Amazonka.EMRServerless.CancelJobRun
import Amazonka.EMRServerless.CreateApplication
import Amazonka.EMRServerless.DeleteApplication
import Amazonka.EMRServerless.GetApplication
import Amazonka.EMRServerless.GetDashboardForJobRun
import Amazonka.EMRServerless.GetJobRun
import Amazonka.EMRServerless.Lens
import Amazonka.EMRServerless.ListApplications
import Amazonka.EMRServerless.ListJobRuns
import Amazonka.EMRServerless.ListTagsForResource
import Amazonka.EMRServerless.StartApplication
import Amazonka.EMRServerless.StartJobRun
import Amazonka.EMRServerless.StopApplication
import Amazonka.EMRServerless.TagResource
import Amazonka.EMRServerless.Types
import Amazonka.EMRServerless.UntagResource
import Amazonka.EMRServerless.UpdateApplication
import Amazonka.EMRServerless.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'EMRServerless'.

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

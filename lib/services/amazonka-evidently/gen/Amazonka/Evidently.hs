{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Evidently
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-02-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- You can use Amazon CloudWatch Evidently to safely validate new features
-- by serving them to a specified percentage of your users while you roll
-- out the feature. You can monitor the performance of the new feature to
-- help you decide when to ramp up traffic to your users. This helps you
-- reduce risk and identify unintended consequences before you fully launch
-- the feature.
--
-- You can also conduct A\/B experiments to make feature design decisions
-- based on evidence and data. An experiment can test as many as five
-- variations at once. Evidently collects experiment data and analyzes it
-- using statistical methods. It also provides clear recommendations about
-- which variations perform better. You can test both user-facing features
-- and backend features.
module Amazonka.Evidently
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

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchEvaluateFeature
    BatchEvaluateFeature (BatchEvaluateFeature'),
    newBatchEvaluateFeature,
    BatchEvaluateFeatureResponse (BatchEvaluateFeatureResponse'),
    newBatchEvaluateFeatureResponse,

    -- ** CreateExperiment
    CreateExperiment (CreateExperiment'),
    newCreateExperiment,
    CreateExperimentResponse (CreateExperimentResponse'),
    newCreateExperimentResponse,

    -- ** CreateFeature
    CreateFeature (CreateFeature'),
    newCreateFeature,
    CreateFeatureResponse (CreateFeatureResponse'),
    newCreateFeatureResponse,

    -- ** CreateLaunch
    CreateLaunch (CreateLaunch'),
    newCreateLaunch,
    CreateLaunchResponse (CreateLaunchResponse'),
    newCreateLaunchResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** CreateSegment
    CreateSegment (CreateSegment'),
    newCreateSegment,
    CreateSegmentResponse (CreateSegmentResponse'),
    newCreateSegmentResponse,

    -- ** DeleteExperiment
    DeleteExperiment (DeleteExperiment'),
    newDeleteExperiment,
    DeleteExperimentResponse (DeleteExperimentResponse'),
    newDeleteExperimentResponse,

    -- ** DeleteFeature
    DeleteFeature (DeleteFeature'),
    newDeleteFeature,
    DeleteFeatureResponse (DeleteFeatureResponse'),
    newDeleteFeatureResponse,

    -- ** DeleteLaunch
    DeleteLaunch (DeleteLaunch'),
    newDeleteLaunch,
    DeleteLaunchResponse (DeleteLaunchResponse'),
    newDeleteLaunchResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DeleteSegment
    DeleteSegment (DeleteSegment'),
    newDeleteSegment,
    DeleteSegmentResponse (DeleteSegmentResponse'),
    newDeleteSegmentResponse,

    -- ** EvaluateFeature
    EvaluateFeature (EvaluateFeature'),
    newEvaluateFeature,
    EvaluateFeatureResponse (EvaluateFeatureResponse'),
    newEvaluateFeatureResponse,

    -- ** GetExperiment
    GetExperiment (GetExperiment'),
    newGetExperiment,
    GetExperimentResponse (GetExperimentResponse'),
    newGetExperimentResponse,

    -- ** GetExperimentResults
    GetExperimentResults (GetExperimentResults'),
    newGetExperimentResults,
    GetExperimentResultsResponse (GetExperimentResultsResponse'),
    newGetExperimentResultsResponse,

    -- ** GetFeature
    GetFeature (GetFeature'),
    newGetFeature,
    GetFeatureResponse (GetFeatureResponse'),
    newGetFeatureResponse,

    -- ** GetLaunch
    GetLaunch (GetLaunch'),
    newGetLaunch,
    GetLaunchResponse (GetLaunchResponse'),
    newGetLaunchResponse,

    -- ** GetProject
    GetProject (GetProject'),
    newGetProject,
    GetProjectResponse (GetProjectResponse'),
    newGetProjectResponse,

    -- ** GetSegment
    GetSegment (GetSegment'),
    newGetSegment,
    GetSegmentResponse (GetSegmentResponse'),
    newGetSegmentResponse,

    -- ** ListExperiments (Paginated)
    ListExperiments (ListExperiments'),
    newListExperiments,
    ListExperimentsResponse (ListExperimentsResponse'),
    newListExperimentsResponse,

    -- ** ListFeatures (Paginated)
    ListFeatures (ListFeatures'),
    newListFeatures,
    ListFeaturesResponse (ListFeaturesResponse'),
    newListFeaturesResponse,

    -- ** ListLaunches (Paginated)
    ListLaunches (ListLaunches'),
    newListLaunches,
    ListLaunchesResponse (ListLaunchesResponse'),
    newListLaunchesResponse,

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** ListSegmentReferences (Paginated)
    ListSegmentReferences (ListSegmentReferences'),
    newListSegmentReferences,
    ListSegmentReferencesResponse (ListSegmentReferencesResponse'),
    newListSegmentReferencesResponse,

    -- ** ListSegments (Paginated)
    ListSegments (ListSegments'),
    newListSegments,
    ListSegmentsResponse (ListSegmentsResponse'),
    newListSegmentsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutProjectEvents
    PutProjectEvents (PutProjectEvents'),
    newPutProjectEvents,
    PutProjectEventsResponse (PutProjectEventsResponse'),
    newPutProjectEventsResponse,

    -- ** StartExperiment
    StartExperiment (StartExperiment'),
    newStartExperiment,
    StartExperimentResponse (StartExperimentResponse'),
    newStartExperimentResponse,

    -- ** StartLaunch
    StartLaunch (StartLaunch'),
    newStartLaunch,
    StartLaunchResponse (StartLaunchResponse'),
    newStartLaunchResponse,

    -- ** StopExperiment
    StopExperiment (StopExperiment'),
    newStopExperiment,
    StopExperimentResponse (StopExperimentResponse'),
    newStopExperimentResponse,

    -- ** StopLaunch
    StopLaunch (StopLaunch'),
    newStopLaunch,
    StopLaunchResponse (StopLaunchResponse'),
    newStopLaunchResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestSegmentPattern
    TestSegmentPattern (TestSegmentPattern'),
    newTestSegmentPattern,
    TestSegmentPatternResponse (TestSegmentPatternResponse'),
    newTestSegmentPatternResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateExperiment
    UpdateExperiment (UpdateExperiment'),
    newUpdateExperiment,
    UpdateExperimentResponse (UpdateExperimentResponse'),
    newUpdateExperimentResponse,

    -- ** UpdateFeature
    UpdateFeature (UpdateFeature'),
    newUpdateFeature,
    UpdateFeatureResponse (UpdateFeatureResponse'),
    newUpdateFeatureResponse,

    -- ** UpdateLaunch
    UpdateLaunch (UpdateLaunch'),
    newUpdateLaunch,
    UpdateLaunchResponse (UpdateLaunchResponse'),
    newUpdateLaunchResponse,

    -- ** UpdateProject
    UpdateProject (UpdateProject'),
    newUpdateProject,
    UpdateProjectResponse (UpdateProjectResponse'),
    newUpdateProjectResponse,

    -- ** UpdateProjectDataDelivery
    UpdateProjectDataDelivery (UpdateProjectDataDelivery'),
    newUpdateProjectDataDelivery,
    UpdateProjectDataDeliveryResponse (UpdateProjectDataDeliveryResponse'),
    newUpdateProjectDataDeliveryResponse,

    -- * Types

    -- ** ChangeDirectionEnum
    ChangeDirectionEnum (..),

    -- ** EventType
    EventType (..),

    -- ** ExperimentBaseStat
    ExperimentBaseStat (..),

    -- ** ExperimentReportName
    ExperimentReportName (..),

    -- ** ExperimentResultRequestType
    ExperimentResultRequestType (..),

    -- ** ExperimentResultResponseType
    ExperimentResultResponseType (..),

    -- ** ExperimentStatus
    ExperimentStatus (..),

    -- ** ExperimentStopDesiredState
    ExperimentStopDesiredState (..),

    -- ** ExperimentType
    ExperimentType (..),

    -- ** FeatureEvaluationStrategy
    FeatureEvaluationStrategy (..),

    -- ** FeatureStatus
    FeatureStatus (..),

    -- ** LaunchStatus
    LaunchStatus (..),

    -- ** LaunchStopDesiredState
    LaunchStopDesiredState (..),

    -- ** LaunchType
    LaunchType (..),

    -- ** ProjectStatus
    ProjectStatus (..),

    -- ** SegmentReferenceResourceType
    SegmentReferenceResourceType (..),

    -- ** VariationValueType
    VariationValueType (..),

    -- ** CloudWatchLogsDestination
    CloudWatchLogsDestination (CloudWatchLogsDestination'),
    newCloudWatchLogsDestination,

    -- ** CloudWatchLogsDestinationConfig
    CloudWatchLogsDestinationConfig (CloudWatchLogsDestinationConfig'),
    newCloudWatchLogsDestinationConfig,

    -- ** EvaluationRequest
    EvaluationRequest (EvaluationRequest'),
    newEvaluationRequest,

    -- ** EvaluationResult
    EvaluationResult (EvaluationResult'),
    newEvaluationResult,

    -- ** EvaluationRule
    EvaluationRule (EvaluationRule'),
    newEvaluationRule,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** Experiment
    Experiment (Experiment'),
    newExperiment,

    -- ** ExperimentExecution
    ExperimentExecution (ExperimentExecution'),
    newExperimentExecution,

    -- ** ExperimentReport
    ExperimentReport (ExperimentReport'),
    newExperimentReport,

    -- ** ExperimentResultsData
    ExperimentResultsData (ExperimentResultsData'),
    newExperimentResultsData,

    -- ** ExperimentSchedule
    ExperimentSchedule (ExperimentSchedule'),
    newExperimentSchedule,

    -- ** Feature
    Feature (Feature'),
    newFeature,

    -- ** FeatureSummary
    FeatureSummary (FeatureSummary'),
    newFeatureSummary,

    -- ** Launch
    Launch (Launch'),
    newLaunch,

    -- ** LaunchExecution
    LaunchExecution (LaunchExecution'),
    newLaunchExecution,

    -- ** LaunchGroup
    LaunchGroup (LaunchGroup'),
    newLaunchGroup,

    -- ** LaunchGroupConfig
    LaunchGroupConfig (LaunchGroupConfig'),
    newLaunchGroupConfig,

    -- ** MetricDefinition
    MetricDefinition (MetricDefinition'),
    newMetricDefinition,

    -- ** MetricDefinitionConfig
    MetricDefinitionConfig (MetricDefinitionConfig'),
    newMetricDefinitionConfig,

    -- ** MetricGoal
    MetricGoal (MetricGoal'),
    newMetricGoal,

    -- ** MetricGoalConfig
    MetricGoalConfig (MetricGoalConfig'),
    newMetricGoalConfig,

    -- ** MetricMonitor
    MetricMonitor (MetricMonitor'),
    newMetricMonitor,

    -- ** MetricMonitorConfig
    MetricMonitorConfig (MetricMonitorConfig'),
    newMetricMonitorConfig,

    -- ** OnlineAbConfig
    OnlineAbConfig (OnlineAbConfig'),
    newOnlineAbConfig,

    -- ** OnlineAbDefinition
    OnlineAbDefinition (OnlineAbDefinition'),
    newOnlineAbDefinition,

    -- ** Project
    Project (Project'),
    newProject,

    -- ** ProjectAppConfigResource
    ProjectAppConfigResource (ProjectAppConfigResource'),
    newProjectAppConfigResource,

    -- ** ProjectAppConfigResourceConfig
    ProjectAppConfigResourceConfig (ProjectAppConfigResourceConfig'),
    newProjectAppConfigResourceConfig,

    -- ** ProjectDataDelivery
    ProjectDataDelivery (ProjectDataDelivery'),
    newProjectDataDelivery,

    -- ** ProjectDataDeliveryConfig
    ProjectDataDeliveryConfig (ProjectDataDeliveryConfig'),
    newProjectDataDeliveryConfig,

    -- ** ProjectSummary
    ProjectSummary (ProjectSummary'),
    newProjectSummary,

    -- ** PutProjectEventsResultEntry
    PutProjectEventsResultEntry (PutProjectEventsResultEntry'),
    newPutProjectEventsResultEntry,

    -- ** RefResource
    RefResource (RefResource'),
    newRefResource,

    -- ** S3Destination
    S3Destination (S3Destination'),
    newS3Destination,

    -- ** S3DestinationConfig
    S3DestinationConfig (S3DestinationConfig'),
    newS3DestinationConfig,

    -- ** ScheduledSplit
    ScheduledSplit (ScheduledSplit'),
    newScheduledSplit,

    -- ** ScheduledSplitConfig
    ScheduledSplitConfig (ScheduledSplitConfig'),
    newScheduledSplitConfig,

    -- ** ScheduledSplitsLaunchConfig
    ScheduledSplitsLaunchConfig (ScheduledSplitsLaunchConfig'),
    newScheduledSplitsLaunchConfig,

    -- ** ScheduledSplitsLaunchDefinition
    ScheduledSplitsLaunchDefinition (ScheduledSplitsLaunchDefinition'),
    newScheduledSplitsLaunchDefinition,

    -- ** Segment
    Segment (Segment'),
    newSegment,

    -- ** SegmentOverride
    SegmentOverride (SegmentOverride'),
    newSegmentOverride,

    -- ** Treatment
    Treatment (Treatment'),
    newTreatment,

    -- ** TreatmentConfig
    TreatmentConfig (TreatmentConfig'),
    newTreatmentConfig,

    -- ** VariableValue
    VariableValue (VariableValue'),
    newVariableValue,

    -- ** Variation
    Variation (Variation'),
    newVariation,

    -- ** VariationConfig
    VariationConfig (VariationConfig'),
    newVariationConfig,
  )
where

import Amazonka.Evidently.BatchEvaluateFeature
import Amazonka.Evidently.CreateExperiment
import Amazonka.Evidently.CreateFeature
import Amazonka.Evidently.CreateLaunch
import Amazonka.Evidently.CreateProject
import Amazonka.Evidently.CreateSegment
import Amazonka.Evidently.DeleteExperiment
import Amazonka.Evidently.DeleteFeature
import Amazonka.Evidently.DeleteLaunch
import Amazonka.Evidently.DeleteProject
import Amazonka.Evidently.DeleteSegment
import Amazonka.Evidently.EvaluateFeature
import Amazonka.Evidently.GetExperiment
import Amazonka.Evidently.GetExperimentResults
import Amazonka.Evidently.GetFeature
import Amazonka.Evidently.GetLaunch
import Amazonka.Evidently.GetProject
import Amazonka.Evidently.GetSegment
import Amazonka.Evidently.Lens
import Amazonka.Evidently.ListExperiments
import Amazonka.Evidently.ListFeatures
import Amazonka.Evidently.ListLaunches
import Amazonka.Evidently.ListProjects
import Amazonka.Evidently.ListSegmentReferences
import Amazonka.Evidently.ListSegments
import Amazonka.Evidently.ListTagsForResource
import Amazonka.Evidently.PutProjectEvents
import Amazonka.Evidently.StartExperiment
import Amazonka.Evidently.StartLaunch
import Amazonka.Evidently.StopExperiment
import Amazonka.Evidently.StopLaunch
import Amazonka.Evidently.TagResource
import Amazonka.Evidently.TestSegmentPattern
import Amazonka.Evidently.Types
import Amazonka.Evidently.UntagResource
import Amazonka.Evidently.UpdateExperiment
import Amazonka.Evidently.UpdateFeature
import Amazonka.Evidently.UpdateLaunch
import Amazonka.Evidently.UpdateProject
import Amazonka.Evidently.UpdateProjectDataDelivery
import Amazonka.Evidently.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Evidently'.

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

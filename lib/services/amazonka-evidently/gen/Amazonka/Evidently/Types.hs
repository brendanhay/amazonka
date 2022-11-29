{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Evidently.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

    -- * ChangeDirectionEnum
    ChangeDirectionEnum (..),

    -- * EventType
    EventType (..),

    -- * ExperimentBaseStat
    ExperimentBaseStat (..),

    -- * ExperimentReportName
    ExperimentReportName (..),

    -- * ExperimentResultRequestType
    ExperimentResultRequestType (..),

    -- * ExperimentResultResponseType
    ExperimentResultResponseType (..),

    -- * ExperimentStatus
    ExperimentStatus (..),

    -- * ExperimentStopDesiredState
    ExperimentStopDesiredState (..),

    -- * ExperimentType
    ExperimentType (..),

    -- * FeatureEvaluationStrategy
    FeatureEvaluationStrategy (..),

    -- * FeatureStatus
    FeatureStatus (..),

    -- * LaunchStatus
    LaunchStatus (..),

    -- * LaunchStopDesiredState
    LaunchStopDesiredState (..),

    -- * LaunchType
    LaunchType (..),

    -- * ProjectStatus
    ProjectStatus (..),

    -- * SegmentReferenceResourceType
    SegmentReferenceResourceType (..),

    -- * VariationValueType
    VariationValueType (..),

    -- * CloudWatchLogsDestination
    CloudWatchLogsDestination (..),
    newCloudWatchLogsDestination,
    cloudWatchLogsDestination_logGroup,

    -- * CloudWatchLogsDestinationConfig
    CloudWatchLogsDestinationConfig (..),
    newCloudWatchLogsDestinationConfig,
    cloudWatchLogsDestinationConfig_logGroup,

    -- * EvaluationRequest
    EvaluationRequest (..),
    newEvaluationRequest,
    evaluationRequest_evaluationContext,
    evaluationRequest_entityId,
    evaluationRequest_feature,

    -- * EvaluationResult
    EvaluationResult (..),
    newEvaluationResult,
    evaluationResult_variation,
    evaluationResult_details,
    evaluationResult_project,
    evaluationResult_reason,
    evaluationResult_value,
    evaluationResult_entityId,
    evaluationResult_feature,

    -- * EvaluationRule
    EvaluationRule (..),
    newEvaluationRule,
    evaluationRule_name,
    evaluationRule_type,

    -- * Event
    Event (..),
    newEvent,
    event_data,
    event_timestamp,
    event_type,

    -- * Experiment
    Experiment (..),
    newExperiment,
    experiment_schedule,
    experiment_tags,
    experiment_onlineAbDefinition,
    experiment_statusReason,
    experiment_description,
    experiment_project,
    experiment_execution,
    experiment_treatments,
    experiment_samplingRate,
    experiment_segment,
    experiment_metricGoals,
    experiment_randomizationSalt,
    experiment_arn,
    experiment_createdTime,
    experiment_lastUpdatedTime,
    experiment_name,
    experiment_status,
    experiment_type,

    -- * ExperimentExecution
    ExperimentExecution (..),
    newExperimentExecution,
    experimentExecution_endedTime,
    experimentExecution_startedTime,

    -- * ExperimentReport
    ExperimentReport (..),
    newExperimentReport,
    experimentReport_reportName,
    experimentReport_treatmentName,
    experimentReport_metricName,
    experimentReport_content,

    -- * ExperimentResultsData
    ExperimentResultsData (..),
    newExperimentResultsData,
    experimentResultsData_treatmentName,
    experimentResultsData_metricName,
    experimentResultsData_values,
    experimentResultsData_resultStat,

    -- * ExperimentSchedule
    ExperimentSchedule (..),
    newExperimentSchedule,
    experimentSchedule_analysisCompleteTime,

    -- * Feature
    Feature (..),
    newFeature,
    feature_tags,
    feature_description,
    feature_project,
    feature_evaluationRules,
    feature_entityOverrides,
    feature_defaultVariation,
    feature_arn,
    feature_createdTime,
    feature_evaluationStrategy,
    feature_lastUpdatedTime,
    feature_name,
    feature_status,
    feature_valueType,
    feature_variations,

    -- * FeatureSummary
    FeatureSummary (..),
    newFeatureSummary,
    featureSummary_tags,
    featureSummary_project,
    featureSummary_evaluationRules,
    featureSummary_defaultVariation,
    featureSummary_arn,
    featureSummary_createdTime,
    featureSummary_evaluationStrategy,
    featureSummary_lastUpdatedTime,
    featureSummary_name,
    featureSummary_status,

    -- * Launch
    Launch (..),
    newLaunch,
    launch_tags,
    launch_scheduledSplitsDefinition,
    launch_statusReason,
    launch_description,
    launch_project,
    launch_execution,
    launch_metricMonitors,
    launch_groups,
    launch_randomizationSalt,
    launch_arn,
    launch_createdTime,
    launch_lastUpdatedTime,
    launch_name,
    launch_status,
    launch_type,

    -- * LaunchExecution
    LaunchExecution (..),
    newLaunchExecution,
    launchExecution_endedTime,
    launchExecution_startedTime,

    -- * LaunchGroup
    LaunchGroup (..),
    newLaunchGroup,
    launchGroup_description,
    launchGroup_featureVariations,
    launchGroup_name,

    -- * LaunchGroupConfig
    LaunchGroupConfig (..),
    newLaunchGroupConfig,
    launchGroupConfig_description,
    launchGroupConfig_feature,
    launchGroupConfig_name,
    launchGroupConfig_variation,

    -- * MetricDefinition
    MetricDefinition (..),
    newMetricDefinition,
    metricDefinition_name,
    metricDefinition_valueKey,
    metricDefinition_unitLabel,
    metricDefinition_eventPattern,
    metricDefinition_entityIdKey,

    -- * MetricDefinitionConfig
    MetricDefinitionConfig (..),
    newMetricDefinitionConfig,
    metricDefinitionConfig_unitLabel,
    metricDefinitionConfig_eventPattern,
    metricDefinitionConfig_entityIdKey,
    metricDefinitionConfig_name,
    metricDefinitionConfig_valueKey,

    -- * MetricGoal
    MetricGoal (..),
    newMetricGoal,
    metricGoal_desiredChange,
    metricGoal_metricDefinition,

    -- * MetricGoalConfig
    MetricGoalConfig (..),
    newMetricGoalConfig,
    metricGoalConfig_desiredChange,
    metricGoalConfig_metricDefinition,

    -- * MetricMonitor
    MetricMonitor (..),
    newMetricMonitor,
    metricMonitor_metricDefinition,

    -- * MetricMonitorConfig
    MetricMonitorConfig (..),
    newMetricMonitorConfig,
    metricMonitorConfig_metricDefinition,

    -- * OnlineAbConfig
    OnlineAbConfig (..),
    newOnlineAbConfig,
    onlineAbConfig_controlTreatmentName,
    onlineAbConfig_treatmentWeights,

    -- * OnlineAbDefinition
    OnlineAbDefinition (..),
    newOnlineAbDefinition,
    onlineAbDefinition_controlTreatmentName,
    onlineAbDefinition_treatmentWeights,

    -- * Project
    Project (..),
    newProject,
    project_tags,
    project_dataDelivery,
    project_appConfigResource,
    project_description,
    project_activeLaunchCount,
    project_featureCount,
    project_launchCount,
    project_experimentCount,
    project_activeExperimentCount,
    project_arn,
    project_createdTime,
    project_lastUpdatedTime,
    project_name,
    project_status,

    -- * ProjectAppConfigResource
    ProjectAppConfigResource (..),
    newProjectAppConfigResource,
    projectAppConfigResource_applicationId,
    projectAppConfigResource_configurationProfileId,
    projectAppConfigResource_environmentId,

    -- * ProjectAppConfigResourceConfig
    ProjectAppConfigResourceConfig (..),
    newProjectAppConfigResourceConfig,
    projectAppConfigResourceConfig_environmentId,
    projectAppConfigResourceConfig_applicationId,

    -- * ProjectDataDelivery
    ProjectDataDelivery (..),
    newProjectDataDelivery,
    projectDataDelivery_cloudWatchLogs,
    projectDataDelivery_s3Destination,

    -- * ProjectDataDeliveryConfig
    ProjectDataDeliveryConfig (..),
    newProjectDataDeliveryConfig,
    projectDataDeliveryConfig_cloudWatchLogs,
    projectDataDeliveryConfig_s3Destination,

    -- * ProjectSummary
    ProjectSummary (..),
    newProjectSummary,
    projectSummary_tags,
    projectSummary_description,
    projectSummary_activeLaunchCount,
    projectSummary_featureCount,
    projectSummary_launchCount,
    projectSummary_experimentCount,
    projectSummary_activeExperimentCount,
    projectSummary_arn,
    projectSummary_createdTime,
    projectSummary_lastUpdatedTime,
    projectSummary_name,
    projectSummary_status,

    -- * PutProjectEventsResultEntry
    PutProjectEventsResultEntry (..),
    newPutProjectEventsResultEntry,
    putProjectEventsResultEntry_errorMessage,
    putProjectEventsResultEntry_eventId,
    putProjectEventsResultEntry_errorCode,

    -- * RefResource
    RefResource (..),
    newRefResource,
    refResource_arn,
    refResource_status,
    refResource_endTime,
    refResource_lastUpdatedOn,
    refResource_startTime,
    refResource_name,
    refResource_type,

    -- * S3Destination
    S3Destination (..),
    newS3Destination,
    s3Destination_bucket,
    s3Destination_prefix,

    -- * S3DestinationConfig
    S3DestinationConfig (..),
    newS3DestinationConfig,
    s3DestinationConfig_bucket,
    s3DestinationConfig_prefix,

    -- * ScheduledSplit
    ScheduledSplit (..),
    newScheduledSplit,
    scheduledSplit_groupWeights,
    scheduledSplit_segmentOverrides,
    scheduledSplit_startTime,

    -- * ScheduledSplitConfig
    ScheduledSplitConfig (..),
    newScheduledSplitConfig,
    scheduledSplitConfig_segmentOverrides,
    scheduledSplitConfig_groupWeights,
    scheduledSplitConfig_startTime,

    -- * ScheduledSplitsLaunchConfig
    ScheduledSplitsLaunchConfig (..),
    newScheduledSplitsLaunchConfig,
    scheduledSplitsLaunchConfig_steps,

    -- * ScheduledSplitsLaunchDefinition
    ScheduledSplitsLaunchDefinition (..),
    newScheduledSplitsLaunchDefinition,
    scheduledSplitsLaunchDefinition_steps,

    -- * Segment
    Segment (..),
    newSegment,
    segment_tags,
    segment_description,
    segment_launchCount,
    segment_experimentCount,
    segment_arn,
    segment_createdTime,
    segment_lastUpdatedTime,
    segment_name,
    segment_pattern,

    -- * SegmentOverride
    SegmentOverride (..),
    newSegmentOverride,
    segmentOverride_evaluationOrder,
    segmentOverride_segment,
    segmentOverride_weights,

    -- * Treatment
    Treatment (..),
    newTreatment,
    treatment_description,
    treatment_featureVariations,
    treatment_name,

    -- * TreatmentConfig
    TreatmentConfig (..),
    newTreatmentConfig,
    treatmentConfig_description,
    treatmentConfig_feature,
    treatmentConfig_name,
    treatmentConfig_variation,

    -- * VariableValue
    VariableValue (..),
    newVariableValue,
    variableValue_doubleValue,
    variableValue_stringValue,
    variableValue_longValue,
    variableValue_boolValue,

    -- * Variation
    Variation (..),
    newVariation,
    variation_name,
    variation_value,

    -- * VariationConfig
    VariationConfig (..),
    newVariationConfig,
    variationConfig_name,
    variationConfig_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Evidently.Types.ChangeDirectionEnum
import Amazonka.Evidently.Types.CloudWatchLogsDestination
import Amazonka.Evidently.Types.CloudWatchLogsDestinationConfig
import Amazonka.Evidently.Types.EvaluationRequest
import Amazonka.Evidently.Types.EvaluationResult
import Amazonka.Evidently.Types.EvaluationRule
import Amazonka.Evidently.Types.Event
import Amazonka.Evidently.Types.EventType
import Amazonka.Evidently.Types.Experiment
import Amazonka.Evidently.Types.ExperimentBaseStat
import Amazonka.Evidently.Types.ExperimentExecution
import Amazonka.Evidently.Types.ExperimentReport
import Amazonka.Evidently.Types.ExperimentReportName
import Amazonka.Evidently.Types.ExperimentResultRequestType
import Amazonka.Evidently.Types.ExperimentResultResponseType
import Amazonka.Evidently.Types.ExperimentResultsData
import Amazonka.Evidently.Types.ExperimentSchedule
import Amazonka.Evidently.Types.ExperimentStatus
import Amazonka.Evidently.Types.ExperimentStopDesiredState
import Amazonka.Evidently.Types.ExperimentType
import Amazonka.Evidently.Types.Feature
import Amazonka.Evidently.Types.FeatureEvaluationStrategy
import Amazonka.Evidently.Types.FeatureStatus
import Amazonka.Evidently.Types.FeatureSummary
import Amazonka.Evidently.Types.Launch
import Amazonka.Evidently.Types.LaunchExecution
import Amazonka.Evidently.Types.LaunchGroup
import Amazonka.Evidently.Types.LaunchGroupConfig
import Amazonka.Evidently.Types.LaunchStatus
import Amazonka.Evidently.Types.LaunchStopDesiredState
import Amazonka.Evidently.Types.LaunchType
import Amazonka.Evidently.Types.MetricDefinition
import Amazonka.Evidently.Types.MetricDefinitionConfig
import Amazonka.Evidently.Types.MetricGoal
import Amazonka.Evidently.Types.MetricGoalConfig
import Amazonka.Evidently.Types.MetricMonitor
import Amazonka.Evidently.Types.MetricMonitorConfig
import Amazonka.Evidently.Types.OnlineAbConfig
import Amazonka.Evidently.Types.OnlineAbDefinition
import Amazonka.Evidently.Types.Project
import Amazonka.Evidently.Types.ProjectAppConfigResource
import Amazonka.Evidently.Types.ProjectAppConfigResourceConfig
import Amazonka.Evidently.Types.ProjectDataDelivery
import Amazonka.Evidently.Types.ProjectDataDeliveryConfig
import Amazonka.Evidently.Types.ProjectStatus
import Amazonka.Evidently.Types.ProjectSummary
import Amazonka.Evidently.Types.PutProjectEventsResultEntry
import Amazonka.Evidently.Types.RefResource
import Amazonka.Evidently.Types.S3Destination
import Amazonka.Evidently.Types.S3DestinationConfig
import Amazonka.Evidently.Types.ScheduledSplit
import Amazonka.Evidently.Types.ScheduledSplitConfig
import Amazonka.Evidently.Types.ScheduledSplitsLaunchConfig
import Amazonka.Evidently.Types.ScheduledSplitsLaunchDefinition
import Amazonka.Evidently.Types.Segment
import Amazonka.Evidently.Types.SegmentOverride
import Amazonka.Evidently.Types.SegmentReferenceResourceType
import Amazonka.Evidently.Types.Treatment
import Amazonka.Evidently.Types.TreatmentConfig
import Amazonka.Evidently.Types.VariableValue
import Amazonka.Evidently.Types.Variation
import Amazonka.Evidently.Types.VariationConfig
import Amazonka.Evidently.Types.VariationValueType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-02-01@ of the Amazon CloudWatch Evidently SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Evidently",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "evidently",
      Core.signingName = "evidently",
      Core.version = "2021-02-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Evidently",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient permissions to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Unexpected error while processing the request. Retry the request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The service was unavailable. Retry the request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The request references a resource that does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | A resource was in an inconsistent state during an update or a deletion.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request was denied because of request throttling. Retry the request.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The value of a parameter in the request caused an error.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

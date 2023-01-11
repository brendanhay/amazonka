{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Evidently.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Lens
  ( -- * Operations

    -- ** BatchEvaluateFeature
    batchEvaluateFeature_project,
    batchEvaluateFeature_requests,
    batchEvaluateFeatureResponse_results,
    batchEvaluateFeatureResponse_httpStatus,

    -- ** CreateExperiment
    createExperiment_description,
    createExperiment_onlineAbConfig,
    createExperiment_randomizationSalt,
    createExperiment_samplingRate,
    createExperiment_segment,
    createExperiment_tags,
    createExperiment_metricGoals,
    createExperiment_name,
    createExperiment_project,
    createExperiment_treatments,
    createExperimentResponse_httpStatus,
    createExperimentResponse_experiment,

    -- ** CreateFeature
    createFeature_defaultVariation,
    createFeature_description,
    createFeature_entityOverrides,
    createFeature_evaluationStrategy,
    createFeature_tags,
    createFeature_name,
    createFeature_project,
    createFeature_variations,
    createFeatureResponse_feature,
    createFeatureResponse_httpStatus,

    -- ** CreateLaunch
    createLaunch_description,
    createLaunch_metricMonitors,
    createLaunch_randomizationSalt,
    createLaunch_scheduledSplitsConfig,
    createLaunch_tags,
    createLaunch_groups,
    createLaunch_name,
    createLaunch_project,
    createLaunchResponse_httpStatus,
    createLaunchResponse_launch,

    -- ** CreateProject
    createProject_appConfigResource,
    createProject_dataDelivery,
    createProject_description,
    createProject_tags,
    createProject_name,
    createProjectResponse_httpStatus,
    createProjectResponse_project,

    -- ** CreateSegment
    createSegment_description,
    createSegment_tags,
    createSegment_name,
    createSegment_pattern,
    createSegmentResponse_httpStatus,
    createSegmentResponse_segment,

    -- ** DeleteExperiment
    deleteExperiment_experiment,
    deleteExperiment_project,
    deleteExperimentResponse_httpStatus,

    -- ** DeleteFeature
    deleteFeature_feature,
    deleteFeature_project,
    deleteFeatureResponse_httpStatus,

    -- ** DeleteLaunch
    deleteLaunch_launch,
    deleteLaunch_project,
    deleteLaunchResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_project,
    deleteProjectResponse_httpStatus,

    -- ** DeleteSegment
    deleteSegment_segment,
    deleteSegmentResponse_httpStatus,

    -- ** EvaluateFeature
    evaluateFeature_evaluationContext,
    evaluateFeature_entityId,
    evaluateFeature_feature,
    evaluateFeature_project,
    evaluateFeatureResponse_details,
    evaluateFeatureResponse_reason,
    evaluateFeatureResponse_value,
    evaluateFeatureResponse_variation,
    evaluateFeatureResponse_httpStatus,

    -- ** GetExperiment
    getExperiment_experiment,
    getExperiment_project,
    getExperimentResponse_experiment,
    getExperimentResponse_httpStatus,

    -- ** GetExperimentResults
    getExperimentResults_baseStat,
    getExperimentResults_endTime,
    getExperimentResults_period,
    getExperimentResults_reportNames,
    getExperimentResults_resultStats,
    getExperimentResults_startTime,
    getExperimentResults_experiment,
    getExperimentResults_metricNames,
    getExperimentResults_project,
    getExperimentResults_treatmentNames,
    getExperimentResultsResponse_details,
    getExperimentResultsResponse_reports,
    getExperimentResultsResponse_resultsData,
    getExperimentResultsResponse_timestamps,
    getExperimentResultsResponse_httpStatus,

    -- ** GetFeature
    getFeature_feature,
    getFeature_project,
    getFeatureResponse_httpStatus,
    getFeatureResponse_feature,

    -- ** GetLaunch
    getLaunch_launch,
    getLaunch_project,
    getLaunchResponse_launch,
    getLaunchResponse_httpStatus,

    -- ** GetProject
    getProject_project,
    getProjectResponse_httpStatus,
    getProjectResponse_project,

    -- ** GetSegment
    getSegment_segment,
    getSegmentResponse_httpStatus,
    getSegmentResponse_segment,

    -- ** ListExperiments
    listExperiments_maxResults,
    listExperiments_nextToken,
    listExperiments_status,
    listExperiments_project,
    listExperimentsResponse_experiments,
    listExperimentsResponse_nextToken,
    listExperimentsResponse_httpStatus,

    -- ** ListFeatures
    listFeatures_maxResults,
    listFeatures_nextToken,
    listFeatures_project,
    listFeaturesResponse_features,
    listFeaturesResponse_nextToken,
    listFeaturesResponse_httpStatus,

    -- ** ListLaunches
    listLaunches_maxResults,
    listLaunches_nextToken,
    listLaunches_status,
    listLaunches_project,
    listLaunchesResponse_launches,
    listLaunchesResponse_nextToken,
    listLaunchesResponse_httpStatus,

    -- ** ListProjects
    listProjects_maxResults,
    listProjects_nextToken,
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,

    -- ** ListSegmentReferences
    listSegmentReferences_maxResults,
    listSegmentReferences_nextToken,
    listSegmentReferences_segment,
    listSegmentReferences_type,
    listSegmentReferencesResponse_nextToken,
    listSegmentReferencesResponse_referencedBy,
    listSegmentReferencesResponse_httpStatus,

    -- ** ListSegments
    listSegments_maxResults,
    listSegments_nextToken,
    listSegmentsResponse_nextToken,
    listSegmentsResponse_segments,
    listSegmentsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutProjectEvents
    putProjectEvents_events,
    putProjectEvents_project,
    putProjectEventsResponse_eventResults,
    putProjectEventsResponse_failedEventCount,
    putProjectEventsResponse_httpStatus,

    -- ** StartExperiment
    startExperiment_analysisCompleteTime,
    startExperiment_experiment,
    startExperiment_project,
    startExperimentResponse_startedTime,
    startExperimentResponse_httpStatus,

    -- ** StartLaunch
    startLaunch_launch,
    startLaunch_project,
    startLaunchResponse_httpStatus,
    startLaunchResponse_launch,

    -- ** StopExperiment
    stopExperiment_desiredState,
    stopExperiment_reason,
    stopExperiment_experiment,
    stopExperiment_project,
    stopExperimentResponse_endedTime,
    stopExperimentResponse_httpStatus,

    -- ** StopLaunch
    stopLaunch_desiredState,
    stopLaunch_reason,
    stopLaunch_launch,
    stopLaunch_project,
    stopLaunchResponse_endedTime,
    stopLaunchResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TestSegmentPattern
    testSegmentPattern_pattern,
    testSegmentPattern_payload,
    testSegmentPatternResponse_httpStatus,
    testSegmentPatternResponse_match,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateExperiment
    updateExperiment_description,
    updateExperiment_metricGoals,
    updateExperiment_onlineAbConfig,
    updateExperiment_randomizationSalt,
    updateExperiment_removeSegment,
    updateExperiment_samplingRate,
    updateExperiment_segment,
    updateExperiment_treatments,
    updateExperiment_experiment,
    updateExperiment_project,
    updateExperimentResponse_httpStatus,
    updateExperimentResponse_experiment,

    -- ** UpdateFeature
    updateFeature_addOrUpdateVariations,
    updateFeature_defaultVariation,
    updateFeature_description,
    updateFeature_entityOverrides,
    updateFeature_evaluationStrategy,
    updateFeature_removeVariations,
    updateFeature_feature,
    updateFeature_project,
    updateFeatureResponse_httpStatus,
    updateFeatureResponse_feature,

    -- ** UpdateLaunch
    updateLaunch_description,
    updateLaunch_groups,
    updateLaunch_metricMonitors,
    updateLaunch_randomizationSalt,
    updateLaunch_scheduledSplitsConfig,
    updateLaunch_launch,
    updateLaunch_project,
    updateLaunchResponse_httpStatus,
    updateLaunchResponse_launch,

    -- ** UpdateProject
    updateProject_appConfigResource,
    updateProject_description,
    updateProject_project,
    updateProjectResponse_httpStatus,
    updateProjectResponse_project,

    -- ** UpdateProjectDataDelivery
    updateProjectDataDelivery_cloudWatchLogs,
    updateProjectDataDelivery_s3Destination,
    updateProjectDataDelivery_project,
    updateProjectDataDeliveryResponse_httpStatus,
    updateProjectDataDeliveryResponse_project,

    -- * Types

    -- ** CloudWatchLogsDestination
    cloudWatchLogsDestination_logGroup,

    -- ** CloudWatchLogsDestinationConfig
    cloudWatchLogsDestinationConfig_logGroup,

    -- ** EvaluationRequest
    evaluationRequest_evaluationContext,
    evaluationRequest_entityId,
    evaluationRequest_feature,

    -- ** EvaluationResult
    evaluationResult_details,
    evaluationResult_project,
    evaluationResult_reason,
    evaluationResult_value,
    evaluationResult_variation,
    evaluationResult_entityId,
    evaluationResult_feature,

    -- ** EvaluationRule
    evaluationRule_name,
    evaluationRule_type,

    -- ** Event
    event_data,
    event_timestamp,
    event_type,

    -- ** Experiment
    experiment_description,
    experiment_execution,
    experiment_metricGoals,
    experiment_onlineAbDefinition,
    experiment_project,
    experiment_randomizationSalt,
    experiment_samplingRate,
    experiment_schedule,
    experiment_segment,
    experiment_statusReason,
    experiment_tags,
    experiment_treatments,
    experiment_arn,
    experiment_createdTime,
    experiment_lastUpdatedTime,
    experiment_name,
    experiment_status,
    experiment_type,

    -- ** ExperimentExecution
    experimentExecution_endedTime,
    experimentExecution_startedTime,

    -- ** ExperimentReport
    experimentReport_content,
    experimentReport_metricName,
    experimentReport_reportName,
    experimentReport_treatmentName,

    -- ** ExperimentResultsData
    experimentResultsData_metricName,
    experimentResultsData_resultStat,
    experimentResultsData_treatmentName,
    experimentResultsData_values,

    -- ** ExperimentSchedule
    experimentSchedule_analysisCompleteTime,

    -- ** Feature
    feature_defaultVariation,
    feature_description,
    feature_entityOverrides,
    feature_evaluationRules,
    feature_project,
    feature_tags,
    feature_arn,
    feature_createdTime,
    feature_evaluationStrategy,
    feature_lastUpdatedTime,
    feature_name,
    feature_status,
    feature_valueType,
    feature_variations,

    -- ** FeatureSummary
    featureSummary_defaultVariation,
    featureSummary_evaluationRules,
    featureSummary_project,
    featureSummary_tags,
    featureSummary_arn,
    featureSummary_createdTime,
    featureSummary_evaluationStrategy,
    featureSummary_lastUpdatedTime,
    featureSummary_name,
    featureSummary_status,

    -- ** Launch
    launch_description,
    launch_execution,
    launch_groups,
    launch_metricMonitors,
    launch_project,
    launch_randomizationSalt,
    launch_scheduledSplitsDefinition,
    launch_statusReason,
    launch_tags,
    launch_arn,
    launch_createdTime,
    launch_lastUpdatedTime,
    launch_name,
    launch_status,
    launch_type,

    -- ** LaunchExecution
    launchExecution_endedTime,
    launchExecution_startedTime,

    -- ** LaunchGroup
    launchGroup_description,
    launchGroup_featureVariations,
    launchGroup_name,

    -- ** LaunchGroupConfig
    launchGroupConfig_description,
    launchGroupConfig_feature,
    launchGroupConfig_name,
    launchGroupConfig_variation,

    -- ** MetricDefinition
    metricDefinition_entityIdKey,
    metricDefinition_eventPattern,
    metricDefinition_name,
    metricDefinition_unitLabel,
    metricDefinition_valueKey,

    -- ** MetricDefinitionConfig
    metricDefinitionConfig_eventPattern,
    metricDefinitionConfig_unitLabel,
    metricDefinitionConfig_entityIdKey,
    metricDefinitionConfig_name,
    metricDefinitionConfig_valueKey,

    -- ** MetricGoal
    metricGoal_desiredChange,
    metricGoal_metricDefinition,

    -- ** MetricGoalConfig
    metricGoalConfig_desiredChange,
    metricGoalConfig_metricDefinition,

    -- ** MetricMonitor
    metricMonitor_metricDefinition,

    -- ** MetricMonitorConfig
    metricMonitorConfig_metricDefinition,

    -- ** OnlineAbConfig
    onlineAbConfig_controlTreatmentName,
    onlineAbConfig_treatmentWeights,

    -- ** OnlineAbDefinition
    onlineAbDefinition_controlTreatmentName,
    onlineAbDefinition_treatmentWeights,

    -- ** Project
    project_activeExperimentCount,
    project_activeLaunchCount,
    project_appConfigResource,
    project_dataDelivery,
    project_description,
    project_experimentCount,
    project_featureCount,
    project_launchCount,
    project_tags,
    project_arn,
    project_createdTime,
    project_lastUpdatedTime,
    project_name,
    project_status,

    -- ** ProjectAppConfigResource
    projectAppConfigResource_applicationId,
    projectAppConfigResource_configurationProfileId,
    projectAppConfigResource_environmentId,

    -- ** ProjectAppConfigResourceConfig
    projectAppConfigResourceConfig_applicationId,
    projectAppConfigResourceConfig_environmentId,

    -- ** ProjectDataDelivery
    projectDataDelivery_cloudWatchLogs,
    projectDataDelivery_s3Destination,

    -- ** ProjectDataDeliveryConfig
    projectDataDeliveryConfig_cloudWatchLogs,
    projectDataDeliveryConfig_s3Destination,

    -- ** ProjectSummary
    projectSummary_activeExperimentCount,
    projectSummary_activeLaunchCount,
    projectSummary_description,
    projectSummary_experimentCount,
    projectSummary_featureCount,
    projectSummary_launchCount,
    projectSummary_tags,
    projectSummary_arn,
    projectSummary_createdTime,
    projectSummary_lastUpdatedTime,
    projectSummary_name,
    projectSummary_status,

    -- ** PutProjectEventsResultEntry
    putProjectEventsResultEntry_errorCode,
    putProjectEventsResultEntry_errorMessage,
    putProjectEventsResultEntry_eventId,

    -- ** RefResource
    refResource_arn,
    refResource_endTime,
    refResource_lastUpdatedOn,
    refResource_startTime,
    refResource_status,
    refResource_name,
    refResource_type,

    -- ** S3Destination
    s3Destination_bucket,
    s3Destination_prefix,

    -- ** S3DestinationConfig
    s3DestinationConfig_bucket,
    s3DestinationConfig_prefix,

    -- ** ScheduledSplit
    scheduledSplit_groupWeights,
    scheduledSplit_segmentOverrides,
    scheduledSplit_startTime,

    -- ** ScheduledSplitConfig
    scheduledSplitConfig_segmentOverrides,
    scheduledSplitConfig_groupWeights,
    scheduledSplitConfig_startTime,

    -- ** ScheduledSplitsLaunchConfig
    scheduledSplitsLaunchConfig_steps,

    -- ** ScheduledSplitsLaunchDefinition
    scheduledSplitsLaunchDefinition_steps,

    -- ** Segment
    segment_description,
    segment_experimentCount,
    segment_launchCount,
    segment_tags,
    segment_arn,
    segment_createdTime,
    segment_lastUpdatedTime,
    segment_name,
    segment_pattern,

    -- ** SegmentOverride
    segmentOverride_evaluationOrder,
    segmentOverride_segment,
    segmentOverride_weights,

    -- ** Treatment
    treatment_description,
    treatment_featureVariations,
    treatment_name,

    -- ** TreatmentConfig
    treatmentConfig_description,
    treatmentConfig_feature,
    treatmentConfig_name,
    treatmentConfig_variation,

    -- ** VariableValue
    variableValue_boolValue,
    variableValue_doubleValue,
    variableValue_longValue,
    variableValue_stringValue,

    -- ** Variation
    variation_name,
    variation_value,

    -- ** VariationConfig
    variationConfig_name,
    variationConfig_value,
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
import Amazonka.Evidently.Types.CloudWatchLogsDestination
import Amazonka.Evidently.Types.CloudWatchLogsDestinationConfig
import Amazonka.Evidently.Types.EvaluationRequest
import Amazonka.Evidently.Types.EvaluationResult
import Amazonka.Evidently.Types.EvaluationRule
import Amazonka.Evidently.Types.Event
import Amazonka.Evidently.Types.Experiment
import Amazonka.Evidently.Types.ExperimentExecution
import Amazonka.Evidently.Types.ExperimentReport
import Amazonka.Evidently.Types.ExperimentResultsData
import Amazonka.Evidently.Types.ExperimentSchedule
import Amazonka.Evidently.Types.Feature
import Amazonka.Evidently.Types.FeatureSummary
import Amazonka.Evidently.Types.Launch
import Amazonka.Evidently.Types.LaunchExecution
import Amazonka.Evidently.Types.LaunchGroup
import Amazonka.Evidently.Types.LaunchGroupConfig
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
import Amazonka.Evidently.Types.Treatment
import Amazonka.Evidently.Types.TreatmentConfig
import Amazonka.Evidently.Types.VariableValue
import Amazonka.Evidently.Types.Variation
import Amazonka.Evidently.Types.VariationConfig
import Amazonka.Evidently.UntagResource
import Amazonka.Evidently.UpdateExperiment
import Amazonka.Evidently.UpdateFeature
import Amazonka.Evidently.UpdateLaunch
import Amazonka.Evidently.UpdateProject
import Amazonka.Evidently.UpdateProjectDataDelivery

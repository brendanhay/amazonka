{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeGuruProfiler.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Lens
  ( -- * Operations

    -- ** AddNotificationChannels
    addNotificationChannels_channels,
    addNotificationChannels_profilingGroupName,
    addNotificationChannelsResponse_notificationConfiguration,
    addNotificationChannelsResponse_httpStatus,

    -- ** BatchGetFrameMetricData
    batchGetFrameMetricData_period,
    batchGetFrameMetricData_endTime,
    batchGetFrameMetricData_frameMetrics,
    batchGetFrameMetricData_targetResolution,
    batchGetFrameMetricData_startTime,
    batchGetFrameMetricData_profilingGroupName,
    batchGetFrameMetricDataResponse_httpStatus,
    batchGetFrameMetricDataResponse_endTime,
    batchGetFrameMetricDataResponse_endTimes,
    batchGetFrameMetricDataResponse_frameMetricData,
    batchGetFrameMetricDataResponse_resolution,
    batchGetFrameMetricDataResponse_startTime,
    batchGetFrameMetricDataResponse_unprocessedEndTimes,

    -- ** ConfigureAgent
    configureAgent_metadata,
    configureAgent_fleetInstanceId,
    configureAgent_profilingGroupName,
    configureAgentResponse_httpStatus,
    configureAgentResponse_configuration,

    -- ** CreateProfilingGroup
    createProfilingGroup_tags,
    createProfilingGroup_computePlatform,
    createProfilingGroup_agentOrchestrationConfig,
    createProfilingGroup_clientToken,
    createProfilingGroup_profilingGroupName,
    createProfilingGroupResponse_httpStatus,
    createProfilingGroupResponse_profilingGroup,

    -- ** DeleteProfilingGroup
    deleteProfilingGroup_profilingGroupName,
    deleteProfilingGroupResponse_httpStatus,

    -- ** DescribeProfilingGroup
    describeProfilingGroup_profilingGroupName,
    describeProfilingGroupResponse_httpStatus,
    describeProfilingGroupResponse_profilingGroup,

    -- ** GetFindingsReportAccountSummary
    getFindingsReportAccountSummary_nextToken,
    getFindingsReportAccountSummary_maxResults,
    getFindingsReportAccountSummary_dailyReportsOnly,
    getFindingsReportAccountSummaryResponse_nextToken,
    getFindingsReportAccountSummaryResponse_httpStatus,
    getFindingsReportAccountSummaryResponse_reportSummaries,

    -- ** GetNotificationConfiguration
    getNotificationConfiguration_profilingGroupName,
    getNotificationConfigurationResponse_httpStatus,
    getNotificationConfigurationResponse_notificationConfiguration,

    -- ** GetPolicy
    getPolicy_profilingGroupName,
    getPolicyResponse_httpStatus,
    getPolicyResponse_policy,
    getPolicyResponse_revisionId,

    -- ** GetProfile
    getProfile_accept,
    getProfile_period,
    getProfile_maxDepth,
    getProfile_endTime,
    getProfile_startTime,
    getProfile_profilingGroupName,
    getProfileResponse_contentEncoding,
    getProfileResponse_httpStatus,
    getProfileResponse_contentType,
    getProfileResponse_profile,

    -- ** GetRecommendations
    getRecommendations_locale,
    getRecommendations_endTime,
    getRecommendations_profilingGroupName,
    getRecommendations_startTime,
    getRecommendationsResponse_httpStatus,
    getRecommendationsResponse_anomalies,
    getRecommendationsResponse_profileEndTime,
    getRecommendationsResponse_profileStartTime,
    getRecommendationsResponse_profilingGroupName,
    getRecommendationsResponse_recommendations,

    -- ** ListFindingsReports
    listFindingsReports_nextToken,
    listFindingsReports_maxResults,
    listFindingsReports_dailyReportsOnly,
    listFindingsReports_endTime,
    listFindingsReports_profilingGroupName,
    listFindingsReports_startTime,
    listFindingsReportsResponse_nextToken,
    listFindingsReportsResponse_httpStatus,
    listFindingsReportsResponse_findingsReportSummaries,

    -- ** ListProfileTimes
    listProfileTimes_nextToken,
    listProfileTimes_maxResults,
    listProfileTimes_orderBy,
    listProfileTimes_endTime,
    listProfileTimes_period,
    listProfileTimes_profilingGroupName,
    listProfileTimes_startTime,
    listProfileTimesResponse_nextToken,
    listProfileTimesResponse_httpStatus,
    listProfileTimesResponse_profileTimes,

    -- ** ListProfilingGroups
    listProfilingGroups_nextToken,
    listProfilingGroups_maxResults,
    listProfilingGroups_includeDescription,
    listProfilingGroupsResponse_nextToken,
    listProfilingGroupsResponse_profilingGroups,
    listProfilingGroupsResponse_httpStatus,
    listProfilingGroupsResponse_profilingGroupNames,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PostAgentProfile
    postAgentProfile_profileToken,
    postAgentProfile_agentProfile,
    postAgentProfile_contentType,
    postAgentProfile_profilingGroupName,
    postAgentProfileResponse_httpStatus,

    -- ** PutPermission
    putPermission_revisionId,
    putPermission_actionGroup,
    putPermission_principals,
    putPermission_profilingGroupName,
    putPermissionResponse_httpStatus,
    putPermissionResponse_policy,
    putPermissionResponse_revisionId,

    -- ** RemoveNotificationChannel
    removeNotificationChannel_channelId,
    removeNotificationChannel_profilingGroupName,
    removeNotificationChannelResponse_notificationConfiguration,
    removeNotificationChannelResponse_httpStatus,

    -- ** RemovePermission
    removePermission_actionGroup,
    removePermission_profilingGroupName,
    removePermission_revisionId,
    removePermissionResponse_httpStatus,
    removePermissionResponse_policy,
    removePermissionResponse_revisionId,

    -- ** SubmitFeedback
    submitFeedback_comment,
    submitFeedback_anomalyInstanceId,
    submitFeedback_profilingGroupName,
    submitFeedback_type,
    submitFeedbackResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateProfilingGroup
    updateProfilingGroup_agentOrchestrationConfig,
    updateProfilingGroup_profilingGroupName,
    updateProfilingGroupResponse_httpStatus,
    updateProfilingGroupResponse_profilingGroup,

    -- * Types

    -- ** AgentConfiguration
    agentConfiguration_agentParameters,
    agentConfiguration_periodInSeconds,
    agentConfiguration_shouldProfile,

    -- ** AgentOrchestrationConfig
    agentOrchestrationConfig_profilingEnabled,

    -- ** AggregatedProfileTime
    aggregatedProfileTime_start,
    aggregatedProfileTime_period,

    -- ** Anomaly
    anomaly_instances,
    anomaly_metric,
    anomaly_reason,

    -- ** AnomalyInstance
    anomalyInstance_userFeedback,
    anomalyInstance_endTime,
    anomalyInstance_id,
    anomalyInstance_startTime,

    -- ** Channel
    channel_id,
    channel_eventPublishers,
    channel_uri,

    -- ** FindingsReportSummary
    findingsReportSummary_profileStartTime,
    findingsReportSummary_totalNumberOfFindings,
    findingsReportSummary_id,
    findingsReportSummary_profilingGroupName,
    findingsReportSummary_profileEndTime,

    -- ** FrameMetric
    frameMetric_frameName,
    frameMetric_threadStates,
    frameMetric_type,

    -- ** FrameMetricDatum
    frameMetricDatum_frameMetric,
    frameMetricDatum_values,

    -- ** Match
    match_thresholdBreachValue,
    match_frameAddress,
    match_targetFramesIndex,

    -- ** Metric
    metric_frameName,
    metric_threadStates,
    metric_type,

    -- ** NotificationConfiguration
    notificationConfiguration_channels,

    -- ** Pattern
    pattern_name,
    pattern_countersToAggregate,
    pattern_description,
    pattern_id,
    pattern_targetFrames,
    pattern_thresholdPercent,
    pattern_resolutionSteps,

    -- ** ProfileTime
    profileTime_start,

    -- ** ProfilingGroupDescription
    profilingGroupDescription_tags,
    profilingGroupDescription_name,
    profilingGroupDescription_arn,
    profilingGroupDescription_computePlatform,
    profilingGroupDescription_profilingStatus,
    profilingGroupDescription_createdAt,
    profilingGroupDescription_updatedAt,
    profilingGroupDescription_agentOrchestrationConfig,

    -- ** ProfilingStatus
    profilingStatus_latestAgentOrchestratedAt,
    profilingStatus_latestAgentProfileReportedAt,
    profilingStatus_latestAggregatedProfile,

    -- ** Recommendation
    recommendation_allMatchesCount,
    recommendation_allMatchesSum,
    recommendation_endTime,
    recommendation_pattern,
    recommendation_startTime,
    recommendation_topMatches,

    -- ** TimestampStructure
    timestampStructure_value,

    -- ** UserFeedback
    userFeedback_type,
  )
where

import Amazonka.CodeGuruProfiler.AddNotificationChannels
import Amazonka.CodeGuruProfiler.BatchGetFrameMetricData
import Amazonka.CodeGuruProfiler.ConfigureAgent
import Amazonka.CodeGuruProfiler.CreateProfilingGroup
import Amazonka.CodeGuruProfiler.DeleteProfilingGroup
import Amazonka.CodeGuruProfiler.DescribeProfilingGroup
import Amazonka.CodeGuruProfiler.GetFindingsReportAccountSummary
import Amazonka.CodeGuruProfiler.GetNotificationConfiguration
import Amazonka.CodeGuruProfiler.GetPolicy
import Amazonka.CodeGuruProfiler.GetProfile
import Amazonka.CodeGuruProfiler.GetRecommendations
import Amazonka.CodeGuruProfiler.ListFindingsReports
import Amazonka.CodeGuruProfiler.ListProfileTimes
import Amazonka.CodeGuruProfiler.ListProfilingGroups
import Amazonka.CodeGuruProfiler.ListTagsForResource
import Amazonka.CodeGuruProfiler.PostAgentProfile
import Amazonka.CodeGuruProfiler.PutPermission
import Amazonka.CodeGuruProfiler.RemoveNotificationChannel
import Amazonka.CodeGuruProfiler.RemovePermission
import Amazonka.CodeGuruProfiler.SubmitFeedback
import Amazonka.CodeGuruProfiler.TagResource
import Amazonka.CodeGuruProfiler.Types.AgentConfiguration
import Amazonka.CodeGuruProfiler.Types.AgentOrchestrationConfig
import Amazonka.CodeGuruProfiler.Types.AggregatedProfileTime
import Amazonka.CodeGuruProfiler.Types.Anomaly
import Amazonka.CodeGuruProfiler.Types.AnomalyInstance
import Amazonka.CodeGuruProfiler.Types.Channel
import Amazonka.CodeGuruProfiler.Types.FindingsReportSummary
import Amazonka.CodeGuruProfiler.Types.FrameMetric
import Amazonka.CodeGuruProfiler.Types.FrameMetricDatum
import Amazonka.CodeGuruProfiler.Types.Match
import Amazonka.CodeGuruProfiler.Types.Metric
import Amazonka.CodeGuruProfiler.Types.NotificationConfiguration
import Amazonka.CodeGuruProfiler.Types.Pattern
import Amazonka.CodeGuruProfiler.Types.ProfileTime
import Amazonka.CodeGuruProfiler.Types.ProfilingGroupDescription
import Amazonka.CodeGuruProfiler.Types.ProfilingStatus
import Amazonka.CodeGuruProfiler.Types.Recommendation
import Amazonka.CodeGuruProfiler.Types.TimestampStructure
import Amazonka.CodeGuruProfiler.Types.UserFeedback
import Amazonka.CodeGuruProfiler.UntagResource
import Amazonka.CodeGuruProfiler.UpdateProfilingGroup

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.XRay.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Lens
  ( -- * Operations

    -- ** BatchGetTraces
    batchGetTraces_nextToken,
    batchGetTraces_traceIds,
    batchGetTracesResponse_nextToken,
    batchGetTracesResponse_traces,
    batchGetTracesResponse_unprocessedTraceIds,
    batchGetTracesResponse_httpStatus,

    -- ** CreateGroup
    createGroup_tags,
    createGroup_insightsConfiguration,
    createGroup_filterExpression,
    createGroup_groupName,
    createGroupResponse_group,
    createGroupResponse_httpStatus,

    -- ** CreateSamplingRule
    createSamplingRule_tags,
    createSamplingRule_samplingRule,
    createSamplingRuleResponse_samplingRuleRecord,
    createSamplingRuleResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupName,
    deleteGroup_groupARN,
    deleteGroupResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_policyRevisionId,
    deleteResourcePolicy_policyName,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteSamplingRule
    deleteSamplingRule_ruleARN,
    deleteSamplingRule_ruleName,
    deleteSamplingRuleResponse_samplingRuleRecord,
    deleteSamplingRuleResponse_httpStatus,

    -- ** GetEncryptionConfig
    getEncryptionConfigResponse_encryptionConfig,
    getEncryptionConfigResponse_httpStatus,

    -- ** GetGroup
    getGroup_groupName,
    getGroup_groupARN,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** GetGroups
    getGroups_nextToken,
    getGroupsResponse_nextToken,
    getGroupsResponse_groups,
    getGroupsResponse_httpStatus,

    -- ** GetInsight
    getInsight_insightId,
    getInsightResponse_insight,
    getInsightResponse_httpStatus,

    -- ** GetInsightEvents
    getInsightEvents_nextToken,
    getInsightEvents_maxResults,
    getInsightEvents_insightId,
    getInsightEventsResponse_nextToken,
    getInsightEventsResponse_insightEvents,
    getInsightEventsResponse_httpStatus,

    -- ** GetInsightImpactGraph
    getInsightImpactGraph_nextToken,
    getInsightImpactGraph_insightId,
    getInsightImpactGraph_startTime,
    getInsightImpactGraph_endTime,
    getInsightImpactGraphResponse_nextToken,
    getInsightImpactGraphResponse_insightId,
    getInsightImpactGraphResponse_endTime,
    getInsightImpactGraphResponse_services,
    getInsightImpactGraphResponse_serviceGraphEndTime,
    getInsightImpactGraphResponse_serviceGraphStartTime,
    getInsightImpactGraphResponse_startTime,
    getInsightImpactGraphResponse_httpStatus,

    -- ** GetInsightSummaries
    getInsightSummaries_nextToken,
    getInsightSummaries_groupName,
    getInsightSummaries_maxResults,
    getInsightSummaries_states,
    getInsightSummaries_groupARN,
    getInsightSummaries_startTime,
    getInsightSummaries_endTime,
    getInsightSummariesResponse_nextToken,
    getInsightSummariesResponse_insightSummaries,
    getInsightSummariesResponse_httpStatus,

    -- ** GetSamplingRules
    getSamplingRules_nextToken,
    getSamplingRulesResponse_nextToken,
    getSamplingRulesResponse_samplingRuleRecords,
    getSamplingRulesResponse_httpStatus,

    -- ** GetSamplingStatisticSummaries
    getSamplingStatisticSummaries_nextToken,
    getSamplingStatisticSummariesResponse_samplingStatisticSummaries,
    getSamplingStatisticSummariesResponse_nextToken,
    getSamplingStatisticSummariesResponse_httpStatus,

    -- ** GetSamplingTargets
    getSamplingTargets_samplingStatisticsDocuments,
    getSamplingTargetsResponse_unprocessedStatistics,
    getSamplingTargetsResponse_samplingTargetDocuments,
    getSamplingTargetsResponse_lastRuleModification,
    getSamplingTargetsResponse_httpStatus,

    -- ** GetServiceGraph
    getServiceGraph_nextToken,
    getServiceGraph_groupName,
    getServiceGraph_groupARN,
    getServiceGraph_startTime,
    getServiceGraph_endTime,
    getServiceGraphResponse_nextToken,
    getServiceGraphResponse_endTime,
    getServiceGraphResponse_services,
    getServiceGraphResponse_containsOldGroupVersions,
    getServiceGraphResponse_startTime,
    getServiceGraphResponse_httpStatus,

    -- ** GetTimeSeriesServiceStatistics
    getTimeSeriesServiceStatistics_nextToken,
    getTimeSeriesServiceStatistics_entitySelectorExpression,
    getTimeSeriesServiceStatistics_forecastStatistics,
    getTimeSeriesServiceStatistics_period,
    getTimeSeriesServiceStatistics_groupName,
    getTimeSeriesServiceStatistics_groupARN,
    getTimeSeriesServiceStatistics_startTime,
    getTimeSeriesServiceStatistics_endTime,
    getTimeSeriesServiceStatisticsResponse_nextToken,
    getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics,
    getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions,
    getTimeSeriesServiceStatisticsResponse_httpStatus,

    -- ** GetTraceGraph
    getTraceGraph_nextToken,
    getTraceGraph_traceIds,
    getTraceGraphResponse_nextToken,
    getTraceGraphResponse_services,
    getTraceGraphResponse_httpStatus,

    -- ** GetTraceSummaries
    getTraceSummaries_nextToken,
    getTraceSummaries_timeRangeType,
    getTraceSummaries_filterExpression,
    getTraceSummaries_samplingStrategy,
    getTraceSummaries_sampling,
    getTraceSummaries_startTime,
    getTraceSummaries_endTime,
    getTraceSummariesResponse_nextToken,
    getTraceSummariesResponse_tracesProcessedCount,
    getTraceSummariesResponse_traceSummaries,
    getTraceSummariesResponse_approximateTime,
    getTraceSummariesResponse_httpStatus,

    -- ** ListResourcePolicies
    listResourcePolicies_nextToken,
    listResourcePoliciesResponse_nextToken,
    listResourcePoliciesResponse_resourcePolicies,
    listResourcePoliciesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** PutEncryptionConfig
    putEncryptionConfig_keyId,
    putEncryptionConfig_type,
    putEncryptionConfigResponse_encryptionConfig,
    putEncryptionConfigResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policyRevisionId,
    putResourcePolicy_bypassPolicyLockoutCheck,
    putResourcePolicy_policyName,
    putResourcePolicy_policyDocument,
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,

    -- ** PutTelemetryRecords
    putTelemetryRecords_eC2InstanceId,
    putTelemetryRecords_hostname,
    putTelemetryRecords_resourceARN,
    putTelemetryRecords_telemetryRecords,
    putTelemetryRecordsResponse_httpStatus,

    -- ** PutTraceSegments
    putTraceSegments_traceSegmentDocuments,
    putTraceSegmentsResponse_unprocessedTraceSegments,
    putTraceSegmentsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_insightsConfiguration,
    updateGroup_filterExpression,
    updateGroup_groupName,
    updateGroup_groupARN,
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,

    -- ** UpdateSamplingRule
    updateSamplingRule_samplingRuleUpdate,
    updateSamplingRuleResponse_samplingRuleRecord,
    updateSamplingRuleResponse_httpStatus,

    -- * Types

    -- ** Alias
    alias_name,
    alias_type,
    alias_names,

    -- ** AnnotationValue
    annotationValue_numberValue,
    annotationValue_booleanValue,
    annotationValue_stringValue,

    -- ** AnomalousService
    anomalousService_serviceId,

    -- ** AvailabilityZoneDetail
    availabilityZoneDetail_name,

    -- ** BackendConnectionErrors
    backendConnectionErrors_otherCount,
    backendConnectionErrors_unknownHostCount,
    backendConnectionErrors_hTTPCode4XXCount,
    backendConnectionErrors_connectionRefusedCount,
    backendConnectionErrors_timeoutCount,
    backendConnectionErrors_hTTPCode5XXCount,

    -- ** Edge
    edge_aliases,
    edge_summaryStatistics,
    edge_endTime,
    edge_responseTimeHistogram,
    edge_edgeType,
    edge_referenceId,
    edge_startTime,
    edge_receivedEventAgeHistogram,

    -- ** EdgeStatistics
    edgeStatistics_faultStatistics,
    edgeStatistics_okCount,
    edgeStatistics_totalResponseTime,
    edgeStatistics_totalCount,
    edgeStatistics_errorStatistics,

    -- ** EncryptionConfig
    encryptionConfig_type,
    encryptionConfig_status,
    encryptionConfig_keyId,

    -- ** ErrorRootCause
    errorRootCause_services,
    errorRootCause_clientImpacting,

    -- ** ErrorRootCauseEntity
    errorRootCauseEntity_exceptions,
    errorRootCauseEntity_name,
    errorRootCauseEntity_remote,

    -- ** ErrorRootCauseService
    errorRootCauseService_name,
    errorRootCauseService_type,
    errorRootCauseService_entityPath,
    errorRootCauseService_names,
    errorRootCauseService_accountId,
    errorRootCauseService_inferred,

    -- ** ErrorStatistics
    errorStatistics_otherCount,
    errorStatistics_totalCount,
    errorStatistics_throttleCount,

    -- ** FaultRootCause
    faultRootCause_services,
    faultRootCause_clientImpacting,

    -- ** FaultRootCauseEntity
    faultRootCauseEntity_exceptions,
    faultRootCauseEntity_name,
    faultRootCauseEntity_remote,

    -- ** FaultRootCauseService
    faultRootCauseService_name,
    faultRootCauseService_type,
    faultRootCauseService_entityPath,
    faultRootCauseService_names,
    faultRootCauseService_accountId,
    faultRootCauseService_inferred,

    -- ** FaultStatistics
    faultStatistics_otherCount,
    faultStatistics_totalCount,

    -- ** ForecastStatistics
    forecastStatistics_faultCountLow,
    forecastStatistics_faultCountHigh,

    -- ** Group
    group_insightsConfiguration,
    group_filterExpression,
    group_groupName,
    group_groupARN,

    -- ** GroupSummary
    groupSummary_insightsConfiguration,
    groupSummary_filterExpression,
    groupSummary_groupName,
    groupSummary_groupARN,

    -- ** HistogramEntry
    histogramEntry_count,
    histogramEntry_value,

    -- ** Http
    http_httpURL,
    http_httpStatus,
    http_httpMethod,
    http_userAgent,
    http_clientIp,

    -- ** Insight
    insight_rootCauseServiceId,
    insight_rootCauseServiceRequestImpactStatistics,
    insight_topAnomalousServices,
    insight_insightId,
    insight_state,
    insight_summary,
    insight_groupName,
    insight_endTime,
    insight_categories,
    insight_clientRequestImpactStatistics,
    insight_groupARN,
    insight_startTime,

    -- ** InsightEvent
    insightEvent_rootCauseServiceRequestImpactStatistics,
    insightEvent_topAnomalousServices,
    insightEvent_summary,
    insightEvent_eventTime,
    insightEvent_clientRequestImpactStatistics,

    -- ** InsightImpactGraphEdge
    insightImpactGraphEdge_referenceId,

    -- ** InsightImpactGraphService
    insightImpactGraphService_edges,
    insightImpactGraphService_name,
    insightImpactGraphService_type,
    insightImpactGraphService_names,
    insightImpactGraphService_accountId,
    insightImpactGraphService_referenceId,

    -- ** InsightSummary
    insightSummary_rootCauseServiceId,
    insightSummary_rootCauseServiceRequestImpactStatistics,
    insightSummary_topAnomalousServices,
    insightSummary_insightId,
    insightSummary_state,
    insightSummary_summary,
    insightSummary_groupName,
    insightSummary_endTime,
    insightSummary_lastUpdateTime,
    insightSummary_categories,
    insightSummary_clientRequestImpactStatistics,
    insightSummary_groupARN,
    insightSummary_startTime,

    -- ** InsightsConfiguration
    insightsConfiguration_insightsEnabled,
    insightsConfiguration_notificationsEnabled,

    -- ** InstanceIdDetail
    instanceIdDetail_id,

    -- ** RequestImpactStatistics
    requestImpactStatistics_okCount,
    requestImpactStatistics_totalCount,
    requestImpactStatistics_faultCount,

    -- ** ResourceARNDetail
    resourceARNDetail_arn,

    -- ** ResourcePolicy
    resourcePolicy_policyName,
    resourcePolicy_lastUpdatedTime,
    resourcePolicy_policyRevisionId,
    resourcePolicy_policyDocument,

    -- ** ResponseTimeRootCause
    responseTimeRootCause_services,
    responseTimeRootCause_clientImpacting,

    -- ** ResponseTimeRootCauseEntity
    responseTimeRootCauseEntity_name,
    responseTimeRootCauseEntity_coverage,
    responseTimeRootCauseEntity_remote,

    -- ** ResponseTimeRootCauseService
    responseTimeRootCauseService_name,
    responseTimeRootCauseService_type,
    responseTimeRootCauseService_entityPath,
    responseTimeRootCauseService_names,
    responseTimeRootCauseService_accountId,
    responseTimeRootCauseService_inferred,

    -- ** RootCauseException
    rootCauseException_message,
    rootCauseException_name,

    -- ** SamplingRule
    samplingRule_ruleARN,
    samplingRule_ruleName,
    samplingRule_attributes,
    samplingRule_resourceARN,
    samplingRule_priority,
    samplingRule_fixedRate,
    samplingRule_reservoirSize,
    samplingRule_serviceName,
    samplingRule_serviceType,
    samplingRule_host,
    samplingRule_hTTPMethod,
    samplingRule_uRLPath,
    samplingRule_version,

    -- ** SamplingRuleRecord
    samplingRuleRecord_modifiedAt,
    samplingRuleRecord_samplingRule,
    samplingRuleRecord_createdAt,

    -- ** SamplingRuleUpdate
    samplingRuleUpdate_ruleARN,
    samplingRuleUpdate_fixedRate,
    samplingRuleUpdate_host,
    samplingRuleUpdate_reservoirSize,
    samplingRuleUpdate_serviceType,
    samplingRuleUpdate_hTTPMethod,
    samplingRuleUpdate_ruleName,
    samplingRuleUpdate_priority,
    samplingRuleUpdate_attributes,
    samplingRuleUpdate_resourceARN,
    samplingRuleUpdate_serviceName,
    samplingRuleUpdate_uRLPath,

    -- ** SamplingStatisticSummary
    samplingStatisticSummary_borrowCount,
    samplingStatisticSummary_timestamp,
    samplingStatisticSummary_requestCount,
    samplingStatisticSummary_ruleName,
    samplingStatisticSummary_sampledCount,

    -- ** SamplingStatisticsDocument
    samplingStatisticsDocument_borrowCount,
    samplingStatisticsDocument_ruleName,
    samplingStatisticsDocument_clientID,
    samplingStatisticsDocument_timestamp,
    samplingStatisticsDocument_requestCount,
    samplingStatisticsDocument_sampledCount,

    -- ** SamplingStrategy
    samplingStrategy_name,
    samplingStrategy_value,

    -- ** SamplingTargetDocument
    samplingTargetDocument_fixedRate,
    samplingTargetDocument_reservoirQuota,
    samplingTargetDocument_interval,
    samplingTargetDocument_reservoirQuotaTTL,
    samplingTargetDocument_ruleName,

    -- ** Segment
    segment_id,
    segment_document,

    -- ** ServiceId
    serviceId_name,
    serviceId_type,
    serviceId_names,
    serviceId_accountId,

    -- ** ServiceInfo
    serviceInfo_edges,
    serviceInfo_name,
    serviceInfo_type,
    serviceInfo_summaryStatistics,
    serviceInfo_state,
    serviceInfo_endTime,
    serviceInfo_names,
    serviceInfo_responseTimeHistogram,
    serviceInfo_accountId,
    serviceInfo_root,
    serviceInfo_referenceId,
    serviceInfo_durationHistogram,
    serviceInfo_startTime,

    -- ** ServiceStatistics
    serviceStatistics_faultStatistics,
    serviceStatistics_okCount,
    serviceStatistics_totalResponseTime,
    serviceStatistics_totalCount,
    serviceStatistics_errorStatistics,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TelemetryRecord
    telemetryRecord_backendConnectionErrors,
    telemetryRecord_segmentsSentCount,
    telemetryRecord_segmentsSpilloverCount,
    telemetryRecord_segmentsReceivedCount,
    telemetryRecord_segmentsRejectedCount,
    telemetryRecord_timestamp,

    -- ** TimeSeriesServiceStatistics
    timeSeriesServiceStatistics_serviceSummaryStatistics,
    timeSeriesServiceStatistics_edgeSummaryStatistics,
    timeSeriesServiceStatistics_timestamp,
    timeSeriesServiceStatistics_responseTimeHistogram,
    timeSeriesServiceStatistics_serviceForecastStatistics,

    -- ** Trace
    trace_id,
    trace_duration,
    trace_limitExceeded,
    trace_segments,

    -- ** TraceSummary
    traceSummary_http,
    traceSummary_responseTimeRootCauses,
    traceSummary_hasFault,
    traceSummary_isPartial,
    traceSummary_availabilityZones,
    traceSummary_matchedEventTime,
    traceSummary_revision,
    traceSummary_errorRootCauses,
    traceSummary_users,
    traceSummary_hasError,
    traceSummary_id,
    traceSummary_annotations,
    traceSummary_duration,
    traceSummary_hasThrottle,
    traceSummary_faultRootCauses,
    traceSummary_serviceIds,
    traceSummary_responseTime,
    traceSummary_entryPoint,
    traceSummary_instanceIds,
    traceSummary_resourceARNs,

    -- ** TraceUser
    traceUser_userName,
    traceUser_serviceIds,

    -- ** UnprocessedStatistics
    unprocessedStatistics_message,
    unprocessedStatistics_ruleName,
    unprocessedStatistics_errorCode,

    -- ** UnprocessedTraceSegment
    unprocessedTraceSegment_message,
    unprocessedTraceSegment_id,
    unprocessedTraceSegment_errorCode,

    -- ** ValueWithServiceIds
    valueWithServiceIds_serviceIds,
    valueWithServiceIds_annotationValue,
  )
where

import Amazonka.XRay.BatchGetTraces
import Amazonka.XRay.CreateGroup
import Amazonka.XRay.CreateSamplingRule
import Amazonka.XRay.DeleteGroup
import Amazonka.XRay.DeleteResourcePolicy
import Amazonka.XRay.DeleteSamplingRule
import Amazonka.XRay.GetEncryptionConfig
import Amazonka.XRay.GetGroup
import Amazonka.XRay.GetGroups
import Amazonka.XRay.GetInsight
import Amazonka.XRay.GetInsightEvents
import Amazonka.XRay.GetInsightImpactGraph
import Amazonka.XRay.GetInsightSummaries
import Amazonka.XRay.GetSamplingRules
import Amazonka.XRay.GetSamplingStatisticSummaries
import Amazonka.XRay.GetSamplingTargets
import Amazonka.XRay.GetServiceGraph
import Amazonka.XRay.GetTimeSeriesServiceStatistics
import Amazonka.XRay.GetTraceGraph
import Amazonka.XRay.GetTraceSummaries
import Amazonka.XRay.ListResourcePolicies
import Amazonka.XRay.ListTagsForResource
import Amazonka.XRay.PutEncryptionConfig
import Amazonka.XRay.PutResourcePolicy
import Amazonka.XRay.PutTelemetryRecords
import Amazonka.XRay.PutTraceSegments
import Amazonka.XRay.TagResource
import Amazonka.XRay.Types.Alias
import Amazonka.XRay.Types.AnnotationValue
import Amazonka.XRay.Types.AnomalousService
import Amazonka.XRay.Types.AvailabilityZoneDetail
import Amazonka.XRay.Types.BackendConnectionErrors
import Amazonka.XRay.Types.Edge
import Amazonka.XRay.Types.EdgeStatistics
import Amazonka.XRay.Types.EncryptionConfig
import Amazonka.XRay.Types.ErrorRootCause
import Amazonka.XRay.Types.ErrorRootCauseEntity
import Amazonka.XRay.Types.ErrorRootCauseService
import Amazonka.XRay.Types.ErrorStatistics
import Amazonka.XRay.Types.FaultRootCause
import Amazonka.XRay.Types.FaultRootCauseEntity
import Amazonka.XRay.Types.FaultRootCauseService
import Amazonka.XRay.Types.FaultStatistics
import Amazonka.XRay.Types.ForecastStatistics
import Amazonka.XRay.Types.Group
import Amazonka.XRay.Types.GroupSummary
import Amazonka.XRay.Types.HistogramEntry
import Amazonka.XRay.Types.Http
import Amazonka.XRay.Types.Insight
import Amazonka.XRay.Types.InsightEvent
import Amazonka.XRay.Types.InsightImpactGraphEdge
import Amazonka.XRay.Types.InsightImpactGraphService
import Amazonka.XRay.Types.InsightSummary
import Amazonka.XRay.Types.InsightsConfiguration
import Amazonka.XRay.Types.InstanceIdDetail
import Amazonka.XRay.Types.RequestImpactStatistics
import Amazonka.XRay.Types.ResourceARNDetail
import Amazonka.XRay.Types.ResourcePolicy
import Amazonka.XRay.Types.ResponseTimeRootCause
import Amazonka.XRay.Types.ResponseTimeRootCauseEntity
import Amazonka.XRay.Types.ResponseTimeRootCauseService
import Amazonka.XRay.Types.RootCauseException
import Amazonka.XRay.Types.SamplingRule
import Amazonka.XRay.Types.SamplingRuleRecord
import Amazonka.XRay.Types.SamplingRuleUpdate
import Amazonka.XRay.Types.SamplingStatisticSummary
import Amazonka.XRay.Types.SamplingStatisticsDocument
import Amazonka.XRay.Types.SamplingStrategy
import Amazonka.XRay.Types.SamplingTargetDocument
import Amazonka.XRay.Types.Segment
import Amazonka.XRay.Types.ServiceId
import Amazonka.XRay.Types.ServiceInfo
import Amazonka.XRay.Types.ServiceStatistics
import Amazonka.XRay.Types.Tag
import Amazonka.XRay.Types.TelemetryRecord
import Amazonka.XRay.Types.TimeSeriesServiceStatistics
import Amazonka.XRay.Types.Trace
import Amazonka.XRay.Types.TraceSummary
import Amazonka.XRay.Types.TraceUser
import Amazonka.XRay.Types.UnprocessedStatistics
import Amazonka.XRay.Types.UnprocessedTraceSegment
import Amazonka.XRay.Types.ValueWithServiceIds
import Amazonka.XRay.UntagResource
import Amazonka.XRay.UpdateGroup
import Amazonka.XRay.UpdateSamplingRule

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
    createGroup_filterExpression,
    createGroup_insightsConfiguration,
    createGroup_tags,
    createGroup_groupName,
    createGroupResponse_group,
    createGroupResponse_httpStatus,

    -- ** CreateSamplingRule
    createSamplingRule_tags,
    createSamplingRule_samplingRule,
    createSamplingRuleResponse_samplingRuleRecord,
    createSamplingRuleResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupARN,
    deleteGroup_groupName,
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
    getGroup_groupARN,
    getGroup_groupName,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** GetGroups
    getGroups_nextToken,
    getGroupsResponse_groups,
    getGroupsResponse_nextToken,
    getGroupsResponse_httpStatus,

    -- ** GetInsight
    getInsight_insightId,
    getInsightResponse_insight,
    getInsightResponse_httpStatus,

    -- ** GetInsightEvents
    getInsightEvents_maxResults,
    getInsightEvents_nextToken,
    getInsightEvents_insightId,
    getInsightEventsResponse_insightEvents,
    getInsightEventsResponse_nextToken,
    getInsightEventsResponse_httpStatus,

    -- ** GetInsightImpactGraph
    getInsightImpactGraph_nextToken,
    getInsightImpactGraph_insightId,
    getInsightImpactGraph_startTime,
    getInsightImpactGraph_endTime,
    getInsightImpactGraphResponse_endTime,
    getInsightImpactGraphResponse_insightId,
    getInsightImpactGraphResponse_nextToken,
    getInsightImpactGraphResponse_serviceGraphEndTime,
    getInsightImpactGraphResponse_serviceGraphStartTime,
    getInsightImpactGraphResponse_services,
    getInsightImpactGraphResponse_startTime,
    getInsightImpactGraphResponse_httpStatus,

    -- ** GetInsightSummaries
    getInsightSummaries_groupARN,
    getInsightSummaries_groupName,
    getInsightSummaries_maxResults,
    getInsightSummaries_nextToken,
    getInsightSummaries_states,
    getInsightSummaries_startTime,
    getInsightSummaries_endTime,
    getInsightSummariesResponse_insightSummaries,
    getInsightSummariesResponse_nextToken,
    getInsightSummariesResponse_httpStatus,

    -- ** GetSamplingRules
    getSamplingRules_nextToken,
    getSamplingRulesResponse_nextToken,
    getSamplingRulesResponse_samplingRuleRecords,
    getSamplingRulesResponse_httpStatus,

    -- ** GetSamplingStatisticSummaries
    getSamplingStatisticSummaries_nextToken,
    getSamplingStatisticSummariesResponse_nextToken,
    getSamplingStatisticSummariesResponse_samplingStatisticSummaries,
    getSamplingStatisticSummariesResponse_httpStatus,

    -- ** GetSamplingTargets
    getSamplingTargets_samplingStatisticsDocuments,
    getSamplingTargetsResponse_lastRuleModification,
    getSamplingTargetsResponse_samplingTargetDocuments,
    getSamplingTargetsResponse_unprocessedStatistics,
    getSamplingTargetsResponse_httpStatus,

    -- ** GetServiceGraph
    getServiceGraph_groupARN,
    getServiceGraph_groupName,
    getServiceGraph_nextToken,
    getServiceGraph_startTime,
    getServiceGraph_endTime,
    getServiceGraphResponse_containsOldGroupVersions,
    getServiceGraphResponse_endTime,
    getServiceGraphResponse_nextToken,
    getServiceGraphResponse_services,
    getServiceGraphResponse_startTime,
    getServiceGraphResponse_httpStatus,

    -- ** GetTimeSeriesServiceStatistics
    getTimeSeriesServiceStatistics_entitySelectorExpression,
    getTimeSeriesServiceStatistics_forecastStatistics,
    getTimeSeriesServiceStatistics_groupARN,
    getTimeSeriesServiceStatistics_groupName,
    getTimeSeriesServiceStatistics_nextToken,
    getTimeSeriesServiceStatistics_period,
    getTimeSeriesServiceStatistics_startTime,
    getTimeSeriesServiceStatistics_endTime,
    getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions,
    getTimeSeriesServiceStatisticsResponse_nextToken,
    getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics,
    getTimeSeriesServiceStatisticsResponse_httpStatus,

    -- ** GetTraceGraph
    getTraceGraph_nextToken,
    getTraceGraph_traceIds,
    getTraceGraphResponse_nextToken,
    getTraceGraphResponse_services,
    getTraceGraphResponse_httpStatus,

    -- ** GetTraceSummaries
    getTraceSummaries_filterExpression,
    getTraceSummaries_nextToken,
    getTraceSummaries_sampling,
    getTraceSummaries_samplingStrategy,
    getTraceSummaries_timeRangeType,
    getTraceSummaries_startTime,
    getTraceSummaries_endTime,
    getTraceSummariesResponse_approximateTime,
    getTraceSummariesResponse_nextToken,
    getTraceSummariesResponse_traceSummaries,
    getTraceSummariesResponse_tracesProcessedCount,
    getTraceSummariesResponse_httpStatus,

    -- ** ListResourcePolicies
    listResourcePolicies_nextToken,
    listResourcePoliciesResponse_nextToken,
    listResourcePoliciesResponse_resourcePolicies,
    listResourcePoliciesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutEncryptionConfig
    putEncryptionConfig_keyId,
    putEncryptionConfig_type,
    putEncryptionConfigResponse_encryptionConfig,
    putEncryptionConfigResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_bypassPolicyLockoutCheck,
    putResourcePolicy_policyRevisionId,
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
    updateGroup_filterExpression,
    updateGroup_groupARN,
    updateGroup_groupName,
    updateGroup_insightsConfiguration,
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,

    -- ** UpdateSamplingRule
    updateSamplingRule_samplingRuleUpdate,
    updateSamplingRuleResponse_samplingRuleRecord,
    updateSamplingRuleResponse_httpStatus,

    -- * Types

    -- ** Alias
    alias_name,
    alias_names,
    alias_type,

    -- ** AnnotationValue
    annotationValue_booleanValue,
    annotationValue_numberValue,
    annotationValue_stringValue,

    -- ** AnomalousService
    anomalousService_serviceId,

    -- ** AvailabilityZoneDetail
    availabilityZoneDetail_name,

    -- ** BackendConnectionErrors
    backendConnectionErrors_connectionRefusedCount,
    backendConnectionErrors_hTTPCode4XXCount,
    backendConnectionErrors_hTTPCode5XXCount,
    backendConnectionErrors_otherCount,
    backendConnectionErrors_timeoutCount,
    backendConnectionErrors_unknownHostCount,

    -- ** Edge
    edge_aliases,
    edge_edgeType,
    edge_endTime,
    edge_receivedEventAgeHistogram,
    edge_referenceId,
    edge_responseTimeHistogram,
    edge_startTime,
    edge_summaryStatistics,

    -- ** EdgeStatistics
    edgeStatistics_errorStatistics,
    edgeStatistics_faultStatistics,
    edgeStatistics_okCount,
    edgeStatistics_totalCount,
    edgeStatistics_totalResponseTime,

    -- ** EncryptionConfig
    encryptionConfig_keyId,
    encryptionConfig_status,
    encryptionConfig_type,

    -- ** ErrorRootCause
    errorRootCause_clientImpacting,
    errorRootCause_services,

    -- ** ErrorRootCauseEntity
    errorRootCauseEntity_exceptions,
    errorRootCauseEntity_name,
    errorRootCauseEntity_remote,

    -- ** ErrorRootCauseService
    errorRootCauseService_accountId,
    errorRootCauseService_entityPath,
    errorRootCauseService_inferred,
    errorRootCauseService_name,
    errorRootCauseService_names,
    errorRootCauseService_type,

    -- ** ErrorStatistics
    errorStatistics_otherCount,
    errorStatistics_throttleCount,
    errorStatistics_totalCount,

    -- ** FaultRootCause
    faultRootCause_clientImpacting,
    faultRootCause_services,

    -- ** FaultRootCauseEntity
    faultRootCauseEntity_exceptions,
    faultRootCauseEntity_name,
    faultRootCauseEntity_remote,

    -- ** FaultRootCauseService
    faultRootCauseService_accountId,
    faultRootCauseService_entityPath,
    faultRootCauseService_inferred,
    faultRootCauseService_name,
    faultRootCauseService_names,
    faultRootCauseService_type,

    -- ** FaultStatistics
    faultStatistics_otherCount,
    faultStatistics_totalCount,

    -- ** ForecastStatistics
    forecastStatistics_faultCountHigh,
    forecastStatistics_faultCountLow,

    -- ** Group
    group_filterExpression,
    group_groupARN,
    group_groupName,
    group_insightsConfiguration,

    -- ** GroupSummary
    groupSummary_filterExpression,
    groupSummary_groupARN,
    groupSummary_groupName,
    groupSummary_insightsConfiguration,

    -- ** HistogramEntry
    histogramEntry_count,
    histogramEntry_value,

    -- ** Http
    http_clientIp,
    http_httpMethod,
    http_httpStatus,
    http_httpURL,
    http_userAgent,

    -- ** Insight
    insight_categories,
    insight_clientRequestImpactStatistics,
    insight_endTime,
    insight_groupARN,
    insight_groupName,
    insight_insightId,
    insight_rootCauseServiceId,
    insight_rootCauseServiceRequestImpactStatistics,
    insight_startTime,
    insight_state,
    insight_summary,
    insight_topAnomalousServices,

    -- ** InsightEvent
    insightEvent_clientRequestImpactStatistics,
    insightEvent_eventTime,
    insightEvent_rootCauseServiceRequestImpactStatistics,
    insightEvent_summary,
    insightEvent_topAnomalousServices,

    -- ** InsightImpactGraphEdge
    insightImpactGraphEdge_referenceId,

    -- ** InsightImpactGraphService
    insightImpactGraphService_accountId,
    insightImpactGraphService_edges,
    insightImpactGraphService_name,
    insightImpactGraphService_names,
    insightImpactGraphService_referenceId,
    insightImpactGraphService_type,

    -- ** InsightSummary
    insightSummary_categories,
    insightSummary_clientRequestImpactStatistics,
    insightSummary_endTime,
    insightSummary_groupARN,
    insightSummary_groupName,
    insightSummary_insightId,
    insightSummary_lastUpdateTime,
    insightSummary_rootCauseServiceId,
    insightSummary_rootCauseServiceRequestImpactStatistics,
    insightSummary_startTime,
    insightSummary_state,
    insightSummary_summary,
    insightSummary_topAnomalousServices,

    -- ** InsightsConfiguration
    insightsConfiguration_insightsEnabled,
    insightsConfiguration_notificationsEnabled,

    -- ** InstanceIdDetail
    instanceIdDetail_id,

    -- ** RequestImpactStatistics
    requestImpactStatistics_faultCount,
    requestImpactStatistics_okCount,
    requestImpactStatistics_totalCount,

    -- ** ResourceARNDetail
    resourceARNDetail_arn,

    -- ** ResourcePolicy
    resourcePolicy_lastUpdatedTime,
    resourcePolicy_policyDocument,
    resourcePolicy_policyName,
    resourcePolicy_policyRevisionId,

    -- ** ResponseTimeRootCause
    responseTimeRootCause_clientImpacting,
    responseTimeRootCause_services,

    -- ** ResponseTimeRootCauseEntity
    responseTimeRootCauseEntity_coverage,
    responseTimeRootCauseEntity_name,
    responseTimeRootCauseEntity_remote,

    -- ** ResponseTimeRootCauseService
    responseTimeRootCauseService_accountId,
    responseTimeRootCauseService_entityPath,
    responseTimeRootCauseService_inferred,
    responseTimeRootCauseService_name,
    responseTimeRootCauseService_names,
    responseTimeRootCauseService_type,

    -- ** RootCauseException
    rootCauseException_message,
    rootCauseException_name,

    -- ** SamplingRule
    samplingRule_attributes,
    samplingRule_ruleARN,
    samplingRule_ruleName,
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
    samplingRuleRecord_createdAt,
    samplingRuleRecord_modifiedAt,
    samplingRuleRecord_samplingRule,

    -- ** SamplingRuleUpdate
    samplingRuleUpdate_attributes,
    samplingRuleUpdate_fixedRate,
    samplingRuleUpdate_hTTPMethod,
    samplingRuleUpdate_host,
    samplingRuleUpdate_priority,
    samplingRuleUpdate_reservoirSize,
    samplingRuleUpdate_resourceARN,
    samplingRuleUpdate_ruleARN,
    samplingRuleUpdate_ruleName,
    samplingRuleUpdate_serviceName,
    samplingRuleUpdate_serviceType,
    samplingRuleUpdate_uRLPath,

    -- ** SamplingStatisticSummary
    samplingStatisticSummary_borrowCount,
    samplingStatisticSummary_requestCount,
    samplingStatisticSummary_ruleName,
    samplingStatisticSummary_sampledCount,
    samplingStatisticSummary_timestamp,

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
    samplingTargetDocument_interval,
    samplingTargetDocument_reservoirQuota,
    samplingTargetDocument_reservoirQuotaTTL,
    samplingTargetDocument_ruleName,

    -- ** Segment
    segment_document,
    segment_id,

    -- ** ServiceId
    serviceId_accountId,
    serviceId_name,
    serviceId_names,
    serviceId_type,

    -- ** ServiceInfo
    serviceInfo_accountId,
    serviceInfo_durationHistogram,
    serviceInfo_edges,
    serviceInfo_endTime,
    serviceInfo_name,
    serviceInfo_names,
    serviceInfo_referenceId,
    serviceInfo_responseTimeHistogram,
    serviceInfo_root,
    serviceInfo_startTime,
    serviceInfo_state,
    serviceInfo_summaryStatistics,
    serviceInfo_type,

    -- ** ServiceStatistics
    serviceStatistics_errorStatistics,
    serviceStatistics_faultStatistics,
    serviceStatistics_okCount,
    serviceStatistics_totalCount,
    serviceStatistics_totalResponseTime,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TelemetryRecord
    telemetryRecord_backendConnectionErrors,
    telemetryRecord_segmentsReceivedCount,
    telemetryRecord_segmentsRejectedCount,
    telemetryRecord_segmentsSentCount,
    telemetryRecord_segmentsSpilloverCount,
    telemetryRecord_timestamp,

    -- ** TimeSeriesServiceStatistics
    timeSeriesServiceStatistics_edgeSummaryStatistics,
    timeSeriesServiceStatistics_responseTimeHistogram,
    timeSeriesServiceStatistics_serviceForecastStatistics,
    timeSeriesServiceStatistics_serviceSummaryStatistics,
    timeSeriesServiceStatistics_timestamp,

    -- ** Trace
    trace_duration,
    trace_id,
    trace_limitExceeded,
    trace_segments,

    -- ** TraceSummary
    traceSummary_annotations,
    traceSummary_availabilityZones,
    traceSummary_duration,
    traceSummary_entryPoint,
    traceSummary_errorRootCauses,
    traceSummary_faultRootCauses,
    traceSummary_hasError,
    traceSummary_hasFault,
    traceSummary_hasThrottle,
    traceSummary_http,
    traceSummary_id,
    traceSummary_instanceIds,
    traceSummary_isPartial,
    traceSummary_matchedEventTime,
    traceSummary_resourceARNs,
    traceSummary_responseTime,
    traceSummary_responseTimeRootCauses,
    traceSummary_revision,
    traceSummary_serviceIds,
    traceSummary_users,

    -- ** TraceUser
    traceUser_serviceIds,
    traceUser_userName,

    -- ** UnprocessedStatistics
    unprocessedStatistics_errorCode,
    unprocessedStatistics_message,
    unprocessedStatistics_ruleName,

    -- ** UnprocessedTraceSegment
    unprocessedTraceSegment_errorCode,
    unprocessedTraceSegment_id,
    unprocessedTraceSegment_message,

    -- ** ValueWithServiceIds
    valueWithServiceIds_annotationValue,
    valueWithServiceIds_serviceIds,
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

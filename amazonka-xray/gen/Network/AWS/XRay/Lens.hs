{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Lens
  ( -- * Operations

    -- ** GetSamplingTargets
    getSamplingTargets_samplingStatisticsDocuments,
    getSamplingTargetsResponse_samplingTargetDocuments,
    getSamplingTargetsResponse_lastRuleModification,
    getSamplingTargetsResponse_unprocessedStatistics,
    getSamplingTargetsResponse_httpStatus,

    -- ** GetSamplingStatisticSummaries
    getSamplingStatisticSummaries_nextToken,
    getSamplingStatisticSummariesResponse_samplingStatisticSummaries,
    getSamplingStatisticSummariesResponse_nextToken,
    getSamplingStatisticSummariesResponse_httpStatus,

    -- ** GetInsightImpactGraph
    getInsightImpactGraph_nextToken,
    getInsightImpactGraph_insightId,
    getInsightImpactGraph_startTime,
    getInsightImpactGraph_endTime,
    getInsightImpactGraphResponse_nextToken,
    getInsightImpactGraphResponse_services,
    getInsightImpactGraphResponse_serviceGraphEndTime,
    getInsightImpactGraphResponse_startTime,
    getInsightImpactGraphResponse_endTime,
    getInsightImpactGraphResponse_serviceGraphStartTime,
    getInsightImpactGraphResponse_insightId,
    getInsightImpactGraphResponse_httpStatus,

    -- ** GetTraceGraph
    getTraceGraph_nextToken,
    getTraceGraph_traceIds,
    getTraceGraphResponse_nextToken,
    getTraceGraphResponse_services,
    getTraceGraphResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateGroup
    createGroup_insightsConfiguration,
    createGroup_filterExpression,
    createGroup_tags,
    createGroup_groupName,
    createGroupResponse_group,
    createGroupResponse_httpStatus,

    -- ** GetInsight
    getInsight_insightId,
    getInsightResponse_insight,
    getInsightResponse_httpStatus,

    -- ** PutTraceSegments
    putTraceSegments_traceSegmentDocuments,
    putTraceSegmentsResponse_unprocessedTraceSegments,
    putTraceSegmentsResponse_httpStatus,

    -- ** GetTimeSeriesServiceStatistics
    getTimeSeriesServiceStatistics_nextToken,
    getTimeSeriesServiceStatistics_entitySelectorExpression,
    getTimeSeriesServiceStatistics_groupName,
    getTimeSeriesServiceStatistics_forecastStatistics,
    getTimeSeriesServiceStatistics_period,
    getTimeSeriesServiceStatistics_groupARN,
    getTimeSeriesServiceStatistics_startTime,
    getTimeSeriesServiceStatistics_endTime,
    getTimeSeriesServiceStatisticsResponse_nextToken,
    getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics,
    getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions,
    getTimeSeriesServiceStatisticsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** PutTelemetryRecords
    putTelemetryRecords_resourceARN,
    putTelemetryRecords_hostname,
    putTelemetryRecords_eC2InstanceId,
    putTelemetryRecords_telemetryRecords,
    putTelemetryRecordsResponse_httpStatus,

    -- ** BatchGetTraces
    batchGetTraces_nextToken,
    batchGetTraces_traceIds,
    batchGetTracesResponse_nextToken,
    batchGetTracesResponse_unprocessedTraceIds,
    batchGetTracesResponse_traces,
    batchGetTracesResponse_httpStatus,

    -- ** GetTraceSummaries
    getTraceSummaries_nextToken,
    getTraceSummaries_filterExpression,
    getTraceSummaries_timeRangeType,
    getTraceSummaries_sampling,
    getTraceSummaries_samplingStrategy,
    getTraceSummaries_startTime,
    getTraceSummaries_endTime,
    getTraceSummariesResponse_nextToken,
    getTraceSummariesResponse_tracesProcessedCount,
    getTraceSummariesResponse_traceSummaries,
    getTraceSummariesResponse_approximateTime,
    getTraceSummariesResponse_httpStatus,

    -- ** GetInsightSummaries
    getInsightSummaries_nextToken,
    getInsightSummaries_states,
    getInsightSummaries_maxResults,
    getInsightSummaries_groupName,
    getInsightSummaries_groupARN,
    getInsightSummaries_startTime,
    getInsightSummaries_endTime,
    getInsightSummariesResponse_insightSummaries,
    getInsightSummariesResponse_nextToken,
    getInsightSummariesResponse_httpStatus,

    -- ** GetGroups
    getGroups_nextToken,
    getGroupsResponse_groups,
    getGroupsResponse_nextToken,
    getGroupsResponse_httpStatus,

    -- ** GetInsightEvents
    getInsightEvents_nextToken,
    getInsightEvents_maxResults,
    getInsightEvents_insightId,
    getInsightEventsResponse_nextToken,
    getInsightEventsResponse_insightEvents,
    getInsightEventsResponse_httpStatus,

    -- ** GetServiceGraph
    getServiceGraph_nextToken,
    getServiceGraph_groupName,
    getServiceGraph_groupARN,
    getServiceGraph_startTime,
    getServiceGraph_endTime,
    getServiceGraphResponse_nextToken,
    getServiceGraphResponse_services,
    getServiceGraphResponse_startTime,
    getServiceGraphResponse_containsOldGroupVersions,
    getServiceGraphResponse_endTime,
    getServiceGraphResponse_httpStatus,

    -- ** PutEncryptionConfig
    putEncryptionConfig_keyId,
    putEncryptionConfig_type,
    putEncryptionConfigResponse_encryptionConfig,
    putEncryptionConfigResponse_httpStatus,

    -- ** DeleteSamplingRule
    deleteSamplingRule_ruleName,
    deleteSamplingRule_ruleARN,
    deleteSamplingRuleResponse_samplingRuleRecord,
    deleteSamplingRuleResponse_httpStatus,

    -- ** UpdateSamplingRule
    updateSamplingRule_samplingRuleUpdate,
    updateSamplingRuleResponse_samplingRuleRecord,
    updateSamplingRuleResponse_httpStatus,

    -- ** GetGroup
    getGroup_groupName,
    getGroup_groupARN,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** CreateSamplingRule
    createSamplingRule_tags,
    createSamplingRule_samplingRule,
    createSamplingRuleResponse_samplingRuleRecord,
    createSamplingRuleResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_groupName,
    updateGroup_insightsConfiguration,
    updateGroup_filterExpression,
    updateGroup_groupARN,
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupName,
    deleteGroup_groupARN,
    deleteGroupResponse_httpStatus,

    -- ** GetEncryptionConfig
    getEncryptionConfigResponse_encryptionConfig,
    getEncryptionConfigResponse_httpStatus,

    -- ** GetSamplingRules
    getSamplingRules_nextToken,
    getSamplingRulesResponse_nextToken,
    getSamplingRulesResponse_samplingRuleRecords,
    getSamplingRulesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** Alias
    alias_names,
    alias_name,
    alias_type,

    -- ** AnnotationValue
    annotationValue_stringValue,
    annotationValue_booleanValue,
    annotationValue_numberValue,

    -- ** AnomalousService
    anomalousService_serviceId,

    -- ** AvailabilityZoneDetail
    availabilityZoneDetail_name,

    -- ** BackendConnectionErrors
    backendConnectionErrors_otherCount,
    backendConnectionErrors_connectionRefusedCount,
    backendConnectionErrors_hTTPCode5XXCount,
    backendConnectionErrors_timeoutCount,
    backendConnectionErrors_unknownHostCount,
    backendConnectionErrors_hTTPCode4XXCount,

    -- ** Edge
    edge_summaryStatistics,
    edge_responseTimeHistogram,
    edge_referenceId,
    edge_startTime,
    edge_endTime,
    edge_aliases,

    -- ** EdgeStatistics
    edgeStatistics_totalResponseTime,
    edgeStatistics_okCount,
    edgeStatistics_faultStatistics,
    edgeStatistics_totalCount,
    edgeStatistics_errorStatistics,

    -- ** EncryptionConfig
    encryptionConfig_status,
    encryptionConfig_type,
    encryptionConfig_keyId,

    -- ** ErrorRootCause
    errorRootCause_services,
    errorRootCause_clientImpacting,

    -- ** ErrorRootCauseEntity
    errorRootCauseEntity_exceptions,
    errorRootCauseEntity_remote,
    errorRootCauseEntity_name,

    -- ** ErrorRootCauseService
    errorRootCauseService_names,
    errorRootCauseService_accountId,
    errorRootCauseService_inferred,
    errorRootCauseService_name,
    errorRootCauseService_entityPath,
    errorRootCauseService_type,

    -- ** ErrorStatistics
    errorStatistics_otherCount,
    errorStatistics_throttleCount,
    errorStatistics_totalCount,

    -- ** FaultRootCause
    faultRootCause_services,
    faultRootCause_clientImpacting,

    -- ** FaultRootCauseEntity
    faultRootCauseEntity_exceptions,
    faultRootCauseEntity_remote,
    faultRootCauseEntity_name,

    -- ** FaultRootCauseService
    faultRootCauseService_names,
    faultRootCauseService_accountId,
    faultRootCauseService_inferred,
    faultRootCauseService_name,
    faultRootCauseService_entityPath,
    faultRootCauseService_type,

    -- ** FaultStatistics
    faultStatistics_otherCount,
    faultStatistics_totalCount,

    -- ** ForecastStatistics
    forecastStatistics_faultCountLow,
    forecastStatistics_faultCountHigh,

    -- ** Group
    group_groupName,
    group_insightsConfiguration,
    group_filterExpression,
    group_groupARN,

    -- ** GroupSummary
    groupSummary_groupName,
    groupSummary_insightsConfiguration,
    groupSummary_filterExpression,
    groupSummary_groupARN,

    -- ** HistogramEntry
    histogramEntry_value,
    histogramEntry_count,

    -- ** Http
    http_httpMethod,
    http_httpURL,
    http_userAgent,
    http_httpStatus,
    http_clientIp,

    -- ** Insight
    insight_clientRequestImpactStatistics,
    insight_rootCauseServiceRequestImpactStatistics,
    insight_groupName,
    insight_startTime,
    insight_endTime,
    insight_rootCauseServiceId,
    insight_state,
    insight_summary,
    insight_topAnomalousServices,
    insight_categories,
    insight_insightId,
    insight_groupARN,

    -- ** InsightEvent
    insightEvent_clientRequestImpactStatistics,
    insightEvent_rootCauseServiceRequestImpactStatistics,
    insightEvent_summary,
    insightEvent_topAnomalousServices,
    insightEvent_eventTime,

    -- ** InsightImpactGraphEdge
    insightImpactGraphEdge_referenceId,

    -- ** InsightImpactGraphService
    insightImpactGraphService_names,
    insightImpactGraphService_accountId,
    insightImpactGraphService_referenceId,
    insightImpactGraphService_edges,
    insightImpactGraphService_name,
    insightImpactGraphService_type,

    -- ** InsightSummary
    insightSummary_clientRequestImpactStatistics,
    insightSummary_rootCauseServiceRequestImpactStatistics,
    insightSummary_lastUpdateTime,
    insightSummary_groupName,
    insightSummary_startTime,
    insightSummary_endTime,
    insightSummary_rootCauseServiceId,
    insightSummary_state,
    insightSummary_summary,
    insightSummary_topAnomalousServices,
    insightSummary_categories,
    insightSummary_insightId,
    insightSummary_groupARN,

    -- ** InsightsConfiguration
    insightsConfiguration_notificationsEnabled,
    insightsConfiguration_insightsEnabled,

    -- ** InstanceIdDetail
    instanceIdDetail_id,

    -- ** RequestImpactStatistics
    requestImpactStatistics_okCount,
    requestImpactStatistics_faultCount,
    requestImpactStatistics_totalCount,

    -- ** ResourceARNDetail
    resourceARNDetail_arn,

    -- ** ResponseTimeRootCause
    responseTimeRootCause_services,
    responseTimeRootCause_clientImpacting,

    -- ** ResponseTimeRootCauseEntity
    responseTimeRootCauseEntity_remote,
    responseTimeRootCauseEntity_name,
    responseTimeRootCauseEntity_coverage,

    -- ** ResponseTimeRootCauseService
    responseTimeRootCauseService_names,
    responseTimeRootCauseService_accountId,
    responseTimeRootCauseService_inferred,
    responseTimeRootCauseService_name,
    responseTimeRootCauseService_entityPath,
    responseTimeRootCauseService_type,

    -- ** RootCauseException
    rootCauseException_message,
    rootCauseException_name,

    -- ** SamplingRule
    samplingRule_ruleName,
    samplingRule_ruleARN,
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
    samplingRuleRecord_createdAt,
    samplingRuleRecord_samplingRule,

    -- ** SamplingRuleUpdate
    samplingRuleUpdate_resourceARN,
    samplingRuleUpdate_hTTPMethod,
    samplingRuleUpdate_reservoirSize,
    samplingRuleUpdate_fixedRate,
    samplingRuleUpdate_ruleName,
    samplingRuleUpdate_ruleARN,
    samplingRuleUpdate_serviceName,
    samplingRuleUpdate_priority,
    samplingRuleUpdate_attributes,
    samplingRuleUpdate_uRLPath,
    samplingRuleUpdate_host,
    samplingRuleUpdate_serviceType,

    -- ** SamplingStatisticSummary
    samplingStatisticSummary_ruleName,
    samplingStatisticSummary_borrowCount,
    samplingStatisticSummary_requestCount,
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
    samplingTargetDocument_reservoirQuota,
    samplingTargetDocument_fixedRate,
    samplingTargetDocument_ruleName,
    samplingTargetDocument_reservoirQuotaTTL,
    samplingTargetDocument_interval,

    -- ** Segment
    segment_id,
    segment_document,

    -- ** ServiceId
    serviceId_names,
    serviceId_accountId,
    serviceId_name,
    serviceId_type,

    -- ** ServiceInfo
    serviceInfo_names,
    serviceInfo_accountId,
    serviceInfo_summaryStatistics,
    serviceInfo_responseTimeHistogram,
    serviceInfo_referenceId,
    serviceInfo_durationHistogram,
    serviceInfo_edges,
    serviceInfo_startTime,
    serviceInfo_endTime,
    serviceInfo_state,
    serviceInfo_name,
    serviceInfo_root,
    serviceInfo_type,

    -- ** ServiceStatistics
    serviceStatistics_totalResponseTime,
    serviceStatistics_okCount,
    serviceStatistics_faultStatistics,
    serviceStatistics_totalCount,
    serviceStatistics_errorStatistics,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TelemetryRecord
    telemetryRecord_segmentsSpilloverCount,
    telemetryRecord_backendConnectionErrors,
    telemetryRecord_segmentsRejectedCount,
    telemetryRecord_segmentsSentCount,
    telemetryRecord_segmentsReceivedCount,
    telemetryRecord_timestamp,

    -- ** TimeSeriesServiceStatistics
    timeSeriesServiceStatistics_serviceSummaryStatistics,
    timeSeriesServiceStatistics_responseTimeHistogram,
    timeSeriesServiceStatistics_serviceForecastStatistics,
    timeSeriesServiceStatistics_edgeSummaryStatistics,
    timeSeriesServiceStatistics_timestamp,

    -- ** Trace
    trace_limitExceeded,
    trace_duration,
    trace_id,
    trace_segments,

    -- ** TraceSummary
    traceSummary_instanceIds,
    traceSummary_errorRootCauses,
    traceSummary_availabilityZones,
    traceSummary_responseTime,
    traceSummary_duration,
    traceSummary_matchedEventTime,
    traceSummary_serviceIds,
    traceSummary_hasFault,
    traceSummary_entryPoint,
    traceSummary_id,
    traceSummary_annotations,
    traceSummary_resourceARNs,
    traceSummary_isPartial,
    traceSummary_faultRootCauses,
    traceSummary_revision,
    traceSummary_http,
    traceSummary_hasError,
    traceSummary_users,
    traceSummary_hasThrottle,
    traceSummary_responseTimeRootCauses,

    -- ** TraceUser
    traceUser_serviceIds,
    traceUser_userName,

    -- ** UnprocessedStatistics
    unprocessedStatistics_ruleName,
    unprocessedStatistics_message,
    unprocessedStatistics_errorCode,

    -- ** UnprocessedTraceSegment
    unprocessedTraceSegment_message,
    unprocessedTraceSegment_id,
    unprocessedTraceSegment_errorCode,

    -- ** ValueWithServiceIds
    valueWithServiceIds_annotationValue,
    valueWithServiceIds_serviceIds,
  )
where

import Network.AWS.XRay.BatchGetTraces
import Network.AWS.XRay.CreateGroup
import Network.AWS.XRay.CreateSamplingRule
import Network.AWS.XRay.DeleteGroup
import Network.AWS.XRay.DeleteSamplingRule
import Network.AWS.XRay.GetEncryptionConfig
import Network.AWS.XRay.GetGroup
import Network.AWS.XRay.GetGroups
import Network.AWS.XRay.GetInsight
import Network.AWS.XRay.GetInsightEvents
import Network.AWS.XRay.GetInsightImpactGraph
import Network.AWS.XRay.GetInsightSummaries
import Network.AWS.XRay.GetSamplingRules
import Network.AWS.XRay.GetSamplingStatisticSummaries
import Network.AWS.XRay.GetSamplingTargets
import Network.AWS.XRay.GetServiceGraph
import Network.AWS.XRay.GetTimeSeriesServiceStatistics
import Network.AWS.XRay.GetTraceGraph
import Network.AWS.XRay.GetTraceSummaries
import Network.AWS.XRay.ListTagsForResource
import Network.AWS.XRay.PutEncryptionConfig
import Network.AWS.XRay.PutTelemetryRecords
import Network.AWS.XRay.PutTraceSegments
import Network.AWS.XRay.TagResource
import Network.AWS.XRay.Types.Alias
import Network.AWS.XRay.Types.AnnotationValue
import Network.AWS.XRay.Types.AnomalousService
import Network.AWS.XRay.Types.AvailabilityZoneDetail
import Network.AWS.XRay.Types.BackendConnectionErrors
import Network.AWS.XRay.Types.Edge
import Network.AWS.XRay.Types.EdgeStatistics
import Network.AWS.XRay.Types.EncryptionConfig
import Network.AWS.XRay.Types.ErrorRootCause
import Network.AWS.XRay.Types.ErrorRootCauseEntity
import Network.AWS.XRay.Types.ErrorRootCauseService
import Network.AWS.XRay.Types.ErrorStatistics
import Network.AWS.XRay.Types.FaultRootCause
import Network.AWS.XRay.Types.FaultRootCauseEntity
import Network.AWS.XRay.Types.FaultRootCauseService
import Network.AWS.XRay.Types.FaultStatistics
import Network.AWS.XRay.Types.ForecastStatistics
import Network.AWS.XRay.Types.Group
import Network.AWS.XRay.Types.GroupSummary
import Network.AWS.XRay.Types.HistogramEntry
import Network.AWS.XRay.Types.Http
import Network.AWS.XRay.Types.Insight
import Network.AWS.XRay.Types.InsightEvent
import Network.AWS.XRay.Types.InsightImpactGraphEdge
import Network.AWS.XRay.Types.InsightImpactGraphService
import Network.AWS.XRay.Types.InsightSummary
import Network.AWS.XRay.Types.InsightsConfiguration
import Network.AWS.XRay.Types.InstanceIdDetail
import Network.AWS.XRay.Types.RequestImpactStatistics
import Network.AWS.XRay.Types.ResourceARNDetail
import Network.AWS.XRay.Types.ResponseTimeRootCause
import Network.AWS.XRay.Types.ResponseTimeRootCauseEntity
import Network.AWS.XRay.Types.ResponseTimeRootCauseService
import Network.AWS.XRay.Types.RootCauseException
import Network.AWS.XRay.Types.SamplingRule
import Network.AWS.XRay.Types.SamplingRuleRecord
import Network.AWS.XRay.Types.SamplingRuleUpdate
import Network.AWS.XRay.Types.SamplingStatisticSummary
import Network.AWS.XRay.Types.SamplingStatisticsDocument
import Network.AWS.XRay.Types.SamplingStrategy
import Network.AWS.XRay.Types.SamplingTargetDocument
import Network.AWS.XRay.Types.Segment
import Network.AWS.XRay.Types.ServiceId
import Network.AWS.XRay.Types.ServiceInfo
import Network.AWS.XRay.Types.ServiceStatistics
import Network.AWS.XRay.Types.Tag
import Network.AWS.XRay.Types.TelemetryRecord
import Network.AWS.XRay.Types.TimeSeriesServiceStatistics
import Network.AWS.XRay.Types.Trace
import Network.AWS.XRay.Types.TraceSummary
import Network.AWS.XRay.Types.TraceUser
import Network.AWS.XRay.Types.UnprocessedStatistics
import Network.AWS.XRay.Types.UnprocessedTraceSegment
import Network.AWS.XRay.Types.ValueWithServiceIds
import Network.AWS.XRay.UntagResource
import Network.AWS.XRay.UpdateGroup
import Network.AWS.XRay.UpdateSamplingRule

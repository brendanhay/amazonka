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

    -- ** PutEncryptionConfig
    putEncryptionConfig_keyId,
    putEncryptionConfig_type,
    putEncryptionConfigResponse_encryptionConfig,
    putEncryptionConfigResponse_httpStatus,

    -- ** GetServiceGraph
    getServiceGraph_nextToken,
    getServiceGraph_groupARN,
    getServiceGraph_groupName,
    getServiceGraph_startTime,
    getServiceGraph_endTime,
    getServiceGraphResponse_containsOldGroupVersions,
    getServiceGraphResponse_startTime,
    getServiceGraphResponse_nextToken,
    getServiceGraphResponse_endTime,
    getServiceGraphResponse_services,
    getServiceGraphResponse_httpStatus,

    -- ** GetSamplingTargets
    getSamplingTargets_samplingStatisticsDocuments,
    getSamplingTargetsResponse_unprocessedStatistics,
    getSamplingTargetsResponse_lastRuleModification,
    getSamplingTargetsResponse_samplingTargetDocuments,
    getSamplingTargetsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetTraceSummaries
    getTraceSummaries_filterExpression,
    getTraceSummaries_nextToken,
    getTraceSummaries_timeRangeType,
    getTraceSummaries_samplingStrategy,
    getTraceSummaries_sampling,
    getTraceSummaries_startTime,
    getTraceSummaries_endTime,
    getTraceSummariesResponse_tracesProcessedCount,
    getTraceSummariesResponse_nextToken,
    getTraceSummariesResponse_approximateTime,
    getTraceSummariesResponse_traceSummaries,
    getTraceSummariesResponse_httpStatus,

    -- ** PutTraceSegments
    putTraceSegments_traceSegmentDocuments,
    putTraceSegmentsResponse_unprocessedTraceSegments,
    putTraceSegmentsResponse_httpStatus,

    -- ** BatchGetTraces
    batchGetTraces_nextToken,
    batchGetTraces_traceIds,
    batchGetTracesResponse_nextToken,
    batchGetTracesResponse_traces,
    batchGetTracesResponse_unprocessedTraceIds,
    batchGetTracesResponse_httpStatus,

    -- ** GetInsight
    getInsight_insightId,
    getInsightResponse_insight,
    getInsightResponse_httpStatus,

    -- ** GetTimeSeriesServiceStatistics
    getTimeSeriesServiceStatistics_entitySelectorExpression,
    getTimeSeriesServiceStatistics_period,
    getTimeSeriesServiceStatistics_forecastStatistics,
    getTimeSeriesServiceStatistics_nextToken,
    getTimeSeriesServiceStatistics_groupARN,
    getTimeSeriesServiceStatistics_groupName,
    getTimeSeriesServiceStatistics_startTime,
    getTimeSeriesServiceStatistics_endTime,
    getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions,
    getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics,
    getTimeSeriesServiceStatisticsResponse_nextToken,
    getTimeSeriesServiceStatisticsResponse_httpStatus,

    -- ** GetEncryptionConfig
    getEncryptionConfigResponse_encryptionConfig,
    getEncryptionConfigResponse_httpStatus,

    -- ** GetInsightImpactGraph
    getInsightImpactGraph_nextToken,
    getInsightImpactGraph_insightId,
    getInsightImpactGraph_startTime,
    getInsightImpactGraph_endTime,
    getInsightImpactGraphResponse_serviceGraphStartTime,
    getInsightImpactGraphResponse_startTime,
    getInsightImpactGraphResponse_insightId,
    getInsightImpactGraphResponse_nextToken,
    getInsightImpactGraphResponse_endTime,
    getInsightImpactGraphResponse_serviceGraphEndTime,
    getInsightImpactGraphResponse_services,
    getInsightImpactGraphResponse_httpStatus,

    -- ** UpdateSamplingRule
    updateSamplingRule_samplingRuleUpdate,
    updateSamplingRuleResponse_samplingRuleRecord,
    updateSamplingRuleResponse_httpStatus,

    -- ** DeleteSamplingRule
    deleteSamplingRule_ruleName,
    deleteSamplingRule_ruleARN,
    deleteSamplingRuleResponse_samplingRuleRecord,
    deleteSamplingRuleResponse_httpStatus,

    -- ** GetInsightEvents
    getInsightEvents_nextToken,
    getInsightEvents_maxResults,
    getInsightEvents_insightId,
    getInsightEventsResponse_insightEvents,
    getInsightEventsResponse_nextToken,
    getInsightEventsResponse_httpStatus,

    -- ** GetGroups
    getGroups_nextToken,
    getGroupsResponse_groups,
    getGroupsResponse_nextToken,
    getGroupsResponse_httpStatus,

    -- ** GetInsightSummaries
    getInsightSummaries_states,
    getInsightSummaries_nextToken,
    getInsightSummaries_groupARN,
    getInsightSummaries_groupName,
    getInsightSummaries_maxResults,
    getInsightSummaries_startTime,
    getInsightSummaries_endTime,
    getInsightSummariesResponse_insightSummaries,
    getInsightSummariesResponse_nextToken,
    getInsightSummariesResponse_httpStatus,

    -- ** PutTelemetryRecords
    putTelemetryRecords_hostname,
    putTelemetryRecords_eC2InstanceId,
    putTelemetryRecords_resourceARN,
    putTelemetryRecords_telemetryRecords,
    putTelemetryRecordsResponse_httpStatus,

    -- ** GetSamplingRules
    getSamplingRules_nextToken,
    getSamplingRulesResponse_samplingRuleRecords,
    getSamplingRulesResponse_nextToken,
    getSamplingRulesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetTraceGraph
    getTraceGraph_nextToken,
    getTraceGraph_traceIds,
    getTraceGraphResponse_nextToken,
    getTraceGraphResponse_services,
    getTraceGraphResponse_httpStatus,

    -- ** CreateGroup
    createGroup_filterExpression,
    createGroup_insightsConfiguration,
    createGroup_tags,
    createGroup_groupName,
    createGroupResponse_group,
    createGroupResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupARN,
    deleteGroup_groupName,
    deleteGroupResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_filterExpression,
    updateGroup_insightsConfiguration,
    updateGroup_groupARN,
    updateGroup_groupName,
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,

    -- ** GetGroup
    getGroup_groupARN,
    getGroup_groupName,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** GetSamplingStatisticSummaries
    getSamplingStatisticSummaries_nextToken,
    getSamplingStatisticSummariesResponse_samplingStatisticSummaries,
    getSamplingStatisticSummariesResponse_nextToken,
    getSamplingStatisticSummariesResponse_httpStatus,

    -- ** CreateSamplingRule
    createSamplingRule_tags,
    createSamplingRule_samplingRule,
    createSamplingRuleResponse_samplingRuleRecord,
    createSamplingRuleResponse_httpStatus,

    -- * Types

    -- ** Alias
    alias_names,
    alias_name,
    alias_type,

    -- ** AnnotationValue
    annotationValue_numberValue,
    annotationValue_stringValue,
    annotationValue_booleanValue,

    -- ** AnomalousService
    anomalousService_serviceId,

    -- ** AvailabilityZoneDetail
    availabilityZoneDetail_name,

    -- ** BackendConnectionErrors
    backendConnectionErrors_otherCount,
    backendConnectionErrors_timeoutCount,
    backendConnectionErrors_hTTPCode5XXCount,
    backendConnectionErrors_connectionRefusedCount,
    backendConnectionErrors_hTTPCode4XXCount,
    backendConnectionErrors_unknownHostCount,

    -- ** Edge
    edge_startTime,
    edge_aliases,
    edge_responseTimeHistogram,
    edge_referenceId,
    edge_endTime,
    edge_summaryStatistics,

    -- ** EdgeStatistics
    edgeStatistics_faultStatistics,
    edgeStatistics_okCount,
    edgeStatistics_totalResponseTime,
    edgeStatistics_errorStatistics,
    edgeStatistics_totalCount,

    -- ** EncryptionConfig
    encryptionConfig_status,
    encryptionConfig_keyId,
    encryptionConfig_type,

    -- ** ErrorRootCause
    errorRootCause_clientImpacting,
    errorRootCause_services,

    -- ** ErrorRootCauseEntity
    errorRootCauseEntity_exceptions,
    errorRootCauseEntity_remote,
    errorRootCauseEntity_name,

    -- ** ErrorRootCauseService
    errorRootCauseService_entityPath,
    errorRootCauseService_accountId,
    errorRootCauseService_names,
    errorRootCauseService_name,
    errorRootCauseService_inferred,
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
    faultRootCauseEntity_remote,
    faultRootCauseEntity_name,

    -- ** FaultRootCauseService
    faultRootCauseService_entityPath,
    faultRootCauseService_accountId,
    faultRootCauseService_names,
    faultRootCauseService_name,
    faultRootCauseService_inferred,
    faultRootCauseService_type,

    -- ** FaultStatistics
    faultStatistics_otherCount,
    faultStatistics_totalCount,

    -- ** ForecastStatistics
    forecastStatistics_faultCountLow,
    forecastStatistics_faultCountHigh,

    -- ** Group
    group_filterExpression,
    group_insightsConfiguration,
    group_groupARN,
    group_groupName,

    -- ** GroupSummary
    groupSummary_filterExpression,
    groupSummary_insightsConfiguration,
    groupSummary_groupARN,
    groupSummary_groupName,

    -- ** HistogramEntry
    histogramEntry_count,
    histogramEntry_value,

    -- ** Http
    http_httpMethod,
    http_httpStatus,
    http_clientIp,
    http_userAgent,
    http_httpURL,

    -- ** Insight
    insight_summary,
    insight_state,
    insight_startTime,
    insight_insightId,
    insight_categories,
    insight_rootCauseServiceRequestImpactStatistics,
    insight_topAnomalousServices,
    insight_rootCauseServiceId,
    insight_clientRequestImpactStatistics,
    insight_endTime,
    insight_groupARN,
    insight_groupName,

    -- ** InsightEvent
    insightEvent_summary,
    insightEvent_eventTime,
    insightEvent_rootCauseServiceRequestImpactStatistics,
    insightEvent_topAnomalousServices,
    insightEvent_clientRequestImpactStatistics,

    -- ** InsightImpactGraphEdge
    insightImpactGraphEdge_referenceId,

    -- ** InsightImpactGraphService
    insightImpactGraphService_referenceId,
    insightImpactGraphService_accountId,
    insightImpactGraphService_names,
    insightImpactGraphService_name,
    insightImpactGraphService_type,
    insightImpactGraphService_edges,

    -- ** InsightSummary
    insightSummary_summary,
    insightSummary_state,
    insightSummary_startTime,
    insightSummary_insightId,
    insightSummary_categories,
    insightSummary_rootCauseServiceRequestImpactStatistics,
    insightSummary_topAnomalousServices,
    insightSummary_rootCauseServiceId,
    insightSummary_clientRequestImpactStatistics,
    insightSummary_endTime,
    insightSummary_groupARN,
    insightSummary_groupName,
    insightSummary_lastUpdateTime,

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
    responseTimeRootCause_clientImpacting,
    responseTimeRootCause_services,

    -- ** ResponseTimeRootCauseEntity
    responseTimeRootCauseEntity_remote,
    responseTimeRootCauseEntity_coverage,
    responseTimeRootCauseEntity_name,

    -- ** ResponseTimeRootCauseService
    responseTimeRootCauseService_entityPath,
    responseTimeRootCauseService_accountId,
    responseTimeRootCauseService_names,
    responseTimeRootCauseService_name,
    responseTimeRootCauseService_inferred,
    responseTimeRootCauseService_type,

    -- ** RootCauseException
    rootCauseException_name,
    rootCauseException_message,

    -- ** SamplingRule
    samplingRule_ruleName,
    samplingRule_attributes,
    samplingRule_ruleARN,
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
    samplingRuleUpdate_hTTPMethod,
    samplingRuleUpdate_priority,
    samplingRuleUpdate_ruleName,
    samplingRuleUpdate_reservoirSize,
    samplingRuleUpdate_fixedRate,
    samplingRuleUpdate_resourceARN,
    samplingRuleUpdate_attributes,
    samplingRuleUpdate_serviceName,
    samplingRuleUpdate_serviceType,
    samplingRuleUpdate_host,
    samplingRuleUpdate_ruleARN,
    samplingRuleUpdate_uRLPath,

    -- ** SamplingStatisticSummary
    samplingStatisticSummary_requestCount,
    samplingStatisticSummary_borrowCount,
    samplingStatisticSummary_ruleName,
    samplingStatisticSummary_timestamp,
    samplingStatisticSummary_sampledCount,

    -- ** SamplingStatisticsDocument
    samplingStatisticsDocument_borrowCount,
    samplingStatisticsDocument_ruleName,
    samplingStatisticsDocument_clientID,
    samplingStatisticsDocument_timestamp,
    samplingStatisticsDocument_requestCount,
    samplingStatisticsDocument_sampledCount,

    -- ** SamplingStrategy
    samplingStrategy_value,
    samplingStrategy_name,

    -- ** SamplingTargetDocument
    samplingTargetDocument_reservoirQuota,
    samplingTargetDocument_ruleName,
    samplingTargetDocument_fixedRate,
    samplingTargetDocument_interval,
    samplingTargetDocument_reservoirQuotaTTL,

    -- ** Segment
    segment_document,
    segment_id,

    -- ** ServiceId
    serviceId_accountId,
    serviceId_names,
    serviceId_name,
    serviceId_type,

    -- ** ServiceInfo
    serviceInfo_state,
    serviceInfo_startTime,
    serviceInfo_root,
    serviceInfo_responseTimeHistogram,
    serviceInfo_durationHistogram,
    serviceInfo_referenceId,
    serviceInfo_accountId,
    serviceInfo_names,
    serviceInfo_name,
    serviceInfo_endTime,
    serviceInfo_type,
    serviceInfo_edges,
    serviceInfo_summaryStatistics,

    -- ** ServiceStatistics
    serviceStatistics_faultStatistics,
    serviceStatistics_okCount,
    serviceStatistics_totalResponseTime,
    serviceStatistics_errorStatistics,
    serviceStatistics_totalCount,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TelemetryRecord
    telemetryRecord_segmentsReceivedCount,
    telemetryRecord_segmentsSentCount,
    telemetryRecord_segmentsSpilloverCount,
    telemetryRecord_segmentsRejectedCount,
    telemetryRecord_backendConnectionErrors,
    telemetryRecord_timestamp,

    -- ** TimeSeriesServiceStatistics
    timeSeriesServiceStatistics_serviceSummaryStatistics,
    timeSeriesServiceStatistics_responseTimeHistogram,
    timeSeriesServiceStatistics_edgeSummaryStatistics,
    timeSeriesServiceStatistics_serviceForecastStatistics,
    timeSeriesServiceStatistics_timestamp,

    -- ** Trace
    trace_limitExceeded,
    trace_id,
    trace_segments,
    trace_duration,

    -- ** TraceSummary
    traceSummary_annotations,
    traceSummary_hasThrottle,
    traceSummary_users,
    traceSummary_entryPoint,
    traceSummary_hasFault,
    traceSummary_serviceIds,
    traceSummary_matchedEventTime,
    traceSummary_isPartial,
    traceSummary_errorRootCauses,
    traceSummary_resourceARNs,
    traceSummary_availabilityZones,
    traceSummary_instanceIds,
    traceSummary_responseTimeRootCauses,
    traceSummary_hasError,
    traceSummary_id,
    traceSummary_http,
    traceSummary_revision,
    traceSummary_duration,
    traceSummary_faultRootCauses,
    traceSummary_responseTime,

    -- ** TraceUser
    traceUser_serviceIds,
    traceUser_userName,

    -- ** UnprocessedStatistics
    unprocessedStatistics_ruleName,
    unprocessedStatistics_errorCode,
    unprocessedStatistics_message,

    -- ** UnprocessedTraceSegment
    unprocessedTraceSegment_errorCode,
    unprocessedTraceSegment_id,
    unprocessedTraceSegment_message,

    -- ** ValueWithServiceIds
    valueWithServiceIds_serviceIds,
    valueWithServiceIds_annotationValue,
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

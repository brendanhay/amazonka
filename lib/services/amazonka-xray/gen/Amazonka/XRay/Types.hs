{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.XRay.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _RuleLimitExceededException,
    _ThrottledException,
    _TooManyTagsException,
    _ResourceNotFoundException,

    -- * EncryptionStatus
    EncryptionStatus (..),

    -- * EncryptionType
    EncryptionType (..),

    -- * InsightCategory
    InsightCategory (..),

    -- * InsightState
    InsightState (..),

    -- * SamplingStrategyName
    SamplingStrategyName (..),

    -- * TimeRangeType
    TimeRangeType (..),

    -- * Alias
    Alias (..),
    newAlias,
    alias_names,
    alias_name,
    alias_type,

    -- * AnnotationValue
    AnnotationValue (..),
    newAnnotationValue,
    annotationValue_numberValue,
    annotationValue_stringValue,
    annotationValue_booleanValue,

    -- * AnomalousService
    AnomalousService (..),
    newAnomalousService,
    anomalousService_serviceId,

    -- * AvailabilityZoneDetail
    AvailabilityZoneDetail (..),
    newAvailabilityZoneDetail,
    availabilityZoneDetail_name,

    -- * BackendConnectionErrors
    BackendConnectionErrors (..),
    newBackendConnectionErrors,
    backendConnectionErrors_otherCount,
    backendConnectionErrors_timeoutCount,
    backendConnectionErrors_hTTPCode5XXCount,
    backendConnectionErrors_connectionRefusedCount,
    backendConnectionErrors_hTTPCode4XXCount,
    backendConnectionErrors_unknownHostCount,

    -- * Edge
    Edge (..),
    newEdge,
    edge_startTime,
    edge_aliases,
    edge_responseTimeHistogram,
    edge_referenceId,
    edge_endTime,
    edge_summaryStatistics,

    -- * EdgeStatistics
    EdgeStatistics (..),
    newEdgeStatistics,
    edgeStatistics_faultStatistics,
    edgeStatistics_okCount,
    edgeStatistics_totalResponseTime,
    edgeStatistics_errorStatistics,
    edgeStatistics_totalCount,

    -- * EncryptionConfig
    EncryptionConfig (..),
    newEncryptionConfig,
    encryptionConfig_status,
    encryptionConfig_keyId,
    encryptionConfig_type,

    -- * ErrorRootCause
    ErrorRootCause (..),
    newErrorRootCause,
    errorRootCause_clientImpacting,
    errorRootCause_services,

    -- * ErrorRootCauseEntity
    ErrorRootCauseEntity (..),
    newErrorRootCauseEntity,
    errorRootCauseEntity_exceptions,
    errorRootCauseEntity_remote,
    errorRootCauseEntity_name,

    -- * ErrorRootCauseService
    ErrorRootCauseService (..),
    newErrorRootCauseService,
    errorRootCauseService_entityPath,
    errorRootCauseService_accountId,
    errorRootCauseService_names,
    errorRootCauseService_name,
    errorRootCauseService_inferred,
    errorRootCauseService_type,

    -- * ErrorStatistics
    ErrorStatistics (..),
    newErrorStatistics,
    errorStatistics_otherCount,
    errorStatistics_throttleCount,
    errorStatistics_totalCount,

    -- * FaultRootCause
    FaultRootCause (..),
    newFaultRootCause,
    faultRootCause_clientImpacting,
    faultRootCause_services,

    -- * FaultRootCauseEntity
    FaultRootCauseEntity (..),
    newFaultRootCauseEntity,
    faultRootCauseEntity_exceptions,
    faultRootCauseEntity_remote,
    faultRootCauseEntity_name,

    -- * FaultRootCauseService
    FaultRootCauseService (..),
    newFaultRootCauseService,
    faultRootCauseService_entityPath,
    faultRootCauseService_accountId,
    faultRootCauseService_names,
    faultRootCauseService_name,
    faultRootCauseService_inferred,
    faultRootCauseService_type,

    -- * FaultStatistics
    FaultStatistics (..),
    newFaultStatistics,
    faultStatistics_otherCount,
    faultStatistics_totalCount,

    -- * ForecastStatistics
    ForecastStatistics (..),
    newForecastStatistics,
    forecastStatistics_faultCountLow,
    forecastStatistics_faultCountHigh,

    -- * Group
    Group (..),
    newGroup,
    group_filterExpression,
    group_insightsConfiguration,
    group_groupARN,
    group_groupName,

    -- * GroupSummary
    GroupSummary (..),
    newGroupSummary,
    groupSummary_filterExpression,
    groupSummary_insightsConfiguration,
    groupSummary_groupARN,
    groupSummary_groupName,

    -- * HistogramEntry
    HistogramEntry (..),
    newHistogramEntry,
    histogramEntry_count,
    histogramEntry_value,

    -- * Http
    Http (..),
    newHttp,
    http_httpMethod,
    http_httpStatus,
    http_clientIp,
    http_userAgent,
    http_httpURL,

    -- * Insight
    Insight (..),
    newInsight,
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

    -- * InsightEvent
    InsightEvent (..),
    newInsightEvent,
    insightEvent_summary,
    insightEvent_eventTime,
    insightEvent_rootCauseServiceRequestImpactStatistics,
    insightEvent_topAnomalousServices,
    insightEvent_clientRequestImpactStatistics,

    -- * InsightImpactGraphEdge
    InsightImpactGraphEdge (..),
    newInsightImpactGraphEdge,
    insightImpactGraphEdge_referenceId,

    -- * InsightImpactGraphService
    InsightImpactGraphService (..),
    newInsightImpactGraphService,
    insightImpactGraphService_referenceId,
    insightImpactGraphService_accountId,
    insightImpactGraphService_names,
    insightImpactGraphService_name,
    insightImpactGraphService_type,
    insightImpactGraphService_edges,

    -- * InsightSummary
    InsightSummary (..),
    newInsightSummary,
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

    -- * InsightsConfiguration
    InsightsConfiguration (..),
    newInsightsConfiguration,
    insightsConfiguration_notificationsEnabled,
    insightsConfiguration_insightsEnabled,

    -- * InstanceIdDetail
    InstanceIdDetail (..),
    newInstanceIdDetail,
    instanceIdDetail_id,

    -- * RequestImpactStatistics
    RequestImpactStatistics (..),
    newRequestImpactStatistics,
    requestImpactStatistics_okCount,
    requestImpactStatistics_faultCount,
    requestImpactStatistics_totalCount,

    -- * ResourceARNDetail
    ResourceARNDetail (..),
    newResourceARNDetail,
    resourceARNDetail_arn,

    -- * ResponseTimeRootCause
    ResponseTimeRootCause (..),
    newResponseTimeRootCause,
    responseTimeRootCause_clientImpacting,
    responseTimeRootCause_services,

    -- * ResponseTimeRootCauseEntity
    ResponseTimeRootCauseEntity (..),
    newResponseTimeRootCauseEntity,
    responseTimeRootCauseEntity_remote,
    responseTimeRootCauseEntity_coverage,
    responseTimeRootCauseEntity_name,

    -- * ResponseTimeRootCauseService
    ResponseTimeRootCauseService (..),
    newResponseTimeRootCauseService,
    responseTimeRootCauseService_entityPath,
    responseTimeRootCauseService_accountId,
    responseTimeRootCauseService_names,
    responseTimeRootCauseService_name,
    responseTimeRootCauseService_inferred,
    responseTimeRootCauseService_type,

    -- * RootCauseException
    RootCauseException (..),
    newRootCauseException,
    rootCauseException_name,
    rootCauseException_message,

    -- * SamplingRule
    SamplingRule (..),
    newSamplingRule,
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

    -- * SamplingRuleRecord
    SamplingRuleRecord (..),
    newSamplingRuleRecord,
    samplingRuleRecord_modifiedAt,
    samplingRuleRecord_samplingRule,
    samplingRuleRecord_createdAt,

    -- * SamplingRuleUpdate
    SamplingRuleUpdate (..),
    newSamplingRuleUpdate,
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

    -- * SamplingStatisticSummary
    SamplingStatisticSummary (..),
    newSamplingStatisticSummary,
    samplingStatisticSummary_requestCount,
    samplingStatisticSummary_borrowCount,
    samplingStatisticSummary_ruleName,
    samplingStatisticSummary_timestamp,
    samplingStatisticSummary_sampledCount,

    -- * SamplingStatisticsDocument
    SamplingStatisticsDocument (..),
    newSamplingStatisticsDocument,
    samplingStatisticsDocument_borrowCount,
    samplingStatisticsDocument_ruleName,
    samplingStatisticsDocument_clientID,
    samplingStatisticsDocument_timestamp,
    samplingStatisticsDocument_requestCount,
    samplingStatisticsDocument_sampledCount,

    -- * SamplingStrategy
    SamplingStrategy (..),
    newSamplingStrategy,
    samplingStrategy_value,
    samplingStrategy_name,

    -- * SamplingTargetDocument
    SamplingTargetDocument (..),
    newSamplingTargetDocument,
    samplingTargetDocument_reservoirQuota,
    samplingTargetDocument_ruleName,
    samplingTargetDocument_fixedRate,
    samplingTargetDocument_interval,
    samplingTargetDocument_reservoirQuotaTTL,

    -- * Segment
    Segment (..),
    newSegment,
    segment_document,
    segment_id,

    -- * ServiceId
    ServiceId (..),
    newServiceId,
    serviceId_accountId,
    serviceId_names,
    serviceId_name,
    serviceId_type,

    -- * ServiceInfo
    ServiceInfo (..),
    newServiceInfo,
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

    -- * ServiceStatistics
    ServiceStatistics (..),
    newServiceStatistics,
    serviceStatistics_faultStatistics,
    serviceStatistics_okCount,
    serviceStatistics_totalResponseTime,
    serviceStatistics_errorStatistics,
    serviceStatistics_totalCount,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TelemetryRecord
    TelemetryRecord (..),
    newTelemetryRecord,
    telemetryRecord_segmentsReceivedCount,
    telemetryRecord_segmentsSentCount,
    telemetryRecord_segmentsSpilloverCount,
    telemetryRecord_segmentsRejectedCount,
    telemetryRecord_backendConnectionErrors,
    telemetryRecord_timestamp,

    -- * TimeSeriesServiceStatistics
    TimeSeriesServiceStatistics (..),
    newTimeSeriesServiceStatistics,
    timeSeriesServiceStatistics_serviceSummaryStatistics,
    timeSeriesServiceStatistics_responseTimeHistogram,
    timeSeriesServiceStatistics_edgeSummaryStatistics,
    timeSeriesServiceStatistics_serviceForecastStatistics,
    timeSeriesServiceStatistics_timestamp,

    -- * Trace
    Trace (..),
    newTrace,
    trace_limitExceeded,
    trace_id,
    trace_segments,
    trace_duration,

    -- * TraceSummary
    TraceSummary (..),
    newTraceSummary,
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

    -- * TraceUser
    TraceUser (..),
    newTraceUser,
    traceUser_serviceIds,
    traceUser_userName,

    -- * UnprocessedStatistics
    UnprocessedStatistics (..),
    newUnprocessedStatistics,
    unprocessedStatistics_ruleName,
    unprocessedStatistics_errorCode,
    unprocessedStatistics_message,

    -- * UnprocessedTraceSegment
    UnprocessedTraceSegment (..),
    newUnprocessedTraceSegment,
    unprocessedTraceSegment_errorCode,
    unprocessedTraceSegment_id,
    unprocessedTraceSegment_message,

    -- * ValueWithServiceIds
    ValueWithServiceIds (..),
    newValueWithServiceIds,
    valueWithServiceIds_serviceIds,
    valueWithServiceIds_annotationValue,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.XRay.Types.Alias
import Amazonka.XRay.Types.AnnotationValue
import Amazonka.XRay.Types.AnomalousService
import Amazonka.XRay.Types.AvailabilityZoneDetail
import Amazonka.XRay.Types.BackendConnectionErrors
import Amazonka.XRay.Types.Edge
import Amazonka.XRay.Types.EdgeStatistics
import Amazonka.XRay.Types.EncryptionConfig
import Amazonka.XRay.Types.EncryptionStatus
import Amazonka.XRay.Types.EncryptionType
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
import Amazonka.XRay.Types.InsightCategory
import Amazonka.XRay.Types.InsightEvent
import Amazonka.XRay.Types.InsightImpactGraphEdge
import Amazonka.XRay.Types.InsightImpactGraphService
import Amazonka.XRay.Types.InsightState
import Amazonka.XRay.Types.InsightSummary
import Amazonka.XRay.Types.InsightsConfiguration
import Amazonka.XRay.Types.InstanceIdDetail
import Amazonka.XRay.Types.RequestImpactStatistics
import Amazonka.XRay.Types.ResourceARNDetail
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
import Amazonka.XRay.Types.SamplingStrategyName
import Amazonka.XRay.Types.SamplingTargetDocument
import Amazonka.XRay.Types.Segment
import Amazonka.XRay.Types.ServiceId
import Amazonka.XRay.Types.ServiceInfo
import Amazonka.XRay.Types.ServiceStatistics
import Amazonka.XRay.Types.Tag
import Amazonka.XRay.Types.TelemetryRecord
import Amazonka.XRay.Types.TimeRangeType
import Amazonka.XRay.Types.TimeSeriesServiceStatistics
import Amazonka.XRay.Types.Trace
import Amazonka.XRay.Types.TraceSummary
import Amazonka.XRay.Types.TraceUser
import Amazonka.XRay.Types.UnprocessedStatistics
import Amazonka.XRay.Types.UnprocessedTraceSegment
import Amazonka.XRay.Types.ValueWithServiceIds

-- | API version @2016-04-12@ of the Amazon X-Ray SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "XRay",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "xray",
      Core._serviceSigningName = "xray",
      Core._serviceVersion = "2016-04-12",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "XRay",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request is missing required parameters or has invalid parameters.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | You have reached the maximum number of sampling rules.
_RuleLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RuleLimitExceededException =
  Core._MatchServiceError
    defaultService
    "RuleLimitExceededException"

-- | The request exceeds the maximum number of requests per second.
_ThrottledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottledException =
  Core._MatchServiceError
    defaultService
    "ThrottledException"
    Prelude.. Core.hasStatus 429

-- | You have exceeded the maximum number of tags you can apply to this
-- resource.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- | The resource was not found. Verify that the name or Amazon Resource Name
-- (ARN) of the resource is correct.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

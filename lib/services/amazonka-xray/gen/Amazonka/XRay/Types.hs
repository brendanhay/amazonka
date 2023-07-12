{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.XRay.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidPolicyRevisionIdException,
    _InvalidRequestException,
    _LockoutPreventionException,
    _MalformedPolicyDocumentException,
    _PolicyCountLimitExceededException,
    _PolicySizeLimitExceededException,
    _ResourceNotFoundException,
    _RuleLimitExceededException,
    _ThrottledException,
    _TooManyTagsException,

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
    alias_name,
    alias_names,
    alias_type,

    -- * AnnotationValue
    AnnotationValue (..),
    newAnnotationValue,
    annotationValue_booleanValue,
    annotationValue_numberValue,
    annotationValue_stringValue,

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
    backendConnectionErrors_connectionRefusedCount,
    backendConnectionErrors_hTTPCode4XXCount,
    backendConnectionErrors_hTTPCode5XXCount,
    backendConnectionErrors_otherCount,
    backendConnectionErrors_timeoutCount,
    backendConnectionErrors_unknownHostCount,

    -- * Edge
    Edge (..),
    newEdge,
    edge_aliases,
    edge_edgeType,
    edge_endTime,
    edge_receivedEventAgeHistogram,
    edge_referenceId,
    edge_responseTimeHistogram,
    edge_startTime,
    edge_summaryStatistics,

    -- * EdgeStatistics
    EdgeStatistics (..),
    newEdgeStatistics,
    edgeStatistics_errorStatistics,
    edgeStatistics_faultStatistics,
    edgeStatistics_okCount,
    edgeStatistics_totalCount,
    edgeStatistics_totalResponseTime,

    -- * EncryptionConfig
    EncryptionConfig (..),
    newEncryptionConfig,
    encryptionConfig_keyId,
    encryptionConfig_status,
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
    errorRootCauseEntity_name,
    errorRootCauseEntity_remote,

    -- * ErrorRootCauseService
    ErrorRootCauseService (..),
    newErrorRootCauseService,
    errorRootCauseService_accountId,
    errorRootCauseService_entityPath,
    errorRootCauseService_inferred,
    errorRootCauseService_name,
    errorRootCauseService_names,
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
    faultRootCauseEntity_name,
    faultRootCauseEntity_remote,

    -- * FaultRootCauseService
    FaultRootCauseService (..),
    newFaultRootCauseService,
    faultRootCauseService_accountId,
    faultRootCauseService_entityPath,
    faultRootCauseService_inferred,
    faultRootCauseService_name,
    faultRootCauseService_names,
    faultRootCauseService_type,

    -- * FaultStatistics
    FaultStatistics (..),
    newFaultStatistics,
    faultStatistics_otherCount,
    faultStatistics_totalCount,

    -- * ForecastStatistics
    ForecastStatistics (..),
    newForecastStatistics,
    forecastStatistics_faultCountHigh,
    forecastStatistics_faultCountLow,

    -- * Group
    Group (..),
    newGroup,
    group_filterExpression,
    group_groupARN,
    group_groupName,
    group_insightsConfiguration,

    -- * GroupSummary
    GroupSummary (..),
    newGroupSummary,
    groupSummary_filterExpression,
    groupSummary_groupARN,
    groupSummary_groupName,
    groupSummary_insightsConfiguration,

    -- * HistogramEntry
    HistogramEntry (..),
    newHistogramEntry,
    histogramEntry_count,
    histogramEntry_value,

    -- * Http
    Http (..),
    newHttp,
    http_clientIp,
    http_httpMethod,
    http_httpStatus,
    http_httpURL,
    http_userAgent,

    -- * Insight
    Insight (..),
    newInsight,
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

    -- * InsightEvent
    InsightEvent (..),
    newInsightEvent,
    insightEvent_clientRequestImpactStatistics,
    insightEvent_eventTime,
    insightEvent_rootCauseServiceRequestImpactStatistics,
    insightEvent_summary,
    insightEvent_topAnomalousServices,

    -- * InsightImpactGraphEdge
    InsightImpactGraphEdge (..),
    newInsightImpactGraphEdge,
    insightImpactGraphEdge_referenceId,

    -- * InsightImpactGraphService
    InsightImpactGraphService (..),
    newInsightImpactGraphService,
    insightImpactGraphService_accountId,
    insightImpactGraphService_edges,
    insightImpactGraphService_name,
    insightImpactGraphService_names,
    insightImpactGraphService_referenceId,
    insightImpactGraphService_type,

    -- * InsightSummary
    InsightSummary (..),
    newInsightSummary,
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

    -- * InsightsConfiguration
    InsightsConfiguration (..),
    newInsightsConfiguration,
    insightsConfiguration_insightsEnabled,
    insightsConfiguration_notificationsEnabled,

    -- * InstanceIdDetail
    InstanceIdDetail (..),
    newInstanceIdDetail,
    instanceIdDetail_id,

    -- * RequestImpactStatistics
    RequestImpactStatistics (..),
    newRequestImpactStatistics,
    requestImpactStatistics_faultCount,
    requestImpactStatistics_okCount,
    requestImpactStatistics_totalCount,

    -- * ResourceARNDetail
    ResourceARNDetail (..),
    newResourceARNDetail,
    resourceARNDetail_arn,

    -- * ResourcePolicy
    ResourcePolicy (..),
    newResourcePolicy,
    resourcePolicy_lastUpdatedTime,
    resourcePolicy_policyDocument,
    resourcePolicy_policyName,
    resourcePolicy_policyRevisionId,

    -- * ResponseTimeRootCause
    ResponseTimeRootCause (..),
    newResponseTimeRootCause,
    responseTimeRootCause_clientImpacting,
    responseTimeRootCause_services,

    -- * ResponseTimeRootCauseEntity
    ResponseTimeRootCauseEntity (..),
    newResponseTimeRootCauseEntity,
    responseTimeRootCauseEntity_coverage,
    responseTimeRootCauseEntity_name,
    responseTimeRootCauseEntity_remote,

    -- * ResponseTimeRootCauseService
    ResponseTimeRootCauseService (..),
    newResponseTimeRootCauseService,
    responseTimeRootCauseService_accountId,
    responseTimeRootCauseService_entityPath,
    responseTimeRootCauseService_inferred,
    responseTimeRootCauseService_name,
    responseTimeRootCauseService_names,
    responseTimeRootCauseService_type,

    -- * RootCauseException
    RootCauseException (..),
    newRootCauseException,
    rootCauseException_message,
    rootCauseException_name,

    -- * SamplingRule
    SamplingRule (..),
    newSamplingRule,
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

    -- * SamplingRuleRecord
    SamplingRuleRecord (..),
    newSamplingRuleRecord,
    samplingRuleRecord_createdAt,
    samplingRuleRecord_modifiedAt,
    samplingRuleRecord_samplingRule,

    -- * SamplingRuleUpdate
    SamplingRuleUpdate (..),
    newSamplingRuleUpdate,
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

    -- * SamplingStatisticSummary
    SamplingStatisticSummary (..),
    newSamplingStatisticSummary,
    samplingStatisticSummary_borrowCount,
    samplingStatisticSummary_requestCount,
    samplingStatisticSummary_ruleName,
    samplingStatisticSummary_sampledCount,
    samplingStatisticSummary_timestamp,

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
    samplingStrategy_name,
    samplingStrategy_value,

    -- * SamplingTargetDocument
    SamplingTargetDocument (..),
    newSamplingTargetDocument,
    samplingTargetDocument_fixedRate,
    samplingTargetDocument_interval,
    samplingTargetDocument_reservoirQuota,
    samplingTargetDocument_reservoirQuotaTTL,
    samplingTargetDocument_ruleName,

    -- * Segment
    Segment (..),
    newSegment,
    segment_document,
    segment_id,

    -- * ServiceId
    ServiceId (..),
    newServiceId,
    serviceId_accountId,
    serviceId_name,
    serviceId_names,
    serviceId_type,

    -- * ServiceInfo
    ServiceInfo (..),
    newServiceInfo,
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

    -- * ServiceStatistics
    ServiceStatistics (..),
    newServiceStatistics,
    serviceStatistics_errorStatistics,
    serviceStatistics_faultStatistics,
    serviceStatistics_okCount,
    serviceStatistics_totalCount,
    serviceStatistics_totalResponseTime,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TelemetryRecord
    TelemetryRecord (..),
    newTelemetryRecord,
    telemetryRecord_backendConnectionErrors,
    telemetryRecord_segmentsReceivedCount,
    telemetryRecord_segmentsRejectedCount,
    telemetryRecord_segmentsSentCount,
    telemetryRecord_segmentsSpilloverCount,
    telemetryRecord_timestamp,

    -- * TimeSeriesServiceStatistics
    TimeSeriesServiceStatistics (..),
    newTimeSeriesServiceStatistics,
    timeSeriesServiceStatistics_edgeSummaryStatistics,
    timeSeriesServiceStatistics_responseTimeHistogram,
    timeSeriesServiceStatistics_serviceForecastStatistics,
    timeSeriesServiceStatistics_serviceSummaryStatistics,
    timeSeriesServiceStatistics_timestamp,

    -- * Trace
    Trace (..),
    newTrace,
    trace_duration,
    trace_id,
    trace_limitExceeded,
    trace_segments,

    -- * TraceSummary
    TraceSummary (..),
    newTraceSummary,
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

    -- * TraceUser
    TraceUser (..),
    newTraceUser,
    traceUser_serviceIds,
    traceUser_userName,

    -- * UnprocessedStatistics
    UnprocessedStatistics (..),
    newUnprocessedStatistics,
    unprocessedStatistics_errorCode,
    unprocessedStatistics_message,
    unprocessedStatistics_ruleName,

    -- * UnprocessedTraceSegment
    UnprocessedTraceSegment (..),
    newUnprocessedTraceSegment,
    unprocessedTraceSegment_errorCode,
    unprocessedTraceSegment_id,
    unprocessedTraceSegment_message,

    -- * ValueWithServiceIds
    ValueWithServiceIds (..),
    newValueWithServiceIds,
    valueWithServiceIds_annotationValue,
    valueWithServiceIds_serviceIds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
    { Core.abbrev = "XRay",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "xray",
      Core.signingName = "xray",
      Core.version = "2016-04-12",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "XRay",
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | A policy revision id was provided which does not match the latest policy
-- revision. This exception is also if a policy revision id of 0 is
-- provided via @PutResourcePolicy@ and a policy with the same name already
-- exists.
_InvalidPolicyRevisionIdException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPolicyRevisionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyRevisionIdException"
    Prelude.. Core.hasStatus 400

-- | The request is missing required parameters or has invalid parameters.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The provided resource policy would prevent the caller of this request
-- from calling PutResourcePolicy in the future.
_LockoutPreventionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LockoutPreventionException =
  Core._MatchServiceError
    defaultService
    "LockoutPreventionException"
    Prelude.. Core.hasStatus 400

-- | Invalid policy document provided in request.
_MalformedPolicyDocumentException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MalformedPolicyDocumentException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyDocumentException"
    Prelude.. Core.hasStatus 400

-- | Exceeded the maximum number of resource policies for a target Amazon Web
-- Services account.
_PolicyCountLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicyCountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "PolicyCountLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | Exceeded the maximum size for a resource policy.
_PolicySizeLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PolicySizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "PolicySizeLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The resource was not found. Verify that the name or Amazon Resource Name
-- (ARN) of the resource is correct.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You have reached the maximum number of sampling rules.
_RuleLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RuleLimitExceededException =
  Core._MatchServiceError
    defaultService
    "RuleLimitExceededException"

-- | The request exceeds the maximum number of requests per second.
_ThrottledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottledException =
  Core._MatchServiceError
    defaultService
    "ThrottledException"
    Prelude.. Core.hasStatus 429

-- | You have exceeded the maximum number of tags you can apply to this
-- resource.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

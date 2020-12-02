{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types
  ( -- * Service Configuration
    xRay,

    -- * Errors

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
    Alias,
    alias,
    aNames,
    aName,
    aType,

    -- * AnnotationValue
    AnnotationValue,
    annotationValue,
    avNumberValue,
    avStringValue,
    avBooleanValue,

    -- * AnomalousService
    AnomalousService,
    anomalousService,
    asServiceId,

    -- * AvailabilityZoneDetail
    AvailabilityZoneDetail,
    availabilityZoneDetail,
    azdName,

    -- * BackendConnectionErrors
    BackendConnectionErrors,
    backendConnectionErrors,
    bceOtherCount,
    bceTimeoutCount,
    bceHTTPCode5XXCount,
    bceConnectionRefusedCount,
    bceHTTPCode4XXCount,
    bceUnknownHostCount,

    -- * Edge
    Edge,
    edge,
    eStartTime,
    eAliases,
    eResponseTimeHistogram,
    eReferenceId,
    eEndTime,
    eSummaryStatistics,

    -- * EdgeStatistics
    EdgeStatistics,
    edgeStatistics,
    esFaultStatistics,
    esOKCount,
    esTotalResponseTime,
    esErrorStatistics,
    esTotalCount,

    -- * EncryptionConfig
    EncryptionConfig,
    encryptionConfig,
    ecStatus,
    ecKeyId,
    ecType,

    -- * ErrorRootCause
    ErrorRootCause,
    errorRootCause,
    ercClientImpacting,
    ercServices,

    -- * ErrorRootCauseEntity
    ErrorRootCauseEntity,
    errorRootCauseEntity,
    erceExceptions,
    erceRemote,
    erceName,

    -- * ErrorRootCauseService
    ErrorRootCauseService,
    errorRootCauseService,
    ercsEntityPath,
    ercsAccountId,
    ercsNames,
    ercsName,
    ercsInferred,
    ercsType,

    -- * ErrorStatistics
    ErrorStatistics,
    errorStatistics,
    eOtherCount,
    eThrottleCount,
    eTotalCount,

    -- * FaultRootCause
    FaultRootCause,
    faultRootCause,
    frcClientImpacting,
    frcServices,

    -- * FaultRootCauseEntity
    FaultRootCauseEntity,
    faultRootCauseEntity,
    frceExceptions,
    frceRemote,
    frceName,

    -- * FaultRootCauseService
    FaultRootCauseService,
    faultRootCauseService,
    frcsEntityPath,
    frcsAccountId,
    frcsNames,
    frcsName,
    frcsInferred,
    frcsType,

    -- * FaultStatistics
    FaultStatistics,
    faultStatistics,
    fsOtherCount,
    fsTotalCount,

    -- * ForecastStatistics
    ForecastStatistics,
    forecastStatistics,
    fsFaultCountLow,
    fsFaultCountHigh,

    -- * Group
    Group,
    group',
    gFilterExpression,
    gInsightsConfiguration,
    gGroupARN,
    gGroupName,

    -- * GroupSummary
    GroupSummary,
    groupSummary,
    gsFilterExpression,
    gsInsightsConfiguration,
    gsGroupARN,
    gsGroupName,

    -- * HTTP
    HTTP,
    hTTP,
    httpHTTPMethod,
    httpHTTPStatus,
    httpClientIP,
    httpUserAgent,
    httpHTTPURL,

    -- * HistogramEntry
    HistogramEntry,
    histogramEntry,
    heCount,
    heValue,

    -- * Insight
    Insight,
    insight,
    iSummary,
    iState,
    iStartTime,
    iInsightId,
    iCategories,
    iRootCauseServiceRequestImpactStatistics,
    iTopAnomalousServices,
    iRootCauseServiceId,
    iClientRequestImpactStatistics,
    iEndTime,
    iGroupARN,
    iGroupName,

    -- * InsightEvent
    InsightEvent,
    insightEvent,
    ieSummary,
    ieEventTime,
    ieRootCauseServiceRequestImpactStatistics,
    ieTopAnomalousServices,
    ieClientRequestImpactStatistics,

    -- * InsightImpactGraphEdge
    InsightImpactGraphEdge,
    insightImpactGraphEdge,
    iigeReferenceId,

    -- * InsightImpactGraphService
    InsightImpactGraphService,
    insightImpactGraphService,
    iigsReferenceId,
    iigsAccountId,
    iigsNames,
    iigsName,
    iigsType,
    iigsEdges,

    -- * InsightSummary
    InsightSummary,
    insightSummary,
    isSummary,
    isState,
    isStartTime,
    isInsightId,
    isCategories,
    isRootCauseServiceRequestImpactStatistics,
    isTopAnomalousServices,
    isRootCauseServiceId,
    isClientRequestImpactStatistics,
    isEndTime,
    isGroupARN,
    isGroupName,
    isLastUpdateTime,

    -- * InsightsConfiguration
    InsightsConfiguration,
    insightsConfiguration,
    icNotificationsEnabled,
    icInsightsEnabled,

    -- * InstanceIdDetail
    InstanceIdDetail,
    instanceIdDetail,
    iidId,

    -- * RequestImpactStatistics
    RequestImpactStatistics,
    requestImpactStatistics,
    risOKCount,
    risFaultCount,
    risTotalCount,

    -- * ResourceARNDetail
    ResourceARNDetail,
    resourceARNDetail,
    radARN,

    -- * ResponseTimeRootCause
    ResponseTimeRootCause,
    responseTimeRootCause,
    rtrcClientImpacting,
    rtrcServices,

    -- * ResponseTimeRootCauseEntity
    ResponseTimeRootCauseEntity,
    responseTimeRootCauseEntity,
    rtrceRemote,
    rtrceCoverage,
    rtrceName,

    -- * ResponseTimeRootCauseService
    ResponseTimeRootCauseService,
    responseTimeRootCauseService,
    rtrcsEntityPath,
    rtrcsAccountId,
    rtrcsNames,
    rtrcsName,
    rtrcsInferred,
    rtrcsType,

    -- * RootCauseException
    RootCauseException,
    rootCauseException,
    rceName,
    rceMessage,

    -- * SamplingRule
    SamplingRule,
    samplingRule,
    srRuleName,
    srAttributes,
    srRuleARN,
    srResourceARN,
    srPriority,
    srFixedRate,
    srReservoirSize,
    srServiceName,
    srServiceType,
    srHost,
    srHTTPMethod,
    srURLPath,
    srVersion,

    -- * SamplingRuleRecord
    SamplingRuleRecord,
    samplingRuleRecord,
    srrModifiedAt,
    srrSamplingRule,
    srrCreatedAt,

    -- * SamplingRuleUpdate
    SamplingRuleUpdate,
    samplingRuleUpdate,
    sruHTTPMethod,
    sruPriority,
    sruRuleName,
    sruReservoirSize,
    sruFixedRate,
    sruResourceARN,
    sruAttributes,
    sruServiceName,
    sruServiceType,
    sruHost,
    sruRuleARN,
    sruURLPath,

    -- * SamplingStatisticSummary
    SamplingStatisticSummary,
    samplingStatisticSummary,
    sssRequestCount,
    sssBorrowCount,
    sssRuleName,
    sssTimestamp,
    sssSampledCount,

    -- * SamplingStatisticsDocument
    SamplingStatisticsDocument,
    samplingStatisticsDocument,
    ssdBorrowCount,
    ssdRuleName,
    ssdClientId,
    ssdTimestamp,
    ssdRequestCount,
    ssdSampledCount,

    -- * SamplingStrategy
    SamplingStrategy,
    samplingStrategy,
    ssValue,
    ssName,

    -- * SamplingTargetDocument
    SamplingTargetDocument,
    samplingTargetDocument,
    stdReservoirQuota,
    stdRuleName,
    stdFixedRate,
    stdInterval,
    stdReservoirQuotaTTL,

    -- * Segment
    Segment,
    segment,
    sDocument,
    sId,

    -- * ServiceId
    ServiceId,
    serviceId,
    siAccountId,
    siNames,
    siName,
    siType,

    -- * ServiceInfo
    ServiceInfo,
    serviceInfo,
    sState,
    sStartTime,
    sRoot,
    sResponseTimeHistogram,
    sDurationHistogram,
    sReferenceId,
    sAccountId,
    sNames,
    sName,
    sEndTime,
    sType,
    sEdges,
    sSummaryStatistics,

    -- * ServiceStatistics
    ServiceStatistics,
    serviceStatistics,
    ssFaultStatistics,
    ssOKCount,
    ssTotalResponseTime,
    ssErrorStatistics,
    ssTotalCount,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TelemetryRecord
    TelemetryRecord,
    telemetryRecord,
    trSegmentsReceivedCount,
    trSegmentsSentCount,
    trSegmentsSpilloverCount,
    trSegmentsRejectedCount,
    trBackendConnectionErrors,
    trTimestamp,

    -- * TimeSeriesServiceStatistics
    TimeSeriesServiceStatistics,
    timeSeriesServiceStatistics,
    tsssServiceSummaryStatistics,
    tsssResponseTimeHistogram,
    tsssEdgeSummaryStatistics,
    tsssServiceForecastStatistics,
    tsssTimestamp,

    -- * Trace
    Trace,
    trace,
    tLimitExceeded,
    tId,
    tSegments,
    tDuration,

    -- * TraceSummary
    TraceSummary,
    traceSummary,
    tsAnnotations,
    tsHasThrottle,
    tsUsers,
    tsEntryPoint,
    tsHasFault,
    tsServiceIds,
    tsMatchedEventTime,
    tsIsPartial,
    tsErrorRootCauses,
    tsResourceARNs,
    tsAvailabilityZones,
    tsInstanceIds,
    tsResponseTimeRootCauses,
    tsHasError,
    tsId,
    tsHTTP,
    tsRevision,
    tsDuration,
    tsFaultRootCauses,
    tsResponseTime,

    -- * TraceUser
    TraceUser,
    traceUser,
    tuServiceIds,
    tuUserName,

    -- * UnprocessedStatistics
    UnprocessedStatistics,
    unprocessedStatistics,
    usRuleName,
    usErrorCode,
    usMessage,

    -- * UnprocessedTraceSegment
    UnprocessedTraceSegment,
    unprocessedTraceSegment,
    utsErrorCode,
    utsId,
    utsMessage,

    -- * ValueWithServiceIds
    ValueWithServiceIds,
    valueWithServiceIds,
    vwsiServiceIds,
    vwsiAnnotationValue,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.XRay.Types.Alias
import Network.AWS.XRay.Types.AnnotationValue
import Network.AWS.XRay.Types.AnomalousService
import Network.AWS.XRay.Types.AvailabilityZoneDetail
import Network.AWS.XRay.Types.BackendConnectionErrors
import Network.AWS.XRay.Types.Edge
import Network.AWS.XRay.Types.EdgeStatistics
import Network.AWS.XRay.Types.EncryptionConfig
import Network.AWS.XRay.Types.EncryptionStatus
import Network.AWS.XRay.Types.EncryptionType
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
import Network.AWS.XRay.Types.HTTP
import Network.AWS.XRay.Types.HistogramEntry
import Network.AWS.XRay.Types.Insight
import Network.AWS.XRay.Types.InsightCategory
import Network.AWS.XRay.Types.InsightEvent
import Network.AWS.XRay.Types.InsightImpactGraphEdge
import Network.AWS.XRay.Types.InsightImpactGraphService
import Network.AWS.XRay.Types.InsightState
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
import Network.AWS.XRay.Types.SamplingStrategyName
import Network.AWS.XRay.Types.SamplingTargetDocument
import Network.AWS.XRay.Types.Segment
import Network.AWS.XRay.Types.ServiceId
import Network.AWS.XRay.Types.ServiceInfo
import Network.AWS.XRay.Types.ServiceStatistics
import Network.AWS.XRay.Types.Tag
import Network.AWS.XRay.Types.TelemetryRecord
import Network.AWS.XRay.Types.TimeRangeType
import Network.AWS.XRay.Types.TimeSeriesServiceStatistics
import Network.AWS.XRay.Types.Trace
import Network.AWS.XRay.Types.TraceSummary
import Network.AWS.XRay.Types.TraceUser
import Network.AWS.XRay.Types.UnprocessedStatistics
import Network.AWS.XRay.Types.UnprocessedTraceSegment
import Network.AWS.XRay.Types.ValueWithServiceIds

-- | API version @2016-04-12@ of the Amazon X-Ray SDK configuration.
xRay :: Service
xRay =
  Service
    { _svcAbbrev = "XRay",
      _svcSigner = v4,
      _svcPrefix = "xray",
      _svcVersion = "2016-04-12",
      _svcEndpoint = defaultEndpoint xRay,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "XRay",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

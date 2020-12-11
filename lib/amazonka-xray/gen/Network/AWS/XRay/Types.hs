-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types
  ( -- * Service configuration
    xRayService,

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
    Alias (..),
    mkAlias,
    aNames,
    aName,
    aType,

    -- * AnnotationValue
    AnnotationValue (..),
    mkAnnotationValue,
    avNumberValue,
    avStringValue,
    avBooleanValue,

    -- * AnomalousService
    AnomalousService (..),
    mkAnomalousService,
    asServiceId,

    -- * AvailabilityZoneDetail
    AvailabilityZoneDetail (..),
    mkAvailabilityZoneDetail,
    azdName,

    -- * BackendConnectionErrors
    BackendConnectionErrors (..),
    mkBackendConnectionErrors,
    bceOtherCount,
    bceTimeoutCount,
    bceHTTPCode5XXCount,
    bceConnectionRefusedCount,
    bceHTTPCode4XXCount,
    bceUnknownHostCount,

    -- * Edge
    Edge (..),
    mkEdge,
    eStartTime,
    eAliases,
    eResponseTimeHistogram,
    eReferenceId,
    eEndTime,
    eSummaryStatistics,

    -- * EdgeStatistics
    EdgeStatistics (..),
    mkEdgeStatistics,
    esFaultStatistics,
    esOKCount,
    esTotalResponseTime,
    esErrorStatistics,
    esTotalCount,

    -- * EncryptionConfig
    EncryptionConfig (..),
    mkEncryptionConfig,
    ecStatus,
    ecKeyId,
    ecType,

    -- * ErrorRootCause
    ErrorRootCause (..),
    mkErrorRootCause,
    ercClientImpacting,
    ercServices,

    -- * ErrorRootCauseEntity
    ErrorRootCauseEntity (..),
    mkErrorRootCauseEntity,
    erceExceptions,
    erceRemote,
    erceName,

    -- * ErrorRootCauseService
    ErrorRootCauseService (..),
    mkErrorRootCauseService,
    ercsEntityPath,
    ercsAccountId,
    ercsNames,
    ercsName,
    ercsInferred,
    ercsType,

    -- * ErrorStatistics
    ErrorStatistics (..),
    mkErrorStatistics,
    eOtherCount,
    eThrottleCount,
    eTotalCount,

    -- * FaultRootCause
    FaultRootCause (..),
    mkFaultRootCause,
    frcClientImpacting,
    frcServices,

    -- * FaultRootCauseEntity
    FaultRootCauseEntity (..),
    mkFaultRootCauseEntity,
    frceExceptions,
    frceRemote,
    frceName,

    -- * FaultRootCauseService
    FaultRootCauseService (..),
    mkFaultRootCauseService,
    frcsEntityPath,
    frcsAccountId,
    frcsNames,
    frcsName,
    frcsInferred,
    frcsType,

    -- * FaultStatistics
    FaultStatistics (..),
    mkFaultStatistics,
    fsOtherCount,
    fsTotalCount,

    -- * ForecastStatistics
    ForecastStatistics (..),
    mkForecastStatistics,
    fsFaultCountLow,
    fsFaultCountHigh,

    -- * Group
    Group (..),
    mkGroup,
    gFilterExpression,
    gInsightsConfiguration,
    gGroupARN,
    gGroupName,

    -- * GroupSummary
    GroupSummary (..),
    mkGroupSummary,
    gsFilterExpression,
    gsInsightsConfiguration,
    gsGroupARN,
    gsGroupName,

    -- * HTTP
    HTTP (..),
    mkHTTP,
    httpHTTPMethod,
    httpHTTPStatus,
    httpClientIP,
    httpUserAgent,
    httpHTTPURL,

    -- * HistogramEntry
    HistogramEntry (..),
    mkHistogramEntry,
    heCount,
    heValue,

    -- * Insight
    Insight (..),
    mkInsight,
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
    InsightEvent (..),
    mkInsightEvent,
    ieSummary,
    ieEventTime,
    ieRootCauseServiceRequestImpactStatistics,
    ieTopAnomalousServices,
    ieClientRequestImpactStatistics,

    -- * InsightImpactGraphEdge
    InsightImpactGraphEdge (..),
    mkInsightImpactGraphEdge,
    iigeReferenceId,

    -- * InsightImpactGraphService
    InsightImpactGraphService (..),
    mkInsightImpactGraphService,
    iigsReferenceId,
    iigsAccountId,
    iigsNames,
    iigsName,
    iigsType,
    iigsEdges,

    -- * InsightSummary
    InsightSummary (..),
    mkInsightSummary,
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
    InsightsConfiguration (..),
    mkInsightsConfiguration,
    icNotificationsEnabled,
    icInsightsEnabled,

    -- * InstanceIdDetail
    InstanceIdDetail (..),
    mkInstanceIdDetail,
    iidId,

    -- * RequestImpactStatistics
    RequestImpactStatistics (..),
    mkRequestImpactStatistics,
    risOKCount,
    risFaultCount,
    risTotalCount,

    -- * ResourceARNDetail
    ResourceARNDetail (..),
    mkResourceARNDetail,
    radARN,

    -- * ResponseTimeRootCause
    ResponseTimeRootCause (..),
    mkResponseTimeRootCause,
    rtrcClientImpacting,
    rtrcServices,

    -- * ResponseTimeRootCauseEntity
    ResponseTimeRootCauseEntity (..),
    mkResponseTimeRootCauseEntity,
    rtrceRemote,
    rtrceCoverage,
    rtrceName,

    -- * ResponseTimeRootCauseService
    ResponseTimeRootCauseService (..),
    mkResponseTimeRootCauseService,
    rtrcsEntityPath,
    rtrcsAccountId,
    rtrcsNames,
    rtrcsName,
    rtrcsInferred,
    rtrcsType,

    -- * RootCauseException
    RootCauseException (..),
    mkRootCauseException,
    rceName,
    rceMessage,

    -- * SamplingRule
    SamplingRule (..),
    mkSamplingRule,
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
    SamplingRuleRecord (..),
    mkSamplingRuleRecord,
    srrModifiedAt,
    srrSamplingRule,
    srrCreatedAt,

    -- * SamplingRuleUpdate
    SamplingRuleUpdate (..),
    mkSamplingRuleUpdate,
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
    SamplingStatisticSummary (..),
    mkSamplingStatisticSummary,
    sssRequestCount,
    sssBorrowCount,
    sssRuleName,
    sssTimestamp,
    sssSampledCount,

    -- * SamplingStatisticsDocument
    SamplingStatisticsDocument (..),
    mkSamplingStatisticsDocument,
    ssdBorrowCount,
    ssdRuleName,
    ssdClientId,
    ssdTimestamp,
    ssdRequestCount,
    ssdSampledCount,

    -- * SamplingStrategy
    SamplingStrategy (..),
    mkSamplingStrategy,
    ssValue,
    ssName,

    -- * SamplingTargetDocument
    SamplingTargetDocument (..),
    mkSamplingTargetDocument,
    stdReservoirQuota,
    stdRuleName,
    stdFixedRate,
    stdInterval,
    stdReservoirQuotaTTL,

    -- * Segment
    Segment (..),
    mkSegment,
    sDocument,
    sId,

    -- * ServiceId
    ServiceId (..),
    mkServiceId,
    siAccountId,
    siNames,
    siName,
    siType,

    -- * ServiceInfo
    ServiceInfo (..),
    mkServiceInfo,
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
    ServiceStatistics (..),
    mkServiceStatistics,
    ssFaultStatistics,
    ssOKCount,
    ssTotalResponseTime,
    ssErrorStatistics,
    ssTotalCount,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TelemetryRecord
    TelemetryRecord (..),
    mkTelemetryRecord,
    trSegmentsReceivedCount,
    trSegmentsSentCount,
    trSegmentsSpilloverCount,
    trSegmentsRejectedCount,
    trBackendConnectionErrors,
    trTimestamp,

    -- * TimeSeriesServiceStatistics
    TimeSeriesServiceStatistics (..),
    mkTimeSeriesServiceStatistics,
    tsssServiceSummaryStatistics,
    tsssResponseTimeHistogram,
    tsssEdgeSummaryStatistics,
    tsssServiceForecastStatistics,
    tsssTimestamp,

    -- * Trace
    Trace (..),
    mkTrace,
    tLimitExceeded,
    tId,
    tSegments,
    tDuration,

    -- * TraceSummary
    TraceSummary (..),
    mkTraceSummary,
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
    TraceUser (..),
    mkTraceUser,
    tuServiceIds,
    tuUserName,

    -- * UnprocessedStatistics
    UnprocessedStatistics (..),
    mkUnprocessedStatistics,
    usRuleName,
    usErrorCode,
    usMessage,

    -- * UnprocessedTraceSegment
    UnprocessedTraceSegment (..),
    mkUnprocessedTraceSegment,
    utsErrorCode,
    utsId,
    utsMessage,

    -- * ValueWithServiceIds
    ValueWithServiceIds (..),
    mkValueWithServiceIds,
    vwsiServiceIds,
    vwsiAnnotationValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign
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
xRayService :: Lude.Service
xRayService =
  Lude.Service
    { Lude._svcAbbrev = "XRay",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "xray",
      Lude._svcVersion = "2016-04-12",
      Lude._svcEndpoint = Lude.defaultEndpoint xRayService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "XRay",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

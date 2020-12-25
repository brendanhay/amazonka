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
    mkServiceConfig,

    -- * Errors
    _InvalidRequestException,
    _RuleLimitExceededException,
    _ThrottledException,
    _TooManyTagsException,
    _ResourceNotFoundException,

    -- * AnomalousService
    AnomalousService (..),
    mkAnomalousService,
    asServiceId,

    -- * SegmentDocument
    SegmentDocument (..),

    -- * HistogramEntry
    HistogramEntry (..),
    mkHistogramEntry,
    heCount,
    heValue,

    -- * ClientID
    ClientID (..),

    -- * UnprocessedStatistics
    UnprocessedStatistics (..),
    mkUnprocessedStatistics,
    usErrorCode,
    usMessage,
    usRuleName,

    -- * EntitySelectorExpression
    EntitySelectorExpression (..),

    -- * ErrorRootCauseEntity
    ErrorRootCauseEntity (..),
    mkErrorRootCauseEntity,
    erceExceptions,
    erceName,
    erceRemote,

    -- * TraceId
    TraceId (..),

    -- * ResponseTimeRootCauseService
    ResponseTimeRootCauseService (..),
    mkResponseTimeRootCauseService,
    rtrcsAccountId,
    rtrcsEntityPath,
    rtrcsInferred,
    rtrcsName,
    rtrcsNames,
    rtrcsType,

    -- * EncryptionType
    EncryptionType (..),

    -- * Group
    Group (..),
    mkGroup,
    gFilterExpression,
    gGroupARN,
    gGroupName,
    gInsightsConfiguration,

    -- * FaultStatistics
    FaultStatistics (..),
    mkFaultStatistics,
    fsOtherCount,
    fsTotalCount,

    -- * Hostname
    Hostname (..),

    -- * SamplingStrategyName
    SamplingStrategyName (..),

    -- * AttributeValue
    AttributeValue (..),

    -- * HTTPMethod
    HTTPMethod (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * ResponseTimeRootCauseEntity
    ResponseTimeRootCauseEntity (..),
    mkResponseTimeRootCauseEntity,
    rtrceCoverage,
    rtrceName,
    rtrceRemote,

    -- * ValueWithServiceIds
    ValueWithServiceIds (..),
    mkValueWithServiceIds,
    vwsiAnnotationValue,
    vwsiServiceIds,

    -- * ErrorRootCauseService
    ErrorRootCauseService (..),
    mkErrorRootCauseService,
    ercsAccountId,
    ercsEntityPath,
    ercsInferred,
    ercsName,
    ercsNames,
    ercsType,

    -- * FilterExpression
    FilterExpression (..),

    -- * TraceSegmentDocument
    TraceSegmentDocument (..),

    -- * TraceSummary
    TraceSummary (..),
    mkTraceSummary,
    tsAnnotations,
    tsAvailabilityZones,
    tsDuration,
    tsEntryPoint,
    tsErrorRootCauses,
    tsFaultRootCauses,
    tsHasError,
    tsHasFault,
    tsHasThrottle,
    tsHttp,
    tsId,
    tsInstanceIds,
    tsIsPartial,
    tsMatchedEventTime,
    tsResourceARNs,
    tsResponseTime,
    tsResponseTimeRootCauses,
    tsRevision,
    tsServiceIds,
    tsUsers,

    -- * FaultRootCause
    FaultRootCause (..),
    mkFaultRootCause,
    frcClientImpacting,
    frcServices,

    -- * String
    String (..),

    -- * ServiceInfo
    ServiceInfo (..),
    mkServiceInfo,
    sAccountId,
    sDurationHistogram,
    sEdges,
    sEndTime,
    sName,
    sNames,
    sReferenceId,
    sResponseTimeHistogram,
    sRoot,
    sStartTime,
    sState,
    sSummaryStatistics,
    sType,

    -- * InsightImpactGraphService
    InsightImpactGraphService (..),
    mkInsightImpactGraphService,
    iigsAccountId,
    iigsEdges,
    iigsName,
    iigsNames,
    iigsReferenceId,
    iigsType,

    -- * EncryptionKeyId
    EncryptionKeyId (..),

    -- * SamplingRule
    SamplingRule (..),
    mkSamplingRule,
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
    srAttributes,
    srRuleARN,
    srRuleName,

    -- * InsightsConfiguration
    InsightsConfiguration (..),
    mkInsightsConfiguration,
    icInsightsEnabled,
    icNotificationsEnabled,

    -- * EventSummaryText
    EventSummaryText (..),

    -- * RootCauseException
    RootCauseException (..),
    mkRootCauseException,
    rceMessage,
    rceName,

    -- * EC2InstanceId
    EC2InstanceId (..),

    -- * AnnotationKey
    AnnotationKey (..),

    -- * Token
    Token (..),

    -- * InsightId
    InsightId (..),

    -- * UnprocessedTraceSegment
    UnprocessedTraceSegment (..),
    mkUnprocessedTraceSegment,
    utsErrorCode,
    utsId,
    utsMessage,

    -- * Alias
    Alias (..),
    mkAlias,
    aName,
    aNames,
    aType,

    -- * FaultRootCauseEntity
    FaultRootCauseEntity (..),
    mkFaultRootCauseEntity,
    frceExceptions,
    frceName,
    frceRemote,

    -- * ForecastStatistics
    ForecastStatistics (..),
    mkForecastStatistics,
    fsFaultCountHigh,
    fsFaultCountLow,

    -- * TimeSeriesServiceStatistics
    TimeSeriesServiceStatistics (..),
    mkTimeSeriesServiceStatistics,
    tsssEdgeSummaryStatistics,
    tsssResponseTimeHistogram,
    tsssServiceForecastStatistics,
    tsssServiceSummaryStatistics,
    tsssTimestamp,

    -- * Insight
    Insight (..),
    mkInsight,
    iCategories,
    iClientRequestImpactStatistics,
    iEndTime,
    iGroupARN,
    iGroupName,
    iInsightId,
    iRootCauseServiceId,
    iRootCauseServiceRequestImpactStatistics,
    iStartTime,
    iState,
    iSummary,
    iTopAnomalousServices,

    -- * RequestImpactStatistics
    RequestImpactStatistics (..),
    mkRequestImpactStatistics,
    risFaultCount,
    risOkCount,
    risTotalCount,

    -- * InsightCategory
    InsightCategory (..),

    -- * RuleName
    RuleName (..),

    -- * AnnotationValue
    AnnotationValue (..),
    mkAnnotationValue,
    avBooleanValue,
    avNumberValue,
    avStringValue,

    -- * SamplingRuleUpdate
    SamplingRuleUpdate (..),
    mkSamplingRuleUpdate,
    sruAttributes,
    sruFixedRate,
    sruHTTPMethod,
    sruHost,
    sruPriority,
    sruReservoirSize,
    sruResourceARN,
    sruRuleARN,
    sruRuleName,
    sruServiceName,
    sruServiceType,
    sruURLPath,

    -- * ResourceARN
    ResourceARN (..),

    -- * InsightSummaryText
    InsightSummaryText (..),

    -- * ErrorRootCause
    ErrorRootCause (..),
    mkErrorRootCause,
    ercClientImpacting,
    ercServices,

    -- * InsightEvent
    InsightEvent (..),
    mkInsightEvent,
    ieClientRequestImpactStatistics,
    ieEventTime,
    ieRootCauseServiceRequestImpactStatistics,
    ieSummary,
    ieTopAnomalousServices,

    -- * TimeRangeType
    TimeRangeType (..),

    -- * ServiceName
    ServiceName (..),

    -- * InsightSummary
    InsightSummary (..),
    mkInsightSummary,
    isCategories,
    isClientRequestImpactStatistics,
    isEndTime,
    isGroupARN,
    isGroupName,
    isInsightId,
    isLastUpdateTime,
    isRootCauseServiceId,
    isRootCauseServiceRequestImpactStatistics,
    isStartTime,
    isState,
    isSummary,
    isTopAnomalousServices,

    -- * TelemetryRecord
    TelemetryRecord (..),
    mkTelemetryRecord,
    trTimestamp,
    trBackendConnectionErrors,
    trSegmentsReceivedCount,
    trSegmentsRejectedCount,
    trSegmentsSentCount,
    trSegmentsSpilloverCount,

    -- * GroupARN
    GroupARN (..),

    -- * ServiceType
    ServiceType (..),

    -- * InstanceIdDetail
    InstanceIdDetail (..),
    mkInstanceIdDetail,
    iidId,

    -- * EncryptionConfig
    EncryptionConfig (..),
    mkEncryptionConfig,
    ecKeyId,
    ecStatus,
    ecType,

    -- * ErrorStatistics
    ErrorStatistics (..),
    mkErrorStatistics,
    eOtherCount,
    eThrottleCount,
    eTotalCount,

    -- * SamplingTargetDocument
    SamplingTargetDocument (..),
    mkSamplingTargetDocument,
    stdFixedRate,
    stdInterval,
    stdReservoirQuota,
    stdReservoirQuotaTTL,
    stdRuleName,

    -- * TagKey
    TagKey (..),

    -- * SamplingStrategy
    SamplingStrategy (..),
    mkSamplingStrategy,
    ssName,
    ssValue,

    -- * Http
    Http (..),
    mkHttp,
    hClientIp,
    hHttpMethod,
    hHttpStatus,
    hHttpURL,
    hUserAgent,

    -- * SamplingRuleRecord
    SamplingRuleRecord (..),
    mkSamplingRuleRecord,
    srrCreatedAt,
    srrModifiedAt,
    srrSamplingRule,

    -- * Host
    Host (..),

    -- * ServiceStatistics
    ServiceStatistics (..),
    mkServiceStatistics,
    ssErrorStatistics,
    ssFaultStatistics,
    ssOkCount,
    ssTotalCount,
    ssTotalResponseTime,

    -- * TraceUser
    TraceUser (..),
    mkTraceUser,
    tuServiceIds,
    tuUserName,

    -- * GroupName
    GroupName (..),

    -- * FaultRootCauseService
    FaultRootCauseService (..),
    mkFaultRootCauseService,
    frcsAccountId,
    frcsEntityPath,
    frcsInferred,
    frcsName,
    frcsNames,
    frcsType,

    -- * ServiceId
    ServiceId (..),
    mkServiceId,
    siAccountId,
    siName,
    siNames,
    siType,

    -- * GroupSummary
    GroupSummary (..),
    mkGroupSummary,
    gsFilterExpression,
    gsGroupARN,
    gsGroupName,
    gsInsightsConfiguration,

    -- * Segment
    Segment (..),
    mkSegment,
    sDocument,
    sId,

    -- * AttributeKey
    AttributeKey (..),

    -- * Edge
    Edge (..),
    mkEdge,
    eAliases,
    eEndTime,
    eReferenceId,
    eResponseTimeHistogram,
    eStartTime,
    eSummaryStatistics,

    -- * InsightImpactGraphEdge
    InsightImpactGraphEdge (..),
    mkInsightImpactGraphEdge,
    iigeReferenceId,

    -- * AmazonResourceName
    AmazonResourceName (..),

    -- * EncryptionStatus
    EncryptionStatus (..),

    -- * EdgeStatistics
    EdgeStatistics (..),
    mkEdgeStatistics,
    esErrorStatistics,
    esFaultStatistics,
    esOkCount,
    esTotalCount,
    esTotalResponseTime,

    -- * AvailabilityZoneDetail
    AvailabilityZoneDetail (..),
    mkAvailabilityZoneDetail,
    azdName,

    -- * ResourceARNDetail
    ResourceARNDetail (..),
    mkResourceARNDetail,
    rarndARN,

    -- * URLPath
    URLPath (..),

    -- * SamplingStatisticsDocument
    SamplingStatisticsDocument (..),
    mkSamplingStatisticsDocument,
    ssdRuleName,
    ssdClientID,
    ssdTimestamp,
    ssdRequestCount,
    ssdSampledCount,
    ssdBorrowCount,

    -- * BackendConnectionErrors
    BackendConnectionErrors (..),
    mkBackendConnectionErrors,
    bceConnectionRefusedCount,
    bceHTTPCode4XXCount,
    bceHTTPCode5XXCount,
    bceOtherCount,
    bceTimeoutCount,
    bceUnknownHostCount,

    -- * InsightState
    InsightState (..),

    -- * SamplingStatisticSummary
    SamplingStatisticSummary (..),
    mkSamplingStatisticSummary,
    sssBorrowCount,
    sssRequestCount,
    sssRuleName,
    sssSampledCount,
    sssTimestamp,

    -- * ResponseTimeRootCause
    ResponseTimeRootCause (..),
    mkResponseTimeRootCause,
    rtrcClientImpacting,
    rtrcServices,

    -- * Trace
    Trace (..),
    mkTrace,
    tDuration,
    tId,
    tLimitExceeded,
    tSegments,

    -- * NextToken
    NextToken (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * Message
    Message (..),

    -- * Name
    Name (..),

    -- * AccountId
    AccountId (..),

    -- * Type
    Type (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * Summary
    Summary (..),

    -- * Id
    Id (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.XRay.Types.AccountId
import Network.AWS.XRay.Types.Alias
import Network.AWS.XRay.Types.AmazonResourceName
import Network.AWS.XRay.Types.AnnotationKey
import Network.AWS.XRay.Types.AnnotationValue
import Network.AWS.XRay.Types.AnomalousService
import Network.AWS.XRay.Types.AttributeKey
import Network.AWS.XRay.Types.AttributeValue
import Network.AWS.XRay.Types.AvailabilityZoneDetail
import Network.AWS.XRay.Types.BackendConnectionErrors
import Network.AWS.XRay.Types.ClientID
import Network.AWS.XRay.Types.EC2InstanceId
import Network.AWS.XRay.Types.Edge
import Network.AWS.XRay.Types.EdgeStatistics
import Network.AWS.XRay.Types.EncryptionConfig
import Network.AWS.XRay.Types.EncryptionKeyId
import Network.AWS.XRay.Types.EncryptionStatus
import Network.AWS.XRay.Types.EncryptionType
import Network.AWS.XRay.Types.EntitySelectorExpression
import Network.AWS.XRay.Types.ErrorCode
import Network.AWS.XRay.Types.ErrorRootCause
import Network.AWS.XRay.Types.ErrorRootCauseEntity
import Network.AWS.XRay.Types.ErrorRootCauseService
import Network.AWS.XRay.Types.ErrorStatistics
import Network.AWS.XRay.Types.EventSummaryText
import Network.AWS.XRay.Types.FaultRootCause
import Network.AWS.XRay.Types.FaultRootCauseEntity
import Network.AWS.XRay.Types.FaultRootCauseService
import Network.AWS.XRay.Types.FaultStatistics
import Network.AWS.XRay.Types.FilterExpression
import Network.AWS.XRay.Types.ForecastStatistics
import Network.AWS.XRay.Types.Group
import Network.AWS.XRay.Types.GroupARN
import Network.AWS.XRay.Types.GroupName
import Network.AWS.XRay.Types.GroupSummary
import Network.AWS.XRay.Types.HTTPMethod
import Network.AWS.XRay.Types.HistogramEntry
import Network.AWS.XRay.Types.Host
import Network.AWS.XRay.Types.Hostname
import Network.AWS.XRay.Types.Http
import Network.AWS.XRay.Types.Id
import Network.AWS.XRay.Types.Insight
import Network.AWS.XRay.Types.InsightCategory
import Network.AWS.XRay.Types.InsightEvent
import Network.AWS.XRay.Types.InsightId
import Network.AWS.XRay.Types.InsightImpactGraphEdge
import Network.AWS.XRay.Types.InsightImpactGraphService
import Network.AWS.XRay.Types.InsightState
import Network.AWS.XRay.Types.InsightSummary
import Network.AWS.XRay.Types.InsightSummaryText
import Network.AWS.XRay.Types.InsightsConfiguration
import Network.AWS.XRay.Types.InstanceIdDetail
import Network.AWS.XRay.Types.Key
import Network.AWS.XRay.Types.Message
import Network.AWS.XRay.Types.Name
import Network.AWS.XRay.Types.NextToken
import Network.AWS.XRay.Types.RequestImpactStatistics
import Network.AWS.XRay.Types.ResourceARN
import Network.AWS.XRay.Types.ResourceARNDetail
import Network.AWS.XRay.Types.ResponseTimeRootCause
import Network.AWS.XRay.Types.ResponseTimeRootCauseEntity
import Network.AWS.XRay.Types.ResponseTimeRootCauseService
import Network.AWS.XRay.Types.RootCauseException
import Network.AWS.XRay.Types.RuleName
import Network.AWS.XRay.Types.SamplingRule
import Network.AWS.XRay.Types.SamplingRuleRecord
import Network.AWS.XRay.Types.SamplingRuleUpdate
import Network.AWS.XRay.Types.SamplingStatisticSummary
import Network.AWS.XRay.Types.SamplingStatisticsDocument
import Network.AWS.XRay.Types.SamplingStrategy
import Network.AWS.XRay.Types.SamplingStrategyName
import Network.AWS.XRay.Types.SamplingTargetDocument
import Network.AWS.XRay.Types.Segment
import Network.AWS.XRay.Types.SegmentDocument
import Network.AWS.XRay.Types.ServiceId
import Network.AWS.XRay.Types.ServiceInfo
import Network.AWS.XRay.Types.ServiceName
import Network.AWS.XRay.Types.ServiceStatistics
import Network.AWS.XRay.Types.ServiceType
import Network.AWS.XRay.Types.String
import Network.AWS.XRay.Types.Summary
import Network.AWS.XRay.Types.Tag
import Network.AWS.XRay.Types.TagKey
import Network.AWS.XRay.Types.TelemetryRecord
import Network.AWS.XRay.Types.TimeRangeType
import Network.AWS.XRay.Types.TimeSeriesServiceStatistics
import Network.AWS.XRay.Types.Token
import Network.AWS.XRay.Types.Trace
import Network.AWS.XRay.Types.TraceId
import Network.AWS.XRay.Types.TraceSegmentDocument
import Network.AWS.XRay.Types.TraceSummary
import Network.AWS.XRay.Types.TraceUser
import Network.AWS.XRay.Types.Type
import Network.AWS.XRay.Types.URLPath
import Network.AWS.XRay.Types.UnprocessedStatistics
import Network.AWS.XRay.Types.UnprocessedTraceSegment
import Network.AWS.XRay.Types.Value
import Network.AWS.XRay.Types.ValueWithServiceIds

-- | API version @2016-04-12@ of the Amazon X-Ray SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "XRay",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "xray",
      Core._svcVersion = "2016-04-12",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "XRay",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request is missing required parameters or has invalid parameters.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError mkServiceConfig "InvalidRequestException"
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead." #-}

-- | You have reached the maximum number of sampling rules.
_RuleLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RuleLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "RuleLimitExceededException"
{-# DEPRECATED _RuleLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The request exceeds the maximum number of requests per second.
_ThrottledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottledException =
  Core._MatchServiceError mkServiceConfig "ThrottledException"
    Core.. Core.hasStatues 429
{-# DEPRECATED _ThrottledException "Use generic-lens or generic-optics instead." #-}

-- | You have exceeded the maximum number of tags you can apply to this resource.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError mkServiceConfig "TooManyTagsException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead." #-}

-- | The resource was not found. Verify that the name or Amazon Resource Name (ARN) of the resource is correct.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

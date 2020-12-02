{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS X-Ray provides APIs for managing debug traces and retrieving service maps and other data created by processing those traces.
module Network.AWS.XRay
  ( -- * Service Configuration
    xRay,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutEncryptionConfig
    module Network.AWS.XRay.PutEncryptionConfig,

    -- ** GetServiceGraph (Paginated)
    module Network.AWS.XRay.GetServiceGraph,

    -- ** GetSamplingTargets
    module Network.AWS.XRay.GetSamplingTargets,

    -- ** ListTagsForResource
    module Network.AWS.XRay.ListTagsForResource,

    -- ** GetTraceSummaries (Paginated)
    module Network.AWS.XRay.GetTraceSummaries,

    -- ** PutTraceSegments
    module Network.AWS.XRay.PutTraceSegments,

    -- ** BatchGetTraces (Paginated)
    module Network.AWS.XRay.BatchGetTraces,

    -- ** GetInsight
    module Network.AWS.XRay.GetInsight,

    -- ** GetTimeSeriesServiceStatistics (Paginated)
    module Network.AWS.XRay.GetTimeSeriesServiceStatistics,

    -- ** GetEncryptionConfig
    module Network.AWS.XRay.GetEncryptionConfig,

    -- ** GetInsightImpactGraph
    module Network.AWS.XRay.GetInsightImpactGraph,

    -- ** UpdateSamplingRule
    module Network.AWS.XRay.UpdateSamplingRule,

    -- ** DeleteSamplingRule
    module Network.AWS.XRay.DeleteSamplingRule,

    -- ** GetInsightEvents
    module Network.AWS.XRay.GetInsightEvents,

    -- ** GetGroups (Paginated)
    module Network.AWS.XRay.GetGroups,

    -- ** GetInsightSummaries
    module Network.AWS.XRay.GetInsightSummaries,

    -- ** PutTelemetryRecords
    module Network.AWS.XRay.PutTelemetryRecords,

    -- ** GetSamplingRules (Paginated)
    module Network.AWS.XRay.GetSamplingRules,

    -- ** TagResource
    module Network.AWS.XRay.TagResource,

    -- ** GetTraceGraph (Paginated)
    module Network.AWS.XRay.GetTraceGraph,

    -- ** CreateGroup
    module Network.AWS.XRay.CreateGroup,

    -- ** UntagResource
    module Network.AWS.XRay.UntagResource,

    -- ** DeleteGroup
    module Network.AWS.XRay.DeleteGroup,

    -- ** UpdateGroup
    module Network.AWS.XRay.UpdateGroup,

    -- ** GetGroup
    module Network.AWS.XRay.GetGroup,

    -- ** GetSamplingStatisticSummaries (Paginated)
    module Network.AWS.XRay.GetSamplingStatisticSummaries,

    -- ** CreateSamplingRule
    module Network.AWS.XRay.CreateSamplingRule,

    -- * Types

    -- ** EncryptionStatus
    EncryptionStatus (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** InsightCategory
    InsightCategory (..),

    -- ** InsightState
    InsightState (..),

    -- ** SamplingStrategyName
    SamplingStrategyName (..),

    -- ** TimeRangeType
    TimeRangeType (..),

    -- ** Alias
    Alias,
    alias,
    aNames,
    aName,
    aType,

    -- ** AnnotationValue
    AnnotationValue,
    annotationValue,
    avNumberValue,
    avStringValue,
    avBooleanValue,

    -- ** AnomalousService
    AnomalousService,
    anomalousService,
    asServiceId,

    -- ** AvailabilityZoneDetail
    AvailabilityZoneDetail,
    availabilityZoneDetail,
    azdName,

    -- ** BackendConnectionErrors
    BackendConnectionErrors,
    backendConnectionErrors,
    bceOtherCount,
    bceTimeoutCount,
    bceHTTPCode5XXCount,
    bceConnectionRefusedCount,
    bceHTTPCode4XXCount,
    bceUnknownHostCount,

    -- ** Edge
    Edge,
    edge,
    eStartTime,
    eAliases,
    eResponseTimeHistogram,
    eReferenceId,
    eEndTime,
    eSummaryStatistics,

    -- ** EdgeStatistics
    EdgeStatistics,
    edgeStatistics,
    esFaultStatistics,
    esOKCount,
    esTotalResponseTime,
    esErrorStatistics,
    esTotalCount,

    -- ** EncryptionConfig
    EncryptionConfig,
    encryptionConfig,
    ecStatus,
    ecKeyId,
    ecType,

    -- ** ErrorRootCause
    ErrorRootCause,
    errorRootCause,
    ercClientImpacting,
    ercServices,

    -- ** ErrorRootCauseEntity
    ErrorRootCauseEntity,
    errorRootCauseEntity,
    erceExceptions,
    erceRemote,
    erceName,

    -- ** ErrorRootCauseService
    ErrorRootCauseService,
    errorRootCauseService,
    ercsEntityPath,
    ercsAccountId,
    ercsNames,
    ercsName,
    ercsInferred,
    ercsType,

    -- ** ErrorStatistics
    ErrorStatistics,
    errorStatistics,
    eOtherCount,
    eThrottleCount,
    eTotalCount,

    -- ** FaultRootCause
    FaultRootCause,
    faultRootCause,
    frcClientImpacting,
    frcServices,

    -- ** FaultRootCauseEntity
    FaultRootCauseEntity,
    faultRootCauseEntity,
    frceExceptions,
    frceRemote,
    frceName,

    -- ** FaultRootCauseService
    FaultRootCauseService,
    faultRootCauseService,
    frcsEntityPath,
    frcsAccountId,
    frcsNames,
    frcsName,
    frcsInferred,
    frcsType,

    -- ** FaultStatistics
    FaultStatistics,
    faultStatistics,
    fsOtherCount,
    fsTotalCount,

    -- ** ForecastStatistics
    ForecastStatistics,
    forecastStatistics,
    fsFaultCountLow,
    fsFaultCountHigh,

    -- ** Group
    Group,
    group',
    gFilterExpression,
    gInsightsConfiguration,
    gGroupARN,
    gGroupName,

    -- ** GroupSummary
    GroupSummary,
    groupSummary,
    gsFilterExpression,
    gsInsightsConfiguration,
    gsGroupARN,
    gsGroupName,

    -- ** HTTP
    HTTP,
    hTTP,
    httpHTTPMethod,
    httpHTTPStatus,
    httpClientIP,
    httpUserAgent,
    httpHTTPURL,

    -- ** HistogramEntry
    HistogramEntry,
    histogramEntry,
    heCount,
    heValue,

    -- ** Insight
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

    -- ** InsightEvent
    InsightEvent,
    insightEvent,
    ieSummary,
    ieEventTime,
    ieRootCauseServiceRequestImpactStatistics,
    ieTopAnomalousServices,
    ieClientRequestImpactStatistics,

    -- ** InsightImpactGraphEdge
    InsightImpactGraphEdge,
    insightImpactGraphEdge,
    iigeReferenceId,

    -- ** InsightImpactGraphService
    InsightImpactGraphService,
    insightImpactGraphService,
    iigsReferenceId,
    iigsAccountId,
    iigsNames,
    iigsName,
    iigsType,
    iigsEdges,

    -- ** InsightSummary
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

    -- ** InsightsConfiguration
    InsightsConfiguration,
    insightsConfiguration,
    icNotificationsEnabled,
    icInsightsEnabled,

    -- ** InstanceIdDetail
    InstanceIdDetail,
    instanceIdDetail,
    iidId,

    -- ** RequestImpactStatistics
    RequestImpactStatistics,
    requestImpactStatistics,
    risOKCount,
    risFaultCount,
    risTotalCount,

    -- ** ResourceARNDetail
    ResourceARNDetail,
    resourceARNDetail,
    radARN,

    -- ** ResponseTimeRootCause
    ResponseTimeRootCause,
    responseTimeRootCause,
    rtrcClientImpacting,
    rtrcServices,

    -- ** ResponseTimeRootCauseEntity
    ResponseTimeRootCauseEntity,
    responseTimeRootCauseEntity,
    rtrceRemote,
    rtrceCoverage,
    rtrceName,

    -- ** ResponseTimeRootCauseService
    ResponseTimeRootCauseService,
    responseTimeRootCauseService,
    rtrcsEntityPath,
    rtrcsAccountId,
    rtrcsNames,
    rtrcsName,
    rtrcsInferred,
    rtrcsType,

    -- ** RootCauseException
    RootCauseException,
    rootCauseException,
    rceName,
    rceMessage,

    -- ** SamplingRule
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

    -- ** SamplingRuleRecord
    SamplingRuleRecord,
    samplingRuleRecord,
    srrModifiedAt,
    srrSamplingRule,
    srrCreatedAt,

    -- ** SamplingRuleUpdate
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

    -- ** SamplingStatisticSummary
    SamplingStatisticSummary,
    samplingStatisticSummary,
    sssRequestCount,
    sssBorrowCount,
    sssRuleName,
    sssTimestamp,
    sssSampledCount,

    -- ** SamplingStatisticsDocument
    SamplingStatisticsDocument,
    samplingStatisticsDocument,
    ssdBorrowCount,
    ssdRuleName,
    ssdClientId,
    ssdTimestamp,
    ssdRequestCount,
    ssdSampledCount,

    -- ** SamplingStrategy
    SamplingStrategy,
    samplingStrategy,
    ssValue,
    ssName,

    -- ** SamplingTargetDocument
    SamplingTargetDocument,
    samplingTargetDocument,
    stdReservoirQuota,
    stdRuleName,
    stdFixedRate,
    stdInterval,
    stdReservoirQuotaTTL,

    -- ** Segment
    Segment,
    segment,
    sDocument,
    sId,

    -- ** ServiceId
    ServiceId,
    serviceId,
    siAccountId,
    siNames,
    siName,
    siType,

    -- ** ServiceInfo
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

    -- ** ServiceStatistics
    ServiceStatistics,
    serviceStatistics,
    ssFaultStatistics,
    ssOKCount,
    ssTotalResponseTime,
    ssErrorStatistics,
    ssTotalCount,

    -- ** Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- ** TelemetryRecord
    TelemetryRecord,
    telemetryRecord,
    trSegmentsReceivedCount,
    trSegmentsSentCount,
    trSegmentsSpilloverCount,
    trSegmentsRejectedCount,
    trBackendConnectionErrors,
    trTimestamp,

    -- ** TimeSeriesServiceStatistics
    TimeSeriesServiceStatistics,
    timeSeriesServiceStatistics,
    tsssServiceSummaryStatistics,
    tsssResponseTimeHistogram,
    tsssEdgeSummaryStatistics,
    tsssServiceForecastStatistics,
    tsssTimestamp,

    -- ** Trace
    Trace,
    trace,
    tLimitExceeded,
    tId,
    tSegments,
    tDuration,

    -- ** TraceSummary
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

    -- ** TraceUser
    TraceUser,
    traceUser,
    tuServiceIds,
    tuUserName,

    -- ** UnprocessedStatistics
    UnprocessedStatistics,
    unprocessedStatistics,
    usRuleName,
    usErrorCode,
    usMessage,

    -- ** UnprocessedTraceSegment
    UnprocessedTraceSegment,
    unprocessedTraceSegment,
    utsErrorCode,
    utsId,
    utsMessage,

    -- ** ValueWithServiceIds
    ValueWithServiceIds,
    valueWithServiceIds,
    vwsiServiceIds,
    vwsiAnnotationValue,
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
import Network.AWS.XRay.Types
import Network.AWS.XRay.UntagResource
import Network.AWS.XRay.UpdateGroup
import Network.AWS.XRay.UpdateSamplingRule
import Network.AWS.XRay.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'XRay'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS X-Ray provides APIs for managing debug traces and retrieving service
-- maps and other data created by processing those traces.
module Network.AWS.XRay
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** RuleLimitExceededException
    _RuleLimitExceededException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ThrottledException
    _ThrottledException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetSamplingTargets
    GetSamplingTargets (GetSamplingTargets'),
    newGetSamplingTargets,
    GetSamplingTargetsResponse (GetSamplingTargetsResponse'),
    newGetSamplingTargetsResponse,

    -- ** GetSamplingStatisticSummaries (Paginated)
    GetSamplingStatisticSummaries (GetSamplingStatisticSummaries'),
    newGetSamplingStatisticSummaries,
    GetSamplingStatisticSummariesResponse (GetSamplingStatisticSummariesResponse'),
    newGetSamplingStatisticSummariesResponse,

    -- ** GetInsightImpactGraph
    GetInsightImpactGraph (GetInsightImpactGraph'),
    newGetInsightImpactGraph,
    GetInsightImpactGraphResponse (GetInsightImpactGraphResponse'),
    newGetInsightImpactGraphResponse,

    -- ** GetTraceGraph (Paginated)
    GetTraceGraph (GetTraceGraph'),
    newGetTraceGraph,
    GetTraceGraphResponse (GetTraceGraphResponse'),
    newGetTraceGraphResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** GetInsight
    GetInsight (GetInsight'),
    newGetInsight,
    GetInsightResponse (GetInsightResponse'),
    newGetInsightResponse,

    -- ** PutTraceSegments
    PutTraceSegments (PutTraceSegments'),
    newPutTraceSegments,
    PutTraceSegmentsResponse (PutTraceSegmentsResponse'),
    newPutTraceSegmentsResponse,

    -- ** GetTimeSeriesServiceStatistics (Paginated)
    GetTimeSeriesServiceStatistics (GetTimeSeriesServiceStatistics'),
    newGetTimeSeriesServiceStatistics,
    GetTimeSeriesServiceStatisticsResponse (GetTimeSeriesServiceStatisticsResponse'),
    newGetTimeSeriesServiceStatisticsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** PutTelemetryRecords
    PutTelemetryRecords (PutTelemetryRecords'),
    newPutTelemetryRecords,
    PutTelemetryRecordsResponse (PutTelemetryRecordsResponse'),
    newPutTelemetryRecordsResponse,

    -- ** BatchGetTraces (Paginated)
    BatchGetTraces (BatchGetTraces'),
    newBatchGetTraces,
    BatchGetTracesResponse (BatchGetTracesResponse'),
    newBatchGetTracesResponse,

    -- ** GetTraceSummaries (Paginated)
    GetTraceSummaries (GetTraceSummaries'),
    newGetTraceSummaries,
    GetTraceSummariesResponse (GetTraceSummariesResponse'),
    newGetTraceSummariesResponse,

    -- ** GetInsightSummaries
    GetInsightSummaries (GetInsightSummaries'),
    newGetInsightSummaries,
    GetInsightSummariesResponse (GetInsightSummariesResponse'),
    newGetInsightSummariesResponse,

    -- ** GetGroups (Paginated)
    GetGroups (GetGroups'),
    newGetGroups,
    GetGroupsResponse (GetGroupsResponse'),
    newGetGroupsResponse,

    -- ** GetInsightEvents
    GetInsightEvents (GetInsightEvents'),
    newGetInsightEvents,
    GetInsightEventsResponse (GetInsightEventsResponse'),
    newGetInsightEventsResponse,

    -- ** GetServiceGraph (Paginated)
    GetServiceGraph (GetServiceGraph'),
    newGetServiceGraph,
    GetServiceGraphResponse (GetServiceGraphResponse'),
    newGetServiceGraphResponse,

    -- ** PutEncryptionConfig
    PutEncryptionConfig (PutEncryptionConfig'),
    newPutEncryptionConfig,
    PutEncryptionConfigResponse (PutEncryptionConfigResponse'),
    newPutEncryptionConfigResponse,

    -- ** DeleteSamplingRule
    DeleteSamplingRule (DeleteSamplingRule'),
    newDeleteSamplingRule,
    DeleteSamplingRuleResponse (DeleteSamplingRuleResponse'),
    newDeleteSamplingRuleResponse,

    -- ** UpdateSamplingRule
    UpdateSamplingRule (UpdateSamplingRule'),
    newUpdateSamplingRule,
    UpdateSamplingRuleResponse (UpdateSamplingRuleResponse'),
    newUpdateSamplingRuleResponse,

    -- ** GetGroup
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** CreateSamplingRule
    CreateSamplingRule (CreateSamplingRule'),
    newCreateSamplingRule,
    CreateSamplingRuleResponse (CreateSamplingRuleResponse'),
    newCreateSamplingRuleResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** GetEncryptionConfig
    GetEncryptionConfig (GetEncryptionConfig'),
    newGetEncryptionConfig,
    GetEncryptionConfigResponse (GetEncryptionConfigResponse'),
    newGetEncryptionConfigResponse,

    -- ** GetSamplingRules (Paginated)
    GetSamplingRules (GetSamplingRules'),
    newGetSamplingRules,
    GetSamplingRulesResponse (GetSamplingRulesResponse'),
    newGetSamplingRulesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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
    Alias (Alias'),
    newAlias,

    -- ** AnnotationValue
    AnnotationValue (AnnotationValue'),
    newAnnotationValue,

    -- ** AnomalousService
    AnomalousService (AnomalousService'),
    newAnomalousService,

    -- ** AvailabilityZoneDetail
    AvailabilityZoneDetail (AvailabilityZoneDetail'),
    newAvailabilityZoneDetail,

    -- ** BackendConnectionErrors
    BackendConnectionErrors (BackendConnectionErrors'),
    newBackendConnectionErrors,

    -- ** Edge
    Edge (Edge'),
    newEdge,

    -- ** EdgeStatistics
    EdgeStatistics (EdgeStatistics'),
    newEdgeStatistics,

    -- ** EncryptionConfig
    EncryptionConfig (EncryptionConfig'),
    newEncryptionConfig,

    -- ** ErrorRootCause
    ErrorRootCause (ErrorRootCause'),
    newErrorRootCause,

    -- ** ErrorRootCauseEntity
    ErrorRootCauseEntity (ErrorRootCauseEntity'),
    newErrorRootCauseEntity,

    -- ** ErrorRootCauseService
    ErrorRootCauseService (ErrorRootCauseService'),
    newErrorRootCauseService,

    -- ** ErrorStatistics
    ErrorStatistics (ErrorStatistics'),
    newErrorStatistics,

    -- ** FaultRootCause
    FaultRootCause (FaultRootCause'),
    newFaultRootCause,

    -- ** FaultRootCauseEntity
    FaultRootCauseEntity (FaultRootCauseEntity'),
    newFaultRootCauseEntity,

    -- ** FaultRootCauseService
    FaultRootCauseService (FaultRootCauseService'),
    newFaultRootCauseService,

    -- ** FaultStatistics
    FaultStatistics (FaultStatistics'),
    newFaultStatistics,

    -- ** ForecastStatistics
    ForecastStatistics (ForecastStatistics'),
    newForecastStatistics,

    -- ** Group
    Group (Group'),
    newGroup,

    -- ** GroupSummary
    GroupSummary (GroupSummary'),
    newGroupSummary,

    -- ** HistogramEntry
    HistogramEntry (HistogramEntry'),
    newHistogramEntry,

    -- ** Http
    Http (Http'),
    newHttp,

    -- ** Insight
    Insight (Insight'),
    newInsight,

    -- ** InsightEvent
    InsightEvent (InsightEvent'),
    newInsightEvent,

    -- ** InsightImpactGraphEdge
    InsightImpactGraphEdge (InsightImpactGraphEdge'),
    newInsightImpactGraphEdge,

    -- ** InsightImpactGraphService
    InsightImpactGraphService (InsightImpactGraphService'),
    newInsightImpactGraphService,

    -- ** InsightSummary
    InsightSummary (InsightSummary'),
    newInsightSummary,

    -- ** InsightsConfiguration
    InsightsConfiguration (InsightsConfiguration'),
    newInsightsConfiguration,

    -- ** InstanceIdDetail
    InstanceIdDetail (InstanceIdDetail'),
    newInstanceIdDetail,

    -- ** RequestImpactStatistics
    RequestImpactStatistics (RequestImpactStatistics'),
    newRequestImpactStatistics,

    -- ** ResourceARNDetail
    ResourceARNDetail (ResourceARNDetail'),
    newResourceARNDetail,

    -- ** ResponseTimeRootCause
    ResponseTimeRootCause (ResponseTimeRootCause'),
    newResponseTimeRootCause,

    -- ** ResponseTimeRootCauseEntity
    ResponseTimeRootCauseEntity (ResponseTimeRootCauseEntity'),
    newResponseTimeRootCauseEntity,

    -- ** ResponseTimeRootCauseService
    ResponseTimeRootCauseService (ResponseTimeRootCauseService'),
    newResponseTimeRootCauseService,

    -- ** RootCauseException
    RootCauseException (RootCauseException'),
    newRootCauseException,

    -- ** SamplingRule
    SamplingRule (SamplingRule'),
    newSamplingRule,

    -- ** SamplingRuleRecord
    SamplingRuleRecord (SamplingRuleRecord'),
    newSamplingRuleRecord,

    -- ** SamplingRuleUpdate
    SamplingRuleUpdate (SamplingRuleUpdate'),
    newSamplingRuleUpdate,

    -- ** SamplingStatisticSummary
    SamplingStatisticSummary (SamplingStatisticSummary'),
    newSamplingStatisticSummary,

    -- ** SamplingStatisticsDocument
    SamplingStatisticsDocument (SamplingStatisticsDocument'),
    newSamplingStatisticsDocument,

    -- ** SamplingStrategy
    SamplingStrategy (SamplingStrategy'),
    newSamplingStrategy,

    -- ** SamplingTargetDocument
    SamplingTargetDocument (SamplingTargetDocument'),
    newSamplingTargetDocument,

    -- ** Segment
    Segment (Segment'),
    newSegment,

    -- ** ServiceId
    ServiceId (ServiceId'),
    newServiceId,

    -- ** ServiceInfo
    ServiceInfo (ServiceInfo'),
    newServiceInfo,

    -- ** ServiceStatistics
    ServiceStatistics (ServiceStatistics'),
    newServiceStatistics,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TelemetryRecord
    TelemetryRecord (TelemetryRecord'),
    newTelemetryRecord,

    -- ** TimeSeriesServiceStatistics
    TimeSeriesServiceStatistics (TimeSeriesServiceStatistics'),
    newTimeSeriesServiceStatistics,

    -- ** Trace
    Trace (Trace'),
    newTrace,

    -- ** TraceSummary
    TraceSummary (TraceSummary'),
    newTraceSummary,

    -- ** TraceUser
    TraceUser (TraceUser'),
    newTraceUser,

    -- ** UnprocessedStatistics
    UnprocessedStatistics (UnprocessedStatistics'),
    newUnprocessedStatistics,

    -- ** UnprocessedTraceSegment
    UnprocessedTraceSegment (UnprocessedTraceSegment'),
    newUnprocessedTraceSegment,

    -- ** ValueWithServiceIds
    ValueWithServiceIds (ValueWithServiceIds'),
    newValueWithServiceIds,
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
import Network.AWS.XRay.Lens
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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.XRay
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-04-12@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services X-Ray provides APIs for managing debug traces and
-- retrieving service maps and other data created by processing those
-- traces.
module Amazonka.XRay
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidPolicyRevisionIdException
    _InvalidPolicyRevisionIdException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LockoutPreventionException
    _LockoutPreventionException,

    -- ** MalformedPolicyDocumentException
    _MalformedPolicyDocumentException,

    -- ** PolicyCountLimitExceededException
    _PolicyCountLimitExceededException,

    -- ** PolicySizeLimitExceededException
    _PolicySizeLimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** RuleLimitExceededException
    _RuleLimitExceededException,

    -- ** ThrottledException
    _ThrottledException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchGetTraces (Paginated)
    BatchGetTraces (BatchGetTraces'),
    newBatchGetTraces,
    BatchGetTracesResponse (BatchGetTracesResponse'),
    newBatchGetTracesResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** CreateSamplingRule
    CreateSamplingRule (CreateSamplingRule'),
    newCreateSamplingRule,
    CreateSamplingRuleResponse (CreateSamplingRuleResponse'),
    newCreateSamplingRuleResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeleteSamplingRule
    DeleteSamplingRule (DeleteSamplingRule'),
    newDeleteSamplingRule,
    DeleteSamplingRuleResponse (DeleteSamplingRuleResponse'),
    newDeleteSamplingRuleResponse,

    -- ** GetEncryptionConfig
    GetEncryptionConfig (GetEncryptionConfig'),
    newGetEncryptionConfig,
    GetEncryptionConfigResponse (GetEncryptionConfigResponse'),
    newGetEncryptionConfigResponse,

    -- ** GetGroup
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** GetGroups (Paginated)
    GetGroups (GetGroups'),
    newGetGroups,
    GetGroupsResponse (GetGroupsResponse'),
    newGetGroupsResponse,

    -- ** GetInsight
    GetInsight (GetInsight'),
    newGetInsight,
    GetInsightResponse (GetInsightResponse'),
    newGetInsightResponse,

    -- ** GetInsightEvents
    GetInsightEvents (GetInsightEvents'),
    newGetInsightEvents,
    GetInsightEventsResponse (GetInsightEventsResponse'),
    newGetInsightEventsResponse,

    -- ** GetInsightImpactGraph
    GetInsightImpactGraph (GetInsightImpactGraph'),
    newGetInsightImpactGraph,
    GetInsightImpactGraphResponse (GetInsightImpactGraphResponse'),
    newGetInsightImpactGraphResponse,

    -- ** GetInsightSummaries
    GetInsightSummaries (GetInsightSummaries'),
    newGetInsightSummaries,
    GetInsightSummariesResponse (GetInsightSummariesResponse'),
    newGetInsightSummariesResponse,

    -- ** GetSamplingRules (Paginated)
    GetSamplingRules (GetSamplingRules'),
    newGetSamplingRules,
    GetSamplingRulesResponse (GetSamplingRulesResponse'),
    newGetSamplingRulesResponse,

    -- ** GetSamplingStatisticSummaries (Paginated)
    GetSamplingStatisticSummaries (GetSamplingStatisticSummaries'),
    newGetSamplingStatisticSummaries,
    GetSamplingStatisticSummariesResponse (GetSamplingStatisticSummariesResponse'),
    newGetSamplingStatisticSummariesResponse,

    -- ** GetSamplingTargets
    GetSamplingTargets (GetSamplingTargets'),
    newGetSamplingTargets,
    GetSamplingTargetsResponse (GetSamplingTargetsResponse'),
    newGetSamplingTargetsResponse,

    -- ** GetServiceGraph (Paginated)
    GetServiceGraph (GetServiceGraph'),
    newGetServiceGraph,
    GetServiceGraphResponse (GetServiceGraphResponse'),
    newGetServiceGraphResponse,

    -- ** GetTimeSeriesServiceStatistics (Paginated)
    GetTimeSeriesServiceStatistics (GetTimeSeriesServiceStatistics'),
    newGetTimeSeriesServiceStatistics,
    GetTimeSeriesServiceStatisticsResponse (GetTimeSeriesServiceStatisticsResponse'),
    newGetTimeSeriesServiceStatisticsResponse,

    -- ** GetTraceGraph (Paginated)
    GetTraceGraph (GetTraceGraph'),
    newGetTraceGraph,
    GetTraceGraphResponse (GetTraceGraphResponse'),
    newGetTraceGraphResponse,

    -- ** GetTraceSummaries (Paginated)
    GetTraceSummaries (GetTraceSummaries'),
    newGetTraceSummaries,
    GetTraceSummariesResponse (GetTraceSummariesResponse'),
    newGetTraceSummariesResponse,

    -- ** ListResourcePolicies (Paginated)
    ListResourcePolicies (ListResourcePolicies'),
    newListResourcePolicies,
    ListResourcePoliciesResponse (ListResourcePoliciesResponse'),
    newListResourcePoliciesResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutEncryptionConfig
    PutEncryptionConfig (PutEncryptionConfig'),
    newPutEncryptionConfig,
    PutEncryptionConfigResponse (PutEncryptionConfigResponse'),
    newPutEncryptionConfigResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** PutTelemetryRecords
    PutTelemetryRecords (PutTelemetryRecords'),
    newPutTelemetryRecords,
    PutTelemetryRecordsResponse (PutTelemetryRecordsResponse'),
    newPutTelemetryRecordsResponse,

    -- ** PutTraceSegments
    PutTraceSegments (PutTraceSegments'),
    newPutTraceSegments,
    PutTraceSegmentsResponse (PutTraceSegmentsResponse'),
    newPutTraceSegmentsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** UpdateSamplingRule
    UpdateSamplingRule (UpdateSamplingRule'),
    newUpdateSamplingRule,
    UpdateSamplingRuleResponse (UpdateSamplingRuleResponse'),
    newUpdateSamplingRuleResponse,

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

    -- ** ResourcePolicy
    ResourcePolicy (ResourcePolicy'),
    newResourcePolicy,

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
import Amazonka.XRay.Lens
import Amazonka.XRay.ListResourcePolicies
import Amazonka.XRay.ListTagsForResource
import Amazonka.XRay.PutEncryptionConfig
import Amazonka.XRay.PutResourcePolicy
import Amazonka.XRay.PutTelemetryRecords
import Amazonka.XRay.PutTraceSegments
import Amazonka.XRay.TagResource
import Amazonka.XRay.Types
import Amazonka.XRay.UntagResource
import Amazonka.XRay.UpdateGroup
import Amazonka.XRay.UpdateSamplingRule
import Amazonka.XRay.Waiters

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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CodeGuruProfiler
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-07-18@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This section provides documentation for the Amazon CodeGuru Profiler API
-- operations.
--
-- Amazon CodeGuru Profiler collects runtime performance data from your
-- live applications, and provides recommendations that can help you
-- fine-tune your application performance. Using machine learning
-- algorithms, CodeGuru Profiler can help you find your most expensive
-- lines of code and suggest ways you can improve efficiency and remove CPU
-- bottlenecks.
--
-- Amazon CodeGuru Profiler provides different visualizations of profiling
-- data to help you identify what code is running on the CPU, see how much
-- time is consumed, and suggest ways to reduce CPU utilization.
--
-- Amazon CodeGuru Profiler currently supports applications written in all
-- Java virtual machine (JVM) languages and Python. While CodeGuru Profiler
-- supports both visualizations and recommendations for applications
-- written in Java, it can also generate visualizations and a subset of
-- recommendations for applications written in other JVM languages and
-- Python.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-ug/what-is-codeguru-profiler.html What is Amazon CodeGuru Profiler>
-- in the /Amazon CodeGuru Profiler User Guide/.
module Amazonka.CodeGuruProfiler
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddNotificationChannels
    AddNotificationChannels (AddNotificationChannels'),
    newAddNotificationChannels,
    AddNotificationChannelsResponse (AddNotificationChannelsResponse'),
    newAddNotificationChannelsResponse,

    -- ** BatchGetFrameMetricData
    BatchGetFrameMetricData (BatchGetFrameMetricData'),
    newBatchGetFrameMetricData,
    BatchGetFrameMetricDataResponse (BatchGetFrameMetricDataResponse'),
    newBatchGetFrameMetricDataResponse,

    -- ** ConfigureAgent
    ConfigureAgent (ConfigureAgent'),
    newConfigureAgent,
    ConfigureAgentResponse (ConfigureAgentResponse'),
    newConfigureAgentResponse,

    -- ** CreateProfilingGroup
    CreateProfilingGroup (CreateProfilingGroup'),
    newCreateProfilingGroup,
    CreateProfilingGroupResponse (CreateProfilingGroupResponse'),
    newCreateProfilingGroupResponse,

    -- ** DeleteProfilingGroup
    DeleteProfilingGroup (DeleteProfilingGroup'),
    newDeleteProfilingGroup,
    DeleteProfilingGroupResponse (DeleteProfilingGroupResponse'),
    newDeleteProfilingGroupResponse,

    -- ** DescribeProfilingGroup
    DescribeProfilingGroup (DescribeProfilingGroup'),
    newDescribeProfilingGroup,
    DescribeProfilingGroupResponse (DescribeProfilingGroupResponse'),
    newDescribeProfilingGroupResponse,

    -- ** GetFindingsReportAccountSummary
    GetFindingsReportAccountSummary (GetFindingsReportAccountSummary'),
    newGetFindingsReportAccountSummary,
    GetFindingsReportAccountSummaryResponse (GetFindingsReportAccountSummaryResponse'),
    newGetFindingsReportAccountSummaryResponse,

    -- ** GetNotificationConfiguration
    GetNotificationConfiguration (GetNotificationConfiguration'),
    newGetNotificationConfiguration,
    GetNotificationConfigurationResponse (GetNotificationConfigurationResponse'),
    newGetNotificationConfigurationResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** GetProfile
    GetProfile (GetProfile'),
    newGetProfile,
    GetProfileResponse (GetProfileResponse'),
    newGetProfileResponse,

    -- ** GetRecommendations
    GetRecommendations (GetRecommendations'),
    newGetRecommendations,
    GetRecommendationsResponse (GetRecommendationsResponse'),
    newGetRecommendationsResponse,

    -- ** ListFindingsReports
    ListFindingsReports (ListFindingsReports'),
    newListFindingsReports,
    ListFindingsReportsResponse (ListFindingsReportsResponse'),
    newListFindingsReportsResponse,

    -- ** ListProfileTimes (Paginated)
    ListProfileTimes (ListProfileTimes'),
    newListProfileTimes,
    ListProfileTimesResponse (ListProfileTimesResponse'),
    newListProfileTimesResponse,

    -- ** ListProfilingGroups
    ListProfilingGroups (ListProfilingGroups'),
    newListProfilingGroups,
    ListProfilingGroupsResponse (ListProfilingGroupsResponse'),
    newListProfilingGroupsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PostAgentProfile
    PostAgentProfile (PostAgentProfile'),
    newPostAgentProfile,
    PostAgentProfileResponse (PostAgentProfileResponse'),
    newPostAgentProfileResponse,

    -- ** PutPermission
    PutPermission (PutPermission'),
    newPutPermission,
    PutPermissionResponse (PutPermissionResponse'),
    newPutPermissionResponse,

    -- ** RemoveNotificationChannel
    RemoveNotificationChannel (RemoveNotificationChannel'),
    newRemoveNotificationChannel,
    RemoveNotificationChannelResponse (RemoveNotificationChannelResponse'),
    newRemoveNotificationChannelResponse,

    -- ** RemovePermission
    RemovePermission (RemovePermission'),
    newRemovePermission,
    RemovePermissionResponse (RemovePermissionResponse'),
    newRemovePermissionResponse,

    -- ** SubmitFeedback
    SubmitFeedback (SubmitFeedback'),
    newSubmitFeedback,
    SubmitFeedbackResponse (SubmitFeedbackResponse'),
    newSubmitFeedbackResponse,

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

    -- ** UpdateProfilingGroup
    UpdateProfilingGroup (UpdateProfilingGroup'),
    newUpdateProfilingGroup,
    UpdateProfilingGroupResponse (UpdateProfilingGroupResponse'),
    newUpdateProfilingGroupResponse,

    -- * Types

    -- ** ActionGroup
    ActionGroup (..),

    -- ** AgentParameterField
    AgentParameterField (..),

    -- ** AggregationPeriod
    AggregationPeriod (..),

    -- ** ComputePlatform
    ComputePlatform (..),

    -- ** EventPublisher
    EventPublisher (..),

    -- ** FeedbackType
    FeedbackType (..),

    -- ** MetadataField
    MetadataField (..),

    -- ** MetricType
    MetricType (..),

    -- ** OrderBy
    OrderBy (..),

    -- ** AgentConfiguration
    AgentConfiguration (AgentConfiguration'),
    newAgentConfiguration,

    -- ** AgentOrchestrationConfig
    AgentOrchestrationConfig (AgentOrchestrationConfig'),
    newAgentOrchestrationConfig,

    -- ** AggregatedProfileTime
    AggregatedProfileTime (AggregatedProfileTime'),
    newAggregatedProfileTime,

    -- ** Anomaly
    Anomaly (Anomaly'),
    newAnomaly,

    -- ** AnomalyInstance
    AnomalyInstance (AnomalyInstance'),
    newAnomalyInstance,

    -- ** Channel
    Channel (Channel'),
    newChannel,

    -- ** FindingsReportSummary
    FindingsReportSummary (FindingsReportSummary'),
    newFindingsReportSummary,

    -- ** FrameMetric
    FrameMetric (FrameMetric'),
    newFrameMetric,

    -- ** FrameMetricDatum
    FrameMetricDatum (FrameMetricDatum'),
    newFrameMetricDatum,

    -- ** Match
    Match (Match'),
    newMatch,

    -- ** Metric
    Metric (Metric'),
    newMetric,

    -- ** NotificationConfiguration
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** Pattern
    Pattern (Pattern'),
    newPattern,

    -- ** ProfileTime
    ProfileTime (ProfileTime'),
    newProfileTime,

    -- ** ProfilingGroupDescription
    ProfilingGroupDescription (ProfilingGroupDescription'),
    newProfilingGroupDescription,

    -- ** ProfilingStatus
    ProfilingStatus (ProfilingStatus'),
    newProfilingStatus,

    -- ** Recommendation
    Recommendation (Recommendation'),
    newRecommendation,

    -- ** TimestampStructure
    TimestampStructure (TimestampStructure'),
    newTimestampStructure,

    -- ** UserFeedback
    UserFeedback (UserFeedback'),
    newUserFeedback,
  )
where

import Amazonka.CodeGuruProfiler.AddNotificationChannels
import Amazonka.CodeGuruProfiler.BatchGetFrameMetricData
import Amazonka.CodeGuruProfiler.ConfigureAgent
import Amazonka.CodeGuruProfiler.CreateProfilingGroup
import Amazonka.CodeGuruProfiler.DeleteProfilingGroup
import Amazonka.CodeGuruProfiler.DescribeProfilingGroup
import Amazonka.CodeGuruProfiler.GetFindingsReportAccountSummary
import Amazonka.CodeGuruProfiler.GetNotificationConfiguration
import Amazonka.CodeGuruProfiler.GetPolicy
import Amazonka.CodeGuruProfiler.GetProfile
import Amazonka.CodeGuruProfiler.GetRecommendations
import Amazonka.CodeGuruProfiler.Lens
import Amazonka.CodeGuruProfiler.ListFindingsReports
import Amazonka.CodeGuruProfiler.ListProfileTimes
import Amazonka.CodeGuruProfiler.ListProfilingGroups
import Amazonka.CodeGuruProfiler.ListTagsForResource
import Amazonka.CodeGuruProfiler.PostAgentProfile
import Amazonka.CodeGuruProfiler.PutPermission
import Amazonka.CodeGuruProfiler.RemoveNotificationChannel
import Amazonka.CodeGuruProfiler.RemovePermission
import Amazonka.CodeGuruProfiler.SubmitFeedback
import Amazonka.CodeGuruProfiler.TagResource
import Amazonka.CodeGuruProfiler.Types
import Amazonka.CodeGuruProfiler.UntagResource
import Amazonka.CodeGuruProfiler.UpdateProfilingGroup
import Amazonka.CodeGuruProfiler.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CodeGuruProfiler'.

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

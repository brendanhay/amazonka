{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.CodeGuruProfiler
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CodeGuruProfiler
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetRecommendations
    GetRecommendations (GetRecommendations'),
    newGetRecommendations,
    GetRecommendationsResponse (GetRecommendationsResponse'),
    newGetRecommendationsResponse,

    -- ** AddNotificationChannels
    AddNotificationChannels (AddNotificationChannels'),
    newAddNotificationChannels,
    AddNotificationChannelsResponse (AddNotificationChannelsResponse'),
    newAddNotificationChannelsResponse,

    -- ** DescribeProfilingGroup
    DescribeProfilingGroup (DescribeProfilingGroup'),
    newDescribeProfilingGroup,
    DescribeProfilingGroupResponse (DescribeProfilingGroupResponse'),
    newDescribeProfilingGroupResponse,

    -- ** PutPermission
    PutPermission (PutPermission'),
    newPutPermission,
    PutPermissionResponse (PutPermissionResponse'),
    newPutPermissionResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** CreateProfilingGroup
    CreateProfilingGroup (CreateProfilingGroup'),
    newCreateProfilingGroup,
    CreateProfilingGroupResponse (CreateProfilingGroupResponse'),
    newCreateProfilingGroupResponse,

    -- ** RemoveNotificationChannel
    RemoveNotificationChannel (RemoveNotificationChannel'),
    newRemoveNotificationChannel,
    RemoveNotificationChannelResponse (RemoveNotificationChannelResponse'),
    newRemoveNotificationChannelResponse,

    -- ** UpdateProfilingGroup
    UpdateProfilingGroup (UpdateProfilingGroup'),
    newUpdateProfilingGroup,
    UpdateProfilingGroupResponse (UpdateProfilingGroupResponse'),
    newUpdateProfilingGroupResponse,

    -- ** DeleteProfilingGroup
    DeleteProfilingGroup (DeleteProfilingGroup'),
    newDeleteProfilingGroup,
    DeleteProfilingGroupResponse (DeleteProfilingGroupResponse'),
    newDeleteProfilingGroupResponse,

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

    -- ** PostAgentProfile
    PostAgentProfile (PostAgentProfile'),
    newPostAgentProfile,
    PostAgentProfileResponse (PostAgentProfileResponse'),
    newPostAgentProfileResponse,

    -- ** GetProfile
    GetProfile (GetProfile'),
    newGetProfile,
    GetProfileResponse (GetProfileResponse'),
    newGetProfileResponse,

    -- ** ListProfilingGroups
    ListProfilingGroups (ListProfilingGroups'),
    newListProfilingGroups,
    ListProfilingGroupsResponse (ListProfilingGroupsResponse'),
    newListProfilingGroupsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetNotificationConfiguration
    GetNotificationConfiguration (GetNotificationConfiguration'),
    newGetNotificationConfiguration,
    GetNotificationConfigurationResponse (GetNotificationConfigurationResponse'),
    newGetNotificationConfigurationResponse,

    -- ** BatchGetFrameMetricData
    BatchGetFrameMetricData (BatchGetFrameMetricData'),
    newBatchGetFrameMetricData,
    BatchGetFrameMetricDataResponse (BatchGetFrameMetricDataResponse'),
    newBatchGetFrameMetricDataResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetFindingsReportAccountSummary
    GetFindingsReportAccountSummary (GetFindingsReportAccountSummary'),
    newGetFindingsReportAccountSummary,
    GetFindingsReportAccountSummaryResponse (GetFindingsReportAccountSummaryResponse'),
    newGetFindingsReportAccountSummaryResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** ConfigureAgent
    ConfigureAgent (ConfigureAgent'),
    newConfigureAgent,
    ConfigureAgentResponse (ConfigureAgentResponse'),
    newConfigureAgentResponse,

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

import Network.AWS.CodeGuruProfiler.AddNotificationChannels
import Network.AWS.CodeGuruProfiler.BatchGetFrameMetricData
import Network.AWS.CodeGuruProfiler.ConfigureAgent
import Network.AWS.CodeGuruProfiler.CreateProfilingGroup
import Network.AWS.CodeGuruProfiler.DeleteProfilingGroup
import Network.AWS.CodeGuruProfiler.DescribeProfilingGroup
import Network.AWS.CodeGuruProfiler.GetFindingsReportAccountSummary
import Network.AWS.CodeGuruProfiler.GetNotificationConfiguration
import Network.AWS.CodeGuruProfiler.GetPolicy
import Network.AWS.CodeGuruProfiler.GetProfile
import Network.AWS.CodeGuruProfiler.GetRecommendations
import Network.AWS.CodeGuruProfiler.Lens
import Network.AWS.CodeGuruProfiler.ListFindingsReports
import Network.AWS.CodeGuruProfiler.ListProfileTimes
import Network.AWS.CodeGuruProfiler.ListProfilingGroups
import Network.AWS.CodeGuruProfiler.ListTagsForResource
import Network.AWS.CodeGuruProfiler.PostAgentProfile
import Network.AWS.CodeGuruProfiler.PutPermission
import Network.AWS.CodeGuruProfiler.RemoveNotificationChannel
import Network.AWS.CodeGuruProfiler.RemovePermission
import Network.AWS.CodeGuruProfiler.SubmitFeedback
import Network.AWS.CodeGuruProfiler.TagResource
import Network.AWS.CodeGuruProfiler.Types
import Network.AWS.CodeGuruProfiler.UntagResource
import Network.AWS.CodeGuruProfiler.UpdateProfilingGroup
import Network.AWS.CodeGuruProfiler.Waiters

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

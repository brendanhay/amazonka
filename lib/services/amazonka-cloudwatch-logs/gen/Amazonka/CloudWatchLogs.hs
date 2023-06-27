{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudWatchLogs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-03-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- You can use Amazon CloudWatch Logs to monitor, store, and access your
-- log files from EC2 instances, CloudTrail, and other sources. You can
-- then retrieve the associated log data from CloudWatch Logs using the
-- CloudWatch console. Alternatively, you can use CloudWatch Logs commands
-- in the Amazon Web Services CLI, CloudWatch Logs API, or CloudWatch Logs
-- SDK.
--
-- You can use CloudWatch Logs to:
--
-- -   __Monitor logs from EC2 instances in real time__: You can use
--     CloudWatch Logs to monitor applications and systems using log data.
--     For example, CloudWatch Logs can track the number of errors that
--     occur in your application logs. Then, it can send you a notification
--     whenever the rate of errors exceeds a threshold that you specify.
--     CloudWatch Logs uses your log data for monitoring so no code changes
--     are required. For example, you can monitor application logs for
--     specific literal terms (such as \"NullReferenceException\"). You can
--     also count the number of occurrences of a literal term at a
--     particular position in log data (such as \"404\" status codes in an
--     Apache access log). When the term you are searching for is found,
--     CloudWatch Logs reports the data to a CloudWatch metric that you
--     specify.
--
-- -   __Monitor CloudTrail logged events__: You can create alarms in
--     CloudWatch and receive notifications of particular API activity as
--     captured by CloudTrail. You can use the notification to perform
--     troubleshooting.
--
-- -   __Archive log data__: You can use CloudWatch Logs to store your log
--     data in highly durable storage. You can change the log retention
--     setting so that any log events earlier than this setting are
--     automatically deleted. The CloudWatch Logs agent helps to quickly
--     send both rotated and non-rotated log data off of a host and into
--     the log service. You can then access the raw log data when you need
--     it.
module Amazonka.CloudWatchLogs
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DataAlreadyAcceptedException
    _DataAlreadyAcceptedException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidSequenceTokenException
    _InvalidSequenceTokenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MalformedQueryException
    _MalformedQueryException,

    -- ** OperationAbortedException
    _OperationAbortedException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnrecognizedClientException
    _UnrecognizedClientException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateKmsKey
    AssociateKmsKey (AssociateKmsKey'),
    newAssociateKmsKey,
    AssociateKmsKeyResponse (AssociateKmsKeyResponse'),
    newAssociateKmsKeyResponse,

    -- ** CancelExportTask
    CancelExportTask (CancelExportTask'),
    newCancelExportTask,
    CancelExportTaskResponse (CancelExportTaskResponse'),
    newCancelExportTaskResponse,

    -- ** CreateExportTask
    CreateExportTask (CreateExportTask'),
    newCreateExportTask,
    CreateExportTaskResponse (CreateExportTaskResponse'),
    newCreateExportTaskResponse,

    -- ** CreateLogGroup
    CreateLogGroup (CreateLogGroup'),
    newCreateLogGroup,
    CreateLogGroupResponse (CreateLogGroupResponse'),
    newCreateLogGroupResponse,

    -- ** CreateLogStream
    CreateLogStream (CreateLogStream'),
    newCreateLogStream,
    CreateLogStreamResponse (CreateLogStreamResponse'),
    newCreateLogStreamResponse,

    -- ** DeleteAccountPolicy
    DeleteAccountPolicy (DeleteAccountPolicy'),
    newDeleteAccountPolicy,
    DeleteAccountPolicyResponse (DeleteAccountPolicyResponse'),
    newDeleteAccountPolicyResponse,

    -- ** DeleteDataProtectionPolicy
    DeleteDataProtectionPolicy (DeleteDataProtectionPolicy'),
    newDeleteDataProtectionPolicy,
    DeleteDataProtectionPolicyResponse (DeleteDataProtectionPolicyResponse'),
    newDeleteDataProtectionPolicyResponse,

    -- ** DeleteDestination
    DeleteDestination (DeleteDestination'),
    newDeleteDestination,
    DeleteDestinationResponse (DeleteDestinationResponse'),
    newDeleteDestinationResponse,

    -- ** DeleteLogGroup
    DeleteLogGroup (DeleteLogGroup'),
    newDeleteLogGroup,
    DeleteLogGroupResponse (DeleteLogGroupResponse'),
    newDeleteLogGroupResponse,

    -- ** DeleteLogStream
    DeleteLogStream (DeleteLogStream'),
    newDeleteLogStream,
    DeleteLogStreamResponse (DeleteLogStreamResponse'),
    newDeleteLogStreamResponse,

    -- ** DeleteMetricFilter
    DeleteMetricFilter (DeleteMetricFilter'),
    newDeleteMetricFilter,
    DeleteMetricFilterResponse (DeleteMetricFilterResponse'),
    newDeleteMetricFilterResponse,

    -- ** DeleteQueryDefinition
    DeleteQueryDefinition (DeleteQueryDefinition'),
    newDeleteQueryDefinition,
    DeleteQueryDefinitionResponse (DeleteQueryDefinitionResponse'),
    newDeleteQueryDefinitionResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeleteRetentionPolicy
    DeleteRetentionPolicy (DeleteRetentionPolicy'),
    newDeleteRetentionPolicy,
    DeleteRetentionPolicyResponse (DeleteRetentionPolicyResponse'),
    newDeleteRetentionPolicyResponse,

    -- ** DeleteSubscriptionFilter
    DeleteSubscriptionFilter (DeleteSubscriptionFilter'),
    newDeleteSubscriptionFilter,
    DeleteSubscriptionFilterResponse (DeleteSubscriptionFilterResponse'),
    newDeleteSubscriptionFilterResponse,

    -- ** DescribeAccountPolicies
    DescribeAccountPolicies (DescribeAccountPolicies'),
    newDescribeAccountPolicies,
    DescribeAccountPoliciesResponse (DescribeAccountPoliciesResponse'),
    newDescribeAccountPoliciesResponse,

    -- ** DescribeDestinations (Paginated)
    DescribeDestinations (DescribeDestinations'),
    newDescribeDestinations,
    DescribeDestinationsResponse (DescribeDestinationsResponse'),
    newDescribeDestinationsResponse,

    -- ** DescribeExportTasks (Paginated)
    DescribeExportTasks (DescribeExportTasks'),
    newDescribeExportTasks,
    DescribeExportTasksResponse (DescribeExportTasksResponse'),
    newDescribeExportTasksResponse,

    -- ** DescribeLogGroups (Paginated)
    DescribeLogGroups (DescribeLogGroups'),
    newDescribeLogGroups,
    DescribeLogGroupsResponse (DescribeLogGroupsResponse'),
    newDescribeLogGroupsResponse,

    -- ** DescribeLogStreams (Paginated)
    DescribeLogStreams (DescribeLogStreams'),
    newDescribeLogStreams,
    DescribeLogStreamsResponse (DescribeLogStreamsResponse'),
    newDescribeLogStreamsResponse,

    -- ** DescribeMetricFilters (Paginated)
    DescribeMetricFilters (DescribeMetricFilters'),
    newDescribeMetricFilters,
    DescribeMetricFiltersResponse (DescribeMetricFiltersResponse'),
    newDescribeMetricFiltersResponse,

    -- ** DescribeQueries (Paginated)
    DescribeQueries (DescribeQueries'),
    newDescribeQueries,
    DescribeQueriesResponse (DescribeQueriesResponse'),
    newDescribeQueriesResponse,

    -- ** DescribeQueryDefinitions
    DescribeQueryDefinitions (DescribeQueryDefinitions'),
    newDescribeQueryDefinitions,
    DescribeQueryDefinitionsResponse (DescribeQueryDefinitionsResponse'),
    newDescribeQueryDefinitionsResponse,

    -- ** DescribeResourcePolicies (Paginated)
    DescribeResourcePolicies (DescribeResourcePolicies'),
    newDescribeResourcePolicies,
    DescribeResourcePoliciesResponse (DescribeResourcePoliciesResponse'),
    newDescribeResourcePoliciesResponse,

    -- ** DescribeSubscriptionFilters (Paginated)
    DescribeSubscriptionFilters (DescribeSubscriptionFilters'),
    newDescribeSubscriptionFilters,
    DescribeSubscriptionFiltersResponse (DescribeSubscriptionFiltersResponse'),
    newDescribeSubscriptionFiltersResponse,

    -- ** DisassociateKmsKey
    DisassociateKmsKey (DisassociateKmsKey'),
    newDisassociateKmsKey,
    DisassociateKmsKeyResponse (DisassociateKmsKeyResponse'),
    newDisassociateKmsKeyResponse,

    -- ** FilterLogEvents (Paginated)
    FilterLogEvents (FilterLogEvents'),
    newFilterLogEvents,
    FilterLogEventsResponse (FilterLogEventsResponse'),
    newFilterLogEventsResponse,

    -- ** GetDataProtectionPolicy
    GetDataProtectionPolicy (GetDataProtectionPolicy'),
    newGetDataProtectionPolicy,
    GetDataProtectionPolicyResponse (GetDataProtectionPolicyResponse'),
    newGetDataProtectionPolicyResponse,

    -- ** GetLogEvents
    GetLogEvents (GetLogEvents'),
    newGetLogEvents,
    GetLogEventsResponse (GetLogEventsResponse'),
    newGetLogEventsResponse,

    -- ** GetLogGroupFields
    GetLogGroupFields (GetLogGroupFields'),
    newGetLogGroupFields,
    GetLogGroupFieldsResponse (GetLogGroupFieldsResponse'),
    newGetLogGroupFieldsResponse,

    -- ** GetLogRecord
    GetLogRecord (GetLogRecord'),
    newGetLogRecord,
    GetLogRecordResponse (GetLogRecordResponse'),
    newGetLogRecordResponse,

    -- ** GetQueryResults
    GetQueryResults (GetQueryResults'),
    newGetQueryResults,
    GetQueryResultsResponse (GetQueryResultsResponse'),
    newGetQueryResultsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutAccountPolicy
    PutAccountPolicy (PutAccountPolicy'),
    newPutAccountPolicy,
    PutAccountPolicyResponse (PutAccountPolicyResponse'),
    newPutAccountPolicyResponse,

    -- ** PutDataProtectionPolicy
    PutDataProtectionPolicy (PutDataProtectionPolicy'),
    newPutDataProtectionPolicy,
    PutDataProtectionPolicyResponse (PutDataProtectionPolicyResponse'),
    newPutDataProtectionPolicyResponse,

    -- ** PutDestination
    PutDestination (PutDestination'),
    newPutDestination,
    PutDestinationResponse (PutDestinationResponse'),
    newPutDestinationResponse,

    -- ** PutDestinationPolicy
    PutDestinationPolicy (PutDestinationPolicy'),
    newPutDestinationPolicy,
    PutDestinationPolicyResponse (PutDestinationPolicyResponse'),
    newPutDestinationPolicyResponse,

    -- ** PutLogEvents
    PutLogEvents (PutLogEvents'),
    newPutLogEvents,
    PutLogEventsResponse (PutLogEventsResponse'),
    newPutLogEventsResponse,

    -- ** PutMetricFilter
    PutMetricFilter (PutMetricFilter'),
    newPutMetricFilter,
    PutMetricFilterResponse (PutMetricFilterResponse'),
    newPutMetricFilterResponse,

    -- ** PutQueryDefinition
    PutQueryDefinition (PutQueryDefinition'),
    newPutQueryDefinition,
    PutQueryDefinitionResponse (PutQueryDefinitionResponse'),
    newPutQueryDefinitionResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** PutRetentionPolicy
    PutRetentionPolicy (PutRetentionPolicy'),
    newPutRetentionPolicy,
    PutRetentionPolicyResponse (PutRetentionPolicyResponse'),
    newPutRetentionPolicyResponse,

    -- ** PutSubscriptionFilter
    PutSubscriptionFilter (PutSubscriptionFilter'),
    newPutSubscriptionFilter,
    PutSubscriptionFilterResponse (PutSubscriptionFilterResponse'),
    newPutSubscriptionFilterResponse,

    -- ** StartQuery
    StartQuery (StartQuery'),
    newStartQuery,
    StartQueryResponse (StartQueryResponse'),
    newStartQueryResponse,

    -- ** StopQuery
    StopQuery (StopQuery'),
    newStopQuery,
    StopQueryResponse (StopQueryResponse'),
    newStopQueryResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestMetricFilter
    TestMetricFilter (TestMetricFilter'),
    newTestMetricFilter,
    TestMetricFilterResponse (TestMetricFilterResponse'),
    newTestMetricFilterResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- * Types

    -- ** DataProtectionStatus
    DataProtectionStatus (..),

    -- ** Distribution
    Distribution (..),

    -- ** ExportTaskStatusCode
    ExportTaskStatusCode (..),

    -- ** InheritedProperty
    InheritedProperty (..),

    -- ** OrderBy
    OrderBy (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** QueryStatus
    QueryStatus (..),

    -- ** Scope
    Scope (..),

    -- ** StandardUnit
    StandardUnit (..),

    -- ** AccountPolicy
    AccountPolicy (AccountPolicy'),
    newAccountPolicy,

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** ExportTask
    ExportTask (ExportTask'),
    newExportTask,

    -- ** ExportTaskExecutionInfo
    ExportTaskExecutionInfo (ExportTaskExecutionInfo'),
    newExportTaskExecutionInfo,

    -- ** ExportTaskStatus
    ExportTaskStatus (ExportTaskStatus'),
    newExportTaskStatus,

    -- ** FilteredLogEvent
    FilteredLogEvent (FilteredLogEvent'),
    newFilteredLogEvent,

    -- ** InputLogEvent
    InputLogEvent (InputLogEvent'),
    newInputLogEvent,

    -- ** LogGroup
    LogGroup (LogGroup'),
    newLogGroup,

    -- ** LogGroupField
    LogGroupField (LogGroupField'),
    newLogGroupField,

    -- ** LogStream
    LogStream (LogStream'),
    newLogStream,

    -- ** MetricFilter
    MetricFilter (MetricFilter'),
    newMetricFilter,

    -- ** MetricFilterMatchRecord
    MetricFilterMatchRecord (MetricFilterMatchRecord'),
    newMetricFilterMatchRecord,

    -- ** MetricTransformation
    MetricTransformation (MetricTransformation'),
    newMetricTransformation,

    -- ** OutputLogEvent
    OutputLogEvent (OutputLogEvent'),
    newOutputLogEvent,

    -- ** QueryDefinition
    QueryDefinition (QueryDefinition'),
    newQueryDefinition,

    -- ** QueryInfo
    QueryInfo (QueryInfo'),
    newQueryInfo,

    -- ** QueryStatistics
    QueryStatistics (QueryStatistics'),
    newQueryStatistics,

    -- ** RejectedLogEventsInfo
    RejectedLogEventsInfo (RejectedLogEventsInfo'),
    newRejectedLogEventsInfo,

    -- ** ResourcePolicy
    ResourcePolicy (ResourcePolicy'),
    newResourcePolicy,

    -- ** ResultField
    ResultField (ResultField'),
    newResultField,

    -- ** SearchedLogStream
    SearchedLogStream (SearchedLogStream'),
    newSearchedLogStream,

    -- ** SubscriptionFilter
    SubscriptionFilter (SubscriptionFilter'),
    newSubscriptionFilter,
  )
where

import Amazonka.CloudWatchLogs.AssociateKmsKey
import Amazonka.CloudWatchLogs.CancelExportTask
import Amazonka.CloudWatchLogs.CreateExportTask
import Amazonka.CloudWatchLogs.CreateLogGroup
import Amazonka.CloudWatchLogs.CreateLogStream
import Amazonka.CloudWatchLogs.DeleteAccountPolicy
import Amazonka.CloudWatchLogs.DeleteDataProtectionPolicy
import Amazonka.CloudWatchLogs.DeleteDestination
import Amazonka.CloudWatchLogs.DeleteLogGroup
import Amazonka.CloudWatchLogs.DeleteLogStream
import Amazonka.CloudWatchLogs.DeleteMetricFilter
import Amazonka.CloudWatchLogs.DeleteQueryDefinition
import Amazonka.CloudWatchLogs.DeleteResourcePolicy
import Amazonka.CloudWatchLogs.DeleteRetentionPolicy
import Amazonka.CloudWatchLogs.DeleteSubscriptionFilter
import Amazonka.CloudWatchLogs.DescribeAccountPolicies
import Amazonka.CloudWatchLogs.DescribeDestinations
import Amazonka.CloudWatchLogs.DescribeExportTasks
import Amazonka.CloudWatchLogs.DescribeLogGroups
import Amazonka.CloudWatchLogs.DescribeLogStreams
import Amazonka.CloudWatchLogs.DescribeMetricFilters
import Amazonka.CloudWatchLogs.DescribeQueries
import Amazonka.CloudWatchLogs.DescribeQueryDefinitions
import Amazonka.CloudWatchLogs.DescribeResourcePolicies
import Amazonka.CloudWatchLogs.DescribeSubscriptionFilters
import Amazonka.CloudWatchLogs.DisassociateKmsKey
import Amazonka.CloudWatchLogs.FilterLogEvents
import Amazonka.CloudWatchLogs.GetDataProtectionPolicy
import Amazonka.CloudWatchLogs.GetLogEvents
import Amazonka.CloudWatchLogs.GetLogGroupFields
import Amazonka.CloudWatchLogs.GetLogRecord
import Amazonka.CloudWatchLogs.GetQueryResults
import Amazonka.CloudWatchLogs.Lens
import Amazonka.CloudWatchLogs.ListTagsForResource
import Amazonka.CloudWatchLogs.PutAccountPolicy
import Amazonka.CloudWatchLogs.PutDataProtectionPolicy
import Amazonka.CloudWatchLogs.PutDestination
import Amazonka.CloudWatchLogs.PutDestinationPolicy
import Amazonka.CloudWatchLogs.PutLogEvents
import Amazonka.CloudWatchLogs.PutMetricFilter
import Amazonka.CloudWatchLogs.PutQueryDefinition
import Amazonka.CloudWatchLogs.PutResourcePolicy
import Amazonka.CloudWatchLogs.PutRetentionPolicy
import Amazonka.CloudWatchLogs.PutSubscriptionFilter
import Amazonka.CloudWatchLogs.StartQuery
import Amazonka.CloudWatchLogs.StopQuery
import Amazonka.CloudWatchLogs.TagResource
import Amazonka.CloudWatchLogs.TestMetricFilter
import Amazonka.CloudWatchLogs.Types
import Amazonka.CloudWatchLogs.UntagResource
import Amazonka.CloudWatchLogs.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudWatchLogs'.

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

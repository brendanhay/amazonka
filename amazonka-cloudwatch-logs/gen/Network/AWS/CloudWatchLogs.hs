{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use Amazon CloudWatch Logs to monitor, store, and access your
-- log files from EC2 instances, AWS CloudTrail, or other sources. You can
-- then retrieve the associated log data from CloudWatch Logs using the
-- CloudWatch console, CloudWatch Logs commands in the AWS CLI, CloudWatch
-- Logs API, or CloudWatch Logs SDK.
--
-- You can use CloudWatch Logs to:
--
-- -   __Monitor logs from EC2 instances in real-time__: You can use
--     CloudWatch Logs to monitor applications and systems using log data.
--     For example, CloudWatch Logs can track the number of errors that
--     occur in your application logs and send you a notification whenever
--     the rate of errors exceeds a threshold that you specify. CloudWatch
--     Logs uses your log data for monitoring so no code changes are
--     required. For example, you can monitor application logs for specific
--     literal terms (such as \"NullReferenceException\") or count the
--     number of occurrences of a literal term at a particular position in
--     log data (such as \"404\" status codes in an Apache access log).
--     When the term you are searching for is found, CloudWatch Logs
--     reports the data to a CloudWatch metric that you specify.
--
-- -   __Monitor AWS CloudTrail logged events__: You can create alarms in
--     CloudWatch and receive notifications of particular API activity as
--     captured by CloudTrail. You can use the notification to perform
--     troubleshooting.
--
-- -   __Archive log data__: You can use CloudWatch Logs to store your log
--     data in highly durable storage. You can change the log retention
--     setting so that any log events older than this setting are
--     automatically deleted. The CloudWatch Logs agent makes it easy to
--     quickly send both rotated and non-rotated log data off of a host and
--     into the log service. You can then access the raw log data when you
--     need it.
module Network.AWS.CloudWatchLogs
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** UnrecognizedClientException
    _UnrecognizedClientException,

    -- ** MalformedQueryException
    _MalformedQueryException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** OperationAbortedException
    _OperationAbortedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** DataAlreadyAcceptedException
    _DataAlreadyAcceptedException,

    -- ** InvalidSequenceTokenException
    _InvalidSequenceTokenException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetLogRecord
    GetLogRecord (GetLogRecord'),
    newGetLogRecord,
    GetLogRecordResponse (GetLogRecordResponse'),
    newGetLogRecordResponse,

    -- ** DescribeExportTasks (Paginated)
    DescribeExportTasks (DescribeExportTasks'),
    newDescribeExportTasks,
    DescribeExportTasksResponse (DescribeExportTasksResponse'),
    newDescribeExportTasksResponse,

    -- ** CreateLogStream
    CreateLogStream (CreateLogStream'),
    newCreateLogStream,
    CreateLogStreamResponse (CreateLogStreamResponse'),
    newCreateLogStreamResponse,

    -- ** DescribeResourcePolicies (Paginated)
    DescribeResourcePolicies (DescribeResourcePolicies'),
    newDescribeResourcePolicies,
    DescribeResourcePoliciesResponse (DescribeResourcePoliciesResponse'),
    newDescribeResourcePoliciesResponse,

    -- ** DescribeQueryDefinitions
    DescribeQueryDefinitions (DescribeQueryDefinitions'),
    newDescribeQueryDefinitions,
    DescribeQueryDefinitionsResponse (DescribeQueryDefinitionsResponse'),
    newDescribeQueryDefinitionsResponse,

    -- ** DeleteQueryDefinition
    DeleteQueryDefinition (DeleteQueryDefinition'),
    newDeleteQueryDefinition,
    DeleteQueryDefinitionResponse (DeleteQueryDefinitionResponse'),
    newDeleteQueryDefinitionResponse,

    -- ** DescribeLogStreams (Paginated)
    DescribeLogStreams (DescribeLogStreams'),
    newDescribeLogStreams,
    DescribeLogStreamsResponse (DescribeLogStreamsResponse'),
    newDescribeLogStreamsResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** DisassociateKmsKey
    DisassociateKmsKey (DisassociateKmsKey'),
    newDisassociateKmsKey,
    DisassociateKmsKeyResponse (DisassociateKmsKeyResponse'),
    newDisassociateKmsKeyResponse,

    -- ** DescribeSubscriptionFilters (Paginated)
    DescribeSubscriptionFilters (DescribeSubscriptionFilters'),
    newDescribeSubscriptionFilters,
    DescribeSubscriptionFiltersResponse (DescribeSubscriptionFiltersResponse'),
    newDescribeSubscriptionFiltersResponse,

    -- ** DescribeLogGroups (Paginated)
    DescribeLogGroups (DescribeLogGroups'),
    newDescribeLogGroups,
    DescribeLogGroupsResponse (DescribeLogGroupsResponse'),
    newDescribeLogGroupsResponse,

    -- ** PutRetentionPolicy
    PutRetentionPolicy (PutRetentionPolicy'),
    newPutRetentionPolicy,
    PutRetentionPolicyResponse (PutRetentionPolicyResponse'),
    newPutRetentionPolicyResponse,

    -- ** PutDestinationPolicy
    PutDestinationPolicy (PutDestinationPolicy'),
    newPutDestinationPolicy,
    PutDestinationPolicyResponse (PutDestinationPolicyResponse'),
    newPutDestinationPolicyResponse,

    -- ** DeleteDestination
    DeleteDestination (DeleteDestination'),
    newDeleteDestination,
    DeleteDestinationResponse (DeleteDestinationResponse'),
    newDeleteDestinationResponse,

    -- ** DeleteMetricFilter
    DeleteMetricFilter (DeleteMetricFilter'),
    newDeleteMetricFilter,
    DeleteMetricFilterResponse (DeleteMetricFilterResponse'),
    newDeleteMetricFilterResponse,

    -- ** DescribeDestinations (Paginated)
    DescribeDestinations (DescribeDestinations'),
    newDescribeDestinations,
    DescribeDestinationsResponse (DescribeDestinationsResponse'),
    newDescribeDestinationsResponse,

    -- ** PutSubscriptionFilter
    PutSubscriptionFilter (PutSubscriptionFilter'),
    newPutSubscriptionFilter,
    PutSubscriptionFilterResponse (PutSubscriptionFilterResponse'),
    newPutSubscriptionFilterResponse,

    -- ** DescribeMetricFilters (Paginated)
    DescribeMetricFilters (DescribeMetricFilters'),
    newDescribeMetricFilters,
    DescribeMetricFiltersResponse (DescribeMetricFiltersResponse'),
    newDescribeMetricFiltersResponse,

    -- ** CancelExportTask
    CancelExportTask (CancelExportTask'),
    newCancelExportTask,
    CancelExportTaskResponse (CancelExportTaskResponse'),
    newCancelExportTaskResponse,

    -- ** CreateLogGroup
    CreateLogGroup (CreateLogGroup'),
    newCreateLogGroup,
    CreateLogGroupResponse (CreateLogGroupResponse'),
    newCreateLogGroupResponse,

    -- ** GetLogGroupFields
    GetLogGroupFields (GetLogGroupFields'),
    newGetLogGroupFields,
    GetLogGroupFieldsResponse (GetLogGroupFieldsResponse'),
    newGetLogGroupFieldsResponse,

    -- ** DescribeQueries (Paginated)
    DescribeQueries (DescribeQueries'),
    newDescribeQueries,
    DescribeQueriesResponse (DescribeQueriesResponse'),
    newDescribeQueriesResponse,

    -- ** DeleteLogStream
    DeleteLogStream (DeleteLogStream'),
    newDeleteLogStream,
    DeleteLogStreamResponse (DeleteLogStreamResponse'),
    newDeleteLogStreamResponse,

    -- ** TagLogGroup
    TagLogGroup (TagLogGroup'),
    newTagLogGroup,
    TagLogGroupResponse (TagLogGroupResponse'),
    newTagLogGroupResponse,

    -- ** AssociateKmsKey
    AssociateKmsKey (AssociateKmsKey'),
    newAssociateKmsKey,
    AssociateKmsKeyResponse (AssociateKmsKeyResponse'),
    newAssociateKmsKeyResponse,

    -- ** GetQueryResults
    GetQueryResults (GetQueryResults'),
    newGetQueryResults,
    GetQueryResultsResponse (GetQueryResultsResponse'),
    newGetQueryResultsResponse,

    -- ** PutQueryDefinition
    PutQueryDefinition (PutQueryDefinition'),
    newPutQueryDefinition,
    PutQueryDefinitionResponse (PutQueryDefinitionResponse'),
    newPutQueryDefinitionResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** FilterLogEvents (Paginated)
    FilterLogEvents (FilterLogEvents'),
    newFilterLogEvents,
    FilterLogEventsResponse (FilterLogEventsResponse'),
    newFilterLogEventsResponse,

    -- ** ListTagsLogGroup
    ListTagsLogGroup (ListTagsLogGroup'),
    newListTagsLogGroup,
    ListTagsLogGroupResponse (ListTagsLogGroupResponse'),
    newListTagsLogGroupResponse,

    -- ** DeleteRetentionPolicy
    DeleteRetentionPolicy (DeleteRetentionPolicy'),
    newDeleteRetentionPolicy,
    DeleteRetentionPolicyResponse (DeleteRetentionPolicyResponse'),
    newDeleteRetentionPolicyResponse,

    -- ** PutDestination
    PutDestination (PutDestination'),
    newPutDestination,
    PutDestinationResponse (PutDestinationResponse'),
    newPutDestinationResponse,

    -- ** PutMetricFilter
    PutMetricFilter (PutMetricFilter'),
    newPutMetricFilter,
    PutMetricFilterResponse (PutMetricFilterResponse'),
    newPutMetricFilterResponse,

    -- ** CreateExportTask
    CreateExportTask (CreateExportTask'),
    newCreateExportTask,
    CreateExportTaskResponse (CreateExportTaskResponse'),
    newCreateExportTaskResponse,

    -- ** GetLogEvents
    GetLogEvents (GetLogEvents'),
    newGetLogEvents,
    GetLogEventsResponse (GetLogEventsResponse'),
    newGetLogEventsResponse,

    -- ** PutLogEvents
    PutLogEvents (PutLogEvents'),
    newPutLogEvents,
    PutLogEventsResponse (PutLogEventsResponse'),
    newPutLogEventsResponse,

    -- ** StopQuery
    StopQuery (StopQuery'),
    newStopQuery,
    StopQueryResponse (StopQueryResponse'),
    newStopQueryResponse,

    -- ** DeleteLogGroup
    DeleteLogGroup (DeleteLogGroup'),
    newDeleteLogGroup,
    DeleteLogGroupResponse (DeleteLogGroupResponse'),
    newDeleteLogGroupResponse,

    -- ** UntagLogGroup
    UntagLogGroup (UntagLogGroup'),
    newUntagLogGroup,
    UntagLogGroupResponse (UntagLogGroupResponse'),
    newUntagLogGroupResponse,

    -- ** TestMetricFilter
    TestMetricFilter (TestMetricFilter'),
    newTestMetricFilter,
    TestMetricFilterResponse (TestMetricFilterResponse'),
    newTestMetricFilterResponse,

    -- ** StartQuery
    StartQuery (StartQuery'),
    newStartQuery,
    StartQueryResponse (StartQueryResponse'),
    newStartQueryResponse,

    -- ** DeleteSubscriptionFilter
    DeleteSubscriptionFilter (DeleteSubscriptionFilter'),
    newDeleteSubscriptionFilter,
    DeleteSubscriptionFilterResponse (DeleteSubscriptionFilterResponse'),
    newDeleteSubscriptionFilterResponse,

    -- * Types

    -- ** Distribution
    Distribution (..),

    -- ** ExportTaskStatusCode
    ExportTaskStatusCode (..),

    -- ** OrderBy
    OrderBy (..),

    -- ** QueryStatus
    QueryStatus (..),

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

import Network.AWS.CloudWatchLogs.AssociateKmsKey
import Network.AWS.CloudWatchLogs.CancelExportTask
import Network.AWS.CloudWatchLogs.CreateExportTask
import Network.AWS.CloudWatchLogs.CreateLogGroup
import Network.AWS.CloudWatchLogs.CreateLogStream
import Network.AWS.CloudWatchLogs.DeleteDestination
import Network.AWS.CloudWatchLogs.DeleteLogGroup
import Network.AWS.CloudWatchLogs.DeleteLogStream
import Network.AWS.CloudWatchLogs.DeleteMetricFilter
import Network.AWS.CloudWatchLogs.DeleteQueryDefinition
import Network.AWS.CloudWatchLogs.DeleteResourcePolicy
import Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
import Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
import Network.AWS.CloudWatchLogs.DescribeDestinations
import Network.AWS.CloudWatchLogs.DescribeExportTasks
import Network.AWS.CloudWatchLogs.DescribeLogGroups
import Network.AWS.CloudWatchLogs.DescribeLogStreams
import Network.AWS.CloudWatchLogs.DescribeMetricFilters
import Network.AWS.CloudWatchLogs.DescribeQueries
import Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
import Network.AWS.CloudWatchLogs.DescribeResourcePolicies
import Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
import Network.AWS.CloudWatchLogs.DisassociateKmsKey
import Network.AWS.CloudWatchLogs.FilterLogEvents
import Network.AWS.CloudWatchLogs.GetLogEvents
import Network.AWS.CloudWatchLogs.GetLogGroupFields
import Network.AWS.CloudWatchLogs.GetLogRecord
import Network.AWS.CloudWatchLogs.GetQueryResults
import Network.AWS.CloudWatchLogs.Lens
import Network.AWS.CloudWatchLogs.ListTagsLogGroup
import Network.AWS.CloudWatchLogs.PutDestination
import Network.AWS.CloudWatchLogs.PutDestinationPolicy
import Network.AWS.CloudWatchLogs.PutLogEvents
import Network.AWS.CloudWatchLogs.PutMetricFilter
import Network.AWS.CloudWatchLogs.PutQueryDefinition
import Network.AWS.CloudWatchLogs.PutResourcePolicy
import Network.AWS.CloudWatchLogs.PutRetentionPolicy
import Network.AWS.CloudWatchLogs.PutSubscriptionFilter
import Network.AWS.CloudWatchLogs.StartQuery
import Network.AWS.CloudWatchLogs.StopQuery
import Network.AWS.CloudWatchLogs.TagLogGroup
import Network.AWS.CloudWatchLogs.TestMetricFilter
import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.UntagLogGroup
import Network.AWS.CloudWatchLogs.Waiters

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

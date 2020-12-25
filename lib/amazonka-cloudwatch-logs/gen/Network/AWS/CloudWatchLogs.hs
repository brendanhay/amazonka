{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use Amazon CloudWatch Logs to monitor, store, and access your log files from EC2 instances, AWS CloudTrail, or other sources. You can then retrieve the associated log data from CloudWatch Logs using the CloudWatch console, CloudWatch Logs commands in the AWS CLI, CloudWatch Logs API, or CloudWatch Logs SDK.
--
-- You can use CloudWatch Logs to:
--
--     * __Monitor logs from EC2 instances in real-time__ : You can use CloudWatch Logs to monitor applications and systems using log data. For example, CloudWatch Logs can track the number of errors that occur in your application logs and send you a notification whenever the rate of errors exceeds a threshold that you specify. CloudWatch Logs uses your log data for monitoring so no code changes are required. For example, you can monitor application logs for specific literal terms (such as "NullReferenceException") or count the number of occurrences of a literal term at a particular position in log data (such as "404" status codes in an Apache access log). When the term you are searching for is found, CloudWatch Logs reports the data to a CloudWatch metric that you specify.
--
--
--     * __Monitor AWS CloudTrail logged events__ : You can create alarms in CloudWatch and receive notifications of particular API activity as captured by CloudTrail. You can use the notification to perform troubleshooting.
--
--
--     * __Archive log data__ : You can use CloudWatch Logs to store your log data in highly durable storage. You can change the log retention setting so that any log events older than this setting are automatically deleted. The CloudWatch Logs agent makes it easy to quickly send both rotated and non-rotated log data off of a host and into the log service. You can then access the raw log data when you need it.
module Network.AWS.CloudWatchLogs
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidSequenceTokenException
    _InvalidSequenceTokenException,

    -- ** UnrecognizedClientException
    _UnrecognizedClientException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** OperationAbortedException
    _OperationAbortedException,

    -- ** MalformedQueryException
    _MalformedQueryException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** DataAlreadyAcceptedException
    _DataAlreadyAcceptedException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetLogGroupFields
    module Network.AWS.CloudWatchLogs.GetLogGroupFields,

    -- ** GetLogRecord
    module Network.AWS.CloudWatchLogs.GetLogRecord,

    -- ** DescribeDestinations (Paginated)
    module Network.AWS.CloudWatchLogs.DescribeDestinations,

    -- ** UntagLogGroup
    module Network.AWS.CloudWatchLogs.UntagLogGroup,

    -- ** StopQuery
    module Network.AWS.CloudWatchLogs.StopQuery,

    -- ** CreateExportTask
    module Network.AWS.CloudWatchLogs.CreateExportTask,

    -- ** PutDestination
    module Network.AWS.CloudWatchLogs.PutDestination,

    -- ** DescribeSubscriptionFilters (Paginated)
    module Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters,

    -- ** GetLogEvents
    module Network.AWS.CloudWatchLogs.GetLogEvents,

    -- ** DescribeLogGroups (Paginated)
    module Network.AWS.CloudWatchLogs.DescribeLogGroups,

    -- ** DeleteDestination
    module Network.AWS.CloudWatchLogs.DeleteDestination,

    -- ** DisassociateKmsKey
    module Network.AWS.CloudWatchLogs.DisassociateKmsKey,

    -- ** FilterLogEvents (Paginated)
    module Network.AWS.CloudWatchLogs.FilterLogEvents,

    -- ** DeleteQueryDefinition
    module Network.AWS.CloudWatchLogs.DeleteQueryDefinition,

    -- ** PutQueryDefinition
    module Network.AWS.CloudWatchLogs.PutQueryDefinition,

    -- ** TagLogGroup
    module Network.AWS.CloudWatchLogs.TagLogGroup,

    -- ** DescribeResourcePolicies (Paginated)
    module Network.AWS.CloudWatchLogs.DescribeResourcePolicies,

    -- ** DescribeQueryDefinitions
    module Network.AWS.CloudWatchLogs.DescribeQueryDefinitions,

    -- ** DeleteLogStream
    module Network.AWS.CloudWatchLogs.DeleteLogStream,

    -- ** DescribeQueries (Paginated)
    module Network.AWS.CloudWatchLogs.DescribeQueries,

    -- ** CreateLogStream
    module Network.AWS.CloudWatchLogs.CreateLogStream,

    -- ** CreateLogGroup
    module Network.AWS.CloudWatchLogs.CreateLogGroup,

    -- ** DescribeExportTasks (Paginated)
    module Network.AWS.CloudWatchLogs.DescribeExportTasks,

    -- ** CancelExportTask
    module Network.AWS.CloudWatchLogs.CancelExportTask,

    -- ** PutSubscriptionFilter
    module Network.AWS.CloudWatchLogs.PutSubscriptionFilter,

    -- ** StartQuery
    module Network.AWS.CloudWatchLogs.StartQuery,

    -- ** DeleteLogGroup
    module Network.AWS.CloudWatchLogs.DeleteLogGroup,

    -- ** DeleteSubscriptionFilter
    module Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter,

    -- ** PutLogEvents
    module Network.AWS.CloudWatchLogs.PutLogEvents,

    -- ** DescribeMetricFilters (Paginated)
    module Network.AWS.CloudWatchLogs.DescribeMetricFilters,

    -- ** TestMetricFilter
    module Network.AWS.CloudWatchLogs.TestMetricFilter,

    -- ** PutDestinationPolicy
    module Network.AWS.CloudWatchLogs.PutDestinationPolicy,

    -- ** PutMetricFilter
    module Network.AWS.CloudWatchLogs.PutMetricFilter,

    -- ** DeleteRetentionPolicy
    module Network.AWS.CloudWatchLogs.DeleteRetentionPolicy,

    -- ** DeleteMetricFilter
    module Network.AWS.CloudWatchLogs.DeleteMetricFilter,

    -- ** PutRetentionPolicy
    module Network.AWS.CloudWatchLogs.PutRetentionPolicy,

    -- ** ListTagsLogGroup
    module Network.AWS.CloudWatchLogs.ListTagsLogGroup,

    -- ** PutResourcePolicy
    module Network.AWS.CloudWatchLogs.PutResourcePolicy,

    -- ** DeleteResourcePolicy
    module Network.AWS.CloudWatchLogs.DeleteResourcePolicy,

    -- ** AssociateKmsKey
    module Network.AWS.CloudWatchLogs.AssociateKmsKey,

    -- ** GetQueryResults
    module Network.AWS.CloudWatchLogs.GetQueryResults,

    -- ** DescribeLogStreams (Paginated)
    module Network.AWS.CloudWatchLogs.DescribeLogStreams,

    -- * Types

    -- ** MetricFilter
    MetricFilter (..),
    mkMetricFilter,
    mfCreationTime,
    mfFilterName,
    mfFilterPattern,
    mfLogGroupName,
    mfMetricTransformations,

    -- ** SequenceToken
    SequenceToken (..),

    -- ** ExportTaskId
    ExportTaskId (..),

    -- ** TargetArn
    TargetArn (..),

    -- ** Destination
    Destination (..),
    mkDestination,
    dAccessPolicy,
    dArn,
    dCreationTime,
    dDestinationName,
    dRoleArn,
    dTargetArn,

    -- ** QueryId
    QueryId (..),

    -- ** PolicyDocument
    PolicyDocument (..),

    -- ** FilterName
    FilterName (..),

    -- ** QueryStatistics
    QueryStatistics (..),
    mkQueryStatistics,
    qsBytesScanned,
    qsRecordsMatched,
    qsRecordsScanned,

    -- ** SearchedLogStream
    SearchedLogStream (..),
    mkSearchedLogStream,
    slsLogStreamName,
    slsSearchedCompletely,

    -- ** PolicyName
    PolicyName (..),

    -- ** MetricFilterMatchRecord
    MetricFilterMatchRecord (..),
    mkMetricFilterMatchRecord,
    mfmrEventMessage,
    mfmrEventNumber,
    mfmrExtractedValues,

    -- ** Field
    Field (..),

    -- ** ResourcePolicy
    ResourcePolicy (..),
    mkResourcePolicy,
    rpLastUpdatedTime,
    rpPolicyDocument,
    rpPolicyName,

    -- ** Distribution
    Distribution (..),

    -- ** LogRecordPointer
    LogRecordPointer (..),

    -- ** Arn
    Arn (..),

    -- ** MetricTransformation
    MetricTransformation (..),
    mkMetricTransformation,
    mtMetricName,
    mtMetricNamespace,
    mtMetricValue,
    mtDefaultValue,

    -- ** OrderBy
    OrderBy (..),

    -- ** QueryDefinitionName
    QueryDefinitionName (..),

    -- ** ExportTaskStatusCode
    ExportTaskStatusCode (..),

    -- ** LogStream
    LogStream (..),
    mkLogStream,
    lsArn,
    lsCreationTime,
    lsFirstEventTimestamp,
    lsLastEventTimestamp,
    lsLastIngestionTime,
    lsLogStreamName,
    lsStoredBytes,
    lsUploadSequenceToken,

    -- ** ExportDestinationPrefix
    ExportDestinationPrefix (..),

    -- ** Token
    Token (..),

    -- ** MetricName
    MetricName (..),

    -- ** LogGroup
    LogGroup (..),
    mkLogGroup,
    lgArn,
    lgCreationTime,
    lgKmsKeyId,
    lgLogGroupName,
    lgMetricFilterCount,
    lgRetentionInDays,
    lgStoredBytes,

    -- ** SubscriptionFilter
    SubscriptionFilter (..),
    mkSubscriptionFilter,
    sfCreationTime,
    sfDestinationArn,
    sfDistribution,
    sfFilterName,
    sfFilterPattern,
    sfLogGroupName,
    sfRoleArn,

    -- ** RejectedLogEventsInfo
    RejectedLogEventsInfo (..),
    mkRejectedLogEventsInfo,
    rleiExpiredLogEventEndIndex,
    rleiTooNewLogEventStartIndex,
    rleiTooOldLogEventEndIndex,

    -- ** Value
    Value (..),

    -- ** ExportTaskStatusMessage
    ExportTaskStatusMessage (..),

    -- ** ExportDestinationBucket
    ExportDestinationBucket (..),

    -- ** QueryStatus
    QueryStatus (..),

    -- ** InputLogEvent
    InputLogEvent (..),
    mkInputLogEvent,
    ileTimestamp,
    ileMessage,

    -- ** TagValue
    TagValue (..),

    -- ** FilteredLogEvent
    FilteredLogEvent (..),
    mkFilteredLogEvent,
    fleEventId,
    fleIngestionTime,
    fleLogStreamName,
    fleMessage,
    fleTimestamp,

    -- ** DestinationArn
    DestinationArn (..),

    -- ** LogGroupName
    LogGroupName (..),

    -- ** QueryInfo
    QueryInfo (..),
    mkQueryInfo,
    qiCreateTime,
    qiLogGroupName,
    qiQueryId,
    qiQueryString,
    qiStatus,

    -- ** NextToken
    NextToken (..),

    -- ** AccessPolicy
    AccessPolicy (..),

    -- ** LogStreamName
    LogStreamName (..),

    -- ** MetricNamespace
    MetricNamespace (..),

    -- ** KmsKeyId
    KmsKeyId (..),

    -- ** MetricValue
    MetricValue (..),

    -- ** EventMessage
    EventMessage (..),

    -- ** QueryString
    QueryString (..),

    -- ** OutputLogEvent
    OutputLogEvent (..),
    mkOutputLogEvent,
    oleIngestionTime,
    oleMessage,
    oleTimestamp,

    -- ** QueryDefinition
    QueryDefinition (..),
    mkQueryDefinition,
    qdLastModified,
    qdLogGroupNames,
    qdName,
    qdQueryDefinitionId,
    qdQueryString,

    -- ** QueryDefinitionString
    QueryDefinitionString (..),

    -- ** ExportTaskStatus
    ExportTaskStatus (..),
    mkExportTaskStatus,
    etsCode,
    etsMessage,

    -- ** ExportTaskExecutionInfo
    ExportTaskExecutionInfo (..),
    mkExportTaskExecutionInfo,
    eteiCompletionTime,
    eteiCreationTime,

    -- ** TagKey
    TagKey (..),

    -- ** FilterPattern
    FilterPattern (..),

    -- ** DestinationName
    DestinationName (..),

    -- ** Message
    Message (..),

    -- ** ResultField
    ResultField (..),
    mkResultField,
    rfField,
    rfValue,

    -- ** ExportTaskName
    ExportTaskName (..),

    -- ** ExportTask
    ExportTask (..),
    mkExportTask,
    etDestination,
    etDestinationPrefix,
    etExecutionInfo,
    etFrom,
    etLogGroupName,
    etStatus,
    etTaskId,
    etTaskName,
    etTo,

    -- ** EventId
    EventId (..),

    -- ** LogGroupField
    LogGroupField (..),
    mkLogGroupField,
    lgfName,
    lgfPercent,

    -- ** RoleArn
    RoleArn (..),

    -- ** DestinationPrefix
    DestinationPrefix (..),

    -- ** LogStreamNamePrefix
    LogStreamNamePrefix (..),

    -- ** TaskName
    TaskName (..),

    -- ** FilterNamePrefix
    FilterNamePrefix (..),

    -- ** LogGroupNamePrefix
    LogGroupNamePrefix (..),

    -- ** Name
    Name (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import qualified Network.AWS.Prelude as Lude

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

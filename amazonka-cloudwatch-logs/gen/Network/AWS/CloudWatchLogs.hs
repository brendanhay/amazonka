{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use Amazon CloudWatch Logs to monitor, store, and access your log files from Amazon EC2 instances, AWS CloudTrail, or other sources. You can then retrieve the associated log data from CloudWatch Logs using the CloudWatch console, CloudWatch Logs commands in the AWS CLI, CloudWatch Logs API, or CloudWatch Logs SDK.
--
--
-- You can use CloudWatch Logs to:
--
--     * __Monitor logs from EC2 instances in real-time__ : You can use CloudWatch Logs to monitor applications and systems using log data. For example, CloudWatch Logs can track the number of errors that occur in your application logs and send you a notification whenever the rate of errors exceeds a threshold that you specify. CloudWatch Logs uses your log data for monitoring; so, no code changes are required. For example, you can monitor application logs for specific literal terms (such as "NullReferenceException") or count the number of occurrences of a literal term at a particular position in log data (such as "404" status codes in an Apache access log). When the term you are searching for is found, CloudWatch Logs reports the data to a CloudWatch metric that you specify.
--
--     * __Monitor AWS CloudTrail logged events__ : You can create alarms in CloudWatch and receive notifications of particular API activity as captured by CloudTrail and use the notification to perform troubleshooting.
--
--     * __Archive log data__ : You can use CloudWatch Logs to store your log data in highly durable storage. You can change the log retention setting so that any log events older than this setting are automatically deleted. The CloudWatch Logs agent makes it easy to quickly send both rotated and non-rotated log data off of a host and into the log service. You can then access the raw log data when you need it.
--
--
--
module Network.AWS.CloudWatchLogs
    (
    -- * Service Configuration
      cloudWatchLogs

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** InvalidSequenceTokenException
    , _InvalidSequenceTokenException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** OperationAbortedException
    , _OperationAbortedException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** DataAlreadyAcceptedException
    , _DataAlreadyAcceptedException

    -- ** InvalidOperationException
    , _InvalidOperationException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeDestinations (Paginated)
    , module Network.AWS.CloudWatchLogs.DescribeDestinations

    -- ** UntagLogGroup
    , module Network.AWS.CloudWatchLogs.UntagLogGroup

    -- ** CreateExportTask
    , module Network.AWS.CloudWatchLogs.CreateExportTask

    -- ** PutDestination
    , module Network.AWS.CloudWatchLogs.PutDestination

    -- ** DescribeSubscriptionFilters (Paginated)
    , module Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters

    -- ** GetLogEvents
    , module Network.AWS.CloudWatchLogs.GetLogEvents

    -- ** DescribeLogGroups (Paginated)
    , module Network.AWS.CloudWatchLogs.DescribeLogGroups

    -- ** DeleteDestination
    , module Network.AWS.CloudWatchLogs.DeleteDestination

    -- ** DisassociateKMSKey
    , module Network.AWS.CloudWatchLogs.DisassociateKMSKey

    -- ** FilterLogEvents (Paginated)
    , module Network.AWS.CloudWatchLogs.FilterLogEvents

    -- ** TagLogGroup
    , module Network.AWS.CloudWatchLogs.TagLogGroup

    -- ** DescribeResourcePolicies
    , module Network.AWS.CloudWatchLogs.DescribeResourcePolicies

    -- ** DeleteLogStream
    , module Network.AWS.CloudWatchLogs.DeleteLogStream

    -- ** CreateLogStream
    , module Network.AWS.CloudWatchLogs.CreateLogStream

    -- ** CreateLogGroup
    , module Network.AWS.CloudWatchLogs.CreateLogGroup

    -- ** DescribeExportTasks
    , module Network.AWS.CloudWatchLogs.DescribeExportTasks

    -- ** CancelExportTask
    , module Network.AWS.CloudWatchLogs.CancelExportTask

    -- ** PutSubscriptionFilter
    , module Network.AWS.CloudWatchLogs.PutSubscriptionFilter

    -- ** DeleteLogGroup
    , module Network.AWS.CloudWatchLogs.DeleteLogGroup

    -- ** DeleteSubscriptionFilter
    , module Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter

    -- ** PutLogEvents
    , module Network.AWS.CloudWatchLogs.PutLogEvents

    -- ** DescribeMetricFilters (Paginated)
    , module Network.AWS.CloudWatchLogs.DescribeMetricFilters

    -- ** TestMetricFilter
    , module Network.AWS.CloudWatchLogs.TestMetricFilter

    -- ** PutDestinationPolicy
    , module Network.AWS.CloudWatchLogs.PutDestinationPolicy

    -- ** PutMetricFilter
    , module Network.AWS.CloudWatchLogs.PutMetricFilter

    -- ** DeleteRetentionPolicy
    , module Network.AWS.CloudWatchLogs.DeleteRetentionPolicy

    -- ** DeleteMetricFilter
    , module Network.AWS.CloudWatchLogs.DeleteMetricFilter

    -- ** PutRetentionPolicy
    , module Network.AWS.CloudWatchLogs.PutRetentionPolicy

    -- ** ListTagsLogGroup
    , module Network.AWS.CloudWatchLogs.ListTagsLogGroup

    -- ** PutResourcePolicy
    , module Network.AWS.CloudWatchLogs.PutResourcePolicy

    -- ** DeleteResourcePolicy
    , module Network.AWS.CloudWatchLogs.DeleteResourcePolicy

    -- ** AssociateKMSKey
    , module Network.AWS.CloudWatchLogs.AssociateKMSKey

    -- ** DescribeLogStreams (Paginated)
    , module Network.AWS.CloudWatchLogs.DescribeLogStreams

    -- * Types

    -- ** Distribution
    , Distribution (..)

    -- ** ExportTaskStatusCode
    , ExportTaskStatusCode (..)

    -- ** OrderBy
    , OrderBy (..)

    -- ** Destination
    , Destination
    , destination
    , dTargetARN
    , dCreationTime
    , dArn
    , dAccessPolicy
    , dDestinationName
    , dRoleARN

    -- ** ExportTask
    , ExportTask
    , exportTask
    , etDestinationPrefix
    , etDestination
    , etStatus
    , etTaskName
    , etTaskId
    , etTo
    , etFrom
    , etLogGroupName
    , etExecutionInfo

    -- ** ExportTaskExecutionInfo
    , ExportTaskExecutionInfo
    , exportTaskExecutionInfo
    , eteiCreationTime
    , eteiCompletionTime

    -- ** ExportTaskStatus
    , ExportTaskStatus
    , exportTaskStatus
    , etsCode
    , etsMessage

    -- ** FilteredLogEvent
    , FilteredLogEvent
    , filteredLogEvent
    , fleIngestionTime
    , fleLogStreamName
    , fleMessage
    , fleTimestamp
    , fleEventId

    -- ** InputLogEvent
    , InputLogEvent
    , inputLogEvent
    , ileTimestamp
    , ileMessage

    -- ** LogGroup
    , LogGroup
    , logGroup
    , lgCreationTime
    , lgMetricFilterCount
    , lgArn
    , lgLogGroupName
    , lgRetentionInDays
    , lgKmsKeyId
    , lgStoredBytes

    -- ** LogStream
    , LogStream
    , logStream
    , lsCreationTime
    , lsUploadSequenceToken
    , lsArn
    , lsFirstEventTimestamp
    , lsLogStreamName
    , lsStoredBytes
    , lsLastIngestionTime
    , lsLastEventTimestamp

    -- ** MetricFilter
    , MetricFilter
    , metricFilter
    , mfCreationTime
    , mfFilterName
    , mfLogGroupName
    , mfFilterPattern
    , mfMetricTransformations

    -- ** MetricFilterMatchRecord
    , MetricFilterMatchRecord
    , metricFilterMatchRecord
    , mfmrExtractedValues
    , mfmrEventNumber
    , mfmrEventMessage

    -- ** MetricTransformation
    , MetricTransformation
    , metricTransformation
    , mtDefaultValue
    , mtMetricName
    , mtMetricNamespace
    , mtMetricValue

    -- ** OutputLogEvent
    , OutputLogEvent
    , outputLogEvent
    , oleIngestionTime
    , oleMessage
    , oleTimestamp

    -- ** RejectedLogEventsInfo
    , RejectedLogEventsInfo
    , rejectedLogEventsInfo
    , rleiTooOldLogEventEndIndex
    , rleiTooNewLogEventStartIndex
    , rleiExpiredLogEventEndIndex

    -- ** ResourcePolicy
    , ResourcePolicy
    , resourcePolicy
    , rpPolicyName
    , rpPolicyDocument
    , rpLastUpdatedTime

    -- ** SearchedLogStream
    , SearchedLogStream
    , searchedLogStream
    , slsLogStreamName
    , slsSearchedCompletely

    -- ** SubscriptionFilter
    , SubscriptionFilter
    , subscriptionFilter
    , sfCreationTime
    , sfFilterName
    , sfDistribution
    , sfDestinationARN
    , sfLogGroupName
    , sfFilterPattern
    , sfRoleARN
    ) where

import Network.AWS.CloudWatchLogs.AssociateKMSKey
import Network.AWS.CloudWatchLogs.CancelExportTask
import Network.AWS.CloudWatchLogs.CreateExportTask
import Network.AWS.CloudWatchLogs.CreateLogGroup
import Network.AWS.CloudWatchLogs.CreateLogStream
import Network.AWS.CloudWatchLogs.DeleteDestination
import Network.AWS.CloudWatchLogs.DeleteLogGroup
import Network.AWS.CloudWatchLogs.DeleteLogStream
import Network.AWS.CloudWatchLogs.DeleteMetricFilter
import Network.AWS.CloudWatchLogs.DeleteResourcePolicy
import Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
import Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
import Network.AWS.CloudWatchLogs.DescribeDestinations
import Network.AWS.CloudWatchLogs.DescribeExportTasks
import Network.AWS.CloudWatchLogs.DescribeLogGroups
import Network.AWS.CloudWatchLogs.DescribeLogStreams
import Network.AWS.CloudWatchLogs.DescribeMetricFilters
import Network.AWS.CloudWatchLogs.DescribeResourcePolicies
import Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
import Network.AWS.CloudWatchLogs.DisassociateKMSKey
import Network.AWS.CloudWatchLogs.FilterLogEvents
import Network.AWS.CloudWatchLogs.GetLogEvents
import Network.AWS.CloudWatchLogs.ListTagsLogGroup
import Network.AWS.CloudWatchLogs.PutDestination
import Network.AWS.CloudWatchLogs.PutDestinationPolicy
import Network.AWS.CloudWatchLogs.PutLogEvents
import Network.AWS.CloudWatchLogs.PutMetricFilter
import Network.AWS.CloudWatchLogs.PutResourcePolicy
import Network.AWS.CloudWatchLogs.PutRetentionPolicy
import Network.AWS.CloudWatchLogs.PutSubscriptionFilter
import Network.AWS.CloudWatchLogs.TagLogGroup
import Network.AWS.CloudWatchLogs.TestMetricFilter
import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.UntagLogGroup
import Network.AWS.CloudWatchLogs.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CloudWatchLogs'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

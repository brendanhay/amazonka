{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon CloudWatch Logs API Reference
--
-- This is the /Amazon CloudWatch Logs API Reference/. Amazon CloudWatch
-- Logs enables you to monitor, store, and access your system, application,
-- and custom log files. This guide provides detailed information about
-- Amazon CloudWatch Logs actions, data types, parameters, and errors. For
-- detailed information about Amazon CloudWatch Logs features and their
-- associated API calls, go to the
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide Amazon CloudWatch Developer Guide>.
--
-- Use the following links to get started using the /Amazon CloudWatch Logs
-- API Reference/:
--
-- -   <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_Operations.html Actions>:
--     An alphabetical list of all Amazon CloudWatch Logs actions.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_Types.html Data Types>:
--     An alphabetical list of all Amazon CloudWatch Logs data types.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/CommonParameters.html Common Parameters>:
--     Parameters that all Query actions can use.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/CommonErrors.html Common Errors>:
--     Client and server errors that all actions can return.
-- -   <http://docs.aws.amazon.com/general/latest/gr/index.html?rande.html Regions and Endpoints>:
--     Itemized regions and endpoints for all AWS products.
--
-- In addition to using the Amazon CloudWatch Logs API, you can also use
-- the following SDKs and third-party libraries to access Amazon CloudWatch
-- Logs programmatically.
--
-- -   <http://aws.amazon.com/documentation/sdkforjava/ AWS SDK for Java Documentation>
-- -   <http://aws.amazon.com/documentation/sdkfornet/ AWS SDK for .NET Documentation>
-- -   <http://aws.amazon.com/documentation/sdkforphp/ AWS SDK for PHP Documentation>
-- -   <http://aws.amazon.com/documentation/sdkforruby/ AWS SDK for Ruby Documentation>
--
-- Developers in the AWS developer community also provide their own
-- libraries, which you can find at the following AWS developer centers:
--
-- -   <http://aws.amazon.com/java/ AWS Java Developer Center>
-- -   <http://aws.amazon.com/php/ AWS PHP Developer Center>
-- -   <http://aws.amazon.com/python/ AWS Python Developer Center>
-- -   <http://aws.amazon.com/ruby/ AWS Ruby Developer Center>
-- -   <http://aws.amazon.com/net/ AWS Windows and .NET Developer Center>
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/Welcome.html AWS API Reference>
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

    -- ** FilterLogEvents (Paginated)
    , module Network.AWS.CloudWatchLogs.FilterLogEvents

    -- ** DeleteLogStream
    , module Network.AWS.CloudWatchLogs.DeleteLogStream

    -- ** CreateLogStream
    , module Network.AWS.CloudWatchLogs.CreateLogStream

    -- ** CreateLogGroup
    , module Network.AWS.CloudWatchLogs.CreateLogGroup

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

    -- ** DescribeLogStreams (Paginated)
    , module Network.AWS.CloudWatchLogs.DescribeLogStreams

    -- * Types

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
    , sfDestinationARN
    , sfLogGroupName
    , sfFilterPattern
    , sfRoleARN
    ) where

import           Network.AWS.CloudWatchLogs.CreateLogGroup
import           Network.AWS.CloudWatchLogs.CreateLogStream
import           Network.AWS.CloudWatchLogs.DeleteDestination
import           Network.AWS.CloudWatchLogs.DeleteLogGroup
import           Network.AWS.CloudWatchLogs.DeleteLogStream
import           Network.AWS.CloudWatchLogs.DeleteMetricFilter
import           Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
import           Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
import           Network.AWS.CloudWatchLogs.DescribeDestinations
import           Network.AWS.CloudWatchLogs.DescribeLogGroups
import           Network.AWS.CloudWatchLogs.DescribeLogStreams
import           Network.AWS.CloudWatchLogs.DescribeMetricFilters
import           Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
import           Network.AWS.CloudWatchLogs.FilterLogEvents
import           Network.AWS.CloudWatchLogs.GetLogEvents
import           Network.AWS.CloudWatchLogs.PutDestination
import           Network.AWS.CloudWatchLogs.PutDestinationPolicy
import           Network.AWS.CloudWatchLogs.PutLogEvents
import           Network.AWS.CloudWatchLogs.PutMetricFilter
import           Network.AWS.CloudWatchLogs.PutRetentionPolicy
import           Network.AWS.CloudWatchLogs.PutSubscriptionFilter
import           Network.AWS.CloudWatchLogs.TestMetricFilter
import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Waiters

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

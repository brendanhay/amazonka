{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudWatch Logs enables you to monitor, store, and access your
-- system, application, and custom log files.
--
-- The 'State' operator variants from "Control.Lens.Setter" such as '.='
-- can be used to modify any additional request parameters before sending.
module Network.AWS.CloudWatchLogs.V2014_03_28.Monadic
    (
    -- * CreateLogGroup
    -- $CreateLogGroup
      createLogGroup
    , createLogGroupCatch

    -- * CreateLogStream
    -- $CreateLogStream
    , createLogStream
    , createLogStreamCatch

    -- * DeleteLogGroup
    -- $DeleteLogGroup
    , deleteLogGroup
    , deleteLogGroupCatch

    -- * DeleteLogStream
    -- $DeleteLogStream
    , deleteLogStream
    , deleteLogStreamCatch

    -- * DeleteMetricFilter
    -- $DeleteMetricFilter
    , deleteMetricFilter
    , deleteMetricFilterCatch

    -- * DeleteRetentionPolicy
    -- $DeleteRetentionPolicy
    , deleteRetentionPolicy
    , deleteRetentionPolicyCatch

    -- * DescribeLogGroups
    -- $DescribeLogGroups
    , describeLogGroups
    , describeLogGroupsCatch

    -- * DescribeLogStreams
    -- $DescribeLogStreams
    , describeLogStreams
    , describeLogStreamsCatch

    -- * DescribeMetricFilters
    -- $DescribeMetricFilters
    , describeMetricFilters
    , describeMetricFiltersCatch

    -- * GetLogEvents
    -- $GetLogEvents
    , getLogEvents
    , getLogEventsCatch

    -- * PutLogEvents
    -- $PutLogEvents
    , putLogEvents
    , putLogEventsCatch

    -- * PutMetricFilter
    -- $PutMetricFilter
    , putMetricFilter
    , putMetricFilterCatch

    -- * PutRetentionPolicy
    -- $PutRetentionPolicy
    , putRetentionPolicy
    , putRetentionPolicyCatch

    -- * TestMetricFilter
    -- $TestMetricFilter
    , testMetricFilter
    , testMetricFilterCatch

    -- * Re-exported
    , module AWS
    , module Network.AWS.CloudWatchLogs.V2014_03_28
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.CloudWatchLogs.V2014_03_28

type ServiceErr = CloudWatchLogsError


-- $CreateLogGroup
-- Creates a new log group with the specified name. The name of the log group
-- must be unique within a region for an AWS account. You can create up to 500
-- log groups per account. You must use the following guidelines when naming a
-- log group: Log group names can be between 1 and 512 characters long.
-- Allowed characters are a–z, A–Z, 0–9, '_' (underscore), '-' (hyphen), '/'
-- (forward slash), and '.' (period). Create a new Log Group The following is
-- an example of a CreateLogGroup request and response. POST / HTTP/1.1 Host:
-- logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.CreateLogGroup { "logGroupName":
-- "exampleLogGroupName" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]>.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.CreateLogGroup'

createLogGroup :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'clgLogGroupName'
    -> State CreateLogGroup a
    -> m CreateLogGroupResponse
createLogGroup p1 s =
    send $ (mkCreateLogGroup p1) &~ s

createLogGroupCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'clgLogGroupName'
    -> State CreateLogGroup a
    -> m (Either ServiceErr CreateLogGroupResponse)
createLogGroupCatch p1 s =
    sendCatch $ (mkCreateLogGroup p1) &~ s

-- $CreateLogStream
-- Creates a new log stream in the specified log group. The name of the log
-- stream must be unique within the log group. There is no limit on the number
-- of log streams that can exist in a log group. You must use the following
-- guidelines when naming a log stream: Log stream names can be between 1 and
-- 512 characters long. The ':' colon character is not allowed. Create a new
-- Log Stream The following is an example of a CreateLogStream request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.CreateLogStream { "logGroupName":
-- "exampleLogGroupName", "logStreamName": "exampleLogStreamName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.CreateLogStream'

createLogStream :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'clsLogGroupName'
    -> Text -- ^ 'clsLogStreamName'
    -> State CreateLogStream a
    -> m CreateLogStreamResponse
createLogStream p1 p2 s =
    send $ (mkCreateLogStream p1 p2) &~ s

createLogStreamCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'clsLogGroupName'
    -> Text -- ^ 'clsLogStreamName'
    -> State CreateLogStream a
    -> m (Either ServiceErr CreateLogStreamResponse)
createLogStreamCatch p1 p2 s =
    sendCatch $ (mkCreateLogStream p1 p2) &~ s

-- $DeleteLogGroup
-- Deletes the log group with the specified name and permanently deletes all
-- the archived log events associated with it. Delete a Log Group The
-- following is an example of a DeleteLogGroup request and response. POST /
-- HTTP/1.1 Host: logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DeleteLogGroup { "logGroupName":
-- "exampleLogGroupName" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]>.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.DeleteLogGroup'

deleteLogGroup :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'dlgLogGroupName'
    -> State DeleteLogGroup a
    -> m DeleteLogGroupResponse
deleteLogGroup p1 s =
    send $ (mkDeleteLogGroup p1) &~ s

deleteLogGroupCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'dlgLogGroupName'
    -> State DeleteLogGroup a
    -> m (Either ServiceErr DeleteLogGroupResponse)
deleteLogGroupCatch p1 s =
    sendCatch $ (mkDeleteLogGroup p1) &~ s

-- $DeleteLogStream
-- Deletes a log stream and permanently deletes all the archived log events
-- associated with it. Delete a Log Stream The following is an example of a
-- DeleteLogStream request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DeleteLogStream { "logGroupName":
-- "exampleLogGroupName", "logStreamName": "exampleLogStreamName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.DeleteLogStream'

deleteLogStream :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'dlsLogGroupName'
    -> Text -- ^ 'dlsLogStreamName'
    -> State DeleteLogStream a
    -> m DeleteLogStreamResponse
deleteLogStream p1 p2 s =
    send $ (mkDeleteLogStream p1 p2) &~ s

deleteLogStreamCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'dlsLogGroupName'
    -> Text -- ^ 'dlsLogStreamName'
    -> State DeleteLogStream a
    -> m (Either ServiceErr DeleteLogStreamResponse)
deleteLogStreamCatch p1 p2 s =
    sendCatch $ (mkDeleteLogStream p1 p2) &~ s

-- $DeleteMetricFilter
-- Deletes a metric filter associated with the specified log group. Delete a
-- metric filter The following is an example of a DeleteMetricFilter request
-- and response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DeleteMetricFilter { "logGroupName":
-- "exampleLogGroupName", "filterName": "exampleMetricFilterName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.DeleteMetricFilter'

deleteMetricFilter :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'dmfLogGroupName'
    -> Text -- ^ 'dmfFilterName'
    -> State DeleteMetricFilter a
    -> m DeleteMetricFilterResponse
deleteMetricFilter p1 p2 s =
    send $ (mkDeleteMetricFilter p1 p2) &~ s

deleteMetricFilterCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'dmfLogGroupName'
    -> Text -- ^ 'dmfFilterName'
    -> State DeleteMetricFilter a
    -> m (Either ServiceErr DeleteMetricFilterResponse)
deleteMetricFilterCatch p1 p2 s =
    sendCatch $ (mkDeleteMetricFilter p1 p2) &~ s

-- $DeleteRetentionPolicy
-- Deletes the retention policy of the specified log group. Log events would
-- not expire if they belong to log groups without a retention policy. Deletes
-- the retention policy of a log group The following is an example of a
-- DeleteRetentionPolicy request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DeleteRetentionPolicy { "logGroupName":
-- "exampleLogGroupName" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]>.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.DeleteRetentionPolicy'

deleteRetentionPolicy :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'drpLogGroupName'
    -> State DeleteRetentionPolicy a
    -> m DeleteRetentionPolicyResponse
deleteRetentionPolicy p1 s =
    send $ (mkDeleteRetentionPolicy p1) &~ s

deleteRetentionPolicyCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'drpLogGroupName'
    -> State DeleteRetentionPolicy a
    -> m (Either ServiceErr DeleteRetentionPolicyResponse)
deleteRetentionPolicyCatch p1 s =
    sendCatch $ (mkDeleteRetentionPolicy p1) &~ s

-- $DescribeLogGroups
-- Returns all the log groups that are associated with the AWS account making
-- the request. The list returned in the response is ASCII-sorted by log group
-- name. By default, this operation returns up to 50 log groups. If there are
-- more log groups to list, the response would contain a nextToken value in
-- the response body. You can also limit the number of log groups returned in
-- the response by specifying the limit parameter in the request. List the log
-- groups for an AWS Account The following is an example of a
-- DescribeLogGroups request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DescribeLogGroups HTTP/1.1 200 OK
-- x-amzn-RequestId: Content-Type: application/x-amz-json-1.1 Content-Length:
-- Date: ]]> { "logGroups": [ { "storageBytes": 1048576, "arn":
-- "arn:aws:logs:us-east-1:123456789:log-group:exampleLogGroupName1:*",
-- "creationTime": 1393545600000, "logGroupName": "exampleLogGroupName1",
-- "metricFilterCount": 0, "retentionInDays": 14 }, { "storageBytes": 5242880,
-- "arn": "arn:aws:logs:us-east-1:123456789:log-group:exampleLogGroupName2:*",
-- "creationTime": 1396224000000, "logGroupName": "exampleLogGroupName2",
-- "metricFilterCount": 0, "retentionInDays": 30 } ] }.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.DescribeLogGroups'

describeLogGroups :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env (ResumableSource m)
                     )
    => State DescribeLogGroups a
    -> ResumableSource m DescribeLogGroupsResponse
describeLogGroups s =
    paginate (mkDescribeLogGroups &~ s)

describeLogGroupsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env (ResumableSource m)
                          )
    => State DescribeLogGroups a
    -> ResumableSource m (Either ServiceErr DescribeLogGroupsResponse)
describeLogGroupsCatch s =
    paginateCatch (mkDescribeLogGroups &~ s)

-- $DescribeLogStreams
-- Returns all the log streams that are associated with the specified log
-- group. The list returned in the response is ASCII-sorted by log stream
-- name. By default, this operation returns up to 50 log streams. If there are
-- more log streams to list, the response would contain a nextToken value in
-- the response body. You can also limit the number of log streams returned in
-- the response by specifying the limit parameter in the request. List the log
-- streams associated with a log group The following is an example of a
-- DescribeLogStreams request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DescribeLogStreams { "logGroupName":
-- "exampleLogGroupName" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "logStreams": [ {
-- "storageBytes": 1048576, "arn":
-- "arn:aws:logs:us-east-1:123456789:log-group:exampleLogGroupName1:log-stream:exampleLogStreamName1",
-- "creationTime": 1393545600000, "firstEventTimestamp": 1393545600000,
-- "lastEventTimestamp": 1393567800000, "lastIngestionTime": 1393589200000,
-- "logStreamName": "exampleLogStreamName1", "uploadSequenceToken":
-- "88602967394531410094953670125156212707622379445839968487" }, {
-- "storageBytes": 5242880, "arn":
-- "arn:aws:logs:us-east-1:123456789:log-group:exampleLogGroupName2:log-stream:exampleLogStreamName2",
-- "creationTime": 1396224000000, "firstEventTimestamp": 1396224000000,
-- "lastEventTimestamp": 1396235500000, "lastIngestionTime": 1396225560000,
-- "logStreamName": "exampleLogStreamName2", "uploadSequenceToken":
-- "07622379445839968487886029673945314100949536701251562127" } ] }.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.DescribeLogStreams'

describeLogStreams :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env (ResumableSource m)
                      )
    => Text -- ^ 'dls1LogGroupName'
    -> State DescribeLogStreams a
    -> ResumableSource m DescribeLogStreamsResponse
describeLogStreams p1 s =
    paginate $ (mkDescribeLogStreams p1) &~ s

describeLogStreamsCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env (ResumableSource m)
                           )
    => Text -- ^ 'dls1LogGroupName'
    -> State DescribeLogStreams a
    -> ResumableSource m (Either ServiceErr DescribeLogStreamsResponse)
describeLogStreamsCatch p1 s =
    paginateCatch $ (mkDescribeLogStreams p1) &~ s

-- $DescribeMetricFilters
-- Returns all the metrics filters associated with the specified log group.
-- The list returned in the response is ASCII-sorted by filter name. By
-- default, this operation returns up to 50 metric filters. If there are more
-- metric filters to list, the response would contain a nextToken value in the
-- response body. You can also limit the number of metric filters returned in
-- the response by specifying the limit parameter in the request. List the
-- metric filters associated with a log group The following is an example of a
-- DescribeMetricFilters request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DescribeMetricFilters { "logGroupName":
-- "exampleLogGroupName" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "metricFilters": [ {
-- "creationTime": 1396224000000, "filterName": "exampleFilterName",
-- "filterPattern": "[ip, identity, user_id, timestamp, request, status_code,
-- size]", "metricTransformations": [ { "metricValue": "$size",
-- "metricNamespace": "MyApp", "metricName": "Volume" }, { "metricValue": "1",
-- "metricNamespace": "MyApp", "metricName": "RequestCount" } ] } ] }.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.DescribeMetricFilters'

describeMetricFilters :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env (ResumableSource m)
                         )
    => Text -- ^ 'dmf1LogGroupName'
    -> State DescribeMetricFilters a
    -> ResumableSource m DescribeMetricFiltersResponse
describeMetricFilters p1 s =
    paginate $ (mkDescribeMetricFilters p1) &~ s

describeMetricFiltersCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env (ResumableSource m)
                              )
    => Text -- ^ 'dmf1LogGroupName'
    -> State DescribeMetricFilters a
    -> ResumableSource m (Either ServiceErr DescribeMetricFiltersResponse)
describeMetricFiltersCatch p1 s =
    paginateCatch $ (mkDescribeMetricFilters p1) &~ s

-- $GetLogEvents
-- Retrieves log events from the specified log stream. You can provide an
-- optional time range to filter the results on the event timestamp. By
-- default, this operation returns as much log events as can fit in a response
-- size of 1MB, up to 10,000 log events. The response will always include a
-- nextForwardToken and a nextBackwardToken in the response body. You can use
-- any of these tokens in subsequent GetLogEvents requests to paginate through
-- events in either forward or backward direction. You can also limit the
-- number of log events returned in the response by specifying the limit
-- parameter in the request. Retrieves all the events from a log stream The
-- following is an example of a GetLogEvents request and response. POST /
-- HTTP/1.1 Host: logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.GetLogEvents { "logGroupName":
-- "exampleLogGroupName", "logStreamName": "exampleLogStreamName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]> { "events": [ { "ingestionTime": 1396035394997,
-- "timestamp": 1396035378988, "message": "Example Event 1" }, {
-- "ingestionTime": 1396035394997, "timestamp": 1396035378988, "message":
-- "Example Event 2" }, { "ingestionTime": 1396035394997, "timestamp":
-- 1396035378989, "message": "Example Event 3" } ], "nextBackwardToken":
-- "b/31132629274945519779805322857203735586714454643391594505",
-- "nextForwardToken":
-- "f/31132629323784151764587387538205132201699397759403884544" }.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.GetLogEvents'

getLogEvents :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'gleLogGroupName'
    -> Text -- ^ 'gleLogStreamName'
    -> State GetLogEvents a
    -> m GetLogEventsResponse
getLogEvents p1 p2 s =
    send $ (mkGetLogEvents p1 p2) &~ s

getLogEventsCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'gleLogGroupName'
    -> Text -- ^ 'gleLogStreamName'
    -> State GetLogEvents a
    -> m (Either ServiceErr GetLogEventsResponse)
getLogEventsCatch p1 p2 s =
    sendCatch $ (mkGetLogEvents p1 p2) &~ s

-- $PutLogEvents
-- Uploads a batch of log events to the specified log stream. Every
-- PutLogEvents request must include the sequenceToken obtained from the
-- response of the previous request. An upload in a newly created log stream
-- does not require a sequenceToken. The batch of events must satisfy the
-- following constraints: The maximum batch size is 32,768 bytes, and this
-- size is calculated as the sum of all event messages in UTF-8, plus 26 bytes
-- for each log event. None of the log events in the batch can be more than 2
-- hours in the future. None of the log events in the batch can be older than
-- 14 days or the retention period of the log group. The log events in the
-- batch must be in chronological ordered by their timestamp. The maximum
-- number of log events in a batch is 1,000. Upload a batch of log events into
-- a log stream The following is an example of a PutLogEvents request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.PutLogEvents { "logGroupName":
-- "exampleLogGroupName", "logStreamName": "exampleLogStreamName",
-- "logEvents": [ { "timestamp": 1396035378988, "message": "Example Event 1"
-- }, { "timestamp": 1396035378988, "message": "Example Event 2" }, {
-- "timestamp": 1396035378989, "message": "Example Event 3" } ] } HTTP/1.1 200
-- OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]> { "nextSequenceToken":
-- "49536701251539826331025683274032969384950891766572122113" }.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.PutLogEvents'

putLogEvents :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'pleLogGroupName'
    -> Text -- ^ 'pleLogStreamName'
    -> List1 InputLogEvent -- ^ 'pleLogEvents'
    -> State PutLogEvents a
    -> m PutLogEventsResponse
putLogEvents p1 p2 p3 s =
    send $ (mkPutLogEvents p1 p2 p3) &~ s

putLogEventsCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'pleLogGroupName'
    -> Text -- ^ 'pleLogStreamName'
    -> List1 InputLogEvent -- ^ 'pleLogEvents'
    -> State PutLogEvents a
    -> m (Either ServiceErr PutLogEventsResponse)
putLogEventsCatch p1 p2 p3 s =
    sendCatch $ (mkPutLogEvents p1 p2 p3) &~ s

-- $PutMetricFilter
-- Creates or updates a metric filter and associates it with the specified log
-- group. Metric filters allow you to configure rules to extract metric data
-- from log events ingested through PutLogEvents requests. Create or update a
-- metric filter The following is an example of a PutMetricFilter request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.PutMetricFilter { "logGroupName":
-- "exampleLogGroupName", "filterName": "exampleMetricFilterName",
-- "filterPattern": "[ip, identity, user_id, timestamp, request, status_code,
-- size]", "metricTransformations": [ { "metricValue": "$size",
-- "metricNamespace": "MyApp", "metricName": "Volume" }, { "metricValue": "1",
-- "metricNamespace": "MyApp", "metricName": "RequestCount" } ] } HTTP/1.1 200
-- OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.PutMetricFilter'

putMetricFilter :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env m
                   )
    => Text -- ^ 'pmfLogGroupName'
    -> Text -- ^ 'pmfFilterName'
    -> Text -- ^ 'pmfFilterPattern'
    -> List1 MetricTransformation -- ^ 'pmfMetricTransformations'
    -> State PutMetricFilter a
    -> m PutMetricFilterResponse
putMetricFilter p1 p2 p3 p4 s =
    send $ (mkPutMetricFilter p1 p2 p3 p4) &~ s

putMetricFilterCatch :: ( MonadCatch m
                        , MonadResource m
                        , MonadReader Env m
                        )
    => Text -- ^ 'pmfLogGroupName'
    -> Text -- ^ 'pmfFilterName'
    -> Text -- ^ 'pmfFilterPattern'
    -> List1 MetricTransformation -- ^ 'pmfMetricTransformations'
    -> State PutMetricFilter a
    -> m (Either ServiceErr PutMetricFilterResponse)
putMetricFilterCatch p1 p2 p3 p4 s =
    sendCatch $ (mkPutMetricFilter p1 p2 p3 p4) &~ s

-- $PutRetentionPolicy
-- Sets the retention of the specified log group. A retention policy allows
-- you to configure the number of days you want to retain log events in the
-- specified log group. Creates or updates a 30 day retention policy for a log
-- group The following is an example of a PutRetentionPolicy request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.PutRetentionPolicy { "logGroupName":
-- "exampleLogGroupName", "retentionInDays": 30 } HTTP/1.1 200 OK
-- x-amzn-RequestId: Content-Type: application/x-amz-json-1.1 Content-Length:
-- Date: ]]>.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.PutRetentionPolicy'

putRetentionPolicy :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'prpLogGroupName'
    -> Integer -- ^ 'prpRetentionInDays'
    -> State PutRetentionPolicy a
    -> m PutRetentionPolicyResponse
putRetentionPolicy p1 p2 s =
    send $ (mkPutRetentionPolicy p1 p2) &~ s

putRetentionPolicyCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'prpLogGroupName'
    -> Integer -- ^ 'prpRetentionInDays'
    -> State PutRetentionPolicy a
    -> m (Either ServiceErr PutRetentionPolicyResponse)
putRetentionPolicyCatch p1 p2 s =
    sendCatch $ (mkPutRetentionPolicy p1 p2) &~ s

-- $TestMetricFilter
-- Tests the filter pattern of a metric filter against a sample of log event
-- messages. You can use this operation to validate the correctness of a
-- metric filter pattern. Test a metric filter pattern on Apache access.log
-- events The following is an example of a TestMetricFilter request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern": "[ip,
-- identity, user_id, timestamp, request, status_code, size]",
-- "logEventMessages": [ "127.0.0.1 - frank [10/Oct/2000:13:25:15 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 200 1534", "127.0.0.1 - frank
-- [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif HTTP/1.0\" 500 5324",
-- "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET /apache_pb.gif
-- HTTP/1.0\" 200 4355" ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 0, "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:25:15
-- -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534", "extractedValues": {
-- "$status_code": "200", "$identity": "-", "$request": "GET /apache_pb.gif
-- HTTP/1.0", "$size": "1534,", "$user_id": "frank", "$ip": "127.0.0.1",
-- "$timestamp": "10/Oct/2000:13:25:15 -0700" } }, { "eventNumber": 1,
-- "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:35:22 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 500 5324", "extractedValues": { "$status_code":
-- "500", "$identity": "-", "$request": "GET /apache_pb.gif HTTP/1.0",
-- "$size": "5324,", "$user_id": "frank", "$ip": "127.0.0.1", "$timestamp":
-- "10/Oct/2000:13:35:22 -0700" } }, { "eventNumber": 2, "eventMessage":
-- "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET /apache_pb.gif
-- HTTP/1.0\" 200 4355", "extractedValues": { "$status_code": "200",
-- "$identity": "-", "$request": "GET /apache_pb.gif HTTP/1.0", "$size":
-- "4355", "$user_id": "frank", "$ip": "127.0.0.1", "$timestamp":
-- "10/Oct/2000:13:50:35 -0700" } } ] } Test a metric filter pattern on Apache
-- access.log events without specifying all the fields The following is an
-- example of a TestMetricFilter request and response. POST / HTTP/1.1 Host:
-- logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern": "[...,
-- size]", "logEventMessages": [ "127.0.0.1 - frank [10/Oct/2000:13:25:15
-- -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534", "127.0.0.1 - frank
-- [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif HTTP/1.0\" 500 5324",
-- "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET /apache_pb.gif
-- HTTP/1.0\" 200 4355" ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 0, "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:25:15
-- -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534", "extractedValues": {
-- "$size": "1534", "$6": "200", "$4": "10/Oct/2000:13:25:15 -0700", "$5":
-- "GET /apache_pb.gif HTTP/1.0", "$2": "-", "$3": "frank", "$1": "127.0.0.1"
-- } }, { "eventNumber": 1, "eventMessage": "127.0.0.1 - frank
-- [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif HTTP/1.0\" 500 5324",
-- "extractedValues": { "$size": "5324", "$6": "500", "$4":
-- "10/Oct/2000:13:35:22 -0700", "$5": "GET /apache_pb.gif HTTP/1.0", "$2":
-- "-", "$3": "frank", "$1": "127.0.0.1" } }, { "eventNumber": 2,
-- "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 200 4355", "extractedValues": { "$size": "4355",
-- "$6": "200", "$4": "10/Oct/2000:13:50:35 -0700", "$5": "GET /apache_pb.gif
-- HTTP/1.0", "$2": "-", "$3": "frank", "$1": "127.0.0.1" } } ] } Test a
-- metric filter pattern on Apache access.log events without specifying any
-- fields The following is an example of a TestMetricFilter request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern": "[]",
-- "logEventMessages": [ "127.0.0.1 - frank [10/Oct/2000:13:25:15 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 200 1534", "127.0.0.1 - frank
-- [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif HTTP/1.0\" 500 5324",
-- "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET /apache_pb.gif
-- HTTP/1.0\" 200 4355" ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 0, "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:25:15
-- -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534", "extractedValues": {
-- "$7": "1534", "$6": "200", "$4": "10/Oct/2000:13:25:15 -0700", "$5": "GET
-- /apache_pb.gif HTTP/1.0", "$2": "-", "$3": "frank", "$1": "127.0.0.1" } },
-- { "eventNumber": 1, "eventMessage": "127.0.0.1 - frank
-- [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif HTTP/1.0\" 500 5324",
-- "extractedValues": { "$7": "5324", "$6": "500", "$4": "10/Oct/2000:13:35:22
-- -0700", "$5": "GET /apache_pb.gif HTTP/1.0", "$2": "-", "$3": "frank",
-- "$1": "127.0.0.1" } }, { "eventNumber": 2, "eventMessage": "127.0.0.1 -
-- frank [10/Oct/2000:13:50:35 -0700] \"GET /apache_pb.gif HTTP/1.0\" 200
-- 4355", "extractedValues": { "$7": "4355", "$6": "200", "$4":
-- "10/Oct/2000:13:50:35 -0700", "$5": "GET /apache_pb.gif HTTP/1.0", "$2":
-- "-", "$3": "frank", "$1": "127.0.0.1" } } ] } Test a metric filter pattern
-- that matches successful requests in Apache access.log events The following
-- is an example of a TestMetricFilter request and response. POST / HTTP/1.1
-- Host: logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern": "[...,
-- status_code=200, size]", "logEventMessages": [ "127.0.0.1 - frank
-- [10/Oct/2000:13:25:15 -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534",
-- "127.0.0.1 - frank [10/Oct/2000:13:35:22 -0700] \"GET /apache_pb.gif
-- HTTP/1.0\" 500 5324", "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 200 4355" ] } HTTP/1.1 200 OK x-amzn-RequestId:
-- Content-Type: application/x-amz-json-1.1 Content-Length: Date: ]]> {
-- "matches": [ { "eventNumber": 0, "eventMessage": "127.0.0.1 - frank
-- [10/Oct/2000:13:25:15 -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 1534",
-- "extractedValues": { "$status_code": "200", "$size": "1534", "$4":
-- "10/Oct/2000:13:25:15 -0700", "$5": "GET /apache_pb.gif HTTP/1.0", "$2":
-- "-", "$3": "frank", "$1": "127.0.0.1" } }, { "eventNumber": 2,
-- "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 200 4355", "extractedValues": { "$status_code":
-- "200", "$size": "4355", "$4": "10/Oct/2000:13:50:35 -0700", "$5": "GET
-- /apache_pb.gif HTTP/1.0", "$2": "-", "$3": "frank", "$1": "127.0.0.1" } } ]
-- } Test a metric filter pattern that matches 4XX response codes for html
-- pages in Apache access.log events The following is an example of a
-- TestMetricFilter request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern": "[...,
-- request=*.html*, status_code=4*,]", "logEventMessages": [ "127.0.0.1 -
-- frank [10/Oct/2000:13:25:15 -0700] \"GET /index.html HTTP/1.0\" 404 1534",
-- "127.0.0.1 - frank [10/Oct/2000:13:35:22 -0700] \"GET /about-us/index.html
-- HTTP/1.0\" 200 5324", "127.0.0.1 - frank [10/Oct/2000:13:50:35 -0700] \"GET
-- /apache_pb.gif HTTP/1.0\" 404 4355", "127.0.0.1 - frank
-- [10/Oct/2000:13:25:15 -0700] \"GET /products/index.html HTTP/1.0\" 400
-- 1534", ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 0, "eventMessage": "127.0.0.1 - frank [10/Oct/2000:13:25:15
-- -0700] \"GET /index.html HTTP/1.0\" 404 1534", "extractedValues": {
-- "$status_code": "404", "$request": "GET /index.html HTTP/1.0", "$7":
-- "1534", "$4": "10/Oct/2000:13:25:15 -0700", "$2": "-", "$3": "frank", "$1":
-- "127.0.0.1" } }, { "eventNumber": 3, "eventMessage": "127.0.0.1 - frank
-- [10/Oct/2000:13:25:15 -0700] \"GET /products/index.html HTTP/1.0\" 400
-- 1534", "extractedValues": { "$status_code": "400", "$request": "GET
-- /products/index.html HTTP/1.0", "$7": "1534", "$4": "10/Oct/2000:13:25:15
-- -0700", "$2": "-", "$3": "frank", "$1": "127.0.0.1" } } ] } Test a metric
-- filter pattern that matches occurrences of "[ERROR]" in log events The
-- following is an example of a TestMetricFilter request and response. POST /
-- HTTP/1.1 Host: logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern":
-- "\"[ERROR]\"", "logEventMessages": [ "02 May 2014 00:34:12,525 [INFO]
-- Starting the application", "02 May 2014 00:35:14,245 [DEBUG] Database
-- connection established", "02 May 2014 00:34:14,663 [INFO] Executing SQL
-- Query", "02 May 2014 00:34:16,142 [ERROR] Unhanded exception:
-- InvalidQueryException", "02 May 2014 00:34:16,224 [ERROR] Terminating the
-- application" ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 3, "eventMessage": "02 May 2014 00:34:16,142 [ERROR]
-- Unhanded exception: InvalidQueryException", "extractedValues": {} }, {
-- "eventNumber": 4, "eventMessage": "02 May 2014 00:34:16,224 [ERROR]
-- Terminating the application", "extractedValues": {} } ] } Test a metric
-- filter pattern that matches occurrences of "[ERROR]" and "Exception" in log
-- events The following is an example of a TestMetricFilter request and
-- response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.TestMetricFilter { "filterPattern":
-- "\"[ERROR]\" Exception", "logEventMessages": [ "02 May 2014 00:34:12,525
-- [INFO] Starting the application", "02 May 2014 00:35:14,245 [DEBUG]
-- Database connection established", "02 May 2014 00:34:14,663 [INFO]
-- Executing SQL Query", "02 May 2014 00:34:16,142 [ERROR] Unhanded exception:
-- InvalidQueryException", "02 May 2014 00:34:16,224 [ERROR] Terminating the
-- application" ] } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "matches": [ {
-- "eventNumber": 3, "eventMessage": "02 May 2014 00:34:16,142 [ERROR]
-- Unhanded exception: InvalidQueryException", "extractedValues": {} } ] }.
--
-- See: 'Network.AWS.CloudWatchLogs.V2014_03_28.TestMetricFilter'

testMetricFilter :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'tmfFilterPattern'
    -> List1 Text -- ^ 'tmfLogEventMessages'
    -> State TestMetricFilter a
    -> m TestMetricFilterResponse
testMetricFilter p1 p2 s =
    send $ (mkTestMetricFilter p1 p2) &~ s

testMetricFilterCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'tmfFilterPattern'
    -> List1 Text -- ^ 'tmfLogEventMessages'
    -> State TestMetricFilter a
    -> m (Either ServiceErr TestMetricFilterResponse)
testMetricFilterCatch p1 p2 s =
    sendCatch $ (mkTestMetricFilter p1 p2) &~ s

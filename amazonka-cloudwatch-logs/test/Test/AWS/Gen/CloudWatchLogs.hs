{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchLogs
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudWatchLogs where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudWatchLogs
import Test.AWS.CloudWatchLogs.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeDestinations $
--             describeDestinations
--
--         , testCreateExportTask $
--             createExportTask
--
--         , testPutDestination $
--             putDestination
--
--         , testDescribeSubscriptionFilters $
--             describeSubscriptionFilters
--
--         , testGetLogEvents $
--             getLogEvents
--
--         , testDescribeLogGroups $
--             describeLogGroups
--
--         , testDeleteDestination $
--             deleteDestination
--
--         , testFilterLogEvents $
--             filterLogEvents
--
--         , testDeleteLogStream $
--             deleteLogStream
--
--         , testCreateLogStream $
--             createLogStream
--
--         , testCreateLogGroup $
--             createLogGroup
--
--         , testDescribeExportTasks $
--             describeExportTasks
--
--         , testCancelExportTask $
--             cancelExportTask
--
--         , testPutSubscriptionFilter $
--             putSubscriptionFilter
--
--         , testDeleteLogGroup $
--             deleteLogGroup
--
--         , testDeleteSubscriptionFilter $
--             deleteSubscriptionFilter
--
--         , testPutLogEvents $
--             putLogEvents
--
--         , testDescribeMetricFilters $
--             describeMetricFilters
--
--         , testTestMetricFilter $
--             testMetricFilter
--
--         , testPutDestinationPolicy $
--             putDestinationPolicy
--
--         , testPutMetricFilter $
--             putMetricFilter
--
--         , testDeleteRetentionPolicy $
--             deleteRetentionPolicy
--
--         , testDeleteMetricFilter $
--             deleteMetricFilter
--
--         , testPutRetentionPolicy $
--             putRetentionPolicy
--
--         , testDescribeLogStreams $
--             describeLogStreams
--
--           ]

--     , testGroup "response"
--         [ testDescribeDestinationsResponse $
--             describeDestinationsResponse
--
--         , testCreateExportTaskResponse $
--             createExportTaskResponse
--
--         , testPutDestinationResponse $
--             putDestinationResponse
--
--         , testDescribeSubscriptionFiltersResponse $
--             describeSubscriptionFiltersResponse
--
--         , testGetLogEventsResponse $
--             getLogEventsResponse
--
--         , testDescribeLogGroupsResponse $
--             describeLogGroupsResponse
--
--         , testDeleteDestinationResponse $
--             deleteDestinationResponse
--
--         , testFilterLogEventsResponse $
--             filterLogEventsResponse
--
--         , testDeleteLogStreamResponse $
--             deleteLogStreamResponse
--
--         , testCreateLogStreamResponse $
--             createLogStreamResponse
--
--         , testCreateLogGroupResponse $
--             createLogGroupResponse
--
--         , testDescribeExportTasksResponse $
--             describeExportTasksResponse
--
--         , testCancelExportTaskResponse $
--             cancelExportTaskResponse
--
--         , testPutSubscriptionFilterResponse $
--             putSubscriptionFilterResponse
--
--         , testDeleteLogGroupResponse $
--             deleteLogGroupResponse
--
--         , testDeleteSubscriptionFilterResponse $
--             deleteSubscriptionFilterResponse
--
--         , testPutLogEventsResponse $
--             putLogEventsResponse
--
--         , testDescribeMetricFiltersResponse $
--             describeMetricFiltersResponse
--
--         , testTestMetricFilterResponse $
--             testMetricFilterResponse
--
--         , testPutDestinationPolicyResponse $
--             putDestinationPolicyResponse
--
--         , testPutMetricFilterResponse $
--             putMetricFilterResponse
--
--         , testDeleteRetentionPolicyResponse $
--             deleteRetentionPolicyResponse
--
--         , testDeleteMetricFilterResponse $
--             deleteMetricFilterResponse
--
--         , testPutRetentionPolicyResponse $
--             putRetentionPolicyResponse
--
--         , testDescribeLogStreamsResponse $
--             describeLogStreamsResponse
--
--           ]
--     ]

-- Requests

testDescribeDestinations :: DescribeDestinations -> TestTree
testDescribeDestinations = req
    "DescribeDestinations"
    "fixture/DescribeDestinations.yaml"

testCreateExportTask :: CreateExportTask -> TestTree
testCreateExportTask = req
    "CreateExportTask"
    "fixture/CreateExportTask.yaml"

testPutDestination :: PutDestination -> TestTree
testPutDestination = req
    "PutDestination"
    "fixture/PutDestination.yaml"

testDescribeSubscriptionFilters :: DescribeSubscriptionFilters -> TestTree
testDescribeSubscriptionFilters = req
    "DescribeSubscriptionFilters"
    "fixture/DescribeSubscriptionFilters.yaml"

testGetLogEvents :: GetLogEvents -> TestTree
testGetLogEvents = req
    "GetLogEvents"
    "fixture/GetLogEvents.yaml"

testDescribeLogGroups :: DescribeLogGroups -> TestTree
testDescribeLogGroups = req
    "DescribeLogGroups"
    "fixture/DescribeLogGroups.yaml"

testDeleteDestination :: DeleteDestination -> TestTree
testDeleteDestination = req
    "DeleteDestination"
    "fixture/DeleteDestination.yaml"

testFilterLogEvents :: FilterLogEvents -> TestTree
testFilterLogEvents = req
    "FilterLogEvents"
    "fixture/FilterLogEvents.yaml"

testDeleteLogStream :: DeleteLogStream -> TestTree
testDeleteLogStream = req
    "DeleteLogStream"
    "fixture/DeleteLogStream.yaml"

testCreateLogStream :: CreateLogStream -> TestTree
testCreateLogStream = req
    "CreateLogStream"
    "fixture/CreateLogStream.yaml"

testCreateLogGroup :: CreateLogGroup -> TestTree
testCreateLogGroup = req
    "CreateLogGroup"
    "fixture/CreateLogGroup.yaml"

testDescribeExportTasks :: DescribeExportTasks -> TestTree
testDescribeExportTasks = req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

testCancelExportTask :: CancelExportTask -> TestTree
testCancelExportTask = req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

testPutSubscriptionFilter :: PutSubscriptionFilter -> TestTree
testPutSubscriptionFilter = req
    "PutSubscriptionFilter"
    "fixture/PutSubscriptionFilter.yaml"

testDeleteLogGroup :: DeleteLogGroup -> TestTree
testDeleteLogGroup = req
    "DeleteLogGroup"
    "fixture/DeleteLogGroup.yaml"

testDeleteSubscriptionFilter :: DeleteSubscriptionFilter -> TestTree
testDeleteSubscriptionFilter = req
    "DeleteSubscriptionFilter"
    "fixture/DeleteSubscriptionFilter.yaml"

testPutLogEvents :: PutLogEvents -> TestTree
testPutLogEvents = req
    "PutLogEvents"
    "fixture/PutLogEvents.yaml"

testDescribeMetricFilters :: DescribeMetricFilters -> TestTree
testDescribeMetricFilters = req
    "DescribeMetricFilters"
    "fixture/DescribeMetricFilters.yaml"

testTestMetricFilter :: TestMetricFilter -> TestTree
testTestMetricFilter = req
    "TestMetricFilter"
    "fixture/TestMetricFilter.yaml"

testPutDestinationPolicy :: PutDestinationPolicy -> TestTree
testPutDestinationPolicy = req
    "PutDestinationPolicy"
    "fixture/PutDestinationPolicy.yaml"

testPutMetricFilter :: PutMetricFilter -> TestTree
testPutMetricFilter = req
    "PutMetricFilter"
    "fixture/PutMetricFilter.yaml"

testDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
testDeleteRetentionPolicy = req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy.yaml"

testDeleteMetricFilter :: DeleteMetricFilter -> TestTree
testDeleteMetricFilter = req
    "DeleteMetricFilter"
    "fixture/DeleteMetricFilter.yaml"

testPutRetentionPolicy :: PutRetentionPolicy -> TestTree
testPutRetentionPolicy = req
    "PutRetentionPolicy"
    "fixture/PutRetentionPolicy.yaml"

testDescribeLogStreams :: DescribeLogStreams -> TestTree
testDescribeLogStreams = req
    "DescribeLogStreams"
    "fixture/DescribeLogStreams.yaml"

-- Responses

testDescribeDestinationsResponse :: DescribeDestinationsResponse -> TestTree
testDescribeDestinationsResponse = res
    "DescribeDestinationsResponse"
    "fixture/DescribeDestinationsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeDestinations)

testCreateExportTaskResponse :: CreateExportTaskResponse -> TestTree
testCreateExportTaskResponse = res
    "CreateExportTaskResponse"
    "fixture/CreateExportTaskResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy CreateExportTask)

testPutDestinationResponse :: PutDestinationResponse -> TestTree
testPutDestinationResponse = res
    "PutDestinationResponse"
    "fixture/PutDestinationResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutDestination)

testDescribeSubscriptionFiltersResponse :: DescribeSubscriptionFiltersResponse -> TestTree
testDescribeSubscriptionFiltersResponse = res
    "DescribeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeSubscriptionFilters)

testGetLogEventsResponse :: GetLogEventsResponse -> TestTree
testGetLogEventsResponse = res
    "GetLogEventsResponse"
    "fixture/GetLogEventsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy GetLogEvents)

testDescribeLogGroupsResponse :: DescribeLogGroupsResponse -> TestTree
testDescribeLogGroupsResponse = res
    "DescribeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeLogGroups)

testDeleteDestinationResponse :: DeleteDestinationResponse -> TestTree
testDeleteDestinationResponse = res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteDestination)

testFilterLogEventsResponse :: FilterLogEventsResponse -> TestTree
testFilterLogEventsResponse = res
    "FilterLogEventsResponse"
    "fixture/FilterLogEventsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy FilterLogEvents)

testDeleteLogStreamResponse :: DeleteLogStreamResponse -> TestTree
testDeleteLogStreamResponse = res
    "DeleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteLogStream)

testCreateLogStreamResponse :: CreateLogStreamResponse -> TestTree
testCreateLogStreamResponse = res
    "CreateLogStreamResponse"
    "fixture/CreateLogStreamResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy CreateLogStream)

testCreateLogGroupResponse :: CreateLogGroupResponse -> TestTree
testCreateLogGroupResponse = res
    "CreateLogGroupResponse"
    "fixture/CreateLogGroupResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy CreateLogGroup)

testDescribeExportTasksResponse :: DescribeExportTasksResponse -> TestTree
testDescribeExportTasksResponse = res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeExportTasks)

testCancelExportTaskResponse :: CancelExportTaskResponse -> TestTree
testCancelExportTaskResponse = res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy CancelExportTask)

testPutSubscriptionFilterResponse :: PutSubscriptionFilterResponse -> TestTree
testPutSubscriptionFilterResponse = res
    "PutSubscriptionFilterResponse"
    "fixture/PutSubscriptionFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutSubscriptionFilter)

testDeleteLogGroupResponse :: DeleteLogGroupResponse -> TestTree
testDeleteLogGroupResponse = res
    "DeleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteLogGroup)

testDeleteSubscriptionFilterResponse :: DeleteSubscriptionFilterResponse -> TestTree
testDeleteSubscriptionFilterResponse = res
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteSubscriptionFilter)

testPutLogEventsResponse :: PutLogEventsResponse -> TestTree
testPutLogEventsResponse = res
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutLogEvents)

testDescribeMetricFiltersResponse :: DescribeMetricFiltersResponse -> TestTree
testDescribeMetricFiltersResponse = res
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeMetricFilters)

testTestMetricFilterResponse :: TestMetricFilterResponse -> TestTree
testTestMetricFilterResponse = res
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy TestMetricFilter)

testPutDestinationPolicyResponse :: PutDestinationPolicyResponse -> TestTree
testPutDestinationPolicyResponse = res
    "PutDestinationPolicyResponse"
    "fixture/PutDestinationPolicyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutDestinationPolicy)

testPutMetricFilterResponse :: PutMetricFilterResponse -> TestTree
testPutMetricFilterResponse = res
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutMetricFilter)

testDeleteRetentionPolicyResponse :: DeleteRetentionPolicyResponse -> TestTree
testDeleteRetentionPolicyResponse = res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteRetentionPolicy)

testDeleteMetricFilterResponse :: DeleteMetricFilterResponse -> TestTree
testDeleteMetricFilterResponse = res
    "DeleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteMetricFilter)

testPutRetentionPolicyResponse :: PutRetentionPolicyResponse -> TestTree
testPutRetentionPolicyResponse = res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutRetentionPolicy)

testDescribeLogStreamsResponse :: DescribeLogStreamsResponse -> TestTree
testDescribeLogStreamsResponse = res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeLogStreams)

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
--         [ requestDescribeDestinations $
--             describeDestinations
--
--         , requestCreateExportTask $
--             createExportTask
--
--         , requestPutDestination $
--             putDestination
--
--         , requestDescribeSubscriptionFilters $
--             describeSubscriptionFilters
--
--         , requestGetLogEvents $
--             getLogEvents
--
--         , requestDescribeLogGroups $
--             describeLogGroups
--
--         , requestDeleteDestination $
--             deleteDestination
--
--         , requestFilterLogEvents $
--             filterLogEvents
--
--         , requestDeleteLogStream $
--             deleteLogStream
--
--         , requestCreateLogStream $
--             createLogStream
--
--         , requestCreateLogGroup $
--             createLogGroup
--
--         , requestDescribeExportTasks $
--             describeExportTasks
--
--         , requestCancelExportTask $
--             cancelExportTask
--
--         , requestPutSubscriptionFilter $
--             putSubscriptionFilter
--
--         , requestDeleteLogGroup $
--             deleteLogGroup
--
--         , requestDeleteSubscriptionFilter $
--             deleteSubscriptionFilter
--
--         , requestPutLogEvents $
--             putLogEvents
--
--         , requestDescribeMetricFilters $
--             describeMetricFilters
--
--         , requestTestMetricFilter $
--             testMetricFilter
--
--         , requestPutDestinationPolicy $
--             putDestinationPolicy
--
--         , requestPutMetricFilter $
--             putMetricFilter
--
--         , requestDeleteRetentionPolicy $
--             deleteRetentionPolicy
--
--         , requestDeleteMetricFilter $
--             deleteMetricFilter
--
--         , requestPutRetentionPolicy $
--             putRetentionPolicy
--
--         , requestDescribeLogStreams $
--             describeLogStreams
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDestinations $
--             describeDestinationsResponse
--
--         , responseCreateExportTask $
--             createExportTaskResponse
--
--         , responsePutDestination $
--             putDestinationResponse
--
--         , responseDescribeSubscriptionFilters $
--             describeSubscriptionFiltersResponse
--
--         , responseGetLogEvents $
--             getLogEventsResponse
--
--         , responseDescribeLogGroups $
--             describeLogGroupsResponse
--
--         , responseDeleteDestination $
--             deleteDestinationResponse
--
--         , responseFilterLogEvents $
--             filterLogEventsResponse
--
--         , responseDeleteLogStream $
--             deleteLogStreamResponse
--
--         , responseCreateLogStream $
--             createLogStreamResponse
--
--         , responseCreateLogGroup $
--             createLogGroupResponse
--
--         , responseDescribeExportTasks $
--             describeExportTasksResponse
--
--         , responseCancelExportTask $
--             cancelExportTaskResponse
--
--         , responsePutSubscriptionFilter $
--             putSubscriptionFilterResponse
--
--         , responseDeleteLogGroup $
--             deleteLogGroupResponse
--
--         , responseDeleteSubscriptionFilter $
--             deleteSubscriptionFilterResponse
--
--         , responsePutLogEvents $
--             putLogEventsResponse
--
--         , responseDescribeMetricFilters $
--             describeMetricFiltersResponse
--
--         , responseTestMetricFilter $
--             testMetricFilterResponse
--
--         , responsePutDestinationPolicy $
--             putDestinationPolicyResponse
--
--         , responsePutMetricFilter $
--             putMetricFilterResponse
--
--         , responseDeleteRetentionPolicy $
--             deleteRetentionPolicyResponse
--
--         , responseDeleteMetricFilter $
--             deleteMetricFilterResponse
--
--         , responsePutRetentionPolicy $
--             putRetentionPolicyResponse
--
--         , responseDescribeLogStreams $
--             describeLogStreamsResponse
--
--           ]
--     ]

-- Requests

requestDescribeDestinations :: DescribeDestinations -> TestTree
requestDescribeDestinations = req
    "DescribeDestinations"
    "fixture/DescribeDestinations.yaml"

requestCreateExportTask :: CreateExportTask -> TestTree
requestCreateExportTask = req
    "CreateExportTask"
    "fixture/CreateExportTask.yaml"

requestPutDestination :: PutDestination -> TestTree
requestPutDestination = req
    "PutDestination"
    "fixture/PutDestination.yaml"

requestDescribeSubscriptionFilters :: DescribeSubscriptionFilters -> TestTree
requestDescribeSubscriptionFilters = req
    "DescribeSubscriptionFilters"
    "fixture/DescribeSubscriptionFilters.yaml"

requestGetLogEvents :: GetLogEvents -> TestTree
requestGetLogEvents = req
    "GetLogEvents"
    "fixture/GetLogEvents.yaml"

requestDescribeLogGroups :: DescribeLogGroups -> TestTree
requestDescribeLogGroups = req
    "DescribeLogGroups"
    "fixture/DescribeLogGroups.yaml"

requestDeleteDestination :: DeleteDestination -> TestTree
requestDeleteDestination = req
    "DeleteDestination"
    "fixture/DeleteDestination.yaml"

requestFilterLogEvents :: FilterLogEvents -> TestTree
requestFilterLogEvents = req
    "FilterLogEvents"
    "fixture/FilterLogEvents.yaml"

requestDeleteLogStream :: DeleteLogStream -> TestTree
requestDeleteLogStream = req
    "DeleteLogStream"
    "fixture/DeleteLogStream.yaml"

requestCreateLogStream :: CreateLogStream -> TestTree
requestCreateLogStream = req
    "CreateLogStream"
    "fixture/CreateLogStream.yaml"

requestCreateLogGroup :: CreateLogGroup -> TestTree
requestCreateLogGroup = req
    "CreateLogGroup"
    "fixture/CreateLogGroup.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks = req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask = req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestPutSubscriptionFilter :: PutSubscriptionFilter -> TestTree
requestPutSubscriptionFilter = req
    "PutSubscriptionFilter"
    "fixture/PutSubscriptionFilter.yaml"

requestDeleteLogGroup :: DeleteLogGroup -> TestTree
requestDeleteLogGroup = req
    "DeleteLogGroup"
    "fixture/DeleteLogGroup.yaml"

requestDeleteSubscriptionFilter :: DeleteSubscriptionFilter -> TestTree
requestDeleteSubscriptionFilter = req
    "DeleteSubscriptionFilter"
    "fixture/DeleteSubscriptionFilter.yaml"

requestPutLogEvents :: PutLogEvents -> TestTree
requestPutLogEvents = req
    "PutLogEvents"
    "fixture/PutLogEvents.yaml"

requestDescribeMetricFilters :: DescribeMetricFilters -> TestTree
requestDescribeMetricFilters = req
    "DescribeMetricFilters"
    "fixture/DescribeMetricFilters.yaml"

requestTestMetricFilter :: TestMetricFilter -> TestTree
requestTestMetricFilter = req
    "TestMetricFilter"
    "fixture/TestMetricFilter.yaml"

requestPutDestinationPolicy :: PutDestinationPolicy -> TestTree
requestPutDestinationPolicy = req
    "PutDestinationPolicy"
    "fixture/PutDestinationPolicy.yaml"

requestPutMetricFilter :: PutMetricFilter -> TestTree
requestPutMetricFilter = req
    "PutMetricFilter"
    "fixture/PutMetricFilter.yaml"

requestDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
requestDeleteRetentionPolicy = req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy.yaml"

requestDeleteMetricFilter :: DeleteMetricFilter -> TestTree
requestDeleteMetricFilter = req
    "DeleteMetricFilter"
    "fixture/DeleteMetricFilter.yaml"

requestPutRetentionPolicy :: PutRetentionPolicy -> TestTree
requestPutRetentionPolicy = req
    "PutRetentionPolicy"
    "fixture/PutRetentionPolicy.yaml"

requestDescribeLogStreams :: DescribeLogStreams -> TestTree
requestDescribeLogStreams = req
    "DescribeLogStreams"
    "fixture/DescribeLogStreams.yaml"

-- Responses

responseDescribeDestinations :: DescribeDestinationsResponse -> TestTree
responseDescribeDestinations = res
    "DescribeDestinationsResponse"
    "fixture/DescribeDestinationsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeDestinations)

responseCreateExportTask :: CreateExportTaskResponse -> TestTree
responseCreateExportTask = res
    "CreateExportTaskResponse"
    "fixture/CreateExportTaskResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy CreateExportTask)

responsePutDestination :: PutDestinationResponse -> TestTree
responsePutDestination = res
    "PutDestinationResponse"
    "fixture/PutDestinationResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutDestination)

responseDescribeSubscriptionFilters :: DescribeSubscriptionFiltersResponse -> TestTree
responseDescribeSubscriptionFilters = res
    "DescribeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeSubscriptionFilters)

responseGetLogEvents :: GetLogEventsResponse -> TestTree
responseGetLogEvents = res
    "GetLogEventsResponse"
    "fixture/GetLogEventsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy GetLogEvents)

responseDescribeLogGroups :: DescribeLogGroupsResponse -> TestTree
responseDescribeLogGroups = res
    "DescribeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeLogGroups)

responseDeleteDestination :: DeleteDestinationResponse -> TestTree
responseDeleteDestination = res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteDestination)

responseFilterLogEvents :: FilterLogEventsResponse -> TestTree
responseFilterLogEvents = res
    "FilterLogEventsResponse"
    "fixture/FilterLogEventsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy FilterLogEvents)

responseDeleteLogStream :: DeleteLogStreamResponse -> TestTree
responseDeleteLogStream = res
    "DeleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteLogStream)

responseCreateLogStream :: CreateLogStreamResponse -> TestTree
responseCreateLogStream = res
    "CreateLogStreamResponse"
    "fixture/CreateLogStreamResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy CreateLogStream)

responseCreateLogGroup :: CreateLogGroupResponse -> TestTree
responseCreateLogGroup = res
    "CreateLogGroupResponse"
    "fixture/CreateLogGroupResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy CreateLogGroup)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks = res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeExportTasks)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask = res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy CancelExportTask)

responsePutSubscriptionFilter :: PutSubscriptionFilterResponse -> TestTree
responsePutSubscriptionFilter = res
    "PutSubscriptionFilterResponse"
    "fixture/PutSubscriptionFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutSubscriptionFilter)

responseDeleteLogGroup :: DeleteLogGroupResponse -> TestTree
responseDeleteLogGroup = res
    "DeleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteLogGroup)

responseDeleteSubscriptionFilter :: DeleteSubscriptionFilterResponse -> TestTree
responseDeleteSubscriptionFilter = res
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteSubscriptionFilter)

responsePutLogEvents :: PutLogEventsResponse -> TestTree
responsePutLogEvents = res
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutLogEvents)

responseDescribeMetricFilters :: DescribeMetricFiltersResponse -> TestTree
responseDescribeMetricFilters = res
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeMetricFilters)

responseTestMetricFilter :: TestMetricFilterResponse -> TestTree
responseTestMetricFilter = res
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy TestMetricFilter)

responsePutDestinationPolicy :: PutDestinationPolicyResponse -> TestTree
responsePutDestinationPolicy = res
    "PutDestinationPolicyResponse"
    "fixture/PutDestinationPolicyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutDestinationPolicy)

responsePutMetricFilter :: PutMetricFilterResponse -> TestTree
responsePutMetricFilter = res
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutMetricFilter)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy = res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteRetentionPolicy)

responseDeleteMetricFilter :: DeleteMetricFilterResponse -> TestTree
responseDeleteMetricFilter = res
    "DeleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteMetricFilter)

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy = res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutRetentionPolicy)

responseDescribeLogStreams :: DescribeLogStreamsResponse -> TestTree
responseDescribeLogStreams = res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeLogStreams)

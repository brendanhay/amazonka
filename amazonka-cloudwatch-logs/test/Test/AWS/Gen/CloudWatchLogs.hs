{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchLogs
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         , testDeleteDestination $
--             deleteDestination
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
--         , testPutLogEvents $
--             putLogEvents
--
--         , testDeleteSubscriptionFilter $
--             deleteSubscriptionFilter
--
--         , testPutSubscriptionFilter $
--             putSubscriptionFilter
--
--         , testDeleteLogGroup $
--             deleteLogGroup
--
--         , testTestMetricFilter $
--             testMetricFilter
--
--         , testDescribeMetricFilters $
--             describeMetricFilters
--
--         , testDeleteMetricFilter $
--             deleteMetricFilter
--
--         , testPutRetentionPolicy $
--             putRetentionPolicy
--
--         , testDeleteRetentionPolicy $
--             deleteRetentionPolicy
--
--         , testPutMetricFilter $
--             putMetricFilter
--
--         , testPutDestinationPolicy $
--             putDestinationPolicy
--
--         , testDescribeLogStreams $
--             describeLogStreams
--
--           ]

--     , testGroup "response"
--         [ testDescribeDestinationsResponse $
--             describeDestinationsResponse
--
--         , testDeleteDestinationResponse $
--             deleteDestinationResponse
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
--         , testPutLogEventsResponse $
--             putLogEventsResponse
--
--         , testDeleteSubscriptionFilterResponse $
--             deleteSubscriptionFilterResponse
--
--         , testPutSubscriptionFilterResponse $
--             putSubscriptionFilterResponse
--
--         , testDeleteLogGroupResponse $
--             deleteLogGroupResponse
--
--         , testTestMetricFilterResponse $
--             testMetricFilterResponse
--
--         , testDescribeMetricFiltersResponse $
--             describeMetricFiltersResponse
--
--         , testDeleteMetricFilterResponse $
--             deleteMetricFilterResponse
--
--         , testPutRetentionPolicyResponse $
--             putRetentionPolicyResponse
--
--         , testDeleteRetentionPolicyResponse $
--             deleteRetentionPolicyResponse
--
--         , testPutMetricFilterResponse $
--             putMetricFilterResponse
--
--         , testPutDestinationPolicyResponse $
--             putDestinationPolicyResponse
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

testDeleteDestination :: DeleteDestination -> TestTree
testDeleteDestination = req
    "DeleteDestination"
    "fixture/DeleteDestination.yaml"

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

testPutLogEvents :: PutLogEvents -> TestTree
testPutLogEvents = req
    "PutLogEvents"
    "fixture/PutLogEvents.yaml"

testDeleteSubscriptionFilter :: DeleteSubscriptionFilter -> TestTree
testDeleteSubscriptionFilter = req
    "DeleteSubscriptionFilter"
    "fixture/DeleteSubscriptionFilter.yaml"

testPutSubscriptionFilter :: PutSubscriptionFilter -> TestTree
testPutSubscriptionFilter = req
    "PutSubscriptionFilter"
    "fixture/PutSubscriptionFilter.yaml"

testDeleteLogGroup :: DeleteLogGroup -> TestTree
testDeleteLogGroup = req
    "DeleteLogGroup"
    "fixture/DeleteLogGroup.yaml"

testTestMetricFilter :: TestMetricFilter -> TestTree
testTestMetricFilter = req
    "TestMetricFilter"
    "fixture/TestMetricFilter.yaml"

testDescribeMetricFilters :: DescribeMetricFilters -> TestTree
testDescribeMetricFilters = req
    "DescribeMetricFilters"
    "fixture/DescribeMetricFilters.yaml"

testDeleteMetricFilter :: DeleteMetricFilter -> TestTree
testDeleteMetricFilter = req
    "DeleteMetricFilter"
    "fixture/DeleteMetricFilter.yaml"

testPutRetentionPolicy :: PutRetentionPolicy -> TestTree
testPutRetentionPolicy = req
    "PutRetentionPolicy"
    "fixture/PutRetentionPolicy.yaml"

testDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
testDeleteRetentionPolicy = req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy.yaml"

testPutMetricFilter :: PutMetricFilter -> TestTree
testPutMetricFilter = req
    "PutMetricFilter"
    "fixture/PutMetricFilter.yaml"

testPutDestinationPolicy :: PutDestinationPolicy -> TestTree
testPutDestinationPolicy = req
    "PutDestinationPolicy"
    "fixture/PutDestinationPolicy.yaml"

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

testDeleteDestinationResponse :: DeleteDestinationResponse -> TestTree
testDeleteDestinationResponse = res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteDestination)

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

testPutLogEventsResponse :: PutLogEventsResponse -> TestTree
testPutLogEventsResponse = res
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutLogEvents)

testDeleteSubscriptionFilterResponse :: DeleteSubscriptionFilterResponse -> TestTree
testDeleteSubscriptionFilterResponse = res
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteSubscriptionFilter)

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

testTestMetricFilterResponse :: TestMetricFilterResponse -> TestTree
testTestMetricFilterResponse = res
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy TestMetricFilter)

testDescribeMetricFiltersResponse :: DescribeMetricFiltersResponse -> TestTree
testDescribeMetricFiltersResponse = res
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeMetricFilters)

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

testDeleteRetentionPolicyResponse :: DeleteRetentionPolicyResponse -> TestTree
testDeleteRetentionPolicyResponse = res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteRetentionPolicy)

testPutMetricFilterResponse :: PutMetricFilterResponse -> TestTree
testPutMetricFilterResponse = res
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutMetricFilter)

testPutDestinationPolicyResponse :: PutDestinationPolicyResponse -> TestTree
testPutDestinationPolicyResponse = res
    "PutDestinationPolicyResponse"
    "fixture/PutDestinationPolicyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutDestinationPolicy)

testDescribeLogStreamsResponse :: DescribeLogStreamsResponse -> TestTree
testDescribeLogStreamsResponse = res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeLogStreams)

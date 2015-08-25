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
    "fixture/DescribeDestinations"

testDeleteDestination :: DeleteDestination -> TestTree
testDeleteDestination = req
    "DeleteDestination"
    "fixture/DeleteDestination"

testPutDestination :: PutDestination -> TestTree
testPutDestination = req
    "PutDestination"
    "fixture/PutDestination"

testDescribeSubscriptionFilters :: DescribeSubscriptionFilters -> TestTree
testDescribeSubscriptionFilters = req
    "DescribeSubscriptionFilters"
    "fixture/DescribeSubscriptionFilters"

testGetLogEvents :: GetLogEvents -> TestTree
testGetLogEvents = req
    "GetLogEvents"
    "fixture/GetLogEvents"

testDescribeLogGroups :: DescribeLogGroups -> TestTree
testDescribeLogGroups = req
    "DescribeLogGroups"
    "fixture/DescribeLogGroups"

testFilterLogEvents :: FilterLogEvents -> TestTree
testFilterLogEvents = req
    "FilterLogEvents"
    "fixture/FilterLogEvents"

testDeleteLogStream :: DeleteLogStream -> TestTree
testDeleteLogStream = req
    "DeleteLogStream"
    "fixture/DeleteLogStream"

testCreateLogStream :: CreateLogStream -> TestTree
testCreateLogStream = req
    "CreateLogStream"
    "fixture/CreateLogStream"

testCreateLogGroup :: CreateLogGroup -> TestTree
testCreateLogGroup = req
    "CreateLogGroup"
    "fixture/CreateLogGroup"

testPutLogEvents :: PutLogEvents -> TestTree
testPutLogEvents = req
    "PutLogEvents"
    "fixture/PutLogEvents"

testDeleteSubscriptionFilter :: DeleteSubscriptionFilter -> TestTree
testDeleteSubscriptionFilter = req
    "DeleteSubscriptionFilter"
    "fixture/DeleteSubscriptionFilter"

testPutSubscriptionFilter :: PutSubscriptionFilter -> TestTree
testPutSubscriptionFilter = req
    "PutSubscriptionFilter"
    "fixture/PutSubscriptionFilter"

testDeleteLogGroup :: DeleteLogGroup -> TestTree
testDeleteLogGroup = req
    "DeleteLogGroup"
    "fixture/DeleteLogGroup"

testTestMetricFilter :: TestMetricFilter -> TestTree
testTestMetricFilter = req
    "TestMetricFilter"
    "fixture/TestMetricFilter"

testDescribeMetricFilters :: DescribeMetricFilters -> TestTree
testDescribeMetricFilters = req
    "DescribeMetricFilters"
    "fixture/DescribeMetricFilters"

testDeleteMetricFilter :: DeleteMetricFilter -> TestTree
testDeleteMetricFilter = req
    "DeleteMetricFilter"
    "fixture/DeleteMetricFilter"

testPutRetentionPolicy :: PutRetentionPolicy -> TestTree
testPutRetentionPolicy = req
    "PutRetentionPolicy"
    "fixture/PutRetentionPolicy"

testDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
testDeleteRetentionPolicy = req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy"

testPutMetricFilter :: PutMetricFilter -> TestTree
testPutMetricFilter = req
    "PutMetricFilter"
    "fixture/PutMetricFilter"

testPutDestinationPolicy :: PutDestinationPolicy -> TestTree
testPutDestinationPolicy = req
    "PutDestinationPolicy"
    "fixture/PutDestinationPolicy"

testDescribeLogStreams :: DescribeLogStreams -> TestTree
testDescribeLogStreams = req
    "DescribeLogStreams"
    "fixture/DescribeLogStreams"

-- Responses

testDescribeDestinationsResponse :: DescribeDestinationsResponse -> TestTree
testDescribeDestinationsResponse = res
    "DescribeDestinationsResponse"
    "fixture/DescribeDestinationsResponse"
    cloudWatchLogs
    (Proxy :: Proxy DescribeDestinations)

testDeleteDestinationResponse :: DeleteDestinationResponse -> TestTree
testDeleteDestinationResponse = res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse"
    cloudWatchLogs
    (Proxy :: Proxy DeleteDestination)

testPutDestinationResponse :: PutDestinationResponse -> TestTree
testPutDestinationResponse = res
    "PutDestinationResponse"
    "fixture/PutDestinationResponse"
    cloudWatchLogs
    (Proxy :: Proxy PutDestination)

testDescribeSubscriptionFiltersResponse :: DescribeSubscriptionFiltersResponse -> TestTree
testDescribeSubscriptionFiltersResponse = res
    "DescribeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse"
    cloudWatchLogs
    (Proxy :: Proxy DescribeSubscriptionFilters)

testGetLogEventsResponse :: GetLogEventsResponse -> TestTree
testGetLogEventsResponse = res
    "GetLogEventsResponse"
    "fixture/GetLogEventsResponse"
    cloudWatchLogs
    (Proxy :: Proxy GetLogEvents)

testDescribeLogGroupsResponse :: DescribeLogGroupsResponse -> TestTree
testDescribeLogGroupsResponse = res
    "DescribeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse"
    cloudWatchLogs
    (Proxy :: Proxy DescribeLogGroups)

testFilterLogEventsResponse :: FilterLogEventsResponse -> TestTree
testFilterLogEventsResponse = res
    "FilterLogEventsResponse"
    "fixture/FilterLogEventsResponse"
    cloudWatchLogs
    (Proxy :: Proxy FilterLogEvents)

testDeleteLogStreamResponse :: DeleteLogStreamResponse -> TestTree
testDeleteLogStreamResponse = res
    "DeleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse"
    cloudWatchLogs
    (Proxy :: Proxy DeleteLogStream)

testCreateLogStreamResponse :: CreateLogStreamResponse -> TestTree
testCreateLogStreamResponse = res
    "CreateLogStreamResponse"
    "fixture/CreateLogStreamResponse"
    cloudWatchLogs
    (Proxy :: Proxy CreateLogStream)

testCreateLogGroupResponse :: CreateLogGroupResponse -> TestTree
testCreateLogGroupResponse = res
    "CreateLogGroupResponse"
    "fixture/CreateLogGroupResponse"
    cloudWatchLogs
    (Proxy :: Proxy CreateLogGroup)

testPutLogEventsResponse :: PutLogEventsResponse -> TestTree
testPutLogEventsResponse = res
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse"
    cloudWatchLogs
    (Proxy :: Proxy PutLogEvents)

testDeleteSubscriptionFilterResponse :: DeleteSubscriptionFilterResponse -> TestTree
testDeleteSubscriptionFilterResponse = res
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse"
    cloudWatchLogs
    (Proxy :: Proxy DeleteSubscriptionFilter)

testPutSubscriptionFilterResponse :: PutSubscriptionFilterResponse -> TestTree
testPutSubscriptionFilterResponse = res
    "PutSubscriptionFilterResponse"
    "fixture/PutSubscriptionFilterResponse"
    cloudWatchLogs
    (Proxy :: Proxy PutSubscriptionFilter)

testDeleteLogGroupResponse :: DeleteLogGroupResponse -> TestTree
testDeleteLogGroupResponse = res
    "DeleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse"
    cloudWatchLogs
    (Proxy :: Proxy DeleteLogGroup)

testTestMetricFilterResponse :: TestMetricFilterResponse -> TestTree
testTestMetricFilterResponse = res
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse"
    cloudWatchLogs
    (Proxy :: Proxy TestMetricFilter)

testDescribeMetricFiltersResponse :: DescribeMetricFiltersResponse -> TestTree
testDescribeMetricFiltersResponse = res
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse"
    cloudWatchLogs
    (Proxy :: Proxy DescribeMetricFilters)

testDeleteMetricFilterResponse :: DeleteMetricFilterResponse -> TestTree
testDeleteMetricFilterResponse = res
    "DeleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse"
    cloudWatchLogs
    (Proxy :: Proxy DeleteMetricFilter)

testPutRetentionPolicyResponse :: PutRetentionPolicyResponse -> TestTree
testPutRetentionPolicyResponse = res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse"
    cloudWatchLogs
    (Proxy :: Proxy PutRetentionPolicy)

testDeleteRetentionPolicyResponse :: DeleteRetentionPolicyResponse -> TestTree
testDeleteRetentionPolicyResponse = res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse"
    cloudWatchLogs
    (Proxy :: Proxy DeleteRetentionPolicy)

testPutMetricFilterResponse :: PutMetricFilterResponse -> TestTree
testPutMetricFilterResponse = res
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse"
    cloudWatchLogs
    (Proxy :: Proxy PutMetricFilter)

testPutDestinationPolicyResponse :: PutDestinationPolicyResponse -> TestTree
testPutDestinationPolicyResponse = res
    "PutDestinationPolicyResponse"
    "fixture/PutDestinationPolicyResponse"
    cloudWatchLogs
    (Proxy :: Proxy PutDestinationPolicy)

testDescribeLogStreamsResponse :: DescribeLogStreamsResponse -> TestTree
testDescribeLogStreamsResponse = res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse"
    cloudWatchLogs
    (Proxy :: Proxy DescribeLogStreams)

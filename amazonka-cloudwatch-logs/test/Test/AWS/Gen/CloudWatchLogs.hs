-- Module      : Test.AWS.Gen.CloudWatchLogs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.CloudWatchLogs where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CloudWatchLogs

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeSubscriptionFilters $
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
--         , testDescribeLogStreams $
--             describeLogStreams
--
--           ]

--     , testGroup "response"
--         [ testDescribeSubscriptionFiltersResponse $
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
--         , testDescribeLogStreamsResponse $
--             describeLogStreamsResponse
--
--           ]
--     ]

-- Requests

testDescribeSubscriptionFilters :: DescribeSubscriptionFilters -> TestTree
testDescribeSubscriptionFilters = undefined

testGetLogEvents :: GetLogEvents -> TestTree
testGetLogEvents = undefined

testDescribeLogGroups :: DescribeLogGroups -> TestTree
testDescribeLogGroups = undefined

testFilterLogEvents :: FilterLogEvents -> TestTree
testFilterLogEvents = undefined

testDeleteLogStream :: DeleteLogStream -> TestTree
testDeleteLogStream = undefined

testCreateLogStream :: CreateLogStream -> TestTree
testCreateLogStream = undefined

testCreateLogGroup :: CreateLogGroup -> TestTree
testCreateLogGroup = undefined

testPutLogEvents :: PutLogEvents -> TestTree
testPutLogEvents = undefined

testDeleteSubscriptionFilter :: DeleteSubscriptionFilter -> TestTree
testDeleteSubscriptionFilter = undefined

testPutSubscriptionFilter :: PutSubscriptionFilter -> TestTree
testPutSubscriptionFilter = undefined

testDeleteLogGroup :: DeleteLogGroup -> TestTree
testDeleteLogGroup = undefined

testTestMetricFilter :: TestMetricFilter -> TestTree
testTestMetricFilter = undefined

testDescribeMetricFilters :: DescribeMetricFilters -> TestTree
testDescribeMetricFilters = undefined

testDeleteMetricFilter :: DeleteMetricFilter -> TestTree
testDeleteMetricFilter = undefined

testPutRetentionPolicy :: PutRetentionPolicy -> TestTree
testPutRetentionPolicy = undefined

testDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
testDeleteRetentionPolicy = undefined

testPutMetricFilter :: PutMetricFilter -> TestTree
testPutMetricFilter = undefined

testDescribeLogStreams :: DescribeLogStreams -> TestTree
testDescribeLogStreams = undefined

-- Responses

testDescribeSubscriptionFiltersResponse :: DescribeSubscriptionFiltersResponse -> TestTree
testDescribeSubscriptionFiltersResponse = resp
    "DescribeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse"
    (Proxy :: Proxy DescribeSubscriptionFilters)

testGetLogEventsResponse :: GetLogEventsResponse -> TestTree
testGetLogEventsResponse = resp
    "GetLogEventsResponse"
    "fixture/GetLogEventsResponse"
    (Proxy :: Proxy GetLogEvents)

testDescribeLogGroupsResponse :: DescribeLogGroupsResponse -> TestTree
testDescribeLogGroupsResponse = resp
    "DescribeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse"
    (Proxy :: Proxy DescribeLogGroups)

testFilterLogEventsResponse :: FilterLogEventsResponse -> TestTree
testFilterLogEventsResponse = resp
    "FilterLogEventsResponse"
    "fixture/FilterLogEventsResponse"
    (Proxy :: Proxy FilterLogEvents)

testDeleteLogStreamResponse :: DeleteLogStreamResponse -> TestTree
testDeleteLogStreamResponse = resp
    "DeleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse"
    (Proxy :: Proxy DeleteLogStream)

testCreateLogStreamResponse :: CreateLogStreamResponse -> TestTree
testCreateLogStreamResponse = resp
    "CreateLogStreamResponse"
    "fixture/CreateLogStreamResponse"
    (Proxy :: Proxy CreateLogStream)

testCreateLogGroupResponse :: CreateLogGroupResponse -> TestTree
testCreateLogGroupResponse = resp
    "CreateLogGroupResponse"
    "fixture/CreateLogGroupResponse"
    (Proxy :: Proxy CreateLogGroup)

testPutLogEventsResponse :: PutLogEventsResponse -> TestTree
testPutLogEventsResponse = resp
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse"
    (Proxy :: Proxy PutLogEvents)

testDeleteSubscriptionFilterResponse :: DeleteSubscriptionFilterResponse -> TestTree
testDeleteSubscriptionFilterResponse = resp
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse"
    (Proxy :: Proxy DeleteSubscriptionFilter)

testPutSubscriptionFilterResponse :: PutSubscriptionFilterResponse -> TestTree
testPutSubscriptionFilterResponse = resp
    "PutSubscriptionFilterResponse"
    "fixture/PutSubscriptionFilterResponse"
    (Proxy :: Proxy PutSubscriptionFilter)

testDeleteLogGroupResponse :: DeleteLogGroupResponse -> TestTree
testDeleteLogGroupResponse = resp
    "DeleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse"
    (Proxy :: Proxy DeleteLogGroup)

testTestMetricFilterResponse :: TestMetricFilterResponse -> TestTree
testTestMetricFilterResponse = resp
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse"
    (Proxy :: Proxy TestMetricFilter)

testDescribeMetricFiltersResponse :: DescribeMetricFiltersResponse -> TestTree
testDescribeMetricFiltersResponse = resp
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse"
    (Proxy :: Proxy DescribeMetricFilters)

testDeleteMetricFilterResponse :: DeleteMetricFilterResponse -> TestTree
testDeleteMetricFilterResponse = resp
    "DeleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse"
    (Proxy :: Proxy DeleteMetricFilter)

testPutRetentionPolicyResponse :: PutRetentionPolicyResponse -> TestTree
testPutRetentionPolicyResponse = resp
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse"
    (Proxy :: Proxy PutRetentionPolicy)

testDeleteRetentionPolicyResponse :: DeleteRetentionPolicyResponse -> TestTree
testDeleteRetentionPolicyResponse = resp
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse"
    (Proxy :: Proxy DeleteRetentionPolicy)

testPutMetricFilterResponse :: PutMetricFilterResponse -> TestTree
testPutMetricFilterResponse = resp
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse"
    (Proxy :: Proxy PutMetricFilter)

testDescribeLogStreamsResponse :: DescribeLogStreamsResponse -> TestTree
testDescribeLogStreamsResponse = resp
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse"
    (Proxy :: Proxy DescribeLogStreams)

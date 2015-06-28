-- Module      : Test.AWS.Gen.CloudWatchLogs
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

import           Data.Proxy
import           Network.AWS.CloudWatchLogs
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeSubscriptionFiltersTest $
--             describeSubscriptionFilters
--
--         , getLogEventsTest $
--             getLogEvents
--
--         , describeLogGroupsTest $
--             describeLogGroups
--
--         , filterLogEventsTest $
--             filterLogEvents
--
--         , deleteLogStreamTest $
--             deleteLogStream
--
--         , createLogStreamTest $
--             createLogStream
--
--         , createLogGroupTest $
--             createLogGroup
--
--         , putLogEventsTest $
--             putLogEvents
--
--         , deleteSubscriptionFilterTest $
--             deleteSubscriptionFilter
--
--         , putSubscriptionFilterTest $
--             putSubscriptionFilter
--
--         , deleteLogGroupTest $
--             deleteLogGroup
--
--         , testMetricFilterTest $
--             testMetricFilter
--
--         , describeMetricFiltersTest $
--             describeMetricFilters
--
--         , deleteMetricFilterTest $
--             deleteMetricFilter
--
--         , putRetentionPolicyTest $
--             putRetentionPolicy
--
--         , deleteRetentionPolicyTest $
--             deleteRetentionPolicy
--
--         , putMetricFilterTest $
--             putMetricFilter
--
--         , describeLogStreamsTest $
--             describeLogStreams
--
--           ]

--     , testGroup "response"
--         [ describeSubscriptionFiltersResponseTest $
--             describeSubscriptionFiltersResponse
--
--         , getLogEventsResponseTest $
--             getLogEventsResponse
--
--         , describeLogGroupsResponseTest $
--             describeLogGroupsResponse
--
--         , filterLogEventsResponseTest $
--             filterLogEventsResponse
--
--         , deleteLogStreamResponseTest $
--             deleteLogStreamResponse
--
--         , createLogStreamResponseTest $
--             createLogStreamResponse
--
--         , createLogGroupResponseTest $
--             createLogGroupResponse
--
--         , putLogEventsResponseTest $
--             putLogEventsResponse
--
--         , deleteSubscriptionFilterResponseTest $
--             deleteSubscriptionFilterResponse
--
--         , putSubscriptionFilterResponseTest $
--             putSubscriptionFilterResponse
--
--         , deleteLogGroupResponseTest $
--             deleteLogGroupResponse
--
--         , testMetricFilterResponseTest $
--             testMetricFilterResponse
--
--         , describeMetricFiltersResponseTest $
--             describeMetricFiltersResponse
--
--         , deleteMetricFilterResponseTest $
--             deleteMetricFilterResponse
--
--         , putRetentionPolicyResponseTest $
--             putRetentionPolicyResponse
--
--         , deleteRetentionPolicyResponseTest $
--             deleteRetentionPolicyResponse
--
--         , putMetricFilterResponseTest $
--             putMetricFilterResponse
--
--         , describeLogStreamsResponseTest $
--             describeLogStreamsResponse
--
--           ]
--     ]

-- Requests

describeSubscriptionFiltersTest :: DescribeSubscriptionFilters -> TestTree
describeSubscriptionFiltersTest = undefined

getLogEventsTest :: GetLogEvents -> TestTree
getLogEventsTest = undefined

describeLogGroupsTest :: DescribeLogGroups -> TestTree
describeLogGroupsTest = undefined

filterLogEventsTest :: FilterLogEvents -> TestTree
filterLogEventsTest = undefined

deleteLogStreamTest :: DeleteLogStream -> TestTree
deleteLogStreamTest = undefined

createLogStreamTest :: CreateLogStream -> TestTree
createLogStreamTest = undefined

createLogGroupTest :: CreateLogGroup -> TestTree
createLogGroupTest = undefined

putLogEventsTest :: PutLogEvents -> TestTree
putLogEventsTest = undefined

deleteSubscriptionFilterTest :: DeleteSubscriptionFilter -> TestTree
deleteSubscriptionFilterTest = undefined

putSubscriptionFilterTest :: PutSubscriptionFilter -> TestTree
putSubscriptionFilterTest = undefined

deleteLogGroupTest :: DeleteLogGroup -> TestTree
deleteLogGroupTest = undefined

testMetricFilterTest :: TestMetricFilter -> TestTree
testMetricFilterTest = undefined

describeMetricFiltersTest :: DescribeMetricFilters -> TestTree
describeMetricFiltersTest = undefined

deleteMetricFilterTest :: DeleteMetricFilter -> TestTree
deleteMetricFilterTest = undefined

putRetentionPolicyTest :: PutRetentionPolicy -> TestTree
putRetentionPolicyTest = undefined

deleteRetentionPolicyTest :: DeleteRetentionPolicy -> TestTree
deleteRetentionPolicyTest = undefined

putMetricFilterTest :: PutMetricFilter -> TestTree
putMetricFilterTest = undefined

describeLogStreamsTest :: DescribeLogStreams -> TestTree
describeLogStreamsTest = undefined

-- Responses

describeSubscriptionFiltersResponseTest :: DescribeSubscriptionFiltersResponse -> TestTree
describeSubscriptionFiltersResponseTest = resp
    "DescribeSubscriptionFilters"
    "fixture/CloudWatchLogs/DescribeSubscriptionFiltersResponse"
    (Proxy :: Proxy DescribeSubscriptionFilters)

getLogEventsResponseTest :: GetLogEventsResponse -> TestTree
getLogEventsResponseTest = resp
    "GetLogEvents"
    "fixture/CloudWatchLogs/GetLogEventsResponse"
    (Proxy :: Proxy GetLogEvents)

describeLogGroupsResponseTest :: DescribeLogGroupsResponse -> TestTree
describeLogGroupsResponseTest = resp
    "DescribeLogGroups"
    "fixture/CloudWatchLogs/DescribeLogGroupsResponse"
    (Proxy :: Proxy DescribeLogGroups)

filterLogEventsResponseTest :: FilterLogEventsResponse -> TestTree
filterLogEventsResponseTest = resp
    "FilterLogEvents"
    "fixture/CloudWatchLogs/FilterLogEventsResponse"
    (Proxy :: Proxy FilterLogEvents)

deleteLogStreamResponseTest :: DeleteLogStreamResponse -> TestTree
deleteLogStreamResponseTest = resp
    "DeleteLogStream"
    "fixture/CloudWatchLogs/DeleteLogStreamResponse"
    (Proxy :: Proxy DeleteLogStream)

createLogStreamResponseTest :: CreateLogStreamResponse -> TestTree
createLogStreamResponseTest = resp
    "CreateLogStream"
    "fixture/CloudWatchLogs/CreateLogStreamResponse"
    (Proxy :: Proxy CreateLogStream)

createLogGroupResponseTest :: CreateLogGroupResponse -> TestTree
createLogGroupResponseTest = resp
    "CreateLogGroup"
    "fixture/CloudWatchLogs/CreateLogGroupResponse"
    (Proxy :: Proxy CreateLogGroup)

putLogEventsResponseTest :: PutLogEventsResponse -> TestTree
putLogEventsResponseTest = resp
    "PutLogEvents"
    "fixture/CloudWatchLogs/PutLogEventsResponse"
    (Proxy :: Proxy PutLogEvents)

deleteSubscriptionFilterResponseTest :: DeleteSubscriptionFilterResponse -> TestTree
deleteSubscriptionFilterResponseTest = resp
    "DeleteSubscriptionFilter"
    "fixture/CloudWatchLogs/DeleteSubscriptionFilterResponse"
    (Proxy :: Proxy DeleteSubscriptionFilter)

putSubscriptionFilterResponseTest :: PutSubscriptionFilterResponse -> TestTree
putSubscriptionFilterResponseTest = resp
    "PutSubscriptionFilter"
    "fixture/CloudWatchLogs/PutSubscriptionFilterResponse"
    (Proxy :: Proxy PutSubscriptionFilter)

deleteLogGroupResponseTest :: DeleteLogGroupResponse -> TestTree
deleteLogGroupResponseTest = resp
    "DeleteLogGroup"
    "fixture/CloudWatchLogs/DeleteLogGroupResponse"
    (Proxy :: Proxy DeleteLogGroup)

testMetricFilterResponseTest :: TestMetricFilterResponse -> TestTree
testMetricFilterResponseTest = resp
    "TestMetricFilter"
    "fixture/CloudWatchLogs/TestMetricFilterResponse"
    (Proxy :: Proxy TestMetricFilter)

describeMetricFiltersResponseTest :: DescribeMetricFiltersResponse -> TestTree
describeMetricFiltersResponseTest = resp
    "DescribeMetricFilters"
    "fixture/CloudWatchLogs/DescribeMetricFiltersResponse"
    (Proxy :: Proxy DescribeMetricFilters)

deleteMetricFilterResponseTest :: DeleteMetricFilterResponse -> TestTree
deleteMetricFilterResponseTest = resp
    "DeleteMetricFilter"
    "fixture/CloudWatchLogs/DeleteMetricFilterResponse"
    (Proxy :: Proxy DeleteMetricFilter)

putRetentionPolicyResponseTest :: PutRetentionPolicyResponse -> TestTree
putRetentionPolicyResponseTest = resp
    "PutRetentionPolicy"
    "fixture/CloudWatchLogs/PutRetentionPolicyResponse"
    (Proxy :: Proxy PutRetentionPolicy)

deleteRetentionPolicyResponseTest :: DeleteRetentionPolicyResponse -> TestTree
deleteRetentionPolicyResponseTest = resp
    "DeleteRetentionPolicy"
    "fixture/CloudWatchLogs/DeleteRetentionPolicyResponse"
    (Proxy :: Proxy DeleteRetentionPolicy)

putMetricFilterResponseTest :: PutMetricFilterResponse -> TestTree
putMetricFilterResponseTest = resp
    "PutMetricFilter"
    "fixture/CloudWatchLogs/PutMetricFilterResponse"
    (Proxy :: Proxy PutMetricFilter)

describeLogStreamsResponseTest :: DescribeLogStreamsResponse -> TestTree
describeLogStreamsResponseTest = resp
    "DescribeLogStreams"
    "fixture/CloudWatchLogs/DescribeLogStreamsResponse"
    (Proxy :: Proxy DescribeLogStreams)

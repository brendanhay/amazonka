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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CloudWatchLogs

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ createLogGroupTest $
--             createLogGroup
--
--         , createLogStreamTest $
--             createLogStream
--
--         , deleteLogGroupTest $
--             deleteLogGroup
--
--         , deleteLogStreamTest $
--             deleteLogStream
--
--         , deleteMetricFilterTest $
--             deleteMetricFilter
--
--         , deleteRetentionPolicyTest $
--             deleteRetentionPolicy
--
--         , deleteSubscriptionFilterTest $
--             deleteSubscriptionFilter
--
--         , describeLogGroupsTest $
--             describeLogGroups
--
--         , describeLogStreamsTest $
--             describeLogStreams
--
--         , describeMetricFiltersTest $
--             describeMetricFilters
--
--         , describeSubscriptionFiltersTest $
--             describeSubscriptionFilters
--
--         , filterLogEventsTest $
--             filterLogEvents
--
--         , getLogEventsTest $
--             getLogEvents
--
--         , putLogEventsTest $
--             putLogEvents
--
--         , putMetricFilterTest $
--             putMetricFilter
--
--         , putRetentionPolicyTest $
--             putRetentionPolicy
--
--         , putSubscriptionFilterTest $
--             putSubscriptionFilter
--
--         , testMetricFilterTest $
--             testMetricFilter
--
--           ]

--     , testGroup "response"
--         [ createLogGroupResponseTest $
--             createLogGroupResponse
--
--         , createLogStreamResponseTest $
--             createLogStreamResponse
--
--         , deleteLogGroupResponseTest $
--             deleteLogGroupResponse
--
--         , deleteLogStreamResponseTest $
--             deleteLogStreamResponse
--
--         , deleteMetricFilterResponseTest $
--             deleteMetricFilterResponse
--
--         , deleteRetentionPolicyResponseTest $
--             deleteRetentionPolicyResponse
--
--         , deleteSubscriptionFilterResponseTest $
--             deleteSubscriptionFilterResponse
--
--         , describeLogGroupsResponseTest $
--             describeLogGroupsResponse
--
--         , describeLogStreamsResponseTest $
--             describeLogStreamsResponse
--
--         , describeMetricFiltersResponseTest $
--             describeMetricFiltersResponse
--
--         , describeSubscriptionFiltersResponseTest $
--             describeSubscriptionFiltersResponse
--
--         , filterLogEventsResponseTest $
--             filterLogEventsResponse
--
--         , getLogEventsResponseTest $
--             getLogEventsResponse
--
--         , putLogEventsResponseTest $
--             putLogEventsResponse
--
--         , putMetricFilterResponseTest $
--             putMetricFilterResponse
--
--         , putRetentionPolicyResponseTest $
--             putRetentionPolicyResponse
--
--         , putSubscriptionFilterResponseTest $
--             putSubscriptionFilterResponse
--
--         , testMetricFilterResponseTest $
--             testMetricFilterResponse
--
--           ]
--     ]

-- Requests

createLogGroupTest :: CreateLogGroup -> TestTree
createLogGroupTest = undefined

createLogStreamTest :: CreateLogStream -> TestTree
createLogStreamTest = undefined

deleteLogGroupTest :: DeleteLogGroup -> TestTree
deleteLogGroupTest = undefined

deleteLogStreamTest :: DeleteLogStream -> TestTree
deleteLogStreamTest = undefined

deleteMetricFilterTest :: DeleteMetricFilter -> TestTree
deleteMetricFilterTest = undefined

deleteRetentionPolicyTest :: DeleteRetentionPolicy -> TestTree
deleteRetentionPolicyTest = undefined

deleteSubscriptionFilterTest :: DeleteSubscriptionFilter -> TestTree
deleteSubscriptionFilterTest = undefined

describeLogGroupsTest :: DescribeLogGroups -> TestTree
describeLogGroupsTest = undefined

describeLogStreamsTest :: DescribeLogStreams -> TestTree
describeLogStreamsTest = undefined

describeMetricFiltersTest :: DescribeMetricFilters -> TestTree
describeMetricFiltersTest = undefined

describeSubscriptionFiltersTest :: DescribeSubscriptionFilters -> TestTree
describeSubscriptionFiltersTest = undefined

filterLogEventsTest :: FilterLogEvents -> TestTree
filterLogEventsTest = undefined

getLogEventsTest :: GetLogEvents -> TestTree
getLogEventsTest = undefined

putLogEventsTest :: PutLogEvents -> TestTree
putLogEventsTest = undefined

putMetricFilterTest :: PutMetricFilter -> TestTree
putMetricFilterTest = undefined

putRetentionPolicyTest :: PutRetentionPolicy -> TestTree
putRetentionPolicyTest = undefined

putSubscriptionFilterTest :: PutSubscriptionFilter -> TestTree
putSubscriptionFilterTest = undefined

testMetricFilterTest :: TestMetricFilter -> TestTree
testMetricFilterTest = undefined

-- Responses

createLogGroupResponseTest :: CreateLogGroupResponse -> TestTree
createLogGroupResponseTest = resp
    "createLogGroupResponse"
    "fixture/CreateLogGroupResponse"
    (Proxy :: Proxy CreateLogGroup)

createLogStreamResponseTest :: CreateLogStreamResponse -> TestTree
createLogStreamResponseTest = resp
    "createLogStreamResponse"
    "fixture/CreateLogStreamResponse"
    (Proxy :: Proxy CreateLogStream)

deleteLogGroupResponseTest :: DeleteLogGroupResponse -> TestTree
deleteLogGroupResponseTest = resp
    "deleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse"
    (Proxy :: Proxy DeleteLogGroup)

deleteLogStreamResponseTest :: DeleteLogStreamResponse -> TestTree
deleteLogStreamResponseTest = resp
    "deleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse"
    (Proxy :: Proxy DeleteLogStream)

deleteMetricFilterResponseTest :: DeleteMetricFilterResponse -> TestTree
deleteMetricFilterResponseTest = resp
    "deleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse"
    (Proxy :: Proxy DeleteMetricFilter)

deleteRetentionPolicyResponseTest :: DeleteRetentionPolicyResponse -> TestTree
deleteRetentionPolicyResponseTest = resp
    "deleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse"
    (Proxy :: Proxy DeleteRetentionPolicy)

deleteSubscriptionFilterResponseTest :: DeleteSubscriptionFilterResponse -> TestTree
deleteSubscriptionFilterResponseTest = resp
    "deleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse"
    (Proxy :: Proxy DeleteSubscriptionFilter)

describeLogGroupsResponseTest :: DescribeLogGroupsResponse -> TestTree
describeLogGroupsResponseTest = resp
    "describeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse"
    (Proxy :: Proxy DescribeLogGroups)

describeLogStreamsResponseTest :: DescribeLogStreamsResponse -> TestTree
describeLogStreamsResponseTest = resp
    "describeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse"
    (Proxy :: Proxy DescribeLogStreams)

describeMetricFiltersResponseTest :: DescribeMetricFiltersResponse -> TestTree
describeMetricFiltersResponseTest = resp
    "describeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse"
    (Proxy :: Proxy DescribeMetricFilters)

describeSubscriptionFiltersResponseTest :: DescribeSubscriptionFiltersResponse -> TestTree
describeSubscriptionFiltersResponseTest = resp
    "describeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse"
    (Proxy :: Proxy DescribeSubscriptionFilters)

filterLogEventsResponseTest :: FilterLogEventsResponse -> TestTree
filterLogEventsResponseTest = resp
    "filterLogEventsResponse"
    "fixture/FilterLogEventsResponse"
    (Proxy :: Proxy FilterLogEvents)

getLogEventsResponseTest :: GetLogEventsResponse -> TestTree
getLogEventsResponseTest = resp
    "getLogEventsResponse"
    "fixture/GetLogEventsResponse"
    (Proxy :: Proxy GetLogEvents)

putLogEventsResponseTest :: PutLogEventsResponse -> TestTree
putLogEventsResponseTest = resp
    "putLogEventsResponse"
    "fixture/PutLogEventsResponse"
    (Proxy :: Proxy PutLogEvents)

putMetricFilterResponseTest :: PutMetricFilterResponse -> TestTree
putMetricFilterResponseTest = resp
    "putMetricFilterResponse"
    "fixture/PutMetricFilterResponse"
    (Proxy :: Proxy PutMetricFilter)

putRetentionPolicyResponseTest :: PutRetentionPolicyResponse -> TestTree
putRetentionPolicyResponseTest = resp
    "putRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse"
    (Proxy :: Proxy PutRetentionPolicy)

putSubscriptionFilterResponseTest :: PutSubscriptionFilterResponse -> TestTree
putSubscriptionFilterResponseTest = resp
    "putSubscriptionFilterResponse"
    "fixture/PutSubscriptionFilterResponse"
    (Proxy :: Proxy PutSubscriptionFilter)

testMetricFilterResponseTest :: TestMetricFilterResponse -> TestTree
testMetricFilterResponseTest = resp
    "testMetricFilterResponse"
    "fixture/TestMetricFilterResponse"
    (Proxy :: Proxy TestMetricFilter)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchLogs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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

testDescribeLogStreams :: DescribeLogStreams -> TestTree
testDescribeLogStreams = req
    "DescribeLogStreams"
    "fixture/DescribeLogStreams"

-- Responses

testDescribeSubscriptionFiltersResponse :: DescribeSubscriptionFiltersResponse -> TestTree
testDescribeSubscriptionFiltersResponse = res
    "DescribeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse"
    (Proxy :: Proxy DescribeSubscriptionFilters)

testGetLogEventsResponse :: GetLogEventsResponse -> TestTree
testGetLogEventsResponse = res
    "GetLogEventsResponse"
    "fixture/GetLogEventsResponse"
    (Proxy :: Proxy GetLogEvents)

testDescribeLogGroupsResponse :: DescribeLogGroupsResponse -> TestTree
testDescribeLogGroupsResponse = res
    "DescribeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse"
    (Proxy :: Proxy DescribeLogGroups)

testFilterLogEventsResponse :: FilterLogEventsResponse -> TestTree
testFilterLogEventsResponse = res
    "FilterLogEventsResponse"
    "fixture/FilterLogEventsResponse"
    (Proxy :: Proxy FilterLogEvents)

testDeleteLogStreamResponse :: DeleteLogStreamResponse -> TestTree
testDeleteLogStreamResponse = res
    "DeleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse"
    (Proxy :: Proxy DeleteLogStream)

testCreateLogStreamResponse :: CreateLogStreamResponse -> TestTree
testCreateLogStreamResponse = res
    "CreateLogStreamResponse"
    "fixture/CreateLogStreamResponse"
    (Proxy :: Proxy CreateLogStream)

testCreateLogGroupResponse :: CreateLogGroupResponse -> TestTree
testCreateLogGroupResponse = res
    "CreateLogGroupResponse"
    "fixture/CreateLogGroupResponse"
    (Proxy :: Proxy CreateLogGroup)

testPutLogEventsResponse :: PutLogEventsResponse -> TestTree
testPutLogEventsResponse = res
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse"
    (Proxy :: Proxy PutLogEvents)

testDeleteSubscriptionFilterResponse :: DeleteSubscriptionFilterResponse -> TestTree
testDeleteSubscriptionFilterResponse = res
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse"
    (Proxy :: Proxy DeleteSubscriptionFilter)

testPutSubscriptionFilterResponse :: PutSubscriptionFilterResponse -> TestTree
testPutSubscriptionFilterResponse = res
    "PutSubscriptionFilterResponse"
    "fixture/PutSubscriptionFilterResponse"
    (Proxy :: Proxy PutSubscriptionFilter)

testDeleteLogGroupResponse :: DeleteLogGroupResponse -> TestTree
testDeleteLogGroupResponse = res
    "DeleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse"
    (Proxy :: Proxy DeleteLogGroup)

testTestMetricFilterResponse :: TestMetricFilterResponse -> TestTree
testTestMetricFilterResponse = res
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse"
    (Proxy :: Proxy TestMetricFilter)

testDescribeMetricFiltersResponse :: DescribeMetricFiltersResponse -> TestTree
testDescribeMetricFiltersResponse = res
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse"
    (Proxy :: Proxy DescribeMetricFilters)

testDeleteMetricFilterResponse :: DeleteMetricFilterResponse -> TestTree
testDeleteMetricFilterResponse = res
    "DeleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse"
    (Proxy :: Proxy DeleteMetricFilter)

testPutRetentionPolicyResponse :: PutRetentionPolicyResponse -> TestTree
testPutRetentionPolicyResponse = res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse"
    (Proxy :: Proxy PutRetentionPolicy)

testDeleteRetentionPolicyResponse :: DeleteRetentionPolicyResponse -> TestTree
testDeleteRetentionPolicyResponse = res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse"
    (Proxy :: Proxy DeleteRetentionPolicy)

testPutMetricFilterResponse :: PutMetricFilterResponse -> TestTree
testPutMetricFilterResponse = res
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse"
    (Proxy :: Proxy PutMetricFilter)

testDescribeLogStreamsResponse :: DescribeLogStreamsResponse -> TestTree
testDescribeLogStreamsResponse = res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse"
    (Proxy :: Proxy DescribeLogStreams)

instance Out CreateLogGroup
instance Out CreateLogGroupResponse
instance Out CreateLogStream
instance Out CreateLogStreamResponse
instance Out DeleteLogGroup
instance Out DeleteLogGroupResponse
instance Out DeleteLogStream
instance Out DeleteLogStreamResponse
instance Out DeleteMetricFilter
instance Out DeleteMetricFilterResponse
instance Out DeleteRetentionPolicy
instance Out DeleteRetentionPolicyResponse
instance Out DeleteSubscriptionFilter
instance Out DeleteSubscriptionFilterResponse
instance Out DescribeLogGroups
instance Out DescribeLogGroupsResponse
instance Out DescribeLogStreams
instance Out DescribeLogStreamsResponse
instance Out DescribeMetricFilters
instance Out DescribeMetricFiltersResponse
instance Out DescribeSubscriptionFilters
instance Out DescribeSubscriptionFiltersResponse
instance Out FilterLogEvents
instance Out FilterLogEventsResponse
instance Out FilteredLogEvent
instance Out GetLogEvents
instance Out GetLogEventsResponse
instance Out InputLogEvent
instance Out LogGroup
instance Out LogStream
instance Out MetricFilter
instance Out MetricFilterMatchRecord
instance Out MetricTransformation
instance Out OrderBy
instance Out OutputLogEvent
instance Out PutLogEvents
instance Out PutLogEventsResponse
instance Out PutMetricFilter
instance Out PutMetricFilterResponse
instance Out PutRetentionPolicy
instance Out PutRetentionPolicyResponse
instance Out PutSubscriptionFilter
instance Out PutSubscriptionFilterResponse
instance Out RejectedLogEventsInfo
instance Out SearchedLogStream
instance Out SubscriptionFilter
instance Out TestMetricFilter
instance Out TestMetricFilterResponse

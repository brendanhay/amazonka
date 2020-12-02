{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchLogs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudWatchLogs where

import Data.Proxy
import Network.AWS.CloudWatchLogs
import Test.AWS.CloudWatchLogs.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

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
--         , requestUntagLogGroup $
--             untagLogGroup
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
--         , requestDisassociateKMSKey $
--             disassociateKMSKey
--
--         , requestFilterLogEvents $
--             filterLogEvents
--
--         , requestTagLogGroup $
--             tagLogGroup
--
--         , requestDescribeResourcePolicies $
--             describeResourcePolicies
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
--         , requestListTagsLogGroup $
--             listTagsLogGroup
--
--         , requestPutResourcePolicy $
--             putResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             deleteResourcePolicy
--
--         , requestAssociateKMSKey $
--             associateKMSKey
--
--         , requestDescribeLogStreams $
--             describeLogStreams
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDestinations $
--             describeDestinationsResponse
--
--         , responseUntagLogGroup $
--             untagLogGroupResponse
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
--         , responseDisassociateKMSKey $
--             disassociateKMSKeyResponse
--
--         , responseFilterLogEvents $
--             filterLogEventsResponse
--
--         , responseTagLogGroup $
--             tagLogGroupResponse
--
--         , responseDescribeResourcePolicies $
--             describeResourcePoliciesResponse
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
--         , responseListTagsLogGroup $
--             listTagsLogGroupResponse
--
--         , responsePutResourcePolicy $
--             putResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             deleteResourcePolicyResponse
--
--         , responseAssociateKMSKey $
--             associateKMSKeyResponse
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

requestUntagLogGroup :: UntagLogGroup -> TestTree
requestUntagLogGroup = req
    "UntagLogGroup"
    "fixture/UntagLogGroup.yaml"

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

requestDisassociateKMSKey :: DisassociateKMSKey -> TestTree
requestDisassociateKMSKey = req
    "DisassociateKMSKey"
    "fixture/DisassociateKMSKey.yaml"

requestFilterLogEvents :: FilterLogEvents -> TestTree
requestFilterLogEvents = req
    "FilterLogEvents"
    "fixture/FilterLogEvents.yaml"

requestTagLogGroup :: TagLogGroup -> TestTree
requestTagLogGroup = req
    "TagLogGroup"
    "fixture/TagLogGroup.yaml"

requestDescribeResourcePolicies :: DescribeResourcePolicies -> TestTree
requestDescribeResourcePolicies = req
    "DescribeResourcePolicies"
    "fixture/DescribeResourcePolicies.yaml"

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

requestListTagsLogGroup :: ListTagsLogGroup -> TestTree
requestListTagsLogGroup = req
    "ListTagsLogGroup"
    "fixture/ListTagsLogGroup.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy = req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy = req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestAssociateKMSKey :: AssociateKMSKey -> TestTree
requestAssociateKMSKey = req
    "AssociateKMSKey"
    "fixture/AssociateKMSKey.yaml"

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

responseUntagLogGroup :: UntagLogGroupResponse -> TestTree
responseUntagLogGroup = res
    "UntagLogGroupResponse"
    "fixture/UntagLogGroupResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy UntagLogGroup)

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

responseDisassociateKMSKey :: DisassociateKMSKeyResponse -> TestTree
responseDisassociateKMSKey = res
    "DisassociateKMSKeyResponse"
    "fixture/DisassociateKMSKeyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DisassociateKMSKey)

responseFilterLogEvents :: FilterLogEventsResponse -> TestTree
responseFilterLogEvents = res
    "FilterLogEventsResponse"
    "fixture/FilterLogEventsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy FilterLogEvents)

responseTagLogGroup :: TagLogGroupResponse -> TestTree
responseTagLogGroup = res
    "TagLogGroupResponse"
    "fixture/TagLogGroupResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy TagLogGroup)

responseDescribeResourcePolicies :: DescribeResourcePoliciesResponse -> TestTree
responseDescribeResourcePolicies = res
    "DescribeResourcePoliciesResponse"
    "fixture/DescribeResourcePoliciesResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeResourcePolicies)

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

responseListTagsLogGroup :: ListTagsLogGroupResponse -> TestTree
responseListTagsLogGroup = res
    "ListTagsLogGroupResponse"
    "fixture/ListTagsLogGroupResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy ListTagsLogGroup)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy = res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy = res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DeleteResourcePolicy)

responseAssociateKMSKey :: AssociateKMSKeyResponse -> TestTree
responseAssociateKMSKey = res
    "AssociateKMSKeyResponse"
    "fixture/AssociateKMSKeyResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy AssociateKMSKey)

responseDescribeLogStreams :: DescribeLogStreamsResponse -> TestTree
responseDescribeLogStreams = res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse.proto"
    cloudWatchLogs
    (Proxy :: Proxy DescribeLogStreams)

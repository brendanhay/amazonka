{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestGetLogGroupFields $
--             mkGetLogGroupFields
--
--         , requestGetLogRecord $
--             mkGetLogRecord
--
--         , requestDescribeDestinations $
--             mkDescribeDestinations
--
--         , requestUntagLogGroup $
--             mkUntagLogGroup
--
--         , requestStopQuery $
--             mkStopQuery
--
--         , requestCreateExportTask $
--             mkCreateExportTask
--
--         , requestPutDestination $
--             mkPutDestination
--
--         , requestDescribeSubscriptionFilters $
--             mkDescribeSubscriptionFilters
--
--         , requestGetLogEvents $
--             mkGetLogEvents
--
--         , requestDescribeLogGroups $
--             mkDescribeLogGroups
--
--         , requestDeleteDestination $
--             mkDeleteDestination
--
--         , requestDisassociateKmsKey $
--             mkDisassociateKmsKey
--
--         , requestFilterLogEvents $
--             mkFilterLogEvents
--
--         , requestDeleteQueryDefinition $
--             mkDeleteQueryDefinition
--
--         , requestPutQueryDefinition $
--             mkPutQueryDefinition
--
--         , requestTagLogGroup $
--             mkTagLogGroup
--
--         , requestDescribeResourcePolicies $
--             mkDescribeResourcePolicies
--
--         , requestDescribeQueryDefinitions $
--             mkDescribeQueryDefinitions
--
--         , requestDeleteLogStream $
--             mkDeleteLogStream
--
--         , requestDescribeQueries $
--             mkDescribeQueries
--
--         , requestCreateLogStream $
--             mkCreateLogStream
--
--         , requestCreateLogGroup $
--             mkCreateLogGroup
--
--         , requestDescribeExportTasks $
--             mkDescribeExportTasks
--
--         , requestCancelExportTask $
--             mkCancelExportTask
--
--         , requestPutSubscriptionFilter $
--             mkPutSubscriptionFilter
--
--         , requestStartQuery $
--             mkStartQuery
--
--         , requestDeleteLogGroup $
--             mkDeleteLogGroup
--
--         , requestDeleteSubscriptionFilter $
--             mkDeleteSubscriptionFilter
--
--         , requestPutLogEvents $
--             mkPutLogEvents
--
--         , requestDescribeMetricFilters $
--             mkDescribeMetricFilters
--
--         , requestTestMetricFilter $
--             mkTestMetricFilter
--
--         , requestPutDestinationPolicy $
--             mkPutDestinationPolicy
--
--         , requestPutMetricFilter $
--             mkPutMetricFilter
--
--         , requestDeleteRetentionPolicy $
--             mkDeleteRetentionPolicy
--
--         , requestDeleteMetricFilter $
--             mkDeleteMetricFilter
--
--         , requestPutRetentionPolicy $
--             mkPutRetentionPolicy
--
--         , requestListTagsLogGroup $
--             mkListTagsLogGroup
--
--         , requestPutResourcePolicy $
--             mkPutResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             mkDeleteResourcePolicy
--
--         , requestAssociateKmsKey $
--             mkAssociateKmsKey
--
--         , requestGetQueryResults $
--             mkGetQueryResults
--
--         , requestDescribeLogStreams $
--             mkDescribeLogStreams
--
--           ]

--     , testGroup "response"
--         [ responseGetLogGroupFields $
--             mkGetLogGroupFieldsResponse
--
--         , responseGetLogRecord $
--             mkGetLogRecordResponse
--
--         , responseDescribeDestinations $
--             mkDescribeDestinationsResponse
--
--         , responseUntagLogGroup $
--             mkUntagLogGroupResponse
--
--         , responseStopQuery $
--             mkStopQueryResponse
--
--         , responseCreateExportTask $
--             mkCreateExportTaskResponse
--
--         , responsePutDestination $
--             mkPutDestinationResponse
--
--         , responseDescribeSubscriptionFilters $
--             mkDescribeSubscriptionFiltersResponse
--
--         , responseGetLogEvents $
--             mkGetLogEventsResponse
--
--         , responseDescribeLogGroups $
--             mkDescribeLogGroupsResponse
--
--         , responseDeleteDestination $
--             mkDeleteDestinationResponse
--
--         , responseDisassociateKmsKey $
--             mkDisassociateKmsKeyResponse
--
--         , responseFilterLogEvents $
--             mkFilterLogEventsResponse
--
--         , responseDeleteQueryDefinition $
--             mkDeleteQueryDefinitionResponse
--
--         , responsePutQueryDefinition $
--             mkPutQueryDefinitionResponse
--
--         , responseTagLogGroup $
--             mkTagLogGroupResponse
--
--         , responseDescribeResourcePolicies $
--             mkDescribeResourcePoliciesResponse
--
--         , responseDescribeQueryDefinitions $
--             mkDescribeQueryDefinitionsResponse
--
--         , responseDeleteLogStream $
--             mkDeleteLogStreamResponse
--
--         , responseDescribeQueries $
--             mkDescribeQueriesResponse
--
--         , responseCreateLogStream $
--             mkCreateLogStreamResponse
--
--         , responseCreateLogGroup $
--             mkCreateLogGroupResponse
--
--         , responseDescribeExportTasks $
--             mkDescribeExportTasksResponse
--
--         , responseCancelExportTask $
--             mkCancelExportTaskResponse
--
--         , responsePutSubscriptionFilter $
--             mkPutSubscriptionFilterResponse
--
--         , responseStartQuery $
--             mkStartQueryResponse
--
--         , responseDeleteLogGroup $
--             mkDeleteLogGroupResponse
--
--         , responseDeleteSubscriptionFilter $
--             mkDeleteSubscriptionFilterResponse
--
--         , responsePutLogEvents $
--             mkPutLogEventsResponse
--
--         , responseDescribeMetricFilters $
--             mkDescribeMetricFiltersResponse
--
--         , responseTestMetricFilter $
--             mkTestMetricFilterResponse
--
--         , responsePutDestinationPolicy $
--             mkPutDestinationPolicyResponse
--
--         , responsePutMetricFilter $
--             mkPutMetricFilterResponse
--
--         , responseDeleteRetentionPolicy $
--             mkDeleteRetentionPolicyResponse
--
--         , responseDeleteMetricFilter $
--             mkDeleteMetricFilterResponse
--
--         , responsePutRetentionPolicy $
--             mkPutRetentionPolicyResponse
--
--         , responseListTagsLogGroup $
--             mkListTagsLogGroupResponse
--
--         , responsePutResourcePolicy $
--             mkPutResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             mkDeleteResourcePolicyResponse
--
--         , responseAssociateKmsKey $
--             mkAssociateKmsKeyResponse
--
--         , responseGetQueryResults $
--             mkGetQueryResultsResponse
--
--         , responseDescribeLogStreams $
--             mkDescribeLogStreamsResponse
--
--           ]
--     ]

-- Requests

requestGetLogGroupFields :: GetLogGroupFields -> TestTree
requestGetLogGroupFields = req
    "GetLogGroupFields"
    "fixture/GetLogGroupFields.yaml"

requestGetLogRecord :: GetLogRecord -> TestTree
requestGetLogRecord = req
    "GetLogRecord"
    "fixture/GetLogRecord.yaml"

requestDescribeDestinations :: DescribeDestinations -> TestTree
requestDescribeDestinations = req
    "DescribeDestinations"
    "fixture/DescribeDestinations.yaml"

requestUntagLogGroup :: UntagLogGroup -> TestTree
requestUntagLogGroup = req
    "UntagLogGroup"
    "fixture/UntagLogGroup.yaml"

requestStopQuery :: StopQuery -> TestTree
requestStopQuery = req
    "StopQuery"
    "fixture/StopQuery.yaml"

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

requestDisassociateKmsKey :: DisassociateKmsKey -> TestTree
requestDisassociateKmsKey = req
    "DisassociateKmsKey"
    "fixture/DisassociateKmsKey.yaml"

requestFilterLogEvents :: FilterLogEvents -> TestTree
requestFilterLogEvents = req
    "FilterLogEvents"
    "fixture/FilterLogEvents.yaml"

requestDeleteQueryDefinition :: DeleteQueryDefinition -> TestTree
requestDeleteQueryDefinition = req
    "DeleteQueryDefinition"
    "fixture/DeleteQueryDefinition.yaml"

requestPutQueryDefinition :: PutQueryDefinition -> TestTree
requestPutQueryDefinition = req
    "PutQueryDefinition"
    "fixture/PutQueryDefinition.yaml"

requestTagLogGroup :: TagLogGroup -> TestTree
requestTagLogGroup = req
    "TagLogGroup"
    "fixture/TagLogGroup.yaml"

requestDescribeResourcePolicies :: DescribeResourcePolicies -> TestTree
requestDescribeResourcePolicies = req
    "DescribeResourcePolicies"
    "fixture/DescribeResourcePolicies.yaml"

requestDescribeQueryDefinitions :: DescribeQueryDefinitions -> TestTree
requestDescribeQueryDefinitions = req
    "DescribeQueryDefinitions"
    "fixture/DescribeQueryDefinitions.yaml"

requestDeleteLogStream :: DeleteLogStream -> TestTree
requestDeleteLogStream = req
    "DeleteLogStream"
    "fixture/DeleteLogStream.yaml"

requestDescribeQueries :: DescribeQueries -> TestTree
requestDescribeQueries = req
    "DescribeQueries"
    "fixture/DescribeQueries.yaml"

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

requestStartQuery :: StartQuery -> TestTree
requestStartQuery = req
    "StartQuery"
    "fixture/StartQuery.yaml"

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

requestAssociateKmsKey :: AssociateKmsKey -> TestTree
requestAssociateKmsKey = req
    "AssociateKmsKey"
    "fixture/AssociateKmsKey.yaml"

requestGetQueryResults :: GetQueryResults -> TestTree
requestGetQueryResults = req
    "GetQueryResults"
    "fixture/GetQueryResults.yaml"

requestDescribeLogStreams :: DescribeLogStreams -> TestTree
requestDescribeLogStreams = req
    "DescribeLogStreams"
    "fixture/DescribeLogStreams.yaml"

-- Responses

responseGetLogGroupFields :: GetLogGroupFieldsResponse -> TestTree
responseGetLogGroupFields = res
    "GetLogGroupFieldsResponse"
    "fixture/GetLogGroupFieldsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLogGroupFields)

responseGetLogRecord :: GetLogRecordResponse -> TestTree
responseGetLogRecord = res
    "GetLogRecordResponse"
    "fixture/GetLogRecordResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLogRecord)

responseDescribeDestinations :: DescribeDestinationsResponse -> TestTree
responseDescribeDestinations = res
    "DescribeDestinationsResponse"
    "fixture/DescribeDestinationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDestinations)

responseUntagLogGroup :: UntagLogGroupResponse -> TestTree
responseUntagLogGroup = res
    "UntagLogGroupResponse"
    "fixture/UntagLogGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagLogGroup)

responseStopQuery :: StopQueryResponse -> TestTree
responseStopQuery = res
    "StopQueryResponse"
    "fixture/StopQueryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopQuery)

responseCreateExportTask :: CreateExportTaskResponse -> TestTree
responseCreateExportTask = res
    "CreateExportTaskResponse"
    "fixture/CreateExportTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateExportTask)

responsePutDestination :: PutDestinationResponse -> TestTree
responsePutDestination = res
    "PutDestinationResponse"
    "fixture/PutDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutDestination)

responseDescribeSubscriptionFilters :: DescribeSubscriptionFiltersResponse -> TestTree
responseDescribeSubscriptionFilters = res
    "DescribeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSubscriptionFilters)

responseGetLogEvents :: GetLogEventsResponse -> TestTree
responseGetLogEvents = res
    "GetLogEventsResponse"
    "fixture/GetLogEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLogEvents)

responseDescribeLogGroups :: DescribeLogGroupsResponse -> TestTree
responseDescribeLogGroups = res
    "DescribeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLogGroups)

responseDeleteDestination :: DeleteDestinationResponse -> TestTree
responseDeleteDestination = res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDestination)

responseDisassociateKmsKey :: DisassociateKmsKeyResponse -> TestTree
responseDisassociateKmsKey = res
    "DisassociateKmsKeyResponse"
    "fixture/DisassociateKmsKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateKmsKey)

responseFilterLogEvents :: FilterLogEventsResponse -> TestTree
responseFilterLogEvents = res
    "FilterLogEventsResponse"
    "fixture/FilterLogEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy FilterLogEvents)

responseDeleteQueryDefinition :: DeleteQueryDefinitionResponse -> TestTree
responseDeleteQueryDefinition = res
    "DeleteQueryDefinitionResponse"
    "fixture/DeleteQueryDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteQueryDefinition)

responsePutQueryDefinition :: PutQueryDefinitionResponse -> TestTree
responsePutQueryDefinition = res
    "PutQueryDefinitionResponse"
    "fixture/PutQueryDefinitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutQueryDefinition)

responseTagLogGroup :: TagLogGroupResponse -> TestTree
responseTagLogGroup = res
    "TagLogGroupResponse"
    "fixture/TagLogGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagLogGroup)

responseDescribeResourcePolicies :: DescribeResourcePoliciesResponse -> TestTree
responseDescribeResourcePolicies = res
    "DescribeResourcePoliciesResponse"
    "fixture/DescribeResourcePoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeResourcePolicies)

responseDescribeQueryDefinitions :: DescribeQueryDefinitionsResponse -> TestTree
responseDescribeQueryDefinitions = res
    "DescribeQueryDefinitionsResponse"
    "fixture/DescribeQueryDefinitionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeQueryDefinitions)

responseDeleteLogStream :: DeleteLogStreamResponse -> TestTree
responseDeleteLogStream = res
    "DeleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLogStream)

responseDescribeQueries :: DescribeQueriesResponse -> TestTree
responseDescribeQueries = res
    "DescribeQueriesResponse"
    "fixture/DescribeQueriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeQueries)

responseCreateLogStream :: CreateLogStreamResponse -> TestTree
responseCreateLogStream = res
    "CreateLogStreamResponse"
    "fixture/CreateLogStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLogStream)

responseCreateLogGroup :: CreateLogGroupResponse -> TestTree
responseCreateLogGroup = res
    "CreateLogGroupResponse"
    "fixture/CreateLogGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLogGroup)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks = res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeExportTasks)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask = res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelExportTask)

responsePutSubscriptionFilter :: PutSubscriptionFilterResponse -> TestTree
responsePutSubscriptionFilter = res
    "PutSubscriptionFilterResponse"
    "fixture/PutSubscriptionFilterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutSubscriptionFilter)

responseStartQuery :: StartQueryResponse -> TestTree
responseStartQuery = res
    "StartQueryResponse"
    "fixture/StartQueryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartQuery)

responseDeleteLogGroup :: DeleteLogGroupResponse -> TestTree
responseDeleteLogGroup = res
    "DeleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLogGroup)

responseDeleteSubscriptionFilter :: DeleteSubscriptionFilterResponse -> TestTree
responseDeleteSubscriptionFilter = res
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSubscriptionFilter)

responsePutLogEvents :: PutLogEventsResponse -> TestTree
responsePutLogEvents = res
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutLogEvents)

responseDescribeMetricFilters :: DescribeMetricFiltersResponse -> TestTree
responseDescribeMetricFilters = res
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMetricFilters)

responseTestMetricFilter :: TestMetricFilterResponse -> TestTree
responseTestMetricFilter = res
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TestMetricFilter)

responsePutDestinationPolicy :: PutDestinationPolicyResponse -> TestTree
responsePutDestinationPolicy = res
    "PutDestinationPolicyResponse"
    "fixture/PutDestinationPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutDestinationPolicy)

responsePutMetricFilter :: PutMetricFilterResponse -> TestTree
responsePutMetricFilter = res
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutMetricFilter)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy = res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRetentionPolicy)

responseDeleteMetricFilter :: DeleteMetricFilterResponse -> TestTree
responseDeleteMetricFilter = res
    "DeleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMetricFilter)

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy = res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutRetentionPolicy)

responseListTagsLogGroup :: ListTagsLogGroupResponse -> TestTree
responseListTagsLogGroup = res
    "ListTagsLogGroupResponse"
    "fixture/ListTagsLogGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsLogGroup)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy = res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy = res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteResourcePolicy)

responseAssociateKmsKey :: AssociateKmsKeyResponse -> TestTree
responseAssociateKmsKey = res
    "AssociateKmsKeyResponse"
    "fixture/AssociateKmsKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateKmsKey)

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults = res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetQueryResults)

responseDescribeLogStreams :: DescribeLogStreamsResponse -> TestTree
responseDescribeLogStreams = res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLogStreams)

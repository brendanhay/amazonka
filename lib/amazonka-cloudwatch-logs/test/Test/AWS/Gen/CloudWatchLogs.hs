{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         , requestDisassociateKMSKey $
--             mkDisassociateKMSKey
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
--         , requestAssociateKMSKey $
--             mkAssociateKMSKey
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
--         , responseDisassociateKMSKey $
--             mkDisassociateKMSKeyResponse
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
--         , responseAssociateKMSKey $
--             mkAssociateKMSKeyResponse
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
requestGetLogGroupFields =
  req
    "GetLogGroupFields"
    "fixture/GetLogGroupFields.yaml"

requestGetLogRecord :: GetLogRecord -> TestTree
requestGetLogRecord =
  req
    "GetLogRecord"
    "fixture/GetLogRecord.yaml"

requestDescribeDestinations :: DescribeDestinations -> TestTree
requestDescribeDestinations =
  req
    "DescribeDestinations"
    "fixture/DescribeDestinations.yaml"

requestUntagLogGroup :: UntagLogGroup -> TestTree
requestUntagLogGroup =
  req
    "UntagLogGroup"
    "fixture/UntagLogGroup.yaml"

requestStopQuery :: StopQuery -> TestTree
requestStopQuery =
  req
    "StopQuery"
    "fixture/StopQuery.yaml"

requestCreateExportTask :: CreateExportTask -> TestTree
requestCreateExportTask =
  req
    "CreateExportTask"
    "fixture/CreateExportTask.yaml"

requestPutDestination :: PutDestination -> TestTree
requestPutDestination =
  req
    "PutDestination"
    "fixture/PutDestination.yaml"

requestDescribeSubscriptionFilters :: DescribeSubscriptionFilters -> TestTree
requestDescribeSubscriptionFilters =
  req
    "DescribeSubscriptionFilters"
    "fixture/DescribeSubscriptionFilters.yaml"

requestGetLogEvents :: GetLogEvents -> TestTree
requestGetLogEvents =
  req
    "GetLogEvents"
    "fixture/GetLogEvents.yaml"

requestDescribeLogGroups :: DescribeLogGroups -> TestTree
requestDescribeLogGroups =
  req
    "DescribeLogGroups"
    "fixture/DescribeLogGroups.yaml"

requestDeleteDestination :: DeleteDestination -> TestTree
requestDeleteDestination =
  req
    "DeleteDestination"
    "fixture/DeleteDestination.yaml"

requestDisassociateKMSKey :: DisassociateKMSKey -> TestTree
requestDisassociateKMSKey =
  req
    "DisassociateKMSKey"
    "fixture/DisassociateKMSKey.yaml"

requestFilterLogEvents :: FilterLogEvents -> TestTree
requestFilterLogEvents =
  req
    "FilterLogEvents"
    "fixture/FilterLogEvents.yaml"

requestDeleteQueryDefinition :: DeleteQueryDefinition -> TestTree
requestDeleteQueryDefinition =
  req
    "DeleteQueryDefinition"
    "fixture/DeleteQueryDefinition.yaml"

requestPutQueryDefinition :: PutQueryDefinition -> TestTree
requestPutQueryDefinition =
  req
    "PutQueryDefinition"
    "fixture/PutQueryDefinition.yaml"

requestTagLogGroup :: TagLogGroup -> TestTree
requestTagLogGroup =
  req
    "TagLogGroup"
    "fixture/TagLogGroup.yaml"

requestDescribeResourcePolicies :: DescribeResourcePolicies -> TestTree
requestDescribeResourcePolicies =
  req
    "DescribeResourcePolicies"
    "fixture/DescribeResourcePolicies.yaml"

requestDescribeQueryDefinitions :: DescribeQueryDefinitions -> TestTree
requestDescribeQueryDefinitions =
  req
    "DescribeQueryDefinitions"
    "fixture/DescribeQueryDefinitions.yaml"

requestDeleteLogStream :: DeleteLogStream -> TestTree
requestDeleteLogStream =
  req
    "DeleteLogStream"
    "fixture/DeleteLogStream.yaml"

requestDescribeQueries :: DescribeQueries -> TestTree
requestDescribeQueries =
  req
    "DescribeQueries"
    "fixture/DescribeQueries.yaml"

requestCreateLogStream :: CreateLogStream -> TestTree
requestCreateLogStream =
  req
    "CreateLogStream"
    "fixture/CreateLogStream.yaml"

requestCreateLogGroup :: CreateLogGroup -> TestTree
requestCreateLogGroup =
  req
    "CreateLogGroup"
    "fixture/CreateLogGroup.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestPutSubscriptionFilter :: PutSubscriptionFilter -> TestTree
requestPutSubscriptionFilter =
  req
    "PutSubscriptionFilter"
    "fixture/PutSubscriptionFilter.yaml"

requestStartQuery :: StartQuery -> TestTree
requestStartQuery =
  req
    "StartQuery"
    "fixture/StartQuery.yaml"

requestDeleteLogGroup :: DeleteLogGroup -> TestTree
requestDeleteLogGroup =
  req
    "DeleteLogGroup"
    "fixture/DeleteLogGroup.yaml"

requestDeleteSubscriptionFilter :: DeleteSubscriptionFilter -> TestTree
requestDeleteSubscriptionFilter =
  req
    "DeleteSubscriptionFilter"
    "fixture/DeleteSubscriptionFilter.yaml"

requestPutLogEvents :: PutLogEvents -> TestTree
requestPutLogEvents =
  req
    "PutLogEvents"
    "fixture/PutLogEvents.yaml"

requestDescribeMetricFilters :: DescribeMetricFilters -> TestTree
requestDescribeMetricFilters =
  req
    "DescribeMetricFilters"
    "fixture/DescribeMetricFilters.yaml"

requestTestMetricFilter :: TestMetricFilter -> TestTree
requestTestMetricFilter =
  req
    "TestMetricFilter"
    "fixture/TestMetricFilter.yaml"

requestPutDestinationPolicy :: PutDestinationPolicy -> TestTree
requestPutDestinationPolicy =
  req
    "PutDestinationPolicy"
    "fixture/PutDestinationPolicy.yaml"

requestPutMetricFilter :: PutMetricFilter -> TestTree
requestPutMetricFilter =
  req
    "PutMetricFilter"
    "fixture/PutMetricFilter.yaml"

requestDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
requestDeleteRetentionPolicy =
  req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy.yaml"

requestDeleteMetricFilter :: DeleteMetricFilter -> TestTree
requestDeleteMetricFilter =
  req
    "DeleteMetricFilter"
    "fixture/DeleteMetricFilter.yaml"

requestPutRetentionPolicy :: PutRetentionPolicy -> TestTree
requestPutRetentionPolicy =
  req
    "PutRetentionPolicy"
    "fixture/PutRetentionPolicy.yaml"

requestListTagsLogGroup :: ListTagsLogGroup -> TestTree
requestListTagsLogGroup =
  req
    "ListTagsLogGroup"
    "fixture/ListTagsLogGroup.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestAssociateKMSKey :: AssociateKMSKey -> TestTree
requestAssociateKMSKey =
  req
    "AssociateKMSKey"
    "fixture/AssociateKMSKey.yaml"

requestGetQueryResults :: GetQueryResults -> TestTree
requestGetQueryResults =
  req
    "GetQueryResults"
    "fixture/GetQueryResults.yaml"

requestDescribeLogStreams :: DescribeLogStreams -> TestTree
requestDescribeLogStreams =
  req
    "DescribeLogStreams"
    "fixture/DescribeLogStreams.yaml"

-- Responses

responseGetLogGroupFields :: GetLogGroupFieldsResponse -> TestTree
responseGetLogGroupFields =
  res
    "GetLogGroupFieldsResponse"
    "fixture/GetLogGroupFieldsResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy GetLogGroupFields)

responseGetLogRecord :: GetLogRecordResponse -> TestTree
responseGetLogRecord =
  res
    "GetLogRecordResponse"
    "fixture/GetLogRecordResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy GetLogRecord)

responseDescribeDestinations :: DescribeDestinationsResponse -> TestTree
responseDescribeDestinations =
  res
    "DescribeDestinationsResponse"
    "fixture/DescribeDestinationsResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DescribeDestinations)

responseUntagLogGroup :: UntagLogGroupResponse -> TestTree
responseUntagLogGroup =
  res
    "UntagLogGroupResponse"
    "fixture/UntagLogGroupResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy UntagLogGroup)

responseStopQuery :: StopQueryResponse -> TestTree
responseStopQuery =
  res
    "StopQueryResponse"
    "fixture/StopQueryResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy StopQuery)

responseCreateExportTask :: CreateExportTaskResponse -> TestTree
responseCreateExportTask =
  res
    "CreateExportTaskResponse"
    "fixture/CreateExportTaskResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy CreateExportTask)

responsePutDestination :: PutDestinationResponse -> TestTree
responsePutDestination =
  res
    "PutDestinationResponse"
    "fixture/PutDestinationResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy PutDestination)

responseDescribeSubscriptionFilters :: DescribeSubscriptionFiltersResponse -> TestTree
responseDescribeSubscriptionFilters =
  res
    "DescribeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DescribeSubscriptionFilters)

responseGetLogEvents :: GetLogEventsResponse -> TestTree
responseGetLogEvents =
  res
    "GetLogEventsResponse"
    "fixture/GetLogEventsResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy GetLogEvents)

responseDescribeLogGroups :: DescribeLogGroupsResponse -> TestTree
responseDescribeLogGroups =
  res
    "DescribeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DescribeLogGroups)

responseDeleteDestination :: DeleteDestinationResponse -> TestTree
responseDeleteDestination =
  res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DeleteDestination)

responseDisassociateKMSKey :: DisassociateKMSKeyResponse -> TestTree
responseDisassociateKMSKey =
  res
    "DisassociateKMSKeyResponse"
    "fixture/DisassociateKMSKeyResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DisassociateKMSKey)

responseFilterLogEvents :: FilterLogEventsResponse -> TestTree
responseFilterLogEvents =
  res
    "FilterLogEventsResponse"
    "fixture/FilterLogEventsResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy FilterLogEvents)

responseDeleteQueryDefinition :: DeleteQueryDefinitionResponse -> TestTree
responseDeleteQueryDefinition =
  res
    "DeleteQueryDefinitionResponse"
    "fixture/DeleteQueryDefinitionResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DeleteQueryDefinition)

responsePutQueryDefinition :: PutQueryDefinitionResponse -> TestTree
responsePutQueryDefinition =
  res
    "PutQueryDefinitionResponse"
    "fixture/PutQueryDefinitionResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy PutQueryDefinition)

responseTagLogGroup :: TagLogGroupResponse -> TestTree
responseTagLogGroup =
  res
    "TagLogGroupResponse"
    "fixture/TagLogGroupResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy TagLogGroup)

responseDescribeResourcePolicies :: DescribeResourcePoliciesResponse -> TestTree
responseDescribeResourcePolicies =
  res
    "DescribeResourcePoliciesResponse"
    "fixture/DescribeResourcePoliciesResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DescribeResourcePolicies)

responseDescribeQueryDefinitions :: DescribeQueryDefinitionsResponse -> TestTree
responseDescribeQueryDefinitions =
  res
    "DescribeQueryDefinitionsResponse"
    "fixture/DescribeQueryDefinitionsResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DescribeQueryDefinitions)

responseDeleteLogStream :: DeleteLogStreamResponse -> TestTree
responseDeleteLogStream =
  res
    "DeleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DeleteLogStream)

responseDescribeQueries :: DescribeQueriesResponse -> TestTree
responseDescribeQueries =
  res
    "DescribeQueriesResponse"
    "fixture/DescribeQueriesResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DescribeQueries)

responseCreateLogStream :: CreateLogStreamResponse -> TestTree
responseCreateLogStream =
  res
    "CreateLogStreamResponse"
    "fixture/CreateLogStreamResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy CreateLogStream)

responseCreateLogGroup :: CreateLogGroupResponse -> TestTree
responseCreateLogGroup =
  res
    "CreateLogGroupResponse"
    "fixture/CreateLogGroupResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy CreateLogGroup)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DescribeExportTasks)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy CancelExportTask)

responsePutSubscriptionFilter :: PutSubscriptionFilterResponse -> TestTree
responsePutSubscriptionFilter =
  res
    "PutSubscriptionFilterResponse"
    "fixture/PutSubscriptionFilterResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy PutSubscriptionFilter)

responseStartQuery :: StartQueryResponse -> TestTree
responseStartQuery =
  res
    "StartQueryResponse"
    "fixture/StartQueryResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy StartQuery)

responseDeleteLogGroup :: DeleteLogGroupResponse -> TestTree
responseDeleteLogGroup =
  res
    "DeleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DeleteLogGroup)

responseDeleteSubscriptionFilter :: DeleteSubscriptionFilterResponse -> TestTree
responseDeleteSubscriptionFilter =
  res
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DeleteSubscriptionFilter)

responsePutLogEvents :: PutLogEventsResponse -> TestTree
responsePutLogEvents =
  res
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy PutLogEvents)

responseDescribeMetricFilters :: DescribeMetricFiltersResponse -> TestTree
responseDescribeMetricFilters =
  res
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DescribeMetricFilters)

responseTestMetricFilter :: TestMetricFilterResponse -> TestTree
responseTestMetricFilter =
  res
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy TestMetricFilter)

responsePutDestinationPolicy :: PutDestinationPolicyResponse -> TestTree
responsePutDestinationPolicy =
  res
    "PutDestinationPolicyResponse"
    "fixture/PutDestinationPolicyResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy PutDestinationPolicy)

responsePutMetricFilter :: PutMetricFilterResponse -> TestTree
responsePutMetricFilter =
  res
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy PutMetricFilter)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy =
  res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DeleteRetentionPolicy)

responseDeleteMetricFilter :: DeleteMetricFilterResponse -> TestTree
responseDeleteMetricFilter =
  res
    "DeleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DeleteMetricFilter)

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy =
  res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy PutRetentionPolicy)

responseListTagsLogGroup :: ListTagsLogGroupResponse -> TestTree
responseListTagsLogGroup =
  res
    "ListTagsLogGroupResponse"
    "fixture/ListTagsLogGroupResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy ListTagsLogGroup)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DeleteResourcePolicy)

responseAssociateKMSKey :: AssociateKMSKeyResponse -> TestTree
responseAssociateKMSKey =
  res
    "AssociateKMSKeyResponse"
    "fixture/AssociateKMSKeyResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy AssociateKMSKey)

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults =
  res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy GetQueryResults)

responseDescribeLogStreams :: DescribeLogStreamsResponse -> TestTree
responseDescribeLogStreams =
  res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse.proto"
    cloudWatchLogsService
    (Proxy :: Proxy DescribeLogStreams)

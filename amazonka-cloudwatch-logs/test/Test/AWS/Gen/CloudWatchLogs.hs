{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudWatchLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestGetLogRecord $
--             newGetLogRecord
--
--         , requestCreateLogStream $
--             newCreateLogStream
--
--         , requestDescribeResourcePolicies $
--             newDescribeResourcePolicies
--
--         , requestDescribeQueryDefinitions $
--             newDescribeQueryDefinitions
--
--         , requestDescribeLogStreams $
--             newDescribeLogStreams
--
--         , requestDeleteQueryDefinition $
--             newDeleteQueryDefinition
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestDisassociateKmsKey $
--             newDisassociateKmsKey
--
--         , requestDeleteDestination $
--             newDeleteDestination
--
--         , requestDescribeSubscriptionFilters $
--             newDescribeSubscriptionFilters
--
--         , requestPutDestinationPolicy $
--             newPutDestinationPolicy
--
--         , requestPutRetentionPolicy $
--             newPutRetentionPolicy
--
--         , requestDeleteMetricFilter $
--             newDeleteMetricFilter
--
--         , requestDescribeLogGroups $
--             newDescribeLogGroups
--
--         , requestDescribeMetricFilters $
--             newDescribeMetricFilters
--
--         , requestPutSubscriptionFilter $
--             newPutSubscriptionFilter
--
--         , requestDescribeDestinations $
--             newDescribeDestinations
--
--         , requestCreateLogGroup $
--             newCreateLogGroup
--
--         , requestCancelExportTask $
--             newCancelExportTask
--
--         , requestGetLogGroupFields $
--             newGetLogGroupFields
--
--         , requestDeleteLogStream $
--             newDeleteLogStream
--
--         , requestTagLogGroup $
--             newTagLogGroup
--
--         , requestDescribeQueries $
--             newDescribeQueries
--
--         , requestAssociateKmsKey $
--             newAssociateKmsKey
--
--         , requestGetQueryResults $
--             newGetQueryResults
--
--         , requestPutQueryDefinition $
--             newPutQueryDefinition
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestFilterLogEvents $
--             newFilterLogEvents
--
--         , requestListTagsLogGroup $
--             newListTagsLogGroup
--
--         , requestGetLogEvents $
--             newGetLogEvents
--
--         , requestCreateExportTask $
--             newCreateExportTask
--
--         , requestDeleteRetentionPolicy $
--             newDeleteRetentionPolicy
--
--         , requestPutDestination $
--             newPutDestination
--
--         , requestPutMetricFilter $
--             newPutMetricFilter
--
--         , requestStartQuery $
--             newStartQuery
--
--         , requestPutLogEvents $
--             newPutLogEvents
--
--         , requestDeleteLogGroup $
--             newDeleteLogGroup
--
--         , requestTestMetricFilter $
--             newTestMetricFilter
--
--         , requestDeleteSubscriptionFilter $
--             newDeleteSubscriptionFilter
--
--         , requestUntagLogGroup $
--             newUntagLogGroup
--
--         , requestStopQuery $
--             newStopQuery
--
--           ]

--     , testGroup "response"
--         [ responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseGetLogRecord $
--             newGetLogRecordResponse
--
--         , responseCreateLogStream $
--             newCreateLogStreamResponse
--
--         , responseDescribeResourcePolicies $
--             newDescribeResourcePoliciesResponse
--
--         , responseDescribeQueryDefinitions $
--             newDescribeQueryDefinitionsResponse
--
--         , responseDescribeLogStreams $
--             newDescribeLogStreamsResponse
--
--         , responseDeleteQueryDefinition $
--             newDeleteQueryDefinitionResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseDisassociateKmsKey $
--             newDisassociateKmsKeyResponse
--
--         , responseDeleteDestination $
--             newDeleteDestinationResponse
--
--         , responseDescribeSubscriptionFilters $
--             newDescribeSubscriptionFiltersResponse
--
--         , responsePutDestinationPolicy $
--             newPutDestinationPolicyResponse
--
--         , responsePutRetentionPolicy $
--             newPutRetentionPolicyResponse
--
--         , responseDeleteMetricFilter $
--             newDeleteMetricFilterResponse
--
--         , responseDescribeLogGroups $
--             newDescribeLogGroupsResponse
--
--         , responseDescribeMetricFilters $
--             newDescribeMetricFiltersResponse
--
--         , responsePutSubscriptionFilter $
--             newPutSubscriptionFilterResponse
--
--         , responseDescribeDestinations $
--             newDescribeDestinationsResponse
--
--         , responseCreateLogGroup $
--             newCreateLogGroupResponse
--
--         , responseCancelExportTask $
--             newCancelExportTaskResponse
--
--         , responseGetLogGroupFields $
--             newGetLogGroupFieldsResponse
--
--         , responseDeleteLogStream $
--             newDeleteLogStreamResponse
--
--         , responseTagLogGroup $
--             newTagLogGroupResponse
--
--         , responseDescribeQueries $
--             newDescribeQueriesResponse
--
--         , responseAssociateKmsKey $
--             newAssociateKmsKeyResponse
--
--         , responseGetQueryResults $
--             newGetQueryResultsResponse
--
--         , responsePutQueryDefinition $
--             newPutQueryDefinitionResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseFilterLogEvents $
--             newFilterLogEventsResponse
--
--         , responseListTagsLogGroup $
--             newListTagsLogGroupResponse
--
--         , responseGetLogEvents $
--             newGetLogEventsResponse
--
--         , responseCreateExportTask $
--             newCreateExportTaskResponse
--
--         , responseDeleteRetentionPolicy $
--             newDeleteRetentionPolicyResponse
--
--         , responsePutDestination $
--             newPutDestinationResponse
--
--         , responsePutMetricFilter $
--             newPutMetricFilterResponse
--
--         , responseStartQuery $
--             newStartQueryResponse
--
--         , responsePutLogEvents $
--             newPutLogEventsResponse
--
--         , responseDeleteLogGroup $
--             newDeleteLogGroupResponse
--
--         , responseTestMetricFilter $
--             newTestMetricFilterResponse
--
--         , responseDeleteSubscriptionFilter $
--             newDeleteSubscriptionFilterResponse
--
--         , responseUntagLogGroup $
--             newUntagLogGroupResponse
--
--         , responseStopQuery $
--             newStopQueryResponse
--
--           ]
--     ]

-- Requests

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestGetLogRecord :: GetLogRecord -> TestTree
requestGetLogRecord =
  req
    "GetLogRecord"
    "fixture/GetLogRecord.yaml"

requestCreateLogStream :: CreateLogStream -> TestTree
requestCreateLogStream =
  req
    "CreateLogStream"
    "fixture/CreateLogStream.yaml"

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

requestDescribeLogStreams :: DescribeLogStreams -> TestTree
requestDescribeLogStreams =
  req
    "DescribeLogStreams"
    "fixture/DescribeLogStreams.yaml"

requestDeleteQueryDefinition :: DeleteQueryDefinition -> TestTree
requestDeleteQueryDefinition =
  req
    "DeleteQueryDefinition"
    "fixture/DeleteQueryDefinition.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestDisassociateKmsKey :: DisassociateKmsKey -> TestTree
requestDisassociateKmsKey =
  req
    "DisassociateKmsKey"
    "fixture/DisassociateKmsKey.yaml"

requestDeleteDestination :: DeleteDestination -> TestTree
requestDeleteDestination =
  req
    "DeleteDestination"
    "fixture/DeleteDestination.yaml"

requestDescribeSubscriptionFilters :: DescribeSubscriptionFilters -> TestTree
requestDescribeSubscriptionFilters =
  req
    "DescribeSubscriptionFilters"
    "fixture/DescribeSubscriptionFilters.yaml"

requestPutDestinationPolicy :: PutDestinationPolicy -> TestTree
requestPutDestinationPolicy =
  req
    "PutDestinationPolicy"
    "fixture/PutDestinationPolicy.yaml"

requestPutRetentionPolicy :: PutRetentionPolicy -> TestTree
requestPutRetentionPolicy =
  req
    "PutRetentionPolicy"
    "fixture/PutRetentionPolicy.yaml"

requestDeleteMetricFilter :: DeleteMetricFilter -> TestTree
requestDeleteMetricFilter =
  req
    "DeleteMetricFilter"
    "fixture/DeleteMetricFilter.yaml"

requestDescribeLogGroups :: DescribeLogGroups -> TestTree
requestDescribeLogGroups =
  req
    "DescribeLogGroups"
    "fixture/DescribeLogGroups.yaml"

requestDescribeMetricFilters :: DescribeMetricFilters -> TestTree
requestDescribeMetricFilters =
  req
    "DescribeMetricFilters"
    "fixture/DescribeMetricFilters.yaml"

requestPutSubscriptionFilter :: PutSubscriptionFilter -> TestTree
requestPutSubscriptionFilter =
  req
    "PutSubscriptionFilter"
    "fixture/PutSubscriptionFilter.yaml"

requestDescribeDestinations :: DescribeDestinations -> TestTree
requestDescribeDestinations =
  req
    "DescribeDestinations"
    "fixture/DescribeDestinations.yaml"

requestCreateLogGroup :: CreateLogGroup -> TestTree
requestCreateLogGroup =
  req
    "CreateLogGroup"
    "fixture/CreateLogGroup.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestGetLogGroupFields :: GetLogGroupFields -> TestTree
requestGetLogGroupFields =
  req
    "GetLogGroupFields"
    "fixture/GetLogGroupFields.yaml"

requestDeleteLogStream :: DeleteLogStream -> TestTree
requestDeleteLogStream =
  req
    "DeleteLogStream"
    "fixture/DeleteLogStream.yaml"

requestTagLogGroup :: TagLogGroup -> TestTree
requestTagLogGroup =
  req
    "TagLogGroup"
    "fixture/TagLogGroup.yaml"

requestDescribeQueries :: DescribeQueries -> TestTree
requestDescribeQueries =
  req
    "DescribeQueries"
    "fixture/DescribeQueries.yaml"

requestAssociateKmsKey :: AssociateKmsKey -> TestTree
requestAssociateKmsKey =
  req
    "AssociateKmsKey"
    "fixture/AssociateKmsKey.yaml"

requestGetQueryResults :: GetQueryResults -> TestTree
requestGetQueryResults =
  req
    "GetQueryResults"
    "fixture/GetQueryResults.yaml"

requestPutQueryDefinition :: PutQueryDefinition -> TestTree
requestPutQueryDefinition =
  req
    "PutQueryDefinition"
    "fixture/PutQueryDefinition.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestFilterLogEvents :: FilterLogEvents -> TestTree
requestFilterLogEvents =
  req
    "FilterLogEvents"
    "fixture/FilterLogEvents.yaml"

requestListTagsLogGroup :: ListTagsLogGroup -> TestTree
requestListTagsLogGroup =
  req
    "ListTagsLogGroup"
    "fixture/ListTagsLogGroup.yaml"

requestGetLogEvents :: GetLogEvents -> TestTree
requestGetLogEvents =
  req
    "GetLogEvents"
    "fixture/GetLogEvents.yaml"

requestCreateExportTask :: CreateExportTask -> TestTree
requestCreateExportTask =
  req
    "CreateExportTask"
    "fixture/CreateExportTask.yaml"

requestDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
requestDeleteRetentionPolicy =
  req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy.yaml"

requestPutDestination :: PutDestination -> TestTree
requestPutDestination =
  req
    "PutDestination"
    "fixture/PutDestination.yaml"

requestPutMetricFilter :: PutMetricFilter -> TestTree
requestPutMetricFilter =
  req
    "PutMetricFilter"
    "fixture/PutMetricFilter.yaml"

requestStartQuery :: StartQuery -> TestTree
requestStartQuery =
  req
    "StartQuery"
    "fixture/StartQuery.yaml"

requestPutLogEvents :: PutLogEvents -> TestTree
requestPutLogEvents =
  req
    "PutLogEvents"
    "fixture/PutLogEvents.yaml"

requestDeleteLogGroup :: DeleteLogGroup -> TestTree
requestDeleteLogGroup =
  req
    "DeleteLogGroup"
    "fixture/DeleteLogGroup.yaml"

requestTestMetricFilter :: TestMetricFilter -> TestTree
requestTestMetricFilter =
  req
    "TestMetricFilter"
    "fixture/TestMetricFilter.yaml"

requestDeleteSubscriptionFilter :: DeleteSubscriptionFilter -> TestTree
requestDeleteSubscriptionFilter =
  req
    "DeleteSubscriptionFilter"
    "fixture/DeleteSubscriptionFilter.yaml"

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

-- Responses

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExportTasks)

responseGetLogRecord :: GetLogRecordResponse -> TestTree
responseGetLogRecord =
  res
    "GetLogRecordResponse"
    "fixture/GetLogRecordResponse.proto"
    defaultService
    (Proxy :: Proxy GetLogRecord)

responseCreateLogStream :: CreateLogStreamResponse -> TestTree
responseCreateLogStream =
  res
    "CreateLogStreamResponse"
    "fixture/CreateLogStreamResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLogStream)

responseDescribeResourcePolicies :: DescribeResourcePoliciesResponse -> TestTree
responseDescribeResourcePolicies =
  res
    "DescribeResourcePoliciesResponse"
    "fixture/DescribeResourcePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResourcePolicies)

responseDescribeQueryDefinitions :: DescribeQueryDefinitionsResponse -> TestTree
responseDescribeQueryDefinitions =
  res
    "DescribeQueryDefinitionsResponse"
    "fixture/DescribeQueryDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeQueryDefinitions)

responseDescribeLogStreams :: DescribeLogStreamsResponse -> TestTree
responseDescribeLogStreams =
  res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLogStreams)

responseDeleteQueryDefinition :: DeleteQueryDefinitionResponse -> TestTree
responseDeleteQueryDefinition =
  res
    "DeleteQueryDefinitionResponse"
    "fixture/DeleteQueryDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteQueryDefinition)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutResourcePolicy)

responseDisassociateKmsKey :: DisassociateKmsKeyResponse -> TestTree
responseDisassociateKmsKey =
  res
    "DisassociateKmsKeyResponse"
    "fixture/DisassociateKmsKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateKmsKey)

responseDeleteDestination :: DeleteDestinationResponse -> TestTree
responseDeleteDestination =
  res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDestination)

responseDescribeSubscriptionFilters :: DescribeSubscriptionFiltersResponse -> TestTree
responseDescribeSubscriptionFilters =
  res
    "DescribeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSubscriptionFilters)

responsePutDestinationPolicy :: PutDestinationPolicyResponse -> TestTree
responsePutDestinationPolicy =
  res
    "PutDestinationPolicyResponse"
    "fixture/PutDestinationPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutDestinationPolicy)

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy =
  res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutRetentionPolicy)

responseDeleteMetricFilter :: DeleteMetricFilterResponse -> TestTree
responseDeleteMetricFilter =
  res
    "DeleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMetricFilter)

responseDescribeLogGroups :: DescribeLogGroupsResponse -> TestTree
responseDescribeLogGroups =
  res
    "DescribeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLogGroups)

responseDescribeMetricFilters :: DescribeMetricFiltersResponse -> TestTree
responseDescribeMetricFilters =
  res
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMetricFilters)

responsePutSubscriptionFilter :: PutSubscriptionFilterResponse -> TestTree
responsePutSubscriptionFilter =
  res
    "PutSubscriptionFilterResponse"
    "fixture/PutSubscriptionFilterResponse.proto"
    defaultService
    (Proxy :: Proxy PutSubscriptionFilter)

responseDescribeDestinations :: DescribeDestinationsResponse -> TestTree
responseDescribeDestinations =
  res
    "DescribeDestinationsResponse"
    "fixture/DescribeDestinationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDestinations)

responseCreateLogGroup :: CreateLogGroupResponse -> TestTree
responseCreateLogGroup =
  res
    "CreateLogGroupResponse"
    "fixture/CreateLogGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLogGroup)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CancelExportTask)

responseGetLogGroupFields :: GetLogGroupFieldsResponse -> TestTree
responseGetLogGroupFields =
  res
    "GetLogGroupFieldsResponse"
    "fixture/GetLogGroupFieldsResponse.proto"
    defaultService
    (Proxy :: Proxy GetLogGroupFields)

responseDeleteLogStream :: DeleteLogStreamResponse -> TestTree
responseDeleteLogStream =
  res
    "DeleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLogStream)

responseTagLogGroup :: TagLogGroupResponse -> TestTree
responseTagLogGroup =
  res
    "TagLogGroupResponse"
    "fixture/TagLogGroupResponse.proto"
    defaultService
    (Proxy :: Proxy TagLogGroup)

responseDescribeQueries :: DescribeQueriesResponse -> TestTree
responseDescribeQueries =
  res
    "DescribeQueriesResponse"
    "fixture/DescribeQueriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeQueries)

responseAssociateKmsKey :: AssociateKmsKeyResponse -> TestTree
responseAssociateKmsKey =
  res
    "AssociateKmsKeyResponse"
    "fixture/AssociateKmsKeyResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateKmsKey)

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults =
  res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    defaultService
    (Proxy :: Proxy GetQueryResults)

responsePutQueryDefinition :: PutQueryDefinitionResponse -> TestTree
responsePutQueryDefinition =
  res
    "PutQueryDefinitionResponse"
    "fixture/PutQueryDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy PutQueryDefinition)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourcePolicy)

responseFilterLogEvents :: FilterLogEventsResponse -> TestTree
responseFilterLogEvents =
  res
    "FilterLogEventsResponse"
    "fixture/FilterLogEventsResponse.proto"
    defaultService
    (Proxy :: Proxy FilterLogEvents)

responseListTagsLogGroup :: ListTagsLogGroupResponse -> TestTree
responseListTagsLogGroup =
  res
    "ListTagsLogGroupResponse"
    "fixture/ListTagsLogGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsLogGroup)

responseGetLogEvents :: GetLogEventsResponse -> TestTree
responseGetLogEvents =
  res
    "GetLogEventsResponse"
    "fixture/GetLogEventsResponse.proto"
    defaultService
    (Proxy :: Proxy GetLogEvents)

responseCreateExportTask :: CreateExportTaskResponse -> TestTree
responseCreateExportTask =
  res
    "CreateExportTaskResponse"
    "fixture/CreateExportTaskResponse.proto"
    defaultService
    (Proxy :: Proxy CreateExportTask)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy =
  res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRetentionPolicy)

responsePutDestination :: PutDestinationResponse -> TestTree
responsePutDestination =
  res
    "PutDestinationResponse"
    "fixture/PutDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy PutDestination)

responsePutMetricFilter :: PutMetricFilterResponse -> TestTree
responsePutMetricFilter =
  res
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse.proto"
    defaultService
    (Proxy :: Proxy PutMetricFilter)

responseStartQuery :: StartQueryResponse -> TestTree
responseStartQuery =
  res
    "StartQueryResponse"
    "fixture/StartQueryResponse.proto"
    defaultService
    (Proxy :: Proxy StartQuery)

responsePutLogEvents :: PutLogEventsResponse -> TestTree
responsePutLogEvents =
  res
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse.proto"
    defaultService
    (Proxy :: Proxy PutLogEvents)

responseDeleteLogGroup :: DeleteLogGroupResponse -> TestTree
responseDeleteLogGroup =
  res
    "DeleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLogGroup)

responseTestMetricFilter :: TestMetricFilterResponse -> TestTree
responseTestMetricFilter =
  res
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse.proto"
    defaultService
    (Proxy :: Proxy TestMetricFilter)

responseDeleteSubscriptionFilter :: DeleteSubscriptionFilterResponse -> TestTree
responseDeleteSubscriptionFilter =
  res
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSubscriptionFilter)

responseUntagLogGroup :: UntagLogGroupResponse -> TestTree
responseUntagLogGroup =
  res
    "UntagLogGroupResponse"
    "fixture/UntagLogGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UntagLogGroup)

responseStopQuery :: StopQueryResponse -> TestTree
responseStopQuery =
  res
    "StopQueryResponse"
    "fixture/StopQueryResponse.proto"
    defaultService
    (Proxy :: Proxy StopQuery)

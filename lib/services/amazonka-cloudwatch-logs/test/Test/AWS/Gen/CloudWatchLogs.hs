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

import Amazonka.CloudWatchLogs
import qualified Data.Proxy as Proxy
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
--             newGetLogGroupFields
--
--         , requestGetLogRecord $
--             newGetLogRecord
--
--         , requestDescribeDestinations $
--             newDescribeDestinations
--
--         , requestUntagLogGroup $
--             newUntagLogGroup
--
--         , requestStopQuery $
--             newStopQuery
--
--         , requestCreateExportTask $
--             newCreateExportTask
--
--         , requestPutDestination $
--             newPutDestination
--
--         , requestDescribeSubscriptionFilters $
--             newDescribeSubscriptionFilters
--
--         , requestGetLogEvents $
--             newGetLogEvents
--
--         , requestDescribeLogGroups $
--             newDescribeLogGroups
--
--         , requestDeleteDestination $
--             newDeleteDestination
--
--         , requestDisassociateKmsKey $
--             newDisassociateKmsKey
--
--         , requestFilterLogEvents $
--             newFilterLogEvents
--
--         , requestDeleteQueryDefinition $
--             newDeleteQueryDefinition
--
--         , requestPutQueryDefinition $
--             newPutQueryDefinition
--
--         , requestTagLogGroup $
--             newTagLogGroup
--
--         , requestDescribeResourcePolicies $
--             newDescribeResourcePolicies
--
--         , requestDescribeQueryDefinitions $
--             newDescribeQueryDefinitions
--
--         , requestDeleteLogStream $
--             newDeleteLogStream
--
--         , requestDescribeQueries $
--             newDescribeQueries
--
--         , requestCreateLogStream $
--             newCreateLogStream
--
--         , requestCreateLogGroup $
--             newCreateLogGroup
--
--         , requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestCancelExportTask $
--             newCancelExportTask
--
--         , requestPutSubscriptionFilter $
--             newPutSubscriptionFilter
--
--         , requestStartQuery $
--             newStartQuery
--
--         , requestDeleteLogGroup $
--             newDeleteLogGroup
--
--         , requestDeleteSubscriptionFilter $
--             newDeleteSubscriptionFilter
--
--         , requestPutLogEvents $
--             newPutLogEvents
--
--         , requestDescribeMetricFilters $
--             newDescribeMetricFilters
--
--         , requestTestMetricFilter $
--             newTestMetricFilter
--
--         , requestPutDestinationPolicy $
--             newPutDestinationPolicy
--
--         , requestPutMetricFilter $
--             newPutMetricFilter
--
--         , requestDeleteRetentionPolicy $
--             newDeleteRetentionPolicy
--
--         , requestDeleteMetricFilter $
--             newDeleteMetricFilter
--
--         , requestPutRetentionPolicy $
--             newPutRetentionPolicy
--
--         , requestListTagsLogGroup $
--             newListTagsLogGroup
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestAssociateKmsKey $
--             newAssociateKmsKey
--
--         , requestGetQueryResults $
--             newGetQueryResults
--
--         , requestDescribeLogStreams $
--             newDescribeLogStreams
--
--           ]

--     , testGroup "response"
--         [ responseGetLogGroupFields $
--             newGetLogGroupFieldsResponse
--
--         , responseGetLogRecord $
--             newGetLogRecordResponse
--
--         , responseDescribeDestinations $
--             newDescribeDestinationsResponse
--
--         , responseUntagLogGroup $
--             newUntagLogGroupResponse
--
--         , responseStopQuery $
--             newStopQueryResponse
--
--         , responseCreateExportTask $
--             newCreateExportTaskResponse
--
--         , responsePutDestination $
--             newPutDestinationResponse
--
--         , responseDescribeSubscriptionFilters $
--             newDescribeSubscriptionFiltersResponse
--
--         , responseGetLogEvents $
--             newGetLogEventsResponse
--
--         , responseDescribeLogGroups $
--             newDescribeLogGroupsResponse
--
--         , responseDeleteDestination $
--             newDeleteDestinationResponse
--
--         , responseDisassociateKmsKey $
--             newDisassociateKmsKeyResponse
--
--         , responseFilterLogEvents $
--             newFilterLogEventsResponse
--
--         , responseDeleteQueryDefinition $
--             newDeleteQueryDefinitionResponse
--
--         , responsePutQueryDefinition $
--             newPutQueryDefinitionResponse
--
--         , responseTagLogGroup $
--             newTagLogGroupResponse
--
--         , responseDescribeResourcePolicies $
--             newDescribeResourcePoliciesResponse
--
--         , responseDescribeQueryDefinitions $
--             newDescribeQueryDefinitionsResponse
--
--         , responseDeleteLogStream $
--             newDeleteLogStreamResponse
--
--         , responseDescribeQueries $
--             newDescribeQueriesResponse
--
--         , responseCreateLogStream $
--             newCreateLogStreamResponse
--
--         , responseCreateLogGroup $
--             newCreateLogGroupResponse
--
--         , responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseCancelExportTask $
--             newCancelExportTaskResponse
--
--         , responsePutSubscriptionFilter $
--             newPutSubscriptionFilterResponse
--
--         , responseStartQuery $
--             newStartQueryResponse
--
--         , responseDeleteLogGroup $
--             newDeleteLogGroupResponse
--
--         , responseDeleteSubscriptionFilter $
--             newDeleteSubscriptionFilterResponse
--
--         , responsePutLogEvents $
--             newPutLogEventsResponse
--
--         , responseDescribeMetricFilters $
--             newDescribeMetricFiltersResponse
--
--         , responseTestMetricFilter $
--             newTestMetricFilterResponse
--
--         , responsePutDestinationPolicy $
--             newPutDestinationPolicyResponse
--
--         , responsePutMetricFilter $
--             newPutMetricFilterResponse
--
--         , responseDeleteRetentionPolicy $
--             newDeleteRetentionPolicyResponse
--
--         , responseDeleteMetricFilter $
--             newDeleteMetricFilterResponse
--
--         , responsePutRetentionPolicy $
--             newPutRetentionPolicyResponse
--
--         , responseListTagsLogGroup $
--             newListTagsLogGroupResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseAssociateKmsKey $
--             newAssociateKmsKeyResponse
--
--         , responseGetQueryResults $
--             newGetQueryResultsResponse
--
--         , responseDescribeLogStreams $
--             newDescribeLogStreamsResponse
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

requestDisassociateKmsKey :: DisassociateKmsKey -> TestTree
requestDisassociateKmsKey =
  req
    "DisassociateKmsKey"
    "fixture/DisassociateKmsKey.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLogGroupFields)

responseGetLogRecord :: GetLogRecordResponse -> TestTree
responseGetLogRecord =
  res
    "GetLogRecordResponse"
    "fixture/GetLogRecordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLogRecord)

responseDescribeDestinations :: DescribeDestinationsResponse -> TestTree
responseDescribeDestinations =
  res
    "DescribeDestinationsResponse"
    "fixture/DescribeDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDestinations)

responseUntagLogGroup :: UntagLogGroupResponse -> TestTree
responseUntagLogGroup =
  res
    "UntagLogGroupResponse"
    "fixture/UntagLogGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagLogGroup)

responseStopQuery :: StopQueryResponse -> TestTree
responseStopQuery =
  res
    "StopQueryResponse"
    "fixture/StopQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopQuery)

responseCreateExportTask :: CreateExportTaskResponse -> TestTree
responseCreateExportTask =
  res
    "CreateExportTaskResponse"
    "fixture/CreateExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExportTask)

responsePutDestination :: PutDestinationResponse -> TestTree
responsePutDestination =
  res
    "PutDestinationResponse"
    "fixture/PutDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDestination)

responseDescribeSubscriptionFilters :: DescribeSubscriptionFiltersResponse -> TestTree
responseDescribeSubscriptionFilters =
  res
    "DescribeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubscriptionFilters)

responseGetLogEvents :: GetLogEventsResponse -> TestTree
responseGetLogEvents =
  res
    "GetLogEventsResponse"
    "fixture/GetLogEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLogEvents)

responseDescribeLogGroups :: DescribeLogGroupsResponse -> TestTree
responseDescribeLogGroups =
  res
    "DescribeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLogGroups)

responseDeleteDestination :: DeleteDestinationResponse -> TestTree
responseDeleteDestination =
  res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDestination)

responseDisassociateKmsKey :: DisassociateKmsKeyResponse -> TestTree
responseDisassociateKmsKey =
  res
    "DisassociateKmsKeyResponse"
    "fixture/DisassociateKmsKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateKmsKey)

responseFilterLogEvents :: FilterLogEventsResponse -> TestTree
responseFilterLogEvents =
  res
    "FilterLogEventsResponse"
    "fixture/FilterLogEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FilterLogEvents)

responseDeleteQueryDefinition :: DeleteQueryDefinitionResponse -> TestTree
responseDeleteQueryDefinition =
  res
    "DeleteQueryDefinitionResponse"
    "fixture/DeleteQueryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueryDefinition)

responsePutQueryDefinition :: PutQueryDefinitionResponse -> TestTree
responsePutQueryDefinition =
  res
    "PutQueryDefinitionResponse"
    "fixture/PutQueryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutQueryDefinition)

responseTagLogGroup :: TagLogGroupResponse -> TestTree
responseTagLogGroup =
  res
    "TagLogGroupResponse"
    "fixture/TagLogGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagLogGroup)

responseDescribeResourcePolicies :: DescribeResourcePoliciesResponse -> TestTree
responseDescribeResourcePolicies =
  res
    "DescribeResourcePoliciesResponse"
    "fixture/DescribeResourcePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourcePolicies)

responseDescribeQueryDefinitions :: DescribeQueryDefinitionsResponse -> TestTree
responseDescribeQueryDefinitions =
  res
    "DescribeQueryDefinitionsResponse"
    "fixture/DescribeQueryDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQueryDefinitions)

responseDeleteLogStream :: DeleteLogStreamResponse -> TestTree
responseDeleteLogStream =
  res
    "DeleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLogStream)

responseDescribeQueries :: DescribeQueriesResponse -> TestTree
responseDescribeQueries =
  res
    "DescribeQueriesResponse"
    "fixture/DescribeQueriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQueries)

responseCreateLogStream :: CreateLogStreamResponse -> TestTree
responseCreateLogStream =
  res
    "CreateLogStreamResponse"
    "fixture/CreateLogStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLogStream)

responseCreateLogGroup :: CreateLogGroupResponse -> TestTree
responseCreateLogGroup =
  res
    "CreateLogGroupResponse"
    "fixture/CreateLogGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLogGroup)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExportTasks)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelExportTask)

responsePutSubscriptionFilter :: PutSubscriptionFilterResponse -> TestTree
responsePutSubscriptionFilter =
  res
    "PutSubscriptionFilterResponse"
    "fixture/PutSubscriptionFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSubscriptionFilter)

responseStartQuery :: StartQueryResponse -> TestTree
responseStartQuery =
  res
    "StartQueryResponse"
    "fixture/StartQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartQuery)

responseDeleteLogGroup :: DeleteLogGroupResponse -> TestTree
responseDeleteLogGroup =
  res
    "DeleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLogGroup)

responseDeleteSubscriptionFilter :: DeleteSubscriptionFilterResponse -> TestTree
responseDeleteSubscriptionFilter =
  res
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubscriptionFilter)

responsePutLogEvents :: PutLogEventsResponse -> TestTree
responsePutLogEvents =
  res
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLogEvents)

responseDescribeMetricFilters :: DescribeMetricFiltersResponse -> TestTree
responseDescribeMetricFilters =
  res
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMetricFilters)

responseTestMetricFilter :: TestMetricFilterResponse -> TestTree
responseTestMetricFilter =
  res
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestMetricFilter)

responsePutDestinationPolicy :: PutDestinationPolicyResponse -> TestTree
responsePutDestinationPolicy =
  res
    "PutDestinationPolicyResponse"
    "fixture/PutDestinationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDestinationPolicy)

responsePutMetricFilter :: PutMetricFilterResponse -> TestTree
responsePutMetricFilter =
  res
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMetricFilter)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy =
  res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRetentionPolicy)

responseDeleteMetricFilter :: DeleteMetricFilterResponse -> TestTree
responseDeleteMetricFilter =
  res
    "DeleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMetricFilter)

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy =
  res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRetentionPolicy)

responseListTagsLogGroup :: ListTagsLogGroupResponse -> TestTree
responseListTagsLogGroup =
  res
    "ListTagsLogGroupResponse"
    "fixture/ListTagsLogGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsLogGroup)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseAssociateKmsKey :: AssociateKmsKeyResponse -> TestTree
responseAssociateKmsKey =
  res
    "AssociateKmsKeyResponse"
    "fixture/AssociateKmsKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateKmsKey)

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults =
  res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueryResults)

responseDescribeLogStreams :: DescribeLogStreamsResponse -> TestTree
responseDescribeLogStreams =
  res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLogStreams)

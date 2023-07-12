{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudWatchLogs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudWatchLogs where

import Amazonka.CloudWatchLogs
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudWatchLogs.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateKmsKey $
--             newAssociateKmsKey
--
--         , requestCancelExportTask $
--             newCancelExportTask
--
--         , requestCreateExportTask $
--             newCreateExportTask
--
--         , requestCreateLogGroup $
--             newCreateLogGroup
--
--         , requestCreateLogStream $
--             newCreateLogStream
--
--         , requestDeleteDataProtectionPolicy $
--             newDeleteDataProtectionPolicy
--
--         , requestDeleteDestination $
--             newDeleteDestination
--
--         , requestDeleteLogGroup $
--             newDeleteLogGroup
--
--         , requestDeleteLogStream $
--             newDeleteLogStream
--
--         , requestDeleteMetricFilter $
--             newDeleteMetricFilter
--
--         , requestDeleteQueryDefinition $
--             newDeleteQueryDefinition
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteRetentionPolicy $
--             newDeleteRetentionPolicy
--
--         , requestDeleteSubscriptionFilter $
--             newDeleteSubscriptionFilter
--
--         , requestDescribeDestinations $
--             newDescribeDestinations
--
--         , requestDescribeExportTasks $
--             newDescribeExportTasks
--
--         , requestDescribeLogGroups $
--             newDescribeLogGroups
--
--         , requestDescribeLogStreams $
--             newDescribeLogStreams
--
--         , requestDescribeMetricFilters $
--             newDescribeMetricFilters
--
--         , requestDescribeQueries $
--             newDescribeQueries
--
--         , requestDescribeQueryDefinitions $
--             newDescribeQueryDefinitions
--
--         , requestDescribeResourcePolicies $
--             newDescribeResourcePolicies
--
--         , requestDescribeSubscriptionFilters $
--             newDescribeSubscriptionFilters
--
--         , requestDisassociateKmsKey $
--             newDisassociateKmsKey
--
--         , requestFilterLogEvents $
--             newFilterLogEvents
--
--         , requestGetDataProtectionPolicy $
--             newGetDataProtectionPolicy
--
--         , requestGetLogEvents $
--             newGetLogEvents
--
--         , requestGetLogGroupFields $
--             newGetLogGroupFields
--
--         , requestGetLogRecord $
--             newGetLogRecord
--
--         , requestGetQueryResults $
--             newGetQueryResults
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutDataProtectionPolicy $
--             newPutDataProtectionPolicy
--
--         , requestPutDestination $
--             newPutDestination
--
--         , requestPutDestinationPolicy $
--             newPutDestinationPolicy
--
--         , requestPutLogEvents $
--             newPutLogEvents
--
--         , requestPutMetricFilter $
--             newPutMetricFilter
--
--         , requestPutQueryDefinition $
--             newPutQueryDefinition
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestPutRetentionPolicy $
--             newPutRetentionPolicy
--
--         , requestPutSubscriptionFilter $
--             newPutSubscriptionFilter
--
--         , requestStartQuery $
--             newStartQuery
--
--         , requestStopQuery $
--             newStopQuery
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestMetricFilter $
--             newTestMetricFilter
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseAssociateKmsKey $
--             newAssociateKmsKeyResponse
--
--         , responseCancelExportTask $
--             newCancelExportTaskResponse
--
--         , responseCreateExportTask $
--             newCreateExportTaskResponse
--
--         , responseCreateLogGroup $
--             newCreateLogGroupResponse
--
--         , responseCreateLogStream $
--             newCreateLogStreamResponse
--
--         , responseDeleteDataProtectionPolicy $
--             newDeleteDataProtectionPolicyResponse
--
--         , responseDeleteDestination $
--             newDeleteDestinationResponse
--
--         , responseDeleteLogGroup $
--             newDeleteLogGroupResponse
--
--         , responseDeleteLogStream $
--             newDeleteLogStreamResponse
--
--         , responseDeleteMetricFilter $
--             newDeleteMetricFilterResponse
--
--         , responseDeleteQueryDefinition $
--             newDeleteQueryDefinitionResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteRetentionPolicy $
--             newDeleteRetentionPolicyResponse
--
--         , responseDeleteSubscriptionFilter $
--             newDeleteSubscriptionFilterResponse
--
--         , responseDescribeDestinations $
--             newDescribeDestinationsResponse
--
--         , responseDescribeExportTasks $
--             newDescribeExportTasksResponse
--
--         , responseDescribeLogGroups $
--             newDescribeLogGroupsResponse
--
--         , responseDescribeLogStreams $
--             newDescribeLogStreamsResponse
--
--         , responseDescribeMetricFilters $
--             newDescribeMetricFiltersResponse
--
--         , responseDescribeQueries $
--             newDescribeQueriesResponse
--
--         , responseDescribeQueryDefinitions $
--             newDescribeQueryDefinitionsResponse
--
--         , responseDescribeResourcePolicies $
--             newDescribeResourcePoliciesResponse
--
--         , responseDescribeSubscriptionFilters $
--             newDescribeSubscriptionFiltersResponse
--
--         , responseDisassociateKmsKey $
--             newDisassociateKmsKeyResponse
--
--         , responseFilterLogEvents $
--             newFilterLogEventsResponse
--
--         , responseGetDataProtectionPolicy $
--             newGetDataProtectionPolicyResponse
--
--         , responseGetLogEvents $
--             newGetLogEventsResponse
--
--         , responseGetLogGroupFields $
--             newGetLogGroupFieldsResponse
--
--         , responseGetLogRecord $
--             newGetLogRecordResponse
--
--         , responseGetQueryResults $
--             newGetQueryResultsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutDataProtectionPolicy $
--             newPutDataProtectionPolicyResponse
--
--         , responsePutDestination $
--             newPutDestinationResponse
--
--         , responsePutDestinationPolicy $
--             newPutDestinationPolicyResponse
--
--         , responsePutLogEvents $
--             newPutLogEventsResponse
--
--         , responsePutMetricFilter $
--             newPutMetricFilterResponse
--
--         , responsePutQueryDefinition $
--             newPutQueryDefinitionResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responsePutRetentionPolicy $
--             newPutRetentionPolicyResponse
--
--         , responsePutSubscriptionFilter $
--             newPutSubscriptionFilterResponse
--
--         , responseStartQuery $
--             newStartQueryResponse
--
--         , responseStopQuery $
--             newStopQueryResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestMetricFilter $
--             newTestMetricFilterResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestAssociateKmsKey :: AssociateKmsKey -> TestTree
requestAssociateKmsKey =
  req
    "AssociateKmsKey"
    "fixture/AssociateKmsKey.yaml"

requestCancelExportTask :: CancelExportTask -> TestTree
requestCancelExportTask =
  req
    "CancelExportTask"
    "fixture/CancelExportTask.yaml"

requestCreateExportTask :: CreateExportTask -> TestTree
requestCreateExportTask =
  req
    "CreateExportTask"
    "fixture/CreateExportTask.yaml"

requestCreateLogGroup :: CreateLogGroup -> TestTree
requestCreateLogGroup =
  req
    "CreateLogGroup"
    "fixture/CreateLogGroup.yaml"

requestCreateLogStream :: CreateLogStream -> TestTree
requestCreateLogStream =
  req
    "CreateLogStream"
    "fixture/CreateLogStream.yaml"

requestDeleteDataProtectionPolicy :: DeleteDataProtectionPolicy -> TestTree
requestDeleteDataProtectionPolicy =
  req
    "DeleteDataProtectionPolicy"
    "fixture/DeleteDataProtectionPolicy.yaml"

requestDeleteDestination :: DeleteDestination -> TestTree
requestDeleteDestination =
  req
    "DeleteDestination"
    "fixture/DeleteDestination.yaml"

requestDeleteLogGroup :: DeleteLogGroup -> TestTree
requestDeleteLogGroup =
  req
    "DeleteLogGroup"
    "fixture/DeleteLogGroup.yaml"

requestDeleteLogStream :: DeleteLogStream -> TestTree
requestDeleteLogStream =
  req
    "DeleteLogStream"
    "fixture/DeleteLogStream.yaml"

requestDeleteMetricFilter :: DeleteMetricFilter -> TestTree
requestDeleteMetricFilter =
  req
    "DeleteMetricFilter"
    "fixture/DeleteMetricFilter.yaml"

requestDeleteQueryDefinition :: DeleteQueryDefinition -> TestTree
requestDeleteQueryDefinition =
  req
    "DeleteQueryDefinition"
    "fixture/DeleteQueryDefinition.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
requestDeleteRetentionPolicy =
  req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy.yaml"

requestDeleteSubscriptionFilter :: DeleteSubscriptionFilter -> TestTree
requestDeleteSubscriptionFilter =
  req
    "DeleteSubscriptionFilter"
    "fixture/DeleteSubscriptionFilter.yaml"

requestDescribeDestinations :: DescribeDestinations -> TestTree
requestDescribeDestinations =
  req
    "DescribeDestinations"
    "fixture/DescribeDestinations.yaml"

requestDescribeExportTasks :: DescribeExportTasks -> TestTree
requestDescribeExportTasks =
  req
    "DescribeExportTasks"
    "fixture/DescribeExportTasks.yaml"

requestDescribeLogGroups :: DescribeLogGroups -> TestTree
requestDescribeLogGroups =
  req
    "DescribeLogGroups"
    "fixture/DescribeLogGroups.yaml"

requestDescribeLogStreams :: DescribeLogStreams -> TestTree
requestDescribeLogStreams =
  req
    "DescribeLogStreams"
    "fixture/DescribeLogStreams.yaml"

requestDescribeMetricFilters :: DescribeMetricFilters -> TestTree
requestDescribeMetricFilters =
  req
    "DescribeMetricFilters"
    "fixture/DescribeMetricFilters.yaml"

requestDescribeQueries :: DescribeQueries -> TestTree
requestDescribeQueries =
  req
    "DescribeQueries"
    "fixture/DescribeQueries.yaml"

requestDescribeQueryDefinitions :: DescribeQueryDefinitions -> TestTree
requestDescribeQueryDefinitions =
  req
    "DescribeQueryDefinitions"
    "fixture/DescribeQueryDefinitions.yaml"

requestDescribeResourcePolicies :: DescribeResourcePolicies -> TestTree
requestDescribeResourcePolicies =
  req
    "DescribeResourcePolicies"
    "fixture/DescribeResourcePolicies.yaml"

requestDescribeSubscriptionFilters :: DescribeSubscriptionFilters -> TestTree
requestDescribeSubscriptionFilters =
  req
    "DescribeSubscriptionFilters"
    "fixture/DescribeSubscriptionFilters.yaml"

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

requestGetDataProtectionPolicy :: GetDataProtectionPolicy -> TestTree
requestGetDataProtectionPolicy =
  req
    "GetDataProtectionPolicy"
    "fixture/GetDataProtectionPolicy.yaml"

requestGetLogEvents :: GetLogEvents -> TestTree
requestGetLogEvents =
  req
    "GetLogEvents"
    "fixture/GetLogEvents.yaml"

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

requestGetQueryResults :: GetQueryResults -> TestTree
requestGetQueryResults =
  req
    "GetQueryResults"
    "fixture/GetQueryResults.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutDataProtectionPolicy :: PutDataProtectionPolicy -> TestTree
requestPutDataProtectionPolicy =
  req
    "PutDataProtectionPolicy"
    "fixture/PutDataProtectionPolicy.yaml"

requestPutDestination :: PutDestination -> TestTree
requestPutDestination =
  req
    "PutDestination"
    "fixture/PutDestination.yaml"

requestPutDestinationPolicy :: PutDestinationPolicy -> TestTree
requestPutDestinationPolicy =
  req
    "PutDestinationPolicy"
    "fixture/PutDestinationPolicy.yaml"

requestPutLogEvents :: PutLogEvents -> TestTree
requestPutLogEvents =
  req
    "PutLogEvents"
    "fixture/PutLogEvents.yaml"

requestPutMetricFilter :: PutMetricFilter -> TestTree
requestPutMetricFilter =
  req
    "PutMetricFilter"
    "fixture/PutMetricFilter.yaml"

requestPutQueryDefinition :: PutQueryDefinition -> TestTree
requestPutQueryDefinition =
  req
    "PutQueryDefinition"
    "fixture/PutQueryDefinition.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestPutRetentionPolicy :: PutRetentionPolicy -> TestTree
requestPutRetentionPolicy =
  req
    "PutRetentionPolicy"
    "fixture/PutRetentionPolicy.yaml"

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

requestStopQuery :: StopQuery -> TestTree
requestStopQuery =
  req
    "StopQuery"
    "fixture/StopQuery.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestMetricFilter :: TestMetricFilter -> TestTree
requestTestMetricFilter =
  req
    "TestMetricFilter"
    "fixture/TestMetricFilter.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

-- Responses

responseAssociateKmsKey :: AssociateKmsKeyResponse -> TestTree
responseAssociateKmsKey =
  res
    "AssociateKmsKeyResponse"
    "fixture/AssociateKmsKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateKmsKey)

responseCancelExportTask :: CancelExportTaskResponse -> TestTree
responseCancelExportTask =
  res
    "CancelExportTaskResponse"
    "fixture/CancelExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelExportTask)

responseCreateExportTask :: CreateExportTaskResponse -> TestTree
responseCreateExportTask =
  res
    "CreateExportTaskResponse"
    "fixture/CreateExportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExportTask)

responseCreateLogGroup :: CreateLogGroupResponse -> TestTree
responseCreateLogGroup =
  res
    "CreateLogGroupResponse"
    "fixture/CreateLogGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLogGroup)

responseCreateLogStream :: CreateLogStreamResponse -> TestTree
responseCreateLogStream =
  res
    "CreateLogStreamResponse"
    "fixture/CreateLogStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLogStream)

responseDeleteDataProtectionPolicy :: DeleteDataProtectionPolicyResponse -> TestTree
responseDeleteDataProtectionPolicy =
  res
    "DeleteDataProtectionPolicyResponse"
    "fixture/DeleteDataProtectionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataProtectionPolicy)

responseDeleteDestination :: DeleteDestinationResponse -> TestTree
responseDeleteDestination =
  res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDestination)

responseDeleteLogGroup :: DeleteLogGroupResponse -> TestTree
responseDeleteLogGroup =
  res
    "DeleteLogGroupResponse"
    "fixture/DeleteLogGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLogGroup)

responseDeleteLogStream :: DeleteLogStreamResponse -> TestTree
responseDeleteLogStream =
  res
    "DeleteLogStreamResponse"
    "fixture/DeleteLogStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLogStream)

responseDeleteMetricFilter :: DeleteMetricFilterResponse -> TestTree
responseDeleteMetricFilter =
  res
    "DeleteMetricFilterResponse"
    "fixture/DeleteMetricFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMetricFilter)

responseDeleteQueryDefinition :: DeleteQueryDefinitionResponse -> TestTree
responseDeleteQueryDefinition =
  res
    "DeleteQueryDefinitionResponse"
    "fixture/DeleteQueryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueryDefinition)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy =
  res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRetentionPolicy)

responseDeleteSubscriptionFilter :: DeleteSubscriptionFilterResponse -> TestTree
responseDeleteSubscriptionFilter =
  res
    "DeleteSubscriptionFilterResponse"
    "fixture/DeleteSubscriptionFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubscriptionFilter)

responseDescribeDestinations :: DescribeDestinationsResponse -> TestTree
responseDescribeDestinations =
  res
    "DescribeDestinationsResponse"
    "fixture/DescribeDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDestinations)

responseDescribeExportTasks :: DescribeExportTasksResponse -> TestTree
responseDescribeExportTasks =
  res
    "DescribeExportTasksResponse"
    "fixture/DescribeExportTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExportTasks)

responseDescribeLogGroups :: DescribeLogGroupsResponse -> TestTree
responseDescribeLogGroups =
  res
    "DescribeLogGroupsResponse"
    "fixture/DescribeLogGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLogGroups)

responseDescribeLogStreams :: DescribeLogStreamsResponse -> TestTree
responseDescribeLogStreams =
  res
    "DescribeLogStreamsResponse"
    "fixture/DescribeLogStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLogStreams)

responseDescribeMetricFilters :: DescribeMetricFiltersResponse -> TestTree
responseDescribeMetricFilters =
  res
    "DescribeMetricFiltersResponse"
    "fixture/DescribeMetricFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMetricFilters)

responseDescribeQueries :: DescribeQueriesResponse -> TestTree
responseDescribeQueries =
  res
    "DescribeQueriesResponse"
    "fixture/DescribeQueriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQueries)

responseDescribeQueryDefinitions :: DescribeQueryDefinitionsResponse -> TestTree
responseDescribeQueryDefinitions =
  res
    "DescribeQueryDefinitionsResponse"
    "fixture/DescribeQueryDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQueryDefinitions)

responseDescribeResourcePolicies :: DescribeResourcePoliciesResponse -> TestTree
responseDescribeResourcePolicies =
  res
    "DescribeResourcePoliciesResponse"
    "fixture/DescribeResourcePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourcePolicies)

responseDescribeSubscriptionFilters :: DescribeSubscriptionFiltersResponse -> TestTree
responseDescribeSubscriptionFilters =
  res
    "DescribeSubscriptionFiltersResponse"
    "fixture/DescribeSubscriptionFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubscriptionFilters)

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

responseGetDataProtectionPolicy :: GetDataProtectionPolicyResponse -> TestTree
responseGetDataProtectionPolicy =
  res
    "GetDataProtectionPolicyResponse"
    "fixture/GetDataProtectionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataProtectionPolicy)

responseGetLogEvents :: GetLogEventsResponse -> TestTree
responseGetLogEvents =
  res
    "GetLogEventsResponse"
    "fixture/GetLogEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLogEvents)

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

responseGetQueryResults :: GetQueryResultsResponse -> TestTree
responseGetQueryResults =
  res
    "GetQueryResultsResponse"
    "fixture/GetQueryResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueryResults)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutDataProtectionPolicy :: PutDataProtectionPolicyResponse -> TestTree
responsePutDataProtectionPolicy =
  res
    "PutDataProtectionPolicyResponse"
    "fixture/PutDataProtectionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDataProtectionPolicy)

responsePutDestination :: PutDestinationResponse -> TestTree
responsePutDestination =
  res
    "PutDestinationResponse"
    "fixture/PutDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDestination)

responsePutDestinationPolicy :: PutDestinationPolicyResponse -> TestTree
responsePutDestinationPolicy =
  res
    "PutDestinationPolicyResponse"
    "fixture/PutDestinationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDestinationPolicy)

responsePutLogEvents :: PutLogEventsResponse -> TestTree
responsePutLogEvents =
  res
    "PutLogEventsResponse"
    "fixture/PutLogEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLogEvents)

responsePutMetricFilter :: PutMetricFilterResponse -> TestTree
responsePutMetricFilter =
  res
    "PutMetricFilterResponse"
    "fixture/PutMetricFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMetricFilter)

responsePutQueryDefinition :: PutQueryDefinitionResponse -> TestTree
responsePutQueryDefinition =
  res
    "PutQueryDefinitionResponse"
    "fixture/PutQueryDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutQueryDefinition)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy =
  res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRetentionPolicy)

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

responseStopQuery :: StopQueryResponse -> TestTree
responseStopQuery =
  res
    "StopQueryResponse"
    "fixture/StopQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopQuery)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestMetricFilter :: TestMetricFilterResponse -> TestTree
responseTestMetricFilter =
  res
    "TestMetricFilterResponse"
    "fixture/TestMetricFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestMetricFilter)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

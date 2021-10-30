{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Kendra
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Kendra where

import qualified Data.Proxy as Proxy
import Network.AWS.Kendra
import Test.AWS.Fixture
import Test.AWS.Kendra.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateDataSource $
--             newUpdateDataSource
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestClearQuerySuggestions $
--             newClearQuerySuggestions
--
--         , requestListFaqs $
--             newListFaqs
--
--         , requestDeleteIndex $
--             newDeleteIndex
--
--         , requestUpdateIndex $
--             newUpdateIndex
--
--         , requestListQuerySuggestionsBlockLists $
--             newListQuerySuggestionsBlockLists
--
--         , requestCreateFaq $
--             newCreateFaq
--
--         , requestCreateQuerySuggestionsBlockList $
--             newCreateQuerySuggestionsBlockList
--
--         , requestBatchPutDocument $
--             newBatchPutDocument
--
--         , requestBatchDeleteDocument $
--             newBatchDeleteDocument
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSubmitFeedback $
--             newSubmitFeedback
--
--         , requestStopDataSourceSyncJob $
--             newStopDataSourceSyncJob
--
--         , requestDescribeDataSource $
--             newDescribeDataSource
--
--         , requestDescribeIndex $
--             newDescribeIndex
--
--         , requestUpdateQuerySuggestionsConfig $
--             newUpdateQuerySuggestionsConfig
--
--         , requestCreateDataSource $
--             newCreateDataSource
--
--         , requestBatchGetDocumentStatus $
--             newBatchGetDocumentStatus
--
--         , requestListDataSourceSyncJobs $
--             newListDataSourceSyncJobs
--
--         , requestListDataSources $
--             newListDataSources
--
--         , requestDeleteQuerySuggestionsBlockList $
--             newDeleteQuerySuggestionsBlockList
--
--         , requestUpdateQuerySuggestionsBlockList $
--             newUpdateQuerySuggestionsBlockList
--
--         , requestDeleteFaq $
--             newDeleteFaq
--
--         , requestPutPrincipalMapping $
--             newPutPrincipalMapping
--
--         , requestDeletePrincipalMapping $
--             newDeletePrincipalMapping
--
--         , requestDescribeThesaurus $
--             newDescribeThesaurus
--
--         , requestListThesauri $
--             newListThesauri
--
--         , requestCreateIndex $
--             newCreateIndex
--
--         , requestQuery $
--             newQuery
--
--         , requestDescribeQuerySuggestionsConfig $
--             newDescribeQuerySuggestionsConfig
--
--         , requestStartDataSourceSyncJob $
--             newStartDataSourceSyncJob
--
--         , requestListIndices $
--             newListIndices
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetQuerySuggestions $
--             newGetQuerySuggestions
--
--         , requestDeleteThesaurus $
--             newDeleteThesaurus
--
--         , requestUpdateThesaurus $
--             newUpdateThesaurus
--
--         , requestDescribeFaq $
--             newDescribeFaq
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeQuerySuggestionsBlockList $
--             newDescribeQuerySuggestionsBlockList
--
--         , requestDescribePrincipalMapping $
--             newDescribePrincipalMapping
--
--         , requestListGroupsOlderThanOrderingId $
--             newListGroupsOlderThanOrderingId
--
--         , requestCreateThesaurus $
--             newCreateThesaurus
--
--           ]

--     , testGroup "response"
--         [ responseUpdateDataSource $
--             newUpdateDataSourceResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseClearQuerySuggestions $
--             newClearQuerySuggestionsResponse
--
--         , responseListFaqs $
--             newListFaqsResponse
--
--         , responseDeleteIndex $
--             newDeleteIndexResponse
--
--         , responseUpdateIndex $
--             newUpdateIndexResponse
--
--         , responseListQuerySuggestionsBlockLists $
--             newListQuerySuggestionsBlockListsResponse
--
--         , responseCreateFaq $
--             newCreateFaqResponse
--
--         , responseCreateQuerySuggestionsBlockList $
--             newCreateQuerySuggestionsBlockListResponse
--
--         , responseBatchPutDocument $
--             newBatchPutDocumentResponse
--
--         , responseBatchDeleteDocument $
--             newBatchDeleteDocumentResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSubmitFeedback $
--             newSubmitFeedbackResponse
--
--         , responseStopDataSourceSyncJob $
--             newStopDataSourceSyncJobResponse
--
--         , responseDescribeDataSource $
--             newDescribeDataSourceResponse
--
--         , responseDescribeIndex $
--             newDescribeIndexResponse
--
--         , responseUpdateQuerySuggestionsConfig $
--             newUpdateQuerySuggestionsConfigResponse
--
--         , responseCreateDataSource $
--             newCreateDataSourceResponse
--
--         , responseBatchGetDocumentStatus $
--             newBatchGetDocumentStatusResponse
--
--         , responseListDataSourceSyncJobs $
--             newListDataSourceSyncJobsResponse
--
--         , responseListDataSources $
--             newListDataSourcesResponse
--
--         , responseDeleteQuerySuggestionsBlockList $
--             newDeleteQuerySuggestionsBlockListResponse
--
--         , responseUpdateQuerySuggestionsBlockList $
--             newUpdateQuerySuggestionsBlockListResponse
--
--         , responseDeleteFaq $
--             newDeleteFaqResponse
--
--         , responsePutPrincipalMapping $
--             newPutPrincipalMappingResponse
--
--         , responseDeletePrincipalMapping $
--             newDeletePrincipalMappingResponse
--
--         , responseDescribeThesaurus $
--             newDescribeThesaurusResponse
--
--         , responseListThesauri $
--             newListThesauriResponse
--
--         , responseCreateIndex $
--             newCreateIndexResponse
--
--         , responseQuery $
--             newQueryResponse
--
--         , responseDescribeQuerySuggestionsConfig $
--             newDescribeQuerySuggestionsConfigResponse
--
--         , responseStartDataSourceSyncJob $
--             newStartDataSourceSyncJobResponse
--
--         , responseListIndices $
--             newListIndicesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetQuerySuggestions $
--             newGetQuerySuggestionsResponse
--
--         , responseDeleteThesaurus $
--             newDeleteThesaurusResponse
--
--         , responseUpdateThesaurus $
--             newUpdateThesaurusResponse
--
--         , responseDescribeFaq $
--             newDescribeFaqResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeQuerySuggestionsBlockList $
--             newDescribeQuerySuggestionsBlockListResponse
--
--         , responseDescribePrincipalMapping $
--             newDescribePrincipalMappingResponse
--
--         , responseListGroupsOlderThanOrderingId $
--             newListGroupsOlderThanOrderingIdResponse
--
--         , responseCreateThesaurus $
--             newCreateThesaurusResponse
--
--           ]
--     ]

-- Requests

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource =
  req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestClearQuerySuggestions :: ClearQuerySuggestions -> TestTree
requestClearQuerySuggestions =
  req
    "ClearQuerySuggestions"
    "fixture/ClearQuerySuggestions.yaml"

requestListFaqs :: ListFaqs -> TestTree
requestListFaqs =
  req
    "ListFaqs"
    "fixture/ListFaqs.yaml"

requestDeleteIndex :: DeleteIndex -> TestTree
requestDeleteIndex =
  req
    "DeleteIndex"
    "fixture/DeleteIndex.yaml"

requestUpdateIndex :: UpdateIndex -> TestTree
requestUpdateIndex =
  req
    "UpdateIndex"
    "fixture/UpdateIndex.yaml"

requestListQuerySuggestionsBlockLists :: ListQuerySuggestionsBlockLists -> TestTree
requestListQuerySuggestionsBlockLists =
  req
    "ListQuerySuggestionsBlockLists"
    "fixture/ListQuerySuggestionsBlockLists.yaml"

requestCreateFaq :: CreateFaq -> TestTree
requestCreateFaq =
  req
    "CreateFaq"
    "fixture/CreateFaq.yaml"

requestCreateQuerySuggestionsBlockList :: CreateQuerySuggestionsBlockList -> TestTree
requestCreateQuerySuggestionsBlockList =
  req
    "CreateQuerySuggestionsBlockList"
    "fixture/CreateQuerySuggestionsBlockList.yaml"

requestBatchPutDocument :: BatchPutDocument -> TestTree
requestBatchPutDocument =
  req
    "BatchPutDocument"
    "fixture/BatchPutDocument.yaml"

requestBatchDeleteDocument :: BatchDeleteDocument -> TestTree
requestBatchDeleteDocument =
  req
    "BatchDeleteDocument"
    "fixture/BatchDeleteDocument.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSubmitFeedback :: SubmitFeedback -> TestTree
requestSubmitFeedback =
  req
    "SubmitFeedback"
    "fixture/SubmitFeedback.yaml"

requestStopDataSourceSyncJob :: StopDataSourceSyncJob -> TestTree
requestStopDataSourceSyncJob =
  req
    "StopDataSourceSyncJob"
    "fixture/StopDataSourceSyncJob.yaml"

requestDescribeDataSource :: DescribeDataSource -> TestTree
requestDescribeDataSource =
  req
    "DescribeDataSource"
    "fixture/DescribeDataSource.yaml"

requestDescribeIndex :: DescribeIndex -> TestTree
requestDescribeIndex =
  req
    "DescribeIndex"
    "fixture/DescribeIndex.yaml"

requestUpdateQuerySuggestionsConfig :: UpdateQuerySuggestionsConfig -> TestTree
requestUpdateQuerySuggestionsConfig =
  req
    "UpdateQuerySuggestionsConfig"
    "fixture/UpdateQuerySuggestionsConfig.yaml"

requestCreateDataSource :: CreateDataSource -> TestTree
requestCreateDataSource =
  req
    "CreateDataSource"
    "fixture/CreateDataSource.yaml"

requestBatchGetDocumentStatus :: BatchGetDocumentStatus -> TestTree
requestBatchGetDocumentStatus =
  req
    "BatchGetDocumentStatus"
    "fixture/BatchGetDocumentStatus.yaml"

requestListDataSourceSyncJobs :: ListDataSourceSyncJobs -> TestTree
requestListDataSourceSyncJobs =
  req
    "ListDataSourceSyncJobs"
    "fixture/ListDataSourceSyncJobs.yaml"

requestListDataSources :: ListDataSources -> TestTree
requestListDataSources =
  req
    "ListDataSources"
    "fixture/ListDataSources.yaml"

requestDeleteQuerySuggestionsBlockList :: DeleteQuerySuggestionsBlockList -> TestTree
requestDeleteQuerySuggestionsBlockList =
  req
    "DeleteQuerySuggestionsBlockList"
    "fixture/DeleteQuerySuggestionsBlockList.yaml"

requestUpdateQuerySuggestionsBlockList :: UpdateQuerySuggestionsBlockList -> TestTree
requestUpdateQuerySuggestionsBlockList =
  req
    "UpdateQuerySuggestionsBlockList"
    "fixture/UpdateQuerySuggestionsBlockList.yaml"

requestDeleteFaq :: DeleteFaq -> TestTree
requestDeleteFaq =
  req
    "DeleteFaq"
    "fixture/DeleteFaq.yaml"

requestPutPrincipalMapping :: PutPrincipalMapping -> TestTree
requestPutPrincipalMapping =
  req
    "PutPrincipalMapping"
    "fixture/PutPrincipalMapping.yaml"

requestDeletePrincipalMapping :: DeletePrincipalMapping -> TestTree
requestDeletePrincipalMapping =
  req
    "DeletePrincipalMapping"
    "fixture/DeletePrincipalMapping.yaml"

requestDescribeThesaurus :: DescribeThesaurus -> TestTree
requestDescribeThesaurus =
  req
    "DescribeThesaurus"
    "fixture/DescribeThesaurus.yaml"

requestListThesauri :: ListThesauri -> TestTree
requestListThesauri =
  req
    "ListThesauri"
    "fixture/ListThesauri.yaml"

requestCreateIndex :: CreateIndex -> TestTree
requestCreateIndex =
  req
    "CreateIndex"
    "fixture/CreateIndex.yaml"

requestQuery :: Query -> TestTree
requestQuery =
  req
    "Query"
    "fixture/Query.yaml"

requestDescribeQuerySuggestionsConfig :: DescribeQuerySuggestionsConfig -> TestTree
requestDescribeQuerySuggestionsConfig =
  req
    "DescribeQuerySuggestionsConfig"
    "fixture/DescribeQuerySuggestionsConfig.yaml"

requestStartDataSourceSyncJob :: StartDataSourceSyncJob -> TestTree
requestStartDataSourceSyncJob =
  req
    "StartDataSourceSyncJob"
    "fixture/StartDataSourceSyncJob.yaml"

requestListIndices :: ListIndices -> TestTree
requestListIndices =
  req
    "ListIndices"
    "fixture/ListIndices.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetQuerySuggestions :: GetQuerySuggestions -> TestTree
requestGetQuerySuggestions =
  req
    "GetQuerySuggestions"
    "fixture/GetQuerySuggestions.yaml"

requestDeleteThesaurus :: DeleteThesaurus -> TestTree
requestDeleteThesaurus =
  req
    "DeleteThesaurus"
    "fixture/DeleteThesaurus.yaml"

requestUpdateThesaurus :: UpdateThesaurus -> TestTree
requestUpdateThesaurus =
  req
    "UpdateThesaurus"
    "fixture/UpdateThesaurus.yaml"

requestDescribeFaq :: DescribeFaq -> TestTree
requestDescribeFaq =
  req
    "DescribeFaq"
    "fixture/DescribeFaq.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeQuerySuggestionsBlockList :: DescribeQuerySuggestionsBlockList -> TestTree
requestDescribeQuerySuggestionsBlockList =
  req
    "DescribeQuerySuggestionsBlockList"
    "fixture/DescribeQuerySuggestionsBlockList.yaml"

requestDescribePrincipalMapping :: DescribePrincipalMapping -> TestTree
requestDescribePrincipalMapping =
  req
    "DescribePrincipalMapping"
    "fixture/DescribePrincipalMapping.yaml"

requestListGroupsOlderThanOrderingId :: ListGroupsOlderThanOrderingId -> TestTree
requestListGroupsOlderThanOrderingId =
  req
    "ListGroupsOlderThanOrderingId"
    "fixture/ListGroupsOlderThanOrderingId.yaml"

requestCreateThesaurus :: CreateThesaurus -> TestTree
requestCreateThesaurus =
  req
    "CreateThesaurus"
    "fixture/CreateThesaurus.yaml"

-- Responses

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource =
  res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSource)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSource)

responseClearQuerySuggestions :: ClearQuerySuggestionsResponse -> TestTree
responseClearQuerySuggestions =
  res
    "ClearQuerySuggestionsResponse"
    "fixture/ClearQuerySuggestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClearQuerySuggestions)

responseListFaqs :: ListFaqsResponse -> TestTree
responseListFaqs =
  res
    "ListFaqsResponse"
    "fixture/ListFaqsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFaqs)

responseDeleteIndex :: DeleteIndexResponse -> TestTree
responseDeleteIndex =
  res
    "DeleteIndexResponse"
    "fixture/DeleteIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIndex)

responseUpdateIndex :: UpdateIndexResponse -> TestTree
responseUpdateIndex =
  res
    "UpdateIndexResponse"
    "fixture/UpdateIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIndex)

responseListQuerySuggestionsBlockLists :: ListQuerySuggestionsBlockListsResponse -> TestTree
responseListQuerySuggestionsBlockLists =
  res
    "ListQuerySuggestionsBlockListsResponse"
    "fixture/ListQuerySuggestionsBlockListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQuerySuggestionsBlockLists)

responseCreateFaq :: CreateFaqResponse -> TestTree
responseCreateFaq =
  res
    "CreateFaqResponse"
    "fixture/CreateFaqResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFaq)

responseCreateQuerySuggestionsBlockList :: CreateQuerySuggestionsBlockListResponse -> TestTree
responseCreateQuerySuggestionsBlockList =
  res
    "CreateQuerySuggestionsBlockListResponse"
    "fixture/CreateQuerySuggestionsBlockListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQuerySuggestionsBlockList)

responseBatchPutDocument :: BatchPutDocumentResponse -> TestTree
responseBatchPutDocument =
  res
    "BatchPutDocumentResponse"
    "fixture/BatchPutDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutDocument)

responseBatchDeleteDocument :: BatchDeleteDocumentResponse -> TestTree
responseBatchDeleteDocument =
  res
    "BatchDeleteDocumentResponse"
    "fixture/BatchDeleteDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteDocument)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseSubmitFeedback :: SubmitFeedbackResponse -> TestTree
responseSubmitFeedback =
  res
    "SubmitFeedbackResponse"
    "fixture/SubmitFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubmitFeedback)

responseStopDataSourceSyncJob :: StopDataSourceSyncJobResponse -> TestTree
responseStopDataSourceSyncJob =
  res
    "StopDataSourceSyncJobResponse"
    "fixture/StopDataSourceSyncJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDataSourceSyncJob)

responseDescribeDataSource :: DescribeDataSourceResponse -> TestTree
responseDescribeDataSource =
  res
    "DescribeDataSourceResponse"
    "fixture/DescribeDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSource)

responseDescribeIndex :: DescribeIndexResponse -> TestTree
responseDescribeIndex =
  res
    "DescribeIndexResponse"
    "fixture/DescribeIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIndex)

responseUpdateQuerySuggestionsConfig :: UpdateQuerySuggestionsConfigResponse -> TestTree
responseUpdateQuerySuggestionsConfig =
  res
    "UpdateQuerySuggestionsConfigResponse"
    "fixture/UpdateQuerySuggestionsConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQuerySuggestionsConfig)

responseCreateDataSource :: CreateDataSourceResponse -> TestTree
responseCreateDataSource =
  res
    "CreateDataSourceResponse"
    "fixture/CreateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSource)

responseBatchGetDocumentStatus :: BatchGetDocumentStatusResponse -> TestTree
responseBatchGetDocumentStatus =
  res
    "BatchGetDocumentStatusResponse"
    "fixture/BatchGetDocumentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDocumentStatus)

responseListDataSourceSyncJobs :: ListDataSourceSyncJobsResponse -> TestTree
responseListDataSourceSyncJobs =
  res
    "ListDataSourceSyncJobsResponse"
    "fixture/ListDataSourceSyncJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSourceSyncJobs)

responseListDataSources :: ListDataSourcesResponse -> TestTree
responseListDataSources =
  res
    "ListDataSourcesResponse"
    "fixture/ListDataSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSources)

responseDeleteQuerySuggestionsBlockList :: DeleteQuerySuggestionsBlockListResponse -> TestTree
responseDeleteQuerySuggestionsBlockList =
  res
    "DeleteQuerySuggestionsBlockListResponse"
    "fixture/DeleteQuerySuggestionsBlockListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQuerySuggestionsBlockList)

responseUpdateQuerySuggestionsBlockList :: UpdateQuerySuggestionsBlockListResponse -> TestTree
responseUpdateQuerySuggestionsBlockList =
  res
    "UpdateQuerySuggestionsBlockListResponse"
    "fixture/UpdateQuerySuggestionsBlockListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQuerySuggestionsBlockList)

responseDeleteFaq :: DeleteFaqResponse -> TestTree
responseDeleteFaq =
  res
    "DeleteFaqResponse"
    "fixture/DeleteFaqResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFaq)

responsePutPrincipalMapping :: PutPrincipalMappingResponse -> TestTree
responsePutPrincipalMapping =
  res
    "PutPrincipalMappingResponse"
    "fixture/PutPrincipalMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPrincipalMapping)

responseDeletePrincipalMapping :: DeletePrincipalMappingResponse -> TestTree
responseDeletePrincipalMapping =
  res
    "DeletePrincipalMappingResponse"
    "fixture/DeletePrincipalMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePrincipalMapping)

responseDescribeThesaurus :: DescribeThesaurusResponse -> TestTree
responseDescribeThesaurus =
  res
    "DescribeThesaurusResponse"
    "fixture/DescribeThesaurusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThesaurus)

responseListThesauri :: ListThesauriResponse -> TestTree
responseListThesauri =
  res
    "ListThesauriResponse"
    "fixture/ListThesauriResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThesauri)

responseCreateIndex :: CreateIndexResponse -> TestTree
responseCreateIndex =
  res
    "CreateIndexResponse"
    "fixture/CreateIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIndex)

responseQuery :: QueryResponse -> TestTree
responseQuery =
  res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Query)

responseDescribeQuerySuggestionsConfig :: DescribeQuerySuggestionsConfigResponse -> TestTree
responseDescribeQuerySuggestionsConfig =
  res
    "DescribeQuerySuggestionsConfigResponse"
    "fixture/DescribeQuerySuggestionsConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQuerySuggestionsConfig)

responseStartDataSourceSyncJob :: StartDataSourceSyncJobResponse -> TestTree
responseStartDataSourceSyncJob =
  res
    "StartDataSourceSyncJobResponse"
    "fixture/StartDataSourceSyncJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDataSourceSyncJob)

responseListIndices :: ListIndicesResponse -> TestTree
responseListIndices =
  res
    "ListIndicesResponse"
    "fixture/ListIndicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIndices)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetQuerySuggestions :: GetQuerySuggestionsResponse -> TestTree
responseGetQuerySuggestions =
  res
    "GetQuerySuggestionsResponse"
    "fixture/GetQuerySuggestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQuerySuggestions)

responseDeleteThesaurus :: DeleteThesaurusResponse -> TestTree
responseDeleteThesaurus =
  res
    "DeleteThesaurusResponse"
    "fixture/DeleteThesaurusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThesaurus)

responseUpdateThesaurus :: UpdateThesaurusResponse -> TestTree
responseUpdateThesaurus =
  res
    "UpdateThesaurusResponse"
    "fixture/UpdateThesaurusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThesaurus)

responseDescribeFaq :: DescribeFaqResponse -> TestTree
responseDescribeFaq =
  res
    "DescribeFaqResponse"
    "fixture/DescribeFaqResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFaq)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeQuerySuggestionsBlockList :: DescribeQuerySuggestionsBlockListResponse -> TestTree
responseDescribeQuerySuggestionsBlockList =
  res
    "DescribeQuerySuggestionsBlockListResponse"
    "fixture/DescribeQuerySuggestionsBlockListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQuerySuggestionsBlockList)

responseDescribePrincipalMapping :: DescribePrincipalMappingResponse -> TestTree
responseDescribePrincipalMapping =
  res
    "DescribePrincipalMappingResponse"
    "fixture/DescribePrincipalMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePrincipalMapping)

responseListGroupsOlderThanOrderingId :: ListGroupsOlderThanOrderingIdResponse -> TestTree
responseListGroupsOlderThanOrderingId =
  res
    "ListGroupsOlderThanOrderingIdResponse"
    "fixture/ListGroupsOlderThanOrderingIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupsOlderThanOrderingId)

responseCreateThesaurus :: CreateThesaurusResponse -> TestTree
responseCreateThesaurus =
  res
    "CreateThesaurusResponse"
    "fixture/CreateThesaurusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThesaurus)

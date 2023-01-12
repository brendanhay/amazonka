{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Kendra
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Kendra where

import Amazonka.Kendra
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Kendra.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateEntitiesToExperience $
--             newAssociateEntitiesToExperience
--
--         , requestAssociatePersonasToEntities $
--             newAssociatePersonasToEntities
--
--         , requestBatchDeleteDocument $
--             newBatchDeleteDocument
--
--         , requestBatchGetDocumentStatus $
--             newBatchGetDocumentStatus
--
--         , requestBatchPutDocument $
--             newBatchPutDocument
--
--         , requestClearQuerySuggestions $
--             newClearQuerySuggestions
--
--         , requestCreateAccessControlConfiguration $
--             newCreateAccessControlConfiguration
--
--         , requestCreateDataSource $
--             newCreateDataSource
--
--         , requestCreateExperience $
--             newCreateExperience
--
--         , requestCreateFaq $
--             newCreateFaq
--
--         , requestCreateIndex $
--             newCreateIndex
--
--         , requestCreateQuerySuggestionsBlockList $
--             newCreateQuerySuggestionsBlockList
--
--         , requestCreateThesaurus $
--             newCreateThesaurus
--
--         , requestDeleteAccessControlConfiguration $
--             newDeleteAccessControlConfiguration
--
--         , requestDeleteDataSource $
--             newDeleteDataSource
--
--         , requestDeleteExperience $
--             newDeleteExperience
--
--         , requestDeleteFaq $
--             newDeleteFaq
--
--         , requestDeleteIndex $
--             newDeleteIndex
--
--         , requestDeletePrincipalMapping $
--             newDeletePrincipalMapping
--
--         , requestDeleteQuerySuggestionsBlockList $
--             newDeleteQuerySuggestionsBlockList
--
--         , requestDeleteThesaurus $
--             newDeleteThesaurus
--
--         , requestDescribeAccessControlConfiguration $
--             newDescribeAccessControlConfiguration
--
--         , requestDescribeDataSource $
--             newDescribeDataSource
--
--         , requestDescribeExperience $
--             newDescribeExperience
--
--         , requestDescribeFaq $
--             newDescribeFaq
--
--         , requestDescribeIndex $
--             newDescribeIndex
--
--         , requestDescribePrincipalMapping $
--             newDescribePrincipalMapping
--
--         , requestDescribeQuerySuggestionsBlockList $
--             newDescribeQuerySuggestionsBlockList
--
--         , requestDescribeQuerySuggestionsConfig $
--             newDescribeQuerySuggestionsConfig
--
--         , requestDescribeThesaurus $
--             newDescribeThesaurus
--
--         , requestDisassociateEntitiesFromExperience $
--             newDisassociateEntitiesFromExperience
--
--         , requestDisassociatePersonasFromEntities $
--             newDisassociatePersonasFromEntities
--
--         , requestGetQuerySuggestions $
--             newGetQuerySuggestions
--
--         , requestGetSnapshots $
--             newGetSnapshots
--
--         , requestListAccessControlConfigurations $
--             newListAccessControlConfigurations
--
--         , requestListDataSourceSyncJobs $
--             newListDataSourceSyncJobs
--
--         , requestListDataSources $
--             newListDataSources
--
--         , requestListEntityPersonas $
--             newListEntityPersonas
--
--         , requestListExperienceEntities $
--             newListExperienceEntities
--
--         , requestListExperiences $
--             newListExperiences
--
--         , requestListFaqs $
--             newListFaqs
--
--         , requestListGroupsOlderThanOrderingId $
--             newListGroupsOlderThanOrderingId
--
--         , requestListIndices $
--             newListIndices
--
--         , requestListQuerySuggestionsBlockLists $
--             newListQuerySuggestionsBlockLists
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListThesauri $
--             newListThesauri
--
--         , requestPutPrincipalMapping $
--             newPutPrincipalMapping
--
--         , requestQuery $
--             newQuery
--
--         , requestStartDataSourceSyncJob $
--             newStartDataSourceSyncJob
--
--         , requestStopDataSourceSyncJob $
--             newStopDataSourceSyncJob
--
--         , requestSubmitFeedback $
--             newSubmitFeedback
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAccessControlConfiguration $
--             newUpdateAccessControlConfiguration
--
--         , requestUpdateDataSource $
--             newUpdateDataSource
--
--         , requestUpdateExperience $
--             newUpdateExperience
--
--         , requestUpdateIndex $
--             newUpdateIndex
--
--         , requestUpdateQuerySuggestionsBlockList $
--             newUpdateQuerySuggestionsBlockList
--
--         , requestUpdateQuerySuggestionsConfig $
--             newUpdateQuerySuggestionsConfig
--
--         , requestUpdateThesaurus $
--             newUpdateThesaurus
--
--           ]

--     , testGroup "response"
--         [ responseAssociateEntitiesToExperience $
--             newAssociateEntitiesToExperienceResponse
--
--         , responseAssociatePersonasToEntities $
--             newAssociatePersonasToEntitiesResponse
--
--         , responseBatchDeleteDocument $
--             newBatchDeleteDocumentResponse
--
--         , responseBatchGetDocumentStatus $
--             newBatchGetDocumentStatusResponse
--
--         , responseBatchPutDocument $
--             newBatchPutDocumentResponse
--
--         , responseClearQuerySuggestions $
--             newClearQuerySuggestionsResponse
--
--         , responseCreateAccessControlConfiguration $
--             newCreateAccessControlConfigurationResponse
--
--         , responseCreateDataSource $
--             newCreateDataSourceResponse
--
--         , responseCreateExperience $
--             newCreateExperienceResponse
--
--         , responseCreateFaq $
--             newCreateFaqResponse
--
--         , responseCreateIndex $
--             newCreateIndexResponse
--
--         , responseCreateQuerySuggestionsBlockList $
--             newCreateQuerySuggestionsBlockListResponse
--
--         , responseCreateThesaurus $
--             newCreateThesaurusResponse
--
--         , responseDeleteAccessControlConfiguration $
--             newDeleteAccessControlConfigurationResponse
--
--         , responseDeleteDataSource $
--             newDeleteDataSourceResponse
--
--         , responseDeleteExperience $
--             newDeleteExperienceResponse
--
--         , responseDeleteFaq $
--             newDeleteFaqResponse
--
--         , responseDeleteIndex $
--             newDeleteIndexResponse
--
--         , responseDeletePrincipalMapping $
--             newDeletePrincipalMappingResponse
--
--         , responseDeleteQuerySuggestionsBlockList $
--             newDeleteQuerySuggestionsBlockListResponse
--
--         , responseDeleteThesaurus $
--             newDeleteThesaurusResponse
--
--         , responseDescribeAccessControlConfiguration $
--             newDescribeAccessControlConfigurationResponse
--
--         , responseDescribeDataSource $
--             newDescribeDataSourceResponse
--
--         , responseDescribeExperience $
--             newDescribeExperienceResponse
--
--         , responseDescribeFaq $
--             newDescribeFaqResponse
--
--         , responseDescribeIndex $
--             newDescribeIndexResponse
--
--         , responseDescribePrincipalMapping $
--             newDescribePrincipalMappingResponse
--
--         , responseDescribeQuerySuggestionsBlockList $
--             newDescribeQuerySuggestionsBlockListResponse
--
--         , responseDescribeQuerySuggestionsConfig $
--             newDescribeQuerySuggestionsConfigResponse
--
--         , responseDescribeThesaurus $
--             newDescribeThesaurusResponse
--
--         , responseDisassociateEntitiesFromExperience $
--             newDisassociateEntitiesFromExperienceResponse
--
--         , responseDisassociatePersonasFromEntities $
--             newDisassociatePersonasFromEntitiesResponse
--
--         , responseGetQuerySuggestions $
--             newGetQuerySuggestionsResponse
--
--         , responseGetSnapshots $
--             newGetSnapshotsResponse
--
--         , responseListAccessControlConfigurations $
--             newListAccessControlConfigurationsResponse
--
--         , responseListDataSourceSyncJobs $
--             newListDataSourceSyncJobsResponse
--
--         , responseListDataSources $
--             newListDataSourcesResponse
--
--         , responseListEntityPersonas $
--             newListEntityPersonasResponse
--
--         , responseListExperienceEntities $
--             newListExperienceEntitiesResponse
--
--         , responseListExperiences $
--             newListExperiencesResponse
--
--         , responseListFaqs $
--             newListFaqsResponse
--
--         , responseListGroupsOlderThanOrderingId $
--             newListGroupsOlderThanOrderingIdResponse
--
--         , responseListIndices $
--             newListIndicesResponse
--
--         , responseListQuerySuggestionsBlockLists $
--             newListQuerySuggestionsBlockListsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListThesauri $
--             newListThesauriResponse
--
--         , responsePutPrincipalMapping $
--             newPutPrincipalMappingResponse
--
--         , responseQuery $
--             newQueryResponse
--
--         , responseStartDataSourceSyncJob $
--             newStartDataSourceSyncJobResponse
--
--         , responseStopDataSourceSyncJob $
--             newStopDataSourceSyncJobResponse
--
--         , responseSubmitFeedback $
--             newSubmitFeedbackResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAccessControlConfiguration $
--             newUpdateAccessControlConfigurationResponse
--
--         , responseUpdateDataSource $
--             newUpdateDataSourceResponse
--
--         , responseUpdateExperience $
--             newUpdateExperienceResponse
--
--         , responseUpdateIndex $
--             newUpdateIndexResponse
--
--         , responseUpdateQuerySuggestionsBlockList $
--             newUpdateQuerySuggestionsBlockListResponse
--
--         , responseUpdateQuerySuggestionsConfig $
--             newUpdateQuerySuggestionsConfigResponse
--
--         , responseUpdateThesaurus $
--             newUpdateThesaurusResponse
--
--           ]
--     ]

-- Requests

requestAssociateEntitiesToExperience :: AssociateEntitiesToExperience -> TestTree
requestAssociateEntitiesToExperience =
  req
    "AssociateEntitiesToExperience"
    "fixture/AssociateEntitiesToExperience.yaml"

requestAssociatePersonasToEntities :: AssociatePersonasToEntities -> TestTree
requestAssociatePersonasToEntities =
  req
    "AssociatePersonasToEntities"
    "fixture/AssociatePersonasToEntities.yaml"

requestBatchDeleteDocument :: BatchDeleteDocument -> TestTree
requestBatchDeleteDocument =
  req
    "BatchDeleteDocument"
    "fixture/BatchDeleteDocument.yaml"

requestBatchGetDocumentStatus :: BatchGetDocumentStatus -> TestTree
requestBatchGetDocumentStatus =
  req
    "BatchGetDocumentStatus"
    "fixture/BatchGetDocumentStatus.yaml"

requestBatchPutDocument :: BatchPutDocument -> TestTree
requestBatchPutDocument =
  req
    "BatchPutDocument"
    "fixture/BatchPutDocument.yaml"

requestClearQuerySuggestions :: ClearQuerySuggestions -> TestTree
requestClearQuerySuggestions =
  req
    "ClearQuerySuggestions"
    "fixture/ClearQuerySuggestions.yaml"

requestCreateAccessControlConfiguration :: CreateAccessControlConfiguration -> TestTree
requestCreateAccessControlConfiguration =
  req
    "CreateAccessControlConfiguration"
    "fixture/CreateAccessControlConfiguration.yaml"

requestCreateDataSource :: CreateDataSource -> TestTree
requestCreateDataSource =
  req
    "CreateDataSource"
    "fixture/CreateDataSource.yaml"

requestCreateExperience :: CreateExperience -> TestTree
requestCreateExperience =
  req
    "CreateExperience"
    "fixture/CreateExperience.yaml"

requestCreateFaq :: CreateFaq -> TestTree
requestCreateFaq =
  req
    "CreateFaq"
    "fixture/CreateFaq.yaml"

requestCreateIndex :: CreateIndex -> TestTree
requestCreateIndex =
  req
    "CreateIndex"
    "fixture/CreateIndex.yaml"

requestCreateQuerySuggestionsBlockList :: CreateQuerySuggestionsBlockList -> TestTree
requestCreateQuerySuggestionsBlockList =
  req
    "CreateQuerySuggestionsBlockList"
    "fixture/CreateQuerySuggestionsBlockList.yaml"

requestCreateThesaurus :: CreateThesaurus -> TestTree
requestCreateThesaurus =
  req
    "CreateThesaurus"
    "fixture/CreateThesaurus.yaml"

requestDeleteAccessControlConfiguration :: DeleteAccessControlConfiguration -> TestTree
requestDeleteAccessControlConfiguration =
  req
    "DeleteAccessControlConfiguration"
    "fixture/DeleteAccessControlConfiguration.yaml"

requestDeleteDataSource :: DeleteDataSource -> TestTree
requestDeleteDataSource =
  req
    "DeleteDataSource"
    "fixture/DeleteDataSource.yaml"

requestDeleteExperience :: DeleteExperience -> TestTree
requestDeleteExperience =
  req
    "DeleteExperience"
    "fixture/DeleteExperience.yaml"

requestDeleteFaq :: DeleteFaq -> TestTree
requestDeleteFaq =
  req
    "DeleteFaq"
    "fixture/DeleteFaq.yaml"

requestDeleteIndex :: DeleteIndex -> TestTree
requestDeleteIndex =
  req
    "DeleteIndex"
    "fixture/DeleteIndex.yaml"

requestDeletePrincipalMapping :: DeletePrincipalMapping -> TestTree
requestDeletePrincipalMapping =
  req
    "DeletePrincipalMapping"
    "fixture/DeletePrincipalMapping.yaml"

requestDeleteQuerySuggestionsBlockList :: DeleteQuerySuggestionsBlockList -> TestTree
requestDeleteQuerySuggestionsBlockList =
  req
    "DeleteQuerySuggestionsBlockList"
    "fixture/DeleteQuerySuggestionsBlockList.yaml"

requestDeleteThesaurus :: DeleteThesaurus -> TestTree
requestDeleteThesaurus =
  req
    "DeleteThesaurus"
    "fixture/DeleteThesaurus.yaml"

requestDescribeAccessControlConfiguration :: DescribeAccessControlConfiguration -> TestTree
requestDescribeAccessControlConfiguration =
  req
    "DescribeAccessControlConfiguration"
    "fixture/DescribeAccessControlConfiguration.yaml"

requestDescribeDataSource :: DescribeDataSource -> TestTree
requestDescribeDataSource =
  req
    "DescribeDataSource"
    "fixture/DescribeDataSource.yaml"

requestDescribeExperience :: DescribeExperience -> TestTree
requestDescribeExperience =
  req
    "DescribeExperience"
    "fixture/DescribeExperience.yaml"

requestDescribeFaq :: DescribeFaq -> TestTree
requestDescribeFaq =
  req
    "DescribeFaq"
    "fixture/DescribeFaq.yaml"

requestDescribeIndex :: DescribeIndex -> TestTree
requestDescribeIndex =
  req
    "DescribeIndex"
    "fixture/DescribeIndex.yaml"

requestDescribePrincipalMapping :: DescribePrincipalMapping -> TestTree
requestDescribePrincipalMapping =
  req
    "DescribePrincipalMapping"
    "fixture/DescribePrincipalMapping.yaml"

requestDescribeQuerySuggestionsBlockList :: DescribeQuerySuggestionsBlockList -> TestTree
requestDescribeQuerySuggestionsBlockList =
  req
    "DescribeQuerySuggestionsBlockList"
    "fixture/DescribeQuerySuggestionsBlockList.yaml"

requestDescribeQuerySuggestionsConfig :: DescribeQuerySuggestionsConfig -> TestTree
requestDescribeQuerySuggestionsConfig =
  req
    "DescribeQuerySuggestionsConfig"
    "fixture/DescribeQuerySuggestionsConfig.yaml"

requestDescribeThesaurus :: DescribeThesaurus -> TestTree
requestDescribeThesaurus =
  req
    "DescribeThesaurus"
    "fixture/DescribeThesaurus.yaml"

requestDisassociateEntitiesFromExperience :: DisassociateEntitiesFromExperience -> TestTree
requestDisassociateEntitiesFromExperience =
  req
    "DisassociateEntitiesFromExperience"
    "fixture/DisassociateEntitiesFromExperience.yaml"

requestDisassociatePersonasFromEntities :: DisassociatePersonasFromEntities -> TestTree
requestDisassociatePersonasFromEntities =
  req
    "DisassociatePersonasFromEntities"
    "fixture/DisassociatePersonasFromEntities.yaml"

requestGetQuerySuggestions :: GetQuerySuggestions -> TestTree
requestGetQuerySuggestions =
  req
    "GetQuerySuggestions"
    "fixture/GetQuerySuggestions.yaml"

requestGetSnapshots :: GetSnapshots -> TestTree
requestGetSnapshots =
  req
    "GetSnapshots"
    "fixture/GetSnapshots.yaml"

requestListAccessControlConfigurations :: ListAccessControlConfigurations -> TestTree
requestListAccessControlConfigurations =
  req
    "ListAccessControlConfigurations"
    "fixture/ListAccessControlConfigurations.yaml"

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

requestListEntityPersonas :: ListEntityPersonas -> TestTree
requestListEntityPersonas =
  req
    "ListEntityPersonas"
    "fixture/ListEntityPersonas.yaml"

requestListExperienceEntities :: ListExperienceEntities -> TestTree
requestListExperienceEntities =
  req
    "ListExperienceEntities"
    "fixture/ListExperienceEntities.yaml"

requestListExperiences :: ListExperiences -> TestTree
requestListExperiences =
  req
    "ListExperiences"
    "fixture/ListExperiences.yaml"

requestListFaqs :: ListFaqs -> TestTree
requestListFaqs =
  req
    "ListFaqs"
    "fixture/ListFaqs.yaml"

requestListGroupsOlderThanOrderingId :: ListGroupsOlderThanOrderingId -> TestTree
requestListGroupsOlderThanOrderingId =
  req
    "ListGroupsOlderThanOrderingId"
    "fixture/ListGroupsOlderThanOrderingId.yaml"

requestListIndices :: ListIndices -> TestTree
requestListIndices =
  req
    "ListIndices"
    "fixture/ListIndices.yaml"

requestListQuerySuggestionsBlockLists :: ListQuerySuggestionsBlockLists -> TestTree
requestListQuerySuggestionsBlockLists =
  req
    "ListQuerySuggestionsBlockLists"
    "fixture/ListQuerySuggestionsBlockLists.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListThesauri :: ListThesauri -> TestTree
requestListThesauri =
  req
    "ListThesauri"
    "fixture/ListThesauri.yaml"

requestPutPrincipalMapping :: PutPrincipalMapping -> TestTree
requestPutPrincipalMapping =
  req
    "PutPrincipalMapping"
    "fixture/PutPrincipalMapping.yaml"

requestQuery :: Query -> TestTree
requestQuery =
  req
    "Query"
    "fixture/Query.yaml"

requestStartDataSourceSyncJob :: StartDataSourceSyncJob -> TestTree
requestStartDataSourceSyncJob =
  req
    "StartDataSourceSyncJob"
    "fixture/StartDataSourceSyncJob.yaml"

requestStopDataSourceSyncJob :: StopDataSourceSyncJob -> TestTree
requestStopDataSourceSyncJob =
  req
    "StopDataSourceSyncJob"
    "fixture/StopDataSourceSyncJob.yaml"

requestSubmitFeedback :: SubmitFeedback -> TestTree
requestSubmitFeedback =
  req
    "SubmitFeedback"
    "fixture/SubmitFeedback.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAccessControlConfiguration :: UpdateAccessControlConfiguration -> TestTree
requestUpdateAccessControlConfiguration =
  req
    "UpdateAccessControlConfiguration"
    "fixture/UpdateAccessControlConfiguration.yaml"

requestUpdateDataSource :: UpdateDataSource -> TestTree
requestUpdateDataSource =
  req
    "UpdateDataSource"
    "fixture/UpdateDataSource.yaml"

requestUpdateExperience :: UpdateExperience -> TestTree
requestUpdateExperience =
  req
    "UpdateExperience"
    "fixture/UpdateExperience.yaml"

requestUpdateIndex :: UpdateIndex -> TestTree
requestUpdateIndex =
  req
    "UpdateIndex"
    "fixture/UpdateIndex.yaml"

requestUpdateQuerySuggestionsBlockList :: UpdateQuerySuggestionsBlockList -> TestTree
requestUpdateQuerySuggestionsBlockList =
  req
    "UpdateQuerySuggestionsBlockList"
    "fixture/UpdateQuerySuggestionsBlockList.yaml"

requestUpdateQuerySuggestionsConfig :: UpdateQuerySuggestionsConfig -> TestTree
requestUpdateQuerySuggestionsConfig =
  req
    "UpdateQuerySuggestionsConfig"
    "fixture/UpdateQuerySuggestionsConfig.yaml"

requestUpdateThesaurus :: UpdateThesaurus -> TestTree
requestUpdateThesaurus =
  req
    "UpdateThesaurus"
    "fixture/UpdateThesaurus.yaml"

-- Responses

responseAssociateEntitiesToExperience :: AssociateEntitiesToExperienceResponse -> TestTree
responseAssociateEntitiesToExperience =
  res
    "AssociateEntitiesToExperienceResponse"
    "fixture/AssociateEntitiesToExperienceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateEntitiesToExperience)

responseAssociatePersonasToEntities :: AssociatePersonasToEntitiesResponse -> TestTree
responseAssociatePersonasToEntities =
  res
    "AssociatePersonasToEntitiesResponse"
    "fixture/AssociatePersonasToEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePersonasToEntities)

responseBatchDeleteDocument :: BatchDeleteDocumentResponse -> TestTree
responseBatchDeleteDocument =
  res
    "BatchDeleteDocumentResponse"
    "fixture/BatchDeleteDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteDocument)

responseBatchGetDocumentStatus :: BatchGetDocumentStatusResponse -> TestTree
responseBatchGetDocumentStatus =
  res
    "BatchGetDocumentStatusResponse"
    "fixture/BatchGetDocumentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDocumentStatus)

responseBatchPutDocument :: BatchPutDocumentResponse -> TestTree
responseBatchPutDocument =
  res
    "BatchPutDocumentResponse"
    "fixture/BatchPutDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutDocument)

responseClearQuerySuggestions :: ClearQuerySuggestionsResponse -> TestTree
responseClearQuerySuggestions =
  res
    "ClearQuerySuggestionsResponse"
    "fixture/ClearQuerySuggestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClearQuerySuggestions)

responseCreateAccessControlConfiguration :: CreateAccessControlConfigurationResponse -> TestTree
responseCreateAccessControlConfiguration =
  res
    "CreateAccessControlConfigurationResponse"
    "fixture/CreateAccessControlConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccessControlConfiguration)

responseCreateDataSource :: CreateDataSourceResponse -> TestTree
responseCreateDataSource =
  res
    "CreateDataSourceResponse"
    "fixture/CreateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSource)

responseCreateExperience :: CreateExperienceResponse -> TestTree
responseCreateExperience =
  res
    "CreateExperienceResponse"
    "fixture/CreateExperienceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExperience)

responseCreateFaq :: CreateFaqResponse -> TestTree
responseCreateFaq =
  res
    "CreateFaqResponse"
    "fixture/CreateFaqResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFaq)

responseCreateIndex :: CreateIndexResponse -> TestTree
responseCreateIndex =
  res
    "CreateIndexResponse"
    "fixture/CreateIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIndex)

responseCreateQuerySuggestionsBlockList :: CreateQuerySuggestionsBlockListResponse -> TestTree
responseCreateQuerySuggestionsBlockList =
  res
    "CreateQuerySuggestionsBlockListResponse"
    "fixture/CreateQuerySuggestionsBlockListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQuerySuggestionsBlockList)

responseCreateThesaurus :: CreateThesaurusResponse -> TestTree
responseCreateThesaurus =
  res
    "CreateThesaurusResponse"
    "fixture/CreateThesaurusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateThesaurus)

responseDeleteAccessControlConfiguration :: DeleteAccessControlConfigurationResponse -> TestTree
responseDeleteAccessControlConfiguration =
  res
    "DeleteAccessControlConfigurationResponse"
    "fixture/DeleteAccessControlConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessControlConfiguration)

responseDeleteDataSource :: DeleteDataSourceResponse -> TestTree
responseDeleteDataSource =
  res
    "DeleteDataSourceResponse"
    "fixture/DeleteDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataSource)

responseDeleteExperience :: DeleteExperienceResponse -> TestTree
responseDeleteExperience =
  res
    "DeleteExperienceResponse"
    "fixture/DeleteExperienceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExperience)

responseDeleteFaq :: DeleteFaqResponse -> TestTree
responseDeleteFaq =
  res
    "DeleteFaqResponse"
    "fixture/DeleteFaqResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFaq)

responseDeleteIndex :: DeleteIndexResponse -> TestTree
responseDeleteIndex =
  res
    "DeleteIndexResponse"
    "fixture/DeleteIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIndex)

responseDeletePrincipalMapping :: DeletePrincipalMappingResponse -> TestTree
responseDeletePrincipalMapping =
  res
    "DeletePrincipalMappingResponse"
    "fixture/DeletePrincipalMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePrincipalMapping)

responseDeleteQuerySuggestionsBlockList :: DeleteQuerySuggestionsBlockListResponse -> TestTree
responseDeleteQuerySuggestionsBlockList =
  res
    "DeleteQuerySuggestionsBlockListResponse"
    "fixture/DeleteQuerySuggestionsBlockListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQuerySuggestionsBlockList)

responseDeleteThesaurus :: DeleteThesaurusResponse -> TestTree
responseDeleteThesaurus =
  res
    "DeleteThesaurusResponse"
    "fixture/DeleteThesaurusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteThesaurus)

responseDescribeAccessControlConfiguration :: DescribeAccessControlConfigurationResponse -> TestTree
responseDescribeAccessControlConfiguration =
  res
    "DescribeAccessControlConfigurationResponse"
    "fixture/DescribeAccessControlConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccessControlConfiguration)

responseDescribeDataSource :: DescribeDataSourceResponse -> TestTree
responseDescribeDataSource =
  res
    "DescribeDataSourceResponse"
    "fixture/DescribeDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataSource)

responseDescribeExperience :: DescribeExperienceResponse -> TestTree
responseDescribeExperience =
  res
    "DescribeExperienceResponse"
    "fixture/DescribeExperienceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExperience)

responseDescribeFaq :: DescribeFaqResponse -> TestTree
responseDescribeFaq =
  res
    "DescribeFaqResponse"
    "fixture/DescribeFaqResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFaq)

responseDescribeIndex :: DescribeIndexResponse -> TestTree
responseDescribeIndex =
  res
    "DescribeIndexResponse"
    "fixture/DescribeIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIndex)

responseDescribePrincipalMapping :: DescribePrincipalMappingResponse -> TestTree
responseDescribePrincipalMapping =
  res
    "DescribePrincipalMappingResponse"
    "fixture/DescribePrincipalMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePrincipalMapping)

responseDescribeQuerySuggestionsBlockList :: DescribeQuerySuggestionsBlockListResponse -> TestTree
responseDescribeQuerySuggestionsBlockList =
  res
    "DescribeQuerySuggestionsBlockListResponse"
    "fixture/DescribeQuerySuggestionsBlockListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQuerySuggestionsBlockList)

responseDescribeQuerySuggestionsConfig :: DescribeQuerySuggestionsConfigResponse -> TestTree
responseDescribeQuerySuggestionsConfig =
  res
    "DescribeQuerySuggestionsConfigResponse"
    "fixture/DescribeQuerySuggestionsConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQuerySuggestionsConfig)

responseDescribeThesaurus :: DescribeThesaurusResponse -> TestTree
responseDescribeThesaurus =
  res
    "DescribeThesaurusResponse"
    "fixture/DescribeThesaurusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeThesaurus)

responseDisassociateEntitiesFromExperience :: DisassociateEntitiesFromExperienceResponse -> TestTree
responseDisassociateEntitiesFromExperience =
  res
    "DisassociateEntitiesFromExperienceResponse"
    "fixture/DisassociateEntitiesFromExperienceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateEntitiesFromExperience)

responseDisassociatePersonasFromEntities :: DisassociatePersonasFromEntitiesResponse -> TestTree
responseDisassociatePersonasFromEntities =
  res
    "DisassociatePersonasFromEntitiesResponse"
    "fixture/DisassociatePersonasFromEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociatePersonasFromEntities)

responseGetQuerySuggestions :: GetQuerySuggestionsResponse -> TestTree
responseGetQuerySuggestions =
  res
    "GetQuerySuggestionsResponse"
    "fixture/GetQuerySuggestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQuerySuggestions)

responseGetSnapshots :: GetSnapshotsResponse -> TestTree
responseGetSnapshots =
  res
    "GetSnapshotsResponse"
    "fixture/GetSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSnapshots)

responseListAccessControlConfigurations :: ListAccessControlConfigurationsResponse -> TestTree
responseListAccessControlConfigurations =
  res
    "ListAccessControlConfigurationsResponse"
    "fixture/ListAccessControlConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessControlConfigurations)

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

responseListEntityPersonas :: ListEntityPersonasResponse -> TestTree
responseListEntityPersonas =
  res
    "ListEntityPersonasResponse"
    "fixture/ListEntityPersonasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntityPersonas)

responseListExperienceEntities :: ListExperienceEntitiesResponse -> TestTree
responseListExperienceEntities =
  res
    "ListExperienceEntitiesResponse"
    "fixture/ListExperienceEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExperienceEntities)

responseListExperiences :: ListExperiencesResponse -> TestTree
responseListExperiences =
  res
    "ListExperiencesResponse"
    "fixture/ListExperiencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExperiences)

responseListFaqs :: ListFaqsResponse -> TestTree
responseListFaqs =
  res
    "ListFaqsResponse"
    "fixture/ListFaqsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFaqs)

responseListGroupsOlderThanOrderingId :: ListGroupsOlderThanOrderingIdResponse -> TestTree
responseListGroupsOlderThanOrderingId =
  res
    "ListGroupsOlderThanOrderingIdResponse"
    "fixture/ListGroupsOlderThanOrderingIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupsOlderThanOrderingId)

responseListIndices :: ListIndicesResponse -> TestTree
responseListIndices =
  res
    "ListIndicesResponse"
    "fixture/ListIndicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIndices)

responseListQuerySuggestionsBlockLists :: ListQuerySuggestionsBlockListsResponse -> TestTree
responseListQuerySuggestionsBlockLists =
  res
    "ListQuerySuggestionsBlockListsResponse"
    "fixture/ListQuerySuggestionsBlockListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQuerySuggestionsBlockLists)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListThesauri :: ListThesauriResponse -> TestTree
responseListThesauri =
  res
    "ListThesauriResponse"
    "fixture/ListThesauriResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListThesauri)

responsePutPrincipalMapping :: PutPrincipalMappingResponse -> TestTree
responsePutPrincipalMapping =
  res
    "PutPrincipalMappingResponse"
    "fixture/PutPrincipalMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPrincipalMapping)

responseQuery :: QueryResponse -> TestTree
responseQuery =
  res
    "QueryResponse"
    "fixture/QueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Query)

responseStartDataSourceSyncJob :: StartDataSourceSyncJobResponse -> TestTree
responseStartDataSourceSyncJob =
  res
    "StartDataSourceSyncJobResponse"
    "fixture/StartDataSourceSyncJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDataSourceSyncJob)

responseStopDataSourceSyncJob :: StopDataSourceSyncJobResponse -> TestTree
responseStopDataSourceSyncJob =
  res
    "StopDataSourceSyncJobResponse"
    "fixture/StopDataSourceSyncJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDataSourceSyncJob)

responseSubmitFeedback :: SubmitFeedbackResponse -> TestTree
responseSubmitFeedback =
  res
    "SubmitFeedbackResponse"
    "fixture/SubmitFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubmitFeedback)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAccessControlConfiguration :: UpdateAccessControlConfigurationResponse -> TestTree
responseUpdateAccessControlConfiguration =
  res
    "UpdateAccessControlConfigurationResponse"
    "fixture/UpdateAccessControlConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccessControlConfiguration)

responseUpdateDataSource :: UpdateDataSourceResponse -> TestTree
responseUpdateDataSource =
  res
    "UpdateDataSourceResponse"
    "fixture/UpdateDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataSource)

responseUpdateExperience :: UpdateExperienceResponse -> TestTree
responseUpdateExperience =
  res
    "UpdateExperienceResponse"
    "fixture/UpdateExperienceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExperience)

responseUpdateIndex :: UpdateIndexResponse -> TestTree
responseUpdateIndex =
  res
    "UpdateIndexResponse"
    "fixture/UpdateIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIndex)

responseUpdateQuerySuggestionsBlockList :: UpdateQuerySuggestionsBlockListResponse -> TestTree
responseUpdateQuerySuggestionsBlockList =
  res
    "UpdateQuerySuggestionsBlockListResponse"
    "fixture/UpdateQuerySuggestionsBlockListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQuerySuggestionsBlockList)

responseUpdateQuerySuggestionsConfig :: UpdateQuerySuggestionsConfigResponse -> TestTree
responseUpdateQuerySuggestionsConfig =
  res
    "UpdateQuerySuggestionsConfigResponse"
    "fixture/UpdateQuerySuggestionsConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQuerySuggestionsConfig)

responseUpdateThesaurus :: UpdateThesaurusResponse -> TestTree
responseUpdateThesaurus =
  res
    "UpdateThesaurusResponse"
    "fixture/UpdateThesaurusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThesaurus)

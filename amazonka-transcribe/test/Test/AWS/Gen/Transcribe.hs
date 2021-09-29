{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Transcribe
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Transcribe where

import Data.Proxy
import Network.AWS.Transcribe
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Transcribe.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListLanguageModels $
--             newListLanguageModels
--
--         , requestGetVocabularyFilter $
--             newGetVocabularyFilter
--
--         , requestStartTranscriptionJob $
--             newStartTranscriptionJob
--
--         , requestCreateLanguageModel $
--             newCreateLanguageModel
--
--         , requestListVocabularies $
--             newListVocabularies
--
--         , requestCreateVocabulary $
--             newCreateVocabulary
--
--         , requestDeleteVocabulary $
--             newDeleteVocabulary
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateVocabulary $
--             newUpdateVocabulary
--
--         , requestListVocabularyFilters $
--             newListVocabularyFilters
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListTranscriptionJobs $
--             newListTranscriptionJobs
--
--         , requestDeleteTranscriptionJob $
--             newDeleteTranscriptionJob
--
--         , requestStartMedicalTranscriptionJob $
--             newStartMedicalTranscriptionJob
--
--         , requestGetCallAnalyticsCategory $
--             newGetCallAnalyticsCategory
--
--         , requestGetMedicalVocabulary $
--             newGetMedicalVocabulary
--
--         , requestGetTranscriptionJob $
--             newGetTranscriptionJob
--
--         , requestGetVocabulary $
--             newGetVocabulary
--
--         , requestGetMedicalTranscriptionJob $
--             newGetMedicalTranscriptionJob
--
--         , requestDeleteLanguageModel $
--             newDeleteLanguageModel
--
--         , requestCreateVocabularyFilter $
--             newCreateVocabularyFilter
--
--         , requestListMedicalTranscriptionJobs $
--             newListMedicalTranscriptionJobs
--
--         , requestStartCallAnalyticsJob $
--             newStartCallAnalyticsJob
--
--         , requestDeleteMedicalTranscriptionJob $
--             newDeleteMedicalTranscriptionJob
--
--         , requestUpdateVocabularyFilter $
--             newUpdateVocabularyFilter
--
--         , requestDeleteVocabularyFilter $
--             newDeleteVocabularyFilter
--
--         , requestUpdateMedicalVocabulary $
--             newUpdateMedicalVocabulary
--
--         , requestDescribeLanguageModel $
--             newDescribeLanguageModel
--
--         , requestCreateCallAnalyticsCategory $
--             newCreateCallAnalyticsCategory
--
--         , requestListCallAnalyticsCategories $
--             newListCallAnalyticsCategories
--
--         , requestDeleteMedicalVocabulary $
--             newDeleteMedicalVocabulary
--
--         , requestListMedicalVocabularies $
--             newListMedicalVocabularies
--
--         , requestGetCallAnalyticsJob $
--             newGetCallAnalyticsJob
--
--         , requestUpdateCallAnalyticsCategory $
--             newUpdateCallAnalyticsCategory
--
--         , requestCreateMedicalVocabulary $
--             newCreateMedicalVocabulary
--
--         , requestDeleteCallAnalyticsCategory $
--             newDeleteCallAnalyticsCategory
--
--         , requestListCallAnalyticsJobs $
--             newListCallAnalyticsJobs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteCallAnalyticsJob $
--             newDeleteCallAnalyticsJob
--
--           ]

--     , testGroup "response"
--         [ responseListLanguageModels $
--             newListLanguageModelsResponse
--
--         , responseGetVocabularyFilter $
--             newGetVocabularyFilterResponse
--
--         , responseStartTranscriptionJob $
--             newStartTranscriptionJobResponse
--
--         , responseCreateLanguageModel $
--             newCreateLanguageModelResponse
--
--         , responseListVocabularies $
--             newListVocabulariesResponse
--
--         , responseCreateVocabulary $
--             newCreateVocabularyResponse
--
--         , responseDeleteVocabulary $
--             newDeleteVocabularyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateVocabulary $
--             newUpdateVocabularyResponse
--
--         , responseListVocabularyFilters $
--             newListVocabularyFiltersResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListTranscriptionJobs $
--             newListTranscriptionJobsResponse
--
--         , responseDeleteTranscriptionJob $
--             newDeleteTranscriptionJobResponse
--
--         , responseStartMedicalTranscriptionJob $
--             newStartMedicalTranscriptionJobResponse
--
--         , responseGetCallAnalyticsCategory $
--             newGetCallAnalyticsCategoryResponse
--
--         , responseGetMedicalVocabulary $
--             newGetMedicalVocabularyResponse
--
--         , responseGetTranscriptionJob $
--             newGetTranscriptionJobResponse
--
--         , responseGetVocabulary $
--             newGetVocabularyResponse
--
--         , responseGetMedicalTranscriptionJob $
--             newGetMedicalTranscriptionJobResponse
--
--         , responseDeleteLanguageModel $
--             newDeleteLanguageModelResponse
--
--         , responseCreateVocabularyFilter $
--             newCreateVocabularyFilterResponse
--
--         , responseListMedicalTranscriptionJobs $
--             newListMedicalTranscriptionJobsResponse
--
--         , responseStartCallAnalyticsJob $
--             newStartCallAnalyticsJobResponse
--
--         , responseDeleteMedicalTranscriptionJob $
--             newDeleteMedicalTranscriptionJobResponse
--
--         , responseUpdateVocabularyFilter $
--             newUpdateVocabularyFilterResponse
--
--         , responseDeleteVocabularyFilter $
--             newDeleteVocabularyFilterResponse
--
--         , responseUpdateMedicalVocabulary $
--             newUpdateMedicalVocabularyResponse
--
--         , responseDescribeLanguageModel $
--             newDescribeLanguageModelResponse
--
--         , responseCreateCallAnalyticsCategory $
--             newCreateCallAnalyticsCategoryResponse
--
--         , responseListCallAnalyticsCategories $
--             newListCallAnalyticsCategoriesResponse
--
--         , responseDeleteMedicalVocabulary $
--             newDeleteMedicalVocabularyResponse
--
--         , responseListMedicalVocabularies $
--             newListMedicalVocabulariesResponse
--
--         , responseGetCallAnalyticsJob $
--             newGetCallAnalyticsJobResponse
--
--         , responseUpdateCallAnalyticsCategory $
--             newUpdateCallAnalyticsCategoryResponse
--
--         , responseCreateMedicalVocabulary $
--             newCreateMedicalVocabularyResponse
--
--         , responseDeleteCallAnalyticsCategory $
--             newDeleteCallAnalyticsCategoryResponse
--
--         , responseListCallAnalyticsJobs $
--             newListCallAnalyticsJobsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteCallAnalyticsJob $
--             newDeleteCallAnalyticsJobResponse
--
--           ]
--     ]

-- Requests

requestListLanguageModels :: ListLanguageModels -> TestTree
requestListLanguageModels =
  req
    "ListLanguageModels"
    "fixture/ListLanguageModels.yaml"

requestGetVocabularyFilter :: GetVocabularyFilter -> TestTree
requestGetVocabularyFilter =
  req
    "GetVocabularyFilter"
    "fixture/GetVocabularyFilter.yaml"

requestStartTranscriptionJob :: StartTranscriptionJob -> TestTree
requestStartTranscriptionJob =
  req
    "StartTranscriptionJob"
    "fixture/StartTranscriptionJob.yaml"

requestCreateLanguageModel :: CreateLanguageModel -> TestTree
requestCreateLanguageModel =
  req
    "CreateLanguageModel"
    "fixture/CreateLanguageModel.yaml"

requestListVocabularies :: ListVocabularies -> TestTree
requestListVocabularies =
  req
    "ListVocabularies"
    "fixture/ListVocabularies.yaml"

requestCreateVocabulary :: CreateVocabulary -> TestTree
requestCreateVocabulary =
  req
    "CreateVocabulary"
    "fixture/CreateVocabulary.yaml"

requestDeleteVocabulary :: DeleteVocabulary -> TestTree
requestDeleteVocabulary =
  req
    "DeleteVocabulary"
    "fixture/DeleteVocabulary.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateVocabulary :: UpdateVocabulary -> TestTree
requestUpdateVocabulary =
  req
    "UpdateVocabulary"
    "fixture/UpdateVocabulary.yaml"

requestListVocabularyFilters :: ListVocabularyFilters -> TestTree
requestListVocabularyFilters =
  req
    "ListVocabularyFilters"
    "fixture/ListVocabularyFilters.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListTranscriptionJobs :: ListTranscriptionJobs -> TestTree
requestListTranscriptionJobs =
  req
    "ListTranscriptionJobs"
    "fixture/ListTranscriptionJobs.yaml"

requestDeleteTranscriptionJob :: DeleteTranscriptionJob -> TestTree
requestDeleteTranscriptionJob =
  req
    "DeleteTranscriptionJob"
    "fixture/DeleteTranscriptionJob.yaml"

requestStartMedicalTranscriptionJob :: StartMedicalTranscriptionJob -> TestTree
requestStartMedicalTranscriptionJob =
  req
    "StartMedicalTranscriptionJob"
    "fixture/StartMedicalTranscriptionJob.yaml"

requestGetCallAnalyticsCategory :: GetCallAnalyticsCategory -> TestTree
requestGetCallAnalyticsCategory =
  req
    "GetCallAnalyticsCategory"
    "fixture/GetCallAnalyticsCategory.yaml"

requestGetMedicalVocabulary :: GetMedicalVocabulary -> TestTree
requestGetMedicalVocabulary =
  req
    "GetMedicalVocabulary"
    "fixture/GetMedicalVocabulary.yaml"

requestGetTranscriptionJob :: GetTranscriptionJob -> TestTree
requestGetTranscriptionJob =
  req
    "GetTranscriptionJob"
    "fixture/GetTranscriptionJob.yaml"

requestGetVocabulary :: GetVocabulary -> TestTree
requestGetVocabulary =
  req
    "GetVocabulary"
    "fixture/GetVocabulary.yaml"

requestGetMedicalTranscriptionJob :: GetMedicalTranscriptionJob -> TestTree
requestGetMedicalTranscriptionJob =
  req
    "GetMedicalTranscriptionJob"
    "fixture/GetMedicalTranscriptionJob.yaml"

requestDeleteLanguageModel :: DeleteLanguageModel -> TestTree
requestDeleteLanguageModel =
  req
    "DeleteLanguageModel"
    "fixture/DeleteLanguageModel.yaml"

requestCreateVocabularyFilter :: CreateVocabularyFilter -> TestTree
requestCreateVocabularyFilter =
  req
    "CreateVocabularyFilter"
    "fixture/CreateVocabularyFilter.yaml"

requestListMedicalTranscriptionJobs :: ListMedicalTranscriptionJobs -> TestTree
requestListMedicalTranscriptionJobs =
  req
    "ListMedicalTranscriptionJobs"
    "fixture/ListMedicalTranscriptionJobs.yaml"

requestStartCallAnalyticsJob :: StartCallAnalyticsJob -> TestTree
requestStartCallAnalyticsJob =
  req
    "StartCallAnalyticsJob"
    "fixture/StartCallAnalyticsJob.yaml"

requestDeleteMedicalTranscriptionJob :: DeleteMedicalTranscriptionJob -> TestTree
requestDeleteMedicalTranscriptionJob =
  req
    "DeleteMedicalTranscriptionJob"
    "fixture/DeleteMedicalTranscriptionJob.yaml"

requestUpdateVocabularyFilter :: UpdateVocabularyFilter -> TestTree
requestUpdateVocabularyFilter =
  req
    "UpdateVocabularyFilter"
    "fixture/UpdateVocabularyFilter.yaml"

requestDeleteVocabularyFilter :: DeleteVocabularyFilter -> TestTree
requestDeleteVocabularyFilter =
  req
    "DeleteVocabularyFilter"
    "fixture/DeleteVocabularyFilter.yaml"

requestUpdateMedicalVocabulary :: UpdateMedicalVocabulary -> TestTree
requestUpdateMedicalVocabulary =
  req
    "UpdateMedicalVocabulary"
    "fixture/UpdateMedicalVocabulary.yaml"

requestDescribeLanguageModel :: DescribeLanguageModel -> TestTree
requestDescribeLanguageModel =
  req
    "DescribeLanguageModel"
    "fixture/DescribeLanguageModel.yaml"

requestCreateCallAnalyticsCategory :: CreateCallAnalyticsCategory -> TestTree
requestCreateCallAnalyticsCategory =
  req
    "CreateCallAnalyticsCategory"
    "fixture/CreateCallAnalyticsCategory.yaml"

requestListCallAnalyticsCategories :: ListCallAnalyticsCategories -> TestTree
requestListCallAnalyticsCategories =
  req
    "ListCallAnalyticsCategories"
    "fixture/ListCallAnalyticsCategories.yaml"

requestDeleteMedicalVocabulary :: DeleteMedicalVocabulary -> TestTree
requestDeleteMedicalVocabulary =
  req
    "DeleteMedicalVocabulary"
    "fixture/DeleteMedicalVocabulary.yaml"

requestListMedicalVocabularies :: ListMedicalVocabularies -> TestTree
requestListMedicalVocabularies =
  req
    "ListMedicalVocabularies"
    "fixture/ListMedicalVocabularies.yaml"

requestGetCallAnalyticsJob :: GetCallAnalyticsJob -> TestTree
requestGetCallAnalyticsJob =
  req
    "GetCallAnalyticsJob"
    "fixture/GetCallAnalyticsJob.yaml"

requestUpdateCallAnalyticsCategory :: UpdateCallAnalyticsCategory -> TestTree
requestUpdateCallAnalyticsCategory =
  req
    "UpdateCallAnalyticsCategory"
    "fixture/UpdateCallAnalyticsCategory.yaml"

requestCreateMedicalVocabulary :: CreateMedicalVocabulary -> TestTree
requestCreateMedicalVocabulary =
  req
    "CreateMedicalVocabulary"
    "fixture/CreateMedicalVocabulary.yaml"

requestDeleteCallAnalyticsCategory :: DeleteCallAnalyticsCategory -> TestTree
requestDeleteCallAnalyticsCategory =
  req
    "DeleteCallAnalyticsCategory"
    "fixture/DeleteCallAnalyticsCategory.yaml"

requestListCallAnalyticsJobs :: ListCallAnalyticsJobs -> TestTree
requestListCallAnalyticsJobs =
  req
    "ListCallAnalyticsJobs"
    "fixture/ListCallAnalyticsJobs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteCallAnalyticsJob :: DeleteCallAnalyticsJob -> TestTree
requestDeleteCallAnalyticsJob =
  req
    "DeleteCallAnalyticsJob"
    "fixture/DeleteCallAnalyticsJob.yaml"

-- Responses

responseListLanguageModels :: ListLanguageModelsResponse -> TestTree
responseListLanguageModels =
  res
    "ListLanguageModelsResponse"
    "fixture/ListLanguageModelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLanguageModels)

responseGetVocabularyFilter :: GetVocabularyFilterResponse -> TestTree
responseGetVocabularyFilter =
  res
    "GetVocabularyFilterResponse"
    "fixture/GetVocabularyFilterResponse.proto"
    defaultService
    (Proxy :: Proxy GetVocabularyFilter)

responseStartTranscriptionJob :: StartTranscriptionJobResponse -> TestTree
responseStartTranscriptionJob =
  res
    "StartTranscriptionJobResponse"
    "fixture/StartTranscriptionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartTranscriptionJob)

responseCreateLanguageModel :: CreateLanguageModelResponse -> TestTree
responseCreateLanguageModel =
  res
    "CreateLanguageModelResponse"
    "fixture/CreateLanguageModelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLanguageModel)

responseListVocabularies :: ListVocabulariesResponse -> TestTree
responseListVocabularies =
  res
    "ListVocabulariesResponse"
    "fixture/ListVocabulariesResponse.proto"
    defaultService
    (Proxy :: Proxy ListVocabularies)

responseCreateVocabulary :: CreateVocabularyResponse -> TestTree
responseCreateVocabulary =
  res
    "CreateVocabularyResponse"
    "fixture/CreateVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVocabulary)

responseDeleteVocabulary :: DeleteVocabularyResponse -> TestTree
responseDeleteVocabulary =
  res
    "DeleteVocabularyResponse"
    "fixture/DeleteVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVocabulary)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseUpdateVocabulary :: UpdateVocabularyResponse -> TestTree
responseUpdateVocabulary =
  res
    "UpdateVocabularyResponse"
    "fixture/UpdateVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVocabulary)

responseListVocabularyFilters :: ListVocabularyFiltersResponse -> TestTree
responseListVocabularyFilters =
  res
    "ListVocabularyFiltersResponse"
    "fixture/ListVocabularyFiltersResponse.proto"
    defaultService
    (Proxy :: Proxy ListVocabularyFilters)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListTranscriptionJobs :: ListTranscriptionJobsResponse -> TestTree
responseListTranscriptionJobs =
  res
    "ListTranscriptionJobsResponse"
    "fixture/ListTranscriptionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTranscriptionJobs)

responseDeleteTranscriptionJob :: DeleteTranscriptionJobResponse -> TestTree
responseDeleteTranscriptionJob =
  res
    "DeleteTranscriptionJobResponse"
    "fixture/DeleteTranscriptionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTranscriptionJob)

responseStartMedicalTranscriptionJob :: StartMedicalTranscriptionJobResponse -> TestTree
responseStartMedicalTranscriptionJob =
  res
    "StartMedicalTranscriptionJobResponse"
    "fixture/StartMedicalTranscriptionJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartMedicalTranscriptionJob)

responseGetCallAnalyticsCategory :: GetCallAnalyticsCategoryResponse -> TestTree
responseGetCallAnalyticsCategory =
  res
    "GetCallAnalyticsCategoryResponse"
    "fixture/GetCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetCallAnalyticsCategory)

responseGetMedicalVocabulary :: GetMedicalVocabularyResponse -> TestTree
responseGetMedicalVocabulary =
  res
    "GetMedicalVocabularyResponse"
    "fixture/GetMedicalVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy GetMedicalVocabulary)

responseGetTranscriptionJob :: GetTranscriptionJobResponse -> TestTree
responseGetTranscriptionJob =
  res
    "GetTranscriptionJobResponse"
    "fixture/GetTranscriptionJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetTranscriptionJob)

responseGetVocabulary :: GetVocabularyResponse -> TestTree
responseGetVocabulary =
  res
    "GetVocabularyResponse"
    "fixture/GetVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy GetVocabulary)

responseGetMedicalTranscriptionJob :: GetMedicalTranscriptionJobResponse -> TestTree
responseGetMedicalTranscriptionJob =
  res
    "GetMedicalTranscriptionJobResponse"
    "fixture/GetMedicalTranscriptionJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetMedicalTranscriptionJob)

responseDeleteLanguageModel :: DeleteLanguageModelResponse -> TestTree
responseDeleteLanguageModel =
  res
    "DeleteLanguageModelResponse"
    "fixture/DeleteLanguageModelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLanguageModel)

responseCreateVocabularyFilter :: CreateVocabularyFilterResponse -> TestTree
responseCreateVocabularyFilter =
  res
    "CreateVocabularyFilterResponse"
    "fixture/CreateVocabularyFilterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVocabularyFilter)

responseListMedicalTranscriptionJobs :: ListMedicalTranscriptionJobsResponse -> TestTree
responseListMedicalTranscriptionJobs =
  res
    "ListMedicalTranscriptionJobsResponse"
    "fixture/ListMedicalTranscriptionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMedicalTranscriptionJobs)

responseStartCallAnalyticsJob :: StartCallAnalyticsJobResponse -> TestTree
responseStartCallAnalyticsJob =
  res
    "StartCallAnalyticsJobResponse"
    "fixture/StartCallAnalyticsJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartCallAnalyticsJob)

responseDeleteMedicalTranscriptionJob :: DeleteMedicalTranscriptionJobResponse -> TestTree
responseDeleteMedicalTranscriptionJob =
  res
    "DeleteMedicalTranscriptionJobResponse"
    "fixture/DeleteMedicalTranscriptionJobResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMedicalTranscriptionJob)

responseUpdateVocabularyFilter :: UpdateVocabularyFilterResponse -> TestTree
responseUpdateVocabularyFilter =
  res
    "UpdateVocabularyFilterResponse"
    "fixture/UpdateVocabularyFilterResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVocabularyFilter)

responseDeleteVocabularyFilter :: DeleteVocabularyFilterResponse -> TestTree
responseDeleteVocabularyFilter =
  res
    "DeleteVocabularyFilterResponse"
    "fixture/DeleteVocabularyFilterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVocabularyFilter)

responseUpdateMedicalVocabulary :: UpdateMedicalVocabularyResponse -> TestTree
responseUpdateMedicalVocabulary =
  res
    "UpdateMedicalVocabularyResponse"
    "fixture/UpdateMedicalVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMedicalVocabulary)

responseDescribeLanguageModel :: DescribeLanguageModelResponse -> TestTree
responseDescribeLanguageModel =
  res
    "DescribeLanguageModelResponse"
    "fixture/DescribeLanguageModelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLanguageModel)

responseCreateCallAnalyticsCategory :: CreateCallAnalyticsCategoryResponse -> TestTree
responseCreateCallAnalyticsCategory =
  res
    "CreateCallAnalyticsCategoryResponse"
    "fixture/CreateCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCallAnalyticsCategory)

responseListCallAnalyticsCategories :: ListCallAnalyticsCategoriesResponse -> TestTree
responseListCallAnalyticsCategories =
  res
    "ListCallAnalyticsCategoriesResponse"
    "fixture/ListCallAnalyticsCategoriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCallAnalyticsCategories)

responseDeleteMedicalVocabulary :: DeleteMedicalVocabularyResponse -> TestTree
responseDeleteMedicalVocabulary =
  res
    "DeleteMedicalVocabularyResponse"
    "fixture/DeleteMedicalVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMedicalVocabulary)

responseListMedicalVocabularies :: ListMedicalVocabulariesResponse -> TestTree
responseListMedicalVocabularies =
  res
    "ListMedicalVocabulariesResponse"
    "fixture/ListMedicalVocabulariesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMedicalVocabularies)

responseGetCallAnalyticsJob :: GetCallAnalyticsJobResponse -> TestTree
responseGetCallAnalyticsJob =
  res
    "GetCallAnalyticsJobResponse"
    "fixture/GetCallAnalyticsJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetCallAnalyticsJob)

responseUpdateCallAnalyticsCategory :: UpdateCallAnalyticsCategoryResponse -> TestTree
responseUpdateCallAnalyticsCategory =
  res
    "UpdateCallAnalyticsCategoryResponse"
    "fixture/UpdateCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCallAnalyticsCategory)

responseCreateMedicalVocabulary :: CreateMedicalVocabularyResponse -> TestTree
responseCreateMedicalVocabulary =
  res
    "CreateMedicalVocabularyResponse"
    "fixture/CreateMedicalVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMedicalVocabulary)

responseDeleteCallAnalyticsCategory :: DeleteCallAnalyticsCategoryResponse -> TestTree
responseDeleteCallAnalyticsCategory =
  res
    "DeleteCallAnalyticsCategoryResponse"
    "fixture/DeleteCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCallAnalyticsCategory)

responseListCallAnalyticsJobs :: ListCallAnalyticsJobsResponse -> TestTree
responseListCallAnalyticsJobs =
  res
    "ListCallAnalyticsJobsResponse"
    "fixture/ListCallAnalyticsJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCallAnalyticsJobs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteCallAnalyticsJob :: DeleteCallAnalyticsJobResponse -> TestTree
responseDeleteCallAnalyticsJob =
  res
    "DeleteCallAnalyticsJobResponse"
    "fixture/DeleteCallAnalyticsJobResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCallAnalyticsJob)

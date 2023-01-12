{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Transcribe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Transcribe where

import Amazonka.Transcribe
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Transcribe.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateCallAnalyticsCategory $
--             newCreateCallAnalyticsCategory
--
--         , requestCreateLanguageModel $
--             newCreateLanguageModel
--
--         , requestCreateMedicalVocabulary $
--             newCreateMedicalVocabulary
--
--         , requestCreateVocabulary $
--             newCreateVocabulary
--
--         , requestCreateVocabularyFilter $
--             newCreateVocabularyFilter
--
--         , requestDeleteCallAnalyticsCategory $
--             newDeleteCallAnalyticsCategory
--
--         , requestDeleteCallAnalyticsJob $
--             newDeleteCallAnalyticsJob
--
--         , requestDeleteLanguageModel $
--             newDeleteLanguageModel
--
--         , requestDeleteMedicalTranscriptionJob $
--             newDeleteMedicalTranscriptionJob
--
--         , requestDeleteMedicalVocabulary $
--             newDeleteMedicalVocabulary
--
--         , requestDeleteTranscriptionJob $
--             newDeleteTranscriptionJob
--
--         , requestDeleteVocabulary $
--             newDeleteVocabulary
--
--         , requestDeleteVocabularyFilter $
--             newDeleteVocabularyFilter
--
--         , requestDescribeLanguageModel $
--             newDescribeLanguageModel
--
--         , requestGetCallAnalyticsCategory $
--             newGetCallAnalyticsCategory
--
--         , requestGetCallAnalyticsJob $
--             newGetCallAnalyticsJob
--
--         , requestGetMedicalTranscriptionJob $
--             newGetMedicalTranscriptionJob
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
--         , requestGetVocabularyFilter $
--             newGetVocabularyFilter
--
--         , requestListCallAnalyticsCategories $
--             newListCallAnalyticsCategories
--
--         , requestListCallAnalyticsJobs $
--             newListCallAnalyticsJobs
--
--         , requestListLanguageModels $
--             newListLanguageModels
--
--         , requestListMedicalTranscriptionJobs $
--             newListMedicalTranscriptionJobs
--
--         , requestListMedicalVocabularies $
--             newListMedicalVocabularies
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTranscriptionJobs $
--             newListTranscriptionJobs
--
--         , requestListVocabularies $
--             newListVocabularies
--
--         , requestListVocabularyFilters $
--             newListVocabularyFilters
--
--         , requestStartCallAnalyticsJob $
--             newStartCallAnalyticsJob
--
--         , requestStartMedicalTranscriptionJob $
--             newStartMedicalTranscriptionJob
--
--         , requestStartTranscriptionJob $
--             newStartTranscriptionJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCallAnalyticsCategory $
--             newUpdateCallAnalyticsCategory
--
--         , requestUpdateMedicalVocabulary $
--             newUpdateMedicalVocabulary
--
--         , requestUpdateVocabulary $
--             newUpdateVocabulary
--
--         , requestUpdateVocabularyFilter $
--             newUpdateVocabularyFilter
--
--           ]

--     , testGroup "response"
--         [ responseCreateCallAnalyticsCategory $
--             newCreateCallAnalyticsCategoryResponse
--
--         , responseCreateLanguageModel $
--             newCreateLanguageModelResponse
--
--         , responseCreateMedicalVocabulary $
--             newCreateMedicalVocabularyResponse
--
--         , responseCreateVocabulary $
--             newCreateVocabularyResponse
--
--         , responseCreateVocabularyFilter $
--             newCreateVocabularyFilterResponse
--
--         , responseDeleteCallAnalyticsCategory $
--             newDeleteCallAnalyticsCategoryResponse
--
--         , responseDeleteCallAnalyticsJob $
--             newDeleteCallAnalyticsJobResponse
--
--         , responseDeleteLanguageModel $
--             newDeleteLanguageModelResponse
--
--         , responseDeleteMedicalTranscriptionJob $
--             newDeleteMedicalTranscriptionJobResponse
--
--         , responseDeleteMedicalVocabulary $
--             newDeleteMedicalVocabularyResponse
--
--         , responseDeleteTranscriptionJob $
--             newDeleteTranscriptionJobResponse
--
--         , responseDeleteVocabulary $
--             newDeleteVocabularyResponse
--
--         , responseDeleteVocabularyFilter $
--             newDeleteVocabularyFilterResponse
--
--         , responseDescribeLanguageModel $
--             newDescribeLanguageModelResponse
--
--         , responseGetCallAnalyticsCategory $
--             newGetCallAnalyticsCategoryResponse
--
--         , responseGetCallAnalyticsJob $
--             newGetCallAnalyticsJobResponse
--
--         , responseGetMedicalTranscriptionJob $
--             newGetMedicalTranscriptionJobResponse
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
--         , responseGetVocabularyFilter $
--             newGetVocabularyFilterResponse
--
--         , responseListCallAnalyticsCategories $
--             newListCallAnalyticsCategoriesResponse
--
--         , responseListCallAnalyticsJobs $
--             newListCallAnalyticsJobsResponse
--
--         , responseListLanguageModels $
--             newListLanguageModelsResponse
--
--         , responseListMedicalTranscriptionJobs $
--             newListMedicalTranscriptionJobsResponse
--
--         , responseListMedicalVocabularies $
--             newListMedicalVocabulariesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTranscriptionJobs $
--             newListTranscriptionJobsResponse
--
--         , responseListVocabularies $
--             newListVocabulariesResponse
--
--         , responseListVocabularyFilters $
--             newListVocabularyFiltersResponse
--
--         , responseStartCallAnalyticsJob $
--             newStartCallAnalyticsJobResponse
--
--         , responseStartMedicalTranscriptionJob $
--             newStartMedicalTranscriptionJobResponse
--
--         , responseStartTranscriptionJob $
--             newStartTranscriptionJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCallAnalyticsCategory $
--             newUpdateCallAnalyticsCategoryResponse
--
--         , responseUpdateMedicalVocabulary $
--             newUpdateMedicalVocabularyResponse
--
--         , responseUpdateVocabulary $
--             newUpdateVocabularyResponse
--
--         , responseUpdateVocabularyFilter $
--             newUpdateVocabularyFilterResponse
--
--           ]
--     ]

-- Requests

requestCreateCallAnalyticsCategory :: CreateCallAnalyticsCategory -> TestTree
requestCreateCallAnalyticsCategory =
  req
    "CreateCallAnalyticsCategory"
    "fixture/CreateCallAnalyticsCategory.yaml"

requestCreateLanguageModel :: CreateLanguageModel -> TestTree
requestCreateLanguageModel =
  req
    "CreateLanguageModel"
    "fixture/CreateLanguageModel.yaml"

requestCreateMedicalVocabulary :: CreateMedicalVocabulary -> TestTree
requestCreateMedicalVocabulary =
  req
    "CreateMedicalVocabulary"
    "fixture/CreateMedicalVocabulary.yaml"

requestCreateVocabulary :: CreateVocabulary -> TestTree
requestCreateVocabulary =
  req
    "CreateVocabulary"
    "fixture/CreateVocabulary.yaml"

requestCreateVocabularyFilter :: CreateVocabularyFilter -> TestTree
requestCreateVocabularyFilter =
  req
    "CreateVocabularyFilter"
    "fixture/CreateVocabularyFilter.yaml"

requestDeleteCallAnalyticsCategory :: DeleteCallAnalyticsCategory -> TestTree
requestDeleteCallAnalyticsCategory =
  req
    "DeleteCallAnalyticsCategory"
    "fixture/DeleteCallAnalyticsCategory.yaml"

requestDeleteCallAnalyticsJob :: DeleteCallAnalyticsJob -> TestTree
requestDeleteCallAnalyticsJob =
  req
    "DeleteCallAnalyticsJob"
    "fixture/DeleteCallAnalyticsJob.yaml"

requestDeleteLanguageModel :: DeleteLanguageModel -> TestTree
requestDeleteLanguageModel =
  req
    "DeleteLanguageModel"
    "fixture/DeleteLanguageModel.yaml"

requestDeleteMedicalTranscriptionJob :: DeleteMedicalTranscriptionJob -> TestTree
requestDeleteMedicalTranscriptionJob =
  req
    "DeleteMedicalTranscriptionJob"
    "fixture/DeleteMedicalTranscriptionJob.yaml"

requestDeleteMedicalVocabulary :: DeleteMedicalVocabulary -> TestTree
requestDeleteMedicalVocabulary =
  req
    "DeleteMedicalVocabulary"
    "fixture/DeleteMedicalVocabulary.yaml"

requestDeleteTranscriptionJob :: DeleteTranscriptionJob -> TestTree
requestDeleteTranscriptionJob =
  req
    "DeleteTranscriptionJob"
    "fixture/DeleteTranscriptionJob.yaml"

requestDeleteVocabulary :: DeleteVocabulary -> TestTree
requestDeleteVocabulary =
  req
    "DeleteVocabulary"
    "fixture/DeleteVocabulary.yaml"

requestDeleteVocabularyFilter :: DeleteVocabularyFilter -> TestTree
requestDeleteVocabularyFilter =
  req
    "DeleteVocabularyFilter"
    "fixture/DeleteVocabularyFilter.yaml"

requestDescribeLanguageModel :: DescribeLanguageModel -> TestTree
requestDescribeLanguageModel =
  req
    "DescribeLanguageModel"
    "fixture/DescribeLanguageModel.yaml"

requestGetCallAnalyticsCategory :: GetCallAnalyticsCategory -> TestTree
requestGetCallAnalyticsCategory =
  req
    "GetCallAnalyticsCategory"
    "fixture/GetCallAnalyticsCategory.yaml"

requestGetCallAnalyticsJob :: GetCallAnalyticsJob -> TestTree
requestGetCallAnalyticsJob =
  req
    "GetCallAnalyticsJob"
    "fixture/GetCallAnalyticsJob.yaml"

requestGetMedicalTranscriptionJob :: GetMedicalTranscriptionJob -> TestTree
requestGetMedicalTranscriptionJob =
  req
    "GetMedicalTranscriptionJob"
    "fixture/GetMedicalTranscriptionJob.yaml"

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

requestGetVocabularyFilter :: GetVocabularyFilter -> TestTree
requestGetVocabularyFilter =
  req
    "GetVocabularyFilter"
    "fixture/GetVocabularyFilter.yaml"

requestListCallAnalyticsCategories :: ListCallAnalyticsCategories -> TestTree
requestListCallAnalyticsCategories =
  req
    "ListCallAnalyticsCategories"
    "fixture/ListCallAnalyticsCategories.yaml"

requestListCallAnalyticsJobs :: ListCallAnalyticsJobs -> TestTree
requestListCallAnalyticsJobs =
  req
    "ListCallAnalyticsJobs"
    "fixture/ListCallAnalyticsJobs.yaml"

requestListLanguageModels :: ListLanguageModels -> TestTree
requestListLanguageModels =
  req
    "ListLanguageModels"
    "fixture/ListLanguageModels.yaml"

requestListMedicalTranscriptionJobs :: ListMedicalTranscriptionJobs -> TestTree
requestListMedicalTranscriptionJobs =
  req
    "ListMedicalTranscriptionJobs"
    "fixture/ListMedicalTranscriptionJobs.yaml"

requestListMedicalVocabularies :: ListMedicalVocabularies -> TestTree
requestListMedicalVocabularies =
  req
    "ListMedicalVocabularies"
    "fixture/ListMedicalVocabularies.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTranscriptionJobs :: ListTranscriptionJobs -> TestTree
requestListTranscriptionJobs =
  req
    "ListTranscriptionJobs"
    "fixture/ListTranscriptionJobs.yaml"

requestListVocabularies :: ListVocabularies -> TestTree
requestListVocabularies =
  req
    "ListVocabularies"
    "fixture/ListVocabularies.yaml"

requestListVocabularyFilters :: ListVocabularyFilters -> TestTree
requestListVocabularyFilters =
  req
    "ListVocabularyFilters"
    "fixture/ListVocabularyFilters.yaml"

requestStartCallAnalyticsJob :: StartCallAnalyticsJob -> TestTree
requestStartCallAnalyticsJob =
  req
    "StartCallAnalyticsJob"
    "fixture/StartCallAnalyticsJob.yaml"

requestStartMedicalTranscriptionJob :: StartMedicalTranscriptionJob -> TestTree
requestStartMedicalTranscriptionJob =
  req
    "StartMedicalTranscriptionJob"
    "fixture/StartMedicalTranscriptionJob.yaml"

requestStartTranscriptionJob :: StartTranscriptionJob -> TestTree
requestStartTranscriptionJob =
  req
    "StartTranscriptionJob"
    "fixture/StartTranscriptionJob.yaml"

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

requestUpdateCallAnalyticsCategory :: UpdateCallAnalyticsCategory -> TestTree
requestUpdateCallAnalyticsCategory =
  req
    "UpdateCallAnalyticsCategory"
    "fixture/UpdateCallAnalyticsCategory.yaml"

requestUpdateMedicalVocabulary :: UpdateMedicalVocabulary -> TestTree
requestUpdateMedicalVocabulary =
  req
    "UpdateMedicalVocabulary"
    "fixture/UpdateMedicalVocabulary.yaml"

requestUpdateVocabulary :: UpdateVocabulary -> TestTree
requestUpdateVocabulary =
  req
    "UpdateVocabulary"
    "fixture/UpdateVocabulary.yaml"

requestUpdateVocabularyFilter :: UpdateVocabularyFilter -> TestTree
requestUpdateVocabularyFilter =
  req
    "UpdateVocabularyFilter"
    "fixture/UpdateVocabularyFilter.yaml"

-- Responses

responseCreateCallAnalyticsCategory :: CreateCallAnalyticsCategoryResponse -> TestTree
responseCreateCallAnalyticsCategory =
  res
    "CreateCallAnalyticsCategoryResponse"
    "fixture/CreateCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCallAnalyticsCategory)

responseCreateLanguageModel :: CreateLanguageModelResponse -> TestTree
responseCreateLanguageModel =
  res
    "CreateLanguageModelResponse"
    "fixture/CreateLanguageModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLanguageModel)

responseCreateMedicalVocabulary :: CreateMedicalVocabularyResponse -> TestTree
responseCreateMedicalVocabulary =
  res
    "CreateMedicalVocabularyResponse"
    "fixture/CreateMedicalVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMedicalVocabulary)

responseCreateVocabulary :: CreateVocabularyResponse -> TestTree
responseCreateVocabulary =
  res
    "CreateVocabularyResponse"
    "fixture/CreateVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVocabulary)

responseCreateVocabularyFilter :: CreateVocabularyFilterResponse -> TestTree
responseCreateVocabularyFilter =
  res
    "CreateVocabularyFilterResponse"
    "fixture/CreateVocabularyFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVocabularyFilter)

responseDeleteCallAnalyticsCategory :: DeleteCallAnalyticsCategoryResponse -> TestTree
responseDeleteCallAnalyticsCategory =
  res
    "DeleteCallAnalyticsCategoryResponse"
    "fixture/DeleteCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCallAnalyticsCategory)

responseDeleteCallAnalyticsJob :: DeleteCallAnalyticsJobResponse -> TestTree
responseDeleteCallAnalyticsJob =
  res
    "DeleteCallAnalyticsJobResponse"
    "fixture/DeleteCallAnalyticsJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCallAnalyticsJob)

responseDeleteLanguageModel :: DeleteLanguageModelResponse -> TestTree
responseDeleteLanguageModel =
  res
    "DeleteLanguageModelResponse"
    "fixture/DeleteLanguageModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLanguageModel)

responseDeleteMedicalTranscriptionJob :: DeleteMedicalTranscriptionJobResponse -> TestTree
responseDeleteMedicalTranscriptionJob =
  res
    "DeleteMedicalTranscriptionJobResponse"
    "fixture/DeleteMedicalTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMedicalTranscriptionJob)

responseDeleteMedicalVocabulary :: DeleteMedicalVocabularyResponse -> TestTree
responseDeleteMedicalVocabulary =
  res
    "DeleteMedicalVocabularyResponse"
    "fixture/DeleteMedicalVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMedicalVocabulary)

responseDeleteTranscriptionJob :: DeleteTranscriptionJobResponse -> TestTree
responseDeleteTranscriptionJob =
  res
    "DeleteTranscriptionJobResponse"
    "fixture/DeleteTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTranscriptionJob)

responseDeleteVocabulary :: DeleteVocabularyResponse -> TestTree
responseDeleteVocabulary =
  res
    "DeleteVocabularyResponse"
    "fixture/DeleteVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVocabulary)

responseDeleteVocabularyFilter :: DeleteVocabularyFilterResponse -> TestTree
responseDeleteVocabularyFilter =
  res
    "DeleteVocabularyFilterResponse"
    "fixture/DeleteVocabularyFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVocabularyFilter)

responseDescribeLanguageModel :: DescribeLanguageModelResponse -> TestTree
responseDescribeLanguageModel =
  res
    "DescribeLanguageModelResponse"
    "fixture/DescribeLanguageModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLanguageModel)

responseGetCallAnalyticsCategory :: GetCallAnalyticsCategoryResponse -> TestTree
responseGetCallAnalyticsCategory =
  res
    "GetCallAnalyticsCategoryResponse"
    "fixture/GetCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCallAnalyticsCategory)

responseGetCallAnalyticsJob :: GetCallAnalyticsJobResponse -> TestTree
responseGetCallAnalyticsJob =
  res
    "GetCallAnalyticsJobResponse"
    "fixture/GetCallAnalyticsJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCallAnalyticsJob)

responseGetMedicalTranscriptionJob :: GetMedicalTranscriptionJobResponse -> TestTree
responseGetMedicalTranscriptionJob =
  res
    "GetMedicalTranscriptionJobResponse"
    "fixture/GetMedicalTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMedicalTranscriptionJob)

responseGetMedicalVocabulary :: GetMedicalVocabularyResponse -> TestTree
responseGetMedicalVocabulary =
  res
    "GetMedicalVocabularyResponse"
    "fixture/GetMedicalVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMedicalVocabulary)

responseGetTranscriptionJob :: GetTranscriptionJobResponse -> TestTree
responseGetTranscriptionJob =
  res
    "GetTranscriptionJobResponse"
    "fixture/GetTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTranscriptionJob)

responseGetVocabulary :: GetVocabularyResponse -> TestTree
responseGetVocabulary =
  res
    "GetVocabularyResponse"
    "fixture/GetVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVocabulary)

responseGetVocabularyFilter :: GetVocabularyFilterResponse -> TestTree
responseGetVocabularyFilter =
  res
    "GetVocabularyFilterResponse"
    "fixture/GetVocabularyFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVocabularyFilter)

responseListCallAnalyticsCategories :: ListCallAnalyticsCategoriesResponse -> TestTree
responseListCallAnalyticsCategories =
  res
    "ListCallAnalyticsCategoriesResponse"
    "fixture/ListCallAnalyticsCategoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCallAnalyticsCategories)

responseListCallAnalyticsJobs :: ListCallAnalyticsJobsResponse -> TestTree
responseListCallAnalyticsJobs =
  res
    "ListCallAnalyticsJobsResponse"
    "fixture/ListCallAnalyticsJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCallAnalyticsJobs)

responseListLanguageModels :: ListLanguageModelsResponse -> TestTree
responseListLanguageModels =
  res
    "ListLanguageModelsResponse"
    "fixture/ListLanguageModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLanguageModels)

responseListMedicalTranscriptionJobs :: ListMedicalTranscriptionJobsResponse -> TestTree
responseListMedicalTranscriptionJobs =
  res
    "ListMedicalTranscriptionJobsResponse"
    "fixture/ListMedicalTranscriptionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMedicalTranscriptionJobs)

responseListMedicalVocabularies :: ListMedicalVocabulariesResponse -> TestTree
responseListMedicalVocabularies =
  res
    "ListMedicalVocabulariesResponse"
    "fixture/ListMedicalVocabulariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMedicalVocabularies)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTranscriptionJobs :: ListTranscriptionJobsResponse -> TestTree
responseListTranscriptionJobs =
  res
    "ListTranscriptionJobsResponse"
    "fixture/ListTranscriptionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTranscriptionJobs)

responseListVocabularies :: ListVocabulariesResponse -> TestTree
responseListVocabularies =
  res
    "ListVocabulariesResponse"
    "fixture/ListVocabulariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVocabularies)

responseListVocabularyFilters :: ListVocabularyFiltersResponse -> TestTree
responseListVocabularyFilters =
  res
    "ListVocabularyFiltersResponse"
    "fixture/ListVocabularyFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVocabularyFilters)

responseStartCallAnalyticsJob :: StartCallAnalyticsJobResponse -> TestTree
responseStartCallAnalyticsJob =
  res
    "StartCallAnalyticsJobResponse"
    "fixture/StartCallAnalyticsJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCallAnalyticsJob)

responseStartMedicalTranscriptionJob :: StartMedicalTranscriptionJobResponse -> TestTree
responseStartMedicalTranscriptionJob =
  res
    "StartMedicalTranscriptionJobResponse"
    "fixture/StartMedicalTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMedicalTranscriptionJob)

responseStartTranscriptionJob :: StartTranscriptionJobResponse -> TestTree
responseStartTranscriptionJob =
  res
    "StartTranscriptionJobResponse"
    "fixture/StartTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTranscriptionJob)

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

responseUpdateCallAnalyticsCategory :: UpdateCallAnalyticsCategoryResponse -> TestTree
responseUpdateCallAnalyticsCategory =
  res
    "UpdateCallAnalyticsCategoryResponse"
    "fixture/UpdateCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCallAnalyticsCategory)

responseUpdateMedicalVocabulary :: UpdateMedicalVocabularyResponse -> TestTree
responseUpdateMedicalVocabulary =
  res
    "UpdateMedicalVocabularyResponse"
    "fixture/UpdateMedicalVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMedicalVocabulary)

responseUpdateVocabulary :: UpdateVocabularyResponse -> TestTree
responseUpdateVocabulary =
  res
    "UpdateVocabularyResponse"
    "fixture/UpdateVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVocabulary)

responseUpdateVocabularyFilter :: UpdateVocabularyFilterResponse -> TestTree
responseUpdateVocabularyFilter =
  res
    "UpdateVocabularyFilterResponse"
    "fixture/UpdateVocabularyFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVocabularyFilter)

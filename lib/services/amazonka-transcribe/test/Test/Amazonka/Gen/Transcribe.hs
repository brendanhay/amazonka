{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Transcribe
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestListLanguageModels $
--             newListLanguageModels
--
--         , requestGetVocabulary $
--             newGetVocabulary
--
--         , requestDeleteLanguageModel $
--             newDeleteLanguageModel
--
--         , requestGetTranscriptionJob $
--             newGetTranscriptionJob
--
--         , requestStartMedicalTranscriptionJob $
--             newStartMedicalTranscriptionJob
--
--         , requestListCallAnalyticsJobs $
--             newListCallAnalyticsJobs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetCallAnalyticsCategory $
--             newGetCallAnalyticsCategory
--
--         , requestDeleteMedicalVocabulary $
--             newDeleteMedicalVocabulary
--
--         , requestUpdateMedicalVocabulary $
--             newUpdateMedicalVocabulary
--
--         , requestCreateCallAnalyticsCategory $
--             newCreateCallAnalyticsCategory
--
--         , requestDeleteTranscriptionJob $
--             newDeleteTranscriptionJob
--
--         , requestDescribeLanguageModel $
--             newDescribeLanguageModel
--
--         , requestListCallAnalyticsCategories $
--             newListCallAnalyticsCategories
--
--         , requestDeleteMedicalTranscriptionJob $
--             newDeleteMedicalTranscriptionJob
--
--         , requestDeleteVocabulary $
--             newDeleteVocabulary
--
--         , requestStartCallAnalyticsJob $
--             newStartCallAnalyticsJob
--
--         , requestUpdateVocabulary $
--             newUpdateVocabulary
--
--         , requestCreateVocabularyFilter $
--             newCreateVocabularyFilter
--
--         , requestGetMedicalTranscriptionJob $
--             newGetMedicalTranscriptionJob
--
--         , requestGetVocabularyFilter $
--             newGetVocabularyFilter
--
--         , requestGetMedicalVocabulary $
--             newGetMedicalVocabulary
--
--         , requestDeleteCallAnalyticsJob $
--             newDeleteCallAnalyticsJob
--
--         , requestCreateMedicalVocabulary $
--             newCreateMedicalVocabulary
--
--         , requestListMedicalVocabularies $
--             newListMedicalVocabularies
--
--         , requestDeleteCallAnalyticsCategory $
--             newDeleteCallAnalyticsCategory
--
--         , requestUpdateCallAnalyticsCategory $
--             newUpdateCallAnalyticsCategory
--
--         , requestGetCallAnalyticsJob $
--             newGetCallAnalyticsJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListTranscriptionJobs $
--             newListTranscriptionJobs
--
--         , requestListMedicalTranscriptionJobs $
--             newListMedicalTranscriptionJobs
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteVocabularyFilter $
--             newDeleteVocabularyFilter
--
--         , requestListVocabularyFilters $
--             newListVocabularyFilters
--
--         , requestUpdateVocabularyFilter $
--             newUpdateVocabularyFilter
--
--         , requestListVocabularies $
--             newListVocabularies
--
--         , requestCreateVocabulary $
--             newCreateVocabulary
--
--         , requestCreateLanguageModel $
--             newCreateLanguageModel
--
--         , requestStartTranscriptionJob $
--             newStartTranscriptionJob
--
--           ]

--     , testGroup "response"
--         [ responseListLanguageModels $
--             newListLanguageModelsResponse
--
--         , responseGetVocabulary $
--             newGetVocabularyResponse
--
--         , responseDeleteLanguageModel $
--             newDeleteLanguageModelResponse
--
--         , responseGetTranscriptionJob $
--             newGetTranscriptionJobResponse
--
--         , responseStartMedicalTranscriptionJob $
--             newStartMedicalTranscriptionJobResponse
--
--         , responseListCallAnalyticsJobs $
--             newListCallAnalyticsJobsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetCallAnalyticsCategory $
--             newGetCallAnalyticsCategoryResponse
--
--         , responseDeleteMedicalVocabulary $
--             newDeleteMedicalVocabularyResponse
--
--         , responseUpdateMedicalVocabulary $
--             newUpdateMedicalVocabularyResponse
--
--         , responseCreateCallAnalyticsCategory $
--             newCreateCallAnalyticsCategoryResponse
--
--         , responseDeleteTranscriptionJob $
--             newDeleteTranscriptionJobResponse
--
--         , responseDescribeLanguageModel $
--             newDescribeLanguageModelResponse
--
--         , responseListCallAnalyticsCategories $
--             newListCallAnalyticsCategoriesResponse
--
--         , responseDeleteMedicalTranscriptionJob $
--             newDeleteMedicalTranscriptionJobResponse
--
--         , responseDeleteVocabulary $
--             newDeleteVocabularyResponse
--
--         , responseStartCallAnalyticsJob $
--             newStartCallAnalyticsJobResponse
--
--         , responseUpdateVocabulary $
--             newUpdateVocabularyResponse
--
--         , responseCreateVocabularyFilter $
--             newCreateVocabularyFilterResponse
--
--         , responseGetMedicalTranscriptionJob $
--             newGetMedicalTranscriptionJobResponse
--
--         , responseGetVocabularyFilter $
--             newGetVocabularyFilterResponse
--
--         , responseGetMedicalVocabulary $
--             newGetMedicalVocabularyResponse
--
--         , responseDeleteCallAnalyticsJob $
--             newDeleteCallAnalyticsJobResponse
--
--         , responseCreateMedicalVocabulary $
--             newCreateMedicalVocabularyResponse
--
--         , responseListMedicalVocabularies $
--             newListMedicalVocabulariesResponse
--
--         , responseDeleteCallAnalyticsCategory $
--             newDeleteCallAnalyticsCategoryResponse
--
--         , responseUpdateCallAnalyticsCategory $
--             newUpdateCallAnalyticsCategoryResponse
--
--         , responseGetCallAnalyticsJob $
--             newGetCallAnalyticsJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListTranscriptionJobs $
--             newListTranscriptionJobsResponse
--
--         , responseListMedicalTranscriptionJobs $
--             newListMedicalTranscriptionJobsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteVocabularyFilter $
--             newDeleteVocabularyFilterResponse
--
--         , responseListVocabularyFilters $
--             newListVocabularyFiltersResponse
--
--         , responseUpdateVocabularyFilter $
--             newUpdateVocabularyFilterResponse
--
--         , responseListVocabularies $
--             newListVocabulariesResponse
--
--         , responseCreateVocabulary $
--             newCreateVocabularyResponse
--
--         , responseCreateLanguageModel $
--             newCreateLanguageModelResponse
--
--         , responseStartTranscriptionJob $
--             newStartTranscriptionJobResponse
--
--           ]
--     ]

-- Requests

requestListLanguageModels :: ListLanguageModels -> TestTree
requestListLanguageModels =
  req
    "ListLanguageModels"
    "fixture/ListLanguageModels.yaml"

requestGetVocabulary :: GetVocabulary -> TestTree
requestGetVocabulary =
  req
    "GetVocabulary"
    "fixture/GetVocabulary.yaml"

requestDeleteLanguageModel :: DeleteLanguageModel -> TestTree
requestDeleteLanguageModel =
  req
    "DeleteLanguageModel"
    "fixture/DeleteLanguageModel.yaml"

requestGetTranscriptionJob :: GetTranscriptionJob -> TestTree
requestGetTranscriptionJob =
  req
    "GetTranscriptionJob"
    "fixture/GetTranscriptionJob.yaml"

requestStartMedicalTranscriptionJob :: StartMedicalTranscriptionJob -> TestTree
requestStartMedicalTranscriptionJob =
  req
    "StartMedicalTranscriptionJob"
    "fixture/StartMedicalTranscriptionJob.yaml"

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

requestGetCallAnalyticsCategory :: GetCallAnalyticsCategory -> TestTree
requestGetCallAnalyticsCategory =
  req
    "GetCallAnalyticsCategory"
    "fixture/GetCallAnalyticsCategory.yaml"

requestDeleteMedicalVocabulary :: DeleteMedicalVocabulary -> TestTree
requestDeleteMedicalVocabulary =
  req
    "DeleteMedicalVocabulary"
    "fixture/DeleteMedicalVocabulary.yaml"

requestUpdateMedicalVocabulary :: UpdateMedicalVocabulary -> TestTree
requestUpdateMedicalVocabulary =
  req
    "UpdateMedicalVocabulary"
    "fixture/UpdateMedicalVocabulary.yaml"

requestCreateCallAnalyticsCategory :: CreateCallAnalyticsCategory -> TestTree
requestCreateCallAnalyticsCategory =
  req
    "CreateCallAnalyticsCategory"
    "fixture/CreateCallAnalyticsCategory.yaml"

requestDeleteTranscriptionJob :: DeleteTranscriptionJob -> TestTree
requestDeleteTranscriptionJob =
  req
    "DeleteTranscriptionJob"
    "fixture/DeleteTranscriptionJob.yaml"

requestDescribeLanguageModel :: DescribeLanguageModel -> TestTree
requestDescribeLanguageModel =
  req
    "DescribeLanguageModel"
    "fixture/DescribeLanguageModel.yaml"

requestListCallAnalyticsCategories :: ListCallAnalyticsCategories -> TestTree
requestListCallAnalyticsCategories =
  req
    "ListCallAnalyticsCategories"
    "fixture/ListCallAnalyticsCategories.yaml"

requestDeleteMedicalTranscriptionJob :: DeleteMedicalTranscriptionJob -> TestTree
requestDeleteMedicalTranscriptionJob =
  req
    "DeleteMedicalTranscriptionJob"
    "fixture/DeleteMedicalTranscriptionJob.yaml"

requestDeleteVocabulary :: DeleteVocabulary -> TestTree
requestDeleteVocabulary =
  req
    "DeleteVocabulary"
    "fixture/DeleteVocabulary.yaml"

requestStartCallAnalyticsJob :: StartCallAnalyticsJob -> TestTree
requestStartCallAnalyticsJob =
  req
    "StartCallAnalyticsJob"
    "fixture/StartCallAnalyticsJob.yaml"

requestUpdateVocabulary :: UpdateVocabulary -> TestTree
requestUpdateVocabulary =
  req
    "UpdateVocabulary"
    "fixture/UpdateVocabulary.yaml"

requestCreateVocabularyFilter :: CreateVocabularyFilter -> TestTree
requestCreateVocabularyFilter =
  req
    "CreateVocabularyFilter"
    "fixture/CreateVocabularyFilter.yaml"

requestGetMedicalTranscriptionJob :: GetMedicalTranscriptionJob -> TestTree
requestGetMedicalTranscriptionJob =
  req
    "GetMedicalTranscriptionJob"
    "fixture/GetMedicalTranscriptionJob.yaml"

requestGetVocabularyFilter :: GetVocabularyFilter -> TestTree
requestGetVocabularyFilter =
  req
    "GetVocabularyFilter"
    "fixture/GetVocabularyFilter.yaml"

requestGetMedicalVocabulary :: GetMedicalVocabulary -> TestTree
requestGetMedicalVocabulary =
  req
    "GetMedicalVocabulary"
    "fixture/GetMedicalVocabulary.yaml"

requestDeleteCallAnalyticsJob :: DeleteCallAnalyticsJob -> TestTree
requestDeleteCallAnalyticsJob =
  req
    "DeleteCallAnalyticsJob"
    "fixture/DeleteCallAnalyticsJob.yaml"

requestCreateMedicalVocabulary :: CreateMedicalVocabulary -> TestTree
requestCreateMedicalVocabulary =
  req
    "CreateMedicalVocabulary"
    "fixture/CreateMedicalVocabulary.yaml"

requestListMedicalVocabularies :: ListMedicalVocabularies -> TestTree
requestListMedicalVocabularies =
  req
    "ListMedicalVocabularies"
    "fixture/ListMedicalVocabularies.yaml"

requestDeleteCallAnalyticsCategory :: DeleteCallAnalyticsCategory -> TestTree
requestDeleteCallAnalyticsCategory =
  req
    "DeleteCallAnalyticsCategory"
    "fixture/DeleteCallAnalyticsCategory.yaml"

requestUpdateCallAnalyticsCategory :: UpdateCallAnalyticsCategory -> TestTree
requestUpdateCallAnalyticsCategory =
  req
    "UpdateCallAnalyticsCategory"
    "fixture/UpdateCallAnalyticsCategory.yaml"

requestGetCallAnalyticsJob :: GetCallAnalyticsJob -> TestTree
requestGetCallAnalyticsJob =
  req
    "GetCallAnalyticsJob"
    "fixture/GetCallAnalyticsJob.yaml"

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

requestListMedicalTranscriptionJobs :: ListMedicalTranscriptionJobs -> TestTree
requestListMedicalTranscriptionJobs =
  req
    "ListMedicalTranscriptionJobs"
    "fixture/ListMedicalTranscriptionJobs.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteVocabularyFilter :: DeleteVocabularyFilter -> TestTree
requestDeleteVocabularyFilter =
  req
    "DeleteVocabularyFilter"
    "fixture/DeleteVocabularyFilter.yaml"

requestListVocabularyFilters :: ListVocabularyFilters -> TestTree
requestListVocabularyFilters =
  req
    "ListVocabularyFilters"
    "fixture/ListVocabularyFilters.yaml"

requestUpdateVocabularyFilter :: UpdateVocabularyFilter -> TestTree
requestUpdateVocabularyFilter =
  req
    "UpdateVocabularyFilter"
    "fixture/UpdateVocabularyFilter.yaml"

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

requestCreateLanguageModel :: CreateLanguageModel -> TestTree
requestCreateLanguageModel =
  req
    "CreateLanguageModel"
    "fixture/CreateLanguageModel.yaml"

requestStartTranscriptionJob :: StartTranscriptionJob -> TestTree
requestStartTranscriptionJob =
  req
    "StartTranscriptionJob"
    "fixture/StartTranscriptionJob.yaml"

-- Responses

responseListLanguageModels :: ListLanguageModelsResponse -> TestTree
responseListLanguageModels =
  res
    "ListLanguageModelsResponse"
    "fixture/ListLanguageModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLanguageModels)

responseGetVocabulary :: GetVocabularyResponse -> TestTree
responseGetVocabulary =
  res
    "GetVocabularyResponse"
    "fixture/GetVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVocabulary)

responseDeleteLanguageModel :: DeleteLanguageModelResponse -> TestTree
responseDeleteLanguageModel =
  res
    "DeleteLanguageModelResponse"
    "fixture/DeleteLanguageModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLanguageModel)

responseGetTranscriptionJob :: GetTranscriptionJobResponse -> TestTree
responseGetTranscriptionJob =
  res
    "GetTranscriptionJobResponse"
    "fixture/GetTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTranscriptionJob)

responseStartMedicalTranscriptionJob :: StartMedicalTranscriptionJobResponse -> TestTree
responseStartMedicalTranscriptionJob =
  res
    "StartMedicalTranscriptionJobResponse"
    "fixture/StartMedicalTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMedicalTranscriptionJob)

responseListCallAnalyticsJobs :: ListCallAnalyticsJobsResponse -> TestTree
responseListCallAnalyticsJobs =
  res
    "ListCallAnalyticsJobsResponse"
    "fixture/ListCallAnalyticsJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCallAnalyticsJobs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetCallAnalyticsCategory :: GetCallAnalyticsCategoryResponse -> TestTree
responseGetCallAnalyticsCategory =
  res
    "GetCallAnalyticsCategoryResponse"
    "fixture/GetCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCallAnalyticsCategory)

responseDeleteMedicalVocabulary :: DeleteMedicalVocabularyResponse -> TestTree
responseDeleteMedicalVocabulary =
  res
    "DeleteMedicalVocabularyResponse"
    "fixture/DeleteMedicalVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMedicalVocabulary)

responseUpdateMedicalVocabulary :: UpdateMedicalVocabularyResponse -> TestTree
responseUpdateMedicalVocabulary =
  res
    "UpdateMedicalVocabularyResponse"
    "fixture/UpdateMedicalVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMedicalVocabulary)

responseCreateCallAnalyticsCategory :: CreateCallAnalyticsCategoryResponse -> TestTree
responseCreateCallAnalyticsCategory =
  res
    "CreateCallAnalyticsCategoryResponse"
    "fixture/CreateCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCallAnalyticsCategory)

responseDeleteTranscriptionJob :: DeleteTranscriptionJobResponse -> TestTree
responseDeleteTranscriptionJob =
  res
    "DeleteTranscriptionJobResponse"
    "fixture/DeleteTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTranscriptionJob)

responseDescribeLanguageModel :: DescribeLanguageModelResponse -> TestTree
responseDescribeLanguageModel =
  res
    "DescribeLanguageModelResponse"
    "fixture/DescribeLanguageModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLanguageModel)

responseListCallAnalyticsCategories :: ListCallAnalyticsCategoriesResponse -> TestTree
responseListCallAnalyticsCategories =
  res
    "ListCallAnalyticsCategoriesResponse"
    "fixture/ListCallAnalyticsCategoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCallAnalyticsCategories)

responseDeleteMedicalTranscriptionJob :: DeleteMedicalTranscriptionJobResponse -> TestTree
responseDeleteMedicalTranscriptionJob =
  res
    "DeleteMedicalTranscriptionJobResponse"
    "fixture/DeleteMedicalTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMedicalTranscriptionJob)

responseDeleteVocabulary :: DeleteVocabularyResponse -> TestTree
responseDeleteVocabulary =
  res
    "DeleteVocabularyResponse"
    "fixture/DeleteVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVocabulary)

responseStartCallAnalyticsJob :: StartCallAnalyticsJobResponse -> TestTree
responseStartCallAnalyticsJob =
  res
    "StartCallAnalyticsJobResponse"
    "fixture/StartCallAnalyticsJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCallAnalyticsJob)

responseUpdateVocabulary :: UpdateVocabularyResponse -> TestTree
responseUpdateVocabulary =
  res
    "UpdateVocabularyResponse"
    "fixture/UpdateVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVocabulary)

responseCreateVocabularyFilter :: CreateVocabularyFilterResponse -> TestTree
responseCreateVocabularyFilter =
  res
    "CreateVocabularyFilterResponse"
    "fixture/CreateVocabularyFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVocabularyFilter)

responseGetMedicalTranscriptionJob :: GetMedicalTranscriptionJobResponse -> TestTree
responseGetMedicalTranscriptionJob =
  res
    "GetMedicalTranscriptionJobResponse"
    "fixture/GetMedicalTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMedicalTranscriptionJob)

responseGetVocabularyFilter :: GetVocabularyFilterResponse -> TestTree
responseGetVocabularyFilter =
  res
    "GetVocabularyFilterResponse"
    "fixture/GetVocabularyFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVocabularyFilter)

responseGetMedicalVocabulary :: GetMedicalVocabularyResponse -> TestTree
responseGetMedicalVocabulary =
  res
    "GetMedicalVocabularyResponse"
    "fixture/GetMedicalVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMedicalVocabulary)

responseDeleteCallAnalyticsJob :: DeleteCallAnalyticsJobResponse -> TestTree
responseDeleteCallAnalyticsJob =
  res
    "DeleteCallAnalyticsJobResponse"
    "fixture/DeleteCallAnalyticsJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCallAnalyticsJob)

responseCreateMedicalVocabulary :: CreateMedicalVocabularyResponse -> TestTree
responseCreateMedicalVocabulary =
  res
    "CreateMedicalVocabularyResponse"
    "fixture/CreateMedicalVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMedicalVocabulary)

responseListMedicalVocabularies :: ListMedicalVocabulariesResponse -> TestTree
responseListMedicalVocabularies =
  res
    "ListMedicalVocabulariesResponse"
    "fixture/ListMedicalVocabulariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMedicalVocabularies)

responseDeleteCallAnalyticsCategory :: DeleteCallAnalyticsCategoryResponse -> TestTree
responseDeleteCallAnalyticsCategory =
  res
    "DeleteCallAnalyticsCategoryResponse"
    "fixture/DeleteCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCallAnalyticsCategory)

responseUpdateCallAnalyticsCategory :: UpdateCallAnalyticsCategoryResponse -> TestTree
responseUpdateCallAnalyticsCategory =
  res
    "UpdateCallAnalyticsCategoryResponse"
    "fixture/UpdateCallAnalyticsCategoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCallAnalyticsCategory)

responseGetCallAnalyticsJob :: GetCallAnalyticsJobResponse -> TestTree
responseGetCallAnalyticsJob =
  res
    "GetCallAnalyticsJobResponse"
    "fixture/GetCallAnalyticsJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCallAnalyticsJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListTranscriptionJobs :: ListTranscriptionJobsResponse -> TestTree
responseListTranscriptionJobs =
  res
    "ListTranscriptionJobsResponse"
    "fixture/ListTranscriptionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTranscriptionJobs)

responseListMedicalTranscriptionJobs :: ListMedicalTranscriptionJobsResponse -> TestTree
responseListMedicalTranscriptionJobs =
  res
    "ListMedicalTranscriptionJobsResponse"
    "fixture/ListMedicalTranscriptionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMedicalTranscriptionJobs)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteVocabularyFilter :: DeleteVocabularyFilterResponse -> TestTree
responseDeleteVocabularyFilter =
  res
    "DeleteVocabularyFilterResponse"
    "fixture/DeleteVocabularyFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVocabularyFilter)

responseListVocabularyFilters :: ListVocabularyFiltersResponse -> TestTree
responseListVocabularyFilters =
  res
    "ListVocabularyFiltersResponse"
    "fixture/ListVocabularyFiltersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVocabularyFilters)

responseUpdateVocabularyFilter :: UpdateVocabularyFilterResponse -> TestTree
responseUpdateVocabularyFilter =
  res
    "UpdateVocabularyFilterResponse"
    "fixture/UpdateVocabularyFilterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVocabularyFilter)

responseListVocabularies :: ListVocabulariesResponse -> TestTree
responseListVocabularies =
  res
    "ListVocabulariesResponse"
    "fixture/ListVocabulariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVocabularies)

responseCreateVocabulary :: CreateVocabularyResponse -> TestTree
responseCreateVocabulary =
  res
    "CreateVocabularyResponse"
    "fixture/CreateVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVocabulary)

responseCreateLanguageModel :: CreateLanguageModelResponse -> TestTree
responseCreateLanguageModel =
  res
    "CreateLanguageModelResponse"
    "fixture/CreateLanguageModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLanguageModel)

responseStartTranscriptionJob :: StartTranscriptionJobResponse -> TestTree
responseStartTranscriptionJob =
  res
    "StartTranscriptionJobResponse"
    "fixture/StartTranscriptionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTranscriptionJob)

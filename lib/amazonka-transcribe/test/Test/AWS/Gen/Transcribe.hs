{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Transcribe
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--             mkListLanguageModels
--
--         , requestGetVocabulary $
--             mkGetVocabulary
--
--         , requestDeleteLanguageModel $
--             mkDeleteLanguageModel
--
--         , requestGetTranscriptionJob $
--             mkGetTranscriptionJob
--
--         , requestStartMedicalTranscriptionJob $
--             mkStartMedicalTranscriptionJob
--
--         , requestDeleteMedicalVocabulary $
--             mkDeleteMedicalVocabulary
--
--         , requestUpdateMedicalVocabulary $
--             mkUpdateMedicalVocabulary
--
--         , requestDeleteTranscriptionJob $
--             mkDeleteTranscriptionJob
--
--         , requestDescribeLanguageModel $
--             mkDescribeLanguageModel
--
--         , requestDeleteMedicalTranscriptionJob $
--             mkDeleteMedicalTranscriptionJob
--
--         , requestDeleteVocabulary $
--             mkDeleteVocabulary
--
--         , requestUpdateVocabulary $
--             mkUpdateVocabulary
--
--         , requestCreateVocabularyFilter $
--             mkCreateVocabularyFilter
--
--         , requestGetMedicalTranscriptionJob $
--             mkGetMedicalTranscriptionJob
--
--         , requestGetVocabularyFilter $
--             mkGetVocabularyFilter
--
--         , requestGetMedicalVocabulary $
--             mkGetMedicalVocabulary
--
--         , requestCreateMedicalVocabulary $
--             mkCreateMedicalVocabulary
--
--         , requestListMedicalVocabularies $
--             mkListMedicalVocabularies
--
--         , requestListTranscriptionJobs $
--             mkListTranscriptionJobs
--
--         , requestListMedicalTranscriptionJobs $
--             mkListMedicalTranscriptionJobs
--
--         , requestDeleteVocabularyFilter $
--             mkDeleteVocabularyFilter
--
--         , requestListVocabularyFilters $
--             mkListVocabularyFilters
--
--         , requestUpdateVocabularyFilter $
--             mkUpdateVocabularyFilter
--
--         , requestListVocabularies $
--             mkListVocabularies
--
--         , requestCreateVocabulary $
--             mkCreateVocabulary
--
--         , requestCreateLanguageModel $
--             mkCreateLanguageModel
--
--         , requestStartTranscriptionJob $
--             mkStartTranscriptionJob
--
--           ]

--     , testGroup "response"
--         [ responseListLanguageModels $
--             mkListLanguageModelsResponse
--
--         , responseGetVocabulary $
--             mkGetVocabularyResponse
--
--         , responseDeleteLanguageModel $
--             mkDeleteLanguageModelResponse
--
--         , responseGetTranscriptionJob $
--             mkGetTranscriptionJobResponse
--
--         , responseStartMedicalTranscriptionJob $
--             mkStartMedicalTranscriptionJobResponse
--
--         , responseDeleteMedicalVocabulary $
--             mkDeleteMedicalVocabularyResponse
--
--         , responseUpdateMedicalVocabulary $
--             mkUpdateMedicalVocabularyResponse
--
--         , responseDeleteTranscriptionJob $
--             mkDeleteTranscriptionJobResponse
--
--         , responseDescribeLanguageModel $
--             mkDescribeLanguageModelResponse
--
--         , responseDeleteMedicalTranscriptionJob $
--             mkDeleteMedicalTranscriptionJobResponse
--
--         , responseDeleteVocabulary $
--             mkDeleteVocabularyResponse
--
--         , responseUpdateVocabulary $
--             mkUpdateVocabularyResponse
--
--         , responseCreateVocabularyFilter $
--             mkCreateVocabularyFilterResponse
--
--         , responseGetMedicalTranscriptionJob $
--             mkGetMedicalTranscriptionJobResponse
--
--         , responseGetVocabularyFilter $
--             mkGetVocabularyFilterResponse
--
--         , responseGetMedicalVocabulary $
--             mkGetMedicalVocabularyResponse
--
--         , responseCreateMedicalVocabulary $
--             mkCreateMedicalVocabularyResponse
--
--         , responseListMedicalVocabularies $
--             mkListMedicalVocabulariesResponse
--
--         , responseListTranscriptionJobs $
--             mkListTranscriptionJobsResponse
--
--         , responseListMedicalTranscriptionJobs $
--             mkListMedicalTranscriptionJobsResponse
--
--         , responseDeleteVocabularyFilter $
--             mkDeleteVocabularyFilterResponse
--
--         , responseListVocabularyFilters $
--             mkListVocabularyFiltersResponse
--
--         , responseUpdateVocabularyFilter $
--             mkUpdateVocabularyFilterResponse
--
--         , responseListVocabularies $
--             mkListVocabulariesResponse
--
--         , responseCreateVocabulary $
--             mkCreateVocabularyResponse
--
--         , responseCreateLanguageModel $
--             mkCreateLanguageModelResponse
--
--         , responseStartTranscriptionJob $
--             mkStartTranscriptionJobResponse
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
    transcribeService
    (Proxy :: Proxy ListLanguageModels)

responseGetVocabulary :: GetVocabularyResponse -> TestTree
responseGetVocabulary =
  res
    "GetVocabularyResponse"
    "fixture/GetVocabularyResponse.proto"
    transcribeService
    (Proxy :: Proxy GetVocabulary)

responseDeleteLanguageModel :: DeleteLanguageModelResponse -> TestTree
responseDeleteLanguageModel =
  res
    "DeleteLanguageModelResponse"
    "fixture/DeleteLanguageModelResponse.proto"
    transcribeService
    (Proxy :: Proxy DeleteLanguageModel)

responseGetTranscriptionJob :: GetTranscriptionJobResponse -> TestTree
responseGetTranscriptionJob =
  res
    "GetTranscriptionJobResponse"
    "fixture/GetTranscriptionJobResponse.proto"
    transcribeService
    (Proxy :: Proxy GetTranscriptionJob)

responseStartMedicalTranscriptionJob :: StartMedicalTranscriptionJobResponse -> TestTree
responseStartMedicalTranscriptionJob =
  res
    "StartMedicalTranscriptionJobResponse"
    "fixture/StartMedicalTranscriptionJobResponse.proto"
    transcribeService
    (Proxy :: Proxy StartMedicalTranscriptionJob)

responseDeleteMedicalVocabulary :: DeleteMedicalVocabularyResponse -> TestTree
responseDeleteMedicalVocabulary =
  res
    "DeleteMedicalVocabularyResponse"
    "fixture/DeleteMedicalVocabularyResponse.proto"
    transcribeService
    (Proxy :: Proxy DeleteMedicalVocabulary)

responseUpdateMedicalVocabulary :: UpdateMedicalVocabularyResponse -> TestTree
responseUpdateMedicalVocabulary =
  res
    "UpdateMedicalVocabularyResponse"
    "fixture/UpdateMedicalVocabularyResponse.proto"
    transcribeService
    (Proxy :: Proxy UpdateMedicalVocabulary)

responseDeleteTranscriptionJob :: DeleteTranscriptionJobResponse -> TestTree
responseDeleteTranscriptionJob =
  res
    "DeleteTranscriptionJobResponse"
    "fixture/DeleteTranscriptionJobResponse.proto"
    transcribeService
    (Proxy :: Proxy DeleteTranscriptionJob)

responseDescribeLanguageModel :: DescribeLanguageModelResponse -> TestTree
responseDescribeLanguageModel =
  res
    "DescribeLanguageModelResponse"
    "fixture/DescribeLanguageModelResponse.proto"
    transcribeService
    (Proxy :: Proxy DescribeLanguageModel)

responseDeleteMedicalTranscriptionJob :: DeleteMedicalTranscriptionJobResponse -> TestTree
responseDeleteMedicalTranscriptionJob =
  res
    "DeleteMedicalTranscriptionJobResponse"
    "fixture/DeleteMedicalTranscriptionJobResponse.proto"
    transcribeService
    (Proxy :: Proxy DeleteMedicalTranscriptionJob)

responseDeleteVocabulary :: DeleteVocabularyResponse -> TestTree
responseDeleteVocabulary =
  res
    "DeleteVocabularyResponse"
    "fixture/DeleteVocabularyResponse.proto"
    transcribeService
    (Proxy :: Proxy DeleteVocabulary)

responseUpdateVocabulary :: UpdateVocabularyResponse -> TestTree
responseUpdateVocabulary =
  res
    "UpdateVocabularyResponse"
    "fixture/UpdateVocabularyResponse.proto"
    transcribeService
    (Proxy :: Proxy UpdateVocabulary)

responseCreateVocabularyFilter :: CreateVocabularyFilterResponse -> TestTree
responseCreateVocabularyFilter =
  res
    "CreateVocabularyFilterResponse"
    "fixture/CreateVocabularyFilterResponse.proto"
    transcribeService
    (Proxy :: Proxy CreateVocabularyFilter)

responseGetMedicalTranscriptionJob :: GetMedicalTranscriptionJobResponse -> TestTree
responseGetMedicalTranscriptionJob =
  res
    "GetMedicalTranscriptionJobResponse"
    "fixture/GetMedicalTranscriptionJobResponse.proto"
    transcribeService
    (Proxy :: Proxy GetMedicalTranscriptionJob)

responseGetVocabularyFilter :: GetVocabularyFilterResponse -> TestTree
responseGetVocabularyFilter =
  res
    "GetVocabularyFilterResponse"
    "fixture/GetVocabularyFilterResponse.proto"
    transcribeService
    (Proxy :: Proxy GetVocabularyFilter)

responseGetMedicalVocabulary :: GetMedicalVocabularyResponse -> TestTree
responseGetMedicalVocabulary =
  res
    "GetMedicalVocabularyResponse"
    "fixture/GetMedicalVocabularyResponse.proto"
    transcribeService
    (Proxy :: Proxy GetMedicalVocabulary)

responseCreateMedicalVocabulary :: CreateMedicalVocabularyResponse -> TestTree
responseCreateMedicalVocabulary =
  res
    "CreateMedicalVocabularyResponse"
    "fixture/CreateMedicalVocabularyResponse.proto"
    transcribeService
    (Proxy :: Proxy CreateMedicalVocabulary)

responseListMedicalVocabularies :: ListMedicalVocabulariesResponse -> TestTree
responseListMedicalVocabularies =
  res
    "ListMedicalVocabulariesResponse"
    "fixture/ListMedicalVocabulariesResponse.proto"
    transcribeService
    (Proxy :: Proxy ListMedicalVocabularies)

responseListTranscriptionJobs :: ListTranscriptionJobsResponse -> TestTree
responseListTranscriptionJobs =
  res
    "ListTranscriptionJobsResponse"
    "fixture/ListTranscriptionJobsResponse.proto"
    transcribeService
    (Proxy :: Proxy ListTranscriptionJobs)

responseListMedicalTranscriptionJobs :: ListMedicalTranscriptionJobsResponse -> TestTree
responseListMedicalTranscriptionJobs =
  res
    "ListMedicalTranscriptionJobsResponse"
    "fixture/ListMedicalTranscriptionJobsResponse.proto"
    transcribeService
    (Proxy :: Proxy ListMedicalTranscriptionJobs)

responseDeleteVocabularyFilter :: DeleteVocabularyFilterResponse -> TestTree
responseDeleteVocabularyFilter =
  res
    "DeleteVocabularyFilterResponse"
    "fixture/DeleteVocabularyFilterResponse.proto"
    transcribeService
    (Proxy :: Proxy DeleteVocabularyFilter)

responseListVocabularyFilters :: ListVocabularyFiltersResponse -> TestTree
responseListVocabularyFilters =
  res
    "ListVocabularyFiltersResponse"
    "fixture/ListVocabularyFiltersResponse.proto"
    transcribeService
    (Proxy :: Proxy ListVocabularyFilters)

responseUpdateVocabularyFilter :: UpdateVocabularyFilterResponse -> TestTree
responseUpdateVocabularyFilter =
  res
    "UpdateVocabularyFilterResponse"
    "fixture/UpdateVocabularyFilterResponse.proto"
    transcribeService
    (Proxy :: Proxy UpdateVocabularyFilter)

responseListVocabularies :: ListVocabulariesResponse -> TestTree
responseListVocabularies =
  res
    "ListVocabulariesResponse"
    "fixture/ListVocabulariesResponse.proto"
    transcribeService
    (Proxy :: Proxy ListVocabularies)

responseCreateVocabulary :: CreateVocabularyResponse -> TestTree
responseCreateVocabulary =
  res
    "CreateVocabularyResponse"
    "fixture/CreateVocabularyResponse.proto"
    transcribeService
    (Proxy :: Proxy CreateVocabulary)

responseCreateLanguageModel :: CreateLanguageModelResponse -> TestTree
responseCreateLanguageModel =
  res
    "CreateLanguageModelResponse"
    "fixture/CreateLanguageModelResponse.proto"
    transcribeService
    (Proxy :: Proxy CreateLanguageModel)

responseStartTranscriptionJob :: StartTranscriptionJobResponse -> TestTree
responseStartTranscriptionJob =
  res
    "StartTranscriptionJobResponse"
    "fixture/StartTranscriptionJobResponse.proto"
    transcribeService
    (Proxy :: Proxy StartTranscriptionJob)

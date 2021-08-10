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
--         [ requestGetVocabularyFilter $
--             newGetVocabularyFilter
--
--         , requestListLanguageModels $
--             newListLanguageModels
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
--         , requestUpdateVocabulary $
--             newUpdateVocabulary
--
--         , requestDeleteVocabulary $
--             newDeleteVocabulary
--
--         , requestListVocabularyFilters $
--             newListVocabularyFilters
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
--         , requestGetMedicalVocabulary $
--             newGetMedicalVocabulary
--
--         , requestGetTranscriptionJob $
--             newGetTranscriptionJob
--
--         , requestDeleteLanguageModel $
--             newDeleteLanguageModel
--
--         , requestGetVocabulary $
--             newGetVocabulary
--
--         , requestGetMedicalTranscriptionJob $
--             newGetMedicalTranscriptionJob
--
--         , requestCreateVocabularyFilter $
--             newCreateVocabularyFilter
--
--         , requestDeleteVocabularyFilter $
--             newDeleteVocabularyFilter
--
--         , requestListMedicalTranscriptionJobs $
--             newListMedicalTranscriptionJobs
--
--         , requestDeleteMedicalTranscriptionJob $
--             newDeleteMedicalTranscriptionJob
--
--         , requestUpdateVocabularyFilter $
--             newUpdateVocabularyFilter
--
--         , requestDeleteMedicalVocabulary $
--             newDeleteMedicalVocabulary
--
--         , requestUpdateMedicalVocabulary $
--             newUpdateMedicalVocabulary
--
--         , requestDescribeLanguageModel $
--             newDescribeLanguageModel
--
--         , requestCreateMedicalVocabulary $
--             newCreateMedicalVocabulary
--
--         , requestListMedicalVocabularies $
--             newListMedicalVocabularies
--
--           ]

--     , testGroup "response"
--         [ responseGetVocabularyFilter $
--             newGetVocabularyFilterResponse
--
--         , responseListLanguageModels $
--             newListLanguageModelsResponse
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
--         , responseUpdateVocabulary $
--             newUpdateVocabularyResponse
--
--         , responseDeleteVocabulary $
--             newDeleteVocabularyResponse
--
--         , responseListVocabularyFilters $
--             newListVocabularyFiltersResponse
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
--         , responseGetMedicalVocabulary $
--             newGetMedicalVocabularyResponse
--
--         , responseGetTranscriptionJob $
--             newGetTranscriptionJobResponse
--
--         , responseDeleteLanguageModel $
--             newDeleteLanguageModelResponse
--
--         , responseGetVocabulary $
--             newGetVocabularyResponse
--
--         , responseGetMedicalTranscriptionJob $
--             newGetMedicalTranscriptionJobResponse
--
--         , responseCreateVocabularyFilter $
--             newCreateVocabularyFilterResponse
--
--         , responseDeleteVocabularyFilter $
--             newDeleteVocabularyFilterResponse
--
--         , responseListMedicalTranscriptionJobs $
--             newListMedicalTranscriptionJobsResponse
--
--         , responseDeleteMedicalTranscriptionJob $
--             newDeleteMedicalTranscriptionJobResponse
--
--         , responseUpdateVocabularyFilter $
--             newUpdateVocabularyFilterResponse
--
--         , responseDeleteMedicalVocabulary $
--             newDeleteMedicalVocabularyResponse
--
--         , responseUpdateMedicalVocabulary $
--             newUpdateMedicalVocabularyResponse
--
--         , responseDescribeLanguageModel $
--             newDescribeLanguageModelResponse
--
--         , responseCreateMedicalVocabulary $
--             newCreateMedicalVocabularyResponse
--
--         , responseListMedicalVocabularies $
--             newListMedicalVocabulariesResponse
--
--           ]
--     ]

-- Requests

requestGetVocabularyFilter :: GetVocabularyFilter -> TestTree
requestGetVocabularyFilter =
  req
    "GetVocabularyFilter"
    "fixture/GetVocabularyFilter.yaml"

requestListLanguageModels :: ListLanguageModels -> TestTree
requestListLanguageModels =
  req
    "ListLanguageModels"
    "fixture/ListLanguageModels.yaml"

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

requestUpdateVocabulary :: UpdateVocabulary -> TestTree
requestUpdateVocabulary =
  req
    "UpdateVocabulary"
    "fixture/UpdateVocabulary.yaml"

requestDeleteVocabulary :: DeleteVocabulary -> TestTree
requestDeleteVocabulary =
  req
    "DeleteVocabulary"
    "fixture/DeleteVocabulary.yaml"

requestListVocabularyFilters :: ListVocabularyFilters -> TestTree
requestListVocabularyFilters =
  req
    "ListVocabularyFilters"
    "fixture/ListVocabularyFilters.yaml"

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

requestDeleteLanguageModel :: DeleteLanguageModel -> TestTree
requestDeleteLanguageModel =
  req
    "DeleteLanguageModel"
    "fixture/DeleteLanguageModel.yaml"

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

requestCreateVocabularyFilter :: CreateVocabularyFilter -> TestTree
requestCreateVocabularyFilter =
  req
    "CreateVocabularyFilter"
    "fixture/CreateVocabularyFilter.yaml"

requestDeleteVocabularyFilter :: DeleteVocabularyFilter -> TestTree
requestDeleteVocabularyFilter =
  req
    "DeleteVocabularyFilter"
    "fixture/DeleteVocabularyFilter.yaml"

requestListMedicalTranscriptionJobs :: ListMedicalTranscriptionJobs -> TestTree
requestListMedicalTranscriptionJobs =
  req
    "ListMedicalTranscriptionJobs"
    "fixture/ListMedicalTranscriptionJobs.yaml"

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

requestDescribeLanguageModel :: DescribeLanguageModel -> TestTree
requestDescribeLanguageModel =
  req
    "DescribeLanguageModel"
    "fixture/DescribeLanguageModel.yaml"

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

-- Responses

responseGetVocabularyFilter :: GetVocabularyFilterResponse -> TestTree
responseGetVocabularyFilter =
  res
    "GetVocabularyFilterResponse"
    "fixture/GetVocabularyFilterResponse.proto"
    defaultService
    (Proxy :: Proxy GetVocabularyFilter)

responseListLanguageModels :: ListLanguageModelsResponse -> TestTree
responseListLanguageModels =
  res
    "ListLanguageModelsResponse"
    "fixture/ListLanguageModelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLanguageModels)

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

responseUpdateVocabulary :: UpdateVocabularyResponse -> TestTree
responseUpdateVocabulary =
  res
    "UpdateVocabularyResponse"
    "fixture/UpdateVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVocabulary)

responseDeleteVocabulary :: DeleteVocabularyResponse -> TestTree
responseDeleteVocabulary =
  res
    "DeleteVocabularyResponse"
    "fixture/DeleteVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVocabulary)

responseListVocabularyFilters :: ListVocabularyFiltersResponse -> TestTree
responseListVocabularyFilters =
  res
    "ListVocabularyFiltersResponse"
    "fixture/ListVocabularyFiltersResponse.proto"
    defaultService
    (Proxy :: Proxy ListVocabularyFilters)

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

responseDeleteLanguageModel :: DeleteLanguageModelResponse -> TestTree
responseDeleteLanguageModel =
  res
    "DeleteLanguageModelResponse"
    "fixture/DeleteLanguageModelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLanguageModel)

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

responseCreateVocabularyFilter :: CreateVocabularyFilterResponse -> TestTree
responseCreateVocabularyFilter =
  res
    "CreateVocabularyFilterResponse"
    "fixture/CreateVocabularyFilterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVocabularyFilter)

responseDeleteVocabularyFilter :: DeleteVocabularyFilterResponse -> TestTree
responseDeleteVocabularyFilter =
  res
    "DeleteVocabularyFilterResponse"
    "fixture/DeleteVocabularyFilterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVocabularyFilter)

responseListMedicalTranscriptionJobs :: ListMedicalTranscriptionJobsResponse -> TestTree
responseListMedicalTranscriptionJobs =
  res
    "ListMedicalTranscriptionJobsResponse"
    "fixture/ListMedicalTranscriptionJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMedicalTranscriptionJobs)

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

responseDeleteMedicalVocabulary :: DeleteMedicalVocabularyResponse -> TestTree
responseDeleteMedicalVocabulary =
  res
    "DeleteMedicalVocabularyResponse"
    "fixture/DeleteMedicalVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMedicalVocabulary)

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

responseCreateMedicalVocabulary :: CreateMedicalVocabularyResponse -> TestTree
responseCreateMedicalVocabulary =
  res
    "CreateMedicalVocabularyResponse"
    "fixture/CreateMedicalVocabularyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMedicalVocabulary)

responseListMedicalVocabularies :: ListMedicalVocabulariesResponse -> TestTree
responseListMedicalVocabularies =
  res
    "ListMedicalVocabulariesResponse"
    "fixture/ListMedicalVocabulariesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMedicalVocabularies)

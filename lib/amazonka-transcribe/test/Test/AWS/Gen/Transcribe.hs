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
--             listLanguageModels
--
--         , requestGetVocabulary $
--             getVocabulary
--
--         , requestDeleteLanguageModel $
--             deleteLanguageModel
--
--         , requestGetTranscriptionJob $
--             getTranscriptionJob
--
--         , requestStartMedicalTranscriptionJob $
--             startMedicalTranscriptionJob
--
--         , requestDeleteMedicalVocabulary $
--             deleteMedicalVocabulary
--
--         , requestUpdateMedicalVocabulary $
--             updateMedicalVocabulary
--
--         , requestDeleteTranscriptionJob $
--             deleteTranscriptionJob
--
--         , requestDescribeLanguageModel $
--             describeLanguageModel
--
--         , requestDeleteMedicalTranscriptionJob $
--             deleteMedicalTranscriptionJob
--
--         , requestDeleteVocabulary $
--             deleteVocabulary
--
--         , requestUpdateVocabulary $
--             updateVocabulary
--
--         , requestCreateVocabularyFilter $
--             createVocabularyFilter
--
--         , requestGetMedicalTranscriptionJob $
--             getMedicalTranscriptionJob
--
--         , requestGetVocabularyFilter $
--             getVocabularyFilter
--
--         , requestGetMedicalVocabulary $
--             getMedicalVocabulary
--
--         , requestCreateMedicalVocabulary $
--             createMedicalVocabulary
--
--         , requestListMedicalVocabularies $
--             listMedicalVocabularies
--
--         , requestListTranscriptionJobs $
--             listTranscriptionJobs
--
--         , requestListMedicalTranscriptionJobs $
--             listMedicalTranscriptionJobs
--
--         , requestDeleteVocabularyFilter $
--             deleteVocabularyFilter
--
--         , requestListVocabularyFilters $
--             listVocabularyFilters
--
--         , requestUpdateVocabularyFilter $
--             updateVocabularyFilter
--
--         , requestListVocabularies $
--             listVocabularies
--
--         , requestCreateVocabulary $
--             createVocabulary
--
--         , requestCreateLanguageModel $
--             createLanguageModel
--
--         , requestStartTranscriptionJob $
--             startTranscriptionJob
--
--           ]

--     , testGroup "response"
--         [ responseListLanguageModels $
--             listLanguageModelsResponse
--
--         , responseGetVocabulary $
--             getVocabularyResponse
--
--         , responseDeleteLanguageModel $
--             deleteLanguageModelResponse
--
--         , responseGetTranscriptionJob $
--             getTranscriptionJobResponse
--
--         , responseStartMedicalTranscriptionJob $
--             startMedicalTranscriptionJobResponse
--
--         , responseDeleteMedicalVocabulary $
--             deleteMedicalVocabularyResponse
--
--         , responseUpdateMedicalVocabulary $
--             updateMedicalVocabularyResponse
--
--         , responseDeleteTranscriptionJob $
--             deleteTranscriptionJobResponse
--
--         , responseDescribeLanguageModel $
--             describeLanguageModelResponse
--
--         , responseDeleteMedicalTranscriptionJob $
--             deleteMedicalTranscriptionJobResponse
--
--         , responseDeleteVocabulary $
--             deleteVocabularyResponse
--
--         , responseUpdateVocabulary $
--             updateVocabularyResponse
--
--         , responseCreateVocabularyFilter $
--             createVocabularyFilterResponse
--
--         , responseGetMedicalTranscriptionJob $
--             getMedicalTranscriptionJobResponse
--
--         , responseGetVocabularyFilter $
--             getVocabularyFilterResponse
--
--         , responseGetMedicalVocabulary $
--             getMedicalVocabularyResponse
--
--         , responseCreateMedicalVocabulary $
--             createMedicalVocabularyResponse
--
--         , responseListMedicalVocabularies $
--             listMedicalVocabulariesResponse
--
--         , responseListTranscriptionJobs $
--             listTranscriptionJobsResponse
--
--         , responseListMedicalTranscriptionJobs $
--             listMedicalTranscriptionJobsResponse
--
--         , responseDeleteVocabularyFilter $
--             deleteVocabularyFilterResponse
--
--         , responseListVocabularyFilters $
--             listVocabularyFiltersResponse
--
--         , responseUpdateVocabularyFilter $
--             updateVocabularyFilterResponse
--
--         , responseListVocabularies $
--             listVocabulariesResponse
--
--         , responseCreateVocabulary $
--             createVocabularyResponse
--
--         , responseCreateLanguageModel $
--             createLanguageModelResponse
--
--         , responseStartTranscriptionJob $
--             startTranscriptionJobResponse
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
    transcribe
    (Proxy :: Proxy ListLanguageModels)

responseGetVocabulary :: GetVocabularyResponse -> TestTree
responseGetVocabulary =
  res
    "GetVocabularyResponse"
    "fixture/GetVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy GetVocabulary)

responseDeleteLanguageModel :: DeleteLanguageModelResponse -> TestTree
responseDeleteLanguageModel =
  res
    "DeleteLanguageModelResponse"
    "fixture/DeleteLanguageModelResponse.proto"
    transcribe
    (Proxy :: Proxy DeleteLanguageModel)

responseGetTranscriptionJob :: GetTranscriptionJobResponse -> TestTree
responseGetTranscriptionJob =
  res
    "GetTranscriptionJobResponse"
    "fixture/GetTranscriptionJobResponse.proto"
    transcribe
    (Proxy :: Proxy GetTranscriptionJob)

responseStartMedicalTranscriptionJob :: StartMedicalTranscriptionJobResponse -> TestTree
responseStartMedicalTranscriptionJob =
  res
    "StartMedicalTranscriptionJobResponse"
    "fixture/StartMedicalTranscriptionJobResponse.proto"
    transcribe
    (Proxy :: Proxy StartMedicalTranscriptionJob)

responseDeleteMedicalVocabulary :: DeleteMedicalVocabularyResponse -> TestTree
responseDeleteMedicalVocabulary =
  res
    "DeleteMedicalVocabularyResponse"
    "fixture/DeleteMedicalVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy DeleteMedicalVocabulary)

responseUpdateMedicalVocabulary :: UpdateMedicalVocabularyResponse -> TestTree
responseUpdateMedicalVocabulary =
  res
    "UpdateMedicalVocabularyResponse"
    "fixture/UpdateMedicalVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy UpdateMedicalVocabulary)

responseDeleteTranscriptionJob :: DeleteTranscriptionJobResponse -> TestTree
responseDeleteTranscriptionJob =
  res
    "DeleteTranscriptionJobResponse"
    "fixture/DeleteTranscriptionJobResponse.proto"
    transcribe
    (Proxy :: Proxy DeleteTranscriptionJob)

responseDescribeLanguageModel :: DescribeLanguageModelResponse -> TestTree
responseDescribeLanguageModel =
  res
    "DescribeLanguageModelResponse"
    "fixture/DescribeLanguageModelResponse.proto"
    transcribe
    (Proxy :: Proxy DescribeLanguageModel)

responseDeleteMedicalTranscriptionJob :: DeleteMedicalTranscriptionJobResponse -> TestTree
responseDeleteMedicalTranscriptionJob =
  res
    "DeleteMedicalTranscriptionJobResponse"
    "fixture/DeleteMedicalTranscriptionJobResponse.proto"
    transcribe
    (Proxy :: Proxy DeleteMedicalTranscriptionJob)

responseDeleteVocabulary :: DeleteVocabularyResponse -> TestTree
responseDeleteVocabulary =
  res
    "DeleteVocabularyResponse"
    "fixture/DeleteVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy DeleteVocabulary)

responseUpdateVocabulary :: UpdateVocabularyResponse -> TestTree
responseUpdateVocabulary =
  res
    "UpdateVocabularyResponse"
    "fixture/UpdateVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy UpdateVocabulary)

responseCreateVocabularyFilter :: CreateVocabularyFilterResponse -> TestTree
responseCreateVocabularyFilter =
  res
    "CreateVocabularyFilterResponse"
    "fixture/CreateVocabularyFilterResponse.proto"
    transcribe
    (Proxy :: Proxy CreateVocabularyFilter)

responseGetMedicalTranscriptionJob :: GetMedicalTranscriptionJobResponse -> TestTree
responseGetMedicalTranscriptionJob =
  res
    "GetMedicalTranscriptionJobResponse"
    "fixture/GetMedicalTranscriptionJobResponse.proto"
    transcribe
    (Proxy :: Proxy GetMedicalTranscriptionJob)

responseGetVocabularyFilter :: GetVocabularyFilterResponse -> TestTree
responseGetVocabularyFilter =
  res
    "GetVocabularyFilterResponse"
    "fixture/GetVocabularyFilterResponse.proto"
    transcribe
    (Proxy :: Proxy GetVocabularyFilter)

responseGetMedicalVocabulary :: GetMedicalVocabularyResponse -> TestTree
responseGetMedicalVocabulary =
  res
    "GetMedicalVocabularyResponse"
    "fixture/GetMedicalVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy GetMedicalVocabulary)

responseCreateMedicalVocabulary :: CreateMedicalVocabularyResponse -> TestTree
responseCreateMedicalVocabulary =
  res
    "CreateMedicalVocabularyResponse"
    "fixture/CreateMedicalVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy CreateMedicalVocabulary)

responseListMedicalVocabularies :: ListMedicalVocabulariesResponse -> TestTree
responseListMedicalVocabularies =
  res
    "ListMedicalVocabulariesResponse"
    "fixture/ListMedicalVocabulariesResponse.proto"
    transcribe
    (Proxy :: Proxy ListMedicalVocabularies)

responseListTranscriptionJobs :: ListTranscriptionJobsResponse -> TestTree
responseListTranscriptionJobs =
  res
    "ListTranscriptionJobsResponse"
    "fixture/ListTranscriptionJobsResponse.proto"
    transcribe
    (Proxy :: Proxy ListTranscriptionJobs)

responseListMedicalTranscriptionJobs :: ListMedicalTranscriptionJobsResponse -> TestTree
responseListMedicalTranscriptionJobs =
  res
    "ListMedicalTranscriptionJobsResponse"
    "fixture/ListMedicalTranscriptionJobsResponse.proto"
    transcribe
    (Proxy :: Proxy ListMedicalTranscriptionJobs)

responseDeleteVocabularyFilter :: DeleteVocabularyFilterResponse -> TestTree
responseDeleteVocabularyFilter =
  res
    "DeleteVocabularyFilterResponse"
    "fixture/DeleteVocabularyFilterResponse.proto"
    transcribe
    (Proxy :: Proxy DeleteVocabularyFilter)

responseListVocabularyFilters :: ListVocabularyFiltersResponse -> TestTree
responseListVocabularyFilters =
  res
    "ListVocabularyFiltersResponse"
    "fixture/ListVocabularyFiltersResponse.proto"
    transcribe
    (Proxy :: Proxy ListVocabularyFilters)

responseUpdateVocabularyFilter :: UpdateVocabularyFilterResponse -> TestTree
responseUpdateVocabularyFilter =
  res
    "UpdateVocabularyFilterResponse"
    "fixture/UpdateVocabularyFilterResponse.proto"
    transcribe
    (Proxy :: Proxy UpdateVocabularyFilter)

responseListVocabularies :: ListVocabulariesResponse -> TestTree
responseListVocabularies =
  res
    "ListVocabulariesResponse"
    "fixture/ListVocabulariesResponse.proto"
    transcribe
    (Proxy :: Proxy ListVocabularies)

responseCreateVocabulary :: CreateVocabularyResponse -> TestTree
responseCreateVocabulary =
  res
    "CreateVocabularyResponse"
    "fixture/CreateVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy CreateVocabulary)

responseCreateLanguageModel :: CreateLanguageModelResponse -> TestTree
responseCreateLanguageModel =
  res
    "CreateLanguageModelResponse"
    "fixture/CreateLanguageModelResponse.proto"
    transcribe
    (Proxy :: Proxy CreateLanguageModel)

responseStartTranscriptionJob :: StartTranscriptionJobResponse -> TestTree
responseStartTranscriptionJob =
  res
    "StartTranscriptionJobResponse"
    "fixture/StartTranscriptionJobResponse.proto"
    transcribe
    (Proxy :: Proxy StartTranscriptionJob)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Transcribe
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestGetVocabulary $
--             getVocabulary
--
--         , requestGetTranscriptionJob $
--             getTranscriptionJob
--
--         , requestDeleteVocabulary $
--             deleteVocabulary
--
--         , requestUpdateVocabulary $
--             updateVocabulary
--
--         , requestListTranscriptionJobs $
--             listTranscriptionJobs
--
--         , requestListVocabularies $
--             listVocabularies
--
--         , requestCreateVocabulary $
--             createVocabulary
--
--         , requestStartTranscriptionJob $
--             startTranscriptionJob
--
--           ]

--     , testGroup "response"
--         [ responseGetVocabulary $
--             getVocabularyResponse
--
--         , responseGetTranscriptionJob $
--             getTranscriptionJobResponse
--
--         , responseDeleteVocabulary $
--             deleteVocabularyResponse
--
--         , responseUpdateVocabulary $
--             updateVocabularyResponse
--
--         , responseListTranscriptionJobs $
--             listTranscriptionJobsResponse
--
--         , responseListVocabularies $
--             listVocabulariesResponse
--
--         , responseCreateVocabulary $
--             createVocabularyResponse
--
--         , responseStartTranscriptionJob $
--             startTranscriptionJobResponse
--
--           ]
--     ]

-- Requests

requestGetVocabulary :: GetVocabulary -> TestTree
requestGetVocabulary = req
    "GetVocabulary"
    "fixture/GetVocabulary.yaml"

requestGetTranscriptionJob :: GetTranscriptionJob -> TestTree
requestGetTranscriptionJob = req
    "GetTranscriptionJob"
    "fixture/GetTranscriptionJob.yaml"

requestDeleteVocabulary :: DeleteVocabulary -> TestTree
requestDeleteVocabulary = req
    "DeleteVocabulary"
    "fixture/DeleteVocabulary.yaml"

requestUpdateVocabulary :: UpdateVocabulary -> TestTree
requestUpdateVocabulary = req
    "UpdateVocabulary"
    "fixture/UpdateVocabulary.yaml"

requestListTranscriptionJobs :: ListTranscriptionJobs -> TestTree
requestListTranscriptionJobs = req
    "ListTranscriptionJobs"
    "fixture/ListTranscriptionJobs.yaml"

requestListVocabularies :: ListVocabularies -> TestTree
requestListVocabularies = req
    "ListVocabularies"
    "fixture/ListVocabularies.yaml"

requestCreateVocabulary :: CreateVocabulary -> TestTree
requestCreateVocabulary = req
    "CreateVocabulary"
    "fixture/CreateVocabulary.yaml"

requestStartTranscriptionJob :: StartTranscriptionJob -> TestTree
requestStartTranscriptionJob = req
    "StartTranscriptionJob"
    "fixture/StartTranscriptionJob.yaml"

-- Responses

responseGetVocabulary :: GetVocabularyResponse -> TestTree
responseGetVocabulary = res
    "GetVocabularyResponse"
    "fixture/GetVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy GetVocabulary)

responseGetTranscriptionJob :: GetTranscriptionJobResponse -> TestTree
responseGetTranscriptionJob = res
    "GetTranscriptionJobResponse"
    "fixture/GetTranscriptionJobResponse.proto"
    transcribe
    (Proxy :: Proxy GetTranscriptionJob)

responseDeleteVocabulary :: DeleteVocabularyResponse -> TestTree
responseDeleteVocabulary = res
    "DeleteVocabularyResponse"
    "fixture/DeleteVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy DeleteVocabulary)

responseUpdateVocabulary :: UpdateVocabularyResponse -> TestTree
responseUpdateVocabulary = res
    "UpdateVocabularyResponse"
    "fixture/UpdateVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy UpdateVocabulary)

responseListTranscriptionJobs :: ListTranscriptionJobsResponse -> TestTree
responseListTranscriptionJobs = res
    "ListTranscriptionJobsResponse"
    "fixture/ListTranscriptionJobsResponse.proto"
    transcribe
    (Proxy :: Proxy ListTranscriptionJobs)

responseListVocabularies :: ListVocabulariesResponse -> TestTree
responseListVocabularies = res
    "ListVocabulariesResponse"
    "fixture/ListVocabulariesResponse.proto"
    transcribe
    (Proxy :: Proxy ListVocabularies)

responseCreateVocabulary :: CreateVocabularyResponse -> TestTree
responseCreateVocabulary = res
    "CreateVocabularyResponse"
    "fixture/CreateVocabularyResponse.proto"
    transcribe
    (Proxy :: Proxy CreateVocabulary)

responseStartTranscriptionJob :: StartTranscriptionJobResponse -> TestTree
responseStartTranscriptionJob = res
    "StartTranscriptionJobResponse"
    "fixture/StartTranscriptionJobResponse.proto"
    transcribe
    (Proxy :: Proxy StartTranscriptionJob)

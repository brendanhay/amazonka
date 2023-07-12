{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Polly
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Polly where

import Amazonka.Polly
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Polly.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteLexicon $
--             newDeleteLexicon
--
--         , requestDescribeVoices $
--             newDescribeVoices
--
--         , requestGetLexicon $
--             newGetLexicon
--
--         , requestGetSpeechSynthesisTask $
--             newGetSpeechSynthesisTask
--
--         , requestListLexicons $
--             newListLexicons
--
--         , requestListSpeechSynthesisTasks $
--             newListSpeechSynthesisTasks
--
--         , requestPutLexicon $
--             newPutLexicon
--
--         , requestStartSpeechSynthesisTask $
--             newStartSpeechSynthesisTask
--
--         , requestSynthesizeSpeech $
--             newSynthesizeSpeech
--
--           ]

--     , testGroup "response"
--         [ responseDeleteLexicon $
--             newDeleteLexiconResponse
--
--         , responseDescribeVoices $
--             newDescribeVoicesResponse
--
--         , responseGetLexicon $
--             newGetLexiconResponse
--
--         , responseGetSpeechSynthesisTask $
--             newGetSpeechSynthesisTaskResponse
--
--         , responseListLexicons $
--             newListLexiconsResponse
--
--         , responseListSpeechSynthesisTasks $
--             newListSpeechSynthesisTasksResponse
--
--         , responsePutLexicon $
--             newPutLexiconResponse
--
--         , responseStartSpeechSynthesisTask $
--             newStartSpeechSynthesisTaskResponse
--
--         , responseSynthesizeSpeech $
--             newSynthesizeSpeechResponse
--
--           ]
--     ]

-- Requests

requestDeleteLexicon :: DeleteLexicon -> TestTree
requestDeleteLexicon =
  req
    "DeleteLexicon"
    "fixture/DeleteLexicon.yaml"

requestDescribeVoices :: DescribeVoices -> TestTree
requestDescribeVoices =
  req
    "DescribeVoices"
    "fixture/DescribeVoices.yaml"

requestGetLexicon :: GetLexicon -> TestTree
requestGetLexicon =
  req
    "GetLexicon"
    "fixture/GetLexicon.yaml"

requestGetSpeechSynthesisTask :: GetSpeechSynthesisTask -> TestTree
requestGetSpeechSynthesisTask =
  req
    "GetSpeechSynthesisTask"
    "fixture/GetSpeechSynthesisTask.yaml"

requestListLexicons :: ListLexicons -> TestTree
requestListLexicons =
  req
    "ListLexicons"
    "fixture/ListLexicons.yaml"

requestListSpeechSynthesisTasks :: ListSpeechSynthesisTasks -> TestTree
requestListSpeechSynthesisTasks =
  req
    "ListSpeechSynthesisTasks"
    "fixture/ListSpeechSynthesisTasks.yaml"

requestPutLexicon :: PutLexicon -> TestTree
requestPutLexicon =
  req
    "PutLexicon"
    "fixture/PutLexicon.yaml"

requestStartSpeechSynthesisTask :: StartSpeechSynthesisTask -> TestTree
requestStartSpeechSynthesisTask =
  req
    "StartSpeechSynthesisTask"
    "fixture/StartSpeechSynthesisTask.yaml"

requestSynthesizeSpeech :: SynthesizeSpeech -> TestTree
requestSynthesizeSpeech =
  req
    "SynthesizeSpeech"
    "fixture/SynthesizeSpeech.yaml"

-- Responses

responseDeleteLexicon :: DeleteLexiconResponse -> TestTree
responseDeleteLexicon =
  res
    "DeleteLexiconResponse"
    "fixture/DeleteLexiconResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLexicon)

responseDescribeVoices :: DescribeVoicesResponse -> TestTree
responseDescribeVoices =
  res
    "DescribeVoicesResponse"
    "fixture/DescribeVoicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVoices)

responseGetLexicon :: GetLexiconResponse -> TestTree
responseGetLexicon =
  res
    "GetLexiconResponse"
    "fixture/GetLexiconResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLexicon)

responseGetSpeechSynthesisTask :: GetSpeechSynthesisTaskResponse -> TestTree
responseGetSpeechSynthesisTask =
  res
    "GetSpeechSynthesisTaskResponse"
    "fixture/GetSpeechSynthesisTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSpeechSynthesisTask)

responseListLexicons :: ListLexiconsResponse -> TestTree
responseListLexicons =
  res
    "ListLexiconsResponse"
    "fixture/ListLexiconsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLexicons)

responseListSpeechSynthesisTasks :: ListSpeechSynthesisTasksResponse -> TestTree
responseListSpeechSynthesisTasks =
  res
    "ListSpeechSynthesisTasksResponse"
    "fixture/ListSpeechSynthesisTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSpeechSynthesisTasks)

responsePutLexicon :: PutLexiconResponse -> TestTree
responsePutLexicon =
  res
    "PutLexiconResponse"
    "fixture/PutLexiconResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLexicon)

responseStartSpeechSynthesisTask :: StartSpeechSynthesisTaskResponse -> TestTree
responseStartSpeechSynthesisTask =
  res
    "StartSpeechSynthesisTaskResponse"
    "fixture/StartSpeechSynthesisTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSpeechSynthesisTask)

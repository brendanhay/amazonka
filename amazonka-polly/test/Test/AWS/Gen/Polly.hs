{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Polly
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Polly where

import Data.Proxy
import Network.AWS.Polly
import Test.AWS.Fixture
import Test.AWS.Polly.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetSpeechSynthesisTask $
--             newGetSpeechSynthesisTask
--
--         , requestGetLexicon $
--             newGetLexicon
--
--         , requestListLexicons $
--             newListLexicons
--
--         , requestDeleteLexicon $
--             newDeleteLexicon
--
--         , requestDescribeVoices $
--             newDescribeVoices
--
--         , requestStartSpeechSynthesisTask $
--             newStartSpeechSynthesisTask
--
--         , requestPutLexicon $
--             newPutLexicon
--
--         , requestSynthesizeSpeech $
--             newSynthesizeSpeech
--
--         , requestListSpeechSynthesisTasks $
--             newListSpeechSynthesisTasks
--
--           ]

--     , testGroup "response"
--         [ responseGetSpeechSynthesisTask $
--             newGetSpeechSynthesisTaskResponse
--
--         , responseGetLexicon $
--             newGetLexiconResponse
--
--         , responseListLexicons $
--             newListLexiconsResponse
--
--         , responseDeleteLexicon $
--             newDeleteLexiconResponse
--
--         , responseDescribeVoices $
--             newDescribeVoicesResponse
--
--         , responseStartSpeechSynthesisTask $
--             newStartSpeechSynthesisTaskResponse
--
--         , responsePutLexicon $
--             newPutLexiconResponse
--
--         , responseSynthesizeSpeech $
--             newSynthesizeSpeechResponse
--
--         , responseListSpeechSynthesisTasks $
--             newListSpeechSynthesisTasksResponse
--
--           ]
--     ]

-- Requests

requestGetSpeechSynthesisTask :: GetSpeechSynthesisTask -> TestTree
requestGetSpeechSynthesisTask =
  req
    "GetSpeechSynthesisTask"
    "fixture/GetSpeechSynthesisTask.yaml"

requestGetLexicon :: GetLexicon -> TestTree
requestGetLexicon =
  req
    "GetLexicon"
    "fixture/GetLexicon.yaml"

requestListLexicons :: ListLexicons -> TestTree
requestListLexicons =
  req
    "ListLexicons"
    "fixture/ListLexicons.yaml"

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

requestStartSpeechSynthesisTask :: StartSpeechSynthesisTask -> TestTree
requestStartSpeechSynthesisTask =
  req
    "StartSpeechSynthesisTask"
    "fixture/StartSpeechSynthesisTask.yaml"

requestPutLexicon :: PutLexicon -> TestTree
requestPutLexicon =
  req
    "PutLexicon"
    "fixture/PutLexicon.yaml"

requestSynthesizeSpeech :: SynthesizeSpeech -> TestTree
requestSynthesizeSpeech =
  req
    "SynthesizeSpeech"
    "fixture/SynthesizeSpeech.yaml"

requestListSpeechSynthesisTasks :: ListSpeechSynthesisTasks -> TestTree
requestListSpeechSynthesisTasks =
  req
    "ListSpeechSynthesisTasks"
    "fixture/ListSpeechSynthesisTasks.yaml"

-- Responses

responseGetSpeechSynthesisTask :: GetSpeechSynthesisTaskResponse -> TestTree
responseGetSpeechSynthesisTask =
  res
    "GetSpeechSynthesisTaskResponse"
    "fixture/GetSpeechSynthesisTaskResponse.proto"
    defaultService
    (Proxy :: Proxy GetSpeechSynthesisTask)

responseGetLexicon :: GetLexiconResponse -> TestTree
responseGetLexicon =
  res
    "GetLexiconResponse"
    "fixture/GetLexiconResponse.proto"
    defaultService
    (Proxy :: Proxy GetLexicon)

responseListLexicons :: ListLexiconsResponse -> TestTree
responseListLexicons =
  res
    "ListLexiconsResponse"
    "fixture/ListLexiconsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLexicons)

responseDeleteLexicon :: DeleteLexiconResponse -> TestTree
responseDeleteLexicon =
  res
    "DeleteLexiconResponse"
    "fixture/DeleteLexiconResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLexicon)

responseDescribeVoices :: DescribeVoicesResponse -> TestTree
responseDescribeVoices =
  res
    "DescribeVoicesResponse"
    "fixture/DescribeVoicesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVoices)

responseStartSpeechSynthesisTask :: StartSpeechSynthesisTaskResponse -> TestTree
responseStartSpeechSynthesisTask =
  res
    "StartSpeechSynthesisTaskResponse"
    "fixture/StartSpeechSynthesisTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartSpeechSynthesisTask)

responsePutLexicon :: PutLexiconResponse -> TestTree
responsePutLexicon =
  res
    "PutLexiconResponse"
    "fixture/PutLexiconResponse.proto"
    defaultService
    (Proxy :: Proxy PutLexicon)

responseListSpeechSynthesisTasks :: ListSpeechSynthesisTasksResponse -> TestTree
responseListSpeechSynthesisTasks =
  res
    "ListSpeechSynthesisTasksResponse"
    "fixture/ListSpeechSynthesisTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListSpeechSynthesisTasks)

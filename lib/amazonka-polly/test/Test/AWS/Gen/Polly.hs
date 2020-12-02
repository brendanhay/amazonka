{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Polly
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestGetLexicon $
--             getLexicon
--
--         , requestGetSpeechSynthesisTask $
--             getSpeechSynthesisTask
--
--         , requestDescribeVoices $
--             describeVoices
--
--         , requestListLexicons $
--             listLexicons
--
--         , requestSynthesizeSpeech $
--             synthesizeSpeech
--
--         , requestListSpeechSynthesisTasks $
--             listSpeechSynthesisTasks
--
--         , requestPutLexicon $
--             putLexicon
--
--         , requestDeleteLexicon $
--             deleteLexicon
--
--         , requestStartSpeechSynthesisTask $
--             startSpeechSynthesisTask
--
--           ]

--     , testGroup "response"
--         [ responseGetLexicon $
--             getLexiconResponse
--
--         , responseGetSpeechSynthesisTask $
--             getSpeechSynthesisTaskResponse
--
--         , responseDescribeVoices $
--             describeVoicesResponse
--
--         , responseListLexicons $
--             listLexiconsResponse
--
--         , responseSynthesizeSpeech $
--             synthesizeSpeechResponse
--
--         , responseListSpeechSynthesisTasks $
--             listSpeechSynthesisTasksResponse
--
--         , responsePutLexicon $
--             putLexiconResponse
--
--         , responseDeleteLexicon $
--             deleteLexiconResponse
--
--         , responseStartSpeechSynthesisTask $
--             startSpeechSynthesisTaskResponse
--
--           ]
--     ]

-- Requests

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

requestDescribeVoices :: DescribeVoices -> TestTree
requestDescribeVoices =
  req
    "DescribeVoices"
    "fixture/DescribeVoices.yaml"

requestListLexicons :: ListLexicons -> TestTree
requestListLexicons =
  req
    "ListLexicons"
    "fixture/ListLexicons.yaml"

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

requestPutLexicon :: PutLexicon -> TestTree
requestPutLexicon =
  req
    "PutLexicon"
    "fixture/PutLexicon.yaml"

requestDeleteLexicon :: DeleteLexicon -> TestTree
requestDeleteLexicon =
  req
    "DeleteLexicon"
    "fixture/DeleteLexicon.yaml"

requestStartSpeechSynthesisTask :: StartSpeechSynthesisTask -> TestTree
requestStartSpeechSynthesisTask =
  req
    "StartSpeechSynthesisTask"
    "fixture/StartSpeechSynthesisTask.yaml"

-- Responses

responseGetLexicon :: GetLexiconResponse -> TestTree
responseGetLexicon =
  res
    "GetLexiconResponse"
    "fixture/GetLexiconResponse.proto"
    polly
    (Proxy :: Proxy GetLexicon)

responseGetSpeechSynthesisTask :: GetSpeechSynthesisTaskResponse -> TestTree
responseGetSpeechSynthesisTask =
  res
    "GetSpeechSynthesisTaskResponse"
    "fixture/GetSpeechSynthesisTaskResponse.proto"
    polly
    (Proxy :: Proxy GetSpeechSynthesisTask)

responseDescribeVoices :: DescribeVoicesResponse -> TestTree
responseDescribeVoices =
  res
    "DescribeVoicesResponse"
    "fixture/DescribeVoicesResponse.proto"
    polly
    (Proxy :: Proxy DescribeVoices)

responseListLexicons :: ListLexiconsResponse -> TestTree
responseListLexicons =
  res
    "ListLexiconsResponse"
    "fixture/ListLexiconsResponse.proto"
    polly
    (Proxy :: Proxy ListLexicons)

responseListSpeechSynthesisTasks :: ListSpeechSynthesisTasksResponse -> TestTree
responseListSpeechSynthesisTasks =
  res
    "ListSpeechSynthesisTasksResponse"
    "fixture/ListSpeechSynthesisTasksResponse.proto"
    polly
    (Proxy :: Proxy ListSpeechSynthesisTasks)

responsePutLexicon :: PutLexiconResponse -> TestTree
responsePutLexicon =
  res
    "PutLexiconResponse"
    "fixture/PutLexiconResponse.proto"
    polly
    (Proxy :: Proxy PutLexicon)

responseDeleteLexicon :: DeleteLexiconResponse -> TestTree
responseDeleteLexicon =
  res
    "DeleteLexiconResponse"
    "fixture/DeleteLexiconResponse.proto"
    polly
    (Proxy :: Proxy DeleteLexicon)

responseStartSpeechSynthesisTask :: StartSpeechSynthesisTaskResponse -> TestTree
responseStartSpeechSynthesisTask =
  res
    "StartSpeechSynthesisTaskResponse"
    "fixture/StartSpeechSynthesisTaskResponse.proto"
    polly
    (Proxy :: Proxy StartSpeechSynthesisTask)

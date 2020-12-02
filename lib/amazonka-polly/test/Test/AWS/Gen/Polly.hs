{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Polly
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         , requestDescribeVoices $
--             describeVoices
--
--         , requestListLexicons $
--             listLexicons
--
--         , requestSynthesizeSpeech $
--             synthesizeSpeech
--
--         , requestPutLexicon $
--             putLexicon
--
--         , requestDeleteLexicon $
--             deleteLexicon
--
--           ]

--     , testGroup "response"
--         [ responseGetLexicon $
--             getLexiconResponse
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
--         , responsePutLexicon $
--             putLexiconResponse
--
--         , responseDeleteLexicon $
--             deleteLexiconResponse
--
--           ]
--     ]

-- Requests

requestGetLexicon :: GetLexicon -> TestTree
requestGetLexicon = req
    "GetLexicon"
    "fixture/GetLexicon.yaml"

requestDescribeVoices :: DescribeVoices -> TestTree
requestDescribeVoices = req
    "DescribeVoices"
    "fixture/DescribeVoices.yaml"

requestListLexicons :: ListLexicons -> TestTree
requestListLexicons = req
    "ListLexicons"
    "fixture/ListLexicons.yaml"

requestSynthesizeSpeech :: SynthesizeSpeech -> TestTree
requestSynthesizeSpeech = req
    "SynthesizeSpeech"
    "fixture/SynthesizeSpeech.yaml"

requestPutLexicon :: PutLexicon -> TestTree
requestPutLexicon = req
    "PutLexicon"
    "fixture/PutLexicon.yaml"

requestDeleteLexicon :: DeleteLexicon -> TestTree
requestDeleteLexicon = req
    "DeleteLexicon"
    "fixture/DeleteLexicon.yaml"

-- Responses

responseGetLexicon :: GetLexiconResponse -> TestTree
responseGetLexicon = res
    "GetLexiconResponse"
    "fixture/GetLexiconResponse.proto"
    polly
    (Proxy :: Proxy GetLexicon)

responseDescribeVoices :: DescribeVoicesResponse -> TestTree
responseDescribeVoices = res
    "DescribeVoicesResponse"
    "fixture/DescribeVoicesResponse.proto"
    polly
    (Proxy :: Proxy DescribeVoices)

responseListLexicons :: ListLexiconsResponse -> TestTree
responseListLexicons = res
    "ListLexiconsResponse"
    "fixture/ListLexiconsResponse.proto"
    polly
    (Proxy :: Proxy ListLexicons)

responsePutLexicon :: PutLexiconResponse -> TestTree
responsePutLexicon = res
    "PutLexiconResponse"
    "fixture/PutLexiconResponse.proto"
    polly
    (Proxy :: Proxy PutLexicon)

responseDeleteLexicon :: DeleteLexiconResponse -> TestTree
responseDeleteLexicon = res
    "DeleteLexiconResponse"
    "fixture/DeleteLexiconResponse.proto"
    polly
    (Proxy :: Proxy DeleteLexicon)

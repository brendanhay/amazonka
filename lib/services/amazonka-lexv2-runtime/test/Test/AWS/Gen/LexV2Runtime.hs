{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.LexV2Runtime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.LexV2Runtime where

import Data.Proxy
import Network.AWS.LexV2Runtime
import Test.AWS.Fixture
import Test.AWS.LexV2Runtime.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutSession $
--             newPutSession
--
--         , requestRecognizeUtterance $
--             newRecognizeUtterance
--
--         , requestDeleteSession $
--             newDeleteSession
--
--         , requestStartConversation $
--             newStartConversation
--
--         , requestGetSession $
--             newGetSession
--
--         , requestRecognizeText $
--             newRecognizeText
--
--           ]

--     , testGroup "response"
--         [ responsePutSession $
--             newPutSessionResponse
--
--         , responseRecognizeUtterance $
--             newRecognizeUtteranceResponse
--
--         , responseDeleteSession $
--             newDeleteSessionResponse
--
--         , responseStartConversation $
--             newStartConversationResponse
--
--         , responseGetSession $
--             newGetSessionResponse
--
--         , responseRecognizeText $
--             newRecognizeTextResponse
--
--           ]
--     ]

-- Requests

requestPutSession :: PutSession -> TestTree
requestPutSession =
  req
    "PutSession"
    "fixture/PutSession.yaml"

requestDeleteSession :: DeleteSession -> TestTree
requestDeleteSession =
  req
    "DeleteSession"
    "fixture/DeleteSession.yaml"

requestStartConversation :: StartConversation -> TestTree
requestStartConversation =
  req
    "StartConversation"
    "fixture/StartConversation.yaml"

requestGetSession :: GetSession -> TestTree
requestGetSession =
  req
    "GetSession"
    "fixture/GetSession.yaml"

requestRecognizeText :: RecognizeText -> TestTree
requestRecognizeText =
  req
    "RecognizeText"
    "fixture/RecognizeText.yaml"

-- Responses

responseDeleteSession :: DeleteSessionResponse -> TestTree
responseDeleteSession =
  res
    "DeleteSessionResponse"
    "fixture/DeleteSessionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSession)

responseStartConversation :: StartConversationResponse -> TestTree
responseStartConversation =
  res
    "StartConversationResponse"
    "fixture/StartConversationResponse.proto"
    defaultService
    (Proxy :: Proxy StartConversation)

responseGetSession :: GetSessionResponse -> TestTree
responseGetSession =
  res
    "GetSessionResponse"
    "fixture/GetSessionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSession)

responseRecognizeText :: RecognizeTextResponse -> TestTree
responseRecognizeText =
  res
    "RecognizeTextResponse"
    "fixture/RecognizeTextResponse.proto"
    defaultService
    (Proxy :: Proxy RecognizeText)

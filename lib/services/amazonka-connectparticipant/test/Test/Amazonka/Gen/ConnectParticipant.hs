{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ConnectParticipant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ConnectParticipant where

import Amazonka.ConnectParticipant
import qualified Data.Proxy as Proxy
import Test.Amazonka.ConnectParticipant.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCompleteAttachmentUpload $
--             newCompleteAttachmentUpload
--
--         , requestCreateParticipantConnection $
--             newCreateParticipantConnection
--
--         , requestDisconnectParticipant $
--             newDisconnectParticipant
--
--         , requestGetAttachment $
--             newGetAttachment
--
--         , requestGetTranscript $
--             newGetTranscript
--
--         , requestSendEvent $
--             newSendEvent
--
--         , requestSendMessage $
--             newSendMessage
--
--         , requestStartAttachmentUpload $
--             newStartAttachmentUpload
--
--           ]

--     , testGroup "response"
--         [ responseCompleteAttachmentUpload $
--             newCompleteAttachmentUploadResponse
--
--         , responseCreateParticipantConnection $
--             newCreateParticipantConnectionResponse
--
--         , responseDisconnectParticipant $
--             newDisconnectParticipantResponse
--
--         , responseGetAttachment $
--             newGetAttachmentResponse
--
--         , responseGetTranscript $
--             newGetTranscriptResponse
--
--         , responseSendEvent $
--             newSendEventResponse
--
--         , responseSendMessage $
--             newSendMessageResponse
--
--         , responseStartAttachmentUpload $
--             newStartAttachmentUploadResponse
--
--           ]
--     ]

-- Requests

requestCompleteAttachmentUpload :: CompleteAttachmentUpload -> TestTree
requestCompleteAttachmentUpload =
  req
    "CompleteAttachmentUpload"
    "fixture/CompleteAttachmentUpload.yaml"

requestCreateParticipantConnection :: CreateParticipantConnection -> TestTree
requestCreateParticipantConnection =
  req
    "CreateParticipantConnection"
    "fixture/CreateParticipantConnection.yaml"

requestDisconnectParticipant :: DisconnectParticipant -> TestTree
requestDisconnectParticipant =
  req
    "DisconnectParticipant"
    "fixture/DisconnectParticipant.yaml"

requestGetAttachment :: GetAttachment -> TestTree
requestGetAttachment =
  req
    "GetAttachment"
    "fixture/GetAttachment.yaml"

requestGetTranscript :: GetTranscript -> TestTree
requestGetTranscript =
  req
    "GetTranscript"
    "fixture/GetTranscript.yaml"

requestSendEvent :: SendEvent -> TestTree
requestSendEvent =
  req
    "SendEvent"
    "fixture/SendEvent.yaml"

requestSendMessage :: SendMessage -> TestTree
requestSendMessage =
  req
    "SendMessage"
    "fixture/SendMessage.yaml"

requestStartAttachmentUpload :: StartAttachmentUpload -> TestTree
requestStartAttachmentUpload =
  req
    "StartAttachmentUpload"
    "fixture/StartAttachmentUpload.yaml"

-- Responses

responseCompleteAttachmentUpload :: CompleteAttachmentUploadResponse -> TestTree
responseCompleteAttachmentUpload =
  res
    "CompleteAttachmentUploadResponse"
    "fixture/CompleteAttachmentUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteAttachmentUpload)

responseCreateParticipantConnection :: CreateParticipantConnectionResponse -> TestTree
responseCreateParticipantConnection =
  res
    "CreateParticipantConnectionResponse"
    "fixture/CreateParticipantConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateParticipantConnection)

responseDisconnectParticipant :: DisconnectParticipantResponse -> TestTree
responseDisconnectParticipant =
  res
    "DisconnectParticipantResponse"
    "fixture/DisconnectParticipantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisconnectParticipant)

responseGetAttachment :: GetAttachmentResponse -> TestTree
responseGetAttachment =
  res
    "GetAttachmentResponse"
    "fixture/GetAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAttachment)

responseGetTranscript :: GetTranscriptResponse -> TestTree
responseGetTranscript =
  res
    "GetTranscriptResponse"
    "fixture/GetTranscriptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTranscript)

responseSendEvent :: SendEventResponse -> TestTree
responseSendEvent =
  res
    "SendEventResponse"
    "fixture/SendEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendEvent)

responseSendMessage :: SendMessageResponse -> TestTree
responseSendMessage =
  res
    "SendMessageResponse"
    "fixture/SendMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendMessage)

responseStartAttachmentUpload :: StartAttachmentUploadResponse -> TestTree
responseStartAttachmentUpload =
  res
    "StartAttachmentUploadResponse"
    "fixture/StartAttachmentUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAttachmentUpload)

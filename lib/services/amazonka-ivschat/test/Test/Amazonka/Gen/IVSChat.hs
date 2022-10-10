{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IVSChat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IVSChat where

import Amazonka.IVSChat
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IVSChat.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateChatToken $
--             newCreateChatToken
--
--         , requestCreateRoom $
--             newCreateRoom
--
--         , requestDeleteMessage $
--             newDeleteMessage
--
--         , requestDeleteRoom $
--             newDeleteRoom
--
--         , requestDisconnectUser $
--             newDisconnectUser
--
--         , requestGetRoom $
--             newGetRoom
--
--         , requestListRooms $
--             newListRooms
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSendEvent $
--             newSendEvent
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateRoom $
--             newUpdateRoom
--
--           ]

--     , testGroup "response"
--         [ responseCreateChatToken $
--             newCreateChatTokenResponse
--
--         , responseCreateRoom $
--             newCreateRoomResponse
--
--         , responseDeleteMessage $
--             newDeleteMessageResponse
--
--         , responseDeleteRoom $
--             newDeleteRoomResponse
--
--         , responseDisconnectUser $
--             newDisconnectUserResponse
--
--         , responseGetRoom $
--             newGetRoomResponse
--
--         , responseListRooms $
--             newListRoomsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSendEvent $
--             newSendEventResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateRoom $
--             newUpdateRoomResponse
--
--           ]
--     ]

-- Requests

requestCreateChatToken :: CreateChatToken -> TestTree
requestCreateChatToken =
  req
    "CreateChatToken"
    "fixture/CreateChatToken.yaml"

requestCreateRoom :: CreateRoom -> TestTree
requestCreateRoom =
  req
    "CreateRoom"
    "fixture/CreateRoom.yaml"

requestDeleteMessage :: DeleteMessage -> TestTree
requestDeleteMessage =
  req
    "DeleteMessage"
    "fixture/DeleteMessage.yaml"

requestDeleteRoom :: DeleteRoom -> TestTree
requestDeleteRoom =
  req
    "DeleteRoom"
    "fixture/DeleteRoom.yaml"

requestDisconnectUser :: DisconnectUser -> TestTree
requestDisconnectUser =
  req
    "DisconnectUser"
    "fixture/DisconnectUser.yaml"

requestGetRoom :: GetRoom -> TestTree
requestGetRoom =
  req
    "GetRoom"
    "fixture/GetRoom.yaml"

requestListRooms :: ListRooms -> TestTree
requestListRooms =
  req
    "ListRooms"
    "fixture/ListRooms.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSendEvent :: SendEvent -> TestTree
requestSendEvent =
  req
    "SendEvent"
    "fixture/SendEvent.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateRoom :: UpdateRoom -> TestTree
requestUpdateRoom =
  req
    "UpdateRoom"
    "fixture/UpdateRoom.yaml"

-- Responses

responseCreateChatToken :: CreateChatTokenResponse -> TestTree
responseCreateChatToken =
  res
    "CreateChatTokenResponse"
    "fixture/CreateChatTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChatToken)

responseCreateRoom :: CreateRoomResponse -> TestTree
responseCreateRoom =
  res
    "CreateRoomResponse"
    "fixture/CreateRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoom)

responseDeleteMessage :: DeleteMessageResponse -> TestTree
responseDeleteMessage =
  res
    "DeleteMessageResponse"
    "fixture/DeleteMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMessage)

responseDeleteRoom :: DeleteRoomResponse -> TestTree
responseDeleteRoom =
  res
    "DeleteRoomResponse"
    "fixture/DeleteRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoom)

responseDisconnectUser :: DisconnectUserResponse -> TestTree
responseDisconnectUser =
  res
    "DisconnectUserResponse"
    "fixture/DisconnectUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisconnectUser)

responseGetRoom :: GetRoomResponse -> TestTree
responseGetRoom =
  res
    "GetRoomResponse"
    "fixture/GetRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoom)

responseListRooms :: ListRoomsResponse -> TestTree
responseListRooms =
  res
    "ListRoomsResponse"
    "fixture/ListRoomsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRooms)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseSendEvent :: SendEventResponse -> TestTree
responseSendEvent =
  res
    "SendEventResponse"
    "fixture/SendEventResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendEvent)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateRoom :: UpdateRoomResponse -> TestTree
responseUpdateRoom =
  res
    "UpdateRoomResponse"
    "fixture/UpdateRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoom)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IVSChat
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         , requestCreateLoggingConfiguration $
--             newCreateLoggingConfiguration
--
--         , requestCreateRoom $
--             newCreateRoom
--
--         , requestDeleteLoggingConfiguration $
--             newDeleteLoggingConfiguration
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
--         , requestGetLoggingConfiguration $
--             newGetLoggingConfiguration
--
--         , requestGetRoom $
--             newGetRoom
--
--         , requestListLoggingConfigurations $
--             newListLoggingConfigurations
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
--         , requestUpdateLoggingConfiguration $
--             newUpdateLoggingConfiguration
--
--         , requestUpdateRoom $
--             newUpdateRoom
--
--           ]

--     , testGroup "response"
--         [ responseCreateChatToken $
--             newCreateChatTokenResponse
--
--         , responseCreateLoggingConfiguration $
--             newCreateLoggingConfigurationResponse
--
--         , responseCreateRoom $
--             newCreateRoomResponse
--
--         , responseDeleteLoggingConfiguration $
--             newDeleteLoggingConfigurationResponse
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
--         , responseGetLoggingConfiguration $
--             newGetLoggingConfigurationResponse
--
--         , responseGetRoom $
--             newGetRoomResponse
--
--         , responseListLoggingConfigurations $
--             newListLoggingConfigurationsResponse
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
--         , responseUpdateLoggingConfiguration $
--             newUpdateLoggingConfigurationResponse
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

requestCreateLoggingConfiguration :: CreateLoggingConfiguration -> TestTree
requestCreateLoggingConfiguration =
  req
    "CreateLoggingConfiguration"
    "fixture/CreateLoggingConfiguration.yaml"

requestCreateRoom :: CreateRoom -> TestTree
requestCreateRoom =
  req
    "CreateRoom"
    "fixture/CreateRoom.yaml"

requestDeleteLoggingConfiguration :: DeleteLoggingConfiguration -> TestTree
requestDeleteLoggingConfiguration =
  req
    "DeleteLoggingConfiguration"
    "fixture/DeleteLoggingConfiguration.yaml"

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

requestGetLoggingConfiguration :: GetLoggingConfiguration -> TestTree
requestGetLoggingConfiguration =
  req
    "GetLoggingConfiguration"
    "fixture/GetLoggingConfiguration.yaml"

requestGetRoom :: GetRoom -> TestTree
requestGetRoom =
  req
    "GetRoom"
    "fixture/GetRoom.yaml"

requestListLoggingConfigurations :: ListLoggingConfigurations -> TestTree
requestListLoggingConfigurations =
  req
    "ListLoggingConfigurations"
    "fixture/ListLoggingConfigurations.yaml"

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

requestUpdateLoggingConfiguration :: UpdateLoggingConfiguration -> TestTree
requestUpdateLoggingConfiguration =
  req
    "UpdateLoggingConfiguration"
    "fixture/UpdateLoggingConfiguration.yaml"

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

responseCreateLoggingConfiguration :: CreateLoggingConfigurationResponse -> TestTree
responseCreateLoggingConfiguration =
  res
    "CreateLoggingConfigurationResponse"
    "fixture/CreateLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoggingConfiguration)

responseCreateRoom :: CreateRoomResponse -> TestTree
responseCreateRoom =
  res
    "CreateRoomResponse"
    "fixture/CreateRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoom)

responseDeleteLoggingConfiguration :: DeleteLoggingConfigurationResponse -> TestTree
responseDeleteLoggingConfiguration =
  res
    "DeleteLoggingConfigurationResponse"
    "fixture/DeleteLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoggingConfiguration)

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

responseGetLoggingConfiguration :: GetLoggingConfigurationResponse -> TestTree
responseGetLoggingConfiguration =
  res
    "GetLoggingConfigurationResponse"
    "fixture/GetLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoggingConfiguration)

responseGetRoom :: GetRoomResponse -> TestTree
responseGetRoom =
  res
    "GetRoomResponse"
    "fixture/GetRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoom)

responseListLoggingConfigurations :: ListLoggingConfigurationsResponse -> TestTree
responseListLoggingConfigurations =
  res
    "ListLoggingConfigurationsResponse"
    "fixture/ListLoggingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLoggingConfigurations)

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

responseUpdateLoggingConfiguration :: UpdateLoggingConfigurationResponse -> TestTree
responseUpdateLoggingConfiguration =
  res
    "UpdateLoggingConfigurationResponse"
    "fixture/UpdateLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLoggingConfiguration)

responseUpdateRoom :: UpdateRoomResponse -> TestTree
responseUpdateRoom =
  res
    "UpdateRoomResponse"
    "fixture/UpdateRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoom)

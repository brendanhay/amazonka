{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ChimeSDKMessaging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ChimeSDKMessaging where

import qualified Data.Proxy as Proxy
import Network.AWS.ChimeSDKMessaging
import Test.AWS.ChimeSDKMessaging.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeChannelMembership $
--             newDescribeChannelMembership
--
--         , requestDescribeChannelFlow $
--             newDescribeChannelFlow
--
--         , requestListChannels $
--             newListChannels
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestUpdateChannel $
--             newUpdateChannel
--
--         , requestAssociateChannelFlow $
--             newAssociateChannelFlow
--
--         , requestGetMessagingSessionEndpoint $
--             newGetMessagingSessionEndpoint
--
--         , requestListChannelsModeratedByAppInstanceUser $
--             newListChannelsModeratedByAppInstanceUser
--
--         , requestRedactChannelMessage $
--             newRedactChannelMessage
--
--         , requestListChannelFlows $
--             newListChannelFlows
--
--         , requestDeleteChannelFlow $
--             newDeleteChannelFlow
--
--         , requestUpdateChannelFlow $
--             newUpdateChannelFlow
--
--         , requestDeleteChannelMembership $
--             newDeleteChannelMembership
--
--         , requestListChannelMemberships $
--             newListChannelMemberships
--
--         , requestDisassociateChannelFlow $
--             newDisassociateChannelFlow
--
--         , requestGetChannelMessage $
--             newGetChannelMessage
--
--         , requestDescribeChannelMembershipForAppInstanceUser $
--             newDescribeChannelMembershipForAppInstanceUser
--
--         , requestCreateChannelModerator $
--             newCreateChannelModerator
--
--         , requestDescribeChannelModeratedByAppInstanceUser $
--             newDescribeChannelModeratedByAppInstanceUser
--
--         , requestSendChannelMessage $
--             newSendChannelMessage
--
--         , requestDeleteChannelBan $
--             newDeleteChannelBan
--
--         , requestListChannelBans $
--             newListChannelBans
--
--         , requestCreateChannel $
--             newCreateChannel
--
--         , requestDescribeChannelModerator $
--             newDescribeChannelModerator
--
--         , requestCreateChannelBan $
--             newCreateChannelBan
--
--         , requestListChannelMembershipsForAppInstanceUser $
--             newListChannelMembershipsForAppInstanceUser
--
--         , requestUpdateChannelReadMarker $
--             newUpdateChannelReadMarker
--
--         , requestGetChannelMessageStatus $
--             newGetChannelMessageStatus
--
--         , requestCreateChannelFlow $
--             newCreateChannelFlow
--
--         , requestCreateChannelMembership $
--             newCreateChannelMembership
--
--         , requestTagResource $
--             newTagResource
--
--         , requestChannelFlowCallback $
--             newChannelFlowCallback
--
--         , requestDeleteChannelModerator $
--             newDeleteChannelModerator
--
--         , requestDescribeChannelBan $
--             newDescribeChannelBan
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListChannelModerators $
--             newListChannelModerators
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestDeleteChannelMessage $
--             newDeleteChannelMessage
--
--         , requestUpdateChannelMessage $
--             newUpdateChannelMessage
--
--         , requestListChannelMessages $
--             newListChannelMessages
--
--         , requestListChannelsAssociatedWithChannelFlow $
--             newListChannelsAssociatedWithChannelFlow
--
--         , requestBatchCreateChannelMembership $
--             newBatchCreateChannelMembership
--
--           ]

--     , testGroup "response"
--         [ responseDescribeChannelMembership $
--             newDescribeChannelMembershipResponse
--
--         , responseDescribeChannelFlow $
--             newDescribeChannelFlowResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseUpdateChannel $
--             newUpdateChannelResponse
--
--         , responseAssociateChannelFlow $
--             newAssociateChannelFlowResponse
--
--         , responseGetMessagingSessionEndpoint $
--             newGetMessagingSessionEndpointResponse
--
--         , responseListChannelsModeratedByAppInstanceUser $
--             newListChannelsModeratedByAppInstanceUserResponse
--
--         , responseRedactChannelMessage $
--             newRedactChannelMessageResponse
--
--         , responseListChannelFlows $
--             newListChannelFlowsResponse
--
--         , responseDeleteChannelFlow $
--             newDeleteChannelFlowResponse
--
--         , responseUpdateChannelFlow $
--             newUpdateChannelFlowResponse
--
--         , responseDeleteChannelMembership $
--             newDeleteChannelMembershipResponse
--
--         , responseListChannelMemberships $
--             newListChannelMembershipsResponse
--
--         , responseDisassociateChannelFlow $
--             newDisassociateChannelFlowResponse
--
--         , responseGetChannelMessage $
--             newGetChannelMessageResponse
--
--         , responseDescribeChannelMembershipForAppInstanceUser $
--             newDescribeChannelMembershipForAppInstanceUserResponse
--
--         , responseCreateChannelModerator $
--             newCreateChannelModeratorResponse
--
--         , responseDescribeChannelModeratedByAppInstanceUser $
--             newDescribeChannelModeratedByAppInstanceUserResponse
--
--         , responseSendChannelMessage $
--             newSendChannelMessageResponse
--
--         , responseDeleteChannelBan $
--             newDeleteChannelBanResponse
--
--         , responseListChannelBans $
--             newListChannelBansResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseDescribeChannelModerator $
--             newDescribeChannelModeratorResponse
--
--         , responseCreateChannelBan $
--             newCreateChannelBanResponse
--
--         , responseListChannelMembershipsForAppInstanceUser $
--             newListChannelMembershipsForAppInstanceUserResponse
--
--         , responseUpdateChannelReadMarker $
--             newUpdateChannelReadMarkerResponse
--
--         , responseGetChannelMessageStatus $
--             newGetChannelMessageStatusResponse
--
--         , responseCreateChannelFlow $
--             newCreateChannelFlowResponse
--
--         , responseCreateChannelMembership $
--             newCreateChannelMembershipResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseChannelFlowCallback $
--             newChannelFlowCallbackResponse
--
--         , responseDeleteChannelModerator $
--             newDeleteChannelModeratorResponse
--
--         , responseDescribeChannelBan $
--             newDescribeChannelBanResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListChannelModerators $
--             newListChannelModeratorsResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseDeleteChannelMessage $
--             newDeleteChannelMessageResponse
--
--         , responseUpdateChannelMessage $
--             newUpdateChannelMessageResponse
--
--         , responseListChannelMessages $
--             newListChannelMessagesResponse
--
--         , responseListChannelsAssociatedWithChannelFlow $
--             newListChannelsAssociatedWithChannelFlowResponse
--
--         , responseBatchCreateChannelMembership $
--             newBatchCreateChannelMembershipResponse
--
--           ]
--     ]

-- Requests

requestDescribeChannelMembership :: DescribeChannelMembership -> TestTree
requestDescribeChannelMembership =
  req
    "DescribeChannelMembership"
    "fixture/DescribeChannelMembership.yaml"

requestDescribeChannelFlow :: DescribeChannelFlow -> TestTree
requestDescribeChannelFlow =
  req
    "DescribeChannelFlow"
    "fixture/DescribeChannelFlow.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestAssociateChannelFlow :: AssociateChannelFlow -> TestTree
requestAssociateChannelFlow =
  req
    "AssociateChannelFlow"
    "fixture/AssociateChannelFlow.yaml"

requestGetMessagingSessionEndpoint :: GetMessagingSessionEndpoint -> TestTree
requestGetMessagingSessionEndpoint =
  req
    "GetMessagingSessionEndpoint"
    "fixture/GetMessagingSessionEndpoint.yaml"

requestListChannelsModeratedByAppInstanceUser :: ListChannelsModeratedByAppInstanceUser -> TestTree
requestListChannelsModeratedByAppInstanceUser =
  req
    "ListChannelsModeratedByAppInstanceUser"
    "fixture/ListChannelsModeratedByAppInstanceUser.yaml"

requestRedactChannelMessage :: RedactChannelMessage -> TestTree
requestRedactChannelMessage =
  req
    "RedactChannelMessage"
    "fixture/RedactChannelMessage.yaml"

requestListChannelFlows :: ListChannelFlows -> TestTree
requestListChannelFlows =
  req
    "ListChannelFlows"
    "fixture/ListChannelFlows.yaml"

requestDeleteChannelFlow :: DeleteChannelFlow -> TestTree
requestDeleteChannelFlow =
  req
    "DeleteChannelFlow"
    "fixture/DeleteChannelFlow.yaml"

requestUpdateChannelFlow :: UpdateChannelFlow -> TestTree
requestUpdateChannelFlow =
  req
    "UpdateChannelFlow"
    "fixture/UpdateChannelFlow.yaml"

requestDeleteChannelMembership :: DeleteChannelMembership -> TestTree
requestDeleteChannelMembership =
  req
    "DeleteChannelMembership"
    "fixture/DeleteChannelMembership.yaml"

requestListChannelMemberships :: ListChannelMemberships -> TestTree
requestListChannelMemberships =
  req
    "ListChannelMemberships"
    "fixture/ListChannelMemberships.yaml"

requestDisassociateChannelFlow :: DisassociateChannelFlow -> TestTree
requestDisassociateChannelFlow =
  req
    "DisassociateChannelFlow"
    "fixture/DisassociateChannelFlow.yaml"

requestGetChannelMessage :: GetChannelMessage -> TestTree
requestGetChannelMessage =
  req
    "GetChannelMessage"
    "fixture/GetChannelMessage.yaml"

requestDescribeChannelMembershipForAppInstanceUser :: DescribeChannelMembershipForAppInstanceUser -> TestTree
requestDescribeChannelMembershipForAppInstanceUser =
  req
    "DescribeChannelMembershipForAppInstanceUser"
    "fixture/DescribeChannelMembershipForAppInstanceUser.yaml"

requestCreateChannelModerator :: CreateChannelModerator -> TestTree
requestCreateChannelModerator =
  req
    "CreateChannelModerator"
    "fixture/CreateChannelModerator.yaml"

requestDescribeChannelModeratedByAppInstanceUser :: DescribeChannelModeratedByAppInstanceUser -> TestTree
requestDescribeChannelModeratedByAppInstanceUser =
  req
    "DescribeChannelModeratedByAppInstanceUser"
    "fixture/DescribeChannelModeratedByAppInstanceUser.yaml"

requestSendChannelMessage :: SendChannelMessage -> TestTree
requestSendChannelMessage =
  req
    "SendChannelMessage"
    "fixture/SendChannelMessage.yaml"

requestDeleteChannelBan :: DeleteChannelBan -> TestTree
requestDeleteChannelBan =
  req
    "DeleteChannelBan"
    "fixture/DeleteChannelBan.yaml"

requestListChannelBans :: ListChannelBans -> TestTree
requestListChannelBans =
  req
    "ListChannelBans"
    "fixture/ListChannelBans.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestDescribeChannelModerator :: DescribeChannelModerator -> TestTree
requestDescribeChannelModerator =
  req
    "DescribeChannelModerator"
    "fixture/DescribeChannelModerator.yaml"

requestCreateChannelBan :: CreateChannelBan -> TestTree
requestCreateChannelBan =
  req
    "CreateChannelBan"
    "fixture/CreateChannelBan.yaml"

requestListChannelMembershipsForAppInstanceUser :: ListChannelMembershipsForAppInstanceUser -> TestTree
requestListChannelMembershipsForAppInstanceUser =
  req
    "ListChannelMembershipsForAppInstanceUser"
    "fixture/ListChannelMembershipsForAppInstanceUser.yaml"

requestUpdateChannelReadMarker :: UpdateChannelReadMarker -> TestTree
requestUpdateChannelReadMarker =
  req
    "UpdateChannelReadMarker"
    "fixture/UpdateChannelReadMarker.yaml"

requestGetChannelMessageStatus :: GetChannelMessageStatus -> TestTree
requestGetChannelMessageStatus =
  req
    "GetChannelMessageStatus"
    "fixture/GetChannelMessageStatus.yaml"

requestCreateChannelFlow :: CreateChannelFlow -> TestTree
requestCreateChannelFlow =
  req
    "CreateChannelFlow"
    "fixture/CreateChannelFlow.yaml"

requestCreateChannelMembership :: CreateChannelMembership -> TestTree
requestCreateChannelMembership =
  req
    "CreateChannelMembership"
    "fixture/CreateChannelMembership.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestChannelFlowCallback :: ChannelFlowCallback -> TestTree
requestChannelFlowCallback =
  req
    "ChannelFlowCallback"
    "fixture/ChannelFlowCallback.yaml"

requestDeleteChannelModerator :: DeleteChannelModerator -> TestTree
requestDeleteChannelModerator =
  req
    "DeleteChannelModerator"
    "fixture/DeleteChannelModerator.yaml"

requestDescribeChannelBan :: DescribeChannelBan -> TestTree
requestDescribeChannelBan =
  req
    "DescribeChannelBan"
    "fixture/DescribeChannelBan.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListChannelModerators :: ListChannelModerators -> TestTree
requestListChannelModerators =
  req
    "ListChannelModerators"
    "fixture/ListChannelModerators.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestDeleteChannelMessage :: DeleteChannelMessage -> TestTree
requestDeleteChannelMessage =
  req
    "DeleteChannelMessage"
    "fixture/DeleteChannelMessage.yaml"

requestUpdateChannelMessage :: UpdateChannelMessage -> TestTree
requestUpdateChannelMessage =
  req
    "UpdateChannelMessage"
    "fixture/UpdateChannelMessage.yaml"

requestListChannelMessages :: ListChannelMessages -> TestTree
requestListChannelMessages =
  req
    "ListChannelMessages"
    "fixture/ListChannelMessages.yaml"

requestListChannelsAssociatedWithChannelFlow :: ListChannelsAssociatedWithChannelFlow -> TestTree
requestListChannelsAssociatedWithChannelFlow =
  req
    "ListChannelsAssociatedWithChannelFlow"
    "fixture/ListChannelsAssociatedWithChannelFlow.yaml"

requestBatchCreateChannelMembership :: BatchCreateChannelMembership -> TestTree
requestBatchCreateChannelMembership =
  req
    "BatchCreateChannelMembership"
    "fixture/BatchCreateChannelMembership.yaml"

-- Responses

responseDescribeChannelMembership :: DescribeChannelMembershipResponse -> TestTree
responseDescribeChannelMembership =
  res
    "DescribeChannelMembershipResponse"
    "fixture/DescribeChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelMembership)

responseDescribeChannelFlow :: DescribeChannelFlowResponse -> TestTree
responseDescribeChannelFlow =
  res
    "DescribeChannelFlowResponse"
    "fixture/DescribeChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelFlow)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannels)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannel)

responseAssociateChannelFlow :: AssociateChannelFlowResponse -> TestTree
responseAssociateChannelFlow =
  res
    "AssociateChannelFlowResponse"
    "fixture/AssociateChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateChannelFlow)

responseGetMessagingSessionEndpoint :: GetMessagingSessionEndpointResponse -> TestTree
responseGetMessagingSessionEndpoint =
  res
    "GetMessagingSessionEndpointResponse"
    "fixture/GetMessagingSessionEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMessagingSessionEndpoint)

responseListChannelsModeratedByAppInstanceUser :: ListChannelsModeratedByAppInstanceUserResponse -> TestTree
responseListChannelsModeratedByAppInstanceUser =
  res
    "ListChannelsModeratedByAppInstanceUserResponse"
    "fixture/ListChannelsModeratedByAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelsModeratedByAppInstanceUser)

responseRedactChannelMessage :: RedactChannelMessageResponse -> TestTree
responseRedactChannelMessage =
  res
    "RedactChannelMessageResponse"
    "fixture/RedactChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RedactChannelMessage)

responseListChannelFlows :: ListChannelFlowsResponse -> TestTree
responseListChannelFlows =
  res
    "ListChannelFlowsResponse"
    "fixture/ListChannelFlowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelFlows)

responseDeleteChannelFlow :: DeleteChannelFlowResponse -> TestTree
responseDeleteChannelFlow =
  res
    "DeleteChannelFlowResponse"
    "fixture/DeleteChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelFlow)

responseUpdateChannelFlow :: UpdateChannelFlowResponse -> TestTree
responseUpdateChannelFlow =
  res
    "UpdateChannelFlowResponse"
    "fixture/UpdateChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelFlow)

responseDeleteChannelMembership :: DeleteChannelMembershipResponse -> TestTree
responseDeleteChannelMembership =
  res
    "DeleteChannelMembershipResponse"
    "fixture/DeleteChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelMembership)

responseListChannelMemberships :: ListChannelMembershipsResponse -> TestTree
responseListChannelMemberships =
  res
    "ListChannelMembershipsResponse"
    "fixture/ListChannelMembershipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelMemberships)

responseDisassociateChannelFlow :: DisassociateChannelFlowResponse -> TestTree
responseDisassociateChannelFlow =
  res
    "DisassociateChannelFlowResponse"
    "fixture/DisassociateChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateChannelFlow)

responseGetChannelMessage :: GetChannelMessageResponse -> TestTree
responseGetChannelMessage =
  res
    "GetChannelMessageResponse"
    "fixture/GetChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannelMessage)

responseDescribeChannelMembershipForAppInstanceUser :: DescribeChannelMembershipForAppInstanceUserResponse -> TestTree
responseDescribeChannelMembershipForAppInstanceUser =
  res
    "DescribeChannelMembershipForAppInstanceUserResponse"
    "fixture/DescribeChannelMembershipForAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelMembershipForAppInstanceUser)

responseCreateChannelModerator :: CreateChannelModeratorResponse -> TestTree
responseCreateChannelModerator =
  res
    "CreateChannelModeratorResponse"
    "fixture/CreateChannelModeratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannelModerator)

responseDescribeChannelModeratedByAppInstanceUser :: DescribeChannelModeratedByAppInstanceUserResponse -> TestTree
responseDescribeChannelModeratedByAppInstanceUser =
  res
    "DescribeChannelModeratedByAppInstanceUserResponse"
    "fixture/DescribeChannelModeratedByAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelModeratedByAppInstanceUser)

responseSendChannelMessage :: SendChannelMessageResponse -> TestTree
responseSendChannelMessage =
  res
    "SendChannelMessageResponse"
    "fixture/SendChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendChannelMessage)

responseDeleteChannelBan :: DeleteChannelBanResponse -> TestTree
responseDeleteChannelBan =
  res
    "DeleteChannelBanResponse"
    "fixture/DeleteChannelBanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelBan)

responseListChannelBans :: ListChannelBansResponse -> TestTree
responseListChannelBans =
  res
    "ListChannelBansResponse"
    "fixture/ListChannelBansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelBans)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannel)

responseDescribeChannelModerator :: DescribeChannelModeratorResponse -> TestTree
responseDescribeChannelModerator =
  res
    "DescribeChannelModeratorResponse"
    "fixture/DescribeChannelModeratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelModerator)

responseCreateChannelBan :: CreateChannelBanResponse -> TestTree
responseCreateChannelBan =
  res
    "CreateChannelBanResponse"
    "fixture/CreateChannelBanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannelBan)

responseListChannelMembershipsForAppInstanceUser :: ListChannelMembershipsForAppInstanceUserResponse -> TestTree
responseListChannelMembershipsForAppInstanceUser =
  res
    "ListChannelMembershipsForAppInstanceUserResponse"
    "fixture/ListChannelMembershipsForAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelMembershipsForAppInstanceUser)

responseUpdateChannelReadMarker :: UpdateChannelReadMarkerResponse -> TestTree
responseUpdateChannelReadMarker =
  res
    "UpdateChannelReadMarkerResponse"
    "fixture/UpdateChannelReadMarkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelReadMarker)

responseGetChannelMessageStatus :: GetChannelMessageStatusResponse -> TestTree
responseGetChannelMessageStatus =
  res
    "GetChannelMessageStatusResponse"
    "fixture/GetChannelMessageStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannelMessageStatus)

responseCreateChannelFlow :: CreateChannelFlowResponse -> TestTree
responseCreateChannelFlow =
  res
    "CreateChannelFlowResponse"
    "fixture/CreateChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannelFlow)

responseCreateChannelMembership :: CreateChannelMembershipResponse -> TestTree
responseCreateChannelMembership =
  res
    "CreateChannelMembershipResponse"
    "fixture/CreateChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannelMembership)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseChannelFlowCallback :: ChannelFlowCallbackResponse -> TestTree
responseChannelFlowCallback =
  res
    "ChannelFlowCallbackResponse"
    "fixture/ChannelFlowCallbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChannelFlowCallback)

responseDeleteChannelModerator :: DeleteChannelModeratorResponse -> TestTree
responseDeleteChannelModerator =
  res
    "DeleteChannelModeratorResponse"
    "fixture/DeleteChannelModeratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelModerator)

responseDescribeChannelBan :: DescribeChannelBanResponse -> TestTree
responseDescribeChannelBan =
  res
    "DescribeChannelBanResponse"
    "fixture/DescribeChannelBanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelBan)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListChannelModerators :: ListChannelModeratorsResponse -> TestTree
responseListChannelModerators =
  res
    "ListChannelModeratorsResponse"
    "fixture/ListChannelModeratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelModerators)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannel)

responseDeleteChannelMessage :: DeleteChannelMessageResponse -> TestTree
responseDeleteChannelMessage =
  res
    "DeleteChannelMessageResponse"
    "fixture/DeleteChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelMessage)

responseUpdateChannelMessage :: UpdateChannelMessageResponse -> TestTree
responseUpdateChannelMessage =
  res
    "UpdateChannelMessageResponse"
    "fixture/UpdateChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelMessage)

responseListChannelMessages :: ListChannelMessagesResponse -> TestTree
responseListChannelMessages =
  res
    "ListChannelMessagesResponse"
    "fixture/ListChannelMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelMessages)

responseListChannelsAssociatedWithChannelFlow :: ListChannelsAssociatedWithChannelFlowResponse -> TestTree
responseListChannelsAssociatedWithChannelFlow =
  res
    "ListChannelsAssociatedWithChannelFlowResponse"
    "fixture/ListChannelsAssociatedWithChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelsAssociatedWithChannelFlow)

responseBatchCreateChannelMembership :: BatchCreateChannelMembershipResponse -> TestTree
responseBatchCreateChannelMembership =
  res
    "BatchCreateChannelMembershipResponse"
    "fixture/BatchCreateChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateChannelMembership)

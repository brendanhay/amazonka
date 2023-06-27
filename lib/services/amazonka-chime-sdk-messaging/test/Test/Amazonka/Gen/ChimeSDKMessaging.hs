{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ChimeSDKMessaging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ChimeSDKMessaging where

import Amazonka.ChimeSDKMessaging
import qualified Data.Proxy as Proxy
import Test.Amazonka.ChimeSDKMessaging.Internal
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
--         [ requestAssociateChannelFlow $
--             newAssociateChannelFlow
--
--         , requestBatchCreateChannelMembership $
--             newBatchCreateChannelMembership
--
--         , requestChannelFlowCallback $
--             newChannelFlowCallback
--
--         , requestCreateChannel $
--             newCreateChannel
--
--         , requestCreateChannelBan $
--             newCreateChannelBan
--
--         , requestCreateChannelFlow $
--             newCreateChannelFlow
--
--         , requestCreateChannelMembership $
--             newCreateChannelMembership
--
--         , requestCreateChannelModerator $
--             newCreateChannelModerator
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestDeleteChannelBan $
--             newDeleteChannelBan
--
--         , requestDeleteChannelFlow $
--             newDeleteChannelFlow
--
--         , requestDeleteChannelMembership $
--             newDeleteChannelMembership
--
--         , requestDeleteChannelMessage $
--             newDeleteChannelMessage
--
--         , requestDeleteChannelModerator $
--             newDeleteChannelModerator
--
--         , requestDeleteMessagingStreamingConfigurations $
--             newDeleteMessagingStreamingConfigurations
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestDescribeChannelBan $
--             newDescribeChannelBan
--
--         , requestDescribeChannelFlow $
--             newDescribeChannelFlow
--
--         , requestDescribeChannelMembership $
--             newDescribeChannelMembership
--
--         , requestDescribeChannelMembershipForAppInstanceUser $
--             newDescribeChannelMembershipForAppInstanceUser
--
--         , requestDescribeChannelModeratedByAppInstanceUser $
--             newDescribeChannelModeratedByAppInstanceUser
--
--         , requestDescribeChannelModerator $
--             newDescribeChannelModerator
--
--         , requestDisassociateChannelFlow $
--             newDisassociateChannelFlow
--
--         , requestGetChannelMembershipPreferences $
--             newGetChannelMembershipPreferences
--
--         , requestGetChannelMessage $
--             newGetChannelMessage
--
--         , requestGetChannelMessageStatus $
--             newGetChannelMessageStatus
--
--         , requestGetMessagingSessionEndpoint $
--             newGetMessagingSessionEndpoint
--
--         , requestGetMessagingStreamingConfigurations $
--             newGetMessagingStreamingConfigurations
--
--         , requestListChannelBans $
--             newListChannelBans
--
--         , requestListChannelFlows $
--             newListChannelFlows
--
--         , requestListChannelMemberships $
--             newListChannelMemberships
--
--         , requestListChannelMembershipsForAppInstanceUser $
--             newListChannelMembershipsForAppInstanceUser
--
--         , requestListChannelMessages $
--             newListChannelMessages
--
--         , requestListChannelModerators $
--             newListChannelModerators
--
--         , requestListChannels $
--             newListChannels
--
--         , requestListChannelsAssociatedWithChannelFlow $
--             newListChannelsAssociatedWithChannelFlow
--
--         , requestListChannelsModeratedByAppInstanceUser $
--             newListChannelsModeratedByAppInstanceUser
--
--         , requestListSubChannels $
--             newListSubChannels
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutChannelExpirationSettings $
--             newPutChannelExpirationSettings
--
--         , requestPutChannelMembershipPreferences $
--             newPutChannelMembershipPreferences
--
--         , requestPutMessagingStreamingConfigurations $
--             newPutMessagingStreamingConfigurations
--
--         , requestRedactChannelMessage $
--             newRedactChannelMessage
--
--         , requestSearchChannels $
--             newSearchChannels
--
--         , requestSendChannelMessage $
--             newSendChannelMessage
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateChannel $
--             newUpdateChannel
--
--         , requestUpdateChannelFlow $
--             newUpdateChannelFlow
--
--         , requestUpdateChannelMessage $
--             newUpdateChannelMessage
--
--         , requestUpdateChannelReadMarker $
--             newUpdateChannelReadMarker
--
--           ]

--     , testGroup "response"
--         [ responseAssociateChannelFlow $
--             newAssociateChannelFlowResponse
--
--         , responseBatchCreateChannelMembership $
--             newBatchCreateChannelMembershipResponse
--
--         , responseChannelFlowCallback $
--             newChannelFlowCallbackResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseCreateChannelBan $
--             newCreateChannelBanResponse
--
--         , responseCreateChannelFlow $
--             newCreateChannelFlowResponse
--
--         , responseCreateChannelMembership $
--             newCreateChannelMembershipResponse
--
--         , responseCreateChannelModerator $
--             newCreateChannelModeratorResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseDeleteChannelBan $
--             newDeleteChannelBanResponse
--
--         , responseDeleteChannelFlow $
--             newDeleteChannelFlowResponse
--
--         , responseDeleteChannelMembership $
--             newDeleteChannelMembershipResponse
--
--         , responseDeleteChannelMessage $
--             newDeleteChannelMessageResponse
--
--         , responseDeleteChannelModerator $
--             newDeleteChannelModeratorResponse
--
--         , responseDeleteMessagingStreamingConfigurations $
--             newDeleteMessagingStreamingConfigurationsResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseDescribeChannelBan $
--             newDescribeChannelBanResponse
--
--         , responseDescribeChannelFlow $
--             newDescribeChannelFlowResponse
--
--         , responseDescribeChannelMembership $
--             newDescribeChannelMembershipResponse
--
--         , responseDescribeChannelMembershipForAppInstanceUser $
--             newDescribeChannelMembershipForAppInstanceUserResponse
--
--         , responseDescribeChannelModeratedByAppInstanceUser $
--             newDescribeChannelModeratedByAppInstanceUserResponse
--
--         , responseDescribeChannelModerator $
--             newDescribeChannelModeratorResponse
--
--         , responseDisassociateChannelFlow $
--             newDisassociateChannelFlowResponse
--
--         , responseGetChannelMembershipPreferences $
--             newGetChannelMembershipPreferencesResponse
--
--         , responseGetChannelMessage $
--             newGetChannelMessageResponse
--
--         , responseGetChannelMessageStatus $
--             newGetChannelMessageStatusResponse
--
--         , responseGetMessagingSessionEndpoint $
--             newGetMessagingSessionEndpointResponse
--
--         , responseGetMessagingStreamingConfigurations $
--             newGetMessagingStreamingConfigurationsResponse
--
--         , responseListChannelBans $
--             newListChannelBansResponse
--
--         , responseListChannelFlows $
--             newListChannelFlowsResponse
--
--         , responseListChannelMemberships $
--             newListChannelMembershipsResponse
--
--         , responseListChannelMembershipsForAppInstanceUser $
--             newListChannelMembershipsForAppInstanceUserResponse
--
--         , responseListChannelMessages $
--             newListChannelMessagesResponse
--
--         , responseListChannelModerators $
--             newListChannelModeratorsResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseListChannelsAssociatedWithChannelFlow $
--             newListChannelsAssociatedWithChannelFlowResponse
--
--         , responseListChannelsModeratedByAppInstanceUser $
--             newListChannelsModeratedByAppInstanceUserResponse
--
--         , responseListSubChannels $
--             newListSubChannelsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutChannelExpirationSettings $
--             newPutChannelExpirationSettingsResponse
--
--         , responsePutChannelMembershipPreferences $
--             newPutChannelMembershipPreferencesResponse
--
--         , responsePutMessagingStreamingConfigurations $
--             newPutMessagingStreamingConfigurationsResponse
--
--         , responseRedactChannelMessage $
--             newRedactChannelMessageResponse
--
--         , responseSearchChannels $
--             newSearchChannelsResponse
--
--         , responseSendChannelMessage $
--             newSendChannelMessageResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateChannel $
--             newUpdateChannelResponse
--
--         , responseUpdateChannelFlow $
--             newUpdateChannelFlowResponse
--
--         , responseUpdateChannelMessage $
--             newUpdateChannelMessageResponse
--
--         , responseUpdateChannelReadMarker $
--             newUpdateChannelReadMarkerResponse
--
--           ]
--     ]

-- Requests

requestAssociateChannelFlow :: AssociateChannelFlow -> TestTree
requestAssociateChannelFlow =
  req
    "AssociateChannelFlow"
    "fixture/AssociateChannelFlow.yaml"

requestBatchCreateChannelMembership :: BatchCreateChannelMembership -> TestTree
requestBatchCreateChannelMembership =
  req
    "BatchCreateChannelMembership"
    "fixture/BatchCreateChannelMembership.yaml"

requestChannelFlowCallback :: ChannelFlowCallback -> TestTree
requestChannelFlowCallback =
  req
    "ChannelFlowCallback"
    "fixture/ChannelFlowCallback.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestCreateChannelBan :: CreateChannelBan -> TestTree
requestCreateChannelBan =
  req
    "CreateChannelBan"
    "fixture/CreateChannelBan.yaml"

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

requestCreateChannelModerator :: CreateChannelModerator -> TestTree
requestCreateChannelModerator =
  req
    "CreateChannelModerator"
    "fixture/CreateChannelModerator.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestDeleteChannelBan :: DeleteChannelBan -> TestTree
requestDeleteChannelBan =
  req
    "DeleteChannelBan"
    "fixture/DeleteChannelBan.yaml"

requestDeleteChannelFlow :: DeleteChannelFlow -> TestTree
requestDeleteChannelFlow =
  req
    "DeleteChannelFlow"
    "fixture/DeleteChannelFlow.yaml"

requestDeleteChannelMembership :: DeleteChannelMembership -> TestTree
requestDeleteChannelMembership =
  req
    "DeleteChannelMembership"
    "fixture/DeleteChannelMembership.yaml"

requestDeleteChannelMessage :: DeleteChannelMessage -> TestTree
requestDeleteChannelMessage =
  req
    "DeleteChannelMessage"
    "fixture/DeleteChannelMessage.yaml"

requestDeleteChannelModerator :: DeleteChannelModerator -> TestTree
requestDeleteChannelModerator =
  req
    "DeleteChannelModerator"
    "fixture/DeleteChannelModerator.yaml"

requestDeleteMessagingStreamingConfigurations :: DeleteMessagingStreamingConfigurations -> TestTree
requestDeleteMessagingStreamingConfigurations =
  req
    "DeleteMessagingStreamingConfigurations"
    "fixture/DeleteMessagingStreamingConfigurations.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestDescribeChannelBan :: DescribeChannelBan -> TestTree
requestDescribeChannelBan =
  req
    "DescribeChannelBan"
    "fixture/DescribeChannelBan.yaml"

requestDescribeChannelFlow :: DescribeChannelFlow -> TestTree
requestDescribeChannelFlow =
  req
    "DescribeChannelFlow"
    "fixture/DescribeChannelFlow.yaml"

requestDescribeChannelMembership :: DescribeChannelMembership -> TestTree
requestDescribeChannelMembership =
  req
    "DescribeChannelMembership"
    "fixture/DescribeChannelMembership.yaml"

requestDescribeChannelMembershipForAppInstanceUser :: DescribeChannelMembershipForAppInstanceUser -> TestTree
requestDescribeChannelMembershipForAppInstanceUser =
  req
    "DescribeChannelMembershipForAppInstanceUser"
    "fixture/DescribeChannelMembershipForAppInstanceUser.yaml"

requestDescribeChannelModeratedByAppInstanceUser :: DescribeChannelModeratedByAppInstanceUser -> TestTree
requestDescribeChannelModeratedByAppInstanceUser =
  req
    "DescribeChannelModeratedByAppInstanceUser"
    "fixture/DescribeChannelModeratedByAppInstanceUser.yaml"

requestDescribeChannelModerator :: DescribeChannelModerator -> TestTree
requestDescribeChannelModerator =
  req
    "DescribeChannelModerator"
    "fixture/DescribeChannelModerator.yaml"

requestDisassociateChannelFlow :: DisassociateChannelFlow -> TestTree
requestDisassociateChannelFlow =
  req
    "DisassociateChannelFlow"
    "fixture/DisassociateChannelFlow.yaml"

requestGetChannelMembershipPreferences :: GetChannelMembershipPreferences -> TestTree
requestGetChannelMembershipPreferences =
  req
    "GetChannelMembershipPreferences"
    "fixture/GetChannelMembershipPreferences.yaml"

requestGetChannelMessage :: GetChannelMessage -> TestTree
requestGetChannelMessage =
  req
    "GetChannelMessage"
    "fixture/GetChannelMessage.yaml"

requestGetChannelMessageStatus :: GetChannelMessageStatus -> TestTree
requestGetChannelMessageStatus =
  req
    "GetChannelMessageStatus"
    "fixture/GetChannelMessageStatus.yaml"

requestGetMessagingSessionEndpoint :: GetMessagingSessionEndpoint -> TestTree
requestGetMessagingSessionEndpoint =
  req
    "GetMessagingSessionEndpoint"
    "fixture/GetMessagingSessionEndpoint.yaml"

requestGetMessagingStreamingConfigurations :: GetMessagingStreamingConfigurations -> TestTree
requestGetMessagingStreamingConfigurations =
  req
    "GetMessagingStreamingConfigurations"
    "fixture/GetMessagingStreamingConfigurations.yaml"

requestListChannelBans :: ListChannelBans -> TestTree
requestListChannelBans =
  req
    "ListChannelBans"
    "fixture/ListChannelBans.yaml"

requestListChannelFlows :: ListChannelFlows -> TestTree
requestListChannelFlows =
  req
    "ListChannelFlows"
    "fixture/ListChannelFlows.yaml"

requestListChannelMemberships :: ListChannelMemberships -> TestTree
requestListChannelMemberships =
  req
    "ListChannelMemberships"
    "fixture/ListChannelMemberships.yaml"

requestListChannelMembershipsForAppInstanceUser :: ListChannelMembershipsForAppInstanceUser -> TestTree
requestListChannelMembershipsForAppInstanceUser =
  req
    "ListChannelMembershipsForAppInstanceUser"
    "fixture/ListChannelMembershipsForAppInstanceUser.yaml"

requestListChannelMessages :: ListChannelMessages -> TestTree
requestListChannelMessages =
  req
    "ListChannelMessages"
    "fixture/ListChannelMessages.yaml"

requestListChannelModerators :: ListChannelModerators -> TestTree
requestListChannelModerators =
  req
    "ListChannelModerators"
    "fixture/ListChannelModerators.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestListChannelsAssociatedWithChannelFlow :: ListChannelsAssociatedWithChannelFlow -> TestTree
requestListChannelsAssociatedWithChannelFlow =
  req
    "ListChannelsAssociatedWithChannelFlow"
    "fixture/ListChannelsAssociatedWithChannelFlow.yaml"

requestListChannelsModeratedByAppInstanceUser :: ListChannelsModeratedByAppInstanceUser -> TestTree
requestListChannelsModeratedByAppInstanceUser =
  req
    "ListChannelsModeratedByAppInstanceUser"
    "fixture/ListChannelsModeratedByAppInstanceUser.yaml"

requestListSubChannels :: ListSubChannels -> TestTree
requestListSubChannels =
  req
    "ListSubChannels"
    "fixture/ListSubChannels.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutChannelExpirationSettings :: PutChannelExpirationSettings -> TestTree
requestPutChannelExpirationSettings =
  req
    "PutChannelExpirationSettings"
    "fixture/PutChannelExpirationSettings.yaml"

requestPutChannelMembershipPreferences :: PutChannelMembershipPreferences -> TestTree
requestPutChannelMembershipPreferences =
  req
    "PutChannelMembershipPreferences"
    "fixture/PutChannelMembershipPreferences.yaml"

requestPutMessagingStreamingConfigurations :: PutMessagingStreamingConfigurations -> TestTree
requestPutMessagingStreamingConfigurations =
  req
    "PutMessagingStreamingConfigurations"
    "fixture/PutMessagingStreamingConfigurations.yaml"

requestRedactChannelMessage :: RedactChannelMessage -> TestTree
requestRedactChannelMessage =
  req
    "RedactChannelMessage"
    "fixture/RedactChannelMessage.yaml"

requestSearchChannels :: SearchChannels -> TestTree
requestSearchChannels =
  req
    "SearchChannels"
    "fixture/SearchChannels.yaml"

requestSendChannelMessage :: SendChannelMessage -> TestTree
requestSendChannelMessage =
  req
    "SendChannelMessage"
    "fixture/SendChannelMessage.yaml"

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

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestUpdateChannelFlow :: UpdateChannelFlow -> TestTree
requestUpdateChannelFlow =
  req
    "UpdateChannelFlow"
    "fixture/UpdateChannelFlow.yaml"

requestUpdateChannelMessage :: UpdateChannelMessage -> TestTree
requestUpdateChannelMessage =
  req
    "UpdateChannelMessage"
    "fixture/UpdateChannelMessage.yaml"

requestUpdateChannelReadMarker :: UpdateChannelReadMarker -> TestTree
requestUpdateChannelReadMarker =
  req
    "UpdateChannelReadMarker"
    "fixture/UpdateChannelReadMarker.yaml"

-- Responses

responseAssociateChannelFlow :: AssociateChannelFlowResponse -> TestTree
responseAssociateChannelFlow =
  res
    "AssociateChannelFlowResponse"
    "fixture/AssociateChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateChannelFlow)

responseBatchCreateChannelMembership :: BatchCreateChannelMembershipResponse -> TestTree
responseBatchCreateChannelMembership =
  res
    "BatchCreateChannelMembershipResponse"
    "fixture/BatchCreateChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateChannelMembership)

responseChannelFlowCallback :: ChannelFlowCallbackResponse -> TestTree
responseChannelFlowCallback =
  res
    "ChannelFlowCallbackResponse"
    "fixture/ChannelFlowCallbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChannelFlowCallback)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannel)

responseCreateChannelBan :: CreateChannelBanResponse -> TestTree
responseCreateChannelBan =
  res
    "CreateChannelBanResponse"
    "fixture/CreateChannelBanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannelBan)

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

responseCreateChannelModerator :: CreateChannelModeratorResponse -> TestTree
responseCreateChannelModerator =
  res
    "CreateChannelModeratorResponse"
    "fixture/CreateChannelModeratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannelModerator)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannel)

responseDeleteChannelBan :: DeleteChannelBanResponse -> TestTree
responseDeleteChannelBan =
  res
    "DeleteChannelBanResponse"
    "fixture/DeleteChannelBanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelBan)

responseDeleteChannelFlow :: DeleteChannelFlowResponse -> TestTree
responseDeleteChannelFlow =
  res
    "DeleteChannelFlowResponse"
    "fixture/DeleteChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelFlow)

responseDeleteChannelMembership :: DeleteChannelMembershipResponse -> TestTree
responseDeleteChannelMembership =
  res
    "DeleteChannelMembershipResponse"
    "fixture/DeleteChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelMembership)

responseDeleteChannelMessage :: DeleteChannelMessageResponse -> TestTree
responseDeleteChannelMessage =
  res
    "DeleteChannelMessageResponse"
    "fixture/DeleteChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelMessage)

responseDeleteChannelModerator :: DeleteChannelModeratorResponse -> TestTree
responseDeleteChannelModerator =
  res
    "DeleteChannelModeratorResponse"
    "fixture/DeleteChannelModeratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelModerator)

responseDeleteMessagingStreamingConfigurations :: DeleteMessagingStreamingConfigurationsResponse -> TestTree
responseDeleteMessagingStreamingConfigurations =
  res
    "DeleteMessagingStreamingConfigurationsResponse"
    "fixture/DeleteMessagingStreamingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMessagingStreamingConfigurations)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannel)

responseDescribeChannelBan :: DescribeChannelBanResponse -> TestTree
responseDescribeChannelBan =
  res
    "DescribeChannelBanResponse"
    "fixture/DescribeChannelBanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelBan)

responseDescribeChannelFlow :: DescribeChannelFlowResponse -> TestTree
responseDescribeChannelFlow =
  res
    "DescribeChannelFlowResponse"
    "fixture/DescribeChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelFlow)

responseDescribeChannelMembership :: DescribeChannelMembershipResponse -> TestTree
responseDescribeChannelMembership =
  res
    "DescribeChannelMembershipResponse"
    "fixture/DescribeChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelMembership)

responseDescribeChannelMembershipForAppInstanceUser :: DescribeChannelMembershipForAppInstanceUserResponse -> TestTree
responseDescribeChannelMembershipForAppInstanceUser =
  res
    "DescribeChannelMembershipForAppInstanceUserResponse"
    "fixture/DescribeChannelMembershipForAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelMembershipForAppInstanceUser)

responseDescribeChannelModeratedByAppInstanceUser :: DescribeChannelModeratedByAppInstanceUserResponse -> TestTree
responseDescribeChannelModeratedByAppInstanceUser =
  res
    "DescribeChannelModeratedByAppInstanceUserResponse"
    "fixture/DescribeChannelModeratedByAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelModeratedByAppInstanceUser)

responseDescribeChannelModerator :: DescribeChannelModeratorResponse -> TestTree
responseDescribeChannelModerator =
  res
    "DescribeChannelModeratorResponse"
    "fixture/DescribeChannelModeratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelModerator)

responseDisassociateChannelFlow :: DisassociateChannelFlowResponse -> TestTree
responseDisassociateChannelFlow =
  res
    "DisassociateChannelFlowResponse"
    "fixture/DisassociateChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateChannelFlow)

responseGetChannelMembershipPreferences :: GetChannelMembershipPreferencesResponse -> TestTree
responseGetChannelMembershipPreferences =
  res
    "GetChannelMembershipPreferencesResponse"
    "fixture/GetChannelMembershipPreferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannelMembershipPreferences)

responseGetChannelMessage :: GetChannelMessageResponse -> TestTree
responseGetChannelMessage =
  res
    "GetChannelMessageResponse"
    "fixture/GetChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannelMessage)

responseGetChannelMessageStatus :: GetChannelMessageStatusResponse -> TestTree
responseGetChannelMessageStatus =
  res
    "GetChannelMessageStatusResponse"
    "fixture/GetChannelMessageStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannelMessageStatus)

responseGetMessagingSessionEndpoint :: GetMessagingSessionEndpointResponse -> TestTree
responseGetMessagingSessionEndpoint =
  res
    "GetMessagingSessionEndpointResponse"
    "fixture/GetMessagingSessionEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMessagingSessionEndpoint)

responseGetMessagingStreamingConfigurations :: GetMessagingStreamingConfigurationsResponse -> TestTree
responseGetMessagingStreamingConfigurations =
  res
    "GetMessagingStreamingConfigurationsResponse"
    "fixture/GetMessagingStreamingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMessagingStreamingConfigurations)

responseListChannelBans :: ListChannelBansResponse -> TestTree
responseListChannelBans =
  res
    "ListChannelBansResponse"
    "fixture/ListChannelBansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelBans)

responseListChannelFlows :: ListChannelFlowsResponse -> TestTree
responseListChannelFlows =
  res
    "ListChannelFlowsResponse"
    "fixture/ListChannelFlowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelFlows)

responseListChannelMemberships :: ListChannelMembershipsResponse -> TestTree
responseListChannelMemberships =
  res
    "ListChannelMembershipsResponse"
    "fixture/ListChannelMembershipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelMemberships)

responseListChannelMembershipsForAppInstanceUser :: ListChannelMembershipsForAppInstanceUserResponse -> TestTree
responseListChannelMembershipsForAppInstanceUser =
  res
    "ListChannelMembershipsForAppInstanceUserResponse"
    "fixture/ListChannelMembershipsForAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelMembershipsForAppInstanceUser)

responseListChannelMessages :: ListChannelMessagesResponse -> TestTree
responseListChannelMessages =
  res
    "ListChannelMessagesResponse"
    "fixture/ListChannelMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelMessages)

responseListChannelModerators :: ListChannelModeratorsResponse -> TestTree
responseListChannelModerators =
  res
    "ListChannelModeratorsResponse"
    "fixture/ListChannelModeratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelModerators)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannels)

responseListChannelsAssociatedWithChannelFlow :: ListChannelsAssociatedWithChannelFlowResponse -> TestTree
responseListChannelsAssociatedWithChannelFlow =
  res
    "ListChannelsAssociatedWithChannelFlowResponse"
    "fixture/ListChannelsAssociatedWithChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelsAssociatedWithChannelFlow)

responseListChannelsModeratedByAppInstanceUser :: ListChannelsModeratedByAppInstanceUserResponse -> TestTree
responseListChannelsModeratedByAppInstanceUser =
  res
    "ListChannelsModeratedByAppInstanceUserResponse"
    "fixture/ListChannelsModeratedByAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelsModeratedByAppInstanceUser)

responseListSubChannels :: ListSubChannelsResponse -> TestTree
responseListSubChannels =
  res
    "ListSubChannelsResponse"
    "fixture/ListSubChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubChannels)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutChannelExpirationSettings :: PutChannelExpirationSettingsResponse -> TestTree
responsePutChannelExpirationSettings =
  res
    "PutChannelExpirationSettingsResponse"
    "fixture/PutChannelExpirationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutChannelExpirationSettings)

responsePutChannelMembershipPreferences :: PutChannelMembershipPreferencesResponse -> TestTree
responsePutChannelMembershipPreferences =
  res
    "PutChannelMembershipPreferencesResponse"
    "fixture/PutChannelMembershipPreferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutChannelMembershipPreferences)

responsePutMessagingStreamingConfigurations :: PutMessagingStreamingConfigurationsResponse -> TestTree
responsePutMessagingStreamingConfigurations =
  res
    "PutMessagingStreamingConfigurationsResponse"
    "fixture/PutMessagingStreamingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMessagingStreamingConfigurations)

responseRedactChannelMessage :: RedactChannelMessageResponse -> TestTree
responseRedactChannelMessage =
  res
    "RedactChannelMessageResponse"
    "fixture/RedactChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RedactChannelMessage)

responseSearchChannels :: SearchChannelsResponse -> TestTree
responseSearchChannels =
  res
    "SearchChannelsResponse"
    "fixture/SearchChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchChannels)

responseSendChannelMessage :: SendChannelMessageResponse -> TestTree
responseSendChannelMessage =
  res
    "SendChannelMessageResponse"
    "fixture/SendChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendChannelMessage)

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

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannel)

responseUpdateChannelFlow :: UpdateChannelFlowResponse -> TestTree
responseUpdateChannelFlow =
  res
    "UpdateChannelFlowResponse"
    "fixture/UpdateChannelFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelFlow)

responseUpdateChannelMessage :: UpdateChannelMessageResponse -> TestTree
responseUpdateChannelMessage =
  res
    "UpdateChannelMessageResponse"
    "fixture/UpdateChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelMessage)

responseUpdateChannelReadMarker :: UpdateChannelReadMarkerResponse -> TestTree
responseUpdateChannelReadMarker =
  res
    "UpdateChannelReadMarkerResponse"
    "fixture/UpdateChannelReadMarkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelReadMarker)

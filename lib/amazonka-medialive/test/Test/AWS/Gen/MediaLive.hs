{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaLive
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.MediaLive where

import Data.Proxy
import Network.AWS.MediaLive
import Test.AWS.Fixture
import Test.AWS.MediaLive.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListInputs $
--             listInputs
--
--         , requestListChannels $
--             listChannels
--
--         , requestDescribeInputSecurityGroup $
--             describeInputSecurityGroup
--
--         , requestCreateInput $
--             createInput
--
--         , requestDeleteChannel $
--             deleteChannel
--
--         , requestUpdateChannel $
--             updateChannel
--
--         , requestCreateInputSecurityGroup $
--             createInputSecurityGroup
--
--         , requestStartChannel $
--             startChannel
--
--         , requestListInputSecurityGroups $
--             listInputSecurityGroups
--
--         , requestCreateChannel $
--             createChannel
--
--         , requestDeleteInput $
--             deleteInput
--
--         , requestUpdateInput $
--             updateInput
--
--         , requestStopChannel $
--             stopChannel
--
--         , requestDescribeInput $
--             describeInput
--
--         , requestDescribeChannel $
--             describeChannel
--
--         , requestUpdateInputSecurityGroup $
--             updateInputSecurityGroup
--
--         , requestDeleteInputSecurityGroup $
--             deleteInputSecurityGroup
--
--           ]

--     , testGroup "response"
--         [ responseListInputs $
--             listInputsResponse
--
--         , responseListChannels $
--             listChannelsResponse
--
--         , responseDescribeInputSecurityGroup $
--             describeInputSecurityGroupResponse
--
--         , responseCreateInput $
--             createInputResponse
--
--         , responseDeleteChannel $
--             deleteChannelResponse
--
--         , responseUpdateChannel $
--             updateChannelResponse
--
--         , responseCreateInputSecurityGroup $
--             createInputSecurityGroupResponse
--
--         , responseStartChannel $
--             startChannelResponse
--
--         , responseListInputSecurityGroups $
--             listInputSecurityGroupsResponse
--
--         , responseCreateChannel $
--             createChannelResponse
--
--         , responseDeleteInput $
--             deleteInputResponse
--
--         , responseUpdateInput $
--             updateInputResponse
--
--         , responseStopChannel $
--             stopChannelResponse
--
--         , responseDescribeInput $
--             describeInputResponse
--
--         , responseDescribeChannel $
--             describeChannelResponse
--
--         , responseUpdateInputSecurityGroup $
--             updateInputSecurityGroupResponse
--
--         , responseDeleteInputSecurityGroup $
--             deleteInputSecurityGroupResponse
--
--           ]
--     ]

-- Requests

requestListInputs :: ListInputs -> TestTree
requestListInputs = req
    "ListInputs"
    "fixture/ListInputs.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels = req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestDescribeInputSecurityGroup :: DescribeInputSecurityGroup -> TestTree
requestDescribeInputSecurityGroup = req
    "DescribeInputSecurityGroup"
    "fixture/DescribeInputSecurityGroup.yaml"

requestCreateInput :: CreateInput -> TestTree
requestCreateInput = req
    "CreateInput"
    "fixture/CreateInput.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel = req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel = req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestCreateInputSecurityGroup :: CreateInputSecurityGroup -> TestTree
requestCreateInputSecurityGroup = req
    "CreateInputSecurityGroup"
    "fixture/CreateInputSecurityGroup.yaml"

requestStartChannel :: StartChannel -> TestTree
requestStartChannel = req
    "StartChannel"
    "fixture/StartChannel.yaml"

requestListInputSecurityGroups :: ListInputSecurityGroups -> TestTree
requestListInputSecurityGroups = req
    "ListInputSecurityGroups"
    "fixture/ListInputSecurityGroups.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel = req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestDeleteInput :: DeleteInput -> TestTree
requestDeleteInput = req
    "DeleteInput"
    "fixture/DeleteInput.yaml"

requestUpdateInput :: UpdateInput -> TestTree
requestUpdateInput = req
    "UpdateInput"
    "fixture/UpdateInput.yaml"

requestStopChannel :: StopChannel -> TestTree
requestStopChannel = req
    "StopChannel"
    "fixture/StopChannel.yaml"

requestDescribeInput :: DescribeInput -> TestTree
requestDescribeInput = req
    "DescribeInput"
    "fixture/DescribeInput.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel = req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestUpdateInputSecurityGroup :: UpdateInputSecurityGroup -> TestTree
requestUpdateInputSecurityGroup = req
    "UpdateInputSecurityGroup"
    "fixture/UpdateInputSecurityGroup.yaml"

requestDeleteInputSecurityGroup :: DeleteInputSecurityGroup -> TestTree
requestDeleteInputSecurityGroup = req
    "DeleteInputSecurityGroup"
    "fixture/DeleteInputSecurityGroup.yaml"

-- Responses

responseListInputs :: ListInputsResponse -> TestTree
responseListInputs = res
    "ListInputsResponse"
    "fixture/ListInputsResponse.proto"
    mediaLive
    (Proxy :: Proxy ListInputs)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels = res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    mediaLive
    (Proxy :: Proxy ListChannels)

responseDescribeInputSecurityGroup :: DescribeInputSecurityGroupResponse -> TestTree
responseDescribeInputSecurityGroup = res
    "DescribeInputSecurityGroupResponse"
    "fixture/DescribeInputSecurityGroupResponse.proto"
    mediaLive
    (Proxy :: Proxy DescribeInputSecurityGroup)

responseCreateInput :: CreateInputResponse -> TestTree
responseCreateInput = res
    "CreateInputResponse"
    "fixture/CreateInputResponse.proto"
    mediaLive
    (Proxy :: Proxy CreateInput)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel = res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    mediaLive
    (Proxy :: Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel = res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    mediaLive
    (Proxy :: Proxy UpdateChannel)

responseCreateInputSecurityGroup :: CreateInputSecurityGroupResponse -> TestTree
responseCreateInputSecurityGroup = res
    "CreateInputSecurityGroupResponse"
    "fixture/CreateInputSecurityGroupResponse.proto"
    mediaLive
    (Proxy :: Proxy CreateInputSecurityGroup)

responseStartChannel :: StartChannelResponse -> TestTree
responseStartChannel = res
    "StartChannelResponse"
    "fixture/StartChannelResponse.proto"
    mediaLive
    (Proxy :: Proxy StartChannel)

responseListInputSecurityGroups :: ListInputSecurityGroupsResponse -> TestTree
responseListInputSecurityGroups = res
    "ListInputSecurityGroupsResponse"
    "fixture/ListInputSecurityGroupsResponse.proto"
    mediaLive
    (Proxy :: Proxy ListInputSecurityGroups)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel = res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    mediaLive
    (Proxy :: Proxy CreateChannel)

responseDeleteInput :: DeleteInputResponse -> TestTree
responseDeleteInput = res
    "DeleteInputResponse"
    "fixture/DeleteInputResponse.proto"
    mediaLive
    (Proxy :: Proxy DeleteInput)

responseUpdateInput :: UpdateInputResponse -> TestTree
responseUpdateInput = res
    "UpdateInputResponse"
    "fixture/UpdateInputResponse.proto"
    mediaLive
    (Proxy :: Proxy UpdateInput)

responseStopChannel :: StopChannelResponse -> TestTree
responseStopChannel = res
    "StopChannelResponse"
    "fixture/StopChannelResponse.proto"
    mediaLive
    (Proxy :: Proxy StopChannel)

responseDescribeInput :: DescribeInputResponse -> TestTree
responseDescribeInput = res
    "DescribeInputResponse"
    "fixture/DescribeInputResponse.proto"
    mediaLive
    (Proxy :: Proxy DescribeInput)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel = res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    mediaLive
    (Proxy :: Proxy DescribeChannel)

responseUpdateInputSecurityGroup :: UpdateInputSecurityGroupResponse -> TestTree
responseUpdateInputSecurityGroup = res
    "UpdateInputSecurityGroupResponse"
    "fixture/UpdateInputSecurityGroupResponse.proto"
    mediaLive
    (Proxy :: Proxy UpdateInputSecurityGroup)

responseDeleteInputSecurityGroup :: DeleteInputSecurityGroupResponse -> TestTree
responseDeleteInputSecurityGroup = res
    "DeleteInputSecurityGroupResponse"
    "fixture/DeleteInputSecurityGroupResponse.proto"
    mediaLive
    (Proxy :: Proxy DeleteInputSecurityGroup)

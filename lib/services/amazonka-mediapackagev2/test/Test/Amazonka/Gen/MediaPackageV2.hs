{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MediaPackageV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MediaPackageV2 where

import Amazonka.MediaPackageV2
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MediaPackageV2.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateChannel $
--             newCreateChannel
--
--         , requestCreateChannelGroup $
--             newCreateChannelGroup
--
--         , requestCreateOriginEndpoint $
--             newCreateOriginEndpoint
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestDeleteChannelGroup $
--             newDeleteChannelGroup
--
--         , requestDeleteChannelPolicy $
--             newDeleteChannelPolicy
--
--         , requestDeleteOriginEndpoint $
--             newDeleteOriginEndpoint
--
--         , requestDeleteOriginEndpointPolicy $
--             newDeleteOriginEndpointPolicy
--
--         , requestGetChannel $
--             newGetChannel
--
--         , requestGetChannelGroup $
--             newGetChannelGroup
--
--         , requestGetChannelPolicy $
--             newGetChannelPolicy
--
--         , requestGetOriginEndpoint $
--             newGetOriginEndpoint
--
--         , requestGetOriginEndpointPolicy $
--             newGetOriginEndpointPolicy
--
--         , requestListChannelGroups $
--             newListChannelGroups
--
--         , requestListChannels $
--             newListChannels
--
--         , requestListOriginEndpoints $
--             newListOriginEndpoints
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutChannelPolicy $
--             newPutChannelPolicy
--
--         , requestPutOriginEndpointPolicy $
--             newPutOriginEndpointPolicy
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
--         , requestUpdateChannelGroup $
--             newUpdateChannelGroup
--
--         , requestUpdateOriginEndpoint $
--             newUpdateOriginEndpoint
--
--           ]

--     , testGroup "response"
--         [ responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseCreateChannelGroup $
--             newCreateChannelGroupResponse
--
--         , responseCreateOriginEndpoint $
--             newCreateOriginEndpointResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseDeleteChannelGroup $
--             newDeleteChannelGroupResponse
--
--         , responseDeleteChannelPolicy $
--             newDeleteChannelPolicyResponse
--
--         , responseDeleteOriginEndpoint $
--             newDeleteOriginEndpointResponse
--
--         , responseDeleteOriginEndpointPolicy $
--             newDeleteOriginEndpointPolicyResponse
--
--         , responseGetChannel $
--             newGetChannelResponse
--
--         , responseGetChannelGroup $
--             newGetChannelGroupResponse
--
--         , responseGetChannelPolicy $
--             newGetChannelPolicyResponse
--
--         , responseGetOriginEndpoint $
--             newGetOriginEndpointResponse
--
--         , responseGetOriginEndpointPolicy $
--             newGetOriginEndpointPolicyResponse
--
--         , responseListChannelGroups $
--             newListChannelGroupsResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseListOriginEndpoints $
--             newListOriginEndpointsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutChannelPolicy $
--             newPutChannelPolicyResponse
--
--         , responsePutOriginEndpointPolicy $
--             newPutOriginEndpointPolicyResponse
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
--         , responseUpdateChannelGroup $
--             newUpdateChannelGroupResponse
--
--         , responseUpdateOriginEndpoint $
--             newUpdateOriginEndpointResponse
--
--           ]
--     ]

-- Requests

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestCreateChannelGroup :: CreateChannelGroup -> TestTree
requestCreateChannelGroup =
  req
    "CreateChannelGroup"
    "fixture/CreateChannelGroup.yaml"

requestCreateOriginEndpoint :: CreateOriginEndpoint -> TestTree
requestCreateOriginEndpoint =
  req
    "CreateOriginEndpoint"
    "fixture/CreateOriginEndpoint.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestDeleteChannelGroup :: DeleteChannelGroup -> TestTree
requestDeleteChannelGroup =
  req
    "DeleteChannelGroup"
    "fixture/DeleteChannelGroup.yaml"

requestDeleteChannelPolicy :: DeleteChannelPolicy -> TestTree
requestDeleteChannelPolicy =
  req
    "DeleteChannelPolicy"
    "fixture/DeleteChannelPolicy.yaml"

requestDeleteOriginEndpoint :: DeleteOriginEndpoint -> TestTree
requestDeleteOriginEndpoint =
  req
    "DeleteOriginEndpoint"
    "fixture/DeleteOriginEndpoint.yaml"

requestDeleteOriginEndpointPolicy :: DeleteOriginEndpointPolicy -> TestTree
requestDeleteOriginEndpointPolicy =
  req
    "DeleteOriginEndpointPolicy"
    "fixture/DeleteOriginEndpointPolicy.yaml"

requestGetChannel :: GetChannel -> TestTree
requestGetChannel =
  req
    "GetChannel"
    "fixture/GetChannel.yaml"

requestGetChannelGroup :: GetChannelGroup -> TestTree
requestGetChannelGroup =
  req
    "GetChannelGroup"
    "fixture/GetChannelGroup.yaml"

requestGetChannelPolicy :: GetChannelPolicy -> TestTree
requestGetChannelPolicy =
  req
    "GetChannelPolicy"
    "fixture/GetChannelPolicy.yaml"

requestGetOriginEndpoint :: GetOriginEndpoint -> TestTree
requestGetOriginEndpoint =
  req
    "GetOriginEndpoint"
    "fixture/GetOriginEndpoint.yaml"

requestGetOriginEndpointPolicy :: GetOriginEndpointPolicy -> TestTree
requestGetOriginEndpointPolicy =
  req
    "GetOriginEndpointPolicy"
    "fixture/GetOriginEndpointPolicy.yaml"

requestListChannelGroups :: ListChannelGroups -> TestTree
requestListChannelGroups =
  req
    "ListChannelGroups"
    "fixture/ListChannelGroups.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestListOriginEndpoints :: ListOriginEndpoints -> TestTree
requestListOriginEndpoints =
  req
    "ListOriginEndpoints"
    "fixture/ListOriginEndpoints.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutChannelPolicy :: PutChannelPolicy -> TestTree
requestPutChannelPolicy =
  req
    "PutChannelPolicy"
    "fixture/PutChannelPolicy.yaml"

requestPutOriginEndpointPolicy :: PutOriginEndpointPolicy -> TestTree
requestPutOriginEndpointPolicy =
  req
    "PutOriginEndpointPolicy"
    "fixture/PutOriginEndpointPolicy.yaml"

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

requestUpdateChannelGroup :: UpdateChannelGroup -> TestTree
requestUpdateChannelGroup =
  req
    "UpdateChannelGroup"
    "fixture/UpdateChannelGroup.yaml"

requestUpdateOriginEndpoint :: UpdateOriginEndpoint -> TestTree
requestUpdateOriginEndpoint =
  req
    "UpdateOriginEndpoint"
    "fixture/UpdateOriginEndpoint.yaml"

-- Responses

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannel)

responseCreateChannelGroup :: CreateChannelGroupResponse -> TestTree
responseCreateChannelGroup =
  res
    "CreateChannelGroupResponse"
    "fixture/CreateChannelGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannelGroup)

responseCreateOriginEndpoint :: CreateOriginEndpointResponse -> TestTree
responseCreateOriginEndpoint =
  res
    "CreateOriginEndpointResponse"
    "fixture/CreateOriginEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOriginEndpoint)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannel)

responseDeleteChannelGroup :: DeleteChannelGroupResponse -> TestTree
responseDeleteChannelGroup =
  res
    "DeleteChannelGroupResponse"
    "fixture/DeleteChannelGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelGroup)

responseDeleteChannelPolicy :: DeleteChannelPolicyResponse -> TestTree
responseDeleteChannelPolicy =
  res
    "DeleteChannelPolicyResponse"
    "fixture/DeleteChannelPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelPolicy)

responseDeleteOriginEndpoint :: DeleteOriginEndpointResponse -> TestTree
responseDeleteOriginEndpoint =
  res
    "DeleteOriginEndpointResponse"
    "fixture/DeleteOriginEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOriginEndpoint)

responseDeleteOriginEndpointPolicy :: DeleteOriginEndpointPolicyResponse -> TestTree
responseDeleteOriginEndpointPolicy =
  res
    "DeleteOriginEndpointPolicyResponse"
    "fixture/DeleteOriginEndpointPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOriginEndpointPolicy)

responseGetChannel :: GetChannelResponse -> TestTree
responseGetChannel =
  res
    "GetChannelResponse"
    "fixture/GetChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannel)

responseGetChannelGroup :: GetChannelGroupResponse -> TestTree
responseGetChannelGroup =
  res
    "GetChannelGroupResponse"
    "fixture/GetChannelGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannelGroup)

responseGetChannelPolicy :: GetChannelPolicyResponse -> TestTree
responseGetChannelPolicy =
  res
    "GetChannelPolicyResponse"
    "fixture/GetChannelPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannelPolicy)

responseGetOriginEndpoint :: GetOriginEndpointResponse -> TestTree
responseGetOriginEndpoint =
  res
    "GetOriginEndpointResponse"
    "fixture/GetOriginEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOriginEndpoint)

responseGetOriginEndpointPolicy :: GetOriginEndpointPolicyResponse -> TestTree
responseGetOriginEndpointPolicy =
  res
    "GetOriginEndpointPolicyResponse"
    "fixture/GetOriginEndpointPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOriginEndpointPolicy)

responseListChannelGroups :: ListChannelGroupsResponse -> TestTree
responseListChannelGroups =
  res
    "ListChannelGroupsResponse"
    "fixture/ListChannelGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelGroups)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannels)

responseListOriginEndpoints :: ListOriginEndpointsResponse -> TestTree
responseListOriginEndpoints =
  res
    "ListOriginEndpointsResponse"
    "fixture/ListOriginEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOriginEndpoints)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutChannelPolicy :: PutChannelPolicyResponse -> TestTree
responsePutChannelPolicy =
  res
    "PutChannelPolicyResponse"
    "fixture/PutChannelPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutChannelPolicy)

responsePutOriginEndpointPolicy :: PutOriginEndpointPolicyResponse -> TestTree
responsePutOriginEndpointPolicy =
  res
    "PutOriginEndpointPolicyResponse"
    "fixture/PutOriginEndpointPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutOriginEndpointPolicy)

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

responseUpdateChannelGroup :: UpdateChannelGroupResponse -> TestTree
responseUpdateChannelGroup =
  res
    "UpdateChannelGroupResponse"
    "fixture/UpdateChannelGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelGroup)

responseUpdateOriginEndpoint :: UpdateOriginEndpointResponse -> TestTree
responseUpdateOriginEndpoint =
  res
    "UpdateOriginEndpointResponse"
    "fixture/UpdateOriginEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOriginEndpoint)

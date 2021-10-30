{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.GlobalAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.GlobalAccelerator where

import qualified Data.Proxy as Proxy
import Network.AWS.GlobalAccelerator
import Test.AWS.Fixture
import Test.AWS.GlobalAccelerator.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDenyCustomRoutingTraffic $
--             newDenyCustomRoutingTraffic
--
--         , requestDescribeCustomRoutingListener $
--             newDescribeCustomRoutingListener
--
--         , requestCreateCustomRoutingEndpointGroup $
--             newCreateCustomRoutingEndpointGroup
--
--         , requestDescribeCustomRoutingAcceleratorAttributes $
--             newDescribeCustomRoutingAcceleratorAttributes
--
--         , requestDeleteCustomRoutingEndpointGroup $
--             newDeleteCustomRoutingEndpointGroup
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeAcceleratorAttributes $
--             newDescribeAcceleratorAttributes
--
--         , requestDeleteEndpointGroup $
--             newDeleteEndpointGroup
--
--         , requestUpdateEndpointGroup $
--             newUpdateEndpointGroup
--
--         , requestListCustomRoutingListeners $
--             newListCustomRoutingListeners
--
--         , requestDeleteCustomRoutingListener $
--             newDeleteCustomRoutingListener
--
--         , requestUpdateCustomRoutingListener $
--             newUpdateCustomRoutingListener
--
--         , requestCreateAccelerator $
--             newCreateAccelerator
--
--         , requestAllowCustomRoutingTraffic $
--             newAllowCustomRoutingTraffic
--
--         , requestWithdrawByoipCidr $
--             newWithdrawByoipCidr
--
--         , requestAdvertiseByoipCidr $
--             newAdvertiseByoipCidr
--
--         , requestDeleteAccelerator $
--             newDeleteAccelerator
--
--         , requestUpdateAccelerator $
--             newUpdateAccelerator
--
--         , requestListAccelerators $
--             newListAccelerators
--
--         , requestDescribeEndpointGroup $
--             newDescribeEndpointGroup
--
--         , requestUpdateAcceleratorAttributes $
--             newUpdateAcceleratorAttributes
--
--         , requestCreateCustomRoutingAccelerator $
--             newCreateCustomRoutingAccelerator
--
--         , requestListCustomRoutingPortMappingsByDestination $
--             newListCustomRoutingPortMappingsByDestination
--
--         , requestDeleteListener $
--             newDeleteListener
--
--         , requestUpdateListener $
--             newUpdateListener
--
--         , requestListListeners $
--             newListListeners
--
--         , requestListCustomRoutingEndpointGroups $
--             newListCustomRoutingEndpointGroups
--
--         , requestCreateListener $
--             newCreateListener
--
--         , requestDescribeAccelerator $
--             newDescribeAccelerator
--
--         , requestCreateCustomRoutingListener $
--             newCreateCustomRoutingListener
--
--         , requestDescribeCustomRoutingAccelerator $
--             newDescribeCustomRoutingAccelerator
--
--         , requestListEndpointGroups $
--             newListEndpointGroups
--
--         , requestProvisionByoipCidr $
--             newProvisionByoipCidr
--
--         , requestCreateEndpointGroup $
--             newCreateEndpointGroup
--
--         , requestListByoipCidrs $
--             newListByoipCidrs
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeListener $
--             newDescribeListener
--
--         , requestListCustomRoutingPortMappings $
--             newListCustomRoutingPortMappings
--
--         , requestAddCustomRoutingEndpoints $
--             newAddCustomRoutingEndpoints
--
--         , requestDescribeCustomRoutingEndpointGroup $
--             newDescribeCustomRoutingEndpointGroup
--
--         , requestUpdateCustomRoutingAcceleratorAttributes $
--             newUpdateCustomRoutingAcceleratorAttributes
--
--         , requestRemoveCustomRoutingEndpoints $
--             newRemoveCustomRoutingEndpoints
--
--         , requestUpdateCustomRoutingAccelerator $
--             newUpdateCustomRoutingAccelerator
--
--         , requestDeleteCustomRoutingAccelerator $
--             newDeleteCustomRoutingAccelerator
--
--         , requestListCustomRoutingAccelerators $
--             newListCustomRoutingAccelerators
--
--         , requestDeprovisionByoipCidr $
--             newDeprovisionByoipCidr
--
--           ]

--     , testGroup "response"
--         [ responseDenyCustomRoutingTraffic $
--             newDenyCustomRoutingTrafficResponse
--
--         , responseDescribeCustomRoutingListener $
--             newDescribeCustomRoutingListenerResponse
--
--         , responseCreateCustomRoutingEndpointGroup $
--             newCreateCustomRoutingEndpointGroupResponse
--
--         , responseDescribeCustomRoutingAcceleratorAttributes $
--             newDescribeCustomRoutingAcceleratorAttributesResponse
--
--         , responseDeleteCustomRoutingEndpointGroup $
--             newDeleteCustomRoutingEndpointGroupResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeAcceleratorAttributes $
--             newDescribeAcceleratorAttributesResponse
--
--         , responseDeleteEndpointGroup $
--             newDeleteEndpointGroupResponse
--
--         , responseUpdateEndpointGroup $
--             newUpdateEndpointGroupResponse
--
--         , responseListCustomRoutingListeners $
--             newListCustomRoutingListenersResponse
--
--         , responseDeleteCustomRoutingListener $
--             newDeleteCustomRoutingListenerResponse
--
--         , responseUpdateCustomRoutingListener $
--             newUpdateCustomRoutingListenerResponse
--
--         , responseCreateAccelerator $
--             newCreateAcceleratorResponse
--
--         , responseAllowCustomRoutingTraffic $
--             newAllowCustomRoutingTrafficResponse
--
--         , responseWithdrawByoipCidr $
--             newWithdrawByoipCidrResponse
--
--         , responseAdvertiseByoipCidr $
--             newAdvertiseByoipCidrResponse
--
--         , responseDeleteAccelerator $
--             newDeleteAcceleratorResponse
--
--         , responseUpdateAccelerator $
--             newUpdateAcceleratorResponse
--
--         , responseListAccelerators $
--             newListAcceleratorsResponse
--
--         , responseDescribeEndpointGroup $
--             newDescribeEndpointGroupResponse
--
--         , responseUpdateAcceleratorAttributes $
--             newUpdateAcceleratorAttributesResponse
--
--         , responseCreateCustomRoutingAccelerator $
--             newCreateCustomRoutingAcceleratorResponse
--
--         , responseListCustomRoutingPortMappingsByDestination $
--             newListCustomRoutingPortMappingsByDestinationResponse
--
--         , responseDeleteListener $
--             newDeleteListenerResponse
--
--         , responseUpdateListener $
--             newUpdateListenerResponse
--
--         , responseListListeners $
--             newListListenersResponse
--
--         , responseListCustomRoutingEndpointGroups $
--             newListCustomRoutingEndpointGroupsResponse
--
--         , responseCreateListener $
--             newCreateListenerResponse
--
--         , responseDescribeAccelerator $
--             newDescribeAcceleratorResponse
--
--         , responseCreateCustomRoutingListener $
--             newCreateCustomRoutingListenerResponse
--
--         , responseDescribeCustomRoutingAccelerator $
--             newDescribeCustomRoutingAcceleratorResponse
--
--         , responseListEndpointGroups $
--             newListEndpointGroupsResponse
--
--         , responseProvisionByoipCidr $
--             newProvisionByoipCidrResponse
--
--         , responseCreateEndpointGroup $
--             newCreateEndpointGroupResponse
--
--         , responseListByoipCidrs $
--             newListByoipCidrsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeListener $
--             newDescribeListenerResponse
--
--         , responseListCustomRoutingPortMappings $
--             newListCustomRoutingPortMappingsResponse
--
--         , responseAddCustomRoutingEndpoints $
--             newAddCustomRoutingEndpointsResponse
--
--         , responseDescribeCustomRoutingEndpointGroup $
--             newDescribeCustomRoutingEndpointGroupResponse
--
--         , responseUpdateCustomRoutingAcceleratorAttributes $
--             newUpdateCustomRoutingAcceleratorAttributesResponse
--
--         , responseRemoveCustomRoutingEndpoints $
--             newRemoveCustomRoutingEndpointsResponse
--
--         , responseUpdateCustomRoutingAccelerator $
--             newUpdateCustomRoutingAcceleratorResponse
--
--         , responseDeleteCustomRoutingAccelerator $
--             newDeleteCustomRoutingAcceleratorResponse
--
--         , responseListCustomRoutingAccelerators $
--             newListCustomRoutingAcceleratorsResponse
--
--         , responseDeprovisionByoipCidr $
--             newDeprovisionByoipCidrResponse
--
--           ]
--     ]

-- Requests

requestDenyCustomRoutingTraffic :: DenyCustomRoutingTraffic -> TestTree
requestDenyCustomRoutingTraffic =
  req
    "DenyCustomRoutingTraffic"
    "fixture/DenyCustomRoutingTraffic.yaml"

requestDescribeCustomRoutingListener :: DescribeCustomRoutingListener -> TestTree
requestDescribeCustomRoutingListener =
  req
    "DescribeCustomRoutingListener"
    "fixture/DescribeCustomRoutingListener.yaml"

requestCreateCustomRoutingEndpointGroup :: CreateCustomRoutingEndpointGroup -> TestTree
requestCreateCustomRoutingEndpointGroup =
  req
    "CreateCustomRoutingEndpointGroup"
    "fixture/CreateCustomRoutingEndpointGroup.yaml"

requestDescribeCustomRoutingAcceleratorAttributes :: DescribeCustomRoutingAcceleratorAttributes -> TestTree
requestDescribeCustomRoutingAcceleratorAttributes =
  req
    "DescribeCustomRoutingAcceleratorAttributes"
    "fixture/DescribeCustomRoutingAcceleratorAttributes.yaml"

requestDeleteCustomRoutingEndpointGroup :: DeleteCustomRoutingEndpointGroup -> TestTree
requestDeleteCustomRoutingEndpointGroup =
  req
    "DeleteCustomRoutingEndpointGroup"
    "fixture/DeleteCustomRoutingEndpointGroup.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeAcceleratorAttributes :: DescribeAcceleratorAttributes -> TestTree
requestDescribeAcceleratorAttributes =
  req
    "DescribeAcceleratorAttributes"
    "fixture/DescribeAcceleratorAttributes.yaml"

requestDeleteEndpointGroup :: DeleteEndpointGroup -> TestTree
requestDeleteEndpointGroup =
  req
    "DeleteEndpointGroup"
    "fixture/DeleteEndpointGroup.yaml"

requestUpdateEndpointGroup :: UpdateEndpointGroup -> TestTree
requestUpdateEndpointGroup =
  req
    "UpdateEndpointGroup"
    "fixture/UpdateEndpointGroup.yaml"

requestListCustomRoutingListeners :: ListCustomRoutingListeners -> TestTree
requestListCustomRoutingListeners =
  req
    "ListCustomRoutingListeners"
    "fixture/ListCustomRoutingListeners.yaml"

requestDeleteCustomRoutingListener :: DeleteCustomRoutingListener -> TestTree
requestDeleteCustomRoutingListener =
  req
    "DeleteCustomRoutingListener"
    "fixture/DeleteCustomRoutingListener.yaml"

requestUpdateCustomRoutingListener :: UpdateCustomRoutingListener -> TestTree
requestUpdateCustomRoutingListener =
  req
    "UpdateCustomRoutingListener"
    "fixture/UpdateCustomRoutingListener.yaml"

requestCreateAccelerator :: CreateAccelerator -> TestTree
requestCreateAccelerator =
  req
    "CreateAccelerator"
    "fixture/CreateAccelerator.yaml"

requestAllowCustomRoutingTraffic :: AllowCustomRoutingTraffic -> TestTree
requestAllowCustomRoutingTraffic =
  req
    "AllowCustomRoutingTraffic"
    "fixture/AllowCustomRoutingTraffic.yaml"

requestWithdrawByoipCidr :: WithdrawByoipCidr -> TestTree
requestWithdrawByoipCidr =
  req
    "WithdrawByoipCidr"
    "fixture/WithdrawByoipCidr.yaml"

requestAdvertiseByoipCidr :: AdvertiseByoipCidr -> TestTree
requestAdvertiseByoipCidr =
  req
    "AdvertiseByoipCidr"
    "fixture/AdvertiseByoipCidr.yaml"

requestDeleteAccelerator :: DeleteAccelerator -> TestTree
requestDeleteAccelerator =
  req
    "DeleteAccelerator"
    "fixture/DeleteAccelerator.yaml"

requestUpdateAccelerator :: UpdateAccelerator -> TestTree
requestUpdateAccelerator =
  req
    "UpdateAccelerator"
    "fixture/UpdateAccelerator.yaml"

requestListAccelerators :: ListAccelerators -> TestTree
requestListAccelerators =
  req
    "ListAccelerators"
    "fixture/ListAccelerators.yaml"

requestDescribeEndpointGroup :: DescribeEndpointGroup -> TestTree
requestDescribeEndpointGroup =
  req
    "DescribeEndpointGroup"
    "fixture/DescribeEndpointGroup.yaml"

requestUpdateAcceleratorAttributes :: UpdateAcceleratorAttributes -> TestTree
requestUpdateAcceleratorAttributes =
  req
    "UpdateAcceleratorAttributes"
    "fixture/UpdateAcceleratorAttributes.yaml"

requestCreateCustomRoutingAccelerator :: CreateCustomRoutingAccelerator -> TestTree
requestCreateCustomRoutingAccelerator =
  req
    "CreateCustomRoutingAccelerator"
    "fixture/CreateCustomRoutingAccelerator.yaml"

requestListCustomRoutingPortMappingsByDestination :: ListCustomRoutingPortMappingsByDestination -> TestTree
requestListCustomRoutingPortMappingsByDestination =
  req
    "ListCustomRoutingPortMappingsByDestination"
    "fixture/ListCustomRoutingPortMappingsByDestination.yaml"

requestDeleteListener :: DeleteListener -> TestTree
requestDeleteListener =
  req
    "DeleteListener"
    "fixture/DeleteListener.yaml"

requestUpdateListener :: UpdateListener -> TestTree
requestUpdateListener =
  req
    "UpdateListener"
    "fixture/UpdateListener.yaml"

requestListListeners :: ListListeners -> TestTree
requestListListeners =
  req
    "ListListeners"
    "fixture/ListListeners.yaml"

requestListCustomRoutingEndpointGroups :: ListCustomRoutingEndpointGroups -> TestTree
requestListCustomRoutingEndpointGroups =
  req
    "ListCustomRoutingEndpointGroups"
    "fixture/ListCustomRoutingEndpointGroups.yaml"

requestCreateListener :: CreateListener -> TestTree
requestCreateListener =
  req
    "CreateListener"
    "fixture/CreateListener.yaml"

requestDescribeAccelerator :: DescribeAccelerator -> TestTree
requestDescribeAccelerator =
  req
    "DescribeAccelerator"
    "fixture/DescribeAccelerator.yaml"

requestCreateCustomRoutingListener :: CreateCustomRoutingListener -> TestTree
requestCreateCustomRoutingListener =
  req
    "CreateCustomRoutingListener"
    "fixture/CreateCustomRoutingListener.yaml"

requestDescribeCustomRoutingAccelerator :: DescribeCustomRoutingAccelerator -> TestTree
requestDescribeCustomRoutingAccelerator =
  req
    "DescribeCustomRoutingAccelerator"
    "fixture/DescribeCustomRoutingAccelerator.yaml"

requestListEndpointGroups :: ListEndpointGroups -> TestTree
requestListEndpointGroups =
  req
    "ListEndpointGroups"
    "fixture/ListEndpointGroups.yaml"

requestProvisionByoipCidr :: ProvisionByoipCidr -> TestTree
requestProvisionByoipCidr =
  req
    "ProvisionByoipCidr"
    "fixture/ProvisionByoipCidr.yaml"

requestCreateEndpointGroup :: CreateEndpointGroup -> TestTree
requestCreateEndpointGroup =
  req
    "CreateEndpointGroup"
    "fixture/CreateEndpointGroup.yaml"

requestListByoipCidrs :: ListByoipCidrs -> TestTree
requestListByoipCidrs =
  req
    "ListByoipCidrs"
    "fixture/ListByoipCidrs.yaml"

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

requestDescribeListener :: DescribeListener -> TestTree
requestDescribeListener =
  req
    "DescribeListener"
    "fixture/DescribeListener.yaml"

requestListCustomRoutingPortMappings :: ListCustomRoutingPortMappings -> TestTree
requestListCustomRoutingPortMappings =
  req
    "ListCustomRoutingPortMappings"
    "fixture/ListCustomRoutingPortMappings.yaml"

requestAddCustomRoutingEndpoints :: AddCustomRoutingEndpoints -> TestTree
requestAddCustomRoutingEndpoints =
  req
    "AddCustomRoutingEndpoints"
    "fixture/AddCustomRoutingEndpoints.yaml"

requestDescribeCustomRoutingEndpointGroup :: DescribeCustomRoutingEndpointGroup -> TestTree
requestDescribeCustomRoutingEndpointGroup =
  req
    "DescribeCustomRoutingEndpointGroup"
    "fixture/DescribeCustomRoutingEndpointGroup.yaml"

requestUpdateCustomRoutingAcceleratorAttributes :: UpdateCustomRoutingAcceleratorAttributes -> TestTree
requestUpdateCustomRoutingAcceleratorAttributes =
  req
    "UpdateCustomRoutingAcceleratorAttributes"
    "fixture/UpdateCustomRoutingAcceleratorAttributes.yaml"

requestRemoveCustomRoutingEndpoints :: RemoveCustomRoutingEndpoints -> TestTree
requestRemoveCustomRoutingEndpoints =
  req
    "RemoveCustomRoutingEndpoints"
    "fixture/RemoveCustomRoutingEndpoints.yaml"

requestUpdateCustomRoutingAccelerator :: UpdateCustomRoutingAccelerator -> TestTree
requestUpdateCustomRoutingAccelerator =
  req
    "UpdateCustomRoutingAccelerator"
    "fixture/UpdateCustomRoutingAccelerator.yaml"

requestDeleteCustomRoutingAccelerator :: DeleteCustomRoutingAccelerator -> TestTree
requestDeleteCustomRoutingAccelerator =
  req
    "DeleteCustomRoutingAccelerator"
    "fixture/DeleteCustomRoutingAccelerator.yaml"

requestListCustomRoutingAccelerators :: ListCustomRoutingAccelerators -> TestTree
requestListCustomRoutingAccelerators =
  req
    "ListCustomRoutingAccelerators"
    "fixture/ListCustomRoutingAccelerators.yaml"

requestDeprovisionByoipCidr :: DeprovisionByoipCidr -> TestTree
requestDeprovisionByoipCidr =
  req
    "DeprovisionByoipCidr"
    "fixture/DeprovisionByoipCidr.yaml"

-- Responses

responseDenyCustomRoutingTraffic :: DenyCustomRoutingTrafficResponse -> TestTree
responseDenyCustomRoutingTraffic =
  res
    "DenyCustomRoutingTrafficResponse"
    "fixture/DenyCustomRoutingTrafficResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DenyCustomRoutingTraffic)

responseDescribeCustomRoutingListener :: DescribeCustomRoutingListenerResponse -> TestTree
responseDescribeCustomRoutingListener =
  res
    "DescribeCustomRoutingListenerResponse"
    "fixture/DescribeCustomRoutingListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomRoutingListener)

responseCreateCustomRoutingEndpointGroup :: CreateCustomRoutingEndpointGroupResponse -> TestTree
responseCreateCustomRoutingEndpointGroup =
  res
    "CreateCustomRoutingEndpointGroupResponse"
    "fixture/CreateCustomRoutingEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomRoutingEndpointGroup)

responseDescribeCustomRoutingAcceleratorAttributes :: DescribeCustomRoutingAcceleratorAttributesResponse -> TestTree
responseDescribeCustomRoutingAcceleratorAttributes =
  res
    "DescribeCustomRoutingAcceleratorAttributesResponse"
    "fixture/DescribeCustomRoutingAcceleratorAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomRoutingAcceleratorAttributes)

responseDeleteCustomRoutingEndpointGroup :: DeleteCustomRoutingEndpointGroupResponse -> TestTree
responseDeleteCustomRoutingEndpointGroup =
  res
    "DeleteCustomRoutingEndpointGroupResponse"
    "fixture/DeleteCustomRoutingEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomRoutingEndpointGroup)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDescribeAcceleratorAttributes :: DescribeAcceleratorAttributesResponse -> TestTree
responseDescribeAcceleratorAttributes =
  res
    "DescribeAcceleratorAttributesResponse"
    "fixture/DescribeAcceleratorAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAcceleratorAttributes)

responseDeleteEndpointGroup :: DeleteEndpointGroupResponse -> TestTree
responseDeleteEndpointGroup =
  res
    "DeleteEndpointGroupResponse"
    "fixture/DeleteEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpointGroup)

responseUpdateEndpointGroup :: UpdateEndpointGroupResponse -> TestTree
responseUpdateEndpointGroup =
  res
    "UpdateEndpointGroupResponse"
    "fixture/UpdateEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpointGroup)

responseListCustomRoutingListeners :: ListCustomRoutingListenersResponse -> TestTree
responseListCustomRoutingListeners =
  res
    "ListCustomRoutingListenersResponse"
    "fixture/ListCustomRoutingListenersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomRoutingListeners)

responseDeleteCustomRoutingListener :: DeleteCustomRoutingListenerResponse -> TestTree
responseDeleteCustomRoutingListener =
  res
    "DeleteCustomRoutingListenerResponse"
    "fixture/DeleteCustomRoutingListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomRoutingListener)

responseUpdateCustomRoutingListener :: UpdateCustomRoutingListenerResponse -> TestTree
responseUpdateCustomRoutingListener =
  res
    "UpdateCustomRoutingListenerResponse"
    "fixture/UpdateCustomRoutingListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomRoutingListener)

responseCreateAccelerator :: CreateAcceleratorResponse -> TestTree
responseCreateAccelerator =
  res
    "CreateAcceleratorResponse"
    "fixture/CreateAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccelerator)

responseAllowCustomRoutingTraffic :: AllowCustomRoutingTrafficResponse -> TestTree
responseAllowCustomRoutingTraffic =
  res
    "AllowCustomRoutingTrafficResponse"
    "fixture/AllowCustomRoutingTrafficResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllowCustomRoutingTraffic)

responseWithdrawByoipCidr :: WithdrawByoipCidrResponse -> TestTree
responseWithdrawByoipCidr =
  res
    "WithdrawByoipCidrResponse"
    "fixture/WithdrawByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy WithdrawByoipCidr)

responseAdvertiseByoipCidr :: AdvertiseByoipCidrResponse -> TestTree
responseAdvertiseByoipCidr =
  res
    "AdvertiseByoipCidrResponse"
    "fixture/AdvertiseByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdvertiseByoipCidr)

responseDeleteAccelerator :: DeleteAcceleratorResponse -> TestTree
responseDeleteAccelerator =
  res
    "DeleteAcceleratorResponse"
    "fixture/DeleteAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccelerator)

responseUpdateAccelerator :: UpdateAcceleratorResponse -> TestTree
responseUpdateAccelerator =
  res
    "UpdateAcceleratorResponse"
    "fixture/UpdateAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccelerator)

responseListAccelerators :: ListAcceleratorsResponse -> TestTree
responseListAccelerators =
  res
    "ListAcceleratorsResponse"
    "fixture/ListAcceleratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccelerators)

responseDescribeEndpointGroup :: DescribeEndpointGroupResponse -> TestTree
responseDescribeEndpointGroup =
  res
    "DescribeEndpointGroupResponse"
    "fixture/DescribeEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpointGroup)

responseUpdateAcceleratorAttributes :: UpdateAcceleratorAttributesResponse -> TestTree
responseUpdateAcceleratorAttributes =
  res
    "UpdateAcceleratorAttributesResponse"
    "fixture/UpdateAcceleratorAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAcceleratorAttributes)

responseCreateCustomRoutingAccelerator :: CreateCustomRoutingAcceleratorResponse -> TestTree
responseCreateCustomRoutingAccelerator =
  res
    "CreateCustomRoutingAcceleratorResponse"
    "fixture/CreateCustomRoutingAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomRoutingAccelerator)

responseListCustomRoutingPortMappingsByDestination :: ListCustomRoutingPortMappingsByDestinationResponse -> TestTree
responseListCustomRoutingPortMappingsByDestination =
  res
    "ListCustomRoutingPortMappingsByDestinationResponse"
    "fixture/ListCustomRoutingPortMappingsByDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomRoutingPortMappingsByDestination)

responseDeleteListener :: DeleteListenerResponse -> TestTree
responseDeleteListener =
  res
    "DeleteListenerResponse"
    "fixture/DeleteListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteListener)

responseUpdateListener :: UpdateListenerResponse -> TestTree
responseUpdateListener =
  res
    "UpdateListenerResponse"
    "fixture/UpdateListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateListener)

responseListListeners :: ListListenersResponse -> TestTree
responseListListeners =
  res
    "ListListenersResponse"
    "fixture/ListListenersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListListeners)

responseListCustomRoutingEndpointGroups :: ListCustomRoutingEndpointGroupsResponse -> TestTree
responseListCustomRoutingEndpointGroups =
  res
    "ListCustomRoutingEndpointGroupsResponse"
    "fixture/ListCustomRoutingEndpointGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomRoutingEndpointGroups)

responseCreateListener :: CreateListenerResponse -> TestTree
responseCreateListener =
  res
    "CreateListenerResponse"
    "fixture/CreateListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateListener)

responseDescribeAccelerator :: DescribeAcceleratorResponse -> TestTree
responseDescribeAccelerator =
  res
    "DescribeAcceleratorResponse"
    "fixture/DescribeAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccelerator)

responseCreateCustomRoutingListener :: CreateCustomRoutingListenerResponse -> TestTree
responseCreateCustomRoutingListener =
  res
    "CreateCustomRoutingListenerResponse"
    "fixture/CreateCustomRoutingListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomRoutingListener)

responseDescribeCustomRoutingAccelerator :: DescribeCustomRoutingAcceleratorResponse -> TestTree
responseDescribeCustomRoutingAccelerator =
  res
    "DescribeCustomRoutingAcceleratorResponse"
    "fixture/DescribeCustomRoutingAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomRoutingAccelerator)

responseListEndpointGroups :: ListEndpointGroupsResponse -> TestTree
responseListEndpointGroups =
  res
    "ListEndpointGroupsResponse"
    "fixture/ListEndpointGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpointGroups)

responseProvisionByoipCidr :: ProvisionByoipCidrResponse -> TestTree
responseProvisionByoipCidr =
  res
    "ProvisionByoipCidrResponse"
    "fixture/ProvisionByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionByoipCidr)

responseCreateEndpointGroup :: CreateEndpointGroupResponse -> TestTree
responseCreateEndpointGroup =
  res
    "CreateEndpointGroupResponse"
    "fixture/CreateEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpointGroup)

responseListByoipCidrs :: ListByoipCidrsResponse -> TestTree
responseListByoipCidrs =
  res
    "ListByoipCidrsResponse"
    "fixture/ListByoipCidrsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListByoipCidrs)

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

responseDescribeListener :: DescribeListenerResponse -> TestTree
responseDescribeListener =
  res
    "DescribeListenerResponse"
    "fixture/DescribeListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeListener)

responseListCustomRoutingPortMappings :: ListCustomRoutingPortMappingsResponse -> TestTree
responseListCustomRoutingPortMappings =
  res
    "ListCustomRoutingPortMappingsResponse"
    "fixture/ListCustomRoutingPortMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomRoutingPortMappings)

responseAddCustomRoutingEndpoints :: AddCustomRoutingEndpointsResponse -> TestTree
responseAddCustomRoutingEndpoints =
  res
    "AddCustomRoutingEndpointsResponse"
    "fixture/AddCustomRoutingEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddCustomRoutingEndpoints)

responseDescribeCustomRoutingEndpointGroup :: DescribeCustomRoutingEndpointGroupResponse -> TestTree
responseDescribeCustomRoutingEndpointGroup =
  res
    "DescribeCustomRoutingEndpointGroupResponse"
    "fixture/DescribeCustomRoutingEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomRoutingEndpointGroup)

responseUpdateCustomRoutingAcceleratorAttributes :: UpdateCustomRoutingAcceleratorAttributesResponse -> TestTree
responseUpdateCustomRoutingAcceleratorAttributes =
  res
    "UpdateCustomRoutingAcceleratorAttributesResponse"
    "fixture/UpdateCustomRoutingAcceleratorAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomRoutingAcceleratorAttributes)

responseRemoveCustomRoutingEndpoints :: RemoveCustomRoutingEndpointsResponse -> TestTree
responseRemoveCustomRoutingEndpoints =
  res
    "RemoveCustomRoutingEndpointsResponse"
    "fixture/RemoveCustomRoutingEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveCustomRoutingEndpoints)

responseUpdateCustomRoutingAccelerator :: UpdateCustomRoutingAcceleratorResponse -> TestTree
responseUpdateCustomRoutingAccelerator =
  res
    "UpdateCustomRoutingAcceleratorResponse"
    "fixture/UpdateCustomRoutingAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomRoutingAccelerator)

responseDeleteCustomRoutingAccelerator :: DeleteCustomRoutingAcceleratorResponse -> TestTree
responseDeleteCustomRoutingAccelerator =
  res
    "DeleteCustomRoutingAcceleratorResponse"
    "fixture/DeleteCustomRoutingAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomRoutingAccelerator)

responseListCustomRoutingAccelerators :: ListCustomRoutingAcceleratorsResponse -> TestTree
responseListCustomRoutingAccelerators =
  res
    "ListCustomRoutingAcceleratorsResponse"
    "fixture/ListCustomRoutingAcceleratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomRoutingAccelerators)

responseDeprovisionByoipCidr :: DeprovisionByoipCidrResponse -> TestTree
responseDeprovisionByoipCidr =
  res
    "DeprovisionByoipCidrResponse"
    "fixture/DeprovisionByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprovisionByoipCidr)

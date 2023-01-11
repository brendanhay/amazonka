{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.GlobalAccelerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.GlobalAccelerator where

import Amazonka.GlobalAccelerator
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.GlobalAccelerator.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddCustomRoutingEndpoints $
--             newAddCustomRoutingEndpoints
--
--         , requestAddEndpoints $
--             newAddEndpoints
--
--         , requestAdvertiseByoipCidr $
--             newAdvertiseByoipCidr
--
--         , requestAllowCustomRoutingTraffic $
--             newAllowCustomRoutingTraffic
--
--         , requestCreateAccelerator $
--             newCreateAccelerator
--
--         , requestCreateCustomRoutingAccelerator $
--             newCreateCustomRoutingAccelerator
--
--         , requestCreateCustomRoutingEndpointGroup $
--             newCreateCustomRoutingEndpointGroup
--
--         , requestCreateCustomRoutingListener $
--             newCreateCustomRoutingListener
--
--         , requestCreateEndpointGroup $
--             newCreateEndpointGroup
--
--         , requestCreateListener $
--             newCreateListener
--
--         , requestDeleteAccelerator $
--             newDeleteAccelerator
--
--         , requestDeleteCustomRoutingAccelerator $
--             newDeleteCustomRoutingAccelerator
--
--         , requestDeleteCustomRoutingEndpointGroup $
--             newDeleteCustomRoutingEndpointGroup
--
--         , requestDeleteCustomRoutingListener $
--             newDeleteCustomRoutingListener
--
--         , requestDeleteEndpointGroup $
--             newDeleteEndpointGroup
--
--         , requestDeleteListener $
--             newDeleteListener
--
--         , requestDenyCustomRoutingTraffic $
--             newDenyCustomRoutingTraffic
--
--         , requestDeprovisionByoipCidr $
--             newDeprovisionByoipCidr
--
--         , requestDescribeAccelerator $
--             newDescribeAccelerator
--
--         , requestDescribeAcceleratorAttributes $
--             newDescribeAcceleratorAttributes
--
--         , requestDescribeCustomRoutingAccelerator $
--             newDescribeCustomRoutingAccelerator
--
--         , requestDescribeCustomRoutingAcceleratorAttributes $
--             newDescribeCustomRoutingAcceleratorAttributes
--
--         , requestDescribeCustomRoutingEndpointGroup $
--             newDescribeCustomRoutingEndpointGroup
--
--         , requestDescribeCustomRoutingListener $
--             newDescribeCustomRoutingListener
--
--         , requestDescribeEndpointGroup $
--             newDescribeEndpointGroup
--
--         , requestDescribeListener $
--             newDescribeListener
--
--         , requestListAccelerators $
--             newListAccelerators
--
--         , requestListByoipCidrs $
--             newListByoipCidrs
--
--         , requestListCustomRoutingAccelerators $
--             newListCustomRoutingAccelerators
--
--         , requestListCustomRoutingEndpointGroups $
--             newListCustomRoutingEndpointGroups
--
--         , requestListCustomRoutingListeners $
--             newListCustomRoutingListeners
--
--         , requestListCustomRoutingPortMappings $
--             newListCustomRoutingPortMappings
--
--         , requestListCustomRoutingPortMappingsByDestination $
--             newListCustomRoutingPortMappingsByDestination
--
--         , requestListEndpointGroups $
--             newListEndpointGroups
--
--         , requestListListeners $
--             newListListeners
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestProvisionByoipCidr $
--             newProvisionByoipCidr
--
--         , requestRemoveCustomRoutingEndpoints $
--             newRemoveCustomRoutingEndpoints
--
--         , requestRemoveEndpoints $
--             newRemoveEndpoints
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAccelerator $
--             newUpdateAccelerator
--
--         , requestUpdateAcceleratorAttributes $
--             newUpdateAcceleratorAttributes
--
--         , requestUpdateCustomRoutingAccelerator $
--             newUpdateCustomRoutingAccelerator
--
--         , requestUpdateCustomRoutingAcceleratorAttributes $
--             newUpdateCustomRoutingAcceleratorAttributes
--
--         , requestUpdateCustomRoutingListener $
--             newUpdateCustomRoutingListener
--
--         , requestUpdateEndpointGroup $
--             newUpdateEndpointGroup
--
--         , requestUpdateListener $
--             newUpdateListener
--
--         , requestWithdrawByoipCidr $
--             newWithdrawByoipCidr
--
--           ]

--     , testGroup "response"
--         [ responseAddCustomRoutingEndpoints $
--             newAddCustomRoutingEndpointsResponse
--
--         , responseAddEndpoints $
--             newAddEndpointsResponse
--
--         , responseAdvertiseByoipCidr $
--             newAdvertiseByoipCidrResponse
--
--         , responseAllowCustomRoutingTraffic $
--             newAllowCustomRoutingTrafficResponse
--
--         , responseCreateAccelerator $
--             newCreateAcceleratorResponse
--
--         , responseCreateCustomRoutingAccelerator $
--             newCreateCustomRoutingAcceleratorResponse
--
--         , responseCreateCustomRoutingEndpointGroup $
--             newCreateCustomRoutingEndpointGroupResponse
--
--         , responseCreateCustomRoutingListener $
--             newCreateCustomRoutingListenerResponse
--
--         , responseCreateEndpointGroup $
--             newCreateEndpointGroupResponse
--
--         , responseCreateListener $
--             newCreateListenerResponse
--
--         , responseDeleteAccelerator $
--             newDeleteAcceleratorResponse
--
--         , responseDeleteCustomRoutingAccelerator $
--             newDeleteCustomRoutingAcceleratorResponse
--
--         , responseDeleteCustomRoutingEndpointGroup $
--             newDeleteCustomRoutingEndpointGroupResponse
--
--         , responseDeleteCustomRoutingListener $
--             newDeleteCustomRoutingListenerResponse
--
--         , responseDeleteEndpointGroup $
--             newDeleteEndpointGroupResponse
--
--         , responseDeleteListener $
--             newDeleteListenerResponse
--
--         , responseDenyCustomRoutingTraffic $
--             newDenyCustomRoutingTrafficResponse
--
--         , responseDeprovisionByoipCidr $
--             newDeprovisionByoipCidrResponse
--
--         , responseDescribeAccelerator $
--             newDescribeAcceleratorResponse
--
--         , responseDescribeAcceleratorAttributes $
--             newDescribeAcceleratorAttributesResponse
--
--         , responseDescribeCustomRoutingAccelerator $
--             newDescribeCustomRoutingAcceleratorResponse
--
--         , responseDescribeCustomRoutingAcceleratorAttributes $
--             newDescribeCustomRoutingAcceleratorAttributesResponse
--
--         , responseDescribeCustomRoutingEndpointGroup $
--             newDescribeCustomRoutingEndpointGroupResponse
--
--         , responseDescribeCustomRoutingListener $
--             newDescribeCustomRoutingListenerResponse
--
--         , responseDescribeEndpointGroup $
--             newDescribeEndpointGroupResponse
--
--         , responseDescribeListener $
--             newDescribeListenerResponse
--
--         , responseListAccelerators $
--             newListAcceleratorsResponse
--
--         , responseListByoipCidrs $
--             newListByoipCidrsResponse
--
--         , responseListCustomRoutingAccelerators $
--             newListCustomRoutingAcceleratorsResponse
--
--         , responseListCustomRoutingEndpointGroups $
--             newListCustomRoutingEndpointGroupsResponse
--
--         , responseListCustomRoutingListeners $
--             newListCustomRoutingListenersResponse
--
--         , responseListCustomRoutingPortMappings $
--             newListCustomRoutingPortMappingsResponse
--
--         , responseListCustomRoutingPortMappingsByDestination $
--             newListCustomRoutingPortMappingsByDestinationResponse
--
--         , responseListEndpointGroups $
--             newListEndpointGroupsResponse
--
--         , responseListListeners $
--             newListListenersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseProvisionByoipCidr $
--             newProvisionByoipCidrResponse
--
--         , responseRemoveCustomRoutingEndpoints $
--             newRemoveCustomRoutingEndpointsResponse
--
--         , responseRemoveEndpoints $
--             newRemoveEndpointsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAccelerator $
--             newUpdateAcceleratorResponse
--
--         , responseUpdateAcceleratorAttributes $
--             newUpdateAcceleratorAttributesResponse
--
--         , responseUpdateCustomRoutingAccelerator $
--             newUpdateCustomRoutingAcceleratorResponse
--
--         , responseUpdateCustomRoutingAcceleratorAttributes $
--             newUpdateCustomRoutingAcceleratorAttributesResponse
--
--         , responseUpdateCustomRoutingListener $
--             newUpdateCustomRoutingListenerResponse
--
--         , responseUpdateEndpointGroup $
--             newUpdateEndpointGroupResponse
--
--         , responseUpdateListener $
--             newUpdateListenerResponse
--
--         , responseWithdrawByoipCidr $
--             newWithdrawByoipCidrResponse
--
--           ]
--     ]

-- Requests

requestAddCustomRoutingEndpoints :: AddCustomRoutingEndpoints -> TestTree
requestAddCustomRoutingEndpoints =
  req
    "AddCustomRoutingEndpoints"
    "fixture/AddCustomRoutingEndpoints.yaml"

requestAddEndpoints :: AddEndpoints -> TestTree
requestAddEndpoints =
  req
    "AddEndpoints"
    "fixture/AddEndpoints.yaml"

requestAdvertiseByoipCidr :: AdvertiseByoipCidr -> TestTree
requestAdvertiseByoipCidr =
  req
    "AdvertiseByoipCidr"
    "fixture/AdvertiseByoipCidr.yaml"

requestAllowCustomRoutingTraffic :: AllowCustomRoutingTraffic -> TestTree
requestAllowCustomRoutingTraffic =
  req
    "AllowCustomRoutingTraffic"
    "fixture/AllowCustomRoutingTraffic.yaml"

requestCreateAccelerator :: CreateAccelerator -> TestTree
requestCreateAccelerator =
  req
    "CreateAccelerator"
    "fixture/CreateAccelerator.yaml"

requestCreateCustomRoutingAccelerator :: CreateCustomRoutingAccelerator -> TestTree
requestCreateCustomRoutingAccelerator =
  req
    "CreateCustomRoutingAccelerator"
    "fixture/CreateCustomRoutingAccelerator.yaml"

requestCreateCustomRoutingEndpointGroup :: CreateCustomRoutingEndpointGroup -> TestTree
requestCreateCustomRoutingEndpointGroup =
  req
    "CreateCustomRoutingEndpointGroup"
    "fixture/CreateCustomRoutingEndpointGroup.yaml"

requestCreateCustomRoutingListener :: CreateCustomRoutingListener -> TestTree
requestCreateCustomRoutingListener =
  req
    "CreateCustomRoutingListener"
    "fixture/CreateCustomRoutingListener.yaml"

requestCreateEndpointGroup :: CreateEndpointGroup -> TestTree
requestCreateEndpointGroup =
  req
    "CreateEndpointGroup"
    "fixture/CreateEndpointGroup.yaml"

requestCreateListener :: CreateListener -> TestTree
requestCreateListener =
  req
    "CreateListener"
    "fixture/CreateListener.yaml"

requestDeleteAccelerator :: DeleteAccelerator -> TestTree
requestDeleteAccelerator =
  req
    "DeleteAccelerator"
    "fixture/DeleteAccelerator.yaml"

requestDeleteCustomRoutingAccelerator :: DeleteCustomRoutingAccelerator -> TestTree
requestDeleteCustomRoutingAccelerator =
  req
    "DeleteCustomRoutingAccelerator"
    "fixture/DeleteCustomRoutingAccelerator.yaml"

requestDeleteCustomRoutingEndpointGroup :: DeleteCustomRoutingEndpointGroup -> TestTree
requestDeleteCustomRoutingEndpointGroup =
  req
    "DeleteCustomRoutingEndpointGroup"
    "fixture/DeleteCustomRoutingEndpointGroup.yaml"

requestDeleteCustomRoutingListener :: DeleteCustomRoutingListener -> TestTree
requestDeleteCustomRoutingListener =
  req
    "DeleteCustomRoutingListener"
    "fixture/DeleteCustomRoutingListener.yaml"

requestDeleteEndpointGroup :: DeleteEndpointGroup -> TestTree
requestDeleteEndpointGroup =
  req
    "DeleteEndpointGroup"
    "fixture/DeleteEndpointGroup.yaml"

requestDeleteListener :: DeleteListener -> TestTree
requestDeleteListener =
  req
    "DeleteListener"
    "fixture/DeleteListener.yaml"

requestDenyCustomRoutingTraffic :: DenyCustomRoutingTraffic -> TestTree
requestDenyCustomRoutingTraffic =
  req
    "DenyCustomRoutingTraffic"
    "fixture/DenyCustomRoutingTraffic.yaml"

requestDeprovisionByoipCidr :: DeprovisionByoipCidr -> TestTree
requestDeprovisionByoipCidr =
  req
    "DeprovisionByoipCidr"
    "fixture/DeprovisionByoipCidr.yaml"

requestDescribeAccelerator :: DescribeAccelerator -> TestTree
requestDescribeAccelerator =
  req
    "DescribeAccelerator"
    "fixture/DescribeAccelerator.yaml"

requestDescribeAcceleratorAttributes :: DescribeAcceleratorAttributes -> TestTree
requestDescribeAcceleratorAttributes =
  req
    "DescribeAcceleratorAttributes"
    "fixture/DescribeAcceleratorAttributes.yaml"

requestDescribeCustomRoutingAccelerator :: DescribeCustomRoutingAccelerator -> TestTree
requestDescribeCustomRoutingAccelerator =
  req
    "DescribeCustomRoutingAccelerator"
    "fixture/DescribeCustomRoutingAccelerator.yaml"

requestDescribeCustomRoutingAcceleratorAttributes :: DescribeCustomRoutingAcceleratorAttributes -> TestTree
requestDescribeCustomRoutingAcceleratorAttributes =
  req
    "DescribeCustomRoutingAcceleratorAttributes"
    "fixture/DescribeCustomRoutingAcceleratorAttributes.yaml"

requestDescribeCustomRoutingEndpointGroup :: DescribeCustomRoutingEndpointGroup -> TestTree
requestDescribeCustomRoutingEndpointGroup =
  req
    "DescribeCustomRoutingEndpointGroup"
    "fixture/DescribeCustomRoutingEndpointGroup.yaml"

requestDescribeCustomRoutingListener :: DescribeCustomRoutingListener -> TestTree
requestDescribeCustomRoutingListener =
  req
    "DescribeCustomRoutingListener"
    "fixture/DescribeCustomRoutingListener.yaml"

requestDescribeEndpointGroup :: DescribeEndpointGroup -> TestTree
requestDescribeEndpointGroup =
  req
    "DescribeEndpointGroup"
    "fixture/DescribeEndpointGroup.yaml"

requestDescribeListener :: DescribeListener -> TestTree
requestDescribeListener =
  req
    "DescribeListener"
    "fixture/DescribeListener.yaml"

requestListAccelerators :: ListAccelerators -> TestTree
requestListAccelerators =
  req
    "ListAccelerators"
    "fixture/ListAccelerators.yaml"

requestListByoipCidrs :: ListByoipCidrs -> TestTree
requestListByoipCidrs =
  req
    "ListByoipCidrs"
    "fixture/ListByoipCidrs.yaml"

requestListCustomRoutingAccelerators :: ListCustomRoutingAccelerators -> TestTree
requestListCustomRoutingAccelerators =
  req
    "ListCustomRoutingAccelerators"
    "fixture/ListCustomRoutingAccelerators.yaml"

requestListCustomRoutingEndpointGroups :: ListCustomRoutingEndpointGroups -> TestTree
requestListCustomRoutingEndpointGroups =
  req
    "ListCustomRoutingEndpointGroups"
    "fixture/ListCustomRoutingEndpointGroups.yaml"

requestListCustomRoutingListeners :: ListCustomRoutingListeners -> TestTree
requestListCustomRoutingListeners =
  req
    "ListCustomRoutingListeners"
    "fixture/ListCustomRoutingListeners.yaml"

requestListCustomRoutingPortMappings :: ListCustomRoutingPortMappings -> TestTree
requestListCustomRoutingPortMappings =
  req
    "ListCustomRoutingPortMappings"
    "fixture/ListCustomRoutingPortMappings.yaml"

requestListCustomRoutingPortMappingsByDestination :: ListCustomRoutingPortMappingsByDestination -> TestTree
requestListCustomRoutingPortMappingsByDestination =
  req
    "ListCustomRoutingPortMappingsByDestination"
    "fixture/ListCustomRoutingPortMappingsByDestination.yaml"

requestListEndpointGroups :: ListEndpointGroups -> TestTree
requestListEndpointGroups =
  req
    "ListEndpointGroups"
    "fixture/ListEndpointGroups.yaml"

requestListListeners :: ListListeners -> TestTree
requestListListeners =
  req
    "ListListeners"
    "fixture/ListListeners.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestProvisionByoipCidr :: ProvisionByoipCidr -> TestTree
requestProvisionByoipCidr =
  req
    "ProvisionByoipCidr"
    "fixture/ProvisionByoipCidr.yaml"

requestRemoveCustomRoutingEndpoints :: RemoveCustomRoutingEndpoints -> TestTree
requestRemoveCustomRoutingEndpoints =
  req
    "RemoveCustomRoutingEndpoints"
    "fixture/RemoveCustomRoutingEndpoints.yaml"

requestRemoveEndpoints :: RemoveEndpoints -> TestTree
requestRemoveEndpoints =
  req
    "RemoveEndpoints"
    "fixture/RemoveEndpoints.yaml"

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

requestUpdateAccelerator :: UpdateAccelerator -> TestTree
requestUpdateAccelerator =
  req
    "UpdateAccelerator"
    "fixture/UpdateAccelerator.yaml"

requestUpdateAcceleratorAttributes :: UpdateAcceleratorAttributes -> TestTree
requestUpdateAcceleratorAttributes =
  req
    "UpdateAcceleratorAttributes"
    "fixture/UpdateAcceleratorAttributes.yaml"

requestUpdateCustomRoutingAccelerator :: UpdateCustomRoutingAccelerator -> TestTree
requestUpdateCustomRoutingAccelerator =
  req
    "UpdateCustomRoutingAccelerator"
    "fixture/UpdateCustomRoutingAccelerator.yaml"

requestUpdateCustomRoutingAcceleratorAttributes :: UpdateCustomRoutingAcceleratorAttributes -> TestTree
requestUpdateCustomRoutingAcceleratorAttributes =
  req
    "UpdateCustomRoutingAcceleratorAttributes"
    "fixture/UpdateCustomRoutingAcceleratorAttributes.yaml"

requestUpdateCustomRoutingListener :: UpdateCustomRoutingListener -> TestTree
requestUpdateCustomRoutingListener =
  req
    "UpdateCustomRoutingListener"
    "fixture/UpdateCustomRoutingListener.yaml"

requestUpdateEndpointGroup :: UpdateEndpointGroup -> TestTree
requestUpdateEndpointGroup =
  req
    "UpdateEndpointGroup"
    "fixture/UpdateEndpointGroup.yaml"

requestUpdateListener :: UpdateListener -> TestTree
requestUpdateListener =
  req
    "UpdateListener"
    "fixture/UpdateListener.yaml"

requestWithdrawByoipCidr :: WithdrawByoipCidr -> TestTree
requestWithdrawByoipCidr =
  req
    "WithdrawByoipCidr"
    "fixture/WithdrawByoipCidr.yaml"

-- Responses

responseAddCustomRoutingEndpoints :: AddCustomRoutingEndpointsResponse -> TestTree
responseAddCustomRoutingEndpoints =
  res
    "AddCustomRoutingEndpointsResponse"
    "fixture/AddCustomRoutingEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddCustomRoutingEndpoints)

responseAddEndpoints :: AddEndpointsResponse -> TestTree
responseAddEndpoints =
  res
    "AddEndpointsResponse"
    "fixture/AddEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddEndpoints)

responseAdvertiseByoipCidr :: AdvertiseByoipCidrResponse -> TestTree
responseAdvertiseByoipCidr =
  res
    "AdvertiseByoipCidrResponse"
    "fixture/AdvertiseByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdvertiseByoipCidr)

responseAllowCustomRoutingTraffic :: AllowCustomRoutingTrafficResponse -> TestTree
responseAllowCustomRoutingTraffic =
  res
    "AllowCustomRoutingTrafficResponse"
    "fixture/AllowCustomRoutingTrafficResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllowCustomRoutingTraffic)

responseCreateAccelerator :: CreateAcceleratorResponse -> TestTree
responseCreateAccelerator =
  res
    "CreateAcceleratorResponse"
    "fixture/CreateAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccelerator)

responseCreateCustomRoutingAccelerator :: CreateCustomRoutingAcceleratorResponse -> TestTree
responseCreateCustomRoutingAccelerator =
  res
    "CreateCustomRoutingAcceleratorResponse"
    "fixture/CreateCustomRoutingAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomRoutingAccelerator)

responseCreateCustomRoutingEndpointGroup :: CreateCustomRoutingEndpointGroupResponse -> TestTree
responseCreateCustomRoutingEndpointGroup =
  res
    "CreateCustomRoutingEndpointGroupResponse"
    "fixture/CreateCustomRoutingEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomRoutingEndpointGroup)

responseCreateCustomRoutingListener :: CreateCustomRoutingListenerResponse -> TestTree
responseCreateCustomRoutingListener =
  res
    "CreateCustomRoutingListenerResponse"
    "fixture/CreateCustomRoutingListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomRoutingListener)

responseCreateEndpointGroup :: CreateEndpointGroupResponse -> TestTree
responseCreateEndpointGroup =
  res
    "CreateEndpointGroupResponse"
    "fixture/CreateEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpointGroup)

responseCreateListener :: CreateListenerResponse -> TestTree
responseCreateListener =
  res
    "CreateListenerResponse"
    "fixture/CreateListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateListener)

responseDeleteAccelerator :: DeleteAcceleratorResponse -> TestTree
responseDeleteAccelerator =
  res
    "DeleteAcceleratorResponse"
    "fixture/DeleteAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccelerator)

responseDeleteCustomRoutingAccelerator :: DeleteCustomRoutingAcceleratorResponse -> TestTree
responseDeleteCustomRoutingAccelerator =
  res
    "DeleteCustomRoutingAcceleratorResponse"
    "fixture/DeleteCustomRoutingAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomRoutingAccelerator)

responseDeleteCustomRoutingEndpointGroup :: DeleteCustomRoutingEndpointGroupResponse -> TestTree
responseDeleteCustomRoutingEndpointGroup =
  res
    "DeleteCustomRoutingEndpointGroupResponse"
    "fixture/DeleteCustomRoutingEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomRoutingEndpointGroup)

responseDeleteCustomRoutingListener :: DeleteCustomRoutingListenerResponse -> TestTree
responseDeleteCustomRoutingListener =
  res
    "DeleteCustomRoutingListenerResponse"
    "fixture/DeleteCustomRoutingListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomRoutingListener)

responseDeleteEndpointGroup :: DeleteEndpointGroupResponse -> TestTree
responseDeleteEndpointGroup =
  res
    "DeleteEndpointGroupResponse"
    "fixture/DeleteEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpointGroup)

responseDeleteListener :: DeleteListenerResponse -> TestTree
responseDeleteListener =
  res
    "DeleteListenerResponse"
    "fixture/DeleteListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteListener)

responseDenyCustomRoutingTraffic :: DenyCustomRoutingTrafficResponse -> TestTree
responseDenyCustomRoutingTraffic =
  res
    "DenyCustomRoutingTrafficResponse"
    "fixture/DenyCustomRoutingTrafficResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DenyCustomRoutingTraffic)

responseDeprovisionByoipCidr :: DeprovisionByoipCidrResponse -> TestTree
responseDeprovisionByoipCidr =
  res
    "DeprovisionByoipCidrResponse"
    "fixture/DeprovisionByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprovisionByoipCidr)

responseDescribeAccelerator :: DescribeAcceleratorResponse -> TestTree
responseDescribeAccelerator =
  res
    "DescribeAcceleratorResponse"
    "fixture/DescribeAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccelerator)

responseDescribeAcceleratorAttributes :: DescribeAcceleratorAttributesResponse -> TestTree
responseDescribeAcceleratorAttributes =
  res
    "DescribeAcceleratorAttributesResponse"
    "fixture/DescribeAcceleratorAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAcceleratorAttributes)

responseDescribeCustomRoutingAccelerator :: DescribeCustomRoutingAcceleratorResponse -> TestTree
responseDescribeCustomRoutingAccelerator =
  res
    "DescribeCustomRoutingAcceleratorResponse"
    "fixture/DescribeCustomRoutingAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomRoutingAccelerator)

responseDescribeCustomRoutingAcceleratorAttributes :: DescribeCustomRoutingAcceleratorAttributesResponse -> TestTree
responseDescribeCustomRoutingAcceleratorAttributes =
  res
    "DescribeCustomRoutingAcceleratorAttributesResponse"
    "fixture/DescribeCustomRoutingAcceleratorAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomRoutingAcceleratorAttributes)

responseDescribeCustomRoutingEndpointGroup :: DescribeCustomRoutingEndpointGroupResponse -> TestTree
responseDescribeCustomRoutingEndpointGroup =
  res
    "DescribeCustomRoutingEndpointGroupResponse"
    "fixture/DescribeCustomRoutingEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomRoutingEndpointGroup)

responseDescribeCustomRoutingListener :: DescribeCustomRoutingListenerResponse -> TestTree
responseDescribeCustomRoutingListener =
  res
    "DescribeCustomRoutingListenerResponse"
    "fixture/DescribeCustomRoutingListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomRoutingListener)

responseDescribeEndpointGroup :: DescribeEndpointGroupResponse -> TestTree
responseDescribeEndpointGroup =
  res
    "DescribeEndpointGroupResponse"
    "fixture/DescribeEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpointGroup)

responseDescribeListener :: DescribeListenerResponse -> TestTree
responseDescribeListener =
  res
    "DescribeListenerResponse"
    "fixture/DescribeListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeListener)

responseListAccelerators :: ListAcceleratorsResponse -> TestTree
responseListAccelerators =
  res
    "ListAcceleratorsResponse"
    "fixture/ListAcceleratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccelerators)

responseListByoipCidrs :: ListByoipCidrsResponse -> TestTree
responseListByoipCidrs =
  res
    "ListByoipCidrsResponse"
    "fixture/ListByoipCidrsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListByoipCidrs)

responseListCustomRoutingAccelerators :: ListCustomRoutingAcceleratorsResponse -> TestTree
responseListCustomRoutingAccelerators =
  res
    "ListCustomRoutingAcceleratorsResponse"
    "fixture/ListCustomRoutingAcceleratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomRoutingAccelerators)

responseListCustomRoutingEndpointGroups :: ListCustomRoutingEndpointGroupsResponse -> TestTree
responseListCustomRoutingEndpointGroups =
  res
    "ListCustomRoutingEndpointGroupsResponse"
    "fixture/ListCustomRoutingEndpointGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomRoutingEndpointGroups)

responseListCustomRoutingListeners :: ListCustomRoutingListenersResponse -> TestTree
responseListCustomRoutingListeners =
  res
    "ListCustomRoutingListenersResponse"
    "fixture/ListCustomRoutingListenersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomRoutingListeners)

responseListCustomRoutingPortMappings :: ListCustomRoutingPortMappingsResponse -> TestTree
responseListCustomRoutingPortMappings =
  res
    "ListCustomRoutingPortMappingsResponse"
    "fixture/ListCustomRoutingPortMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomRoutingPortMappings)

responseListCustomRoutingPortMappingsByDestination :: ListCustomRoutingPortMappingsByDestinationResponse -> TestTree
responseListCustomRoutingPortMappingsByDestination =
  res
    "ListCustomRoutingPortMappingsByDestinationResponse"
    "fixture/ListCustomRoutingPortMappingsByDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomRoutingPortMappingsByDestination)

responseListEndpointGroups :: ListEndpointGroupsResponse -> TestTree
responseListEndpointGroups =
  res
    "ListEndpointGroupsResponse"
    "fixture/ListEndpointGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpointGroups)

responseListListeners :: ListListenersResponse -> TestTree
responseListListeners =
  res
    "ListListenersResponse"
    "fixture/ListListenersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListListeners)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseProvisionByoipCidr :: ProvisionByoipCidrResponse -> TestTree
responseProvisionByoipCidr =
  res
    "ProvisionByoipCidrResponse"
    "fixture/ProvisionByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionByoipCidr)

responseRemoveCustomRoutingEndpoints :: RemoveCustomRoutingEndpointsResponse -> TestTree
responseRemoveCustomRoutingEndpoints =
  res
    "RemoveCustomRoutingEndpointsResponse"
    "fixture/RemoveCustomRoutingEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveCustomRoutingEndpoints)

responseRemoveEndpoints :: RemoveEndpointsResponse -> TestTree
responseRemoveEndpoints =
  res
    "RemoveEndpointsResponse"
    "fixture/RemoveEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveEndpoints)

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

responseUpdateAccelerator :: UpdateAcceleratorResponse -> TestTree
responseUpdateAccelerator =
  res
    "UpdateAcceleratorResponse"
    "fixture/UpdateAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccelerator)

responseUpdateAcceleratorAttributes :: UpdateAcceleratorAttributesResponse -> TestTree
responseUpdateAcceleratorAttributes =
  res
    "UpdateAcceleratorAttributesResponse"
    "fixture/UpdateAcceleratorAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAcceleratorAttributes)

responseUpdateCustomRoutingAccelerator :: UpdateCustomRoutingAcceleratorResponse -> TestTree
responseUpdateCustomRoutingAccelerator =
  res
    "UpdateCustomRoutingAcceleratorResponse"
    "fixture/UpdateCustomRoutingAcceleratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomRoutingAccelerator)

responseUpdateCustomRoutingAcceleratorAttributes :: UpdateCustomRoutingAcceleratorAttributesResponse -> TestTree
responseUpdateCustomRoutingAcceleratorAttributes =
  res
    "UpdateCustomRoutingAcceleratorAttributesResponse"
    "fixture/UpdateCustomRoutingAcceleratorAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomRoutingAcceleratorAttributes)

responseUpdateCustomRoutingListener :: UpdateCustomRoutingListenerResponse -> TestTree
responseUpdateCustomRoutingListener =
  res
    "UpdateCustomRoutingListenerResponse"
    "fixture/UpdateCustomRoutingListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomRoutingListener)

responseUpdateEndpointGroup :: UpdateEndpointGroupResponse -> TestTree
responseUpdateEndpointGroup =
  res
    "UpdateEndpointGroupResponse"
    "fixture/UpdateEndpointGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpointGroup)

responseUpdateListener :: UpdateListenerResponse -> TestTree
responseUpdateListener =
  res
    "UpdateListenerResponse"
    "fixture/UpdateListenerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateListener)

responseWithdrawByoipCidr :: WithdrawByoipCidrResponse -> TestTree
responseWithdrawByoipCidr =
  res
    "WithdrawByoipCidrResponse"
    "fixture/WithdrawByoipCidrResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy WithdrawByoipCidr)

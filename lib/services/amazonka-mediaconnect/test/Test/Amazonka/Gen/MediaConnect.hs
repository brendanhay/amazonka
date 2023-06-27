{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MediaConnect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MediaConnect where

import Amazonka.MediaConnect
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MediaConnect.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddBridgeOutputs $
--             newAddBridgeOutputs
--
--         , requestAddBridgeSources $
--             newAddBridgeSources
--
--         , requestAddFlowMediaStreams $
--             newAddFlowMediaStreams
--
--         , requestAddFlowOutputs $
--             newAddFlowOutputs
--
--         , requestAddFlowSources $
--             newAddFlowSources
--
--         , requestAddFlowVpcInterfaces $
--             newAddFlowVpcInterfaces
--
--         , requestCreateBridge $
--             newCreateBridge
--
--         , requestCreateFlow $
--             newCreateFlow
--
--         , requestCreateGateway $
--             newCreateGateway
--
--         , requestDeleteBridge $
--             newDeleteBridge
--
--         , requestDeleteFlow $
--             newDeleteFlow
--
--         , requestDeleteGateway $
--             newDeleteGateway
--
--         , requestDeregisterGatewayInstance $
--             newDeregisterGatewayInstance
--
--         , requestDescribeBridge $
--             newDescribeBridge
--
--         , requestDescribeFlow $
--             newDescribeFlow
--
--         , requestDescribeGateway $
--             newDescribeGateway
--
--         , requestDescribeGatewayInstance $
--             newDescribeGatewayInstance
--
--         , requestDescribeOffering $
--             newDescribeOffering
--
--         , requestDescribeReservation $
--             newDescribeReservation
--
--         , requestGrantFlowEntitlements $
--             newGrantFlowEntitlements
--
--         , requestListBridges $
--             newListBridges
--
--         , requestListEntitlements $
--             newListEntitlements
--
--         , requestListFlows $
--             newListFlows
--
--         , requestListGatewayInstances $
--             newListGatewayInstances
--
--         , requestListGateways $
--             newListGateways
--
--         , requestListOfferings $
--             newListOfferings
--
--         , requestListReservations $
--             newListReservations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPurchaseOffering $
--             newPurchaseOffering
--
--         , requestRemoveBridgeOutput $
--             newRemoveBridgeOutput
--
--         , requestRemoveBridgeSource $
--             newRemoveBridgeSource
--
--         , requestRemoveFlowMediaStream $
--             newRemoveFlowMediaStream
--
--         , requestRemoveFlowOutput $
--             newRemoveFlowOutput
--
--         , requestRemoveFlowSource $
--             newRemoveFlowSource
--
--         , requestRemoveFlowVpcInterface $
--             newRemoveFlowVpcInterface
--
--         , requestRevokeFlowEntitlement $
--             newRevokeFlowEntitlement
--
--         , requestStartFlow $
--             newStartFlow
--
--         , requestStopFlow $
--             newStopFlow
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateBridge $
--             newUpdateBridge
--
--         , requestUpdateBridgeOutput $
--             newUpdateBridgeOutput
--
--         , requestUpdateBridgeSource $
--             newUpdateBridgeSource
--
--         , requestUpdateBridgeState $
--             newUpdateBridgeState
--
--         , requestUpdateFlow $
--             newUpdateFlow
--
--         , requestUpdateFlowEntitlement $
--             newUpdateFlowEntitlement
--
--         , requestUpdateFlowMediaStream $
--             newUpdateFlowMediaStream
--
--         , requestUpdateFlowOutput $
--             newUpdateFlowOutput
--
--         , requestUpdateFlowSource $
--             newUpdateFlowSource
--
--         , requestUpdateGatewayInstance $
--             newUpdateGatewayInstance
--
--           ]

--     , testGroup "response"
--         [ responseAddBridgeOutputs $
--             newAddBridgeOutputsResponse
--
--         , responseAddBridgeSources $
--             newAddBridgeSourcesResponse
--
--         , responseAddFlowMediaStreams $
--             newAddFlowMediaStreamsResponse
--
--         , responseAddFlowOutputs $
--             newAddFlowOutputsResponse
--
--         , responseAddFlowSources $
--             newAddFlowSourcesResponse
--
--         , responseAddFlowVpcInterfaces $
--             newAddFlowVpcInterfacesResponse
--
--         , responseCreateBridge $
--             newCreateBridgeResponse
--
--         , responseCreateFlow $
--             newCreateFlowResponse
--
--         , responseCreateGateway $
--             newCreateGatewayResponse
--
--         , responseDeleteBridge $
--             newDeleteBridgeResponse
--
--         , responseDeleteFlow $
--             newDeleteFlowResponse
--
--         , responseDeleteGateway $
--             newDeleteGatewayResponse
--
--         , responseDeregisterGatewayInstance $
--             newDeregisterGatewayInstanceResponse
--
--         , responseDescribeBridge $
--             newDescribeBridgeResponse
--
--         , responseDescribeFlow $
--             newDescribeFlowResponse
--
--         , responseDescribeGateway $
--             newDescribeGatewayResponse
--
--         , responseDescribeGatewayInstance $
--             newDescribeGatewayInstanceResponse
--
--         , responseDescribeOffering $
--             newDescribeOfferingResponse
--
--         , responseDescribeReservation $
--             newDescribeReservationResponse
--
--         , responseGrantFlowEntitlements $
--             newGrantFlowEntitlementsResponse
--
--         , responseListBridges $
--             newListBridgesResponse
--
--         , responseListEntitlements $
--             newListEntitlementsResponse
--
--         , responseListFlows $
--             newListFlowsResponse
--
--         , responseListGatewayInstances $
--             newListGatewayInstancesResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseListOfferings $
--             newListOfferingsResponse
--
--         , responseListReservations $
--             newListReservationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePurchaseOffering $
--             newPurchaseOfferingResponse
--
--         , responseRemoveBridgeOutput $
--             newRemoveBridgeOutputResponse
--
--         , responseRemoveBridgeSource $
--             newRemoveBridgeSourceResponse
--
--         , responseRemoveFlowMediaStream $
--             newRemoveFlowMediaStreamResponse
--
--         , responseRemoveFlowOutput $
--             newRemoveFlowOutputResponse
--
--         , responseRemoveFlowSource $
--             newRemoveFlowSourceResponse
--
--         , responseRemoveFlowVpcInterface $
--             newRemoveFlowVpcInterfaceResponse
--
--         , responseRevokeFlowEntitlement $
--             newRevokeFlowEntitlementResponse
--
--         , responseStartFlow $
--             newStartFlowResponse
--
--         , responseStopFlow $
--             newStopFlowResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateBridge $
--             newUpdateBridgeResponse
--
--         , responseUpdateBridgeOutput $
--             newUpdateBridgeOutputResponse
--
--         , responseUpdateBridgeSource $
--             newUpdateBridgeSourceResponse
--
--         , responseUpdateBridgeState $
--             newUpdateBridgeStateResponse
--
--         , responseUpdateFlow $
--             newUpdateFlowResponse
--
--         , responseUpdateFlowEntitlement $
--             newUpdateFlowEntitlementResponse
--
--         , responseUpdateFlowMediaStream $
--             newUpdateFlowMediaStreamResponse
--
--         , responseUpdateFlowOutput $
--             newUpdateFlowOutputResponse
--
--         , responseUpdateFlowSource $
--             newUpdateFlowSourceResponse
--
--         , responseUpdateGatewayInstance $
--             newUpdateGatewayInstanceResponse
--
--           ]
--     ]

-- Requests

requestAddBridgeOutputs :: AddBridgeOutputs -> TestTree
requestAddBridgeOutputs =
  req
    "AddBridgeOutputs"
    "fixture/AddBridgeOutputs.yaml"

requestAddBridgeSources :: AddBridgeSources -> TestTree
requestAddBridgeSources =
  req
    "AddBridgeSources"
    "fixture/AddBridgeSources.yaml"

requestAddFlowMediaStreams :: AddFlowMediaStreams -> TestTree
requestAddFlowMediaStreams =
  req
    "AddFlowMediaStreams"
    "fixture/AddFlowMediaStreams.yaml"

requestAddFlowOutputs :: AddFlowOutputs -> TestTree
requestAddFlowOutputs =
  req
    "AddFlowOutputs"
    "fixture/AddFlowOutputs.yaml"

requestAddFlowSources :: AddFlowSources -> TestTree
requestAddFlowSources =
  req
    "AddFlowSources"
    "fixture/AddFlowSources.yaml"

requestAddFlowVpcInterfaces :: AddFlowVpcInterfaces -> TestTree
requestAddFlowVpcInterfaces =
  req
    "AddFlowVpcInterfaces"
    "fixture/AddFlowVpcInterfaces.yaml"

requestCreateBridge :: CreateBridge -> TestTree
requestCreateBridge =
  req
    "CreateBridge"
    "fixture/CreateBridge.yaml"

requestCreateFlow :: CreateFlow -> TestTree
requestCreateFlow =
  req
    "CreateFlow"
    "fixture/CreateFlow.yaml"

requestCreateGateway :: CreateGateway -> TestTree
requestCreateGateway =
  req
    "CreateGateway"
    "fixture/CreateGateway.yaml"

requestDeleteBridge :: DeleteBridge -> TestTree
requestDeleteBridge =
  req
    "DeleteBridge"
    "fixture/DeleteBridge.yaml"

requestDeleteFlow :: DeleteFlow -> TestTree
requestDeleteFlow =
  req
    "DeleteFlow"
    "fixture/DeleteFlow.yaml"

requestDeleteGateway :: DeleteGateway -> TestTree
requestDeleteGateway =
  req
    "DeleteGateway"
    "fixture/DeleteGateway.yaml"

requestDeregisterGatewayInstance :: DeregisterGatewayInstance -> TestTree
requestDeregisterGatewayInstance =
  req
    "DeregisterGatewayInstance"
    "fixture/DeregisterGatewayInstance.yaml"

requestDescribeBridge :: DescribeBridge -> TestTree
requestDescribeBridge =
  req
    "DescribeBridge"
    "fixture/DescribeBridge.yaml"

requestDescribeFlow :: DescribeFlow -> TestTree
requestDescribeFlow =
  req
    "DescribeFlow"
    "fixture/DescribeFlow.yaml"

requestDescribeGateway :: DescribeGateway -> TestTree
requestDescribeGateway =
  req
    "DescribeGateway"
    "fixture/DescribeGateway.yaml"

requestDescribeGatewayInstance :: DescribeGatewayInstance -> TestTree
requestDescribeGatewayInstance =
  req
    "DescribeGatewayInstance"
    "fixture/DescribeGatewayInstance.yaml"

requestDescribeOffering :: DescribeOffering -> TestTree
requestDescribeOffering =
  req
    "DescribeOffering"
    "fixture/DescribeOffering.yaml"

requestDescribeReservation :: DescribeReservation -> TestTree
requestDescribeReservation =
  req
    "DescribeReservation"
    "fixture/DescribeReservation.yaml"

requestGrantFlowEntitlements :: GrantFlowEntitlements -> TestTree
requestGrantFlowEntitlements =
  req
    "GrantFlowEntitlements"
    "fixture/GrantFlowEntitlements.yaml"

requestListBridges :: ListBridges -> TestTree
requestListBridges =
  req
    "ListBridges"
    "fixture/ListBridges.yaml"

requestListEntitlements :: ListEntitlements -> TestTree
requestListEntitlements =
  req
    "ListEntitlements"
    "fixture/ListEntitlements.yaml"

requestListFlows :: ListFlows -> TestTree
requestListFlows =
  req
    "ListFlows"
    "fixture/ListFlows.yaml"

requestListGatewayInstances :: ListGatewayInstances -> TestTree
requestListGatewayInstances =
  req
    "ListGatewayInstances"
    "fixture/ListGatewayInstances.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways =
  req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings =
  req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

requestListReservations :: ListReservations -> TestTree
requestListReservations =
  req
    "ListReservations"
    "fixture/ListReservations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPurchaseOffering :: PurchaseOffering -> TestTree
requestPurchaseOffering =
  req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestRemoveBridgeOutput :: RemoveBridgeOutput -> TestTree
requestRemoveBridgeOutput =
  req
    "RemoveBridgeOutput"
    "fixture/RemoveBridgeOutput.yaml"

requestRemoveBridgeSource :: RemoveBridgeSource -> TestTree
requestRemoveBridgeSource =
  req
    "RemoveBridgeSource"
    "fixture/RemoveBridgeSource.yaml"

requestRemoveFlowMediaStream :: RemoveFlowMediaStream -> TestTree
requestRemoveFlowMediaStream =
  req
    "RemoveFlowMediaStream"
    "fixture/RemoveFlowMediaStream.yaml"

requestRemoveFlowOutput :: RemoveFlowOutput -> TestTree
requestRemoveFlowOutput =
  req
    "RemoveFlowOutput"
    "fixture/RemoveFlowOutput.yaml"

requestRemoveFlowSource :: RemoveFlowSource -> TestTree
requestRemoveFlowSource =
  req
    "RemoveFlowSource"
    "fixture/RemoveFlowSource.yaml"

requestRemoveFlowVpcInterface :: RemoveFlowVpcInterface -> TestTree
requestRemoveFlowVpcInterface =
  req
    "RemoveFlowVpcInterface"
    "fixture/RemoveFlowVpcInterface.yaml"

requestRevokeFlowEntitlement :: RevokeFlowEntitlement -> TestTree
requestRevokeFlowEntitlement =
  req
    "RevokeFlowEntitlement"
    "fixture/RevokeFlowEntitlement.yaml"

requestStartFlow :: StartFlow -> TestTree
requestStartFlow =
  req
    "StartFlow"
    "fixture/StartFlow.yaml"

requestStopFlow :: StopFlow -> TestTree
requestStopFlow =
  req
    "StopFlow"
    "fixture/StopFlow.yaml"

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

requestUpdateBridge :: UpdateBridge -> TestTree
requestUpdateBridge =
  req
    "UpdateBridge"
    "fixture/UpdateBridge.yaml"

requestUpdateBridgeOutput :: UpdateBridgeOutput -> TestTree
requestUpdateBridgeOutput =
  req
    "UpdateBridgeOutput"
    "fixture/UpdateBridgeOutput.yaml"

requestUpdateBridgeSource :: UpdateBridgeSource -> TestTree
requestUpdateBridgeSource =
  req
    "UpdateBridgeSource"
    "fixture/UpdateBridgeSource.yaml"

requestUpdateBridgeState :: UpdateBridgeState -> TestTree
requestUpdateBridgeState =
  req
    "UpdateBridgeState"
    "fixture/UpdateBridgeState.yaml"

requestUpdateFlow :: UpdateFlow -> TestTree
requestUpdateFlow =
  req
    "UpdateFlow"
    "fixture/UpdateFlow.yaml"

requestUpdateFlowEntitlement :: UpdateFlowEntitlement -> TestTree
requestUpdateFlowEntitlement =
  req
    "UpdateFlowEntitlement"
    "fixture/UpdateFlowEntitlement.yaml"

requestUpdateFlowMediaStream :: UpdateFlowMediaStream -> TestTree
requestUpdateFlowMediaStream =
  req
    "UpdateFlowMediaStream"
    "fixture/UpdateFlowMediaStream.yaml"

requestUpdateFlowOutput :: UpdateFlowOutput -> TestTree
requestUpdateFlowOutput =
  req
    "UpdateFlowOutput"
    "fixture/UpdateFlowOutput.yaml"

requestUpdateFlowSource :: UpdateFlowSource -> TestTree
requestUpdateFlowSource =
  req
    "UpdateFlowSource"
    "fixture/UpdateFlowSource.yaml"

requestUpdateGatewayInstance :: UpdateGatewayInstance -> TestTree
requestUpdateGatewayInstance =
  req
    "UpdateGatewayInstance"
    "fixture/UpdateGatewayInstance.yaml"

-- Responses

responseAddBridgeOutputs :: AddBridgeOutputsResponse -> TestTree
responseAddBridgeOutputs =
  res
    "AddBridgeOutputsResponse"
    "fixture/AddBridgeOutputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddBridgeOutputs)

responseAddBridgeSources :: AddBridgeSourcesResponse -> TestTree
responseAddBridgeSources =
  res
    "AddBridgeSourcesResponse"
    "fixture/AddBridgeSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddBridgeSources)

responseAddFlowMediaStreams :: AddFlowMediaStreamsResponse -> TestTree
responseAddFlowMediaStreams =
  res
    "AddFlowMediaStreamsResponse"
    "fixture/AddFlowMediaStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddFlowMediaStreams)

responseAddFlowOutputs :: AddFlowOutputsResponse -> TestTree
responseAddFlowOutputs =
  res
    "AddFlowOutputsResponse"
    "fixture/AddFlowOutputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddFlowOutputs)

responseAddFlowSources :: AddFlowSourcesResponse -> TestTree
responseAddFlowSources =
  res
    "AddFlowSourcesResponse"
    "fixture/AddFlowSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddFlowSources)

responseAddFlowVpcInterfaces :: AddFlowVpcInterfacesResponse -> TestTree
responseAddFlowVpcInterfaces =
  res
    "AddFlowVpcInterfacesResponse"
    "fixture/AddFlowVpcInterfacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddFlowVpcInterfaces)

responseCreateBridge :: CreateBridgeResponse -> TestTree
responseCreateBridge =
  res
    "CreateBridgeResponse"
    "fixture/CreateBridgeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBridge)

responseCreateFlow :: CreateFlowResponse -> TestTree
responseCreateFlow =
  res
    "CreateFlowResponse"
    "fixture/CreateFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFlow)

responseCreateGateway :: CreateGatewayResponse -> TestTree
responseCreateGateway =
  res
    "CreateGatewayResponse"
    "fixture/CreateGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGateway)

responseDeleteBridge :: DeleteBridgeResponse -> TestTree
responseDeleteBridge =
  res
    "DeleteBridgeResponse"
    "fixture/DeleteBridgeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBridge)

responseDeleteFlow :: DeleteFlowResponse -> TestTree
responseDeleteFlow =
  res
    "DeleteFlowResponse"
    "fixture/DeleteFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlow)

responseDeleteGateway :: DeleteGatewayResponse -> TestTree
responseDeleteGateway =
  res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGateway)

responseDeregisterGatewayInstance :: DeregisterGatewayInstanceResponse -> TestTree
responseDeregisterGatewayInstance =
  res
    "DeregisterGatewayInstanceResponse"
    "fixture/DeregisterGatewayInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterGatewayInstance)

responseDescribeBridge :: DescribeBridgeResponse -> TestTree
responseDescribeBridge =
  res
    "DescribeBridgeResponse"
    "fixture/DescribeBridgeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBridge)

responseDescribeFlow :: DescribeFlowResponse -> TestTree
responseDescribeFlow =
  res
    "DescribeFlowResponse"
    "fixture/DescribeFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlow)

responseDescribeGateway :: DescribeGatewayResponse -> TestTree
responseDescribeGateway =
  res
    "DescribeGatewayResponse"
    "fixture/DescribeGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGateway)

responseDescribeGatewayInstance :: DescribeGatewayInstanceResponse -> TestTree
responseDescribeGatewayInstance =
  res
    "DescribeGatewayInstanceResponse"
    "fixture/DescribeGatewayInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGatewayInstance)

responseDescribeOffering :: DescribeOfferingResponse -> TestTree
responseDescribeOffering =
  res
    "DescribeOfferingResponse"
    "fixture/DescribeOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOffering)

responseDescribeReservation :: DescribeReservationResponse -> TestTree
responseDescribeReservation =
  res
    "DescribeReservationResponse"
    "fixture/DescribeReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservation)

responseGrantFlowEntitlements :: GrantFlowEntitlementsResponse -> TestTree
responseGrantFlowEntitlements =
  res
    "GrantFlowEntitlementsResponse"
    "fixture/GrantFlowEntitlementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GrantFlowEntitlements)

responseListBridges :: ListBridgesResponse -> TestTree
responseListBridges =
  res
    "ListBridgesResponse"
    "fixture/ListBridgesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBridges)

responseListEntitlements :: ListEntitlementsResponse -> TestTree
responseListEntitlements =
  res
    "ListEntitlementsResponse"
    "fixture/ListEntitlementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntitlements)

responseListFlows :: ListFlowsResponse -> TestTree
responseListFlows =
  res
    "ListFlowsResponse"
    "fixture/ListFlowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFlows)

responseListGatewayInstances :: ListGatewayInstancesResponse -> TestTree
responseListGatewayInstances =
  res
    "ListGatewayInstancesResponse"
    "fixture/ListGatewayInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGatewayInstances)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGateways)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOfferings)

responseListReservations :: ListReservationsResponse -> TestTree
responseListReservations =
  res
    "ListReservationsResponse"
    "fixture/ListReservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReservations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseOffering)

responseRemoveBridgeOutput :: RemoveBridgeOutputResponse -> TestTree
responseRemoveBridgeOutput =
  res
    "RemoveBridgeOutputResponse"
    "fixture/RemoveBridgeOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveBridgeOutput)

responseRemoveBridgeSource :: RemoveBridgeSourceResponse -> TestTree
responseRemoveBridgeSource =
  res
    "RemoveBridgeSourceResponse"
    "fixture/RemoveBridgeSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveBridgeSource)

responseRemoveFlowMediaStream :: RemoveFlowMediaStreamResponse -> TestTree
responseRemoveFlowMediaStream =
  res
    "RemoveFlowMediaStreamResponse"
    "fixture/RemoveFlowMediaStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFlowMediaStream)

responseRemoveFlowOutput :: RemoveFlowOutputResponse -> TestTree
responseRemoveFlowOutput =
  res
    "RemoveFlowOutputResponse"
    "fixture/RemoveFlowOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFlowOutput)

responseRemoveFlowSource :: RemoveFlowSourceResponse -> TestTree
responseRemoveFlowSource =
  res
    "RemoveFlowSourceResponse"
    "fixture/RemoveFlowSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFlowSource)

responseRemoveFlowVpcInterface :: RemoveFlowVpcInterfaceResponse -> TestTree
responseRemoveFlowVpcInterface =
  res
    "RemoveFlowVpcInterfaceResponse"
    "fixture/RemoveFlowVpcInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFlowVpcInterface)

responseRevokeFlowEntitlement :: RevokeFlowEntitlementResponse -> TestTree
responseRevokeFlowEntitlement =
  res
    "RevokeFlowEntitlementResponse"
    "fixture/RevokeFlowEntitlementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeFlowEntitlement)

responseStartFlow :: StartFlowResponse -> TestTree
responseStartFlow =
  res
    "StartFlowResponse"
    "fixture/StartFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFlow)

responseStopFlow :: StopFlowResponse -> TestTree
responseStopFlow =
  res
    "StopFlowResponse"
    "fixture/StopFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopFlow)

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

responseUpdateBridge :: UpdateBridgeResponse -> TestTree
responseUpdateBridge =
  res
    "UpdateBridgeResponse"
    "fixture/UpdateBridgeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBridge)

responseUpdateBridgeOutput :: UpdateBridgeOutputResponse -> TestTree
responseUpdateBridgeOutput =
  res
    "UpdateBridgeOutputResponse"
    "fixture/UpdateBridgeOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBridgeOutput)

responseUpdateBridgeSource :: UpdateBridgeSourceResponse -> TestTree
responseUpdateBridgeSource =
  res
    "UpdateBridgeSourceResponse"
    "fixture/UpdateBridgeSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBridgeSource)

responseUpdateBridgeState :: UpdateBridgeStateResponse -> TestTree
responseUpdateBridgeState =
  res
    "UpdateBridgeStateResponse"
    "fixture/UpdateBridgeStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBridgeState)

responseUpdateFlow :: UpdateFlowResponse -> TestTree
responseUpdateFlow =
  res
    "UpdateFlowResponse"
    "fixture/UpdateFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlow)

responseUpdateFlowEntitlement :: UpdateFlowEntitlementResponse -> TestTree
responseUpdateFlowEntitlement =
  res
    "UpdateFlowEntitlementResponse"
    "fixture/UpdateFlowEntitlementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlowEntitlement)

responseUpdateFlowMediaStream :: UpdateFlowMediaStreamResponse -> TestTree
responseUpdateFlowMediaStream =
  res
    "UpdateFlowMediaStreamResponse"
    "fixture/UpdateFlowMediaStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlowMediaStream)

responseUpdateFlowOutput :: UpdateFlowOutputResponse -> TestTree
responseUpdateFlowOutput =
  res
    "UpdateFlowOutputResponse"
    "fixture/UpdateFlowOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlowOutput)

responseUpdateFlowSource :: UpdateFlowSourceResponse -> TestTree
responseUpdateFlowSource =
  res
    "UpdateFlowSourceResponse"
    "fixture/UpdateFlowSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlowSource)

responseUpdateGatewayInstance :: UpdateGatewayInstanceResponse -> TestTree
responseUpdateGatewayInstance =
  res
    "UpdateGatewayInstanceResponse"
    "fixture/UpdateGatewayInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayInstance)

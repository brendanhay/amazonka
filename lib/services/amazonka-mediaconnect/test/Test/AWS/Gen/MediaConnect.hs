{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MediaConnect where

import Amazonka.MediaConnect
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.MediaConnect.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestRemoveFlowVpcInterface $
--             newRemoveFlowVpcInterface
--
--         , requestRemoveFlowMediaStream $
--             newRemoveFlowMediaStream
--
--         , requestUpdateFlowOutput $
--             newUpdateFlowOutput
--
--         , requestAddFlowOutputs $
--             newAddFlowOutputs
--
--         , requestStartFlow $
--             newStartFlow
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestAddFlowSources $
--             newAddFlowSources
--
--         , requestDescribeReservation $
--             newDescribeReservation
--
--         , requestAddFlowMediaStreams $
--             newAddFlowMediaStreams
--
--         , requestRemoveFlowOutput $
--             newRemoveFlowOutput
--
--         , requestRevokeFlowEntitlement $
--             newRevokeFlowEntitlement
--
--         , requestCreateFlow $
--             newCreateFlow
--
--         , requestRemoveFlowSource $
--             newRemoveFlowSource
--
--         , requestDescribeFlow $
--             newDescribeFlow
--
--         , requestUpdateFlowEntitlement $
--             newUpdateFlowEntitlement
--
--         , requestStopFlow $
--             newStopFlow
--
--         , requestDescribeOffering $
--             newDescribeOffering
--
--         , requestAddFlowVpcInterfaces $
--             newAddFlowVpcInterfaces
--
--         , requestListEntitlements $
--             newListEntitlements
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListFlows $
--             newListFlows
--
--         , requestPurchaseOffering $
--             newPurchaseOffering
--
--         , requestUpdateFlowMediaStream $
--             newUpdateFlowMediaStream
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFlow $
--             newUpdateFlow
--
--         , requestDeleteFlow $
--             newDeleteFlow
--
--         , requestUpdateFlowSource $
--             newUpdateFlowSource
--
--         , requestGrantFlowEntitlements $
--             newGrantFlowEntitlements
--
--         , requestListReservations $
--             newListReservations
--
--         , requestListOfferings $
--             newListOfferings
--
--           ]

--     , testGroup "response"
--         [ responseRemoveFlowVpcInterface $
--             newRemoveFlowVpcInterfaceResponse
--
--         , responseRemoveFlowMediaStream $
--             newRemoveFlowMediaStreamResponse
--
--         , responseUpdateFlowOutput $
--             newUpdateFlowOutputResponse
--
--         , responseAddFlowOutputs $
--             newAddFlowOutputsResponse
--
--         , responseStartFlow $
--             newStartFlowResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseAddFlowSources $
--             newAddFlowSourcesResponse
--
--         , responseDescribeReservation $
--             newDescribeReservationResponse
--
--         , responseAddFlowMediaStreams $
--             newAddFlowMediaStreamsResponse
--
--         , responseRemoveFlowOutput $
--             newRemoveFlowOutputResponse
--
--         , responseRevokeFlowEntitlement $
--             newRevokeFlowEntitlementResponse
--
--         , responseCreateFlow $
--             newCreateFlowResponse
--
--         , responseRemoveFlowSource $
--             newRemoveFlowSourceResponse
--
--         , responseDescribeFlow $
--             newDescribeFlowResponse
--
--         , responseUpdateFlowEntitlement $
--             newUpdateFlowEntitlementResponse
--
--         , responseStopFlow $
--             newStopFlowResponse
--
--         , responseDescribeOffering $
--             newDescribeOfferingResponse
--
--         , responseAddFlowVpcInterfaces $
--             newAddFlowVpcInterfacesResponse
--
--         , responseListEntitlements $
--             newListEntitlementsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListFlows $
--             newListFlowsResponse
--
--         , responsePurchaseOffering $
--             newPurchaseOfferingResponse
--
--         , responseUpdateFlowMediaStream $
--             newUpdateFlowMediaStreamResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFlow $
--             newUpdateFlowResponse
--
--         , responseDeleteFlow $
--             newDeleteFlowResponse
--
--         , responseUpdateFlowSource $
--             newUpdateFlowSourceResponse
--
--         , responseGrantFlowEntitlements $
--             newGrantFlowEntitlementsResponse
--
--         , responseListReservations $
--             newListReservationsResponse
--
--         , responseListOfferings $
--             newListOfferingsResponse
--
--           ]
--     ]

-- Requests

requestRemoveFlowVpcInterface :: RemoveFlowVpcInterface -> TestTree
requestRemoveFlowVpcInterface =
  req
    "RemoveFlowVpcInterface"
    "fixture/RemoveFlowVpcInterface.yaml"

requestRemoveFlowMediaStream :: RemoveFlowMediaStream -> TestTree
requestRemoveFlowMediaStream =
  req
    "RemoveFlowMediaStream"
    "fixture/RemoveFlowMediaStream.yaml"

requestUpdateFlowOutput :: UpdateFlowOutput -> TestTree
requestUpdateFlowOutput =
  req
    "UpdateFlowOutput"
    "fixture/UpdateFlowOutput.yaml"

requestAddFlowOutputs :: AddFlowOutputs -> TestTree
requestAddFlowOutputs =
  req
    "AddFlowOutputs"
    "fixture/AddFlowOutputs.yaml"

requestStartFlow :: StartFlow -> TestTree
requestStartFlow =
  req
    "StartFlow"
    "fixture/StartFlow.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestAddFlowSources :: AddFlowSources -> TestTree
requestAddFlowSources =
  req
    "AddFlowSources"
    "fixture/AddFlowSources.yaml"

requestDescribeReservation :: DescribeReservation -> TestTree
requestDescribeReservation =
  req
    "DescribeReservation"
    "fixture/DescribeReservation.yaml"

requestAddFlowMediaStreams :: AddFlowMediaStreams -> TestTree
requestAddFlowMediaStreams =
  req
    "AddFlowMediaStreams"
    "fixture/AddFlowMediaStreams.yaml"

requestRemoveFlowOutput :: RemoveFlowOutput -> TestTree
requestRemoveFlowOutput =
  req
    "RemoveFlowOutput"
    "fixture/RemoveFlowOutput.yaml"

requestRevokeFlowEntitlement :: RevokeFlowEntitlement -> TestTree
requestRevokeFlowEntitlement =
  req
    "RevokeFlowEntitlement"
    "fixture/RevokeFlowEntitlement.yaml"

requestCreateFlow :: CreateFlow -> TestTree
requestCreateFlow =
  req
    "CreateFlow"
    "fixture/CreateFlow.yaml"

requestRemoveFlowSource :: RemoveFlowSource -> TestTree
requestRemoveFlowSource =
  req
    "RemoveFlowSource"
    "fixture/RemoveFlowSource.yaml"

requestDescribeFlow :: DescribeFlow -> TestTree
requestDescribeFlow =
  req
    "DescribeFlow"
    "fixture/DescribeFlow.yaml"

requestUpdateFlowEntitlement :: UpdateFlowEntitlement -> TestTree
requestUpdateFlowEntitlement =
  req
    "UpdateFlowEntitlement"
    "fixture/UpdateFlowEntitlement.yaml"

requestStopFlow :: StopFlow -> TestTree
requestStopFlow =
  req
    "StopFlow"
    "fixture/StopFlow.yaml"

requestDescribeOffering :: DescribeOffering -> TestTree
requestDescribeOffering =
  req
    "DescribeOffering"
    "fixture/DescribeOffering.yaml"

requestAddFlowVpcInterfaces :: AddFlowVpcInterfaces -> TestTree
requestAddFlowVpcInterfaces =
  req
    "AddFlowVpcInterfaces"
    "fixture/AddFlowVpcInterfaces.yaml"

requestListEntitlements :: ListEntitlements -> TestTree
requestListEntitlements =
  req
    "ListEntitlements"
    "fixture/ListEntitlements.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListFlows :: ListFlows -> TestTree
requestListFlows =
  req
    "ListFlows"
    "fixture/ListFlows.yaml"

requestPurchaseOffering :: PurchaseOffering -> TestTree
requestPurchaseOffering =
  req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestUpdateFlowMediaStream :: UpdateFlowMediaStream -> TestTree
requestUpdateFlowMediaStream =
  req
    "UpdateFlowMediaStream"
    "fixture/UpdateFlowMediaStream.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateFlow :: UpdateFlow -> TestTree
requestUpdateFlow =
  req
    "UpdateFlow"
    "fixture/UpdateFlow.yaml"

requestDeleteFlow :: DeleteFlow -> TestTree
requestDeleteFlow =
  req
    "DeleteFlow"
    "fixture/DeleteFlow.yaml"

requestUpdateFlowSource :: UpdateFlowSource -> TestTree
requestUpdateFlowSource =
  req
    "UpdateFlowSource"
    "fixture/UpdateFlowSource.yaml"

requestGrantFlowEntitlements :: GrantFlowEntitlements -> TestTree
requestGrantFlowEntitlements =
  req
    "GrantFlowEntitlements"
    "fixture/GrantFlowEntitlements.yaml"

requestListReservations :: ListReservations -> TestTree
requestListReservations =
  req
    "ListReservations"
    "fixture/ListReservations.yaml"

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings =
  req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

-- Responses

responseRemoveFlowVpcInterface :: RemoveFlowVpcInterfaceResponse -> TestTree
responseRemoveFlowVpcInterface =
  res
    "RemoveFlowVpcInterfaceResponse"
    "fixture/RemoveFlowVpcInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFlowVpcInterface)

responseRemoveFlowMediaStream :: RemoveFlowMediaStreamResponse -> TestTree
responseRemoveFlowMediaStream =
  res
    "RemoveFlowMediaStreamResponse"
    "fixture/RemoveFlowMediaStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFlowMediaStream)

responseUpdateFlowOutput :: UpdateFlowOutputResponse -> TestTree
responseUpdateFlowOutput =
  res
    "UpdateFlowOutputResponse"
    "fixture/UpdateFlowOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlowOutput)

responseAddFlowOutputs :: AddFlowOutputsResponse -> TestTree
responseAddFlowOutputs =
  res
    "AddFlowOutputsResponse"
    "fixture/AddFlowOutputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddFlowOutputs)

responseStartFlow :: StartFlowResponse -> TestTree
responseStartFlow =
  res
    "StartFlowResponse"
    "fixture/StartFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFlow)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseAddFlowSources :: AddFlowSourcesResponse -> TestTree
responseAddFlowSources =
  res
    "AddFlowSourcesResponse"
    "fixture/AddFlowSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddFlowSources)

responseDescribeReservation :: DescribeReservationResponse -> TestTree
responseDescribeReservation =
  res
    "DescribeReservationResponse"
    "fixture/DescribeReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservation)

responseAddFlowMediaStreams :: AddFlowMediaStreamsResponse -> TestTree
responseAddFlowMediaStreams =
  res
    "AddFlowMediaStreamsResponse"
    "fixture/AddFlowMediaStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddFlowMediaStreams)

responseRemoveFlowOutput :: RemoveFlowOutputResponse -> TestTree
responseRemoveFlowOutput =
  res
    "RemoveFlowOutputResponse"
    "fixture/RemoveFlowOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFlowOutput)

responseRevokeFlowEntitlement :: RevokeFlowEntitlementResponse -> TestTree
responseRevokeFlowEntitlement =
  res
    "RevokeFlowEntitlementResponse"
    "fixture/RevokeFlowEntitlementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeFlowEntitlement)

responseCreateFlow :: CreateFlowResponse -> TestTree
responseCreateFlow =
  res
    "CreateFlowResponse"
    "fixture/CreateFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFlow)

responseRemoveFlowSource :: RemoveFlowSourceResponse -> TestTree
responseRemoveFlowSource =
  res
    "RemoveFlowSourceResponse"
    "fixture/RemoveFlowSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFlowSource)

responseDescribeFlow :: DescribeFlowResponse -> TestTree
responseDescribeFlow =
  res
    "DescribeFlowResponse"
    "fixture/DescribeFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlow)

responseUpdateFlowEntitlement :: UpdateFlowEntitlementResponse -> TestTree
responseUpdateFlowEntitlement =
  res
    "UpdateFlowEntitlementResponse"
    "fixture/UpdateFlowEntitlementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlowEntitlement)

responseStopFlow :: StopFlowResponse -> TestTree
responseStopFlow =
  res
    "StopFlowResponse"
    "fixture/StopFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopFlow)

responseDescribeOffering :: DescribeOfferingResponse -> TestTree
responseDescribeOffering =
  res
    "DescribeOfferingResponse"
    "fixture/DescribeOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOffering)

responseAddFlowVpcInterfaces :: AddFlowVpcInterfacesResponse -> TestTree
responseAddFlowVpcInterfaces =
  res
    "AddFlowVpcInterfacesResponse"
    "fixture/AddFlowVpcInterfacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddFlowVpcInterfaces)

responseListEntitlements :: ListEntitlementsResponse -> TestTree
responseListEntitlements =
  res
    "ListEntitlementsResponse"
    "fixture/ListEntitlementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEntitlements)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListFlows :: ListFlowsResponse -> TestTree
responseListFlows =
  res
    "ListFlowsResponse"
    "fixture/ListFlowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFlows)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseOffering)

responseUpdateFlowMediaStream :: UpdateFlowMediaStreamResponse -> TestTree
responseUpdateFlowMediaStream =
  res
    "UpdateFlowMediaStreamResponse"
    "fixture/UpdateFlowMediaStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlowMediaStream)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateFlow :: UpdateFlowResponse -> TestTree
responseUpdateFlow =
  res
    "UpdateFlowResponse"
    "fixture/UpdateFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlow)

responseDeleteFlow :: DeleteFlowResponse -> TestTree
responseDeleteFlow =
  res
    "DeleteFlowResponse"
    "fixture/DeleteFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlow)

responseUpdateFlowSource :: UpdateFlowSourceResponse -> TestTree
responseUpdateFlowSource =
  res
    "UpdateFlowSourceResponse"
    "fixture/UpdateFlowSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlowSource)

responseGrantFlowEntitlements :: GrantFlowEntitlementsResponse -> TestTree
responseGrantFlowEntitlements =
  res
    "GrantFlowEntitlementsResponse"
    "fixture/GrantFlowEntitlementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GrantFlowEntitlements)

responseListReservations :: ListReservationsResponse -> TestTree
responseListReservations =
  res
    "ListReservationsResponse"
    "fixture/ListReservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReservations)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOfferings)

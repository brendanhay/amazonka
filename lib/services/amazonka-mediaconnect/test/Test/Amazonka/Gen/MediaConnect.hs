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
--         [ requestAddFlowMediaStreams $
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
--         , requestCreateFlow $
--             newCreateFlow
--
--         , requestDeleteFlow $
--             newDeleteFlow
--
--         , requestDescribeFlow $
--             newDescribeFlow
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
--         , requestListEntitlements $
--             newListEntitlements
--
--         , requestListFlows $
--             newListFlows
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
--           ]

--     , testGroup "response"
--         [ responseAddFlowMediaStreams $
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
--         , responseCreateFlow $
--             newCreateFlowResponse
--
--         , responseDeleteFlow $
--             newDeleteFlowResponse
--
--         , responseDescribeFlow $
--             newDescribeFlowResponse
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
--         , responseListEntitlements $
--             newListEntitlementsResponse
--
--         , responseListFlows $
--             newListFlowsResponse
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
--           ]
--     ]

-- Requests

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

requestCreateFlow :: CreateFlow -> TestTree
requestCreateFlow =
  req
    "CreateFlow"
    "fixture/CreateFlow.yaml"

requestDeleteFlow :: DeleteFlow -> TestTree
requestDeleteFlow =
  req
    "DeleteFlow"
    "fixture/DeleteFlow.yaml"

requestDescribeFlow :: DescribeFlow -> TestTree
requestDescribeFlow =
  req
    "DescribeFlow"
    "fixture/DescribeFlow.yaml"

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

-- Responses

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

responseCreateFlow :: CreateFlowResponse -> TestTree
responseCreateFlow =
  res
    "CreateFlowResponse"
    "fixture/CreateFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFlow)

responseDeleteFlow :: DeleteFlowResponse -> TestTree
responseDeleteFlow =
  res
    "DeleteFlowResponse"
    "fixture/DeleteFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlow)

responseDescribeFlow :: DescribeFlowResponse -> TestTree
responseDescribeFlow =
  res
    "DescribeFlowResponse"
    "fixture/DescribeFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlow)

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

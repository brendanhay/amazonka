{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.PrivateNetworks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.PrivateNetworks where

import Amazonka.PrivateNetworks
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.PrivateNetworks.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcknowledgeOrderReceipt $
--             newAcknowledgeOrderReceipt
--
--         , requestActivateDeviceIdentifier $
--             newActivateDeviceIdentifier
--
--         , requestActivateNetworkSite $
--             newActivateNetworkSite
--
--         , requestConfigureAccessPoint $
--             newConfigureAccessPoint
--
--         , requestCreateNetwork $
--             newCreateNetwork
--
--         , requestCreateNetworkSite $
--             newCreateNetworkSite
--
--         , requestDeactivateDeviceIdentifier $
--             newDeactivateDeviceIdentifier
--
--         , requestDeleteNetwork $
--             newDeleteNetwork
--
--         , requestDeleteNetworkSite $
--             newDeleteNetworkSite
--
--         , requestGetDeviceIdentifier $
--             newGetDeviceIdentifier
--
--         , requestGetNetwork $
--             newGetNetwork
--
--         , requestGetNetworkResource $
--             newGetNetworkResource
--
--         , requestGetNetworkSite $
--             newGetNetworkSite
--
--         , requestGetOrder $
--             newGetOrder
--
--         , requestListDeviceIdentifiers $
--             newListDeviceIdentifiers
--
--         , requestListNetworkResources $
--             newListNetworkResources
--
--         , requestListNetworkSites $
--             newListNetworkSites
--
--         , requestListNetworks $
--             newListNetworks
--
--         , requestListOrders $
--             newListOrders
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPing $
--             newPing
--
--         , requestStartNetworkResourceUpdate $
--             newStartNetworkResourceUpdate
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateNetworkSite $
--             newUpdateNetworkSite
--
--         , requestUpdateNetworkSitePlan $
--             newUpdateNetworkSitePlan
--
--           ]

--     , testGroup "response"
--         [ responseAcknowledgeOrderReceipt $
--             newAcknowledgeOrderReceiptResponse
--
--         , responseActivateDeviceIdentifier $
--             newActivateDeviceIdentifierResponse
--
--         , responseActivateNetworkSite $
--             newActivateNetworkSiteResponse
--
--         , responseConfigureAccessPoint $
--             newConfigureAccessPointResponse
--
--         , responseCreateNetwork $
--             newCreateNetworkResponse
--
--         , responseCreateNetworkSite $
--             newCreateNetworkSiteResponse
--
--         , responseDeactivateDeviceIdentifier $
--             newDeactivateDeviceIdentifierResponse
--
--         , responseDeleteNetwork $
--             newDeleteNetworkResponse
--
--         , responseDeleteNetworkSite $
--             newDeleteNetworkSiteResponse
--
--         , responseGetDeviceIdentifier $
--             newGetDeviceIdentifierResponse
--
--         , responseGetNetwork $
--             newGetNetworkResponse
--
--         , responseGetNetworkResource $
--             newGetNetworkResourceResponse
--
--         , responseGetNetworkSite $
--             newGetNetworkSiteResponse
--
--         , responseGetOrder $
--             newGetOrderResponse
--
--         , responseListDeviceIdentifiers $
--             newListDeviceIdentifiersResponse
--
--         , responseListNetworkResources $
--             newListNetworkResourcesResponse
--
--         , responseListNetworkSites $
--             newListNetworkSitesResponse
--
--         , responseListNetworks $
--             newListNetworksResponse
--
--         , responseListOrders $
--             newListOrdersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePing $
--             newPingResponse
--
--         , responseStartNetworkResourceUpdate $
--             newStartNetworkResourceUpdateResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateNetworkSite $
--             newUpdateNetworkSiteResponse
--
--         , responseUpdateNetworkSitePlan $
--             newUpdateNetworkSiteResponse
--
--           ]
--     ]

-- Requests

requestAcknowledgeOrderReceipt :: AcknowledgeOrderReceipt -> TestTree
requestAcknowledgeOrderReceipt =
  req
    "AcknowledgeOrderReceipt"
    "fixture/AcknowledgeOrderReceipt.yaml"

requestActivateDeviceIdentifier :: ActivateDeviceIdentifier -> TestTree
requestActivateDeviceIdentifier =
  req
    "ActivateDeviceIdentifier"
    "fixture/ActivateDeviceIdentifier.yaml"

requestActivateNetworkSite :: ActivateNetworkSite -> TestTree
requestActivateNetworkSite =
  req
    "ActivateNetworkSite"
    "fixture/ActivateNetworkSite.yaml"

requestConfigureAccessPoint :: ConfigureAccessPoint -> TestTree
requestConfigureAccessPoint =
  req
    "ConfigureAccessPoint"
    "fixture/ConfigureAccessPoint.yaml"

requestCreateNetwork :: CreateNetwork -> TestTree
requestCreateNetwork =
  req
    "CreateNetwork"
    "fixture/CreateNetwork.yaml"

requestCreateNetworkSite :: CreateNetworkSite -> TestTree
requestCreateNetworkSite =
  req
    "CreateNetworkSite"
    "fixture/CreateNetworkSite.yaml"

requestDeactivateDeviceIdentifier :: DeactivateDeviceIdentifier -> TestTree
requestDeactivateDeviceIdentifier =
  req
    "DeactivateDeviceIdentifier"
    "fixture/DeactivateDeviceIdentifier.yaml"

requestDeleteNetwork :: DeleteNetwork -> TestTree
requestDeleteNetwork =
  req
    "DeleteNetwork"
    "fixture/DeleteNetwork.yaml"

requestDeleteNetworkSite :: DeleteNetworkSite -> TestTree
requestDeleteNetworkSite =
  req
    "DeleteNetworkSite"
    "fixture/DeleteNetworkSite.yaml"

requestGetDeviceIdentifier :: GetDeviceIdentifier -> TestTree
requestGetDeviceIdentifier =
  req
    "GetDeviceIdentifier"
    "fixture/GetDeviceIdentifier.yaml"

requestGetNetwork :: GetNetwork -> TestTree
requestGetNetwork =
  req
    "GetNetwork"
    "fixture/GetNetwork.yaml"

requestGetNetworkResource :: GetNetworkResource -> TestTree
requestGetNetworkResource =
  req
    "GetNetworkResource"
    "fixture/GetNetworkResource.yaml"

requestGetNetworkSite :: GetNetworkSite -> TestTree
requestGetNetworkSite =
  req
    "GetNetworkSite"
    "fixture/GetNetworkSite.yaml"

requestGetOrder :: GetOrder -> TestTree
requestGetOrder =
  req
    "GetOrder"
    "fixture/GetOrder.yaml"

requestListDeviceIdentifiers :: ListDeviceIdentifiers -> TestTree
requestListDeviceIdentifiers =
  req
    "ListDeviceIdentifiers"
    "fixture/ListDeviceIdentifiers.yaml"

requestListNetworkResources :: ListNetworkResources -> TestTree
requestListNetworkResources =
  req
    "ListNetworkResources"
    "fixture/ListNetworkResources.yaml"

requestListNetworkSites :: ListNetworkSites -> TestTree
requestListNetworkSites =
  req
    "ListNetworkSites"
    "fixture/ListNetworkSites.yaml"

requestListNetworks :: ListNetworks -> TestTree
requestListNetworks =
  req
    "ListNetworks"
    "fixture/ListNetworks.yaml"

requestListOrders :: ListOrders -> TestTree
requestListOrders =
  req
    "ListOrders"
    "fixture/ListOrders.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPing :: Ping -> TestTree
requestPing =
  req
    "Ping"
    "fixture/Ping.yaml"

requestStartNetworkResourceUpdate :: StartNetworkResourceUpdate -> TestTree
requestStartNetworkResourceUpdate =
  req
    "StartNetworkResourceUpdate"
    "fixture/StartNetworkResourceUpdate.yaml"

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

requestUpdateNetworkSite :: UpdateNetworkSite -> TestTree
requestUpdateNetworkSite =
  req
    "UpdateNetworkSite"
    "fixture/UpdateNetworkSite.yaml"

requestUpdateNetworkSitePlan :: UpdateNetworkSitePlan -> TestTree
requestUpdateNetworkSitePlan =
  req
    "UpdateNetworkSitePlan"
    "fixture/UpdateNetworkSitePlan.yaml"

-- Responses

responseAcknowledgeOrderReceipt :: AcknowledgeOrderReceiptResponse -> TestTree
responseAcknowledgeOrderReceipt =
  res
    "AcknowledgeOrderReceiptResponse"
    "fixture/AcknowledgeOrderReceiptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcknowledgeOrderReceipt)

responseActivateDeviceIdentifier :: ActivateDeviceIdentifierResponse -> TestTree
responseActivateDeviceIdentifier =
  res
    "ActivateDeviceIdentifierResponse"
    "fixture/ActivateDeviceIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateDeviceIdentifier)

responseActivateNetworkSite :: ActivateNetworkSiteResponse -> TestTree
responseActivateNetworkSite =
  res
    "ActivateNetworkSiteResponse"
    "fixture/ActivateNetworkSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateNetworkSite)

responseConfigureAccessPoint :: ConfigureAccessPointResponse -> TestTree
responseConfigureAccessPoint =
  res
    "ConfigureAccessPointResponse"
    "fixture/ConfigureAccessPointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfigureAccessPoint)

responseCreateNetwork :: CreateNetworkResponse -> TestTree
responseCreateNetwork =
  res
    "CreateNetworkResponse"
    "fixture/CreateNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetwork)

responseCreateNetworkSite :: CreateNetworkSiteResponse -> TestTree
responseCreateNetworkSite =
  res
    "CreateNetworkSiteResponse"
    "fixture/CreateNetworkSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkSite)

responseDeactivateDeviceIdentifier :: DeactivateDeviceIdentifierResponse -> TestTree
responseDeactivateDeviceIdentifier =
  res
    "DeactivateDeviceIdentifierResponse"
    "fixture/DeactivateDeviceIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateDeviceIdentifier)

responseDeleteNetwork :: DeleteNetworkResponse -> TestTree
responseDeleteNetwork =
  res
    "DeleteNetworkResponse"
    "fixture/DeleteNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetwork)

responseDeleteNetworkSite :: DeleteNetworkSiteResponse -> TestTree
responseDeleteNetworkSite =
  res
    "DeleteNetworkSiteResponse"
    "fixture/DeleteNetworkSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkSite)

responseGetDeviceIdentifier :: GetDeviceIdentifierResponse -> TestTree
responseGetDeviceIdentifier =
  res
    "GetDeviceIdentifierResponse"
    "fixture/GetDeviceIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceIdentifier)

responseGetNetwork :: GetNetworkResponse -> TestTree
responseGetNetwork =
  res
    "GetNetworkResponse"
    "fixture/GetNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetwork)

responseGetNetworkResource :: GetNetworkResourceResponse -> TestTree
responseGetNetworkResource =
  res
    "GetNetworkResourceResponse"
    "fixture/GetNetworkResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkResource)

responseGetNetworkSite :: GetNetworkSiteResponse -> TestTree
responseGetNetworkSite =
  res
    "GetNetworkSiteResponse"
    "fixture/GetNetworkSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkSite)

responseGetOrder :: GetOrderResponse -> TestTree
responseGetOrder =
  res
    "GetOrderResponse"
    "fixture/GetOrderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOrder)

responseListDeviceIdentifiers :: ListDeviceIdentifiersResponse -> TestTree
responseListDeviceIdentifiers =
  res
    "ListDeviceIdentifiersResponse"
    "fixture/ListDeviceIdentifiersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceIdentifiers)

responseListNetworkResources :: ListNetworkResourcesResponse -> TestTree
responseListNetworkResources =
  res
    "ListNetworkResourcesResponse"
    "fixture/ListNetworkResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNetworkResources)

responseListNetworkSites :: ListNetworkSitesResponse -> TestTree
responseListNetworkSites =
  res
    "ListNetworkSitesResponse"
    "fixture/ListNetworkSitesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNetworkSites)

responseListNetworks :: ListNetworksResponse -> TestTree
responseListNetworks =
  res
    "ListNetworksResponse"
    "fixture/ListNetworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNetworks)

responseListOrders :: ListOrdersResponse -> TestTree
responseListOrders =
  res
    "ListOrdersResponse"
    "fixture/ListOrdersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrders)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePing :: PingResponse -> TestTree
responsePing =
  res
    "PingResponse"
    "fixture/PingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Ping)

responseStartNetworkResourceUpdate :: StartNetworkResourceUpdateResponse -> TestTree
responseStartNetworkResourceUpdate =
  res
    "StartNetworkResourceUpdateResponse"
    "fixture/StartNetworkResourceUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartNetworkResourceUpdate)

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

responseUpdateNetworkSite :: UpdateNetworkSiteResponse -> TestTree
responseUpdateNetworkSite =
  res
    "UpdateNetworkSiteResponse"
    "fixture/UpdateNetworkSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNetworkSite)

responseUpdateNetworkSitePlan :: UpdateNetworkSiteResponse -> TestTree
responseUpdateNetworkSitePlan =
  res
    "UpdateNetworkSitePlanResponse"
    "fixture/UpdateNetworkSitePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNetworkSitePlan)

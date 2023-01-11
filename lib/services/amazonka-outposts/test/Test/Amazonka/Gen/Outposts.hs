{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Outposts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Outposts where

import Amazonka.Outposts
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Outposts.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelOrder $
--             newCancelOrder
--
--         , requestCreateOrder $
--             newCreateOrder
--
--         , requestCreateOutpost $
--             newCreateOutpost
--
--         , requestCreateSite $
--             newCreateSite
--
--         , requestDeleteOutpost $
--             newDeleteOutpost
--
--         , requestDeleteSite $
--             newDeleteSite
--
--         , requestGetCatalogItem $
--             newGetCatalogItem
--
--         , requestGetConnection $
--             newGetConnection
--
--         , requestGetOrder $
--             newGetOrder
--
--         , requestGetOutpost $
--             newGetOutpost
--
--         , requestGetOutpostInstanceTypes $
--             newGetOutpostInstanceTypes
--
--         , requestGetSite $
--             newGetSite
--
--         , requestGetSiteAddress $
--             newGetSiteAddress
--
--         , requestListAssets $
--             newListAssets
--
--         , requestListCatalogItems $
--             newListCatalogItems
--
--         , requestListOrders $
--             newListOrders
--
--         , requestListOutposts $
--             newListOutposts
--
--         , requestListSites $
--             newListSites
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartConnection $
--             newStartConnection
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateOutpost $
--             newUpdateOutpost
--
--         , requestUpdateSite $
--             newUpdateSite
--
--         , requestUpdateSiteAddress $
--             newUpdateSiteAddress
--
--         , requestUpdateSiteRackPhysicalProperties $
--             newUpdateSiteRackPhysicalProperties
--
--           ]

--     , testGroup "response"
--         [ responseCancelOrder $
--             newCancelOrderResponse
--
--         , responseCreateOrder $
--             newCreateOrderResponse
--
--         , responseCreateOutpost $
--             newCreateOutpostResponse
--
--         , responseCreateSite $
--             newCreateSiteResponse
--
--         , responseDeleteOutpost $
--             newDeleteOutpostResponse
--
--         , responseDeleteSite $
--             newDeleteSiteResponse
--
--         , responseGetCatalogItem $
--             newGetCatalogItemResponse
--
--         , responseGetConnection $
--             newGetConnectionResponse
--
--         , responseGetOrder $
--             newGetOrderResponse
--
--         , responseGetOutpost $
--             newGetOutpostResponse
--
--         , responseGetOutpostInstanceTypes $
--             newGetOutpostInstanceTypesResponse
--
--         , responseGetSite $
--             newGetSiteResponse
--
--         , responseGetSiteAddress $
--             newGetSiteAddressResponse
--
--         , responseListAssets $
--             newListAssetsResponse
--
--         , responseListCatalogItems $
--             newListCatalogItemsResponse
--
--         , responseListOrders $
--             newListOrdersResponse
--
--         , responseListOutposts $
--             newListOutpostsResponse
--
--         , responseListSites $
--             newListSitesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartConnection $
--             newStartConnectionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateOutpost $
--             newUpdateOutpostResponse
--
--         , responseUpdateSite $
--             newUpdateSiteResponse
--
--         , responseUpdateSiteAddress $
--             newUpdateSiteAddressResponse
--
--         , responseUpdateSiteRackPhysicalProperties $
--             newUpdateSiteRackPhysicalPropertiesResponse
--
--           ]
--     ]

-- Requests

requestCancelOrder :: CancelOrder -> TestTree
requestCancelOrder =
  req
    "CancelOrder"
    "fixture/CancelOrder.yaml"

requestCreateOrder :: CreateOrder -> TestTree
requestCreateOrder =
  req
    "CreateOrder"
    "fixture/CreateOrder.yaml"

requestCreateOutpost :: CreateOutpost -> TestTree
requestCreateOutpost =
  req
    "CreateOutpost"
    "fixture/CreateOutpost.yaml"

requestCreateSite :: CreateSite -> TestTree
requestCreateSite =
  req
    "CreateSite"
    "fixture/CreateSite.yaml"

requestDeleteOutpost :: DeleteOutpost -> TestTree
requestDeleteOutpost =
  req
    "DeleteOutpost"
    "fixture/DeleteOutpost.yaml"

requestDeleteSite :: DeleteSite -> TestTree
requestDeleteSite =
  req
    "DeleteSite"
    "fixture/DeleteSite.yaml"

requestGetCatalogItem :: GetCatalogItem -> TestTree
requestGetCatalogItem =
  req
    "GetCatalogItem"
    "fixture/GetCatalogItem.yaml"

requestGetConnection :: GetConnection -> TestTree
requestGetConnection =
  req
    "GetConnection"
    "fixture/GetConnection.yaml"

requestGetOrder :: GetOrder -> TestTree
requestGetOrder =
  req
    "GetOrder"
    "fixture/GetOrder.yaml"

requestGetOutpost :: GetOutpost -> TestTree
requestGetOutpost =
  req
    "GetOutpost"
    "fixture/GetOutpost.yaml"

requestGetOutpostInstanceTypes :: GetOutpostInstanceTypes -> TestTree
requestGetOutpostInstanceTypes =
  req
    "GetOutpostInstanceTypes"
    "fixture/GetOutpostInstanceTypes.yaml"

requestGetSite :: GetSite -> TestTree
requestGetSite =
  req
    "GetSite"
    "fixture/GetSite.yaml"

requestGetSiteAddress :: GetSiteAddress -> TestTree
requestGetSiteAddress =
  req
    "GetSiteAddress"
    "fixture/GetSiteAddress.yaml"

requestListAssets :: ListAssets -> TestTree
requestListAssets =
  req
    "ListAssets"
    "fixture/ListAssets.yaml"

requestListCatalogItems :: ListCatalogItems -> TestTree
requestListCatalogItems =
  req
    "ListCatalogItems"
    "fixture/ListCatalogItems.yaml"

requestListOrders :: ListOrders -> TestTree
requestListOrders =
  req
    "ListOrders"
    "fixture/ListOrders.yaml"

requestListOutposts :: ListOutposts -> TestTree
requestListOutposts =
  req
    "ListOutposts"
    "fixture/ListOutposts.yaml"

requestListSites :: ListSites -> TestTree
requestListSites =
  req
    "ListSites"
    "fixture/ListSites.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartConnection :: StartConnection -> TestTree
requestStartConnection =
  req
    "StartConnection"
    "fixture/StartConnection.yaml"

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

requestUpdateOutpost :: UpdateOutpost -> TestTree
requestUpdateOutpost =
  req
    "UpdateOutpost"
    "fixture/UpdateOutpost.yaml"

requestUpdateSite :: UpdateSite -> TestTree
requestUpdateSite =
  req
    "UpdateSite"
    "fixture/UpdateSite.yaml"

requestUpdateSiteAddress :: UpdateSiteAddress -> TestTree
requestUpdateSiteAddress =
  req
    "UpdateSiteAddress"
    "fixture/UpdateSiteAddress.yaml"

requestUpdateSiteRackPhysicalProperties :: UpdateSiteRackPhysicalProperties -> TestTree
requestUpdateSiteRackPhysicalProperties =
  req
    "UpdateSiteRackPhysicalProperties"
    "fixture/UpdateSiteRackPhysicalProperties.yaml"

-- Responses

responseCancelOrder :: CancelOrderResponse -> TestTree
responseCancelOrder =
  res
    "CancelOrderResponse"
    "fixture/CancelOrderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelOrder)

responseCreateOrder :: CreateOrderResponse -> TestTree
responseCreateOrder =
  res
    "CreateOrderResponse"
    "fixture/CreateOrderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOrder)

responseCreateOutpost :: CreateOutpostResponse -> TestTree
responseCreateOutpost =
  res
    "CreateOutpostResponse"
    "fixture/CreateOutpostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOutpost)

responseCreateSite :: CreateSiteResponse -> TestTree
responseCreateSite =
  res
    "CreateSiteResponse"
    "fixture/CreateSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSite)

responseDeleteOutpost :: DeleteOutpostResponse -> TestTree
responseDeleteOutpost =
  res
    "DeleteOutpostResponse"
    "fixture/DeleteOutpostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOutpost)

responseDeleteSite :: DeleteSiteResponse -> TestTree
responseDeleteSite =
  res
    "DeleteSiteResponse"
    "fixture/DeleteSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSite)

responseGetCatalogItem :: GetCatalogItemResponse -> TestTree
responseGetCatalogItem =
  res
    "GetCatalogItemResponse"
    "fixture/GetCatalogItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCatalogItem)

responseGetConnection :: GetConnectionResponse -> TestTree
responseGetConnection =
  res
    "GetConnectionResponse"
    "fixture/GetConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnection)

responseGetOrder :: GetOrderResponse -> TestTree
responseGetOrder =
  res
    "GetOrderResponse"
    "fixture/GetOrderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOrder)

responseGetOutpost :: GetOutpostResponse -> TestTree
responseGetOutpost =
  res
    "GetOutpostResponse"
    "fixture/GetOutpostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOutpost)

responseGetOutpostInstanceTypes :: GetOutpostInstanceTypesResponse -> TestTree
responseGetOutpostInstanceTypes =
  res
    "GetOutpostInstanceTypesResponse"
    "fixture/GetOutpostInstanceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOutpostInstanceTypes)

responseGetSite :: GetSiteResponse -> TestTree
responseGetSite =
  res
    "GetSiteResponse"
    "fixture/GetSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSite)

responseGetSiteAddress :: GetSiteAddressResponse -> TestTree
responseGetSiteAddress =
  res
    "GetSiteAddressResponse"
    "fixture/GetSiteAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSiteAddress)

responseListAssets :: ListAssetsResponse -> TestTree
responseListAssets =
  res
    "ListAssetsResponse"
    "fixture/ListAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssets)

responseListCatalogItems :: ListCatalogItemsResponse -> TestTree
responseListCatalogItems =
  res
    "ListCatalogItemsResponse"
    "fixture/ListCatalogItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCatalogItems)

responseListOrders :: ListOrdersResponse -> TestTree
responseListOrders =
  res
    "ListOrdersResponse"
    "fixture/ListOrdersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrders)

responseListOutposts :: ListOutpostsResponse -> TestTree
responseListOutposts =
  res
    "ListOutpostsResponse"
    "fixture/ListOutpostsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOutposts)

responseListSites :: ListSitesResponse -> TestTree
responseListSites =
  res
    "ListSitesResponse"
    "fixture/ListSitesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSites)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartConnection :: StartConnectionResponse -> TestTree
responseStartConnection =
  res
    "StartConnectionResponse"
    "fixture/StartConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartConnection)

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

responseUpdateOutpost :: UpdateOutpostResponse -> TestTree
responseUpdateOutpost =
  res
    "UpdateOutpostResponse"
    "fixture/UpdateOutpostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOutpost)

responseUpdateSite :: UpdateSiteResponse -> TestTree
responseUpdateSite =
  res
    "UpdateSiteResponse"
    "fixture/UpdateSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSite)

responseUpdateSiteAddress :: UpdateSiteAddressResponse -> TestTree
responseUpdateSiteAddress =
  res
    "UpdateSiteAddressResponse"
    "fixture/UpdateSiteAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSiteAddress)

responseUpdateSiteRackPhysicalProperties :: UpdateSiteRackPhysicalPropertiesResponse -> TestTree
responseUpdateSiteRackPhysicalProperties =
  res
    "UpdateSiteRackPhysicalPropertiesResponse"
    "fixture/UpdateSiteRackPhysicalPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSiteRackPhysicalProperties)

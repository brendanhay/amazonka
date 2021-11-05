{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Outposts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Outposts where

import qualified Data.Proxy as Proxy
import Network.AWS.Outposts
import Test.AWS.Fixture
import Test.AWS.Outposts.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteOutpost $
--             newDeleteOutpost
--
--         , requestDeleteSite $
--             newDeleteSite
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListOutposts $
--             newListOutposts
--
--         , requestListSites $
--             newListSites
--
--         , requestCreateOrder $
--             newCreateOrder
--
--         , requestGetOutpostInstanceTypes $
--             newGetOutpostInstanceTypes
--
--         , requestCreateOutpost $
--             newCreateOutpost
--
--         , requestGetOutpost $
--             newGetOutpost
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseDeleteOutpost $
--             newDeleteOutpostResponse
--
--         , responseDeleteSite $
--             newDeleteSiteResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListOutposts $
--             newListOutpostsResponse
--
--         , responseListSites $
--             newListSitesResponse
--
--         , responseCreateOrder $
--             newCreateOrderResponse
--
--         , responseGetOutpostInstanceTypes $
--             newGetOutpostInstanceTypesResponse
--
--         , responseCreateOutpost $
--             newCreateOutpostResponse
--
--         , responseGetOutpost $
--             newGetOutpostResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestCreateOrder :: CreateOrder -> TestTree
requestCreateOrder =
  req
    "CreateOrder"
    "fixture/CreateOrder.yaml"

requestGetOutpostInstanceTypes :: GetOutpostInstanceTypes -> TestTree
requestGetOutpostInstanceTypes =
  req
    "GetOutpostInstanceTypes"
    "fixture/GetOutpostInstanceTypes.yaml"

requestCreateOutpost :: CreateOutpost -> TestTree
requestCreateOutpost =
  req
    "CreateOutpost"
    "fixture/CreateOutpost.yaml"

requestGetOutpost :: GetOutpost -> TestTree
requestGetOutpost =
  req
    "GetOutpost"
    "fixture/GetOutpost.yaml"

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

-- Responses

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

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseCreateOrder :: CreateOrderResponse -> TestTree
responseCreateOrder =
  res
    "CreateOrderResponse"
    "fixture/CreateOrderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOrder)

responseGetOutpostInstanceTypes :: GetOutpostInstanceTypesResponse -> TestTree
responseGetOutpostInstanceTypes =
  res
    "GetOutpostInstanceTypesResponse"
    "fixture/GetOutpostInstanceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOutpostInstanceTypes)

responseCreateOutpost :: CreateOutpostResponse -> TestTree
responseCreateOutpost =
  res
    "CreateOutpostResponse"
    "fixture/CreateOutpostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOutpost)

responseGetOutpost :: GetOutpostResponse -> TestTree
responseGetOutpost =
  res
    "GetOutpostResponse"
    "fixture/GetOutpostResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOutpost)

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

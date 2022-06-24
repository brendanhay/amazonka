{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Outposts
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestCreateOrder $
--             newCreateOrder
--
--         , requestCreateOutpost $
--             newCreateOutpost
--
--         , requestDeleteOutpost $
--             newDeleteOutpost
--
--         , requestDeleteSite $
--             newDeleteSite
--
--         , requestGetOutpost $
--             newGetOutpost
--
--         , requestGetOutpostInstanceTypes $
--             newGetOutpostInstanceTypes
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
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCreateOrder $
--             newCreateOrderResponse
--
--         , responseCreateOutpost $
--             newCreateOutpostResponse
--
--         , responseDeleteOutpost $
--             newDeleteOutpostResponse
--
--         , responseDeleteSite $
--             newDeleteSiteResponse
--
--         , responseGetOutpost $
--             newGetOutpostResponse
--
--         , responseGetOutpostInstanceTypes $
--             newGetOutpostInstanceTypesResponse
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
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

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

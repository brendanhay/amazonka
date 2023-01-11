{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudHSM
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudHSM where

import Amazonka.CloudHSM
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudHSM.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestCreateHapg $
--             newCreateHapg
--
--         , requestCreateHsm $
--             newCreateHsm
--
--         , requestCreateLunaClient $
--             newCreateLunaClient
--
--         , requestDeleteHapg $
--             newDeleteHapg
--
--         , requestDeleteHsm $
--             newDeleteHsm
--
--         , requestDeleteLunaClient $
--             newDeleteLunaClient
--
--         , requestDescribeHapg $
--             newDescribeHapg
--
--         , requestDescribeHsm $
--             newDescribeHsm
--
--         , requestDescribeLunaClient $
--             newDescribeLunaClient
--
--         , requestGetConfig $
--             newGetConfig
--
--         , requestListAvailableZones $
--             newListAvailableZones
--
--         , requestListHapgs $
--             newListHapgs
--
--         , requestListHsms $
--             newListHsms
--
--         , requestListLunaClients $
--             newListLunaClients
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestModifyHapg $
--             newModifyHapg
--
--         , requestModifyHsm $
--             newModifyHsm
--
--         , requestModifyLunaClient $
--             newModifyLunaClient
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--           ]

--     , testGroup "response"
--         [ responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseCreateHapg $
--             newCreateHapgResponse
--
--         , responseCreateHsm $
--             newCreateHsmResponse
--
--         , responseCreateLunaClient $
--             newCreateLunaClientResponse
--
--         , responseDeleteHapg $
--             newDeleteHapgResponse
--
--         , responseDeleteHsm $
--             newDeleteHsmResponse
--
--         , responseDeleteLunaClient $
--             newDeleteLunaClientResponse
--
--         , responseDescribeHapg $
--             newDescribeHapgResponse
--
--         , responseDescribeHsm $
--             newDescribeHsmResponse
--
--         , responseDescribeLunaClient $
--             newDescribeLunaClientResponse
--
--         , responseGetConfig $
--             newGetConfigResponse
--
--         , responseListAvailableZones $
--             newListAvailableZonesResponse
--
--         , responseListHapgs $
--             newListHapgsResponse
--
--         , responseListHsms $
--             newListHsmsResponse
--
--         , responseListLunaClients $
--             newListLunaClientsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseModifyHapg $
--             newModifyHapgResponse
--
--         , responseModifyHsm $
--             newModifyHsmResponse
--
--         , responseModifyLunaClient $
--             newModifyLunaClientResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--           ]
--     ]

-- Requests

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestCreateHapg :: CreateHapg -> TestTree
requestCreateHapg =
  req
    "CreateHapg"
    "fixture/CreateHapg.yaml"

requestCreateHsm :: CreateHsm -> TestTree
requestCreateHsm =
  req
    "CreateHsm"
    "fixture/CreateHsm.yaml"

requestCreateLunaClient :: CreateLunaClient -> TestTree
requestCreateLunaClient =
  req
    "CreateLunaClient"
    "fixture/CreateLunaClient.yaml"

requestDeleteHapg :: DeleteHapg -> TestTree
requestDeleteHapg =
  req
    "DeleteHapg"
    "fixture/DeleteHapg.yaml"

requestDeleteHsm :: DeleteHsm -> TestTree
requestDeleteHsm =
  req
    "DeleteHsm"
    "fixture/DeleteHsm.yaml"

requestDeleteLunaClient :: DeleteLunaClient -> TestTree
requestDeleteLunaClient =
  req
    "DeleteLunaClient"
    "fixture/DeleteLunaClient.yaml"

requestDescribeHapg :: DescribeHapg -> TestTree
requestDescribeHapg =
  req
    "DescribeHapg"
    "fixture/DescribeHapg.yaml"

requestDescribeHsm :: DescribeHsm -> TestTree
requestDescribeHsm =
  req
    "DescribeHsm"
    "fixture/DescribeHsm.yaml"

requestDescribeLunaClient :: DescribeLunaClient -> TestTree
requestDescribeLunaClient =
  req
    "DescribeLunaClient"
    "fixture/DescribeLunaClient.yaml"

requestGetConfig :: GetConfig -> TestTree
requestGetConfig =
  req
    "GetConfig"
    "fixture/GetConfig.yaml"

requestListAvailableZones :: ListAvailableZones -> TestTree
requestListAvailableZones =
  req
    "ListAvailableZones"
    "fixture/ListAvailableZones.yaml"

requestListHapgs :: ListHapgs -> TestTree
requestListHapgs =
  req
    "ListHapgs"
    "fixture/ListHapgs.yaml"

requestListHsms :: ListHsms -> TestTree
requestListHsms =
  req
    "ListHsms"
    "fixture/ListHsms.yaml"

requestListLunaClients :: ListLunaClients -> TestTree
requestListLunaClients =
  req
    "ListLunaClients"
    "fixture/ListLunaClients.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestModifyHapg :: ModifyHapg -> TestTree
requestModifyHapg =
  req
    "ModifyHapg"
    "fixture/ModifyHapg.yaml"

requestModifyHsm :: ModifyHsm -> TestTree
requestModifyHsm =
  req
    "ModifyHsm"
    "fixture/ModifyHsm.yaml"

requestModifyLunaClient :: ModifyLunaClient -> TestTree
requestModifyLunaClient =
  req
    "ModifyLunaClient"
    "fixture/ModifyLunaClient.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

-- Responses

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseCreateHapg :: CreateHapgResponse -> TestTree
responseCreateHapg =
  res
    "CreateHapgResponse"
    "fixture/CreateHapgResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHapg)

responseCreateHsm :: CreateHsmResponse -> TestTree
responseCreateHsm =
  res
    "CreateHsmResponse"
    "fixture/CreateHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHsm)

responseCreateLunaClient :: CreateLunaClientResponse -> TestTree
responseCreateLunaClient =
  res
    "CreateLunaClientResponse"
    "fixture/CreateLunaClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLunaClient)

responseDeleteHapg :: DeleteHapgResponse -> TestTree
responseDeleteHapg =
  res
    "DeleteHapgResponse"
    "fixture/DeleteHapgResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHapg)

responseDeleteHsm :: DeleteHsmResponse -> TestTree
responseDeleteHsm =
  res
    "DeleteHsmResponse"
    "fixture/DeleteHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHsm)

responseDeleteLunaClient :: DeleteLunaClientResponse -> TestTree
responseDeleteLunaClient =
  res
    "DeleteLunaClientResponse"
    "fixture/DeleteLunaClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLunaClient)

responseDescribeHapg :: DescribeHapgResponse -> TestTree
responseDescribeHapg =
  res
    "DescribeHapgResponse"
    "fixture/DescribeHapgResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHapg)

responseDescribeHsm :: DescribeHsmResponse -> TestTree
responseDescribeHsm =
  res
    "DescribeHsmResponse"
    "fixture/DescribeHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHsm)

responseDescribeLunaClient :: DescribeLunaClientResponse -> TestTree
responseDescribeLunaClient =
  res
    "DescribeLunaClientResponse"
    "fixture/DescribeLunaClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLunaClient)

responseGetConfig :: GetConfigResponse -> TestTree
responseGetConfig =
  res
    "GetConfigResponse"
    "fixture/GetConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfig)

responseListAvailableZones :: ListAvailableZonesResponse -> TestTree
responseListAvailableZones =
  res
    "ListAvailableZonesResponse"
    "fixture/ListAvailableZonesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAvailableZones)

responseListHapgs :: ListHapgsResponse -> TestTree
responseListHapgs =
  res
    "ListHapgsResponse"
    "fixture/ListHapgsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHapgs)

responseListHsms :: ListHsmsResponse -> TestTree
responseListHsms =
  res
    "ListHsmsResponse"
    "fixture/ListHsmsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHsms)

responseListLunaClients :: ListLunaClientsResponse -> TestTree
responseListLunaClients =
  res
    "ListLunaClientsResponse"
    "fixture/ListLunaClientsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLunaClients)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseModifyHapg :: ModifyHapgResponse -> TestTree
responseModifyHapg =
  res
    "ModifyHapgResponse"
    "fixture/ModifyHapgResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyHapg)

responseModifyHsm :: ModifyHsmResponse -> TestTree
responseModifyHsm =
  res
    "ModifyHsmResponse"
    "fixture/ModifyHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyHsm)

responseModifyLunaClient :: ModifyLunaClientResponse -> TestTree
responseModifyLunaClient =
  res
    "ModifyLunaClientResponse"
    "fixture/ModifyLunaClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyLunaClient)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudHSM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudHSM where

import Amazonka.CloudHSM
import qualified Data.Proxy as Proxy
import Test.AWS.CloudHSM.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteHapg $
--             newDeleteHapg
--
--         , requestListHapgs $
--             newListHapgs
--
--         , requestModifyLunaClient $
--             newModifyLunaClient
--
--         , requestListHsms $
--             newListHsms
--
--         , requestDescribeLunaClient $
--             newDescribeLunaClient
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateHapg $
--             newCreateHapg
--
--         , requestCreateHsm $
--             newCreateHsm
--
--         , requestRemoveTagsFromResource $
--             newRemoveTagsFromResource
--
--         , requestDescribeHapg $
--             newDescribeHapg
--
--         , requestCreateLunaClient $
--             newCreateLunaClient
--
--         , requestListLunaClients $
--             newListLunaClients
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestGetConfig $
--             newGetConfig
--
--         , requestDeleteHsm $
--             newDeleteHsm
--
--         , requestDescribeHsm $
--             newDescribeHsm
--
--         , requestModifyHapg $
--             newModifyHapg
--
--         , requestDeleteLunaClient $
--             newDeleteLunaClient
--
--         , requestModifyHsm $
--             newModifyHsm
--
--         , requestListAvailableZones $
--             newListAvailableZones
--
--           ]

--     , testGroup "response"
--         [ responseDeleteHapg $
--             newDeleteHapgResponse
--
--         , responseListHapgs $
--             newListHapgsResponse
--
--         , responseModifyLunaClient $
--             newModifyLunaClientResponse
--
--         , responseListHsms $
--             newListHsmsResponse
--
--         , responseDescribeLunaClient $
--             newDescribeLunaClientResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateHapg $
--             newCreateHapgResponse
--
--         , responseCreateHsm $
--             newCreateHsmResponse
--
--         , responseRemoveTagsFromResource $
--             newRemoveTagsFromResourceResponse
--
--         , responseDescribeHapg $
--             newDescribeHapgResponse
--
--         , responseCreateLunaClient $
--             newCreateLunaClientResponse
--
--         , responseListLunaClients $
--             newListLunaClientsResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseGetConfig $
--             newGetConfigResponse
--
--         , responseDeleteHsm $
--             newDeleteHsmResponse
--
--         , responseDescribeHsm $
--             newDescribeHsmResponse
--
--         , responseModifyHapg $
--             newModifyHapgResponse
--
--         , responseDeleteLunaClient $
--             newDeleteLunaClientResponse
--
--         , responseModifyHsm $
--             newModifyHsmResponse
--
--         , responseListAvailableZones $
--             newListAvailableZonesResponse
--
--           ]
--     ]

-- Requests

requestDeleteHapg :: DeleteHapg -> TestTree
requestDeleteHapg =
  req
    "DeleteHapg"
    "fixture/DeleteHapg.yaml"

requestListHapgs :: ListHapgs -> TestTree
requestListHapgs =
  req
    "ListHapgs"
    "fixture/ListHapgs.yaml"

requestModifyLunaClient :: ModifyLunaClient -> TestTree
requestModifyLunaClient =
  req
    "ModifyLunaClient"
    "fixture/ModifyLunaClient.yaml"

requestListHsms :: ListHsms -> TestTree
requestListHsms =
  req
    "ListHsms"
    "fixture/ListHsms.yaml"

requestDescribeLunaClient :: DescribeLunaClient -> TestTree
requestDescribeLunaClient =
  req
    "DescribeLunaClient"
    "fixture/DescribeLunaClient.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDescribeHapg :: DescribeHapg -> TestTree
requestDescribeHapg =
  req
    "DescribeHapg"
    "fixture/DescribeHapg.yaml"

requestCreateLunaClient :: CreateLunaClient -> TestTree
requestCreateLunaClient =
  req
    "CreateLunaClient"
    "fixture/CreateLunaClient.yaml"

requestListLunaClients :: ListLunaClients -> TestTree
requestListLunaClients =
  req
    "ListLunaClients"
    "fixture/ListLunaClients.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestGetConfig :: GetConfig -> TestTree
requestGetConfig =
  req
    "GetConfig"
    "fixture/GetConfig.yaml"

requestDeleteHsm :: DeleteHsm -> TestTree
requestDeleteHsm =
  req
    "DeleteHsm"
    "fixture/DeleteHsm.yaml"

requestDescribeHsm :: DescribeHsm -> TestTree
requestDescribeHsm =
  req
    "DescribeHsm"
    "fixture/DescribeHsm.yaml"

requestModifyHapg :: ModifyHapg -> TestTree
requestModifyHapg =
  req
    "ModifyHapg"
    "fixture/ModifyHapg.yaml"

requestDeleteLunaClient :: DeleteLunaClient -> TestTree
requestDeleteLunaClient =
  req
    "DeleteLunaClient"
    "fixture/DeleteLunaClient.yaml"

requestModifyHsm :: ModifyHsm -> TestTree
requestModifyHsm =
  req
    "ModifyHsm"
    "fixture/ModifyHsm.yaml"

requestListAvailableZones :: ListAvailableZones -> TestTree
requestListAvailableZones =
  req
    "ListAvailableZones"
    "fixture/ListAvailableZones.yaml"

-- Responses

responseDeleteHapg :: DeleteHapgResponse -> TestTree
responseDeleteHapg =
  res
    "DeleteHapgResponse"
    "fixture/DeleteHapgResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHapg)

responseListHapgs :: ListHapgsResponse -> TestTree
responseListHapgs =
  res
    "ListHapgsResponse"
    "fixture/ListHapgsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHapgs)

responseModifyLunaClient :: ModifyLunaClientResponse -> TestTree
responseModifyLunaClient =
  res
    "ModifyLunaClientResponse"
    "fixture/ModifyLunaClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyLunaClient)

responseListHsms :: ListHsmsResponse -> TestTree
responseListHsms =
  res
    "ListHsmsResponse"
    "fixture/ListHsmsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHsms)

responseDescribeLunaClient :: DescribeLunaClientResponse -> TestTree
responseDescribeLunaClient =
  res
    "DescribeLunaClientResponse"
    "fixture/DescribeLunaClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLunaClient)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromResource)

responseDescribeHapg :: DescribeHapgResponse -> TestTree
responseDescribeHapg =
  res
    "DescribeHapgResponse"
    "fixture/DescribeHapgResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHapg)

responseCreateLunaClient :: CreateLunaClientResponse -> TestTree
responseCreateLunaClient =
  res
    "CreateLunaClientResponse"
    "fixture/CreateLunaClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLunaClient)

responseListLunaClients :: ListLunaClientsResponse -> TestTree
responseListLunaClients =
  res
    "ListLunaClientsResponse"
    "fixture/ListLunaClientsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLunaClients)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToResource)

responseGetConfig :: GetConfigResponse -> TestTree
responseGetConfig =
  res
    "GetConfigResponse"
    "fixture/GetConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfig)

responseDeleteHsm :: DeleteHsmResponse -> TestTree
responseDeleteHsm =
  res
    "DeleteHsmResponse"
    "fixture/DeleteHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHsm)

responseDescribeHsm :: DescribeHsmResponse -> TestTree
responseDescribeHsm =
  res
    "DescribeHsmResponse"
    "fixture/DescribeHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHsm)

responseModifyHapg :: ModifyHapgResponse -> TestTree
responseModifyHapg =
  res
    "ModifyHapgResponse"
    "fixture/ModifyHapgResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyHapg)

responseDeleteLunaClient :: DeleteLunaClientResponse -> TestTree
responseDeleteLunaClient =
  res
    "DeleteLunaClientResponse"
    "fixture/DeleteLunaClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLunaClient)

responseModifyHsm :: ModifyHsmResponse -> TestTree
responseModifyHsm =
  res
    "ModifyHsmResponse"
    "fixture/ModifyHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyHsm)

responseListAvailableZones :: ListAvailableZonesResponse -> TestTree
responseListAvailableZones =
  res
    "ListAvailableZonesResponse"
    "fixture/ListAvailableZonesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAvailableZones)

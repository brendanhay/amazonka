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

import Data.Proxy
import Network.AWS.CloudHSM
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
--         [ requestListHsms $
--             newListHsms
--
--         , requestDeleteHsm $
--             newDeleteHsm
--
--         , requestModifyLunaClient $
--             newModifyLunaClient
--
--         , requestDeleteHapg $
--             newDeleteHapg
--
--         , requestGetConfig $
--             newGetConfig
--
--         , requestDeleteLunaClient $
--             newDeleteLunaClient
--
--         , requestListAvailableZones $
--             newListAvailableZones
--
--         , requestModifyHapg $
--             newModifyHapg
--
--         , requestListLunaClients $
--             newListLunaClients
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
--         , requestDescribeHsm $
--             newDescribeHsm
--
--         , requestCreateHapg $
--             newCreateHapg
--
--         , requestDescribeLunaClient $
--             newDescribeLunaClient
--
--         , requestListHapgs $
--             newListHapgs
--
--         , requestAddTagsToResource $
--             newAddTagsToResource
--
--         , requestModifyHsm $
--             newModifyHsm
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateHsm $
--             newCreateHsm
--
--           ]

--     , testGroup "response"
--         [ responseListHsms $
--             newListHsmsResponse
--
--         , responseDeleteHsm $
--             newDeleteHsmResponse
--
--         , responseModifyLunaClient $
--             newModifyLunaClientResponse
--
--         , responseDeleteHapg $
--             newDeleteHapgResponse
--
--         , responseGetConfig $
--             newGetConfigResponse
--
--         , responseDeleteLunaClient $
--             newDeleteLunaClientResponse
--
--         , responseListAvailableZones $
--             newListAvailableZonesResponse
--
--         , responseModifyHapg $
--             newModifyHapgResponse
--
--         , responseListLunaClients $
--             newListLunaClientsResponse
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
--         , responseDescribeHsm $
--             newDescribeHsmResponse
--
--         , responseCreateHapg $
--             newCreateHapgResponse
--
--         , responseDescribeLunaClient $
--             newDescribeLunaClientResponse
--
--         , responseListHapgs $
--             newListHapgsResponse
--
--         , responseAddTagsToResource $
--             newAddTagsToResourceResponse
--
--         , responseModifyHsm $
--             newModifyHsmResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateHsm $
--             newCreateHsmResponse
--
--           ]
--     ]

-- Requests

requestListHsms :: ListHsms -> TestTree
requestListHsms =
  req
    "ListHsms"
    "fixture/ListHsms.yaml"

requestDeleteHsm :: DeleteHsm -> TestTree
requestDeleteHsm =
  req
    "DeleteHsm"
    "fixture/DeleteHsm.yaml"

requestModifyLunaClient :: ModifyLunaClient -> TestTree
requestModifyLunaClient =
  req
    "ModifyLunaClient"
    "fixture/ModifyLunaClient.yaml"

requestDeleteHapg :: DeleteHapg -> TestTree
requestDeleteHapg =
  req
    "DeleteHapg"
    "fixture/DeleteHapg.yaml"

requestGetConfig :: GetConfig -> TestTree
requestGetConfig =
  req
    "GetConfig"
    "fixture/GetConfig.yaml"

requestDeleteLunaClient :: DeleteLunaClient -> TestTree
requestDeleteLunaClient =
  req
    "DeleteLunaClient"
    "fixture/DeleteLunaClient.yaml"

requestListAvailableZones :: ListAvailableZones -> TestTree
requestListAvailableZones =
  req
    "ListAvailableZones"
    "fixture/ListAvailableZones.yaml"

requestModifyHapg :: ModifyHapg -> TestTree
requestModifyHapg =
  req
    "ModifyHapg"
    "fixture/ModifyHapg.yaml"

requestListLunaClients :: ListLunaClients -> TestTree
requestListLunaClients =
  req
    "ListLunaClients"
    "fixture/ListLunaClients.yaml"

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

requestDescribeHsm :: DescribeHsm -> TestTree
requestDescribeHsm =
  req
    "DescribeHsm"
    "fixture/DescribeHsm.yaml"

requestCreateHapg :: CreateHapg -> TestTree
requestCreateHapg =
  req
    "CreateHapg"
    "fixture/CreateHapg.yaml"

requestDescribeLunaClient :: DescribeLunaClient -> TestTree
requestDescribeLunaClient =
  req
    "DescribeLunaClient"
    "fixture/DescribeLunaClient.yaml"

requestListHapgs :: ListHapgs -> TestTree
requestListHapgs =
  req
    "ListHapgs"
    "fixture/ListHapgs.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource =
  req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestModifyHsm :: ModifyHsm -> TestTree
requestModifyHsm =
  req
    "ModifyHsm"
    "fixture/ModifyHsm.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateHsm :: CreateHsm -> TestTree
requestCreateHsm =
  req
    "CreateHsm"
    "fixture/CreateHsm.yaml"

-- Responses

responseListHsms :: ListHsmsResponse -> TestTree
responseListHsms =
  res
    "ListHsmsResponse"
    "fixture/ListHsmsResponse.proto"
    defaultService
    (Proxy :: Proxy ListHsms)

responseDeleteHsm :: DeleteHsmResponse -> TestTree
responseDeleteHsm =
  res
    "DeleteHsmResponse"
    "fixture/DeleteHsmResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHsm)

responseModifyLunaClient :: ModifyLunaClientResponse -> TestTree
responseModifyLunaClient =
  res
    "ModifyLunaClientResponse"
    "fixture/ModifyLunaClientResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyLunaClient)

responseDeleteHapg :: DeleteHapgResponse -> TestTree
responseDeleteHapg =
  res
    "DeleteHapgResponse"
    "fixture/DeleteHapgResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHapg)

responseGetConfig :: GetConfigResponse -> TestTree
responseGetConfig =
  res
    "GetConfigResponse"
    "fixture/GetConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetConfig)

responseDeleteLunaClient :: DeleteLunaClientResponse -> TestTree
responseDeleteLunaClient =
  res
    "DeleteLunaClientResponse"
    "fixture/DeleteLunaClientResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLunaClient)

responseListAvailableZones :: ListAvailableZonesResponse -> TestTree
responseListAvailableZones =
  res
    "ListAvailableZonesResponse"
    "fixture/ListAvailableZonesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAvailableZones)

responseModifyHapg :: ModifyHapgResponse -> TestTree
responseModifyHapg =
  res
    "ModifyHapgResponse"
    "fixture/ModifyHapgResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyHapg)

responseListLunaClients :: ListLunaClientsResponse -> TestTree
responseListLunaClients =
  res
    "ListLunaClientsResponse"
    "fixture/ListLunaClientsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLunaClients)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromResource)

responseDescribeHapg :: DescribeHapgResponse -> TestTree
responseDescribeHapg =
  res
    "DescribeHapgResponse"
    "fixture/DescribeHapgResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHapg)

responseCreateLunaClient :: CreateLunaClientResponse -> TestTree
responseCreateLunaClient =
  res
    "CreateLunaClientResponse"
    "fixture/CreateLunaClientResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLunaClient)

responseDescribeHsm :: DescribeHsmResponse -> TestTree
responseDescribeHsm =
  res
    "DescribeHsmResponse"
    "fixture/DescribeHsmResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHsm)

responseCreateHapg :: CreateHapgResponse -> TestTree
responseCreateHapg =
  res
    "CreateHapgResponse"
    "fixture/CreateHapgResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHapg)

responseDescribeLunaClient :: DescribeLunaClientResponse -> TestTree
responseDescribeLunaClient =
  res
    "DescribeLunaClientResponse"
    "fixture/DescribeLunaClientResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLunaClient)

responseListHapgs :: ListHapgsResponse -> TestTree
responseListHapgs =
  res
    "ListHapgsResponse"
    "fixture/ListHapgsResponse.proto"
    defaultService
    (Proxy :: Proxy ListHapgs)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToResource)

responseModifyHsm :: ModifyHsmResponse -> TestTree
responseModifyHsm =
  res
    "ModifyHsmResponse"
    "fixture/ModifyHsmResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyHsm)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateHsm :: CreateHsmResponse -> TestTree
responseCreateHsm =
  res
    "CreateHsmResponse"
    "fixture/CreateHsmResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHsm)

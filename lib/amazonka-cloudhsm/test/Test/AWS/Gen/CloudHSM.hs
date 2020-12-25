{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudHSM
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestDeleteHapg $
--             mkDeleteHapg
--
--         , requestListHapgs $
--             mkListHapgs
--
--         , requestModifyLunaClient $
--             mkModifyLunaClient
--
--         , requestListHsms $
--             mkListHsms
--
--         , requestDescribeLunaClient $
--             mkDescribeLunaClient
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestCreateHapg $
--             mkCreateHapg
--
--         , requestCreateHsm $
--             mkCreateHsm
--
--         , requestRemoveTagsFromResource $
--             mkRemoveTagsFromResource
--
--         , requestDescribeHapg $
--             mkDescribeHapg
--
--         , requestCreateLunaClient $
--             mkCreateLunaClient
--
--         , requestListLunaClients $
--             mkListLunaClients
--
--         , requestAddTagsToResource $
--             mkAddTagsToResource
--
--         , requestGetConfig $
--             mkGetConfig
--
--         , requestDeleteHsm $
--             mkDeleteHsm
--
--         , requestDescribeHsm $
--             mkDescribeHsm
--
--         , requestModifyHapg $
--             mkModifyHapg
--
--         , requestDeleteLunaClient $
--             mkDeleteLunaClient
--
--         , requestModifyHsm $
--             mkModifyHsm
--
--         , requestListAvailableZones $
--             mkListAvailableZones
--
--           ]

--     , testGroup "response"
--         [ responseDeleteHapg $
--             mkDeleteHapgResponse
--
--         , responseListHapgs $
--             mkListHapgsResponse
--
--         , responseModifyLunaClient $
--             mkModifyLunaClientResponse
--
--         , responseListHsms $
--             mkListHsmsResponse
--
--         , responseDescribeLunaClient $
--             mkDescribeLunaClientResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseCreateHapg $
--             mkCreateHapgResponse
--
--         , responseCreateHsm $
--             mkCreateHsmResponse
--
--         , responseRemoveTagsFromResource $
--             mkRemoveTagsFromResourceResponse
--
--         , responseDescribeHapg $
--             mkDescribeHapgResponse
--
--         , responseCreateLunaClient $
--             mkCreateLunaClientResponse
--
--         , responseListLunaClients $
--             mkListLunaClientsResponse
--
--         , responseAddTagsToResource $
--             mkAddTagsToResourceResponse
--
--         , responseGetConfig $
--             mkGetConfigResponse
--
--         , responseDeleteHsm $
--             mkDeleteHsmResponse
--
--         , responseDescribeHsm $
--             mkDescribeHsmResponse
--
--         , responseModifyHapg $
--             mkModifyHapgResponse
--
--         , responseDeleteLunaClient $
--             mkDeleteLunaClientResponse
--
--         , responseModifyHsm $
--             mkModifyHsmResponse
--
--         , responseListAvailableZones $
--             mkListAvailableZonesResponse
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
    mkServiceConfig
    (Proxy :: Proxy DeleteHapg)

responseListHapgs :: ListHapgsResponse -> TestTree
responseListHapgs =
  res
    "ListHapgsResponse"
    "fixture/ListHapgsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListHapgs)

responseModifyLunaClient :: ModifyLunaClientResponse -> TestTree
responseModifyLunaClient =
  res
    "ModifyLunaClientResponse"
    "fixture/ModifyLunaClientResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyLunaClient)

responseListHsms :: ListHsmsResponse -> TestTree
responseListHsms =
  res
    "ListHsmsResponse"
    "fixture/ListHsmsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListHsms)

responseDescribeLunaClient :: DescribeLunaClientResponse -> TestTree
responseDescribeLunaClient =
  res
    "DescribeLunaClientResponse"
    "fixture/DescribeLunaClientResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLunaClient)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseCreateHapg :: CreateHapgResponse -> TestTree
responseCreateHapg =
  res
    "CreateHapgResponse"
    "fixture/CreateHapgResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateHapg)

responseCreateHsm :: CreateHsmResponse -> TestTree
responseCreateHsm =
  res
    "CreateHsmResponse"
    "fixture/CreateHsmResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateHsm)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveTagsFromResource)

responseDescribeHapg :: DescribeHapgResponse -> TestTree
responseDescribeHapg =
  res
    "DescribeHapgResponse"
    "fixture/DescribeHapgResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeHapg)

responseCreateLunaClient :: CreateLunaClientResponse -> TestTree
responseCreateLunaClient =
  res
    "CreateLunaClientResponse"
    "fixture/CreateLunaClientResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLunaClient)

responseListLunaClients :: ListLunaClientsResponse -> TestTree
responseListLunaClients =
  res
    "ListLunaClientsResponse"
    "fixture/ListLunaClientsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListLunaClients)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddTagsToResource)

responseGetConfig :: GetConfigResponse -> TestTree
responseGetConfig =
  res
    "GetConfigResponse"
    "fixture/GetConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetConfig)

responseDeleteHsm :: DeleteHsmResponse -> TestTree
responseDeleteHsm =
  res
    "DeleteHsmResponse"
    "fixture/DeleteHsmResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteHsm)

responseDescribeHsm :: DescribeHsmResponse -> TestTree
responseDescribeHsm =
  res
    "DescribeHsmResponse"
    "fixture/DescribeHsmResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeHsm)

responseModifyHapg :: ModifyHapgResponse -> TestTree
responseModifyHapg =
  res
    "ModifyHapgResponse"
    "fixture/ModifyHapgResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyHapg)

responseDeleteLunaClient :: DeleteLunaClientResponse -> TestTree
responseDeleteLunaClient =
  res
    "DeleteLunaClientResponse"
    "fixture/DeleteLunaClientResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLunaClient)

responseModifyHsm :: ModifyHsmResponse -> TestTree
responseModifyHsm =
  res
    "ModifyHsmResponse"
    "fixture/ModifyHsmResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyHsm)

responseListAvailableZones :: ListAvailableZonesResponse -> TestTree
responseListAvailableZones =
  res
    "ListAvailableZonesResponse"
    "fixture/ListAvailableZonesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAvailableZones)

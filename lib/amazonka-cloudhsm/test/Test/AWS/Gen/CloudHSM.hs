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
--         [ requestDeleteHAPG $
--             mkDeleteHAPG
--
--         , requestListHAPGs $
--             mkListHAPGs
--
--         , requestModifyLunaClient $
--             mkModifyLunaClient
--
--         , requestListHSMs $
--             mkListHSMs
--
--         , requestDescribeLunaClient $
--             mkDescribeLunaClient
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestCreateHAPG $
--             mkCreateHAPG
--
--         , requestCreateHSM $
--             mkCreateHSM
--
--         , requestRemoveTagsFromResource $
--             mkRemoveTagsFromResource
--
--         , requestDescribeHAPG $
--             mkDescribeHAPG
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
--         , requestDeleteHSM $
--             mkDeleteHSM
--
--         , requestDescribeHSM $
--             mkDescribeHSM
--
--         , requestModifyHAPG $
--             mkModifyHAPG
--
--         , requestDeleteLunaClient $
--             mkDeleteLunaClient
--
--         , requestModifyHSM $
--             mkModifyHSM
--
--         , requestListAvailableZones $
--             mkListAvailableZones
--
--           ]

--     , testGroup "response"
--         [ responseDeleteHAPG $
--             mkDeleteHAPGResponse
--
--         , responseListHAPGs $
--             mkListHAPGsResponse
--
--         , responseModifyLunaClient $
--             mkModifyLunaClientResponse
--
--         , responseListHSMs $
--             mkListHSMsResponse
--
--         , responseDescribeLunaClient $
--             mkDescribeLunaClientResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseCreateHAPG $
--             mkCreateHAPGResponse
--
--         , responseCreateHSM $
--             mkCreateHSMResponse
--
--         , responseRemoveTagsFromResource $
--             mkRemoveTagsFromResourceResponse
--
--         , responseDescribeHAPG $
--             mkDescribeHAPGResponse
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
--         , responseDeleteHSM $
--             mkDeleteHSMResponse
--
--         , responseDescribeHSM $
--             mkDescribeHSMResponse
--
--         , responseModifyHAPG $
--             mkModifyHAPGResponse
--
--         , responseDeleteLunaClient $
--             mkDeleteLunaClientResponse
--
--         , responseModifyHSM $
--             mkModifyHSMResponse
--
--         , responseListAvailableZones $
--             mkListAvailableZonesResponse
--
--           ]
--     ]

-- Requests

requestDeleteHAPG :: DeleteHAPG -> TestTree
requestDeleteHAPG =
  req
    "DeleteHAPG"
    "fixture/DeleteHAPG.yaml"

requestListHAPGs :: ListHAPGs -> TestTree
requestListHAPGs =
  req
    "ListHAPGs"
    "fixture/ListHAPGs.yaml"

requestModifyLunaClient :: ModifyLunaClient -> TestTree
requestModifyLunaClient =
  req
    "ModifyLunaClient"
    "fixture/ModifyLunaClient.yaml"

requestListHSMs :: ListHSMs -> TestTree
requestListHSMs =
  req
    "ListHSMs"
    "fixture/ListHSMs.yaml"

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

requestCreateHAPG :: CreateHAPG -> TestTree
requestCreateHAPG =
  req
    "CreateHAPG"
    "fixture/CreateHAPG.yaml"

requestCreateHSM :: CreateHSM -> TestTree
requestCreateHSM =
  req
    "CreateHSM"
    "fixture/CreateHSM.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource =
  req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestDescribeHAPG :: DescribeHAPG -> TestTree
requestDescribeHAPG =
  req
    "DescribeHAPG"
    "fixture/DescribeHAPG.yaml"

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

requestDeleteHSM :: DeleteHSM -> TestTree
requestDeleteHSM =
  req
    "DeleteHSM"
    "fixture/DeleteHSM.yaml"

requestDescribeHSM :: DescribeHSM -> TestTree
requestDescribeHSM =
  req
    "DescribeHSM"
    "fixture/DescribeHSM.yaml"

requestModifyHAPG :: ModifyHAPG -> TestTree
requestModifyHAPG =
  req
    "ModifyHAPG"
    "fixture/ModifyHAPG.yaml"

requestDeleteLunaClient :: DeleteLunaClient -> TestTree
requestDeleteLunaClient =
  req
    "DeleteLunaClient"
    "fixture/DeleteLunaClient.yaml"

requestModifyHSM :: ModifyHSM -> TestTree
requestModifyHSM =
  req
    "ModifyHSM"
    "fixture/ModifyHSM.yaml"

requestListAvailableZones :: ListAvailableZones -> TestTree
requestListAvailableZones =
  req
    "ListAvailableZones"
    "fixture/ListAvailableZones.yaml"

-- Responses

responseDeleteHAPG :: DeleteHAPGResponse -> TestTree
responseDeleteHAPG =
  res
    "DeleteHAPGResponse"
    "fixture/DeleteHAPGResponse.proto"
    cloudHSMService
    (Proxy :: Proxy DeleteHAPG)

responseListHAPGs :: ListHAPGsResponse -> TestTree
responseListHAPGs =
  res
    "ListHAPGsResponse"
    "fixture/ListHAPGsResponse.proto"
    cloudHSMService
    (Proxy :: Proxy ListHAPGs)

responseModifyLunaClient :: ModifyLunaClientResponse -> TestTree
responseModifyLunaClient =
  res
    "ModifyLunaClientResponse"
    "fixture/ModifyLunaClientResponse.proto"
    cloudHSMService
    (Proxy :: Proxy ModifyLunaClient)

responseListHSMs :: ListHSMsResponse -> TestTree
responseListHSMs =
  res
    "ListHSMsResponse"
    "fixture/ListHSMsResponse.proto"
    cloudHSMService
    (Proxy :: Proxy ListHSMs)

responseDescribeLunaClient :: DescribeLunaClientResponse -> TestTree
responseDescribeLunaClient =
  res
    "DescribeLunaClientResponse"
    "fixture/DescribeLunaClientResponse.proto"
    cloudHSMService
    (Proxy :: Proxy DescribeLunaClient)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    cloudHSMService
    (Proxy :: Proxy ListTagsForResource)

responseCreateHAPG :: CreateHAPGResponse -> TestTree
responseCreateHAPG =
  res
    "CreateHAPGResponse"
    "fixture/CreateHAPGResponse.proto"
    cloudHSMService
    (Proxy :: Proxy CreateHAPG)

responseCreateHSM :: CreateHSMResponse -> TestTree
responseCreateHSM =
  res
    "CreateHSMResponse"
    "fixture/CreateHSMResponse.proto"
    cloudHSMService
    (Proxy :: Proxy CreateHSM)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource =
  res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    cloudHSMService
    (Proxy :: Proxy RemoveTagsFromResource)

responseDescribeHAPG :: DescribeHAPGResponse -> TestTree
responseDescribeHAPG =
  res
    "DescribeHAPGResponse"
    "fixture/DescribeHAPGResponse.proto"
    cloudHSMService
    (Proxy :: Proxy DescribeHAPG)

responseCreateLunaClient :: CreateLunaClientResponse -> TestTree
responseCreateLunaClient =
  res
    "CreateLunaClientResponse"
    "fixture/CreateLunaClientResponse.proto"
    cloudHSMService
    (Proxy :: Proxy CreateLunaClient)

responseListLunaClients :: ListLunaClientsResponse -> TestTree
responseListLunaClients =
  res
    "ListLunaClientsResponse"
    "fixture/ListLunaClientsResponse.proto"
    cloudHSMService
    (Proxy :: Proxy ListLunaClients)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource =
  res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    cloudHSMService
    (Proxy :: Proxy AddTagsToResource)

responseGetConfig :: GetConfigResponse -> TestTree
responseGetConfig =
  res
    "GetConfigResponse"
    "fixture/GetConfigResponse.proto"
    cloudHSMService
    (Proxy :: Proxy GetConfig)

responseDeleteHSM :: DeleteHSMResponse -> TestTree
responseDeleteHSM =
  res
    "DeleteHSMResponse"
    "fixture/DeleteHSMResponse.proto"
    cloudHSMService
    (Proxy :: Proxy DeleteHSM)

responseDescribeHSM :: DescribeHSMResponse -> TestTree
responseDescribeHSM =
  res
    "DescribeHSMResponse"
    "fixture/DescribeHSMResponse.proto"
    cloudHSMService
    (Proxy :: Proxy DescribeHSM)

responseModifyHAPG :: ModifyHAPGResponse -> TestTree
responseModifyHAPG =
  res
    "ModifyHAPGResponse"
    "fixture/ModifyHAPGResponse.proto"
    cloudHSMService
    (Proxy :: Proxy ModifyHAPG)

responseDeleteLunaClient :: DeleteLunaClientResponse -> TestTree
responseDeleteLunaClient =
  res
    "DeleteLunaClientResponse"
    "fixture/DeleteLunaClientResponse.proto"
    cloudHSMService
    (Proxy :: Proxy DeleteLunaClient)

responseModifyHSM :: ModifyHSMResponse -> TestTree
responseModifyHSM =
  res
    "ModifyHSMResponse"
    "fixture/ModifyHSMResponse.proto"
    cloudHSMService
    (Proxy :: Proxy ModifyHSM)

responseListAvailableZones :: ListAvailableZonesResponse -> TestTree
responseListAvailableZones =
  res
    "ListAvailableZonesResponse"
    "fixture/ListAvailableZonesResponse.proto"
    cloudHSMService
    (Proxy :: Proxy ListAvailableZones)

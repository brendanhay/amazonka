{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudHSM
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudHSM where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudHSM
import Test.AWS.CloudHSM.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDeleteHAPG $
--             deleteHAPG
--
--         , testListHAPGs $
--             listHAPGs
--
--         , testModifyLunaClient $
--             modifyLunaClient
--
--         , testListHSMs $
--             listHSMs
--
--         , testDescribeLunaClient $
--             describeLunaClient
--
--         , testListTagsForResource $
--             listTagsForResource
--
--         , testCreateHAPG $
--             createHAPG
--
--         , testCreateHSM $
--             createHSM
--
--         , testRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , testDescribeHAPG $
--             describeHAPG
--
--         , testCreateLunaClient $
--             createLunaClient
--
--         , testListLunaClients $
--             listLunaClients
--
--         , testAddTagsToResource $
--             addTagsToResource
--
--         , testGetConfig $
--             getConfig
--
--         , testDeleteHSM $
--             deleteHSM
--
--         , testDescribeHSM $
--             describeHSM
--
--         , testModifyHAPG $
--             modifyHAPG
--
--         , testDeleteLunaClient $
--             deleteLunaClient
--
--         , testModifyHSM $
--             modifyHSM
--
--         , testListAvailableZones $
--             listAvailableZones
--
--           ]

--     , testGroup "response"
--         [ testDeleteHAPGResponse $
--             deleteHAPGResponse
--
--         , testListHAPGsResponse $
--             listHAPGsResponse
--
--         , testModifyLunaClientResponse $
--             modifyLunaClientResponse
--
--         , testListHSMsResponse $
--             listHSMsResponse
--
--         , testDescribeLunaClientResponse $
--             describeLunaClientResponse
--
--         , testListTagsForResourceResponse $
--             listTagsForResourceResponse
--
--         , testCreateHAPGResponse $
--             createHAPGResponse
--
--         , testCreateHSMResponse $
--             createHSMResponse
--
--         , testRemoveTagsFromResourceResponse $
--             removeTagsFromResourceResponse
--
--         , testDescribeHAPGResponse $
--             describeHAPGResponse
--
--         , testCreateLunaClientResponse $
--             createLunaClientResponse
--
--         , testListLunaClientsResponse $
--             listLunaClientsResponse
--
--         , testAddTagsToResourceResponse $
--             addTagsToResourceResponse
--
--         , testGetConfigResponse $
--             getConfigResponse
--
--         , testDeleteHSMResponse $
--             deleteHSMResponse
--
--         , testDescribeHSMResponse $
--             describeHSMResponse
--
--         , testModifyHAPGResponse $
--             modifyHAPGResponse
--
--         , testDeleteLunaClientResponse $
--             deleteLunaClientResponse
--
--         , testModifyHSMResponse $
--             modifyHSMResponse
--
--         , testListAvailableZonesResponse $
--             listAvailableZonesResponse
--
--           ]
--     ]

-- Requests

testDeleteHAPG :: DeleteHAPG -> TestTree
testDeleteHAPG = req
    "DeleteHAPG"
    "fixture/DeleteHAPG.yaml"

testListHAPGs :: ListHAPGs -> TestTree
testListHAPGs = req
    "ListHAPGs"
    "fixture/ListHAPGs.yaml"

testModifyLunaClient :: ModifyLunaClient -> TestTree
testModifyLunaClient = req
    "ModifyLunaClient"
    "fixture/ModifyLunaClient.yaml"

testListHSMs :: ListHSMs -> TestTree
testListHSMs = req
    "ListHSMs"
    "fixture/ListHSMs.yaml"

testDescribeLunaClient :: DescribeLunaClient -> TestTree
testDescribeLunaClient = req
    "DescribeLunaClient"
    "fixture/DescribeLunaClient.yaml"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

testCreateHAPG :: CreateHAPG -> TestTree
testCreateHAPG = req
    "CreateHAPG"
    "fixture/CreateHAPG.yaml"

testCreateHSM :: CreateHSM -> TestTree
testCreateHSM = req
    "CreateHSM"
    "fixture/CreateHSM.yaml"

testRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
testRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

testDescribeHAPG :: DescribeHAPG -> TestTree
testDescribeHAPG = req
    "DescribeHAPG"
    "fixture/DescribeHAPG.yaml"

testCreateLunaClient :: CreateLunaClient -> TestTree
testCreateLunaClient = req
    "CreateLunaClient"
    "fixture/CreateLunaClient.yaml"

testListLunaClients :: ListLunaClients -> TestTree
testListLunaClients = req
    "ListLunaClients"
    "fixture/ListLunaClients.yaml"

testAddTagsToResource :: AddTagsToResource -> TestTree
testAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

testGetConfig :: GetConfig -> TestTree
testGetConfig = req
    "GetConfig"
    "fixture/GetConfig.yaml"

testDeleteHSM :: DeleteHSM -> TestTree
testDeleteHSM = req
    "DeleteHSM"
    "fixture/DeleteHSM.yaml"

testDescribeHSM :: DescribeHSM -> TestTree
testDescribeHSM = req
    "DescribeHSM"
    "fixture/DescribeHSM.yaml"

testModifyHAPG :: ModifyHAPG -> TestTree
testModifyHAPG = req
    "ModifyHAPG"
    "fixture/ModifyHAPG.yaml"

testDeleteLunaClient :: DeleteLunaClient -> TestTree
testDeleteLunaClient = req
    "DeleteLunaClient"
    "fixture/DeleteLunaClient.yaml"

testModifyHSM :: ModifyHSM -> TestTree
testModifyHSM = req
    "ModifyHSM"
    "fixture/ModifyHSM.yaml"

testListAvailableZones :: ListAvailableZones -> TestTree
testListAvailableZones = req
    "ListAvailableZones"
    "fixture/ListAvailableZones.yaml"

-- Responses

testDeleteHAPGResponse :: DeleteHAPGResponse -> TestTree
testDeleteHAPGResponse = res
    "DeleteHAPGResponse"
    "fixture/DeleteHAPGResponse.proto"
    cloudHSM
    (Proxy :: Proxy DeleteHAPG)

testListHAPGsResponse :: ListHAPGsResponse -> TestTree
testListHAPGsResponse = res
    "ListHAPGsResponse"
    "fixture/ListHAPGsResponse.proto"
    cloudHSM
    (Proxy :: Proxy ListHAPGs)

testModifyLunaClientResponse :: ModifyLunaClientResponse -> TestTree
testModifyLunaClientResponse = res
    "ModifyLunaClientResponse"
    "fixture/ModifyLunaClientResponse.proto"
    cloudHSM
    (Proxy :: Proxy ModifyLunaClient)

testListHSMsResponse :: ListHSMsResponse -> TestTree
testListHSMsResponse = res
    "ListHSMsResponse"
    "fixture/ListHSMsResponse.proto"
    cloudHSM
    (Proxy :: Proxy ListHSMs)

testDescribeLunaClientResponse :: DescribeLunaClientResponse -> TestTree
testDescribeLunaClientResponse = res
    "DescribeLunaClientResponse"
    "fixture/DescribeLunaClientResponse.proto"
    cloudHSM
    (Proxy :: Proxy DescribeLunaClient)

testListTagsForResourceResponse :: ListTagsForResourceResponse -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    cloudHSM
    (Proxy :: Proxy ListTagsForResource)

testCreateHAPGResponse :: CreateHAPGResponse -> TestTree
testCreateHAPGResponse = res
    "CreateHAPGResponse"
    "fixture/CreateHAPGResponse.proto"
    cloudHSM
    (Proxy :: Proxy CreateHAPG)

testCreateHSMResponse :: CreateHSMResponse -> TestTree
testCreateHSMResponse = res
    "CreateHSMResponse"
    "fixture/CreateHSMResponse.proto"
    cloudHSM
    (Proxy :: Proxy CreateHSM)

testRemoveTagsFromResourceResponse :: RemoveTagsFromResourceResponse -> TestTree
testRemoveTagsFromResourceResponse = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    cloudHSM
    (Proxy :: Proxy RemoveTagsFromResource)

testDescribeHAPGResponse :: DescribeHAPGResponse -> TestTree
testDescribeHAPGResponse = res
    "DescribeHAPGResponse"
    "fixture/DescribeHAPGResponse.proto"
    cloudHSM
    (Proxy :: Proxy DescribeHAPG)

testCreateLunaClientResponse :: CreateLunaClientResponse -> TestTree
testCreateLunaClientResponse = res
    "CreateLunaClientResponse"
    "fixture/CreateLunaClientResponse.proto"
    cloudHSM
    (Proxy :: Proxy CreateLunaClient)

testListLunaClientsResponse :: ListLunaClientsResponse -> TestTree
testListLunaClientsResponse = res
    "ListLunaClientsResponse"
    "fixture/ListLunaClientsResponse.proto"
    cloudHSM
    (Proxy :: Proxy ListLunaClients)

testAddTagsToResourceResponse :: AddTagsToResourceResponse -> TestTree
testAddTagsToResourceResponse = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    cloudHSM
    (Proxy :: Proxy AddTagsToResource)

testGetConfigResponse :: GetConfigResponse -> TestTree
testGetConfigResponse = res
    "GetConfigResponse"
    "fixture/GetConfigResponse.proto"
    cloudHSM
    (Proxy :: Proxy GetConfig)

testDeleteHSMResponse :: DeleteHSMResponse -> TestTree
testDeleteHSMResponse = res
    "DeleteHSMResponse"
    "fixture/DeleteHSMResponse.proto"
    cloudHSM
    (Proxy :: Proxy DeleteHSM)

testDescribeHSMResponse :: DescribeHSMResponse -> TestTree
testDescribeHSMResponse = res
    "DescribeHSMResponse"
    "fixture/DescribeHSMResponse.proto"
    cloudHSM
    (Proxy :: Proxy DescribeHSM)

testModifyHAPGResponse :: ModifyHAPGResponse -> TestTree
testModifyHAPGResponse = res
    "ModifyHAPGResponse"
    "fixture/ModifyHAPGResponse.proto"
    cloudHSM
    (Proxy :: Proxy ModifyHAPG)

testDeleteLunaClientResponse :: DeleteLunaClientResponse -> TestTree
testDeleteLunaClientResponse = res
    "DeleteLunaClientResponse"
    "fixture/DeleteLunaClientResponse.proto"
    cloudHSM
    (Proxy :: Proxy DeleteLunaClient)

testModifyHSMResponse :: ModifyHSMResponse -> TestTree
testModifyHSMResponse = res
    "ModifyHSMResponse"
    "fixture/ModifyHSMResponse.proto"
    cloudHSM
    (Proxy :: Proxy ModifyHSM)

testListAvailableZonesResponse :: ListAvailableZonesResponse -> TestTree
testListAvailableZonesResponse = res
    "ListAvailableZonesResponse"
    "fixture/ListAvailableZonesResponse.proto"
    cloudHSM
    (Proxy :: Proxy ListAvailableZones)

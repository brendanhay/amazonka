{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudHSM
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         [ testListHAPGs $
--             listHAPGs
--
--         , testListHSMs $
--             listHSMs
--
--         , testDeleteHAPG $
--             deleteHAPG
--
--         , testModifyLunaClient $
--             modifyLunaClient
--
--         , testDescribeLunaClient $
--             describeLunaClient
--
--         , testCreateHSM $
--             createHSM
--
--         , testCreateHAPG $
--             createHAPG
--
--         , testCreateLunaClient $
--             createLunaClient
--
--         , testDescribeHAPG $
--             describeHAPG
--
--         , testListLunaClients $
--             listLunaClients
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
--         , testListAvailableZones $
--             listAvailableZones
--
--         , testModifyHSM $
--             modifyHSM
--
--         , testDeleteLunaClient $
--             deleteLunaClient
--
--           ]

--     , testGroup "response"
--         [ testListHAPGsResponse $
--             listHAPGsResponse
--
--         , testListHSMsResponse $
--             listHSMsResponse
--
--         , testDeleteHAPGResponse $
--             deleteHAPGResponse
--
--         , testModifyLunaClientResponse $
--             modifyLunaClientResponse
--
--         , testDescribeLunaClientResponse $
--             describeLunaClientResponse
--
--         , testCreateHSMResponse $
--             createHSMResponse
--
--         , testCreateHAPGResponse $
--             createHAPGResponse
--
--         , testCreateLunaClientResponse $
--             createLunaClientResponse
--
--         , testDescribeHAPGResponse $
--             describeHAPGResponse
--
--         , testListLunaClientsResponse $
--             listLunaClientsResponse
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
--         , testListAvailableZonesResponse $
--             listAvailableZonesResponse
--
--         , testModifyHSMResponse $
--             modifyHSMResponse
--
--         , testDeleteLunaClientResponse $
--             deleteLunaClientResponse
--
--           ]
--     ]

-- Requests

testListHAPGs :: ListHAPGs -> TestTree
testListHAPGs = req
    "ListHAPGs"
    "fixture/ListHAPGs.yaml"

testListHSMs :: ListHSMs -> TestTree
testListHSMs = req
    "ListHSMs"
    "fixture/ListHSMs.yaml"

testDeleteHAPG :: DeleteHAPG -> TestTree
testDeleteHAPG = req
    "DeleteHAPG"
    "fixture/DeleteHAPG.yaml"

testModifyLunaClient :: ModifyLunaClient -> TestTree
testModifyLunaClient = req
    "ModifyLunaClient"
    "fixture/ModifyLunaClient.yaml"

testDescribeLunaClient :: DescribeLunaClient -> TestTree
testDescribeLunaClient = req
    "DescribeLunaClient"
    "fixture/DescribeLunaClient.yaml"

testCreateHSM :: CreateHSM -> TestTree
testCreateHSM = req
    "CreateHSM"
    "fixture/CreateHSM.yaml"

testCreateHAPG :: CreateHAPG -> TestTree
testCreateHAPG = req
    "CreateHAPG"
    "fixture/CreateHAPG.yaml"

testCreateLunaClient :: CreateLunaClient -> TestTree
testCreateLunaClient = req
    "CreateLunaClient"
    "fixture/CreateLunaClient.yaml"

testDescribeHAPG :: DescribeHAPG -> TestTree
testDescribeHAPG = req
    "DescribeHAPG"
    "fixture/DescribeHAPG.yaml"

testListLunaClients :: ListLunaClients -> TestTree
testListLunaClients = req
    "ListLunaClients"
    "fixture/ListLunaClients.yaml"

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

testListAvailableZones :: ListAvailableZones -> TestTree
testListAvailableZones = req
    "ListAvailableZones"
    "fixture/ListAvailableZones.yaml"

testModifyHSM :: ModifyHSM -> TestTree
testModifyHSM = req
    "ModifyHSM"
    "fixture/ModifyHSM.yaml"

testDeleteLunaClient :: DeleteLunaClient -> TestTree
testDeleteLunaClient = req
    "DeleteLunaClient"
    "fixture/DeleteLunaClient.yaml"

-- Responses

testListHAPGsResponse :: ListHAPGsResponse -> TestTree
testListHAPGsResponse = res
    "ListHAPGsResponse"
    "fixture/ListHAPGsResponse.proto"
    cloudHSM
    (Proxy :: Proxy ListHAPGs)

testListHSMsResponse :: ListHSMsResponse -> TestTree
testListHSMsResponse = res
    "ListHSMsResponse"
    "fixture/ListHSMsResponse.proto"
    cloudHSM
    (Proxy :: Proxy ListHSMs)

testDeleteHAPGResponse :: DeleteHAPGResponse -> TestTree
testDeleteHAPGResponse = res
    "DeleteHAPGResponse"
    "fixture/DeleteHAPGResponse.proto"
    cloudHSM
    (Proxy :: Proxy DeleteHAPG)

testModifyLunaClientResponse :: ModifyLunaClientResponse -> TestTree
testModifyLunaClientResponse = res
    "ModifyLunaClientResponse"
    "fixture/ModifyLunaClientResponse.proto"
    cloudHSM
    (Proxy :: Proxy ModifyLunaClient)

testDescribeLunaClientResponse :: DescribeLunaClientResponse -> TestTree
testDescribeLunaClientResponse = res
    "DescribeLunaClientResponse"
    "fixture/DescribeLunaClientResponse.proto"
    cloudHSM
    (Proxy :: Proxy DescribeLunaClient)

testCreateHSMResponse :: CreateHSMResponse -> TestTree
testCreateHSMResponse = res
    "CreateHSMResponse"
    "fixture/CreateHSMResponse.proto"
    cloudHSM
    (Proxy :: Proxy CreateHSM)

testCreateHAPGResponse :: CreateHAPGResponse -> TestTree
testCreateHAPGResponse = res
    "CreateHAPGResponse"
    "fixture/CreateHAPGResponse.proto"
    cloudHSM
    (Proxy :: Proxy CreateHAPG)

testCreateLunaClientResponse :: CreateLunaClientResponse -> TestTree
testCreateLunaClientResponse = res
    "CreateLunaClientResponse"
    "fixture/CreateLunaClientResponse.proto"
    cloudHSM
    (Proxy :: Proxy CreateLunaClient)

testDescribeHAPGResponse :: DescribeHAPGResponse -> TestTree
testDescribeHAPGResponse = res
    "DescribeHAPGResponse"
    "fixture/DescribeHAPGResponse.proto"
    cloudHSM
    (Proxy :: Proxy DescribeHAPG)

testListLunaClientsResponse :: ListLunaClientsResponse -> TestTree
testListLunaClientsResponse = res
    "ListLunaClientsResponse"
    "fixture/ListLunaClientsResponse.proto"
    cloudHSM
    (Proxy :: Proxy ListLunaClients)

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

testListAvailableZonesResponse :: ListAvailableZonesResponse -> TestTree
testListAvailableZonesResponse = res
    "ListAvailableZonesResponse"
    "fixture/ListAvailableZonesResponse.proto"
    cloudHSM
    (Proxy :: Proxy ListAvailableZones)

testModifyHSMResponse :: ModifyHSMResponse -> TestTree
testModifyHSMResponse = res
    "ModifyHSMResponse"
    "fixture/ModifyHSMResponse.proto"
    cloudHSM
    (Proxy :: Proxy ModifyHSM)

testDeleteLunaClientResponse :: DeleteLunaClientResponse -> TestTree
testDeleteLunaClientResponse = res
    "DeleteLunaClientResponse"
    "fixture/DeleteLunaClientResponse.proto"
    cloudHSM
    (Proxy :: Proxy DeleteLunaClient)

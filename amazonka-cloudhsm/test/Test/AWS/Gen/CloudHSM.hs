{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudHSM
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
    "fixture/ListHAPGs"

testListHSMs :: ListHSMs -> TestTree
testListHSMs = req
    "ListHSMs"
    "fixture/ListHSMs"

testDeleteHAPG :: DeleteHAPG -> TestTree
testDeleteHAPG = req
    "DeleteHAPG"
    "fixture/DeleteHAPG"

testModifyLunaClient :: ModifyLunaClient -> TestTree
testModifyLunaClient = req
    "ModifyLunaClient"
    "fixture/ModifyLunaClient"

testDescribeLunaClient :: DescribeLunaClient -> TestTree
testDescribeLunaClient = req
    "DescribeLunaClient"
    "fixture/DescribeLunaClient"

testCreateHSM :: CreateHSM -> TestTree
testCreateHSM = req
    "CreateHSM"
    "fixture/CreateHSM"

testCreateHAPG :: CreateHAPG -> TestTree
testCreateHAPG = req
    "CreateHAPG"
    "fixture/CreateHAPG"

testCreateLunaClient :: CreateLunaClient -> TestTree
testCreateLunaClient = req
    "CreateLunaClient"
    "fixture/CreateLunaClient"

testDescribeHAPG :: DescribeHAPG -> TestTree
testDescribeHAPG = req
    "DescribeHAPG"
    "fixture/DescribeHAPG"

testListLunaClients :: ListLunaClients -> TestTree
testListLunaClients = req
    "ListLunaClients"
    "fixture/ListLunaClients"

testGetConfig :: GetConfig -> TestTree
testGetConfig = req
    "GetConfig"
    "fixture/GetConfig"

testDeleteHSM :: DeleteHSM -> TestTree
testDeleteHSM = req
    "DeleteHSM"
    "fixture/DeleteHSM"

testDescribeHSM :: DescribeHSM -> TestTree
testDescribeHSM = req
    "DescribeHSM"
    "fixture/DescribeHSM"

testModifyHAPG :: ModifyHAPG -> TestTree
testModifyHAPG = req
    "ModifyHAPG"
    "fixture/ModifyHAPG"

testListAvailableZones :: ListAvailableZones -> TestTree
testListAvailableZones = req
    "ListAvailableZones"
    "fixture/ListAvailableZones"

testModifyHSM :: ModifyHSM -> TestTree
testModifyHSM = req
    "ModifyHSM"
    "fixture/ModifyHSM"

testDeleteLunaClient :: DeleteLunaClient -> TestTree
testDeleteLunaClient = req
    "DeleteLunaClient"
    "fixture/DeleteLunaClient"

-- Responses

testListHAPGsResponse :: ListHAPGsResponse -> TestTree
testListHAPGsResponse = res
    "ListHAPGsResponse"
    "fixture/ListHAPGsResponse"
    (Proxy :: Proxy ListHAPGs)

testListHSMsResponse :: ListHSMsResponse -> TestTree
testListHSMsResponse = res
    "ListHSMsResponse"
    "fixture/ListHSMsResponse"
    (Proxy :: Proxy ListHSMs)

testDeleteHAPGResponse :: DeleteHAPGResponse -> TestTree
testDeleteHAPGResponse = res
    "DeleteHAPGResponse"
    "fixture/DeleteHAPGResponse"
    (Proxy :: Proxy DeleteHAPG)

testModifyLunaClientResponse :: ModifyLunaClientResponse -> TestTree
testModifyLunaClientResponse = res
    "ModifyLunaClientResponse"
    "fixture/ModifyLunaClientResponse"
    (Proxy :: Proxy ModifyLunaClient)

testDescribeLunaClientResponse :: DescribeLunaClientResponse -> TestTree
testDescribeLunaClientResponse = res
    "DescribeLunaClientResponse"
    "fixture/DescribeLunaClientResponse"
    (Proxy :: Proxy DescribeLunaClient)

testCreateHSMResponse :: CreateHSMResponse -> TestTree
testCreateHSMResponse = res
    "CreateHSMResponse"
    "fixture/CreateHSMResponse"
    (Proxy :: Proxy CreateHSM)

testCreateHAPGResponse :: CreateHAPGResponse -> TestTree
testCreateHAPGResponse = res
    "CreateHAPGResponse"
    "fixture/CreateHAPGResponse"
    (Proxy :: Proxy CreateHAPG)

testCreateLunaClientResponse :: CreateLunaClientResponse -> TestTree
testCreateLunaClientResponse = res
    "CreateLunaClientResponse"
    "fixture/CreateLunaClientResponse"
    (Proxy :: Proxy CreateLunaClient)

testDescribeHAPGResponse :: DescribeHAPGResponse -> TestTree
testDescribeHAPGResponse = res
    "DescribeHAPGResponse"
    "fixture/DescribeHAPGResponse"
    (Proxy :: Proxy DescribeHAPG)

testListLunaClientsResponse :: ListLunaClientsResponse -> TestTree
testListLunaClientsResponse = res
    "ListLunaClientsResponse"
    "fixture/ListLunaClientsResponse"
    (Proxy :: Proxy ListLunaClients)

testGetConfigResponse :: GetConfigResponse -> TestTree
testGetConfigResponse = res
    "GetConfigResponse"
    "fixture/GetConfigResponse"
    (Proxy :: Proxy GetConfig)

testDeleteHSMResponse :: DeleteHSMResponse -> TestTree
testDeleteHSMResponse = res
    "DeleteHSMResponse"
    "fixture/DeleteHSMResponse"
    (Proxy :: Proxy DeleteHSM)

testDescribeHSMResponse :: DescribeHSMResponse -> TestTree
testDescribeHSMResponse = res
    "DescribeHSMResponse"
    "fixture/DescribeHSMResponse"
    (Proxy :: Proxy DescribeHSM)

testModifyHAPGResponse :: ModifyHAPGResponse -> TestTree
testModifyHAPGResponse = res
    "ModifyHAPGResponse"
    "fixture/ModifyHAPGResponse"
    (Proxy :: Proxy ModifyHAPG)

testListAvailableZonesResponse :: ListAvailableZonesResponse -> TestTree
testListAvailableZonesResponse = res
    "ListAvailableZonesResponse"
    "fixture/ListAvailableZonesResponse"
    (Proxy :: Proxy ListAvailableZones)

testModifyHSMResponse :: ModifyHSMResponse -> TestTree
testModifyHSMResponse = res
    "ModifyHSMResponse"
    "fixture/ModifyHSMResponse"
    (Proxy :: Proxy ModifyHSM)

testDeleteLunaClientResponse :: DeleteLunaClientResponse -> TestTree
testDeleteLunaClientResponse = res
    "DeleteLunaClientResponse"
    "fixture/DeleteLunaClientResponse"
    (Proxy :: Proxy DeleteLunaClient)

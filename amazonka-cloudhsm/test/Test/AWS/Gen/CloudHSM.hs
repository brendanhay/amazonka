{-# OPTIONS_GHC -fno-warn-orphans #-}

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
testListHAPGs = undefined

testListHSMs :: ListHSMs -> TestTree
testListHSMs = undefined

testDeleteHAPG :: DeleteHAPG -> TestTree
testDeleteHAPG = undefined

testModifyLunaClient :: ModifyLunaClient -> TestTree
testModifyLunaClient = undefined

testDescribeLunaClient :: DescribeLunaClient -> TestTree
testDescribeLunaClient = undefined

testCreateHSM :: CreateHSM -> TestTree
testCreateHSM = undefined

testCreateHAPG :: CreateHAPG -> TestTree
testCreateHAPG = undefined

testCreateLunaClient :: CreateLunaClient -> TestTree
testCreateLunaClient = undefined

testDescribeHAPG :: DescribeHAPG -> TestTree
testDescribeHAPG = undefined

testListLunaClients :: ListLunaClients -> TestTree
testListLunaClients = undefined

testGetConfig :: GetConfig -> TestTree
testGetConfig = undefined

testDeleteHSM :: DeleteHSM -> TestTree
testDeleteHSM = undefined

testDescribeHSM :: DescribeHSM -> TestTree
testDescribeHSM = undefined

testModifyHAPG :: ModifyHAPG -> TestTree
testModifyHAPG = undefined

testListAvailableZones :: ListAvailableZones -> TestTree
testListAvailableZones = undefined

testModifyHSM :: ModifyHSM -> TestTree
testModifyHSM = undefined

testDeleteLunaClient :: DeleteLunaClient -> TestTree
testDeleteLunaClient = undefined

-- Responses

testListHAPGsResponse :: ListHAPGsResponse -> TestTree
testListHAPGsResponse = resp
    "ListHAPGsResponse"
    "fixture/ListHAPGsResponse"
    (Proxy :: Proxy ListHAPGs)

testListHSMsResponse :: ListHSMsResponse -> TestTree
testListHSMsResponse = resp
    "ListHSMsResponse"
    "fixture/ListHSMsResponse"
    (Proxy :: Proxy ListHSMs)

testDeleteHAPGResponse :: DeleteHAPGResponse -> TestTree
testDeleteHAPGResponse = resp
    "DeleteHAPGResponse"
    "fixture/DeleteHAPGResponse"
    (Proxy :: Proxy DeleteHAPG)

testModifyLunaClientResponse :: ModifyLunaClientResponse -> TestTree
testModifyLunaClientResponse = resp
    "ModifyLunaClientResponse"
    "fixture/ModifyLunaClientResponse"
    (Proxy :: Proxy ModifyLunaClient)

testDescribeLunaClientResponse :: DescribeLunaClientResponse -> TestTree
testDescribeLunaClientResponse = resp
    "DescribeLunaClientResponse"
    "fixture/DescribeLunaClientResponse"
    (Proxy :: Proxy DescribeLunaClient)

testCreateHSMResponse :: CreateHSMResponse -> TestTree
testCreateHSMResponse = resp
    "CreateHSMResponse"
    "fixture/CreateHSMResponse"
    (Proxy :: Proxy CreateHSM)

testCreateHAPGResponse :: CreateHAPGResponse -> TestTree
testCreateHAPGResponse = resp
    "CreateHAPGResponse"
    "fixture/CreateHAPGResponse"
    (Proxy :: Proxy CreateHAPG)

testCreateLunaClientResponse :: CreateLunaClientResponse -> TestTree
testCreateLunaClientResponse = resp
    "CreateLunaClientResponse"
    "fixture/CreateLunaClientResponse"
    (Proxy :: Proxy CreateLunaClient)

testDescribeHAPGResponse :: DescribeHAPGResponse -> TestTree
testDescribeHAPGResponse = resp
    "DescribeHAPGResponse"
    "fixture/DescribeHAPGResponse"
    (Proxy :: Proxy DescribeHAPG)

testListLunaClientsResponse :: ListLunaClientsResponse -> TestTree
testListLunaClientsResponse = resp
    "ListLunaClientsResponse"
    "fixture/ListLunaClientsResponse"
    (Proxy :: Proxy ListLunaClients)

testGetConfigResponse :: GetConfigResponse -> TestTree
testGetConfigResponse = resp
    "GetConfigResponse"
    "fixture/GetConfigResponse"
    (Proxy :: Proxy GetConfig)

testDeleteHSMResponse :: DeleteHSMResponse -> TestTree
testDeleteHSMResponse = resp
    "DeleteHSMResponse"
    "fixture/DeleteHSMResponse"
    (Proxy :: Proxy DeleteHSM)

testDescribeHSMResponse :: DescribeHSMResponse -> TestTree
testDescribeHSMResponse = resp
    "DescribeHSMResponse"
    "fixture/DescribeHSMResponse"
    (Proxy :: Proxy DescribeHSM)

testModifyHAPGResponse :: ModifyHAPGResponse -> TestTree
testModifyHAPGResponse = resp
    "ModifyHAPGResponse"
    "fixture/ModifyHAPGResponse"
    (Proxy :: Proxy ModifyHAPG)

testListAvailableZonesResponse :: ListAvailableZonesResponse -> TestTree
testListAvailableZonesResponse = resp
    "ListAvailableZonesResponse"
    "fixture/ListAvailableZonesResponse"
    (Proxy :: Proxy ListAvailableZones)

testModifyHSMResponse :: ModifyHSMResponse -> TestTree
testModifyHSMResponse = resp
    "ModifyHSMResponse"
    "fixture/ModifyHSMResponse"
    (Proxy :: Proxy ModifyHSM)

testDeleteLunaClientResponse :: DeleteLunaClientResponse -> TestTree
testDeleteLunaClientResponse = resp
    "DeleteLunaClientResponse"
    "fixture/DeleteLunaClientResponse"
    (Proxy :: Proxy DeleteLunaClient)

instance Out ClientVersion
instance Out CloudHSMObjectState
instance Out CreateHAPG
instance Out CreateHAPGResponse
instance Out CreateHSM
instance Out CreateHSMResponse
instance Out CreateLunaClient
instance Out CreateLunaClientResponse
instance Out DeleteHAPG
instance Out DeleteHAPGResponse
instance Out DeleteHSM
instance Out DeleteHSMResponse
instance Out DeleteLunaClient
instance Out DeleteLunaClientResponse
instance Out DescribeHAPG
instance Out DescribeHAPGResponse
instance Out DescribeHSM
instance Out DescribeHSMResponse
instance Out DescribeLunaClient
instance Out DescribeLunaClientResponse
instance Out GetConfig
instance Out GetConfigResponse
instance Out HSMStatus
instance Out ListAvailableZones
instance Out ListAvailableZonesResponse
instance Out ListHAPGs
instance Out ListHAPGsResponse
instance Out ListHSMs
instance Out ListHSMsResponse
instance Out ListLunaClients
instance Out ListLunaClientsResponse
instance Out ModifyHAPG
instance Out ModifyHAPGResponse
instance Out ModifyHSM
instance Out ModifyHSMResponse
instance Out ModifyLunaClient
instance Out ModifyLunaClientResponse
instance Out SubscriptionType

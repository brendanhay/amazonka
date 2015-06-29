-- Module      : Test.AWS.Gen.CloudHSM
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.CloudHSM where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CloudHSM

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ createHAPGTest $
--             createHAPG
--
--         , createHSMTest $
--             createHSM
--
--         , createLunaClientTest $
--             createLunaClient
--
--         , deleteHAPGTest $
--             deleteHAPG
--
--         , deleteHSMTest $
--             deleteHSM
--
--         , deleteLunaClientTest $
--             deleteLunaClient
--
--         , describeHAPGTest $
--             describeHAPG
--
--         , describeHSMTest $
--             describeHSM
--
--         , describeLunaClientTest $
--             describeLunaClient
--
--         , getConfigTest $
--             getConfig
--
--         , listAvailableZonesTest $
--             listAvailableZones
--
--         , listHAPGsTest $
--             listHAPGs
--
--         , listHSMsTest $
--             listHSMs
--
--         , listLunaClientsTest $
--             listLunaClients
--
--         , modifyHAPGTest $
--             modifyHAPG
--
--         , modifyHSMTest $
--             modifyHSM
--
--         , modifyLunaClientTest $
--             modifyLunaClient
--
--           ]

--     , testGroup "response"
--         [ createHAPGResponseTest $
--             createHAPGResponse
--
--         , createHSMResponseTest $
--             createHSMResponse
--
--         , createLunaClientResponseTest $
--             createLunaClientResponse
--
--         , deleteHAPGResponseTest $
--             deleteHAPGResponse
--
--         , deleteHSMResponseTest $
--             deleteHSMResponse
--
--         , deleteLunaClientResponseTest $
--             deleteLunaClientResponse
--
--         , describeHAPGResponseTest $
--             describeHAPGResponse
--
--         , describeHSMResponseTest $
--             describeHSMResponse
--
--         , describeLunaClientResponseTest $
--             describeLunaClientResponse
--
--         , getConfigResponseTest $
--             getConfigResponse
--
--         , listAvailableZonesResponseTest $
--             listAvailableZonesResponse
--
--         , listHAPGsResponseTest $
--             listHAPGsResponse
--
--         , listHSMsResponseTest $
--             listHSMsResponse
--
--         , listLunaClientsResponseTest $
--             listLunaClientsResponse
--
--         , modifyHAPGResponseTest $
--             modifyHAPGResponse
--
--         , modifyHSMResponseTest $
--             modifyHSMResponse
--
--         , modifyLunaClientResponseTest $
--             modifyLunaClientResponse
--
--           ]
--     ]

-- Requests

createHAPGTest :: CreateHAPG -> TestTree
createHAPGTest = undefined

createHSMTest :: CreateHSM -> TestTree
createHSMTest = undefined

createLunaClientTest :: CreateLunaClient -> TestTree
createLunaClientTest = undefined

deleteHAPGTest :: DeleteHAPG -> TestTree
deleteHAPGTest = undefined

deleteHSMTest :: DeleteHSM -> TestTree
deleteHSMTest = undefined

deleteLunaClientTest :: DeleteLunaClient -> TestTree
deleteLunaClientTest = undefined

describeHAPGTest :: DescribeHAPG -> TestTree
describeHAPGTest = undefined

describeHSMTest :: DescribeHSM -> TestTree
describeHSMTest = undefined

describeLunaClientTest :: DescribeLunaClient -> TestTree
describeLunaClientTest = undefined

getConfigTest :: GetConfig -> TestTree
getConfigTest = undefined

listAvailableZonesTest :: ListAvailableZones -> TestTree
listAvailableZonesTest = undefined

listHAPGsTest :: ListHAPGs -> TestTree
listHAPGsTest = undefined

listHSMsTest :: ListHSMs -> TestTree
listHSMsTest = undefined

listLunaClientsTest :: ListLunaClients -> TestTree
listLunaClientsTest = undefined

modifyHAPGTest :: ModifyHAPG -> TestTree
modifyHAPGTest = undefined

modifyHSMTest :: ModifyHSM -> TestTree
modifyHSMTest = undefined

modifyLunaClientTest :: ModifyLunaClient -> TestTree
modifyLunaClientTest = undefined

-- Responses

createHAPGResponseTest :: CreateHAPGResponse -> TestTree
createHAPGResponseTest = resp
    "createHAPGResponse"
    "fixture/CreateHAPGResponse"
    (Proxy :: Proxy CreateHAPG)

createHSMResponseTest :: CreateHSMResponse -> TestTree
createHSMResponseTest = resp
    "createHSMResponse"
    "fixture/CreateHSMResponse"
    (Proxy :: Proxy CreateHSM)

createLunaClientResponseTest :: CreateLunaClientResponse -> TestTree
createLunaClientResponseTest = resp
    "createLunaClientResponse"
    "fixture/CreateLunaClientResponse"
    (Proxy :: Proxy CreateLunaClient)

deleteHAPGResponseTest :: DeleteHAPGResponse -> TestTree
deleteHAPGResponseTest = resp
    "deleteHAPGResponse"
    "fixture/DeleteHAPGResponse"
    (Proxy :: Proxy DeleteHAPG)

deleteHSMResponseTest :: DeleteHSMResponse -> TestTree
deleteHSMResponseTest = resp
    "deleteHSMResponse"
    "fixture/DeleteHSMResponse"
    (Proxy :: Proxy DeleteHSM)

deleteLunaClientResponseTest :: DeleteLunaClientResponse -> TestTree
deleteLunaClientResponseTest = resp
    "deleteLunaClientResponse"
    "fixture/DeleteLunaClientResponse"
    (Proxy :: Proxy DeleteLunaClient)

describeHAPGResponseTest :: DescribeHAPGResponse -> TestTree
describeHAPGResponseTest = resp
    "describeHAPGResponse"
    "fixture/DescribeHAPGResponse"
    (Proxy :: Proxy DescribeHAPG)

describeHSMResponseTest :: DescribeHSMResponse -> TestTree
describeHSMResponseTest = resp
    "describeHSMResponse"
    "fixture/DescribeHSMResponse"
    (Proxy :: Proxy DescribeHSM)

describeLunaClientResponseTest :: DescribeLunaClientResponse -> TestTree
describeLunaClientResponseTest = resp
    "describeLunaClientResponse"
    "fixture/DescribeLunaClientResponse"
    (Proxy :: Proxy DescribeLunaClient)

getConfigResponseTest :: GetConfigResponse -> TestTree
getConfigResponseTest = resp
    "getConfigResponse"
    "fixture/GetConfigResponse"
    (Proxy :: Proxy GetConfig)

listAvailableZonesResponseTest :: ListAvailableZonesResponse -> TestTree
listAvailableZonesResponseTest = resp
    "listAvailableZonesResponse"
    "fixture/ListAvailableZonesResponse"
    (Proxy :: Proxy ListAvailableZones)

listHAPGsResponseTest :: ListHAPGsResponse -> TestTree
listHAPGsResponseTest = resp
    "listHAPGsResponse"
    "fixture/ListHAPGsResponse"
    (Proxy :: Proxy ListHAPGs)

listHSMsResponseTest :: ListHSMsResponse -> TestTree
listHSMsResponseTest = resp
    "listHSMsResponse"
    "fixture/ListHSMsResponse"
    (Proxy :: Proxy ListHSMs)

listLunaClientsResponseTest :: ListLunaClientsResponse -> TestTree
listLunaClientsResponseTest = resp
    "listLunaClientsResponse"
    "fixture/ListLunaClientsResponse"
    (Proxy :: Proxy ListLunaClients)

modifyHAPGResponseTest :: ModifyHAPGResponse -> TestTree
modifyHAPGResponseTest = resp
    "modifyHAPGResponse"
    "fixture/ModifyHAPGResponse"
    (Proxy :: Proxy ModifyHAPG)

modifyHSMResponseTest :: ModifyHSMResponse -> TestTree
modifyHSMResponseTest = resp
    "modifyHSMResponse"
    "fixture/ModifyHSMResponse"
    (Proxy :: Proxy ModifyHSM)

modifyLunaClientResponseTest :: ModifyLunaClientResponse -> TestTree
modifyLunaClientResponseTest = resp
    "modifyLunaClientResponse"
    "fixture/ModifyLunaClientResponse"
    (Proxy :: Proxy ModifyLunaClient)

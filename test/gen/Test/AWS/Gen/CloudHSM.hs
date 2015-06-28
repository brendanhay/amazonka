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

import           Data.Proxy
import           Network.AWS.CloudHSM
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ listHAPGsTest $
--             listHAPGs
--
--         , listHSMsTest $
--             listHSMs
--
--         , deleteHAPGTest $
--             deleteHAPG
--
--         , modifyLunaClientTest $
--             modifyLunaClient
--
--         , describeLunaClientTest $
--             describeLunaClient
--
--         , createHSMTest $
--             createHSM
--
--         , createHAPGTest $
--             createHAPG
--
--         , createLunaClientTest $
--             createLunaClient
--
--         , describeHAPGTest $
--             describeHAPG
--
--         , listLunaClientsTest $
--             listLunaClients
--
--         , getConfigTest $
--             getConfig
--
--         , deleteHSMTest $
--             deleteHSM
--
--         , describeHSMTest $
--             describeHSM
--
--         , modifyHAPGTest $
--             modifyHAPG
--
--         , listAvailableZonesTest $
--             listAvailableZones
--
--         , modifyHSMTest $
--             modifyHSM
--
--         , deleteLunaClientTest $
--             deleteLunaClient
--
--           ]

--     , testGroup "response"
--         [ listHAPGsResponseTest $
--             listHAPGsResponse
--
--         , listHSMsResponseTest $
--             listHSMsResponse
--
--         , deleteHAPGResponseTest $
--             deleteHAPGResponse
--
--         , modifyLunaClientResponseTest $
--             modifyLunaClientResponse
--
--         , describeLunaClientResponseTest $
--             describeLunaClientResponse
--
--         , createHSMResponseTest $
--             createHSMResponse
--
--         , createHAPGResponseTest $
--             createHAPGResponse
--
--         , createLunaClientResponseTest $
--             createLunaClientResponse
--
--         , describeHAPGResponseTest $
--             describeHAPGResponse
--
--         , listLunaClientsResponseTest $
--             listLunaClientsResponse
--
--         , getConfigResponseTest $
--             getConfigResponse
--
--         , deleteHSMResponseTest $
--             deleteHSMResponse
--
--         , describeHSMResponseTest $
--             describeHSMResponse
--
--         , modifyHAPGResponseTest $
--             modifyHAPGResponse
--
--         , listAvailableZonesResponseTest $
--             listAvailableZonesResponse
--
--         , modifyHSMResponseTest $
--             modifyHSMResponse
--
--         , deleteLunaClientResponseTest $
--             deleteLunaClientResponse
--
--           ]
--     ]

-- Requests

listHAPGsTest :: ListHAPGs -> TestTree
listHAPGsTest = undefined

listHSMsTest :: ListHSMs -> TestTree
listHSMsTest = undefined

deleteHAPGTest :: DeleteHAPG -> TestTree
deleteHAPGTest = undefined

modifyLunaClientTest :: ModifyLunaClient -> TestTree
modifyLunaClientTest = undefined

describeLunaClientTest :: DescribeLunaClient -> TestTree
describeLunaClientTest = undefined

createHSMTest :: CreateHSM -> TestTree
createHSMTest = undefined

createHAPGTest :: CreateHAPG -> TestTree
createHAPGTest = undefined

createLunaClientTest :: CreateLunaClient -> TestTree
createLunaClientTest = undefined

describeHAPGTest :: DescribeHAPG -> TestTree
describeHAPGTest = undefined

listLunaClientsTest :: ListLunaClients -> TestTree
listLunaClientsTest = undefined

getConfigTest :: GetConfig -> TestTree
getConfigTest = undefined

deleteHSMTest :: DeleteHSM -> TestTree
deleteHSMTest = undefined

describeHSMTest :: DescribeHSM -> TestTree
describeHSMTest = undefined

modifyHAPGTest :: ModifyHAPG -> TestTree
modifyHAPGTest = undefined

listAvailableZonesTest :: ListAvailableZones -> TestTree
listAvailableZonesTest = undefined

modifyHSMTest :: ModifyHSM -> TestTree
modifyHSMTest = undefined

deleteLunaClientTest :: DeleteLunaClient -> TestTree
deleteLunaClientTest = undefined

-- Responses

listHAPGsResponseTest :: ListHAPGsResponse -> TestTree
listHAPGsResponseTest = resp
    "ListHAPGs"
    "fixture/CloudHSM/ListHAPGsResponse"
    (Proxy :: Proxy ListHAPGs)

listHSMsResponseTest :: ListHSMsResponse -> TestTree
listHSMsResponseTest = resp
    "ListHSMs"
    "fixture/CloudHSM/ListHSMsResponse"
    (Proxy :: Proxy ListHSMs)

deleteHAPGResponseTest :: DeleteHAPGResponse -> TestTree
deleteHAPGResponseTest = resp
    "DeleteHAPG"
    "fixture/CloudHSM/DeleteHAPGResponse"
    (Proxy :: Proxy DeleteHAPG)

modifyLunaClientResponseTest :: ModifyLunaClientResponse -> TestTree
modifyLunaClientResponseTest = resp
    "ModifyLunaClient"
    "fixture/CloudHSM/ModifyLunaClientResponse"
    (Proxy :: Proxy ModifyLunaClient)

describeLunaClientResponseTest :: DescribeLunaClientResponse -> TestTree
describeLunaClientResponseTest = resp
    "DescribeLunaClient"
    "fixture/CloudHSM/DescribeLunaClientResponse"
    (Proxy :: Proxy DescribeLunaClient)

createHSMResponseTest :: CreateHSMResponse -> TestTree
createHSMResponseTest = resp
    "CreateHSM"
    "fixture/CloudHSM/CreateHSMResponse"
    (Proxy :: Proxy CreateHSM)

createHAPGResponseTest :: CreateHAPGResponse -> TestTree
createHAPGResponseTest = resp
    "CreateHAPG"
    "fixture/CloudHSM/CreateHAPGResponse"
    (Proxy :: Proxy CreateHAPG)

createLunaClientResponseTest :: CreateLunaClientResponse -> TestTree
createLunaClientResponseTest = resp
    "CreateLunaClient"
    "fixture/CloudHSM/CreateLunaClientResponse"
    (Proxy :: Proxy CreateLunaClient)

describeHAPGResponseTest :: DescribeHAPGResponse -> TestTree
describeHAPGResponseTest = resp
    "DescribeHAPG"
    "fixture/CloudHSM/DescribeHAPGResponse"
    (Proxy :: Proxy DescribeHAPG)

listLunaClientsResponseTest :: ListLunaClientsResponse -> TestTree
listLunaClientsResponseTest = resp
    "ListLunaClients"
    "fixture/CloudHSM/ListLunaClientsResponse"
    (Proxy :: Proxy ListLunaClients)

getConfigResponseTest :: GetConfigResponse -> TestTree
getConfigResponseTest = resp
    "GetConfig"
    "fixture/CloudHSM/GetConfigResponse"
    (Proxy :: Proxy GetConfig)

deleteHSMResponseTest :: DeleteHSMResponse -> TestTree
deleteHSMResponseTest = resp
    "DeleteHSM"
    "fixture/CloudHSM/DeleteHSMResponse"
    (Proxy :: Proxy DeleteHSM)

describeHSMResponseTest :: DescribeHSMResponse -> TestTree
describeHSMResponseTest = resp
    "DescribeHSM"
    "fixture/CloudHSM/DescribeHSMResponse"
    (Proxy :: Proxy DescribeHSM)

modifyHAPGResponseTest :: ModifyHAPGResponse -> TestTree
modifyHAPGResponseTest = resp
    "ModifyHAPG"
    "fixture/CloudHSM/ModifyHAPGResponse"
    (Proxy :: Proxy ModifyHAPG)

listAvailableZonesResponseTest :: ListAvailableZonesResponse -> TestTree
listAvailableZonesResponseTest = resp
    "ListAvailableZones"
    "fixture/CloudHSM/ListAvailableZonesResponse"
    (Proxy :: Proxy ListAvailableZones)

modifyHSMResponseTest :: ModifyHSMResponse -> TestTree
modifyHSMResponseTest = resp
    "ModifyHSM"
    "fixture/CloudHSM/ModifyHSMResponse"
    (Proxy :: Proxy ModifyHSM)

deleteLunaClientResponseTest :: DeleteLunaClientResponse -> TestTree
deleteLunaClientResponseTest = resp
    "DeleteLunaClient"
    "fixture/CloudHSM/DeleteLunaClientResponse"
    (Proxy :: Proxy DeleteLunaClient)

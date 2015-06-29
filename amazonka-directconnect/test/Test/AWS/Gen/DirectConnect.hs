-- Module      : Test.AWS.Gen.DirectConnect
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

module Test.AWS.Gen.DirectConnect where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.DirectConnect

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ allocateConnectionOnInterconnectTest $
--             allocateConnectionOnInterconnect
--
--         , allocatePrivateVirtualInterfaceTest $
--             allocatePrivateVirtualInterface
--
--         , allocatePublicVirtualInterfaceTest $
--             allocatePublicVirtualInterface
--
--         , confirmConnectionTest $
--             confirmConnection
--
--         , confirmPrivateVirtualInterfaceTest $
--             confirmPrivateVirtualInterface
--
--         , confirmPublicVirtualInterfaceTest $
--             confirmPublicVirtualInterface
--
--         , createConnectionTest $
--             createConnection
--
--         , createInterconnectTest $
--             createInterconnect
--
--         , createPrivateVirtualInterfaceTest $
--             createPrivateVirtualInterface
--
--         , createPublicVirtualInterfaceTest $
--             createPublicVirtualInterface
--
--         , deleteConnectionTest $
--             deleteConnection
--
--         , deleteInterconnectTest $
--             deleteInterconnect
--
--         , deleteVirtualInterfaceTest $
--             deleteVirtualInterface
--
--         , describeConnectionsTest $
--             describeConnections
--
--         , describeConnectionsOnInterconnectTest $
--             describeConnectionsOnInterconnect
--
--         , describeInterconnectsTest $
--             describeInterconnects
--
--         , describeLocationsTest $
--             describeLocations
--
--         , describeVirtualGatewaysTest $
--             describeVirtualGateways
--
--         , describeVirtualInterfacesTest $
--             describeVirtualInterfaces
--
--           ]

--     , testGroup "response"
--         [ allocateConnectionOnInterconnectResponseTest $
--             connection
--
--         , allocatePrivateVirtualInterfaceResponseTest $
--             virtualInterface
--
--         , allocatePublicVirtualInterfaceResponseTest $
--             virtualInterface
--
--         , confirmConnectionResponseTest $
--             confirmConnectionResponse
--
--         , confirmPrivateVirtualInterfaceResponseTest $
--             confirmPrivateVirtualInterfaceResponse
--
--         , confirmPublicVirtualInterfaceResponseTest $
--             confirmPublicVirtualInterfaceResponse
--
--         , createConnectionResponseTest $
--             connection
--
--         , createInterconnectResponseTest $
--             interconnect
--
--         , createPrivateVirtualInterfaceResponseTest $
--             virtualInterface
--
--         , createPublicVirtualInterfaceResponseTest $
--             virtualInterface
--
--         , deleteConnectionResponseTest $
--             connection
--
--         , deleteInterconnectResponseTest $
--             deleteInterconnectResponse
--
--         , deleteVirtualInterfaceResponseTest $
--             deleteVirtualInterfaceResponse
--
--         , describeConnectionsResponseTest $
--             connections
--
--         , describeConnectionsOnInterconnectResponseTest $
--             connections
--
--         , describeInterconnectsResponseTest $
--             describeInterconnectsResponse
--
--         , describeLocationsResponseTest $
--             describeLocationsResponse
--
--         , describeVirtualGatewaysResponseTest $
--             describeVirtualGatewaysResponse
--
--         , describeVirtualInterfacesResponseTest $
--             describeVirtualInterfacesResponse
--
--           ]
--     ]

-- Requests

allocateConnectionOnInterconnectTest :: AllocateConnectionOnInterconnect -> TestTree
allocateConnectionOnInterconnectTest = undefined

allocatePrivateVirtualInterfaceTest :: AllocatePrivateVirtualInterface -> TestTree
allocatePrivateVirtualInterfaceTest = undefined

allocatePublicVirtualInterfaceTest :: AllocatePublicVirtualInterface -> TestTree
allocatePublicVirtualInterfaceTest = undefined

confirmConnectionTest :: ConfirmConnection -> TestTree
confirmConnectionTest = undefined

confirmPrivateVirtualInterfaceTest :: ConfirmPrivateVirtualInterface -> TestTree
confirmPrivateVirtualInterfaceTest = undefined

confirmPublicVirtualInterfaceTest :: ConfirmPublicVirtualInterface -> TestTree
confirmPublicVirtualInterfaceTest = undefined

createConnectionTest :: CreateConnection -> TestTree
createConnectionTest = undefined

createInterconnectTest :: CreateInterconnect -> TestTree
createInterconnectTest = undefined

createPrivateVirtualInterfaceTest :: CreatePrivateVirtualInterface -> TestTree
createPrivateVirtualInterfaceTest = undefined

createPublicVirtualInterfaceTest :: CreatePublicVirtualInterface -> TestTree
createPublicVirtualInterfaceTest = undefined

deleteConnectionTest :: DeleteConnection -> TestTree
deleteConnectionTest = undefined

deleteInterconnectTest :: DeleteInterconnect -> TestTree
deleteInterconnectTest = undefined

deleteVirtualInterfaceTest :: DeleteVirtualInterface -> TestTree
deleteVirtualInterfaceTest = undefined

describeConnectionsTest :: DescribeConnections -> TestTree
describeConnectionsTest = undefined

describeConnectionsOnInterconnectTest :: DescribeConnectionsOnInterconnect -> TestTree
describeConnectionsOnInterconnectTest = undefined

describeInterconnectsTest :: DescribeInterconnects -> TestTree
describeInterconnectsTest = undefined

describeLocationsTest :: DescribeLocations -> TestTree
describeLocationsTest = undefined

describeVirtualGatewaysTest :: DescribeVirtualGateways -> TestTree
describeVirtualGatewaysTest = undefined

describeVirtualInterfacesTest :: DescribeVirtualInterfaces -> TestTree
describeVirtualInterfacesTest = undefined

-- Responses

allocateConnectionOnInterconnectResponseTest :: Connection -> TestTree
allocateConnectionOnInterconnectResponseTest = resp
    "allocateConnectionOnInterconnectResponse"
    "fixture/Connection"
    (Proxy :: Proxy AllocateConnectionOnInterconnect)

allocatePrivateVirtualInterfaceResponseTest :: VirtualInterface -> TestTree
allocatePrivateVirtualInterfaceResponseTest = resp
    "allocatePrivateVirtualInterfaceResponse"
    "fixture/VirtualInterface"
    (Proxy :: Proxy AllocatePrivateVirtualInterface)

allocatePublicVirtualInterfaceResponseTest :: VirtualInterface -> TestTree
allocatePublicVirtualInterfaceResponseTest = resp
    "allocatePublicVirtualInterfaceResponse"
    "fixture/VirtualInterface"
    (Proxy :: Proxy AllocatePublicVirtualInterface)

confirmConnectionResponseTest :: ConfirmConnectionResponse -> TestTree
confirmConnectionResponseTest = resp
    "confirmConnectionResponse"
    "fixture/ConfirmConnectionResponse"
    (Proxy :: Proxy ConfirmConnection)

confirmPrivateVirtualInterfaceResponseTest :: ConfirmPrivateVirtualInterfaceResponse -> TestTree
confirmPrivateVirtualInterfaceResponseTest = resp
    "confirmPrivateVirtualInterfaceResponse"
    "fixture/ConfirmPrivateVirtualInterfaceResponse"
    (Proxy :: Proxy ConfirmPrivateVirtualInterface)

confirmPublicVirtualInterfaceResponseTest :: ConfirmPublicVirtualInterfaceResponse -> TestTree
confirmPublicVirtualInterfaceResponseTest = resp
    "confirmPublicVirtualInterfaceResponse"
    "fixture/ConfirmPublicVirtualInterfaceResponse"
    (Proxy :: Proxy ConfirmPublicVirtualInterface)

createConnectionResponseTest :: Connection -> TestTree
createConnectionResponseTest = resp
    "createConnectionResponse"
    "fixture/Connection"
    (Proxy :: Proxy CreateConnection)

createInterconnectResponseTest :: Interconnect -> TestTree
createInterconnectResponseTest = resp
    "createInterconnectResponse"
    "fixture/Interconnect"
    (Proxy :: Proxy CreateInterconnect)

createPrivateVirtualInterfaceResponseTest :: VirtualInterface -> TestTree
createPrivateVirtualInterfaceResponseTest = resp
    "createPrivateVirtualInterfaceResponse"
    "fixture/VirtualInterface"
    (Proxy :: Proxy CreatePrivateVirtualInterface)

createPublicVirtualInterfaceResponseTest :: VirtualInterface -> TestTree
createPublicVirtualInterfaceResponseTest = resp
    "createPublicVirtualInterfaceResponse"
    "fixture/VirtualInterface"
    (Proxy :: Proxy CreatePublicVirtualInterface)

deleteConnectionResponseTest :: Connection -> TestTree
deleteConnectionResponseTest = resp
    "deleteConnectionResponse"
    "fixture/Connection"
    (Proxy :: Proxy DeleteConnection)

deleteInterconnectResponseTest :: DeleteInterconnectResponse -> TestTree
deleteInterconnectResponseTest = resp
    "deleteInterconnectResponse"
    "fixture/DeleteInterconnectResponse"
    (Proxy :: Proxy DeleteInterconnect)

deleteVirtualInterfaceResponseTest :: DeleteVirtualInterfaceResponse -> TestTree
deleteVirtualInterfaceResponseTest = resp
    "deleteVirtualInterfaceResponse"
    "fixture/DeleteVirtualInterfaceResponse"
    (Proxy :: Proxy DeleteVirtualInterface)

describeConnectionsResponseTest :: Connections -> TestTree
describeConnectionsResponseTest = resp
    "describeConnectionsResponse"
    "fixture/Connections"
    (Proxy :: Proxy DescribeConnections)

describeConnectionsOnInterconnectResponseTest :: Connections -> TestTree
describeConnectionsOnInterconnectResponseTest = resp
    "describeConnectionsOnInterconnectResponse"
    "fixture/Connections"
    (Proxy :: Proxy DescribeConnectionsOnInterconnect)

describeInterconnectsResponseTest :: DescribeInterconnectsResponse -> TestTree
describeInterconnectsResponseTest = resp
    "describeInterconnectsResponse"
    "fixture/DescribeInterconnectsResponse"
    (Proxy :: Proxy DescribeInterconnects)

describeLocationsResponseTest :: DescribeLocationsResponse -> TestTree
describeLocationsResponseTest = resp
    "describeLocationsResponse"
    "fixture/DescribeLocationsResponse"
    (Proxy :: Proxy DescribeLocations)

describeVirtualGatewaysResponseTest :: DescribeVirtualGatewaysResponse -> TestTree
describeVirtualGatewaysResponseTest = resp
    "describeVirtualGatewaysResponse"
    "fixture/DescribeVirtualGatewaysResponse"
    (Proxy :: Proxy DescribeVirtualGateways)

describeVirtualInterfacesResponseTest :: DescribeVirtualInterfacesResponse -> TestTree
describeVirtualInterfacesResponseTest = resp
    "describeVirtualInterfacesResponse"
    "fixture/DescribeVirtualInterfacesResponse"
    (Proxy :: Proxy DescribeVirtualInterfaces)

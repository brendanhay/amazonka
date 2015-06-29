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
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeInterconnects $
--             describeInterconnects
--
--         , testDeleteConnection $
--             deleteConnection
--
--         , testCreateConnection $
--             createConnection
--
--         , testDescribeConnections $
--             describeConnections
--
--         , testDeleteInterconnect $
--             deleteInterconnect
--
--         , testConfirmPrivateVirtualInterface $
--             confirmPrivateVirtualInterface
--
--         , testDescribeConnectionsOnInterconnect $
--             describeConnectionsOnInterconnect
--
--         , testDescribeLocations $
--             describeLocations
--
--         , testCreatePublicVirtualInterface $
--             createPublicVirtualInterface
--
--         , testAllocatePrivateVirtualInterface $
--             allocatePrivateVirtualInterface
--
--         , testConfirmConnection $
--             confirmConnection
--
--         , testDescribeVirtualGateways $
--             describeVirtualGateways
--
--         , testConfirmPublicVirtualInterface $
--             confirmPublicVirtualInterface
--
--         , testDescribeVirtualInterfaces $
--             describeVirtualInterfaces
--
--         , testCreatePrivateVirtualInterface $
--             createPrivateVirtualInterface
--
--         , testDeleteVirtualInterface $
--             deleteVirtualInterface
--
--         , testAllocatePublicVirtualInterface $
--             allocatePublicVirtualInterface
--
--         , testAllocateConnectionOnInterconnect $
--             allocateConnectionOnInterconnect
--
--         , testCreateInterconnect $
--             createInterconnect
--
--           ]

--     , testGroup "response"
--         [ testDescribeInterconnectsResponse $
--             describeInterconnectsResponse
--
--         , testDeleteConnectionResponse $
--             connection
--
--         , testCreateConnectionResponse $
--             connection
--
--         , testDescribeConnectionsResponse $
--             connections
--
--         , testDeleteInterconnectResponse $
--             deleteInterconnectResponse
--
--         , testConfirmPrivateVirtualInterfaceResponse $
--             confirmPrivateVirtualInterfaceResponse
--
--         , testDescribeConnectionsOnInterconnectResponse $
--             connections
--
--         , testDescribeLocationsResponse $
--             describeLocationsResponse
--
--         , testCreatePublicVirtualInterfaceResponse $
--             virtualInterface
--
--         , testAllocatePrivateVirtualInterfaceResponse $
--             virtualInterface
--
--         , testConfirmConnectionResponse $
--             confirmConnectionResponse
--
--         , testDescribeVirtualGatewaysResponse $
--             describeVirtualGatewaysResponse
--
--         , testConfirmPublicVirtualInterfaceResponse $
--             confirmPublicVirtualInterfaceResponse
--
--         , testDescribeVirtualInterfacesResponse $
--             describeVirtualInterfacesResponse
--
--         , testCreatePrivateVirtualInterfaceResponse $
--             virtualInterface
--
--         , testDeleteVirtualInterfaceResponse $
--             deleteVirtualInterfaceResponse
--
--         , testAllocatePublicVirtualInterfaceResponse $
--             virtualInterface
--
--         , testAllocateConnectionOnInterconnectResponse $
--             connection
--
--         , testCreateInterconnectResponse $
--             interconnect
--
--           ]
--     ]

-- Requests

testDescribeInterconnects :: DescribeInterconnects -> TestTree
testDescribeInterconnects = undefined

testDeleteConnection :: DeleteConnection -> TestTree
testDeleteConnection = undefined

testCreateConnection :: CreateConnection -> TestTree
testCreateConnection = undefined

testDescribeConnections :: DescribeConnections -> TestTree
testDescribeConnections = undefined

testDeleteInterconnect :: DeleteInterconnect -> TestTree
testDeleteInterconnect = undefined

testConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterface -> TestTree
testConfirmPrivateVirtualInterface = undefined

testDescribeConnectionsOnInterconnect :: DescribeConnectionsOnInterconnect -> TestTree
testDescribeConnectionsOnInterconnect = undefined

testDescribeLocations :: DescribeLocations -> TestTree
testDescribeLocations = undefined

testCreatePublicVirtualInterface :: CreatePublicVirtualInterface -> TestTree
testCreatePublicVirtualInterface = undefined

testAllocatePrivateVirtualInterface :: AllocatePrivateVirtualInterface -> TestTree
testAllocatePrivateVirtualInterface = undefined

testConfirmConnection :: ConfirmConnection -> TestTree
testConfirmConnection = undefined

testDescribeVirtualGateways :: DescribeVirtualGateways -> TestTree
testDescribeVirtualGateways = undefined

testConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterface -> TestTree
testConfirmPublicVirtualInterface = undefined

testDescribeVirtualInterfaces :: DescribeVirtualInterfaces -> TestTree
testDescribeVirtualInterfaces = undefined

testCreatePrivateVirtualInterface :: CreatePrivateVirtualInterface -> TestTree
testCreatePrivateVirtualInterface = undefined

testDeleteVirtualInterface :: DeleteVirtualInterface -> TestTree
testDeleteVirtualInterface = undefined

testAllocatePublicVirtualInterface :: AllocatePublicVirtualInterface -> TestTree
testAllocatePublicVirtualInterface = undefined

testAllocateConnectionOnInterconnect :: AllocateConnectionOnInterconnect -> TestTree
testAllocateConnectionOnInterconnect = undefined

testCreateInterconnect :: CreateInterconnect -> TestTree
testCreateInterconnect = undefined

-- Responses

testDescribeInterconnectsResponse :: DescribeInterconnectsResponse -> TestTree
testDescribeInterconnectsResponse = resp
    "DescribeInterconnectsResponse"
    "fixture/DescribeInterconnectsResponse"
    (Proxy :: Proxy DescribeInterconnects)

testDeleteConnectionResponse :: Connection -> TestTree
testDeleteConnectionResponse = resp
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse"
    (Proxy :: Proxy DeleteConnection)

testCreateConnectionResponse :: Connection -> TestTree
testCreateConnectionResponse = resp
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse"
    (Proxy :: Proxy CreateConnection)

testDescribeConnectionsResponse :: Connections -> TestTree
testDescribeConnectionsResponse = resp
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse"
    (Proxy :: Proxy DescribeConnections)

testDeleteInterconnectResponse :: DeleteInterconnectResponse -> TestTree
testDeleteInterconnectResponse = resp
    "DeleteInterconnectResponse"
    "fixture/DeleteInterconnectResponse"
    (Proxy :: Proxy DeleteInterconnect)

testConfirmPrivateVirtualInterfaceResponse :: ConfirmPrivateVirtualInterfaceResponse -> TestTree
testConfirmPrivateVirtualInterfaceResponse = resp
    "ConfirmPrivateVirtualInterfaceResponse"
    "fixture/ConfirmPrivateVirtualInterfaceResponse"
    (Proxy :: Proxy ConfirmPrivateVirtualInterface)

testDescribeConnectionsOnInterconnectResponse :: Connections -> TestTree
testDescribeConnectionsOnInterconnectResponse = resp
    "DescribeConnectionsOnInterconnectResponse"
    "fixture/DescribeConnectionsOnInterconnectResponse"
    (Proxy :: Proxy DescribeConnectionsOnInterconnect)

testDescribeLocationsResponse :: DescribeLocationsResponse -> TestTree
testDescribeLocationsResponse = resp
    "DescribeLocationsResponse"
    "fixture/DescribeLocationsResponse"
    (Proxy :: Proxy DescribeLocations)

testCreatePublicVirtualInterfaceResponse :: VirtualInterface -> TestTree
testCreatePublicVirtualInterfaceResponse = resp
    "CreatePublicVirtualInterfaceResponse"
    "fixture/CreatePublicVirtualInterfaceResponse"
    (Proxy :: Proxy CreatePublicVirtualInterface)

testAllocatePrivateVirtualInterfaceResponse :: VirtualInterface -> TestTree
testAllocatePrivateVirtualInterfaceResponse = resp
    "AllocatePrivateVirtualInterfaceResponse"
    "fixture/AllocatePrivateVirtualInterfaceResponse"
    (Proxy :: Proxy AllocatePrivateVirtualInterface)

testConfirmConnectionResponse :: ConfirmConnectionResponse -> TestTree
testConfirmConnectionResponse = resp
    "ConfirmConnectionResponse"
    "fixture/ConfirmConnectionResponse"
    (Proxy :: Proxy ConfirmConnection)

testDescribeVirtualGatewaysResponse :: DescribeVirtualGatewaysResponse -> TestTree
testDescribeVirtualGatewaysResponse = resp
    "DescribeVirtualGatewaysResponse"
    "fixture/DescribeVirtualGatewaysResponse"
    (Proxy :: Proxy DescribeVirtualGateways)

testConfirmPublicVirtualInterfaceResponse :: ConfirmPublicVirtualInterfaceResponse -> TestTree
testConfirmPublicVirtualInterfaceResponse = resp
    "ConfirmPublicVirtualInterfaceResponse"
    "fixture/ConfirmPublicVirtualInterfaceResponse"
    (Proxy :: Proxy ConfirmPublicVirtualInterface)

testDescribeVirtualInterfacesResponse :: DescribeVirtualInterfacesResponse -> TestTree
testDescribeVirtualInterfacesResponse = resp
    "DescribeVirtualInterfacesResponse"
    "fixture/DescribeVirtualInterfacesResponse"
    (Proxy :: Proxy DescribeVirtualInterfaces)

testCreatePrivateVirtualInterfaceResponse :: VirtualInterface -> TestTree
testCreatePrivateVirtualInterfaceResponse = resp
    "CreatePrivateVirtualInterfaceResponse"
    "fixture/CreatePrivateVirtualInterfaceResponse"
    (Proxy :: Proxy CreatePrivateVirtualInterface)

testDeleteVirtualInterfaceResponse :: DeleteVirtualInterfaceResponse -> TestTree
testDeleteVirtualInterfaceResponse = resp
    "DeleteVirtualInterfaceResponse"
    "fixture/DeleteVirtualInterfaceResponse"
    (Proxy :: Proxy DeleteVirtualInterface)

testAllocatePublicVirtualInterfaceResponse :: VirtualInterface -> TestTree
testAllocatePublicVirtualInterfaceResponse = resp
    "AllocatePublicVirtualInterfaceResponse"
    "fixture/AllocatePublicVirtualInterfaceResponse"
    (Proxy :: Proxy AllocatePublicVirtualInterface)

testAllocateConnectionOnInterconnectResponse :: Connection -> TestTree
testAllocateConnectionOnInterconnectResponse = resp
    "AllocateConnectionOnInterconnectResponse"
    "fixture/AllocateConnectionOnInterconnectResponse"
    (Proxy :: Proxy AllocateConnectionOnInterconnect)

testCreateInterconnectResponse :: Interconnect -> TestTree
testCreateInterconnectResponse = resp
    "CreateInterconnectResponse"
    "fixture/CreateInterconnectResponse"
    (Proxy :: Proxy CreateInterconnect)

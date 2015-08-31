{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectConnect
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DirectConnect where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DirectConnect
import Test.AWS.DirectConnect.Internal

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
--         , testDescribeConnectionsOnInterconnect $
--             describeConnectionsOnInterconnect
--
--         , testDeleteInterconnect $
--             deleteInterconnect
--
--         , testConfirmPrivateVirtualInterface $
--             confirmPrivateVirtualInterface
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
--         , testConfirmPublicVirtualInterface $
--             confirmPublicVirtualInterface
--
--         , testDescribeVirtualGateways $
--             describeVirtualGateways
--
--         , testDescribeVirtualInterfaces $
--             describeVirtualInterfaces
--
--         , testDeleteVirtualInterface $
--             deleteVirtualInterface
--
--         , testCreatePrivateVirtualInterface $
--             createPrivateVirtualInterface
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
--         , testDescribeConnectionsOnInterconnectResponse $
--             connections
--
--         , testDeleteInterconnectResponse $
--             deleteInterconnectResponse
--
--         , testConfirmPrivateVirtualInterfaceResponse $
--             confirmPrivateVirtualInterfaceResponse
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
--         , testConfirmPublicVirtualInterfaceResponse $
--             confirmPublicVirtualInterfaceResponse
--
--         , testDescribeVirtualGatewaysResponse $
--             describeVirtualGatewaysResponse
--
--         , testDescribeVirtualInterfacesResponse $
--             describeVirtualInterfacesResponse
--
--         , testDeleteVirtualInterfaceResponse $
--             deleteVirtualInterfaceResponse
--
--         , testCreatePrivateVirtualInterfaceResponse $
--             virtualInterface
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
testDescribeInterconnects = req
    "DescribeInterconnects"
    "fixture/DescribeInterconnects.yaml"

testDeleteConnection :: DeleteConnection -> TestTree
testDeleteConnection = req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

testCreateConnection :: CreateConnection -> TestTree
testCreateConnection = req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

testDescribeConnections :: DescribeConnections -> TestTree
testDescribeConnections = req
    "DescribeConnections"
    "fixture/DescribeConnections.yaml"

testDescribeConnectionsOnInterconnect :: DescribeConnectionsOnInterconnect -> TestTree
testDescribeConnectionsOnInterconnect = req
    "DescribeConnectionsOnInterconnect"
    "fixture/DescribeConnectionsOnInterconnect.yaml"

testDeleteInterconnect :: DeleteInterconnect -> TestTree
testDeleteInterconnect = req
    "DeleteInterconnect"
    "fixture/DeleteInterconnect.yaml"

testConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterface -> TestTree
testConfirmPrivateVirtualInterface = req
    "ConfirmPrivateVirtualInterface"
    "fixture/ConfirmPrivateVirtualInterface.yaml"

testDescribeLocations :: DescribeLocations -> TestTree
testDescribeLocations = req
    "DescribeLocations"
    "fixture/DescribeLocations.yaml"

testCreatePublicVirtualInterface :: CreatePublicVirtualInterface -> TestTree
testCreatePublicVirtualInterface = req
    "CreatePublicVirtualInterface"
    "fixture/CreatePublicVirtualInterface.yaml"

testAllocatePrivateVirtualInterface :: AllocatePrivateVirtualInterface -> TestTree
testAllocatePrivateVirtualInterface = req
    "AllocatePrivateVirtualInterface"
    "fixture/AllocatePrivateVirtualInterface.yaml"

testConfirmConnection :: ConfirmConnection -> TestTree
testConfirmConnection = req
    "ConfirmConnection"
    "fixture/ConfirmConnection.yaml"

testConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterface -> TestTree
testConfirmPublicVirtualInterface = req
    "ConfirmPublicVirtualInterface"
    "fixture/ConfirmPublicVirtualInterface.yaml"

testDescribeVirtualGateways :: DescribeVirtualGateways -> TestTree
testDescribeVirtualGateways = req
    "DescribeVirtualGateways"
    "fixture/DescribeVirtualGateways.yaml"

testDescribeVirtualInterfaces :: DescribeVirtualInterfaces -> TestTree
testDescribeVirtualInterfaces = req
    "DescribeVirtualInterfaces"
    "fixture/DescribeVirtualInterfaces.yaml"

testDeleteVirtualInterface :: DeleteVirtualInterface -> TestTree
testDeleteVirtualInterface = req
    "DeleteVirtualInterface"
    "fixture/DeleteVirtualInterface.yaml"

testCreatePrivateVirtualInterface :: CreatePrivateVirtualInterface -> TestTree
testCreatePrivateVirtualInterface = req
    "CreatePrivateVirtualInterface"
    "fixture/CreatePrivateVirtualInterface.yaml"

testAllocatePublicVirtualInterface :: AllocatePublicVirtualInterface -> TestTree
testAllocatePublicVirtualInterface = req
    "AllocatePublicVirtualInterface"
    "fixture/AllocatePublicVirtualInterface.yaml"

testAllocateConnectionOnInterconnect :: AllocateConnectionOnInterconnect -> TestTree
testAllocateConnectionOnInterconnect = req
    "AllocateConnectionOnInterconnect"
    "fixture/AllocateConnectionOnInterconnect.yaml"

testCreateInterconnect :: CreateInterconnect -> TestTree
testCreateInterconnect = req
    "CreateInterconnect"
    "fixture/CreateInterconnect.yaml"

-- Responses

testDescribeInterconnectsResponse :: DescribeInterconnectsResponse -> TestTree
testDescribeInterconnectsResponse = res
    "DescribeInterconnectsResponse"
    "fixture/DescribeInterconnectsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeInterconnects)

testDeleteConnectionResponse :: Connection -> TestTree
testDeleteConnectionResponse = res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteConnection)

testCreateConnectionResponse :: Connection -> TestTree
testCreateConnectionResponse = res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy CreateConnection)

testDescribeConnectionsResponse :: Connections -> TestTree
testDescribeConnectionsResponse = res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeConnections)

testDescribeConnectionsOnInterconnectResponse :: Connections -> TestTree
testDescribeConnectionsOnInterconnectResponse = res
    "DescribeConnectionsOnInterconnectResponse"
    "fixture/DescribeConnectionsOnInterconnectResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeConnectionsOnInterconnect)

testDeleteInterconnectResponse :: DeleteInterconnectResponse -> TestTree
testDeleteInterconnectResponse = res
    "DeleteInterconnectResponse"
    "fixture/DeleteInterconnectResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteInterconnect)

testConfirmPrivateVirtualInterfaceResponse :: ConfirmPrivateVirtualInterfaceResponse -> TestTree
testConfirmPrivateVirtualInterfaceResponse = res
    "ConfirmPrivateVirtualInterfaceResponse"
    "fixture/ConfirmPrivateVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy ConfirmPrivateVirtualInterface)

testDescribeLocationsResponse :: DescribeLocationsResponse -> TestTree
testDescribeLocationsResponse = res
    "DescribeLocationsResponse"
    "fixture/DescribeLocationsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeLocations)

testCreatePublicVirtualInterfaceResponse :: VirtualInterface -> TestTree
testCreatePublicVirtualInterfaceResponse = res
    "CreatePublicVirtualInterfaceResponse"
    "fixture/CreatePublicVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy CreatePublicVirtualInterface)

testAllocatePrivateVirtualInterfaceResponse :: VirtualInterface -> TestTree
testAllocatePrivateVirtualInterfaceResponse = res
    "AllocatePrivateVirtualInterfaceResponse"
    "fixture/AllocatePrivateVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy AllocatePrivateVirtualInterface)

testConfirmConnectionResponse :: ConfirmConnectionResponse -> TestTree
testConfirmConnectionResponse = res
    "ConfirmConnectionResponse"
    "fixture/ConfirmConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy ConfirmConnection)

testConfirmPublicVirtualInterfaceResponse :: ConfirmPublicVirtualInterfaceResponse -> TestTree
testConfirmPublicVirtualInterfaceResponse = res
    "ConfirmPublicVirtualInterfaceResponse"
    "fixture/ConfirmPublicVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy ConfirmPublicVirtualInterface)

testDescribeVirtualGatewaysResponse :: DescribeVirtualGatewaysResponse -> TestTree
testDescribeVirtualGatewaysResponse = res
    "DescribeVirtualGatewaysResponse"
    "fixture/DescribeVirtualGatewaysResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeVirtualGateways)

testDescribeVirtualInterfacesResponse :: DescribeVirtualInterfacesResponse -> TestTree
testDescribeVirtualInterfacesResponse = res
    "DescribeVirtualInterfacesResponse"
    "fixture/DescribeVirtualInterfacesResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeVirtualInterfaces)

testDeleteVirtualInterfaceResponse :: DeleteVirtualInterfaceResponse -> TestTree
testDeleteVirtualInterfaceResponse = res
    "DeleteVirtualInterfaceResponse"
    "fixture/DeleteVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteVirtualInterface)

testCreatePrivateVirtualInterfaceResponse :: VirtualInterface -> TestTree
testCreatePrivateVirtualInterfaceResponse = res
    "CreatePrivateVirtualInterfaceResponse"
    "fixture/CreatePrivateVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy CreatePrivateVirtualInterface)

testAllocatePublicVirtualInterfaceResponse :: VirtualInterface -> TestTree
testAllocatePublicVirtualInterfaceResponse = res
    "AllocatePublicVirtualInterfaceResponse"
    "fixture/AllocatePublicVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy AllocatePublicVirtualInterface)

testAllocateConnectionOnInterconnectResponse :: Connection -> TestTree
testAllocateConnectionOnInterconnectResponse = res
    "AllocateConnectionOnInterconnectResponse"
    "fixture/AllocateConnectionOnInterconnectResponse.proto"
    directConnect
    (Proxy :: Proxy AllocateConnectionOnInterconnect)

testCreateInterconnectResponse :: Interconnect -> TestTree
testCreateInterconnectResponse = res
    "CreateInterconnectResponse"
    "fixture/CreateInterconnectResponse.proto"
    directConnect
    (Proxy :: Proxy CreateInterconnect)

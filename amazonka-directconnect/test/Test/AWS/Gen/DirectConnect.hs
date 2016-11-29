{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectConnect
-- Copyright   : (c) 2013-2016 Brendan Hay
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
--         [ requestDescribeInterconnects $
--             describeInterconnects
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestDeleteConnection $
--             deleteConnection
--
--         , requestCreateConnection $
--             createConnection
--
--         , requestDescribeConnections $
--             describeConnections
--
--         , requestDescribeConnectionsOnInterconnect $
--             describeConnectionsOnInterconnect
--
--         , requestDeleteInterconnect $
--             deleteInterconnect
--
--         , requestConfirmPrivateVirtualInterface $
--             confirmPrivateVirtualInterface
--
--         , requestDescribeLocations $
--             describeLocations
--
--         , requestCreatePublicVirtualInterface $
--             createPublicVirtualInterface
--
--         , requestAllocatePrivateVirtualInterface $
--             allocatePrivateVirtualInterface
--
--         , requestConfirmConnection $
--             confirmConnection
--
--         , requestConfirmPublicVirtualInterface $
--             confirmPublicVirtualInterface
--
--         , requestDescribeVirtualGateways $
--             describeVirtualGateways
--
--         , requestDescribeVirtualInterfaces $
--             describeVirtualInterfaces
--
--         , requestDeleteVirtualInterface $
--             deleteVirtualInterface
--
--         , requestDescribeInterconnectLoa $
--             describeInterconnectLoa
--
--         , requestCreatePrivateVirtualInterface $
--             createPrivateVirtualInterface
--
--         , requestAllocatePublicVirtualInterface $
--             allocatePublicVirtualInterface
--
--         , requestAllocateConnectionOnInterconnect $
--             allocateConnectionOnInterconnect
--
--         , requestTagResource $
--             tagResource
--
--         , requestUntagResource $
--             untagResource
--
--         , requestCreateInterconnect $
--             createInterconnect
--
--         , requestDescribeConnectionLoa $
--             describeConnectionLoa
--
--           ]

--     , testGroup "response"
--         [ responseDescribeInterconnects $
--             describeInterconnectsResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseDeleteConnection $
--             connection
--
--         , responseCreateConnection $
--             connection
--
--         , responseDescribeConnections $
--             connections
--
--         , responseDescribeConnectionsOnInterconnect $
--             connections
--
--         , responseDeleteInterconnect $
--             deleteInterconnectResponse
--
--         , responseConfirmPrivateVirtualInterface $
--             confirmPrivateVirtualInterfaceResponse
--
--         , responseDescribeLocations $
--             describeLocationsResponse
--
--         , responseCreatePublicVirtualInterface $
--             virtualInterface
--
--         , responseAllocatePrivateVirtualInterface $
--             virtualInterface
--
--         , responseConfirmConnection $
--             confirmConnectionResponse
--
--         , responseConfirmPublicVirtualInterface $
--             confirmPublicVirtualInterfaceResponse
--
--         , responseDescribeVirtualGateways $
--             describeVirtualGatewaysResponse
--
--         , responseDescribeVirtualInterfaces $
--             describeVirtualInterfacesResponse
--
--         , responseDeleteVirtualInterface $
--             deleteVirtualInterfaceResponse
--
--         , responseDescribeInterconnectLoa $
--             describeInterconnectLoaResponse
--
--         , responseCreatePrivateVirtualInterface $
--             virtualInterface
--
--         , responseAllocatePublicVirtualInterface $
--             virtualInterface
--
--         , responseAllocateConnectionOnInterconnect $
--             connection
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseCreateInterconnect $
--             interconnect
--
--         , responseDescribeConnectionLoa $
--             describeConnectionLoaResponse
--
--           ]
--     ]

-- Requests

requestDescribeInterconnects :: DescribeInterconnects -> TestTree
requestDescribeInterconnects = req
    "DescribeInterconnects"
    "fixture/DescribeInterconnects.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection = req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection = req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestDescribeConnections :: DescribeConnections -> TestTree
requestDescribeConnections = req
    "DescribeConnections"
    "fixture/DescribeConnections.yaml"

requestDescribeConnectionsOnInterconnect :: DescribeConnectionsOnInterconnect -> TestTree
requestDescribeConnectionsOnInterconnect = req
    "DescribeConnectionsOnInterconnect"
    "fixture/DescribeConnectionsOnInterconnect.yaml"

requestDeleteInterconnect :: DeleteInterconnect -> TestTree
requestDeleteInterconnect = req
    "DeleteInterconnect"
    "fixture/DeleteInterconnect.yaml"

requestConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterface -> TestTree
requestConfirmPrivateVirtualInterface = req
    "ConfirmPrivateVirtualInterface"
    "fixture/ConfirmPrivateVirtualInterface.yaml"

requestDescribeLocations :: DescribeLocations -> TestTree
requestDescribeLocations = req
    "DescribeLocations"
    "fixture/DescribeLocations.yaml"

requestCreatePublicVirtualInterface :: CreatePublicVirtualInterface -> TestTree
requestCreatePublicVirtualInterface = req
    "CreatePublicVirtualInterface"
    "fixture/CreatePublicVirtualInterface.yaml"

requestAllocatePrivateVirtualInterface :: AllocatePrivateVirtualInterface -> TestTree
requestAllocatePrivateVirtualInterface = req
    "AllocatePrivateVirtualInterface"
    "fixture/AllocatePrivateVirtualInterface.yaml"

requestConfirmConnection :: ConfirmConnection -> TestTree
requestConfirmConnection = req
    "ConfirmConnection"
    "fixture/ConfirmConnection.yaml"

requestConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterface -> TestTree
requestConfirmPublicVirtualInterface = req
    "ConfirmPublicVirtualInterface"
    "fixture/ConfirmPublicVirtualInterface.yaml"

requestDescribeVirtualGateways :: DescribeVirtualGateways -> TestTree
requestDescribeVirtualGateways = req
    "DescribeVirtualGateways"
    "fixture/DescribeVirtualGateways.yaml"

requestDescribeVirtualInterfaces :: DescribeVirtualInterfaces -> TestTree
requestDescribeVirtualInterfaces = req
    "DescribeVirtualInterfaces"
    "fixture/DescribeVirtualInterfaces.yaml"

requestDeleteVirtualInterface :: DeleteVirtualInterface -> TestTree
requestDeleteVirtualInterface = req
    "DeleteVirtualInterface"
    "fixture/DeleteVirtualInterface.yaml"

requestDescribeInterconnectLoa :: DescribeInterconnectLoa -> TestTree
requestDescribeInterconnectLoa = req
    "DescribeInterconnectLoa"
    "fixture/DescribeInterconnectLoa.yaml"

requestCreatePrivateVirtualInterface :: CreatePrivateVirtualInterface -> TestTree
requestCreatePrivateVirtualInterface = req
    "CreatePrivateVirtualInterface"
    "fixture/CreatePrivateVirtualInterface.yaml"

requestAllocatePublicVirtualInterface :: AllocatePublicVirtualInterface -> TestTree
requestAllocatePublicVirtualInterface = req
    "AllocatePublicVirtualInterface"
    "fixture/AllocatePublicVirtualInterface.yaml"

requestAllocateConnectionOnInterconnect :: AllocateConnectionOnInterconnect -> TestTree
requestAllocateConnectionOnInterconnect = req
    "AllocateConnectionOnInterconnect"
    "fixture/AllocateConnectionOnInterconnect.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateInterconnect :: CreateInterconnect -> TestTree
requestCreateInterconnect = req
    "CreateInterconnect"
    "fixture/CreateInterconnect.yaml"

requestDescribeConnectionLoa :: DescribeConnectionLoa -> TestTree
requestDescribeConnectionLoa = req
    "DescribeConnectionLoa"
    "fixture/DescribeConnectionLoa.yaml"

-- Responses

responseDescribeInterconnects :: DescribeInterconnectsResponse -> TestTree
responseDescribeInterconnects = res
    "DescribeInterconnectsResponse"
    "fixture/DescribeInterconnectsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeInterconnects)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeTags)

responseDeleteConnection :: Connection -> TestTree
responseDeleteConnection = res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteConnection)

responseCreateConnection :: Connection -> TestTree
responseCreateConnection = res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy CreateConnection)

responseDescribeConnections :: Connections -> TestTree
responseDescribeConnections = res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeConnections)

responseDescribeConnectionsOnInterconnect :: Connections -> TestTree
responseDescribeConnectionsOnInterconnect = res
    "DescribeConnectionsOnInterconnectResponse"
    "fixture/DescribeConnectionsOnInterconnectResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeConnectionsOnInterconnect)

responseDeleteInterconnect :: DeleteInterconnectResponse -> TestTree
responseDeleteInterconnect = res
    "DeleteInterconnectResponse"
    "fixture/DeleteInterconnectResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteInterconnect)

responseConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterfaceResponse -> TestTree
responseConfirmPrivateVirtualInterface = res
    "ConfirmPrivateVirtualInterfaceResponse"
    "fixture/ConfirmPrivateVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy ConfirmPrivateVirtualInterface)

responseDescribeLocations :: DescribeLocationsResponse -> TestTree
responseDescribeLocations = res
    "DescribeLocationsResponse"
    "fixture/DescribeLocationsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeLocations)

responseCreatePublicVirtualInterface :: VirtualInterface -> TestTree
responseCreatePublicVirtualInterface = res
    "CreatePublicVirtualInterfaceResponse"
    "fixture/CreatePublicVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy CreatePublicVirtualInterface)

responseAllocatePrivateVirtualInterface :: VirtualInterface -> TestTree
responseAllocatePrivateVirtualInterface = res
    "AllocatePrivateVirtualInterfaceResponse"
    "fixture/AllocatePrivateVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy AllocatePrivateVirtualInterface)

responseConfirmConnection :: ConfirmConnectionResponse -> TestTree
responseConfirmConnection = res
    "ConfirmConnectionResponse"
    "fixture/ConfirmConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy ConfirmConnection)

responseConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterfaceResponse -> TestTree
responseConfirmPublicVirtualInterface = res
    "ConfirmPublicVirtualInterfaceResponse"
    "fixture/ConfirmPublicVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy ConfirmPublicVirtualInterface)

responseDescribeVirtualGateways :: DescribeVirtualGatewaysResponse -> TestTree
responseDescribeVirtualGateways = res
    "DescribeVirtualGatewaysResponse"
    "fixture/DescribeVirtualGatewaysResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeVirtualGateways)

responseDescribeVirtualInterfaces :: DescribeVirtualInterfacesResponse -> TestTree
responseDescribeVirtualInterfaces = res
    "DescribeVirtualInterfacesResponse"
    "fixture/DescribeVirtualInterfacesResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeVirtualInterfaces)

responseDeleteVirtualInterface :: DeleteVirtualInterfaceResponse -> TestTree
responseDeleteVirtualInterface = res
    "DeleteVirtualInterfaceResponse"
    "fixture/DeleteVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteVirtualInterface)

responseDescribeInterconnectLoa :: DescribeInterconnectLoaResponse -> TestTree
responseDescribeInterconnectLoa = res
    "DescribeInterconnectLoaResponse"
    "fixture/DescribeInterconnectLoaResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeInterconnectLoa)

responseCreatePrivateVirtualInterface :: VirtualInterface -> TestTree
responseCreatePrivateVirtualInterface = res
    "CreatePrivateVirtualInterfaceResponse"
    "fixture/CreatePrivateVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy CreatePrivateVirtualInterface)

responseAllocatePublicVirtualInterface :: VirtualInterface -> TestTree
responseAllocatePublicVirtualInterface = res
    "AllocatePublicVirtualInterfaceResponse"
    "fixture/AllocatePublicVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy AllocatePublicVirtualInterface)

responseAllocateConnectionOnInterconnect :: Connection -> TestTree
responseAllocateConnectionOnInterconnect = res
    "AllocateConnectionOnInterconnectResponse"
    "fixture/AllocateConnectionOnInterconnectResponse.proto"
    directConnect
    (Proxy :: Proxy AllocateConnectionOnInterconnect)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    directConnect
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    directConnect
    (Proxy :: Proxy UntagResource)

responseCreateInterconnect :: Interconnect -> TestTree
responseCreateInterconnect = res
    "CreateInterconnectResponse"
    "fixture/CreateInterconnectResponse.proto"
    directConnect
    (Proxy :: Proxy CreateInterconnect)

responseDescribeConnectionLoa :: DescribeConnectionLoaResponse -> TestTree
responseDescribeConnectionLoa = res
    "DescribeConnectionLoaResponse"
    "fixture/DescribeConnectionLoaResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeConnectionLoa)

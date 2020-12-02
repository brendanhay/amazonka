{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectConnect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DirectConnect where

import Data.Proxy
import Network.AWS.DirectConnect
import Test.AWS.DirectConnect.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeDirectConnectGatewayAssociations $
--             describeDirectConnectGatewayAssociations
--
--         , requestDescribeInterconnects $
--             describeInterconnects
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestDescribeLoa $
--             describeLoa
--
--         , requestDeleteConnection $
--             deleteConnection
--
--         , requestAssociateConnectionWithLag $
--             associateConnectionWithLag
--
--         , requestCreateConnection $
--             createConnection
--
--         , requestDescribeDirectConnectGateways $
--             describeDirectConnectGateways
--
--         , requestAssociateVirtualInterface $
--             associateVirtualInterface
--
--         , requestDescribeConnections $
--             describeConnections
--
--         , requestDeleteInterconnect $
--             deleteInterconnect
--
--         , requestConfirmPrivateVirtualInterface $
--             confirmPrivateVirtualInterface
--
--         , requestDeleteDirectConnectGatewayAssociation $
--             deleteDirectConnectGatewayAssociation
--
--         , requestDescribeLocations $
--             describeLocations
--
--         , requestCreateDirectConnectGatewayAssociation $
--             createDirectConnectGatewayAssociation
--
--         , requestCreatePublicVirtualInterface $
--             createPublicVirtualInterface
--
--         , requestAllocatePrivateVirtualInterface $
--             allocatePrivateVirtualInterface
--
--         , requestDescribeLags $
--             describeLags
--
--         , requestConfirmConnection $
--             confirmConnection
--
--         , requestDescribeDirectConnectGatewayAttachments $
--             describeDirectConnectGatewayAttachments
--
--         , requestConfirmPublicVirtualInterface $
--             confirmPublicVirtualInterface
--
--         , requestDescribeVirtualGateways $
--             describeVirtualGateways
--
--         , requestCreateDirectConnectGateway $
--             createDirectConnectGateway
--
--         , requestDeleteDirectConnectGateway $
--             deleteDirectConnectGateway
--
--         , requestDescribeVirtualInterfaces $
--             describeVirtualInterfaces
--
--         , requestAllocateHostedConnection $
--             allocateHostedConnection
--
--         , requestDeleteVirtualInterface $
--             deleteVirtualInterface
--
--         , requestCreatePrivateVirtualInterface $
--             createPrivateVirtualInterface
--
--         , requestAllocatePublicVirtualInterface $
--             allocatePublicVirtualInterface
--
--         , requestDisassociateConnectionFromLag $
--             disassociateConnectionFromLag
--
--         , requestTagResource $
--             tagResource
--
--         , requestDeleteLag $
--             deleteLag
--
--         , requestUpdateLag $
--             updateLag
--
--         , requestUntagResource $
--             untagResource
--
--         , requestCreateBGPPeer $
--             createBGPPeer
--
--         , requestAssociateHostedConnection $
--             associateHostedConnection
--
--         , requestCreateInterconnect $
--             createInterconnect
--
--         , requestDeleteBGPPeer $
--             deleteBGPPeer
--
--         , requestCreateLag $
--             createLag
--
--         , requestDescribeHostedConnections $
--             describeHostedConnections
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDirectConnectGatewayAssociations $
--             describeDirectConnectGatewayAssociationsResponse
--
--         , responseDescribeInterconnects $
--             describeInterconnectsResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseDescribeLoa $
--             describeLoaResponse
--
--         , responseDeleteConnection $
--             connection
--
--         , responseAssociateConnectionWithLag $
--             connection
--
--         , responseCreateConnection $
--             connection
--
--         , responseDescribeDirectConnectGateways $
--             describeDirectConnectGatewaysResponse
--
--         , responseAssociateVirtualInterface $
--             virtualInterface
--
--         , responseDescribeConnections $
--             connections
--
--         , responseDeleteInterconnect $
--             deleteInterconnectResponse
--
--         , responseConfirmPrivateVirtualInterface $
--             confirmPrivateVirtualInterfaceResponse
--
--         , responseDeleteDirectConnectGatewayAssociation $
--             deleteDirectConnectGatewayAssociationResponse
--
--         , responseDescribeLocations $
--             describeLocationsResponse
--
--         , responseCreateDirectConnectGatewayAssociation $
--             createDirectConnectGatewayAssociationResponse
--
--         , responseCreatePublicVirtualInterface $
--             virtualInterface
--
--         , responseAllocatePrivateVirtualInterface $
--             virtualInterface
--
--         , responseDescribeLags $
--             describeLagsResponse
--
--         , responseConfirmConnection $
--             confirmConnectionResponse
--
--         , responseDescribeDirectConnectGatewayAttachments $
--             describeDirectConnectGatewayAttachmentsResponse
--
--         , responseConfirmPublicVirtualInterface $
--             confirmPublicVirtualInterfaceResponse
--
--         , responseDescribeVirtualGateways $
--             describeVirtualGatewaysResponse
--
--         , responseCreateDirectConnectGateway $
--             createDirectConnectGatewayResponse
--
--         , responseDeleteDirectConnectGateway $
--             deleteDirectConnectGatewayResponse
--
--         , responseDescribeVirtualInterfaces $
--             describeVirtualInterfacesResponse
--
--         , responseAllocateHostedConnection $
--             connection
--
--         , responseDeleteVirtualInterface $
--             deleteVirtualInterfaceResponse
--
--         , responseCreatePrivateVirtualInterface $
--             virtualInterface
--
--         , responseAllocatePublicVirtualInterface $
--             virtualInterface
--
--         , responseDisassociateConnectionFromLag $
--             connection
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseDeleteLag $
--             lag
--
--         , responseUpdateLag $
--             lag
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseCreateBGPPeer $
--             createBGPPeerResponse
--
--         , responseAssociateHostedConnection $
--             connection
--
--         , responseCreateInterconnect $
--             interconnect
--
--         , responseDeleteBGPPeer $
--             deleteBGPPeerResponse
--
--         , responseCreateLag $
--             lag
--
--         , responseDescribeHostedConnections $
--             connections
--
--           ]
--     ]

-- Requests

requestDescribeDirectConnectGatewayAssociations :: DescribeDirectConnectGatewayAssociations -> TestTree
requestDescribeDirectConnectGatewayAssociations = req
    "DescribeDirectConnectGatewayAssociations"
    "fixture/DescribeDirectConnectGatewayAssociations.yaml"

requestDescribeInterconnects :: DescribeInterconnects -> TestTree
requestDescribeInterconnects = req
    "DescribeInterconnects"
    "fixture/DescribeInterconnects.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeLoa :: DescribeLoa -> TestTree
requestDescribeLoa = req
    "DescribeLoa"
    "fixture/DescribeLoa.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection = req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestAssociateConnectionWithLag :: AssociateConnectionWithLag -> TestTree
requestAssociateConnectionWithLag = req
    "AssociateConnectionWithLag"
    "fixture/AssociateConnectionWithLag.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection = req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestDescribeDirectConnectGateways :: DescribeDirectConnectGateways -> TestTree
requestDescribeDirectConnectGateways = req
    "DescribeDirectConnectGateways"
    "fixture/DescribeDirectConnectGateways.yaml"

requestAssociateVirtualInterface :: AssociateVirtualInterface -> TestTree
requestAssociateVirtualInterface = req
    "AssociateVirtualInterface"
    "fixture/AssociateVirtualInterface.yaml"

requestDescribeConnections :: DescribeConnections -> TestTree
requestDescribeConnections = req
    "DescribeConnections"
    "fixture/DescribeConnections.yaml"

requestDeleteInterconnect :: DeleteInterconnect -> TestTree
requestDeleteInterconnect = req
    "DeleteInterconnect"
    "fixture/DeleteInterconnect.yaml"

requestConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterface -> TestTree
requestConfirmPrivateVirtualInterface = req
    "ConfirmPrivateVirtualInterface"
    "fixture/ConfirmPrivateVirtualInterface.yaml"

requestDeleteDirectConnectGatewayAssociation :: DeleteDirectConnectGatewayAssociation -> TestTree
requestDeleteDirectConnectGatewayAssociation = req
    "DeleteDirectConnectGatewayAssociation"
    "fixture/DeleteDirectConnectGatewayAssociation.yaml"

requestDescribeLocations :: DescribeLocations -> TestTree
requestDescribeLocations = req
    "DescribeLocations"
    "fixture/DescribeLocations.yaml"

requestCreateDirectConnectGatewayAssociation :: CreateDirectConnectGatewayAssociation -> TestTree
requestCreateDirectConnectGatewayAssociation = req
    "CreateDirectConnectGatewayAssociation"
    "fixture/CreateDirectConnectGatewayAssociation.yaml"

requestCreatePublicVirtualInterface :: CreatePublicVirtualInterface -> TestTree
requestCreatePublicVirtualInterface = req
    "CreatePublicVirtualInterface"
    "fixture/CreatePublicVirtualInterface.yaml"

requestAllocatePrivateVirtualInterface :: AllocatePrivateVirtualInterface -> TestTree
requestAllocatePrivateVirtualInterface = req
    "AllocatePrivateVirtualInterface"
    "fixture/AllocatePrivateVirtualInterface.yaml"

requestDescribeLags :: DescribeLags -> TestTree
requestDescribeLags = req
    "DescribeLags"
    "fixture/DescribeLags.yaml"

requestConfirmConnection :: ConfirmConnection -> TestTree
requestConfirmConnection = req
    "ConfirmConnection"
    "fixture/ConfirmConnection.yaml"

requestDescribeDirectConnectGatewayAttachments :: DescribeDirectConnectGatewayAttachments -> TestTree
requestDescribeDirectConnectGatewayAttachments = req
    "DescribeDirectConnectGatewayAttachments"
    "fixture/DescribeDirectConnectGatewayAttachments.yaml"

requestConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterface -> TestTree
requestConfirmPublicVirtualInterface = req
    "ConfirmPublicVirtualInterface"
    "fixture/ConfirmPublicVirtualInterface.yaml"

requestDescribeVirtualGateways :: DescribeVirtualGateways -> TestTree
requestDescribeVirtualGateways = req
    "DescribeVirtualGateways"
    "fixture/DescribeVirtualGateways.yaml"

requestCreateDirectConnectGateway :: CreateDirectConnectGateway -> TestTree
requestCreateDirectConnectGateway = req
    "CreateDirectConnectGateway"
    "fixture/CreateDirectConnectGateway.yaml"

requestDeleteDirectConnectGateway :: DeleteDirectConnectGateway -> TestTree
requestDeleteDirectConnectGateway = req
    "DeleteDirectConnectGateway"
    "fixture/DeleteDirectConnectGateway.yaml"

requestDescribeVirtualInterfaces :: DescribeVirtualInterfaces -> TestTree
requestDescribeVirtualInterfaces = req
    "DescribeVirtualInterfaces"
    "fixture/DescribeVirtualInterfaces.yaml"

requestAllocateHostedConnection :: AllocateHostedConnection -> TestTree
requestAllocateHostedConnection = req
    "AllocateHostedConnection"
    "fixture/AllocateHostedConnection.yaml"

requestDeleteVirtualInterface :: DeleteVirtualInterface -> TestTree
requestDeleteVirtualInterface = req
    "DeleteVirtualInterface"
    "fixture/DeleteVirtualInterface.yaml"

requestCreatePrivateVirtualInterface :: CreatePrivateVirtualInterface -> TestTree
requestCreatePrivateVirtualInterface = req
    "CreatePrivateVirtualInterface"
    "fixture/CreatePrivateVirtualInterface.yaml"

requestAllocatePublicVirtualInterface :: AllocatePublicVirtualInterface -> TestTree
requestAllocatePublicVirtualInterface = req
    "AllocatePublicVirtualInterface"
    "fixture/AllocatePublicVirtualInterface.yaml"

requestDisassociateConnectionFromLag :: DisassociateConnectionFromLag -> TestTree
requestDisassociateConnectionFromLag = req
    "DisassociateConnectionFromLag"
    "fixture/DisassociateConnectionFromLag.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteLag :: DeleteLag -> TestTree
requestDeleteLag = req
    "DeleteLag"
    "fixture/DeleteLag.yaml"

requestUpdateLag :: UpdateLag -> TestTree
requestUpdateLag = req
    "UpdateLag"
    "fixture/UpdateLag.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateBGPPeer :: CreateBGPPeer -> TestTree
requestCreateBGPPeer = req
    "CreateBGPPeer"
    "fixture/CreateBGPPeer.yaml"

requestAssociateHostedConnection :: AssociateHostedConnection -> TestTree
requestAssociateHostedConnection = req
    "AssociateHostedConnection"
    "fixture/AssociateHostedConnection.yaml"

requestCreateInterconnect :: CreateInterconnect -> TestTree
requestCreateInterconnect = req
    "CreateInterconnect"
    "fixture/CreateInterconnect.yaml"

requestDeleteBGPPeer :: DeleteBGPPeer -> TestTree
requestDeleteBGPPeer = req
    "DeleteBGPPeer"
    "fixture/DeleteBGPPeer.yaml"

requestCreateLag :: CreateLag -> TestTree
requestCreateLag = req
    "CreateLag"
    "fixture/CreateLag.yaml"

requestDescribeHostedConnections :: DescribeHostedConnections -> TestTree
requestDescribeHostedConnections = req
    "DescribeHostedConnections"
    "fixture/DescribeHostedConnections.yaml"

-- Responses

responseDescribeDirectConnectGatewayAssociations :: DescribeDirectConnectGatewayAssociationsResponse -> TestTree
responseDescribeDirectConnectGatewayAssociations = res
    "DescribeDirectConnectGatewayAssociationsResponse"
    "fixture/DescribeDirectConnectGatewayAssociationsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeDirectConnectGatewayAssociations)

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

responseDescribeLoa :: DescribeLoaResponse -> TestTree
responseDescribeLoa = res
    "DescribeLoaResponse"
    "fixture/DescribeLoaResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeLoa)

responseDeleteConnection :: Connection -> TestTree
responseDeleteConnection = res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteConnection)

responseAssociateConnectionWithLag :: Connection -> TestTree
responseAssociateConnectionWithLag = res
    "AssociateConnectionWithLagResponse"
    "fixture/AssociateConnectionWithLagResponse.proto"
    directConnect
    (Proxy :: Proxy AssociateConnectionWithLag)

responseCreateConnection :: Connection -> TestTree
responseCreateConnection = res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy CreateConnection)

responseDescribeDirectConnectGateways :: DescribeDirectConnectGatewaysResponse -> TestTree
responseDescribeDirectConnectGateways = res
    "DescribeDirectConnectGatewaysResponse"
    "fixture/DescribeDirectConnectGatewaysResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeDirectConnectGateways)

responseAssociateVirtualInterface :: VirtualInterface -> TestTree
responseAssociateVirtualInterface = res
    "AssociateVirtualInterfaceResponse"
    "fixture/AssociateVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy AssociateVirtualInterface)

responseDescribeConnections :: Connections -> TestTree
responseDescribeConnections = res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeConnections)

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

responseDeleteDirectConnectGatewayAssociation :: DeleteDirectConnectGatewayAssociationResponse -> TestTree
responseDeleteDirectConnectGatewayAssociation = res
    "DeleteDirectConnectGatewayAssociationResponse"
    "fixture/DeleteDirectConnectGatewayAssociationResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteDirectConnectGatewayAssociation)

responseDescribeLocations :: DescribeLocationsResponse -> TestTree
responseDescribeLocations = res
    "DescribeLocationsResponse"
    "fixture/DescribeLocationsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeLocations)

responseCreateDirectConnectGatewayAssociation :: CreateDirectConnectGatewayAssociationResponse -> TestTree
responseCreateDirectConnectGatewayAssociation = res
    "CreateDirectConnectGatewayAssociationResponse"
    "fixture/CreateDirectConnectGatewayAssociationResponse.proto"
    directConnect
    (Proxy :: Proxy CreateDirectConnectGatewayAssociation)

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

responseDescribeLags :: DescribeLagsResponse -> TestTree
responseDescribeLags = res
    "DescribeLagsResponse"
    "fixture/DescribeLagsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeLags)

responseConfirmConnection :: ConfirmConnectionResponse -> TestTree
responseConfirmConnection = res
    "ConfirmConnectionResponse"
    "fixture/ConfirmConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy ConfirmConnection)

responseDescribeDirectConnectGatewayAttachments :: DescribeDirectConnectGatewayAttachmentsResponse -> TestTree
responseDescribeDirectConnectGatewayAttachments = res
    "DescribeDirectConnectGatewayAttachmentsResponse"
    "fixture/DescribeDirectConnectGatewayAttachmentsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeDirectConnectGatewayAttachments)

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

responseCreateDirectConnectGateway :: CreateDirectConnectGatewayResponse -> TestTree
responseCreateDirectConnectGateway = res
    "CreateDirectConnectGatewayResponse"
    "fixture/CreateDirectConnectGatewayResponse.proto"
    directConnect
    (Proxy :: Proxy CreateDirectConnectGateway)

responseDeleteDirectConnectGateway :: DeleteDirectConnectGatewayResponse -> TestTree
responseDeleteDirectConnectGateway = res
    "DeleteDirectConnectGatewayResponse"
    "fixture/DeleteDirectConnectGatewayResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteDirectConnectGateway)

responseDescribeVirtualInterfaces :: DescribeVirtualInterfacesResponse -> TestTree
responseDescribeVirtualInterfaces = res
    "DescribeVirtualInterfacesResponse"
    "fixture/DescribeVirtualInterfacesResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeVirtualInterfaces)

responseAllocateHostedConnection :: Connection -> TestTree
responseAllocateHostedConnection = res
    "AllocateHostedConnectionResponse"
    "fixture/AllocateHostedConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy AllocateHostedConnection)

responseDeleteVirtualInterface :: DeleteVirtualInterfaceResponse -> TestTree
responseDeleteVirtualInterface = res
    "DeleteVirtualInterfaceResponse"
    "fixture/DeleteVirtualInterfaceResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteVirtualInterface)

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

responseDisassociateConnectionFromLag :: Connection -> TestTree
responseDisassociateConnectionFromLag = res
    "DisassociateConnectionFromLagResponse"
    "fixture/DisassociateConnectionFromLagResponse.proto"
    directConnect
    (Proxy :: Proxy DisassociateConnectionFromLag)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    directConnect
    (Proxy :: Proxy TagResource)

responseDeleteLag :: Lag -> TestTree
responseDeleteLag = res
    "DeleteLagResponse"
    "fixture/DeleteLagResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteLag)

responseUpdateLag :: Lag -> TestTree
responseUpdateLag = res
    "UpdateLagResponse"
    "fixture/UpdateLagResponse.proto"
    directConnect
    (Proxy :: Proxy UpdateLag)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    directConnect
    (Proxy :: Proxy UntagResource)

responseCreateBGPPeer :: CreateBGPPeerResponse -> TestTree
responseCreateBGPPeer = res
    "CreateBGPPeerResponse"
    "fixture/CreateBGPPeerResponse.proto"
    directConnect
    (Proxy :: Proxy CreateBGPPeer)

responseAssociateHostedConnection :: Connection -> TestTree
responseAssociateHostedConnection = res
    "AssociateHostedConnectionResponse"
    "fixture/AssociateHostedConnectionResponse.proto"
    directConnect
    (Proxy :: Proxy AssociateHostedConnection)

responseCreateInterconnect :: Interconnect -> TestTree
responseCreateInterconnect = res
    "CreateInterconnectResponse"
    "fixture/CreateInterconnectResponse.proto"
    directConnect
    (Proxy :: Proxy CreateInterconnect)

responseDeleteBGPPeer :: DeleteBGPPeerResponse -> TestTree
responseDeleteBGPPeer = res
    "DeleteBGPPeerResponse"
    "fixture/DeleteBGPPeerResponse.proto"
    directConnect
    (Proxy :: Proxy DeleteBGPPeer)

responseCreateLag :: Lag -> TestTree
responseCreateLag = res
    "CreateLagResponse"
    "fixture/CreateLagResponse.proto"
    directConnect
    (Proxy :: Proxy CreateLag)

responseDescribeHostedConnections :: Connections -> TestTree
responseDescribeHostedConnections = res
    "DescribeHostedConnectionsResponse"
    "fixture/DescribeHostedConnectionsResponse.proto"
    directConnect
    (Proxy :: Proxy DescribeHostedConnections)

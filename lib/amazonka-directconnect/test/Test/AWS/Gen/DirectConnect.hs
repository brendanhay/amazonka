{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectConnect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkDescribeDirectConnectGatewayAssociations
--
--         , requestDescribeInterconnects $
--             mkDescribeInterconnects
--
--         , requestDescribeTags $
--             mkDescribeTags
--
--         , requestCreateTransitVirtualInterface $
--             mkCreateTransitVirtualInterface
--
--         , requestDescribeLoa $
--             mkDescribeLoa
--
--         , requestDeleteConnection $
--             mkDeleteConnection
--
--         , requestStartBGPFailoverTest $
--             mkStartBGPFailoverTest
--
--         , requestUpdateVirtualInterfaceAttributes $
--             mkUpdateVirtualInterfaceAttributes
--
--         , requestAssociateConnectionWithLag $
--             mkAssociateConnectionWithLag
--
--         , requestCreateDirectConnectGatewayAssociationProposal $
--             mkCreateDirectConnectGatewayAssociationProposal
--
--         , requestCreateConnection $
--             mkCreateConnection
--
--         , requestDescribeDirectConnectGateways $
--             mkDescribeDirectConnectGateways
--
--         , requestAssociateVirtualInterface $
--             mkAssociateVirtualInterface
--
--         , requestDescribeConnections $
--             mkDescribeConnections
--
--         , requestDeleteInterconnect $
--             mkDeleteInterconnect
--
--         , requestConfirmPrivateVirtualInterface $
--             mkConfirmPrivateVirtualInterface
--
--         , requestUpdateDirectConnectGatewayAssociation $
--             mkUpdateDirectConnectGatewayAssociation
--
--         , requestDeleteDirectConnectGatewayAssociation $
--             mkDeleteDirectConnectGatewayAssociation
--
--         , requestDescribeLocations $
--             mkDescribeLocations
--
--         , requestCreateDirectConnectGatewayAssociation $
--             mkCreateDirectConnectGatewayAssociation
--
--         , requestAcceptDirectConnectGatewayAssociationProposal $
--             mkAcceptDirectConnectGatewayAssociationProposal
--
--         , requestCreatePublicVirtualInterface $
--             mkCreatePublicVirtualInterface
--
--         , requestAllocatePrivateVirtualInterface $
--             mkAllocatePrivateVirtualInterface
--
--         , requestDescribeLags $
--             mkDescribeLags
--
--         , requestConfirmConnection $
--             mkConfirmConnection
--
--         , requestDescribeDirectConnectGatewayAttachments $
--             mkDescribeDirectConnectGatewayAttachments
--
--         , requestConfirmPublicVirtualInterface $
--             mkConfirmPublicVirtualInterface
--
--         , requestDescribeVirtualGateways $
--             mkDescribeVirtualGateways
--
--         , requestDeleteDirectConnectGatewayAssociationProposal $
--             mkDeleteDirectConnectGatewayAssociationProposal
--
--         , requestStopBGPFailoverTest $
--             mkStopBGPFailoverTest
--
--         , requestCreateDirectConnectGateway $
--             mkCreateDirectConnectGateway
--
--         , requestDeleteDirectConnectGateway $
--             mkDeleteDirectConnectGateway
--
--         , requestDescribeVirtualInterfaces $
--             mkDescribeVirtualInterfaces
--
--         , requestListVirtualInterfaceTestHistory $
--             mkListVirtualInterfaceTestHistory
--
--         , requestAllocateHostedConnection $
--             mkAllocateHostedConnection
--
--         , requestDeleteVirtualInterface $
--             mkDeleteVirtualInterface
--
--         , requestCreatePrivateVirtualInterface $
--             mkCreatePrivateVirtualInterface
--
--         , requestAllocatePublicVirtualInterface $
--             mkAllocatePublicVirtualInterface
--
--         , requestDescribeDirectConnectGatewayAssociationProposals $
--             mkDescribeDirectConnectGatewayAssociationProposals
--
--         , requestDisassociateConnectionFromLag $
--             mkDisassociateConnectionFromLag
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDeleteLag $
--             mkDeleteLag
--
--         , requestUpdateLag $
--             mkUpdateLag
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestCreateBGPPeer $
--             mkCreateBGPPeer
--
--         , requestAssociateHostedConnection $
--             mkAssociateHostedConnection
--
--         , requestCreateInterconnect $
--             mkCreateInterconnect
--
--         , requestDeleteBGPPeer $
--             mkDeleteBGPPeer
--
--         , requestAllocateTransitVirtualInterface $
--             mkAllocateTransitVirtualInterface
--
--         , requestCreateLag $
--             mkCreateLag
--
--         , requestConfirmTransitVirtualInterface $
--             mkConfirmTransitVirtualInterface
--
--         , requestDescribeHostedConnections $
--             mkDescribeHostedConnections
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDirectConnectGatewayAssociations $
--             mkDescribeDirectConnectGatewayAssociationsResponse
--
--         , responseDescribeInterconnects $
--             mkDescribeInterconnectsResponse
--
--         , responseDescribeTags $
--             mkDescribeTagsResponse
--
--         , responseCreateTransitVirtualInterface $
--             mkCreateTransitVirtualInterfaceResponse
--
--         , responseDescribeLoa $
--             mkDescribeLoaResponse
--
--         , responseDeleteConnection $
--             mkConnection
--
--         , responseStartBGPFailoverTest $
--             mkStartBGPFailoverTestResponse
--
--         , responseUpdateVirtualInterfaceAttributes $
--             mkVirtualInterface
--
--         , responseAssociateConnectionWithLag $
--             mkConnection
--
--         , responseCreateDirectConnectGatewayAssociationProposal $
--             mkCreateDirectConnectGatewayAssociationProposalResponse
--
--         , responseCreateConnection $
--             mkConnection
--
--         , responseDescribeDirectConnectGateways $
--             mkDescribeDirectConnectGatewaysResponse
--
--         , responseAssociateVirtualInterface $
--             mkVirtualInterface
--
--         , responseDescribeConnections $
--             mkConnections
--
--         , responseDeleteInterconnect $
--             mkDeleteInterconnectResponse
--
--         , responseConfirmPrivateVirtualInterface $
--             mkConfirmPrivateVirtualInterfaceResponse
--
--         , responseUpdateDirectConnectGatewayAssociation $
--             mkUpdateDirectConnectGatewayAssociationResponse
--
--         , responseDeleteDirectConnectGatewayAssociation $
--             mkDeleteDirectConnectGatewayAssociationResponse
--
--         , responseDescribeLocations $
--             mkDescribeLocationsResponse
--
--         , responseCreateDirectConnectGatewayAssociation $
--             mkCreateDirectConnectGatewayAssociationResponse
--
--         , responseAcceptDirectConnectGatewayAssociationProposal $
--             mkAcceptDirectConnectGatewayAssociationProposalResponse
--
--         , responseCreatePublicVirtualInterface $
--             mkVirtualInterface
--
--         , responseAllocatePrivateVirtualInterface $
--             mkVirtualInterface
--
--         , responseDescribeLags $
--             mkDescribeLagsResponse
--
--         , responseConfirmConnection $
--             mkConfirmConnectionResponse
--
--         , responseDescribeDirectConnectGatewayAttachments $
--             mkDescribeDirectConnectGatewayAttachmentsResponse
--
--         , responseConfirmPublicVirtualInterface $
--             mkConfirmPublicVirtualInterfaceResponse
--
--         , responseDescribeVirtualGateways $
--             mkDescribeVirtualGatewaysResponse
--
--         , responseDeleteDirectConnectGatewayAssociationProposal $
--             mkDeleteDirectConnectGatewayAssociationProposalResponse
--
--         , responseStopBGPFailoverTest $
--             mkStopBGPFailoverTestResponse
--
--         , responseCreateDirectConnectGateway $
--             mkCreateDirectConnectGatewayResponse
--
--         , responseDeleteDirectConnectGateway $
--             mkDeleteDirectConnectGatewayResponse
--
--         , responseDescribeVirtualInterfaces $
--             mkDescribeVirtualInterfacesResponse
--
--         , responseListVirtualInterfaceTestHistory $
--             mkListVirtualInterfaceTestHistoryResponse
--
--         , responseAllocateHostedConnection $
--             mkConnection
--
--         , responseDeleteVirtualInterface $
--             mkDeleteVirtualInterfaceResponse
--
--         , responseCreatePrivateVirtualInterface $
--             mkVirtualInterface
--
--         , responseAllocatePublicVirtualInterface $
--             mkVirtualInterface
--
--         , responseDescribeDirectConnectGatewayAssociationProposals $
--             mkDescribeDirectConnectGatewayAssociationProposalsResponse
--
--         , responseDisassociateConnectionFromLag $
--             mkConnection
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDeleteLag $
--             mkLag
--
--         , responseUpdateLag $
--             mkLag
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseCreateBGPPeer $
--             mkCreateBGPPeerResponse
--
--         , responseAssociateHostedConnection $
--             mkConnection
--
--         , responseCreateInterconnect $
--             mkInterconnect
--
--         , responseDeleteBGPPeer $
--             mkDeleteBGPPeerResponse
--
--         , responseAllocateTransitVirtualInterface $
--             mkAllocateTransitVirtualInterfaceResponse
--
--         , responseCreateLag $
--             mkLag
--
--         , responseConfirmTransitVirtualInterface $
--             mkConfirmTransitVirtualInterfaceResponse
--
--         , responseDescribeHostedConnections $
--             mkConnections
--
--           ]
--     ]

-- Requests

requestDescribeDirectConnectGatewayAssociations :: DescribeDirectConnectGatewayAssociations -> TestTree
requestDescribeDirectConnectGatewayAssociations =
  req
    "DescribeDirectConnectGatewayAssociations"
    "fixture/DescribeDirectConnectGatewayAssociations.yaml"

requestDescribeInterconnects :: DescribeInterconnects -> TestTree
requestDescribeInterconnects =
  req
    "DescribeInterconnects"
    "fixture/DescribeInterconnects.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestCreateTransitVirtualInterface :: CreateTransitVirtualInterface -> TestTree
requestCreateTransitVirtualInterface =
  req
    "CreateTransitVirtualInterface"
    "fixture/CreateTransitVirtualInterface.yaml"

requestDescribeLoa :: DescribeLoa -> TestTree
requestDescribeLoa =
  req
    "DescribeLoa"
    "fixture/DescribeLoa.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestStartBGPFailoverTest :: StartBGPFailoverTest -> TestTree
requestStartBGPFailoverTest =
  req
    "StartBGPFailoverTest"
    "fixture/StartBGPFailoverTest.yaml"

requestUpdateVirtualInterfaceAttributes :: UpdateVirtualInterfaceAttributes -> TestTree
requestUpdateVirtualInterfaceAttributes =
  req
    "UpdateVirtualInterfaceAttributes"
    "fixture/UpdateVirtualInterfaceAttributes.yaml"

requestAssociateConnectionWithLag :: AssociateConnectionWithLag -> TestTree
requestAssociateConnectionWithLag =
  req
    "AssociateConnectionWithLag"
    "fixture/AssociateConnectionWithLag.yaml"

requestCreateDirectConnectGatewayAssociationProposal :: CreateDirectConnectGatewayAssociationProposal -> TestTree
requestCreateDirectConnectGatewayAssociationProposal =
  req
    "CreateDirectConnectGatewayAssociationProposal"
    "fixture/CreateDirectConnectGatewayAssociationProposal.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestDescribeDirectConnectGateways :: DescribeDirectConnectGateways -> TestTree
requestDescribeDirectConnectGateways =
  req
    "DescribeDirectConnectGateways"
    "fixture/DescribeDirectConnectGateways.yaml"

requestAssociateVirtualInterface :: AssociateVirtualInterface -> TestTree
requestAssociateVirtualInterface =
  req
    "AssociateVirtualInterface"
    "fixture/AssociateVirtualInterface.yaml"

requestDescribeConnections :: DescribeConnections -> TestTree
requestDescribeConnections =
  req
    "DescribeConnections"
    "fixture/DescribeConnections.yaml"

requestDeleteInterconnect :: DeleteInterconnect -> TestTree
requestDeleteInterconnect =
  req
    "DeleteInterconnect"
    "fixture/DeleteInterconnect.yaml"

requestConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterface -> TestTree
requestConfirmPrivateVirtualInterface =
  req
    "ConfirmPrivateVirtualInterface"
    "fixture/ConfirmPrivateVirtualInterface.yaml"

requestUpdateDirectConnectGatewayAssociation :: UpdateDirectConnectGatewayAssociation -> TestTree
requestUpdateDirectConnectGatewayAssociation =
  req
    "UpdateDirectConnectGatewayAssociation"
    "fixture/UpdateDirectConnectGatewayAssociation.yaml"

requestDeleteDirectConnectGatewayAssociation :: DeleteDirectConnectGatewayAssociation -> TestTree
requestDeleteDirectConnectGatewayAssociation =
  req
    "DeleteDirectConnectGatewayAssociation"
    "fixture/DeleteDirectConnectGatewayAssociation.yaml"

requestDescribeLocations :: DescribeLocations -> TestTree
requestDescribeLocations =
  req
    "DescribeLocations"
    "fixture/DescribeLocations.yaml"

requestCreateDirectConnectGatewayAssociation :: CreateDirectConnectGatewayAssociation -> TestTree
requestCreateDirectConnectGatewayAssociation =
  req
    "CreateDirectConnectGatewayAssociation"
    "fixture/CreateDirectConnectGatewayAssociation.yaml"

requestAcceptDirectConnectGatewayAssociationProposal :: AcceptDirectConnectGatewayAssociationProposal -> TestTree
requestAcceptDirectConnectGatewayAssociationProposal =
  req
    "AcceptDirectConnectGatewayAssociationProposal"
    "fixture/AcceptDirectConnectGatewayAssociationProposal.yaml"

requestCreatePublicVirtualInterface :: CreatePublicVirtualInterface -> TestTree
requestCreatePublicVirtualInterface =
  req
    "CreatePublicVirtualInterface"
    "fixture/CreatePublicVirtualInterface.yaml"

requestAllocatePrivateVirtualInterface :: AllocatePrivateVirtualInterface -> TestTree
requestAllocatePrivateVirtualInterface =
  req
    "AllocatePrivateVirtualInterface"
    "fixture/AllocatePrivateVirtualInterface.yaml"

requestDescribeLags :: DescribeLags -> TestTree
requestDescribeLags =
  req
    "DescribeLags"
    "fixture/DescribeLags.yaml"

requestConfirmConnection :: ConfirmConnection -> TestTree
requestConfirmConnection =
  req
    "ConfirmConnection"
    "fixture/ConfirmConnection.yaml"

requestDescribeDirectConnectGatewayAttachments :: DescribeDirectConnectGatewayAttachments -> TestTree
requestDescribeDirectConnectGatewayAttachments =
  req
    "DescribeDirectConnectGatewayAttachments"
    "fixture/DescribeDirectConnectGatewayAttachments.yaml"

requestConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterface -> TestTree
requestConfirmPublicVirtualInterface =
  req
    "ConfirmPublicVirtualInterface"
    "fixture/ConfirmPublicVirtualInterface.yaml"

requestDescribeVirtualGateways :: DescribeVirtualGateways -> TestTree
requestDescribeVirtualGateways =
  req
    "DescribeVirtualGateways"
    "fixture/DescribeVirtualGateways.yaml"

requestDeleteDirectConnectGatewayAssociationProposal :: DeleteDirectConnectGatewayAssociationProposal -> TestTree
requestDeleteDirectConnectGatewayAssociationProposal =
  req
    "DeleteDirectConnectGatewayAssociationProposal"
    "fixture/DeleteDirectConnectGatewayAssociationProposal.yaml"

requestStopBGPFailoverTest :: StopBGPFailoverTest -> TestTree
requestStopBGPFailoverTest =
  req
    "StopBGPFailoverTest"
    "fixture/StopBGPFailoverTest.yaml"

requestCreateDirectConnectGateway :: CreateDirectConnectGateway -> TestTree
requestCreateDirectConnectGateway =
  req
    "CreateDirectConnectGateway"
    "fixture/CreateDirectConnectGateway.yaml"

requestDeleteDirectConnectGateway :: DeleteDirectConnectGateway -> TestTree
requestDeleteDirectConnectGateway =
  req
    "DeleteDirectConnectGateway"
    "fixture/DeleteDirectConnectGateway.yaml"

requestDescribeVirtualInterfaces :: DescribeVirtualInterfaces -> TestTree
requestDescribeVirtualInterfaces =
  req
    "DescribeVirtualInterfaces"
    "fixture/DescribeVirtualInterfaces.yaml"

requestListVirtualInterfaceTestHistory :: ListVirtualInterfaceTestHistory -> TestTree
requestListVirtualInterfaceTestHistory =
  req
    "ListVirtualInterfaceTestHistory"
    "fixture/ListVirtualInterfaceTestHistory.yaml"

requestAllocateHostedConnection :: AllocateHostedConnection -> TestTree
requestAllocateHostedConnection =
  req
    "AllocateHostedConnection"
    "fixture/AllocateHostedConnection.yaml"

requestDeleteVirtualInterface :: DeleteVirtualInterface -> TestTree
requestDeleteVirtualInterface =
  req
    "DeleteVirtualInterface"
    "fixture/DeleteVirtualInterface.yaml"

requestCreatePrivateVirtualInterface :: CreatePrivateVirtualInterface -> TestTree
requestCreatePrivateVirtualInterface =
  req
    "CreatePrivateVirtualInterface"
    "fixture/CreatePrivateVirtualInterface.yaml"

requestAllocatePublicVirtualInterface :: AllocatePublicVirtualInterface -> TestTree
requestAllocatePublicVirtualInterface =
  req
    "AllocatePublicVirtualInterface"
    "fixture/AllocatePublicVirtualInterface.yaml"

requestDescribeDirectConnectGatewayAssociationProposals :: DescribeDirectConnectGatewayAssociationProposals -> TestTree
requestDescribeDirectConnectGatewayAssociationProposals =
  req
    "DescribeDirectConnectGatewayAssociationProposals"
    "fixture/DescribeDirectConnectGatewayAssociationProposals.yaml"

requestDisassociateConnectionFromLag :: DisassociateConnectionFromLag -> TestTree
requestDisassociateConnectionFromLag =
  req
    "DisassociateConnectionFromLag"
    "fixture/DisassociateConnectionFromLag.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteLag :: DeleteLag -> TestTree
requestDeleteLag =
  req
    "DeleteLag"
    "fixture/DeleteLag.yaml"

requestUpdateLag :: UpdateLag -> TestTree
requestUpdateLag =
  req
    "UpdateLag"
    "fixture/UpdateLag.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateBGPPeer :: CreateBGPPeer -> TestTree
requestCreateBGPPeer =
  req
    "CreateBGPPeer"
    "fixture/CreateBGPPeer.yaml"

requestAssociateHostedConnection :: AssociateHostedConnection -> TestTree
requestAssociateHostedConnection =
  req
    "AssociateHostedConnection"
    "fixture/AssociateHostedConnection.yaml"

requestCreateInterconnect :: CreateInterconnect -> TestTree
requestCreateInterconnect =
  req
    "CreateInterconnect"
    "fixture/CreateInterconnect.yaml"

requestDeleteBGPPeer :: DeleteBGPPeer -> TestTree
requestDeleteBGPPeer =
  req
    "DeleteBGPPeer"
    "fixture/DeleteBGPPeer.yaml"

requestAllocateTransitVirtualInterface :: AllocateTransitVirtualInterface -> TestTree
requestAllocateTransitVirtualInterface =
  req
    "AllocateTransitVirtualInterface"
    "fixture/AllocateTransitVirtualInterface.yaml"

requestCreateLag :: CreateLag -> TestTree
requestCreateLag =
  req
    "CreateLag"
    "fixture/CreateLag.yaml"

requestConfirmTransitVirtualInterface :: ConfirmTransitVirtualInterface -> TestTree
requestConfirmTransitVirtualInterface =
  req
    "ConfirmTransitVirtualInterface"
    "fixture/ConfirmTransitVirtualInterface.yaml"

requestDescribeHostedConnections :: DescribeHostedConnections -> TestTree
requestDescribeHostedConnections =
  req
    "DescribeHostedConnections"
    "fixture/DescribeHostedConnections.yaml"

-- Responses

responseDescribeDirectConnectGatewayAssociations :: DescribeDirectConnectGatewayAssociationsResponse -> TestTree
responseDescribeDirectConnectGatewayAssociations =
  res
    "DescribeDirectConnectGatewayAssociationsResponse"
    "fixture/DescribeDirectConnectGatewayAssociationsResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeDirectConnectGatewayAssociations)

responseDescribeInterconnects :: DescribeInterconnectsResponse -> TestTree
responseDescribeInterconnects =
  res
    "DescribeInterconnectsResponse"
    "fixture/DescribeInterconnectsResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeInterconnects)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeTags)

responseCreateTransitVirtualInterface :: CreateTransitVirtualInterfaceResponse -> TestTree
responseCreateTransitVirtualInterface =
  res
    "CreateTransitVirtualInterfaceResponse"
    "fixture/CreateTransitVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy CreateTransitVirtualInterface)

responseDescribeLoa :: DescribeLoaResponse -> TestTree
responseDescribeLoa =
  res
    "DescribeLoaResponse"
    "fixture/DescribeLoaResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeLoa)

responseDeleteConnection :: Connection -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    directConnectService
    (Proxy :: Proxy DeleteConnection)

responseStartBGPFailoverTest :: StartBGPFailoverTestResponse -> TestTree
responseStartBGPFailoverTest =
  res
    "StartBGPFailoverTestResponse"
    "fixture/StartBGPFailoverTestResponse.proto"
    directConnectService
    (Proxy :: Proxy StartBGPFailoverTest)

responseUpdateVirtualInterfaceAttributes :: VirtualInterface -> TestTree
responseUpdateVirtualInterfaceAttributes =
  res
    "UpdateVirtualInterfaceAttributesResponse"
    "fixture/UpdateVirtualInterfaceAttributesResponse.proto"
    directConnectService
    (Proxy :: Proxy UpdateVirtualInterfaceAttributes)

responseAssociateConnectionWithLag :: Connection -> TestTree
responseAssociateConnectionWithLag =
  res
    "AssociateConnectionWithLagResponse"
    "fixture/AssociateConnectionWithLagResponse.proto"
    directConnectService
    (Proxy :: Proxy AssociateConnectionWithLag)

responseCreateDirectConnectGatewayAssociationProposal :: CreateDirectConnectGatewayAssociationProposalResponse -> TestTree
responseCreateDirectConnectGatewayAssociationProposal =
  res
    "CreateDirectConnectGatewayAssociationProposalResponse"
    "fixture/CreateDirectConnectGatewayAssociationProposalResponse.proto"
    directConnectService
    (Proxy :: Proxy CreateDirectConnectGatewayAssociationProposal)

responseCreateConnection :: Connection -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    directConnectService
    (Proxy :: Proxy CreateConnection)

responseDescribeDirectConnectGateways :: DescribeDirectConnectGatewaysResponse -> TestTree
responseDescribeDirectConnectGateways =
  res
    "DescribeDirectConnectGatewaysResponse"
    "fixture/DescribeDirectConnectGatewaysResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeDirectConnectGateways)

responseAssociateVirtualInterface :: VirtualInterface -> TestTree
responseAssociateVirtualInterface =
  res
    "AssociateVirtualInterfaceResponse"
    "fixture/AssociateVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy AssociateVirtualInterface)

responseDescribeConnections :: Connections -> TestTree
responseDescribeConnections =
  res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeConnections)

responseDeleteInterconnect :: DeleteInterconnectResponse -> TestTree
responseDeleteInterconnect =
  res
    "DeleteInterconnectResponse"
    "fixture/DeleteInterconnectResponse.proto"
    directConnectService
    (Proxy :: Proxy DeleteInterconnect)

responseConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterfaceResponse -> TestTree
responseConfirmPrivateVirtualInterface =
  res
    "ConfirmPrivateVirtualInterfaceResponse"
    "fixture/ConfirmPrivateVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy ConfirmPrivateVirtualInterface)

responseUpdateDirectConnectGatewayAssociation :: UpdateDirectConnectGatewayAssociationResponse -> TestTree
responseUpdateDirectConnectGatewayAssociation =
  res
    "UpdateDirectConnectGatewayAssociationResponse"
    "fixture/UpdateDirectConnectGatewayAssociationResponse.proto"
    directConnectService
    (Proxy :: Proxy UpdateDirectConnectGatewayAssociation)

responseDeleteDirectConnectGatewayAssociation :: DeleteDirectConnectGatewayAssociationResponse -> TestTree
responseDeleteDirectConnectGatewayAssociation =
  res
    "DeleteDirectConnectGatewayAssociationResponse"
    "fixture/DeleteDirectConnectGatewayAssociationResponse.proto"
    directConnectService
    (Proxy :: Proxy DeleteDirectConnectGatewayAssociation)

responseDescribeLocations :: DescribeLocationsResponse -> TestTree
responseDescribeLocations =
  res
    "DescribeLocationsResponse"
    "fixture/DescribeLocationsResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeLocations)

responseCreateDirectConnectGatewayAssociation :: CreateDirectConnectGatewayAssociationResponse -> TestTree
responseCreateDirectConnectGatewayAssociation =
  res
    "CreateDirectConnectGatewayAssociationResponse"
    "fixture/CreateDirectConnectGatewayAssociationResponse.proto"
    directConnectService
    (Proxy :: Proxy CreateDirectConnectGatewayAssociation)

responseAcceptDirectConnectGatewayAssociationProposal :: AcceptDirectConnectGatewayAssociationProposalResponse -> TestTree
responseAcceptDirectConnectGatewayAssociationProposal =
  res
    "AcceptDirectConnectGatewayAssociationProposalResponse"
    "fixture/AcceptDirectConnectGatewayAssociationProposalResponse.proto"
    directConnectService
    (Proxy :: Proxy AcceptDirectConnectGatewayAssociationProposal)

responseCreatePublicVirtualInterface :: VirtualInterface -> TestTree
responseCreatePublicVirtualInterface =
  res
    "CreatePublicVirtualInterfaceResponse"
    "fixture/CreatePublicVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy CreatePublicVirtualInterface)

responseAllocatePrivateVirtualInterface :: VirtualInterface -> TestTree
responseAllocatePrivateVirtualInterface =
  res
    "AllocatePrivateVirtualInterfaceResponse"
    "fixture/AllocatePrivateVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy AllocatePrivateVirtualInterface)

responseDescribeLags :: DescribeLagsResponse -> TestTree
responseDescribeLags =
  res
    "DescribeLagsResponse"
    "fixture/DescribeLagsResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeLags)

responseConfirmConnection :: ConfirmConnectionResponse -> TestTree
responseConfirmConnection =
  res
    "ConfirmConnectionResponse"
    "fixture/ConfirmConnectionResponse.proto"
    directConnectService
    (Proxy :: Proxy ConfirmConnection)

responseDescribeDirectConnectGatewayAttachments :: DescribeDirectConnectGatewayAttachmentsResponse -> TestTree
responseDescribeDirectConnectGatewayAttachments =
  res
    "DescribeDirectConnectGatewayAttachmentsResponse"
    "fixture/DescribeDirectConnectGatewayAttachmentsResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeDirectConnectGatewayAttachments)

responseConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterfaceResponse -> TestTree
responseConfirmPublicVirtualInterface =
  res
    "ConfirmPublicVirtualInterfaceResponse"
    "fixture/ConfirmPublicVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy ConfirmPublicVirtualInterface)

responseDescribeVirtualGateways :: DescribeVirtualGatewaysResponse -> TestTree
responseDescribeVirtualGateways =
  res
    "DescribeVirtualGatewaysResponse"
    "fixture/DescribeVirtualGatewaysResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeVirtualGateways)

responseDeleteDirectConnectGatewayAssociationProposal :: DeleteDirectConnectGatewayAssociationProposalResponse -> TestTree
responseDeleteDirectConnectGatewayAssociationProposal =
  res
    "DeleteDirectConnectGatewayAssociationProposalResponse"
    "fixture/DeleteDirectConnectGatewayAssociationProposalResponse.proto"
    directConnectService
    (Proxy :: Proxy DeleteDirectConnectGatewayAssociationProposal)

responseStopBGPFailoverTest :: StopBGPFailoverTestResponse -> TestTree
responseStopBGPFailoverTest =
  res
    "StopBGPFailoverTestResponse"
    "fixture/StopBGPFailoverTestResponse.proto"
    directConnectService
    (Proxy :: Proxy StopBGPFailoverTest)

responseCreateDirectConnectGateway :: CreateDirectConnectGatewayResponse -> TestTree
responseCreateDirectConnectGateway =
  res
    "CreateDirectConnectGatewayResponse"
    "fixture/CreateDirectConnectGatewayResponse.proto"
    directConnectService
    (Proxy :: Proxy CreateDirectConnectGateway)

responseDeleteDirectConnectGateway :: DeleteDirectConnectGatewayResponse -> TestTree
responseDeleteDirectConnectGateway =
  res
    "DeleteDirectConnectGatewayResponse"
    "fixture/DeleteDirectConnectGatewayResponse.proto"
    directConnectService
    (Proxy :: Proxy DeleteDirectConnectGateway)

responseDescribeVirtualInterfaces :: DescribeVirtualInterfacesResponse -> TestTree
responseDescribeVirtualInterfaces =
  res
    "DescribeVirtualInterfacesResponse"
    "fixture/DescribeVirtualInterfacesResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeVirtualInterfaces)

responseListVirtualInterfaceTestHistory :: ListVirtualInterfaceTestHistoryResponse -> TestTree
responseListVirtualInterfaceTestHistory =
  res
    "ListVirtualInterfaceTestHistoryResponse"
    "fixture/ListVirtualInterfaceTestHistoryResponse.proto"
    directConnectService
    (Proxy :: Proxy ListVirtualInterfaceTestHistory)

responseAllocateHostedConnection :: Connection -> TestTree
responseAllocateHostedConnection =
  res
    "AllocateHostedConnectionResponse"
    "fixture/AllocateHostedConnectionResponse.proto"
    directConnectService
    (Proxy :: Proxy AllocateHostedConnection)

responseDeleteVirtualInterface :: DeleteVirtualInterfaceResponse -> TestTree
responseDeleteVirtualInterface =
  res
    "DeleteVirtualInterfaceResponse"
    "fixture/DeleteVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy DeleteVirtualInterface)

responseCreatePrivateVirtualInterface :: VirtualInterface -> TestTree
responseCreatePrivateVirtualInterface =
  res
    "CreatePrivateVirtualInterfaceResponse"
    "fixture/CreatePrivateVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy CreatePrivateVirtualInterface)

responseAllocatePublicVirtualInterface :: VirtualInterface -> TestTree
responseAllocatePublicVirtualInterface =
  res
    "AllocatePublicVirtualInterfaceResponse"
    "fixture/AllocatePublicVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy AllocatePublicVirtualInterface)

responseDescribeDirectConnectGatewayAssociationProposals :: DescribeDirectConnectGatewayAssociationProposalsResponse -> TestTree
responseDescribeDirectConnectGatewayAssociationProposals =
  res
    "DescribeDirectConnectGatewayAssociationProposalsResponse"
    "fixture/DescribeDirectConnectGatewayAssociationProposalsResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeDirectConnectGatewayAssociationProposals)

responseDisassociateConnectionFromLag :: Connection -> TestTree
responseDisassociateConnectionFromLag =
  res
    "DisassociateConnectionFromLagResponse"
    "fixture/DisassociateConnectionFromLagResponse.proto"
    directConnectService
    (Proxy :: Proxy DisassociateConnectionFromLag)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    directConnectService
    (Proxy :: Proxy TagResource)

responseDeleteLag :: Lag -> TestTree
responseDeleteLag =
  res
    "DeleteLagResponse"
    "fixture/DeleteLagResponse.proto"
    directConnectService
    (Proxy :: Proxy DeleteLag)

responseUpdateLag :: Lag -> TestTree
responseUpdateLag =
  res
    "UpdateLagResponse"
    "fixture/UpdateLagResponse.proto"
    directConnectService
    (Proxy :: Proxy UpdateLag)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    directConnectService
    (Proxy :: Proxy UntagResource)

responseCreateBGPPeer :: CreateBGPPeerResponse -> TestTree
responseCreateBGPPeer =
  res
    "CreateBGPPeerResponse"
    "fixture/CreateBGPPeerResponse.proto"
    directConnectService
    (Proxy :: Proxy CreateBGPPeer)

responseAssociateHostedConnection :: Connection -> TestTree
responseAssociateHostedConnection =
  res
    "AssociateHostedConnectionResponse"
    "fixture/AssociateHostedConnectionResponse.proto"
    directConnectService
    (Proxy :: Proxy AssociateHostedConnection)

responseCreateInterconnect :: Interconnect -> TestTree
responseCreateInterconnect =
  res
    "CreateInterconnectResponse"
    "fixture/CreateInterconnectResponse.proto"
    directConnectService
    (Proxy :: Proxy CreateInterconnect)

responseDeleteBGPPeer :: DeleteBGPPeerResponse -> TestTree
responseDeleteBGPPeer =
  res
    "DeleteBGPPeerResponse"
    "fixture/DeleteBGPPeerResponse.proto"
    directConnectService
    (Proxy :: Proxy DeleteBGPPeer)

responseAllocateTransitVirtualInterface :: AllocateTransitVirtualInterfaceResponse -> TestTree
responseAllocateTransitVirtualInterface =
  res
    "AllocateTransitVirtualInterfaceResponse"
    "fixture/AllocateTransitVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy AllocateTransitVirtualInterface)

responseCreateLag :: Lag -> TestTree
responseCreateLag =
  res
    "CreateLagResponse"
    "fixture/CreateLagResponse.proto"
    directConnectService
    (Proxy :: Proxy CreateLag)

responseConfirmTransitVirtualInterface :: ConfirmTransitVirtualInterfaceResponse -> TestTree
responseConfirmTransitVirtualInterface =
  res
    "ConfirmTransitVirtualInterfaceResponse"
    "fixture/ConfirmTransitVirtualInterfaceResponse.proto"
    directConnectService
    (Proxy :: Proxy ConfirmTransitVirtualInterface)

responseDescribeHostedConnections :: Connections -> TestTree
responseDescribeHostedConnections =
  res
    "DescribeHostedConnectionsResponse"
    "fixture/DescribeHostedConnectionsResponse.proto"
    directConnectService
    (Proxy :: Proxy DescribeHostedConnections)

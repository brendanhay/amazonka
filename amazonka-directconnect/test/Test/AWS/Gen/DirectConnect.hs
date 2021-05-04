{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DirectConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestConfirmPublicVirtualInterface $
--             newConfirmPublicVirtualInterface
--
--         , requestStartBgpFailoverTest $
--             newStartBgpFailoverTest
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDeleteDirectConnectGatewayAssociationProposal $
--             newDeleteDirectConnectGatewayAssociationProposal
--
--         , requestStopBgpFailoverTest $
--             newStopBgpFailoverTest
--
--         , requestDescribeVirtualGateways $
--             newDescribeVirtualGateways
--
--         , requestAllocatePrivateVirtualInterface $
--             newAllocatePrivateVirtualInterface
--
--         , requestDescribeDirectConnectGatewayAssociations $
--             newDescribeDirectConnectGatewayAssociations
--
--         , requestConfirmConnection $
--             newConfirmConnection
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDescribeDirectConnectGatewayAttachments $
--             newDescribeDirectConnectGatewayAttachments
--
--         , requestDescribeHostedConnections $
--             newDescribeHostedConnections
--
--         , requestCreatePublicVirtualInterface $
--             newCreatePublicVirtualInterface
--
--         , requestAcceptDirectConnectGatewayAssociationProposal $
--             newAcceptDirectConnectGatewayAssociationProposal
--
--         , requestCreateDirectConnectGatewayAssociation $
--             newCreateDirectConnectGatewayAssociation
--
--         , requestCreateInterconnect $
--             newCreateInterconnect
--
--         , requestConfirmPrivateVirtualInterface $
--             newConfirmPrivateVirtualInterface
--
--         , requestUpdateLag $
--             newUpdateLag
--
--         , requestDeleteInterconnect $
--             newDeleteInterconnect
--
--         , requestDeleteLag $
--             newDeleteLag
--
--         , requestAssociateHostedConnection $
--             newAssociateHostedConnection
--
--         , requestCreateBGPPeer $
--             newCreateBGPPeer
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDisassociateConnectionFromLag $
--             newDisassociateConnectionFromLag
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteVirtualInterface $
--             newDeleteVirtualInterface
--
--         , requestDescribeDirectConnectGateways $
--             newDescribeDirectConnectGateways
--
--         , requestUpdateVirtualInterfaceAttributes $
--             newUpdateVirtualInterfaceAttributes
--
--         , requestAssociateConnectionWithLag $
--             newAssociateConnectionWithLag
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestDescribeVirtualInterfaces $
--             newDescribeVirtualInterfaces
--
--         , requestListVirtualInterfaceTestHistory $
--             newListVirtualInterfaceTestHistory
--
--         , requestCreateTransitVirtualInterface $
--             newCreateTransitVirtualInterface
--
--         , requestDescribeLoa $
--             newDescribeLoa
--
--         , requestCreateDirectConnectGateway $
--             newCreateDirectConnectGateway
--
--         , requestDescribeInterconnects $
--             newDescribeInterconnects
--
--         , requestDescribeLags $
--             newDescribeLags
--
--         , requestConfirmTransitVirtualInterface $
--             newConfirmTransitVirtualInterface
--
--         , requestCreateLag $
--             newCreateLag
--
--         , requestDeleteBGPPeer $
--             newDeleteBGPPeer
--
--         , requestAllocateTransitVirtualInterface $
--             newAllocateTransitVirtualInterface
--
--         , requestDescribeLocations $
--             newDescribeLocations
--
--         , requestDeleteDirectConnectGatewayAssociation $
--             newDeleteDirectConnectGatewayAssociation
--
--         , requestUpdateDirectConnectGatewayAssociation $
--             newUpdateDirectConnectGatewayAssociation
--
--         , requestAllocatePublicVirtualInterface $
--             newAllocatePublicVirtualInterface
--
--         , requestDescribeConnections $
--             newDescribeConnections
--
--         , requestAssociateVirtualInterface $
--             newAssociateVirtualInterface
--
--         , requestDescribeDirectConnectGatewayAssociationProposals $
--             newDescribeDirectConnectGatewayAssociationProposals
--
--         , requestCreatePrivateVirtualInterface $
--             newCreatePrivateVirtualInterface
--
--         , requestAllocateHostedConnection $
--             newAllocateHostedConnection
--
--         , requestCreateDirectConnectGatewayAssociationProposal $
--             newCreateDirectConnectGatewayAssociationProposal
--
--         , requestDeleteDirectConnectGateway $
--             newDeleteDirectConnectGateway
--
--           ]

--     , testGroup "response"
--         [ responseConfirmPublicVirtualInterface $
--             newConfirmPublicVirtualInterfaceResponse
--
--         , responseStartBgpFailoverTest $
--             newStartBgpFailoverTestResponse
--
--         , responseDeleteConnection $
--             newConnection
--
--         , responseDeleteDirectConnectGatewayAssociationProposal $
--             newDeleteDirectConnectGatewayAssociationProposalResponse
--
--         , responseStopBgpFailoverTest $
--             newStopBgpFailoverTestResponse
--
--         , responseDescribeVirtualGateways $
--             newDescribeVirtualGatewaysResponse
--
--         , responseAllocatePrivateVirtualInterface $
--             newVirtualInterface
--
--         , responseDescribeDirectConnectGatewayAssociations $
--             newDescribeDirectConnectGatewayAssociationsResponse
--
--         , responseConfirmConnection $
--             newConfirmConnectionResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDescribeDirectConnectGatewayAttachments $
--             newDescribeDirectConnectGatewayAttachmentsResponse
--
--         , responseDescribeHostedConnections $
--             newConnections
--
--         , responseCreatePublicVirtualInterface $
--             newVirtualInterface
--
--         , responseAcceptDirectConnectGatewayAssociationProposal $
--             newAcceptDirectConnectGatewayAssociationProposalResponse
--
--         , responseCreateDirectConnectGatewayAssociation $
--             newCreateDirectConnectGatewayAssociationResponse
--
--         , responseCreateInterconnect $
--             newInterconnect
--
--         , responseConfirmPrivateVirtualInterface $
--             newConfirmPrivateVirtualInterfaceResponse
--
--         , responseUpdateLag $
--             newLag
--
--         , responseDeleteInterconnect $
--             newDeleteInterconnectResponse
--
--         , responseDeleteLag $
--             newLag
--
--         , responseAssociateHostedConnection $
--             newConnection
--
--         , responseCreateBGPPeer $
--             newCreateBGPPeerResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDisassociateConnectionFromLag $
--             newConnection
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteVirtualInterface $
--             newDeleteVirtualInterfaceResponse
--
--         , responseDescribeDirectConnectGateways $
--             newDescribeDirectConnectGatewaysResponse
--
--         , responseUpdateVirtualInterfaceAttributes $
--             newVirtualInterface
--
--         , responseAssociateConnectionWithLag $
--             newConnection
--
--         , responseCreateConnection $
--             newConnection
--
--         , responseDescribeVirtualInterfaces $
--             newDescribeVirtualInterfacesResponse
--
--         , responseListVirtualInterfaceTestHistory $
--             newListVirtualInterfaceTestHistoryResponse
--
--         , responseCreateTransitVirtualInterface $
--             newCreateTransitVirtualInterfaceResponse
--
--         , responseDescribeLoa $
--             newDescribeLoaResponse
--
--         , responseCreateDirectConnectGateway $
--             newCreateDirectConnectGatewayResponse
--
--         , responseDescribeInterconnects $
--             newDescribeInterconnectsResponse
--
--         , responseDescribeLags $
--             newDescribeLagsResponse
--
--         , responseConfirmTransitVirtualInterface $
--             newConfirmTransitVirtualInterfaceResponse
--
--         , responseCreateLag $
--             newLag
--
--         , responseDeleteBGPPeer $
--             newDeleteBGPPeerResponse
--
--         , responseAllocateTransitVirtualInterface $
--             newAllocateTransitVirtualInterfaceResponse
--
--         , responseDescribeLocations $
--             newDescribeLocationsResponse
--
--         , responseDeleteDirectConnectGatewayAssociation $
--             newDeleteDirectConnectGatewayAssociationResponse
--
--         , responseUpdateDirectConnectGatewayAssociation $
--             newUpdateDirectConnectGatewayAssociationResponse
--
--         , responseAllocatePublicVirtualInterface $
--             newVirtualInterface
--
--         , responseDescribeConnections $
--             newConnections
--
--         , responseAssociateVirtualInterface $
--             newVirtualInterface
--
--         , responseDescribeDirectConnectGatewayAssociationProposals $
--             newDescribeDirectConnectGatewayAssociationProposalsResponse
--
--         , responseCreatePrivateVirtualInterface $
--             newVirtualInterface
--
--         , responseAllocateHostedConnection $
--             newConnection
--
--         , responseCreateDirectConnectGatewayAssociationProposal $
--             newCreateDirectConnectGatewayAssociationProposalResponse
--
--         , responseDeleteDirectConnectGateway $
--             newDeleteDirectConnectGatewayResponse
--
--           ]
--     ]

-- Requests

requestConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterface -> TestTree
requestConfirmPublicVirtualInterface =
  req
    "ConfirmPublicVirtualInterface"
    "fixture/ConfirmPublicVirtualInterface.yaml"

requestStartBgpFailoverTest :: StartBgpFailoverTest -> TestTree
requestStartBgpFailoverTest =
  req
    "StartBgpFailoverTest"
    "fixture/StartBgpFailoverTest.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestDeleteDirectConnectGatewayAssociationProposal :: DeleteDirectConnectGatewayAssociationProposal -> TestTree
requestDeleteDirectConnectGatewayAssociationProposal =
  req
    "DeleteDirectConnectGatewayAssociationProposal"
    "fixture/DeleteDirectConnectGatewayAssociationProposal.yaml"

requestStopBgpFailoverTest :: StopBgpFailoverTest -> TestTree
requestStopBgpFailoverTest =
  req
    "StopBgpFailoverTest"
    "fixture/StopBgpFailoverTest.yaml"

requestDescribeVirtualGateways :: DescribeVirtualGateways -> TestTree
requestDescribeVirtualGateways =
  req
    "DescribeVirtualGateways"
    "fixture/DescribeVirtualGateways.yaml"

requestAllocatePrivateVirtualInterface :: AllocatePrivateVirtualInterface -> TestTree
requestAllocatePrivateVirtualInterface =
  req
    "AllocatePrivateVirtualInterface"
    "fixture/AllocatePrivateVirtualInterface.yaml"

requestDescribeDirectConnectGatewayAssociations :: DescribeDirectConnectGatewayAssociations -> TestTree
requestDescribeDirectConnectGatewayAssociations =
  req
    "DescribeDirectConnectGatewayAssociations"
    "fixture/DescribeDirectConnectGatewayAssociations.yaml"

requestConfirmConnection :: ConfirmConnection -> TestTree
requestConfirmConnection =
  req
    "ConfirmConnection"
    "fixture/ConfirmConnection.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeDirectConnectGatewayAttachments :: DescribeDirectConnectGatewayAttachments -> TestTree
requestDescribeDirectConnectGatewayAttachments =
  req
    "DescribeDirectConnectGatewayAttachments"
    "fixture/DescribeDirectConnectGatewayAttachments.yaml"

requestDescribeHostedConnections :: DescribeHostedConnections -> TestTree
requestDescribeHostedConnections =
  req
    "DescribeHostedConnections"
    "fixture/DescribeHostedConnections.yaml"

requestCreatePublicVirtualInterface :: CreatePublicVirtualInterface -> TestTree
requestCreatePublicVirtualInterface =
  req
    "CreatePublicVirtualInterface"
    "fixture/CreatePublicVirtualInterface.yaml"

requestAcceptDirectConnectGatewayAssociationProposal :: AcceptDirectConnectGatewayAssociationProposal -> TestTree
requestAcceptDirectConnectGatewayAssociationProposal =
  req
    "AcceptDirectConnectGatewayAssociationProposal"
    "fixture/AcceptDirectConnectGatewayAssociationProposal.yaml"

requestCreateDirectConnectGatewayAssociation :: CreateDirectConnectGatewayAssociation -> TestTree
requestCreateDirectConnectGatewayAssociation =
  req
    "CreateDirectConnectGatewayAssociation"
    "fixture/CreateDirectConnectGatewayAssociation.yaml"

requestCreateInterconnect :: CreateInterconnect -> TestTree
requestCreateInterconnect =
  req
    "CreateInterconnect"
    "fixture/CreateInterconnect.yaml"

requestConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterface -> TestTree
requestConfirmPrivateVirtualInterface =
  req
    "ConfirmPrivateVirtualInterface"
    "fixture/ConfirmPrivateVirtualInterface.yaml"

requestUpdateLag :: UpdateLag -> TestTree
requestUpdateLag =
  req
    "UpdateLag"
    "fixture/UpdateLag.yaml"

requestDeleteInterconnect :: DeleteInterconnect -> TestTree
requestDeleteInterconnect =
  req
    "DeleteInterconnect"
    "fixture/DeleteInterconnect.yaml"

requestDeleteLag :: DeleteLag -> TestTree
requestDeleteLag =
  req
    "DeleteLag"
    "fixture/DeleteLag.yaml"

requestAssociateHostedConnection :: AssociateHostedConnection -> TestTree
requestAssociateHostedConnection =
  req
    "AssociateHostedConnection"
    "fixture/AssociateHostedConnection.yaml"

requestCreateBGPPeer :: CreateBGPPeer -> TestTree
requestCreateBGPPeer =
  req
    "CreateBGPPeer"
    "fixture/CreateBGPPeer.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestDeleteVirtualInterface :: DeleteVirtualInterface -> TestTree
requestDeleteVirtualInterface =
  req
    "DeleteVirtualInterface"
    "fixture/DeleteVirtualInterface.yaml"

requestDescribeDirectConnectGateways :: DescribeDirectConnectGateways -> TestTree
requestDescribeDirectConnectGateways =
  req
    "DescribeDirectConnectGateways"
    "fixture/DescribeDirectConnectGateways.yaml"

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

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

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

requestCreateDirectConnectGateway :: CreateDirectConnectGateway -> TestTree
requestCreateDirectConnectGateway =
  req
    "CreateDirectConnectGateway"
    "fixture/CreateDirectConnectGateway.yaml"

requestDescribeInterconnects :: DescribeInterconnects -> TestTree
requestDescribeInterconnects =
  req
    "DescribeInterconnects"
    "fixture/DescribeInterconnects.yaml"

requestDescribeLags :: DescribeLags -> TestTree
requestDescribeLags =
  req
    "DescribeLags"
    "fixture/DescribeLags.yaml"

requestConfirmTransitVirtualInterface :: ConfirmTransitVirtualInterface -> TestTree
requestConfirmTransitVirtualInterface =
  req
    "ConfirmTransitVirtualInterface"
    "fixture/ConfirmTransitVirtualInterface.yaml"

requestCreateLag :: CreateLag -> TestTree
requestCreateLag =
  req
    "CreateLag"
    "fixture/CreateLag.yaml"

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

requestDescribeLocations :: DescribeLocations -> TestTree
requestDescribeLocations =
  req
    "DescribeLocations"
    "fixture/DescribeLocations.yaml"

requestDeleteDirectConnectGatewayAssociation :: DeleteDirectConnectGatewayAssociation -> TestTree
requestDeleteDirectConnectGatewayAssociation =
  req
    "DeleteDirectConnectGatewayAssociation"
    "fixture/DeleteDirectConnectGatewayAssociation.yaml"

requestUpdateDirectConnectGatewayAssociation :: UpdateDirectConnectGatewayAssociation -> TestTree
requestUpdateDirectConnectGatewayAssociation =
  req
    "UpdateDirectConnectGatewayAssociation"
    "fixture/UpdateDirectConnectGatewayAssociation.yaml"

requestAllocatePublicVirtualInterface :: AllocatePublicVirtualInterface -> TestTree
requestAllocatePublicVirtualInterface =
  req
    "AllocatePublicVirtualInterface"
    "fixture/AllocatePublicVirtualInterface.yaml"

requestDescribeConnections :: DescribeConnections -> TestTree
requestDescribeConnections =
  req
    "DescribeConnections"
    "fixture/DescribeConnections.yaml"

requestAssociateVirtualInterface :: AssociateVirtualInterface -> TestTree
requestAssociateVirtualInterface =
  req
    "AssociateVirtualInterface"
    "fixture/AssociateVirtualInterface.yaml"

requestDescribeDirectConnectGatewayAssociationProposals :: DescribeDirectConnectGatewayAssociationProposals -> TestTree
requestDescribeDirectConnectGatewayAssociationProposals =
  req
    "DescribeDirectConnectGatewayAssociationProposals"
    "fixture/DescribeDirectConnectGatewayAssociationProposals.yaml"

requestCreatePrivateVirtualInterface :: CreatePrivateVirtualInterface -> TestTree
requestCreatePrivateVirtualInterface =
  req
    "CreatePrivateVirtualInterface"
    "fixture/CreatePrivateVirtualInterface.yaml"

requestAllocateHostedConnection :: AllocateHostedConnection -> TestTree
requestAllocateHostedConnection =
  req
    "AllocateHostedConnection"
    "fixture/AllocateHostedConnection.yaml"

requestCreateDirectConnectGatewayAssociationProposal :: CreateDirectConnectGatewayAssociationProposal -> TestTree
requestCreateDirectConnectGatewayAssociationProposal =
  req
    "CreateDirectConnectGatewayAssociationProposal"
    "fixture/CreateDirectConnectGatewayAssociationProposal.yaml"

requestDeleteDirectConnectGateway :: DeleteDirectConnectGateway -> TestTree
requestDeleteDirectConnectGateway =
  req
    "DeleteDirectConnectGateway"
    "fixture/DeleteDirectConnectGateway.yaml"

-- Responses

responseConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterfaceResponse -> TestTree
responseConfirmPublicVirtualInterface =
  res
    "ConfirmPublicVirtualInterfaceResponse"
    "fixture/ConfirmPublicVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmPublicVirtualInterface)

responseStartBgpFailoverTest :: StartBgpFailoverTestResponse -> TestTree
responseStartBgpFailoverTest =
  res
    "StartBgpFailoverTestResponse"
    "fixture/StartBgpFailoverTestResponse.proto"
    defaultService
    (Proxy :: Proxy StartBgpFailoverTest)

responseDeleteConnection :: Connection -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnection)

responseDeleteDirectConnectGatewayAssociationProposal :: DeleteDirectConnectGatewayAssociationProposalResponse -> TestTree
responseDeleteDirectConnectGatewayAssociationProposal =
  res
    "DeleteDirectConnectGatewayAssociationProposalResponse"
    "fixture/DeleteDirectConnectGatewayAssociationProposalResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDirectConnectGatewayAssociationProposal)

responseStopBgpFailoverTest :: StopBgpFailoverTestResponse -> TestTree
responseStopBgpFailoverTest =
  res
    "StopBgpFailoverTestResponse"
    "fixture/StopBgpFailoverTestResponse.proto"
    defaultService
    (Proxy :: Proxy StopBgpFailoverTest)

responseDescribeVirtualGateways :: DescribeVirtualGatewaysResponse -> TestTree
responseDescribeVirtualGateways =
  res
    "DescribeVirtualGatewaysResponse"
    "fixture/DescribeVirtualGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVirtualGateways)

responseAllocatePrivateVirtualInterface :: VirtualInterface -> TestTree
responseAllocatePrivateVirtualInterface =
  res
    "AllocatePrivateVirtualInterfaceResponse"
    "fixture/AllocatePrivateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy AllocatePrivateVirtualInterface)

responseDescribeDirectConnectGatewayAssociations :: DescribeDirectConnectGatewayAssociationsResponse -> TestTree
responseDescribeDirectConnectGatewayAssociations =
  res
    "DescribeDirectConnectGatewayAssociationsResponse"
    "fixture/DescribeDirectConnectGatewayAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDirectConnectGatewayAssociations)

responseConfirmConnection :: ConfirmConnectionResponse -> TestTree
responseConfirmConnection =
  res
    "ConfirmConnectionResponse"
    "fixture/ConfirmConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmConnection)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseDescribeDirectConnectGatewayAttachments :: DescribeDirectConnectGatewayAttachmentsResponse -> TestTree
responseDescribeDirectConnectGatewayAttachments =
  res
    "DescribeDirectConnectGatewayAttachmentsResponse"
    "fixture/DescribeDirectConnectGatewayAttachmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDirectConnectGatewayAttachments)

responseDescribeHostedConnections :: Connections -> TestTree
responseDescribeHostedConnections =
  res
    "DescribeHostedConnectionsResponse"
    "fixture/DescribeHostedConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHostedConnections)

responseCreatePublicVirtualInterface :: VirtualInterface -> TestTree
responseCreatePublicVirtualInterface =
  res
    "CreatePublicVirtualInterfaceResponse"
    "fixture/CreatePublicVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePublicVirtualInterface)

responseAcceptDirectConnectGatewayAssociationProposal :: AcceptDirectConnectGatewayAssociationProposalResponse -> TestTree
responseAcceptDirectConnectGatewayAssociationProposal =
  res
    "AcceptDirectConnectGatewayAssociationProposalResponse"
    "fixture/AcceptDirectConnectGatewayAssociationProposalResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptDirectConnectGatewayAssociationProposal)

responseCreateDirectConnectGatewayAssociation :: CreateDirectConnectGatewayAssociationResponse -> TestTree
responseCreateDirectConnectGatewayAssociation =
  res
    "CreateDirectConnectGatewayAssociationResponse"
    "fixture/CreateDirectConnectGatewayAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDirectConnectGatewayAssociation)

responseCreateInterconnect :: Interconnect -> TestTree
responseCreateInterconnect =
  res
    "CreateInterconnectResponse"
    "fixture/CreateInterconnectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInterconnect)

responseConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterfaceResponse -> TestTree
responseConfirmPrivateVirtualInterface =
  res
    "ConfirmPrivateVirtualInterfaceResponse"
    "fixture/ConfirmPrivateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmPrivateVirtualInterface)

responseUpdateLag :: Lag -> TestTree
responseUpdateLag =
  res
    "UpdateLagResponse"
    "fixture/UpdateLagResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLag)

responseDeleteInterconnect :: DeleteInterconnectResponse -> TestTree
responseDeleteInterconnect =
  res
    "DeleteInterconnectResponse"
    "fixture/DeleteInterconnectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInterconnect)

responseDeleteLag :: Lag -> TestTree
responseDeleteLag =
  res
    "DeleteLagResponse"
    "fixture/DeleteLagResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLag)

responseAssociateHostedConnection :: Connection -> TestTree
responseAssociateHostedConnection =
  res
    "AssociateHostedConnectionResponse"
    "fixture/AssociateHostedConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateHostedConnection)

responseCreateBGPPeer :: CreateBGPPeerResponse -> TestTree
responseCreateBGPPeer =
  res
    "CreateBGPPeerResponse"
    "fixture/CreateBGPPeerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBGPPeer)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDisassociateConnectionFromLag :: Connection -> TestTree
responseDisassociateConnectionFromLag =
  res
    "DisassociateConnectionFromLagResponse"
    "fixture/DisassociateConnectionFromLagResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateConnectionFromLag)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDeleteVirtualInterface :: DeleteVirtualInterfaceResponse -> TestTree
responseDeleteVirtualInterface =
  res
    "DeleteVirtualInterfaceResponse"
    "fixture/DeleteVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVirtualInterface)

responseDescribeDirectConnectGateways :: DescribeDirectConnectGatewaysResponse -> TestTree
responseDescribeDirectConnectGateways =
  res
    "DescribeDirectConnectGatewaysResponse"
    "fixture/DescribeDirectConnectGatewaysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDirectConnectGateways)

responseUpdateVirtualInterfaceAttributes :: VirtualInterface -> TestTree
responseUpdateVirtualInterfaceAttributes =
  res
    "UpdateVirtualInterfaceAttributesResponse"
    "fixture/UpdateVirtualInterfaceAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVirtualInterfaceAttributes)

responseAssociateConnectionWithLag :: Connection -> TestTree
responseAssociateConnectionWithLag =
  res
    "AssociateConnectionWithLagResponse"
    "fixture/AssociateConnectionWithLagResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateConnectionWithLag)

responseCreateConnection :: Connection -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnection)

responseDescribeVirtualInterfaces :: DescribeVirtualInterfacesResponse -> TestTree
responseDescribeVirtualInterfaces =
  res
    "DescribeVirtualInterfacesResponse"
    "fixture/DescribeVirtualInterfacesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVirtualInterfaces)

responseListVirtualInterfaceTestHistory :: ListVirtualInterfaceTestHistoryResponse -> TestTree
responseListVirtualInterfaceTestHistory =
  res
    "ListVirtualInterfaceTestHistoryResponse"
    "fixture/ListVirtualInterfaceTestHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy ListVirtualInterfaceTestHistory)

responseCreateTransitVirtualInterface :: CreateTransitVirtualInterfaceResponse -> TestTree
responseCreateTransitVirtualInterface =
  res
    "CreateTransitVirtualInterfaceResponse"
    "fixture/CreateTransitVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransitVirtualInterface)

responseDescribeLoa :: DescribeLoaResponse -> TestTree
responseDescribeLoa =
  res
    "DescribeLoaResponse"
    "fixture/DescribeLoaResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoa)

responseCreateDirectConnectGateway :: CreateDirectConnectGatewayResponse -> TestTree
responseCreateDirectConnectGateway =
  res
    "CreateDirectConnectGatewayResponse"
    "fixture/CreateDirectConnectGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDirectConnectGateway)

responseDescribeInterconnects :: DescribeInterconnectsResponse -> TestTree
responseDescribeInterconnects =
  res
    "DescribeInterconnectsResponse"
    "fixture/DescribeInterconnectsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInterconnects)

responseDescribeLags :: DescribeLagsResponse -> TestTree
responseDescribeLags =
  res
    "DescribeLagsResponse"
    "fixture/DescribeLagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLags)

responseConfirmTransitVirtualInterface :: ConfirmTransitVirtualInterfaceResponse -> TestTree
responseConfirmTransitVirtualInterface =
  res
    "ConfirmTransitVirtualInterfaceResponse"
    "fixture/ConfirmTransitVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmTransitVirtualInterface)

responseCreateLag :: Lag -> TestTree
responseCreateLag =
  res
    "CreateLagResponse"
    "fixture/CreateLagResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLag)

responseDeleteBGPPeer :: DeleteBGPPeerResponse -> TestTree
responseDeleteBGPPeer =
  res
    "DeleteBGPPeerResponse"
    "fixture/DeleteBGPPeerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBGPPeer)

responseAllocateTransitVirtualInterface :: AllocateTransitVirtualInterfaceResponse -> TestTree
responseAllocateTransitVirtualInterface =
  res
    "AllocateTransitVirtualInterfaceResponse"
    "fixture/AllocateTransitVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy AllocateTransitVirtualInterface)

responseDescribeLocations :: DescribeLocationsResponse -> TestTree
responseDescribeLocations =
  res
    "DescribeLocationsResponse"
    "fixture/DescribeLocationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLocations)

responseDeleteDirectConnectGatewayAssociation :: DeleteDirectConnectGatewayAssociationResponse -> TestTree
responseDeleteDirectConnectGatewayAssociation =
  res
    "DeleteDirectConnectGatewayAssociationResponse"
    "fixture/DeleteDirectConnectGatewayAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDirectConnectGatewayAssociation)

responseUpdateDirectConnectGatewayAssociation :: UpdateDirectConnectGatewayAssociationResponse -> TestTree
responseUpdateDirectConnectGatewayAssociation =
  res
    "UpdateDirectConnectGatewayAssociationResponse"
    "fixture/UpdateDirectConnectGatewayAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDirectConnectGatewayAssociation)

responseAllocatePublicVirtualInterface :: VirtualInterface -> TestTree
responseAllocatePublicVirtualInterface =
  res
    "AllocatePublicVirtualInterfaceResponse"
    "fixture/AllocatePublicVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy AllocatePublicVirtualInterface)

responseDescribeConnections :: Connections -> TestTree
responseDescribeConnections =
  res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConnections)

responseAssociateVirtualInterface :: VirtualInterface -> TestTree
responseAssociateVirtualInterface =
  res
    "AssociateVirtualInterfaceResponse"
    "fixture/AssociateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateVirtualInterface)

responseDescribeDirectConnectGatewayAssociationProposals :: DescribeDirectConnectGatewayAssociationProposalsResponse -> TestTree
responseDescribeDirectConnectGatewayAssociationProposals =
  res
    "DescribeDirectConnectGatewayAssociationProposalsResponse"
    "fixture/DescribeDirectConnectGatewayAssociationProposalsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDirectConnectGatewayAssociationProposals)

responseCreatePrivateVirtualInterface :: VirtualInterface -> TestTree
responseCreatePrivateVirtualInterface =
  res
    "CreatePrivateVirtualInterfaceResponse"
    "fixture/CreatePrivateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePrivateVirtualInterface)

responseAllocateHostedConnection :: Connection -> TestTree
responseAllocateHostedConnection =
  res
    "AllocateHostedConnectionResponse"
    "fixture/AllocateHostedConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy AllocateHostedConnection)

responseCreateDirectConnectGatewayAssociationProposal :: CreateDirectConnectGatewayAssociationProposalResponse -> TestTree
responseCreateDirectConnectGatewayAssociationProposal =
  res
    "CreateDirectConnectGatewayAssociationProposalResponse"
    "fixture/CreateDirectConnectGatewayAssociationProposalResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDirectConnectGatewayAssociationProposal)

responseDeleteDirectConnectGateway :: DeleteDirectConnectGatewayResponse -> TestTree
responseDeleteDirectConnectGateway =
  res
    "DeleteDirectConnectGatewayResponse"
    "fixture/DeleteDirectConnectGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDirectConnectGateway)

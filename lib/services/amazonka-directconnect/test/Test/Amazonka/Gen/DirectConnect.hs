{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DirectConnect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DirectConnect where

import Amazonka.DirectConnect
import qualified Data.Proxy as Proxy
import Test.Amazonka.DirectConnect.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptDirectConnectGatewayAssociationProposal $
--             newAcceptDirectConnectGatewayAssociationProposal
--
--         , requestAllocateHostedConnection $
--             newAllocateHostedConnection
--
--         , requestAllocatePrivateVirtualInterface $
--             newAllocatePrivateVirtualInterface
--
--         , requestAllocatePublicVirtualInterface $
--             newAllocatePublicVirtualInterface
--
--         , requestAllocateTransitVirtualInterface $
--             newAllocateTransitVirtualInterface
--
--         , requestAssociateConnectionWithLag $
--             newAssociateConnectionWithLag
--
--         , requestAssociateHostedConnection $
--             newAssociateHostedConnection
--
--         , requestAssociateMacSecKey $
--             newAssociateMacSecKey
--
--         , requestAssociateVirtualInterface $
--             newAssociateVirtualInterface
--
--         , requestConfirmConnection $
--             newConfirmConnection
--
--         , requestConfirmCustomerAgreement $
--             newConfirmCustomerAgreement
--
--         , requestConfirmPrivateVirtualInterface $
--             newConfirmPrivateVirtualInterface
--
--         , requestConfirmPublicVirtualInterface $
--             newConfirmPublicVirtualInterface
--
--         , requestConfirmTransitVirtualInterface $
--             newConfirmTransitVirtualInterface
--
--         , requestCreateBGPPeer $
--             newCreateBGPPeer
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestCreateDirectConnectGateway $
--             newCreateDirectConnectGateway
--
--         , requestCreateDirectConnectGatewayAssociation $
--             newCreateDirectConnectGatewayAssociation
--
--         , requestCreateDirectConnectGatewayAssociationProposal $
--             newCreateDirectConnectGatewayAssociationProposal
--
--         , requestCreateInterconnect $
--             newCreateInterconnect
--
--         , requestCreateLag $
--             newCreateLag
--
--         , requestCreatePrivateVirtualInterface $
--             newCreatePrivateVirtualInterface
--
--         , requestCreatePublicVirtualInterface $
--             newCreatePublicVirtualInterface
--
--         , requestCreateTransitVirtualInterface $
--             newCreateTransitVirtualInterface
--
--         , requestDeleteBGPPeer $
--             newDeleteBGPPeer
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDeleteDirectConnectGateway $
--             newDeleteDirectConnectGateway
--
--         , requestDeleteDirectConnectGatewayAssociation $
--             newDeleteDirectConnectGatewayAssociation
--
--         , requestDeleteDirectConnectGatewayAssociationProposal $
--             newDeleteDirectConnectGatewayAssociationProposal
--
--         , requestDeleteInterconnect $
--             newDeleteInterconnect
--
--         , requestDeleteLag $
--             newDeleteLag
--
--         , requestDeleteVirtualInterface $
--             newDeleteVirtualInterface
--
--         , requestDescribeConnections $
--             newDescribeConnections
--
--         , requestDescribeCustomerMetadata $
--             newDescribeCustomerMetadata
--
--         , requestDescribeDirectConnectGatewayAssociationProposals $
--             newDescribeDirectConnectGatewayAssociationProposals
--
--         , requestDescribeDirectConnectGatewayAssociations $
--             newDescribeDirectConnectGatewayAssociations
--
--         , requestDescribeDirectConnectGatewayAttachments $
--             newDescribeDirectConnectGatewayAttachments
--
--         , requestDescribeDirectConnectGateways $
--             newDescribeDirectConnectGateways
--
--         , requestDescribeHostedConnections $
--             newDescribeHostedConnections
--
--         , requestDescribeInterconnects $
--             newDescribeInterconnects
--
--         , requestDescribeLags $
--             newDescribeLags
--
--         , requestDescribeLoa $
--             newDescribeLoa
--
--         , requestDescribeLocations $
--             newDescribeLocations
--
--         , requestDescribeRouterConfiguration $
--             newDescribeRouterConfiguration
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDescribeVirtualGateways $
--             newDescribeVirtualGateways
--
--         , requestDescribeVirtualInterfaces $
--             newDescribeVirtualInterfaces
--
--         , requestDisassociateConnectionFromLag $
--             newDisassociateConnectionFromLag
--
--         , requestDisassociateMacSecKey $
--             newDisassociateMacSecKey
--
--         , requestListVirtualInterfaceTestHistory $
--             newListVirtualInterfaceTestHistory
--
--         , requestStartBgpFailoverTest $
--             newStartBgpFailoverTest
--
--         , requestStopBgpFailoverTest $
--             newStopBgpFailoverTest
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestUpdateDirectConnectGateway $
--             newUpdateDirectConnectGateway
--
--         , requestUpdateDirectConnectGatewayAssociation $
--             newUpdateDirectConnectGatewayAssociation
--
--         , requestUpdateLag $
--             newUpdateLag
--
--         , requestUpdateVirtualInterfaceAttributes $
--             newUpdateVirtualInterfaceAttributes
--
--           ]

--     , testGroup "response"
--         [ responseAcceptDirectConnectGatewayAssociationProposal $
--             newAcceptDirectConnectGatewayAssociationProposalResponse
--
--         , responseAllocateHostedConnection $
--             newConnection
--
--         , responseAllocatePrivateVirtualInterface $
--             newVirtualInterface
--
--         , responseAllocatePublicVirtualInterface $
--             newVirtualInterface
--
--         , responseAllocateTransitVirtualInterface $
--             newAllocateTransitVirtualInterfaceResponse
--
--         , responseAssociateConnectionWithLag $
--             newConnection
--
--         , responseAssociateHostedConnection $
--             newConnection
--
--         , responseAssociateMacSecKey $
--             newAssociateMacSecKeyResponse
--
--         , responseAssociateVirtualInterface $
--             newVirtualInterface
--
--         , responseConfirmConnection $
--             newConfirmConnectionResponse
--
--         , responseConfirmCustomerAgreement $
--             newConfirmCustomerAgreementResponse
--
--         , responseConfirmPrivateVirtualInterface $
--             newConfirmPrivateVirtualInterfaceResponse
--
--         , responseConfirmPublicVirtualInterface $
--             newConfirmPublicVirtualInterfaceResponse
--
--         , responseConfirmTransitVirtualInterface $
--             newConfirmTransitVirtualInterfaceResponse
--
--         , responseCreateBGPPeer $
--             newCreateBGPPeerResponse
--
--         , responseCreateConnection $
--             newConnection
--
--         , responseCreateDirectConnectGateway $
--             newCreateDirectConnectGatewayResponse
--
--         , responseCreateDirectConnectGatewayAssociation $
--             newCreateDirectConnectGatewayAssociationResponse
--
--         , responseCreateDirectConnectGatewayAssociationProposal $
--             newCreateDirectConnectGatewayAssociationProposalResponse
--
--         , responseCreateInterconnect $
--             newInterconnect
--
--         , responseCreateLag $
--             newLag
--
--         , responseCreatePrivateVirtualInterface $
--             newVirtualInterface
--
--         , responseCreatePublicVirtualInterface $
--             newVirtualInterface
--
--         , responseCreateTransitVirtualInterface $
--             newCreateTransitVirtualInterfaceResponse
--
--         , responseDeleteBGPPeer $
--             newDeleteBGPPeerResponse
--
--         , responseDeleteConnection $
--             newConnection
--
--         , responseDeleteDirectConnectGateway $
--             newDeleteDirectConnectGatewayResponse
--
--         , responseDeleteDirectConnectGatewayAssociation $
--             newDeleteDirectConnectGatewayAssociationResponse
--
--         , responseDeleteDirectConnectGatewayAssociationProposal $
--             newDeleteDirectConnectGatewayAssociationProposalResponse
--
--         , responseDeleteInterconnect $
--             newDeleteInterconnectResponse
--
--         , responseDeleteLag $
--             newLag
--
--         , responseDeleteVirtualInterface $
--             newDeleteVirtualInterfaceResponse
--
--         , responseDescribeConnections $
--             newConnections
--
--         , responseDescribeCustomerMetadata $
--             newDescribeCustomerMetadataResponse
--
--         , responseDescribeDirectConnectGatewayAssociationProposals $
--             newDescribeDirectConnectGatewayAssociationProposalsResponse
--
--         , responseDescribeDirectConnectGatewayAssociations $
--             newDescribeDirectConnectGatewayAssociationsResponse
--
--         , responseDescribeDirectConnectGatewayAttachments $
--             newDescribeDirectConnectGatewayAttachmentsResponse
--
--         , responseDescribeDirectConnectGateways $
--             newDescribeDirectConnectGatewaysResponse
--
--         , responseDescribeHostedConnections $
--             newConnections
--
--         , responseDescribeInterconnects $
--             newDescribeInterconnectsResponse
--
--         , responseDescribeLags $
--             newDescribeLagsResponse
--
--         , responseDescribeLoa $
--             newDescribeLoaResponse
--
--         , responseDescribeLocations $
--             newDescribeLocationsResponse
--
--         , responseDescribeRouterConfiguration $
--             newDescribeRouterConfigurationResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDescribeVirtualGateways $
--             newDescribeVirtualGatewaysResponse
--
--         , responseDescribeVirtualInterfaces $
--             newDescribeVirtualInterfacesResponse
--
--         , responseDisassociateConnectionFromLag $
--             newConnection
--
--         , responseDisassociateMacSecKey $
--             newDisassociateMacSecKeyResponse
--
--         , responseListVirtualInterfaceTestHistory $
--             newListVirtualInterfaceTestHistoryResponse
--
--         , responseStartBgpFailoverTest $
--             newStartBgpFailoverTestResponse
--
--         , responseStopBgpFailoverTest $
--             newStopBgpFailoverTestResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateConnection $
--             newConnection
--
--         , responseUpdateDirectConnectGateway $
--             newUpdateDirectConnectGatewayResponse
--
--         , responseUpdateDirectConnectGatewayAssociation $
--             newUpdateDirectConnectGatewayAssociationResponse
--
--         , responseUpdateLag $
--             newLag
--
--         , responseUpdateVirtualInterfaceAttributes $
--             newVirtualInterface
--
--           ]
--     ]

-- Requests

requestAcceptDirectConnectGatewayAssociationProposal :: AcceptDirectConnectGatewayAssociationProposal -> TestTree
requestAcceptDirectConnectGatewayAssociationProposal =
  req
    "AcceptDirectConnectGatewayAssociationProposal"
    "fixture/AcceptDirectConnectGatewayAssociationProposal.yaml"

requestAllocateHostedConnection :: AllocateHostedConnection -> TestTree
requestAllocateHostedConnection =
  req
    "AllocateHostedConnection"
    "fixture/AllocateHostedConnection.yaml"

requestAllocatePrivateVirtualInterface :: AllocatePrivateVirtualInterface -> TestTree
requestAllocatePrivateVirtualInterface =
  req
    "AllocatePrivateVirtualInterface"
    "fixture/AllocatePrivateVirtualInterface.yaml"

requestAllocatePublicVirtualInterface :: AllocatePublicVirtualInterface -> TestTree
requestAllocatePublicVirtualInterface =
  req
    "AllocatePublicVirtualInterface"
    "fixture/AllocatePublicVirtualInterface.yaml"

requestAllocateTransitVirtualInterface :: AllocateTransitVirtualInterface -> TestTree
requestAllocateTransitVirtualInterface =
  req
    "AllocateTransitVirtualInterface"
    "fixture/AllocateTransitVirtualInterface.yaml"

requestAssociateConnectionWithLag :: AssociateConnectionWithLag -> TestTree
requestAssociateConnectionWithLag =
  req
    "AssociateConnectionWithLag"
    "fixture/AssociateConnectionWithLag.yaml"

requestAssociateHostedConnection :: AssociateHostedConnection -> TestTree
requestAssociateHostedConnection =
  req
    "AssociateHostedConnection"
    "fixture/AssociateHostedConnection.yaml"

requestAssociateMacSecKey :: AssociateMacSecKey -> TestTree
requestAssociateMacSecKey =
  req
    "AssociateMacSecKey"
    "fixture/AssociateMacSecKey.yaml"

requestAssociateVirtualInterface :: AssociateVirtualInterface -> TestTree
requestAssociateVirtualInterface =
  req
    "AssociateVirtualInterface"
    "fixture/AssociateVirtualInterface.yaml"

requestConfirmConnection :: ConfirmConnection -> TestTree
requestConfirmConnection =
  req
    "ConfirmConnection"
    "fixture/ConfirmConnection.yaml"

requestConfirmCustomerAgreement :: ConfirmCustomerAgreement -> TestTree
requestConfirmCustomerAgreement =
  req
    "ConfirmCustomerAgreement"
    "fixture/ConfirmCustomerAgreement.yaml"

requestConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterface -> TestTree
requestConfirmPrivateVirtualInterface =
  req
    "ConfirmPrivateVirtualInterface"
    "fixture/ConfirmPrivateVirtualInterface.yaml"

requestConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterface -> TestTree
requestConfirmPublicVirtualInterface =
  req
    "ConfirmPublicVirtualInterface"
    "fixture/ConfirmPublicVirtualInterface.yaml"

requestConfirmTransitVirtualInterface :: ConfirmTransitVirtualInterface -> TestTree
requestConfirmTransitVirtualInterface =
  req
    "ConfirmTransitVirtualInterface"
    "fixture/ConfirmTransitVirtualInterface.yaml"

requestCreateBGPPeer :: CreateBGPPeer -> TestTree
requestCreateBGPPeer =
  req
    "CreateBGPPeer"
    "fixture/CreateBGPPeer.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestCreateDirectConnectGateway :: CreateDirectConnectGateway -> TestTree
requestCreateDirectConnectGateway =
  req
    "CreateDirectConnectGateway"
    "fixture/CreateDirectConnectGateway.yaml"

requestCreateDirectConnectGatewayAssociation :: CreateDirectConnectGatewayAssociation -> TestTree
requestCreateDirectConnectGatewayAssociation =
  req
    "CreateDirectConnectGatewayAssociation"
    "fixture/CreateDirectConnectGatewayAssociation.yaml"

requestCreateDirectConnectGatewayAssociationProposal :: CreateDirectConnectGatewayAssociationProposal -> TestTree
requestCreateDirectConnectGatewayAssociationProposal =
  req
    "CreateDirectConnectGatewayAssociationProposal"
    "fixture/CreateDirectConnectGatewayAssociationProposal.yaml"

requestCreateInterconnect :: CreateInterconnect -> TestTree
requestCreateInterconnect =
  req
    "CreateInterconnect"
    "fixture/CreateInterconnect.yaml"

requestCreateLag :: CreateLag -> TestTree
requestCreateLag =
  req
    "CreateLag"
    "fixture/CreateLag.yaml"

requestCreatePrivateVirtualInterface :: CreatePrivateVirtualInterface -> TestTree
requestCreatePrivateVirtualInterface =
  req
    "CreatePrivateVirtualInterface"
    "fixture/CreatePrivateVirtualInterface.yaml"

requestCreatePublicVirtualInterface :: CreatePublicVirtualInterface -> TestTree
requestCreatePublicVirtualInterface =
  req
    "CreatePublicVirtualInterface"
    "fixture/CreatePublicVirtualInterface.yaml"

requestCreateTransitVirtualInterface :: CreateTransitVirtualInterface -> TestTree
requestCreateTransitVirtualInterface =
  req
    "CreateTransitVirtualInterface"
    "fixture/CreateTransitVirtualInterface.yaml"

requestDeleteBGPPeer :: DeleteBGPPeer -> TestTree
requestDeleteBGPPeer =
  req
    "DeleteBGPPeer"
    "fixture/DeleteBGPPeer.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestDeleteDirectConnectGateway :: DeleteDirectConnectGateway -> TestTree
requestDeleteDirectConnectGateway =
  req
    "DeleteDirectConnectGateway"
    "fixture/DeleteDirectConnectGateway.yaml"

requestDeleteDirectConnectGatewayAssociation :: DeleteDirectConnectGatewayAssociation -> TestTree
requestDeleteDirectConnectGatewayAssociation =
  req
    "DeleteDirectConnectGatewayAssociation"
    "fixture/DeleteDirectConnectGatewayAssociation.yaml"

requestDeleteDirectConnectGatewayAssociationProposal :: DeleteDirectConnectGatewayAssociationProposal -> TestTree
requestDeleteDirectConnectGatewayAssociationProposal =
  req
    "DeleteDirectConnectGatewayAssociationProposal"
    "fixture/DeleteDirectConnectGatewayAssociationProposal.yaml"

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

requestDeleteVirtualInterface :: DeleteVirtualInterface -> TestTree
requestDeleteVirtualInterface =
  req
    "DeleteVirtualInterface"
    "fixture/DeleteVirtualInterface.yaml"

requestDescribeConnections :: DescribeConnections -> TestTree
requestDescribeConnections =
  req
    "DescribeConnections"
    "fixture/DescribeConnections.yaml"

requestDescribeCustomerMetadata :: DescribeCustomerMetadata -> TestTree
requestDescribeCustomerMetadata =
  req
    "DescribeCustomerMetadata"
    "fixture/DescribeCustomerMetadata.yaml"

requestDescribeDirectConnectGatewayAssociationProposals :: DescribeDirectConnectGatewayAssociationProposals -> TestTree
requestDescribeDirectConnectGatewayAssociationProposals =
  req
    "DescribeDirectConnectGatewayAssociationProposals"
    "fixture/DescribeDirectConnectGatewayAssociationProposals.yaml"

requestDescribeDirectConnectGatewayAssociations :: DescribeDirectConnectGatewayAssociations -> TestTree
requestDescribeDirectConnectGatewayAssociations =
  req
    "DescribeDirectConnectGatewayAssociations"
    "fixture/DescribeDirectConnectGatewayAssociations.yaml"

requestDescribeDirectConnectGatewayAttachments :: DescribeDirectConnectGatewayAttachments -> TestTree
requestDescribeDirectConnectGatewayAttachments =
  req
    "DescribeDirectConnectGatewayAttachments"
    "fixture/DescribeDirectConnectGatewayAttachments.yaml"

requestDescribeDirectConnectGateways :: DescribeDirectConnectGateways -> TestTree
requestDescribeDirectConnectGateways =
  req
    "DescribeDirectConnectGateways"
    "fixture/DescribeDirectConnectGateways.yaml"

requestDescribeHostedConnections :: DescribeHostedConnections -> TestTree
requestDescribeHostedConnections =
  req
    "DescribeHostedConnections"
    "fixture/DescribeHostedConnections.yaml"

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

requestDescribeLoa :: DescribeLoa -> TestTree
requestDescribeLoa =
  req
    "DescribeLoa"
    "fixture/DescribeLoa.yaml"

requestDescribeLocations :: DescribeLocations -> TestTree
requestDescribeLocations =
  req
    "DescribeLocations"
    "fixture/DescribeLocations.yaml"

requestDescribeRouterConfiguration :: DescribeRouterConfiguration -> TestTree
requestDescribeRouterConfiguration =
  req
    "DescribeRouterConfiguration"
    "fixture/DescribeRouterConfiguration.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeVirtualGateways :: DescribeVirtualGateways -> TestTree
requestDescribeVirtualGateways =
  req
    "DescribeVirtualGateways"
    "fixture/DescribeVirtualGateways.yaml"

requestDescribeVirtualInterfaces :: DescribeVirtualInterfaces -> TestTree
requestDescribeVirtualInterfaces =
  req
    "DescribeVirtualInterfaces"
    "fixture/DescribeVirtualInterfaces.yaml"

requestDisassociateConnectionFromLag :: DisassociateConnectionFromLag -> TestTree
requestDisassociateConnectionFromLag =
  req
    "DisassociateConnectionFromLag"
    "fixture/DisassociateConnectionFromLag.yaml"

requestDisassociateMacSecKey :: DisassociateMacSecKey -> TestTree
requestDisassociateMacSecKey =
  req
    "DisassociateMacSecKey"
    "fixture/DisassociateMacSecKey.yaml"

requestListVirtualInterfaceTestHistory :: ListVirtualInterfaceTestHistory -> TestTree
requestListVirtualInterfaceTestHistory =
  req
    "ListVirtualInterfaceTestHistory"
    "fixture/ListVirtualInterfaceTestHistory.yaml"

requestStartBgpFailoverTest :: StartBgpFailoverTest -> TestTree
requestStartBgpFailoverTest =
  req
    "StartBgpFailoverTest"
    "fixture/StartBgpFailoverTest.yaml"

requestStopBgpFailoverTest :: StopBgpFailoverTest -> TestTree
requestStopBgpFailoverTest =
  req
    "StopBgpFailoverTest"
    "fixture/StopBgpFailoverTest.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateConnection :: UpdateConnection -> TestTree
requestUpdateConnection =
  req
    "UpdateConnection"
    "fixture/UpdateConnection.yaml"

requestUpdateDirectConnectGateway :: UpdateDirectConnectGateway -> TestTree
requestUpdateDirectConnectGateway =
  req
    "UpdateDirectConnectGateway"
    "fixture/UpdateDirectConnectGateway.yaml"

requestUpdateDirectConnectGatewayAssociation :: UpdateDirectConnectGatewayAssociation -> TestTree
requestUpdateDirectConnectGatewayAssociation =
  req
    "UpdateDirectConnectGatewayAssociation"
    "fixture/UpdateDirectConnectGatewayAssociation.yaml"

requestUpdateLag :: UpdateLag -> TestTree
requestUpdateLag =
  req
    "UpdateLag"
    "fixture/UpdateLag.yaml"

requestUpdateVirtualInterfaceAttributes :: UpdateVirtualInterfaceAttributes -> TestTree
requestUpdateVirtualInterfaceAttributes =
  req
    "UpdateVirtualInterfaceAttributes"
    "fixture/UpdateVirtualInterfaceAttributes.yaml"

-- Responses

responseAcceptDirectConnectGatewayAssociationProposal :: AcceptDirectConnectGatewayAssociationProposalResponse -> TestTree
responseAcceptDirectConnectGatewayAssociationProposal =
  res
    "AcceptDirectConnectGatewayAssociationProposalResponse"
    "fixture/AcceptDirectConnectGatewayAssociationProposalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptDirectConnectGatewayAssociationProposal)

responseAllocateHostedConnection :: Connection -> TestTree
responseAllocateHostedConnection =
  res
    "AllocateHostedConnectionResponse"
    "fixture/AllocateHostedConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateHostedConnection)

responseAllocatePrivateVirtualInterface :: VirtualInterface -> TestTree
responseAllocatePrivateVirtualInterface =
  res
    "AllocatePrivateVirtualInterfaceResponse"
    "fixture/AllocatePrivateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocatePrivateVirtualInterface)

responseAllocatePublicVirtualInterface :: VirtualInterface -> TestTree
responseAllocatePublicVirtualInterface =
  res
    "AllocatePublicVirtualInterfaceResponse"
    "fixture/AllocatePublicVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocatePublicVirtualInterface)

responseAllocateTransitVirtualInterface :: AllocateTransitVirtualInterfaceResponse -> TestTree
responseAllocateTransitVirtualInterface =
  res
    "AllocateTransitVirtualInterfaceResponse"
    "fixture/AllocateTransitVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateTransitVirtualInterface)

responseAssociateConnectionWithLag :: Connection -> TestTree
responseAssociateConnectionWithLag =
  res
    "AssociateConnectionWithLagResponse"
    "fixture/AssociateConnectionWithLagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateConnectionWithLag)

responseAssociateHostedConnection :: Connection -> TestTree
responseAssociateHostedConnection =
  res
    "AssociateHostedConnectionResponse"
    "fixture/AssociateHostedConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateHostedConnection)

responseAssociateMacSecKey :: AssociateMacSecKeyResponse -> TestTree
responseAssociateMacSecKey =
  res
    "AssociateMacSecKeyResponse"
    "fixture/AssociateMacSecKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateMacSecKey)

responseAssociateVirtualInterface :: VirtualInterface -> TestTree
responseAssociateVirtualInterface =
  res
    "AssociateVirtualInterfaceResponse"
    "fixture/AssociateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateVirtualInterface)

responseConfirmConnection :: ConfirmConnectionResponse -> TestTree
responseConfirmConnection =
  res
    "ConfirmConnectionResponse"
    "fixture/ConfirmConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmConnection)

responseConfirmCustomerAgreement :: ConfirmCustomerAgreementResponse -> TestTree
responseConfirmCustomerAgreement =
  res
    "ConfirmCustomerAgreementResponse"
    "fixture/ConfirmCustomerAgreementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmCustomerAgreement)

responseConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterfaceResponse -> TestTree
responseConfirmPrivateVirtualInterface =
  res
    "ConfirmPrivateVirtualInterfaceResponse"
    "fixture/ConfirmPrivateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmPrivateVirtualInterface)

responseConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterfaceResponse -> TestTree
responseConfirmPublicVirtualInterface =
  res
    "ConfirmPublicVirtualInterfaceResponse"
    "fixture/ConfirmPublicVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmPublicVirtualInterface)

responseConfirmTransitVirtualInterface :: ConfirmTransitVirtualInterfaceResponse -> TestTree
responseConfirmTransitVirtualInterface =
  res
    "ConfirmTransitVirtualInterfaceResponse"
    "fixture/ConfirmTransitVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmTransitVirtualInterface)

responseCreateBGPPeer :: CreateBGPPeerResponse -> TestTree
responseCreateBGPPeer =
  res
    "CreateBGPPeerResponse"
    "fixture/CreateBGPPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBGPPeer)

responseCreateConnection :: Connection -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseCreateDirectConnectGateway :: CreateDirectConnectGatewayResponse -> TestTree
responseCreateDirectConnectGateway =
  res
    "CreateDirectConnectGatewayResponse"
    "fixture/CreateDirectConnectGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectConnectGateway)

responseCreateDirectConnectGatewayAssociation :: CreateDirectConnectGatewayAssociationResponse -> TestTree
responseCreateDirectConnectGatewayAssociation =
  res
    "CreateDirectConnectGatewayAssociationResponse"
    "fixture/CreateDirectConnectGatewayAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectConnectGatewayAssociation)

responseCreateDirectConnectGatewayAssociationProposal :: CreateDirectConnectGatewayAssociationProposalResponse -> TestTree
responseCreateDirectConnectGatewayAssociationProposal =
  res
    "CreateDirectConnectGatewayAssociationProposalResponse"
    "fixture/CreateDirectConnectGatewayAssociationProposalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectConnectGatewayAssociationProposal)

responseCreateInterconnect :: Interconnect -> TestTree
responseCreateInterconnect =
  res
    "CreateInterconnectResponse"
    "fixture/CreateInterconnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInterconnect)

responseCreateLag :: Lag -> TestTree
responseCreateLag =
  res
    "CreateLagResponse"
    "fixture/CreateLagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLag)

responseCreatePrivateVirtualInterface :: VirtualInterface -> TestTree
responseCreatePrivateVirtualInterface =
  res
    "CreatePrivateVirtualInterfaceResponse"
    "fixture/CreatePrivateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePrivateVirtualInterface)

responseCreatePublicVirtualInterface :: VirtualInterface -> TestTree
responseCreatePublicVirtualInterface =
  res
    "CreatePublicVirtualInterfaceResponse"
    "fixture/CreatePublicVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePublicVirtualInterface)

responseCreateTransitVirtualInterface :: CreateTransitVirtualInterfaceResponse -> TestTree
responseCreateTransitVirtualInterface =
  res
    "CreateTransitVirtualInterfaceResponse"
    "fixture/CreateTransitVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitVirtualInterface)

responseDeleteBGPPeer :: DeleteBGPPeerResponse -> TestTree
responseDeleteBGPPeer =
  res
    "DeleteBGPPeerResponse"
    "fixture/DeleteBGPPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBGPPeer)

responseDeleteConnection :: Connection -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseDeleteDirectConnectGateway :: DeleteDirectConnectGatewayResponse -> TestTree
responseDeleteDirectConnectGateway =
  res
    "DeleteDirectConnectGatewayResponse"
    "fixture/DeleteDirectConnectGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectConnectGateway)

responseDeleteDirectConnectGatewayAssociation :: DeleteDirectConnectGatewayAssociationResponse -> TestTree
responseDeleteDirectConnectGatewayAssociation =
  res
    "DeleteDirectConnectGatewayAssociationResponse"
    "fixture/DeleteDirectConnectGatewayAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectConnectGatewayAssociation)

responseDeleteDirectConnectGatewayAssociationProposal :: DeleteDirectConnectGatewayAssociationProposalResponse -> TestTree
responseDeleteDirectConnectGatewayAssociationProposal =
  res
    "DeleteDirectConnectGatewayAssociationProposalResponse"
    "fixture/DeleteDirectConnectGatewayAssociationProposalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectConnectGatewayAssociationProposal)

responseDeleteInterconnect :: DeleteInterconnectResponse -> TestTree
responseDeleteInterconnect =
  res
    "DeleteInterconnectResponse"
    "fixture/DeleteInterconnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInterconnect)

responseDeleteLag :: Lag -> TestTree
responseDeleteLag =
  res
    "DeleteLagResponse"
    "fixture/DeleteLagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLag)

responseDeleteVirtualInterface :: DeleteVirtualInterfaceResponse -> TestTree
responseDeleteVirtualInterface =
  res
    "DeleteVirtualInterfaceResponse"
    "fixture/DeleteVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualInterface)

responseDescribeConnections :: Connections -> TestTree
responseDescribeConnections =
  res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnections)

responseDescribeCustomerMetadata :: DescribeCustomerMetadataResponse -> TestTree
responseDescribeCustomerMetadata =
  res
    "DescribeCustomerMetadataResponse"
    "fixture/DescribeCustomerMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomerMetadata)

responseDescribeDirectConnectGatewayAssociationProposals :: DescribeDirectConnectGatewayAssociationProposalsResponse -> TestTree
responseDescribeDirectConnectGatewayAssociationProposals =
  res
    "DescribeDirectConnectGatewayAssociationProposalsResponse"
    "fixture/DescribeDirectConnectGatewayAssociationProposalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectConnectGatewayAssociationProposals)

responseDescribeDirectConnectGatewayAssociations :: DescribeDirectConnectGatewayAssociationsResponse -> TestTree
responseDescribeDirectConnectGatewayAssociations =
  res
    "DescribeDirectConnectGatewayAssociationsResponse"
    "fixture/DescribeDirectConnectGatewayAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectConnectGatewayAssociations)

responseDescribeDirectConnectGatewayAttachments :: DescribeDirectConnectGatewayAttachmentsResponse -> TestTree
responseDescribeDirectConnectGatewayAttachments =
  res
    "DescribeDirectConnectGatewayAttachmentsResponse"
    "fixture/DescribeDirectConnectGatewayAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectConnectGatewayAttachments)

responseDescribeDirectConnectGateways :: DescribeDirectConnectGatewaysResponse -> TestTree
responseDescribeDirectConnectGateways =
  res
    "DescribeDirectConnectGatewaysResponse"
    "fixture/DescribeDirectConnectGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectConnectGateways)

responseDescribeHostedConnections :: Connections -> TestTree
responseDescribeHostedConnections =
  res
    "DescribeHostedConnectionsResponse"
    "fixture/DescribeHostedConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHostedConnections)

responseDescribeInterconnects :: DescribeInterconnectsResponse -> TestTree
responseDescribeInterconnects =
  res
    "DescribeInterconnectsResponse"
    "fixture/DescribeInterconnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInterconnects)

responseDescribeLags :: DescribeLagsResponse -> TestTree
responseDescribeLags =
  res
    "DescribeLagsResponse"
    "fixture/DescribeLagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLags)

responseDescribeLoa :: DescribeLoaResponse -> TestTree
responseDescribeLoa =
  res
    "DescribeLoaResponse"
    "fixture/DescribeLoaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoa)

responseDescribeLocations :: DescribeLocationsResponse -> TestTree
responseDescribeLocations =
  res
    "DescribeLocationsResponse"
    "fixture/DescribeLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocations)

responseDescribeRouterConfiguration :: DescribeRouterConfigurationResponse -> TestTree
responseDescribeRouterConfiguration =
  res
    "DescribeRouterConfigurationResponse"
    "fixture/DescribeRouterConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRouterConfiguration)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseDescribeVirtualGateways :: DescribeVirtualGatewaysResponse -> TestTree
responseDescribeVirtualGateways =
  res
    "DescribeVirtualGatewaysResponse"
    "fixture/DescribeVirtualGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualGateways)

responseDescribeVirtualInterfaces :: DescribeVirtualInterfacesResponse -> TestTree
responseDescribeVirtualInterfaces =
  res
    "DescribeVirtualInterfacesResponse"
    "fixture/DescribeVirtualInterfacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualInterfaces)

responseDisassociateConnectionFromLag :: Connection -> TestTree
responseDisassociateConnectionFromLag =
  res
    "DisassociateConnectionFromLagResponse"
    "fixture/DisassociateConnectionFromLagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateConnectionFromLag)

responseDisassociateMacSecKey :: DisassociateMacSecKeyResponse -> TestTree
responseDisassociateMacSecKey =
  res
    "DisassociateMacSecKeyResponse"
    "fixture/DisassociateMacSecKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMacSecKey)

responseListVirtualInterfaceTestHistory :: ListVirtualInterfaceTestHistoryResponse -> TestTree
responseListVirtualInterfaceTestHistory =
  res
    "ListVirtualInterfaceTestHistoryResponse"
    "fixture/ListVirtualInterfaceTestHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualInterfaceTestHistory)

responseStartBgpFailoverTest :: StartBgpFailoverTestResponse -> TestTree
responseStartBgpFailoverTest =
  res
    "StartBgpFailoverTestResponse"
    "fixture/StartBgpFailoverTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBgpFailoverTest)

responseStopBgpFailoverTest :: StopBgpFailoverTestResponse -> TestTree
responseStopBgpFailoverTest =
  res
    "StopBgpFailoverTestResponse"
    "fixture/StopBgpFailoverTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBgpFailoverTest)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateConnection :: Connection -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnection)

responseUpdateDirectConnectGateway :: UpdateDirectConnectGatewayResponse -> TestTree
responseUpdateDirectConnectGateway =
  res
    "UpdateDirectConnectGatewayResponse"
    "fixture/UpdateDirectConnectGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDirectConnectGateway)

responseUpdateDirectConnectGatewayAssociation :: UpdateDirectConnectGatewayAssociationResponse -> TestTree
responseUpdateDirectConnectGatewayAssociation =
  res
    "UpdateDirectConnectGatewayAssociationResponse"
    "fixture/UpdateDirectConnectGatewayAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDirectConnectGatewayAssociation)

responseUpdateLag :: Lag -> TestTree
responseUpdateLag =
  res
    "UpdateLagResponse"
    "fixture/UpdateLagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLag)

responseUpdateVirtualInterfaceAttributes :: VirtualInterface -> TestTree
responseUpdateVirtualInterfaceAttributes =
  res
    "UpdateVirtualInterfaceAttributesResponse"
    "fixture/UpdateVirtualInterfaceAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVirtualInterfaceAttributes)

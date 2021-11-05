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

import Amazonka.DirectConnect
import qualified Data.Proxy as Proxy
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
--             newDescribeDirectConnectGatewayAssociations
--
--         , requestDescribeInterconnects $
--             newDescribeInterconnects
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestCreateTransitVirtualInterface $
--             newCreateTransitVirtualInterface
--
--         , requestDescribeLoa $
--             newDescribeLoa
--
--         , requestDisassociateMacSecKey $
--             newDisassociateMacSecKey
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestStartBgpFailoverTest $
--             newStartBgpFailoverTest
--
--         , requestUpdateVirtualInterfaceAttributes $
--             newUpdateVirtualInterfaceAttributes
--
--         , requestAssociateConnectionWithLag $
--             newAssociateConnectionWithLag
--
--         , requestCreateDirectConnectGatewayAssociationProposal $
--             newCreateDirectConnectGatewayAssociationProposal
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestDescribeDirectConnectGateways $
--             newDescribeDirectConnectGateways
--
--         , requestAssociateVirtualInterface $
--             newAssociateVirtualInterface
--
--         , requestDescribeConnections $
--             newDescribeConnections
--
--         , requestConfirmCustomerAgreement $
--             newConfirmCustomerAgreement
--
--         , requestDeleteInterconnect $
--             newDeleteInterconnect
--
--         , requestConfirmPrivateVirtualInterface $
--             newConfirmPrivateVirtualInterface
--
--         , requestUpdateDirectConnectGatewayAssociation $
--             newUpdateDirectConnectGatewayAssociation
--
--         , requestDeleteDirectConnectGatewayAssociation $
--             newDeleteDirectConnectGatewayAssociation
--
--         , requestDescribeLocations $
--             newDescribeLocations
--
--         , requestCreateDirectConnectGatewayAssociation $
--             newCreateDirectConnectGatewayAssociation
--
--         , requestAcceptDirectConnectGatewayAssociationProposal $
--             newAcceptDirectConnectGatewayAssociationProposal
--
--         , requestCreatePublicVirtualInterface $
--             newCreatePublicVirtualInterface
--
--         , requestAssociateMacSecKey $
--             newAssociateMacSecKey
--
--         , requestAllocatePrivateVirtualInterface $
--             newAllocatePrivateVirtualInterface
--
--         , requestDescribeLags $
--             newDescribeLags
--
--         , requestConfirmConnection $
--             newConfirmConnection
--
--         , requestDescribeDirectConnectGatewayAttachments $
--             newDescribeDirectConnectGatewayAttachments
--
--         , requestDescribeCustomerMetadata $
--             newDescribeCustomerMetadata
--
--         , requestConfirmPublicVirtualInterface $
--             newConfirmPublicVirtualInterface
--
--         , requestDescribeVirtualGateways $
--             newDescribeVirtualGateways
--
--         , requestDeleteDirectConnectGatewayAssociationProposal $
--             newDeleteDirectConnectGatewayAssociationProposal
--
--         , requestStopBgpFailoverTest $
--             newStopBgpFailoverTest
--
--         , requestCreateDirectConnectGateway $
--             newCreateDirectConnectGateway
--
--         , requestDeleteDirectConnectGateway $
--             newDeleteDirectConnectGateway
--
--         , requestUpdateDirectConnectGateway $
--             newUpdateDirectConnectGateway
--
--         , requestDescribeVirtualInterfaces $
--             newDescribeVirtualInterfaces
--
--         , requestListVirtualInterfaceTestHistory $
--             newListVirtualInterfaceTestHistory
--
--         , requestAllocateHostedConnection $
--             newAllocateHostedConnection
--
--         , requestDeleteVirtualInterface $
--             newDeleteVirtualInterface
--
--         , requestCreatePrivateVirtualInterface $
--             newCreatePrivateVirtualInterface
--
--         , requestAllocatePublicVirtualInterface $
--             newAllocatePublicVirtualInterface
--
--         , requestDescribeDirectConnectGatewayAssociationProposals $
--             newDescribeDirectConnectGatewayAssociationProposals
--
--         , requestDisassociateConnectionFromLag $
--             newDisassociateConnectionFromLag
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteLag $
--             newDeleteLag
--
--         , requestUpdateLag $
--             newUpdateLag
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateBGPPeer $
--             newCreateBGPPeer
--
--         , requestAssociateHostedConnection $
--             newAssociateHostedConnection
--
--         , requestCreateInterconnect $
--             newCreateInterconnect
--
--         , requestDescribeRouterConfiguration $
--             newDescribeRouterConfiguration
--
--         , requestDeleteBGPPeer $
--             newDeleteBGPPeer
--
--         , requestAllocateTransitVirtualInterface $
--             newAllocateTransitVirtualInterface
--
--         , requestCreateLag $
--             newCreateLag
--
--         , requestConfirmTransitVirtualInterface $
--             newConfirmTransitVirtualInterface
--
--         , requestDescribeHostedConnections $
--             newDescribeHostedConnections
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDirectConnectGatewayAssociations $
--             newDescribeDirectConnectGatewayAssociationsResponse
--
--         , responseDescribeInterconnects $
--             newDescribeInterconnectsResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseCreateTransitVirtualInterface $
--             newCreateTransitVirtualInterfaceResponse
--
--         , responseDescribeLoa $
--             newDescribeLoaResponse
--
--         , responseDisassociateMacSecKey $
--             newDisassociateMacSecKeyResponse
--
--         , responseDeleteConnection $
--             newConnection
--
--         , responseUpdateConnection $
--             newConnection
--
--         , responseStartBgpFailoverTest $
--             newStartBgpFailoverTestResponse
--
--         , responseUpdateVirtualInterfaceAttributes $
--             newVirtualInterface
--
--         , responseAssociateConnectionWithLag $
--             newConnection
--
--         , responseCreateDirectConnectGatewayAssociationProposal $
--             newCreateDirectConnectGatewayAssociationProposalResponse
--
--         , responseCreateConnection $
--             newConnection
--
--         , responseDescribeDirectConnectGateways $
--             newDescribeDirectConnectGatewaysResponse
--
--         , responseAssociateVirtualInterface $
--             newVirtualInterface
--
--         , responseDescribeConnections $
--             newConnections
--
--         , responseConfirmCustomerAgreement $
--             newConfirmCustomerAgreementResponse
--
--         , responseDeleteInterconnect $
--             newDeleteInterconnectResponse
--
--         , responseConfirmPrivateVirtualInterface $
--             newConfirmPrivateVirtualInterfaceResponse
--
--         , responseUpdateDirectConnectGatewayAssociation $
--             newUpdateDirectConnectGatewayAssociationResponse
--
--         , responseDeleteDirectConnectGatewayAssociation $
--             newDeleteDirectConnectGatewayAssociationResponse
--
--         , responseDescribeLocations $
--             newDescribeLocationsResponse
--
--         , responseCreateDirectConnectGatewayAssociation $
--             newCreateDirectConnectGatewayAssociationResponse
--
--         , responseAcceptDirectConnectGatewayAssociationProposal $
--             newAcceptDirectConnectGatewayAssociationProposalResponse
--
--         , responseCreatePublicVirtualInterface $
--             newVirtualInterface
--
--         , responseAssociateMacSecKey $
--             newAssociateMacSecKeyResponse
--
--         , responseAllocatePrivateVirtualInterface $
--             newVirtualInterface
--
--         , responseDescribeLags $
--             newDescribeLagsResponse
--
--         , responseConfirmConnection $
--             newConfirmConnectionResponse
--
--         , responseDescribeDirectConnectGatewayAttachments $
--             newDescribeDirectConnectGatewayAttachmentsResponse
--
--         , responseDescribeCustomerMetadata $
--             newDescribeCustomerMetadataResponse
--
--         , responseConfirmPublicVirtualInterface $
--             newConfirmPublicVirtualInterfaceResponse
--
--         , responseDescribeVirtualGateways $
--             newDescribeVirtualGatewaysResponse
--
--         , responseDeleteDirectConnectGatewayAssociationProposal $
--             newDeleteDirectConnectGatewayAssociationProposalResponse
--
--         , responseStopBgpFailoverTest $
--             newStopBgpFailoverTestResponse
--
--         , responseCreateDirectConnectGateway $
--             newCreateDirectConnectGatewayResponse
--
--         , responseDeleteDirectConnectGateway $
--             newDeleteDirectConnectGatewayResponse
--
--         , responseUpdateDirectConnectGateway $
--             newUpdateDirectConnectGatewayResponse
--
--         , responseDescribeVirtualInterfaces $
--             newDescribeVirtualInterfacesResponse
--
--         , responseListVirtualInterfaceTestHistory $
--             newListVirtualInterfaceTestHistoryResponse
--
--         , responseAllocateHostedConnection $
--             newConnection
--
--         , responseDeleteVirtualInterface $
--             newDeleteVirtualInterfaceResponse
--
--         , responseCreatePrivateVirtualInterface $
--             newVirtualInterface
--
--         , responseAllocatePublicVirtualInterface $
--             newVirtualInterface
--
--         , responseDescribeDirectConnectGatewayAssociationProposals $
--             newDescribeDirectConnectGatewayAssociationProposalsResponse
--
--         , responseDisassociateConnectionFromLag $
--             newConnection
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteLag $
--             newLag
--
--         , responseUpdateLag $
--             newLag
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateBGPPeer $
--             newCreateBGPPeerResponse
--
--         , responseAssociateHostedConnection $
--             newConnection
--
--         , responseCreateInterconnect $
--             newInterconnect
--
--         , responseDescribeRouterConfiguration $
--             newDescribeRouterConfigurationResponse
--
--         , responseDeleteBGPPeer $
--             newDeleteBGPPeerResponse
--
--         , responseAllocateTransitVirtualInterface $
--             newAllocateTransitVirtualInterfaceResponse
--
--         , responseCreateLag $
--             newLag
--
--         , responseConfirmTransitVirtualInterface $
--             newConfirmTransitVirtualInterfaceResponse
--
--         , responseDescribeHostedConnections $
--             newConnections
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

requestDisassociateMacSecKey :: DisassociateMacSecKey -> TestTree
requestDisassociateMacSecKey =
  req
    "DisassociateMacSecKey"
    "fixture/DisassociateMacSecKey.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestUpdateConnection :: UpdateConnection -> TestTree
requestUpdateConnection =
  req
    "UpdateConnection"
    "fixture/UpdateConnection.yaml"

requestStartBgpFailoverTest :: StartBgpFailoverTest -> TestTree
requestStartBgpFailoverTest =
  req
    "StartBgpFailoverTest"
    "fixture/StartBgpFailoverTest.yaml"

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

requestConfirmCustomerAgreement :: ConfirmCustomerAgreement -> TestTree
requestConfirmCustomerAgreement =
  req
    "ConfirmCustomerAgreement"
    "fixture/ConfirmCustomerAgreement.yaml"

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

requestAssociateMacSecKey :: AssociateMacSecKey -> TestTree
requestAssociateMacSecKey =
  req
    "AssociateMacSecKey"
    "fixture/AssociateMacSecKey.yaml"

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

requestDescribeCustomerMetadata :: DescribeCustomerMetadata -> TestTree
requestDescribeCustomerMetadata =
  req
    "DescribeCustomerMetadata"
    "fixture/DescribeCustomerMetadata.yaml"

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

requestStopBgpFailoverTest :: StopBgpFailoverTest -> TestTree
requestStopBgpFailoverTest =
  req
    "StopBgpFailoverTest"
    "fixture/StopBgpFailoverTest.yaml"

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

requestUpdateDirectConnectGateway :: UpdateDirectConnectGateway -> TestTree
requestUpdateDirectConnectGateway =
  req
    "UpdateDirectConnectGateway"
    "fixture/UpdateDirectConnectGateway.yaml"

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

requestDescribeRouterConfiguration :: DescribeRouterConfiguration -> TestTree
requestDescribeRouterConfiguration =
  req
    "DescribeRouterConfiguration"
    "fixture/DescribeRouterConfiguration.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectConnectGatewayAssociations)

responseDescribeInterconnects :: DescribeInterconnectsResponse -> TestTree
responseDescribeInterconnects =
  res
    "DescribeInterconnectsResponse"
    "fixture/DescribeInterconnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInterconnects)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseCreateTransitVirtualInterface :: CreateTransitVirtualInterfaceResponse -> TestTree
responseCreateTransitVirtualInterface =
  res
    "CreateTransitVirtualInterfaceResponse"
    "fixture/CreateTransitVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitVirtualInterface)

responseDescribeLoa :: DescribeLoaResponse -> TestTree
responseDescribeLoa =
  res
    "DescribeLoaResponse"
    "fixture/DescribeLoaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoa)

responseDisassociateMacSecKey :: DisassociateMacSecKeyResponse -> TestTree
responseDisassociateMacSecKey =
  res
    "DisassociateMacSecKeyResponse"
    "fixture/DisassociateMacSecKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMacSecKey)

responseDeleteConnection :: Connection -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseUpdateConnection :: Connection -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnection)

responseStartBgpFailoverTest :: StartBgpFailoverTestResponse -> TestTree
responseStartBgpFailoverTest =
  res
    "StartBgpFailoverTestResponse"
    "fixture/StartBgpFailoverTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBgpFailoverTest)

responseUpdateVirtualInterfaceAttributes :: VirtualInterface -> TestTree
responseUpdateVirtualInterfaceAttributes =
  res
    "UpdateVirtualInterfaceAttributesResponse"
    "fixture/UpdateVirtualInterfaceAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVirtualInterfaceAttributes)

responseAssociateConnectionWithLag :: Connection -> TestTree
responseAssociateConnectionWithLag =
  res
    "AssociateConnectionWithLagResponse"
    "fixture/AssociateConnectionWithLagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateConnectionWithLag)

responseCreateDirectConnectGatewayAssociationProposal :: CreateDirectConnectGatewayAssociationProposalResponse -> TestTree
responseCreateDirectConnectGatewayAssociationProposal =
  res
    "CreateDirectConnectGatewayAssociationProposalResponse"
    "fixture/CreateDirectConnectGatewayAssociationProposalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectConnectGatewayAssociationProposal)

responseCreateConnection :: Connection -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseDescribeDirectConnectGateways :: DescribeDirectConnectGatewaysResponse -> TestTree
responseDescribeDirectConnectGateways =
  res
    "DescribeDirectConnectGatewaysResponse"
    "fixture/DescribeDirectConnectGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectConnectGateways)

responseAssociateVirtualInterface :: VirtualInterface -> TestTree
responseAssociateVirtualInterface =
  res
    "AssociateVirtualInterfaceResponse"
    "fixture/AssociateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateVirtualInterface)

responseDescribeConnections :: Connections -> TestTree
responseDescribeConnections =
  res
    "DescribeConnectionsResponse"
    "fixture/DescribeConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnections)

responseConfirmCustomerAgreement :: ConfirmCustomerAgreementResponse -> TestTree
responseConfirmCustomerAgreement =
  res
    "ConfirmCustomerAgreementResponse"
    "fixture/ConfirmCustomerAgreementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmCustomerAgreement)

responseDeleteInterconnect :: DeleteInterconnectResponse -> TestTree
responseDeleteInterconnect =
  res
    "DeleteInterconnectResponse"
    "fixture/DeleteInterconnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInterconnect)

responseConfirmPrivateVirtualInterface :: ConfirmPrivateVirtualInterfaceResponse -> TestTree
responseConfirmPrivateVirtualInterface =
  res
    "ConfirmPrivateVirtualInterfaceResponse"
    "fixture/ConfirmPrivateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmPrivateVirtualInterface)

responseUpdateDirectConnectGatewayAssociation :: UpdateDirectConnectGatewayAssociationResponse -> TestTree
responseUpdateDirectConnectGatewayAssociation =
  res
    "UpdateDirectConnectGatewayAssociationResponse"
    "fixture/UpdateDirectConnectGatewayAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDirectConnectGatewayAssociation)

responseDeleteDirectConnectGatewayAssociation :: DeleteDirectConnectGatewayAssociationResponse -> TestTree
responseDeleteDirectConnectGatewayAssociation =
  res
    "DeleteDirectConnectGatewayAssociationResponse"
    "fixture/DeleteDirectConnectGatewayAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectConnectGatewayAssociation)

responseDescribeLocations :: DescribeLocationsResponse -> TestTree
responseDescribeLocations =
  res
    "DescribeLocationsResponse"
    "fixture/DescribeLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocations)

responseCreateDirectConnectGatewayAssociation :: CreateDirectConnectGatewayAssociationResponse -> TestTree
responseCreateDirectConnectGatewayAssociation =
  res
    "CreateDirectConnectGatewayAssociationResponse"
    "fixture/CreateDirectConnectGatewayAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectConnectGatewayAssociation)

responseAcceptDirectConnectGatewayAssociationProposal :: AcceptDirectConnectGatewayAssociationProposalResponse -> TestTree
responseAcceptDirectConnectGatewayAssociationProposal =
  res
    "AcceptDirectConnectGatewayAssociationProposalResponse"
    "fixture/AcceptDirectConnectGatewayAssociationProposalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptDirectConnectGatewayAssociationProposal)

responseCreatePublicVirtualInterface :: VirtualInterface -> TestTree
responseCreatePublicVirtualInterface =
  res
    "CreatePublicVirtualInterfaceResponse"
    "fixture/CreatePublicVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePublicVirtualInterface)

responseAssociateMacSecKey :: AssociateMacSecKeyResponse -> TestTree
responseAssociateMacSecKey =
  res
    "AssociateMacSecKeyResponse"
    "fixture/AssociateMacSecKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateMacSecKey)

responseAllocatePrivateVirtualInterface :: VirtualInterface -> TestTree
responseAllocatePrivateVirtualInterface =
  res
    "AllocatePrivateVirtualInterfaceResponse"
    "fixture/AllocatePrivateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocatePrivateVirtualInterface)

responseDescribeLags :: DescribeLagsResponse -> TestTree
responseDescribeLags =
  res
    "DescribeLagsResponse"
    "fixture/DescribeLagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLags)

responseConfirmConnection :: ConfirmConnectionResponse -> TestTree
responseConfirmConnection =
  res
    "ConfirmConnectionResponse"
    "fixture/ConfirmConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmConnection)

responseDescribeDirectConnectGatewayAttachments :: DescribeDirectConnectGatewayAttachmentsResponse -> TestTree
responseDescribeDirectConnectGatewayAttachments =
  res
    "DescribeDirectConnectGatewayAttachmentsResponse"
    "fixture/DescribeDirectConnectGatewayAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectConnectGatewayAttachments)

responseDescribeCustomerMetadata :: DescribeCustomerMetadataResponse -> TestTree
responseDescribeCustomerMetadata =
  res
    "DescribeCustomerMetadataResponse"
    "fixture/DescribeCustomerMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomerMetadata)

responseConfirmPublicVirtualInterface :: ConfirmPublicVirtualInterfaceResponse -> TestTree
responseConfirmPublicVirtualInterface =
  res
    "ConfirmPublicVirtualInterfaceResponse"
    "fixture/ConfirmPublicVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmPublicVirtualInterface)

responseDescribeVirtualGateways :: DescribeVirtualGatewaysResponse -> TestTree
responseDescribeVirtualGateways =
  res
    "DescribeVirtualGatewaysResponse"
    "fixture/DescribeVirtualGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualGateways)

responseDeleteDirectConnectGatewayAssociationProposal :: DeleteDirectConnectGatewayAssociationProposalResponse -> TestTree
responseDeleteDirectConnectGatewayAssociationProposal =
  res
    "DeleteDirectConnectGatewayAssociationProposalResponse"
    "fixture/DeleteDirectConnectGatewayAssociationProposalResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectConnectGatewayAssociationProposal)

responseStopBgpFailoverTest :: StopBgpFailoverTestResponse -> TestTree
responseStopBgpFailoverTest =
  res
    "StopBgpFailoverTestResponse"
    "fixture/StopBgpFailoverTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBgpFailoverTest)

responseCreateDirectConnectGateway :: CreateDirectConnectGatewayResponse -> TestTree
responseCreateDirectConnectGateway =
  res
    "CreateDirectConnectGatewayResponse"
    "fixture/CreateDirectConnectGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectConnectGateway)

responseDeleteDirectConnectGateway :: DeleteDirectConnectGatewayResponse -> TestTree
responseDeleteDirectConnectGateway =
  res
    "DeleteDirectConnectGatewayResponse"
    "fixture/DeleteDirectConnectGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectConnectGateway)

responseUpdateDirectConnectGateway :: UpdateDirectConnectGatewayResponse -> TestTree
responseUpdateDirectConnectGateway =
  res
    "UpdateDirectConnectGatewayResponse"
    "fixture/UpdateDirectConnectGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDirectConnectGateway)

responseDescribeVirtualInterfaces :: DescribeVirtualInterfacesResponse -> TestTree
responseDescribeVirtualInterfaces =
  res
    "DescribeVirtualInterfacesResponse"
    "fixture/DescribeVirtualInterfacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVirtualInterfaces)

responseListVirtualInterfaceTestHistory :: ListVirtualInterfaceTestHistoryResponse -> TestTree
responseListVirtualInterfaceTestHistory =
  res
    "ListVirtualInterfaceTestHistoryResponse"
    "fixture/ListVirtualInterfaceTestHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualInterfaceTestHistory)

responseAllocateHostedConnection :: Connection -> TestTree
responseAllocateHostedConnection =
  res
    "AllocateHostedConnectionResponse"
    "fixture/AllocateHostedConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateHostedConnection)

responseDeleteVirtualInterface :: DeleteVirtualInterfaceResponse -> TestTree
responseDeleteVirtualInterface =
  res
    "DeleteVirtualInterfaceResponse"
    "fixture/DeleteVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVirtualInterface)

responseCreatePrivateVirtualInterface :: VirtualInterface -> TestTree
responseCreatePrivateVirtualInterface =
  res
    "CreatePrivateVirtualInterfaceResponse"
    "fixture/CreatePrivateVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePrivateVirtualInterface)

responseAllocatePublicVirtualInterface :: VirtualInterface -> TestTree
responseAllocatePublicVirtualInterface =
  res
    "AllocatePublicVirtualInterfaceResponse"
    "fixture/AllocatePublicVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocatePublicVirtualInterface)

responseDescribeDirectConnectGatewayAssociationProposals :: DescribeDirectConnectGatewayAssociationProposalsResponse -> TestTree
responseDescribeDirectConnectGatewayAssociationProposals =
  res
    "DescribeDirectConnectGatewayAssociationProposalsResponse"
    "fixture/DescribeDirectConnectGatewayAssociationProposalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectConnectGatewayAssociationProposals)

responseDisassociateConnectionFromLag :: Connection -> TestTree
responseDisassociateConnectionFromLag =
  res
    "DisassociateConnectionFromLagResponse"
    "fixture/DisassociateConnectionFromLagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateConnectionFromLag)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDeleteLag :: Lag -> TestTree
responseDeleteLag =
  res
    "DeleteLagResponse"
    "fixture/DeleteLagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLag)

responseUpdateLag :: Lag -> TestTree
responseUpdateLag =
  res
    "UpdateLagResponse"
    "fixture/UpdateLagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLag)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseCreateBGPPeer :: CreateBGPPeerResponse -> TestTree
responseCreateBGPPeer =
  res
    "CreateBGPPeerResponse"
    "fixture/CreateBGPPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBGPPeer)

responseAssociateHostedConnection :: Connection -> TestTree
responseAssociateHostedConnection =
  res
    "AssociateHostedConnectionResponse"
    "fixture/AssociateHostedConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateHostedConnection)

responseCreateInterconnect :: Interconnect -> TestTree
responseCreateInterconnect =
  res
    "CreateInterconnectResponse"
    "fixture/CreateInterconnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInterconnect)

responseDescribeRouterConfiguration :: DescribeRouterConfigurationResponse -> TestTree
responseDescribeRouterConfiguration =
  res
    "DescribeRouterConfigurationResponse"
    "fixture/DescribeRouterConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRouterConfiguration)

responseDeleteBGPPeer :: DeleteBGPPeerResponse -> TestTree
responseDeleteBGPPeer =
  res
    "DeleteBGPPeerResponse"
    "fixture/DeleteBGPPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBGPPeer)

responseAllocateTransitVirtualInterface :: AllocateTransitVirtualInterfaceResponse -> TestTree
responseAllocateTransitVirtualInterface =
  res
    "AllocateTransitVirtualInterfaceResponse"
    "fixture/AllocateTransitVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateTransitVirtualInterface)

responseCreateLag :: Lag -> TestTree
responseCreateLag =
  res
    "CreateLagResponse"
    "fixture/CreateLagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLag)

responseConfirmTransitVirtualInterface :: ConfirmTransitVirtualInterfaceResponse -> TestTree
responseConfirmTransitVirtualInterface =
  res
    "ConfirmTransitVirtualInterfaceResponse"
    "fixture/ConfirmTransitVirtualInterfaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmTransitVirtualInterface)

responseDescribeHostedConnections :: Connections -> TestTree
responseDescribeHostedConnections =
  res
    "DescribeHostedConnectionsResponse"
    "fixture/DescribeHostedConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHostedConnections)

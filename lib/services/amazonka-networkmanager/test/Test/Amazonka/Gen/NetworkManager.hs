{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.NetworkManager
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.NetworkManager where

import Amazonka.NetworkManager
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.NetworkManager.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptAttachment $
--             newAcceptAttachment
--
--         , requestAssociateConnectPeer $
--             newAssociateConnectPeer
--
--         , requestAssociateCustomerGateway $
--             newAssociateCustomerGateway
--
--         , requestAssociateLink $
--             newAssociateLink
--
--         , requestAssociateTransitGatewayConnectPeer $
--             newAssociateTransitGatewayConnectPeer
--
--         , requestCreateConnectAttachment $
--             newCreateConnectAttachment
--
--         , requestCreateConnectPeer $
--             newCreateConnectPeer
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestCreateCoreNetwork $
--             newCreateCoreNetwork
--
--         , requestCreateDevice $
--             newCreateDevice
--
--         , requestCreateGlobalNetwork $
--             newCreateGlobalNetwork
--
--         , requestCreateLink $
--             newCreateLink
--
--         , requestCreateSite $
--             newCreateSite
--
--         , requestCreateSiteToSiteVpnAttachment $
--             newCreateSiteToSiteVpnAttachment
--
--         , requestCreateTransitGatewayPeering $
--             newCreateTransitGatewayPeering
--
--         , requestCreateTransitGatewayRouteTableAttachment $
--             newCreateTransitGatewayRouteTableAttachment
--
--         , requestCreateVpcAttachment $
--             newCreateVpcAttachment
--
--         , requestDeleteAttachment $
--             newDeleteAttachment
--
--         , requestDeleteConnectPeer $
--             newDeleteConnectPeer
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDeleteCoreNetwork $
--             newDeleteCoreNetwork
--
--         , requestDeleteCoreNetworkPolicyVersion $
--             newDeleteCoreNetworkPolicyVersion
--
--         , requestDeleteDevice $
--             newDeleteDevice
--
--         , requestDeleteGlobalNetwork $
--             newDeleteGlobalNetwork
--
--         , requestDeleteLink $
--             newDeleteLink
--
--         , requestDeletePeering $
--             newDeletePeering
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteSite $
--             newDeleteSite
--
--         , requestDeregisterTransitGateway $
--             newDeregisterTransitGateway
--
--         , requestDescribeGlobalNetworks $
--             newDescribeGlobalNetworks
--
--         , requestDisassociateConnectPeer $
--             newDisassociateConnectPeer
--
--         , requestDisassociateCustomerGateway $
--             newDisassociateCustomerGateway
--
--         , requestDisassociateLink $
--             newDisassociateLink
--
--         , requestDisassociateTransitGatewayConnectPeer $
--             newDisassociateTransitGatewayConnectPeer
--
--         , requestExecuteCoreNetworkChangeSet $
--             newExecuteCoreNetworkChangeSet
--
--         , requestGetConnectAttachment $
--             newGetConnectAttachment
--
--         , requestGetConnectPeer $
--             newGetConnectPeer
--
--         , requestGetConnectPeerAssociations $
--             newGetConnectPeerAssociations
--
--         , requestGetConnections $
--             newGetConnections
--
--         , requestGetCoreNetwork $
--             newGetCoreNetwork
--
--         , requestGetCoreNetworkChangeEvents $
--             newGetCoreNetworkChangeEvents
--
--         , requestGetCoreNetworkChangeSet $
--             newGetCoreNetworkChangeSet
--
--         , requestGetCoreNetworkPolicy $
--             newGetCoreNetworkPolicy
--
--         , requestGetCustomerGatewayAssociations $
--             newGetCustomerGatewayAssociations
--
--         , requestGetDevices $
--             newGetDevices
--
--         , requestGetLinkAssociations $
--             newGetLinkAssociations
--
--         , requestGetLinks $
--             newGetLinks
--
--         , requestGetNetworkResourceCounts $
--             newGetNetworkResourceCounts
--
--         , requestGetNetworkResourceRelationships $
--             newGetNetworkResourceRelationships
--
--         , requestGetNetworkResources $
--             newGetNetworkResources
--
--         , requestGetNetworkRoutes $
--             newGetNetworkRoutes
--
--         , requestGetNetworkTelemetry $
--             newGetNetworkTelemetry
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestGetRouteAnalysis $
--             newGetRouteAnalysis
--
--         , requestGetSiteToSiteVpnAttachment $
--             newGetSiteToSiteVpnAttachment
--
--         , requestGetSites $
--             newGetSites
--
--         , requestGetTransitGatewayConnectPeerAssociations $
--             newGetTransitGatewayConnectPeerAssociations
--
--         , requestGetTransitGatewayPeering $
--             newGetTransitGatewayPeering
--
--         , requestGetTransitGatewayRegistrations $
--             newGetTransitGatewayRegistrations
--
--         , requestGetTransitGatewayRouteTableAttachment $
--             newGetTransitGatewayRouteTableAttachment
--
--         , requestGetVpcAttachment $
--             newGetVpcAttachment
--
--         , requestListAttachments $
--             newListAttachments
--
--         , requestListConnectPeers $
--             newListConnectPeers
--
--         , requestListCoreNetworkPolicyVersions $
--             newListCoreNetworkPolicyVersions
--
--         , requestListCoreNetworks $
--             newListCoreNetworks
--
--         , requestListOrganizationServiceAccessStatus $
--             newListOrganizationServiceAccessStatus
--
--         , requestListPeerings $
--             newListPeerings
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutCoreNetworkPolicy $
--             newPutCoreNetworkPolicy
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestRegisterTransitGateway $
--             newRegisterTransitGateway
--
--         , requestRejectAttachment $
--             newRejectAttachment
--
--         , requestRestoreCoreNetworkPolicyVersion $
--             newRestoreCoreNetworkPolicyVersion
--
--         , requestStartOrganizationServiceAccessUpdate $
--             newStartOrganizationServiceAccessUpdate
--
--         , requestStartRouteAnalysis $
--             newStartRouteAnalysis
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
--         , requestUpdateCoreNetwork $
--             newUpdateCoreNetwork
--
--         , requestUpdateDevice $
--             newUpdateDevice
--
--         , requestUpdateGlobalNetwork $
--             newUpdateGlobalNetwork
--
--         , requestUpdateLink $
--             newUpdateLink
--
--         , requestUpdateNetworkResourceMetadata $
--             newUpdateNetworkResourceMetadata
--
--         , requestUpdateSite $
--             newUpdateSite
--
--         , requestUpdateVpcAttachment $
--             newUpdateVpcAttachment
--
--           ]

--     , testGroup "response"
--         [ responseAcceptAttachment $
--             newAcceptAttachmentResponse
--
--         , responseAssociateConnectPeer $
--             newAssociateConnectPeerResponse
--
--         , responseAssociateCustomerGateway $
--             newAssociateCustomerGatewayResponse
--
--         , responseAssociateLink $
--             newAssociateLinkResponse
--
--         , responseAssociateTransitGatewayConnectPeer $
--             newAssociateTransitGatewayConnectPeerResponse
--
--         , responseCreateConnectAttachment $
--             newCreateConnectAttachmentResponse
--
--         , responseCreateConnectPeer $
--             newCreateConnectPeerResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseCreateCoreNetwork $
--             newCreateCoreNetworkResponse
--
--         , responseCreateDevice $
--             newCreateDeviceResponse
--
--         , responseCreateGlobalNetwork $
--             newCreateGlobalNetworkResponse
--
--         , responseCreateLink $
--             newCreateLinkResponse
--
--         , responseCreateSite $
--             newCreateSiteResponse
--
--         , responseCreateSiteToSiteVpnAttachment $
--             newCreateSiteToSiteVpnAttachmentResponse
--
--         , responseCreateTransitGatewayPeering $
--             newCreateTransitGatewayPeeringResponse
--
--         , responseCreateTransitGatewayRouteTableAttachment $
--             newCreateTransitGatewayRouteTableAttachmentResponse
--
--         , responseCreateVpcAttachment $
--             newCreateVpcAttachmentResponse
--
--         , responseDeleteAttachment $
--             newDeleteAttachmentResponse
--
--         , responseDeleteConnectPeer $
--             newDeleteConnectPeerResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseDeleteCoreNetwork $
--             newDeleteCoreNetworkResponse
--
--         , responseDeleteCoreNetworkPolicyVersion $
--             newDeleteCoreNetworkPolicyVersionResponse
--
--         , responseDeleteDevice $
--             newDeleteDeviceResponse
--
--         , responseDeleteGlobalNetwork $
--             newDeleteGlobalNetworkResponse
--
--         , responseDeleteLink $
--             newDeleteLinkResponse
--
--         , responseDeletePeering $
--             newDeletePeeringResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteSite $
--             newDeleteSiteResponse
--
--         , responseDeregisterTransitGateway $
--             newDeregisterTransitGatewayResponse
--
--         , responseDescribeGlobalNetworks $
--             newDescribeGlobalNetworksResponse
--
--         , responseDisassociateConnectPeer $
--             newDisassociateConnectPeerResponse
--
--         , responseDisassociateCustomerGateway $
--             newDisassociateCustomerGatewayResponse
--
--         , responseDisassociateLink $
--             newDisassociateLinkResponse
--
--         , responseDisassociateTransitGatewayConnectPeer $
--             newDisassociateTransitGatewayConnectPeerResponse
--
--         , responseExecuteCoreNetworkChangeSet $
--             newExecuteCoreNetworkChangeSetResponse
--
--         , responseGetConnectAttachment $
--             newGetConnectAttachmentResponse
--
--         , responseGetConnectPeer $
--             newGetConnectPeerResponse
--
--         , responseGetConnectPeerAssociations $
--             newGetConnectPeerAssociationsResponse
--
--         , responseGetConnections $
--             newGetConnectionsResponse
--
--         , responseGetCoreNetwork $
--             newGetCoreNetworkResponse
--
--         , responseGetCoreNetworkChangeEvents $
--             newGetCoreNetworkChangeEventsResponse
--
--         , responseGetCoreNetworkChangeSet $
--             newGetCoreNetworkChangeSetResponse
--
--         , responseGetCoreNetworkPolicy $
--             newGetCoreNetworkPolicyResponse
--
--         , responseGetCustomerGatewayAssociations $
--             newGetCustomerGatewayAssociationsResponse
--
--         , responseGetDevices $
--             newGetDevicesResponse
--
--         , responseGetLinkAssociations $
--             newGetLinkAssociationsResponse
--
--         , responseGetLinks $
--             newGetLinksResponse
--
--         , responseGetNetworkResourceCounts $
--             newGetNetworkResourceCountsResponse
--
--         , responseGetNetworkResourceRelationships $
--             newGetNetworkResourceRelationshipsResponse
--
--         , responseGetNetworkResources $
--             newGetNetworkResourcesResponse
--
--         , responseGetNetworkRoutes $
--             newGetNetworkRoutesResponse
--
--         , responseGetNetworkTelemetry $
--             newGetNetworkTelemetryResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseGetRouteAnalysis $
--             newGetRouteAnalysisResponse
--
--         , responseGetSiteToSiteVpnAttachment $
--             newGetSiteToSiteVpnAttachmentResponse
--
--         , responseGetSites $
--             newGetSitesResponse
--
--         , responseGetTransitGatewayConnectPeerAssociations $
--             newGetTransitGatewayConnectPeerAssociationsResponse
--
--         , responseGetTransitGatewayPeering $
--             newGetTransitGatewayPeeringResponse
--
--         , responseGetTransitGatewayRegistrations $
--             newGetTransitGatewayRegistrationsResponse
--
--         , responseGetTransitGatewayRouteTableAttachment $
--             newGetTransitGatewayRouteTableAttachmentResponse
--
--         , responseGetVpcAttachment $
--             newGetVpcAttachmentResponse
--
--         , responseListAttachments $
--             newListAttachmentsResponse
--
--         , responseListConnectPeers $
--             newListConnectPeersResponse
--
--         , responseListCoreNetworkPolicyVersions $
--             newListCoreNetworkPolicyVersionsResponse
--
--         , responseListCoreNetworks $
--             newListCoreNetworksResponse
--
--         , responseListOrganizationServiceAccessStatus $
--             newListOrganizationServiceAccessStatusResponse
--
--         , responseListPeerings $
--             newListPeeringsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutCoreNetworkPolicy $
--             newPutCoreNetworkPolicyResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseRegisterTransitGateway $
--             newRegisterTransitGatewayResponse
--
--         , responseRejectAttachment $
--             newRejectAttachmentResponse
--
--         , responseRestoreCoreNetworkPolicyVersion $
--             newRestoreCoreNetworkPolicyVersionResponse
--
--         , responseStartOrganizationServiceAccessUpdate $
--             newStartOrganizationServiceAccessUpdateResponse
--
--         , responseStartRouteAnalysis $
--             newStartRouteAnalysisResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateConnection $
--             newUpdateConnectionResponse
--
--         , responseUpdateCoreNetwork $
--             newUpdateCoreNetworkResponse
--
--         , responseUpdateDevice $
--             newUpdateDeviceResponse
--
--         , responseUpdateGlobalNetwork $
--             newUpdateGlobalNetworkResponse
--
--         , responseUpdateLink $
--             newUpdateLinkResponse
--
--         , responseUpdateNetworkResourceMetadata $
--             newUpdateNetworkResourceMetadataResponse
--
--         , responseUpdateSite $
--             newUpdateSiteResponse
--
--         , responseUpdateVpcAttachment $
--             newUpdateVpcAttachmentResponse
--
--           ]
--     ]

-- Requests

requestAcceptAttachment :: AcceptAttachment -> TestTree
requestAcceptAttachment =
  req
    "AcceptAttachment"
    "fixture/AcceptAttachment.yaml"

requestAssociateConnectPeer :: AssociateConnectPeer -> TestTree
requestAssociateConnectPeer =
  req
    "AssociateConnectPeer"
    "fixture/AssociateConnectPeer.yaml"

requestAssociateCustomerGateway :: AssociateCustomerGateway -> TestTree
requestAssociateCustomerGateway =
  req
    "AssociateCustomerGateway"
    "fixture/AssociateCustomerGateway.yaml"

requestAssociateLink :: AssociateLink -> TestTree
requestAssociateLink =
  req
    "AssociateLink"
    "fixture/AssociateLink.yaml"

requestAssociateTransitGatewayConnectPeer :: AssociateTransitGatewayConnectPeer -> TestTree
requestAssociateTransitGatewayConnectPeer =
  req
    "AssociateTransitGatewayConnectPeer"
    "fixture/AssociateTransitGatewayConnectPeer.yaml"

requestCreateConnectAttachment :: CreateConnectAttachment -> TestTree
requestCreateConnectAttachment =
  req
    "CreateConnectAttachment"
    "fixture/CreateConnectAttachment.yaml"

requestCreateConnectPeer :: CreateConnectPeer -> TestTree
requestCreateConnectPeer =
  req
    "CreateConnectPeer"
    "fixture/CreateConnectPeer.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestCreateCoreNetwork :: CreateCoreNetwork -> TestTree
requestCreateCoreNetwork =
  req
    "CreateCoreNetwork"
    "fixture/CreateCoreNetwork.yaml"

requestCreateDevice :: CreateDevice -> TestTree
requestCreateDevice =
  req
    "CreateDevice"
    "fixture/CreateDevice.yaml"

requestCreateGlobalNetwork :: CreateGlobalNetwork -> TestTree
requestCreateGlobalNetwork =
  req
    "CreateGlobalNetwork"
    "fixture/CreateGlobalNetwork.yaml"

requestCreateLink :: CreateLink -> TestTree
requestCreateLink =
  req
    "CreateLink"
    "fixture/CreateLink.yaml"

requestCreateSite :: CreateSite -> TestTree
requestCreateSite =
  req
    "CreateSite"
    "fixture/CreateSite.yaml"

requestCreateSiteToSiteVpnAttachment :: CreateSiteToSiteVpnAttachment -> TestTree
requestCreateSiteToSiteVpnAttachment =
  req
    "CreateSiteToSiteVpnAttachment"
    "fixture/CreateSiteToSiteVpnAttachment.yaml"

requestCreateTransitGatewayPeering :: CreateTransitGatewayPeering -> TestTree
requestCreateTransitGatewayPeering =
  req
    "CreateTransitGatewayPeering"
    "fixture/CreateTransitGatewayPeering.yaml"

requestCreateTransitGatewayRouteTableAttachment :: CreateTransitGatewayRouteTableAttachment -> TestTree
requestCreateTransitGatewayRouteTableAttachment =
  req
    "CreateTransitGatewayRouteTableAttachment"
    "fixture/CreateTransitGatewayRouteTableAttachment.yaml"

requestCreateVpcAttachment :: CreateVpcAttachment -> TestTree
requestCreateVpcAttachment =
  req
    "CreateVpcAttachment"
    "fixture/CreateVpcAttachment.yaml"

requestDeleteAttachment :: DeleteAttachment -> TestTree
requestDeleteAttachment =
  req
    "DeleteAttachment"
    "fixture/DeleteAttachment.yaml"

requestDeleteConnectPeer :: DeleteConnectPeer -> TestTree
requestDeleteConnectPeer =
  req
    "DeleteConnectPeer"
    "fixture/DeleteConnectPeer.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestDeleteCoreNetwork :: DeleteCoreNetwork -> TestTree
requestDeleteCoreNetwork =
  req
    "DeleteCoreNetwork"
    "fixture/DeleteCoreNetwork.yaml"

requestDeleteCoreNetworkPolicyVersion :: DeleteCoreNetworkPolicyVersion -> TestTree
requestDeleteCoreNetworkPolicyVersion =
  req
    "DeleteCoreNetworkPolicyVersion"
    "fixture/DeleteCoreNetworkPolicyVersion.yaml"

requestDeleteDevice :: DeleteDevice -> TestTree
requestDeleteDevice =
  req
    "DeleteDevice"
    "fixture/DeleteDevice.yaml"

requestDeleteGlobalNetwork :: DeleteGlobalNetwork -> TestTree
requestDeleteGlobalNetwork =
  req
    "DeleteGlobalNetwork"
    "fixture/DeleteGlobalNetwork.yaml"

requestDeleteLink :: DeleteLink -> TestTree
requestDeleteLink =
  req
    "DeleteLink"
    "fixture/DeleteLink.yaml"

requestDeletePeering :: DeletePeering -> TestTree
requestDeletePeering =
  req
    "DeletePeering"
    "fixture/DeletePeering.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteSite :: DeleteSite -> TestTree
requestDeleteSite =
  req
    "DeleteSite"
    "fixture/DeleteSite.yaml"

requestDeregisterTransitGateway :: DeregisterTransitGateway -> TestTree
requestDeregisterTransitGateway =
  req
    "DeregisterTransitGateway"
    "fixture/DeregisterTransitGateway.yaml"

requestDescribeGlobalNetworks :: DescribeGlobalNetworks -> TestTree
requestDescribeGlobalNetworks =
  req
    "DescribeGlobalNetworks"
    "fixture/DescribeGlobalNetworks.yaml"

requestDisassociateConnectPeer :: DisassociateConnectPeer -> TestTree
requestDisassociateConnectPeer =
  req
    "DisassociateConnectPeer"
    "fixture/DisassociateConnectPeer.yaml"

requestDisassociateCustomerGateway :: DisassociateCustomerGateway -> TestTree
requestDisassociateCustomerGateway =
  req
    "DisassociateCustomerGateway"
    "fixture/DisassociateCustomerGateway.yaml"

requestDisassociateLink :: DisassociateLink -> TestTree
requestDisassociateLink =
  req
    "DisassociateLink"
    "fixture/DisassociateLink.yaml"

requestDisassociateTransitGatewayConnectPeer :: DisassociateTransitGatewayConnectPeer -> TestTree
requestDisassociateTransitGatewayConnectPeer =
  req
    "DisassociateTransitGatewayConnectPeer"
    "fixture/DisassociateTransitGatewayConnectPeer.yaml"

requestExecuteCoreNetworkChangeSet :: ExecuteCoreNetworkChangeSet -> TestTree
requestExecuteCoreNetworkChangeSet =
  req
    "ExecuteCoreNetworkChangeSet"
    "fixture/ExecuteCoreNetworkChangeSet.yaml"

requestGetConnectAttachment :: GetConnectAttachment -> TestTree
requestGetConnectAttachment =
  req
    "GetConnectAttachment"
    "fixture/GetConnectAttachment.yaml"

requestGetConnectPeer :: GetConnectPeer -> TestTree
requestGetConnectPeer =
  req
    "GetConnectPeer"
    "fixture/GetConnectPeer.yaml"

requestGetConnectPeerAssociations :: GetConnectPeerAssociations -> TestTree
requestGetConnectPeerAssociations =
  req
    "GetConnectPeerAssociations"
    "fixture/GetConnectPeerAssociations.yaml"

requestGetConnections :: GetConnections -> TestTree
requestGetConnections =
  req
    "GetConnections"
    "fixture/GetConnections.yaml"

requestGetCoreNetwork :: GetCoreNetwork -> TestTree
requestGetCoreNetwork =
  req
    "GetCoreNetwork"
    "fixture/GetCoreNetwork.yaml"

requestGetCoreNetworkChangeEvents :: GetCoreNetworkChangeEvents -> TestTree
requestGetCoreNetworkChangeEvents =
  req
    "GetCoreNetworkChangeEvents"
    "fixture/GetCoreNetworkChangeEvents.yaml"

requestGetCoreNetworkChangeSet :: GetCoreNetworkChangeSet -> TestTree
requestGetCoreNetworkChangeSet =
  req
    "GetCoreNetworkChangeSet"
    "fixture/GetCoreNetworkChangeSet.yaml"

requestGetCoreNetworkPolicy :: GetCoreNetworkPolicy -> TestTree
requestGetCoreNetworkPolicy =
  req
    "GetCoreNetworkPolicy"
    "fixture/GetCoreNetworkPolicy.yaml"

requestGetCustomerGatewayAssociations :: GetCustomerGatewayAssociations -> TestTree
requestGetCustomerGatewayAssociations =
  req
    "GetCustomerGatewayAssociations"
    "fixture/GetCustomerGatewayAssociations.yaml"

requestGetDevices :: GetDevices -> TestTree
requestGetDevices =
  req
    "GetDevices"
    "fixture/GetDevices.yaml"

requestGetLinkAssociations :: GetLinkAssociations -> TestTree
requestGetLinkAssociations =
  req
    "GetLinkAssociations"
    "fixture/GetLinkAssociations.yaml"

requestGetLinks :: GetLinks -> TestTree
requestGetLinks =
  req
    "GetLinks"
    "fixture/GetLinks.yaml"

requestGetNetworkResourceCounts :: GetNetworkResourceCounts -> TestTree
requestGetNetworkResourceCounts =
  req
    "GetNetworkResourceCounts"
    "fixture/GetNetworkResourceCounts.yaml"

requestGetNetworkResourceRelationships :: GetNetworkResourceRelationships -> TestTree
requestGetNetworkResourceRelationships =
  req
    "GetNetworkResourceRelationships"
    "fixture/GetNetworkResourceRelationships.yaml"

requestGetNetworkResources :: GetNetworkResources -> TestTree
requestGetNetworkResources =
  req
    "GetNetworkResources"
    "fixture/GetNetworkResources.yaml"

requestGetNetworkRoutes :: GetNetworkRoutes -> TestTree
requestGetNetworkRoutes =
  req
    "GetNetworkRoutes"
    "fixture/GetNetworkRoutes.yaml"

requestGetNetworkTelemetry :: GetNetworkTelemetry -> TestTree
requestGetNetworkTelemetry =
  req
    "GetNetworkTelemetry"
    "fixture/GetNetworkTelemetry.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestGetRouteAnalysis :: GetRouteAnalysis -> TestTree
requestGetRouteAnalysis =
  req
    "GetRouteAnalysis"
    "fixture/GetRouteAnalysis.yaml"

requestGetSiteToSiteVpnAttachment :: GetSiteToSiteVpnAttachment -> TestTree
requestGetSiteToSiteVpnAttachment =
  req
    "GetSiteToSiteVpnAttachment"
    "fixture/GetSiteToSiteVpnAttachment.yaml"

requestGetSites :: GetSites -> TestTree
requestGetSites =
  req
    "GetSites"
    "fixture/GetSites.yaml"

requestGetTransitGatewayConnectPeerAssociations :: GetTransitGatewayConnectPeerAssociations -> TestTree
requestGetTransitGatewayConnectPeerAssociations =
  req
    "GetTransitGatewayConnectPeerAssociations"
    "fixture/GetTransitGatewayConnectPeerAssociations.yaml"

requestGetTransitGatewayPeering :: GetTransitGatewayPeering -> TestTree
requestGetTransitGatewayPeering =
  req
    "GetTransitGatewayPeering"
    "fixture/GetTransitGatewayPeering.yaml"

requestGetTransitGatewayRegistrations :: GetTransitGatewayRegistrations -> TestTree
requestGetTransitGatewayRegistrations =
  req
    "GetTransitGatewayRegistrations"
    "fixture/GetTransitGatewayRegistrations.yaml"

requestGetTransitGatewayRouteTableAttachment :: GetTransitGatewayRouteTableAttachment -> TestTree
requestGetTransitGatewayRouteTableAttachment =
  req
    "GetTransitGatewayRouteTableAttachment"
    "fixture/GetTransitGatewayRouteTableAttachment.yaml"

requestGetVpcAttachment :: GetVpcAttachment -> TestTree
requestGetVpcAttachment =
  req
    "GetVpcAttachment"
    "fixture/GetVpcAttachment.yaml"

requestListAttachments :: ListAttachments -> TestTree
requestListAttachments =
  req
    "ListAttachments"
    "fixture/ListAttachments.yaml"

requestListConnectPeers :: ListConnectPeers -> TestTree
requestListConnectPeers =
  req
    "ListConnectPeers"
    "fixture/ListConnectPeers.yaml"

requestListCoreNetworkPolicyVersions :: ListCoreNetworkPolicyVersions -> TestTree
requestListCoreNetworkPolicyVersions =
  req
    "ListCoreNetworkPolicyVersions"
    "fixture/ListCoreNetworkPolicyVersions.yaml"

requestListCoreNetworks :: ListCoreNetworks -> TestTree
requestListCoreNetworks =
  req
    "ListCoreNetworks"
    "fixture/ListCoreNetworks.yaml"

requestListOrganizationServiceAccessStatus :: ListOrganizationServiceAccessStatus -> TestTree
requestListOrganizationServiceAccessStatus =
  req
    "ListOrganizationServiceAccessStatus"
    "fixture/ListOrganizationServiceAccessStatus.yaml"

requestListPeerings :: ListPeerings -> TestTree
requestListPeerings =
  req
    "ListPeerings"
    "fixture/ListPeerings.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutCoreNetworkPolicy :: PutCoreNetworkPolicy -> TestTree
requestPutCoreNetworkPolicy =
  req
    "PutCoreNetworkPolicy"
    "fixture/PutCoreNetworkPolicy.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestRegisterTransitGateway :: RegisterTransitGateway -> TestTree
requestRegisterTransitGateway =
  req
    "RegisterTransitGateway"
    "fixture/RegisterTransitGateway.yaml"

requestRejectAttachment :: RejectAttachment -> TestTree
requestRejectAttachment =
  req
    "RejectAttachment"
    "fixture/RejectAttachment.yaml"

requestRestoreCoreNetworkPolicyVersion :: RestoreCoreNetworkPolicyVersion -> TestTree
requestRestoreCoreNetworkPolicyVersion =
  req
    "RestoreCoreNetworkPolicyVersion"
    "fixture/RestoreCoreNetworkPolicyVersion.yaml"

requestStartOrganizationServiceAccessUpdate :: StartOrganizationServiceAccessUpdate -> TestTree
requestStartOrganizationServiceAccessUpdate =
  req
    "StartOrganizationServiceAccessUpdate"
    "fixture/StartOrganizationServiceAccessUpdate.yaml"

requestStartRouteAnalysis :: StartRouteAnalysis -> TestTree
requestStartRouteAnalysis =
  req
    "StartRouteAnalysis"
    "fixture/StartRouteAnalysis.yaml"

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

requestUpdateCoreNetwork :: UpdateCoreNetwork -> TestTree
requestUpdateCoreNetwork =
  req
    "UpdateCoreNetwork"
    "fixture/UpdateCoreNetwork.yaml"

requestUpdateDevice :: UpdateDevice -> TestTree
requestUpdateDevice =
  req
    "UpdateDevice"
    "fixture/UpdateDevice.yaml"

requestUpdateGlobalNetwork :: UpdateGlobalNetwork -> TestTree
requestUpdateGlobalNetwork =
  req
    "UpdateGlobalNetwork"
    "fixture/UpdateGlobalNetwork.yaml"

requestUpdateLink :: UpdateLink -> TestTree
requestUpdateLink =
  req
    "UpdateLink"
    "fixture/UpdateLink.yaml"

requestUpdateNetworkResourceMetadata :: UpdateNetworkResourceMetadata -> TestTree
requestUpdateNetworkResourceMetadata =
  req
    "UpdateNetworkResourceMetadata"
    "fixture/UpdateNetworkResourceMetadata.yaml"

requestUpdateSite :: UpdateSite -> TestTree
requestUpdateSite =
  req
    "UpdateSite"
    "fixture/UpdateSite.yaml"

requestUpdateVpcAttachment :: UpdateVpcAttachment -> TestTree
requestUpdateVpcAttachment =
  req
    "UpdateVpcAttachment"
    "fixture/UpdateVpcAttachment.yaml"

-- Responses

responseAcceptAttachment :: AcceptAttachmentResponse -> TestTree
responseAcceptAttachment =
  res
    "AcceptAttachmentResponse"
    "fixture/AcceptAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptAttachment)

responseAssociateConnectPeer :: AssociateConnectPeerResponse -> TestTree
responseAssociateConnectPeer =
  res
    "AssociateConnectPeerResponse"
    "fixture/AssociateConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateConnectPeer)

responseAssociateCustomerGateway :: AssociateCustomerGatewayResponse -> TestTree
responseAssociateCustomerGateway =
  res
    "AssociateCustomerGatewayResponse"
    "fixture/AssociateCustomerGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateCustomerGateway)

responseAssociateLink :: AssociateLinkResponse -> TestTree
responseAssociateLink =
  res
    "AssociateLinkResponse"
    "fixture/AssociateLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateLink)

responseAssociateTransitGatewayConnectPeer :: AssociateTransitGatewayConnectPeerResponse -> TestTree
responseAssociateTransitGatewayConnectPeer =
  res
    "AssociateTransitGatewayConnectPeerResponse"
    "fixture/AssociateTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTransitGatewayConnectPeer)

responseCreateConnectAttachment :: CreateConnectAttachmentResponse -> TestTree
responseCreateConnectAttachment =
  res
    "CreateConnectAttachmentResponse"
    "fixture/CreateConnectAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnectAttachment)

responseCreateConnectPeer :: CreateConnectPeerResponse -> TestTree
responseCreateConnectPeer =
  res
    "CreateConnectPeerResponse"
    "fixture/CreateConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnectPeer)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseCreateCoreNetwork :: CreateCoreNetworkResponse -> TestTree
responseCreateCoreNetwork =
  res
    "CreateCoreNetworkResponse"
    "fixture/CreateCoreNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCoreNetwork)

responseCreateDevice :: CreateDeviceResponse -> TestTree
responseCreateDevice =
  res
    "CreateDeviceResponse"
    "fixture/CreateDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDevice)

responseCreateGlobalNetwork :: CreateGlobalNetworkResponse -> TestTree
responseCreateGlobalNetwork =
  res
    "CreateGlobalNetworkResponse"
    "fixture/CreateGlobalNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGlobalNetwork)

responseCreateLink :: CreateLinkResponse -> TestTree
responseCreateLink =
  res
    "CreateLinkResponse"
    "fixture/CreateLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLink)

responseCreateSite :: CreateSiteResponse -> TestTree
responseCreateSite =
  res
    "CreateSiteResponse"
    "fixture/CreateSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSite)

responseCreateSiteToSiteVpnAttachment :: CreateSiteToSiteVpnAttachmentResponse -> TestTree
responseCreateSiteToSiteVpnAttachment =
  res
    "CreateSiteToSiteVpnAttachmentResponse"
    "fixture/CreateSiteToSiteVpnAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSiteToSiteVpnAttachment)

responseCreateTransitGatewayPeering :: CreateTransitGatewayPeeringResponse -> TestTree
responseCreateTransitGatewayPeering =
  res
    "CreateTransitGatewayPeeringResponse"
    "fixture/CreateTransitGatewayPeeringResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayPeering)

responseCreateTransitGatewayRouteTableAttachment :: CreateTransitGatewayRouteTableAttachmentResponse -> TestTree
responseCreateTransitGatewayRouteTableAttachment =
  res
    "CreateTransitGatewayRouteTableAttachmentResponse"
    "fixture/CreateTransitGatewayRouteTableAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransitGatewayRouteTableAttachment)

responseCreateVpcAttachment :: CreateVpcAttachmentResponse -> TestTree
responseCreateVpcAttachment =
  res
    "CreateVpcAttachmentResponse"
    "fixture/CreateVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcAttachment)

responseDeleteAttachment :: DeleteAttachmentResponse -> TestTree
responseDeleteAttachment =
  res
    "DeleteAttachmentResponse"
    "fixture/DeleteAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAttachment)

responseDeleteConnectPeer :: DeleteConnectPeerResponse -> TestTree
responseDeleteConnectPeer =
  res
    "DeleteConnectPeerResponse"
    "fixture/DeleteConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnectPeer)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseDeleteCoreNetwork :: DeleteCoreNetworkResponse -> TestTree
responseDeleteCoreNetwork =
  res
    "DeleteCoreNetworkResponse"
    "fixture/DeleteCoreNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCoreNetwork)

responseDeleteCoreNetworkPolicyVersion :: DeleteCoreNetworkPolicyVersionResponse -> TestTree
responseDeleteCoreNetworkPolicyVersion =
  res
    "DeleteCoreNetworkPolicyVersionResponse"
    "fixture/DeleteCoreNetworkPolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCoreNetworkPolicyVersion)

responseDeleteDevice :: DeleteDeviceResponse -> TestTree
responseDeleteDevice =
  res
    "DeleteDeviceResponse"
    "fixture/DeleteDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDevice)

responseDeleteGlobalNetwork :: DeleteGlobalNetworkResponse -> TestTree
responseDeleteGlobalNetwork =
  res
    "DeleteGlobalNetworkResponse"
    "fixture/DeleteGlobalNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGlobalNetwork)

responseDeleteLink :: DeleteLinkResponse -> TestTree
responseDeleteLink =
  res
    "DeleteLinkResponse"
    "fixture/DeleteLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLink)

responseDeletePeering :: DeletePeeringResponse -> TestTree
responseDeletePeering =
  res
    "DeletePeeringResponse"
    "fixture/DeletePeeringResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePeering)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteSite :: DeleteSiteResponse -> TestTree
responseDeleteSite =
  res
    "DeleteSiteResponse"
    "fixture/DeleteSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSite)

responseDeregisterTransitGateway :: DeregisterTransitGatewayResponse -> TestTree
responseDeregisterTransitGateway =
  res
    "DeregisterTransitGatewayResponse"
    "fixture/DeregisterTransitGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTransitGateway)

responseDescribeGlobalNetworks :: DescribeGlobalNetworksResponse -> TestTree
responseDescribeGlobalNetworks =
  res
    "DescribeGlobalNetworksResponse"
    "fixture/DescribeGlobalNetworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalNetworks)

responseDisassociateConnectPeer :: DisassociateConnectPeerResponse -> TestTree
responseDisassociateConnectPeer =
  res
    "DisassociateConnectPeerResponse"
    "fixture/DisassociateConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateConnectPeer)

responseDisassociateCustomerGateway :: DisassociateCustomerGatewayResponse -> TestTree
responseDisassociateCustomerGateway =
  res
    "DisassociateCustomerGatewayResponse"
    "fixture/DisassociateCustomerGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateCustomerGateway)

responseDisassociateLink :: DisassociateLinkResponse -> TestTree
responseDisassociateLink =
  res
    "DisassociateLinkResponse"
    "fixture/DisassociateLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateLink)

responseDisassociateTransitGatewayConnectPeer :: DisassociateTransitGatewayConnectPeerResponse -> TestTree
responseDisassociateTransitGatewayConnectPeer =
  res
    "DisassociateTransitGatewayConnectPeerResponse"
    "fixture/DisassociateTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTransitGatewayConnectPeer)

responseExecuteCoreNetworkChangeSet :: ExecuteCoreNetworkChangeSetResponse -> TestTree
responseExecuteCoreNetworkChangeSet =
  res
    "ExecuteCoreNetworkChangeSetResponse"
    "fixture/ExecuteCoreNetworkChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteCoreNetworkChangeSet)

responseGetConnectAttachment :: GetConnectAttachmentResponse -> TestTree
responseGetConnectAttachment =
  res
    "GetConnectAttachmentResponse"
    "fixture/GetConnectAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectAttachment)

responseGetConnectPeer :: GetConnectPeerResponse -> TestTree
responseGetConnectPeer =
  res
    "GetConnectPeerResponse"
    "fixture/GetConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectPeer)

responseGetConnectPeerAssociations :: GetConnectPeerAssociationsResponse -> TestTree
responseGetConnectPeerAssociations =
  res
    "GetConnectPeerAssociationsResponse"
    "fixture/GetConnectPeerAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectPeerAssociations)

responseGetConnections :: GetConnectionsResponse -> TestTree
responseGetConnections =
  res
    "GetConnectionsResponse"
    "fixture/GetConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnections)

responseGetCoreNetwork :: GetCoreNetworkResponse -> TestTree
responseGetCoreNetwork =
  res
    "GetCoreNetworkResponse"
    "fixture/GetCoreNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoreNetwork)

responseGetCoreNetworkChangeEvents :: GetCoreNetworkChangeEventsResponse -> TestTree
responseGetCoreNetworkChangeEvents =
  res
    "GetCoreNetworkChangeEventsResponse"
    "fixture/GetCoreNetworkChangeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoreNetworkChangeEvents)

responseGetCoreNetworkChangeSet :: GetCoreNetworkChangeSetResponse -> TestTree
responseGetCoreNetworkChangeSet =
  res
    "GetCoreNetworkChangeSetResponse"
    "fixture/GetCoreNetworkChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoreNetworkChangeSet)

responseGetCoreNetworkPolicy :: GetCoreNetworkPolicyResponse -> TestTree
responseGetCoreNetworkPolicy =
  res
    "GetCoreNetworkPolicyResponse"
    "fixture/GetCoreNetworkPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoreNetworkPolicy)

responseGetCustomerGatewayAssociations :: GetCustomerGatewayAssociationsResponse -> TestTree
responseGetCustomerGatewayAssociations =
  res
    "GetCustomerGatewayAssociationsResponse"
    "fixture/GetCustomerGatewayAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCustomerGatewayAssociations)

responseGetDevices :: GetDevicesResponse -> TestTree
responseGetDevices =
  res
    "GetDevicesResponse"
    "fixture/GetDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevices)

responseGetLinkAssociations :: GetLinkAssociationsResponse -> TestTree
responseGetLinkAssociations =
  res
    "GetLinkAssociationsResponse"
    "fixture/GetLinkAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLinkAssociations)

responseGetLinks :: GetLinksResponse -> TestTree
responseGetLinks =
  res
    "GetLinksResponse"
    "fixture/GetLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLinks)

responseGetNetworkResourceCounts :: GetNetworkResourceCountsResponse -> TestTree
responseGetNetworkResourceCounts =
  res
    "GetNetworkResourceCountsResponse"
    "fixture/GetNetworkResourceCountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkResourceCounts)

responseGetNetworkResourceRelationships :: GetNetworkResourceRelationshipsResponse -> TestTree
responseGetNetworkResourceRelationships =
  res
    "GetNetworkResourceRelationshipsResponse"
    "fixture/GetNetworkResourceRelationshipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkResourceRelationships)

responseGetNetworkResources :: GetNetworkResourcesResponse -> TestTree
responseGetNetworkResources =
  res
    "GetNetworkResourcesResponse"
    "fixture/GetNetworkResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkResources)

responseGetNetworkRoutes :: GetNetworkRoutesResponse -> TestTree
responseGetNetworkRoutes =
  res
    "GetNetworkRoutesResponse"
    "fixture/GetNetworkRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkRoutes)

responseGetNetworkTelemetry :: GetNetworkTelemetryResponse -> TestTree
responseGetNetworkTelemetry =
  res
    "GetNetworkTelemetryResponse"
    "fixture/GetNetworkTelemetryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkTelemetry)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responseGetRouteAnalysis :: GetRouteAnalysisResponse -> TestTree
responseGetRouteAnalysis =
  res
    "GetRouteAnalysisResponse"
    "fixture/GetRouteAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRouteAnalysis)

responseGetSiteToSiteVpnAttachment :: GetSiteToSiteVpnAttachmentResponse -> TestTree
responseGetSiteToSiteVpnAttachment =
  res
    "GetSiteToSiteVpnAttachmentResponse"
    "fixture/GetSiteToSiteVpnAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSiteToSiteVpnAttachment)

responseGetSites :: GetSitesResponse -> TestTree
responseGetSites =
  res
    "GetSitesResponse"
    "fixture/GetSitesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSites)

responseGetTransitGatewayConnectPeerAssociations :: GetTransitGatewayConnectPeerAssociationsResponse -> TestTree
responseGetTransitGatewayConnectPeerAssociations =
  res
    "GetTransitGatewayConnectPeerAssociationsResponse"
    "fixture/GetTransitGatewayConnectPeerAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayConnectPeerAssociations)

responseGetTransitGatewayPeering :: GetTransitGatewayPeeringResponse -> TestTree
responseGetTransitGatewayPeering =
  res
    "GetTransitGatewayPeeringResponse"
    "fixture/GetTransitGatewayPeeringResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayPeering)

responseGetTransitGatewayRegistrations :: GetTransitGatewayRegistrationsResponse -> TestTree
responseGetTransitGatewayRegistrations =
  res
    "GetTransitGatewayRegistrationsResponse"
    "fixture/GetTransitGatewayRegistrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayRegistrations)

responseGetTransitGatewayRouteTableAttachment :: GetTransitGatewayRouteTableAttachmentResponse -> TestTree
responseGetTransitGatewayRouteTableAttachment =
  res
    "GetTransitGatewayRouteTableAttachmentResponse"
    "fixture/GetTransitGatewayRouteTableAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayRouteTableAttachment)

responseGetVpcAttachment :: GetVpcAttachmentResponse -> TestTree
responseGetVpcAttachment =
  res
    "GetVpcAttachmentResponse"
    "fixture/GetVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpcAttachment)

responseListAttachments :: ListAttachmentsResponse -> TestTree
responseListAttachments =
  res
    "ListAttachmentsResponse"
    "fixture/ListAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttachments)

responseListConnectPeers :: ListConnectPeersResponse -> TestTree
responseListConnectPeers =
  res
    "ListConnectPeersResponse"
    "fixture/ListConnectPeersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnectPeers)

responseListCoreNetworkPolicyVersions :: ListCoreNetworkPolicyVersionsResponse -> TestTree
responseListCoreNetworkPolicyVersions =
  res
    "ListCoreNetworkPolicyVersionsResponse"
    "fixture/ListCoreNetworkPolicyVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCoreNetworkPolicyVersions)

responseListCoreNetworks :: ListCoreNetworksResponse -> TestTree
responseListCoreNetworks =
  res
    "ListCoreNetworksResponse"
    "fixture/ListCoreNetworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCoreNetworks)

responseListOrganizationServiceAccessStatus :: ListOrganizationServiceAccessStatusResponse -> TestTree
responseListOrganizationServiceAccessStatus =
  res
    "ListOrganizationServiceAccessStatusResponse"
    "fixture/ListOrganizationServiceAccessStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationServiceAccessStatus)

responseListPeerings :: ListPeeringsResponse -> TestTree
responseListPeerings =
  res
    "ListPeeringsResponse"
    "fixture/ListPeeringsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPeerings)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutCoreNetworkPolicy :: PutCoreNetworkPolicyResponse -> TestTree
responsePutCoreNetworkPolicy =
  res
    "PutCoreNetworkPolicyResponse"
    "fixture/PutCoreNetworkPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutCoreNetworkPolicy)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseRegisterTransitGateway :: RegisterTransitGatewayResponse -> TestTree
responseRegisterTransitGateway =
  res
    "RegisterTransitGatewayResponse"
    "fixture/RegisterTransitGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTransitGateway)

responseRejectAttachment :: RejectAttachmentResponse -> TestTree
responseRejectAttachment =
  res
    "RejectAttachmentResponse"
    "fixture/RejectAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectAttachment)

responseRestoreCoreNetworkPolicyVersion :: RestoreCoreNetworkPolicyVersionResponse -> TestTree
responseRestoreCoreNetworkPolicyVersion =
  res
    "RestoreCoreNetworkPolicyVersionResponse"
    "fixture/RestoreCoreNetworkPolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreCoreNetworkPolicyVersion)

responseStartOrganizationServiceAccessUpdate :: StartOrganizationServiceAccessUpdateResponse -> TestTree
responseStartOrganizationServiceAccessUpdate =
  res
    "StartOrganizationServiceAccessUpdateResponse"
    "fixture/StartOrganizationServiceAccessUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartOrganizationServiceAccessUpdate)

responseStartRouteAnalysis :: StartRouteAnalysisResponse -> TestTree
responseStartRouteAnalysis =
  res
    "StartRouteAnalysisResponse"
    "fixture/StartRouteAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRouteAnalysis)

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

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnection)

responseUpdateCoreNetwork :: UpdateCoreNetworkResponse -> TestTree
responseUpdateCoreNetwork =
  res
    "UpdateCoreNetworkResponse"
    "fixture/UpdateCoreNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCoreNetwork)

responseUpdateDevice :: UpdateDeviceResponse -> TestTree
responseUpdateDevice =
  res
    "UpdateDeviceResponse"
    "fixture/UpdateDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevice)

responseUpdateGlobalNetwork :: UpdateGlobalNetworkResponse -> TestTree
responseUpdateGlobalNetwork =
  res
    "UpdateGlobalNetworkResponse"
    "fixture/UpdateGlobalNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalNetwork)

responseUpdateLink :: UpdateLinkResponse -> TestTree
responseUpdateLink =
  res
    "UpdateLinkResponse"
    "fixture/UpdateLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLink)

responseUpdateNetworkResourceMetadata :: UpdateNetworkResourceMetadataResponse -> TestTree
responseUpdateNetworkResourceMetadata =
  res
    "UpdateNetworkResourceMetadataResponse"
    "fixture/UpdateNetworkResourceMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNetworkResourceMetadata)

responseUpdateSite :: UpdateSiteResponse -> TestTree
responseUpdateSite =
  res
    "UpdateSiteResponse"
    "fixture/UpdateSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSite)

responseUpdateVpcAttachment :: UpdateVpcAttachmentResponse -> TestTree
responseUpdateVpcAttachment =
  res
    "UpdateVpcAttachmentResponse"
    "fixture/UpdateVpcAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVpcAttachment)

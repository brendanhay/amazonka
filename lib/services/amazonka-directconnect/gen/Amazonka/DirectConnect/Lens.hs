{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DirectConnect.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Lens
  ( -- * Operations

    -- ** AcceptDirectConnectGatewayAssociationProposal
    acceptDirectConnectGatewayAssociationProposal_overrideAllowedPrefixesToDirectConnectGateway,
    acceptDirectConnectGatewayAssociationProposal_directConnectGatewayId,
    acceptDirectConnectGatewayAssociationProposal_proposalId,
    acceptDirectConnectGatewayAssociationProposal_associatedGatewayOwnerAccount,
    acceptDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociation,
    acceptDirectConnectGatewayAssociationProposalResponse_httpStatus,

    -- ** AllocateHostedConnection
    allocateHostedConnection_tags,
    allocateHostedConnection_connectionId,
    allocateHostedConnection_ownerAccount,
    allocateHostedConnection_bandwidth,
    allocateHostedConnection_connectionName,
    allocateHostedConnection_vlan,
    connection_tags,
    connection_macSecKeys,
    connection_macSecCapable,
    connection_providerName,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_connectionState,
    connection_hasLogicalRedundancy,
    connection_vlan,
    connection_loaIssueTime,
    connection_awsDevice,
    connection_connectionId,
    connection_location,
    connection_region,
    connection_partnerName,
    connection_ownerAccount,
    connection_awsLogicalDeviceId,
    connection_encryptionMode,
    connection_connectionName,
    connection_awsDeviceV2,

    -- ** AllocatePrivateVirtualInterface
    allocatePrivateVirtualInterface_connectionId,
    allocatePrivateVirtualInterface_ownerAccount,
    allocatePrivateVirtualInterface_newPrivateVirtualInterfaceAllocation,
    virtualInterface_tags,
    virtualInterface_addressFamily,
    virtualInterface_authKey,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceType,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualGatewayId,
    virtualInterface_vlan,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_virtualInterfaceState,
    virtualInterface_asn,
    virtualInterface_location,
    virtualInterface_region,
    virtualInterface_amazonAddress,
    virtualInterface_virtualInterfaceId,
    virtualInterface_bgpPeers,
    virtualInterface_customerRouterConfig,
    virtualInterface_amazonSideAsn,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,

    -- ** AllocatePublicVirtualInterface
    allocatePublicVirtualInterface_connectionId,
    allocatePublicVirtualInterface_ownerAccount,
    allocatePublicVirtualInterface_newPublicVirtualInterfaceAllocation,
    virtualInterface_tags,
    virtualInterface_addressFamily,
    virtualInterface_authKey,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceType,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualGatewayId,
    virtualInterface_vlan,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_virtualInterfaceState,
    virtualInterface_asn,
    virtualInterface_location,
    virtualInterface_region,
    virtualInterface_amazonAddress,
    virtualInterface_virtualInterfaceId,
    virtualInterface_bgpPeers,
    virtualInterface_customerRouterConfig,
    virtualInterface_amazonSideAsn,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,

    -- ** AllocateTransitVirtualInterface
    allocateTransitVirtualInterface_connectionId,
    allocateTransitVirtualInterface_ownerAccount,
    allocateTransitVirtualInterface_newTransitVirtualInterfaceAllocation,
    allocateTransitVirtualInterfaceResponse_virtualInterface,
    allocateTransitVirtualInterfaceResponse_httpStatus,

    -- ** AssociateConnectionWithLag
    associateConnectionWithLag_connectionId,
    associateConnectionWithLag_lagId,
    connection_tags,
    connection_macSecKeys,
    connection_macSecCapable,
    connection_providerName,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_connectionState,
    connection_hasLogicalRedundancy,
    connection_vlan,
    connection_loaIssueTime,
    connection_awsDevice,
    connection_connectionId,
    connection_location,
    connection_region,
    connection_partnerName,
    connection_ownerAccount,
    connection_awsLogicalDeviceId,
    connection_encryptionMode,
    connection_connectionName,
    connection_awsDeviceV2,

    -- ** AssociateHostedConnection
    associateHostedConnection_connectionId,
    associateHostedConnection_parentConnectionId,
    connection_tags,
    connection_macSecKeys,
    connection_macSecCapable,
    connection_providerName,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_connectionState,
    connection_hasLogicalRedundancy,
    connection_vlan,
    connection_loaIssueTime,
    connection_awsDevice,
    connection_connectionId,
    connection_location,
    connection_region,
    connection_partnerName,
    connection_ownerAccount,
    connection_awsLogicalDeviceId,
    connection_encryptionMode,
    connection_connectionName,
    connection_awsDeviceV2,

    -- ** AssociateMacSecKey
    associateMacSecKey_cak,
    associateMacSecKey_secretARN,
    associateMacSecKey_ckn,
    associateMacSecKey_connectionId,
    associateMacSecKeyResponse_macSecKeys,
    associateMacSecKeyResponse_connectionId,
    associateMacSecKeyResponse_httpStatus,

    -- ** AssociateVirtualInterface
    associateVirtualInterface_virtualInterfaceId,
    associateVirtualInterface_connectionId,
    virtualInterface_tags,
    virtualInterface_addressFamily,
    virtualInterface_authKey,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceType,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualGatewayId,
    virtualInterface_vlan,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_virtualInterfaceState,
    virtualInterface_asn,
    virtualInterface_location,
    virtualInterface_region,
    virtualInterface_amazonAddress,
    virtualInterface_virtualInterfaceId,
    virtualInterface_bgpPeers,
    virtualInterface_customerRouterConfig,
    virtualInterface_amazonSideAsn,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,

    -- ** ConfirmConnection
    confirmConnection_connectionId,
    confirmConnectionResponse_connectionState,
    confirmConnectionResponse_httpStatus,

    -- ** ConfirmCustomerAgreement
    confirmCustomerAgreement_agreementName,
    confirmCustomerAgreementResponse_status,
    confirmCustomerAgreementResponse_httpStatus,

    -- ** ConfirmPrivateVirtualInterface
    confirmPrivateVirtualInterface_directConnectGatewayId,
    confirmPrivateVirtualInterface_virtualGatewayId,
    confirmPrivateVirtualInterface_virtualInterfaceId,
    confirmPrivateVirtualInterfaceResponse_virtualInterfaceState,
    confirmPrivateVirtualInterfaceResponse_httpStatus,

    -- ** ConfirmPublicVirtualInterface
    confirmPublicVirtualInterface_virtualInterfaceId,
    confirmPublicVirtualInterfaceResponse_virtualInterfaceState,
    confirmPublicVirtualInterfaceResponse_httpStatus,

    -- ** ConfirmTransitVirtualInterface
    confirmTransitVirtualInterface_virtualInterfaceId,
    confirmTransitVirtualInterface_directConnectGatewayId,
    confirmTransitVirtualInterfaceResponse_virtualInterfaceState,
    confirmTransitVirtualInterfaceResponse_httpStatus,

    -- ** CreateBGPPeer
    createBGPPeer_newBGPPeer,
    createBGPPeer_virtualInterfaceId,
    createBGPPeerResponse_virtualInterface,
    createBGPPeerResponse_httpStatus,

    -- ** CreateConnection
    createConnection_tags,
    createConnection_providerName,
    createConnection_lagId,
    createConnection_requestMACSec,
    createConnection_location,
    createConnection_bandwidth,
    createConnection_connectionName,
    connection_tags,
    connection_macSecKeys,
    connection_macSecCapable,
    connection_providerName,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_connectionState,
    connection_hasLogicalRedundancy,
    connection_vlan,
    connection_loaIssueTime,
    connection_awsDevice,
    connection_connectionId,
    connection_location,
    connection_region,
    connection_partnerName,
    connection_ownerAccount,
    connection_awsLogicalDeviceId,
    connection_encryptionMode,
    connection_connectionName,
    connection_awsDeviceV2,

    -- ** CreateDirectConnectGateway
    createDirectConnectGateway_amazonSideAsn,
    createDirectConnectGateway_directConnectGatewayName,
    createDirectConnectGatewayResponse_directConnectGateway,
    createDirectConnectGatewayResponse_httpStatus,

    -- ** CreateDirectConnectGatewayAssociation
    createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociation_virtualGatewayId,
    createDirectConnectGatewayAssociation_gatewayId,
    createDirectConnectGatewayAssociation_directConnectGatewayId,
    createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    createDirectConnectGatewayAssociationResponse_httpStatus,

    -- ** CreateDirectConnectGatewayAssociationProposal
    createDirectConnectGatewayAssociationProposal_addAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociationProposal_removeAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociationProposal_directConnectGatewayId,
    createDirectConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,
    createDirectConnectGatewayAssociationProposal_gatewayId,
    createDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal,
    createDirectConnectGatewayAssociationProposalResponse_httpStatus,

    -- ** CreateInterconnect
    createInterconnect_tags,
    createInterconnect_providerName,
    createInterconnect_lagId,
    createInterconnect_interconnectName,
    createInterconnect_bandwidth,
    createInterconnect_location,
    interconnect_tags,
    interconnect_providerName,
    interconnect_bandwidth,
    interconnect_interconnectName,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_hasLogicalRedundancy,
    interconnect_loaIssueTime,
    interconnect_interconnectState,
    interconnect_awsDevice,
    interconnect_location,
    interconnect_region,
    interconnect_interconnectId,
    interconnect_awsLogicalDeviceId,
    interconnect_awsDeviceV2,

    -- ** CreateLag
    createLag_tags,
    createLag_providerName,
    createLag_childConnectionTags,
    createLag_connectionId,
    createLag_requestMACSec,
    createLag_numberOfConnections,
    createLag_location,
    createLag_connectionsBandwidth,
    createLag_lagName,
    lag_tags,
    lag_numberOfConnections,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_macSecCapable,
    lag_providerName,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_hasLogicalRedundancy,
    lag_awsDevice,
    lag_lagName,
    lag_location,
    lag_region,
    lag_allowsHostedConnections,
    lag_connections,
    lag_connectionsBandwidth,
    lag_ownerAccount,
    lag_awsLogicalDeviceId,
    lag_encryptionMode,
    lag_awsDeviceV2,

    -- ** CreatePrivateVirtualInterface
    createPrivateVirtualInterface_connectionId,
    createPrivateVirtualInterface_newPrivateVirtualInterface,
    virtualInterface_tags,
    virtualInterface_addressFamily,
    virtualInterface_authKey,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceType,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualGatewayId,
    virtualInterface_vlan,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_virtualInterfaceState,
    virtualInterface_asn,
    virtualInterface_location,
    virtualInterface_region,
    virtualInterface_amazonAddress,
    virtualInterface_virtualInterfaceId,
    virtualInterface_bgpPeers,
    virtualInterface_customerRouterConfig,
    virtualInterface_amazonSideAsn,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,

    -- ** CreatePublicVirtualInterface
    createPublicVirtualInterface_connectionId,
    createPublicVirtualInterface_newPublicVirtualInterface,
    virtualInterface_tags,
    virtualInterface_addressFamily,
    virtualInterface_authKey,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceType,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualGatewayId,
    virtualInterface_vlan,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_virtualInterfaceState,
    virtualInterface_asn,
    virtualInterface_location,
    virtualInterface_region,
    virtualInterface_amazonAddress,
    virtualInterface_virtualInterfaceId,
    virtualInterface_bgpPeers,
    virtualInterface_customerRouterConfig,
    virtualInterface_amazonSideAsn,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,

    -- ** CreateTransitVirtualInterface
    createTransitVirtualInterface_connectionId,
    createTransitVirtualInterface_newTransitVirtualInterface,
    createTransitVirtualInterfaceResponse_virtualInterface,
    createTransitVirtualInterfaceResponse_httpStatus,

    -- ** DeleteBGPPeer
    deleteBGPPeer_bgpPeerId,
    deleteBGPPeer_customerAddress,
    deleteBGPPeer_asn,
    deleteBGPPeer_virtualInterfaceId,
    deleteBGPPeerResponse_virtualInterface,
    deleteBGPPeerResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_connectionId,
    connection_tags,
    connection_macSecKeys,
    connection_macSecCapable,
    connection_providerName,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_connectionState,
    connection_hasLogicalRedundancy,
    connection_vlan,
    connection_loaIssueTime,
    connection_awsDevice,
    connection_connectionId,
    connection_location,
    connection_region,
    connection_partnerName,
    connection_ownerAccount,
    connection_awsLogicalDeviceId,
    connection_encryptionMode,
    connection_connectionName,
    connection_awsDeviceV2,

    -- ** DeleteDirectConnectGateway
    deleteDirectConnectGateway_directConnectGatewayId,
    deleteDirectConnectGatewayResponse_directConnectGateway,
    deleteDirectConnectGatewayResponse_httpStatus,

    -- ** DeleteDirectConnectGatewayAssociation
    deleteDirectConnectGatewayAssociation_directConnectGatewayId,
    deleteDirectConnectGatewayAssociation_virtualGatewayId,
    deleteDirectConnectGatewayAssociation_associationId,
    deleteDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    deleteDirectConnectGatewayAssociationResponse_httpStatus,

    -- ** DeleteDirectConnectGatewayAssociationProposal
    deleteDirectConnectGatewayAssociationProposal_proposalId,
    deleteDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal,
    deleteDirectConnectGatewayAssociationProposalResponse_httpStatus,

    -- ** DeleteInterconnect
    deleteInterconnect_interconnectId,
    deleteInterconnectResponse_interconnectState,
    deleteInterconnectResponse_httpStatus,

    -- ** DeleteLag
    deleteLag_lagId,
    lag_tags,
    lag_numberOfConnections,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_macSecCapable,
    lag_providerName,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_hasLogicalRedundancy,
    lag_awsDevice,
    lag_lagName,
    lag_location,
    lag_region,
    lag_allowsHostedConnections,
    lag_connections,
    lag_connectionsBandwidth,
    lag_ownerAccount,
    lag_awsLogicalDeviceId,
    lag_encryptionMode,
    lag_awsDeviceV2,

    -- ** DeleteVirtualInterface
    deleteVirtualInterface_virtualInterfaceId,
    deleteVirtualInterfaceResponse_virtualInterfaceState,
    deleteVirtualInterfaceResponse_httpStatus,

    -- ** DescribeConnections
    describeConnections_connectionId,
    connections_connections,

    -- ** DescribeCustomerMetadata
    describeCustomerMetadataResponse_agreements,
    describeCustomerMetadataResponse_nniPartnerType,
    describeCustomerMetadataResponse_httpStatus,

    -- ** DescribeDirectConnectGatewayAssociationProposals
    describeDirectConnectGatewayAssociationProposals_proposalId,
    describeDirectConnectGatewayAssociationProposals_nextToken,
    describeDirectConnectGatewayAssociationProposals_directConnectGatewayId,
    describeDirectConnectGatewayAssociationProposals_maxResults,
    describeDirectConnectGatewayAssociationProposals_associatedGatewayId,
    describeDirectConnectGatewayAssociationProposalsResponse_nextToken,
    describeDirectConnectGatewayAssociationProposalsResponse_directConnectGatewayAssociationProposals,
    describeDirectConnectGatewayAssociationProposalsResponse_httpStatus,

    -- ** DescribeDirectConnectGatewayAssociations
    describeDirectConnectGatewayAssociations_nextToken,
    describeDirectConnectGatewayAssociations_directConnectGatewayId,
    describeDirectConnectGatewayAssociations_virtualGatewayId,
    describeDirectConnectGatewayAssociations_maxResults,
    describeDirectConnectGatewayAssociations_associatedGatewayId,
    describeDirectConnectGatewayAssociations_associationId,
    describeDirectConnectGatewayAssociationsResponse_nextToken,
    describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations,
    describeDirectConnectGatewayAssociationsResponse_httpStatus,

    -- ** DescribeDirectConnectGatewayAttachments
    describeDirectConnectGatewayAttachments_nextToken,
    describeDirectConnectGatewayAttachments_directConnectGatewayId,
    describeDirectConnectGatewayAttachments_virtualInterfaceId,
    describeDirectConnectGatewayAttachments_maxResults,
    describeDirectConnectGatewayAttachmentsResponse_nextToken,
    describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments,
    describeDirectConnectGatewayAttachmentsResponse_httpStatus,

    -- ** DescribeDirectConnectGateways
    describeDirectConnectGateways_nextToken,
    describeDirectConnectGateways_directConnectGatewayId,
    describeDirectConnectGateways_maxResults,
    describeDirectConnectGatewaysResponse_nextToken,
    describeDirectConnectGatewaysResponse_directConnectGateways,
    describeDirectConnectGatewaysResponse_httpStatus,

    -- ** DescribeHostedConnections
    describeHostedConnections_connectionId,
    connections_connections,

    -- ** DescribeInterconnects
    describeInterconnects_interconnectId,
    describeInterconnectsResponse_interconnects,
    describeInterconnectsResponse_httpStatus,

    -- ** DescribeLags
    describeLags_lagId,
    describeLagsResponse_lags,
    describeLagsResponse_httpStatus,

    -- ** DescribeLoa
    describeLoa_providerName,
    describeLoa_loaContentType,
    describeLoa_connectionId,
    describeLoaResponse_loaContent,
    describeLoaResponse_loaContentType,
    describeLoaResponse_httpStatus,

    -- ** DescribeLocations
    describeLocationsResponse_locations,
    describeLocationsResponse_httpStatus,

    -- ** DescribeRouterConfiguration
    describeRouterConfiguration_routerTypeIdentifier,
    describeRouterConfiguration_virtualInterfaceId,
    describeRouterConfigurationResponse_router,
    describeRouterConfigurationResponse_virtualInterfaceId,
    describeRouterConfigurationResponse_customerRouterConfig,
    describeRouterConfigurationResponse_virtualInterfaceName,
    describeRouterConfigurationResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceArns,
    describeTagsResponse_resourceTags,
    describeTagsResponse_httpStatus,

    -- ** DescribeVirtualGateways
    describeVirtualGatewaysResponse_virtualGateways,
    describeVirtualGatewaysResponse_httpStatus,

    -- ** DescribeVirtualInterfaces
    describeVirtualInterfaces_connectionId,
    describeVirtualInterfaces_virtualInterfaceId,
    describeVirtualInterfacesResponse_virtualInterfaces,
    describeVirtualInterfacesResponse_httpStatus,

    -- ** DisassociateConnectionFromLag
    disassociateConnectionFromLag_connectionId,
    disassociateConnectionFromLag_lagId,
    connection_tags,
    connection_macSecKeys,
    connection_macSecCapable,
    connection_providerName,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_connectionState,
    connection_hasLogicalRedundancy,
    connection_vlan,
    connection_loaIssueTime,
    connection_awsDevice,
    connection_connectionId,
    connection_location,
    connection_region,
    connection_partnerName,
    connection_ownerAccount,
    connection_awsLogicalDeviceId,
    connection_encryptionMode,
    connection_connectionName,
    connection_awsDeviceV2,

    -- ** DisassociateMacSecKey
    disassociateMacSecKey_connectionId,
    disassociateMacSecKey_secretARN,
    disassociateMacSecKeyResponse_macSecKeys,
    disassociateMacSecKeyResponse_connectionId,
    disassociateMacSecKeyResponse_httpStatus,

    -- ** ListVirtualInterfaceTestHistory
    listVirtualInterfaceTestHistory_nextToken,
    listVirtualInterfaceTestHistory_status,
    listVirtualInterfaceTestHistory_virtualInterfaceId,
    listVirtualInterfaceTestHistory_bgpPeers,
    listVirtualInterfaceTestHistory_maxResults,
    listVirtualInterfaceTestHistory_testId,
    listVirtualInterfaceTestHistoryResponse_nextToken,
    listVirtualInterfaceTestHistoryResponse_virtualInterfaceTestHistory,
    listVirtualInterfaceTestHistoryResponse_httpStatus,

    -- ** StartBgpFailoverTest
    startBgpFailoverTest_testDurationInMinutes,
    startBgpFailoverTest_bgpPeers,
    startBgpFailoverTest_virtualInterfaceId,
    startBgpFailoverTestResponse_virtualInterfaceTest,
    startBgpFailoverTestResponse_httpStatus,

    -- ** StopBgpFailoverTest
    stopBgpFailoverTest_virtualInterfaceId,
    stopBgpFailoverTestResponse_virtualInterfaceTest,
    stopBgpFailoverTestResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateConnection
    updateConnection_encryptionMode,
    updateConnection_connectionName,
    updateConnection_connectionId,
    connection_tags,
    connection_macSecKeys,
    connection_macSecCapable,
    connection_providerName,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_connectionState,
    connection_hasLogicalRedundancy,
    connection_vlan,
    connection_loaIssueTime,
    connection_awsDevice,
    connection_connectionId,
    connection_location,
    connection_region,
    connection_partnerName,
    connection_ownerAccount,
    connection_awsLogicalDeviceId,
    connection_encryptionMode,
    connection_connectionName,
    connection_awsDeviceV2,

    -- ** UpdateDirectConnectGateway
    updateDirectConnectGateway_directConnectGatewayId,
    updateDirectConnectGateway_newDirectConnectGatewayName,
    updateDirectConnectGatewayResponse_directConnectGateway,
    updateDirectConnectGatewayResponse_httpStatus,

    -- ** UpdateDirectConnectGatewayAssociation
    updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociation_associationId,
    updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    updateDirectConnectGatewayAssociationResponse_httpStatus,

    -- ** UpdateLag
    updateLag_minimumLinks,
    updateLag_lagName,
    updateLag_encryptionMode,
    updateLag_lagId,
    lag_tags,
    lag_numberOfConnections,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_macSecCapable,
    lag_providerName,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_hasLogicalRedundancy,
    lag_awsDevice,
    lag_lagName,
    lag_location,
    lag_region,
    lag_allowsHostedConnections,
    lag_connections,
    lag_connectionsBandwidth,
    lag_ownerAccount,
    lag_awsLogicalDeviceId,
    lag_encryptionMode,
    lag_awsDeviceV2,

    -- ** UpdateVirtualInterfaceAttributes
    updateVirtualInterfaceAttributes_mtu,
    updateVirtualInterfaceAttributes_virtualInterfaceId,
    virtualInterface_tags,
    virtualInterface_addressFamily,
    virtualInterface_authKey,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceType,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualGatewayId,
    virtualInterface_vlan,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_virtualInterfaceState,
    virtualInterface_asn,
    virtualInterface_location,
    virtualInterface_region,
    virtualInterface_amazonAddress,
    virtualInterface_virtualInterfaceId,
    virtualInterface_bgpPeers,
    virtualInterface_customerRouterConfig,
    virtualInterface_amazonSideAsn,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,

    -- * Types

    -- ** AssociatedGateway
    associatedGateway_type,
    associatedGateway_id,
    associatedGateway_region,
    associatedGateway_ownerAccount,

    -- ** BGPPeer
    bGPPeer_bgpPeerId,
    bGPPeer_addressFamily,
    bGPPeer_authKey,
    bGPPeer_bgpPeerState,
    bGPPeer_customerAddress,
    bGPPeer_asn,
    bGPPeer_amazonAddress,
    bGPPeer_bgpStatus,
    bGPPeer_awsLogicalDeviceId,
    bGPPeer_awsDeviceV2,

    -- ** Connection
    connection_tags,
    connection_macSecKeys,
    connection_macSecCapable,
    connection_providerName,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_connectionState,
    connection_hasLogicalRedundancy,
    connection_vlan,
    connection_loaIssueTime,
    connection_awsDevice,
    connection_connectionId,
    connection_location,
    connection_region,
    connection_partnerName,
    connection_ownerAccount,
    connection_awsLogicalDeviceId,
    connection_encryptionMode,
    connection_connectionName,
    connection_awsDeviceV2,

    -- ** Connections
    connections_connections,

    -- ** CustomerAgreement
    customerAgreement_agreementName,
    customerAgreement_status,

    -- ** DirectConnectGateway
    directConnectGateway_directConnectGatewayId,
    directConnectGateway_stateChangeError,
    directConnectGateway_directConnectGatewayState,
    directConnectGateway_amazonSideAsn,
    directConnectGateway_directConnectGatewayName,
    directConnectGateway_ownerAccount,

    -- ** DirectConnectGatewayAssociation
    directConnectGatewayAssociation_directConnectGatewayOwnerAccount,
    directConnectGatewayAssociation_directConnectGatewayId,
    directConnectGatewayAssociation_virtualGatewayRegion,
    directConnectGatewayAssociation_associationState,
    directConnectGatewayAssociation_virtualGatewayId,
    directConnectGatewayAssociation_stateChangeError,
    directConnectGatewayAssociation_virtualGatewayOwnerAccount,
    directConnectGatewayAssociation_associatedGateway,
    directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociation_associationId,

    -- ** DirectConnectGatewayAssociationProposal
    directConnectGatewayAssociationProposal_proposalId,
    directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,
    directConnectGatewayAssociationProposal_directConnectGatewayId,
    directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_associatedGateway,
    directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_proposalState,

    -- ** DirectConnectGatewayAttachment
    directConnectGatewayAttachment_directConnectGatewayId,
    directConnectGatewayAttachment_stateChangeError,
    directConnectGatewayAttachment_virtualInterfaceOwnerAccount,
    directConnectGatewayAttachment_virtualInterfaceId,
    directConnectGatewayAttachment_virtualInterfaceRegion,
    directConnectGatewayAttachment_attachmentType,
    directConnectGatewayAttachment_attachmentState,

    -- ** Interconnect
    interconnect_tags,
    interconnect_providerName,
    interconnect_bandwidth,
    interconnect_interconnectName,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_hasLogicalRedundancy,
    interconnect_loaIssueTime,
    interconnect_interconnectState,
    interconnect_awsDevice,
    interconnect_location,
    interconnect_region,
    interconnect_interconnectId,
    interconnect_awsLogicalDeviceId,
    interconnect_awsDeviceV2,

    -- ** Lag
    lag_tags,
    lag_numberOfConnections,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_macSecCapable,
    lag_providerName,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_hasLogicalRedundancy,
    lag_awsDevice,
    lag_lagName,
    lag_location,
    lag_region,
    lag_allowsHostedConnections,
    lag_connections,
    lag_connectionsBandwidth,
    lag_ownerAccount,
    lag_awsLogicalDeviceId,
    lag_encryptionMode,
    lag_awsDeviceV2,

    -- ** Location
    location_availablePortSpeeds,
    location_region,
    location_availableProviders,
    location_locationName,
    location_locationCode,
    location_availableMacSecPortSpeeds,

    -- ** MacSecKey
    macSecKey_startOn,
    macSecKey_state,
    macSecKey_secretARN,
    macSecKey_ckn,

    -- ** NewBGPPeer
    newBGPPeer_addressFamily,
    newBGPPeer_authKey,
    newBGPPeer_customerAddress,
    newBGPPeer_asn,
    newBGPPeer_amazonAddress,

    -- ** NewPrivateVirtualInterface
    newPrivateVirtualInterface_tags,
    newPrivateVirtualInterface_addressFamily,
    newPrivateVirtualInterface_authKey,
    newPrivateVirtualInterface_directConnectGatewayId,
    newPrivateVirtualInterface_virtualGatewayId,
    newPrivateVirtualInterface_customerAddress,
    newPrivateVirtualInterface_amazonAddress,
    newPrivateVirtualInterface_mtu,
    newPrivateVirtualInterface_virtualInterfaceName,
    newPrivateVirtualInterface_vlan,
    newPrivateVirtualInterface_asn,

    -- ** NewPrivateVirtualInterfaceAllocation
    newPrivateVirtualInterfaceAllocation_tags,
    newPrivateVirtualInterfaceAllocation_addressFamily,
    newPrivateVirtualInterfaceAllocation_authKey,
    newPrivateVirtualInterfaceAllocation_customerAddress,
    newPrivateVirtualInterfaceAllocation_amazonAddress,
    newPrivateVirtualInterfaceAllocation_mtu,
    newPrivateVirtualInterfaceAllocation_virtualInterfaceName,
    newPrivateVirtualInterfaceAllocation_vlan,
    newPrivateVirtualInterfaceAllocation_asn,

    -- ** NewPublicVirtualInterface
    newPublicVirtualInterface_tags,
    newPublicVirtualInterface_addressFamily,
    newPublicVirtualInterface_authKey,
    newPublicVirtualInterface_routeFilterPrefixes,
    newPublicVirtualInterface_customerAddress,
    newPublicVirtualInterface_amazonAddress,
    newPublicVirtualInterface_virtualInterfaceName,
    newPublicVirtualInterface_vlan,
    newPublicVirtualInterface_asn,

    -- ** NewPublicVirtualInterfaceAllocation
    newPublicVirtualInterfaceAllocation_tags,
    newPublicVirtualInterfaceAllocation_addressFamily,
    newPublicVirtualInterfaceAllocation_authKey,
    newPublicVirtualInterfaceAllocation_routeFilterPrefixes,
    newPublicVirtualInterfaceAllocation_customerAddress,
    newPublicVirtualInterfaceAllocation_amazonAddress,
    newPublicVirtualInterfaceAllocation_virtualInterfaceName,
    newPublicVirtualInterfaceAllocation_vlan,
    newPublicVirtualInterfaceAllocation_asn,

    -- ** NewTransitVirtualInterface
    newTransitVirtualInterface_tags,
    newTransitVirtualInterface_addressFamily,
    newTransitVirtualInterface_authKey,
    newTransitVirtualInterface_directConnectGatewayId,
    newTransitVirtualInterface_vlan,
    newTransitVirtualInterface_customerAddress,
    newTransitVirtualInterface_asn,
    newTransitVirtualInterface_amazonAddress,
    newTransitVirtualInterface_mtu,
    newTransitVirtualInterface_virtualInterfaceName,

    -- ** NewTransitVirtualInterfaceAllocation
    newTransitVirtualInterfaceAllocation_tags,
    newTransitVirtualInterfaceAllocation_addressFamily,
    newTransitVirtualInterfaceAllocation_authKey,
    newTransitVirtualInterfaceAllocation_vlan,
    newTransitVirtualInterfaceAllocation_customerAddress,
    newTransitVirtualInterfaceAllocation_asn,
    newTransitVirtualInterfaceAllocation_amazonAddress,
    newTransitVirtualInterfaceAllocation_mtu,
    newTransitVirtualInterfaceAllocation_virtualInterfaceName,

    -- ** ResourceTag
    resourceTag_tags,
    resourceTag_resourceArn,

    -- ** RouteFilterPrefix
    routeFilterPrefix_cidr,

    -- ** RouterType
    routerType_xsltTemplateName,
    routerType_routerTypeIdentifier,
    routerType_software,
    routerType_platform,
    routerType_xsltTemplateNameForMacSec,
    routerType_vendor,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** VirtualGateway
    virtualGateway_virtualGatewayState,
    virtualGateway_virtualGatewayId,

    -- ** VirtualInterface
    virtualInterface_tags,
    virtualInterface_addressFamily,
    virtualInterface_authKey,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceType,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_virtualGatewayId,
    virtualInterface_vlan,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_virtualInterfaceState,
    virtualInterface_asn,
    virtualInterface_location,
    virtualInterface_region,
    virtualInterface_amazonAddress,
    virtualInterface_virtualInterfaceId,
    virtualInterface_bgpPeers,
    virtualInterface_customerRouterConfig,
    virtualInterface_amazonSideAsn,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,

    -- ** VirtualInterfaceTestHistory
    virtualInterfaceTestHistory_testDurationInMinutes,
    virtualInterfaceTestHistory_status,
    virtualInterfaceTestHistory_endTime,
    virtualInterfaceTestHistory_virtualInterfaceId,
    virtualInterfaceTestHistory_bgpPeers,
    virtualInterfaceTestHistory_testId,
    virtualInterfaceTestHistory_ownerAccount,
    virtualInterfaceTestHistory_startTime,
  )
where

import Amazonka.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
import Amazonka.DirectConnect.AllocateHostedConnection
import Amazonka.DirectConnect.AllocatePrivateVirtualInterface
import Amazonka.DirectConnect.AllocatePublicVirtualInterface
import Amazonka.DirectConnect.AllocateTransitVirtualInterface
import Amazonka.DirectConnect.AssociateConnectionWithLag
import Amazonka.DirectConnect.AssociateHostedConnection
import Amazonka.DirectConnect.AssociateMacSecKey
import Amazonka.DirectConnect.AssociateVirtualInterface
import Amazonka.DirectConnect.ConfirmConnection
import Amazonka.DirectConnect.ConfirmCustomerAgreement
import Amazonka.DirectConnect.ConfirmPrivateVirtualInterface
import Amazonka.DirectConnect.ConfirmPublicVirtualInterface
import Amazonka.DirectConnect.ConfirmTransitVirtualInterface
import Amazonka.DirectConnect.CreateBGPPeer
import Amazonka.DirectConnect.CreateConnection
import Amazonka.DirectConnect.CreateDirectConnectGateway
import Amazonka.DirectConnect.CreateDirectConnectGatewayAssociation
import Amazonka.DirectConnect.CreateDirectConnectGatewayAssociationProposal
import Amazonka.DirectConnect.CreateInterconnect
import Amazonka.DirectConnect.CreateLag
import Amazonka.DirectConnect.CreatePrivateVirtualInterface
import Amazonka.DirectConnect.CreatePublicVirtualInterface
import Amazonka.DirectConnect.CreateTransitVirtualInterface
import Amazonka.DirectConnect.DeleteBGPPeer
import Amazonka.DirectConnect.DeleteConnection
import Amazonka.DirectConnect.DeleteDirectConnectGateway
import Amazonka.DirectConnect.DeleteDirectConnectGatewayAssociation
import Amazonka.DirectConnect.DeleteDirectConnectGatewayAssociationProposal
import Amazonka.DirectConnect.DeleteInterconnect
import Amazonka.DirectConnect.DeleteLag
import Amazonka.DirectConnect.DeleteVirtualInterface
import Amazonka.DirectConnect.DescribeConnections
import Amazonka.DirectConnect.DescribeCustomerMetadata
import Amazonka.DirectConnect.DescribeDirectConnectGatewayAssociationProposals
import Amazonka.DirectConnect.DescribeDirectConnectGatewayAssociations
import Amazonka.DirectConnect.DescribeDirectConnectGatewayAttachments
import Amazonka.DirectConnect.DescribeDirectConnectGateways
import Amazonka.DirectConnect.DescribeHostedConnections
import Amazonka.DirectConnect.DescribeInterconnects
import Amazonka.DirectConnect.DescribeLags
import Amazonka.DirectConnect.DescribeLoa
import Amazonka.DirectConnect.DescribeLocations
import Amazonka.DirectConnect.DescribeRouterConfiguration
import Amazonka.DirectConnect.DescribeTags
import Amazonka.DirectConnect.DescribeVirtualGateways
import Amazonka.DirectConnect.DescribeVirtualInterfaces
import Amazonka.DirectConnect.DisassociateConnectionFromLag
import Amazonka.DirectConnect.DisassociateMacSecKey
import Amazonka.DirectConnect.ListVirtualInterfaceTestHistory
import Amazonka.DirectConnect.StartBgpFailoverTest
import Amazonka.DirectConnect.StopBgpFailoverTest
import Amazonka.DirectConnect.TagResource
import Amazonka.DirectConnect.Types.AssociatedGateway
import Amazonka.DirectConnect.Types.BGPPeer
import Amazonka.DirectConnect.Types.Connection
import Amazonka.DirectConnect.Types.Connections
import Amazonka.DirectConnect.Types.CustomerAgreement
import Amazonka.DirectConnect.Types.DirectConnectGateway
import Amazonka.DirectConnect.Types.DirectConnectGatewayAssociation
import Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationProposal
import Amazonka.DirectConnect.Types.DirectConnectGatewayAttachment
import Amazonka.DirectConnect.Types.Interconnect
import Amazonka.DirectConnect.Types.Lag
import Amazonka.DirectConnect.Types.Location
import Amazonka.DirectConnect.Types.MacSecKey
import Amazonka.DirectConnect.Types.NewBGPPeer
import Amazonka.DirectConnect.Types.NewPrivateVirtualInterface
import Amazonka.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
import Amazonka.DirectConnect.Types.NewPublicVirtualInterface
import Amazonka.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
import Amazonka.DirectConnect.Types.NewTransitVirtualInterface
import Amazonka.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
import Amazonka.DirectConnect.Types.ResourceTag
import Amazonka.DirectConnect.Types.RouteFilterPrefix
import Amazonka.DirectConnect.Types.RouterType
import Amazonka.DirectConnect.Types.Tag
import Amazonka.DirectConnect.Types.VirtualGateway
import Amazonka.DirectConnect.Types.VirtualInterface
import Amazonka.DirectConnect.Types.VirtualInterfaceTestHistory
import Amazonka.DirectConnect.UntagResource
import Amazonka.DirectConnect.UpdateConnection
import Amazonka.DirectConnect.UpdateDirectConnectGateway
import Amazonka.DirectConnect.UpdateDirectConnectGatewayAssociation
import Amazonka.DirectConnect.UpdateLag
import Amazonka.DirectConnect.UpdateVirtualInterfaceAttributes

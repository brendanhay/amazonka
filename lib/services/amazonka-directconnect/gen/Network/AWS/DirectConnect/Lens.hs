{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Lens
  ( -- * Operations

    -- ** DescribeDirectConnectGatewayAssociations
    describeDirectConnectGatewayAssociations_virtualGatewayId,
    describeDirectConnectGatewayAssociations_associationId,
    describeDirectConnectGatewayAssociations_associatedGatewayId,
    describeDirectConnectGatewayAssociations_directConnectGatewayId,
    describeDirectConnectGatewayAssociations_nextToken,
    describeDirectConnectGatewayAssociations_maxResults,
    describeDirectConnectGatewayAssociationsResponse_nextToken,
    describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations,
    describeDirectConnectGatewayAssociationsResponse_httpStatus,

    -- ** DescribeInterconnects
    describeInterconnects_interconnectId,
    describeInterconnectsResponse_interconnects,
    describeInterconnectsResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceArns,
    describeTagsResponse_resourceTags,
    describeTagsResponse_httpStatus,

    -- ** CreateTransitVirtualInterface
    createTransitVirtualInterface_connectionId,
    createTransitVirtualInterface_newTransitVirtualInterface,
    createTransitVirtualInterfaceResponse_virtualInterface,
    createTransitVirtualInterfaceResponse_httpStatus,

    -- ** DescribeLoa
    describeLoa_loaContentType,
    describeLoa_providerName,
    describeLoa_connectionId,
    describeLoaResponse_loaContent,
    describeLoaResponse_loaContentType,
    describeLoaResponse_httpStatus,

    -- ** DisassociateMacSecKey
    disassociateMacSecKey_connectionId,
    disassociateMacSecKey_secretARN,
    disassociateMacSecKeyResponse_connectionId,
    disassociateMacSecKeyResponse_macSecKeys,
    disassociateMacSecKeyResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_connectionId,
    connection_lagId,
    connection_macSecCapable,
    connection_portEncryptionStatus,
    connection_vlan,
    connection_location,
    connection_awsDevice,
    connection_hasLogicalRedundancy,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_loaIssueTime,
    connection_partnerName,
    connection_connectionName,
    connection_encryptionMode,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_ownerAccount,
    connection_region,
    connection_macSecKeys,
    connection_providerName,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_tags,

    -- ** UpdateConnection
    updateConnection_connectionName,
    updateConnection_encryptionMode,
    updateConnection_connectionId,
    connection_lagId,
    connection_macSecCapable,
    connection_portEncryptionStatus,
    connection_vlan,
    connection_location,
    connection_awsDevice,
    connection_hasLogicalRedundancy,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_loaIssueTime,
    connection_partnerName,
    connection_connectionName,
    connection_encryptionMode,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_ownerAccount,
    connection_region,
    connection_macSecKeys,
    connection_providerName,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_tags,

    -- ** StartBgpFailoverTest
    startBgpFailoverTest_bgpPeers,
    startBgpFailoverTest_testDurationInMinutes,
    startBgpFailoverTest_virtualInterfaceId,
    startBgpFailoverTestResponse_virtualInterfaceTest,
    startBgpFailoverTestResponse_httpStatus,

    -- ** UpdateVirtualInterfaceAttributes
    updateVirtualInterfaceAttributes_mtu,
    updateVirtualInterfaceAttributes_virtualInterfaceId,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_mtu,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_customerAddress,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_virtualInterfaceState,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_directConnectGatewayId,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceType,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_jumboFrameCapable,
    virtualInterface_customerRouterConfig,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,

    -- ** AssociateConnectionWithLag
    associateConnectionWithLag_connectionId,
    associateConnectionWithLag_lagId,
    connection_lagId,
    connection_macSecCapable,
    connection_portEncryptionStatus,
    connection_vlan,
    connection_location,
    connection_awsDevice,
    connection_hasLogicalRedundancy,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_loaIssueTime,
    connection_partnerName,
    connection_connectionName,
    connection_encryptionMode,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_ownerAccount,
    connection_region,
    connection_macSecKeys,
    connection_providerName,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_tags,

    -- ** CreateDirectConnectGatewayAssociationProposal
    createDirectConnectGatewayAssociationProposal_addAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociationProposal_removeAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociationProposal_directConnectGatewayId,
    createDirectConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,
    createDirectConnectGatewayAssociationProposal_gatewayId,
    createDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal,
    createDirectConnectGatewayAssociationProposalResponse_httpStatus,

    -- ** CreateConnection
    createConnection_lagId,
    createConnection_requestMACSec,
    createConnection_providerName,
    createConnection_tags,
    createConnection_location,
    createConnection_bandwidth,
    createConnection_connectionName,
    connection_lagId,
    connection_macSecCapable,
    connection_portEncryptionStatus,
    connection_vlan,
    connection_location,
    connection_awsDevice,
    connection_hasLogicalRedundancy,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_loaIssueTime,
    connection_partnerName,
    connection_connectionName,
    connection_encryptionMode,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_ownerAccount,
    connection_region,
    connection_macSecKeys,
    connection_providerName,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_tags,

    -- ** DescribeDirectConnectGateways
    describeDirectConnectGateways_directConnectGatewayId,
    describeDirectConnectGateways_nextToken,
    describeDirectConnectGateways_maxResults,
    describeDirectConnectGatewaysResponse_directConnectGateways,
    describeDirectConnectGatewaysResponse_nextToken,
    describeDirectConnectGatewaysResponse_httpStatus,

    -- ** AssociateVirtualInterface
    associateVirtualInterface_virtualInterfaceId,
    associateVirtualInterface_connectionId,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_mtu,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_customerAddress,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_virtualInterfaceState,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_directConnectGatewayId,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceType,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_jumboFrameCapable,
    virtualInterface_customerRouterConfig,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,

    -- ** DescribeConnections
    describeConnections_connectionId,
    connections_connections,

    -- ** DeleteInterconnect
    deleteInterconnect_interconnectId,
    deleteInterconnectResponse_interconnectState,
    deleteInterconnectResponse_httpStatus,

    -- ** ConfirmPrivateVirtualInterface
    confirmPrivateVirtualInterface_virtualGatewayId,
    confirmPrivateVirtualInterface_directConnectGatewayId,
    confirmPrivateVirtualInterface_virtualInterfaceId,
    confirmPrivateVirtualInterfaceResponse_virtualInterfaceState,
    confirmPrivateVirtualInterfaceResponse_httpStatus,

    -- ** UpdateDirectConnectGatewayAssociation
    updateDirectConnectGatewayAssociation_associationId,
    updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    updateDirectConnectGatewayAssociationResponse_httpStatus,

    -- ** DeleteDirectConnectGatewayAssociation
    deleteDirectConnectGatewayAssociation_virtualGatewayId,
    deleteDirectConnectGatewayAssociation_associationId,
    deleteDirectConnectGatewayAssociation_directConnectGatewayId,
    deleteDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    deleteDirectConnectGatewayAssociationResponse_httpStatus,

    -- ** DescribeLocations
    describeLocationsResponse_locations,
    describeLocationsResponse_httpStatus,

    -- ** CreateDirectConnectGatewayAssociation
    createDirectConnectGatewayAssociation_virtualGatewayId,
    createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociation_gatewayId,
    createDirectConnectGatewayAssociation_directConnectGatewayId,
    createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    createDirectConnectGatewayAssociationResponse_httpStatus,

    -- ** AcceptDirectConnectGatewayAssociationProposal
    acceptDirectConnectGatewayAssociationProposal_overrideAllowedPrefixesToDirectConnectGateway,
    acceptDirectConnectGatewayAssociationProposal_directConnectGatewayId,
    acceptDirectConnectGatewayAssociationProposal_proposalId,
    acceptDirectConnectGatewayAssociationProposal_associatedGatewayOwnerAccount,
    acceptDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociation,
    acceptDirectConnectGatewayAssociationProposalResponse_httpStatus,

    -- ** CreatePublicVirtualInterface
    createPublicVirtualInterface_connectionId,
    createPublicVirtualInterface_newPublicVirtualInterface,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_mtu,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_customerAddress,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_virtualInterfaceState,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_directConnectGatewayId,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceType,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_jumboFrameCapable,
    virtualInterface_customerRouterConfig,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,

    -- ** AssociateMacSecKey
    associateMacSecKey_ckn,
    associateMacSecKey_cak,
    associateMacSecKey_secretARN,
    associateMacSecKey_connectionId,
    associateMacSecKeyResponse_connectionId,
    associateMacSecKeyResponse_macSecKeys,
    associateMacSecKeyResponse_httpStatus,

    -- ** AllocatePrivateVirtualInterface
    allocatePrivateVirtualInterface_connectionId,
    allocatePrivateVirtualInterface_ownerAccount,
    allocatePrivateVirtualInterface_newPrivateVirtualInterfaceAllocation,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_mtu,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_customerAddress,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_virtualInterfaceState,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_directConnectGatewayId,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceType,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_jumboFrameCapable,
    virtualInterface_customerRouterConfig,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,

    -- ** DescribeLags
    describeLags_lagId,
    describeLagsResponse_lags,
    describeLagsResponse_httpStatus,

    -- ** ConfirmConnection
    confirmConnection_connectionId,
    confirmConnectionResponse_connectionState,
    confirmConnectionResponse_httpStatus,

    -- ** DescribeDirectConnectGatewayAttachments
    describeDirectConnectGatewayAttachments_directConnectGatewayId,
    describeDirectConnectGatewayAttachments_nextToken,
    describeDirectConnectGatewayAttachments_maxResults,
    describeDirectConnectGatewayAttachments_virtualInterfaceId,
    describeDirectConnectGatewayAttachmentsResponse_nextToken,
    describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments,
    describeDirectConnectGatewayAttachmentsResponse_httpStatus,

    -- ** ConfirmPublicVirtualInterface
    confirmPublicVirtualInterface_virtualInterfaceId,
    confirmPublicVirtualInterfaceResponse_virtualInterfaceState,
    confirmPublicVirtualInterfaceResponse_httpStatus,

    -- ** DescribeVirtualGateways
    describeVirtualGatewaysResponse_virtualGateways,
    describeVirtualGatewaysResponse_httpStatus,

    -- ** DeleteDirectConnectGatewayAssociationProposal
    deleteDirectConnectGatewayAssociationProposal_proposalId,
    deleteDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal,
    deleteDirectConnectGatewayAssociationProposalResponse_httpStatus,

    -- ** StopBgpFailoverTest
    stopBgpFailoverTest_virtualInterfaceId,
    stopBgpFailoverTestResponse_virtualInterfaceTest,
    stopBgpFailoverTestResponse_httpStatus,

    -- ** CreateDirectConnectGateway
    createDirectConnectGateway_amazonSideAsn,
    createDirectConnectGateway_directConnectGatewayName,
    createDirectConnectGatewayResponse_directConnectGateway,
    createDirectConnectGatewayResponse_httpStatus,

    -- ** DeleteDirectConnectGateway
    deleteDirectConnectGateway_directConnectGatewayId,
    deleteDirectConnectGatewayResponse_directConnectGateway,
    deleteDirectConnectGatewayResponse_httpStatus,

    -- ** DescribeVirtualInterfaces
    describeVirtualInterfaces_connectionId,
    describeVirtualInterfaces_virtualInterfaceId,
    describeVirtualInterfacesResponse_virtualInterfaces,
    describeVirtualInterfacesResponse_httpStatus,

    -- ** ListVirtualInterfaceTestHistory
    listVirtualInterfaceTestHistory_bgpPeers,
    listVirtualInterfaceTestHistory_status,
    listVirtualInterfaceTestHistory_testId,
    listVirtualInterfaceTestHistory_nextToken,
    listVirtualInterfaceTestHistory_maxResults,
    listVirtualInterfaceTestHistory_virtualInterfaceId,
    listVirtualInterfaceTestHistoryResponse_nextToken,
    listVirtualInterfaceTestHistoryResponse_virtualInterfaceTestHistory,
    listVirtualInterfaceTestHistoryResponse_httpStatus,

    -- ** AllocateHostedConnection
    allocateHostedConnection_tags,
    allocateHostedConnection_connectionId,
    allocateHostedConnection_ownerAccount,
    allocateHostedConnection_bandwidth,
    allocateHostedConnection_connectionName,
    allocateHostedConnection_vlan,
    connection_lagId,
    connection_macSecCapable,
    connection_portEncryptionStatus,
    connection_vlan,
    connection_location,
    connection_awsDevice,
    connection_hasLogicalRedundancy,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_loaIssueTime,
    connection_partnerName,
    connection_connectionName,
    connection_encryptionMode,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_ownerAccount,
    connection_region,
    connection_macSecKeys,
    connection_providerName,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_tags,

    -- ** DeleteVirtualInterface
    deleteVirtualInterface_virtualInterfaceId,
    deleteVirtualInterfaceResponse_virtualInterfaceState,
    deleteVirtualInterfaceResponse_httpStatus,

    -- ** CreatePrivateVirtualInterface
    createPrivateVirtualInterface_connectionId,
    createPrivateVirtualInterface_newPrivateVirtualInterface,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_mtu,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_customerAddress,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_virtualInterfaceState,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_directConnectGatewayId,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceType,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_jumboFrameCapable,
    virtualInterface_customerRouterConfig,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,

    -- ** AllocatePublicVirtualInterface
    allocatePublicVirtualInterface_connectionId,
    allocatePublicVirtualInterface_ownerAccount,
    allocatePublicVirtualInterface_newPublicVirtualInterfaceAllocation,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_mtu,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_customerAddress,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_virtualInterfaceState,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_directConnectGatewayId,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceType,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_jumboFrameCapable,
    virtualInterface_customerRouterConfig,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,

    -- ** DescribeDirectConnectGatewayAssociationProposals
    describeDirectConnectGatewayAssociationProposals_associatedGatewayId,
    describeDirectConnectGatewayAssociationProposals_directConnectGatewayId,
    describeDirectConnectGatewayAssociationProposals_proposalId,
    describeDirectConnectGatewayAssociationProposals_nextToken,
    describeDirectConnectGatewayAssociationProposals_maxResults,
    describeDirectConnectGatewayAssociationProposalsResponse_directConnectGatewayAssociationProposals,
    describeDirectConnectGatewayAssociationProposalsResponse_nextToken,
    describeDirectConnectGatewayAssociationProposalsResponse_httpStatus,

    -- ** DisassociateConnectionFromLag
    disassociateConnectionFromLag_connectionId,
    disassociateConnectionFromLag_lagId,
    connection_lagId,
    connection_macSecCapable,
    connection_portEncryptionStatus,
    connection_vlan,
    connection_location,
    connection_awsDevice,
    connection_hasLogicalRedundancy,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_loaIssueTime,
    connection_partnerName,
    connection_connectionName,
    connection_encryptionMode,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_ownerAccount,
    connection_region,
    connection_macSecKeys,
    connection_providerName,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_tags,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DeleteLag
    deleteLag_lagId,
    lag_lagId,
    lag_macSecCapable,
    lag_connectionsBandwidth,
    lag_minimumLinks,
    lag_lagName,
    lag_location,
    lag_connections,
    lag_awsDevice,
    lag_hasLogicalRedundancy,
    lag_awsLogicalDeviceId,
    lag_allowsHostedConnections,
    lag_encryptionMode,
    lag_numberOfConnections,
    lag_jumboFrameCapable,
    lag_lagState,
    lag_ownerAccount,
    lag_region,
    lag_macSecKeys,
    lag_providerName,
    lag_awsDeviceV2,
    lag_tags,

    -- ** UpdateLag
    updateLag_minimumLinks,
    updateLag_lagName,
    updateLag_encryptionMode,
    updateLag_lagId,
    lag_lagId,
    lag_macSecCapable,
    lag_connectionsBandwidth,
    lag_minimumLinks,
    lag_lagName,
    lag_location,
    lag_connections,
    lag_awsDevice,
    lag_hasLogicalRedundancy,
    lag_awsLogicalDeviceId,
    lag_allowsHostedConnections,
    lag_encryptionMode,
    lag_numberOfConnections,
    lag_jumboFrameCapable,
    lag_lagState,
    lag_ownerAccount,
    lag_region,
    lag_macSecKeys,
    lag_providerName,
    lag_awsDeviceV2,
    lag_tags,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateBGPPeer
    createBGPPeer_newBGPPeer,
    createBGPPeer_virtualInterfaceId,
    createBGPPeerResponse_virtualInterface,
    createBGPPeerResponse_httpStatus,

    -- ** AssociateHostedConnection
    associateHostedConnection_connectionId,
    associateHostedConnection_parentConnectionId,
    connection_lagId,
    connection_macSecCapable,
    connection_portEncryptionStatus,
    connection_vlan,
    connection_location,
    connection_awsDevice,
    connection_hasLogicalRedundancy,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_loaIssueTime,
    connection_partnerName,
    connection_connectionName,
    connection_encryptionMode,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_ownerAccount,
    connection_region,
    connection_macSecKeys,
    connection_providerName,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_tags,

    -- ** CreateInterconnect
    createInterconnect_lagId,
    createInterconnect_providerName,
    createInterconnect_tags,
    createInterconnect_interconnectName,
    createInterconnect_bandwidth,
    createInterconnect_location,
    interconnect_lagId,
    interconnect_interconnectId,
    interconnect_location,
    interconnect_interconnectName,
    interconnect_awsDevice,
    interconnect_hasLogicalRedundancy,
    interconnect_awsLogicalDeviceId,
    interconnect_loaIssueTime,
    interconnect_bandwidth,
    interconnect_jumboFrameCapable,
    interconnect_interconnectState,
    interconnect_region,
    interconnect_providerName,
    interconnect_awsDeviceV2,
    interconnect_tags,

    -- ** DeleteBGPPeer
    deleteBGPPeer_customerAddress,
    deleteBGPPeer_asn,
    deleteBGPPeer_bgpPeerId,
    deleteBGPPeer_virtualInterfaceId,
    deleteBGPPeerResponse_virtualInterface,
    deleteBGPPeerResponse_httpStatus,

    -- ** AllocateTransitVirtualInterface
    allocateTransitVirtualInterface_connectionId,
    allocateTransitVirtualInterface_ownerAccount,
    allocateTransitVirtualInterface_newTransitVirtualInterfaceAllocation,
    allocateTransitVirtualInterfaceResponse_virtualInterface,
    allocateTransitVirtualInterfaceResponse_httpStatus,

    -- ** CreateLag
    createLag_childConnectionTags,
    createLag_connectionId,
    createLag_requestMACSec,
    createLag_providerName,
    createLag_tags,
    createLag_numberOfConnections,
    createLag_location,
    createLag_connectionsBandwidth,
    createLag_lagName,
    lag_lagId,
    lag_macSecCapable,
    lag_connectionsBandwidth,
    lag_minimumLinks,
    lag_lagName,
    lag_location,
    lag_connections,
    lag_awsDevice,
    lag_hasLogicalRedundancy,
    lag_awsLogicalDeviceId,
    lag_allowsHostedConnections,
    lag_encryptionMode,
    lag_numberOfConnections,
    lag_jumboFrameCapable,
    lag_lagState,
    lag_ownerAccount,
    lag_region,
    lag_macSecKeys,
    lag_providerName,
    lag_awsDeviceV2,
    lag_tags,

    -- ** ConfirmTransitVirtualInterface
    confirmTransitVirtualInterface_virtualInterfaceId,
    confirmTransitVirtualInterface_directConnectGatewayId,
    confirmTransitVirtualInterfaceResponse_virtualInterfaceState,
    confirmTransitVirtualInterfaceResponse_httpStatus,

    -- ** DescribeHostedConnections
    describeHostedConnections_connectionId,
    connections_connections,

    -- * Types

    -- ** AssociatedGateway
    associatedGateway_id,
    associatedGateway_ownerAccount,
    associatedGateway_region,
    associatedGateway_type,

    -- ** BGPPeer
    bGPPeer_customerAddress,
    bGPPeer_amazonAddress,
    bGPPeer_addressFamily,
    bGPPeer_awsLogicalDeviceId,
    bGPPeer_bgpStatus,
    bGPPeer_asn,
    bGPPeer_authKey,
    bGPPeer_bgpPeerId,
    bGPPeer_bgpPeerState,
    bGPPeer_awsDeviceV2,

    -- ** Connection
    connection_lagId,
    connection_macSecCapable,
    connection_portEncryptionStatus,
    connection_vlan,
    connection_location,
    connection_awsDevice,
    connection_hasLogicalRedundancy,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_loaIssueTime,
    connection_partnerName,
    connection_connectionName,
    connection_encryptionMode,
    connection_bandwidth,
    connection_jumboFrameCapable,
    connection_ownerAccount,
    connection_region,
    connection_macSecKeys,
    connection_providerName,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_tags,

    -- ** Connections
    connections_connections,

    -- ** DirectConnectGateway
    directConnectGateway_directConnectGatewayId,
    directConnectGateway_stateChangeError,
    directConnectGateway_amazonSideAsn,
    directConnectGateway_directConnectGatewayName,
    directConnectGateway_directConnectGatewayState,
    directConnectGateway_ownerAccount,

    -- ** DirectConnectGatewayAssociation
    directConnectGatewayAssociation_virtualGatewayId,
    directConnectGatewayAssociation_associationId,
    directConnectGatewayAssociation_directConnectGatewayId,
    directConnectGatewayAssociation_virtualGatewayOwnerAccount,
    directConnectGatewayAssociation_stateChangeError,
    directConnectGatewayAssociation_virtualGatewayRegion,
    directConnectGatewayAssociation_associatedGateway,
    directConnectGatewayAssociation_directConnectGatewayOwnerAccount,
    directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociation_associationState,

    -- ** DirectConnectGatewayAssociationProposal
    directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_directConnectGatewayId,
    directConnectGatewayAssociationProposal_proposalId,
    directConnectGatewayAssociationProposal_associatedGateway,
    directConnectGatewayAssociationProposal_proposalState,
    directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,
    directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway,

    -- ** DirectConnectGatewayAttachment
    directConnectGatewayAttachment_directConnectGatewayId,
    directConnectGatewayAttachment_attachmentState,
    directConnectGatewayAttachment_stateChangeError,
    directConnectGatewayAttachment_virtualInterfaceRegion,
    directConnectGatewayAttachment_virtualInterfaceOwnerAccount,
    directConnectGatewayAttachment_virtualInterfaceId,
    directConnectGatewayAttachment_attachmentType,

    -- ** Interconnect
    interconnect_lagId,
    interconnect_interconnectId,
    interconnect_location,
    interconnect_interconnectName,
    interconnect_awsDevice,
    interconnect_hasLogicalRedundancy,
    interconnect_awsLogicalDeviceId,
    interconnect_loaIssueTime,
    interconnect_bandwidth,
    interconnect_jumboFrameCapable,
    interconnect_interconnectState,
    interconnect_region,
    interconnect_providerName,
    interconnect_awsDeviceV2,
    interconnect_tags,

    -- ** Lag
    lag_lagId,
    lag_macSecCapable,
    lag_connectionsBandwidth,
    lag_minimumLinks,
    lag_lagName,
    lag_location,
    lag_connections,
    lag_awsDevice,
    lag_hasLogicalRedundancy,
    lag_awsLogicalDeviceId,
    lag_allowsHostedConnections,
    lag_encryptionMode,
    lag_numberOfConnections,
    lag_jumboFrameCapable,
    lag_lagState,
    lag_ownerAccount,
    lag_region,
    lag_macSecKeys,
    lag_providerName,
    lag_awsDeviceV2,
    lag_tags,

    -- ** Location
    location_availablePortSpeeds,
    location_locationName,
    location_locationCode,
    location_region,
    location_availableProviders,
    location_availableMacSecPortSpeeds,

    -- ** MacSecKey
    macSecKey_state,
    macSecKey_ckn,
    macSecKey_secretARN,
    macSecKey_startOn,

    -- ** NewBGPPeer
    newBGPPeer_customerAddress,
    newBGPPeer_amazonAddress,
    newBGPPeer_addressFamily,
    newBGPPeer_asn,
    newBGPPeer_authKey,

    -- ** NewPrivateVirtualInterface
    newPrivateVirtualInterface_virtualGatewayId,
    newPrivateVirtualInterface_mtu,
    newPrivateVirtualInterface_customerAddress,
    newPrivateVirtualInterface_amazonAddress,
    newPrivateVirtualInterface_addressFamily,
    newPrivateVirtualInterface_directConnectGatewayId,
    newPrivateVirtualInterface_authKey,
    newPrivateVirtualInterface_tags,
    newPrivateVirtualInterface_virtualInterfaceName,
    newPrivateVirtualInterface_vlan,
    newPrivateVirtualInterface_asn,

    -- ** NewPrivateVirtualInterfaceAllocation
    newPrivateVirtualInterfaceAllocation_mtu,
    newPrivateVirtualInterfaceAllocation_customerAddress,
    newPrivateVirtualInterfaceAllocation_amazonAddress,
    newPrivateVirtualInterfaceAllocation_addressFamily,
    newPrivateVirtualInterfaceAllocation_authKey,
    newPrivateVirtualInterfaceAllocation_tags,
    newPrivateVirtualInterfaceAllocation_virtualInterfaceName,
    newPrivateVirtualInterfaceAllocation_vlan,
    newPrivateVirtualInterfaceAllocation_asn,

    -- ** NewPublicVirtualInterface
    newPublicVirtualInterface_routeFilterPrefixes,
    newPublicVirtualInterface_customerAddress,
    newPublicVirtualInterface_amazonAddress,
    newPublicVirtualInterface_addressFamily,
    newPublicVirtualInterface_authKey,
    newPublicVirtualInterface_tags,
    newPublicVirtualInterface_virtualInterfaceName,
    newPublicVirtualInterface_vlan,
    newPublicVirtualInterface_asn,

    -- ** NewPublicVirtualInterfaceAllocation
    newPublicVirtualInterfaceAllocation_routeFilterPrefixes,
    newPublicVirtualInterfaceAllocation_customerAddress,
    newPublicVirtualInterfaceAllocation_amazonAddress,
    newPublicVirtualInterfaceAllocation_addressFamily,
    newPublicVirtualInterfaceAllocation_authKey,
    newPublicVirtualInterfaceAllocation_tags,
    newPublicVirtualInterfaceAllocation_virtualInterfaceName,
    newPublicVirtualInterfaceAllocation_vlan,
    newPublicVirtualInterfaceAllocation_asn,

    -- ** NewTransitVirtualInterface
    newTransitVirtualInterface_mtu,
    newTransitVirtualInterface_customerAddress,
    newTransitVirtualInterface_vlan,
    newTransitVirtualInterface_amazonAddress,
    newTransitVirtualInterface_addressFamily,
    newTransitVirtualInterface_directConnectGatewayId,
    newTransitVirtualInterface_asn,
    newTransitVirtualInterface_authKey,
    newTransitVirtualInterface_virtualInterfaceName,
    newTransitVirtualInterface_tags,

    -- ** NewTransitVirtualInterfaceAllocation
    newTransitVirtualInterfaceAllocation_mtu,
    newTransitVirtualInterfaceAllocation_customerAddress,
    newTransitVirtualInterfaceAllocation_vlan,
    newTransitVirtualInterfaceAllocation_amazonAddress,
    newTransitVirtualInterfaceAllocation_addressFamily,
    newTransitVirtualInterfaceAllocation_asn,
    newTransitVirtualInterfaceAllocation_authKey,
    newTransitVirtualInterfaceAllocation_virtualInterfaceName,
    newTransitVirtualInterfaceAllocation_tags,

    -- ** ResourceTag
    resourceTag_resourceArn,
    resourceTag_tags,

    -- ** RouteFilterPrefix
    routeFilterPrefix_cidr,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** VirtualGateway
    virtualGateway_virtualGatewayId,
    virtualGateway_virtualGatewayState,

    -- ** VirtualInterface
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_mtu,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_customerAddress,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_virtualInterfaceState,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_directConnectGatewayId,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceType,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_jumboFrameCapable,
    virtualInterface_customerRouterConfig,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_virtualInterfaceName,
    virtualInterface_awsDeviceV2,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,

    -- ** VirtualInterfaceTestHistory
    virtualInterfaceTestHistory_bgpPeers,
    virtualInterfaceTestHistory_status,
    virtualInterfaceTestHistory_testDurationInMinutes,
    virtualInterfaceTestHistory_startTime,
    virtualInterfaceTestHistory_testId,
    virtualInterfaceTestHistory_endTime,
    virtualInterfaceTestHistory_ownerAccount,
    virtualInterfaceTestHistory_virtualInterfaceId,
  )
where

import Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.AllocateHostedConnection
import Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
import Network.AWS.DirectConnect.AllocatePublicVirtualInterface
import Network.AWS.DirectConnect.AllocateTransitVirtualInterface
import Network.AWS.DirectConnect.AssociateConnectionWithLag
import Network.AWS.DirectConnect.AssociateHostedConnection
import Network.AWS.DirectConnect.AssociateMacSecKey
import Network.AWS.DirectConnect.AssociateVirtualInterface
import Network.AWS.DirectConnect.ConfirmConnection
import Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
import Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
import Network.AWS.DirectConnect.ConfirmTransitVirtualInterface
import Network.AWS.DirectConnect.CreateBGPPeer
import Network.AWS.DirectConnect.CreateConnection
import Network.AWS.DirectConnect.CreateDirectConnectGateway
import Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.CreateInterconnect
import Network.AWS.DirectConnect.CreateLag
import Network.AWS.DirectConnect.CreatePrivateVirtualInterface
import Network.AWS.DirectConnect.CreatePublicVirtualInterface
import Network.AWS.DirectConnect.CreateTransitVirtualInterface
import Network.AWS.DirectConnect.DeleteBGPPeer
import Network.AWS.DirectConnect.DeleteConnection
import Network.AWS.DirectConnect.DeleteDirectConnectGateway
import Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.DeleteInterconnect
import Network.AWS.DirectConnect.DeleteLag
import Network.AWS.DirectConnect.DeleteVirtualInterface
import Network.AWS.DirectConnect.DescribeConnections
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociationProposals
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
import Network.AWS.DirectConnect.DescribeDirectConnectGateways
import Network.AWS.DirectConnect.DescribeHostedConnections
import Network.AWS.DirectConnect.DescribeInterconnects
import Network.AWS.DirectConnect.DescribeLags
import Network.AWS.DirectConnect.DescribeLoa
import Network.AWS.DirectConnect.DescribeLocations
import Network.AWS.DirectConnect.DescribeTags
import Network.AWS.DirectConnect.DescribeVirtualGateways
import Network.AWS.DirectConnect.DescribeVirtualInterfaces
import Network.AWS.DirectConnect.DisassociateConnectionFromLag
import Network.AWS.DirectConnect.DisassociateMacSecKey
import Network.AWS.DirectConnect.ListVirtualInterfaceTestHistory
import Network.AWS.DirectConnect.StartBgpFailoverTest
import Network.AWS.DirectConnect.StopBgpFailoverTest
import Network.AWS.DirectConnect.TagResource
import Network.AWS.DirectConnect.Types.AssociatedGateway
import Network.AWS.DirectConnect.Types.BGPPeer
import Network.AWS.DirectConnect.Types.Connection
import Network.AWS.DirectConnect.Types.Connections
import Network.AWS.DirectConnect.Types.DirectConnectGateway
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachment
import Network.AWS.DirectConnect.Types.Interconnect
import Network.AWS.DirectConnect.Types.Lag
import Network.AWS.DirectConnect.Types.Location
import Network.AWS.DirectConnect.Types.MacSecKey
import Network.AWS.DirectConnect.Types.NewBGPPeer
import Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface
import Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.NewPublicVirtualInterface
import Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.NewTransitVirtualInterface
import Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
import Network.AWS.DirectConnect.Types.ResourceTag
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.DirectConnect.Types.VirtualGateway
import Network.AWS.DirectConnect.Types.VirtualInterface
import Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory
import Network.AWS.DirectConnect.UntagResource
import Network.AWS.DirectConnect.UpdateConnection
import Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.UpdateLag
import Network.AWS.DirectConnect.UpdateVirtualInterfaceAttributes

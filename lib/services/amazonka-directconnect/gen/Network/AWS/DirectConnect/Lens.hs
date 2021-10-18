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

    -- ** UpdateConnection
    updateConnection_connectionName,
    updateConnection_encryptionMode,
    updateConnection_connectionId,
    connection_bandwidth,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_connectionName,
    connection_macSecKeys,
    connection_providerName,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_encryptionMode,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_vlan,
    connection_location,
    connection_macSecCapable,

    -- ** StartBgpFailoverTest
    startBgpFailoverTest_bgpPeers,
    startBgpFailoverTest_testDurationInMinutes,
    startBgpFailoverTest_virtualInterfaceId,
    startBgpFailoverTestResponse_virtualInterfaceTest,
    startBgpFailoverTestResponse_httpStatus,

    -- ** DeleteDirectConnectGatewayAssociationProposal
    deleteDirectConnectGatewayAssociationProposal_proposalId,
    deleteDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal,
    deleteDirectConnectGatewayAssociationProposalResponse_httpStatus,

    -- ** DescribeVirtualGateways
    describeVirtualGatewaysResponse_virtualGateways,
    describeVirtualGatewaysResponse_httpStatus,

    -- ** StopBgpFailoverTest
    stopBgpFailoverTest_virtualInterfaceId,
    stopBgpFailoverTestResponse_virtualInterfaceTest,
    stopBgpFailoverTestResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_connectionId,
    connection_bandwidth,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_connectionName,
    connection_macSecKeys,
    connection_providerName,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_encryptionMode,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_vlan,
    connection_location,
    connection_macSecCapable,

    -- ** ConfirmPublicVirtualInterface
    confirmPublicVirtualInterface_virtualInterfaceId,
    confirmPublicVirtualInterfaceResponse_virtualInterfaceState,
    confirmPublicVirtualInterfaceResponse_httpStatus,

    -- ** AllocatePrivateVirtualInterface
    allocatePrivateVirtualInterface_connectionId,
    allocatePrivateVirtualInterface_ownerAccount,
    allocatePrivateVirtualInterface_newPrivateVirtualInterfaceAllocation,
    virtualInterface_authKey,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_asn,
    virtualInterface_awsDeviceV2,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_customerRouterConfig,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_mtu,
    virtualInterface_virtualInterfaceType,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_customerAddress,

    -- ** DescribeDirectConnectGatewayAssociations
    describeDirectConnectGatewayAssociations_nextToken,
    describeDirectConnectGatewayAssociations_virtualGatewayId,
    describeDirectConnectGatewayAssociations_maxResults,
    describeDirectConnectGatewayAssociations_associatedGatewayId,
    describeDirectConnectGatewayAssociations_associationId,
    describeDirectConnectGatewayAssociations_directConnectGatewayId,
    describeDirectConnectGatewayAssociationsResponse_nextToken,
    describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations,
    describeDirectConnectGatewayAssociationsResponse_httpStatus,

    -- ** ConfirmConnection
    confirmConnection_connectionId,
    confirmConnectionResponse_connectionState,
    confirmConnectionResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceArns,
    describeTagsResponse_resourceTags,
    describeTagsResponse_httpStatus,

    -- ** DescribeDirectConnectGatewayAttachments
    describeDirectConnectGatewayAttachments_nextToken,
    describeDirectConnectGatewayAttachments_maxResults,
    describeDirectConnectGatewayAttachments_virtualInterfaceId,
    describeDirectConnectGatewayAttachments_directConnectGatewayId,
    describeDirectConnectGatewayAttachmentsResponse_nextToken,
    describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments,
    describeDirectConnectGatewayAttachmentsResponse_httpStatus,

    -- ** CreatePublicVirtualInterface
    createPublicVirtualInterface_connectionId,
    createPublicVirtualInterface_newPublicVirtualInterface,
    virtualInterface_authKey,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_asn,
    virtualInterface_awsDeviceV2,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_customerRouterConfig,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_mtu,
    virtualInterface_virtualInterfaceType,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_customerAddress,

    -- ** AssociateMacSecKey
    associateMacSecKey_ckn,
    associateMacSecKey_secretARN,
    associateMacSecKey_cak,
    associateMacSecKey_connectionId,
    associateMacSecKeyResponse_macSecKeys,
    associateMacSecKeyResponse_connectionId,
    associateMacSecKeyResponse_httpStatus,

    -- ** DescribeHostedConnections
    describeHostedConnections_connectionId,
    connections_connections,

    -- ** AcceptDirectConnectGatewayAssociationProposal
    acceptDirectConnectGatewayAssociationProposal_overrideAllowedPrefixesToDirectConnectGateway,
    acceptDirectConnectGatewayAssociationProposal_directConnectGatewayId,
    acceptDirectConnectGatewayAssociationProposal_proposalId,
    acceptDirectConnectGatewayAssociationProposal_associatedGatewayOwnerAccount,
    acceptDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociation,
    acceptDirectConnectGatewayAssociationProposalResponse_httpStatus,

    -- ** CreateInterconnect
    createInterconnect_providerName,
    createInterconnect_lagId,
    createInterconnect_tags,
    createInterconnect_interconnectName,
    createInterconnect_bandwidth,
    createInterconnect_location,
    interconnect_interconnectId,
    interconnect_bandwidth,
    interconnect_awsDeviceV2,
    interconnect_providerName,
    interconnect_awsLogicalDeviceId,
    interconnect_hasLogicalRedundancy,
    interconnect_awsDevice,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_tags,
    interconnect_loaIssueTime,
    interconnect_region,
    interconnect_interconnectName,
    interconnect_interconnectState,
    interconnect_location,

    -- ** CreateDirectConnectGatewayAssociation
    createDirectConnectGatewayAssociation_virtualGatewayId,
    createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociation_gatewayId,
    createDirectConnectGatewayAssociation_directConnectGatewayId,
    createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    createDirectConnectGatewayAssociationResponse_httpStatus,

    -- ** DeleteLag
    deleteLag_lagId,
    lag_numberOfConnections,
    lag_awsDeviceV2,
    lag_allowsHostedConnections,
    lag_macSecKeys,
    lag_providerName,
    lag_awsLogicalDeviceId,
    lag_hasLogicalRedundancy,
    lag_lagName,
    lag_connections,
    lag_awsDevice,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_connectionsBandwidth,
    lag_lagId,
    lag_encryptionMode,
    lag_tags,
    lag_ownerAccount,
    lag_region,
    lag_location,
    lag_minimumLinks,
    lag_macSecCapable,

    -- ** DeleteInterconnect
    deleteInterconnect_interconnectId,
    deleteInterconnectResponse_interconnectState,
    deleteInterconnectResponse_httpStatus,

    -- ** AssociateHostedConnection
    associateHostedConnection_connectionId,
    associateHostedConnection_parentConnectionId,
    connection_bandwidth,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_connectionName,
    connection_macSecKeys,
    connection_providerName,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_encryptionMode,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_vlan,
    connection_location,
    connection_macSecCapable,

    -- ** CreateBGPPeer
    createBGPPeer_virtualInterfaceId,
    createBGPPeer_newBGPPeer,
    createBGPPeerResponse_virtualInterface,
    createBGPPeerResponse_httpStatus,

    -- ** UpdateLag
    updateLag_lagName,
    updateLag_encryptionMode,
    updateLag_minimumLinks,
    updateLag_lagId,
    lag_numberOfConnections,
    lag_awsDeviceV2,
    lag_allowsHostedConnections,
    lag_macSecKeys,
    lag_providerName,
    lag_awsLogicalDeviceId,
    lag_hasLogicalRedundancy,
    lag_lagName,
    lag_connections,
    lag_awsDevice,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_connectionsBandwidth,
    lag_lagId,
    lag_encryptionMode,
    lag_tags,
    lag_ownerAccount,
    lag_region,
    lag_location,
    lag_minimumLinks,
    lag_macSecCapable,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ConfirmPrivateVirtualInterface
    confirmPrivateVirtualInterface_virtualGatewayId,
    confirmPrivateVirtualInterface_directConnectGatewayId,
    confirmPrivateVirtualInterface_virtualInterfaceId,
    confirmPrivateVirtualInterfaceResponse_virtualInterfaceState,
    confirmPrivateVirtualInterfaceResponse_httpStatus,

    -- ** DisassociateConnectionFromLag
    disassociateConnectionFromLag_connectionId,
    disassociateConnectionFromLag_lagId,
    connection_bandwidth,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_connectionName,
    connection_macSecKeys,
    connection_providerName,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_encryptionMode,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_vlan,
    connection_location,
    connection_macSecCapable,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DeleteVirtualInterface
    deleteVirtualInterface_virtualInterfaceId,
    deleteVirtualInterfaceResponse_virtualInterfaceState,
    deleteVirtualInterfaceResponse_httpStatus,

    -- ** DescribeDirectConnectGateways
    describeDirectConnectGateways_nextToken,
    describeDirectConnectGateways_maxResults,
    describeDirectConnectGateways_directConnectGatewayId,
    describeDirectConnectGatewaysResponse_nextToken,
    describeDirectConnectGatewaysResponse_directConnectGateways,
    describeDirectConnectGatewaysResponse_httpStatus,

    -- ** DescribeVirtualInterfaces
    describeVirtualInterfaces_connectionId,
    describeVirtualInterfaces_virtualInterfaceId,
    describeVirtualInterfacesResponse_virtualInterfaces,
    describeVirtualInterfacesResponse_httpStatus,

    -- ** UpdateVirtualInterfaceAttributes
    updateVirtualInterfaceAttributes_mtu,
    updateVirtualInterfaceAttributes_virtualInterfaceId,
    virtualInterface_authKey,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_asn,
    virtualInterface_awsDeviceV2,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_customerRouterConfig,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_mtu,
    virtualInterface_virtualInterfaceType,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_customerAddress,

    -- ** CreateConnection
    createConnection_providerName,
    createConnection_lagId,
    createConnection_tags,
    createConnection_requestMACSec,
    createConnection_location,
    createConnection_bandwidth,
    createConnection_connectionName,
    connection_bandwidth,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_connectionName,
    connection_macSecKeys,
    connection_providerName,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_encryptionMode,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_vlan,
    connection_location,
    connection_macSecCapable,

    -- ** AssociateConnectionWithLag
    associateConnectionWithLag_connectionId,
    associateConnectionWithLag_lagId,
    connection_bandwidth,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_connectionName,
    connection_macSecKeys,
    connection_providerName,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_encryptionMode,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_vlan,
    connection_location,
    connection_macSecCapable,

    -- ** ListVirtualInterfaceTestHistory
    listVirtualInterfaceTestHistory_bgpPeers,
    listVirtualInterfaceTestHistory_nextToken,
    listVirtualInterfaceTestHistory_status,
    listVirtualInterfaceTestHistory_maxResults,
    listVirtualInterfaceTestHistory_testId,
    listVirtualInterfaceTestHistory_virtualInterfaceId,
    listVirtualInterfaceTestHistoryResponse_nextToken,
    listVirtualInterfaceTestHistoryResponse_virtualInterfaceTestHistory,
    listVirtualInterfaceTestHistoryResponse_httpStatus,

    -- ** DisassociateMacSecKey
    disassociateMacSecKey_connectionId,
    disassociateMacSecKey_secretARN,
    disassociateMacSecKeyResponse_macSecKeys,
    disassociateMacSecKeyResponse_connectionId,
    disassociateMacSecKeyResponse_httpStatus,

    -- ** DescribeLoa
    describeLoa_providerName,
    describeLoa_loaContentType,
    describeLoa_connectionId,
    describeLoaResponse_loaContent,
    describeLoaResponse_loaContentType,
    describeLoaResponse_httpStatus,

    -- ** CreateTransitVirtualInterface
    createTransitVirtualInterface_connectionId,
    createTransitVirtualInterface_newTransitVirtualInterface,
    createTransitVirtualInterfaceResponse_virtualInterface,
    createTransitVirtualInterfaceResponse_httpStatus,

    -- ** CreateDirectConnectGateway
    createDirectConnectGateway_amazonSideAsn,
    createDirectConnectGateway_directConnectGatewayName,
    createDirectConnectGatewayResponse_directConnectGateway,
    createDirectConnectGatewayResponse_httpStatus,

    -- ** DescribeInterconnects
    describeInterconnects_interconnectId,
    describeInterconnectsResponse_interconnects,
    describeInterconnectsResponse_httpStatus,

    -- ** DescribeLags
    describeLags_lagId,
    describeLagsResponse_lags,
    describeLagsResponse_httpStatus,

    -- ** ConfirmTransitVirtualInterface
    confirmTransitVirtualInterface_virtualInterfaceId,
    confirmTransitVirtualInterface_directConnectGatewayId,
    confirmTransitVirtualInterfaceResponse_virtualInterfaceState,
    confirmTransitVirtualInterfaceResponse_httpStatus,

    -- ** CreateLag
    createLag_providerName,
    createLag_connectionId,
    createLag_childConnectionTags,
    createLag_tags,
    createLag_requestMACSec,
    createLag_numberOfConnections,
    createLag_location,
    createLag_connectionsBandwidth,
    createLag_lagName,
    lag_numberOfConnections,
    lag_awsDeviceV2,
    lag_allowsHostedConnections,
    lag_macSecKeys,
    lag_providerName,
    lag_awsLogicalDeviceId,
    lag_hasLogicalRedundancy,
    lag_lagName,
    lag_connections,
    lag_awsDevice,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_connectionsBandwidth,
    lag_lagId,
    lag_encryptionMode,
    lag_tags,
    lag_ownerAccount,
    lag_region,
    lag_location,
    lag_minimumLinks,
    lag_macSecCapable,

    -- ** DeleteBGPPeer
    deleteBGPPeer_asn,
    deleteBGPPeer_bgpPeerId,
    deleteBGPPeer_virtualInterfaceId,
    deleteBGPPeer_customerAddress,
    deleteBGPPeerResponse_virtualInterface,
    deleteBGPPeerResponse_httpStatus,

    -- ** AllocateTransitVirtualInterface
    allocateTransitVirtualInterface_connectionId,
    allocateTransitVirtualInterface_ownerAccount,
    allocateTransitVirtualInterface_newTransitVirtualInterfaceAllocation,
    allocateTransitVirtualInterfaceResponse_virtualInterface,
    allocateTransitVirtualInterfaceResponse_httpStatus,

    -- ** DeleteDirectConnectGatewayAssociation
    deleteDirectConnectGatewayAssociation_virtualGatewayId,
    deleteDirectConnectGatewayAssociation_associationId,
    deleteDirectConnectGatewayAssociation_directConnectGatewayId,
    deleteDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    deleteDirectConnectGatewayAssociationResponse_httpStatus,

    -- ** UpdateDirectConnectGatewayAssociation
    updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociation_associationId,
    updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    updateDirectConnectGatewayAssociationResponse_httpStatus,

    -- ** DescribeLocations
    describeLocationsResponse_locations,
    describeLocationsResponse_httpStatus,

    -- ** DescribeConnections
    describeConnections_connectionId,
    connections_connections,

    -- ** AllocatePublicVirtualInterface
    allocatePublicVirtualInterface_connectionId,
    allocatePublicVirtualInterface_ownerAccount,
    allocatePublicVirtualInterface_newPublicVirtualInterfaceAllocation,
    virtualInterface_authKey,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_asn,
    virtualInterface_awsDeviceV2,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_customerRouterConfig,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_mtu,
    virtualInterface_virtualInterfaceType,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_customerAddress,

    -- ** AssociateVirtualInterface
    associateVirtualInterface_virtualInterfaceId,
    associateVirtualInterface_connectionId,
    virtualInterface_authKey,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_asn,
    virtualInterface_awsDeviceV2,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_customerRouterConfig,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_mtu,
    virtualInterface_virtualInterfaceType,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_customerAddress,

    -- ** DescribeDirectConnectGatewayAssociationProposals
    describeDirectConnectGatewayAssociationProposals_nextToken,
    describeDirectConnectGatewayAssociationProposals_proposalId,
    describeDirectConnectGatewayAssociationProposals_maxResults,
    describeDirectConnectGatewayAssociationProposals_associatedGatewayId,
    describeDirectConnectGatewayAssociationProposals_directConnectGatewayId,
    describeDirectConnectGatewayAssociationProposalsResponse_nextToken,
    describeDirectConnectGatewayAssociationProposalsResponse_directConnectGatewayAssociationProposals,
    describeDirectConnectGatewayAssociationProposalsResponse_httpStatus,

    -- ** CreatePrivateVirtualInterface
    createPrivateVirtualInterface_connectionId,
    createPrivateVirtualInterface_newPrivateVirtualInterface,
    virtualInterface_authKey,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_asn,
    virtualInterface_awsDeviceV2,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_customerRouterConfig,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_mtu,
    virtualInterface_virtualInterfaceType,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_customerAddress,

    -- ** CreateDirectConnectGatewayAssociationProposal
    createDirectConnectGatewayAssociationProposal_removeAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociationProposal_addAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociationProposal_directConnectGatewayId,
    createDirectConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,
    createDirectConnectGatewayAssociationProposal_gatewayId,
    createDirectConnectGatewayAssociationProposalResponse_directConnectGatewayAssociationProposal,
    createDirectConnectGatewayAssociationProposalResponse_httpStatus,

    -- ** DeleteDirectConnectGateway
    deleteDirectConnectGateway_directConnectGatewayId,
    deleteDirectConnectGatewayResponse_directConnectGateway,
    deleteDirectConnectGatewayResponse_httpStatus,

    -- ** AllocateHostedConnection
    allocateHostedConnection_tags,
    allocateHostedConnection_connectionId,
    allocateHostedConnection_ownerAccount,
    allocateHostedConnection_bandwidth,
    allocateHostedConnection_connectionName,
    allocateHostedConnection_vlan,
    connection_bandwidth,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_connectionName,
    connection_macSecKeys,
    connection_providerName,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_encryptionMode,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_vlan,
    connection_location,
    connection_macSecCapable,

    -- * Types

    -- ** AssociatedGateway
    associatedGateway_id,
    associatedGateway_type,
    associatedGateway_ownerAccount,
    associatedGateway_region,

    -- ** BGPPeer
    bGPPeer_authKey,
    bGPPeer_asn,
    bGPPeer_awsDeviceV2,
    bGPPeer_awsLogicalDeviceId,
    bGPPeer_bgpPeerId,
    bGPPeer_bgpStatus,
    bGPPeer_bgpPeerState,
    bGPPeer_amazonAddress,
    bGPPeer_addressFamily,
    bGPPeer_customerAddress,

    -- ** Connection
    connection_bandwidth,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_connectionName,
    connection_macSecKeys,
    connection_providerName,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_encryptionMode,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_vlan,
    connection_location,
    connection_macSecCapable,

    -- ** Connections
    connections_connections,

    -- ** DirectConnectGateway
    directConnectGateway_directConnectGatewayState,
    directConnectGateway_stateChangeError,
    directConnectGateway_directConnectGatewayName,
    directConnectGateway_amazonSideAsn,
    directConnectGateway_directConnectGatewayId,
    directConnectGateway_ownerAccount,

    -- ** DirectConnectGatewayAssociation
    directConnectGatewayAssociation_virtualGatewayId,
    directConnectGatewayAssociation_virtualGatewayRegion,
    directConnectGatewayAssociation_virtualGatewayOwnerAccount,
    directConnectGatewayAssociation_stateChangeError,
    directConnectGatewayAssociation_associationState,
    directConnectGatewayAssociation_associatedGateway,
    directConnectGatewayAssociation_associationId,
    directConnectGatewayAssociation_directConnectGatewayId,
    directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociation_directConnectGatewayOwnerAccount,

    -- ** DirectConnectGatewayAssociationProposal
    directConnectGatewayAssociationProposal_proposalId,
    directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_proposalState,
    directConnectGatewayAssociationProposal_associatedGateway,
    directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_directConnectGatewayId,
    directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,

    -- ** DirectConnectGatewayAttachment
    directConnectGatewayAttachment_stateChangeError,
    directConnectGatewayAttachment_attachmentState,
    directConnectGatewayAttachment_virtualInterfaceOwnerAccount,
    directConnectGatewayAttachment_virtualInterfaceRegion,
    directConnectGatewayAttachment_attachmentType,
    directConnectGatewayAttachment_virtualInterfaceId,
    directConnectGatewayAttachment_directConnectGatewayId,

    -- ** Interconnect
    interconnect_interconnectId,
    interconnect_bandwidth,
    interconnect_awsDeviceV2,
    interconnect_providerName,
    interconnect_awsLogicalDeviceId,
    interconnect_hasLogicalRedundancy,
    interconnect_awsDevice,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_tags,
    interconnect_loaIssueTime,
    interconnect_region,
    interconnect_interconnectName,
    interconnect_interconnectState,
    interconnect_location,

    -- ** Lag
    lag_numberOfConnections,
    lag_awsDeviceV2,
    lag_allowsHostedConnections,
    lag_macSecKeys,
    lag_providerName,
    lag_awsLogicalDeviceId,
    lag_hasLogicalRedundancy,
    lag_lagName,
    lag_connections,
    lag_awsDevice,
    lag_lagState,
    lag_jumboFrameCapable,
    lag_connectionsBandwidth,
    lag_lagId,
    lag_encryptionMode,
    lag_tags,
    lag_ownerAccount,
    lag_region,
    lag_location,
    lag_minimumLinks,
    lag_macSecCapable,

    -- ** Location
    location_availablePortSpeeds,
    location_availableMacSecPortSpeeds,
    location_availableProviders,
    location_locationCode,
    location_region,
    location_locationName,

    -- ** MacSecKey
    macSecKey_startOn,
    macSecKey_ckn,
    macSecKey_secretARN,
    macSecKey_state,

    -- ** NewBGPPeer
    newBGPPeer_authKey,
    newBGPPeer_asn,
    newBGPPeer_amazonAddress,
    newBGPPeer_addressFamily,
    newBGPPeer_customerAddress,

    -- ** NewPrivateVirtualInterface
    newPrivateVirtualInterface_authKey,
    newPrivateVirtualInterface_virtualGatewayId,
    newPrivateVirtualInterface_mtu,
    newPrivateVirtualInterface_tags,
    newPrivateVirtualInterface_directConnectGatewayId,
    newPrivateVirtualInterface_amazonAddress,
    newPrivateVirtualInterface_addressFamily,
    newPrivateVirtualInterface_customerAddress,
    newPrivateVirtualInterface_virtualInterfaceName,
    newPrivateVirtualInterface_vlan,
    newPrivateVirtualInterface_asn,

    -- ** NewPrivateVirtualInterfaceAllocation
    newPrivateVirtualInterfaceAllocation_authKey,
    newPrivateVirtualInterfaceAllocation_mtu,
    newPrivateVirtualInterfaceAllocation_tags,
    newPrivateVirtualInterfaceAllocation_amazonAddress,
    newPrivateVirtualInterfaceAllocation_addressFamily,
    newPrivateVirtualInterfaceAllocation_customerAddress,
    newPrivateVirtualInterfaceAllocation_virtualInterfaceName,
    newPrivateVirtualInterfaceAllocation_vlan,
    newPrivateVirtualInterfaceAllocation_asn,

    -- ** NewPublicVirtualInterface
    newPublicVirtualInterface_authKey,
    newPublicVirtualInterface_routeFilterPrefixes,
    newPublicVirtualInterface_tags,
    newPublicVirtualInterface_amazonAddress,
    newPublicVirtualInterface_addressFamily,
    newPublicVirtualInterface_customerAddress,
    newPublicVirtualInterface_virtualInterfaceName,
    newPublicVirtualInterface_vlan,
    newPublicVirtualInterface_asn,

    -- ** NewPublicVirtualInterfaceAllocation
    newPublicVirtualInterfaceAllocation_authKey,
    newPublicVirtualInterfaceAllocation_routeFilterPrefixes,
    newPublicVirtualInterfaceAllocation_tags,
    newPublicVirtualInterfaceAllocation_amazonAddress,
    newPublicVirtualInterfaceAllocation_addressFamily,
    newPublicVirtualInterfaceAllocation_customerAddress,
    newPublicVirtualInterfaceAllocation_virtualInterfaceName,
    newPublicVirtualInterfaceAllocation_vlan,
    newPublicVirtualInterfaceAllocation_asn,

    -- ** NewTransitVirtualInterface
    newTransitVirtualInterface_authKey,
    newTransitVirtualInterface_asn,
    newTransitVirtualInterface_mtu,
    newTransitVirtualInterface_tags,
    newTransitVirtualInterface_directConnectGatewayId,
    newTransitVirtualInterface_virtualInterfaceName,
    newTransitVirtualInterface_amazonAddress,
    newTransitVirtualInterface_addressFamily,
    newTransitVirtualInterface_vlan,
    newTransitVirtualInterface_customerAddress,

    -- ** NewTransitVirtualInterfaceAllocation
    newTransitVirtualInterfaceAllocation_authKey,
    newTransitVirtualInterfaceAllocation_asn,
    newTransitVirtualInterfaceAllocation_mtu,
    newTransitVirtualInterfaceAllocation_tags,
    newTransitVirtualInterfaceAllocation_virtualInterfaceName,
    newTransitVirtualInterfaceAllocation_amazonAddress,
    newTransitVirtualInterfaceAllocation_addressFamily,
    newTransitVirtualInterfaceAllocation_vlan,
    newTransitVirtualInterfaceAllocation_customerAddress,

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
    virtualInterface_authKey,
    virtualInterface_bgpPeers,
    virtualInterface_virtualGatewayId,
    virtualInterface_asn,
    virtualInterface_awsDeviceV2,
    virtualInterface_connectionId,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_customerRouterConfig,
    virtualInterface_jumboFrameCapable,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_mtu,
    virtualInterface_virtualInterfaceType,
    virtualInterface_amazonSideAsn,
    virtualInterface_virtualInterfaceId,
    virtualInterface_tags,
    virtualInterface_directConnectGatewayId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_amazonAddress,
    virtualInterface_addressFamily,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_vlan,
    virtualInterface_location,
    virtualInterface_customerAddress,

    -- ** VirtualInterfaceTestHistory
    virtualInterfaceTestHistory_bgpPeers,
    virtualInterfaceTestHistory_status,
    virtualInterfaceTestHistory_testId,
    virtualInterfaceTestHistory_startTime,
    virtualInterfaceTestHistory_endTime,
    virtualInterfaceTestHistory_virtualInterfaceId,
    virtualInterfaceTestHistory_ownerAccount,
    virtualInterfaceTestHistory_testDurationInMinutes,
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

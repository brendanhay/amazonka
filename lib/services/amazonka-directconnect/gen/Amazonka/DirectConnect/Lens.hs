{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DirectConnect.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,

    -- ** AllocatePrivateVirtualInterface
    allocatePrivateVirtualInterface_connectionId,
    allocatePrivateVirtualInterface_ownerAccount,
    allocatePrivateVirtualInterface_newPrivateVirtualInterfaceAllocation,
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_amazonSideAsn,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_awsDeviceV2,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_bgpPeers,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_customerRouterConfig,
    virtualInterface_directConnectGatewayId,
    virtualInterface_jumboFrameCapable,
    virtualInterface_location,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_siteLinkEnabled,
    virtualInterface_tags,
    virtualInterface_virtualGatewayId,
    virtualInterface_virtualInterfaceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceType,
    virtualInterface_vlan,

    -- ** AllocatePublicVirtualInterface
    allocatePublicVirtualInterface_connectionId,
    allocatePublicVirtualInterface_ownerAccount,
    allocatePublicVirtualInterface_newPublicVirtualInterfaceAllocation,
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_amazonSideAsn,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_awsDeviceV2,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_bgpPeers,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_customerRouterConfig,
    virtualInterface_directConnectGatewayId,
    virtualInterface_jumboFrameCapable,
    virtualInterface_location,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_siteLinkEnabled,
    virtualInterface_tags,
    virtualInterface_virtualGatewayId,
    virtualInterface_virtualInterfaceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceType,
    virtualInterface_vlan,

    -- ** AllocateTransitVirtualInterface
    allocateTransitVirtualInterface_connectionId,
    allocateTransitVirtualInterface_ownerAccount,
    allocateTransitVirtualInterface_newTransitVirtualInterfaceAllocation,
    allocateTransitVirtualInterfaceResponse_virtualInterface,
    allocateTransitVirtualInterfaceResponse_httpStatus,

    -- ** AssociateConnectionWithLag
    associateConnectionWithLag_connectionId,
    associateConnectionWithLag_lagId,
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,

    -- ** AssociateHostedConnection
    associateHostedConnection_connectionId,
    associateHostedConnection_parentConnectionId,
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,

    -- ** AssociateMacSecKey
    associateMacSecKey_cak,
    associateMacSecKey_ckn,
    associateMacSecKey_secretARN,
    associateMacSecKey_connectionId,
    associateMacSecKeyResponse_connectionId,
    associateMacSecKeyResponse_macSecKeys,
    associateMacSecKeyResponse_httpStatus,

    -- ** AssociateVirtualInterface
    associateVirtualInterface_virtualInterfaceId,
    associateVirtualInterface_connectionId,
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_amazonSideAsn,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_awsDeviceV2,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_bgpPeers,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_customerRouterConfig,
    virtualInterface_directConnectGatewayId,
    virtualInterface_jumboFrameCapable,
    virtualInterface_location,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_siteLinkEnabled,
    virtualInterface_tags,
    virtualInterface_virtualGatewayId,
    virtualInterface_virtualInterfaceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceType,
    virtualInterface_vlan,

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
    createConnection_lagId,
    createConnection_providerName,
    createConnection_requestMACSec,
    createConnection_tags,
    createConnection_location,
    createConnection_bandwidth,
    createConnection_connectionName,
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,

    -- ** CreateDirectConnectGateway
    createDirectConnectGateway_amazonSideAsn,
    createDirectConnectGateway_directConnectGatewayName,
    createDirectConnectGatewayResponse_directConnectGateway,
    createDirectConnectGatewayResponse_httpStatus,

    -- ** CreateDirectConnectGatewayAssociation
    createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociation_gatewayId,
    createDirectConnectGatewayAssociation_virtualGatewayId,
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
    createInterconnect_lagId,
    createInterconnect_providerName,
    createInterconnect_tags,
    createInterconnect_interconnectName,
    createInterconnect_bandwidth,
    createInterconnect_location,
    interconnect_awsDevice,
    interconnect_awsDeviceV2,
    interconnect_awsLogicalDeviceId,
    interconnect_bandwidth,
    interconnect_hasLogicalRedundancy,
    interconnect_interconnectId,
    interconnect_interconnectName,
    interconnect_interconnectState,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_loaIssueTime,
    interconnect_location,
    interconnect_providerName,
    interconnect_region,
    interconnect_tags,

    -- ** CreateLag
    createLag_childConnectionTags,
    createLag_connectionId,
    createLag_providerName,
    createLag_requestMACSec,
    createLag_tags,
    createLag_numberOfConnections,
    createLag_location,
    createLag_connectionsBandwidth,
    createLag_lagName,
    lag_allowsHostedConnections,
    lag_awsDevice,
    lag_awsDeviceV2,
    lag_awsLogicalDeviceId,
    lag_connections,
    lag_connectionsBandwidth,
    lag_encryptionMode,
    lag_hasLogicalRedundancy,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_lagName,
    lag_lagState,
    lag_location,
    lag_macSecCapable,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_numberOfConnections,
    lag_ownerAccount,
    lag_providerName,
    lag_region,
    lag_tags,

    -- ** CreatePrivateVirtualInterface
    createPrivateVirtualInterface_connectionId,
    createPrivateVirtualInterface_newPrivateVirtualInterface,
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_amazonSideAsn,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_awsDeviceV2,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_bgpPeers,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_customerRouterConfig,
    virtualInterface_directConnectGatewayId,
    virtualInterface_jumboFrameCapable,
    virtualInterface_location,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_siteLinkEnabled,
    virtualInterface_tags,
    virtualInterface_virtualGatewayId,
    virtualInterface_virtualInterfaceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceType,
    virtualInterface_vlan,

    -- ** CreatePublicVirtualInterface
    createPublicVirtualInterface_connectionId,
    createPublicVirtualInterface_newPublicVirtualInterface,
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_amazonSideAsn,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_awsDeviceV2,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_bgpPeers,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_customerRouterConfig,
    virtualInterface_directConnectGatewayId,
    virtualInterface_jumboFrameCapable,
    virtualInterface_location,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_siteLinkEnabled,
    virtualInterface_tags,
    virtualInterface_virtualGatewayId,
    virtualInterface_virtualInterfaceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceType,
    virtualInterface_vlan,

    -- ** CreateTransitVirtualInterface
    createTransitVirtualInterface_connectionId,
    createTransitVirtualInterface_newTransitVirtualInterface,
    createTransitVirtualInterfaceResponse_virtualInterface,
    createTransitVirtualInterfaceResponse_httpStatus,

    -- ** DeleteBGPPeer
    deleteBGPPeer_asn,
    deleteBGPPeer_bgpPeerId,
    deleteBGPPeer_customerAddress,
    deleteBGPPeer_virtualInterfaceId,
    deleteBGPPeerResponse_virtualInterface,
    deleteBGPPeerResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_connectionId,
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,

    -- ** DeleteDirectConnectGateway
    deleteDirectConnectGateway_directConnectGatewayId,
    deleteDirectConnectGatewayResponse_directConnectGateway,
    deleteDirectConnectGatewayResponse_httpStatus,

    -- ** DeleteDirectConnectGatewayAssociation
    deleteDirectConnectGatewayAssociation_associationId,
    deleteDirectConnectGatewayAssociation_directConnectGatewayId,
    deleteDirectConnectGatewayAssociation_virtualGatewayId,
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
    lag_allowsHostedConnections,
    lag_awsDevice,
    lag_awsDeviceV2,
    lag_awsLogicalDeviceId,
    lag_connections,
    lag_connectionsBandwidth,
    lag_encryptionMode,
    lag_hasLogicalRedundancy,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_lagName,
    lag_lagState,
    lag_location,
    lag_macSecCapable,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_numberOfConnections,
    lag_ownerAccount,
    lag_providerName,
    lag_region,
    lag_tags,

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
    describeDirectConnectGatewayAssociationProposals_associatedGatewayId,
    describeDirectConnectGatewayAssociationProposals_directConnectGatewayId,
    describeDirectConnectGatewayAssociationProposals_maxResults,
    describeDirectConnectGatewayAssociationProposals_nextToken,
    describeDirectConnectGatewayAssociationProposals_proposalId,
    describeDirectConnectGatewayAssociationProposalsResponse_directConnectGatewayAssociationProposals,
    describeDirectConnectGatewayAssociationProposalsResponse_nextToken,
    describeDirectConnectGatewayAssociationProposalsResponse_httpStatus,

    -- ** DescribeDirectConnectGatewayAssociations
    describeDirectConnectGatewayAssociations_associatedGatewayId,
    describeDirectConnectGatewayAssociations_associationId,
    describeDirectConnectGatewayAssociations_directConnectGatewayId,
    describeDirectConnectGatewayAssociations_maxResults,
    describeDirectConnectGatewayAssociations_nextToken,
    describeDirectConnectGatewayAssociations_virtualGatewayId,
    describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations,
    describeDirectConnectGatewayAssociationsResponse_nextToken,
    describeDirectConnectGatewayAssociationsResponse_httpStatus,

    -- ** DescribeDirectConnectGatewayAttachments
    describeDirectConnectGatewayAttachments_directConnectGatewayId,
    describeDirectConnectGatewayAttachments_maxResults,
    describeDirectConnectGatewayAttachments_nextToken,
    describeDirectConnectGatewayAttachments_virtualInterfaceId,
    describeDirectConnectGatewayAttachmentsResponse_directConnectGatewayAttachments,
    describeDirectConnectGatewayAttachmentsResponse_nextToken,
    describeDirectConnectGatewayAttachmentsResponse_httpStatus,

    -- ** DescribeDirectConnectGateways
    describeDirectConnectGateways_directConnectGatewayId,
    describeDirectConnectGateways_maxResults,
    describeDirectConnectGateways_nextToken,
    describeDirectConnectGatewaysResponse_directConnectGateways,
    describeDirectConnectGatewaysResponse_nextToken,
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
    describeLoa_loaContentType,
    describeLoa_providerName,
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
    describeRouterConfigurationResponse_customerRouterConfig,
    describeRouterConfigurationResponse_router,
    describeRouterConfigurationResponse_virtualInterfaceId,
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
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,

    -- ** DisassociateMacSecKey
    disassociateMacSecKey_connectionId,
    disassociateMacSecKey_secretARN,
    disassociateMacSecKeyResponse_connectionId,
    disassociateMacSecKeyResponse_macSecKeys,
    disassociateMacSecKeyResponse_httpStatus,

    -- ** ListVirtualInterfaceTestHistory
    listVirtualInterfaceTestHistory_bgpPeers,
    listVirtualInterfaceTestHistory_maxResults,
    listVirtualInterfaceTestHistory_nextToken,
    listVirtualInterfaceTestHistory_status,
    listVirtualInterfaceTestHistory_testId,
    listVirtualInterfaceTestHistory_virtualInterfaceId,
    listVirtualInterfaceTestHistoryResponse_nextToken,
    listVirtualInterfaceTestHistoryResponse_virtualInterfaceTestHistory,
    listVirtualInterfaceTestHistoryResponse_httpStatus,

    -- ** StartBgpFailoverTest
    startBgpFailoverTest_bgpPeers,
    startBgpFailoverTest_testDurationInMinutes,
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
    updateConnection_connectionName,
    updateConnection_encryptionMode,
    updateConnection_connectionId,
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,

    -- ** UpdateDirectConnectGateway
    updateDirectConnectGateway_directConnectGatewayId,
    updateDirectConnectGateway_newDirectConnectGatewayName,
    updateDirectConnectGatewayResponse_directConnectGateway,
    updateDirectConnectGatewayResponse_httpStatus,

    -- ** UpdateDirectConnectGatewayAssociation
    updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociation_associationId,
    updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    updateDirectConnectGatewayAssociationResponse_httpStatus,

    -- ** UpdateLag
    updateLag_encryptionMode,
    updateLag_lagName,
    updateLag_minimumLinks,
    updateLag_lagId,
    lag_allowsHostedConnections,
    lag_awsDevice,
    lag_awsDeviceV2,
    lag_awsLogicalDeviceId,
    lag_connections,
    lag_connectionsBandwidth,
    lag_encryptionMode,
    lag_hasLogicalRedundancy,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_lagName,
    lag_lagState,
    lag_location,
    lag_macSecCapable,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_numberOfConnections,
    lag_ownerAccount,
    lag_providerName,
    lag_region,
    lag_tags,

    -- ** UpdateVirtualInterfaceAttributes
    updateVirtualInterfaceAttributes_enableSiteLink,
    updateVirtualInterfaceAttributes_mtu,
    updateVirtualInterfaceAttributes_virtualInterfaceName,
    updateVirtualInterfaceAttributes_virtualInterfaceId,
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_amazonSideAsn,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_awsDeviceV2,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_bgpPeers,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_customerRouterConfig,
    virtualInterface_directConnectGatewayId,
    virtualInterface_jumboFrameCapable,
    virtualInterface_location,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_siteLinkEnabled,
    virtualInterface_tags,
    virtualInterface_virtualGatewayId,
    virtualInterface_virtualInterfaceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceType,
    virtualInterface_vlan,

    -- * Types

    -- ** AssociatedGateway
    associatedGateway_id,
    associatedGateway_ownerAccount,
    associatedGateway_region,
    associatedGateway_type,

    -- ** BGPPeer
    bGPPeer_addressFamily,
    bGPPeer_amazonAddress,
    bGPPeer_asn,
    bGPPeer_authKey,
    bGPPeer_awsDeviceV2,
    bGPPeer_awsLogicalDeviceId,
    bGPPeer_bgpPeerId,
    bGPPeer_bgpPeerState,
    bGPPeer_bgpStatus,
    bGPPeer_customerAddress,

    -- ** Connection
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,

    -- ** Connections
    connections_connections,

    -- ** CustomerAgreement
    customerAgreement_agreementName,
    customerAgreement_status,

    -- ** DirectConnectGateway
    directConnectGateway_amazonSideAsn,
    directConnectGateway_directConnectGatewayId,
    directConnectGateway_directConnectGatewayName,
    directConnectGateway_directConnectGatewayState,
    directConnectGateway_ownerAccount,
    directConnectGateway_stateChangeError,

    -- ** DirectConnectGatewayAssociation
    directConnectGatewayAssociation_allowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociation_associatedGateway,
    directConnectGatewayAssociation_associationId,
    directConnectGatewayAssociation_associationState,
    directConnectGatewayAssociation_directConnectGatewayId,
    directConnectGatewayAssociation_directConnectGatewayOwnerAccount,
    directConnectGatewayAssociation_stateChangeError,
    directConnectGatewayAssociation_virtualGatewayId,
    directConnectGatewayAssociation_virtualGatewayOwnerAccount,
    directConnectGatewayAssociation_virtualGatewayRegion,

    -- ** DirectConnectGatewayAssociationProposal
    directConnectGatewayAssociationProposal_associatedGateway,
    directConnectGatewayAssociationProposal_directConnectGatewayId,
    directConnectGatewayAssociationProposal_directConnectGatewayOwnerAccount,
    directConnectGatewayAssociationProposal_existingAllowedPrefixesToDirectConnectGateway,
    directConnectGatewayAssociationProposal_proposalId,
    directConnectGatewayAssociationProposal_proposalState,
    directConnectGatewayAssociationProposal_requestedAllowedPrefixesToDirectConnectGateway,

    -- ** DirectConnectGatewayAttachment
    directConnectGatewayAttachment_attachmentState,
    directConnectGatewayAttachment_attachmentType,
    directConnectGatewayAttachment_directConnectGatewayId,
    directConnectGatewayAttachment_stateChangeError,
    directConnectGatewayAttachment_virtualInterfaceId,
    directConnectGatewayAttachment_virtualInterfaceOwnerAccount,
    directConnectGatewayAttachment_virtualInterfaceRegion,

    -- ** Interconnect
    interconnect_awsDevice,
    interconnect_awsDeviceV2,
    interconnect_awsLogicalDeviceId,
    interconnect_bandwidth,
    interconnect_hasLogicalRedundancy,
    interconnect_interconnectId,
    interconnect_interconnectName,
    interconnect_interconnectState,
    interconnect_jumboFrameCapable,
    interconnect_lagId,
    interconnect_loaIssueTime,
    interconnect_location,
    interconnect_providerName,
    interconnect_region,
    interconnect_tags,

    -- ** Lag
    lag_allowsHostedConnections,
    lag_awsDevice,
    lag_awsDeviceV2,
    lag_awsLogicalDeviceId,
    lag_connections,
    lag_connectionsBandwidth,
    lag_encryptionMode,
    lag_hasLogicalRedundancy,
    lag_jumboFrameCapable,
    lag_lagId,
    lag_lagName,
    lag_lagState,
    lag_location,
    lag_macSecCapable,
    lag_macSecKeys,
    lag_minimumLinks,
    lag_numberOfConnections,
    lag_ownerAccount,
    lag_providerName,
    lag_region,
    lag_tags,

    -- ** Location
    location_availableMacSecPortSpeeds,
    location_availablePortSpeeds,
    location_availableProviders,
    location_locationCode,
    location_locationName,
    location_region,

    -- ** MacSecKey
    macSecKey_ckn,
    macSecKey_secretARN,
    macSecKey_startOn,
    macSecKey_state,

    -- ** NewBGPPeer
    newBGPPeer_addressFamily,
    newBGPPeer_amazonAddress,
    newBGPPeer_asn,
    newBGPPeer_authKey,
    newBGPPeer_customerAddress,

    -- ** NewPrivateVirtualInterface
    newPrivateVirtualInterface_addressFamily,
    newPrivateVirtualInterface_amazonAddress,
    newPrivateVirtualInterface_authKey,
    newPrivateVirtualInterface_customerAddress,
    newPrivateVirtualInterface_directConnectGatewayId,
    newPrivateVirtualInterface_enableSiteLink,
    newPrivateVirtualInterface_mtu,
    newPrivateVirtualInterface_tags,
    newPrivateVirtualInterface_virtualGatewayId,
    newPrivateVirtualInterface_virtualInterfaceName,
    newPrivateVirtualInterface_vlan,
    newPrivateVirtualInterface_asn,

    -- ** NewPrivateVirtualInterfaceAllocation
    newPrivateVirtualInterfaceAllocation_addressFamily,
    newPrivateVirtualInterfaceAllocation_amazonAddress,
    newPrivateVirtualInterfaceAllocation_authKey,
    newPrivateVirtualInterfaceAllocation_customerAddress,
    newPrivateVirtualInterfaceAllocation_mtu,
    newPrivateVirtualInterfaceAllocation_tags,
    newPrivateVirtualInterfaceAllocation_virtualInterfaceName,
    newPrivateVirtualInterfaceAllocation_vlan,
    newPrivateVirtualInterfaceAllocation_asn,

    -- ** NewPublicVirtualInterface
    newPublicVirtualInterface_addressFamily,
    newPublicVirtualInterface_amazonAddress,
    newPublicVirtualInterface_authKey,
    newPublicVirtualInterface_customerAddress,
    newPublicVirtualInterface_routeFilterPrefixes,
    newPublicVirtualInterface_tags,
    newPublicVirtualInterface_virtualInterfaceName,
    newPublicVirtualInterface_vlan,
    newPublicVirtualInterface_asn,

    -- ** NewPublicVirtualInterfaceAllocation
    newPublicVirtualInterfaceAllocation_addressFamily,
    newPublicVirtualInterfaceAllocation_amazonAddress,
    newPublicVirtualInterfaceAllocation_authKey,
    newPublicVirtualInterfaceAllocation_customerAddress,
    newPublicVirtualInterfaceAllocation_routeFilterPrefixes,
    newPublicVirtualInterfaceAllocation_tags,
    newPublicVirtualInterfaceAllocation_virtualInterfaceName,
    newPublicVirtualInterfaceAllocation_vlan,
    newPublicVirtualInterfaceAllocation_asn,

    -- ** NewTransitVirtualInterface
    newTransitVirtualInterface_addressFamily,
    newTransitVirtualInterface_amazonAddress,
    newTransitVirtualInterface_asn,
    newTransitVirtualInterface_authKey,
    newTransitVirtualInterface_customerAddress,
    newTransitVirtualInterface_directConnectGatewayId,
    newTransitVirtualInterface_enableSiteLink,
    newTransitVirtualInterface_mtu,
    newTransitVirtualInterface_tags,
    newTransitVirtualInterface_virtualInterfaceName,
    newTransitVirtualInterface_vlan,

    -- ** NewTransitVirtualInterfaceAllocation
    newTransitVirtualInterfaceAllocation_addressFamily,
    newTransitVirtualInterfaceAllocation_amazonAddress,
    newTransitVirtualInterfaceAllocation_asn,
    newTransitVirtualInterfaceAllocation_authKey,
    newTransitVirtualInterfaceAllocation_customerAddress,
    newTransitVirtualInterfaceAllocation_mtu,
    newTransitVirtualInterfaceAllocation_tags,
    newTransitVirtualInterfaceAllocation_virtualInterfaceName,
    newTransitVirtualInterfaceAllocation_vlan,

    -- ** ResourceTag
    resourceTag_resourceArn,
    resourceTag_tags,

    -- ** RouteFilterPrefix
    routeFilterPrefix_cidr,

    -- ** RouterType
    routerType_platform,
    routerType_routerTypeIdentifier,
    routerType_software,
    routerType_vendor,
    routerType_xsltTemplateName,
    routerType_xsltTemplateNameForMacSec,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** VirtualGateway
    virtualGateway_virtualGatewayId,
    virtualGateway_virtualGatewayState,

    -- ** VirtualInterface
    virtualInterface_addressFamily,
    virtualInterface_amazonAddress,
    virtualInterface_amazonSideAsn,
    virtualInterface_asn,
    virtualInterface_authKey,
    virtualInterface_awsDeviceV2,
    virtualInterface_awsLogicalDeviceId,
    virtualInterface_bgpPeers,
    virtualInterface_connectionId,
    virtualInterface_customerAddress,
    virtualInterface_customerRouterConfig,
    virtualInterface_directConnectGatewayId,
    virtualInterface_jumboFrameCapable,
    virtualInterface_location,
    virtualInterface_mtu,
    virtualInterface_ownerAccount,
    virtualInterface_region,
    virtualInterface_routeFilterPrefixes,
    virtualInterface_siteLinkEnabled,
    virtualInterface_tags,
    virtualInterface_virtualGatewayId,
    virtualInterface_virtualInterfaceId,
    virtualInterface_virtualInterfaceName,
    virtualInterface_virtualInterfaceState,
    virtualInterface_virtualInterfaceType,
    virtualInterface_vlan,

    -- ** VirtualInterfaceTestHistory
    virtualInterfaceTestHistory_bgpPeers,
    virtualInterfaceTestHistory_endTime,
    virtualInterfaceTestHistory_ownerAccount,
    virtualInterfaceTestHistory_startTime,
    virtualInterfaceTestHistory_status,
    virtualInterfaceTestHistory_testDurationInMinutes,
    virtualInterfaceTestHistory_testId,
    virtualInterfaceTestHistory_virtualInterfaceId,
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

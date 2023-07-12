{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.NetworkManager.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Lens
  ( -- * Operations

    -- ** AcceptAttachment
    acceptAttachment_attachmentId,
    acceptAttachmentResponse_attachment,
    acceptAttachmentResponse_httpStatus,

    -- ** AssociateConnectPeer
    associateConnectPeer_linkId,
    associateConnectPeer_globalNetworkId,
    associateConnectPeer_connectPeerId,
    associateConnectPeer_deviceId,
    associateConnectPeerResponse_connectPeerAssociation,
    associateConnectPeerResponse_httpStatus,

    -- ** AssociateCustomerGateway
    associateCustomerGateway_linkId,
    associateCustomerGateway_customerGatewayArn,
    associateCustomerGateway_globalNetworkId,
    associateCustomerGateway_deviceId,
    associateCustomerGatewayResponse_customerGatewayAssociation,
    associateCustomerGatewayResponse_httpStatus,

    -- ** AssociateLink
    associateLink_globalNetworkId,
    associateLink_deviceId,
    associateLink_linkId,
    associateLinkResponse_linkAssociation,
    associateLinkResponse_httpStatus,

    -- ** AssociateTransitGatewayConnectPeer
    associateTransitGatewayConnectPeer_linkId,
    associateTransitGatewayConnectPeer_globalNetworkId,
    associateTransitGatewayConnectPeer_transitGatewayConnectPeerArn,
    associateTransitGatewayConnectPeer_deviceId,
    associateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation,
    associateTransitGatewayConnectPeerResponse_httpStatus,

    -- ** CreateConnectAttachment
    createConnectAttachment_clientToken,
    createConnectAttachment_tags,
    createConnectAttachment_coreNetworkId,
    createConnectAttachment_edgeLocation,
    createConnectAttachment_transportAttachmentId,
    createConnectAttachment_options,
    createConnectAttachmentResponse_connectAttachment,
    createConnectAttachmentResponse_httpStatus,

    -- ** CreateConnectPeer
    createConnectPeer_bgpOptions,
    createConnectPeer_clientToken,
    createConnectPeer_coreNetworkAddress,
    createConnectPeer_tags,
    createConnectPeer_connectAttachmentId,
    createConnectPeer_peerAddress,
    createConnectPeer_insideCidrBlocks,
    createConnectPeerResponse_connectPeer,
    createConnectPeerResponse_httpStatus,

    -- ** CreateConnection
    createConnection_connectedLinkId,
    createConnection_description,
    createConnection_linkId,
    createConnection_tags,
    createConnection_globalNetworkId,
    createConnection_deviceId,
    createConnection_connectedDeviceId,
    createConnectionResponse_connection,
    createConnectionResponse_httpStatus,

    -- ** CreateCoreNetwork
    createCoreNetwork_clientToken,
    createCoreNetwork_description,
    createCoreNetwork_policyDocument,
    createCoreNetwork_tags,
    createCoreNetwork_globalNetworkId,
    createCoreNetworkResponse_coreNetwork,
    createCoreNetworkResponse_httpStatus,

    -- ** CreateDevice
    createDevice_aWSLocation,
    createDevice_description,
    createDevice_location,
    createDevice_model,
    createDevice_serialNumber,
    createDevice_siteId,
    createDevice_tags,
    createDevice_type,
    createDevice_vendor,
    createDevice_globalNetworkId,
    createDeviceResponse_device,
    createDeviceResponse_httpStatus,

    -- ** CreateGlobalNetwork
    createGlobalNetwork_description,
    createGlobalNetwork_tags,
    createGlobalNetworkResponse_globalNetwork,
    createGlobalNetworkResponse_httpStatus,

    -- ** CreateLink
    createLink_description,
    createLink_provider,
    createLink_tags,
    createLink_type,
    createLink_globalNetworkId,
    createLink_bandwidth,
    createLink_siteId,
    createLinkResponse_link,
    createLinkResponse_httpStatus,

    -- ** CreateSite
    createSite_description,
    createSite_location,
    createSite_tags,
    createSite_globalNetworkId,
    createSiteResponse_site,
    createSiteResponse_httpStatus,

    -- ** CreateSiteToSiteVpnAttachment
    createSiteToSiteVpnAttachment_clientToken,
    createSiteToSiteVpnAttachment_tags,
    createSiteToSiteVpnAttachment_coreNetworkId,
    createSiteToSiteVpnAttachment_vpnConnectionArn,
    createSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment,
    createSiteToSiteVpnAttachmentResponse_httpStatus,

    -- ** CreateTransitGatewayPeering
    createTransitGatewayPeering_clientToken,
    createTransitGatewayPeering_tags,
    createTransitGatewayPeering_coreNetworkId,
    createTransitGatewayPeering_transitGatewayArn,
    createTransitGatewayPeeringResponse_transitGatewayPeering,
    createTransitGatewayPeeringResponse_httpStatus,

    -- ** CreateTransitGatewayRouteTableAttachment
    createTransitGatewayRouteTableAttachment_clientToken,
    createTransitGatewayRouteTableAttachment_tags,
    createTransitGatewayRouteTableAttachment_peeringId,
    createTransitGatewayRouteTableAttachment_transitGatewayRouteTableArn,
    createTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment,
    createTransitGatewayRouteTableAttachmentResponse_httpStatus,

    -- ** CreateVpcAttachment
    createVpcAttachment_clientToken,
    createVpcAttachment_options,
    createVpcAttachment_tags,
    createVpcAttachment_coreNetworkId,
    createVpcAttachment_vpcArn,
    createVpcAttachment_subnetArns,
    createVpcAttachmentResponse_vpcAttachment,
    createVpcAttachmentResponse_httpStatus,

    -- ** DeleteAttachment
    deleteAttachment_attachmentId,
    deleteAttachmentResponse_attachment,
    deleteAttachmentResponse_httpStatus,

    -- ** DeleteConnectPeer
    deleteConnectPeer_connectPeerId,
    deleteConnectPeerResponse_connectPeer,
    deleteConnectPeerResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_globalNetworkId,
    deleteConnection_connectionId,
    deleteConnectionResponse_connection,
    deleteConnectionResponse_httpStatus,

    -- ** DeleteCoreNetwork
    deleteCoreNetwork_coreNetworkId,
    deleteCoreNetworkResponse_coreNetwork,
    deleteCoreNetworkResponse_httpStatus,

    -- ** DeleteCoreNetworkPolicyVersion
    deleteCoreNetworkPolicyVersion_coreNetworkId,
    deleteCoreNetworkPolicyVersion_policyVersionId,
    deleteCoreNetworkPolicyVersionResponse_coreNetworkPolicy,
    deleteCoreNetworkPolicyVersionResponse_httpStatus,

    -- ** DeleteDevice
    deleteDevice_globalNetworkId,
    deleteDevice_deviceId,
    deleteDeviceResponse_device,
    deleteDeviceResponse_httpStatus,

    -- ** DeleteGlobalNetwork
    deleteGlobalNetwork_globalNetworkId,
    deleteGlobalNetworkResponse_globalNetwork,
    deleteGlobalNetworkResponse_httpStatus,

    -- ** DeleteLink
    deleteLink_globalNetworkId,
    deleteLink_linkId,
    deleteLinkResponse_link,
    deleteLinkResponse_httpStatus,

    -- ** DeletePeering
    deletePeering_peeringId,
    deletePeeringResponse_peering,
    deletePeeringResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteSite
    deleteSite_globalNetworkId,
    deleteSite_siteId,
    deleteSiteResponse_site,
    deleteSiteResponse_httpStatus,

    -- ** DeregisterTransitGateway
    deregisterTransitGateway_globalNetworkId,
    deregisterTransitGateway_transitGatewayArn,
    deregisterTransitGatewayResponse_transitGatewayRegistration,
    deregisterTransitGatewayResponse_httpStatus,

    -- ** DescribeGlobalNetworks
    describeGlobalNetworks_globalNetworkIds,
    describeGlobalNetworks_maxResults,
    describeGlobalNetworks_nextToken,
    describeGlobalNetworksResponse_globalNetworks,
    describeGlobalNetworksResponse_nextToken,
    describeGlobalNetworksResponse_httpStatus,

    -- ** DisassociateConnectPeer
    disassociateConnectPeer_globalNetworkId,
    disassociateConnectPeer_connectPeerId,
    disassociateConnectPeerResponse_connectPeerAssociation,
    disassociateConnectPeerResponse_httpStatus,

    -- ** DisassociateCustomerGateway
    disassociateCustomerGateway_globalNetworkId,
    disassociateCustomerGateway_customerGatewayArn,
    disassociateCustomerGatewayResponse_customerGatewayAssociation,
    disassociateCustomerGatewayResponse_httpStatus,

    -- ** DisassociateLink
    disassociateLink_globalNetworkId,
    disassociateLink_deviceId,
    disassociateLink_linkId,
    disassociateLinkResponse_linkAssociation,
    disassociateLinkResponse_httpStatus,

    -- ** DisassociateTransitGatewayConnectPeer
    disassociateTransitGatewayConnectPeer_globalNetworkId,
    disassociateTransitGatewayConnectPeer_transitGatewayConnectPeerArn,
    disassociateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation,
    disassociateTransitGatewayConnectPeerResponse_httpStatus,

    -- ** ExecuteCoreNetworkChangeSet
    executeCoreNetworkChangeSet_coreNetworkId,
    executeCoreNetworkChangeSet_policyVersionId,
    executeCoreNetworkChangeSetResponse_httpStatus,

    -- ** GetConnectAttachment
    getConnectAttachment_attachmentId,
    getConnectAttachmentResponse_connectAttachment,
    getConnectAttachmentResponse_httpStatus,

    -- ** GetConnectPeer
    getConnectPeer_connectPeerId,
    getConnectPeerResponse_connectPeer,
    getConnectPeerResponse_httpStatus,

    -- ** GetConnectPeerAssociations
    getConnectPeerAssociations_connectPeerIds,
    getConnectPeerAssociations_maxResults,
    getConnectPeerAssociations_nextToken,
    getConnectPeerAssociations_globalNetworkId,
    getConnectPeerAssociationsResponse_connectPeerAssociations,
    getConnectPeerAssociationsResponse_nextToken,
    getConnectPeerAssociationsResponse_httpStatus,

    -- ** GetConnections
    getConnections_connectionIds,
    getConnections_deviceId,
    getConnections_maxResults,
    getConnections_nextToken,
    getConnections_globalNetworkId,
    getConnectionsResponse_connections,
    getConnectionsResponse_nextToken,
    getConnectionsResponse_httpStatus,

    -- ** GetCoreNetwork
    getCoreNetwork_coreNetworkId,
    getCoreNetworkResponse_coreNetwork,
    getCoreNetworkResponse_httpStatus,

    -- ** GetCoreNetworkChangeEvents
    getCoreNetworkChangeEvents_maxResults,
    getCoreNetworkChangeEvents_nextToken,
    getCoreNetworkChangeEvents_coreNetworkId,
    getCoreNetworkChangeEvents_policyVersionId,
    getCoreNetworkChangeEventsResponse_coreNetworkChangeEvents,
    getCoreNetworkChangeEventsResponse_nextToken,
    getCoreNetworkChangeEventsResponse_httpStatus,

    -- ** GetCoreNetworkChangeSet
    getCoreNetworkChangeSet_maxResults,
    getCoreNetworkChangeSet_nextToken,
    getCoreNetworkChangeSet_coreNetworkId,
    getCoreNetworkChangeSet_policyVersionId,
    getCoreNetworkChangeSetResponse_coreNetworkChanges,
    getCoreNetworkChangeSetResponse_nextToken,
    getCoreNetworkChangeSetResponse_httpStatus,

    -- ** GetCoreNetworkPolicy
    getCoreNetworkPolicy_alias,
    getCoreNetworkPolicy_policyVersionId,
    getCoreNetworkPolicy_coreNetworkId,
    getCoreNetworkPolicyResponse_coreNetworkPolicy,
    getCoreNetworkPolicyResponse_httpStatus,

    -- ** GetCustomerGatewayAssociations
    getCustomerGatewayAssociations_customerGatewayArns,
    getCustomerGatewayAssociations_maxResults,
    getCustomerGatewayAssociations_nextToken,
    getCustomerGatewayAssociations_globalNetworkId,
    getCustomerGatewayAssociationsResponse_customerGatewayAssociations,
    getCustomerGatewayAssociationsResponse_nextToken,
    getCustomerGatewayAssociationsResponse_httpStatus,

    -- ** GetDevices
    getDevices_deviceIds,
    getDevices_maxResults,
    getDevices_nextToken,
    getDevices_siteId,
    getDevices_globalNetworkId,
    getDevicesResponse_devices,
    getDevicesResponse_nextToken,
    getDevicesResponse_httpStatus,

    -- ** GetLinkAssociations
    getLinkAssociations_deviceId,
    getLinkAssociations_linkId,
    getLinkAssociations_maxResults,
    getLinkAssociations_nextToken,
    getLinkAssociations_globalNetworkId,
    getLinkAssociationsResponse_linkAssociations,
    getLinkAssociationsResponse_nextToken,
    getLinkAssociationsResponse_httpStatus,

    -- ** GetLinks
    getLinks_linkIds,
    getLinks_maxResults,
    getLinks_nextToken,
    getLinks_provider,
    getLinks_siteId,
    getLinks_type,
    getLinks_globalNetworkId,
    getLinksResponse_links,
    getLinksResponse_nextToken,
    getLinksResponse_httpStatus,

    -- ** GetNetworkResourceCounts
    getNetworkResourceCounts_maxResults,
    getNetworkResourceCounts_nextToken,
    getNetworkResourceCounts_resourceType,
    getNetworkResourceCounts_globalNetworkId,
    getNetworkResourceCountsResponse_networkResourceCounts,
    getNetworkResourceCountsResponse_nextToken,
    getNetworkResourceCountsResponse_httpStatus,

    -- ** GetNetworkResourceRelationships
    getNetworkResourceRelationships_accountId,
    getNetworkResourceRelationships_awsRegion,
    getNetworkResourceRelationships_coreNetworkId,
    getNetworkResourceRelationships_maxResults,
    getNetworkResourceRelationships_nextToken,
    getNetworkResourceRelationships_registeredGatewayArn,
    getNetworkResourceRelationships_resourceArn,
    getNetworkResourceRelationships_resourceType,
    getNetworkResourceRelationships_globalNetworkId,
    getNetworkResourceRelationshipsResponse_nextToken,
    getNetworkResourceRelationshipsResponse_relationships,
    getNetworkResourceRelationshipsResponse_httpStatus,

    -- ** GetNetworkResources
    getNetworkResources_accountId,
    getNetworkResources_awsRegion,
    getNetworkResources_coreNetworkId,
    getNetworkResources_maxResults,
    getNetworkResources_nextToken,
    getNetworkResources_registeredGatewayArn,
    getNetworkResources_resourceArn,
    getNetworkResources_resourceType,
    getNetworkResources_globalNetworkId,
    getNetworkResourcesResponse_networkResources,
    getNetworkResourcesResponse_nextToken,
    getNetworkResourcesResponse_httpStatus,

    -- ** GetNetworkRoutes
    getNetworkRoutes_destinationFilters,
    getNetworkRoutes_exactCidrMatches,
    getNetworkRoutes_longestPrefixMatches,
    getNetworkRoutes_prefixListIds,
    getNetworkRoutes_states,
    getNetworkRoutes_subnetOfMatches,
    getNetworkRoutes_supernetOfMatches,
    getNetworkRoutes_types,
    getNetworkRoutes_globalNetworkId,
    getNetworkRoutes_routeTableIdentifier,
    getNetworkRoutesResponse_coreNetworkSegmentEdge,
    getNetworkRoutesResponse_networkRoutes,
    getNetworkRoutesResponse_routeTableArn,
    getNetworkRoutesResponse_routeTableTimestamp,
    getNetworkRoutesResponse_routeTableType,
    getNetworkRoutesResponse_httpStatus,

    -- ** GetNetworkTelemetry
    getNetworkTelemetry_accountId,
    getNetworkTelemetry_awsRegion,
    getNetworkTelemetry_coreNetworkId,
    getNetworkTelemetry_maxResults,
    getNetworkTelemetry_nextToken,
    getNetworkTelemetry_registeredGatewayArn,
    getNetworkTelemetry_resourceArn,
    getNetworkTelemetry_resourceType,
    getNetworkTelemetry_globalNetworkId,
    getNetworkTelemetryResponse_networkTelemetry,
    getNetworkTelemetryResponse_nextToken,
    getNetworkTelemetryResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_resourceArn,
    getResourcePolicyResponse_policyDocument,
    getResourcePolicyResponse_httpStatus,

    -- ** GetRouteAnalysis
    getRouteAnalysis_globalNetworkId,
    getRouteAnalysis_routeAnalysisId,
    getRouteAnalysisResponse_routeAnalysis,
    getRouteAnalysisResponse_httpStatus,

    -- ** GetSiteToSiteVpnAttachment
    getSiteToSiteVpnAttachment_attachmentId,
    getSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment,
    getSiteToSiteVpnAttachmentResponse_httpStatus,

    -- ** GetSites
    getSites_maxResults,
    getSites_nextToken,
    getSites_siteIds,
    getSites_globalNetworkId,
    getSitesResponse_nextToken,
    getSitesResponse_sites,
    getSitesResponse_httpStatus,

    -- ** GetTransitGatewayConnectPeerAssociations
    getTransitGatewayConnectPeerAssociations_maxResults,
    getTransitGatewayConnectPeerAssociations_nextToken,
    getTransitGatewayConnectPeerAssociations_transitGatewayConnectPeerArns,
    getTransitGatewayConnectPeerAssociations_globalNetworkId,
    getTransitGatewayConnectPeerAssociationsResponse_nextToken,
    getTransitGatewayConnectPeerAssociationsResponse_transitGatewayConnectPeerAssociations,
    getTransitGatewayConnectPeerAssociationsResponse_httpStatus,

    -- ** GetTransitGatewayPeering
    getTransitGatewayPeering_peeringId,
    getTransitGatewayPeeringResponse_transitGatewayPeering,
    getTransitGatewayPeeringResponse_httpStatus,

    -- ** GetTransitGatewayRegistrations
    getTransitGatewayRegistrations_maxResults,
    getTransitGatewayRegistrations_nextToken,
    getTransitGatewayRegistrations_transitGatewayArns,
    getTransitGatewayRegistrations_globalNetworkId,
    getTransitGatewayRegistrationsResponse_nextToken,
    getTransitGatewayRegistrationsResponse_transitGatewayRegistrations,
    getTransitGatewayRegistrationsResponse_httpStatus,

    -- ** GetTransitGatewayRouteTableAttachment
    getTransitGatewayRouteTableAttachment_attachmentId,
    getTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment,
    getTransitGatewayRouteTableAttachmentResponse_httpStatus,

    -- ** GetVpcAttachment
    getVpcAttachment_attachmentId,
    getVpcAttachmentResponse_vpcAttachment,
    getVpcAttachmentResponse_httpStatus,

    -- ** ListAttachments
    listAttachments_attachmentType,
    listAttachments_coreNetworkId,
    listAttachments_edgeLocation,
    listAttachments_maxResults,
    listAttachments_nextToken,
    listAttachments_state,
    listAttachmentsResponse_attachments,
    listAttachmentsResponse_nextToken,
    listAttachmentsResponse_httpStatus,

    -- ** ListConnectPeers
    listConnectPeers_connectAttachmentId,
    listConnectPeers_coreNetworkId,
    listConnectPeers_maxResults,
    listConnectPeers_nextToken,
    listConnectPeersResponse_connectPeers,
    listConnectPeersResponse_nextToken,
    listConnectPeersResponse_httpStatus,

    -- ** ListCoreNetworkPolicyVersions
    listCoreNetworkPolicyVersions_maxResults,
    listCoreNetworkPolicyVersions_nextToken,
    listCoreNetworkPolicyVersions_coreNetworkId,
    listCoreNetworkPolicyVersionsResponse_coreNetworkPolicyVersions,
    listCoreNetworkPolicyVersionsResponse_nextToken,
    listCoreNetworkPolicyVersionsResponse_httpStatus,

    -- ** ListCoreNetworks
    listCoreNetworks_maxResults,
    listCoreNetworks_nextToken,
    listCoreNetworksResponse_coreNetworks,
    listCoreNetworksResponse_nextToken,
    listCoreNetworksResponse_httpStatus,

    -- ** ListOrganizationServiceAccessStatus
    listOrganizationServiceAccessStatus_maxResults,
    listOrganizationServiceAccessStatus_nextToken,
    listOrganizationServiceAccessStatusResponse_nextToken,
    listOrganizationServiceAccessStatusResponse_organizationStatus,
    listOrganizationServiceAccessStatusResponse_httpStatus,

    -- ** ListPeerings
    listPeerings_coreNetworkId,
    listPeerings_edgeLocation,
    listPeerings_maxResults,
    listPeerings_nextToken,
    listPeerings_peeringType,
    listPeerings_state,
    listPeeringsResponse_nextToken,
    listPeeringsResponse_peerings,
    listPeeringsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** PutCoreNetworkPolicy
    putCoreNetworkPolicy_clientToken,
    putCoreNetworkPolicy_description,
    putCoreNetworkPolicy_latestVersionId,
    putCoreNetworkPolicy_coreNetworkId,
    putCoreNetworkPolicy_policyDocument,
    putCoreNetworkPolicyResponse_coreNetworkPolicy,
    putCoreNetworkPolicyResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policyDocument,
    putResourcePolicy_resourceArn,
    putResourcePolicyResponse_httpStatus,

    -- ** RegisterTransitGateway
    registerTransitGateway_globalNetworkId,
    registerTransitGateway_transitGatewayArn,
    registerTransitGatewayResponse_transitGatewayRegistration,
    registerTransitGatewayResponse_httpStatus,

    -- ** RejectAttachment
    rejectAttachment_attachmentId,
    rejectAttachmentResponse_attachment,
    rejectAttachmentResponse_httpStatus,

    -- ** RestoreCoreNetworkPolicyVersion
    restoreCoreNetworkPolicyVersion_coreNetworkId,
    restoreCoreNetworkPolicyVersion_policyVersionId,
    restoreCoreNetworkPolicyVersionResponse_coreNetworkPolicy,
    restoreCoreNetworkPolicyVersionResponse_httpStatus,

    -- ** StartOrganizationServiceAccessUpdate
    startOrganizationServiceAccessUpdate_action,
    startOrganizationServiceAccessUpdateResponse_organizationStatus,
    startOrganizationServiceAccessUpdateResponse_httpStatus,

    -- ** StartRouteAnalysis
    startRouteAnalysis_includeReturnPath,
    startRouteAnalysis_useMiddleboxes,
    startRouteAnalysis_globalNetworkId,
    startRouteAnalysis_source,
    startRouteAnalysis_destination,
    startRouteAnalysisResponse_routeAnalysis,
    startRouteAnalysisResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateConnection
    updateConnection_connectedLinkId,
    updateConnection_description,
    updateConnection_linkId,
    updateConnection_globalNetworkId,
    updateConnection_connectionId,
    updateConnectionResponse_connection,
    updateConnectionResponse_httpStatus,

    -- ** UpdateCoreNetwork
    updateCoreNetwork_description,
    updateCoreNetwork_coreNetworkId,
    updateCoreNetworkResponse_coreNetwork,
    updateCoreNetworkResponse_httpStatus,

    -- ** UpdateDevice
    updateDevice_aWSLocation,
    updateDevice_description,
    updateDevice_location,
    updateDevice_model,
    updateDevice_serialNumber,
    updateDevice_siteId,
    updateDevice_type,
    updateDevice_vendor,
    updateDevice_globalNetworkId,
    updateDevice_deviceId,
    updateDeviceResponse_device,
    updateDeviceResponse_httpStatus,

    -- ** UpdateGlobalNetwork
    updateGlobalNetwork_description,
    updateGlobalNetwork_globalNetworkId,
    updateGlobalNetworkResponse_globalNetwork,
    updateGlobalNetworkResponse_httpStatus,

    -- ** UpdateLink
    updateLink_bandwidth,
    updateLink_description,
    updateLink_provider,
    updateLink_type,
    updateLink_globalNetworkId,
    updateLink_linkId,
    updateLinkResponse_link,
    updateLinkResponse_httpStatus,

    -- ** UpdateNetworkResourceMetadata
    updateNetworkResourceMetadata_globalNetworkId,
    updateNetworkResourceMetadata_resourceArn,
    updateNetworkResourceMetadata_metadata,
    updateNetworkResourceMetadataResponse_metadata,
    updateNetworkResourceMetadataResponse_resourceArn,
    updateNetworkResourceMetadataResponse_httpStatus,

    -- ** UpdateSite
    updateSite_description,
    updateSite_location,
    updateSite_globalNetworkId,
    updateSite_siteId,
    updateSiteResponse_site,
    updateSiteResponse_httpStatus,

    -- ** UpdateVpcAttachment
    updateVpcAttachment_addSubnetArns,
    updateVpcAttachment_options,
    updateVpcAttachment_removeSubnetArns,
    updateVpcAttachment_attachmentId,
    updateVpcAttachmentResponse_vpcAttachment,
    updateVpcAttachmentResponse_httpStatus,

    -- * Types

    -- ** AWSLocation
    aWSLocation_subnetArn,
    aWSLocation_zone,

    -- ** AccountStatus
    accountStatus_accountId,
    accountStatus_sLRDeploymentStatus,

    -- ** Attachment
    attachment_attachmentId,
    attachment_attachmentPolicyRuleNumber,
    attachment_attachmentType,
    attachment_coreNetworkArn,
    attachment_coreNetworkId,
    attachment_createdAt,
    attachment_edgeLocation,
    attachment_ownerAccountId,
    attachment_proposedSegmentChange,
    attachment_resourceArn,
    attachment_segmentName,
    attachment_state,
    attachment_tags,
    attachment_updatedAt,

    -- ** Bandwidth
    bandwidth_downloadSpeed,
    bandwidth_uploadSpeed,

    -- ** BgpOptions
    bgpOptions_peerAsn,

    -- ** ConnectAttachment
    connectAttachment_attachment,
    connectAttachment_options,
    connectAttachment_transportAttachmentId,

    -- ** ConnectAttachmentOptions
    connectAttachmentOptions_protocol,

    -- ** ConnectPeer
    connectPeer_configuration,
    connectPeer_connectAttachmentId,
    connectPeer_connectPeerId,
    connectPeer_coreNetworkId,
    connectPeer_createdAt,
    connectPeer_edgeLocation,
    connectPeer_state,
    connectPeer_tags,

    -- ** ConnectPeerAssociation
    connectPeerAssociation_connectPeerId,
    connectPeerAssociation_deviceId,
    connectPeerAssociation_globalNetworkId,
    connectPeerAssociation_linkId,
    connectPeerAssociation_state,

    -- ** ConnectPeerBgpConfiguration
    connectPeerBgpConfiguration_coreNetworkAddress,
    connectPeerBgpConfiguration_coreNetworkAsn,
    connectPeerBgpConfiguration_peerAddress,
    connectPeerBgpConfiguration_peerAsn,

    -- ** ConnectPeerConfiguration
    connectPeerConfiguration_bgpConfigurations,
    connectPeerConfiguration_coreNetworkAddress,
    connectPeerConfiguration_insideCidrBlocks,
    connectPeerConfiguration_peerAddress,
    connectPeerConfiguration_protocol,

    -- ** ConnectPeerSummary
    connectPeerSummary_connectAttachmentId,
    connectPeerSummary_connectPeerId,
    connectPeerSummary_connectPeerState,
    connectPeerSummary_coreNetworkId,
    connectPeerSummary_createdAt,
    connectPeerSummary_edgeLocation,
    connectPeerSummary_tags,

    -- ** Connection
    connection_connectedDeviceId,
    connection_connectedLinkId,
    connection_connectionArn,
    connection_connectionId,
    connection_createdAt,
    connection_description,
    connection_deviceId,
    connection_globalNetworkId,
    connection_linkId,
    connection_state,
    connection_tags,

    -- ** ConnectionHealth
    connectionHealth_status,
    connectionHealth_timestamp,
    connectionHealth_type,

    -- ** CoreNetwork
    coreNetwork_coreNetworkArn,
    coreNetwork_coreNetworkId,
    coreNetwork_createdAt,
    coreNetwork_description,
    coreNetwork_edges,
    coreNetwork_globalNetworkId,
    coreNetwork_segments,
    coreNetwork_state,
    coreNetwork_tags,

    -- ** CoreNetworkChange
    coreNetworkChange_action,
    coreNetworkChange_identifier,
    coreNetworkChange_identifierPath,
    coreNetworkChange_newValues,
    coreNetworkChange_previousValues,
    coreNetworkChange_type,

    -- ** CoreNetworkChangeEvent
    coreNetworkChangeEvent_action,
    coreNetworkChangeEvent_eventTime,
    coreNetworkChangeEvent_identifierPath,
    coreNetworkChangeEvent_status,
    coreNetworkChangeEvent_type,
    coreNetworkChangeEvent_values,

    -- ** CoreNetworkChangeEventValues
    coreNetworkChangeEventValues_attachmentId,
    coreNetworkChangeEventValues_cidr,
    coreNetworkChangeEventValues_edgeLocation,
    coreNetworkChangeEventValues_segmentName,

    -- ** CoreNetworkChangeValues
    coreNetworkChangeValues_asn,
    coreNetworkChangeValues_cidr,
    coreNetworkChangeValues_destinationIdentifier,
    coreNetworkChangeValues_edgeLocations,
    coreNetworkChangeValues_insideCidrBlocks,
    coreNetworkChangeValues_segmentName,
    coreNetworkChangeValues_sharedSegments,

    -- ** CoreNetworkEdge
    coreNetworkEdge_asn,
    coreNetworkEdge_edgeLocation,
    coreNetworkEdge_insideCidrBlocks,

    -- ** CoreNetworkPolicy
    coreNetworkPolicy_alias,
    coreNetworkPolicy_changeSetState,
    coreNetworkPolicy_coreNetworkId,
    coreNetworkPolicy_createdAt,
    coreNetworkPolicy_description,
    coreNetworkPolicy_policyDocument,
    coreNetworkPolicy_policyErrors,
    coreNetworkPolicy_policyVersionId,

    -- ** CoreNetworkPolicyError
    coreNetworkPolicyError_path,
    coreNetworkPolicyError_errorCode,
    coreNetworkPolicyError_message,

    -- ** CoreNetworkPolicyVersion
    coreNetworkPolicyVersion_alias,
    coreNetworkPolicyVersion_changeSetState,
    coreNetworkPolicyVersion_coreNetworkId,
    coreNetworkPolicyVersion_createdAt,
    coreNetworkPolicyVersion_description,
    coreNetworkPolicyVersion_policyVersionId,

    -- ** CoreNetworkSegment
    coreNetworkSegment_edgeLocations,
    coreNetworkSegment_name,
    coreNetworkSegment_sharedSegments,

    -- ** CoreNetworkSegmentEdgeIdentifier
    coreNetworkSegmentEdgeIdentifier_coreNetworkId,
    coreNetworkSegmentEdgeIdentifier_edgeLocation,
    coreNetworkSegmentEdgeIdentifier_segmentName,

    -- ** CoreNetworkSummary
    coreNetworkSummary_coreNetworkArn,
    coreNetworkSummary_coreNetworkId,
    coreNetworkSummary_description,
    coreNetworkSummary_globalNetworkId,
    coreNetworkSummary_ownerAccountId,
    coreNetworkSummary_state,
    coreNetworkSummary_tags,

    -- ** CustomerGatewayAssociation
    customerGatewayAssociation_customerGatewayArn,
    customerGatewayAssociation_deviceId,
    customerGatewayAssociation_globalNetworkId,
    customerGatewayAssociation_linkId,
    customerGatewayAssociation_state,

    -- ** Device
    device_aWSLocation,
    device_createdAt,
    device_description,
    device_deviceArn,
    device_deviceId,
    device_globalNetworkId,
    device_location,
    device_model,
    device_serialNumber,
    device_siteId,
    device_state,
    device_tags,
    device_type,
    device_vendor,

    -- ** GlobalNetwork
    globalNetwork_createdAt,
    globalNetwork_description,
    globalNetwork_globalNetworkArn,
    globalNetwork_globalNetworkId,
    globalNetwork_state,
    globalNetwork_tags,

    -- ** Link
    link_bandwidth,
    link_createdAt,
    link_description,
    link_globalNetworkId,
    link_linkArn,
    link_linkId,
    link_provider,
    link_siteId,
    link_state,
    link_tags,
    link_type,

    -- ** LinkAssociation
    linkAssociation_deviceId,
    linkAssociation_globalNetworkId,
    linkAssociation_linkAssociationState,
    linkAssociation_linkId,

    -- ** Location
    location_address,
    location_latitude,
    location_longitude,

    -- ** NetworkResource
    networkResource_accountId,
    networkResource_awsRegion,
    networkResource_coreNetworkId,
    networkResource_definition,
    networkResource_definitionTimestamp,
    networkResource_metadata,
    networkResource_registeredGatewayArn,
    networkResource_resourceArn,
    networkResource_resourceId,
    networkResource_resourceType,
    networkResource_tags,

    -- ** NetworkResourceCount
    networkResourceCount_count,
    networkResourceCount_resourceType,

    -- ** NetworkResourceSummary
    networkResourceSummary_definition,
    networkResourceSummary_isMiddlebox,
    networkResourceSummary_nameTag,
    networkResourceSummary_registeredGatewayArn,
    networkResourceSummary_resourceArn,
    networkResourceSummary_resourceType,

    -- ** NetworkRoute
    networkRoute_destinationCidrBlock,
    networkRoute_destinations,
    networkRoute_prefixListId,
    networkRoute_state,
    networkRoute_type,

    -- ** NetworkRouteDestination
    networkRouteDestination_coreNetworkAttachmentId,
    networkRouteDestination_edgeLocation,
    networkRouteDestination_resourceId,
    networkRouteDestination_resourceType,
    networkRouteDestination_segmentName,
    networkRouteDestination_transitGatewayAttachmentId,

    -- ** NetworkTelemetry
    networkTelemetry_accountId,
    networkTelemetry_address,
    networkTelemetry_awsRegion,
    networkTelemetry_coreNetworkId,
    networkTelemetry_health,
    networkTelemetry_registeredGatewayArn,
    networkTelemetry_resourceArn,
    networkTelemetry_resourceId,
    networkTelemetry_resourceType,

    -- ** OrganizationStatus
    organizationStatus_accountStatusList,
    organizationStatus_organizationAwsServiceAccessStatus,
    organizationStatus_organizationId,
    organizationStatus_sLRDeploymentStatus,

    -- ** PathComponent
    pathComponent_destinationCidrBlock,
    pathComponent_resource,
    pathComponent_sequence,

    -- ** Peering
    peering_coreNetworkArn,
    peering_coreNetworkId,
    peering_createdAt,
    peering_edgeLocation,
    peering_ownerAccountId,
    peering_peeringId,
    peering_peeringType,
    peering_resourceArn,
    peering_state,
    peering_tags,

    -- ** ProposedSegmentChange
    proposedSegmentChange_attachmentPolicyRuleNumber,
    proposedSegmentChange_segmentName,
    proposedSegmentChange_tags,

    -- ** Relationship
    relationship_from,
    relationship_to,

    -- ** RouteAnalysis
    routeAnalysis_destination,
    routeAnalysis_forwardPath,
    routeAnalysis_globalNetworkId,
    routeAnalysis_includeReturnPath,
    routeAnalysis_ownerAccountId,
    routeAnalysis_returnPath,
    routeAnalysis_routeAnalysisId,
    routeAnalysis_source,
    routeAnalysis_startTimestamp,
    routeAnalysis_status,
    routeAnalysis_useMiddleboxes,

    -- ** RouteAnalysisCompletion
    routeAnalysisCompletion_reasonCode,
    routeAnalysisCompletion_reasonContext,
    routeAnalysisCompletion_resultCode,

    -- ** RouteAnalysisEndpointOptions
    routeAnalysisEndpointOptions_ipAddress,
    routeAnalysisEndpointOptions_transitGatewayArn,
    routeAnalysisEndpointOptions_transitGatewayAttachmentArn,

    -- ** RouteAnalysisEndpointOptionsSpecification
    routeAnalysisEndpointOptionsSpecification_ipAddress,
    routeAnalysisEndpointOptionsSpecification_transitGatewayAttachmentArn,

    -- ** RouteAnalysisPath
    routeAnalysisPath_completionStatus,
    routeAnalysisPath_path,

    -- ** RouteTableIdentifier
    routeTableIdentifier_coreNetworkSegmentEdge,
    routeTableIdentifier_transitGatewayRouteTableArn,

    -- ** Site
    site_createdAt,
    site_description,
    site_globalNetworkId,
    site_location,
    site_siteArn,
    site_siteId,
    site_state,
    site_tags,

    -- ** SiteToSiteVpnAttachment
    siteToSiteVpnAttachment_attachment,
    siteToSiteVpnAttachment_vpnConnectionArn,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TransitGatewayConnectPeerAssociation
    transitGatewayConnectPeerAssociation_deviceId,
    transitGatewayConnectPeerAssociation_globalNetworkId,
    transitGatewayConnectPeerAssociation_linkId,
    transitGatewayConnectPeerAssociation_state,
    transitGatewayConnectPeerAssociation_transitGatewayConnectPeerArn,

    -- ** TransitGatewayPeering
    transitGatewayPeering_peering,
    transitGatewayPeering_transitGatewayArn,
    transitGatewayPeering_transitGatewayPeeringAttachmentId,

    -- ** TransitGatewayRegistration
    transitGatewayRegistration_globalNetworkId,
    transitGatewayRegistration_state,
    transitGatewayRegistration_transitGatewayArn,

    -- ** TransitGatewayRegistrationStateReason
    transitGatewayRegistrationStateReason_code,
    transitGatewayRegistrationStateReason_message,

    -- ** TransitGatewayRouteTableAttachment
    transitGatewayRouteTableAttachment_attachment,
    transitGatewayRouteTableAttachment_peeringId,
    transitGatewayRouteTableAttachment_transitGatewayRouteTableArn,

    -- ** VpcAttachment
    vpcAttachment_attachment,
    vpcAttachment_options,
    vpcAttachment_subnetArns,

    -- ** VpcOptions
    vpcOptions_applianceModeSupport,
    vpcOptions_ipv6Support,
  )
where

import Amazonka.NetworkManager.AcceptAttachment
import Amazonka.NetworkManager.AssociateConnectPeer
import Amazonka.NetworkManager.AssociateCustomerGateway
import Amazonka.NetworkManager.AssociateLink
import Amazonka.NetworkManager.AssociateTransitGatewayConnectPeer
import Amazonka.NetworkManager.CreateConnectAttachment
import Amazonka.NetworkManager.CreateConnectPeer
import Amazonka.NetworkManager.CreateConnection
import Amazonka.NetworkManager.CreateCoreNetwork
import Amazonka.NetworkManager.CreateDevice
import Amazonka.NetworkManager.CreateGlobalNetwork
import Amazonka.NetworkManager.CreateLink
import Amazonka.NetworkManager.CreateSite
import Amazonka.NetworkManager.CreateSiteToSiteVpnAttachment
import Amazonka.NetworkManager.CreateTransitGatewayPeering
import Amazonka.NetworkManager.CreateTransitGatewayRouteTableAttachment
import Amazonka.NetworkManager.CreateVpcAttachment
import Amazonka.NetworkManager.DeleteAttachment
import Amazonka.NetworkManager.DeleteConnectPeer
import Amazonka.NetworkManager.DeleteConnection
import Amazonka.NetworkManager.DeleteCoreNetwork
import Amazonka.NetworkManager.DeleteCoreNetworkPolicyVersion
import Amazonka.NetworkManager.DeleteDevice
import Amazonka.NetworkManager.DeleteGlobalNetwork
import Amazonka.NetworkManager.DeleteLink
import Amazonka.NetworkManager.DeletePeering
import Amazonka.NetworkManager.DeleteResourcePolicy
import Amazonka.NetworkManager.DeleteSite
import Amazonka.NetworkManager.DeregisterTransitGateway
import Amazonka.NetworkManager.DescribeGlobalNetworks
import Amazonka.NetworkManager.DisassociateConnectPeer
import Amazonka.NetworkManager.DisassociateCustomerGateway
import Amazonka.NetworkManager.DisassociateLink
import Amazonka.NetworkManager.DisassociateTransitGatewayConnectPeer
import Amazonka.NetworkManager.ExecuteCoreNetworkChangeSet
import Amazonka.NetworkManager.GetConnectAttachment
import Amazonka.NetworkManager.GetConnectPeer
import Amazonka.NetworkManager.GetConnectPeerAssociations
import Amazonka.NetworkManager.GetConnections
import Amazonka.NetworkManager.GetCoreNetwork
import Amazonka.NetworkManager.GetCoreNetworkChangeEvents
import Amazonka.NetworkManager.GetCoreNetworkChangeSet
import Amazonka.NetworkManager.GetCoreNetworkPolicy
import Amazonka.NetworkManager.GetCustomerGatewayAssociations
import Amazonka.NetworkManager.GetDevices
import Amazonka.NetworkManager.GetLinkAssociations
import Amazonka.NetworkManager.GetLinks
import Amazonka.NetworkManager.GetNetworkResourceCounts
import Amazonka.NetworkManager.GetNetworkResourceRelationships
import Amazonka.NetworkManager.GetNetworkResources
import Amazonka.NetworkManager.GetNetworkRoutes
import Amazonka.NetworkManager.GetNetworkTelemetry
import Amazonka.NetworkManager.GetResourcePolicy
import Amazonka.NetworkManager.GetRouteAnalysis
import Amazonka.NetworkManager.GetSiteToSiteVpnAttachment
import Amazonka.NetworkManager.GetSites
import Amazonka.NetworkManager.GetTransitGatewayConnectPeerAssociations
import Amazonka.NetworkManager.GetTransitGatewayPeering
import Amazonka.NetworkManager.GetTransitGatewayRegistrations
import Amazonka.NetworkManager.GetTransitGatewayRouteTableAttachment
import Amazonka.NetworkManager.GetVpcAttachment
import Amazonka.NetworkManager.ListAttachments
import Amazonka.NetworkManager.ListConnectPeers
import Amazonka.NetworkManager.ListCoreNetworkPolicyVersions
import Amazonka.NetworkManager.ListCoreNetworks
import Amazonka.NetworkManager.ListOrganizationServiceAccessStatus
import Amazonka.NetworkManager.ListPeerings
import Amazonka.NetworkManager.ListTagsForResource
import Amazonka.NetworkManager.PutCoreNetworkPolicy
import Amazonka.NetworkManager.PutResourcePolicy
import Amazonka.NetworkManager.RegisterTransitGateway
import Amazonka.NetworkManager.RejectAttachment
import Amazonka.NetworkManager.RestoreCoreNetworkPolicyVersion
import Amazonka.NetworkManager.StartOrganizationServiceAccessUpdate
import Amazonka.NetworkManager.StartRouteAnalysis
import Amazonka.NetworkManager.TagResource
import Amazonka.NetworkManager.Types.AWSLocation
import Amazonka.NetworkManager.Types.AccountStatus
import Amazonka.NetworkManager.Types.Attachment
import Amazonka.NetworkManager.Types.Bandwidth
import Amazonka.NetworkManager.Types.BgpOptions
import Amazonka.NetworkManager.Types.ConnectAttachment
import Amazonka.NetworkManager.Types.ConnectAttachmentOptions
import Amazonka.NetworkManager.Types.ConnectPeer
import Amazonka.NetworkManager.Types.ConnectPeerAssociation
import Amazonka.NetworkManager.Types.ConnectPeerBgpConfiguration
import Amazonka.NetworkManager.Types.ConnectPeerConfiguration
import Amazonka.NetworkManager.Types.ConnectPeerSummary
import Amazonka.NetworkManager.Types.Connection
import Amazonka.NetworkManager.Types.ConnectionHealth
import Amazonka.NetworkManager.Types.CoreNetwork
import Amazonka.NetworkManager.Types.CoreNetworkChange
import Amazonka.NetworkManager.Types.CoreNetworkChangeEvent
import Amazonka.NetworkManager.Types.CoreNetworkChangeEventValues
import Amazonka.NetworkManager.Types.CoreNetworkChangeValues
import Amazonka.NetworkManager.Types.CoreNetworkEdge
import Amazonka.NetworkManager.Types.CoreNetworkPolicy
import Amazonka.NetworkManager.Types.CoreNetworkPolicyError
import Amazonka.NetworkManager.Types.CoreNetworkPolicyVersion
import Amazonka.NetworkManager.Types.CoreNetworkSegment
import Amazonka.NetworkManager.Types.CoreNetworkSegmentEdgeIdentifier
import Amazonka.NetworkManager.Types.CoreNetworkSummary
import Amazonka.NetworkManager.Types.CustomerGatewayAssociation
import Amazonka.NetworkManager.Types.Device
import Amazonka.NetworkManager.Types.GlobalNetwork
import Amazonka.NetworkManager.Types.Link
import Amazonka.NetworkManager.Types.LinkAssociation
import Amazonka.NetworkManager.Types.Location
import Amazonka.NetworkManager.Types.NetworkResource
import Amazonka.NetworkManager.Types.NetworkResourceCount
import Amazonka.NetworkManager.Types.NetworkResourceSummary
import Amazonka.NetworkManager.Types.NetworkRoute
import Amazonka.NetworkManager.Types.NetworkRouteDestination
import Amazonka.NetworkManager.Types.NetworkTelemetry
import Amazonka.NetworkManager.Types.OrganizationStatus
import Amazonka.NetworkManager.Types.PathComponent
import Amazonka.NetworkManager.Types.Peering
import Amazonka.NetworkManager.Types.ProposedSegmentChange
import Amazonka.NetworkManager.Types.Relationship
import Amazonka.NetworkManager.Types.RouteAnalysis
import Amazonka.NetworkManager.Types.RouteAnalysisCompletion
import Amazonka.NetworkManager.Types.RouteAnalysisEndpointOptions
import Amazonka.NetworkManager.Types.RouteAnalysisEndpointOptionsSpecification
import Amazonka.NetworkManager.Types.RouteAnalysisPath
import Amazonka.NetworkManager.Types.RouteTableIdentifier
import Amazonka.NetworkManager.Types.Site
import Amazonka.NetworkManager.Types.SiteToSiteVpnAttachment
import Amazonka.NetworkManager.Types.Tag
import Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociation
import Amazonka.NetworkManager.Types.TransitGatewayPeering
import Amazonka.NetworkManager.Types.TransitGatewayRegistration
import Amazonka.NetworkManager.Types.TransitGatewayRegistrationStateReason
import Amazonka.NetworkManager.Types.TransitGatewayRouteTableAttachment
import Amazonka.NetworkManager.Types.VpcAttachment
import Amazonka.NetworkManager.Types.VpcOptions
import Amazonka.NetworkManager.UntagResource
import Amazonka.NetworkManager.UpdateConnection
import Amazonka.NetworkManager.UpdateCoreNetwork
import Amazonka.NetworkManager.UpdateDevice
import Amazonka.NetworkManager.UpdateGlobalNetwork
import Amazonka.NetworkManager.UpdateLink
import Amazonka.NetworkManager.UpdateNetworkResourceMetadata
import Amazonka.NetworkManager.UpdateSite
import Amazonka.NetworkManager.UpdateVpcAttachment

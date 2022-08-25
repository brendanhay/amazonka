{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.NetworkManager.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    createConnectAttachment_tags,
    createConnectAttachment_clientToken,
    createConnectAttachment_coreNetworkId,
    createConnectAttachment_edgeLocation,
    createConnectAttachment_transportAttachmentId,
    createConnectAttachment_options,
    createConnectAttachmentResponse_connectAttachment,
    createConnectAttachmentResponse_httpStatus,

    -- ** CreateConnectPeer
    createConnectPeer_tags,
    createConnectPeer_clientToken,
    createConnectPeer_coreNetworkAddress,
    createConnectPeer_bgpOptions,
    createConnectPeer_connectAttachmentId,
    createConnectPeer_peerAddress,
    createConnectPeer_insideCidrBlocks,
    createConnectPeerResponse_connectPeer,
    createConnectPeerResponse_httpStatus,

    -- ** CreateConnection
    createConnection_tags,
    createConnection_linkId,
    createConnection_description,
    createConnection_connectedLinkId,
    createConnection_globalNetworkId,
    createConnection_deviceId,
    createConnection_connectedDeviceId,
    createConnectionResponse_connection,
    createConnectionResponse_httpStatus,

    -- ** CreateCoreNetwork
    createCoreNetwork_tags,
    createCoreNetwork_clientToken,
    createCoreNetwork_description,
    createCoreNetwork_policyDocument,
    createCoreNetwork_globalNetworkId,
    createCoreNetworkResponse_coreNetwork,
    createCoreNetworkResponse_httpStatus,

    -- ** CreateDevice
    createDevice_tags,
    createDevice_type,
    createDevice_model,
    createDevice_aWSLocation,
    createDevice_description,
    createDevice_siteId,
    createDevice_location,
    createDevice_serialNumber,
    createDevice_vendor,
    createDevice_globalNetworkId,
    createDeviceResponse_device,
    createDeviceResponse_httpStatus,

    -- ** CreateGlobalNetwork
    createGlobalNetwork_tags,
    createGlobalNetwork_description,
    createGlobalNetworkResponse_globalNetwork,
    createGlobalNetworkResponse_httpStatus,

    -- ** CreateLink
    createLink_tags,
    createLink_type,
    createLink_provider,
    createLink_description,
    createLink_globalNetworkId,
    createLink_bandwidth,
    createLink_siteId,
    createLinkResponse_link,
    createLinkResponse_httpStatus,

    -- ** CreateSite
    createSite_tags,
    createSite_description,
    createSite_location,
    createSite_globalNetworkId,
    createSiteResponse_site,
    createSiteResponse_httpStatus,

    -- ** CreateSiteToSiteVpnAttachment
    createSiteToSiteVpnAttachment_tags,
    createSiteToSiteVpnAttachment_clientToken,
    createSiteToSiteVpnAttachment_coreNetworkId,
    createSiteToSiteVpnAttachment_vpnConnectionArn,
    createSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment,
    createSiteToSiteVpnAttachmentResponse_httpStatus,

    -- ** CreateTransitGatewayPeering
    createTransitGatewayPeering_tags,
    createTransitGatewayPeering_clientToken,
    createTransitGatewayPeering_coreNetworkId,
    createTransitGatewayPeering_transitGatewayArn,
    createTransitGatewayPeeringResponse_transitGatewayPeering,
    createTransitGatewayPeeringResponse_httpStatus,

    -- ** CreateTransitGatewayRouteTableAttachment
    createTransitGatewayRouteTableAttachment_tags,
    createTransitGatewayRouteTableAttachment_clientToken,
    createTransitGatewayRouteTableAttachment_peeringId,
    createTransitGatewayRouteTableAttachment_transitGatewayRouteTableArn,
    createTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment,
    createTransitGatewayRouteTableAttachmentResponse_httpStatus,

    -- ** CreateVpcAttachment
    createVpcAttachment_tags,
    createVpcAttachment_clientToken,
    createVpcAttachment_options,
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
    describeGlobalNetworks_nextToken,
    describeGlobalNetworks_globalNetworkIds,
    describeGlobalNetworks_maxResults,
    describeGlobalNetworksResponse_nextToken,
    describeGlobalNetworksResponse_globalNetworks,
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
    getConnectPeerAssociations_nextToken,
    getConnectPeerAssociations_connectPeerIds,
    getConnectPeerAssociations_maxResults,
    getConnectPeerAssociations_globalNetworkId,
    getConnectPeerAssociationsResponse_nextToken,
    getConnectPeerAssociationsResponse_connectPeerAssociations,
    getConnectPeerAssociationsResponse_httpStatus,

    -- ** GetConnections
    getConnections_nextToken,
    getConnections_deviceId,
    getConnections_connectionIds,
    getConnections_maxResults,
    getConnections_globalNetworkId,
    getConnectionsResponse_nextToken,
    getConnectionsResponse_connections,
    getConnectionsResponse_httpStatus,

    -- ** GetCoreNetwork
    getCoreNetwork_coreNetworkId,
    getCoreNetworkResponse_coreNetwork,
    getCoreNetworkResponse_httpStatus,

    -- ** GetCoreNetworkChangeEvents
    getCoreNetworkChangeEvents_nextToken,
    getCoreNetworkChangeEvents_maxResults,
    getCoreNetworkChangeEvents_coreNetworkId,
    getCoreNetworkChangeEvents_policyVersionId,
    getCoreNetworkChangeEventsResponse_nextToken,
    getCoreNetworkChangeEventsResponse_coreNetworkChangeEvents,
    getCoreNetworkChangeEventsResponse_httpStatus,

    -- ** GetCoreNetworkChangeSet
    getCoreNetworkChangeSet_nextToken,
    getCoreNetworkChangeSet_maxResults,
    getCoreNetworkChangeSet_coreNetworkId,
    getCoreNetworkChangeSet_policyVersionId,
    getCoreNetworkChangeSetResponse_nextToken,
    getCoreNetworkChangeSetResponse_coreNetworkChanges,
    getCoreNetworkChangeSetResponse_httpStatus,

    -- ** GetCoreNetworkPolicy
    getCoreNetworkPolicy_alias,
    getCoreNetworkPolicy_policyVersionId,
    getCoreNetworkPolicy_coreNetworkId,
    getCoreNetworkPolicyResponse_coreNetworkPolicy,
    getCoreNetworkPolicyResponse_httpStatus,

    -- ** GetCustomerGatewayAssociations
    getCustomerGatewayAssociations_nextToken,
    getCustomerGatewayAssociations_customerGatewayArns,
    getCustomerGatewayAssociations_maxResults,
    getCustomerGatewayAssociations_globalNetworkId,
    getCustomerGatewayAssociationsResponse_nextToken,
    getCustomerGatewayAssociationsResponse_customerGatewayAssociations,
    getCustomerGatewayAssociationsResponse_httpStatus,

    -- ** GetDevices
    getDevices_nextToken,
    getDevices_siteId,
    getDevices_maxResults,
    getDevices_deviceIds,
    getDevices_globalNetworkId,
    getDevicesResponse_devices,
    getDevicesResponse_nextToken,
    getDevicesResponse_httpStatus,

    -- ** GetLinkAssociations
    getLinkAssociations_linkId,
    getLinkAssociations_nextToken,
    getLinkAssociations_deviceId,
    getLinkAssociations_maxResults,
    getLinkAssociations_globalNetworkId,
    getLinkAssociationsResponse_nextToken,
    getLinkAssociationsResponse_linkAssociations,
    getLinkAssociationsResponse_httpStatus,

    -- ** GetLinks
    getLinks_linkIds,
    getLinks_nextToken,
    getLinks_type,
    getLinks_provider,
    getLinks_siteId,
    getLinks_maxResults,
    getLinks_globalNetworkId,
    getLinksResponse_nextToken,
    getLinksResponse_links,
    getLinksResponse_httpStatus,

    -- ** GetNetworkResourceCounts
    getNetworkResourceCounts_resourceType,
    getNetworkResourceCounts_nextToken,
    getNetworkResourceCounts_maxResults,
    getNetworkResourceCounts_globalNetworkId,
    getNetworkResourceCountsResponse_nextToken,
    getNetworkResourceCountsResponse_networkResourceCounts,
    getNetworkResourceCountsResponse_httpStatus,

    -- ** GetNetworkResourceRelationships
    getNetworkResourceRelationships_resourceType,
    getNetworkResourceRelationships_coreNetworkId,
    getNetworkResourceRelationships_nextToken,
    getNetworkResourceRelationships_accountId,
    getNetworkResourceRelationships_maxResults,
    getNetworkResourceRelationships_registeredGatewayArn,
    getNetworkResourceRelationships_awsRegion,
    getNetworkResourceRelationships_resourceArn,
    getNetworkResourceRelationships_globalNetworkId,
    getNetworkResourceRelationshipsResponse_nextToken,
    getNetworkResourceRelationshipsResponse_relationships,
    getNetworkResourceRelationshipsResponse_httpStatus,

    -- ** GetNetworkResources
    getNetworkResources_resourceType,
    getNetworkResources_coreNetworkId,
    getNetworkResources_nextToken,
    getNetworkResources_accountId,
    getNetworkResources_maxResults,
    getNetworkResources_registeredGatewayArn,
    getNetworkResources_awsRegion,
    getNetworkResources_resourceArn,
    getNetworkResources_globalNetworkId,
    getNetworkResourcesResponse_nextToken,
    getNetworkResourcesResponse_networkResources,
    getNetworkResourcesResponse_httpStatus,

    -- ** GetNetworkRoutes
    getNetworkRoutes_supernetOfMatches,
    getNetworkRoutes_subnetOfMatches,
    getNetworkRoutes_types,
    getNetworkRoutes_exactCidrMatches,
    getNetworkRoutes_prefixListIds,
    getNetworkRoutes_destinationFilters,
    getNetworkRoutes_longestPrefixMatches,
    getNetworkRoutes_states,
    getNetworkRoutes_globalNetworkId,
    getNetworkRoutes_routeTableIdentifier,
    getNetworkRoutesResponse_routeTableArn,
    getNetworkRoutesResponse_networkRoutes,
    getNetworkRoutesResponse_routeTableType,
    getNetworkRoutesResponse_coreNetworkSegmentEdge,
    getNetworkRoutesResponse_routeTableTimestamp,
    getNetworkRoutesResponse_httpStatus,

    -- ** GetNetworkTelemetry
    getNetworkTelemetry_resourceType,
    getNetworkTelemetry_coreNetworkId,
    getNetworkTelemetry_nextToken,
    getNetworkTelemetry_accountId,
    getNetworkTelemetry_maxResults,
    getNetworkTelemetry_registeredGatewayArn,
    getNetworkTelemetry_awsRegion,
    getNetworkTelemetry_resourceArn,
    getNetworkTelemetry_globalNetworkId,
    getNetworkTelemetryResponse_nextToken,
    getNetworkTelemetryResponse_networkTelemetry,
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
    getSites_nextToken,
    getSites_siteIds,
    getSites_maxResults,
    getSites_globalNetworkId,
    getSitesResponse_sites,
    getSitesResponse_nextToken,
    getSitesResponse_httpStatus,

    -- ** GetTransitGatewayConnectPeerAssociations
    getTransitGatewayConnectPeerAssociations_transitGatewayConnectPeerArns,
    getTransitGatewayConnectPeerAssociations_nextToken,
    getTransitGatewayConnectPeerAssociations_maxResults,
    getTransitGatewayConnectPeerAssociations_globalNetworkId,
    getTransitGatewayConnectPeerAssociationsResponse_nextToken,
    getTransitGatewayConnectPeerAssociationsResponse_transitGatewayConnectPeerAssociations,
    getTransitGatewayConnectPeerAssociationsResponse_httpStatus,

    -- ** GetTransitGatewayPeering
    getTransitGatewayPeering_peeringId,
    getTransitGatewayPeeringResponse_transitGatewayPeering,
    getTransitGatewayPeeringResponse_httpStatus,

    -- ** GetTransitGatewayRegistrations
    getTransitGatewayRegistrations_nextToken,
    getTransitGatewayRegistrations_transitGatewayArns,
    getTransitGatewayRegistrations_maxResults,
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
    listAttachments_coreNetworkId,
    listAttachments_nextToken,
    listAttachments_state,
    listAttachments_edgeLocation,
    listAttachments_maxResults,
    listAttachments_attachmentType,
    listAttachmentsResponse_nextToken,
    listAttachmentsResponse_attachments,
    listAttachmentsResponse_httpStatus,

    -- ** ListConnectPeers
    listConnectPeers_coreNetworkId,
    listConnectPeers_nextToken,
    listConnectPeers_connectAttachmentId,
    listConnectPeers_maxResults,
    listConnectPeersResponse_nextToken,
    listConnectPeersResponse_connectPeers,
    listConnectPeersResponse_httpStatus,

    -- ** ListCoreNetworkPolicyVersions
    listCoreNetworkPolicyVersions_nextToken,
    listCoreNetworkPolicyVersions_maxResults,
    listCoreNetworkPolicyVersions_coreNetworkId,
    listCoreNetworkPolicyVersionsResponse_nextToken,
    listCoreNetworkPolicyVersionsResponse_coreNetworkPolicyVersions,
    listCoreNetworkPolicyVersionsResponse_httpStatus,

    -- ** ListCoreNetworks
    listCoreNetworks_nextToken,
    listCoreNetworks_maxResults,
    listCoreNetworksResponse_nextToken,
    listCoreNetworksResponse_coreNetworks,
    listCoreNetworksResponse_httpStatus,

    -- ** ListOrganizationServiceAccessStatus
    listOrganizationServiceAccessStatus_nextToken,
    listOrganizationServiceAccessStatus_maxResults,
    listOrganizationServiceAccessStatusResponse_nextToken,
    listOrganizationServiceAccessStatusResponse_organizationStatus,
    listOrganizationServiceAccessStatusResponse_httpStatus,

    -- ** ListPeerings
    listPeerings_coreNetworkId,
    listPeerings_nextToken,
    listPeerings_peeringType,
    listPeerings_state,
    listPeerings_edgeLocation,
    listPeerings_maxResults,
    listPeeringsResponse_nextToken,
    listPeeringsResponse_peerings,
    listPeeringsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** PutCoreNetworkPolicy
    putCoreNetworkPolicy_clientToken,
    putCoreNetworkPolicy_latestVersionId,
    putCoreNetworkPolicy_description,
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
    updateConnection_linkId,
    updateConnection_description,
    updateConnection_connectedLinkId,
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
    updateDevice_type,
    updateDevice_model,
    updateDevice_aWSLocation,
    updateDevice_description,
    updateDevice_siteId,
    updateDevice_location,
    updateDevice_serialNumber,
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
    updateLink_type,
    updateLink_bandwidth,
    updateLink_provider,
    updateLink_description,
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
    updateVpcAttachment_options,
    updateVpcAttachment_removeSubnetArns,
    updateVpcAttachment_addSubnetArns,
    updateVpcAttachment_attachmentId,
    updateVpcAttachmentResponse_vpcAttachment,
    updateVpcAttachmentResponse_httpStatus,

    -- * Types

    -- ** AWSLocation
    aWSLocation_zone,
    aWSLocation_subnetArn,

    -- ** AccountStatus
    accountStatus_sLRDeploymentStatus,
    accountStatus_accountId,

    -- ** Attachment
    attachment_tags,
    attachment_proposedSegmentChange,
    attachment_coreNetworkId,
    attachment_state,
    attachment_edgeLocation,
    attachment_attachmentId,
    attachment_segmentName,
    attachment_coreNetworkArn,
    attachment_ownerAccountId,
    attachment_attachmentType,
    attachment_resourceArn,
    attachment_attachmentPolicyRuleNumber,
    attachment_createdAt,
    attachment_updatedAt,

    -- ** Bandwidth
    bandwidth_downloadSpeed,
    bandwidth_uploadSpeed,

    -- ** BgpOptions
    bgpOptions_peerAsn,

    -- ** ConnectAttachment
    connectAttachment_attachment,
    connectAttachment_transportAttachmentId,
    connectAttachment_options,

    -- ** ConnectAttachmentOptions
    connectAttachmentOptions_protocol,

    -- ** ConnectPeer
    connectPeer_tags,
    connectPeer_coreNetworkId,
    connectPeer_connectAttachmentId,
    connectPeer_configuration,
    connectPeer_state,
    connectPeer_edgeLocation,
    connectPeer_connectPeerId,
    connectPeer_createdAt,

    -- ** ConnectPeerAssociation
    connectPeerAssociation_globalNetworkId,
    connectPeerAssociation_linkId,
    connectPeerAssociation_deviceId,
    connectPeerAssociation_state,
    connectPeerAssociation_connectPeerId,

    -- ** ConnectPeerBgpConfiguration
    connectPeerBgpConfiguration_peerAsn,
    connectPeerBgpConfiguration_coreNetworkAddress,
    connectPeerBgpConfiguration_coreNetworkAsn,
    connectPeerBgpConfiguration_peerAddress,

    -- ** ConnectPeerConfiguration
    connectPeerConfiguration_bgpConfigurations,
    connectPeerConfiguration_coreNetworkAddress,
    connectPeerConfiguration_insideCidrBlocks,
    connectPeerConfiguration_peerAddress,
    connectPeerConfiguration_protocol,

    -- ** ConnectPeerSummary
    connectPeerSummary_tags,
    connectPeerSummary_coreNetworkId,
    connectPeerSummary_connectAttachmentId,
    connectPeerSummary_edgeLocation,
    connectPeerSummary_connectPeerState,
    connectPeerSummary_connectPeerId,
    connectPeerSummary_createdAt,

    -- ** Connection
    connection_globalNetworkId,
    connection_tags,
    connection_linkId,
    connection_deviceId,
    connection_connectedDeviceId,
    connection_state,
    connection_connectionId,
    connection_description,
    connection_connectionArn,
    connection_connectedLinkId,
    connection_createdAt,

    -- ** ConnectionHealth
    connectionHealth_type,
    connectionHealth_timestamp,
    connectionHealth_status,

    -- ** CoreNetwork
    coreNetwork_globalNetworkId,
    coreNetwork_tags,
    coreNetwork_edges,
    coreNetwork_coreNetworkId,
    coreNetwork_state,
    coreNetwork_description,
    coreNetwork_coreNetworkArn,
    coreNetwork_createdAt,
    coreNetwork_segments,

    -- ** CoreNetworkChange
    coreNetworkChange_newValues,
    coreNetworkChange_type,
    coreNetworkChange_previousValues,
    coreNetworkChange_identifier,
    coreNetworkChange_identifierPath,
    coreNetworkChange_action,

    -- ** CoreNetworkChangeEvent
    coreNetworkChangeEvent_type,
    coreNetworkChangeEvent_status,
    coreNetworkChangeEvent_identifierPath,
    coreNetworkChangeEvent_action,
    coreNetworkChangeEvent_values,
    coreNetworkChangeEvent_eventTime,

    -- ** CoreNetworkChangeEventValues
    coreNetworkChangeEventValues_cidr,
    coreNetworkChangeEventValues_edgeLocation,
    coreNetworkChangeEventValues_attachmentId,
    coreNetworkChangeEventValues_segmentName,

    -- ** CoreNetworkChangeValues
    coreNetworkChangeValues_sharedSegments,
    coreNetworkChangeValues_cidr,
    coreNetworkChangeValues_asn,
    coreNetworkChangeValues_segmentName,
    coreNetworkChangeValues_edgeLocations,
    coreNetworkChangeValues_insideCidrBlocks,
    coreNetworkChangeValues_destinationIdentifier,

    -- ** CoreNetworkEdge
    coreNetworkEdge_edgeLocation,
    coreNetworkEdge_asn,
    coreNetworkEdge_insideCidrBlocks,

    -- ** CoreNetworkPolicy
    coreNetworkPolicy_alias,
    coreNetworkPolicy_coreNetworkId,
    coreNetworkPolicy_changeSetState,
    coreNetworkPolicy_policyVersionId,
    coreNetworkPolicy_policyErrors,
    coreNetworkPolicy_description,
    coreNetworkPolicy_policyDocument,
    coreNetworkPolicy_createdAt,

    -- ** CoreNetworkPolicyError
    coreNetworkPolicyError_path,
    coreNetworkPolicyError_errorCode,
    coreNetworkPolicyError_message,

    -- ** CoreNetworkPolicyVersion
    coreNetworkPolicyVersion_alias,
    coreNetworkPolicyVersion_coreNetworkId,
    coreNetworkPolicyVersion_changeSetState,
    coreNetworkPolicyVersion_policyVersionId,
    coreNetworkPolicyVersion_description,
    coreNetworkPolicyVersion_createdAt,

    -- ** CoreNetworkSegment
    coreNetworkSegment_name,
    coreNetworkSegment_sharedSegments,
    coreNetworkSegment_edgeLocations,

    -- ** CoreNetworkSegmentEdgeIdentifier
    coreNetworkSegmentEdgeIdentifier_coreNetworkId,
    coreNetworkSegmentEdgeIdentifier_edgeLocation,
    coreNetworkSegmentEdgeIdentifier_segmentName,

    -- ** CoreNetworkSummary
    coreNetworkSummary_globalNetworkId,
    coreNetworkSummary_tags,
    coreNetworkSummary_coreNetworkId,
    coreNetworkSummary_state,
    coreNetworkSummary_description,
    coreNetworkSummary_coreNetworkArn,
    coreNetworkSummary_ownerAccountId,

    -- ** CustomerGatewayAssociation
    customerGatewayAssociation_globalNetworkId,
    customerGatewayAssociation_linkId,
    customerGatewayAssociation_deviceId,
    customerGatewayAssociation_state,
    customerGatewayAssociation_customerGatewayArn,

    -- ** Device
    device_globalNetworkId,
    device_tags,
    device_type,
    device_model,
    device_aWSLocation,
    device_deviceId,
    device_state,
    device_description,
    device_siteId,
    device_location,
    device_serialNumber,
    device_vendor,
    device_createdAt,
    device_deviceArn,

    -- ** GlobalNetwork
    globalNetwork_globalNetworkId,
    globalNetwork_tags,
    globalNetwork_state,
    globalNetwork_description,
    globalNetwork_globalNetworkArn,
    globalNetwork_createdAt,

    -- ** Link
    link_globalNetworkId,
    link_tags,
    link_linkId,
    link_type,
    link_bandwidth,
    link_state,
    link_provider,
    link_description,
    link_siteId,
    link_createdAt,
    link_linkArn,

    -- ** LinkAssociation
    linkAssociation_globalNetworkId,
    linkAssociation_linkId,
    linkAssociation_deviceId,
    linkAssociation_linkAssociationState,

    -- ** Location
    location_longitude,
    location_address,
    location_latitude,

    -- ** NetworkResource
    networkResource_resourceId,
    networkResource_tags,
    networkResource_resourceType,
    networkResource_coreNetworkId,
    networkResource_metadata,
    networkResource_accountId,
    networkResource_registeredGatewayArn,
    networkResource_awsRegion,
    networkResource_definitionTimestamp,
    networkResource_resourceArn,
    networkResource_definition,

    -- ** NetworkResourceCount
    networkResourceCount_resourceType,
    networkResourceCount_count,

    -- ** NetworkResourceSummary
    networkResourceSummary_resourceType,
    networkResourceSummary_isMiddlebox,
    networkResourceSummary_registeredGatewayArn,
    networkResourceSummary_resourceArn,
    networkResourceSummary_definition,
    networkResourceSummary_nameTag,

    -- ** NetworkRoute
    networkRoute_type,
    networkRoute_prefixListId,
    networkRoute_state,
    networkRoute_destinationCidrBlock,
    networkRoute_destinations,

    -- ** NetworkRouteDestination
    networkRouteDestination_resourceId,
    networkRouteDestination_resourceType,
    networkRouteDestination_coreNetworkAttachmentId,
    networkRouteDestination_transitGatewayAttachmentId,
    networkRouteDestination_edgeLocation,
    networkRouteDestination_segmentName,

    -- ** NetworkTelemetry
    networkTelemetry_resourceId,
    networkTelemetry_resourceType,
    networkTelemetry_coreNetworkId,
    networkTelemetry_accountId,
    networkTelemetry_address,
    networkTelemetry_registeredGatewayArn,
    networkTelemetry_health,
    networkTelemetry_awsRegion,
    networkTelemetry_resourceArn,

    -- ** OrganizationStatus
    organizationStatus_sLRDeploymentStatus,
    organizationStatus_accountStatusList,
    organizationStatus_organizationId,
    organizationStatus_organizationAwsServiceAccessStatus,

    -- ** PathComponent
    pathComponent_sequence,
    pathComponent_destinationCidrBlock,
    pathComponent_resource,

    -- ** Peering
    peering_tags,
    peering_coreNetworkId,
    peering_peeringType,
    peering_state,
    peering_edgeLocation,
    peering_coreNetworkArn,
    peering_ownerAccountId,
    peering_peeringId,
    peering_resourceArn,
    peering_createdAt,

    -- ** ProposedSegmentChange
    proposedSegmentChange_tags,
    proposedSegmentChange_segmentName,
    proposedSegmentChange_attachmentPolicyRuleNumber,

    -- ** Relationship
    relationship_from,
    relationship_to,

    -- ** RouteAnalysis
    routeAnalysis_globalNetworkId,
    routeAnalysis_destination,
    routeAnalysis_returnPath,
    routeAnalysis_routeAnalysisId,
    routeAnalysis_includeReturnPath,
    routeAnalysis_startTimestamp,
    routeAnalysis_status,
    routeAnalysis_source,
    routeAnalysis_ownerAccountId,
    routeAnalysis_useMiddleboxes,
    routeAnalysis_forwardPath,

    -- ** RouteAnalysisCompletion
    routeAnalysisCompletion_resultCode,
    routeAnalysisCompletion_reasonCode,
    routeAnalysisCompletion_reasonContext,

    -- ** RouteAnalysisEndpointOptions
    routeAnalysisEndpointOptions_transitGatewayAttachmentArn,
    routeAnalysisEndpointOptions_transitGatewayArn,
    routeAnalysisEndpointOptions_ipAddress,

    -- ** RouteAnalysisEndpointOptionsSpecification
    routeAnalysisEndpointOptionsSpecification_transitGatewayAttachmentArn,
    routeAnalysisEndpointOptionsSpecification_ipAddress,

    -- ** RouteAnalysisPath
    routeAnalysisPath_path,
    routeAnalysisPath_completionStatus,

    -- ** RouteTableIdentifier
    routeTableIdentifier_coreNetworkSegmentEdge,
    routeTableIdentifier_transitGatewayRouteTableArn,

    -- ** Site
    site_globalNetworkId,
    site_tags,
    site_siteArn,
    site_state,
    site_description,
    site_siteId,
    site_location,
    site_createdAt,

    -- ** SiteToSiteVpnAttachment
    siteToSiteVpnAttachment_attachment,
    siteToSiteVpnAttachment_vpnConnectionArn,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TransitGatewayConnectPeerAssociation
    transitGatewayConnectPeerAssociation_globalNetworkId,
    transitGatewayConnectPeerAssociation_linkId,
    transitGatewayConnectPeerAssociation_deviceId,
    transitGatewayConnectPeerAssociation_state,
    transitGatewayConnectPeerAssociation_transitGatewayConnectPeerArn,

    -- ** TransitGatewayPeering
    transitGatewayPeering_transitGatewayArn,
    transitGatewayPeering_transitGatewayPeeringAttachmentId,
    transitGatewayPeering_peering,

    -- ** TransitGatewayRegistration
    transitGatewayRegistration_globalNetworkId,
    transitGatewayRegistration_transitGatewayArn,
    transitGatewayRegistration_state,

    -- ** TransitGatewayRegistrationStateReason
    transitGatewayRegistrationStateReason_message,
    transitGatewayRegistrationStateReason_code,

    -- ** TransitGatewayRouteTableAttachment
    transitGatewayRouteTableAttachment_attachment,
    transitGatewayRouteTableAttachment_peeringId,
    transitGatewayRouteTableAttachment_transitGatewayRouteTableArn,

    -- ** VpcAttachment
    vpcAttachment_attachment,
    vpcAttachment_subnetArns,
    vpcAttachment_options,

    -- ** VpcOptions
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

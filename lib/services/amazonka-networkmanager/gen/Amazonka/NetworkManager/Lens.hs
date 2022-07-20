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

    -- ** DeleteConnection
    deleteConnection_globalNetworkId,
    deleteConnection_connectionId,
    deleteConnectionResponse_connection,
    deleteConnectionResponse_httpStatus,

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

    -- ** GetConnections
    getConnections_nextToken,
    getConnections_deviceId,
    getConnections_connectionIds,
    getConnections_maxResults,
    getConnections_globalNetworkId,
    getConnectionsResponse_nextToken,
    getConnectionsResponse_connections,
    getConnectionsResponse_httpStatus,

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

    -- ** GetTransitGatewayRegistrations
    getTransitGatewayRegistrations_nextToken,
    getTransitGatewayRegistrations_transitGatewayArns,
    getTransitGatewayRegistrations_maxResults,
    getTransitGatewayRegistrations_globalNetworkId,
    getTransitGatewayRegistrationsResponse_nextToken,
    getTransitGatewayRegistrationsResponse_transitGatewayRegistrations,
    getTransitGatewayRegistrationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterTransitGateway
    registerTransitGateway_globalNetworkId,
    registerTransitGateway_transitGatewayArn,
    registerTransitGatewayResponse_transitGatewayRegistration,
    registerTransitGatewayResponse_httpStatus,

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

    -- ** UpdateSite
    updateSite_description,
    updateSite_location,
    updateSite_globalNetworkId,
    updateSite_siteId,
    updateSiteResponse_site,
    updateSiteResponse_httpStatus,

    -- * Types

    -- ** AWSLocation
    aWSLocation_zone,
    aWSLocation_subnetArn,

    -- ** Bandwidth
    bandwidth_downloadSpeed,
    bandwidth_uploadSpeed,

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

    -- ** Site
    site_globalNetworkId,
    site_tags,
    site_siteArn,
    site_state,
    site_description,
    site_siteId,
    site_location,
    site_createdAt,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TransitGatewayConnectPeerAssociation
    transitGatewayConnectPeerAssociation_globalNetworkId,
    transitGatewayConnectPeerAssociation_linkId,
    transitGatewayConnectPeerAssociation_deviceId,
    transitGatewayConnectPeerAssociation_state,
    transitGatewayConnectPeerAssociation_transitGatewayConnectPeerArn,

    -- ** TransitGatewayRegistration
    transitGatewayRegistration_globalNetworkId,
    transitGatewayRegistration_transitGatewayArn,
    transitGatewayRegistration_state,

    -- ** TransitGatewayRegistrationStateReason
    transitGatewayRegistrationStateReason_message,
    transitGatewayRegistrationStateReason_code,
  )
where

import Amazonka.NetworkManager.AssociateCustomerGateway
import Amazonka.NetworkManager.AssociateLink
import Amazonka.NetworkManager.AssociateTransitGatewayConnectPeer
import Amazonka.NetworkManager.CreateConnection
import Amazonka.NetworkManager.CreateDevice
import Amazonka.NetworkManager.CreateGlobalNetwork
import Amazonka.NetworkManager.CreateLink
import Amazonka.NetworkManager.CreateSite
import Amazonka.NetworkManager.DeleteConnection
import Amazonka.NetworkManager.DeleteDevice
import Amazonka.NetworkManager.DeleteGlobalNetwork
import Amazonka.NetworkManager.DeleteLink
import Amazonka.NetworkManager.DeleteSite
import Amazonka.NetworkManager.DeregisterTransitGateway
import Amazonka.NetworkManager.DescribeGlobalNetworks
import Amazonka.NetworkManager.DisassociateCustomerGateway
import Amazonka.NetworkManager.DisassociateLink
import Amazonka.NetworkManager.DisassociateTransitGatewayConnectPeer
import Amazonka.NetworkManager.GetConnections
import Amazonka.NetworkManager.GetCustomerGatewayAssociations
import Amazonka.NetworkManager.GetDevices
import Amazonka.NetworkManager.GetLinkAssociations
import Amazonka.NetworkManager.GetLinks
import Amazonka.NetworkManager.GetSites
import Amazonka.NetworkManager.GetTransitGatewayConnectPeerAssociations
import Amazonka.NetworkManager.GetTransitGatewayRegistrations
import Amazonka.NetworkManager.ListTagsForResource
import Amazonka.NetworkManager.RegisterTransitGateway
import Amazonka.NetworkManager.TagResource
import Amazonka.NetworkManager.Types.AWSLocation
import Amazonka.NetworkManager.Types.Bandwidth
import Amazonka.NetworkManager.Types.Connection
import Amazonka.NetworkManager.Types.CustomerGatewayAssociation
import Amazonka.NetworkManager.Types.Device
import Amazonka.NetworkManager.Types.GlobalNetwork
import Amazonka.NetworkManager.Types.Link
import Amazonka.NetworkManager.Types.LinkAssociation
import Amazonka.NetworkManager.Types.Location
import Amazonka.NetworkManager.Types.Site
import Amazonka.NetworkManager.Types.Tag
import Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociation
import Amazonka.NetworkManager.Types.TransitGatewayRegistration
import Amazonka.NetworkManager.Types.TransitGatewayRegistrationStateReason
import Amazonka.NetworkManager.UntagResource
import Amazonka.NetworkManager.UpdateConnection
import Amazonka.NetworkManager.UpdateDevice
import Amazonka.NetworkManager.UpdateGlobalNetwork
import Amazonka.NetworkManager.UpdateLink
import Amazonka.NetworkManager.UpdateSite

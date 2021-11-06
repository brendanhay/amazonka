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

    -- ** GetLinkAssociations
    getLinkAssociations_nextToken,
    getLinkAssociations_linkId,
    getLinkAssociations_deviceId,
    getLinkAssociations_maxResults,
    getLinkAssociations_globalNetworkId,
    getLinkAssociationsResponse_nextToken,
    getLinkAssociationsResponse_linkAssociations,
    getLinkAssociationsResponse_httpStatus,

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

    -- ** CreateSite
    createSite_location,
    createSite_description,
    createSite_tags,
    createSite_globalNetworkId,
    createSiteResponse_site,
    createSiteResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_globalNetworkId,
    deleteConnection_connectionId,
    deleteConnectionResponse_connection,
    deleteConnectionResponse_httpStatus,

    -- ** UpdateConnection
    updateConnection_connectedLinkId,
    updateConnection_linkId,
    updateConnection_description,
    updateConnection_globalNetworkId,
    updateConnection_connectionId,
    updateConnectionResponse_connection,
    updateConnectionResponse_httpStatus,

    -- ** DeregisterTransitGateway
    deregisterTransitGateway_globalNetworkId,
    deregisterTransitGateway_transitGatewayArn,
    deregisterTransitGatewayResponse_transitGatewayRegistration,
    deregisterTransitGatewayResponse_httpStatus,

    -- ** GetTransitGatewayConnectPeerAssociations
    getTransitGatewayConnectPeerAssociations_transitGatewayConnectPeerArns,
    getTransitGatewayConnectPeerAssociations_nextToken,
    getTransitGatewayConnectPeerAssociations_maxResults,
    getTransitGatewayConnectPeerAssociations_globalNetworkId,
    getTransitGatewayConnectPeerAssociationsResponse_nextToken,
    getTransitGatewayConnectPeerAssociationsResponse_transitGatewayConnectPeerAssociations,
    getTransitGatewayConnectPeerAssociationsResponse_httpStatus,

    -- ** UpdateSite
    updateSite_location,
    updateSite_description,
    updateSite_globalNetworkId,
    updateSite_siteId,
    updateSiteResponse_site,
    updateSiteResponse_httpStatus,

    -- ** DeleteSite
    deleteSite_globalNetworkId,
    deleteSite_siteId,
    deleteSiteResponse_site,
    deleteSiteResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** DisassociateLink
    disassociateLink_globalNetworkId,
    disassociateLink_deviceId,
    disassociateLink_linkId,
    disassociateLinkResponse_linkAssociation,
    disassociateLinkResponse_httpStatus,

    -- ** CreateConnection
    createConnection_connectedLinkId,
    createConnection_linkId,
    createConnection_description,
    createConnection_tags,
    createConnection_globalNetworkId,
    createConnection_deviceId,
    createConnection_connectedDeviceId,
    createConnectionResponse_connection,
    createConnectionResponse_httpStatus,

    -- ** GetDevices
    getDevices_deviceIds,
    getDevices_nextToken,
    getDevices_siteId,
    getDevices_maxResults,
    getDevices_globalNetworkId,
    getDevicesResponse_nextToken,
    getDevicesResponse_devices,
    getDevicesResponse_httpStatus,

    -- ** GetLinks
    getLinks_linkIds,
    getLinks_nextToken,
    getLinks_type,
    getLinks_siteId,
    getLinks_maxResults,
    getLinks_provider,
    getLinks_globalNetworkId,
    getLinksResponse_nextToken,
    getLinksResponse_links,
    getLinksResponse_httpStatus,

    -- ** DescribeGlobalNetworks
    describeGlobalNetworks_nextToken,
    describeGlobalNetworks_globalNetworkIds,
    describeGlobalNetworks_maxResults,
    describeGlobalNetworksResponse_globalNetworks,
    describeGlobalNetworksResponse_nextToken,
    describeGlobalNetworksResponse_httpStatus,

    -- ** DisassociateCustomerGateway
    disassociateCustomerGateway_globalNetworkId,
    disassociateCustomerGateway_customerGatewayArn,
    disassociateCustomerGatewayResponse_customerGatewayAssociation,
    disassociateCustomerGatewayResponse_httpStatus,

    -- ** DisassociateTransitGatewayConnectPeer
    disassociateTransitGatewayConnectPeer_globalNetworkId,
    disassociateTransitGatewayConnectPeer_transitGatewayConnectPeerArn,
    disassociateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation,
    disassociateTransitGatewayConnectPeerResponse_httpStatus,

    -- ** CreateGlobalNetwork
    createGlobalNetwork_description,
    createGlobalNetwork_tags,
    createGlobalNetworkResponse_globalNetwork,
    createGlobalNetworkResponse_httpStatus,

    -- ** CreateLink
    createLink_type,
    createLink_description,
    createLink_provider,
    createLink_tags,
    createLink_globalNetworkId,
    createLink_bandwidth,
    createLink_siteId,
    createLinkResponse_link,
    createLinkResponse_httpStatus,

    -- ** DeleteGlobalNetwork
    deleteGlobalNetwork_globalNetworkId,
    deleteGlobalNetworkResponse_globalNetwork,
    deleteGlobalNetworkResponse_httpStatus,

    -- ** UpdateGlobalNetwork
    updateGlobalNetwork_description,
    updateGlobalNetwork_globalNetworkId,
    updateGlobalNetworkResponse_globalNetwork,
    updateGlobalNetworkResponse_httpStatus,

    -- ** CreateDevice
    createDevice_vendor,
    createDevice_location,
    createDevice_aWSLocation,
    createDevice_model,
    createDevice_type,
    createDevice_serialNumber,
    createDevice_siteId,
    createDevice_description,
    createDevice_tags,
    createDevice_globalNetworkId,
    createDeviceResponse_device,
    createDeviceResponse_httpStatus,

    -- ** AssociateCustomerGateway
    associateCustomerGateway_linkId,
    associateCustomerGateway_customerGatewayArn,
    associateCustomerGateway_globalNetworkId,
    associateCustomerGateway_deviceId,
    associateCustomerGatewayResponse_customerGatewayAssociation,
    associateCustomerGatewayResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetCustomerGatewayAssociations
    getCustomerGatewayAssociations_nextToken,
    getCustomerGatewayAssociations_customerGatewayArns,
    getCustomerGatewayAssociations_maxResults,
    getCustomerGatewayAssociations_globalNetworkId,
    getCustomerGatewayAssociationsResponse_nextToken,
    getCustomerGatewayAssociationsResponse_customerGatewayAssociations,
    getCustomerGatewayAssociationsResponse_httpStatus,

    -- ** GetTransitGatewayRegistrations
    getTransitGatewayRegistrations_transitGatewayArns,
    getTransitGatewayRegistrations_nextToken,
    getTransitGatewayRegistrations_maxResults,
    getTransitGatewayRegistrations_globalNetworkId,
    getTransitGatewayRegistrationsResponse_transitGatewayRegistrations,
    getTransitGatewayRegistrationsResponse_nextToken,
    getTransitGatewayRegistrationsResponse_httpStatus,

    -- ** GetConnections
    getConnections_connectionIds,
    getConnections_nextToken,
    getConnections_deviceId,
    getConnections_maxResults,
    getConnections_globalNetworkId,
    getConnectionsResponse_connections,
    getConnectionsResponse_nextToken,
    getConnectionsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetSites
    getSites_nextToken,
    getSites_siteIds,
    getSites_maxResults,
    getSites_globalNetworkId,
    getSitesResponse_nextToken,
    getSitesResponse_sites,
    getSitesResponse_httpStatus,

    -- ** RegisterTransitGateway
    registerTransitGateway_globalNetworkId,
    registerTransitGateway_transitGatewayArn,
    registerTransitGatewayResponse_transitGatewayRegistration,
    registerTransitGatewayResponse_httpStatus,

    -- ** DeleteDevice
    deleteDevice_globalNetworkId,
    deleteDevice_deviceId,
    deleteDeviceResponse_device,
    deleteDeviceResponse_httpStatus,

    -- ** UpdateDevice
    updateDevice_vendor,
    updateDevice_location,
    updateDevice_aWSLocation,
    updateDevice_model,
    updateDevice_type,
    updateDevice_serialNumber,
    updateDevice_siteId,
    updateDevice_description,
    updateDevice_globalNetworkId,
    updateDevice_deviceId,
    updateDeviceResponse_device,
    updateDeviceResponse_httpStatus,

    -- ** DeleteLink
    deleteLink_globalNetworkId,
    deleteLink_linkId,
    deleteLinkResponse_link,
    deleteLinkResponse_httpStatus,

    -- ** UpdateLink
    updateLink_bandwidth,
    updateLink_type,
    updateLink_description,
    updateLink_provider,
    updateLink_globalNetworkId,
    updateLink_linkId,
    updateLinkResponse_link,
    updateLinkResponse_httpStatus,

    -- * Types

    -- ** AWSLocation
    aWSLocation_zone,
    aWSLocation_subnetArn,

    -- ** Bandwidth
    bandwidth_downloadSpeed,
    bandwidth_uploadSpeed,

    -- ** Connection
    connection_state,
    connection_createdAt,
    connection_globalNetworkId,
    connection_connectionId,
    connection_connectedDeviceId,
    connection_connectedLinkId,
    connection_linkId,
    connection_deviceId,
    connection_connectionArn,
    connection_description,
    connection_tags,

    -- ** CustomerGatewayAssociation
    customerGatewayAssociation_state,
    customerGatewayAssociation_globalNetworkId,
    customerGatewayAssociation_linkId,
    customerGatewayAssociation_deviceId,
    customerGatewayAssociation_customerGatewayArn,

    -- ** Device
    device_vendor,
    device_state,
    device_location,
    device_createdAt,
    device_deviceArn,
    device_aWSLocation,
    device_globalNetworkId,
    device_model,
    device_deviceId,
    device_type,
    device_serialNumber,
    device_siteId,
    device_description,
    device_tags,

    -- ** GlobalNetwork
    globalNetwork_state,
    globalNetwork_createdAt,
    globalNetwork_globalNetworkArn,
    globalNetwork_globalNetworkId,
    globalNetwork_description,
    globalNetwork_tags,

    -- ** Link
    link_state,
    link_linkArn,
    link_createdAt,
    link_globalNetworkId,
    link_bandwidth,
    link_linkId,
    link_type,
    link_siteId,
    link_description,
    link_provider,
    link_tags,

    -- ** LinkAssociation
    linkAssociation_globalNetworkId,
    linkAssociation_linkId,
    linkAssociation_deviceId,
    linkAssociation_linkAssociationState,

    -- ** Location
    location_latitude,
    location_address,
    location_longitude,

    -- ** Site
    site_state,
    site_location,
    site_createdAt,
    site_globalNetworkId,
    site_siteId,
    site_siteArn,
    site_description,
    site_tags,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TransitGatewayConnectPeerAssociation
    transitGatewayConnectPeerAssociation_state,
    transitGatewayConnectPeerAssociation_globalNetworkId,
    transitGatewayConnectPeerAssociation_transitGatewayConnectPeerArn,
    transitGatewayConnectPeerAssociation_linkId,
    transitGatewayConnectPeerAssociation_deviceId,

    -- ** TransitGatewayRegistration
    transitGatewayRegistration_state,
    transitGatewayRegistration_globalNetworkId,
    transitGatewayRegistration_transitGatewayArn,

    -- ** TransitGatewayRegistrationStateReason
    transitGatewayRegistrationStateReason_code,
    transitGatewayRegistrationStateReason_message,
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

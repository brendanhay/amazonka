{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PrivateNetworks.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Lens
  ( -- * Operations

    -- ** AcknowledgeOrderReceipt
    acknowledgeOrderReceipt_orderArn,
    acknowledgeOrderReceiptResponse_httpStatus,
    acknowledgeOrderReceiptResponse_order,

    -- ** ActivateDeviceIdentifier
    activateDeviceIdentifier_clientToken,
    activateDeviceIdentifier_deviceIdentifierArn,
    activateDeviceIdentifierResponse_tags,
    activateDeviceIdentifierResponse_httpStatus,
    activateDeviceIdentifierResponse_deviceIdentifier,

    -- ** ActivateNetworkSite
    activateNetworkSite_clientToken,
    activateNetworkSite_networkSiteArn,
    activateNetworkSite_shippingAddress,
    activateNetworkSiteResponse_networkSite,
    activateNetworkSiteResponse_httpStatus,

    -- ** ConfigureAccessPoint
    configureAccessPoint_cpiUserId,
    configureAccessPoint_cpiUsername,
    configureAccessPoint_position,
    configureAccessPoint_cpiUserPassword,
    configureAccessPoint_cpiSecretKey,
    configureAccessPoint_accessPointArn,
    configureAccessPointResponse_httpStatus,
    configureAccessPointResponse_accessPoint,

    -- ** CreateNetwork
    createNetwork_tags,
    createNetwork_clientToken,
    createNetwork_description,
    createNetwork_networkName,
    createNetworkResponse_tags,
    createNetworkResponse_httpStatus,
    createNetworkResponse_network,

    -- ** CreateNetworkSite
    createNetworkSite_tags,
    createNetworkSite_clientToken,
    createNetworkSite_description,
    createNetworkSite_availabilityZone,
    createNetworkSite_pendingPlan,
    createNetworkSite_availabilityZoneId,
    createNetworkSite_networkArn,
    createNetworkSite_networkSiteName,
    createNetworkSiteResponse_tags,
    createNetworkSiteResponse_networkSite,
    createNetworkSiteResponse_httpStatus,

    -- ** DeactivateDeviceIdentifier
    deactivateDeviceIdentifier_clientToken,
    deactivateDeviceIdentifier_deviceIdentifierArn,
    deactivateDeviceIdentifierResponse_httpStatus,
    deactivateDeviceIdentifierResponse_deviceIdentifier,

    -- ** DeleteNetwork
    deleteNetwork_clientToken,
    deleteNetwork_networkArn,
    deleteNetworkResponse_httpStatus,
    deleteNetworkResponse_network,

    -- ** DeleteNetworkSite
    deleteNetworkSite_clientToken,
    deleteNetworkSite_networkSiteArn,
    deleteNetworkSiteResponse_networkSite,
    deleteNetworkSiteResponse_httpStatus,

    -- ** GetDeviceIdentifier
    getDeviceIdentifier_deviceIdentifierArn,
    getDeviceIdentifierResponse_tags,
    getDeviceIdentifierResponse_deviceIdentifier,
    getDeviceIdentifierResponse_httpStatus,

    -- ** GetNetwork
    getNetwork_networkArn,
    getNetworkResponse_tags,
    getNetworkResponse_httpStatus,
    getNetworkResponse_network,

    -- ** GetNetworkResource
    getNetworkResource_networkResourceArn,
    getNetworkResourceResponse_tags,
    getNetworkResourceResponse_httpStatus,
    getNetworkResourceResponse_networkResource,

    -- ** GetNetworkSite
    getNetworkSite_networkSiteArn,
    getNetworkSiteResponse_tags,
    getNetworkSiteResponse_networkSite,
    getNetworkSiteResponse_httpStatus,

    -- ** GetOrder
    getOrder_orderArn,
    getOrderResponse_tags,
    getOrderResponse_httpStatus,
    getOrderResponse_order,

    -- ** ListDeviceIdentifiers
    listDeviceIdentifiers_filters,
    listDeviceIdentifiers_maxResults,
    listDeviceIdentifiers_startToken,
    listDeviceIdentifiers_networkArn,
    listDeviceIdentifiersResponse_nextToken,
    listDeviceIdentifiersResponse_deviceIdentifiers,
    listDeviceIdentifiersResponse_httpStatus,

    -- ** ListNetworkResources
    listNetworkResources_filters,
    listNetworkResources_maxResults,
    listNetworkResources_startToken,
    listNetworkResources_networkArn,
    listNetworkResourcesResponse_nextToken,
    listNetworkResourcesResponse_networkResources,
    listNetworkResourcesResponse_httpStatus,

    -- ** ListNetworkSites
    listNetworkSites_filters,
    listNetworkSites_maxResults,
    listNetworkSites_startToken,
    listNetworkSites_networkArn,
    listNetworkSitesResponse_nextToken,
    listNetworkSitesResponse_networkSites,
    listNetworkSitesResponse_httpStatus,

    -- ** ListNetworks
    listNetworks_filters,
    listNetworks_maxResults,
    listNetworks_startToken,
    listNetworksResponse_networks,
    listNetworksResponse_nextToken,
    listNetworksResponse_httpStatus,

    -- ** ListOrders
    listOrders_filters,
    listOrders_maxResults,
    listOrders_startToken,
    listOrders_networkArn,
    listOrdersResponse_nextToken,
    listOrdersResponse_orders,
    listOrdersResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** Ping
    pingResponse_status,
    pingResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateNetworkSite
    updateNetworkSite_clientToken,
    updateNetworkSite_description,
    updateNetworkSite_networkSiteArn,
    updateNetworkSiteResponse_tags,
    updateNetworkSiteResponse_networkSite,

    -- ** UpdateNetworkSitePlan
    updateNetworkSitePlan_clientToken,
    updateNetworkSitePlan_networkSiteArn,
    updateNetworkSitePlan_pendingPlan,
    updateNetworkSiteResponse_tags,
    updateNetworkSiteResponse_networkSite,

    -- * Types

    -- ** Address
    address_company,
    address_street3,
    address_phoneNumber,
    address_street2,
    address_city,
    address_country,
    address_name,
    address_postalCode,
    address_stateOrProvince,
    address_street1,

    -- ** DeviceIdentifier
    deviceIdentifier_imsi,
    deviceIdentifier_trafficGroupArn,
    deviceIdentifier_networkArn,
    deviceIdentifier_status,
    deviceIdentifier_orderArn,
    deviceIdentifier_vendor,
    deviceIdentifier_createdAt,
    deviceIdentifier_iccid,
    deviceIdentifier_deviceIdentifierArn,

    -- ** NameValuePair
    nameValuePair_value,
    nameValuePair_name,

    -- ** Network
    network_statusReason,
    network_description,
    network_createdAt,
    network_networkArn,
    network_networkName,
    network_status,

    -- ** NetworkResource
    networkResource_type,
    networkResource_model,
    networkResource_networkSiteArn,
    networkResource_statusReason,
    networkResource_networkArn,
    networkResource_status,
    networkResource_description,
    networkResource_orderArn,
    networkResource_health,
    networkResource_serialNumber,
    networkResource_attributes,
    networkResource_networkResourceArn,
    networkResource_position,
    networkResource_vendor,
    networkResource_createdAt,

    -- ** NetworkResourceDefinition
    networkResourceDefinition_options,
    networkResourceDefinition_count,
    networkResourceDefinition_type,

    -- ** NetworkSite
    networkSite_statusReason,
    networkSite_description,
    networkSite_availabilityZone,
    networkSite_pendingPlan,
    networkSite_currentPlan,
    networkSite_createdAt,
    networkSite_availabilityZoneId,
    networkSite_networkArn,
    networkSite_networkSiteArn,
    networkSite_networkSiteName,
    networkSite_status,

    -- ** Order
    order_shippingAddress,
    order_acknowledgmentStatus,
    order_networkSiteArn,
    order_networkArn,
    order_orderArn,
    order_trackingInformation,
    order_createdAt,

    -- ** Position
    position_longitude,
    position_latitude,
    position_elevation,
    position_elevationReference,
    position_elevationUnit,

    -- ** SitePlan
    sitePlan_resourceDefinitions,
    sitePlan_options,

    -- ** TrackingInformation
    trackingInformation_trackingNumber,

    -- ** UpdateNetworkSiteResponse
    updateNetworkSiteResponse_tags,
    updateNetworkSiteResponse_networkSite,
  )
where

import Amazonka.PrivateNetworks.AcknowledgeOrderReceipt
import Amazonka.PrivateNetworks.ActivateDeviceIdentifier
import Amazonka.PrivateNetworks.ActivateNetworkSite
import Amazonka.PrivateNetworks.ConfigureAccessPoint
import Amazonka.PrivateNetworks.CreateNetwork
import Amazonka.PrivateNetworks.CreateNetworkSite
import Amazonka.PrivateNetworks.DeactivateDeviceIdentifier
import Amazonka.PrivateNetworks.DeleteNetwork
import Amazonka.PrivateNetworks.DeleteNetworkSite
import Amazonka.PrivateNetworks.GetDeviceIdentifier
import Amazonka.PrivateNetworks.GetNetwork
import Amazonka.PrivateNetworks.GetNetworkResource
import Amazonka.PrivateNetworks.GetNetworkSite
import Amazonka.PrivateNetworks.GetOrder
import Amazonka.PrivateNetworks.ListDeviceIdentifiers
import Amazonka.PrivateNetworks.ListNetworkResources
import Amazonka.PrivateNetworks.ListNetworkSites
import Amazonka.PrivateNetworks.ListNetworks
import Amazonka.PrivateNetworks.ListOrders
import Amazonka.PrivateNetworks.ListTagsForResource
import Amazonka.PrivateNetworks.Ping
import Amazonka.PrivateNetworks.TagResource
import Amazonka.PrivateNetworks.Types.Address
import Amazonka.PrivateNetworks.Types.DeviceIdentifier
import Amazonka.PrivateNetworks.Types.NameValuePair
import Amazonka.PrivateNetworks.Types.Network
import Amazonka.PrivateNetworks.Types.NetworkResource
import Amazonka.PrivateNetworks.Types.NetworkResourceDefinition
import Amazonka.PrivateNetworks.Types.NetworkSite
import Amazonka.PrivateNetworks.Types.Order
import Amazonka.PrivateNetworks.Types.Position
import Amazonka.PrivateNetworks.Types.SitePlan
import Amazonka.PrivateNetworks.Types.TrackingInformation
import Amazonka.PrivateNetworks.Types.UpdateNetworkSiteResponse
import Amazonka.PrivateNetworks.UntagResource
import Amazonka.PrivateNetworks.UpdateNetworkSite
import Amazonka.PrivateNetworks.UpdateNetworkSitePlan

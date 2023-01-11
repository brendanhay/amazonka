{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Outposts.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Lens
  ( -- * Operations

    -- ** CancelOrder
    cancelOrder_orderId,
    cancelOrderResponse_httpStatus,

    -- ** CreateOrder
    createOrder_paymentTerm,
    createOrder_outpostIdentifier,
    createOrder_lineItems,
    createOrder_paymentOption,
    createOrderResponse_order,
    createOrderResponse_httpStatus,

    -- ** CreateOutpost
    createOutpost_availabilityZone,
    createOutpost_availabilityZoneId,
    createOutpost_description,
    createOutpost_supportedHardwareType,
    createOutpost_tags,
    createOutpost_name,
    createOutpost_siteId,
    createOutpostResponse_outpost,
    createOutpostResponse_httpStatus,

    -- ** CreateSite
    createSite_description,
    createSite_notes,
    createSite_operatingAddress,
    createSite_rackPhysicalProperties,
    createSite_shippingAddress,
    createSite_tags,
    createSite_name,
    createSiteResponse_site,
    createSiteResponse_httpStatus,

    -- ** DeleteOutpost
    deleteOutpost_outpostId,
    deleteOutpostResponse_httpStatus,

    -- ** DeleteSite
    deleteSite_siteId,
    deleteSiteResponse_httpStatus,

    -- ** GetCatalogItem
    getCatalogItem_catalogItemId,
    getCatalogItemResponse_catalogItem,
    getCatalogItemResponse_httpStatus,

    -- ** GetConnection
    getConnection_connectionId,
    getConnectionResponse_connectionDetails,
    getConnectionResponse_connectionId,
    getConnectionResponse_httpStatus,

    -- ** GetOrder
    getOrder_orderId,
    getOrderResponse_order,
    getOrderResponse_httpStatus,

    -- ** GetOutpost
    getOutpost_outpostId,
    getOutpostResponse_outpost,
    getOutpostResponse_httpStatus,

    -- ** GetOutpostInstanceTypes
    getOutpostInstanceTypes_maxResults,
    getOutpostInstanceTypes_nextToken,
    getOutpostInstanceTypes_outpostId,
    getOutpostInstanceTypesResponse_instanceTypes,
    getOutpostInstanceTypesResponse_nextToken,
    getOutpostInstanceTypesResponse_outpostArn,
    getOutpostInstanceTypesResponse_outpostId,
    getOutpostInstanceTypesResponse_httpStatus,

    -- ** GetSite
    getSite_siteId,
    getSiteResponse_site,
    getSiteResponse_httpStatus,

    -- ** GetSiteAddress
    getSiteAddress_siteId,
    getSiteAddress_addressType,
    getSiteAddressResponse_address,
    getSiteAddressResponse_addressType,
    getSiteAddressResponse_siteId,
    getSiteAddressResponse_httpStatus,

    -- ** ListAssets
    listAssets_hostIdFilter,
    listAssets_maxResults,
    listAssets_nextToken,
    listAssets_statusFilter,
    listAssets_outpostIdentifier,
    listAssetsResponse_assets,
    listAssetsResponse_nextToken,
    listAssetsResponse_httpStatus,

    -- ** ListCatalogItems
    listCatalogItems_eC2FamilyFilter,
    listCatalogItems_itemClassFilter,
    listCatalogItems_maxResults,
    listCatalogItems_nextToken,
    listCatalogItems_supportedStorageFilter,
    listCatalogItemsResponse_catalogItems,
    listCatalogItemsResponse_nextToken,
    listCatalogItemsResponse_httpStatus,

    -- ** ListOrders
    listOrders_maxResults,
    listOrders_nextToken,
    listOrders_outpostIdentifierFilter,
    listOrdersResponse_nextToken,
    listOrdersResponse_orders,
    listOrdersResponse_httpStatus,

    -- ** ListOutposts
    listOutposts_availabilityZoneFilter,
    listOutposts_availabilityZoneIdFilter,
    listOutposts_lifeCycleStatusFilter,
    listOutposts_maxResults,
    listOutposts_nextToken,
    listOutpostsResponse_nextToken,
    listOutpostsResponse_outposts,
    listOutpostsResponse_httpStatus,

    -- ** ListSites
    listSites_maxResults,
    listSites_nextToken,
    listSites_operatingAddressCityFilter,
    listSites_operatingAddressCountryCodeFilter,
    listSites_operatingAddressStateOrRegionFilter,
    listSitesResponse_nextToken,
    listSitesResponse_sites,
    listSitesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartConnection
    startConnection_deviceSerialNumber,
    startConnection_assetId,
    startConnection_clientPublicKey,
    startConnection_networkInterfaceDeviceIndex,
    startConnectionResponse_connectionId,
    startConnectionResponse_underlayIpAddress,
    startConnectionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateOutpost
    updateOutpost_description,
    updateOutpost_name,
    updateOutpost_supportedHardwareType,
    updateOutpost_outpostId,
    updateOutpostResponse_outpost,
    updateOutpostResponse_httpStatus,

    -- ** UpdateSite
    updateSite_description,
    updateSite_name,
    updateSite_notes,
    updateSite_siteId,
    updateSiteResponse_site,
    updateSiteResponse_httpStatus,

    -- ** UpdateSiteAddress
    updateSiteAddress_siteId,
    updateSiteAddress_addressType,
    updateSiteAddress_address,
    updateSiteAddressResponse_address,
    updateSiteAddressResponse_addressType,
    updateSiteAddressResponse_httpStatus,

    -- ** UpdateSiteRackPhysicalProperties
    updateSiteRackPhysicalProperties_fiberOpticCableType,
    updateSiteRackPhysicalProperties_maximumSupportedWeightLbs,
    updateSiteRackPhysicalProperties_opticalStandard,
    updateSiteRackPhysicalProperties_powerConnector,
    updateSiteRackPhysicalProperties_powerDrawKva,
    updateSiteRackPhysicalProperties_powerFeedDrop,
    updateSiteRackPhysicalProperties_powerPhase,
    updateSiteRackPhysicalProperties_uplinkCount,
    updateSiteRackPhysicalProperties_uplinkGbps,
    updateSiteRackPhysicalProperties_siteId,
    updateSiteRackPhysicalPropertiesResponse_site,
    updateSiteRackPhysicalPropertiesResponse_httpStatus,

    -- * Types

    -- ** Address
    address_addressLine2,
    address_addressLine3,
    address_contactName,
    address_contactPhoneNumber,
    address_districtOrCounty,
    address_municipality,
    address_addressLine1,
    address_city,
    address_stateOrRegion,
    address_postalCode,
    address_countryCode,

    -- ** AssetInfo
    assetInfo_assetId,
    assetInfo_assetLocation,
    assetInfo_assetType,
    assetInfo_computeAttributes,
    assetInfo_rackId,

    -- ** AssetLocation
    assetLocation_rackElevation,

    -- ** CatalogItem
    catalogItem_catalogItemId,
    catalogItem_eC2Capacities,
    catalogItem_itemStatus,
    catalogItem_powerKva,
    catalogItem_supportedStorage,
    catalogItem_supportedUplinkGbps,
    catalogItem_weightLbs,

    -- ** ComputeAttributes
    computeAttributes_hostId,
    computeAttributes_state,

    -- ** ConnectionDetails
    connectionDetails_allowedIps,
    connectionDetails_clientPublicKey,
    connectionDetails_clientTunnelAddress,
    connectionDetails_serverEndpoint,
    connectionDetails_serverPublicKey,
    connectionDetails_serverTunnelAddress,

    -- ** EC2Capacity
    eC2Capacity_family,
    eC2Capacity_maxSize,
    eC2Capacity_quantity,

    -- ** InstanceTypeItem
    instanceTypeItem_instanceType,

    -- ** LineItem
    lineItem_assetInformationList,
    lineItem_catalogItemId,
    lineItem_lineItemId,
    lineItem_quantity,
    lineItem_shipmentInformation,
    lineItem_status,

    -- ** LineItemAssetInformation
    lineItemAssetInformation_assetId,
    lineItemAssetInformation_macAddressList,

    -- ** LineItemRequest
    lineItemRequest_catalogItemId,
    lineItemRequest_quantity,

    -- ** Order
    order_lineItems,
    order_orderFulfilledDate,
    order_orderId,
    order_orderSubmissionDate,
    order_outpostId,
    order_paymentOption,
    order_status,

    -- ** OrderSummary
    orderSummary_lineItemCountsByStatus,
    orderSummary_orderFulfilledDate,
    orderSummary_orderId,
    orderSummary_orderSubmissionDate,
    orderSummary_orderType,
    orderSummary_outpostId,
    orderSummary_status,

    -- ** Outpost
    outpost_availabilityZone,
    outpost_availabilityZoneId,
    outpost_description,
    outpost_lifeCycleStatus,
    outpost_name,
    outpost_outpostArn,
    outpost_outpostId,
    outpost_ownerId,
    outpost_siteArn,
    outpost_siteId,
    outpost_supportedHardwareType,
    outpost_tags,

    -- ** RackPhysicalProperties
    rackPhysicalProperties_fiberOpticCableType,
    rackPhysicalProperties_maximumSupportedWeightLbs,
    rackPhysicalProperties_opticalStandard,
    rackPhysicalProperties_powerConnector,
    rackPhysicalProperties_powerDrawKva,
    rackPhysicalProperties_powerFeedDrop,
    rackPhysicalProperties_powerPhase,
    rackPhysicalProperties_uplinkCount,
    rackPhysicalProperties_uplinkGbps,

    -- ** ShipmentInformation
    shipmentInformation_shipmentCarrier,
    shipmentInformation_shipmentTrackingNumber,

    -- ** Site
    site_accountId,
    site_description,
    site_name,
    site_notes,
    site_operatingAddressCity,
    site_operatingAddressCountryCode,
    site_operatingAddressStateOrRegion,
    site_rackPhysicalProperties,
    site_siteArn,
    site_siteId,
    site_tags,
  )
where

import Amazonka.Outposts.CancelOrder
import Amazonka.Outposts.CreateOrder
import Amazonka.Outposts.CreateOutpost
import Amazonka.Outposts.CreateSite
import Amazonka.Outposts.DeleteOutpost
import Amazonka.Outposts.DeleteSite
import Amazonka.Outposts.GetCatalogItem
import Amazonka.Outposts.GetConnection
import Amazonka.Outposts.GetOrder
import Amazonka.Outposts.GetOutpost
import Amazonka.Outposts.GetOutpostInstanceTypes
import Amazonka.Outposts.GetSite
import Amazonka.Outposts.GetSiteAddress
import Amazonka.Outposts.ListAssets
import Amazonka.Outposts.ListCatalogItems
import Amazonka.Outposts.ListOrders
import Amazonka.Outposts.ListOutposts
import Amazonka.Outposts.ListSites
import Amazonka.Outposts.ListTagsForResource
import Amazonka.Outposts.StartConnection
import Amazonka.Outposts.TagResource
import Amazonka.Outposts.Types.Address
import Amazonka.Outposts.Types.AssetInfo
import Amazonka.Outposts.Types.AssetLocation
import Amazonka.Outposts.Types.CatalogItem
import Amazonka.Outposts.Types.ComputeAttributes
import Amazonka.Outposts.Types.ConnectionDetails
import Amazonka.Outposts.Types.EC2Capacity
import Amazonka.Outposts.Types.InstanceTypeItem
import Amazonka.Outposts.Types.LineItem
import Amazonka.Outposts.Types.LineItemAssetInformation
import Amazonka.Outposts.Types.LineItemRequest
import Amazonka.Outposts.Types.Order
import Amazonka.Outposts.Types.OrderSummary
import Amazonka.Outposts.Types.Outpost
import Amazonka.Outposts.Types.RackPhysicalProperties
import Amazonka.Outposts.Types.ShipmentInformation
import Amazonka.Outposts.Types.Site
import Amazonka.Outposts.UntagResource
import Amazonka.Outposts.UpdateOutpost
import Amazonka.Outposts.UpdateSite
import Amazonka.Outposts.UpdateSiteAddress
import Amazonka.Outposts.UpdateSiteRackPhysicalProperties

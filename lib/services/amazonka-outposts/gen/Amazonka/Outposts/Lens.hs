{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Outposts.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createOutpost_tags,
    createOutpost_supportedHardwareType,
    createOutpost_availabilityZone,
    createOutpost_description,
    createOutpost_availabilityZoneId,
    createOutpost_name,
    createOutpost_siteId,
    createOutpostResponse_outpost,
    createOutpostResponse_httpStatus,

    -- ** CreateSite
    createSite_tags,
    createSite_shippingAddress,
    createSite_operatingAddress,
    createSite_description,
    createSite_notes,
    createSite_rackPhysicalProperties,
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
    getOutpostInstanceTypes_nextToken,
    getOutpostInstanceTypes_maxResults,
    getOutpostInstanceTypes_outpostId,
    getOutpostInstanceTypesResponse_nextToken,
    getOutpostInstanceTypesResponse_outpostId,
    getOutpostInstanceTypesResponse_outpostArn,
    getOutpostInstanceTypesResponse_instanceTypes,
    getOutpostInstanceTypesResponse_httpStatus,

    -- ** GetSite
    getSite_siteId,
    getSiteResponse_site,
    getSiteResponse_httpStatus,

    -- ** GetSiteAddress
    getSiteAddress_siteId,
    getSiteAddress_addressType,
    getSiteAddressResponse_addressType,
    getSiteAddressResponse_siteId,
    getSiteAddressResponse_address,
    getSiteAddressResponse_httpStatus,

    -- ** ListAssets
    listAssets_nextToken,
    listAssets_hostIdFilter,
    listAssets_maxResults,
    listAssets_statusFilter,
    listAssets_outpostIdentifier,
    listAssetsResponse_nextToken,
    listAssetsResponse_assets,
    listAssetsResponse_httpStatus,

    -- ** ListCatalogItems
    listCatalogItems_nextToken,
    listCatalogItems_itemClassFilter,
    listCatalogItems_supportedStorageFilter,
    listCatalogItems_eC2FamilyFilter,
    listCatalogItems_maxResults,
    listCatalogItemsResponse_nextToken,
    listCatalogItemsResponse_catalogItems,
    listCatalogItemsResponse_httpStatus,

    -- ** ListOrders
    listOrders_nextToken,
    listOrders_outpostIdentifierFilter,
    listOrders_maxResults,
    listOrdersResponse_nextToken,
    listOrdersResponse_orders,
    listOrdersResponse_httpStatus,

    -- ** ListOutposts
    listOutposts_nextToken,
    listOutposts_maxResults,
    listOutposts_lifeCycleStatusFilter,
    listOutposts_availabilityZoneFilter,
    listOutposts_availabilityZoneIdFilter,
    listOutpostsResponse_nextToken,
    listOutpostsResponse_outposts,
    listOutpostsResponse_httpStatus,

    -- ** ListSites
    listSites_operatingAddressCityFilter,
    listSites_nextToken,
    listSites_maxResults,
    listSites_operatingAddressCountryCodeFilter,
    listSites_operatingAddressStateOrRegionFilter,
    listSitesResponse_sites,
    listSitesResponse_nextToken,
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
    updateOutpost_name,
    updateOutpost_supportedHardwareType,
    updateOutpost_description,
    updateOutpost_outpostId,
    updateOutpostResponse_outpost,
    updateOutpostResponse_httpStatus,

    -- ** UpdateSite
    updateSite_name,
    updateSite_description,
    updateSite_notes,
    updateSite_siteId,
    updateSiteResponse_site,
    updateSiteResponse_httpStatus,

    -- ** UpdateSiteAddress
    updateSiteAddress_siteId,
    updateSiteAddress_addressType,
    updateSiteAddress_address,
    updateSiteAddressResponse_addressType,
    updateSiteAddressResponse_address,
    updateSiteAddressResponse_httpStatus,

    -- ** UpdateSiteRackPhysicalProperties
    updateSiteRackPhysicalProperties_powerPhase,
    updateSiteRackPhysicalProperties_powerDrawKva,
    updateSiteRackPhysicalProperties_fiberOpticCableType,
    updateSiteRackPhysicalProperties_maximumSupportedWeightLbs,
    updateSiteRackPhysicalProperties_powerConnector,
    updateSiteRackPhysicalProperties_opticalStandard,
    updateSiteRackPhysicalProperties_powerFeedDrop,
    updateSiteRackPhysicalProperties_uplinkCount,
    updateSiteRackPhysicalProperties_uplinkGbps,
    updateSiteRackPhysicalProperties_siteId,
    updateSiteRackPhysicalPropertiesResponse_site,
    updateSiteRackPhysicalPropertiesResponse_httpStatus,

    -- * Types

    -- ** Address
    address_addressLine2,
    address_contactName,
    address_contactPhoneNumber,
    address_districtOrCounty,
    address_addressLine3,
    address_municipality,
    address_addressLine1,
    address_city,
    address_stateOrRegion,
    address_postalCode,
    address_countryCode,

    -- ** AssetInfo
    assetInfo_assetLocation,
    assetInfo_assetId,
    assetInfo_computeAttributes,
    assetInfo_assetType,
    assetInfo_rackId,

    -- ** AssetLocation
    assetLocation_rackElevation,

    -- ** CatalogItem
    catalogItem_powerKva,
    catalogItem_weightLbs,
    catalogItem_eC2Capacities,
    catalogItem_catalogItemId,
    catalogItem_itemStatus,
    catalogItem_supportedUplinkGbps,
    catalogItem_supportedStorage,

    -- ** ComputeAttributes
    computeAttributes_hostId,
    computeAttributes_state,

    -- ** ConnectionDetails
    connectionDetails_serverEndpoint,
    connectionDetails_serverPublicKey,
    connectionDetails_clientPublicKey,
    connectionDetails_clientTunnelAddress,
    connectionDetails_serverTunnelAddress,
    connectionDetails_allowedIps,

    -- ** EC2Capacity
    eC2Capacity_quantity,
    eC2Capacity_family,
    eC2Capacity_maxSize,

    -- ** InstanceTypeItem
    instanceTypeItem_instanceType,

    -- ** LineItem
    lineItem_quantity,
    lineItem_assetInformationList,
    lineItem_status,
    lineItem_catalogItemId,
    lineItem_shipmentInformation,
    lineItem_lineItemId,

    -- ** LineItemAssetInformation
    lineItemAssetInformation_macAddressList,
    lineItemAssetInformation_assetId,

    -- ** LineItemRequest
    lineItemRequest_quantity,
    lineItemRequest_catalogItemId,

    -- ** Order
    order_outpostId,
    order_orderFulfilledDate,
    order_lineItems,
    order_status,
    order_orderId,
    order_orderSubmissionDate,
    order_paymentOption,

    -- ** OrderSummary
    orderSummary_outpostId,
    orderSummary_orderFulfilledDate,
    orderSummary_lineItemCountsByStatus,
    orderSummary_status,
    orderSummary_orderId,
    orderSummary_orderType,
    orderSummary_orderSubmissionDate,

    -- ** Outpost
    outpost_tags,
    outpost_name,
    outpost_outpostId,
    outpost_outpostArn,
    outpost_ownerId,
    outpost_siteArn,
    outpost_supportedHardwareType,
    outpost_availabilityZone,
    outpost_description,
    outpost_siteId,
    outpost_lifeCycleStatus,
    outpost_availabilityZoneId,

    -- ** RackPhysicalProperties
    rackPhysicalProperties_powerPhase,
    rackPhysicalProperties_powerDrawKva,
    rackPhysicalProperties_fiberOpticCableType,
    rackPhysicalProperties_maximumSupportedWeightLbs,
    rackPhysicalProperties_powerConnector,
    rackPhysicalProperties_opticalStandard,
    rackPhysicalProperties_powerFeedDrop,
    rackPhysicalProperties_uplinkCount,
    rackPhysicalProperties_uplinkGbps,

    -- ** ShipmentInformation
    shipmentInformation_shipmentTrackingNumber,
    shipmentInformation_shipmentCarrier,

    -- ** Site
    site_tags,
    site_name,
    site_siteArn,
    site_description,
    site_siteId,
    site_accountId,
    site_operatingAddressCity,
    site_notes,
    site_rackPhysicalProperties,
    site_operatingAddressStateOrRegion,
    site_operatingAddressCountryCode,
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

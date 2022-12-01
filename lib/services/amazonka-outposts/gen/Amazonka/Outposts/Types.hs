{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Outposts.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _NotFoundException,
    _ServiceQuotaExceededException,
    _ConflictException,
    _ValidationException,

    -- * AddressType
    AddressType (..),

    -- * AssetState
    AssetState (..),

    -- * AssetType
    AssetType (..),

    -- * CatalogItemClass
    CatalogItemClass (..),

    -- * CatalogItemStatus
    CatalogItemStatus (..),

    -- * ComputeAssetState
    ComputeAssetState (..),

    -- * FiberOpticCableType
    FiberOpticCableType (..),

    -- * LineItemStatus
    LineItemStatus (..),

    -- * MaximumSupportedWeightLbs
    MaximumSupportedWeightLbs (..),

    -- * OpticalStandard
    OpticalStandard (..),

    -- * OrderStatus
    OrderStatus (..),

    -- * OrderType
    OrderType (..),

    -- * PaymentOption
    PaymentOption (..),

    -- * PaymentTerm
    PaymentTerm (..),

    -- * PowerConnector
    PowerConnector (..),

    -- * PowerDrawKva
    PowerDrawKva (..),

    -- * PowerFeedDrop
    PowerFeedDrop (..),

    -- * PowerPhase
    PowerPhase (..),

    -- * ShipmentCarrier
    ShipmentCarrier (..),

    -- * SupportedHardwareType
    SupportedHardwareType (..),

    -- * SupportedStorageEnum
    SupportedStorageEnum (..),

    -- * UplinkCount
    UplinkCount (..),

    -- * UplinkGbps
    UplinkGbps (..),

    -- * Address
    Address (..),
    newAddress,
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

    -- * AssetInfo
    AssetInfo (..),
    newAssetInfo,
    assetInfo_assetLocation,
    assetInfo_assetId,
    assetInfo_computeAttributes,
    assetInfo_assetType,
    assetInfo_rackId,

    -- * AssetLocation
    AssetLocation (..),
    newAssetLocation,
    assetLocation_rackElevation,

    -- * CatalogItem
    CatalogItem (..),
    newCatalogItem,
    catalogItem_powerKva,
    catalogItem_weightLbs,
    catalogItem_eC2Capacities,
    catalogItem_catalogItemId,
    catalogItem_itemStatus,
    catalogItem_supportedUplinkGbps,
    catalogItem_supportedStorage,

    -- * ComputeAttributes
    ComputeAttributes (..),
    newComputeAttributes,
    computeAttributes_hostId,
    computeAttributes_state,

    -- * ConnectionDetails
    ConnectionDetails (..),
    newConnectionDetails,
    connectionDetails_serverEndpoint,
    connectionDetails_serverPublicKey,
    connectionDetails_clientPublicKey,
    connectionDetails_clientTunnelAddress,
    connectionDetails_serverTunnelAddress,
    connectionDetails_allowedIps,

    -- * EC2Capacity
    EC2Capacity (..),
    newEC2Capacity,
    eC2Capacity_quantity,
    eC2Capacity_family,
    eC2Capacity_maxSize,

    -- * InstanceTypeItem
    InstanceTypeItem (..),
    newInstanceTypeItem,
    instanceTypeItem_instanceType,

    -- * LineItem
    LineItem (..),
    newLineItem,
    lineItem_quantity,
    lineItem_assetInformationList,
    lineItem_status,
    lineItem_catalogItemId,
    lineItem_shipmentInformation,
    lineItem_lineItemId,

    -- * LineItemAssetInformation
    LineItemAssetInformation (..),
    newLineItemAssetInformation,
    lineItemAssetInformation_macAddressList,
    lineItemAssetInformation_assetId,

    -- * LineItemRequest
    LineItemRequest (..),
    newLineItemRequest,
    lineItemRequest_quantity,
    lineItemRequest_catalogItemId,

    -- * Order
    Order (..),
    newOrder,
    order_outpostId,
    order_orderFulfilledDate,
    order_lineItems,
    order_status,
    order_orderId,
    order_orderSubmissionDate,
    order_paymentOption,

    -- * OrderSummary
    OrderSummary (..),
    newOrderSummary,
    orderSummary_outpostId,
    orderSummary_orderFulfilledDate,
    orderSummary_lineItemCountsByStatus,
    orderSummary_status,
    orderSummary_orderId,
    orderSummary_orderType,
    orderSummary_orderSubmissionDate,

    -- * Outpost
    Outpost (..),
    newOutpost,
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

    -- * RackPhysicalProperties
    RackPhysicalProperties (..),
    newRackPhysicalProperties,
    rackPhysicalProperties_powerPhase,
    rackPhysicalProperties_powerDrawKva,
    rackPhysicalProperties_fiberOpticCableType,
    rackPhysicalProperties_maximumSupportedWeightLbs,
    rackPhysicalProperties_powerConnector,
    rackPhysicalProperties_opticalStandard,
    rackPhysicalProperties_powerFeedDrop,
    rackPhysicalProperties_uplinkCount,
    rackPhysicalProperties_uplinkGbps,

    -- * ShipmentInformation
    ShipmentInformation (..),
    newShipmentInformation,
    shipmentInformation_shipmentTrackingNumber,
    shipmentInformation_shipmentCarrier,

    -- * Site
    Site (..),
    newSite,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Outposts.Types.Address
import Amazonka.Outposts.Types.AddressType
import Amazonka.Outposts.Types.AssetInfo
import Amazonka.Outposts.Types.AssetLocation
import Amazonka.Outposts.Types.AssetState
import Amazonka.Outposts.Types.AssetType
import Amazonka.Outposts.Types.CatalogItem
import Amazonka.Outposts.Types.CatalogItemClass
import Amazonka.Outposts.Types.CatalogItemStatus
import Amazonka.Outposts.Types.ComputeAssetState
import Amazonka.Outposts.Types.ComputeAttributes
import Amazonka.Outposts.Types.ConnectionDetails
import Amazonka.Outposts.Types.EC2Capacity
import Amazonka.Outposts.Types.FiberOpticCableType
import Amazonka.Outposts.Types.InstanceTypeItem
import Amazonka.Outposts.Types.LineItem
import Amazonka.Outposts.Types.LineItemAssetInformation
import Amazonka.Outposts.Types.LineItemRequest
import Amazonka.Outposts.Types.LineItemStatus
import Amazonka.Outposts.Types.MaximumSupportedWeightLbs
import Amazonka.Outposts.Types.OpticalStandard
import Amazonka.Outposts.Types.Order
import Amazonka.Outposts.Types.OrderStatus
import Amazonka.Outposts.Types.OrderSummary
import Amazonka.Outposts.Types.OrderType
import Amazonka.Outposts.Types.Outpost
import Amazonka.Outposts.Types.PaymentOption
import Amazonka.Outposts.Types.PaymentTerm
import Amazonka.Outposts.Types.PowerConnector
import Amazonka.Outposts.Types.PowerDrawKva
import Amazonka.Outposts.Types.PowerFeedDrop
import Amazonka.Outposts.Types.PowerPhase
import Amazonka.Outposts.Types.RackPhysicalProperties
import Amazonka.Outposts.Types.ShipmentCarrier
import Amazonka.Outposts.Types.ShipmentInformation
import Amazonka.Outposts.Types.Site
import Amazonka.Outposts.Types.SupportedHardwareType
import Amazonka.Outposts.Types.SupportedStorageEnum
import Amazonka.Outposts.Types.UplinkCount
import Amazonka.Outposts.Types.UplinkGbps
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-12-03@ of the Amazon Outposts SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Outposts",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "outposts",
      Core.signingName = "outposts",
      Core.version = "2019-12-03",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Outposts",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have permission to perform this operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An internal error has occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified request is not valid.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | You have exceeded a service quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Updating or deleting this resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | A parameter is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

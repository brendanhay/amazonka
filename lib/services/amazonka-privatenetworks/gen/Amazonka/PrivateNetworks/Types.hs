{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PrivateNetworks.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _ValidationException,

    -- * AcknowledgmentStatus
    AcknowledgmentStatus (..),

    -- * DeviceIdentifierFilterKeys
    DeviceIdentifierFilterKeys (..),

    -- * DeviceIdentifierStatus
    DeviceIdentifierStatus (..),

    -- * ElevationReference
    ElevationReference (..),

    -- * ElevationUnit
    ElevationUnit (..),

    -- * HealthStatus
    HealthStatus (..),

    -- * NetworkFilterKeys
    NetworkFilterKeys (..),

    -- * NetworkResourceDefinitionType
    NetworkResourceDefinitionType (..),

    -- * NetworkResourceFilterKeys
    NetworkResourceFilterKeys (..),

    -- * NetworkResourceStatus
    NetworkResourceStatus (..),

    -- * NetworkResourceType
    NetworkResourceType (..),

    -- * NetworkSiteFilterKeys
    NetworkSiteFilterKeys (..),

    -- * NetworkSiteStatus
    NetworkSiteStatus (..),

    -- * NetworkStatus
    NetworkStatus (..),

    -- * OrderFilterKeys
    OrderFilterKeys (..),

    -- * Address
    Address (..),
    newAddress,
    address_company,
    address_phoneNumber,
    address_street2,
    address_street3,
    address_city,
    address_country,
    address_name,
    address_postalCode,
    address_stateOrProvince,
    address_street1,

    -- * DeviceIdentifier
    DeviceIdentifier (..),
    newDeviceIdentifier,
    deviceIdentifier_createdAt,
    deviceIdentifier_deviceIdentifierArn,
    deviceIdentifier_iccid,
    deviceIdentifier_imsi,
    deviceIdentifier_networkArn,
    deviceIdentifier_orderArn,
    deviceIdentifier_status,
    deviceIdentifier_trafficGroupArn,
    deviceIdentifier_vendor,

    -- * NameValuePair
    NameValuePair (..),
    newNameValuePair,
    nameValuePair_value,
    nameValuePair_name,

    -- * Network
    Network (..),
    newNetwork,
    network_createdAt,
    network_description,
    network_statusReason,
    network_networkArn,
    network_networkName,
    network_status,

    -- * NetworkResource
    NetworkResource (..),
    newNetworkResource,
    networkResource_attributes,
    networkResource_createdAt,
    networkResource_description,
    networkResource_health,
    networkResource_model,
    networkResource_networkArn,
    networkResource_networkResourceArn,
    networkResource_networkSiteArn,
    networkResource_orderArn,
    networkResource_position,
    networkResource_serialNumber,
    networkResource_status,
    networkResource_statusReason,
    networkResource_type,
    networkResource_vendor,

    -- * NetworkResourceDefinition
    NetworkResourceDefinition (..),
    newNetworkResourceDefinition,
    networkResourceDefinition_options,
    networkResourceDefinition_count,
    networkResourceDefinition_type,

    -- * NetworkSite
    NetworkSite (..),
    newNetworkSite,
    networkSite_availabilityZone,
    networkSite_availabilityZoneId,
    networkSite_createdAt,
    networkSite_currentPlan,
    networkSite_description,
    networkSite_pendingPlan,
    networkSite_statusReason,
    networkSite_networkArn,
    networkSite_networkSiteArn,
    networkSite_networkSiteName,
    networkSite_status,

    -- * Order
    Order (..),
    newOrder,
    order_acknowledgmentStatus,
    order_createdAt,
    order_networkArn,
    order_networkSiteArn,
    order_orderArn,
    order_shippingAddress,
    order_trackingInformation,

    -- * Position
    Position (..),
    newPosition,
    position_elevation,
    position_elevationReference,
    position_elevationUnit,
    position_latitude,
    position_longitude,

    -- * SitePlan
    SitePlan (..),
    newSitePlan,
    sitePlan_options,
    sitePlan_resourceDefinitions,

    -- * TrackingInformation
    TrackingInformation (..),
    newTrackingInformation,
    trackingInformation_trackingNumber,

    -- * UpdateNetworkSiteResponse
    UpdateNetworkSiteResponse (..),
    newUpdateNetworkSiteResponse,
    updateNetworkSiteResponse_networkSite,
    updateNetworkSiteResponse_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types.AcknowledgmentStatus
import Amazonka.PrivateNetworks.Types.Address
import Amazonka.PrivateNetworks.Types.DeviceIdentifier
import Amazonka.PrivateNetworks.Types.DeviceIdentifierFilterKeys
import Amazonka.PrivateNetworks.Types.DeviceIdentifierStatus
import Amazonka.PrivateNetworks.Types.ElevationReference
import Amazonka.PrivateNetworks.Types.ElevationUnit
import Amazonka.PrivateNetworks.Types.HealthStatus
import Amazonka.PrivateNetworks.Types.NameValuePair
import Amazonka.PrivateNetworks.Types.Network
import Amazonka.PrivateNetworks.Types.NetworkFilterKeys
import Amazonka.PrivateNetworks.Types.NetworkResource
import Amazonka.PrivateNetworks.Types.NetworkResourceDefinition
import Amazonka.PrivateNetworks.Types.NetworkResourceDefinitionType
import Amazonka.PrivateNetworks.Types.NetworkResourceFilterKeys
import Amazonka.PrivateNetworks.Types.NetworkResourceStatus
import Amazonka.PrivateNetworks.Types.NetworkResourceType
import Amazonka.PrivateNetworks.Types.NetworkSite
import Amazonka.PrivateNetworks.Types.NetworkSiteFilterKeys
import Amazonka.PrivateNetworks.Types.NetworkSiteStatus
import Amazonka.PrivateNetworks.Types.NetworkStatus
import Amazonka.PrivateNetworks.Types.Order
import Amazonka.PrivateNetworks.Types.OrderFilterKeys
import Amazonka.PrivateNetworks.Types.Position
import Amazonka.PrivateNetworks.Types.SitePlan
import Amazonka.PrivateNetworks.Types.TrackingInformation
import Amazonka.PrivateNetworks.Types.UpdateNetworkSiteResponse
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-12-03@ of the Amazon Private 5G SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "PrivateNetworks",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "private-networks",
      Core.signingName = "private-networks",
      Core.version = "2021-12-03",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "PrivateNetworks",
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have permission to perform this operation.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Information about an internal error.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The limit was exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The resource was not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request failed validation.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

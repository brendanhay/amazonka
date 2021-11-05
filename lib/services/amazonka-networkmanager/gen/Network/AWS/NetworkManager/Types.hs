{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.NetworkManager.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * ConnectionState
    ConnectionState (..),

    -- * CustomerGatewayAssociationState
    CustomerGatewayAssociationState (..),

    -- * DeviceState
    DeviceState (..),

    -- * GlobalNetworkState
    GlobalNetworkState (..),

    -- * LinkAssociationState
    LinkAssociationState (..),

    -- * LinkState
    LinkState (..),

    -- * SiteState
    SiteState (..),

    -- * TransitGatewayConnectPeerAssociationState
    TransitGatewayConnectPeerAssociationState (..),

    -- * TransitGatewayRegistrationState
    TransitGatewayRegistrationState (..),

    -- * AWSLocation
    AWSLocation (..),
    newAWSLocation,
    aWSLocation_zone,
    aWSLocation_subnetArn,

    -- * Bandwidth
    Bandwidth (..),
    newBandwidth,
    bandwidth_downloadSpeed,
    bandwidth_uploadSpeed,

    -- * Connection
    Connection (..),
    newConnection,
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

    -- * CustomerGatewayAssociation
    CustomerGatewayAssociation (..),
    newCustomerGatewayAssociation,
    customerGatewayAssociation_state,
    customerGatewayAssociation_globalNetworkId,
    customerGatewayAssociation_linkId,
    customerGatewayAssociation_deviceId,
    customerGatewayAssociation_customerGatewayArn,

    -- * Device
    Device (..),
    newDevice,
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

    -- * GlobalNetwork
    GlobalNetwork (..),
    newGlobalNetwork,
    globalNetwork_state,
    globalNetwork_createdAt,
    globalNetwork_globalNetworkArn,
    globalNetwork_globalNetworkId,
    globalNetwork_description,
    globalNetwork_tags,

    -- * Link
    Link (..),
    newLink,
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

    -- * LinkAssociation
    LinkAssociation (..),
    newLinkAssociation,
    linkAssociation_globalNetworkId,
    linkAssociation_linkId,
    linkAssociation_deviceId,
    linkAssociation_linkAssociationState,

    -- * Location
    Location (..),
    newLocation,
    location_latitude,
    location_address,
    location_longitude,

    -- * Site
    Site (..),
    newSite,
    site_state,
    site_location,
    site_createdAt,
    site_globalNetworkId,
    site_siteId,
    site_siteArn,
    site_description,
    site_tags,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TransitGatewayConnectPeerAssociation
    TransitGatewayConnectPeerAssociation (..),
    newTransitGatewayConnectPeerAssociation,
    transitGatewayConnectPeerAssociation_state,
    transitGatewayConnectPeerAssociation_globalNetworkId,
    transitGatewayConnectPeerAssociation_transitGatewayConnectPeerArn,
    transitGatewayConnectPeerAssociation_linkId,
    transitGatewayConnectPeerAssociation_deviceId,

    -- * TransitGatewayRegistration
    TransitGatewayRegistration (..),
    newTransitGatewayRegistration,
    transitGatewayRegistration_state,
    transitGatewayRegistration_globalNetworkId,
    transitGatewayRegistration_transitGatewayArn,

    -- * TransitGatewayRegistrationStateReason
    TransitGatewayRegistrationStateReason (..),
    newTransitGatewayRegistrationStateReason,
    transitGatewayRegistrationStateReason_code,
    transitGatewayRegistrationStateReason_message,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkManager.Types.AWSLocation
import Amazonka.NetworkManager.Types.Bandwidth
import Amazonka.NetworkManager.Types.Connection
import Amazonka.NetworkManager.Types.ConnectionState
import Amazonka.NetworkManager.Types.CustomerGatewayAssociation
import Amazonka.NetworkManager.Types.CustomerGatewayAssociationState
import Amazonka.NetworkManager.Types.Device
import Amazonka.NetworkManager.Types.DeviceState
import Amazonka.NetworkManager.Types.GlobalNetwork
import Amazonka.NetworkManager.Types.GlobalNetworkState
import Amazonka.NetworkManager.Types.Link
import Amazonka.NetworkManager.Types.LinkAssociation
import Amazonka.NetworkManager.Types.LinkAssociationState
import Amazonka.NetworkManager.Types.LinkState
import Amazonka.NetworkManager.Types.Location
import Amazonka.NetworkManager.Types.Site
import Amazonka.NetworkManager.Types.SiteState
import Amazonka.NetworkManager.Types.Tag
import Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociation
import Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociationState
import Amazonka.NetworkManager.Types.TransitGatewayRegistration
import Amazonka.NetworkManager.Types.TransitGatewayRegistrationState
import Amazonka.NetworkManager.Types.TransitGatewayRegistrationStateReason
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-07-05@ of the Amazon Network Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "NetworkManager",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "networkmanager",
      Core._serviceSigningName = "networkmanager",
      Core._serviceVersion = "2019-07-05",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "NetworkManager",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The input fails to satisfy the constraints.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | There was a conflict processing the request. Updating or deleting the
-- resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | A service limit was exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request has failed due to an internal error.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

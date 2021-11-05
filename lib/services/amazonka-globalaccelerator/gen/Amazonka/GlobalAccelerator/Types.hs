{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GlobalAccelerator.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _AssociatedListenerFoundException,
    _EndpointAlreadyExistsException,
    _InvalidArgumentException,
    _AssociatedEndpointGroupFoundException,
    _AcceleratorNotDisabledException,
    _ConflictException,
    _EndpointNotFoundException,
    _ListenerNotFoundException,
    _InvalidNextTokenException,
    _InternalServiceErrorException,
    _EndpointGroupAlreadyExistsException,
    _ByoipCidrNotFoundException,
    _IncorrectCidrStateException,
    _InvalidPortRangeException,
    _EndpointGroupNotFoundException,
    _AcceleratorNotFoundException,
    _LimitExceededException,

    -- * AcceleratorStatus
    AcceleratorStatus (..),

    -- * ByoipCidrState
    ByoipCidrState (..),

    -- * ClientAffinity
    ClientAffinity (..),

    -- * CustomRoutingAcceleratorStatus
    CustomRoutingAcceleratorStatus (..),

    -- * CustomRoutingDestinationTrafficState
    CustomRoutingDestinationTrafficState (..),

    -- * CustomRoutingProtocol
    CustomRoutingProtocol (..),

    -- * HealthCheckProtocol
    HealthCheckProtocol (..),

    -- * HealthState
    HealthState (..),

    -- * IpAddressType
    IpAddressType (..),

    -- * Protocol
    Protocol (..),

    -- * Accelerator
    Accelerator (..),
    newAccelerator,
    accelerator_status,
    accelerator_acceleratorArn,
    accelerator_enabled,
    accelerator_createdTime,
    accelerator_lastModifiedTime,
    accelerator_ipAddressType,
    accelerator_name,
    accelerator_ipSets,
    accelerator_dnsName,

    -- * AcceleratorAttributes
    AcceleratorAttributes (..),
    newAcceleratorAttributes,
    acceleratorAttributes_flowLogsS3Prefix,
    acceleratorAttributes_flowLogsEnabled,
    acceleratorAttributes_flowLogsS3Bucket,

    -- * ByoipCidr
    ByoipCidr (..),
    newByoipCidr,
    byoipCidr_state,
    byoipCidr_cidr,
    byoipCidr_events,

    -- * ByoipCidrEvent
    ByoipCidrEvent (..),
    newByoipCidrEvent,
    byoipCidrEvent_message,
    byoipCidrEvent_timestamp,

    -- * CidrAuthorizationContext
    CidrAuthorizationContext (..),
    newCidrAuthorizationContext,
    cidrAuthorizationContext_message,
    cidrAuthorizationContext_signature,

    -- * CustomRoutingAccelerator
    CustomRoutingAccelerator (..),
    newCustomRoutingAccelerator,
    customRoutingAccelerator_status,
    customRoutingAccelerator_acceleratorArn,
    customRoutingAccelerator_enabled,
    customRoutingAccelerator_createdTime,
    customRoutingAccelerator_lastModifiedTime,
    customRoutingAccelerator_ipAddressType,
    customRoutingAccelerator_name,
    customRoutingAccelerator_ipSets,
    customRoutingAccelerator_dnsName,

    -- * CustomRoutingAcceleratorAttributes
    CustomRoutingAcceleratorAttributes (..),
    newCustomRoutingAcceleratorAttributes,
    customRoutingAcceleratorAttributes_flowLogsS3Prefix,
    customRoutingAcceleratorAttributes_flowLogsEnabled,
    customRoutingAcceleratorAttributes_flowLogsS3Bucket,

    -- * CustomRoutingDestinationConfiguration
    CustomRoutingDestinationConfiguration (..),
    newCustomRoutingDestinationConfiguration,
    customRoutingDestinationConfiguration_fromPort,
    customRoutingDestinationConfiguration_toPort,
    customRoutingDestinationConfiguration_protocols,

    -- * CustomRoutingDestinationDescription
    CustomRoutingDestinationDescription (..),
    newCustomRoutingDestinationDescription,
    customRoutingDestinationDescription_fromPort,
    customRoutingDestinationDescription_protocols,
    customRoutingDestinationDescription_toPort,

    -- * CustomRoutingEndpointConfiguration
    CustomRoutingEndpointConfiguration (..),
    newCustomRoutingEndpointConfiguration,
    customRoutingEndpointConfiguration_endpointId,

    -- * CustomRoutingEndpointDescription
    CustomRoutingEndpointDescription (..),
    newCustomRoutingEndpointDescription,
    customRoutingEndpointDescription_endpointId,

    -- * CustomRoutingEndpointGroup
    CustomRoutingEndpointGroup (..),
    newCustomRoutingEndpointGroup,
    customRoutingEndpointGroup_endpointGroupArn,
    customRoutingEndpointGroup_endpointGroupRegion,
    customRoutingEndpointGroup_endpointDescriptions,
    customRoutingEndpointGroup_destinationDescriptions,

    -- * CustomRoutingListener
    CustomRoutingListener (..),
    newCustomRoutingListener,
    customRoutingListener_portRanges,
    customRoutingListener_listenerArn,

    -- * DestinationPortMapping
    DestinationPortMapping (..),
    newDestinationPortMapping,
    destinationPortMapping_destinationSocketAddress,
    destinationPortMapping_acceleratorArn,
    destinationPortMapping_endpointGroupArn,
    destinationPortMapping_endpointGroupRegion,
    destinationPortMapping_ipAddressType,
    destinationPortMapping_acceleratorSocketAddresses,
    destinationPortMapping_endpointId,
    destinationPortMapping_destinationTrafficState,

    -- * EndpointConfiguration
    EndpointConfiguration (..),
    newEndpointConfiguration,
    endpointConfiguration_weight,
    endpointConfiguration_clientIPPreservationEnabled,
    endpointConfiguration_endpointId,

    -- * EndpointDescription
    EndpointDescription (..),
    newEndpointDescription,
    endpointDescription_healthReason,
    endpointDescription_weight,
    endpointDescription_clientIPPreservationEnabled,
    endpointDescription_healthState,
    endpointDescription_endpointId,

    -- * EndpointGroup
    EndpointGroup (..),
    newEndpointGroup,
    endpointGroup_thresholdCount,
    endpointGroup_healthCheckPath,
    endpointGroup_healthCheckIntervalSeconds,
    endpointGroup_endpointGroupArn,
    endpointGroup_healthCheckProtocol,
    endpointGroup_endpointGroupRegion,
    endpointGroup_trafficDialPercentage,
    endpointGroup_healthCheckPort,
    endpointGroup_portOverrides,
    endpointGroup_endpointDescriptions,

    -- * IpSet
    IpSet (..),
    newIpSet,
    ipSet_ipFamily,
    ipSet_ipAddresses,

    -- * Listener
    Listener (..),
    newListener,
    listener_portRanges,
    listener_listenerArn,
    listener_protocol,
    listener_clientAffinity,

    -- * PortMapping
    PortMapping (..),
    newPortMapping,
    portMapping_destinationSocketAddress,
    portMapping_protocols,
    portMapping_endpointGroupArn,
    portMapping_endpointId,
    portMapping_destinationTrafficState,
    portMapping_acceleratorPort,

    -- * PortOverride
    PortOverride (..),
    newPortOverride,
    portOverride_endpointPort,
    portOverride_listenerPort,

    -- * PortRange
    PortRange (..),
    newPortRange,
    portRange_fromPort,
    portRange_toPort,

    -- * SocketAddress
    SocketAddress (..),
    newSocketAddress,
    socketAddress_ipAddress,
    socketAddress_port,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GlobalAccelerator.Types.Accelerator
import Amazonka.GlobalAccelerator.Types.AcceleratorAttributes
import Amazonka.GlobalAccelerator.Types.AcceleratorStatus
import Amazonka.GlobalAccelerator.Types.ByoipCidr
import Amazonka.GlobalAccelerator.Types.ByoipCidrEvent
import Amazonka.GlobalAccelerator.Types.ByoipCidrState
import Amazonka.GlobalAccelerator.Types.CidrAuthorizationContext
import Amazonka.GlobalAccelerator.Types.ClientAffinity
import Amazonka.GlobalAccelerator.Types.CustomRoutingAccelerator
import Amazonka.GlobalAccelerator.Types.CustomRoutingAcceleratorAttributes
import Amazonka.GlobalAccelerator.Types.CustomRoutingAcceleratorStatus
import Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationConfiguration
import Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationDescription
import Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationTrafficState
import Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointConfiguration
import Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointDescription
import Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointGroup
import Amazonka.GlobalAccelerator.Types.CustomRoutingListener
import Amazonka.GlobalAccelerator.Types.CustomRoutingProtocol
import Amazonka.GlobalAccelerator.Types.DestinationPortMapping
import Amazonka.GlobalAccelerator.Types.EndpointConfiguration
import Amazonka.GlobalAccelerator.Types.EndpointDescription
import Amazonka.GlobalAccelerator.Types.EndpointGroup
import Amazonka.GlobalAccelerator.Types.HealthCheckProtocol
import Amazonka.GlobalAccelerator.Types.HealthState
import Amazonka.GlobalAccelerator.Types.IpAddressType
import Amazonka.GlobalAccelerator.Types.IpSet
import Amazonka.GlobalAccelerator.Types.Listener
import Amazonka.GlobalAccelerator.Types.PortMapping
import Amazonka.GlobalAccelerator.Types.PortOverride
import Amazonka.GlobalAccelerator.Types.PortRange
import Amazonka.GlobalAccelerator.Types.Protocol
import Amazonka.GlobalAccelerator.Types.SocketAddress
import Amazonka.GlobalAccelerator.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-08-08@ of the Amazon Global Accelerator SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "GlobalAccelerator",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "globalaccelerator",
      Core._serviceSigningName = "globalaccelerator",
      Core._serviceVersion = "2018-08-08",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "GlobalAccelerator",
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

-- | You don\'t have access permission.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The accelerator that you specified has a listener associated with it.
-- You must remove all dependent resources from an accelerator before you
-- can delete it.
_AssociatedListenerFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociatedListenerFoundException =
  Core._MatchServiceError
    defaultService
    "AssociatedListenerFoundException"

-- | The endpoint that you specified doesn\'t exist.
_EndpointAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EndpointAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EndpointAlreadyExistsException"

-- | An argument that you specified is invalid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | The listener that you specified has an endpoint group associated with
-- it. You must remove all dependent resources from a listener before you
-- can delete it.
_AssociatedEndpointGroupFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociatedEndpointGroupFoundException =
  Core._MatchServiceError
    defaultService
    "AssociatedEndpointGroupFoundException"

-- | The accelerator that you specified could not be disabled.
_AcceleratorNotDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AcceleratorNotDisabledException =
  Core._MatchServiceError
    defaultService
    "AcceleratorNotDisabledException"

-- | You can\'t use both of those options.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The endpoint that you specified doesn\'t exist.
_EndpointNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EndpointNotFoundException =
  Core._MatchServiceError
    defaultService
    "EndpointNotFoundException"

-- | The listener that you specified doesn\'t exist.
_ListenerNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ListenerNotFoundException =
  Core._MatchServiceError
    defaultService
    "ListenerNotFoundException"

-- | There isn\'t another item to return.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | There was an internal error for AWS Global Accelerator.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"

-- | The endpoint group that you specified already exists.
_EndpointGroupAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EndpointGroupAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EndpointGroupAlreadyExistsException"

-- | The CIDR that you specified was not found or is incorrect.
_ByoipCidrNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ByoipCidrNotFoundException =
  Core._MatchServiceError
    defaultService
    "ByoipCidrNotFoundException"

-- | The CIDR that you specified is not valid for this action. For example,
-- the state of the CIDR might be incorrect for this action.
_IncorrectCidrStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncorrectCidrStateException =
  Core._MatchServiceError
    defaultService
    "IncorrectCidrStateException"

-- | The port numbers that you specified are not valid numbers or are not
-- unique for this accelerator.
_InvalidPortRangeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPortRangeException =
  Core._MatchServiceError
    defaultService
    "InvalidPortRangeException"

-- | The endpoint group that you specified doesn\'t exist.
_EndpointGroupNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EndpointGroupNotFoundException =
  Core._MatchServiceError
    defaultService
    "EndpointGroupNotFoundException"

-- | The accelerator that you specified doesn\'t exist.
_AcceleratorNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AcceleratorNotFoundException =
  Core._MatchServiceError
    defaultService
    "AcceleratorNotFoundException"

-- | Processing your request would cause you to exceed an AWS Global
-- Accelerator limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GlobalAccelerator.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidArgumentException,
    _ListenerNotFoundException,
    _EndpointAlreadyExistsException,
    _AccessDeniedException,
    _AcceleratorNotDisabledException,
    _AssociatedEndpointGroupFoundException,
    _LimitExceededException,
    _InvalidNextTokenException,
    _ConflictException,
    _EndpointGroupAlreadyExistsException,
    _AssociatedListenerFoundException,
    _ByoipCidrNotFoundException,
    _AcceleratorNotFoundException,
    _EndpointNotFoundException,
    _EndpointGroupNotFoundException,
    _InternalServiceErrorException,
    _IncorrectCidrStateException,
    _InvalidPortRangeException,
    _TransactionInProgressException,

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

    -- * IpAddressFamily
    IpAddressFamily (..),

    -- * IpAddressType
    IpAddressType (..),

    -- * Protocol
    Protocol (..),

    -- * Accelerator
    Accelerator (..),
    newAccelerator,
    accelerator_ipSets,
    accelerator_name,
    accelerator_acceleratorArn,
    accelerator_createdTime,
    accelerator_status,
    accelerator_enabled,
    accelerator_lastModifiedTime,
    accelerator_dualStackDnsName,
    accelerator_events,
    accelerator_dnsName,
    accelerator_ipAddressType,

    -- * AcceleratorAttributes
    AcceleratorAttributes (..),
    newAcceleratorAttributes,
    acceleratorAttributes_flowLogsEnabled,
    acceleratorAttributes_flowLogsS3Bucket,
    acceleratorAttributes_flowLogsS3Prefix,

    -- * AcceleratorEvent
    AcceleratorEvent (..),
    newAcceleratorEvent,
    acceleratorEvent_message,
    acceleratorEvent_timestamp,

    -- * ByoipCidr
    ByoipCidr (..),
    newByoipCidr,
    byoipCidr_cidr,
    byoipCidr_state,
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
    customRoutingAccelerator_ipSets,
    customRoutingAccelerator_name,
    customRoutingAccelerator_acceleratorArn,
    customRoutingAccelerator_createdTime,
    customRoutingAccelerator_status,
    customRoutingAccelerator_enabled,
    customRoutingAccelerator_lastModifiedTime,
    customRoutingAccelerator_dnsName,
    customRoutingAccelerator_ipAddressType,

    -- * CustomRoutingAcceleratorAttributes
    CustomRoutingAcceleratorAttributes (..),
    newCustomRoutingAcceleratorAttributes,
    customRoutingAcceleratorAttributes_flowLogsEnabled,
    customRoutingAcceleratorAttributes_flowLogsS3Bucket,
    customRoutingAcceleratorAttributes_flowLogsS3Prefix,

    -- * CustomRoutingDestinationConfiguration
    CustomRoutingDestinationConfiguration (..),
    newCustomRoutingDestinationConfiguration,
    customRoutingDestinationConfiguration_fromPort,
    customRoutingDestinationConfiguration_toPort,
    customRoutingDestinationConfiguration_protocols,

    -- * CustomRoutingDestinationDescription
    CustomRoutingDestinationDescription (..),
    newCustomRoutingDestinationDescription,
    customRoutingDestinationDescription_toPort,
    customRoutingDestinationDescription_protocols,
    customRoutingDestinationDescription_fromPort,

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
    customRoutingEndpointGroup_endpointGroupRegion,
    customRoutingEndpointGroup_endpointDescriptions,
    customRoutingEndpointGroup_destinationDescriptions,
    customRoutingEndpointGroup_endpointGroupArn,

    -- * CustomRoutingListener
    CustomRoutingListener (..),
    newCustomRoutingListener,
    customRoutingListener_listenerArn,
    customRoutingListener_portRanges,

    -- * DestinationPortMapping
    DestinationPortMapping (..),
    newDestinationPortMapping,
    destinationPortMapping_endpointGroupRegion,
    destinationPortMapping_acceleratorArn,
    destinationPortMapping_endpointId,
    destinationPortMapping_destinationTrafficState,
    destinationPortMapping_ipAddressType,
    destinationPortMapping_endpointGroupArn,
    destinationPortMapping_destinationSocketAddress,
    destinationPortMapping_acceleratorSocketAddresses,

    -- * EndpointConfiguration
    EndpointConfiguration (..),
    newEndpointConfiguration,
    endpointConfiguration_endpointId,
    endpointConfiguration_weight,
    endpointConfiguration_clientIPPreservationEnabled,

    -- * EndpointDescription
    EndpointDescription (..),
    newEndpointDescription,
    endpointDescription_endpointId,
    endpointDescription_healthReason,
    endpointDescription_weight,
    endpointDescription_clientIPPreservationEnabled,
    endpointDescription_healthState,

    -- * EndpointGroup
    EndpointGroup (..),
    newEndpointGroup,
    endpointGroup_healthCheckProtocol,
    endpointGroup_endpointGroupRegion,
    endpointGroup_portOverrides,
    endpointGroup_healthCheckPath,
    endpointGroup_healthCheckIntervalSeconds,
    endpointGroup_endpointDescriptions,
    endpointGroup_trafficDialPercentage,
    endpointGroup_thresholdCount,
    endpointGroup_healthCheckPort,
    endpointGroup_endpointGroupArn,

    -- * EndpointIdentifier
    EndpointIdentifier (..),
    newEndpointIdentifier,
    endpointIdentifier_clientIPPreservationEnabled,
    endpointIdentifier_endpointId,

    -- * IpSet
    IpSet (..),
    newIpSet,
    ipSet_ipFamily,
    ipSet_ipAddressFamily,
    ipSet_ipAddresses,

    -- * Listener
    Listener (..),
    newListener,
    listener_listenerArn,
    listener_clientAffinity,
    listener_protocol,
    listener_portRanges,

    -- * PortMapping
    PortMapping (..),
    newPortMapping,
    portMapping_acceleratorPort,
    portMapping_endpointId,
    portMapping_protocols,
    portMapping_destinationTrafficState,
    portMapping_endpointGroupArn,
    portMapping_destinationSocketAddress,

    -- * PortOverride
    PortOverride (..),
    newPortOverride,
    portOverride_endpointPort,
    portOverride_listenerPort,

    -- * PortRange
    PortRange (..),
    newPortRange,
    portRange_toPort,
    portRange_fromPort,

    -- * SocketAddress
    SocketAddress (..),
    newSocketAddress,
    socketAddress_port,
    socketAddress_ipAddress,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types.Accelerator
import Amazonka.GlobalAccelerator.Types.AcceleratorAttributes
import Amazonka.GlobalAccelerator.Types.AcceleratorEvent
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
import Amazonka.GlobalAccelerator.Types.EndpointIdentifier
import Amazonka.GlobalAccelerator.Types.HealthCheckProtocol
import Amazonka.GlobalAccelerator.Types.HealthState
import Amazonka.GlobalAccelerator.Types.IpAddressFamily
import Amazonka.GlobalAccelerator.Types.IpAddressType
import Amazonka.GlobalAccelerator.Types.IpSet
import Amazonka.GlobalAccelerator.Types.Listener
import Amazonka.GlobalAccelerator.Types.PortMapping
import Amazonka.GlobalAccelerator.Types.PortOverride
import Amazonka.GlobalAccelerator.Types.PortRange
import Amazonka.GlobalAccelerator.Types.Protocol
import Amazonka.GlobalAccelerator.Types.SocketAddress
import Amazonka.GlobalAccelerator.Types.Tag
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-08-08@ of the Amazon Global Accelerator SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "GlobalAccelerator",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "globalaccelerator",
      Core.signingName = "globalaccelerator",
      Core.version = "2018-08-08",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "GlobalAccelerator",
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

-- | An argument that you specified is invalid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | The listener that you specified doesn\'t exist.
_ListenerNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ListenerNotFoundException =
  Core._MatchServiceError
    defaultService
    "ListenerNotFoundException"

-- | The endpoint that you specified doesn\'t exist.
_EndpointAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EndpointAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EndpointAlreadyExistsException"

-- | You don\'t have access permission.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The accelerator that you specified could not be disabled.
_AcceleratorNotDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AcceleratorNotDisabledException =
  Core._MatchServiceError
    defaultService
    "AcceleratorNotDisabledException"

-- | The listener that you specified has an endpoint group associated with
-- it. You must remove all dependent resources from a listener before you
-- can delete it.
_AssociatedEndpointGroupFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociatedEndpointGroupFoundException =
  Core._MatchServiceError
    defaultService
    "AssociatedEndpointGroupFoundException"

-- | Processing your request would cause you to exceed an Global Accelerator
-- limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | There isn\'t another item to return.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | You can\'t use both of those options.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The endpoint group that you specified already exists.
_EndpointGroupAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EndpointGroupAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EndpointGroupAlreadyExistsException"

-- | The accelerator that you specified has a listener associated with it.
-- You must remove all dependent resources from an accelerator before you
-- can delete it.
_AssociatedListenerFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AssociatedListenerFoundException =
  Core._MatchServiceError
    defaultService
    "AssociatedListenerFoundException"

-- | The CIDR that you specified was not found or is incorrect.
_ByoipCidrNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ByoipCidrNotFoundException =
  Core._MatchServiceError
    defaultService
    "ByoipCidrNotFoundException"

-- | The accelerator that you specified doesn\'t exist.
_AcceleratorNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AcceleratorNotFoundException =
  Core._MatchServiceError
    defaultService
    "AcceleratorNotFoundException"

-- | The endpoint that you specified doesn\'t exist.
_EndpointNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EndpointNotFoundException =
  Core._MatchServiceError
    defaultService
    "EndpointNotFoundException"

-- | The endpoint group that you specified doesn\'t exist.
_EndpointGroupNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EndpointGroupNotFoundException =
  Core._MatchServiceError
    defaultService
    "EndpointGroupNotFoundException"

-- | There was an internal error for Global Accelerator.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"

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

-- | There\'s already a transaction in progress. Another transaction can\'t
-- be processed.
_TransactionInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TransactionInProgressException =
  Core._MatchServiceError
    defaultService
    "TransactionInProgressException"

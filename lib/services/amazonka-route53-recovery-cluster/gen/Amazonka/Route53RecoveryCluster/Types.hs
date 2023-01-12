{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53RecoveryCluster.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryCluster.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _EndpointTemporarilyUnavailableException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceLimitExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * RoutingControlState
    RoutingControlState (..),

    -- * RoutingControl
    RoutingControl (..),
    newRoutingControl,
    routingControl_controlPanelArn,
    routingControl_controlPanelName,
    routingControl_routingControlArn,
    routingControl_routingControlName,
    routingControl_routingControlState,

    -- * UpdateRoutingControlStateEntry
    UpdateRoutingControlStateEntry (..),
    newUpdateRoutingControlStateEntry,
    updateRoutingControlStateEntry_routingControlArn,
    updateRoutingControlStateEntry_routingControlState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryCluster.Types.RoutingControl
import Amazonka.Route53RecoveryCluster.Types.RoutingControlState
import Amazonka.Route53RecoveryCluster.Types.UpdateRoutingControlStateEntry
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-12-02@ of the Amazon Route53 Recovery Cluster SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "Route53RecoveryCluster",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "route53-recovery-cluster",
      Core.signingName = "route53-recovery-cluster",
      Core.version = "2019-12-02",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "Route53RecoveryCluster",
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

-- | You don\'t have sufficient permissions to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | There was a conflict with this request. Try again.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The cluster endpoint isn\'t available. Try another cluster endpoint.
_EndpointTemporarilyUnavailableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_EndpointTemporarilyUnavailableException =
  Core._MatchServiceError
    defaultService
    "EndpointTemporarilyUnavailableException"

-- | There was an unexpected error during processing of the request.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The request references a routing control or control panel that was not
-- found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request can\'t update that many routing control states at the same
-- time. Try again with fewer routing control states.
_ServiceLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceLimitExceededException"

-- | The request was denied because of request throttling.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | There was a validation error on the request.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

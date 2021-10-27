{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudControl.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudControl.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TypeNotFoundException,
    _GeneralServiceException,
    _HandlerFailureException,
    _InvalidRequestException,
    _PrivateTypeException,
    _ServiceInternalErrorException,
    _HandlerInternalFailureException,
    _ClientTokenConflictException,
    _RequestTokenNotFoundException,
    _NetworkFailureException,
    _NotStabilizedException,
    _NotUpdatableException,
    _UnsupportedActionException,
    _ServiceLimitExceededException,
    _ThrottlingException,
    _ConcurrentModificationException,
    _ResourceConflictException,
    _InvalidCredentialsException,
    _ResourceNotFoundException,
    _ConcurrentOperationException,
    _AlreadyExistsException,

    -- * HandlerErrorCode
    HandlerErrorCode (..),

    -- * Operation
    Operation (..),

    -- * OperationStatus
    OperationStatus (..),

    -- * ProgressEvent
    ProgressEvent (..),
    newProgressEvent,
    progressEvent_retryAfter,
    progressEvent_typeName,
    progressEvent_requestToken,
    progressEvent_resourceModel,
    progressEvent_operation,
    progressEvent_identifier,
    progressEvent_operationStatus,
    progressEvent_eventTime,
    progressEvent_statusMessage,
    progressEvent_errorCode,

    -- * ResourceDescription
    ResourceDescription (..),
    newResourceDescription,
    resourceDescription_identifier,
    resourceDescription_properties,

    -- * ResourceRequestStatusFilter
    ResourceRequestStatusFilter (..),
    newResourceRequestStatusFilter,
    resourceRequestStatusFilter_operationStatuses,
    resourceRequestStatusFilter_operations,
  )
where

import Network.AWS.CloudControl.Types.HandlerErrorCode
import Network.AWS.CloudControl.Types.Operation
import Network.AWS.CloudControl.Types.OperationStatus
import Network.AWS.CloudControl.Types.ProgressEvent
import Network.AWS.CloudControl.Types.ResourceDescription
import Network.AWS.CloudControl.Types.ResourceRequestStatusFilter
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2021-09-30@ of the Amazon Cloud Control API SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CloudControl",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cloudcontrolapi",
      Core._serviceSigningName = "cloudcontrolapi",
      Core._serviceVersion = "2021-09-30",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CloudControl",
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

-- | The specified extension does not exist in the CloudFormation registry.
_TypeNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TypeNotFoundException =
  Core._MatchServiceError
    defaultService
    "TypeNotFoundException"

-- | The resource handler has returned that the downstream service generated
-- an error that does not map to any other handler error code.
_GeneralServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GeneralServiceException =
  Core._MatchServiceError
    defaultService
    "GeneralServiceException"

-- | The resource handler has failed without a returning a more specific
-- error code. This can include timeouts.
_HandlerFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HandlerFailureException =
  Core._MatchServiceError
    defaultService
    "HandlerFailureException"

-- | The resource handler has returned that invalid input from the user has
-- generated a generic exception.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | Cloud Control API has not received a valid response from the resource
-- handler, due to a configuration error. This includes issues such as the
-- resource handler returning an invalid response, or timing out.
_PrivateTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PrivateTypeException =
  Core._MatchServiceError
    defaultService
    "PrivateTypeException"

-- | The resource handler has returned that the downstream service returned
-- an internal error, typically with a @5XX HTTP@ status code.
_ServiceInternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceInternalErrorException =
  Core._MatchServiceError
    defaultService
    "ServiceInternalErrorException"

-- | The resource handler has returned that an unexpected error occurred
-- within the resource handler.
_HandlerInternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HandlerInternalFailureException =
  Core._MatchServiceError
    defaultService
    "HandlerInternalFailureException"

-- | The specified client token has already been used in another resource
-- request.
--
-- It is best practice for client tokens to be unique for each resource
-- operation request. However, client token expire after 36 hours.
_ClientTokenConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClientTokenConflictException =
  Core._MatchServiceError
    defaultService
    "ClientTokenConflictException"

-- | A resource operation with the specified request token cannot be found.
_RequestTokenNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestTokenNotFoundException =
  Core._MatchServiceError
    defaultService
    "RequestTokenNotFoundException"

-- | The resource handler has returned that the request could not be
-- completed due to networking issues, such as a failure to receive a
-- response from the server.
_NetworkFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NetworkFailureException =
  Core._MatchServiceError
    defaultService
    "NetworkFailureException"

-- | The resource handler has returned that the downstream resource failed to
-- complete all of its ready-state checks.
_NotStabilizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotStabilizedException =
  Core._MatchServiceError
    defaultService
    "NotStabilizedException"

-- | One or more properties included in this resource operation are defined
-- as create-only, and therefore cannot be updated.
_NotUpdatableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotUpdatableException =
  Core._MatchServiceError
    defaultService
    "NotUpdatableException"

-- | The specified resource does not support this resource operation.
_UnsupportedActionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedActionException =
  Core._MatchServiceError
    defaultService
    "UnsupportedActionException"

-- | The resource handler has returned that a non-transient resource limit
-- was reached on the service side.
_ServiceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceLimitExceededException"

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The resource is currently being modified by another operation.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The resource is temporarily unavailable to be acted upon. For example,
-- if the resource is currently undergoing an operation and cannot be acted
-- upon until that operation is finished.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"

-- | The resource handler has returned that the credentials provided by the
-- user are invalid.
_InvalidCredentialsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCredentialsException =
  Core._MatchServiceError
    defaultService
    "InvalidCredentialsException"

-- | A resource with the specified identifier cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Another resource operation is currently being performed on this
-- resource.
_ConcurrentOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentOperationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentOperationException"

-- | The resource with the name requested already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _PolicyNotFoundException,
    _CorsPolicyNotFoundException,
    _ContainerInUseException,
    _InternalServerError,
    _ContainerNotFoundException,
    _LimitExceededException,

    -- * ContainerLevelMetrics
    ContainerLevelMetrics (..),

    -- * ContainerStatus
    ContainerStatus (..),

    -- * MethodName
    MethodName (..),

    -- * Container
    Container (..),
    newContainer,
    container_creationTime,
    container_status,
    container_accessLoggingEnabled,
    container_arn,
    container_name,
    container_endpoint,

    -- * CorsRule
    CorsRule (..),
    newCorsRule,
    corsRule_allowedMethods,
    corsRule_maxAgeSeconds,
    corsRule_exposeHeaders,
    corsRule_allowedOrigins,
    corsRule_allowedHeaders,

    -- * MetricPolicy
    MetricPolicy (..),
    newMetricPolicy,
    metricPolicy_metricPolicyRules,
    metricPolicy_containerLevelMetrics,

    -- * MetricPolicyRule
    MetricPolicyRule (..),
    newMetricPolicyRule,
    metricPolicyRule_objectGroup,
    metricPolicyRule_objectGroupName,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types.Container
import Network.AWS.MediaStore.Types.ContainerLevelMetrics
import Network.AWS.MediaStore.Types.ContainerStatus
import Network.AWS.MediaStore.Types.CorsRule
import Network.AWS.MediaStore.Types.MethodName
import Network.AWS.MediaStore.Types.MetricPolicy
import Network.AWS.MediaStore.Types.MetricPolicyRule
import Network.AWS.MediaStore.Types.Tag
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-01@ of the Amazon Elemental MediaStore SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MediaStore",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "mediastore",
      Core._serviceSigningName = "mediastore",
      Core._serviceVersion = "2017-09-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MediaStore",
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

-- | The policy that you specified in the request does not exist.
_PolicyNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "PolicyNotFoundException"

-- | The CORS policy that you specified in the request does not exist.
_CorsPolicyNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CorsPolicyNotFoundException =
  Core._MatchServiceError
    defaultService
    "CorsPolicyNotFoundException"

-- | The container that you specified in the request already exists or is
-- being updated.
_ContainerInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ContainerInUseException =
  Core._MatchServiceError
    defaultService
    "ContainerInUseException"

-- | The service is temporarily unavailable.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | The container that you specified in the request does not exist.
_ContainerNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ContainerNotFoundException =
  Core._MatchServiceError
    defaultService
    "ContainerNotFoundException"

-- | A service limit has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

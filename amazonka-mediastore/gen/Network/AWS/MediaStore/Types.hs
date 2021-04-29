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
    _ContainerInUseException,
    _InternalServerError,
    _PolicyNotFoundException,
    _LimitExceededException,
    _CorsPolicyNotFoundException,
    _ContainerNotFoundException,

    -- * ContainerLevelMetrics
    ContainerLevelMetrics (..),

    -- * ContainerStatus
    ContainerStatus (..),

    -- * MethodName
    MethodName (..),

    -- * Container
    Container (..),
    newContainer,
    container_status,
    container_creationTime,
    container_arn,
    container_accessLoggingEnabled,
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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "MediaStore",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "mediastore",
      Prelude._svcVersion = "2017-09-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "MediaStore",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The container that you specified in the request already exists or is
-- being updated.
_ContainerInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ContainerInUseException =
  Prelude._MatchServiceError
    defaultService
    "ContainerInUseException"

-- | The service is temporarily unavailable.
_InternalServerError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerError =
  Prelude._MatchServiceError
    defaultService
    "InternalServerError"

-- | The policy that you specified in the request does not exist.
_PolicyNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PolicyNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "PolicyNotFoundException"

-- | A service limit has been exceeded.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The CORS policy that you specified in the request does not exist.
_CorsPolicyNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CorsPolicyNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "CorsPolicyNotFoundException"

-- | The container that you specified in the request does not exist.
_ContainerNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ContainerNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ContainerNotFoundException"

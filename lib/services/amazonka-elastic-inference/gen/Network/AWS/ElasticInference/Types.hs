{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticInference.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticInference.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerException,
    _ResourceNotFoundException,
    _BadRequestException,

    -- * LocationType
    LocationType (..),

    -- * AcceleratorType
    AcceleratorType (..),
    newAcceleratorType,
    acceleratorType_throughputInfo,
    acceleratorType_memoryInfo,
    acceleratorType_acceleratorTypeName,

    -- * AcceleratorTypeOffering
    AcceleratorTypeOffering (..),
    newAcceleratorTypeOffering,
    acceleratorTypeOffering_acceleratorType,
    acceleratorTypeOffering_location,
    acceleratorTypeOffering_locationType,

    -- * ElasticInferenceAccelerator
    ElasticInferenceAccelerator (..),
    newElasticInferenceAccelerator,
    elasticInferenceAccelerator_acceleratorType,
    elasticInferenceAccelerator_acceleratorId,
    elasticInferenceAccelerator_attachedResource,
    elasticInferenceAccelerator_acceleratorHealth,
    elasticInferenceAccelerator_availabilityZone,

    -- * ElasticInferenceAcceleratorHealth
    ElasticInferenceAcceleratorHealth (..),
    newElasticInferenceAcceleratorHealth,
    elasticInferenceAcceleratorHealth_status,

    -- * Filter
    Filter (..),
    newFilter,
    filter_values,
    filter_name,

    -- * KeyValuePair
    KeyValuePair (..),
    newKeyValuePair,
    keyValuePair_value,
    keyValuePair_key,

    -- * MemoryInfo
    MemoryInfo (..),
    newMemoryInfo,
    memoryInfo_sizeInMiB,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticInference.Types.AcceleratorType
import Network.AWS.ElasticInference.Types.AcceleratorTypeOffering
import Network.AWS.ElasticInference.Types.ElasticInferenceAccelerator
import Network.AWS.ElasticInference.Types.ElasticInferenceAcceleratorHealth
import Network.AWS.ElasticInference.Types.Filter
import Network.AWS.ElasticInference.Types.KeyValuePair
import Network.AWS.ElasticInference.Types.LocationType
import Network.AWS.ElasticInference.Types.MemoryInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon Elastic  Inference SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ElasticInference",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix =
        "api.elastic-inference",
      Core._serviceSigningName = "elastic-inference",
      Core._serviceVersion = "2017-07-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ElasticInference",
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

-- | Raised when an unexpected error occurred during request processing.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Raised when the requested resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Raised when a malformed input has been provided to the API.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

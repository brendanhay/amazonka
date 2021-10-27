{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pi.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pi.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidArgumentException,
    _NotAuthorizedException,
    _InternalServiceError,

    -- * DetailStatus
    DetailStatus (..),

    -- * ServiceType
    ServiceType (..),

    -- * DataPoint
    DataPoint (..),
    newDataPoint,
    dataPoint_timestamp,
    dataPoint_value,

    -- * DimensionGroup
    DimensionGroup (..),
    newDimensionGroup,
    dimensionGroup_limit,
    dimensionGroup_dimensions,
    dimensionGroup_group,

    -- * DimensionKeyDescription
    DimensionKeyDescription (..),
    newDimensionKeyDescription,
    dimensionKeyDescription_partitions,
    dimensionKeyDescription_total,
    dimensionKeyDescription_dimensions,

    -- * DimensionKeyDetail
    DimensionKeyDetail (..),
    newDimensionKeyDetail,
    dimensionKeyDetail_status,
    dimensionKeyDetail_dimension,
    dimensionKeyDetail_value,

    -- * MetricKeyDataPoints
    MetricKeyDataPoints (..),
    newMetricKeyDataPoints,
    metricKeyDataPoints_dataPoints,
    metricKeyDataPoints_key,

    -- * MetricQuery
    MetricQuery (..),
    newMetricQuery,
    metricQuery_groupBy,
    metricQuery_filter,
    metricQuery_metric,

    -- * ResponsePartitionKey
    ResponsePartitionKey (..),
    newResponsePartitionKey,
    responsePartitionKey_dimensions,

    -- * ResponseResourceMetricKey
    ResponseResourceMetricKey (..),
    newResponseResourceMetricKey,
    responseResourceMetricKey_dimensions,
    responseResourceMetricKey_metric,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pi.Types.DataPoint
import Network.AWS.Pi.Types.DetailStatus
import Network.AWS.Pi.Types.DimensionGroup
import Network.AWS.Pi.Types.DimensionKeyDescription
import Network.AWS.Pi.Types.DimensionKeyDetail
import Network.AWS.Pi.Types.MetricKeyDataPoints
import Network.AWS.Pi.Types.MetricQuery
import Network.AWS.Pi.Types.ResponsePartitionKey
import Network.AWS.Pi.Types.ResponseResourceMetricKey
import Network.AWS.Pi.Types.ServiceType
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-02-27@ of the Amazon Performance Insights SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Pi",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "pi",
      Core._serviceSigningName = "pi",
      Core._serviceVersion = "2018-02-27",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Pi",
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

-- | One of the arguments provided is invalid for this request.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | The user is not authorized to perform this request.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"

-- | The request failed due to an unknown error.
_InternalServiceError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceError =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"

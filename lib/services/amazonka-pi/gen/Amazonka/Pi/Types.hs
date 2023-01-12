{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pi.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServiceError,
    _InvalidArgumentException,
    _NotAuthorizedException,

    -- * DetailStatus
    DetailStatus (..),

    -- * FeatureStatus
    FeatureStatus (..),

    -- * ServiceType
    ServiceType (..),

    -- * DataPoint
    DataPoint (..),
    newDataPoint,
    dataPoint_timestamp,
    dataPoint_value,

    -- * DimensionDetail
    DimensionDetail (..),
    newDimensionDetail,
    dimensionDetail_identifier,

    -- * DimensionGroup
    DimensionGroup (..),
    newDimensionGroup,
    dimensionGroup_dimensions,
    dimensionGroup_limit,
    dimensionGroup_group,

    -- * DimensionGroupDetail
    DimensionGroupDetail (..),
    newDimensionGroupDetail,
    dimensionGroupDetail_dimensions,
    dimensionGroupDetail_group,

    -- * DimensionKeyDescription
    DimensionKeyDescription (..),
    newDimensionKeyDescription,
    dimensionKeyDescription_additionalMetrics,
    dimensionKeyDescription_dimensions,
    dimensionKeyDescription_partitions,
    dimensionKeyDescription_total,

    -- * DimensionKeyDetail
    DimensionKeyDetail (..),
    newDimensionKeyDetail,
    dimensionKeyDetail_dimension,
    dimensionKeyDetail_status,
    dimensionKeyDetail_value,

    -- * FeatureMetadata
    FeatureMetadata (..),
    newFeatureMetadata,
    featureMetadata_status,

    -- * MetricDimensionGroups
    MetricDimensionGroups (..),
    newMetricDimensionGroups,
    metricDimensionGroups_groups,
    metricDimensionGroups_metric,

    -- * MetricKeyDataPoints
    MetricKeyDataPoints (..),
    newMetricKeyDataPoints,
    metricKeyDataPoints_dataPoints,
    metricKeyDataPoints_key,

    -- * MetricQuery
    MetricQuery (..),
    newMetricQuery,
    metricQuery_filter,
    metricQuery_groupBy,
    metricQuery_metric,

    -- * ResponsePartitionKey
    ResponsePartitionKey (..),
    newResponsePartitionKey,
    responsePartitionKey_dimensions,

    -- * ResponseResourceMetric
    ResponseResourceMetric (..),
    newResponseResourceMetric,
    responseResourceMetric_description,
    responseResourceMetric_metric,
    responseResourceMetric_unit,

    -- * ResponseResourceMetricKey
    ResponseResourceMetricKey (..),
    newResponseResourceMetricKey,
    responseResourceMetricKey_dimensions,
    responseResourceMetricKey_metric,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pi.Types.DataPoint
import Amazonka.Pi.Types.DetailStatus
import Amazonka.Pi.Types.DimensionDetail
import Amazonka.Pi.Types.DimensionGroup
import Amazonka.Pi.Types.DimensionGroupDetail
import Amazonka.Pi.Types.DimensionKeyDescription
import Amazonka.Pi.Types.DimensionKeyDetail
import Amazonka.Pi.Types.FeatureMetadata
import Amazonka.Pi.Types.FeatureStatus
import Amazonka.Pi.Types.MetricDimensionGroups
import Amazonka.Pi.Types.MetricKeyDataPoints
import Amazonka.Pi.Types.MetricQuery
import Amazonka.Pi.Types.ResponsePartitionKey
import Amazonka.Pi.Types.ResponseResourceMetric
import Amazonka.Pi.Types.ResponseResourceMetricKey
import Amazonka.Pi.Types.ServiceType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-02-27@ of the Amazon Performance Insights SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Pi",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "pi",
      Core.signingName = "pi",
      Core.version = "2018-02-27",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Pi",
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

-- | The request failed due to an unknown error.
_InternalServiceError :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServiceError =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"

-- | One of the arguments provided is invalid for this request.
_InvalidArgumentException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | The user is not authorized to perform this request.
_NotAuthorizedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"

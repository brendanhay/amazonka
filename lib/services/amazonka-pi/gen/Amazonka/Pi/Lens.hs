{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pi.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Lens
  ( -- * Operations

    -- ** DescribeDimensionKeys
    describeDimensionKeys_nextToken,
    describeDimensionKeys_filter,
    describeDimensionKeys_maxResults,
    describeDimensionKeys_periodInSeconds,
    describeDimensionKeys_partitionBy,
    describeDimensionKeys_serviceType,
    describeDimensionKeys_identifier,
    describeDimensionKeys_startTime,
    describeDimensionKeys_endTime,
    describeDimensionKeys_metric,
    describeDimensionKeys_groupBy,
    describeDimensionKeysResponse_nextToken,
    describeDimensionKeysResponse_alignedEndTime,
    describeDimensionKeysResponse_partitionKeys,
    describeDimensionKeysResponse_keys,
    describeDimensionKeysResponse_alignedStartTime,
    describeDimensionKeysResponse_httpStatus,

    -- ** GetDimensionKeyDetails
    getDimensionKeyDetails_requestedDimensions,
    getDimensionKeyDetails_serviceType,
    getDimensionKeyDetails_identifier,
    getDimensionKeyDetails_group,
    getDimensionKeyDetails_groupIdentifier,
    getDimensionKeyDetailsResponse_dimensions,
    getDimensionKeyDetailsResponse_httpStatus,

    -- ** GetResourceMetrics
    getResourceMetrics_nextToken,
    getResourceMetrics_maxResults,
    getResourceMetrics_periodInSeconds,
    getResourceMetrics_serviceType,
    getResourceMetrics_identifier,
    getResourceMetrics_metricQueries,
    getResourceMetrics_startTime,
    getResourceMetrics_endTime,
    getResourceMetricsResponse_nextToken,
    getResourceMetricsResponse_alignedEndTime,
    getResourceMetricsResponse_identifier,
    getResourceMetricsResponse_metricList,
    getResourceMetricsResponse_alignedStartTime,
    getResourceMetricsResponse_httpStatus,

    -- * Types

    -- ** DataPoint
    dataPoint_timestamp,
    dataPoint_value,

    -- ** DimensionGroup
    dimensionGroup_dimensions,
    dimensionGroup_limit,
    dimensionGroup_group,

    -- ** DimensionKeyDescription
    dimensionKeyDescription_total,
    dimensionKeyDescription_dimensions,
    dimensionKeyDescription_partitions,

    -- ** DimensionKeyDetail
    dimensionKeyDetail_status,
    dimensionKeyDetail_dimension,
    dimensionKeyDetail_value,

    -- ** MetricKeyDataPoints
    metricKeyDataPoints_dataPoints,
    metricKeyDataPoints_key,

    -- ** MetricQuery
    metricQuery_groupBy,
    metricQuery_filter,
    metricQuery_metric,

    -- ** ResponsePartitionKey
    responsePartitionKey_dimensions,

    -- ** ResponseResourceMetricKey
    responseResourceMetricKey_dimensions,
    responseResourceMetricKey_metric,
  )
where

import Amazonka.Pi.DescribeDimensionKeys
import Amazonka.Pi.GetDimensionKeyDetails
import Amazonka.Pi.GetResourceMetrics
import Amazonka.Pi.Types.DataPoint
import Amazonka.Pi.Types.DimensionGroup
import Amazonka.Pi.Types.DimensionKeyDescription
import Amazonka.Pi.Types.DimensionKeyDetail
import Amazonka.Pi.Types.MetricKeyDataPoints
import Amazonka.Pi.Types.MetricQuery
import Amazonka.Pi.Types.ResponsePartitionKey
import Amazonka.Pi.Types.ResponseResourceMetricKey

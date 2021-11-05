{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pi.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pi.Lens
  ( -- * Operations

    -- ** GetDimensionKeyDetails
    getDimensionKeyDetails_requestedDimensions,
    getDimensionKeyDetails_serviceType,
    getDimensionKeyDetails_identifier,
    getDimensionKeyDetails_group,
    getDimensionKeyDetails_groupIdentifier,
    getDimensionKeyDetailsResponse_dimensions,
    getDimensionKeyDetailsResponse_httpStatus,

    -- ** GetResourceMetrics
    getResourceMetrics_periodInSeconds,
    getResourceMetrics_nextToken,
    getResourceMetrics_maxResults,
    getResourceMetrics_serviceType,
    getResourceMetrics_identifier,
    getResourceMetrics_metricQueries,
    getResourceMetrics_startTime,
    getResourceMetrics_endTime,
    getResourceMetricsResponse_identifier,
    getResourceMetricsResponse_metricList,
    getResourceMetricsResponse_alignedEndTime,
    getResourceMetricsResponse_alignedStartTime,
    getResourceMetricsResponse_nextToken,
    getResourceMetricsResponse_httpStatus,

    -- ** DescribeDimensionKeys
    describeDimensionKeys_periodInSeconds,
    describeDimensionKeys_nextToken,
    describeDimensionKeys_filter,
    describeDimensionKeys_maxResults,
    describeDimensionKeys_partitionBy,
    describeDimensionKeys_serviceType,
    describeDimensionKeys_identifier,
    describeDimensionKeys_startTime,
    describeDimensionKeys_endTime,
    describeDimensionKeys_metric,
    describeDimensionKeys_groupBy,
    describeDimensionKeysResponse_alignedEndTime,
    describeDimensionKeysResponse_alignedStartTime,
    describeDimensionKeysResponse_keys,
    describeDimensionKeysResponse_nextToken,
    describeDimensionKeysResponse_partitionKeys,
    describeDimensionKeysResponse_httpStatus,

    -- * Types

    -- ** DataPoint
    dataPoint_timestamp,
    dataPoint_value,

    -- ** DimensionGroup
    dimensionGroup_limit,
    dimensionGroup_dimensions,
    dimensionGroup_group,

    -- ** DimensionKeyDescription
    dimensionKeyDescription_partitions,
    dimensionKeyDescription_total,
    dimensionKeyDescription_dimensions,

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

import Network.AWS.Pi.DescribeDimensionKeys
import Network.AWS.Pi.GetDimensionKeyDetails
import Network.AWS.Pi.GetResourceMetrics
import Network.AWS.Pi.Types.DataPoint
import Network.AWS.Pi.Types.DimensionGroup
import Network.AWS.Pi.Types.DimensionKeyDescription
import Network.AWS.Pi.Types.DimensionKeyDetail
import Network.AWS.Pi.Types.MetricKeyDataPoints
import Network.AWS.Pi.Types.MetricQuery
import Network.AWS.Pi.Types.ResponsePartitionKey
import Network.AWS.Pi.Types.ResponseResourceMetricKey

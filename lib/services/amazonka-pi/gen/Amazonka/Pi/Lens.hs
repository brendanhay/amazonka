{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pi.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Lens
  ( -- * Operations

    -- ** DescribeDimensionKeys
    describeDimensionKeys_additionalMetrics,
    describeDimensionKeys_filter,
    describeDimensionKeys_maxResults,
    describeDimensionKeys_nextToken,
    describeDimensionKeys_partitionBy,
    describeDimensionKeys_periodInSeconds,
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

    -- ** GetDimensionKeyDetails
    getDimensionKeyDetails_requestedDimensions,
    getDimensionKeyDetails_serviceType,
    getDimensionKeyDetails_identifier,
    getDimensionKeyDetails_group,
    getDimensionKeyDetails_groupIdentifier,
    getDimensionKeyDetailsResponse_dimensions,
    getDimensionKeyDetailsResponse_httpStatus,

    -- ** GetResourceMetadata
    getResourceMetadata_serviceType,
    getResourceMetadata_identifier,
    getResourceMetadataResponse_features,
    getResourceMetadataResponse_identifier,
    getResourceMetadataResponse_httpStatus,

    -- ** GetResourceMetrics
    getResourceMetrics_maxResults,
    getResourceMetrics_nextToken,
    getResourceMetrics_periodAlignment,
    getResourceMetrics_periodInSeconds,
    getResourceMetrics_serviceType,
    getResourceMetrics_identifier,
    getResourceMetrics_metricQueries,
    getResourceMetrics_startTime,
    getResourceMetrics_endTime,
    getResourceMetricsResponse_alignedEndTime,
    getResourceMetricsResponse_alignedStartTime,
    getResourceMetricsResponse_identifier,
    getResourceMetricsResponse_metricList,
    getResourceMetricsResponse_nextToken,
    getResourceMetricsResponse_httpStatus,

    -- ** ListAvailableResourceDimensions
    listAvailableResourceDimensions_maxResults,
    listAvailableResourceDimensions_nextToken,
    listAvailableResourceDimensions_serviceType,
    listAvailableResourceDimensions_identifier,
    listAvailableResourceDimensions_metrics,
    listAvailableResourceDimensionsResponse_metricDimensions,
    listAvailableResourceDimensionsResponse_nextToken,
    listAvailableResourceDimensionsResponse_httpStatus,

    -- ** ListAvailableResourceMetrics
    listAvailableResourceMetrics_maxResults,
    listAvailableResourceMetrics_nextToken,
    listAvailableResourceMetrics_serviceType,
    listAvailableResourceMetrics_identifier,
    listAvailableResourceMetrics_metricTypes,
    listAvailableResourceMetricsResponse_metrics,
    listAvailableResourceMetricsResponse_nextToken,
    listAvailableResourceMetricsResponse_httpStatus,

    -- * Types

    -- ** DataPoint
    dataPoint_timestamp,
    dataPoint_value,

    -- ** DimensionDetail
    dimensionDetail_identifier,

    -- ** DimensionGroup
    dimensionGroup_dimensions,
    dimensionGroup_limit,
    dimensionGroup_group,

    -- ** DimensionGroupDetail
    dimensionGroupDetail_dimensions,
    dimensionGroupDetail_group,

    -- ** DimensionKeyDescription
    dimensionKeyDescription_additionalMetrics,
    dimensionKeyDescription_dimensions,
    dimensionKeyDescription_partitions,
    dimensionKeyDescription_total,

    -- ** DimensionKeyDetail
    dimensionKeyDetail_dimension,
    dimensionKeyDetail_status,
    dimensionKeyDetail_value,

    -- ** FeatureMetadata
    featureMetadata_status,

    -- ** MetricDimensionGroups
    metricDimensionGroups_groups,
    metricDimensionGroups_metric,

    -- ** MetricKeyDataPoints
    metricKeyDataPoints_dataPoints,
    metricKeyDataPoints_key,

    -- ** MetricQuery
    metricQuery_filter,
    metricQuery_groupBy,
    metricQuery_metric,

    -- ** ResponsePartitionKey
    responsePartitionKey_dimensions,

    -- ** ResponseResourceMetric
    responseResourceMetric_description,
    responseResourceMetric_metric,
    responseResourceMetric_unit,

    -- ** ResponseResourceMetricKey
    responseResourceMetricKey_dimensions,
    responseResourceMetricKey_metric,
  )
where

import Amazonka.Pi.DescribeDimensionKeys
import Amazonka.Pi.GetDimensionKeyDetails
import Amazonka.Pi.GetResourceMetadata
import Amazonka.Pi.GetResourceMetrics
import Amazonka.Pi.ListAvailableResourceDimensions
import Amazonka.Pi.ListAvailableResourceMetrics
import Amazonka.Pi.Types.DataPoint
import Amazonka.Pi.Types.DimensionDetail
import Amazonka.Pi.Types.DimensionGroup
import Amazonka.Pi.Types.DimensionGroupDetail
import Amazonka.Pi.Types.DimensionKeyDescription
import Amazonka.Pi.Types.DimensionKeyDetail
import Amazonka.Pi.Types.FeatureMetadata
import Amazonka.Pi.Types.MetricDimensionGroups
import Amazonka.Pi.Types.MetricKeyDataPoints
import Amazonka.Pi.Types.MetricQuery
import Amazonka.Pi.Types.ResponsePartitionKey
import Amazonka.Pi.Types.ResponseResourceMetric
import Amazonka.Pi.Types.ResponseResourceMetricKey

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Pi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-02-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon RDS Performance Insights
--
-- Amazon RDS Performance Insights enables you to monitor and explore
-- different dimensions of database load based on data captured from a
-- running DB instance. The guide provides detailed information about
-- Performance Insights data types, parameters and errors.
--
-- When Performance Insights is enabled, the Amazon RDS Performance
-- Insights API provides visibility into the performance of your DB
-- instance. Amazon CloudWatch provides the authoritative source for Amazon
-- Web Services service-vended monitoring metrics. Performance Insights
-- offers a domain-specific view of DB load.
--
-- DB load is measured as average active sessions. Performance Insights
-- provides the data to API consumers as a two-dimensional time-series
-- dataset. The time dimension provides DB load data for each time point in
-- the queried time range. Each time point decomposes overall load in
-- relation to the requested dimensions, measured at that time point.
-- Examples include SQL, Wait event, User, and Host.
--
-- -   To learn more about Performance Insights and Amazon Aurora DB
--     instances, go to the
--     /<https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_PerfInsights.html Amazon Aurora User Guide>/
--     .
--
-- -   To learn more about Performance Insights and Amazon RDS DB
--     instances, go to the
--     /<https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Amazon RDS User Guide>/
--     .
--
-- -   To learn more about Performance Insights and Amazon DocumentDB
--     clusters, go to the
--     /<https://docs.aws.amazon.com/documentdb/latest/developerguide/performance-insights.html Amazon DocumentDB Developer Guide>/
--     .
module Amazonka.Pi
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServiceError
    _InternalServiceError,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeDimensionKeys
    DescribeDimensionKeys (DescribeDimensionKeys'),
    newDescribeDimensionKeys,
    DescribeDimensionKeysResponse (DescribeDimensionKeysResponse'),
    newDescribeDimensionKeysResponse,

    -- ** GetDimensionKeyDetails
    GetDimensionKeyDetails (GetDimensionKeyDetails'),
    newGetDimensionKeyDetails,
    GetDimensionKeyDetailsResponse (GetDimensionKeyDetailsResponse'),
    newGetDimensionKeyDetailsResponse,

    -- ** GetResourceMetadata
    GetResourceMetadata (GetResourceMetadata'),
    newGetResourceMetadata,
    GetResourceMetadataResponse (GetResourceMetadataResponse'),
    newGetResourceMetadataResponse,

    -- ** GetResourceMetrics
    GetResourceMetrics (GetResourceMetrics'),
    newGetResourceMetrics,
    GetResourceMetricsResponse (GetResourceMetricsResponse'),
    newGetResourceMetricsResponse,

    -- ** ListAvailableResourceDimensions
    ListAvailableResourceDimensions (ListAvailableResourceDimensions'),
    newListAvailableResourceDimensions,
    ListAvailableResourceDimensionsResponse (ListAvailableResourceDimensionsResponse'),
    newListAvailableResourceDimensionsResponse,

    -- ** ListAvailableResourceMetrics
    ListAvailableResourceMetrics (ListAvailableResourceMetrics'),
    newListAvailableResourceMetrics,
    ListAvailableResourceMetricsResponse (ListAvailableResourceMetricsResponse'),
    newListAvailableResourceMetricsResponse,

    -- * Types

    -- ** DetailStatus
    DetailStatus (..),

    -- ** FeatureStatus
    FeatureStatus (..),

    -- ** ServiceType
    ServiceType (..),

    -- ** DataPoint
    DataPoint (DataPoint'),
    newDataPoint,

    -- ** DimensionDetail
    DimensionDetail (DimensionDetail'),
    newDimensionDetail,

    -- ** DimensionGroup
    DimensionGroup (DimensionGroup'),
    newDimensionGroup,

    -- ** DimensionGroupDetail
    DimensionGroupDetail (DimensionGroupDetail'),
    newDimensionGroupDetail,

    -- ** DimensionKeyDescription
    DimensionKeyDescription (DimensionKeyDescription'),
    newDimensionKeyDescription,

    -- ** DimensionKeyDetail
    DimensionKeyDetail (DimensionKeyDetail'),
    newDimensionKeyDetail,

    -- ** FeatureMetadata
    FeatureMetadata (FeatureMetadata'),
    newFeatureMetadata,

    -- ** MetricDimensionGroups
    MetricDimensionGroups (MetricDimensionGroups'),
    newMetricDimensionGroups,

    -- ** MetricKeyDataPoints
    MetricKeyDataPoints (MetricKeyDataPoints'),
    newMetricKeyDataPoints,

    -- ** MetricQuery
    MetricQuery (MetricQuery'),
    newMetricQuery,

    -- ** ResponsePartitionKey
    ResponsePartitionKey (ResponsePartitionKey'),
    newResponsePartitionKey,

    -- ** ResponseResourceMetric
    ResponseResourceMetric (ResponseResourceMetric'),
    newResponseResourceMetric,

    -- ** ResponseResourceMetricKey
    ResponseResourceMetricKey (ResponseResourceMetricKey'),
    newResponseResourceMetricKey,
  )
where

import Amazonka.Pi.DescribeDimensionKeys
import Amazonka.Pi.GetDimensionKeyDetails
import Amazonka.Pi.GetResourceMetadata
import Amazonka.Pi.GetResourceMetrics
import Amazonka.Pi.Lens
import Amazonka.Pi.ListAvailableResourceDimensions
import Amazonka.Pi.ListAvailableResourceMetrics
import Amazonka.Pi.Types
import Amazonka.Pi.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Pi'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.LookoutMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the /Amazon Lookout for Metrics API Reference/. For an
-- introduction to the service with tutorials for getting started, visit
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev Amazon Lookout for Metrics Developer Guide>.
module Amazonka.LookoutMetrics
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ActivateAnomalyDetector
    ActivateAnomalyDetector (ActivateAnomalyDetector'),
    newActivateAnomalyDetector,
    ActivateAnomalyDetectorResponse (ActivateAnomalyDetectorResponse'),
    newActivateAnomalyDetectorResponse,

    -- ** BackTestAnomalyDetector
    BackTestAnomalyDetector (BackTestAnomalyDetector'),
    newBackTestAnomalyDetector,
    BackTestAnomalyDetectorResponse (BackTestAnomalyDetectorResponse'),
    newBackTestAnomalyDetectorResponse,

    -- ** CreateAlert
    CreateAlert (CreateAlert'),
    newCreateAlert,
    CreateAlertResponse (CreateAlertResponse'),
    newCreateAlertResponse,

    -- ** CreateAnomalyDetector
    CreateAnomalyDetector (CreateAnomalyDetector'),
    newCreateAnomalyDetector,
    CreateAnomalyDetectorResponse (CreateAnomalyDetectorResponse'),
    newCreateAnomalyDetectorResponse,

    -- ** CreateMetricSet
    CreateMetricSet (CreateMetricSet'),
    newCreateMetricSet,
    CreateMetricSetResponse (CreateMetricSetResponse'),
    newCreateMetricSetResponse,

    -- ** DeactivateAnomalyDetector
    DeactivateAnomalyDetector (DeactivateAnomalyDetector'),
    newDeactivateAnomalyDetector,
    DeactivateAnomalyDetectorResponse (DeactivateAnomalyDetectorResponse'),
    newDeactivateAnomalyDetectorResponse,

    -- ** DeleteAlert
    DeleteAlert (DeleteAlert'),
    newDeleteAlert,
    DeleteAlertResponse (DeleteAlertResponse'),
    newDeleteAlertResponse,

    -- ** DeleteAnomalyDetector
    DeleteAnomalyDetector (DeleteAnomalyDetector'),
    newDeleteAnomalyDetector,
    DeleteAnomalyDetectorResponse (DeleteAnomalyDetectorResponse'),
    newDeleteAnomalyDetectorResponse,

    -- ** DescribeAlert
    DescribeAlert (DescribeAlert'),
    newDescribeAlert,
    DescribeAlertResponse (DescribeAlertResponse'),
    newDescribeAlertResponse,

    -- ** DescribeAnomalyDetectionExecutions
    DescribeAnomalyDetectionExecutions (DescribeAnomalyDetectionExecutions'),
    newDescribeAnomalyDetectionExecutions,
    DescribeAnomalyDetectionExecutionsResponse (DescribeAnomalyDetectionExecutionsResponse'),
    newDescribeAnomalyDetectionExecutionsResponse,

    -- ** DescribeAnomalyDetector
    DescribeAnomalyDetector (DescribeAnomalyDetector'),
    newDescribeAnomalyDetector,
    DescribeAnomalyDetectorResponse (DescribeAnomalyDetectorResponse'),
    newDescribeAnomalyDetectorResponse,

    -- ** DescribeMetricSet
    DescribeMetricSet (DescribeMetricSet'),
    newDescribeMetricSet,
    DescribeMetricSetResponse (DescribeMetricSetResponse'),
    newDescribeMetricSetResponse,

    -- ** DetectMetricSetConfig
    DetectMetricSetConfig (DetectMetricSetConfig'),
    newDetectMetricSetConfig,
    DetectMetricSetConfigResponse (DetectMetricSetConfigResponse'),
    newDetectMetricSetConfigResponse,

    -- ** GetAnomalyGroup
    GetAnomalyGroup (GetAnomalyGroup'),
    newGetAnomalyGroup,
    GetAnomalyGroupResponse (GetAnomalyGroupResponse'),
    newGetAnomalyGroupResponse,

    -- ** GetDataQualityMetrics
    GetDataQualityMetrics (GetDataQualityMetrics'),
    newGetDataQualityMetrics,
    GetDataQualityMetricsResponse (GetDataQualityMetricsResponse'),
    newGetDataQualityMetricsResponse,

    -- ** GetFeedback
    GetFeedback (GetFeedback'),
    newGetFeedback,
    GetFeedbackResponse (GetFeedbackResponse'),
    newGetFeedbackResponse,

    -- ** GetSampleData
    GetSampleData (GetSampleData'),
    newGetSampleData,
    GetSampleDataResponse (GetSampleDataResponse'),
    newGetSampleDataResponse,

    -- ** ListAlerts
    ListAlerts (ListAlerts'),
    newListAlerts,
    ListAlertsResponse (ListAlertsResponse'),
    newListAlertsResponse,

    -- ** ListAnomalyDetectors
    ListAnomalyDetectors (ListAnomalyDetectors'),
    newListAnomalyDetectors,
    ListAnomalyDetectorsResponse (ListAnomalyDetectorsResponse'),
    newListAnomalyDetectorsResponse,

    -- ** ListAnomalyGroupRelatedMetrics
    ListAnomalyGroupRelatedMetrics (ListAnomalyGroupRelatedMetrics'),
    newListAnomalyGroupRelatedMetrics,
    ListAnomalyGroupRelatedMetricsResponse (ListAnomalyGroupRelatedMetricsResponse'),
    newListAnomalyGroupRelatedMetricsResponse,

    -- ** ListAnomalyGroupSummaries
    ListAnomalyGroupSummaries (ListAnomalyGroupSummaries'),
    newListAnomalyGroupSummaries,
    ListAnomalyGroupSummariesResponse (ListAnomalyGroupSummariesResponse'),
    newListAnomalyGroupSummariesResponse,

    -- ** ListAnomalyGroupTimeSeries
    ListAnomalyGroupTimeSeries (ListAnomalyGroupTimeSeries'),
    newListAnomalyGroupTimeSeries,
    ListAnomalyGroupTimeSeriesResponse (ListAnomalyGroupTimeSeriesResponse'),
    newListAnomalyGroupTimeSeriesResponse,

    -- ** ListMetricSets
    ListMetricSets (ListMetricSets'),
    newListMetricSets,
    ListMetricSetsResponse (ListMetricSetsResponse'),
    newListMetricSetsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutFeedback
    PutFeedback (PutFeedback'),
    newPutFeedback,
    PutFeedbackResponse (PutFeedbackResponse'),
    newPutFeedbackResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAlert
    UpdateAlert (UpdateAlert'),
    newUpdateAlert,
    UpdateAlertResponse (UpdateAlertResponse'),
    newUpdateAlertResponse,

    -- ** UpdateAnomalyDetector
    UpdateAnomalyDetector (UpdateAnomalyDetector'),
    newUpdateAnomalyDetector,
    UpdateAnomalyDetectorResponse (UpdateAnomalyDetectorResponse'),
    newUpdateAnomalyDetectorResponse,

    -- ** UpdateMetricSet
    UpdateMetricSet (UpdateMetricSet'),
    newUpdateMetricSet,
    UpdateMetricSetResponse (UpdateMetricSetResponse'),
    newUpdateMetricSetResponse,

    -- * Types

    -- ** AggregationFunction
    AggregationFunction (..),

    -- ** AlertStatus
    AlertStatus (..),

    -- ** AlertType
    AlertType (..),

    -- ** AnomalyDetectionTaskStatus
    AnomalyDetectionTaskStatus (..),

    -- ** AnomalyDetectorFailureType
    AnomalyDetectorFailureType (..),

    -- ** AnomalyDetectorStatus
    AnomalyDetectorStatus (..),

    -- ** CSVFileCompression
    CSVFileCompression (..),

    -- ** Confidence
    Confidence (..),

    -- ** DataQualityMetricType
    DataQualityMetricType (..),

    -- ** FilterOperation
    FilterOperation (..),

    -- ** Frequency
    Frequency (..),

    -- ** JsonFileCompression
    JsonFileCompression (..),

    -- ** RelationshipType
    RelationshipType (..),

    -- ** SnsFormat
    SnsFormat (..),

    -- ** Action
    Action (Action'),
    newAction,

    -- ** Alert
    Alert (Alert'),
    newAlert,

    -- ** AlertFilters
    AlertFilters (AlertFilters'),
    newAlertFilters,

    -- ** AlertSummary
    AlertSummary (AlertSummary'),
    newAlertSummary,

    -- ** AnomalyDetectorConfig
    AnomalyDetectorConfig (AnomalyDetectorConfig'),
    newAnomalyDetectorConfig,

    -- ** AnomalyDetectorConfigSummary
    AnomalyDetectorConfigSummary (AnomalyDetectorConfigSummary'),
    newAnomalyDetectorConfigSummary,

    -- ** AnomalyDetectorDataQualityMetric
    AnomalyDetectorDataQualityMetric (AnomalyDetectorDataQualityMetric'),
    newAnomalyDetectorDataQualityMetric,

    -- ** AnomalyDetectorSummary
    AnomalyDetectorSummary (AnomalyDetectorSummary'),
    newAnomalyDetectorSummary,

    -- ** AnomalyGroup
    AnomalyGroup (AnomalyGroup'),
    newAnomalyGroup,

    -- ** AnomalyGroupStatistics
    AnomalyGroupStatistics (AnomalyGroupStatistics'),
    newAnomalyGroupStatistics,

    -- ** AnomalyGroupSummary
    AnomalyGroupSummary (AnomalyGroupSummary'),
    newAnomalyGroupSummary,

    -- ** AnomalyGroupTimeSeries
    AnomalyGroupTimeSeries (AnomalyGroupTimeSeries'),
    newAnomalyGroupTimeSeries,

    -- ** AnomalyGroupTimeSeriesFeedback
    AnomalyGroupTimeSeriesFeedback (AnomalyGroupTimeSeriesFeedback'),
    newAnomalyGroupTimeSeriesFeedback,

    -- ** AppFlowConfig
    AppFlowConfig (AppFlowConfig'),
    newAppFlowConfig,

    -- ** AthenaSourceConfig
    AthenaSourceConfig (AthenaSourceConfig'),
    newAthenaSourceConfig,

    -- ** AttributeValue
    AttributeValue (AttributeValue'),
    newAttributeValue,

    -- ** AutoDetectionMetricSource
    AutoDetectionMetricSource (AutoDetectionMetricSource'),
    newAutoDetectionMetricSource,

    -- ** AutoDetectionS3SourceConfig
    AutoDetectionS3SourceConfig (AutoDetectionS3SourceConfig'),
    newAutoDetectionS3SourceConfig,

    -- ** BackTestConfiguration
    BackTestConfiguration (BackTestConfiguration'),
    newBackTestConfiguration,

    -- ** CloudWatchConfig
    CloudWatchConfig (CloudWatchConfig'),
    newCloudWatchConfig,

    -- ** ContributionMatrix
    ContributionMatrix (ContributionMatrix'),
    newContributionMatrix,

    -- ** CsvFormatDescriptor
    CsvFormatDescriptor (CsvFormatDescriptor'),
    newCsvFormatDescriptor,

    -- ** DataQualityMetric
    DataQualityMetric (DataQualityMetric'),
    newDataQualityMetric,

    -- ** DetectedCsvFormatDescriptor
    DetectedCsvFormatDescriptor (DetectedCsvFormatDescriptor'),
    newDetectedCsvFormatDescriptor,

    -- ** DetectedField
    DetectedField (DetectedField'),
    newDetectedField,

    -- ** DetectedFileFormatDescriptor
    DetectedFileFormatDescriptor (DetectedFileFormatDescriptor'),
    newDetectedFileFormatDescriptor,

    -- ** DetectedJsonFormatDescriptor
    DetectedJsonFormatDescriptor (DetectedJsonFormatDescriptor'),
    newDetectedJsonFormatDescriptor,

    -- ** DetectedMetricSetConfig
    DetectedMetricSetConfig (DetectedMetricSetConfig'),
    newDetectedMetricSetConfig,

    -- ** DetectedMetricSource
    DetectedMetricSource (DetectedMetricSource'),
    newDetectedMetricSource,

    -- ** DetectedS3SourceConfig
    DetectedS3SourceConfig (DetectedS3SourceConfig'),
    newDetectedS3SourceConfig,

    -- ** DimensionContribution
    DimensionContribution (DimensionContribution'),
    newDimensionContribution,

    -- ** DimensionFilter
    DimensionFilter (DimensionFilter'),
    newDimensionFilter,

    -- ** DimensionNameValue
    DimensionNameValue (DimensionNameValue'),
    newDimensionNameValue,

    -- ** DimensionValueContribution
    DimensionValueContribution (DimensionValueContribution'),
    newDimensionValueContribution,

    -- ** ExecutionStatus
    ExecutionStatus (ExecutionStatus'),
    newExecutionStatus,

    -- ** FileFormatDescriptor
    FileFormatDescriptor (FileFormatDescriptor'),
    newFileFormatDescriptor,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** InterMetricImpactDetails
    InterMetricImpactDetails (InterMetricImpactDetails'),
    newInterMetricImpactDetails,

    -- ** ItemizedMetricStats
    ItemizedMetricStats (ItemizedMetricStats'),
    newItemizedMetricStats,

    -- ** JsonFormatDescriptor
    JsonFormatDescriptor (JsonFormatDescriptor'),
    newJsonFormatDescriptor,

    -- ** LambdaConfiguration
    LambdaConfiguration (LambdaConfiguration'),
    newLambdaConfiguration,

    -- ** Metric
    Metric (Metric'),
    newMetric,

    -- ** MetricLevelImpact
    MetricLevelImpact (MetricLevelImpact'),
    newMetricLevelImpact,

    -- ** MetricSetDataQualityMetric
    MetricSetDataQualityMetric (MetricSetDataQualityMetric'),
    newMetricSetDataQualityMetric,

    -- ** MetricSetDimensionFilter
    MetricSetDimensionFilter (MetricSetDimensionFilter'),
    newMetricSetDimensionFilter,

    -- ** MetricSetSummary
    MetricSetSummary (MetricSetSummary'),
    newMetricSetSummary,

    -- ** MetricSource
    MetricSource (MetricSource'),
    newMetricSource,

    -- ** RDSSourceConfig
    RDSSourceConfig (RDSSourceConfig'),
    newRDSSourceConfig,

    -- ** RedshiftSourceConfig
    RedshiftSourceConfig (RedshiftSourceConfig'),
    newRedshiftSourceConfig,

    -- ** S3SourceConfig
    S3SourceConfig (S3SourceConfig'),
    newS3SourceConfig,

    -- ** SNSConfiguration
    SNSConfiguration (SNSConfiguration'),
    newSNSConfiguration,

    -- ** SampleDataS3SourceConfig
    SampleDataS3SourceConfig (SampleDataS3SourceConfig'),
    newSampleDataS3SourceConfig,

    -- ** TimeSeries
    TimeSeries (TimeSeries'),
    newTimeSeries,

    -- ** TimeSeriesFeedback
    TimeSeriesFeedback (TimeSeriesFeedback'),
    newTimeSeriesFeedback,

    -- ** TimestampColumn
    TimestampColumn (TimestampColumn'),
    newTimestampColumn,

    -- ** VpcConfiguration
    VpcConfiguration (VpcConfiguration'),
    newVpcConfiguration,
  )
where

import Amazonka.LookoutMetrics.ActivateAnomalyDetector
import Amazonka.LookoutMetrics.BackTestAnomalyDetector
import Amazonka.LookoutMetrics.CreateAlert
import Amazonka.LookoutMetrics.CreateAnomalyDetector
import Amazonka.LookoutMetrics.CreateMetricSet
import Amazonka.LookoutMetrics.DeactivateAnomalyDetector
import Amazonka.LookoutMetrics.DeleteAlert
import Amazonka.LookoutMetrics.DeleteAnomalyDetector
import Amazonka.LookoutMetrics.DescribeAlert
import Amazonka.LookoutMetrics.DescribeAnomalyDetectionExecutions
import Amazonka.LookoutMetrics.DescribeAnomalyDetector
import Amazonka.LookoutMetrics.DescribeMetricSet
import Amazonka.LookoutMetrics.DetectMetricSetConfig
import Amazonka.LookoutMetrics.GetAnomalyGroup
import Amazonka.LookoutMetrics.GetDataQualityMetrics
import Amazonka.LookoutMetrics.GetFeedback
import Amazonka.LookoutMetrics.GetSampleData
import Amazonka.LookoutMetrics.Lens
import Amazonka.LookoutMetrics.ListAlerts
import Amazonka.LookoutMetrics.ListAnomalyDetectors
import Amazonka.LookoutMetrics.ListAnomalyGroupRelatedMetrics
import Amazonka.LookoutMetrics.ListAnomalyGroupSummaries
import Amazonka.LookoutMetrics.ListAnomalyGroupTimeSeries
import Amazonka.LookoutMetrics.ListMetricSets
import Amazonka.LookoutMetrics.ListTagsForResource
import Amazonka.LookoutMetrics.PutFeedback
import Amazonka.LookoutMetrics.TagResource
import Amazonka.LookoutMetrics.Types
import Amazonka.LookoutMetrics.UntagResource
import Amazonka.LookoutMetrics.UpdateAlert
import Amazonka.LookoutMetrics.UpdateAnomalyDetector
import Amazonka.LookoutMetrics.UpdateMetricSet
import Amazonka.LookoutMetrics.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LookoutMetrics'.

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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.LookoutMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the /Amazon Lookout for Metrics API Reference/. For an
-- introduction to the service with tutorials for getting started, visit
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev Amazon Lookout for Metrics Developer Guide>.
module Network.AWS.LookoutMetrics
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetFeedback
    GetFeedback (GetFeedback'),
    newGetFeedback,
    GetFeedbackResponse (GetFeedbackResponse'),
    newGetFeedbackResponse,

    -- ** ListAlerts
    ListAlerts (ListAlerts'),
    newListAlerts,
    ListAlertsResponse (ListAlertsResponse'),
    newListAlertsResponse,

    -- ** ListMetricSets
    ListMetricSets (ListMetricSets'),
    newListMetricSets,
    ListMetricSetsResponse (ListMetricSetsResponse'),
    newListMetricSetsResponse,

    -- ** DeleteAnomalyDetector
    DeleteAnomalyDetector (DeleteAnomalyDetector'),
    newDeleteAnomalyDetector,
    DeleteAnomalyDetectorResponse (DeleteAnomalyDetectorResponse'),
    newDeleteAnomalyDetectorResponse,

    -- ** UpdateAnomalyDetector
    UpdateAnomalyDetector (UpdateAnomalyDetector'),
    newUpdateAnomalyDetector,
    UpdateAnomalyDetectorResponse (UpdateAnomalyDetectorResponse'),
    newUpdateAnomalyDetectorResponse,

    -- ** ListAnomalyDetectors
    ListAnomalyDetectors (ListAnomalyDetectors'),
    newListAnomalyDetectors,
    ListAnomalyDetectorsResponse (ListAnomalyDetectorsResponse'),
    newListAnomalyDetectorsResponse,

    -- ** DescribeAnomalyDetectionExecutions
    DescribeAnomalyDetectionExecutions (DescribeAnomalyDetectionExecutions'),
    newDescribeAnomalyDetectionExecutions,
    DescribeAnomalyDetectionExecutionsResponse (DescribeAnomalyDetectionExecutionsResponse'),
    newDescribeAnomalyDetectionExecutionsResponse,

    -- ** CreateMetricSet
    CreateMetricSet (CreateMetricSet'),
    newCreateMetricSet,
    CreateMetricSetResponse (CreateMetricSetResponse'),
    newCreateMetricSetResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateAlert
    CreateAlert (CreateAlert'),
    newCreateAlert,
    CreateAlertResponse (CreateAlertResponse'),
    newCreateAlertResponse,

    -- ** GetAnomalyGroup
    GetAnomalyGroup (GetAnomalyGroup'),
    newGetAnomalyGroup,
    GetAnomalyGroupResponse (GetAnomalyGroupResponse'),
    newGetAnomalyGroupResponse,

    -- ** PutFeedback
    PutFeedback (PutFeedback'),
    newPutFeedback,
    PutFeedbackResponse (PutFeedbackResponse'),
    newPutFeedbackResponse,

    -- ** BackTestAnomalyDetector
    BackTestAnomalyDetector (BackTestAnomalyDetector'),
    newBackTestAnomalyDetector,
    BackTestAnomalyDetectorResponse (BackTestAnomalyDetectorResponse'),
    newBackTestAnomalyDetectorResponse,

    -- ** DeleteAlert
    DeleteAlert (DeleteAlert'),
    newDeleteAlert,
    DeleteAlertResponse (DeleteAlertResponse'),
    newDeleteAlertResponse,

    -- ** CreateAnomalyDetector
    CreateAnomalyDetector (CreateAnomalyDetector'),
    newCreateAnomalyDetector,
    CreateAnomalyDetectorResponse (CreateAnomalyDetectorResponse'),
    newCreateAnomalyDetectorResponse,

    -- ** UpdateMetricSet
    UpdateMetricSet (UpdateMetricSet'),
    newUpdateMetricSet,
    UpdateMetricSetResponse (UpdateMetricSetResponse'),
    newUpdateMetricSetResponse,

    -- ** ActivateAnomalyDetector
    ActivateAnomalyDetector (ActivateAnomalyDetector'),
    newActivateAnomalyDetector,
    ActivateAnomalyDetectorResponse (ActivateAnomalyDetectorResponse'),
    newActivateAnomalyDetectorResponse,

    -- ** ListAnomalyGroupTimeSeries
    ListAnomalyGroupTimeSeries (ListAnomalyGroupTimeSeries'),
    newListAnomalyGroupTimeSeries,
    ListAnomalyGroupTimeSeriesResponse (ListAnomalyGroupTimeSeriesResponse'),
    newListAnomalyGroupTimeSeriesResponse,

    -- ** GetSampleData
    GetSampleData (GetSampleData'),
    newGetSampleData,
    GetSampleDataResponse (GetSampleDataResponse'),
    newGetSampleDataResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeMetricSet
    DescribeMetricSet (DescribeMetricSet'),
    newDescribeMetricSet,
    DescribeMetricSetResponse (DescribeMetricSetResponse'),
    newDescribeMetricSetResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeAlert
    DescribeAlert (DescribeAlert'),
    newDescribeAlert,
    DescribeAlertResponse (DescribeAlertResponse'),
    newDescribeAlertResponse,

    -- ** ListAnomalyGroupSummaries
    ListAnomalyGroupSummaries (ListAnomalyGroupSummaries'),
    newListAnomalyGroupSummaries,
    ListAnomalyGroupSummariesResponse (ListAnomalyGroupSummariesResponse'),
    newListAnomalyGroupSummariesResponse,

    -- ** DescribeAnomalyDetector
    DescribeAnomalyDetector (DescribeAnomalyDetector'),
    newDescribeAnomalyDetector,
    DescribeAnomalyDetectorResponse (DescribeAnomalyDetectorResponse'),
    newDescribeAnomalyDetectorResponse,

    -- * Types

    -- ** AggregationFunction
    AggregationFunction (..),

    -- ** AlertStatus
    AlertStatus (..),

    -- ** AlertType
    AlertType (..),

    -- ** AnomalyDetectionTaskStatus
    AnomalyDetectionTaskStatus (..),

    -- ** AnomalyDetectorStatus
    AnomalyDetectorStatus (..),

    -- ** CSVFileCompression
    CSVFileCompression (..),

    -- ** Frequency
    Frequency (..),

    -- ** JsonFileCompression
    JsonFileCompression (..),

    -- ** Action
    Action (Action'),
    newAction,

    -- ** Alert
    Alert (Alert'),
    newAlert,

    -- ** AlertSummary
    AlertSummary (AlertSummary'),
    newAlertSummary,

    -- ** AnomalyDetectorConfig
    AnomalyDetectorConfig (AnomalyDetectorConfig'),
    newAnomalyDetectorConfig,

    -- ** AnomalyDetectorConfigSummary
    AnomalyDetectorConfigSummary (AnomalyDetectorConfigSummary'),
    newAnomalyDetectorConfigSummary,

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

    -- ** CloudWatchConfig
    CloudWatchConfig (CloudWatchConfig'),
    newCloudWatchConfig,

    -- ** ContributionMatrix
    ContributionMatrix (ContributionMatrix'),
    newContributionMatrix,

    -- ** CsvFormatDescriptor
    CsvFormatDescriptor (CsvFormatDescriptor'),
    newCsvFormatDescriptor,

    -- ** DimensionContribution
    DimensionContribution (DimensionContribution'),
    newDimensionContribution,

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

import Network.AWS.LookoutMetrics.ActivateAnomalyDetector
import Network.AWS.LookoutMetrics.BackTestAnomalyDetector
import Network.AWS.LookoutMetrics.CreateAlert
import Network.AWS.LookoutMetrics.CreateAnomalyDetector
import Network.AWS.LookoutMetrics.CreateMetricSet
import Network.AWS.LookoutMetrics.DeleteAlert
import Network.AWS.LookoutMetrics.DeleteAnomalyDetector
import Network.AWS.LookoutMetrics.DescribeAlert
import Network.AWS.LookoutMetrics.DescribeAnomalyDetectionExecutions
import Network.AWS.LookoutMetrics.DescribeAnomalyDetector
import Network.AWS.LookoutMetrics.DescribeMetricSet
import Network.AWS.LookoutMetrics.GetAnomalyGroup
import Network.AWS.LookoutMetrics.GetFeedback
import Network.AWS.LookoutMetrics.GetSampleData
import Network.AWS.LookoutMetrics.Lens
import Network.AWS.LookoutMetrics.ListAlerts
import Network.AWS.LookoutMetrics.ListAnomalyDetectors
import Network.AWS.LookoutMetrics.ListAnomalyGroupSummaries
import Network.AWS.LookoutMetrics.ListAnomalyGroupTimeSeries
import Network.AWS.LookoutMetrics.ListMetricSets
import Network.AWS.LookoutMetrics.ListTagsForResource
import Network.AWS.LookoutMetrics.PutFeedback
import Network.AWS.LookoutMetrics.TagResource
import Network.AWS.LookoutMetrics.Types
import Network.AWS.LookoutMetrics.UntagResource
import Network.AWS.LookoutMetrics.UpdateAnomalyDetector
import Network.AWS.LookoutMetrics.UpdateMetricSet
import Network.AWS.LookoutMetrics.Waiters

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

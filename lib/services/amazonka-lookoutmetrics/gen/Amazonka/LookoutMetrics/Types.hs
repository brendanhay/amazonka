{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutMetrics.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _TooManyRequestsException,
    _ValidationException,

    -- * AggregationFunction
    AggregationFunction (..),

    -- * AlertStatus
    AlertStatus (..),

    -- * AlertType
    AlertType (..),

    -- * AnomalyDetectionTaskStatus
    AnomalyDetectionTaskStatus (..),

    -- * AnomalyDetectorFailureType
    AnomalyDetectorFailureType (..),

    -- * AnomalyDetectorStatus
    AnomalyDetectorStatus (..),

    -- * CSVFileCompression
    CSVFileCompression (..),

    -- * Confidence
    Confidence (..),

    -- * DataQualityMetricType
    DataQualityMetricType (..),

    -- * FilterOperation
    FilterOperation (..),

    -- * Frequency
    Frequency (..),

    -- * JsonFileCompression
    JsonFileCompression (..),

    -- * RelationshipType
    RelationshipType (..),

    -- * SnsFormat
    SnsFormat (..),

    -- * Action
    Action (..),
    newAction,
    action_lambdaConfiguration,
    action_sNSConfiguration,

    -- * Alert
    Alert (..),
    newAlert,
    alert_action,
    alert_alertArn,
    alert_alertDescription,
    alert_alertFilters,
    alert_alertName,
    alert_alertSensitivityThreshold,
    alert_alertStatus,
    alert_alertType,
    alert_anomalyDetectorArn,
    alert_creationTime,
    alert_lastModificationTime,

    -- * AlertFilters
    AlertFilters (..),
    newAlertFilters,
    alertFilters_dimensionFilterList,
    alertFilters_metricList,

    -- * AlertSummary
    AlertSummary (..),
    newAlertSummary,
    alertSummary_alertArn,
    alertSummary_alertName,
    alertSummary_alertSensitivityThreshold,
    alertSummary_alertStatus,
    alertSummary_alertType,
    alertSummary_anomalyDetectorArn,
    alertSummary_creationTime,
    alertSummary_lastModificationTime,
    alertSummary_tags,

    -- * AnomalyDetectorConfig
    AnomalyDetectorConfig (..),
    newAnomalyDetectorConfig,
    anomalyDetectorConfig_anomalyDetectorFrequency,

    -- * AnomalyDetectorConfigSummary
    AnomalyDetectorConfigSummary (..),
    newAnomalyDetectorConfigSummary,
    anomalyDetectorConfigSummary_anomalyDetectorFrequency,

    -- * AnomalyDetectorDataQualityMetric
    AnomalyDetectorDataQualityMetric (..),
    newAnomalyDetectorDataQualityMetric,
    anomalyDetectorDataQualityMetric_metricSetDataQualityMetricList,
    anomalyDetectorDataQualityMetric_startTimestamp,

    -- * AnomalyDetectorSummary
    AnomalyDetectorSummary (..),
    newAnomalyDetectorSummary,
    anomalyDetectorSummary_anomalyDetectorArn,
    anomalyDetectorSummary_anomalyDetectorDescription,
    anomalyDetectorSummary_anomalyDetectorName,
    anomalyDetectorSummary_creationTime,
    anomalyDetectorSummary_lastModificationTime,
    anomalyDetectorSummary_status,
    anomalyDetectorSummary_tags,

    -- * AnomalyGroup
    AnomalyGroup (..),
    newAnomalyGroup,
    anomalyGroup_anomalyGroupId,
    anomalyGroup_anomalyGroupScore,
    anomalyGroup_endTime,
    anomalyGroup_metricLevelImpactList,
    anomalyGroup_primaryMetricName,
    anomalyGroup_startTime,

    -- * AnomalyGroupStatistics
    AnomalyGroupStatistics (..),
    newAnomalyGroupStatistics,
    anomalyGroupStatistics_evaluationStartDate,
    anomalyGroupStatistics_itemizedMetricStatsList,
    anomalyGroupStatistics_totalCount,

    -- * AnomalyGroupSummary
    AnomalyGroupSummary (..),
    newAnomalyGroupSummary,
    anomalyGroupSummary_anomalyGroupId,
    anomalyGroupSummary_anomalyGroupScore,
    anomalyGroupSummary_endTime,
    anomalyGroupSummary_primaryMetricName,
    anomalyGroupSummary_startTime,

    -- * AnomalyGroupTimeSeries
    AnomalyGroupTimeSeries (..),
    newAnomalyGroupTimeSeries,
    anomalyGroupTimeSeries_timeSeriesId,
    anomalyGroupTimeSeries_anomalyGroupId,

    -- * AnomalyGroupTimeSeriesFeedback
    AnomalyGroupTimeSeriesFeedback (..),
    newAnomalyGroupTimeSeriesFeedback,
    anomalyGroupTimeSeriesFeedback_anomalyGroupId,
    anomalyGroupTimeSeriesFeedback_timeSeriesId,
    anomalyGroupTimeSeriesFeedback_isAnomaly,

    -- * AppFlowConfig
    AppFlowConfig (..),
    newAppFlowConfig,
    appFlowConfig_flowName,
    appFlowConfig_roleArn,

    -- * AthenaSourceConfig
    AthenaSourceConfig (..),
    newAthenaSourceConfig,
    athenaSourceConfig_backTestConfiguration,
    athenaSourceConfig_dataCatalog,
    athenaSourceConfig_databaseName,
    athenaSourceConfig_roleArn,
    athenaSourceConfig_s3ResultsPath,
    athenaSourceConfig_tableName,
    athenaSourceConfig_workGroupName,

    -- * AttributeValue
    AttributeValue (..),
    newAttributeValue,
    attributeValue_b,
    attributeValue_bs,
    attributeValue_n,
    attributeValue_ns,
    attributeValue_s,
    attributeValue_ss,

    -- * AutoDetectionMetricSource
    AutoDetectionMetricSource (..),
    newAutoDetectionMetricSource,
    autoDetectionMetricSource_s3SourceConfig,

    -- * AutoDetectionS3SourceConfig
    AutoDetectionS3SourceConfig (..),
    newAutoDetectionS3SourceConfig,
    autoDetectionS3SourceConfig_historicalDataPathList,
    autoDetectionS3SourceConfig_templatedPathList,

    -- * BackTestConfiguration
    BackTestConfiguration (..),
    newBackTestConfiguration,
    backTestConfiguration_runBackTestMode,

    -- * CloudWatchConfig
    CloudWatchConfig (..),
    newCloudWatchConfig,
    cloudWatchConfig_backTestConfiguration,
    cloudWatchConfig_roleArn,

    -- * ContributionMatrix
    ContributionMatrix (..),
    newContributionMatrix,
    contributionMatrix_dimensionContributionList,

    -- * CsvFormatDescriptor
    CsvFormatDescriptor (..),
    newCsvFormatDescriptor,
    csvFormatDescriptor_charset,
    csvFormatDescriptor_containsHeader,
    csvFormatDescriptor_delimiter,
    csvFormatDescriptor_fileCompression,
    csvFormatDescriptor_headerList,
    csvFormatDescriptor_quoteSymbol,

    -- * DataQualityMetric
    DataQualityMetric (..),
    newDataQualityMetric,
    dataQualityMetric_metricDescription,
    dataQualityMetric_metricType,
    dataQualityMetric_metricValue,
    dataQualityMetric_relatedColumnName,

    -- * DetectedCsvFormatDescriptor
    DetectedCsvFormatDescriptor (..),
    newDetectedCsvFormatDescriptor,
    detectedCsvFormatDescriptor_charset,
    detectedCsvFormatDescriptor_containsHeader,
    detectedCsvFormatDescriptor_delimiter,
    detectedCsvFormatDescriptor_fileCompression,
    detectedCsvFormatDescriptor_headerList,
    detectedCsvFormatDescriptor_quoteSymbol,

    -- * DetectedField
    DetectedField (..),
    newDetectedField,
    detectedField_confidence,
    detectedField_message,
    detectedField_value,

    -- * DetectedFileFormatDescriptor
    DetectedFileFormatDescriptor (..),
    newDetectedFileFormatDescriptor,
    detectedFileFormatDescriptor_csvFormatDescriptor,
    detectedFileFormatDescriptor_jsonFormatDescriptor,

    -- * DetectedJsonFormatDescriptor
    DetectedJsonFormatDescriptor (..),
    newDetectedJsonFormatDescriptor,
    detectedJsonFormatDescriptor_charset,
    detectedJsonFormatDescriptor_fileCompression,

    -- * DetectedMetricSetConfig
    DetectedMetricSetConfig (..),
    newDetectedMetricSetConfig,
    detectedMetricSetConfig_metricSetFrequency,
    detectedMetricSetConfig_metricSource,
    detectedMetricSetConfig_offset,

    -- * DetectedMetricSource
    DetectedMetricSource (..),
    newDetectedMetricSource,
    detectedMetricSource_s3SourceConfig,

    -- * DetectedS3SourceConfig
    DetectedS3SourceConfig (..),
    newDetectedS3SourceConfig,
    detectedS3SourceConfig_fileFormatDescriptor,

    -- * DimensionContribution
    DimensionContribution (..),
    newDimensionContribution,
    dimensionContribution_dimensionName,
    dimensionContribution_dimensionValueContributionList,

    -- * DimensionFilter
    DimensionFilter (..),
    newDimensionFilter,
    dimensionFilter_dimensionName,
    dimensionFilter_dimensionValueList,

    -- * DimensionNameValue
    DimensionNameValue (..),
    newDimensionNameValue,
    dimensionNameValue_dimensionName,
    dimensionNameValue_dimensionValue,

    -- * DimensionValueContribution
    DimensionValueContribution (..),
    newDimensionValueContribution,
    dimensionValueContribution_contributionScore,
    dimensionValueContribution_dimensionValue,

    -- * ExecutionStatus
    ExecutionStatus (..),
    newExecutionStatus,
    executionStatus_failureReason,
    executionStatus_status,
    executionStatus_timestamp,

    -- * FileFormatDescriptor
    FileFormatDescriptor (..),
    newFileFormatDescriptor,
    fileFormatDescriptor_csvFormatDescriptor,
    fileFormatDescriptor_jsonFormatDescriptor,

    -- * Filter
    Filter (..),
    newFilter,
    filter_dimensionValue,
    filter_filterOperation,

    -- * InterMetricImpactDetails
    InterMetricImpactDetails (..),
    newInterMetricImpactDetails,
    interMetricImpactDetails_anomalyGroupId,
    interMetricImpactDetails_contributionPercentage,
    interMetricImpactDetails_metricName,
    interMetricImpactDetails_relationshipType,

    -- * ItemizedMetricStats
    ItemizedMetricStats (..),
    newItemizedMetricStats,
    itemizedMetricStats_metricName,
    itemizedMetricStats_occurrenceCount,

    -- * JsonFormatDescriptor
    JsonFormatDescriptor (..),
    newJsonFormatDescriptor,
    jsonFormatDescriptor_charset,
    jsonFormatDescriptor_fileCompression,

    -- * LambdaConfiguration
    LambdaConfiguration (..),
    newLambdaConfiguration,
    lambdaConfiguration_roleArn,
    lambdaConfiguration_lambdaArn,

    -- * Metric
    Metric (..),
    newMetric,
    metric_namespace,
    metric_metricName,
    metric_aggregationFunction,

    -- * MetricLevelImpact
    MetricLevelImpact (..),
    newMetricLevelImpact,
    metricLevelImpact_contributionMatrix,
    metricLevelImpact_metricName,
    metricLevelImpact_numTimeSeries,

    -- * MetricSetDataQualityMetric
    MetricSetDataQualityMetric (..),
    newMetricSetDataQualityMetric,
    metricSetDataQualityMetric_dataQualityMetricList,
    metricSetDataQualityMetric_metricSetArn,

    -- * MetricSetDimensionFilter
    MetricSetDimensionFilter (..),
    newMetricSetDimensionFilter,
    metricSetDimensionFilter_filterList,
    metricSetDimensionFilter_name,

    -- * MetricSetSummary
    MetricSetSummary (..),
    newMetricSetSummary,
    metricSetSummary_anomalyDetectorArn,
    metricSetSummary_creationTime,
    metricSetSummary_lastModificationTime,
    metricSetSummary_metricSetArn,
    metricSetSummary_metricSetDescription,
    metricSetSummary_metricSetName,
    metricSetSummary_tags,

    -- * MetricSource
    MetricSource (..),
    newMetricSource,
    metricSource_appFlowConfig,
    metricSource_athenaSourceConfig,
    metricSource_cloudWatchConfig,
    metricSource_rDSSourceConfig,
    metricSource_redshiftSourceConfig,
    metricSource_s3SourceConfig,

    -- * RDSSourceConfig
    RDSSourceConfig (..),
    newRDSSourceConfig,
    rDSSourceConfig_dbInstanceIdentifier,
    rDSSourceConfig_databaseHost,
    rDSSourceConfig_databaseName,
    rDSSourceConfig_databasePort,
    rDSSourceConfig_roleArn,
    rDSSourceConfig_secretManagerArn,
    rDSSourceConfig_tableName,
    rDSSourceConfig_vpcConfiguration,

    -- * RedshiftSourceConfig
    RedshiftSourceConfig (..),
    newRedshiftSourceConfig,
    redshiftSourceConfig_clusterIdentifier,
    redshiftSourceConfig_databaseHost,
    redshiftSourceConfig_databaseName,
    redshiftSourceConfig_databasePort,
    redshiftSourceConfig_roleArn,
    redshiftSourceConfig_secretManagerArn,
    redshiftSourceConfig_tableName,
    redshiftSourceConfig_vpcConfiguration,

    -- * S3SourceConfig
    S3SourceConfig (..),
    newS3SourceConfig,
    s3SourceConfig_fileFormatDescriptor,
    s3SourceConfig_historicalDataPathList,
    s3SourceConfig_roleArn,
    s3SourceConfig_templatedPathList,

    -- * SNSConfiguration
    SNSConfiguration (..),
    newSNSConfiguration,
    sNSConfiguration_snsFormat,
    sNSConfiguration_roleArn,
    sNSConfiguration_snsTopicArn,

    -- * SampleDataS3SourceConfig
    SampleDataS3SourceConfig (..),
    newSampleDataS3SourceConfig,
    sampleDataS3SourceConfig_historicalDataPathList,
    sampleDataS3SourceConfig_templatedPathList,
    sampleDataS3SourceConfig_roleArn,
    sampleDataS3SourceConfig_fileFormatDescriptor,

    -- * TimeSeries
    TimeSeries (..),
    newTimeSeries,
    timeSeries_timeSeriesId,
    timeSeries_dimensionList,
    timeSeries_metricValueList,

    -- * TimeSeriesFeedback
    TimeSeriesFeedback (..),
    newTimeSeriesFeedback,
    timeSeriesFeedback_isAnomaly,
    timeSeriesFeedback_timeSeriesId,

    -- * TimestampColumn
    TimestampColumn (..),
    newTimestampColumn,
    timestampColumn_columnFormat,
    timestampColumn_columnName,

    -- * VpcConfiguration
    VpcConfiguration (..),
    newVpcConfiguration,
    vpcConfiguration_subnetIdList,
    vpcConfiguration_securityGroupIdList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.Action
import Amazonka.LookoutMetrics.Types.AggregationFunction
import Amazonka.LookoutMetrics.Types.Alert
import Amazonka.LookoutMetrics.Types.AlertFilters
import Amazonka.LookoutMetrics.Types.AlertStatus
import Amazonka.LookoutMetrics.Types.AlertSummary
import Amazonka.LookoutMetrics.Types.AlertType
import Amazonka.LookoutMetrics.Types.AnomalyDetectionTaskStatus
import Amazonka.LookoutMetrics.Types.AnomalyDetectorConfig
import Amazonka.LookoutMetrics.Types.AnomalyDetectorConfigSummary
import Amazonka.LookoutMetrics.Types.AnomalyDetectorDataQualityMetric
import Amazonka.LookoutMetrics.Types.AnomalyDetectorFailureType
import Amazonka.LookoutMetrics.Types.AnomalyDetectorStatus
import Amazonka.LookoutMetrics.Types.AnomalyDetectorSummary
import Amazonka.LookoutMetrics.Types.AnomalyGroup
import Amazonka.LookoutMetrics.Types.AnomalyGroupStatistics
import Amazonka.LookoutMetrics.Types.AnomalyGroupSummary
import Amazonka.LookoutMetrics.Types.AnomalyGroupTimeSeries
import Amazonka.LookoutMetrics.Types.AnomalyGroupTimeSeriesFeedback
import Amazonka.LookoutMetrics.Types.AppFlowConfig
import Amazonka.LookoutMetrics.Types.AthenaSourceConfig
import Amazonka.LookoutMetrics.Types.AttributeValue
import Amazonka.LookoutMetrics.Types.AutoDetectionMetricSource
import Amazonka.LookoutMetrics.Types.AutoDetectionS3SourceConfig
import Amazonka.LookoutMetrics.Types.BackTestConfiguration
import Amazonka.LookoutMetrics.Types.CSVFileCompression
import Amazonka.LookoutMetrics.Types.CloudWatchConfig
import Amazonka.LookoutMetrics.Types.Confidence
import Amazonka.LookoutMetrics.Types.ContributionMatrix
import Amazonka.LookoutMetrics.Types.CsvFormatDescriptor
import Amazonka.LookoutMetrics.Types.DataQualityMetric
import Amazonka.LookoutMetrics.Types.DataQualityMetricType
import Amazonka.LookoutMetrics.Types.DetectedCsvFormatDescriptor
import Amazonka.LookoutMetrics.Types.DetectedField
import Amazonka.LookoutMetrics.Types.DetectedFileFormatDescriptor
import Amazonka.LookoutMetrics.Types.DetectedJsonFormatDescriptor
import Amazonka.LookoutMetrics.Types.DetectedMetricSetConfig
import Amazonka.LookoutMetrics.Types.DetectedMetricSource
import Amazonka.LookoutMetrics.Types.DetectedS3SourceConfig
import Amazonka.LookoutMetrics.Types.DimensionContribution
import Amazonka.LookoutMetrics.Types.DimensionFilter
import Amazonka.LookoutMetrics.Types.DimensionNameValue
import Amazonka.LookoutMetrics.Types.DimensionValueContribution
import Amazonka.LookoutMetrics.Types.ExecutionStatus
import Amazonka.LookoutMetrics.Types.FileFormatDescriptor
import Amazonka.LookoutMetrics.Types.Filter
import Amazonka.LookoutMetrics.Types.FilterOperation
import Amazonka.LookoutMetrics.Types.Frequency
import Amazonka.LookoutMetrics.Types.InterMetricImpactDetails
import Amazonka.LookoutMetrics.Types.ItemizedMetricStats
import Amazonka.LookoutMetrics.Types.JsonFileCompression
import Amazonka.LookoutMetrics.Types.JsonFormatDescriptor
import Amazonka.LookoutMetrics.Types.LambdaConfiguration
import Amazonka.LookoutMetrics.Types.Metric
import Amazonka.LookoutMetrics.Types.MetricLevelImpact
import Amazonka.LookoutMetrics.Types.MetricSetDataQualityMetric
import Amazonka.LookoutMetrics.Types.MetricSetDimensionFilter
import Amazonka.LookoutMetrics.Types.MetricSetSummary
import Amazonka.LookoutMetrics.Types.MetricSource
import Amazonka.LookoutMetrics.Types.RDSSourceConfig
import Amazonka.LookoutMetrics.Types.RedshiftSourceConfig
import Amazonka.LookoutMetrics.Types.RelationshipType
import Amazonka.LookoutMetrics.Types.S3SourceConfig
import Amazonka.LookoutMetrics.Types.SNSConfiguration
import Amazonka.LookoutMetrics.Types.SampleDataS3SourceConfig
import Amazonka.LookoutMetrics.Types.SnsFormat
import Amazonka.LookoutMetrics.Types.TimeSeries
import Amazonka.LookoutMetrics.Types.TimeSeriesFeedback
import Amazonka.LookoutMetrics.Types.TimestampColumn
import Amazonka.LookoutMetrics.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon Lookout for Metrics SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "LookoutMetrics",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "lookoutmetrics",
      Core.signingName = "lookoutmetrics",
      Core.version = "2017-07-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "LookoutMetrics",
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

-- | You do not have sufficient permissions to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | There was a conflict processing the request. Try your request again.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request processing has failed because of an unknown error,
-- exception, or failure.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource cannot be found. Check the ARN of the resource
-- and try again.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 400

-- | The request exceeded the service\'s quotas. Check the service quotas and
-- try again.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to too many requests being submitted at the
-- same time.
_TooManyRequestsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by the AWS service.
-- Check your input values and try again.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

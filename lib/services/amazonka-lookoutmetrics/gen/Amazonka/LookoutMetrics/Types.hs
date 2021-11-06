{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutMetrics.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _TooManyRequestsException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AggregationFunction
    AggregationFunction (..),

    -- * AlertStatus
    AlertStatus (..),

    -- * AlertType
    AlertType (..),

    -- * AnomalyDetectionTaskStatus
    AnomalyDetectionTaskStatus (..),

    -- * AnomalyDetectorStatus
    AnomalyDetectorStatus (..),

    -- * CSVFileCompression
    CSVFileCompression (..),

    -- * Frequency
    Frequency (..),

    -- * JsonFileCompression
    JsonFileCompression (..),

    -- * Action
    Action (..),
    newAction,
    action_lambdaConfiguration,
    action_sNSConfiguration,

    -- * Alert
    Alert (..),
    newAlert,
    alert_creationTime,
    alert_action,
    alert_anomalyDetectorArn,
    alert_alertName,
    alert_alertSensitivityThreshold,
    alert_alertStatus,
    alert_alertDescription,
    alert_alertArn,
    alert_alertType,
    alert_lastModificationTime,

    -- * AlertSummary
    AlertSummary (..),
    newAlertSummary,
    alertSummary_creationTime,
    alertSummary_anomalyDetectorArn,
    alertSummary_alertName,
    alertSummary_alertSensitivityThreshold,
    alertSummary_alertStatus,
    alertSummary_alertArn,
    alertSummary_alertType,
    alertSummary_tags,
    alertSummary_lastModificationTime,

    -- * AnomalyDetectorConfig
    AnomalyDetectorConfig (..),
    newAnomalyDetectorConfig,
    anomalyDetectorConfig_anomalyDetectorFrequency,

    -- * AnomalyDetectorConfigSummary
    AnomalyDetectorConfigSummary (..),
    newAnomalyDetectorConfigSummary,
    anomalyDetectorConfigSummary_anomalyDetectorFrequency,

    -- * AnomalyDetectorSummary
    AnomalyDetectorSummary (..),
    newAnomalyDetectorSummary,
    anomalyDetectorSummary_creationTime,
    anomalyDetectorSummary_status,
    anomalyDetectorSummary_anomalyDetectorArn,
    anomalyDetectorSummary_anomalyDetectorName,
    anomalyDetectorSummary_anomalyDetectorDescription,
    anomalyDetectorSummary_tags,
    anomalyDetectorSummary_lastModificationTime,

    -- * AnomalyGroup
    AnomalyGroup (..),
    newAnomalyGroup,
    anomalyGroup_metricLevelImpactList,
    anomalyGroup_startTime,
    anomalyGroup_anomalyGroupId,
    anomalyGroup_anomalyGroupScore,
    anomalyGroup_primaryMetricName,
    anomalyGroup_endTime,

    -- * AnomalyGroupStatistics
    AnomalyGroupStatistics (..),
    newAnomalyGroupStatistics,
    anomalyGroupStatistics_itemizedMetricStatsList,
    anomalyGroupStatistics_evaluationStartDate,
    anomalyGroupStatistics_totalCount,

    -- * AnomalyGroupSummary
    AnomalyGroupSummary (..),
    newAnomalyGroupSummary,
    anomalyGroupSummary_startTime,
    anomalyGroupSummary_anomalyGroupId,
    anomalyGroupSummary_anomalyGroupScore,
    anomalyGroupSummary_primaryMetricName,
    anomalyGroupSummary_endTime,

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
    appFlowConfig_roleArn,
    appFlowConfig_flowName,

    -- * CloudWatchConfig
    CloudWatchConfig (..),
    newCloudWatchConfig,
    cloudWatchConfig_roleArn,

    -- * ContributionMatrix
    ContributionMatrix (..),
    newContributionMatrix,
    contributionMatrix_dimensionContributionList,

    -- * CsvFormatDescriptor
    CsvFormatDescriptor (..),
    newCsvFormatDescriptor,
    csvFormatDescriptor_quoteSymbol,
    csvFormatDescriptor_containsHeader,
    csvFormatDescriptor_charset,
    csvFormatDescriptor_headerList,
    csvFormatDescriptor_fileCompression,
    csvFormatDescriptor_delimiter,

    -- * DimensionContribution
    DimensionContribution (..),
    newDimensionContribution,
    dimensionContribution_dimensionValueContributionList,
    dimensionContribution_dimensionName,

    -- * DimensionNameValue
    DimensionNameValue (..),
    newDimensionNameValue,
    dimensionNameValue_dimensionName,
    dimensionNameValue_dimensionValue,

    -- * DimensionValueContribution
    DimensionValueContribution (..),
    newDimensionValueContribution,
    dimensionValueContribution_dimensionValue,
    dimensionValueContribution_contributionScore,

    -- * ExecutionStatus
    ExecutionStatus (..),
    newExecutionStatus,
    executionStatus_status,
    executionStatus_failureReason,
    executionStatus_timestamp,

    -- * FileFormatDescriptor
    FileFormatDescriptor (..),
    newFileFormatDescriptor,
    fileFormatDescriptor_jsonFormatDescriptor,
    fileFormatDescriptor_csvFormatDescriptor,

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

    -- * MetricSetSummary
    MetricSetSummary (..),
    newMetricSetSummary,
    metricSetSummary_creationTime,
    metricSetSummary_anomalyDetectorArn,
    metricSetSummary_metricSetName,
    metricSetSummary_metricSetDescription,
    metricSetSummary_metricSetArn,
    metricSetSummary_tags,
    metricSetSummary_lastModificationTime,

    -- * MetricSource
    MetricSource (..),
    newMetricSource,
    metricSource_redshiftSourceConfig,
    metricSource_s3SourceConfig,
    metricSource_rDSSourceConfig,
    metricSource_appFlowConfig,
    metricSource_cloudWatchConfig,

    -- * RDSSourceConfig
    RDSSourceConfig (..),
    newRDSSourceConfig,
    rDSSourceConfig_dbInstanceIdentifier,
    rDSSourceConfig_databaseHost,
    rDSSourceConfig_databasePort,
    rDSSourceConfig_secretManagerArn,
    rDSSourceConfig_databaseName,
    rDSSourceConfig_tableName,
    rDSSourceConfig_roleArn,
    rDSSourceConfig_vpcConfiguration,

    -- * RedshiftSourceConfig
    RedshiftSourceConfig (..),
    newRedshiftSourceConfig,
    redshiftSourceConfig_clusterIdentifier,
    redshiftSourceConfig_databaseHost,
    redshiftSourceConfig_databasePort,
    redshiftSourceConfig_secretManagerArn,
    redshiftSourceConfig_databaseName,
    redshiftSourceConfig_tableName,
    redshiftSourceConfig_roleArn,
    redshiftSourceConfig_vpcConfiguration,

    -- * S3SourceConfig
    S3SourceConfig (..),
    newS3SourceConfig,
    s3SourceConfig_templatedPathList,
    s3SourceConfig_historicalDataPathList,
    s3SourceConfig_fileFormatDescriptor,
    s3SourceConfig_roleArn,

    -- * SNSConfiguration
    SNSConfiguration (..),
    newSNSConfiguration,
    sNSConfiguration_roleArn,
    sNSConfiguration_snsTopicArn,

    -- * SampleDataS3SourceConfig
    SampleDataS3SourceConfig (..),
    newSampleDataS3SourceConfig,
    sampleDataS3SourceConfig_templatedPathList,
    sampleDataS3SourceConfig_historicalDataPathList,
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
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types.Action
import Amazonka.LookoutMetrics.Types.AggregationFunction
import Amazonka.LookoutMetrics.Types.Alert
import Amazonka.LookoutMetrics.Types.AlertStatus
import Amazonka.LookoutMetrics.Types.AlertSummary
import Amazonka.LookoutMetrics.Types.AlertType
import Amazonka.LookoutMetrics.Types.AnomalyDetectionTaskStatus
import Amazonka.LookoutMetrics.Types.AnomalyDetectorConfig
import Amazonka.LookoutMetrics.Types.AnomalyDetectorConfigSummary
import Amazonka.LookoutMetrics.Types.AnomalyDetectorStatus
import Amazonka.LookoutMetrics.Types.AnomalyDetectorSummary
import Amazonka.LookoutMetrics.Types.AnomalyGroup
import Amazonka.LookoutMetrics.Types.AnomalyGroupStatistics
import Amazonka.LookoutMetrics.Types.AnomalyGroupSummary
import Amazonka.LookoutMetrics.Types.AnomalyGroupTimeSeries
import Amazonka.LookoutMetrics.Types.AnomalyGroupTimeSeriesFeedback
import Amazonka.LookoutMetrics.Types.AppFlowConfig
import Amazonka.LookoutMetrics.Types.CSVFileCompression
import Amazonka.LookoutMetrics.Types.CloudWatchConfig
import Amazonka.LookoutMetrics.Types.ContributionMatrix
import Amazonka.LookoutMetrics.Types.CsvFormatDescriptor
import Amazonka.LookoutMetrics.Types.DimensionContribution
import Amazonka.LookoutMetrics.Types.DimensionNameValue
import Amazonka.LookoutMetrics.Types.DimensionValueContribution
import Amazonka.LookoutMetrics.Types.ExecutionStatus
import Amazonka.LookoutMetrics.Types.FileFormatDescriptor
import Amazonka.LookoutMetrics.Types.Frequency
import Amazonka.LookoutMetrics.Types.ItemizedMetricStats
import Amazonka.LookoutMetrics.Types.JsonFileCompression
import Amazonka.LookoutMetrics.Types.JsonFormatDescriptor
import Amazonka.LookoutMetrics.Types.LambdaConfiguration
import Amazonka.LookoutMetrics.Types.Metric
import Amazonka.LookoutMetrics.Types.MetricLevelImpact
import Amazonka.LookoutMetrics.Types.MetricSetSummary
import Amazonka.LookoutMetrics.Types.MetricSource
import Amazonka.LookoutMetrics.Types.RDSSourceConfig
import Amazonka.LookoutMetrics.Types.RedshiftSourceConfig
import Amazonka.LookoutMetrics.Types.S3SourceConfig
import Amazonka.LookoutMetrics.Types.SNSConfiguration
import Amazonka.LookoutMetrics.Types.SampleDataS3SourceConfig
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
    { Core._serviceAbbrev =
        "LookoutMetrics",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "lookoutmetrics",
      Core._serviceSigningName = "lookoutmetrics",
      Core._serviceVersion = "2017-07-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "LookoutMetrics",
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

-- | The input fails to satisfy the constraints specified by the AWS service.
-- Check your input values and try again.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You do not have sufficient permissions to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | There was a conflict processing the request. Try your request again.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request exceeded the service\'s quotas. Check the service quotas and
-- try again.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to too many requests being submitted at the
-- same time.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The request processing has failed because of an unknown error,
-- exception, or failure.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource cannot be found. Check the ARN of the resource
-- and try again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 400

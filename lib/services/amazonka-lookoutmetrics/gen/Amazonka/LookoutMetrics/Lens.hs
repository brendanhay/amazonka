{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutMetrics.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Lens
  ( -- * Operations

    -- ** ActivateAnomalyDetector
    activateAnomalyDetector_anomalyDetectorArn,
    activateAnomalyDetectorResponse_httpStatus,

    -- ** BackTestAnomalyDetector
    backTestAnomalyDetector_anomalyDetectorArn,
    backTestAnomalyDetectorResponse_httpStatus,

    -- ** CreateAlert
    createAlert_tags,
    createAlert_alertDescription,
    createAlert_alertSensitivityThreshold,
    createAlert_alertFilters,
    createAlert_alertName,
    createAlert_anomalyDetectorArn,
    createAlert_action,
    createAlertResponse_alertArn,
    createAlertResponse_httpStatus,

    -- ** CreateAnomalyDetector
    createAnomalyDetector_tags,
    createAnomalyDetector_kmsKeyArn,
    createAnomalyDetector_anomalyDetectorDescription,
    createAnomalyDetector_anomalyDetectorName,
    createAnomalyDetector_anomalyDetectorConfig,
    createAnomalyDetectorResponse_anomalyDetectorArn,
    createAnomalyDetectorResponse_httpStatus,

    -- ** CreateMetricSet
    createMetricSet_tags,
    createMetricSet_timestampColumn,
    createMetricSet_metricSetDescription,
    createMetricSet_timezone,
    createMetricSet_offset,
    createMetricSet_dimensionList,
    createMetricSet_metricSetFrequency,
    createMetricSet_anomalyDetectorArn,
    createMetricSet_metricSetName,
    createMetricSet_metricList,
    createMetricSet_metricSource,
    createMetricSetResponse_metricSetArn,
    createMetricSetResponse_httpStatus,

    -- ** DeactivateAnomalyDetector
    deactivateAnomalyDetector_anomalyDetectorArn,
    deactivateAnomalyDetectorResponse_httpStatus,

    -- ** DeleteAlert
    deleteAlert_alertArn,
    deleteAlertResponse_httpStatus,

    -- ** DeleteAnomalyDetector
    deleteAnomalyDetector_anomalyDetectorArn,
    deleteAnomalyDetectorResponse_httpStatus,

    -- ** DescribeAlert
    describeAlert_alertArn,
    describeAlertResponse_alert,
    describeAlertResponse_httpStatus,

    -- ** DescribeAnomalyDetectionExecutions
    describeAnomalyDetectionExecutions_nextToken,
    describeAnomalyDetectionExecutions_timestamp,
    describeAnomalyDetectionExecutions_maxResults,
    describeAnomalyDetectionExecutions_anomalyDetectorArn,
    describeAnomalyDetectionExecutionsResponse_nextToken,
    describeAnomalyDetectionExecutionsResponse_executionList,
    describeAnomalyDetectionExecutionsResponse_httpStatus,

    -- ** DescribeAnomalyDetector
    describeAnomalyDetector_anomalyDetectorArn,
    describeAnomalyDetectorResponse_lastModificationTime,
    describeAnomalyDetectorResponse_anomalyDetectorArn,
    describeAnomalyDetectorResponse_failureType,
    describeAnomalyDetectorResponse_status,
    describeAnomalyDetectorResponse_kmsKeyArn,
    describeAnomalyDetectorResponse_anomalyDetectorName,
    describeAnomalyDetectorResponse_anomalyDetectorDescription,
    describeAnomalyDetectorResponse_creationTime,
    describeAnomalyDetectorResponse_anomalyDetectorConfig,
    describeAnomalyDetectorResponse_failureReason,
    describeAnomalyDetectorResponse_httpStatus,

    -- ** DescribeMetricSet
    describeMetricSet_metricSetArn,
    describeMetricSetResponse_lastModificationTime,
    describeMetricSetResponse_timestampColumn,
    describeMetricSetResponse_metricSetDescription,
    describeMetricSetResponse_anomalyDetectorArn,
    describeMetricSetResponse_metricSetName,
    describeMetricSetResponse_timezone,
    describeMetricSetResponse_offset,
    describeMetricSetResponse_metricSource,
    describeMetricSetResponse_dimensionList,
    describeMetricSetResponse_metricSetArn,
    describeMetricSetResponse_metricSetFrequency,
    describeMetricSetResponse_creationTime,
    describeMetricSetResponse_metricList,
    describeMetricSetResponse_httpStatus,

    -- ** DetectMetricSetConfig
    detectMetricSetConfig_anomalyDetectorArn,
    detectMetricSetConfig_autoDetectionMetricSource,
    detectMetricSetConfigResponse_detectedMetricSetConfig,
    detectMetricSetConfigResponse_httpStatus,

    -- ** GetAnomalyGroup
    getAnomalyGroup_anomalyGroupId,
    getAnomalyGroup_anomalyDetectorArn,
    getAnomalyGroupResponse_anomalyGroup,
    getAnomalyGroupResponse_httpStatus,

    -- ** GetDataQualityMetrics
    getDataQualityMetrics_metricSetArn,
    getDataQualityMetrics_anomalyDetectorArn,
    getDataQualityMetricsResponse_anomalyDetectorDataQualityMetricList,
    getDataQualityMetricsResponse_httpStatus,

    -- ** GetFeedback
    getFeedback_nextToken,
    getFeedback_maxResults,
    getFeedback_anomalyDetectorArn,
    getFeedback_anomalyGroupTimeSeriesFeedback,
    getFeedbackResponse_nextToken,
    getFeedbackResponse_anomalyGroupTimeSeriesFeedback,
    getFeedbackResponse_httpStatus,

    -- ** GetSampleData
    getSampleData_s3SourceConfig,
    getSampleDataResponse_sampleRows,
    getSampleDataResponse_headerValues,
    getSampleDataResponse_httpStatus,

    -- ** ListAlerts
    listAlerts_nextToken,
    listAlerts_anomalyDetectorArn,
    listAlerts_maxResults,
    listAlertsResponse_nextToken,
    listAlertsResponse_alertSummaryList,
    listAlertsResponse_httpStatus,

    -- ** ListAnomalyDetectors
    listAnomalyDetectors_nextToken,
    listAnomalyDetectors_maxResults,
    listAnomalyDetectorsResponse_nextToken,
    listAnomalyDetectorsResponse_anomalyDetectorSummaryList,
    listAnomalyDetectorsResponse_httpStatus,

    -- ** ListAnomalyGroupRelatedMetrics
    listAnomalyGroupRelatedMetrics_nextToken,
    listAnomalyGroupRelatedMetrics_maxResults,
    listAnomalyGroupRelatedMetrics_relationshipTypeFilter,
    listAnomalyGroupRelatedMetrics_anomalyDetectorArn,
    listAnomalyGroupRelatedMetrics_anomalyGroupId,
    listAnomalyGroupRelatedMetricsResponse_nextToken,
    listAnomalyGroupRelatedMetricsResponse_interMetricImpactList,
    listAnomalyGroupRelatedMetricsResponse_httpStatus,

    -- ** ListAnomalyGroupSummaries
    listAnomalyGroupSummaries_nextToken,
    listAnomalyGroupSummaries_maxResults,
    listAnomalyGroupSummaries_anomalyDetectorArn,
    listAnomalyGroupSummaries_sensitivityThreshold,
    listAnomalyGroupSummariesResponse_anomalyGroupSummaryList,
    listAnomalyGroupSummariesResponse_nextToken,
    listAnomalyGroupSummariesResponse_anomalyGroupStatistics,
    listAnomalyGroupSummariesResponse_httpStatus,

    -- ** ListAnomalyGroupTimeSeries
    listAnomalyGroupTimeSeries_nextToken,
    listAnomalyGroupTimeSeries_maxResults,
    listAnomalyGroupTimeSeries_anomalyDetectorArn,
    listAnomalyGroupTimeSeries_anomalyGroupId,
    listAnomalyGroupTimeSeries_metricName,
    listAnomalyGroupTimeSeriesResponse_nextToken,
    listAnomalyGroupTimeSeriesResponse_timeSeriesList,
    listAnomalyGroupTimeSeriesResponse_anomalyGroupId,
    listAnomalyGroupTimeSeriesResponse_metricName,
    listAnomalyGroupTimeSeriesResponse_timestampList,
    listAnomalyGroupTimeSeriesResponse_httpStatus,

    -- ** ListMetricSets
    listMetricSets_nextToken,
    listMetricSets_anomalyDetectorArn,
    listMetricSets_maxResults,
    listMetricSetsResponse_nextToken,
    listMetricSetsResponse_metricSetSummaryList,
    listMetricSetsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutFeedback
    putFeedback_anomalyDetectorArn,
    putFeedback_anomalyGroupTimeSeriesFeedback,
    putFeedbackResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAlert
    updateAlert_alertDescription,
    updateAlert_alertSensitivityThreshold,
    updateAlert_action,
    updateAlert_alertFilters,
    updateAlert_alertArn,
    updateAlertResponse_alertArn,
    updateAlertResponse_httpStatus,

    -- ** UpdateAnomalyDetector
    updateAnomalyDetector_kmsKeyArn,
    updateAnomalyDetector_anomalyDetectorDescription,
    updateAnomalyDetector_anomalyDetectorConfig,
    updateAnomalyDetector_anomalyDetectorArn,
    updateAnomalyDetectorResponse_anomalyDetectorArn,
    updateAnomalyDetectorResponse_httpStatus,

    -- ** UpdateMetricSet
    updateMetricSet_timestampColumn,
    updateMetricSet_metricSetDescription,
    updateMetricSet_offset,
    updateMetricSet_metricSource,
    updateMetricSet_dimensionList,
    updateMetricSet_metricSetFrequency,
    updateMetricSet_metricList,
    updateMetricSet_metricSetArn,
    updateMetricSetResponse_metricSetArn,
    updateMetricSetResponse_httpStatus,

    -- * Types

    -- ** Action
    action_lambdaConfiguration,
    action_sNSConfiguration,

    -- ** Alert
    alert_lastModificationTime,
    alert_anomalyDetectorArn,
    alert_alertDescription,
    alert_alertSensitivityThreshold,
    alert_action,
    alert_creationTime,
    alert_alertName,
    alert_alertArn,
    alert_alertStatus,
    alert_alertType,
    alert_alertFilters,

    -- ** AlertFilters
    alertFilters_dimensionFilterList,
    alertFilters_metricList,

    -- ** AlertSummary
    alertSummary_lastModificationTime,
    alertSummary_tags,
    alertSummary_anomalyDetectorArn,
    alertSummary_alertSensitivityThreshold,
    alertSummary_creationTime,
    alertSummary_alertName,
    alertSummary_alertArn,
    alertSummary_alertStatus,
    alertSummary_alertType,

    -- ** AnomalyDetectorConfig
    anomalyDetectorConfig_anomalyDetectorFrequency,

    -- ** AnomalyDetectorConfigSummary
    anomalyDetectorConfigSummary_anomalyDetectorFrequency,

    -- ** AnomalyDetectorDataQualityMetric
    anomalyDetectorDataQualityMetric_startTimestamp,
    anomalyDetectorDataQualityMetric_metricSetDataQualityMetricList,

    -- ** AnomalyDetectorSummary
    anomalyDetectorSummary_lastModificationTime,
    anomalyDetectorSummary_tags,
    anomalyDetectorSummary_anomalyDetectorArn,
    anomalyDetectorSummary_status,
    anomalyDetectorSummary_anomalyDetectorName,
    anomalyDetectorSummary_anomalyDetectorDescription,
    anomalyDetectorSummary_creationTime,

    -- ** AnomalyGroup
    anomalyGroup_anomalyGroupScore,
    anomalyGroup_metricLevelImpactList,
    anomalyGroup_endTime,
    anomalyGroup_anomalyGroupId,
    anomalyGroup_startTime,
    anomalyGroup_primaryMetricName,

    -- ** AnomalyGroupStatistics
    anomalyGroupStatistics_evaluationStartDate,
    anomalyGroupStatistics_itemizedMetricStatsList,
    anomalyGroupStatistics_totalCount,

    -- ** AnomalyGroupSummary
    anomalyGroupSummary_anomalyGroupScore,
    anomalyGroupSummary_endTime,
    anomalyGroupSummary_anomalyGroupId,
    anomalyGroupSummary_startTime,
    anomalyGroupSummary_primaryMetricName,

    -- ** AnomalyGroupTimeSeries
    anomalyGroupTimeSeries_timeSeriesId,
    anomalyGroupTimeSeries_anomalyGroupId,

    -- ** AnomalyGroupTimeSeriesFeedback
    anomalyGroupTimeSeriesFeedback_anomalyGroupId,
    anomalyGroupTimeSeriesFeedback_timeSeriesId,
    anomalyGroupTimeSeriesFeedback_isAnomaly,

    -- ** AppFlowConfig
    appFlowConfig_roleArn,
    appFlowConfig_flowName,

    -- ** AthenaSourceConfig
    athenaSourceConfig_tableName,
    athenaSourceConfig_roleArn,
    athenaSourceConfig_dataCatalog,
    athenaSourceConfig_databaseName,
    athenaSourceConfig_backTestConfiguration,
    athenaSourceConfig_s3ResultsPath,
    athenaSourceConfig_workGroupName,

    -- ** AttributeValue
    attributeValue_ss,
    attributeValue_ns,
    attributeValue_b,
    attributeValue_s,
    attributeValue_bs,
    attributeValue_n,

    -- ** AutoDetectionMetricSource
    autoDetectionMetricSource_s3SourceConfig,

    -- ** AutoDetectionS3SourceConfig
    autoDetectionS3SourceConfig_templatedPathList,
    autoDetectionS3SourceConfig_historicalDataPathList,

    -- ** BackTestConfiguration
    backTestConfiguration_runBackTestMode,

    -- ** CloudWatchConfig
    cloudWatchConfig_roleArn,
    cloudWatchConfig_backTestConfiguration,

    -- ** ContributionMatrix
    contributionMatrix_dimensionContributionList,

    -- ** CsvFormatDescriptor
    csvFormatDescriptor_quoteSymbol,
    csvFormatDescriptor_containsHeader,
    csvFormatDescriptor_delimiter,
    csvFormatDescriptor_headerList,
    csvFormatDescriptor_fileCompression,
    csvFormatDescriptor_charset,

    -- ** DataQualityMetric
    dataQualityMetric_metricValue,
    dataQualityMetric_relatedColumnName,
    dataQualityMetric_metricType,
    dataQualityMetric_metricDescription,

    -- ** DetectedCsvFormatDescriptor
    detectedCsvFormatDescriptor_quoteSymbol,
    detectedCsvFormatDescriptor_containsHeader,
    detectedCsvFormatDescriptor_delimiter,
    detectedCsvFormatDescriptor_headerList,
    detectedCsvFormatDescriptor_fileCompression,
    detectedCsvFormatDescriptor_charset,

    -- ** DetectedField
    detectedField_message,
    detectedField_confidence,
    detectedField_value,

    -- ** DetectedFileFormatDescriptor
    detectedFileFormatDescriptor_jsonFormatDescriptor,
    detectedFileFormatDescriptor_csvFormatDescriptor,

    -- ** DetectedJsonFormatDescriptor
    detectedJsonFormatDescriptor_fileCompression,
    detectedJsonFormatDescriptor_charset,

    -- ** DetectedMetricSetConfig
    detectedMetricSetConfig_offset,
    detectedMetricSetConfig_metricSource,
    detectedMetricSetConfig_metricSetFrequency,

    -- ** DetectedMetricSource
    detectedMetricSource_s3SourceConfig,

    -- ** DetectedS3SourceConfig
    detectedS3SourceConfig_fileFormatDescriptor,

    -- ** DimensionContribution
    dimensionContribution_dimensionName,
    dimensionContribution_dimensionValueContributionList,

    -- ** DimensionFilter
    dimensionFilter_dimensionValueList,
    dimensionFilter_dimensionName,

    -- ** DimensionNameValue
    dimensionNameValue_dimensionName,
    dimensionNameValue_dimensionValue,

    -- ** DimensionValueContribution
    dimensionValueContribution_dimensionValue,
    dimensionValueContribution_contributionScore,

    -- ** ExecutionStatus
    executionStatus_timestamp,
    executionStatus_status,
    executionStatus_failureReason,

    -- ** FileFormatDescriptor
    fileFormatDescriptor_jsonFormatDescriptor,
    fileFormatDescriptor_csvFormatDescriptor,

    -- ** InterMetricImpactDetails
    interMetricImpactDetails_anomalyGroupId,
    interMetricImpactDetails_relationshipType,
    interMetricImpactDetails_metricName,
    interMetricImpactDetails_contributionPercentage,

    -- ** ItemizedMetricStats
    itemizedMetricStats_occurrenceCount,
    itemizedMetricStats_metricName,

    -- ** JsonFormatDescriptor
    jsonFormatDescriptor_fileCompression,
    jsonFormatDescriptor_charset,

    -- ** LambdaConfiguration
    lambdaConfiguration_roleArn,
    lambdaConfiguration_lambdaArn,

    -- ** Metric
    metric_namespace,
    metric_metricName,
    metric_aggregationFunction,

    -- ** MetricLevelImpact
    metricLevelImpact_contributionMatrix,
    metricLevelImpact_numTimeSeries,
    metricLevelImpact_metricName,

    -- ** MetricSetDataQualityMetric
    metricSetDataQualityMetric_dataQualityMetricList,
    metricSetDataQualityMetric_metricSetArn,

    -- ** MetricSetSummary
    metricSetSummary_lastModificationTime,
    metricSetSummary_tags,
    metricSetSummary_metricSetDescription,
    metricSetSummary_anomalyDetectorArn,
    metricSetSummary_metricSetName,
    metricSetSummary_metricSetArn,
    metricSetSummary_creationTime,

    -- ** MetricSource
    metricSource_s3SourceConfig,
    metricSource_cloudWatchConfig,
    metricSource_appFlowConfig,
    metricSource_athenaSourceConfig,
    metricSource_rDSSourceConfig,
    metricSource_redshiftSourceConfig,

    -- ** RDSSourceConfig
    rDSSourceConfig_vpcConfiguration,
    rDSSourceConfig_tableName,
    rDSSourceConfig_roleArn,
    rDSSourceConfig_dbInstanceIdentifier,
    rDSSourceConfig_databaseName,
    rDSSourceConfig_databaseHost,
    rDSSourceConfig_databasePort,
    rDSSourceConfig_secretManagerArn,

    -- ** RedshiftSourceConfig
    redshiftSourceConfig_vpcConfiguration,
    redshiftSourceConfig_tableName,
    redshiftSourceConfig_clusterIdentifier,
    redshiftSourceConfig_roleArn,
    redshiftSourceConfig_databaseName,
    redshiftSourceConfig_databaseHost,
    redshiftSourceConfig_databasePort,
    redshiftSourceConfig_secretManagerArn,

    -- ** S3SourceConfig
    s3SourceConfig_roleArn,
    s3SourceConfig_fileFormatDescriptor,
    s3SourceConfig_templatedPathList,
    s3SourceConfig_historicalDataPathList,

    -- ** SNSConfiguration
    sNSConfiguration_snsFormat,
    sNSConfiguration_roleArn,
    sNSConfiguration_snsTopicArn,

    -- ** SampleDataS3SourceConfig
    sampleDataS3SourceConfig_templatedPathList,
    sampleDataS3SourceConfig_historicalDataPathList,
    sampleDataS3SourceConfig_roleArn,
    sampleDataS3SourceConfig_fileFormatDescriptor,

    -- ** TimeSeries
    timeSeries_timeSeriesId,
    timeSeries_dimensionList,
    timeSeries_metricValueList,

    -- ** TimeSeriesFeedback
    timeSeriesFeedback_isAnomaly,
    timeSeriesFeedback_timeSeriesId,

    -- ** TimestampColumn
    timestampColumn_columnName,
    timestampColumn_columnFormat,

    -- ** VpcConfiguration
    vpcConfiguration_subnetIdList,
    vpcConfiguration_securityGroupIdList,
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
import Amazonka.LookoutMetrics.ListAlerts
import Amazonka.LookoutMetrics.ListAnomalyDetectors
import Amazonka.LookoutMetrics.ListAnomalyGroupRelatedMetrics
import Amazonka.LookoutMetrics.ListAnomalyGroupSummaries
import Amazonka.LookoutMetrics.ListAnomalyGroupTimeSeries
import Amazonka.LookoutMetrics.ListMetricSets
import Amazonka.LookoutMetrics.ListTagsForResource
import Amazonka.LookoutMetrics.PutFeedback
import Amazonka.LookoutMetrics.TagResource
import Amazonka.LookoutMetrics.Types.Action
import Amazonka.LookoutMetrics.Types.Alert
import Amazonka.LookoutMetrics.Types.AlertFilters
import Amazonka.LookoutMetrics.Types.AlertSummary
import Amazonka.LookoutMetrics.Types.AnomalyDetectorConfig
import Amazonka.LookoutMetrics.Types.AnomalyDetectorConfigSummary
import Amazonka.LookoutMetrics.Types.AnomalyDetectorDataQualityMetric
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
import Amazonka.LookoutMetrics.Types.CloudWatchConfig
import Amazonka.LookoutMetrics.Types.ContributionMatrix
import Amazonka.LookoutMetrics.Types.CsvFormatDescriptor
import Amazonka.LookoutMetrics.Types.DataQualityMetric
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
import Amazonka.LookoutMetrics.Types.InterMetricImpactDetails
import Amazonka.LookoutMetrics.Types.ItemizedMetricStats
import Amazonka.LookoutMetrics.Types.JsonFormatDescriptor
import Amazonka.LookoutMetrics.Types.LambdaConfiguration
import Amazonka.LookoutMetrics.Types.Metric
import Amazonka.LookoutMetrics.Types.MetricLevelImpact
import Amazonka.LookoutMetrics.Types.MetricSetDataQualityMetric
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
import Amazonka.LookoutMetrics.UntagResource
import Amazonka.LookoutMetrics.UpdateAlert
import Amazonka.LookoutMetrics.UpdateAnomalyDetector
import Amazonka.LookoutMetrics.UpdateMetricSet

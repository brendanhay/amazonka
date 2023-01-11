{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutMetrics.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createAlert_alertDescription,
    createAlert_alertFilters,
    createAlert_alertSensitivityThreshold,
    createAlert_tags,
    createAlert_alertName,
    createAlert_anomalyDetectorArn,
    createAlert_action,
    createAlertResponse_alertArn,
    createAlertResponse_httpStatus,

    -- ** CreateAnomalyDetector
    createAnomalyDetector_anomalyDetectorDescription,
    createAnomalyDetector_kmsKeyArn,
    createAnomalyDetector_tags,
    createAnomalyDetector_anomalyDetectorName,
    createAnomalyDetector_anomalyDetectorConfig,
    createAnomalyDetectorResponse_anomalyDetectorArn,
    createAnomalyDetectorResponse_httpStatus,

    -- ** CreateMetricSet
    createMetricSet_dimensionFilterList,
    createMetricSet_dimensionList,
    createMetricSet_metricSetDescription,
    createMetricSet_metricSetFrequency,
    createMetricSet_offset,
    createMetricSet_tags,
    createMetricSet_timestampColumn,
    createMetricSet_timezone,
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
    describeAnomalyDetectionExecutions_maxResults,
    describeAnomalyDetectionExecutions_nextToken,
    describeAnomalyDetectionExecutions_timestamp,
    describeAnomalyDetectionExecutions_anomalyDetectorArn,
    describeAnomalyDetectionExecutionsResponse_executionList,
    describeAnomalyDetectionExecutionsResponse_nextToken,
    describeAnomalyDetectionExecutionsResponse_httpStatus,

    -- ** DescribeAnomalyDetector
    describeAnomalyDetector_anomalyDetectorArn,
    describeAnomalyDetectorResponse_anomalyDetectorArn,
    describeAnomalyDetectorResponse_anomalyDetectorConfig,
    describeAnomalyDetectorResponse_anomalyDetectorDescription,
    describeAnomalyDetectorResponse_anomalyDetectorName,
    describeAnomalyDetectorResponse_creationTime,
    describeAnomalyDetectorResponse_failureReason,
    describeAnomalyDetectorResponse_failureType,
    describeAnomalyDetectorResponse_kmsKeyArn,
    describeAnomalyDetectorResponse_lastModificationTime,
    describeAnomalyDetectorResponse_status,
    describeAnomalyDetectorResponse_httpStatus,

    -- ** DescribeMetricSet
    describeMetricSet_metricSetArn,
    describeMetricSetResponse_anomalyDetectorArn,
    describeMetricSetResponse_creationTime,
    describeMetricSetResponse_dimensionFilterList,
    describeMetricSetResponse_dimensionList,
    describeMetricSetResponse_lastModificationTime,
    describeMetricSetResponse_metricList,
    describeMetricSetResponse_metricSetArn,
    describeMetricSetResponse_metricSetDescription,
    describeMetricSetResponse_metricSetFrequency,
    describeMetricSetResponse_metricSetName,
    describeMetricSetResponse_metricSource,
    describeMetricSetResponse_offset,
    describeMetricSetResponse_timestampColumn,
    describeMetricSetResponse_timezone,
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
    getFeedback_maxResults,
    getFeedback_nextToken,
    getFeedback_anomalyDetectorArn,
    getFeedback_anomalyGroupTimeSeriesFeedback,
    getFeedbackResponse_anomalyGroupTimeSeriesFeedback,
    getFeedbackResponse_nextToken,
    getFeedbackResponse_httpStatus,

    -- ** GetSampleData
    getSampleData_s3SourceConfig,
    getSampleDataResponse_headerValues,
    getSampleDataResponse_sampleRows,
    getSampleDataResponse_httpStatus,

    -- ** ListAlerts
    listAlerts_anomalyDetectorArn,
    listAlerts_maxResults,
    listAlerts_nextToken,
    listAlertsResponse_alertSummaryList,
    listAlertsResponse_nextToken,
    listAlertsResponse_httpStatus,

    -- ** ListAnomalyDetectors
    listAnomalyDetectors_maxResults,
    listAnomalyDetectors_nextToken,
    listAnomalyDetectorsResponse_anomalyDetectorSummaryList,
    listAnomalyDetectorsResponse_nextToken,
    listAnomalyDetectorsResponse_httpStatus,

    -- ** ListAnomalyGroupRelatedMetrics
    listAnomalyGroupRelatedMetrics_maxResults,
    listAnomalyGroupRelatedMetrics_nextToken,
    listAnomalyGroupRelatedMetrics_relationshipTypeFilter,
    listAnomalyGroupRelatedMetrics_anomalyDetectorArn,
    listAnomalyGroupRelatedMetrics_anomalyGroupId,
    listAnomalyGroupRelatedMetricsResponse_interMetricImpactList,
    listAnomalyGroupRelatedMetricsResponse_nextToken,
    listAnomalyGroupRelatedMetricsResponse_httpStatus,

    -- ** ListAnomalyGroupSummaries
    listAnomalyGroupSummaries_maxResults,
    listAnomalyGroupSummaries_nextToken,
    listAnomalyGroupSummaries_anomalyDetectorArn,
    listAnomalyGroupSummaries_sensitivityThreshold,
    listAnomalyGroupSummariesResponse_anomalyGroupStatistics,
    listAnomalyGroupSummariesResponse_anomalyGroupSummaryList,
    listAnomalyGroupSummariesResponse_nextToken,
    listAnomalyGroupSummariesResponse_httpStatus,

    -- ** ListAnomalyGroupTimeSeries
    listAnomalyGroupTimeSeries_maxResults,
    listAnomalyGroupTimeSeries_nextToken,
    listAnomalyGroupTimeSeries_anomalyDetectorArn,
    listAnomalyGroupTimeSeries_anomalyGroupId,
    listAnomalyGroupTimeSeries_metricName,
    listAnomalyGroupTimeSeriesResponse_anomalyGroupId,
    listAnomalyGroupTimeSeriesResponse_metricName,
    listAnomalyGroupTimeSeriesResponse_nextToken,
    listAnomalyGroupTimeSeriesResponse_timeSeriesList,
    listAnomalyGroupTimeSeriesResponse_timestampList,
    listAnomalyGroupTimeSeriesResponse_httpStatus,

    -- ** ListMetricSets
    listMetricSets_anomalyDetectorArn,
    listMetricSets_maxResults,
    listMetricSets_nextToken,
    listMetricSetsResponse_metricSetSummaryList,
    listMetricSetsResponse_nextToken,
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
    updateAlert_action,
    updateAlert_alertDescription,
    updateAlert_alertFilters,
    updateAlert_alertSensitivityThreshold,
    updateAlert_alertArn,
    updateAlertResponse_alertArn,
    updateAlertResponse_httpStatus,

    -- ** UpdateAnomalyDetector
    updateAnomalyDetector_anomalyDetectorConfig,
    updateAnomalyDetector_anomalyDetectorDescription,
    updateAnomalyDetector_kmsKeyArn,
    updateAnomalyDetector_anomalyDetectorArn,
    updateAnomalyDetectorResponse_anomalyDetectorArn,
    updateAnomalyDetectorResponse_httpStatus,

    -- ** UpdateMetricSet
    updateMetricSet_dimensionFilterList,
    updateMetricSet_dimensionList,
    updateMetricSet_metricList,
    updateMetricSet_metricSetDescription,
    updateMetricSet_metricSetFrequency,
    updateMetricSet_metricSource,
    updateMetricSet_offset,
    updateMetricSet_timestampColumn,
    updateMetricSet_metricSetArn,
    updateMetricSetResponse_metricSetArn,
    updateMetricSetResponse_httpStatus,

    -- * Types

    -- ** Action
    action_lambdaConfiguration,
    action_sNSConfiguration,

    -- ** Alert
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

    -- ** AlertFilters
    alertFilters_dimensionFilterList,
    alertFilters_metricList,

    -- ** AlertSummary
    alertSummary_alertArn,
    alertSummary_alertName,
    alertSummary_alertSensitivityThreshold,
    alertSummary_alertStatus,
    alertSummary_alertType,
    alertSummary_anomalyDetectorArn,
    alertSummary_creationTime,
    alertSummary_lastModificationTime,
    alertSummary_tags,

    -- ** AnomalyDetectorConfig
    anomalyDetectorConfig_anomalyDetectorFrequency,

    -- ** AnomalyDetectorConfigSummary
    anomalyDetectorConfigSummary_anomalyDetectorFrequency,

    -- ** AnomalyDetectorDataQualityMetric
    anomalyDetectorDataQualityMetric_metricSetDataQualityMetricList,
    anomalyDetectorDataQualityMetric_startTimestamp,

    -- ** AnomalyDetectorSummary
    anomalyDetectorSummary_anomalyDetectorArn,
    anomalyDetectorSummary_anomalyDetectorDescription,
    anomalyDetectorSummary_anomalyDetectorName,
    anomalyDetectorSummary_creationTime,
    anomalyDetectorSummary_lastModificationTime,
    anomalyDetectorSummary_status,
    anomalyDetectorSummary_tags,

    -- ** AnomalyGroup
    anomalyGroup_anomalyGroupId,
    anomalyGroup_anomalyGroupScore,
    anomalyGroup_endTime,
    anomalyGroup_metricLevelImpactList,
    anomalyGroup_primaryMetricName,
    anomalyGroup_startTime,

    -- ** AnomalyGroupStatistics
    anomalyGroupStatistics_evaluationStartDate,
    anomalyGroupStatistics_itemizedMetricStatsList,
    anomalyGroupStatistics_totalCount,

    -- ** AnomalyGroupSummary
    anomalyGroupSummary_anomalyGroupId,
    anomalyGroupSummary_anomalyGroupScore,
    anomalyGroupSummary_endTime,
    anomalyGroupSummary_primaryMetricName,
    anomalyGroupSummary_startTime,

    -- ** AnomalyGroupTimeSeries
    anomalyGroupTimeSeries_timeSeriesId,
    anomalyGroupTimeSeries_anomalyGroupId,

    -- ** AnomalyGroupTimeSeriesFeedback
    anomalyGroupTimeSeriesFeedback_anomalyGroupId,
    anomalyGroupTimeSeriesFeedback_timeSeriesId,
    anomalyGroupTimeSeriesFeedback_isAnomaly,

    -- ** AppFlowConfig
    appFlowConfig_flowName,
    appFlowConfig_roleArn,

    -- ** AthenaSourceConfig
    athenaSourceConfig_backTestConfiguration,
    athenaSourceConfig_dataCatalog,
    athenaSourceConfig_databaseName,
    athenaSourceConfig_roleArn,
    athenaSourceConfig_s3ResultsPath,
    athenaSourceConfig_tableName,
    athenaSourceConfig_workGroupName,

    -- ** AttributeValue
    attributeValue_b,
    attributeValue_bs,
    attributeValue_n,
    attributeValue_ns,
    attributeValue_s,
    attributeValue_ss,

    -- ** AutoDetectionMetricSource
    autoDetectionMetricSource_s3SourceConfig,

    -- ** AutoDetectionS3SourceConfig
    autoDetectionS3SourceConfig_historicalDataPathList,
    autoDetectionS3SourceConfig_templatedPathList,

    -- ** BackTestConfiguration
    backTestConfiguration_runBackTestMode,

    -- ** CloudWatchConfig
    cloudWatchConfig_backTestConfiguration,
    cloudWatchConfig_roleArn,

    -- ** ContributionMatrix
    contributionMatrix_dimensionContributionList,

    -- ** CsvFormatDescriptor
    csvFormatDescriptor_charset,
    csvFormatDescriptor_containsHeader,
    csvFormatDescriptor_delimiter,
    csvFormatDescriptor_fileCompression,
    csvFormatDescriptor_headerList,
    csvFormatDescriptor_quoteSymbol,

    -- ** DataQualityMetric
    dataQualityMetric_metricDescription,
    dataQualityMetric_metricType,
    dataQualityMetric_metricValue,
    dataQualityMetric_relatedColumnName,

    -- ** DetectedCsvFormatDescriptor
    detectedCsvFormatDescriptor_charset,
    detectedCsvFormatDescriptor_containsHeader,
    detectedCsvFormatDescriptor_delimiter,
    detectedCsvFormatDescriptor_fileCompression,
    detectedCsvFormatDescriptor_headerList,
    detectedCsvFormatDescriptor_quoteSymbol,

    -- ** DetectedField
    detectedField_confidence,
    detectedField_message,
    detectedField_value,

    -- ** DetectedFileFormatDescriptor
    detectedFileFormatDescriptor_csvFormatDescriptor,
    detectedFileFormatDescriptor_jsonFormatDescriptor,

    -- ** DetectedJsonFormatDescriptor
    detectedJsonFormatDescriptor_charset,
    detectedJsonFormatDescriptor_fileCompression,

    -- ** DetectedMetricSetConfig
    detectedMetricSetConfig_metricSetFrequency,
    detectedMetricSetConfig_metricSource,
    detectedMetricSetConfig_offset,

    -- ** DetectedMetricSource
    detectedMetricSource_s3SourceConfig,

    -- ** DetectedS3SourceConfig
    detectedS3SourceConfig_fileFormatDescriptor,

    -- ** DimensionContribution
    dimensionContribution_dimensionName,
    dimensionContribution_dimensionValueContributionList,

    -- ** DimensionFilter
    dimensionFilter_dimensionName,
    dimensionFilter_dimensionValueList,

    -- ** DimensionNameValue
    dimensionNameValue_dimensionName,
    dimensionNameValue_dimensionValue,

    -- ** DimensionValueContribution
    dimensionValueContribution_contributionScore,
    dimensionValueContribution_dimensionValue,

    -- ** ExecutionStatus
    executionStatus_failureReason,
    executionStatus_status,
    executionStatus_timestamp,

    -- ** FileFormatDescriptor
    fileFormatDescriptor_csvFormatDescriptor,
    fileFormatDescriptor_jsonFormatDescriptor,

    -- ** Filter
    filter_dimensionValue,
    filter_filterOperation,

    -- ** InterMetricImpactDetails
    interMetricImpactDetails_anomalyGroupId,
    interMetricImpactDetails_contributionPercentage,
    interMetricImpactDetails_metricName,
    interMetricImpactDetails_relationshipType,

    -- ** ItemizedMetricStats
    itemizedMetricStats_metricName,
    itemizedMetricStats_occurrenceCount,

    -- ** JsonFormatDescriptor
    jsonFormatDescriptor_charset,
    jsonFormatDescriptor_fileCompression,

    -- ** LambdaConfiguration
    lambdaConfiguration_roleArn,
    lambdaConfiguration_lambdaArn,

    -- ** Metric
    metric_namespace,
    metric_metricName,
    metric_aggregationFunction,

    -- ** MetricLevelImpact
    metricLevelImpact_contributionMatrix,
    metricLevelImpact_metricName,
    metricLevelImpact_numTimeSeries,

    -- ** MetricSetDataQualityMetric
    metricSetDataQualityMetric_dataQualityMetricList,
    metricSetDataQualityMetric_metricSetArn,

    -- ** MetricSetDimensionFilter
    metricSetDimensionFilter_filterList,
    metricSetDimensionFilter_name,

    -- ** MetricSetSummary
    metricSetSummary_anomalyDetectorArn,
    metricSetSummary_creationTime,
    metricSetSummary_lastModificationTime,
    metricSetSummary_metricSetArn,
    metricSetSummary_metricSetDescription,
    metricSetSummary_metricSetName,
    metricSetSummary_tags,

    -- ** MetricSource
    metricSource_appFlowConfig,
    metricSource_athenaSourceConfig,
    metricSource_cloudWatchConfig,
    metricSource_rDSSourceConfig,
    metricSource_redshiftSourceConfig,
    metricSource_s3SourceConfig,

    -- ** RDSSourceConfig
    rDSSourceConfig_dbInstanceIdentifier,
    rDSSourceConfig_databaseHost,
    rDSSourceConfig_databaseName,
    rDSSourceConfig_databasePort,
    rDSSourceConfig_roleArn,
    rDSSourceConfig_secretManagerArn,
    rDSSourceConfig_tableName,
    rDSSourceConfig_vpcConfiguration,

    -- ** RedshiftSourceConfig
    redshiftSourceConfig_clusterIdentifier,
    redshiftSourceConfig_databaseHost,
    redshiftSourceConfig_databaseName,
    redshiftSourceConfig_databasePort,
    redshiftSourceConfig_roleArn,
    redshiftSourceConfig_secretManagerArn,
    redshiftSourceConfig_tableName,
    redshiftSourceConfig_vpcConfiguration,

    -- ** S3SourceConfig
    s3SourceConfig_fileFormatDescriptor,
    s3SourceConfig_historicalDataPathList,
    s3SourceConfig_roleArn,
    s3SourceConfig_templatedPathList,

    -- ** SNSConfiguration
    sNSConfiguration_snsFormat,
    sNSConfiguration_roleArn,
    sNSConfiguration_snsTopicArn,

    -- ** SampleDataS3SourceConfig
    sampleDataS3SourceConfig_historicalDataPathList,
    sampleDataS3SourceConfig_templatedPathList,
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
    timestampColumn_columnFormat,
    timestampColumn_columnName,

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
import Amazonka.LookoutMetrics.Types.Filter
import Amazonka.LookoutMetrics.Types.InterMetricImpactDetails
import Amazonka.LookoutMetrics.Types.ItemizedMetricStats
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

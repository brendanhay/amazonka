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

    -- ** GetFeedback
    getFeedback_nextToken,
    getFeedback_maxResults,
    getFeedback_anomalyDetectorArn,
    getFeedback_anomalyGroupTimeSeriesFeedback,
    getFeedbackResponse_anomalyGroupTimeSeriesFeedback,
    getFeedbackResponse_nextToken,
    getFeedbackResponse_httpStatus,

    -- ** ListAlerts
    listAlerts_anomalyDetectorArn,
    listAlerts_nextToken,
    listAlerts_maxResults,
    listAlertsResponse_nextToken,
    listAlertsResponse_alertSummaryList,
    listAlertsResponse_httpStatus,

    -- ** ListMetricSets
    listMetricSets_anomalyDetectorArn,
    listMetricSets_nextToken,
    listMetricSets_maxResults,
    listMetricSetsResponse_nextToken,
    listMetricSetsResponse_metricSetSummaryList,
    listMetricSetsResponse_httpStatus,

    -- ** DeleteAnomalyDetector
    deleteAnomalyDetector_anomalyDetectorArn,
    deleteAnomalyDetectorResponse_httpStatus,

    -- ** UpdateAnomalyDetector
    updateAnomalyDetector_kmsKeyArn,
    updateAnomalyDetector_anomalyDetectorConfig,
    updateAnomalyDetector_anomalyDetectorDescription,
    updateAnomalyDetector_anomalyDetectorArn,
    updateAnomalyDetectorResponse_anomalyDetectorArn,
    updateAnomalyDetectorResponse_httpStatus,

    -- ** ListAnomalyDetectors
    listAnomalyDetectors_nextToken,
    listAnomalyDetectors_maxResults,
    listAnomalyDetectorsResponse_nextToken,
    listAnomalyDetectorsResponse_anomalyDetectorSummaryList,
    listAnomalyDetectorsResponse_httpStatus,

    -- ** DescribeAnomalyDetectionExecutions
    describeAnomalyDetectionExecutions_nextToken,
    describeAnomalyDetectionExecutions_timestamp,
    describeAnomalyDetectionExecutions_maxResults,
    describeAnomalyDetectionExecutions_anomalyDetectorArn,
    describeAnomalyDetectionExecutionsResponse_nextToken,
    describeAnomalyDetectionExecutionsResponse_executionList,
    describeAnomalyDetectionExecutionsResponse_httpStatus,

    -- ** CreateMetricSet
    createMetricSet_dimensionList,
    createMetricSet_offset,
    createMetricSet_timestampColumn,
    createMetricSet_metricSetFrequency,
    createMetricSet_metricSetDescription,
    createMetricSet_timezone,
    createMetricSet_tags,
    createMetricSet_anomalyDetectorArn,
    createMetricSet_metricSetName,
    createMetricSet_metricList,
    createMetricSet_metricSource,
    createMetricSetResponse_metricSetArn,
    createMetricSetResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateAlert
    createAlert_alertDescription,
    createAlert_tags,
    createAlert_alertName,
    createAlert_alertSensitivityThreshold,
    createAlert_anomalyDetectorArn,
    createAlert_action,
    createAlertResponse_alertArn,
    createAlertResponse_httpStatus,

    -- ** GetAnomalyGroup
    getAnomalyGroup_anomalyGroupId,
    getAnomalyGroup_anomalyDetectorArn,
    getAnomalyGroupResponse_anomalyGroup,
    getAnomalyGroupResponse_httpStatus,

    -- ** PutFeedback
    putFeedback_anomalyDetectorArn,
    putFeedback_anomalyGroupTimeSeriesFeedback,
    putFeedbackResponse_httpStatus,

    -- ** BackTestAnomalyDetector
    backTestAnomalyDetector_anomalyDetectorArn,
    backTestAnomalyDetectorResponse_httpStatus,

    -- ** DeleteAlert
    deleteAlert_alertArn,
    deleteAlertResponse_httpStatus,

    -- ** CreateAnomalyDetector
    createAnomalyDetector_kmsKeyArn,
    createAnomalyDetector_anomalyDetectorDescription,
    createAnomalyDetector_tags,
    createAnomalyDetector_anomalyDetectorName,
    createAnomalyDetector_anomalyDetectorConfig,
    createAnomalyDetectorResponse_anomalyDetectorArn,
    createAnomalyDetectorResponse_httpStatus,

    -- ** UpdateMetricSet
    updateMetricSet_dimensionList,
    updateMetricSet_offset,
    updateMetricSet_timestampColumn,
    updateMetricSet_metricList,
    updateMetricSet_metricSource,
    updateMetricSet_metricSetFrequency,
    updateMetricSet_metricSetDescription,
    updateMetricSet_metricSetArn,
    updateMetricSetResponse_metricSetArn,
    updateMetricSetResponse_httpStatus,

    -- ** ActivateAnomalyDetector
    activateAnomalyDetector_anomalyDetectorArn,
    activateAnomalyDetectorResponse_httpStatus,

    -- ** ListAnomalyGroupTimeSeries
    listAnomalyGroupTimeSeries_nextToken,
    listAnomalyGroupTimeSeries_maxResults,
    listAnomalyGroupTimeSeries_anomalyDetectorArn,
    listAnomalyGroupTimeSeries_anomalyGroupId,
    listAnomalyGroupTimeSeries_metricName,
    listAnomalyGroupTimeSeriesResponse_timeSeriesList,
    listAnomalyGroupTimeSeriesResponse_timestampList,
    listAnomalyGroupTimeSeriesResponse_metricName,
    listAnomalyGroupTimeSeriesResponse_anomalyGroupId,
    listAnomalyGroupTimeSeriesResponse_nextToken,
    listAnomalyGroupTimeSeriesResponse_httpStatus,

    -- ** GetSampleData
    getSampleData_s3SourceConfig,
    getSampleDataResponse_sampleRows,
    getSampleDataResponse_headerValues,
    getSampleDataResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DescribeMetricSet
    describeMetricSet_metricSetArn,
    describeMetricSetResponse_creationTime,
    describeMetricSetResponse_dimensionList,
    describeMetricSetResponse_offset,
    describeMetricSetResponse_timestampColumn,
    describeMetricSetResponse_metricList,
    describeMetricSetResponse_anomalyDetectorArn,
    describeMetricSetResponse_metricSource,
    describeMetricSetResponse_metricSetName,
    describeMetricSetResponse_metricSetFrequency,
    describeMetricSetResponse_metricSetDescription,
    describeMetricSetResponse_timezone,
    describeMetricSetResponse_metricSetArn,
    describeMetricSetResponse_lastModificationTime,
    describeMetricSetResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeAlert
    describeAlert_alertArn,
    describeAlertResponse_alert,
    describeAlertResponse_httpStatus,

    -- ** ListAnomalyGroupSummaries
    listAnomalyGroupSummaries_nextToken,
    listAnomalyGroupSummaries_maxResults,
    listAnomalyGroupSummaries_anomalyDetectorArn,
    listAnomalyGroupSummaries_sensitivityThreshold,
    listAnomalyGroupSummariesResponse_anomalyGroupStatistics,
    listAnomalyGroupSummariesResponse_nextToken,
    listAnomalyGroupSummariesResponse_anomalyGroupSummaryList,
    listAnomalyGroupSummariesResponse_httpStatus,

    -- ** DescribeAnomalyDetector
    describeAnomalyDetector_anomalyDetectorArn,
    describeAnomalyDetectorResponse_creationTime,
    describeAnomalyDetectorResponse_status,
    describeAnomalyDetectorResponse_failureReason,
    describeAnomalyDetectorResponse_kmsKeyArn,
    describeAnomalyDetectorResponse_anomalyDetectorArn,
    describeAnomalyDetectorResponse_anomalyDetectorConfig,
    describeAnomalyDetectorResponse_anomalyDetectorName,
    describeAnomalyDetectorResponse_anomalyDetectorDescription,
    describeAnomalyDetectorResponse_lastModificationTime,
    describeAnomalyDetectorResponse_httpStatus,

    -- * Types

    -- ** Action
    action_lambdaConfiguration,
    action_sNSConfiguration,

    -- ** Alert
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

    -- ** AlertSummary
    alertSummary_creationTime,
    alertSummary_anomalyDetectorArn,
    alertSummary_alertName,
    alertSummary_alertSensitivityThreshold,
    alertSummary_alertStatus,
    alertSummary_alertArn,
    alertSummary_alertType,
    alertSummary_tags,
    alertSummary_lastModificationTime,

    -- ** AnomalyDetectorConfig
    anomalyDetectorConfig_anomalyDetectorFrequency,

    -- ** AnomalyDetectorConfigSummary
    anomalyDetectorConfigSummary_anomalyDetectorFrequency,

    -- ** AnomalyDetectorSummary
    anomalyDetectorSummary_creationTime,
    anomalyDetectorSummary_status,
    anomalyDetectorSummary_anomalyDetectorArn,
    anomalyDetectorSummary_anomalyDetectorName,
    anomalyDetectorSummary_anomalyDetectorDescription,
    anomalyDetectorSummary_tags,
    anomalyDetectorSummary_lastModificationTime,

    -- ** AnomalyGroup
    anomalyGroup_metricLevelImpactList,
    anomalyGroup_startTime,
    anomalyGroup_anomalyGroupId,
    anomalyGroup_anomalyGroupScore,
    anomalyGroup_primaryMetricName,
    anomalyGroup_endTime,

    -- ** AnomalyGroupStatistics
    anomalyGroupStatistics_itemizedMetricStatsList,
    anomalyGroupStatistics_evaluationStartDate,
    anomalyGroupStatistics_totalCount,

    -- ** AnomalyGroupSummary
    anomalyGroupSummary_startTime,
    anomalyGroupSummary_anomalyGroupId,
    anomalyGroupSummary_anomalyGroupScore,
    anomalyGroupSummary_primaryMetricName,
    anomalyGroupSummary_endTime,

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

    -- ** CloudWatchConfig
    cloudWatchConfig_roleArn,

    -- ** ContributionMatrix
    contributionMatrix_dimensionContributionList,

    -- ** CsvFormatDescriptor
    csvFormatDescriptor_quoteSymbol,
    csvFormatDescriptor_containsHeader,
    csvFormatDescriptor_charset,
    csvFormatDescriptor_headerList,
    csvFormatDescriptor_fileCompression,
    csvFormatDescriptor_delimiter,

    -- ** DimensionContribution
    dimensionContribution_dimensionValueContributionList,
    dimensionContribution_dimensionName,

    -- ** DimensionNameValue
    dimensionNameValue_dimensionName,
    dimensionNameValue_dimensionValue,

    -- ** DimensionValueContribution
    dimensionValueContribution_dimensionValue,
    dimensionValueContribution_contributionScore,

    -- ** ExecutionStatus
    executionStatus_status,
    executionStatus_failureReason,
    executionStatus_timestamp,

    -- ** FileFormatDescriptor
    fileFormatDescriptor_jsonFormatDescriptor,
    fileFormatDescriptor_csvFormatDescriptor,

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

    -- ** MetricSetSummary
    metricSetSummary_creationTime,
    metricSetSummary_anomalyDetectorArn,
    metricSetSummary_metricSetName,
    metricSetSummary_metricSetDescription,
    metricSetSummary_metricSetArn,
    metricSetSummary_tags,
    metricSetSummary_lastModificationTime,

    -- ** MetricSource
    metricSource_redshiftSourceConfig,
    metricSource_s3SourceConfig,
    metricSource_rDSSourceConfig,
    metricSource_appFlowConfig,
    metricSource_cloudWatchConfig,

    -- ** RDSSourceConfig
    rDSSourceConfig_dbInstanceIdentifier,
    rDSSourceConfig_databaseHost,
    rDSSourceConfig_databasePort,
    rDSSourceConfig_secretManagerArn,
    rDSSourceConfig_databaseName,
    rDSSourceConfig_tableName,
    rDSSourceConfig_roleArn,
    rDSSourceConfig_vpcConfiguration,

    -- ** RedshiftSourceConfig
    redshiftSourceConfig_clusterIdentifier,
    redshiftSourceConfig_databaseHost,
    redshiftSourceConfig_databasePort,
    redshiftSourceConfig_secretManagerArn,
    redshiftSourceConfig_databaseName,
    redshiftSourceConfig_tableName,
    redshiftSourceConfig_roleArn,
    redshiftSourceConfig_vpcConfiguration,

    -- ** S3SourceConfig
    s3SourceConfig_templatedPathList,
    s3SourceConfig_historicalDataPathList,
    s3SourceConfig_fileFormatDescriptor,
    s3SourceConfig_roleArn,

    -- ** SNSConfiguration
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
import Amazonka.LookoutMetrics.DeleteAlert
import Amazonka.LookoutMetrics.DeleteAnomalyDetector
import Amazonka.LookoutMetrics.DescribeAlert
import Amazonka.LookoutMetrics.DescribeAnomalyDetectionExecutions
import Amazonka.LookoutMetrics.DescribeAnomalyDetector
import Amazonka.LookoutMetrics.DescribeMetricSet
import Amazonka.LookoutMetrics.GetAnomalyGroup
import Amazonka.LookoutMetrics.GetFeedback
import Amazonka.LookoutMetrics.GetSampleData
import Amazonka.LookoutMetrics.ListAlerts
import Amazonka.LookoutMetrics.ListAnomalyDetectors
import Amazonka.LookoutMetrics.ListAnomalyGroupSummaries
import Amazonka.LookoutMetrics.ListAnomalyGroupTimeSeries
import Amazonka.LookoutMetrics.ListMetricSets
import Amazonka.LookoutMetrics.ListTagsForResource
import Amazonka.LookoutMetrics.PutFeedback
import Amazonka.LookoutMetrics.TagResource
import Amazonka.LookoutMetrics.Types.Action
import Amazonka.LookoutMetrics.Types.Alert
import Amazonka.LookoutMetrics.Types.AlertSummary
import Amazonka.LookoutMetrics.Types.AnomalyDetectorConfig
import Amazonka.LookoutMetrics.Types.AnomalyDetectorConfigSummary
import Amazonka.LookoutMetrics.Types.AnomalyDetectorSummary
import Amazonka.LookoutMetrics.Types.AnomalyGroup
import Amazonka.LookoutMetrics.Types.AnomalyGroupStatistics
import Amazonka.LookoutMetrics.Types.AnomalyGroupSummary
import Amazonka.LookoutMetrics.Types.AnomalyGroupTimeSeries
import Amazonka.LookoutMetrics.Types.AnomalyGroupTimeSeriesFeedback
import Amazonka.LookoutMetrics.Types.AppFlowConfig
import Amazonka.LookoutMetrics.Types.CloudWatchConfig
import Amazonka.LookoutMetrics.Types.ContributionMatrix
import Amazonka.LookoutMetrics.Types.CsvFormatDescriptor
import Amazonka.LookoutMetrics.Types.DimensionContribution
import Amazonka.LookoutMetrics.Types.DimensionNameValue
import Amazonka.LookoutMetrics.Types.DimensionValueContribution
import Amazonka.LookoutMetrics.Types.ExecutionStatus
import Amazonka.LookoutMetrics.Types.FileFormatDescriptor
import Amazonka.LookoutMetrics.Types.ItemizedMetricStats
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
import Amazonka.LookoutMetrics.UntagResource
import Amazonka.LookoutMetrics.UpdateAnomalyDetector
import Amazonka.LookoutMetrics.UpdateMetricSet

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Lens
  ( -- * Operations

    -- ** EnableAlarmActions
    enableAlarmActions_alarmNames,

    -- ** GetMetricStatistics
    getMetricStatistics_unit,
    getMetricStatistics_statistics,
    getMetricStatistics_dimensions,
    getMetricStatistics_extendedStatistics,
    getMetricStatistics_namespace,
    getMetricStatistics_metricName,
    getMetricStatistics_startTime,
    getMetricStatistics_endTime,
    getMetricStatistics_period,
    getMetricStatisticsResponse_datapoints,
    getMetricStatisticsResponse_label,
    getMetricStatisticsResponse_httpStatus,

    -- ** PutInsightRule
    putInsightRule_ruleState,
    putInsightRule_tags,
    putInsightRule_ruleName,
    putInsightRule_ruleDefinition,
    putInsightRuleResponse_httpStatus,

    -- ** DeleteAlarms
    deleteAlarms_alarmNames,

    -- ** GetMetricWidgetImage
    getMetricWidgetImage_outputFormat,
    getMetricWidgetImage_metricWidget,
    getMetricWidgetImageResponse_metricWidgetImage,
    getMetricWidgetImageResponse_httpStatus,

    -- ** DescribeInsightRules
    describeInsightRules_nextToken,
    describeInsightRules_maxResults,
    describeInsightRulesResponse_nextToken,
    describeInsightRulesResponse_insightRules,
    describeInsightRulesResponse_httpStatus,

    -- ** PutMetricAlarm
    putMetricAlarm_threshold,
    putMetricAlarm_datapointsToAlarm,
    putMetricAlarm_evaluateLowSampleCountPercentile,
    putMetricAlarm_extendedStatistic,
    putMetricAlarm_alarmActions,
    putMetricAlarm_unit,
    putMetricAlarm_thresholdMetricId,
    putMetricAlarm_metricName,
    putMetricAlarm_insufficientDataActions,
    putMetricAlarm_treatMissingData,
    putMetricAlarm_metrics,
    putMetricAlarm_tags,
    putMetricAlarm_oKActions,
    putMetricAlarm_statistic,
    putMetricAlarm_dimensions,
    putMetricAlarm_namespace,
    putMetricAlarm_actionsEnabled,
    putMetricAlarm_alarmDescription,
    putMetricAlarm_period,
    putMetricAlarm_alarmName,
    putMetricAlarm_evaluationPeriods,
    putMetricAlarm_comparisonOperator,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetInsightRuleReport
    getInsightRuleReport_orderBy,
    getInsightRuleReport_metrics,
    getInsightRuleReport_maxContributorCount,
    getInsightRuleReport_ruleName,
    getInsightRuleReport_startTime,
    getInsightRuleReport_endTime,
    getInsightRuleReport_period,
    getInsightRuleReportResponse_approximateUniqueCount,
    getInsightRuleReportResponse_metricDatapoints,
    getInsightRuleReportResponse_contributors,
    getInsightRuleReportResponse_aggregateValue,
    getInsightRuleReportResponse_keyLabels,
    getInsightRuleReportResponse_aggregationStatistic,
    getInsightRuleReportResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DescribeAnomalyDetectors
    describeAnomalyDetectors_nextToken,
    describeAnomalyDetectors_maxResults,
    describeAnomalyDetectors_metricName,
    describeAnomalyDetectors_dimensions,
    describeAnomalyDetectors_namespace,
    describeAnomalyDetectorsResponse_nextToken,
    describeAnomalyDetectorsResponse_anomalyDetectors,
    describeAnomalyDetectorsResponse_httpStatus,

    -- ** ListMetrics
    listMetrics_nextToken,
    listMetrics_metricName,
    listMetrics_dimensions,
    listMetrics_namespace,
    listMetrics_recentlyActive,
    listMetricsResponse_nextToken,
    listMetricsResponse_metrics,
    listMetricsResponse_httpStatus,

    -- ** PutMetricData
    putMetricData_namespace,
    putMetricData_metricData,

    -- ** PutDashboard
    putDashboard_dashboardName,
    putDashboard_dashboardBody,
    putDashboardResponse_dashboardValidationMessages,
    putDashboardResponse_httpStatus,

    -- ** DescribeAlarmsForMetric
    describeAlarmsForMetric_extendedStatistic,
    describeAlarmsForMetric_unit,
    describeAlarmsForMetric_statistic,
    describeAlarmsForMetric_dimensions,
    describeAlarmsForMetric_period,
    describeAlarmsForMetric_metricName,
    describeAlarmsForMetric_namespace,
    describeAlarmsForMetricResponse_metricAlarms,
    describeAlarmsForMetricResponse_httpStatus,

    -- ** PutAnomalyDetector
    putAnomalyDetector_configuration,
    putAnomalyDetector_dimensions,
    putAnomalyDetector_namespace,
    putAnomalyDetector_metricName,
    putAnomalyDetector_stat,
    putAnomalyDetectorResponse_httpStatus,

    -- ** DeleteInsightRules
    deleteInsightRules_ruleNames,
    deleteInsightRulesResponse_failures,
    deleteInsightRulesResponse_httpStatus,

    -- ** DisableAlarmActions
    disableAlarmActions_alarmNames,

    -- ** GetDashboard
    getDashboard_dashboardName,
    getDashboardResponse_dashboardBody,
    getDashboardResponse_dashboardArn,
    getDashboardResponse_dashboardName,
    getDashboardResponse_httpStatus,

    -- ** PutCompositeAlarm
    putCompositeAlarm_alarmActions,
    putCompositeAlarm_insufficientDataActions,
    putCompositeAlarm_tags,
    putCompositeAlarm_oKActions,
    putCompositeAlarm_actionsEnabled,
    putCompositeAlarm_alarmDescription,
    putCompositeAlarm_alarmName,
    putCompositeAlarm_alarmRule,

    -- ** DisableInsightRules
    disableInsightRules_ruleNames,
    disableInsightRulesResponse_failures,
    disableInsightRulesResponse_httpStatus,

    -- ** DescribeAlarmHistory
    describeAlarmHistory_nextToken,
    describeAlarmHistory_startDate,
    describeAlarmHistory_alarmTypes,
    describeAlarmHistory_historyItemType,
    describeAlarmHistory_scanBy,
    describeAlarmHistory_alarmName,
    describeAlarmHistory_endDate,
    describeAlarmHistory_maxRecords,
    describeAlarmHistoryResponse_nextToken,
    describeAlarmHistoryResponse_alarmHistoryItems,
    describeAlarmHistoryResponse_httpStatus,

    -- ** DeleteDashboards
    deleteDashboards_dashboardNames,
    deleteDashboardsResponse_httpStatus,

    -- ** SetAlarmState
    setAlarmState_stateReasonData,
    setAlarmState_alarmName,
    setAlarmState_stateValue,
    setAlarmState_stateReason,

    -- ** ListDashboards
    listDashboards_nextToken,
    listDashboards_dashboardNamePrefix,
    listDashboardsResponse_nextToken,
    listDashboardsResponse_dashboardEntries,
    listDashboardsResponse_httpStatus,

    -- ** DescribeAlarms
    describeAlarms_nextToken,
    describeAlarms_alarmTypes,
    describeAlarms_alarmNames,
    describeAlarms_stateValue,
    describeAlarms_alarmNamePrefix,
    describeAlarms_actionPrefix,
    describeAlarms_childrenOfAlarmName,
    describeAlarms_parentsOfAlarmName,
    describeAlarms_maxRecords,
    describeAlarmsResponse_nextToken,
    describeAlarmsResponse_metricAlarms,
    describeAlarmsResponse_compositeAlarms,
    describeAlarmsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteAnomalyDetector
    deleteAnomalyDetector_dimensions,
    deleteAnomalyDetector_namespace,
    deleteAnomalyDetector_metricName,
    deleteAnomalyDetector_stat,
    deleteAnomalyDetectorResponse_httpStatus,

    -- ** EnableInsightRules
    enableInsightRules_ruleNames,
    enableInsightRulesResponse_failures,
    enableInsightRulesResponse_httpStatus,

    -- ** GetMetricData
    getMetricData_nextToken,
    getMetricData_scanBy,
    getMetricData_labelOptions,
    getMetricData_maxDatapoints,
    getMetricData_metricDataQueries,
    getMetricData_startTime,
    getMetricData_endTime,
    getMetricDataResponse_nextToken,
    getMetricDataResponse_metricDataResults,
    getMetricDataResponse_messages,
    getMetricDataResponse_httpStatus,

    -- * Types

    -- ** AlarmHistoryItem
    alarmHistoryItem_historySummary,
    alarmHistoryItem_historyItemType,
    alarmHistoryItem_alarmName,
    alarmHistoryItem_timestamp,
    alarmHistoryItem_alarmType,
    alarmHistoryItem_historyData,

    -- ** AnomalyDetector
    anomalyDetector_metricName,
    anomalyDetector_configuration,
    anomalyDetector_stateValue,
    anomalyDetector_dimensions,
    anomalyDetector_namespace,
    anomalyDetector_stat,

    -- ** AnomalyDetectorConfiguration
    anomalyDetectorConfiguration_metricTimezone,
    anomalyDetectorConfiguration_excludedTimeRanges,

    -- ** CompositeAlarm
    compositeAlarm_alarmArn,
    compositeAlarm_alarmActions,
    compositeAlarm_stateReason,
    compositeAlarm_stateReasonData,
    compositeAlarm_insufficientDataActions,
    compositeAlarm_alarmRule,
    compositeAlarm_stateUpdatedTimestamp,
    compositeAlarm_stateValue,
    compositeAlarm_alarmName,
    compositeAlarm_oKActions,
    compositeAlarm_actionsEnabled,
    compositeAlarm_alarmConfigurationUpdatedTimestamp,
    compositeAlarm_alarmDescription,

    -- ** DashboardEntry
    dashboardEntry_dashboardArn,
    dashboardEntry_lastModified,
    dashboardEntry_dashboardName,
    dashboardEntry_size,

    -- ** DashboardValidationMessage
    dashboardValidationMessage_message,
    dashboardValidationMessage_dataPath,

    -- ** Datapoint
    datapoint_unit,
    datapoint_minimum,
    datapoint_sum,
    datapoint_sampleCount,
    datapoint_timestamp,
    datapoint_average,
    datapoint_maximum,
    datapoint_extendedStatistics,

    -- ** Dimension
    dimension_name,
    dimension_value,

    -- ** DimensionFilter
    dimensionFilter_value,
    dimensionFilter_name,

    -- ** InsightRule
    insightRule_name,
    insightRule_state,
    insightRule_schema,
    insightRule_definition,

    -- ** InsightRuleContributor
    insightRuleContributor_keys,
    insightRuleContributor_approximateAggregateValue,
    insightRuleContributor_datapoints,

    -- ** InsightRuleContributorDatapoint
    insightRuleContributorDatapoint_timestamp,
    insightRuleContributorDatapoint_approximateValue,

    -- ** InsightRuleMetricDatapoint
    insightRuleMetricDatapoint_minimum,
    insightRuleMetricDatapoint_sum,
    insightRuleMetricDatapoint_sampleCount,
    insightRuleMetricDatapoint_maxContributorValue,
    insightRuleMetricDatapoint_average,
    insightRuleMetricDatapoint_uniqueContributors,
    insightRuleMetricDatapoint_maximum,
    insightRuleMetricDatapoint_timestamp,

    -- ** LabelOptions
    labelOptions_timezone,

    -- ** MessageData
    messageData_code,
    messageData_value,

    -- ** Metric
    metric_metricName,
    metric_dimensions,
    metric_namespace,

    -- ** MetricAlarm
    metricAlarm_threshold,
    metricAlarm_datapointsToAlarm,
    metricAlarm_evaluateLowSampleCountPercentile,
    metricAlarm_comparisonOperator,
    metricAlarm_extendedStatistic,
    metricAlarm_alarmArn,
    metricAlarm_alarmActions,
    metricAlarm_unit,
    metricAlarm_thresholdMetricId,
    metricAlarm_stateReason,
    metricAlarm_stateReasonData,
    metricAlarm_metricName,
    metricAlarm_insufficientDataActions,
    metricAlarm_treatMissingData,
    metricAlarm_metrics,
    metricAlarm_stateUpdatedTimestamp,
    metricAlarm_stateValue,
    metricAlarm_alarmName,
    metricAlarm_oKActions,
    metricAlarm_statistic,
    metricAlarm_dimensions,
    metricAlarm_namespace,
    metricAlarm_evaluationPeriods,
    metricAlarm_actionsEnabled,
    metricAlarm_alarmConfigurationUpdatedTimestamp,
    metricAlarm_alarmDescription,
    metricAlarm_period,

    -- ** MetricDataQuery
    metricDataQuery_metricStat,
    metricDataQuery_returnData,
    metricDataQuery_label,
    metricDataQuery_period,
    metricDataQuery_expression,
    metricDataQuery_id,

    -- ** MetricDataResult
    metricDataResult_values,
    metricDataResult_id,
    metricDataResult_timestamps,
    metricDataResult_statusCode,
    metricDataResult_label,
    metricDataResult_messages,

    -- ** MetricDatum
    metricDatum_storageResolution,
    metricDatum_unit,
    metricDatum_values,
    metricDatum_counts,
    metricDatum_timestamp,
    metricDatum_statisticValues,
    metricDatum_value,
    metricDatum_dimensions,
    metricDatum_metricName,

    -- ** MetricStat
    metricStat_unit,
    metricStat_metric,
    metricStat_period,
    metricStat_stat,

    -- ** PartialFailure
    partialFailure_exceptionType,
    partialFailure_failureCode,
    partialFailure_failureDescription,
    partialFailure_failureResource,

    -- ** Range
    range_startTime,
    range_endTime,

    -- ** StatisticSet
    statisticSet_sampleCount,
    statisticSet_sum,
    statisticSet_minimum,
    statisticSet_maximum,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.CloudWatch.DeleteAlarms
import Network.AWS.CloudWatch.DeleteAnomalyDetector
import Network.AWS.CloudWatch.DeleteDashboards
import Network.AWS.CloudWatch.DeleteInsightRules
import Network.AWS.CloudWatch.DescribeAlarmHistory
import Network.AWS.CloudWatch.DescribeAlarms
import Network.AWS.CloudWatch.DescribeAlarmsForMetric
import Network.AWS.CloudWatch.DescribeAnomalyDetectors
import Network.AWS.CloudWatch.DescribeInsightRules
import Network.AWS.CloudWatch.DisableAlarmActions
import Network.AWS.CloudWatch.DisableInsightRules
import Network.AWS.CloudWatch.EnableAlarmActions
import Network.AWS.CloudWatch.EnableInsightRules
import Network.AWS.CloudWatch.GetDashboard
import Network.AWS.CloudWatch.GetInsightRuleReport
import Network.AWS.CloudWatch.GetMetricData
import Network.AWS.CloudWatch.GetMetricStatistics
import Network.AWS.CloudWatch.GetMetricWidgetImage
import Network.AWS.CloudWatch.ListDashboards
import Network.AWS.CloudWatch.ListMetrics
import Network.AWS.CloudWatch.ListTagsForResource
import Network.AWS.CloudWatch.PutAnomalyDetector
import Network.AWS.CloudWatch.PutCompositeAlarm
import Network.AWS.CloudWatch.PutDashboard
import Network.AWS.CloudWatch.PutInsightRule
import Network.AWS.CloudWatch.PutMetricAlarm
import Network.AWS.CloudWatch.PutMetricData
import Network.AWS.CloudWatch.SetAlarmState
import Network.AWS.CloudWatch.TagResource
import Network.AWS.CloudWatch.Types.AlarmHistoryItem
import Network.AWS.CloudWatch.Types.AnomalyDetector
import Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration
import Network.AWS.CloudWatch.Types.CompositeAlarm
import Network.AWS.CloudWatch.Types.DashboardEntry
import Network.AWS.CloudWatch.Types.DashboardValidationMessage
import Network.AWS.CloudWatch.Types.Datapoint
import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.CloudWatch.Types.DimensionFilter
import Network.AWS.CloudWatch.Types.InsightRule
import Network.AWS.CloudWatch.Types.InsightRuleContributor
import Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
import Network.AWS.CloudWatch.Types.InsightRuleMetricDatapoint
import Network.AWS.CloudWatch.Types.LabelOptions
import Network.AWS.CloudWatch.Types.MessageData
import Network.AWS.CloudWatch.Types.Metric
import Network.AWS.CloudWatch.Types.MetricAlarm
import Network.AWS.CloudWatch.Types.MetricDataQuery
import Network.AWS.CloudWatch.Types.MetricDataResult
import Network.AWS.CloudWatch.Types.MetricDatum
import Network.AWS.CloudWatch.Types.MetricStat
import Network.AWS.CloudWatch.Types.PartialFailure
import Network.AWS.CloudWatch.Types.Range
import Network.AWS.CloudWatch.Types.StatisticSet
import Network.AWS.CloudWatch.Types.Tag
import Network.AWS.CloudWatch.UntagResource

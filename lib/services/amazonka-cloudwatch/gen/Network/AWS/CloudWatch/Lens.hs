{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatch.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Lens
  ( -- * Operations

    -- ** EnableAlarmActions
    enableAlarmActions_alarmNames,

    -- ** DisableInsightRules
    disableInsightRules_ruleNames,
    disableInsightRulesResponse_failures,
    disableInsightRulesResponse_httpStatus,

    -- ** PutCompositeAlarm
    putCompositeAlarm_alarmDescription,
    putCompositeAlarm_oKActions,
    putCompositeAlarm_actionsEnabled,
    putCompositeAlarm_insufficientDataActions,
    putCompositeAlarm_alarmActions,
    putCompositeAlarm_tags,
    putCompositeAlarm_alarmName,
    putCompositeAlarm_alarmRule,

    -- ** DeleteAnomalyDetector
    deleteAnomalyDetector_dimensions,
    deleteAnomalyDetector_namespace,
    deleteAnomalyDetector_metricName,
    deleteAnomalyDetector_stat,
    deleteAnomalyDetectorResponse_httpStatus,

    -- ** DeleteInsightRules
    deleteInsightRules_ruleNames,
    deleteInsightRulesResponse_failures,
    deleteInsightRulesResponse_httpStatus,

    -- ** GetDashboard
    getDashboard_dashboardName,
    getDashboardResponse_dashboardName,
    getDashboardResponse_dashboardBody,
    getDashboardResponse_dashboardArn,
    getDashboardResponse_httpStatus,

    -- ** PutAnomalyDetector
    putAnomalyDetector_configuration,
    putAnomalyDetector_dimensions,
    putAnomalyDetector_namespace,
    putAnomalyDetector_metricName,
    putAnomalyDetector_stat,
    putAnomalyDetectorResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetMetricData
    getMetricData_maxDatapoints,
    getMetricData_labelOptions,
    getMetricData_nextToken,
    getMetricData_scanBy,
    getMetricData_metricDataQueries,
    getMetricData_startTime,
    getMetricData_endTime,
    getMetricDataResponse_metricDataResults,
    getMetricDataResponse_nextToken,
    getMetricDataResponse_messages,
    getMetricDataResponse_httpStatus,

    -- ** PutMetricData
    putMetricData_namespace,
    putMetricData_metricData,

    -- ** ListDashboards
    listDashboards_dashboardNamePrefix,
    listDashboards_nextToken,
    listDashboardsResponse_dashboardEntries,
    listDashboardsResponse_nextToken,
    listDashboardsResponse_httpStatus,

    -- ** DescribeAlarms
    describeAlarms_alarmNamePrefix,
    describeAlarms_alarmTypes,
    describeAlarms_actionPrefix,
    describeAlarms_nextToken,
    describeAlarms_stateValue,
    describeAlarms_alarmNames,
    describeAlarms_maxRecords,
    describeAlarms_parentsOfAlarmName,
    describeAlarms_childrenOfAlarmName,
    describeAlarmsResponse_metricAlarms,
    describeAlarmsResponse_compositeAlarms,
    describeAlarmsResponse_nextToken,
    describeAlarmsResponse_httpStatus,

    -- ** ListMetrics
    listMetrics_metricName,
    listMetrics_namespace,
    listMetrics_nextToken,
    listMetrics_recentlyActive,
    listMetrics_dimensions,
    listMetricsResponse_metrics,
    listMetricsResponse_nextToken,
    listMetricsResponse_httpStatus,

    -- ** GetInsightRuleReport
    getInsightRuleReport_maxContributorCount,
    getInsightRuleReport_metrics,
    getInsightRuleReport_orderBy,
    getInsightRuleReport_ruleName,
    getInsightRuleReport_startTime,
    getInsightRuleReport_endTime,
    getInsightRuleReport_period,
    getInsightRuleReportResponse_keyLabels,
    getInsightRuleReportResponse_approximateUniqueCount,
    getInsightRuleReportResponse_aggregationStatistic,
    getInsightRuleReportResponse_aggregateValue,
    getInsightRuleReportResponse_contributors,
    getInsightRuleReportResponse_metricDatapoints,
    getInsightRuleReportResponse_httpStatus,

    -- ** StartMetricStreams
    startMetricStreams_names,
    startMetricStreamsResponse_httpStatus,

    -- ** DeleteDashboards
    deleteDashboards_dashboardNames,
    deleteDashboardsResponse_httpStatus,

    -- ** PutInsightRule
    putInsightRule_tags,
    putInsightRule_ruleState,
    putInsightRule_ruleName,
    putInsightRule_ruleDefinition,
    putInsightRuleResponse_httpStatus,

    -- ** ListMetricStreams
    listMetricStreams_nextToken,
    listMetricStreams_maxResults,
    listMetricStreamsResponse_entries,
    listMetricStreamsResponse_nextToken,
    listMetricStreamsResponse_httpStatus,

    -- ** GetMetricWidgetImage
    getMetricWidgetImage_outputFormat,
    getMetricWidgetImage_metricWidget,
    getMetricWidgetImageResponse_metricWidgetImage,
    getMetricWidgetImageResponse_httpStatus,

    -- ** DeleteMetricStream
    deleteMetricStream_name,
    deleteMetricStreamResponse_httpStatus,

    -- ** DeleteAlarms
    deleteAlarms_alarmNames,

    -- ** PutMetricStream
    putMetricStream_includeFilters,
    putMetricStream_excludeFilters,
    putMetricStream_tags,
    putMetricStream_name,
    putMetricStream_firehoseArn,
    putMetricStream_roleArn,
    putMetricStream_outputFormat,
    putMetricStreamResponse_arn,
    putMetricStreamResponse_httpStatus,

    -- ** DescribeAlarmHistory
    describeAlarmHistory_alarmName,
    describeAlarmHistory_historyItemType,
    describeAlarmHistory_alarmTypes,
    describeAlarmHistory_endDate,
    describeAlarmHistory_startDate,
    describeAlarmHistory_nextToken,
    describeAlarmHistory_scanBy,
    describeAlarmHistory_maxRecords,
    describeAlarmHistoryResponse_alarmHistoryItems,
    describeAlarmHistoryResponse_nextToken,
    describeAlarmHistoryResponse_httpStatus,

    -- ** GetMetricStatistics
    getMetricStatistics_extendedStatistics,
    getMetricStatistics_statistics,
    getMetricStatistics_dimensions,
    getMetricStatistics_unit,
    getMetricStatistics_namespace,
    getMetricStatistics_metricName,
    getMetricStatistics_startTime,
    getMetricStatistics_endTime,
    getMetricStatistics_period,
    getMetricStatisticsResponse_datapoints,
    getMetricStatisticsResponse_label,
    getMetricStatisticsResponse_httpStatus,

    -- ** DescribeAlarmsForMetric
    describeAlarmsForMetric_period,
    describeAlarmsForMetric_dimensions,
    describeAlarmsForMetric_unit,
    describeAlarmsForMetric_statistic,
    describeAlarmsForMetric_extendedStatistic,
    describeAlarmsForMetric_metricName,
    describeAlarmsForMetric_namespace,
    describeAlarmsForMetricResponse_metricAlarms,
    describeAlarmsForMetricResponse_httpStatus,

    -- ** EnableInsightRules
    enableInsightRules_ruleNames,
    enableInsightRulesResponse_failures,
    enableInsightRulesResponse_httpStatus,

    -- ** DisableAlarmActions
    disableAlarmActions_alarmNames,

    -- ** DescribeAnomalyDetectors
    describeAnomalyDetectors_metricName,
    describeAnomalyDetectors_namespace,
    describeAnomalyDetectors_nextToken,
    describeAnomalyDetectors_dimensions,
    describeAnomalyDetectors_maxResults,
    describeAnomalyDetectorsResponse_anomalyDetectors,
    describeAnomalyDetectorsResponse_nextToken,
    describeAnomalyDetectorsResponse_httpStatus,

    -- ** PutDashboard
    putDashboard_dashboardName,
    putDashboard_dashboardBody,
    putDashboardResponse_dashboardValidationMessages,
    putDashboardResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** StopMetricStreams
    stopMetricStreams_names,
    stopMetricStreamsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetMetricStream
    getMetricStream_name,
    getMetricStreamResponse_includeFilters,
    getMetricStreamResponse_state,
    getMetricStreamResponse_excludeFilters,
    getMetricStreamResponse_arn,
    getMetricStreamResponse_firehoseArn,
    getMetricStreamResponse_outputFormat,
    getMetricStreamResponse_lastUpdateDate,
    getMetricStreamResponse_name,
    getMetricStreamResponse_creationDate,
    getMetricStreamResponse_roleArn,
    getMetricStreamResponse_httpStatus,

    -- ** PutMetricAlarm
    putMetricAlarm_metrics,
    putMetricAlarm_treatMissingData,
    putMetricAlarm_period,
    putMetricAlarm_alarmDescription,
    putMetricAlarm_metricName,
    putMetricAlarm_namespace,
    putMetricAlarm_thresholdMetricId,
    putMetricAlarm_oKActions,
    putMetricAlarm_evaluateLowSampleCountPercentile,
    putMetricAlarm_datapointsToAlarm,
    putMetricAlarm_threshold,
    putMetricAlarm_actionsEnabled,
    putMetricAlarm_insufficientDataActions,
    putMetricAlarm_dimensions,
    putMetricAlarm_alarmActions,
    putMetricAlarm_unit,
    putMetricAlarm_statistic,
    putMetricAlarm_tags,
    putMetricAlarm_extendedStatistic,
    putMetricAlarm_alarmName,
    putMetricAlarm_evaluationPeriods,
    putMetricAlarm_comparisonOperator,

    -- ** SetAlarmState
    setAlarmState_stateReasonData,
    setAlarmState_alarmName,
    setAlarmState_stateValue,
    setAlarmState_stateReason,

    -- ** DescribeInsightRules
    describeInsightRules_nextToken,
    describeInsightRules_maxResults,
    describeInsightRulesResponse_nextToken,
    describeInsightRulesResponse_insightRules,
    describeInsightRulesResponse_httpStatus,

    -- * Types

    -- ** AlarmHistoryItem
    alarmHistoryItem_alarmName,
    alarmHistoryItem_historyItemType,
    alarmHistoryItem_historyData,
    alarmHistoryItem_alarmType,
    alarmHistoryItem_historySummary,
    alarmHistoryItem_timestamp,

    -- ** AnomalyDetector
    anomalyDetector_metricName,
    anomalyDetector_namespace,
    anomalyDetector_stateValue,
    anomalyDetector_stat,
    anomalyDetector_configuration,
    anomalyDetector_dimensions,

    -- ** AnomalyDetectorConfiguration
    anomalyDetectorConfiguration_metricTimezone,
    anomalyDetectorConfiguration_excludedTimeRanges,

    -- ** CompositeAlarm
    compositeAlarm_alarmName,
    compositeAlarm_stateUpdatedTimestamp,
    compositeAlarm_alarmDescription,
    compositeAlarm_alarmRule,
    compositeAlarm_oKActions,
    compositeAlarm_stateValue,
    compositeAlarm_alarmConfigurationUpdatedTimestamp,
    compositeAlarm_actionsEnabled,
    compositeAlarm_insufficientDataActions,
    compositeAlarm_stateReason,
    compositeAlarm_stateReasonData,
    compositeAlarm_alarmArn,
    compositeAlarm_alarmActions,

    -- ** DashboardEntry
    dashboardEntry_size,
    dashboardEntry_dashboardName,
    dashboardEntry_lastModified,
    dashboardEntry_dashboardArn,

    -- ** DashboardValidationMessage
    dashboardValidationMessage_dataPath,
    dashboardValidationMessage_message,

    -- ** Datapoint
    datapoint_sampleCount,
    datapoint_maximum,
    datapoint_average,
    datapoint_minimum,
    datapoint_extendedStatistics,
    datapoint_sum,
    datapoint_unit,
    datapoint_timestamp,

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
    insightRuleMetricDatapoint_maxContributorValue,
    insightRuleMetricDatapoint_sampleCount,
    insightRuleMetricDatapoint_maximum,
    insightRuleMetricDatapoint_average,
    insightRuleMetricDatapoint_minimum,
    insightRuleMetricDatapoint_uniqueContributors,
    insightRuleMetricDatapoint_sum,
    insightRuleMetricDatapoint_timestamp,

    -- ** LabelOptions
    labelOptions_timezone,

    -- ** MessageData
    messageData_value,
    messageData_code,

    -- ** Metric
    metric_metricName,
    metric_namespace,
    metric_dimensions,

    -- ** MetricAlarm
    metricAlarm_alarmName,
    metricAlarm_stateUpdatedTimestamp,
    metricAlarm_metrics,
    metricAlarm_treatMissingData,
    metricAlarm_period,
    metricAlarm_alarmDescription,
    metricAlarm_evaluationPeriods,
    metricAlarm_metricName,
    metricAlarm_namespace,
    metricAlarm_thresholdMetricId,
    metricAlarm_comparisonOperator,
    metricAlarm_oKActions,
    metricAlarm_evaluateLowSampleCountPercentile,
    metricAlarm_stateValue,
    metricAlarm_datapointsToAlarm,
    metricAlarm_threshold,
    metricAlarm_alarmConfigurationUpdatedTimestamp,
    metricAlarm_actionsEnabled,
    metricAlarm_insufficientDataActions,
    metricAlarm_stateReason,
    metricAlarm_stateReasonData,
    metricAlarm_dimensions,
    metricAlarm_alarmArn,
    metricAlarm_alarmActions,
    metricAlarm_unit,
    metricAlarm_statistic,
    metricAlarm_extendedStatistic,

    -- ** MetricDataQuery
    metricDataQuery_returnData,
    metricDataQuery_period,
    metricDataQuery_accountId,
    metricDataQuery_expression,
    metricDataQuery_label,
    metricDataQuery_metricStat,
    metricDataQuery_id,

    -- ** MetricDataResult
    metricDataResult_values,
    metricDataResult_id,
    metricDataResult_timestamps,
    metricDataResult_messages,
    metricDataResult_label,
    metricDataResult_statusCode,

    -- ** MetricDatum
    metricDatum_values,
    metricDatum_counts,
    metricDatum_value,
    metricDatum_storageResolution,
    metricDatum_dimensions,
    metricDatum_unit,
    metricDatum_timestamp,
    metricDatum_statisticValues,
    metricDatum_metricName,

    -- ** MetricStat
    metricStat_unit,
    metricStat_metric,
    metricStat_period,
    metricStat_stat,

    -- ** MetricStreamEntry
    metricStreamEntry_state,
    metricStreamEntry_arn,
    metricStreamEntry_firehoseArn,
    metricStreamEntry_outputFormat,
    metricStreamEntry_lastUpdateDate,
    metricStreamEntry_name,
    metricStreamEntry_creationDate,

    -- ** MetricStreamFilter
    metricStreamFilter_namespace,

    -- ** PartialFailure
    partialFailure_failureResource,
    partialFailure_failureCode,
    partialFailure_failureDescription,
    partialFailure_exceptionType,

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

import Amazonka.CloudWatch.DeleteAlarms
import Amazonka.CloudWatch.DeleteAnomalyDetector
import Amazonka.CloudWatch.DeleteDashboards
import Amazonka.CloudWatch.DeleteInsightRules
import Amazonka.CloudWatch.DeleteMetricStream
import Amazonka.CloudWatch.DescribeAlarmHistory
import Amazonka.CloudWatch.DescribeAlarms
import Amazonka.CloudWatch.DescribeAlarmsForMetric
import Amazonka.CloudWatch.DescribeAnomalyDetectors
import Amazonka.CloudWatch.DescribeInsightRules
import Amazonka.CloudWatch.DisableAlarmActions
import Amazonka.CloudWatch.DisableInsightRules
import Amazonka.CloudWatch.EnableAlarmActions
import Amazonka.CloudWatch.EnableInsightRules
import Amazonka.CloudWatch.GetDashboard
import Amazonka.CloudWatch.GetInsightRuleReport
import Amazonka.CloudWatch.GetMetricData
import Amazonka.CloudWatch.GetMetricStatistics
import Amazonka.CloudWatch.GetMetricStream
import Amazonka.CloudWatch.GetMetricWidgetImage
import Amazonka.CloudWatch.ListDashboards
import Amazonka.CloudWatch.ListMetricStreams
import Amazonka.CloudWatch.ListMetrics
import Amazonka.CloudWatch.ListTagsForResource
import Amazonka.CloudWatch.PutAnomalyDetector
import Amazonka.CloudWatch.PutCompositeAlarm
import Amazonka.CloudWatch.PutDashboard
import Amazonka.CloudWatch.PutInsightRule
import Amazonka.CloudWatch.PutMetricAlarm
import Amazonka.CloudWatch.PutMetricData
import Amazonka.CloudWatch.PutMetricStream
import Amazonka.CloudWatch.SetAlarmState
import Amazonka.CloudWatch.StartMetricStreams
import Amazonka.CloudWatch.StopMetricStreams
import Amazonka.CloudWatch.TagResource
import Amazonka.CloudWatch.Types.AlarmHistoryItem
import Amazonka.CloudWatch.Types.AnomalyDetector
import Amazonka.CloudWatch.Types.AnomalyDetectorConfiguration
import Amazonka.CloudWatch.Types.CompositeAlarm
import Amazonka.CloudWatch.Types.DashboardEntry
import Amazonka.CloudWatch.Types.DashboardValidationMessage
import Amazonka.CloudWatch.Types.Datapoint
import Amazonka.CloudWatch.Types.Dimension
import Amazonka.CloudWatch.Types.DimensionFilter
import Amazonka.CloudWatch.Types.InsightRule
import Amazonka.CloudWatch.Types.InsightRuleContributor
import Amazonka.CloudWatch.Types.InsightRuleContributorDatapoint
import Amazonka.CloudWatch.Types.InsightRuleMetricDatapoint
import Amazonka.CloudWatch.Types.LabelOptions
import Amazonka.CloudWatch.Types.MessageData
import Amazonka.CloudWatch.Types.Metric
import Amazonka.CloudWatch.Types.MetricAlarm
import Amazonka.CloudWatch.Types.MetricDataQuery
import Amazonka.CloudWatch.Types.MetricDataResult
import Amazonka.CloudWatch.Types.MetricDatum
import Amazonka.CloudWatch.Types.MetricStat
import Amazonka.CloudWatch.Types.MetricStreamEntry
import Amazonka.CloudWatch.Types.MetricStreamFilter
import Amazonka.CloudWatch.Types.PartialFailure
import Amazonka.CloudWatch.Types.Range
import Amazonka.CloudWatch.Types.StatisticSet
import Amazonka.CloudWatch.Types.Tag
import Amazonka.CloudWatch.UntagResource

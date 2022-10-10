{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatch.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Lens
  ( -- * Operations

    -- ** DeleteAlarms
    deleteAlarms_alarmNames,

    -- ** DeleteAnomalyDetector
    deleteAnomalyDetector_singleMetricAnomalyDetector,
    deleteAnomalyDetector_dimensions,
    deleteAnomalyDetector_metricMathAnomalyDetector,
    deleteAnomalyDetector_stat,
    deleteAnomalyDetector_metricName,
    deleteAnomalyDetector_namespace,
    deleteAnomalyDetectorResponse_httpStatus,

    -- ** DeleteDashboards
    deleteDashboards_dashboardNames,
    deleteDashboardsResponse_httpStatus,

    -- ** DeleteInsightRules
    deleteInsightRules_ruleNames,
    deleteInsightRulesResponse_failures,
    deleteInsightRulesResponse_httpStatus,

    -- ** DeleteMetricStream
    deleteMetricStream_name,
    deleteMetricStreamResponse_httpStatus,

    -- ** DescribeAlarmHistory
    describeAlarmHistory_nextToken,
    describeAlarmHistory_alarmTypes,
    describeAlarmHistory_endDate,
    describeAlarmHistory_maxRecords,
    describeAlarmHistory_startDate,
    describeAlarmHistory_historyItemType,
    describeAlarmHistory_scanBy,
    describeAlarmHistory_alarmName,
    describeAlarmHistoryResponse_nextToken,
    describeAlarmHistoryResponse_alarmHistoryItems,
    describeAlarmHistoryResponse_httpStatus,

    -- ** DescribeAlarms
    describeAlarms_nextToken,
    describeAlarms_alarmNamePrefix,
    describeAlarms_alarmTypes,
    describeAlarms_childrenOfAlarmName,
    describeAlarms_maxRecords,
    describeAlarms_parentsOfAlarmName,
    describeAlarms_alarmNames,
    describeAlarms_stateValue,
    describeAlarms_actionPrefix,
    describeAlarmsResponse_nextToken,
    describeAlarmsResponse_compositeAlarms,
    describeAlarmsResponse_metricAlarms,
    describeAlarmsResponse_httpStatus,

    -- ** DescribeAlarmsForMetric
    describeAlarmsForMetric_extendedStatistic,
    describeAlarmsForMetric_period,
    describeAlarmsForMetric_dimensions,
    describeAlarmsForMetric_statistic,
    describeAlarmsForMetric_unit,
    describeAlarmsForMetric_metricName,
    describeAlarmsForMetric_namespace,
    describeAlarmsForMetricResponse_metricAlarms,
    describeAlarmsForMetricResponse_httpStatus,

    -- ** DescribeAnomalyDetectors
    describeAnomalyDetectors_nextToken,
    describeAnomalyDetectors_dimensions,
    describeAnomalyDetectors_maxResults,
    describeAnomalyDetectors_anomalyDetectorTypes,
    describeAnomalyDetectors_metricName,
    describeAnomalyDetectors_namespace,
    describeAnomalyDetectorsResponse_nextToken,
    describeAnomalyDetectorsResponse_anomalyDetectors,
    describeAnomalyDetectorsResponse_httpStatus,

    -- ** DescribeInsightRules
    describeInsightRules_nextToken,
    describeInsightRules_maxResults,
    describeInsightRulesResponse_nextToken,
    describeInsightRulesResponse_insightRules,
    describeInsightRulesResponse_httpStatus,

    -- ** DisableAlarmActions
    disableAlarmActions_alarmNames,

    -- ** DisableInsightRules
    disableInsightRules_ruleNames,
    disableInsightRulesResponse_failures,
    disableInsightRulesResponse_httpStatus,

    -- ** EnableAlarmActions
    enableAlarmActions_alarmNames,

    -- ** EnableInsightRules
    enableInsightRules_ruleNames,
    enableInsightRulesResponse_failures,
    enableInsightRulesResponse_httpStatus,

    -- ** GetDashboard
    getDashboard_dashboardName,
    getDashboardResponse_dashboardBody,
    getDashboardResponse_dashboardName,
    getDashboardResponse_dashboardArn,
    getDashboardResponse_httpStatus,

    -- ** GetInsightRuleReport
    getInsightRuleReport_metrics,
    getInsightRuleReport_maxContributorCount,
    getInsightRuleReport_orderBy,
    getInsightRuleReport_ruleName,
    getInsightRuleReport_startTime,
    getInsightRuleReport_endTime,
    getInsightRuleReport_period,
    getInsightRuleReportResponse_aggregationStatistic,
    getInsightRuleReportResponse_metricDatapoints,
    getInsightRuleReportResponse_aggregateValue,
    getInsightRuleReportResponse_keyLabels,
    getInsightRuleReportResponse_approximateUniqueCount,
    getInsightRuleReportResponse_contributors,
    getInsightRuleReportResponse_httpStatus,

    -- ** GetMetricData
    getMetricData_nextToken,
    getMetricData_labelOptions,
    getMetricData_scanBy,
    getMetricData_maxDatapoints,
    getMetricData_metricDataQueries,
    getMetricData_startTime,
    getMetricData_endTime,
    getMetricDataResponse_nextToken,
    getMetricDataResponse_messages,
    getMetricDataResponse_metricDataResults,
    getMetricDataResponse_httpStatus,

    -- ** GetMetricStatistics
    getMetricStatistics_statistics,
    getMetricStatistics_dimensions,
    getMetricStatistics_extendedStatistics,
    getMetricStatistics_unit,
    getMetricStatistics_namespace,
    getMetricStatistics_metricName,
    getMetricStatistics_startTime,
    getMetricStatistics_endTime,
    getMetricStatistics_period,
    getMetricStatisticsResponse_datapoints,
    getMetricStatisticsResponse_label,
    getMetricStatisticsResponse_httpStatus,

    -- ** GetMetricStream
    getMetricStream_name,
    getMetricStreamResponse_name,
    getMetricStreamResponse_roleArn,
    getMetricStreamResponse_statisticsConfigurations,
    getMetricStreamResponse_arn,
    getMetricStreamResponse_state,
    getMetricStreamResponse_creationDate,
    getMetricStreamResponse_lastUpdateDate,
    getMetricStreamResponse_outputFormat,
    getMetricStreamResponse_excludeFilters,
    getMetricStreamResponse_includeFilters,
    getMetricStreamResponse_firehoseArn,
    getMetricStreamResponse_httpStatus,

    -- ** GetMetricWidgetImage
    getMetricWidgetImage_outputFormat,
    getMetricWidgetImage_metricWidget,
    getMetricWidgetImageResponse_metricWidgetImage,
    getMetricWidgetImageResponse_httpStatus,

    -- ** ListDashboards
    listDashboards_nextToken,
    listDashboards_dashboardNamePrefix,
    listDashboardsResponse_nextToken,
    listDashboardsResponse_dashboardEntries,
    listDashboardsResponse_httpStatus,

    -- ** ListManagedInsightRules
    listManagedInsightRules_nextToken,
    listManagedInsightRules_maxResults,
    listManagedInsightRules_resourceARN,
    listManagedInsightRulesResponse_nextToken,
    listManagedInsightRulesResponse_managedRules,
    listManagedInsightRulesResponse_httpStatus,

    -- ** ListMetricStreams
    listMetricStreams_nextToken,
    listMetricStreams_maxResults,
    listMetricStreamsResponse_nextToken,
    listMetricStreamsResponse_entries,
    listMetricStreamsResponse_httpStatus,

    -- ** ListMetrics
    listMetrics_nextToken,
    listMetrics_dimensions,
    listMetrics_metricName,
    listMetrics_namespace,
    listMetrics_recentlyActive,
    listMetricsResponse_nextToken,
    listMetricsResponse_metrics,
    listMetricsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutAnomalyDetector
    putAnomalyDetector_singleMetricAnomalyDetector,
    putAnomalyDetector_configuration,
    putAnomalyDetector_dimensions,
    putAnomalyDetector_metricMathAnomalyDetector,
    putAnomalyDetector_stat,
    putAnomalyDetector_metricName,
    putAnomalyDetector_namespace,
    putAnomalyDetectorResponse_httpStatus,

    -- ** PutCompositeAlarm
    putCompositeAlarm_tags,
    putCompositeAlarm_alarmActions,
    putCompositeAlarm_actionsSuppressorExtensionPeriod,
    putCompositeAlarm_alarmDescription,
    putCompositeAlarm_actionsEnabled,
    putCompositeAlarm_insufficientDataActions,
    putCompositeAlarm_oKActions,
    putCompositeAlarm_actionsSuppressor,
    putCompositeAlarm_actionsSuppressorWaitPeriod,
    putCompositeAlarm_alarmName,
    putCompositeAlarm_alarmRule,

    -- ** PutDashboard
    putDashboard_dashboardName,
    putDashboard_dashboardBody,
    putDashboardResponse_dashboardValidationMessages,
    putDashboardResponse_httpStatus,

    -- ** PutInsightRule
    putInsightRule_tags,
    putInsightRule_ruleState,
    putInsightRule_ruleName,
    putInsightRule_ruleDefinition,
    putInsightRuleResponse_httpStatus,

    -- ** PutManagedInsightRules
    putManagedInsightRules_managedRules,
    putManagedInsightRulesResponse_failures,
    putManagedInsightRulesResponse_httpStatus,

    -- ** PutMetricAlarm
    putMetricAlarm_tags,
    putMetricAlarm_alarmActions,
    putMetricAlarm_alarmDescription,
    putMetricAlarm_extendedStatistic,
    putMetricAlarm_actionsEnabled,
    putMetricAlarm_period,
    putMetricAlarm_evaluateLowSampleCountPercentile,
    putMetricAlarm_dimensions,
    putMetricAlarm_thresholdMetricId,
    putMetricAlarm_treatMissingData,
    putMetricAlarm_metrics,
    putMetricAlarm_datapointsToAlarm,
    putMetricAlarm_insufficientDataActions,
    putMetricAlarm_metricName,
    putMetricAlarm_threshold,
    putMetricAlarm_oKActions,
    putMetricAlarm_namespace,
    putMetricAlarm_statistic,
    putMetricAlarm_unit,
    putMetricAlarm_alarmName,
    putMetricAlarm_evaluationPeriods,
    putMetricAlarm_comparisonOperator,

    -- ** PutMetricData
    putMetricData_namespace,
    putMetricData_metricData,

    -- ** PutMetricStream
    putMetricStream_tags,
    putMetricStream_statisticsConfigurations,
    putMetricStream_excludeFilters,
    putMetricStream_includeFilters,
    putMetricStream_name,
    putMetricStream_firehoseArn,
    putMetricStream_roleArn,
    putMetricStream_outputFormat,
    putMetricStreamResponse_arn,
    putMetricStreamResponse_httpStatus,

    -- ** SetAlarmState
    setAlarmState_stateReasonData,
    setAlarmState_alarmName,
    setAlarmState_stateValue,
    setAlarmState_stateReason,

    -- ** StartMetricStreams
    startMetricStreams_names,
    startMetricStreamsResponse_httpStatus,

    -- ** StopMetricStreams
    stopMetricStreams_names,
    stopMetricStreamsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** AlarmHistoryItem
    alarmHistoryItem_alarmType,
    alarmHistoryItem_timestamp,
    alarmHistoryItem_historyData,
    alarmHistoryItem_historyItemType,
    alarmHistoryItem_historySummary,
    alarmHistoryItem_alarmName,

    -- ** AnomalyDetector
    anomalyDetector_singleMetricAnomalyDetector,
    anomalyDetector_configuration,
    anomalyDetector_dimensions,
    anomalyDetector_metricMathAnomalyDetector,
    anomalyDetector_stat,
    anomalyDetector_metricName,
    anomalyDetector_stateValue,
    anomalyDetector_namespace,

    -- ** AnomalyDetectorConfiguration
    anomalyDetectorConfiguration_metricTimezone,
    anomalyDetectorConfiguration_excludedTimeRanges,

    -- ** CompositeAlarm
    compositeAlarm_alarmActions,
    compositeAlarm_stateUpdatedTimestamp,
    compositeAlarm_actionsSuppressorExtensionPeriod,
    compositeAlarm_alarmDescription,
    compositeAlarm_actionsEnabled,
    compositeAlarm_actionsSuppressedBy,
    compositeAlarm_insufficientDataActions,
    compositeAlarm_stateTransitionedTimestamp,
    compositeAlarm_alarmArn,
    compositeAlarm_alarmConfigurationUpdatedTimestamp,
    compositeAlarm_stateValue,
    compositeAlarm_stateReasonData,
    compositeAlarm_oKActions,
    compositeAlarm_actionsSuppressor,
    compositeAlarm_actionsSuppressorWaitPeriod,
    compositeAlarm_alarmName,
    compositeAlarm_actionsSuppressedReason,
    compositeAlarm_alarmRule,
    compositeAlarm_stateReason,

    -- ** DashboardEntry
    dashboardEntry_size,
    dashboardEntry_lastModified,
    dashboardEntry_dashboardName,
    dashboardEntry_dashboardArn,

    -- ** DashboardValidationMessage
    dashboardValidationMessage_message,
    dashboardValidationMessage_dataPath,

    -- ** Datapoint
    datapoint_minimum,
    datapoint_average,
    datapoint_timestamp,
    datapoint_sampleCount,
    datapoint_sum,
    datapoint_extendedStatistics,
    datapoint_maximum,
    datapoint_unit,

    -- ** Dimension
    dimension_name,
    dimension_value,

    -- ** DimensionFilter
    dimensionFilter_value,
    dimensionFilter_name,

    -- ** InsightRule
    insightRule_managedRule,
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
    insightRuleMetricDatapoint_maxContributorValue,
    insightRuleMetricDatapoint_average,
    insightRuleMetricDatapoint_sampleCount,
    insightRuleMetricDatapoint_uniqueContributors,
    insightRuleMetricDatapoint_sum,
    insightRuleMetricDatapoint_maximum,
    insightRuleMetricDatapoint_timestamp,

    -- ** LabelOptions
    labelOptions_timezone,

    -- ** ManagedRule
    managedRule_tags,
    managedRule_templateName,
    managedRule_resourceARN,

    -- ** ManagedRuleDescription
    managedRuleDescription_templateName,
    managedRuleDescription_ruleState,
    managedRuleDescription_resourceARN,

    -- ** ManagedRuleState
    managedRuleState_ruleName,
    managedRuleState_state,

    -- ** MessageData
    messageData_code,
    messageData_value,

    -- ** Metric
    metric_dimensions,
    metric_metricName,
    metric_namespace,

    -- ** MetricAlarm
    metricAlarm_alarmActions,
    metricAlarm_stateUpdatedTimestamp,
    metricAlarm_alarmDescription,
    metricAlarm_extendedStatistic,
    metricAlarm_actionsEnabled,
    metricAlarm_period,
    metricAlarm_evaluateLowSampleCountPercentile,
    metricAlarm_dimensions,
    metricAlarm_thresholdMetricId,
    metricAlarm_treatMissingData,
    metricAlarm_metrics,
    metricAlarm_evaluationPeriods,
    metricAlarm_datapointsToAlarm,
    metricAlarm_insufficientDataActions,
    metricAlarm_alarmArn,
    metricAlarm_metricName,
    metricAlarm_alarmConfigurationUpdatedTimestamp,
    metricAlarm_threshold,
    metricAlarm_stateValue,
    metricAlarm_stateReasonData,
    metricAlarm_oKActions,
    metricAlarm_alarmName,
    metricAlarm_comparisonOperator,
    metricAlarm_namespace,
    metricAlarm_statistic,
    metricAlarm_unit,
    metricAlarm_stateReason,

    -- ** MetricDataQuery
    metricDataQuery_metricStat,
    metricDataQuery_returnData,
    metricDataQuery_label,
    metricDataQuery_period,
    metricDataQuery_expression,
    metricDataQuery_accountId,
    metricDataQuery_id,

    -- ** MetricDataResult
    metricDataResult_timestamps,
    metricDataResult_label,
    metricDataResult_id,
    metricDataResult_messages,
    metricDataResult_values,
    metricDataResult_statusCode,

    -- ** MetricDatum
    metricDatum_statisticValues,
    metricDatum_dimensions,
    metricDatum_timestamp,
    metricDatum_counts,
    metricDatum_values,
    metricDatum_unit,
    metricDatum_storageResolution,
    metricDatum_value,
    metricDatum_metricName,

    -- ** MetricMathAnomalyDetector
    metricMathAnomalyDetector_metricDataQueries,

    -- ** MetricStat
    metricStat_unit,
    metricStat_metric,
    metricStat_period,
    metricStat_stat,

    -- ** MetricStreamEntry
    metricStreamEntry_name,
    metricStreamEntry_arn,
    metricStreamEntry_state,
    metricStreamEntry_creationDate,
    metricStreamEntry_lastUpdateDate,
    metricStreamEntry_outputFormat,
    metricStreamEntry_firehoseArn,

    -- ** MetricStreamFilter
    metricStreamFilter_namespace,

    -- ** MetricStreamStatisticsConfiguration
    metricStreamStatisticsConfiguration_includeMetrics,
    metricStreamStatisticsConfiguration_additionalStatistics,

    -- ** MetricStreamStatisticsMetric
    metricStreamStatisticsMetric_namespace,
    metricStreamStatisticsMetric_metricName,

    -- ** PartialFailure
    partialFailure_failureDescription,
    partialFailure_failureCode,
    partialFailure_exceptionType,
    partialFailure_failureResource,

    -- ** Range
    range_startTime,
    range_endTime,

    -- ** SingleMetricAnomalyDetector
    singleMetricAnomalyDetector_dimensions,
    singleMetricAnomalyDetector_stat,
    singleMetricAnomalyDetector_metricName,
    singleMetricAnomalyDetector_namespace,

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
import Amazonka.CloudWatch.ListManagedInsightRules
import Amazonka.CloudWatch.ListMetricStreams
import Amazonka.CloudWatch.ListMetrics
import Amazonka.CloudWatch.ListTagsForResource
import Amazonka.CloudWatch.PutAnomalyDetector
import Amazonka.CloudWatch.PutCompositeAlarm
import Amazonka.CloudWatch.PutDashboard
import Amazonka.CloudWatch.PutInsightRule
import Amazonka.CloudWatch.PutManagedInsightRules
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
import Amazonka.CloudWatch.Types.ManagedRule
import Amazonka.CloudWatch.Types.ManagedRuleDescription
import Amazonka.CloudWatch.Types.ManagedRuleState
import Amazonka.CloudWatch.Types.MessageData
import Amazonka.CloudWatch.Types.Metric
import Amazonka.CloudWatch.Types.MetricAlarm
import Amazonka.CloudWatch.Types.MetricDataQuery
import Amazonka.CloudWatch.Types.MetricDataResult
import Amazonka.CloudWatch.Types.MetricDatum
import Amazonka.CloudWatch.Types.MetricMathAnomalyDetector
import Amazonka.CloudWatch.Types.MetricStat
import Amazonka.CloudWatch.Types.MetricStreamEntry
import Amazonka.CloudWatch.Types.MetricStreamFilter
import Amazonka.CloudWatch.Types.MetricStreamStatisticsConfiguration
import Amazonka.CloudWatch.Types.MetricStreamStatisticsMetric
import Amazonka.CloudWatch.Types.PartialFailure
import Amazonka.CloudWatch.Types.Range
import Amazonka.CloudWatch.Types.SingleMetricAnomalyDetector
import Amazonka.CloudWatch.Types.StatisticSet
import Amazonka.CloudWatch.Types.Tag
import Amazonka.CloudWatch.UntagResource

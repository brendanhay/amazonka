{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchLogs.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Lens
  ( -- * Operations

    -- ** GetLogGroupFields
    getLogGroupFields_time,
    getLogGroupFields_logGroupName,
    getLogGroupFieldsResponse_logGroupFields,
    getLogGroupFieldsResponse_httpStatus,

    -- ** GetLogRecord
    getLogRecord_logRecordPointer,
    getLogRecordResponse_logRecord,
    getLogRecordResponse_httpStatus,

    -- ** DescribeDestinations
    describeDestinations_nextToken,
    describeDestinations_limit,
    describeDestinations_destinationNamePrefix,
    describeDestinationsResponse_nextToken,
    describeDestinationsResponse_destinations,
    describeDestinationsResponse_httpStatus,

    -- ** UntagLogGroup
    untagLogGroup_logGroupName,
    untagLogGroup_tags,

    -- ** StopQuery
    stopQuery_queryId,
    stopQueryResponse_success,
    stopQueryResponse_httpStatus,

    -- ** CreateExportTask
    createExportTask_destinationPrefix,
    createExportTask_taskName,
    createExportTask_logStreamNamePrefix,
    createExportTask_logGroupName,
    createExportTask_from,
    createExportTask_to,
    createExportTask_destination,
    createExportTaskResponse_taskId,
    createExportTaskResponse_httpStatus,

    -- ** PutDestination
    putDestination_destinationName,
    putDestination_targetArn,
    putDestination_roleArn,
    putDestinationResponse_destination,
    putDestinationResponse_httpStatus,

    -- ** DescribeSubscriptionFilters
    describeSubscriptionFilters_filterNamePrefix,
    describeSubscriptionFilters_nextToken,
    describeSubscriptionFilters_limit,
    describeSubscriptionFilters_logGroupName,
    describeSubscriptionFiltersResponse_subscriptionFilters,
    describeSubscriptionFiltersResponse_nextToken,
    describeSubscriptionFiltersResponse_httpStatus,

    -- ** GetLogEvents
    getLogEvents_startTime,
    getLogEvents_startFromHead,
    getLogEvents_nextToken,
    getLogEvents_endTime,
    getLogEvents_limit,
    getLogEvents_logGroupName,
    getLogEvents_logStreamName,
    getLogEventsResponse_nextBackwardToken,
    getLogEventsResponse_nextForwardToken,
    getLogEventsResponse_events,
    getLogEventsResponse_httpStatus,

    -- ** DescribeLogGroups
    describeLogGroups_logGroupNamePrefix,
    describeLogGroups_nextToken,
    describeLogGroups_limit,
    describeLogGroupsResponse_logGroups,
    describeLogGroupsResponse_nextToken,
    describeLogGroupsResponse_httpStatus,

    -- ** DeleteDestination
    deleteDestination_destinationName,

    -- ** DisassociateKmsKey
    disassociateKmsKey_logGroupName,

    -- ** FilterLogEvents
    filterLogEvents_startTime,
    filterLogEvents_nextToken,
    filterLogEvents_logStreamNames,
    filterLogEvents_logStreamNamePrefix,
    filterLogEvents_endTime,
    filterLogEvents_limit,
    filterLogEvents_filterPattern,
    filterLogEvents_interleaved,
    filterLogEvents_logGroupName,
    filterLogEventsResponse_searchedLogStreams,
    filterLogEventsResponse_nextToken,
    filterLogEventsResponse_events,
    filterLogEventsResponse_httpStatus,

    -- ** DeleteQueryDefinition
    deleteQueryDefinition_queryDefinitionId,
    deleteQueryDefinitionResponse_success,
    deleteQueryDefinitionResponse_httpStatus,

    -- ** PutQueryDefinition
    putQueryDefinition_logGroupNames,
    putQueryDefinition_queryDefinitionId,
    putQueryDefinition_name,
    putQueryDefinition_queryString,
    putQueryDefinitionResponse_queryDefinitionId,
    putQueryDefinitionResponse_httpStatus,

    -- ** TagLogGroup
    tagLogGroup_logGroupName,
    tagLogGroup_tags,

    -- ** DescribeResourcePolicies
    describeResourcePolicies_nextToken,
    describeResourcePolicies_limit,
    describeResourcePoliciesResponse_resourcePolicies,
    describeResourcePoliciesResponse_nextToken,
    describeResourcePoliciesResponse_httpStatus,

    -- ** DescribeQueryDefinitions
    describeQueryDefinitions_queryDefinitionNamePrefix,
    describeQueryDefinitions_nextToken,
    describeQueryDefinitions_maxResults,
    describeQueryDefinitionsResponse_queryDefinitions,
    describeQueryDefinitionsResponse_nextToken,
    describeQueryDefinitionsResponse_httpStatus,

    -- ** DeleteLogStream
    deleteLogStream_logGroupName,
    deleteLogStream_logStreamName,

    -- ** DescribeQueries
    describeQueries_status,
    describeQueries_logGroupName,
    describeQueries_nextToken,
    describeQueries_maxResults,
    describeQueriesResponse_queries,
    describeQueriesResponse_nextToken,
    describeQueriesResponse_httpStatus,

    -- ** CreateLogStream
    createLogStream_logGroupName,
    createLogStream_logStreamName,

    -- ** CreateLogGroup
    createLogGroup_kmsKeyId,
    createLogGroup_tags,
    createLogGroup_logGroupName,

    -- ** DescribeExportTasks
    describeExportTasks_taskId,
    describeExportTasks_nextToken,
    describeExportTasks_limit,
    describeExportTasks_statusCode,
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,

    -- ** CancelExportTask
    cancelExportTask_taskId,

    -- ** PutSubscriptionFilter
    putSubscriptionFilter_distribution,
    putSubscriptionFilter_roleArn,
    putSubscriptionFilter_logGroupName,
    putSubscriptionFilter_filterName,
    putSubscriptionFilter_filterPattern,
    putSubscriptionFilter_destinationArn,

    -- ** StartQuery
    startQuery_logGroupNames,
    startQuery_logGroupName,
    startQuery_limit,
    startQuery_startTime,
    startQuery_endTime,
    startQuery_queryString,
    startQueryResponse_queryId,
    startQueryResponse_httpStatus,

    -- ** DeleteLogGroup
    deleteLogGroup_logGroupName,

    -- ** DeleteSubscriptionFilter
    deleteSubscriptionFilter_logGroupName,
    deleteSubscriptionFilter_filterName,

    -- ** PutLogEvents
    putLogEvents_sequenceToken,
    putLogEvents_logGroupName,
    putLogEvents_logStreamName,
    putLogEvents_logEvents,
    putLogEventsResponse_rejectedLogEventsInfo,
    putLogEventsResponse_nextSequenceToken,
    putLogEventsResponse_httpStatus,

    -- ** DescribeMetricFilters
    describeMetricFilters_filterNamePrefix,
    describeMetricFilters_metricName,
    describeMetricFilters_logGroupName,
    describeMetricFilters_nextToken,
    describeMetricFilters_metricNamespace,
    describeMetricFilters_limit,
    describeMetricFiltersResponse_nextToken,
    describeMetricFiltersResponse_metricFilters,
    describeMetricFiltersResponse_httpStatus,

    -- ** TestMetricFilter
    testMetricFilter_filterPattern,
    testMetricFilter_logEventMessages,
    testMetricFilterResponse_matches,
    testMetricFilterResponse_httpStatus,

    -- ** PutDestinationPolicy
    putDestinationPolicy_destinationName,
    putDestinationPolicy_accessPolicy,

    -- ** PutMetricFilter
    putMetricFilter_logGroupName,
    putMetricFilter_filterName,
    putMetricFilter_filterPattern,
    putMetricFilter_metricTransformations,

    -- ** DeleteRetentionPolicy
    deleteRetentionPolicy_logGroupName,

    -- ** DeleteMetricFilter
    deleteMetricFilter_logGroupName,
    deleteMetricFilter_filterName,

    -- ** PutRetentionPolicy
    putRetentionPolicy_logGroupName,
    putRetentionPolicy_retentionInDays,

    -- ** ListTagsLogGroup
    listTagsLogGroup_logGroupName,
    listTagsLogGroupResponse_tags,
    listTagsLogGroupResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policyName,
    putResourcePolicy_policyDocument,
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_policyName,

    -- ** AssociateKmsKey
    associateKmsKey_logGroupName,
    associateKmsKey_kmsKeyId,

    -- ** GetQueryResults
    getQueryResults_queryId,
    getQueryResultsResponse_status,
    getQueryResultsResponse_results,
    getQueryResultsResponse_statistics,
    getQueryResultsResponse_httpStatus,

    -- ** DescribeLogStreams
    describeLogStreams_orderBy,
    describeLogStreams_descending,
    describeLogStreams_nextToken,
    describeLogStreams_logStreamNamePrefix,
    describeLogStreams_limit,
    describeLogStreams_logGroupName,
    describeLogStreamsResponse_nextToken,
    describeLogStreamsResponse_logStreams,
    describeLogStreamsResponse_httpStatus,

    -- * Types

    -- ** Destination
    destination_targetArn,
    destination_creationTime,
    destination_arn,
    destination_accessPolicy,
    destination_destinationName,
    destination_roleArn,

    -- ** ExportTask
    exportTask_destinationPrefix,
    exportTask_destination,
    exportTask_status,
    exportTask_taskName,
    exportTask_taskId,
    exportTask_to,
    exportTask_from,
    exportTask_logGroupName,
    exportTask_executionInfo,

    -- ** ExportTaskExecutionInfo
    exportTaskExecutionInfo_creationTime,
    exportTaskExecutionInfo_completionTime,

    -- ** ExportTaskStatus
    exportTaskStatus_code,
    exportTaskStatus_message,

    -- ** FilteredLogEvent
    filteredLogEvent_ingestionTime,
    filteredLogEvent_logStreamName,
    filteredLogEvent_message,
    filteredLogEvent_timestamp,
    filteredLogEvent_eventId,

    -- ** InputLogEvent
    inputLogEvent_timestamp,
    inputLogEvent_message,

    -- ** LogGroup
    logGroup_creationTime,
    logGroup_metricFilterCount,
    logGroup_arn,
    logGroup_logGroupName,
    logGroup_retentionInDays,
    logGroup_kmsKeyId,
    logGroup_storedBytes,

    -- ** LogGroupField
    logGroupField_percent,
    logGroupField_name,

    -- ** LogStream
    logStream_creationTime,
    logStream_uploadSequenceToken,
    logStream_arn,
    logStream_firstEventTimestamp,
    logStream_logStreamName,
    logStream_storedBytes,
    logStream_lastIngestionTime,
    logStream_lastEventTimestamp,

    -- ** MetricFilter
    metricFilter_creationTime,
    metricFilter_filterName,
    metricFilter_logGroupName,
    metricFilter_filterPattern,
    metricFilter_metricTransformations,

    -- ** MetricFilterMatchRecord
    metricFilterMatchRecord_extractedValues,
    metricFilterMatchRecord_eventNumber,
    metricFilterMatchRecord_eventMessage,

    -- ** MetricTransformation
    metricTransformation_defaultValue,
    metricTransformation_dimensions,
    metricTransformation_unit,
    metricTransformation_metricName,
    metricTransformation_metricNamespace,
    metricTransformation_metricValue,

    -- ** OutputLogEvent
    outputLogEvent_ingestionTime,
    outputLogEvent_message,
    outputLogEvent_timestamp,

    -- ** QueryDefinition
    queryDefinition_logGroupNames,
    queryDefinition_queryDefinitionId,
    queryDefinition_name,
    queryDefinition_queryString,
    queryDefinition_lastModified,

    -- ** QueryInfo
    queryInfo_status,
    queryInfo_queryId,
    queryInfo_logGroupName,
    queryInfo_queryString,
    queryInfo_createTime,

    -- ** QueryStatistics
    queryStatistics_recordsScanned,
    queryStatistics_bytesScanned,
    queryStatistics_recordsMatched,

    -- ** RejectedLogEventsInfo
    rejectedLogEventsInfo_tooOldLogEventEndIndex,
    rejectedLogEventsInfo_tooNewLogEventStartIndex,
    rejectedLogEventsInfo_expiredLogEventEndIndex,

    -- ** ResourcePolicy
    resourcePolicy_policyName,
    resourcePolicy_policyDocument,
    resourcePolicy_lastUpdatedTime,

    -- ** ResultField
    resultField_field,
    resultField_value,

    -- ** SearchedLogStream
    searchedLogStream_logStreamName,
    searchedLogStream_searchedCompletely,

    -- ** SubscriptionFilter
    subscriptionFilter_creationTime,
    subscriptionFilter_filterName,
    subscriptionFilter_distribution,
    subscriptionFilter_destinationArn,
    subscriptionFilter_logGroupName,
    subscriptionFilter_filterPattern,
    subscriptionFilter_roleArn,
  )
where

import Amazonka.CloudWatchLogs.AssociateKmsKey
import Amazonka.CloudWatchLogs.CancelExportTask
import Amazonka.CloudWatchLogs.CreateExportTask
import Amazonka.CloudWatchLogs.CreateLogGroup
import Amazonka.CloudWatchLogs.CreateLogStream
import Amazonka.CloudWatchLogs.DeleteDestination
import Amazonka.CloudWatchLogs.DeleteLogGroup
import Amazonka.CloudWatchLogs.DeleteLogStream
import Amazonka.CloudWatchLogs.DeleteMetricFilter
import Amazonka.CloudWatchLogs.DeleteQueryDefinition
import Amazonka.CloudWatchLogs.DeleteResourcePolicy
import Amazonka.CloudWatchLogs.DeleteRetentionPolicy
import Amazonka.CloudWatchLogs.DeleteSubscriptionFilter
import Amazonka.CloudWatchLogs.DescribeDestinations
import Amazonka.CloudWatchLogs.DescribeExportTasks
import Amazonka.CloudWatchLogs.DescribeLogGroups
import Amazonka.CloudWatchLogs.DescribeLogStreams
import Amazonka.CloudWatchLogs.DescribeMetricFilters
import Amazonka.CloudWatchLogs.DescribeQueries
import Amazonka.CloudWatchLogs.DescribeQueryDefinitions
import Amazonka.CloudWatchLogs.DescribeResourcePolicies
import Amazonka.CloudWatchLogs.DescribeSubscriptionFilters
import Amazonka.CloudWatchLogs.DisassociateKmsKey
import Amazonka.CloudWatchLogs.FilterLogEvents
import Amazonka.CloudWatchLogs.GetLogEvents
import Amazonka.CloudWatchLogs.GetLogGroupFields
import Amazonka.CloudWatchLogs.GetLogRecord
import Amazonka.CloudWatchLogs.GetQueryResults
import Amazonka.CloudWatchLogs.ListTagsLogGroup
import Amazonka.CloudWatchLogs.PutDestination
import Amazonka.CloudWatchLogs.PutDestinationPolicy
import Amazonka.CloudWatchLogs.PutLogEvents
import Amazonka.CloudWatchLogs.PutMetricFilter
import Amazonka.CloudWatchLogs.PutQueryDefinition
import Amazonka.CloudWatchLogs.PutResourcePolicy
import Amazonka.CloudWatchLogs.PutRetentionPolicy
import Amazonka.CloudWatchLogs.PutSubscriptionFilter
import Amazonka.CloudWatchLogs.StartQuery
import Amazonka.CloudWatchLogs.StopQuery
import Amazonka.CloudWatchLogs.TagLogGroup
import Amazonka.CloudWatchLogs.TestMetricFilter
import Amazonka.CloudWatchLogs.Types.Destination
import Amazonka.CloudWatchLogs.Types.ExportTask
import Amazonka.CloudWatchLogs.Types.ExportTaskExecutionInfo
import Amazonka.CloudWatchLogs.Types.ExportTaskStatus
import Amazonka.CloudWatchLogs.Types.FilteredLogEvent
import Amazonka.CloudWatchLogs.Types.InputLogEvent
import Amazonka.CloudWatchLogs.Types.LogGroup
import Amazonka.CloudWatchLogs.Types.LogGroupField
import Amazonka.CloudWatchLogs.Types.LogStream
import Amazonka.CloudWatchLogs.Types.MetricFilter
import Amazonka.CloudWatchLogs.Types.MetricFilterMatchRecord
import Amazonka.CloudWatchLogs.Types.MetricTransformation
import Amazonka.CloudWatchLogs.Types.OutputLogEvent
import Amazonka.CloudWatchLogs.Types.QueryDefinition
import Amazonka.CloudWatchLogs.Types.QueryInfo
import Amazonka.CloudWatchLogs.Types.QueryStatistics
import Amazonka.CloudWatchLogs.Types.RejectedLogEventsInfo
import Amazonka.CloudWatchLogs.Types.ResourcePolicy
import Amazonka.CloudWatchLogs.Types.ResultField
import Amazonka.CloudWatchLogs.Types.SearchedLogStream
import Amazonka.CloudWatchLogs.Types.SubscriptionFilter
import Amazonka.CloudWatchLogs.UntagLogGroup

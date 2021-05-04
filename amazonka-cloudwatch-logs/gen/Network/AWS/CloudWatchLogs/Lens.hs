{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Lens
  ( -- * Operations

    -- ** GetLogRecord
    getLogRecord_logRecordPointer,
    getLogRecordResponse_logRecord,
    getLogRecordResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_nextToken,
    describeExportTasks_taskId,
    describeExportTasks_statusCode,
    describeExportTasks_limit,
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,

    -- ** CreateLogStream
    createLogStream_logGroupName,
    createLogStream_logStreamName,

    -- ** DescribeResourcePolicies
    describeResourcePolicies_nextToken,
    describeResourcePolicies_limit,
    describeResourcePoliciesResponse_nextToken,
    describeResourcePoliciesResponse_resourcePolicies,
    describeResourcePoliciesResponse_httpStatus,

    -- ** DescribeQueryDefinitions
    describeQueryDefinitions_nextToken,
    describeQueryDefinitions_maxResults,
    describeQueryDefinitions_queryDefinitionNamePrefix,
    describeQueryDefinitionsResponse_nextToken,
    describeQueryDefinitionsResponse_queryDefinitions,
    describeQueryDefinitionsResponse_httpStatus,

    -- ** DeleteQueryDefinition
    deleteQueryDefinition_queryDefinitionId,
    deleteQueryDefinitionResponse_success,
    deleteQueryDefinitionResponse_httpStatus,

    -- ** DescribeLogStreams
    describeLogStreams_logStreamNamePrefix,
    describeLogStreams_nextToken,
    describeLogStreams_orderBy,
    describeLogStreams_descending,
    describeLogStreams_limit,
    describeLogStreams_logGroupName,
    describeLogStreamsResponse_nextToken,
    describeLogStreamsResponse_logStreams,
    describeLogStreamsResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policyName,
    putResourcePolicy_policyDocument,
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,

    -- ** DisassociateKmsKey
    disassociateKmsKey_logGroupName,

    -- ** DescribeSubscriptionFilters
    describeSubscriptionFilters_filterNamePrefix,
    describeSubscriptionFilters_nextToken,
    describeSubscriptionFilters_limit,
    describeSubscriptionFilters_logGroupName,
    describeSubscriptionFiltersResponse_nextToken,
    describeSubscriptionFiltersResponse_subscriptionFilters,
    describeSubscriptionFiltersResponse_httpStatus,

    -- ** DescribeLogGroups
    describeLogGroups_nextToken,
    describeLogGroups_logGroupNamePrefix,
    describeLogGroups_limit,
    describeLogGroupsResponse_nextToken,
    describeLogGroupsResponse_logGroups,
    describeLogGroupsResponse_httpStatus,

    -- ** PutRetentionPolicy
    putRetentionPolicy_logGroupName,
    putRetentionPolicy_retentionInDays,

    -- ** PutDestinationPolicy
    putDestinationPolicy_destinationName,
    putDestinationPolicy_accessPolicy,

    -- ** DeleteDestination
    deleteDestination_destinationName,

    -- ** DeleteMetricFilter
    deleteMetricFilter_logGroupName,
    deleteMetricFilter_filterName,

    -- ** DescribeDestinations
    describeDestinations_nextToken,
    describeDestinations_destinationNamePrefix,
    describeDestinations_limit,
    describeDestinationsResponse_nextToken,
    describeDestinationsResponse_destinations,
    describeDestinationsResponse_httpStatus,

    -- ** PutSubscriptionFilter
    putSubscriptionFilter_roleArn,
    putSubscriptionFilter_distribution,
    putSubscriptionFilter_logGroupName,
    putSubscriptionFilter_filterName,
    putSubscriptionFilter_filterPattern,
    putSubscriptionFilter_destinationArn,

    -- ** DescribeMetricFilters
    describeMetricFilters_filterNamePrefix,
    describeMetricFilters_metricNamespace,
    describeMetricFilters_nextToken,
    describeMetricFilters_metricName,
    describeMetricFilters_logGroupName,
    describeMetricFilters_limit,
    describeMetricFiltersResponse_metricFilters,
    describeMetricFiltersResponse_nextToken,
    describeMetricFiltersResponse_httpStatus,

    -- ** CancelExportTask
    cancelExportTask_taskId,

    -- ** CreateLogGroup
    createLogGroup_kmsKeyId,
    createLogGroup_tags,
    createLogGroup_logGroupName,

    -- ** GetLogGroupFields
    getLogGroupFields_time,
    getLogGroupFields_logGroupName,
    getLogGroupFieldsResponse_logGroupFields,
    getLogGroupFieldsResponse_httpStatus,

    -- ** DescribeQueries
    describeQueries_nextToken,
    describeQueries_status,
    describeQueries_maxResults,
    describeQueries_logGroupName,
    describeQueriesResponse_nextToken,
    describeQueriesResponse_queries,
    describeQueriesResponse_httpStatus,

    -- ** DeleteLogStream
    deleteLogStream_logGroupName,
    deleteLogStream_logStreamName,

    -- ** TagLogGroup
    tagLogGroup_logGroupName,
    tagLogGroup_tags,

    -- ** AssociateKmsKey
    associateKmsKey_logGroupName,
    associateKmsKey_kmsKeyId,

    -- ** GetQueryResults
    getQueryResults_queryId,
    getQueryResultsResponse_status,
    getQueryResultsResponse_statistics,
    getQueryResultsResponse_results,
    getQueryResultsResponse_httpStatus,

    -- ** PutQueryDefinition
    putQueryDefinition_logGroupNames,
    putQueryDefinition_queryDefinitionId,
    putQueryDefinition_name,
    putQueryDefinition_queryString,
    putQueryDefinitionResponse_queryDefinitionId,
    putQueryDefinitionResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_policyName,

    -- ** FilterLogEvents
    filterLogEvents_logStreamNamePrefix,
    filterLogEvents_nextToken,
    filterLogEvents_interleaved,
    filterLogEvents_filterPattern,
    filterLogEvents_startTime,
    filterLogEvents_endTime,
    filterLogEvents_logStreamNames,
    filterLogEvents_limit,
    filterLogEvents_logGroupName,
    filterLogEventsResponse_nextToken,
    filterLogEventsResponse_searchedLogStreams,
    filterLogEventsResponse_events,
    filterLogEventsResponse_httpStatus,

    -- ** ListTagsLogGroup
    listTagsLogGroup_logGroupName,
    listTagsLogGroupResponse_tags,
    listTagsLogGroupResponse_httpStatus,

    -- ** DeleteRetentionPolicy
    deleteRetentionPolicy_logGroupName,

    -- ** PutDestination
    putDestination_destinationName,
    putDestination_targetArn,
    putDestination_roleArn,
    putDestinationResponse_destination,
    putDestinationResponse_httpStatus,

    -- ** PutMetricFilter
    putMetricFilter_logGroupName,
    putMetricFilter_filterName,
    putMetricFilter_filterPattern,
    putMetricFilter_metricTransformations,

    -- ** CreateExportTask
    createExportTask_logStreamNamePrefix,
    createExportTask_taskName,
    createExportTask_destinationPrefix,
    createExportTask_logGroupName,
    createExportTask_from,
    createExportTask_to,
    createExportTask_destination,
    createExportTaskResponse_taskId,
    createExportTaskResponse_httpStatus,

    -- ** GetLogEvents
    getLogEvents_nextToken,
    getLogEvents_startFromHead,
    getLogEvents_startTime,
    getLogEvents_endTime,
    getLogEvents_limit,
    getLogEvents_logGroupName,
    getLogEvents_logStreamName,
    getLogEventsResponse_nextBackwardToken,
    getLogEventsResponse_nextForwardToken,
    getLogEventsResponse_events,
    getLogEventsResponse_httpStatus,

    -- ** PutLogEvents
    putLogEvents_sequenceToken,
    putLogEvents_logGroupName,
    putLogEvents_logStreamName,
    putLogEvents_logEvents,
    putLogEventsResponse_nextSequenceToken,
    putLogEventsResponse_rejectedLogEventsInfo,
    putLogEventsResponse_httpStatus,

    -- ** StopQuery
    stopQuery_queryId,
    stopQueryResponse_success,
    stopQueryResponse_httpStatus,

    -- ** DeleteLogGroup
    deleteLogGroup_logGroupName,

    -- ** UntagLogGroup
    untagLogGroup_logGroupName,
    untagLogGroup_tags,

    -- ** TestMetricFilter
    testMetricFilter_filterPattern,
    testMetricFilter_logEventMessages,
    testMetricFilterResponse_matches,
    testMetricFilterResponse_httpStatus,

    -- ** StartQuery
    startQuery_logGroupNames,
    startQuery_logGroupName,
    startQuery_limit,
    startQuery_startTime,
    startQuery_endTime,
    startQuery_queryString,
    startQueryResponse_queryId,
    startQueryResponse_httpStatus,

    -- ** DeleteSubscriptionFilter
    deleteSubscriptionFilter_logGroupName,
    deleteSubscriptionFilter_filterName,

    -- * Types

    -- ** Destination
    destination_creationTime,
    destination_roleArn,
    destination_destinationName,
    destination_arn,
    destination_targetArn,
    destination_accessPolicy,

    -- ** ExportTask
    exportTask_status,
    exportTask_executionInfo,
    exportTask_to,
    exportTask_taskId,
    exportTask_taskName,
    exportTask_logGroupName,
    exportTask_destination,
    exportTask_destinationPrefix,
    exportTask_from,

    -- ** ExportTaskExecutionInfo
    exportTaskExecutionInfo_creationTime,
    exportTaskExecutionInfo_completionTime,

    -- ** ExportTaskStatus
    exportTaskStatus_message,
    exportTaskStatus_code,

    -- ** FilteredLogEvent
    filteredLogEvent_logStreamName,
    filteredLogEvent_eventId,
    filteredLogEvent_message,
    filteredLogEvent_ingestionTime,
    filteredLogEvent_timestamp,

    -- ** InputLogEvent
    inputLogEvent_timestamp,
    inputLogEvent_message,

    -- ** LogGroup
    logGroup_retentionInDays,
    logGroup_creationTime,
    logGroup_arn,
    logGroup_storedBytes,
    logGroup_metricFilterCount,
    logGroup_kmsKeyId,
    logGroup_logGroupName,

    -- ** LogGroupField
    logGroupField_name,
    logGroupField_percent,

    -- ** LogStream
    logStream_logStreamName,
    logStream_creationTime,
    logStream_arn,
    logStream_storedBytes,
    logStream_uploadSequenceToken,
    logStream_firstEventTimestamp,
    logStream_lastEventTimestamp,
    logStream_lastIngestionTime,

    -- ** MetricFilter
    metricFilter_filterName,
    metricFilter_creationTime,
    metricFilter_filterPattern,
    metricFilter_logGroupName,
    metricFilter_metricTransformations,

    -- ** MetricFilterMatchRecord
    metricFilterMatchRecord_eventNumber,
    metricFilterMatchRecord_eventMessage,
    metricFilterMatchRecord_extractedValues,

    -- ** MetricTransformation
    metricTransformation_defaultValue,
    metricTransformation_metricName,
    metricTransformation_metricNamespace,
    metricTransformation_metricValue,

    -- ** OutputLogEvent
    outputLogEvent_message,
    outputLogEvent_ingestionTime,
    outputLogEvent_timestamp,

    -- ** QueryDefinition
    queryDefinition_queryString,
    queryDefinition_name,
    queryDefinition_logGroupNames,
    queryDefinition_lastModified,
    queryDefinition_queryDefinitionId,

    -- ** QueryInfo
    queryInfo_queryString,
    queryInfo_status,
    queryInfo_queryId,
    queryInfo_logGroupName,
    queryInfo_createTime,

    -- ** QueryStatistics
    queryStatistics_bytesScanned,
    queryStatistics_recordsMatched,
    queryStatistics_recordsScanned,

    -- ** RejectedLogEventsInfo
    rejectedLogEventsInfo_tooOldLogEventEndIndex,
    rejectedLogEventsInfo_expiredLogEventEndIndex,
    rejectedLogEventsInfo_tooNewLogEventStartIndex,

    -- ** ResourcePolicy
    resourcePolicy_policyName,
    resourcePolicy_policyDocument,
    resourcePolicy_lastUpdatedTime,

    -- ** ResultField
    resultField_value,
    resultField_field,

    -- ** SearchedLogStream
    searchedLogStream_logStreamName,
    searchedLogStream_searchedCompletely,

    -- ** SubscriptionFilter
    subscriptionFilter_filterName,
    subscriptionFilter_creationTime,
    subscriptionFilter_destinationArn,
    subscriptionFilter_roleArn,
    subscriptionFilter_filterPattern,
    subscriptionFilter_distribution,
    subscriptionFilter_logGroupName,
  )
where

import Network.AWS.CloudWatchLogs.AssociateKmsKey
import Network.AWS.CloudWatchLogs.CancelExportTask
import Network.AWS.CloudWatchLogs.CreateExportTask
import Network.AWS.CloudWatchLogs.CreateLogGroup
import Network.AWS.CloudWatchLogs.CreateLogStream
import Network.AWS.CloudWatchLogs.DeleteDestination
import Network.AWS.CloudWatchLogs.DeleteLogGroup
import Network.AWS.CloudWatchLogs.DeleteLogStream
import Network.AWS.CloudWatchLogs.DeleteMetricFilter
import Network.AWS.CloudWatchLogs.DeleteQueryDefinition
import Network.AWS.CloudWatchLogs.DeleteResourcePolicy
import Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
import Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
import Network.AWS.CloudWatchLogs.DescribeDestinations
import Network.AWS.CloudWatchLogs.DescribeExportTasks
import Network.AWS.CloudWatchLogs.DescribeLogGroups
import Network.AWS.CloudWatchLogs.DescribeLogStreams
import Network.AWS.CloudWatchLogs.DescribeMetricFilters
import Network.AWS.CloudWatchLogs.DescribeQueries
import Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
import Network.AWS.CloudWatchLogs.DescribeResourcePolicies
import Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
import Network.AWS.CloudWatchLogs.DisassociateKmsKey
import Network.AWS.CloudWatchLogs.FilterLogEvents
import Network.AWS.CloudWatchLogs.GetLogEvents
import Network.AWS.CloudWatchLogs.GetLogGroupFields
import Network.AWS.CloudWatchLogs.GetLogRecord
import Network.AWS.CloudWatchLogs.GetQueryResults
import Network.AWS.CloudWatchLogs.ListTagsLogGroup
import Network.AWS.CloudWatchLogs.PutDestination
import Network.AWS.CloudWatchLogs.PutDestinationPolicy
import Network.AWS.CloudWatchLogs.PutLogEvents
import Network.AWS.CloudWatchLogs.PutMetricFilter
import Network.AWS.CloudWatchLogs.PutQueryDefinition
import Network.AWS.CloudWatchLogs.PutResourcePolicy
import Network.AWS.CloudWatchLogs.PutRetentionPolicy
import Network.AWS.CloudWatchLogs.PutSubscriptionFilter
import Network.AWS.CloudWatchLogs.StartQuery
import Network.AWS.CloudWatchLogs.StopQuery
import Network.AWS.CloudWatchLogs.TagLogGroup
import Network.AWS.CloudWatchLogs.TestMetricFilter
import Network.AWS.CloudWatchLogs.Types.Destination
import Network.AWS.CloudWatchLogs.Types.ExportTask
import Network.AWS.CloudWatchLogs.Types.ExportTaskExecutionInfo
import Network.AWS.CloudWatchLogs.Types.ExportTaskStatus
import Network.AWS.CloudWatchLogs.Types.FilteredLogEvent
import Network.AWS.CloudWatchLogs.Types.InputLogEvent
import Network.AWS.CloudWatchLogs.Types.LogGroup
import Network.AWS.CloudWatchLogs.Types.LogGroupField
import Network.AWS.CloudWatchLogs.Types.LogStream
import Network.AWS.CloudWatchLogs.Types.MetricFilter
import Network.AWS.CloudWatchLogs.Types.MetricFilterMatchRecord
import Network.AWS.CloudWatchLogs.Types.MetricTransformation
import Network.AWS.CloudWatchLogs.Types.OutputLogEvent
import Network.AWS.CloudWatchLogs.Types.QueryDefinition
import Network.AWS.CloudWatchLogs.Types.QueryInfo
import Network.AWS.CloudWatchLogs.Types.QueryStatistics
import Network.AWS.CloudWatchLogs.Types.RejectedLogEventsInfo
import Network.AWS.CloudWatchLogs.Types.ResourcePolicy
import Network.AWS.CloudWatchLogs.Types.ResultField
import Network.AWS.CloudWatchLogs.Types.SearchedLogStream
import Network.AWS.CloudWatchLogs.Types.SubscriptionFilter
import Network.AWS.CloudWatchLogs.UntagLogGroup

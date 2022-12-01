{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchLogs.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Lens
  ( -- * Operations

    -- ** AssociateKmsKey
    associateKmsKey_logGroupName,
    associateKmsKey_kmsKeyId,

    -- ** CancelExportTask
    cancelExportTask_taskId,

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

    -- ** CreateLogGroup
    createLogGroup_tags,
    createLogGroup_kmsKeyId,
    createLogGroup_logGroupName,

    -- ** CreateLogStream
    createLogStream_logGroupName,
    createLogStream_logStreamName,

    -- ** DeleteDestination
    deleteDestination_destinationName,

    -- ** DeleteLogGroup
    deleteLogGroup_logGroupName,

    -- ** DeleteLogStream
    deleteLogStream_logGroupName,
    deleteLogStream_logStreamName,

    -- ** DeleteMetricFilter
    deleteMetricFilter_logGroupName,
    deleteMetricFilter_filterName,

    -- ** DeleteQueryDefinition
    deleteQueryDefinition_queryDefinitionId,
    deleteQueryDefinitionResponse_success,
    deleteQueryDefinitionResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_policyName,

    -- ** DeleteRetentionPolicy
    deleteRetentionPolicy_logGroupName,

    -- ** DeleteSubscriptionFilter
    deleteSubscriptionFilter_logGroupName,
    deleteSubscriptionFilter_filterName,

    -- ** DescribeDestinations
    describeDestinations_nextToken,
    describeDestinations_limit,
    describeDestinations_destinationNamePrefix,
    describeDestinationsResponse_nextToken,
    describeDestinationsResponse_destinations,
    describeDestinationsResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_nextToken,
    describeExportTasks_taskId,
    describeExportTasks_limit,
    describeExportTasks_statusCode,
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,

    -- ** DescribeLogGroups
    describeLogGroups_nextToken,
    describeLogGroups_limit,
    describeLogGroups_logGroupNamePrefix,
    describeLogGroupsResponse_nextToken,
    describeLogGroupsResponse_logGroups,
    describeLogGroupsResponse_httpStatus,

    -- ** DescribeLogStreams
    describeLogStreams_nextToken,
    describeLogStreams_limit,
    describeLogStreams_logStreamNamePrefix,
    describeLogStreams_descending,
    describeLogStreams_orderBy,
    describeLogStreams_logGroupName,
    describeLogStreamsResponse_nextToken,
    describeLogStreamsResponse_logStreams,
    describeLogStreamsResponse_httpStatus,

    -- ** DescribeMetricFilters
    describeMetricFilters_nextToken,
    describeMetricFilters_limit,
    describeMetricFilters_metricName,
    describeMetricFilters_filterNamePrefix,
    describeMetricFilters_metricNamespace,
    describeMetricFilters_logGroupName,
    describeMetricFiltersResponse_nextToken,
    describeMetricFiltersResponse_metricFilters,
    describeMetricFiltersResponse_httpStatus,

    -- ** DescribeQueries
    describeQueries_nextToken,
    describeQueries_status,
    describeQueries_maxResults,
    describeQueries_logGroupName,
    describeQueriesResponse_nextToken,
    describeQueriesResponse_queries,
    describeQueriesResponse_httpStatus,

    -- ** DescribeQueryDefinitions
    describeQueryDefinitions_nextToken,
    describeQueryDefinitions_queryDefinitionNamePrefix,
    describeQueryDefinitions_maxResults,
    describeQueryDefinitionsResponse_nextToken,
    describeQueryDefinitionsResponse_queryDefinitions,
    describeQueryDefinitionsResponse_httpStatus,

    -- ** DescribeResourcePolicies
    describeResourcePolicies_nextToken,
    describeResourcePolicies_limit,
    describeResourcePoliciesResponse_nextToken,
    describeResourcePoliciesResponse_resourcePolicies,
    describeResourcePoliciesResponse_httpStatus,

    -- ** DescribeSubscriptionFilters
    describeSubscriptionFilters_nextToken,
    describeSubscriptionFilters_limit,
    describeSubscriptionFilters_filterNamePrefix,
    describeSubscriptionFilters_logGroupName,
    describeSubscriptionFiltersResponse_nextToken,
    describeSubscriptionFiltersResponse_subscriptionFilters,
    describeSubscriptionFiltersResponse_httpStatus,

    -- ** DisassociateKmsKey
    disassociateKmsKey_logGroupName,

    -- ** FilterLogEvents
    filterLogEvents_nextToken,
    filterLogEvents_interleaved,
    filterLogEvents_endTime,
    filterLogEvents_limit,
    filterLogEvents_logStreamNamePrefix,
    filterLogEvents_filterPattern,
    filterLogEvents_startTime,
    filterLogEvents_logStreamNames,
    filterLogEvents_logGroupName,
    filterLogEventsResponse_nextToken,
    filterLogEventsResponse_searchedLogStreams,
    filterLogEventsResponse_events,
    filterLogEventsResponse_httpStatus,

    -- ** GetLogEvents
    getLogEvents_nextToken,
    getLogEvents_startFromHead,
    getLogEvents_endTime,
    getLogEvents_limit,
    getLogEvents_startTime,
    getLogEvents_logGroupName,
    getLogEvents_logStreamName,
    getLogEventsResponse_nextForwardToken,
    getLogEventsResponse_events,
    getLogEventsResponse_nextBackwardToken,
    getLogEventsResponse_httpStatus,

    -- ** GetLogGroupFields
    getLogGroupFields_time,
    getLogGroupFields_logGroupName,
    getLogGroupFieldsResponse_logGroupFields,
    getLogGroupFieldsResponse_httpStatus,

    -- ** GetLogRecord
    getLogRecord_logRecordPointer,
    getLogRecordResponse_logRecord,
    getLogRecordResponse_httpStatus,

    -- ** GetQueryResults
    getQueryResults_queryId,
    getQueryResultsResponse_statistics,
    getQueryResultsResponse_status,
    getQueryResultsResponse_results,
    getQueryResultsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutDestination
    putDestination_tags,
    putDestination_destinationName,
    putDestination_targetArn,
    putDestination_roleArn,
    putDestinationResponse_destination,
    putDestinationResponse_httpStatus,

    -- ** PutDestinationPolicy
    putDestinationPolicy_forceUpdate,
    putDestinationPolicy_destinationName,
    putDestinationPolicy_accessPolicy,

    -- ** PutLogEvents
    putLogEvents_sequenceToken,
    putLogEvents_logGroupName,
    putLogEvents_logStreamName,
    putLogEvents_logEvents,
    putLogEventsResponse_nextSequenceToken,
    putLogEventsResponse_rejectedLogEventsInfo,
    putLogEventsResponse_httpStatus,

    -- ** PutMetricFilter
    putMetricFilter_logGroupName,
    putMetricFilter_filterName,
    putMetricFilter_filterPattern,
    putMetricFilter_metricTransformations,

    -- ** PutQueryDefinition
    putQueryDefinition_queryDefinitionId,
    putQueryDefinition_logGroupNames,
    putQueryDefinition_name,
    putQueryDefinition_queryString,
    putQueryDefinitionResponse_queryDefinitionId,
    putQueryDefinitionResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policyName,
    putResourcePolicy_policyDocument,
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,

    -- ** PutRetentionPolicy
    putRetentionPolicy_logGroupName,
    putRetentionPolicy_retentionInDays,

    -- ** PutSubscriptionFilter
    putSubscriptionFilter_roleArn,
    putSubscriptionFilter_distribution,
    putSubscriptionFilter_logGroupName,
    putSubscriptionFilter_filterName,
    putSubscriptionFilter_filterPattern,
    putSubscriptionFilter_destinationArn,

    -- ** StartQuery
    startQuery_limit,
    startQuery_logGroupNames,
    startQuery_logGroupName,
    startQuery_startTime,
    startQuery_endTime,
    startQuery_queryString,
    startQueryResponse_queryId,
    startQueryResponse_httpStatus,

    -- ** StopQuery
    stopQuery_queryId,
    stopQueryResponse_success,
    stopQueryResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** TestMetricFilter
    testMetricFilter_filterPattern,
    testMetricFilter_logEventMessages,
    testMetricFilterResponse_matches,
    testMetricFilterResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- * Types

    -- ** Destination
    destination_roleArn,
    destination_destinationName,
    destination_targetArn,
    destination_arn,
    destination_accessPolicy,
    destination_creationTime,

    -- ** ExportTask
    exportTask_destination,
    exportTask_destinationPrefix,
    exportTask_from,
    exportTask_taskName,
    exportTask_taskId,
    exportTask_to,
    exportTask_status,
    exportTask_executionInfo,
    exportTask_logGroupName,

    -- ** ExportTaskExecutionInfo
    exportTaskExecutionInfo_completionTime,
    exportTaskExecutionInfo_creationTime,

    -- ** ExportTaskStatus
    exportTaskStatus_message,
    exportTaskStatus_code,

    -- ** FilteredLogEvent
    filteredLogEvent_message,
    filteredLogEvent_timestamp,
    filteredLogEvent_eventId,
    filteredLogEvent_ingestionTime,
    filteredLogEvent_logStreamName,

    -- ** InputLogEvent
    inputLogEvent_timestamp,
    inputLogEvent_message,

    -- ** LogGroup
    logGroup_storedBytes,
    logGroup_arn,
    logGroup_retentionInDays,
    logGroup_kmsKeyId,
    logGroup_metricFilterCount,
    logGroup_creationTime,
    logGroup_logGroupName,

    -- ** LogGroupField
    logGroupField_name,
    logGroupField_percent,

    -- ** LogStream
    logStream_uploadSequenceToken,
    logStream_storedBytes,
    logStream_arn,
    logStream_firstEventTimestamp,
    logStream_lastEventTimestamp,
    logStream_creationTime,
    logStream_logStreamName,
    logStream_lastIngestionTime,

    -- ** MetricFilter
    metricFilter_filterName,
    metricFilter_creationTime,
    metricFilter_metricTransformations,
    metricFilter_filterPattern,
    metricFilter_logGroupName,

    -- ** MetricFilterMatchRecord
    metricFilterMatchRecord_eventNumber,
    metricFilterMatchRecord_extractedValues,
    metricFilterMatchRecord_eventMessage,

    -- ** MetricTransformation
    metricTransformation_defaultValue,
    metricTransformation_dimensions,
    metricTransformation_unit,
    metricTransformation_metricName,
    metricTransformation_metricNamespace,
    metricTransformation_metricValue,

    -- ** OutputLogEvent
    outputLogEvent_message,
    outputLogEvent_timestamp,
    outputLogEvent_ingestionTime,

    -- ** QueryDefinition
    queryDefinition_name,
    queryDefinition_queryDefinitionId,
    queryDefinition_logGroupNames,
    queryDefinition_lastModified,
    queryDefinition_queryString,

    -- ** QueryInfo
    queryInfo_queryId,
    queryInfo_status,
    queryInfo_queryString,
    queryInfo_createTime,
    queryInfo_logGroupName,

    -- ** QueryStatistics
    queryStatistics_recordsMatched,
    queryStatistics_bytesScanned,
    queryStatistics_recordsScanned,

    -- ** RejectedLogEventsInfo
    rejectedLogEventsInfo_tooNewLogEventStartIndex,
    rejectedLogEventsInfo_expiredLogEventEndIndex,
    rejectedLogEventsInfo_tooOldLogEventEndIndex,

    -- ** ResourcePolicy
    resourcePolicy_policyName,
    resourcePolicy_lastUpdatedTime,
    resourcePolicy_policyDocument,

    -- ** ResultField
    resultField_field,
    resultField_value,

    -- ** SearchedLogStream
    searchedLogStream_searchedCompletely,
    searchedLogStream_logStreamName,

    -- ** SubscriptionFilter
    subscriptionFilter_roleArn,
    subscriptionFilter_filterName,
    subscriptionFilter_distribution,
    subscriptionFilter_creationTime,
    subscriptionFilter_filterPattern,
    subscriptionFilter_destinationArn,
    subscriptionFilter_logGroupName,
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
import Amazonka.CloudWatchLogs.ListTagsForResource
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
import Amazonka.CloudWatchLogs.TagResource
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
import Amazonka.CloudWatchLogs.UntagResource

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchLogs.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createExportTask_logStreamNamePrefix,
    createExportTask_taskName,
    createExportTask_logGroupName,
    createExportTask_from,
    createExportTask_to,
    createExportTask_destination,
    createExportTaskResponse_taskId,
    createExportTaskResponse_httpStatus,

    -- ** CreateLogGroup
    createLogGroup_kmsKeyId,
    createLogGroup_tags,
    createLogGroup_logGroupName,

    -- ** CreateLogStream
    createLogStream_logGroupName,
    createLogStream_logStreamName,

    -- ** DeleteAccountPolicy
    deleteAccountPolicy_policyName,
    deleteAccountPolicy_policyType,

    -- ** DeleteDataProtectionPolicy
    deleteDataProtectionPolicy_logGroupIdentifier,

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

    -- ** DescribeAccountPolicies
    describeAccountPolicies_accountIdentifiers,
    describeAccountPolicies_policyName,
    describeAccountPolicies_policyType,
    describeAccountPoliciesResponse_accountPolicies,
    describeAccountPoliciesResponse_httpStatus,

    -- ** DescribeDestinations
    describeDestinations_destinationNamePrefix,
    describeDestinations_limit,
    describeDestinations_nextToken,
    describeDestinationsResponse_destinations,
    describeDestinationsResponse_nextToken,
    describeDestinationsResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_limit,
    describeExportTasks_nextToken,
    describeExportTasks_statusCode,
    describeExportTasks_taskId,
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_httpStatus,

    -- ** DescribeLogGroups
    describeLogGroups_accountIdentifiers,
    describeLogGroups_includeLinkedAccounts,
    describeLogGroups_limit,
    describeLogGroups_logGroupNamePattern,
    describeLogGroups_logGroupNamePrefix,
    describeLogGroups_nextToken,
    describeLogGroupsResponse_logGroups,
    describeLogGroupsResponse_nextToken,
    describeLogGroupsResponse_httpStatus,

    -- ** DescribeLogStreams
    describeLogStreams_descending,
    describeLogStreams_limit,
    describeLogStreams_logGroupIdentifier,
    describeLogStreams_logGroupName,
    describeLogStreams_logStreamNamePrefix,
    describeLogStreams_nextToken,
    describeLogStreams_orderBy,
    describeLogStreamsResponse_logStreams,
    describeLogStreamsResponse_nextToken,
    describeLogStreamsResponse_httpStatus,

    -- ** DescribeMetricFilters
    describeMetricFilters_filterNamePrefix,
    describeMetricFilters_limit,
    describeMetricFilters_logGroupName,
    describeMetricFilters_metricName,
    describeMetricFilters_metricNamespace,
    describeMetricFilters_nextToken,
    describeMetricFiltersResponse_metricFilters,
    describeMetricFiltersResponse_nextToken,
    describeMetricFiltersResponse_httpStatus,

    -- ** DescribeQueries
    describeQueries_logGroupName,
    describeQueries_maxResults,
    describeQueries_nextToken,
    describeQueries_status,
    describeQueriesResponse_nextToken,
    describeQueriesResponse_queries,
    describeQueriesResponse_httpStatus,

    -- ** DescribeQueryDefinitions
    describeQueryDefinitions_maxResults,
    describeQueryDefinitions_nextToken,
    describeQueryDefinitions_queryDefinitionNamePrefix,
    describeQueryDefinitionsResponse_nextToken,
    describeQueryDefinitionsResponse_queryDefinitions,
    describeQueryDefinitionsResponse_httpStatus,

    -- ** DescribeResourcePolicies
    describeResourcePolicies_limit,
    describeResourcePolicies_nextToken,
    describeResourcePoliciesResponse_nextToken,
    describeResourcePoliciesResponse_resourcePolicies,
    describeResourcePoliciesResponse_httpStatus,

    -- ** DescribeSubscriptionFilters
    describeSubscriptionFilters_filterNamePrefix,
    describeSubscriptionFilters_limit,
    describeSubscriptionFilters_nextToken,
    describeSubscriptionFilters_logGroupName,
    describeSubscriptionFiltersResponse_nextToken,
    describeSubscriptionFiltersResponse_subscriptionFilters,
    describeSubscriptionFiltersResponse_httpStatus,

    -- ** DisassociateKmsKey
    disassociateKmsKey_logGroupName,

    -- ** FilterLogEvents
    filterLogEvents_endTime,
    filterLogEvents_filterPattern,
    filterLogEvents_interleaved,
    filterLogEvents_limit,
    filterLogEvents_logGroupIdentifier,
    filterLogEvents_logGroupName,
    filterLogEvents_logStreamNamePrefix,
    filterLogEvents_logStreamNames,
    filterLogEvents_nextToken,
    filterLogEvents_startTime,
    filterLogEvents_unmask,
    filterLogEventsResponse_events,
    filterLogEventsResponse_nextToken,
    filterLogEventsResponse_searchedLogStreams,
    filterLogEventsResponse_httpStatus,

    -- ** GetDataProtectionPolicy
    getDataProtectionPolicy_logGroupIdentifier,
    getDataProtectionPolicyResponse_lastUpdatedTime,
    getDataProtectionPolicyResponse_logGroupIdentifier,
    getDataProtectionPolicyResponse_policyDocument,
    getDataProtectionPolicyResponse_httpStatus,

    -- ** GetLogEvents
    getLogEvents_endTime,
    getLogEvents_limit,
    getLogEvents_logGroupIdentifier,
    getLogEvents_logGroupName,
    getLogEvents_nextToken,
    getLogEvents_startFromHead,
    getLogEvents_startTime,
    getLogEvents_unmask,
    getLogEvents_logStreamName,
    getLogEventsResponse_events,
    getLogEventsResponse_nextBackwardToken,
    getLogEventsResponse_nextForwardToken,
    getLogEventsResponse_httpStatus,

    -- ** GetLogGroupFields
    getLogGroupFields_logGroupIdentifier,
    getLogGroupFields_logGroupName,
    getLogGroupFields_time,
    getLogGroupFieldsResponse_logGroupFields,
    getLogGroupFieldsResponse_httpStatus,

    -- ** GetLogRecord
    getLogRecord_unmask,
    getLogRecord_logRecordPointer,
    getLogRecordResponse_logRecord,
    getLogRecordResponse_httpStatus,

    -- ** GetQueryResults
    getQueryResults_queryId,
    getQueryResultsResponse_results,
    getQueryResultsResponse_statistics,
    getQueryResultsResponse_status,
    getQueryResultsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutAccountPolicy
    putAccountPolicy_scope,
    putAccountPolicy_policyName,
    putAccountPolicy_policyDocument,
    putAccountPolicy_policyType,
    putAccountPolicyResponse_accountPolicy,
    putAccountPolicyResponse_httpStatus,

    -- ** PutDataProtectionPolicy
    putDataProtectionPolicy_logGroupIdentifier,
    putDataProtectionPolicy_policyDocument,
    putDataProtectionPolicyResponse_lastUpdatedTime,
    putDataProtectionPolicyResponse_logGroupIdentifier,
    putDataProtectionPolicyResponse_policyDocument,
    putDataProtectionPolicyResponse_httpStatus,

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
    putQueryDefinition_logGroupNames,
    putQueryDefinition_queryDefinitionId,
    putQueryDefinition_name,
    putQueryDefinition_queryString,
    putQueryDefinitionResponse_queryDefinitionId,
    putQueryDefinitionResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policyDocument,
    putResourcePolicy_policyName,
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,

    -- ** PutRetentionPolicy
    putRetentionPolicy_logGroupName,
    putRetentionPolicy_retentionInDays,

    -- ** PutSubscriptionFilter
    putSubscriptionFilter_distribution,
    putSubscriptionFilter_roleArn,
    putSubscriptionFilter_logGroupName,
    putSubscriptionFilter_filterName,
    putSubscriptionFilter_filterPattern,
    putSubscriptionFilter_destinationArn,

    -- ** StartQuery
    startQuery_limit,
    startQuery_logGroupIdentifiers,
    startQuery_logGroupName,
    startQuery_logGroupNames,
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

    -- ** AccountPolicy
    accountPolicy_accountId,
    accountPolicy_lastUpdatedTime,
    accountPolicy_policyDocument,
    accountPolicy_policyName,
    accountPolicy_policyType,
    accountPolicy_scope,

    -- ** Destination
    destination_accessPolicy,
    destination_arn,
    destination_creationTime,
    destination_destinationName,
    destination_roleArn,
    destination_targetArn,

    -- ** ExportTask
    exportTask_destination,
    exportTask_destinationPrefix,
    exportTask_executionInfo,
    exportTask_from,
    exportTask_logGroupName,
    exportTask_status,
    exportTask_taskId,
    exportTask_taskName,
    exportTask_to,

    -- ** ExportTaskExecutionInfo
    exportTaskExecutionInfo_completionTime,
    exportTaskExecutionInfo_creationTime,

    -- ** ExportTaskStatus
    exportTaskStatus_code,
    exportTaskStatus_message,

    -- ** FilteredLogEvent
    filteredLogEvent_eventId,
    filteredLogEvent_ingestionTime,
    filteredLogEvent_logStreamName,
    filteredLogEvent_message,
    filteredLogEvent_timestamp,

    -- ** InputLogEvent
    inputLogEvent_timestamp,
    inputLogEvent_message,

    -- ** LogGroup
    logGroup_arn,
    logGroup_creationTime,
    logGroup_dataProtectionStatus,
    logGroup_inheritedProperties,
    logGroup_kmsKeyId,
    logGroup_logGroupName,
    logGroup_metricFilterCount,
    logGroup_retentionInDays,
    logGroup_storedBytes,

    -- ** LogGroupField
    logGroupField_name,
    logGroupField_percent,

    -- ** LogStream
    logStream_arn,
    logStream_creationTime,
    logStream_firstEventTimestamp,
    logStream_lastEventTimestamp,
    logStream_lastIngestionTime,
    logStream_logStreamName,
    logStream_storedBytes,
    logStream_uploadSequenceToken,

    -- ** MetricFilter
    metricFilter_creationTime,
    metricFilter_filterName,
    metricFilter_filterPattern,
    metricFilter_logGroupName,
    metricFilter_metricTransformations,

    -- ** MetricFilterMatchRecord
    metricFilterMatchRecord_eventMessage,
    metricFilterMatchRecord_eventNumber,
    metricFilterMatchRecord_extractedValues,

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
    queryDefinition_lastModified,
    queryDefinition_logGroupNames,
    queryDefinition_name,
    queryDefinition_queryDefinitionId,
    queryDefinition_queryString,

    -- ** QueryInfo
    queryInfo_createTime,
    queryInfo_logGroupName,
    queryInfo_queryId,
    queryInfo_queryString,
    queryInfo_status,

    -- ** QueryStatistics
    queryStatistics_bytesScanned,
    queryStatistics_recordsMatched,
    queryStatistics_recordsScanned,

    -- ** RejectedLogEventsInfo
    rejectedLogEventsInfo_expiredLogEventEndIndex,
    rejectedLogEventsInfo_tooNewLogEventStartIndex,
    rejectedLogEventsInfo_tooOldLogEventEndIndex,

    -- ** ResourcePolicy
    resourcePolicy_lastUpdatedTime,
    resourcePolicy_policyDocument,
    resourcePolicy_policyName,

    -- ** ResultField
    resultField_field,
    resultField_value,

    -- ** SearchedLogStream
    searchedLogStream_logStreamName,
    searchedLogStream_searchedCompletely,

    -- ** SubscriptionFilter
    subscriptionFilter_creationTime,
    subscriptionFilter_destinationArn,
    subscriptionFilter_distribution,
    subscriptionFilter_filterName,
    subscriptionFilter_filterPattern,
    subscriptionFilter_logGroupName,
    subscriptionFilter_roleArn,
  )
where

import Amazonka.CloudWatchLogs.AssociateKmsKey
import Amazonka.CloudWatchLogs.CancelExportTask
import Amazonka.CloudWatchLogs.CreateExportTask
import Amazonka.CloudWatchLogs.CreateLogGroup
import Amazonka.CloudWatchLogs.CreateLogStream
import Amazonka.CloudWatchLogs.DeleteAccountPolicy
import Amazonka.CloudWatchLogs.DeleteDataProtectionPolicy
import Amazonka.CloudWatchLogs.DeleteDestination
import Amazonka.CloudWatchLogs.DeleteLogGroup
import Amazonka.CloudWatchLogs.DeleteLogStream
import Amazonka.CloudWatchLogs.DeleteMetricFilter
import Amazonka.CloudWatchLogs.DeleteQueryDefinition
import Amazonka.CloudWatchLogs.DeleteResourcePolicy
import Amazonka.CloudWatchLogs.DeleteRetentionPolicy
import Amazonka.CloudWatchLogs.DeleteSubscriptionFilter
import Amazonka.CloudWatchLogs.DescribeAccountPolicies
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
import Amazonka.CloudWatchLogs.GetDataProtectionPolicy
import Amazonka.CloudWatchLogs.GetLogEvents
import Amazonka.CloudWatchLogs.GetLogGroupFields
import Amazonka.CloudWatchLogs.GetLogRecord
import Amazonka.CloudWatchLogs.GetQueryResults
import Amazonka.CloudWatchLogs.ListTagsForResource
import Amazonka.CloudWatchLogs.PutAccountPolicy
import Amazonka.CloudWatchLogs.PutDataProtectionPolicy
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
import Amazonka.CloudWatchLogs.Types.AccountPolicy
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

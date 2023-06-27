{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SWF.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Lens
  ( -- * Operations

    -- ** CountClosedWorkflowExecutions
    countClosedWorkflowExecutions_closeStatusFilter,
    countClosedWorkflowExecutions_closeTimeFilter,
    countClosedWorkflowExecutions_executionFilter,
    countClosedWorkflowExecutions_startTimeFilter,
    countClosedWorkflowExecutions_tagFilter,
    countClosedWorkflowExecutions_typeFilter,
    countClosedWorkflowExecutions_domain,
    workflowExecutionCount_truncated,
    workflowExecutionCount_count,

    -- ** CountOpenWorkflowExecutions
    countOpenWorkflowExecutions_executionFilter,
    countOpenWorkflowExecutions_tagFilter,
    countOpenWorkflowExecutions_typeFilter,
    countOpenWorkflowExecutions_domain,
    countOpenWorkflowExecutions_startTimeFilter,
    workflowExecutionCount_truncated,
    workflowExecutionCount_count,

    -- ** CountPendingActivityTasks
    countPendingActivityTasks_domain,
    countPendingActivityTasks_taskList,
    pendingTaskCount_truncated,
    pendingTaskCount_count,

    -- ** CountPendingDecisionTasks
    countPendingDecisionTasks_domain,
    countPendingDecisionTasks_taskList,
    pendingTaskCount_truncated,
    pendingTaskCount_count,

    -- ** DeprecateActivityType
    deprecateActivityType_domain,
    deprecateActivityType_activityType,

    -- ** DeprecateDomain
    deprecateDomain_name,

    -- ** DeprecateWorkflowType
    deprecateWorkflowType_domain,
    deprecateWorkflowType_workflowType,

    -- ** DescribeActivityType
    describeActivityType_domain,
    describeActivityType_activityType,
    describeActivityTypeResponse_httpStatus,
    describeActivityTypeResponse_typeInfo,
    describeActivityTypeResponse_configuration,

    -- ** DescribeDomain
    describeDomain_name,
    describeDomainResponse_httpStatus,
    describeDomainResponse_domainInfo,
    describeDomainResponse_configuration,

    -- ** DescribeWorkflowExecution
    describeWorkflowExecution_domain,
    describeWorkflowExecution_execution,
    describeWorkflowExecutionResponse_latestActivityTaskTimestamp,
    describeWorkflowExecutionResponse_latestExecutionContext,
    describeWorkflowExecutionResponse_httpStatus,
    describeWorkflowExecutionResponse_executionInfo,
    describeWorkflowExecutionResponse_executionConfiguration,
    describeWorkflowExecutionResponse_openCounts,

    -- ** DescribeWorkflowType
    describeWorkflowType_domain,
    describeWorkflowType_workflowType,
    describeWorkflowTypeResponse_httpStatus,
    describeWorkflowTypeResponse_typeInfo,
    describeWorkflowTypeResponse_configuration,

    -- ** GetWorkflowExecutionHistory
    getWorkflowExecutionHistory_maximumPageSize,
    getWorkflowExecutionHistory_nextPageToken,
    getWorkflowExecutionHistory_reverseOrder,
    getWorkflowExecutionHistory_domain,
    getWorkflowExecutionHistory_execution,
    getWorkflowExecutionHistoryResponse_nextPageToken,
    getWorkflowExecutionHistoryResponse_httpStatus,
    getWorkflowExecutionHistoryResponse_events,

    -- ** ListActivityTypes
    listActivityTypes_maximumPageSize,
    listActivityTypes_name,
    listActivityTypes_nextPageToken,
    listActivityTypes_reverseOrder,
    listActivityTypes_domain,
    listActivityTypes_registrationStatus,
    listActivityTypesResponse_nextPageToken,
    listActivityTypesResponse_httpStatus,
    listActivityTypesResponse_typeInfos,

    -- ** ListClosedWorkflowExecutions
    listClosedWorkflowExecutions_closeStatusFilter,
    listClosedWorkflowExecutions_closeTimeFilter,
    listClosedWorkflowExecutions_executionFilter,
    listClosedWorkflowExecutions_maximumPageSize,
    listClosedWorkflowExecutions_nextPageToken,
    listClosedWorkflowExecutions_reverseOrder,
    listClosedWorkflowExecutions_startTimeFilter,
    listClosedWorkflowExecutions_tagFilter,
    listClosedWorkflowExecutions_typeFilter,
    listClosedWorkflowExecutions_domain,
    workflowExecutionInfos_nextPageToken,
    workflowExecutionInfos_executionInfos,

    -- ** ListDomains
    listDomains_maximumPageSize,
    listDomains_nextPageToken,
    listDomains_reverseOrder,
    listDomains_registrationStatus,
    listDomainsResponse_nextPageToken,
    listDomainsResponse_httpStatus,
    listDomainsResponse_domainInfos,

    -- ** ListOpenWorkflowExecutions
    listOpenWorkflowExecutions_executionFilter,
    listOpenWorkflowExecutions_maximumPageSize,
    listOpenWorkflowExecutions_nextPageToken,
    listOpenWorkflowExecutions_reverseOrder,
    listOpenWorkflowExecutions_tagFilter,
    listOpenWorkflowExecutions_typeFilter,
    listOpenWorkflowExecutions_domain,
    listOpenWorkflowExecutions_startTimeFilter,
    workflowExecutionInfos_nextPageToken,
    workflowExecutionInfos_executionInfos,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorkflowTypes
    listWorkflowTypes_maximumPageSize,
    listWorkflowTypes_name,
    listWorkflowTypes_nextPageToken,
    listWorkflowTypes_reverseOrder,
    listWorkflowTypes_domain,
    listWorkflowTypes_registrationStatus,
    listWorkflowTypesResponse_nextPageToken,
    listWorkflowTypesResponse_httpStatus,
    listWorkflowTypesResponse_typeInfos,

    -- ** PollForActivityTask
    pollForActivityTask_identity,
    pollForActivityTask_domain,
    pollForActivityTask_taskList,
    pollForActivityTaskResponse_activityId,
    pollForActivityTaskResponse_activityType,
    pollForActivityTaskResponse_input,
    pollForActivityTaskResponse_taskToken,
    pollForActivityTaskResponse_workflowExecution,
    pollForActivityTaskResponse_httpStatus,
    pollForActivityTaskResponse_startedEventId,

    -- ** PollForDecisionTask
    pollForDecisionTask_identity,
    pollForDecisionTask_maximumPageSize,
    pollForDecisionTask_nextPageToken,
    pollForDecisionTask_reverseOrder,
    pollForDecisionTask_startAtPreviousStartedEvent,
    pollForDecisionTask_domain,
    pollForDecisionTask_taskList,
    pollForDecisionTaskResponse_events,
    pollForDecisionTaskResponse_nextPageToken,
    pollForDecisionTaskResponse_previousStartedEventId,
    pollForDecisionTaskResponse_taskToken,
    pollForDecisionTaskResponse_workflowExecution,
    pollForDecisionTaskResponse_workflowType,
    pollForDecisionTaskResponse_httpStatus,
    pollForDecisionTaskResponse_startedEventId,

    -- ** RecordActivityTaskHeartbeat
    recordActivityTaskHeartbeat_details,
    recordActivityTaskHeartbeat_taskToken,
    recordActivityTaskHeartbeatResponse_httpStatus,
    recordActivityTaskHeartbeatResponse_cancelRequested,

    -- ** RegisterActivityType
    registerActivityType_defaultTaskHeartbeatTimeout,
    registerActivityType_defaultTaskList,
    registerActivityType_defaultTaskPriority,
    registerActivityType_defaultTaskScheduleToCloseTimeout,
    registerActivityType_defaultTaskScheduleToStartTimeout,
    registerActivityType_defaultTaskStartToCloseTimeout,
    registerActivityType_description,
    registerActivityType_domain,
    registerActivityType_name,
    registerActivityType_version,

    -- ** RegisterDomain
    registerDomain_description,
    registerDomain_tags,
    registerDomain_name,
    registerDomain_workflowExecutionRetentionPeriodInDays,

    -- ** RegisterWorkflowType
    registerWorkflowType_defaultChildPolicy,
    registerWorkflowType_defaultExecutionStartToCloseTimeout,
    registerWorkflowType_defaultLambdaRole,
    registerWorkflowType_defaultTaskList,
    registerWorkflowType_defaultTaskPriority,
    registerWorkflowType_defaultTaskStartToCloseTimeout,
    registerWorkflowType_description,
    registerWorkflowType_domain,
    registerWorkflowType_name,
    registerWorkflowType_version,

    -- ** RequestCancelWorkflowExecution
    requestCancelWorkflowExecution_runId,
    requestCancelWorkflowExecution_domain,
    requestCancelWorkflowExecution_workflowId,

    -- ** RespondActivityTaskCanceled
    respondActivityTaskCanceled_details,
    respondActivityTaskCanceled_taskToken,

    -- ** RespondActivityTaskCompleted
    respondActivityTaskCompleted_result,
    respondActivityTaskCompleted_taskToken,

    -- ** RespondActivityTaskFailed
    respondActivityTaskFailed_details,
    respondActivityTaskFailed_reason,
    respondActivityTaskFailed_taskToken,

    -- ** RespondDecisionTaskCompleted
    respondDecisionTaskCompleted_decisions,
    respondDecisionTaskCompleted_executionContext,
    respondDecisionTaskCompleted_taskToken,

    -- ** SignalWorkflowExecution
    signalWorkflowExecution_input,
    signalWorkflowExecution_runId,
    signalWorkflowExecution_domain,
    signalWorkflowExecution_workflowId,
    signalWorkflowExecution_signalName,

    -- ** StartWorkflowExecution
    startWorkflowExecution_childPolicy,
    startWorkflowExecution_executionStartToCloseTimeout,
    startWorkflowExecution_input,
    startWorkflowExecution_lambdaRole,
    startWorkflowExecution_tagList,
    startWorkflowExecution_taskList,
    startWorkflowExecution_taskPriority,
    startWorkflowExecution_taskStartToCloseTimeout,
    startWorkflowExecution_domain,
    startWorkflowExecution_workflowId,
    startWorkflowExecution_workflowType,
    startWorkflowExecutionResponse_runId,
    startWorkflowExecutionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** TerminateWorkflowExecution
    terminateWorkflowExecution_childPolicy,
    terminateWorkflowExecution_details,
    terminateWorkflowExecution_reason,
    terminateWorkflowExecution_runId,
    terminateWorkflowExecution_domain,
    terminateWorkflowExecution_workflowId,

    -- ** UndeprecateActivityType
    undeprecateActivityType_domain,
    undeprecateActivityType_activityType,

    -- ** UndeprecateDomain
    undeprecateDomain_name,

    -- ** UndeprecateWorkflowType
    undeprecateWorkflowType_domain,
    undeprecateWorkflowType_workflowType,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- * Types

    -- ** ActivityTaskCancelRequestedEventAttributes
    activityTaskCancelRequestedEventAttributes_decisionTaskCompletedEventId,
    activityTaskCancelRequestedEventAttributes_activityId,

    -- ** ActivityTaskCanceledEventAttributes
    activityTaskCanceledEventAttributes_details,
    activityTaskCanceledEventAttributes_latestCancelRequestedEventId,
    activityTaskCanceledEventAttributes_scheduledEventId,
    activityTaskCanceledEventAttributes_startedEventId,

    -- ** ActivityTaskCompletedEventAttributes
    activityTaskCompletedEventAttributes_result,
    activityTaskCompletedEventAttributes_scheduledEventId,
    activityTaskCompletedEventAttributes_startedEventId,

    -- ** ActivityTaskFailedEventAttributes
    activityTaskFailedEventAttributes_details,
    activityTaskFailedEventAttributes_reason,
    activityTaskFailedEventAttributes_scheduledEventId,
    activityTaskFailedEventAttributes_startedEventId,

    -- ** ActivityTaskScheduledEventAttributes
    activityTaskScheduledEventAttributes_control,
    activityTaskScheduledEventAttributes_heartbeatTimeout,
    activityTaskScheduledEventAttributes_input,
    activityTaskScheduledEventAttributes_scheduleToCloseTimeout,
    activityTaskScheduledEventAttributes_scheduleToStartTimeout,
    activityTaskScheduledEventAttributes_startToCloseTimeout,
    activityTaskScheduledEventAttributes_taskPriority,
    activityTaskScheduledEventAttributes_activityType,
    activityTaskScheduledEventAttributes_activityId,
    activityTaskScheduledEventAttributes_taskList,
    activityTaskScheduledEventAttributes_decisionTaskCompletedEventId,

    -- ** ActivityTaskStartedEventAttributes
    activityTaskStartedEventAttributes_identity,
    activityTaskStartedEventAttributes_scheduledEventId,

    -- ** ActivityTaskTimedOutEventAttributes
    activityTaskTimedOutEventAttributes_details,
    activityTaskTimedOutEventAttributes_timeoutType,
    activityTaskTimedOutEventAttributes_scheduledEventId,
    activityTaskTimedOutEventAttributes_startedEventId,

    -- ** ActivityType
    activityType_name,
    activityType_version,

    -- ** ActivityTypeConfiguration
    activityTypeConfiguration_defaultTaskHeartbeatTimeout,
    activityTypeConfiguration_defaultTaskList,
    activityTypeConfiguration_defaultTaskPriority,
    activityTypeConfiguration_defaultTaskScheduleToCloseTimeout,
    activityTypeConfiguration_defaultTaskScheduleToStartTimeout,
    activityTypeConfiguration_defaultTaskStartToCloseTimeout,

    -- ** ActivityTypeInfo
    activityTypeInfo_deprecationDate,
    activityTypeInfo_description,
    activityTypeInfo_activityType,
    activityTypeInfo_status,
    activityTypeInfo_creationDate,

    -- ** CancelTimerDecisionAttributes
    cancelTimerDecisionAttributes_timerId,

    -- ** CancelTimerFailedEventAttributes
    cancelTimerFailedEventAttributes_timerId,
    cancelTimerFailedEventAttributes_cause,
    cancelTimerFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** CancelWorkflowExecutionDecisionAttributes
    cancelWorkflowExecutionDecisionAttributes_details,

    -- ** CancelWorkflowExecutionFailedEventAttributes
    cancelWorkflowExecutionFailedEventAttributes_cause,
    cancelWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** ChildWorkflowExecutionCanceledEventAttributes
    childWorkflowExecutionCanceledEventAttributes_details,
    childWorkflowExecutionCanceledEventAttributes_workflowExecution,
    childWorkflowExecutionCanceledEventAttributes_workflowType,
    childWorkflowExecutionCanceledEventAttributes_initiatedEventId,
    childWorkflowExecutionCanceledEventAttributes_startedEventId,

    -- ** ChildWorkflowExecutionCompletedEventAttributes
    childWorkflowExecutionCompletedEventAttributes_result,
    childWorkflowExecutionCompletedEventAttributes_workflowExecution,
    childWorkflowExecutionCompletedEventAttributes_workflowType,
    childWorkflowExecutionCompletedEventAttributes_initiatedEventId,
    childWorkflowExecutionCompletedEventAttributes_startedEventId,

    -- ** ChildWorkflowExecutionFailedEventAttributes
    childWorkflowExecutionFailedEventAttributes_details,
    childWorkflowExecutionFailedEventAttributes_reason,
    childWorkflowExecutionFailedEventAttributes_workflowExecution,
    childWorkflowExecutionFailedEventAttributes_workflowType,
    childWorkflowExecutionFailedEventAttributes_initiatedEventId,
    childWorkflowExecutionFailedEventAttributes_startedEventId,

    -- ** ChildWorkflowExecutionStartedEventAttributes
    childWorkflowExecutionStartedEventAttributes_workflowExecution,
    childWorkflowExecutionStartedEventAttributes_workflowType,
    childWorkflowExecutionStartedEventAttributes_initiatedEventId,

    -- ** ChildWorkflowExecutionTerminatedEventAttributes
    childWorkflowExecutionTerminatedEventAttributes_workflowExecution,
    childWorkflowExecutionTerminatedEventAttributes_workflowType,
    childWorkflowExecutionTerminatedEventAttributes_initiatedEventId,
    childWorkflowExecutionTerminatedEventAttributes_startedEventId,

    -- ** ChildWorkflowExecutionTimedOutEventAttributes
    childWorkflowExecutionTimedOutEventAttributes_workflowExecution,
    childWorkflowExecutionTimedOutEventAttributes_workflowType,
    childWorkflowExecutionTimedOutEventAttributes_timeoutType,
    childWorkflowExecutionTimedOutEventAttributes_initiatedEventId,
    childWorkflowExecutionTimedOutEventAttributes_startedEventId,

    -- ** CloseStatusFilter
    closeStatusFilter_status,

    -- ** CompleteWorkflowExecutionDecisionAttributes
    completeWorkflowExecutionDecisionAttributes_result,

    -- ** CompleteWorkflowExecutionFailedEventAttributes
    completeWorkflowExecutionFailedEventAttributes_cause,
    completeWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** ContinueAsNewWorkflowExecutionDecisionAttributes
    continueAsNewWorkflowExecutionDecisionAttributes_childPolicy,
    continueAsNewWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout,
    continueAsNewWorkflowExecutionDecisionAttributes_input,
    continueAsNewWorkflowExecutionDecisionAttributes_lambdaRole,
    continueAsNewWorkflowExecutionDecisionAttributes_tagList,
    continueAsNewWorkflowExecutionDecisionAttributes_taskList,
    continueAsNewWorkflowExecutionDecisionAttributes_taskPriority,
    continueAsNewWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout,
    continueAsNewWorkflowExecutionDecisionAttributes_workflowTypeVersion,

    -- ** ContinueAsNewWorkflowExecutionFailedEventAttributes
    continueAsNewWorkflowExecutionFailedEventAttributes_cause,
    continueAsNewWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** Decision
    decision_cancelTimerDecisionAttributes,
    decision_cancelWorkflowExecutionDecisionAttributes,
    decision_completeWorkflowExecutionDecisionAttributes,
    decision_continueAsNewWorkflowExecutionDecisionAttributes,
    decision_failWorkflowExecutionDecisionAttributes,
    decision_recordMarkerDecisionAttributes,
    decision_requestCancelActivityTaskDecisionAttributes,
    decision_requestCancelExternalWorkflowExecutionDecisionAttributes,
    decision_scheduleActivityTaskDecisionAttributes,
    decision_scheduleLambdaFunctionDecisionAttributes,
    decision_signalExternalWorkflowExecutionDecisionAttributes,
    decision_startChildWorkflowExecutionDecisionAttributes,
    decision_startTimerDecisionAttributes,
    decision_decisionType,

    -- ** DecisionTaskCompletedEventAttributes
    decisionTaskCompletedEventAttributes_executionContext,
    decisionTaskCompletedEventAttributes_scheduledEventId,
    decisionTaskCompletedEventAttributes_startedEventId,

    -- ** DecisionTaskScheduledEventAttributes
    decisionTaskScheduledEventAttributes_startToCloseTimeout,
    decisionTaskScheduledEventAttributes_taskPriority,
    decisionTaskScheduledEventAttributes_taskList,

    -- ** DecisionTaskStartedEventAttributes
    decisionTaskStartedEventAttributes_identity,
    decisionTaskStartedEventAttributes_scheduledEventId,

    -- ** DecisionTaskTimedOutEventAttributes
    decisionTaskTimedOutEventAttributes_timeoutType,
    decisionTaskTimedOutEventAttributes_scheduledEventId,
    decisionTaskTimedOutEventAttributes_startedEventId,

    -- ** DomainConfiguration
    domainConfiguration_workflowExecutionRetentionPeriodInDays,

    -- ** DomainInfo
    domainInfo_arn,
    domainInfo_description,
    domainInfo_name,
    domainInfo_status,

    -- ** ExecutionTimeFilter
    executionTimeFilter_latestDate,
    executionTimeFilter_oldestDate,

    -- ** ExternalWorkflowExecutionCancelRequestedEventAttributes
    externalWorkflowExecutionCancelRequestedEventAttributes_workflowExecution,
    externalWorkflowExecutionCancelRequestedEventAttributes_initiatedEventId,

    -- ** ExternalWorkflowExecutionSignaledEventAttributes
    externalWorkflowExecutionSignaledEventAttributes_workflowExecution,
    externalWorkflowExecutionSignaledEventAttributes_initiatedEventId,

    -- ** FailWorkflowExecutionDecisionAttributes
    failWorkflowExecutionDecisionAttributes_details,
    failWorkflowExecutionDecisionAttributes_reason,

    -- ** FailWorkflowExecutionFailedEventAttributes
    failWorkflowExecutionFailedEventAttributes_cause,
    failWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** HistoryEvent
    historyEvent_activityTaskCancelRequestedEventAttributes,
    historyEvent_activityTaskCanceledEventAttributes,
    historyEvent_activityTaskCompletedEventAttributes,
    historyEvent_activityTaskFailedEventAttributes,
    historyEvent_activityTaskScheduledEventAttributes,
    historyEvent_activityTaskStartedEventAttributes,
    historyEvent_activityTaskTimedOutEventAttributes,
    historyEvent_cancelTimerFailedEventAttributes,
    historyEvent_cancelWorkflowExecutionFailedEventAttributes,
    historyEvent_childWorkflowExecutionCanceledEventAttributes,
    historyEvent_childWorkflowExecutionCompletedEventAttributes,
    historyEvent_childWorkflowExecutionFailedEventAttributes,
    historyEvent_childWorkflowExecutionStartedEventAttributes,
    historyEvent_childWorkflowExecutionTerminatedEventAttributes,
    historyEvent_childWorkflowExecutionTimedOutEventAttributes,
    historyEvent_completeWorkflowExecutionFailedEventAttributes,
    historyEvent_continueAsNewWorkflowExecutionFailedEventAttributes,
    historyEvent_decisionTaskCompletedEventAttributes,
    historyEvent_decisionTaskScheduledEventAttributes,
    historyEvent_decisionTaskStartedEventAttributes,
    historyEvent_decisionTaskTimedOutEventAttributes,
    historyEvent_externalWorkflowExecutionCancelRequestedEventAttributes,
    historyEvent_externalWorkflowExecutionSignaledEventAttributes,
    historyEvent_failWorkflowExecutionFailedEventAttributes,
    historyEvent_lambdaFunctionCompletedEventAttributes,
    historyEvent_lambdaFunctionFailedEventAttributes,
    historyEvent_lambdaFunctionScheduledEventAttributes,
    historyEvent_lambdaFunctionStartedEventAttributes,
    historyEvent_lambdaFunctionTimedOutEventAttributes,
    historyEvent_markerRecordedEventAttributes,
    historyEvent_recordMarkerFailedEventAttributes,
    historyEvent_requestCancelActivityTaskFailedEventAttributes,
    historyEvent_requestCancelExternalWorkflowExecutionFailedEventAttributes,
    historyEvent_requestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    historyEvent_scheduleActivityTaskFailedEventAttributes,
    historyEvent_scheduleLambdaFunctionFailedEventAttributes,
    historyEvent_signalExternalWorkflowExecutionFailedEventAttributes,
    historyEvent_signalExternalWorkflowExecutionInitiatedEventAttributes,
    historyEvent_startChildWorkflowExecutionFailedEventAttributes,
    historyEvent_startChildWorkflowExecutionInitiatedEventAttributes,
    historyEvent_startLambdaFunctionFailedEventAttributes,
    historyEvent_startTimerFailedEventAttributes,
    historyEvent_timerCanceledEventAttributes,
    historyEvent_timerFiredEventAttributes,
    historyEvent_timerStartedEventAttributes,
    historyEvent_workflowExecutionCancelRequestedEventAttributes,
    historyEvent_workflowExecutionCanceledEventAttributes,
    historyEvent_workflowExecutionCompletedEventAttributes,
    historyEvent_workflowExecutionContinuedAsNewEventAttributes,
    historyEvent_workflowExecutionFailedEventAttributes,
    historyEvent_workflowExecutionSignaledEventAttributes,
    historyEvent_workflowExecutionStartedEventAttributes,
    historyEvent_workflowExecutionTerminatedEventAttributes,
    historyEvent_workflowExecutionTimedOutEventAttributes,
    historyEvent_eventTimestamp,
    historyEvent_eventType,
    historyEvent_eventId,

    -- ** LambdaFunctionCompletedEventAttributes
    lambdaFunctionCompletedEventAttributes_result,
    lambdaFunctionCompletedEventAttributes_scheduledEventId,
    lambdaFunctionCompletedEventAttributes_startedEventId,

    -- ** LambdaFunctionFailedEventAttributes
    lambdaFunctionFailedEventAttributes_details,
    lambdaFunctionFailedEventAttributes_reason,
    lambdaFunctionFailedEventAttributes_scheduledEventId,
    lambdaFunctionFailedEventAttributes_startedEventId,

    -- ** LambdaFunctionScheduledEventAttributes
    lambdaFunctionScheduledEventAttributes_control,
    lambdaFunctionScheduledEventAttributes_input,
    lambdaFunctionScheduledEventAttributes_startToCloseTimeout,
    lambdaFunctionScheduledEventAttributes_id,
    lambdaFunctionScheduledEventAttributes_name,
    lambdaFunctionScheduledEventAttributes_decisionTaskCompletedEventId,

    -- ** LambdaFunctionStartedEventAttributes
    lambdaFunctionStartedEventAttributes_scheduledEventId,

    -- ** LambdaFunctionTimedOutEventAttributes
    lambdaFunctionTimedOutEventAttributes_timeoutType,
    lambdaFunctionTimedOutEventAttributes_scheduledEventId,
    lambdaFunctionTimedOutEventAttributes_startedEventId,

    -- ** MarkerRecordedEventAttributes
    markerRecordedEventAttributes_details,
    markerRecordedEventAttributes_markerName,
    markerRecordedEventAttributes_decisionTaskCompletedEventId,

    -- ** PendingTaskCount
    pendingTaskCount_truncated,
    pendingTaskCount_count,

    -- ** RecordMarkerDecisionAttributes
    recordMarkerDecisionAttributes_details,
    recordMarkerDecisionAttributes_markerName,

    -- ** RecordMarkerFailedEventAttributes
    recordMarkerFailedEventAttributes_markerName,
    recordMarkerFailedEventAttributes_cause,
    recordMarkerFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** RequestCancelActivityTaskDecisionAttributes
    requestCancelActivityTaskDecisionAttributes_activityId,

    -- ** RequestCancelActivityTaskFailedEventAttributes
    requestCancelActivityTaskFailedEventAttributes_activityId,
    requestCancelActivityTaskFailedEventAttributes_cause,
    requestCancelActivityTaskFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** RequestCancelExternalWorkflowExecutionDecisionAttributes
    requestCancelExternalWorkflowExecutionDecisionAttributes_control,
    requestCancelExternalWorkflowExecutionDecisionAttributes_runId,
    requestCancelExternalWorkflowExecutionDecisionAttributes_workflowId,

    -- ** RequestCancelExternalWorkflowExecutionFailedEventAttributes
    requestCancelExternalWorkflowExecutionFailedEventAttributes_control,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_runId,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_workflowId,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_cause,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_initiatedEventId,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes_control,
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes_runId,
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes_workflowId,
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId,

    -- ** ResourceTag
    resourceTag_value,
    resourceTag_key,

    -- ** ScheduleActivityTaskDecisionAttributes
    scheduleActivityTaskDecisionAttributes_control,
    scheduleActivityTaskDecisionAttributes_heartbeatTimeout,
    scheduleActivityTaskDecisionAttributes_input,
    scheduleActivityTaskDecisionAttributes_scheduleToCloseTimeout,
    scheduleActivityTaskDecisionAttributes_scheduleToStartTimeout,
    scheduleActivityTaskDecisionAttributes_startToCloseTimeout,
    scheduleActivityTaskDecisionAttributes_taskList,
    scheduleActivityTaskDecisionAttributes_taskPriority,
    scheduleActivityTaskDecisionAttributes_activityType,
    scheduleActivityTaskDecisionAttributes_activityId,

    -- ** ScheduleActivityTaskFailedEventAttributes
    scheduleActivityTaskFailedEventAttributes_activityType,
    scheduleActivityTaskFailedEventAttributes_activityId,
    scheduleActivityTaskFailedEventAttributes_cause,
    scheduleActivityTaskFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** ScheduleLambdaFunctionDecisionAttributes
    scheduleLambdaFunctionDecisionAttributes_control,
    scheduleLambdaFunctionDecisionAttributes_input,
    scheduleLambdaFunctionDecisionAttributes_startToCloseTimeout,
    scheduleLambdaFunctionDecisionAttributes_id,
    scheduleLambdaFunctionDecisionAttributes_name,

    -- ** ScheduleLambdaFunctionFailedEventAttributes
    scheduleLambdaFunctionFailedEventAttributes_id,
    scheduleLambdaFunctionFailedEventAttributes_name,
    scheduleLambdaFunctionFailedEventAttributes_cause,
    scheduleLambdaFunctionFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** SignalExternalWorkflowExecutionDecisionAttributes
    signalExternalWorkflowExecutionDecisionAttributes_control,
    signalExternalWorkflowExecutionDecisionAttributes_input,
    signalExternalWorkflowExecutionDecisionAttributes_runId,
    signalExternalWorkflowExecutionDecisionAttributes_workflowId,
    signalExternalWorkflowExecutionDecisionAttributes_signalName,

    -- ** SignalExternalWorkflowExecutionFailedEventAttributes
    signalExternalWorkflowExecutionFailedEventAttributes_control,
    signalExternalWorkflowExecutionFailedEventAttributes_runId,
    signalExternalWorkflowExecutionFailedEventAttributes_workflowId,
    signalExternalWorkflowExecutionFailedEventAttributes_cause,
    signalExternalWorkflowExecutionFailedEventAttributes_initiatedEventId,
    signalExternalWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** SignalExternalWorkflowExecutionInitiatedEventAttributes
    signalExternalWorkflowExecutionInitiatedEventAttributes_control,
    signalExternalWorkflowExecutionInitiatedEventAttributes_input,
    signalExternalWorkflowExecutionInitiatedEventAttributes_runId,
    signalExternalWorkflowExecutionInitiatedEventAttributes_workflowId,
    signalExternalWorkflowExecutionInitiatedEventAttributes_signalName,
    signalExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId,

    -- ** StartChildWorkflowExecutionDecisionAttributes
    startChildWorkflowExecutionDecisionAttributes_childPolicy,
    startChildWorkflowExecutionDecisionAttributes_control,
    startChildWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout,
    startChildWorkflowExecutionDecisionAttributes_input,
    startChildWorkflowExecutionDecisionAttributes_lambdaRole,
    startChildWorkflowExecutionDecisionAttributes_tagList,
    startChildWorkflowExecutionDecisionAttributes_taskList,
    startChildWorkflowExecutionDecisionAttributes_taskPriority,
    startChildWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout,
    startChildWorkflowExecutionDecisionAttributes_workflowType,
    startChildWorkflowExecutionDecisionAttributes_workflowId,

    -- ** StartChildWorkflowExecutionFailedEventAttributes
    startChildWorkflowExecutionFailedEventAttributes_control,
    startChildWorkflowExecutionFailedEventAttributes_workflowType,
    startChildWorkflowExecutionFailedEventAttributes_cause,
    startChildWorkflowExecutionFailedEventAttributes_workflowId,
    startChildWorkflowExecutionFailedEventAttributes_initiatedEventId,
    startChildWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** StartChildWorkflowExecutionInitiatedEventAttributes
    startChildWorkflowExecutionInitiatedEventAttributes_control,
    startChildWorkflowExecutionInitiatedEventAttributes_executionStartToCloseTimeout,
    startChildWorkflowExecutionInitiatedEventAttributes_input,
    startChildWorkflowExecutionInitiatedEventAttributes_lambdaRole,
    startChildWorkflowExecutionInitiatedEventAttributes_tagList,
    startChildWorkflowExecutionInitiatedEventAttributes_taskPriority,
    startChildWorkflowExecutionInitiatedEventAttributes_taskStartToCloseTimeout,
    startChildWorkflowExecutionInitiatedEventAttributes_workflowId,
    startChildWorkflowExecutionInitiatedEventAttributes_workflowType,
    startChildWorkflowExecutionInitiatedEventAttributes_taskList,
    startChildWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId,
    startChildWorkflowExecutionInitiatedEventAttributes_childPolicy,

    -- ** StartLambdaFunctionFailedEventAttributes
    startLambdaFunctionFailedEventAttributes_cause,
    startLambdaFunctionFailedEventAttributes_message,
    startLambdaFunctionFailedEventAttributes_scheduledEventId,

    -- ** StartTimerDecisionAttributes
    startTimerDecisionAttributes_control,
    startTimerDecisionAttributes_timerId,
    startTimerDecisionAttributes_startToFireTimeout,

    -- ** StartTimerFailedEventAttributes
    startTimerFailedEventAttributes_timerId,
    startTimerFailedEventAttributes_cause,
    startTimerFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** TagFilter
    tagFilter_tag,

    -- ** TaskList
    taskList_name,

    -- ** TimerCanceledEventAttributes
    timerCanceledEventAttributes_timerId,
    timerCanceledEventAttributes_startedEventId,
    timerCanceledEventAttributes_decisionTaskCompletedEventId,

    -- ** TimerFiredEventAttributes
    timerFiredEventAttributes_timerId,
    timerFiredEventAttributes_startedEventId,

    -- ** TimerStartedEventAttributes
    timerStartedEventAttributes_control,
    timerStartedEventAttributes_timerId,
    timerStartedEventAttributes_startToFireTimeout,
    timerStartedEventAttributes_decisionTaskCompletedEventId,

    -- ** WorkflowExecution
    workflowExecution_workflowId,
    workflowExecution_runId,

    -- ** WorkflowExecutionCancelRequestedEventAttributes
    workflowExecutionCancelRequestedEventAttributes_cause,
    workflowExecutionCancelRequestedEventAttributes_externalInitiatedEventId,
    workflowExecutionCancelRequestedEventAttributes_externalWorkflowExecution,

    -- ** WorkflowExecutionCanceledEventAttributes
    workflowExecutionCanceledEventAttributes_details,
    workflowExecutionCanceledEventAttributes_decisionTaskCompletedEventId,

    -- ** WorkflowExecutionCompletedEventAttributes
    workflowExecutionCompletedEventAttributes_result,
    workflowExecutionCompletedEventAttributes_decisionTaskCompletedEventId,

    -- ** WorkflowExecutionConfiguration
    workflowExecutionConfiguration_lambdaRole,
    workflowExecutionConfiguration_taskPriority,
    workflowExecutionConfiguration_taskStartToCloseTimeout,
    workflowExecutionConfiguration_executionStartToCloseTimeout,
    workflowExecutionConfiguration_taskList,
    workflowExecutionConfiguration_childPolicy,

    -- ** WorkflowExecutionContinuedAsNewEventAttributes
    workflowExecutionContinuedAsNewEventAttributes_executionStartToCloseTimeout,
    workflowExecutionContinuedAsNewEventAttributes_input,
    workflowExecutionContinuedAsNewEventAttributes_lambdaRole,
    workflowExecutionContinuedAsNewEventAttributes_tagList,
    workflowExecutionContinuedAsNewEventAttributes_taskPriority,
    workflowExecutionContinuedAsNewEventAttributes_taskStartToCloseTimeout,
    workflowExecutionContinuedAsNewEventAttributes_decisionTaskCompletedEventId,
    workflowExecutionContinuedAsNewEventAttributes_newExecutionRunId,
    workflowExecutionContinuedAsNewEventAttributes_taskList,
    workflowExecutionContinuedAsNewEventAttributes_childPolicy,
    workflowExecutionContinuedAsNewEventAttributes_workflowType,

    -- ** WorkflowExecutionCount
    workflowExecutionCount_truncated,
    workflowExecutionCount_count,

    -- ** WorkflowExecutionFailedEventAttributes
    workflowExecutionFailedEventAttributes_details,
    workflowExecutionFailedEventAttributes_reason,
    workflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- ** WorkflowExecutionFilter
    workflowExecutionFilter_workflowId,

    -- ** WorkflowExecutionInfo
    workflowExecutionInfo_cancelRequested,
    workflowExecutionInfo_closeStatus,
    workflowExecutionInfo_closeTimestamp,
    workflowExecutionInfo_parent,
    workflowExecutionInfo_tagList,
    workflowExecutionInfo_execution,
    workflowExecutionInfo_workflowType,
    workflowExecutionInfo_startTimestamp,
    workflowExecutionInfo_executionStatus,

    -- ** WorkflowExecutionInfos
    workflowExecutionInfos_nextPageToken,
    workflowExecutionInfos_executionInfos,

    -- ** WorkflowExecutionOpenCounts
    workflowExecutionOpenCounts_openLambdaFunctions,
    workflowExecutionOpenCounts_openActivityTasks,
    workflowExecutionOpenCounts_openDecisionTasks,
    workflowExecutionOpenCounts_openTimers,
    workflowExecutionOpenCounts_openChildWorkflowExecutions,

    -- ** WorkflowExecutionSignaledEventAttributes
    workflowExecutionSignaledEventAttributes_externalInitiatedEventId,
    workflowExecutionSignaledEventAttributes_externalWorkflowExecution,
    workflowExecutionSignaledEventAttributes_input,
    workflowExecutionSignaledEventAttributes_signalName,

    -- ** WorkflowExecutionStartedEventAttributes
    workflowExecutionStartedEventAttributes_continuedExecutionRunId,
    workflowExecutionStartedEventAttributes_executionStartToCloseTimeout,
    workflowExecutionStartedEventAttributes_input,
    workflowExecutionStartedEventAttributes_lambdaRole,
    workflowExecutionStartedEventAttributes_parentInitiatedEventId,
    workflowExecutionStartedEventAttributes_parentWorkflowExecution,
    workflowExecutionStartedEventAttributes_tagList,
    workflowExecutionStartedEventAttributes_taskPriority,
    workflowExecutionStartedEventAttributes_taskStartToCloseTimeout,
    workflowExecutionStartedEventAttributes_childPolicy,
    workflowExecutionStartedEventAttributes_taskList,
    workflowExecutionStartedEventAttributes_workflowType,

    -- ** WorkflowExecutionTerminatedEventAttributes
    workflowExecutionTerminatedEventAttributes_cause,
    workflowExecutionTerminatedEventAttributes_details,
    workflowExecutionTerminatedEventAttributes_reason,
    workflowExecutionTerminatedEventAttributes_childPolicy,

    -- ** WorkflowExecutionTimedOutEventAttributes
    workflowExecutionTimedOutEventAttributes_timeoutType,
    workflowExecutionTimedOutEventAttributes_childPolicy,

    -- ** WorkflowType
    workflowType_name,
    workflowType_version,

    -- ** WorkflowTypeConfiguration
    workflowTypeConfiguration_defaultChildPolicy,
    workflowTypeConfiguration_defaultExecutionStartToCloseTimeout,
    workflowTypeConfiguration_defaultLambdaRole,
    workflowTypeConfiguration_defaultTaskList,
    workflowTypeConfiguration_defaultTaskPriority,
    workflowTypeConfiguration_defaultTaskStartToCloseTimeout,

    -- ** WorkflowTypeFilter
    workflowTypeFilter_version,
    workflowTypeFilter_name,

    -- ** WorkflowTypeInfo
    workflowTypeInfo_deprecationDate,
    workflowTypeInfo_description,
    workflowTypeInfo_workflowType,
    workflowTypeInfo_status,
    workflowTypeInfo_creationDate,
  )
where

import Amazonka.SWF.CountClosedWorkflowExecutions
import Amazonka.SWF.CountOpenWorkflowExecutions
import Amazonka.SWF.CountPendingActivityTasks
import Amazonka.SWF.CountPendingDecisionTasks
import Amazonka.SWF.DeprecateActivityType
import Amazonka.SWF.DeprecateDomain
import Amazonka.SWF.DeprecateWorkflowType
import Amazonka.SWF.DescribeActivityType
import Amazonka.SWF.DescribeDomain
import Amazonka.SWF.DescribeWorkflowExecution
import Amazonka.SWF.DescribeWorkflowType
import Amazonka.SWF.GetWorkflowExecutionHistory
import Amazonka.SWF.ListActivityTypes
import Amazonka.SWF.ListClosedWorkflowExecutions
import Amazonka.SWF.ListDomains
import Amazonka.SWF.ListOpenWorkflowExecutions
import Amazonka.SWF.ListTagsForResource
import Amazonka.SWF.ListWorkflowTypes
import Amazonka.SWF.PollForActivityTask
import Amazonka.SWF.PollForDecisionTask
import Amazonka.SWF.RecordActivityTaskHeartbeat
import Amazonka.SWF.RegisterActivityType
import Amazonka.SWF.RegisterDomain
import Amazonka.SWF.RegisterWorkflowType
import Amazonka.SWF.RequestCancelWorkflowExecution
import Amazonka.SWF.RespondActivityTaskCanceled
import Amazonka.SWF.RespondActivityTaskCompleted
import Amazonka.SWF.RespondActivityTaskFailed
import Amazonka.SWF.RespondDecisionTaskCompleted
import Amazonka.SWF.SignalWorkflowExecution
import Amazonka.SWF.StartWorkflowExecution
import Amazonka.SWF.TagResource
import Amazonka.SWF.TerminateWorkflowExecution
import Amazonka.SWF.Types.ActivityTaskCancelRequestedEventAttributes
import Amazonka.SWF.Types.ActivityTaskCanceledEventAttributes
import Amazonka.SWF.Types.ActivityTaskCompletedEventAttributes
import Amazonka.SWF.Types.ActivityTaskFailedEventAttributes
import Amazonka.SWF.Types.ActivityTaskScheduledEventAttributes
import Amazonka.SWF.Types.ActivityTaskStartedEventAttributes
import Amazonka.SWF.Types.ActivityTaskTimedOutEventAttributes
import Amazonka.SWF.Types.ActivityType
import Amazonka.SWF.Types.ActivityTypeConfiguration
import Amazonka.SWF.Types.ActivityTypeInfo
import Amazonka.SWF.Types.CancelTimerDecisionAttributes
import Amazonka.SWF.Types.CancelTimerFailedEventAttributes
import Amazonka.SWF.Types.CancelWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.CancelWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
import Amazonka.SWF.Types.CloseStatusFilter
import Amazonka.SWF.Types.CompleteWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.CompleteWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.Decision
import Amazonka.SWF.Types.DecisionTaskCompletedEventAttributes
import Amazonka.SWF.Types.DecisionTaskScheduledEventAttributes
import Amazonka.SWF.Types.DecisionTaskStartedEventAttributes
import Amazonka.SWF.Types.DecisionTaskTimedOutEventAttributes
import Amazonka.SWF.Types.DomainConfiguration
import Amazonka.SWF.Types.DomainInfo
import Amazonka.SWF.Types.ExecutionTimeFilter
import Amazonka.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
import Amazonka.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes
import Amazonka.SWF.Types.FailWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.FailWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.HistoryEvent
import Amazonka.SWF.Types.LambdaFunctionCompletedEventAttributes
import Amazonka.SWF.Types.LambdaFunctionFailedEventAttributes
import Amazonka.SWF.Types.LambdaFunctionScheduledEventAttributes
import Amazonka.SWF.Types.LambdaFunctionStartedEventAttributes
import Amazonka.SWF.Types.LambdaFunctionTimedOutEventAttributes
import Amazonka.SWF.Types.MarkerRecordedEventAttributes
import Amazonka.SWF.Types.PendingTaskCount
import Amazonka.SWF.Types.RecordMarkerDecisionAttributes
import Amazonka.SWF.Types.RecordMarkerFailedEventAttributes
import Amazonka.SWF.Types.RequestCancelActivityTaskDecisionAttributes
import Amazonka.SWF.Types.RequestCancelActivityTaskFailedEventAttributes
import Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
import Amazonka.SWF.Types.ResourceTag
import Amazonka.SWF.Types.ScheduleActivityTaskDecisionAttributes
import Amazonka.SWF.Types.ScheduleActivityTaskFailedEventAttributes
import Amazonka.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
import Amazonka.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
import Amazonka.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.SignalExternalWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
import Amazonka.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.StartChildWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
import Amazonka.SWF.Types.StartLambdaFunctionFailedEventAttributes
import Amazonka.SWF.Types.StartTimerDecisionAttributes
import Amazonka.SWF.Types.StartTimerFailedEventAttributes
import Amazonka.SWF.Types.TagFilter
import Amazonka.SWF.Types.TaskList
import Amazonka.SWF.Types.TimerCanceledEventAttributes
import Amazonka.SWF.Types.TimerFiredEventAttributes
import Amazonka.SWF.Types.TimerStartedEventAttributes
import Amazonka.SWF.Types.WorkflowExecution
import Amazonka.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionCanceledEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionCompletedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionConfiguration
import Amazonka.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionCount
import Amazonka.SWF.Types.WorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionFilter
import Amazonka.SWF.Types.WorkflowExecutionInfo
import Amazonka.SWF.Types.WorkflowExecutionInfos
import Amazonka.SWF.Types.WorkflowExecutionOpenCounts
import Amazonka.SWF.Types.WorkflowExecutionSignaledEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionStartedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionTerminatedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionTimedOutEventAttributes
import Amazonka.SWF.Types.WorkflowType
import Amazonka.SWF.Types.WorkflowTypeConfiguration
import Amazonka.SWF.Types.WorkflowTypeFilter
import Amazonka.SWF.Types.WorkflowTypeInfo
import Amazonka.SWF.UndeprecateActivityType
import Amazonka.SWF.UndeprecateDomain
import Amazonka.SWF.UndeprecateWorkflowType
import Amazonka.SWF.UntagResource

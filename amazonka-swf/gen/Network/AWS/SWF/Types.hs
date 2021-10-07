{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DomainAlreadyExistsFault,
    _WorkflowExecutionAlreadyStartedFault,
    _LimitExceededFault,
    _DomainDeprecatedFault,
    _UnknownResourceFault,
    _OperationNotPermittedFault,
    _TypeAlreadyExistsFault,
    _TooManyTagsFault,
    _TypeDeprecatedFault,
    _DefaultUndefinedFault,

    -- * ActivityTaskTimeoutType
    ActivityTaskTimeoutType (..),

    -- * CancelTimerFailedCause
    CancelTimerFailedCause (..),

    -- * CancelWorkflowExecutionFailedCause
    CancelWorkflowExecutionFailedCause (..),

    -- * ChildPolicy
    ChildPolicy (..),

    -- * CloseStatus
    CloseStatus (..),

    -- * CompleteWorkflowExecutionFailedCause
    CompleteWorkflowExecutionFailedCause (..),

    -- * ContinueAsNewWorkflowExecutionFailedCause
    ContinueAsNewWorkflowExecutionFailedCause (..),

    -- * DecisionTaskTimeoutType
    DecisionTaskTimeoutType (..),

    -- * DecisionType
    DecisionType (..),

    -- * EventType
    EventType (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * FailWorkflowExecutionFailedCause
    FailWorkflowExecutionFailedCause (..),

    -- * LambdaFunctionTimeoutType
    LambdaFunctionTimeoutType (..),

    -- * RecordMarkerFailedCause
    RecordMarkerFailedCause (..),

    -- * RegistrationStatus
    RegistrationStatus (..),

    -- * RequestCancelActivityTaskFailedCause
    RequestCancelActivityTaskFailedCause (..),

    -- * RequestCancelExternalWorkflowExecutionFailedCause
    RequestCancelExternalWorkflowExecutionFailedCause (..),

    -- * ScheduleActivityTaskFailedCause
    ScheduleActivityTaskFailedCause (..),

    -- * ScheduleLambdaFunctionFailedCause
    ScheduleLambdaFunctionFailedCause (..),

    -- * SignalExternalWorkflowExecutionFailedCause
    SignalExternalWorkflowExecutionFailedCause (..),

    -- * StartChildWorkflowExecutionFailedCause
    StartChildWorkflowExecutionFailedCause (..),

    -- * StartLambdaFunctionFailedCause
    StartLambdaFunctionFailedCause (..),

    -- * StartTimerFailedCause
    StartTimerFailedCause (..),

    -- * WorkflowExecutionCancelRequestedCause
    WorkflowExecutionCancelRequestedCause (..),

    -- * WorkflowExecutionTerminatedCause
    WorkflowExecutionTerminatedCause (..),

    -- * WorkflowExecutionTimeoutType
    WorkflowExecutionTimeoutType (..),

    -- * ActivityTaskCancelRequestedEventAttributes
    ActivityTaskCancelRequestedEventAttributes (..),
    newActivityTaskCancelRequestedEventAttributes,
    activityTaskCancelRequestedEventAttributes_decisionTaskCompletedEventId,
    activityTaskCancelRequestedEventAttributes_activityId,

    -- * ActivityTaskCanceledEventAttributes
    ActivityTaskCanceledEventAttributes (..),
    newActivityTaskCanceledEventAttributes,
    activityTaskCanceledEventAttributes_latestCancelRequestedEventId,
    activityTaskCanceledEventAttributes_details,
    activityTaskCanceledEventAttributes_scheduledEventId,
    activityTaskCanceledEventAttributes_startedEventId,

    -- * ActivityTaskCompletedEventAttributes
    ActivityTaskCompletedEventAttributes (..),
    newActivityTaskCompletedEventAttributes,
    activityTaskCompletedEventAttributes_result,
    activityTaskCompletedEventAttributes_scheduledEventId,
    activityTaskCompletedEventAttributes_startedEventId,

    -- * ActivityTaskFailedEventAttributes
    ActivityTaskFailedEventAttributes (..),
    newActivityTaskFailedEventAttributes,
    activityTaskFailedEventAttributes_details,
    activityTaskFailedEventAttributes_reason,
    activityTaskFailedEventAttributes_scheduledEventId,
    activityTaskFailedEventAttributes_startedEventId,

    -- * ActivityTaskScheduledEventAttributes
    ActivityTaskScheduledEventAttributes (..),
    newActivityTaskScheduledEventAttributes,
    activityTaskScheduledEventAttributes_input,
    activityTaskScheduledEventAttributes_heartbeatTimeout,
    activityTaskScheduledEventAttributes_scheduleToCloseTimeout,
    activityTaskScheduledEventAttributes_scheduleToStartTimeout,
    activityTaskScheduledEventAttributes_taskPriority,
    activityTaskScheduledEventAttributes_control,
    activityTaskScheduledEventAttributes_startToCloseTimeout,
    activityTaskScheduledEventAttributes_activityType,
    activityTaskScheduledEventAttributes_activityId,
    activityTaskScheduledEventAttributes_taskList,
    activityTaskScheduledEventAttributes_decisionTaskCompletedEventId,

    -- * ActivityTaskStartedEventAttributes
    ActivityTaskStartedEventAttributes (..),
    newActivityTaskStartedEventAttributes,
    activityTaskStartedEventAttributes_identity,
    activityTaskStartedEventAttributes_scheduledEventId,

    -- * ActivityTaskTimedOutEventAttributes
    ActivityTaskTimedOutEventAttributes (..),
    newActivityTaskTimedOutEventAttributes,
    activityTaskTimedOutEventAttributes_details,
    activityTaskTimedOutEventAttributes_timeoutType,
    activityTaskTimedOutEventAttributes_scheduledEventId,
    activityTaskTimedOutEventAttributes_startedEventId,

    -- * ActivityType
    ActivityType (..),
    newActivityType,
    activityType_name,
    activityType_version,

    -- * ActivityTypeConfiguration
    ActivityTypeConfiguration (..),
    newActivityTypeConfiguration,
    activityTypeConfiguration_defaultTaskPriority,
    activityTypeConfiguration_defaultTaskList,
    activityTypeConfiguration_defaultTaskScheduleToStartTimeout,
    activityTypeConfiguration_defaultTaskStartToCloseTimeout,
    activityTypeConfiguration_defaultTaskScheduleToCloseTimeout,
    activityTypeConfiguration_defaultTaskHeartbeatTimeout,

    -- * ActivityTypeInfo
    ActivityTypeInfo (..),
    newActivityTypeInfo,
    activityTypeInfo_deprecationDate,
    activityTypeInfo_description,
    activityTypeInfo_activityType,
    activityTypeInfo_status,
    activityTypeInfo_creationDate,

    -- * CancelTimerDecisionAttributes
    CancelTimerDecisionAttributes (..),
    newCancelTimerDecisionAttributes,
    cancelTimerDecisionAttributes_timerId,

    -- * CancelTimerFailedEventAttributes
    CancelTimerFailedEventAttributes (..),
    newCancelTimerFailedEventAttributes,
    cancelTimerFailedEventAttributes_timerId,
    cancelTimerFailedEventAttributes_cause,
    cancelTimerFailedEventAttributes_decisionTaskCompletedEventId,

    -- * CancelWorkflowExecutionDecisionAttributes
    CancelWorkflowExecutionDecisionAttributes (..),
    newCancelWorkflowExecutionDecisionAttributes,
    cancelWorkflowExecutionDecisionAttributes_details,

    -- * CancelWorkflowExecutionFailedEventAttributes
    CancelWorkflowExecutionFailedEventAttributes (..),
    newCancelWorkflowExecutionFailedEventAttributes,
    cancelWorkflowExecutionFailedEventAttributes_cause,
    cancelWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * ChildWorkflowExecutionCanceledEventAttributes
    ChildWorkflowExecutionCanceledEventAttributes (..),
    newChildWorkflowExecutionCanceledEventAttributes,
    childWorkflowExecutionCanceledEventAttributes_details,
    childWorkflowExecutionCanceledEventAttributes_workflowExecution,
    childWorkflowExecutionCanceledEventAttributes_workflowType,
    childWorkflowExecutionCanceledEventAttributes_initiatedEventId,
    childWorkflowExecutionCanceledEventAttributes_startedEventId,

    -- * ChildWorkflowExecutionCompletedEventAttributes
    ChildWorkflowExecutionCompletedEventAttributes (..),
    newChildWorkflowExecutionCompletedEventAttributes,
    childWorkflowExecutionCompletedEventAttributes_result,
    childWorkflowExecutionCompletedEventAttributes_workflowExecution,
    childWorkflowExecutionCompletedEventAttributes_workflowType,
    childWorkflowExecutionCompletedEventAttributes_initiatedEventId,
    childWorkflowExecutionCompletedEventAttributes_startedEventId,

    -- * ChildWorkflowExecutionFailedEventAttributes
    ChildWorkflowExecutionFailedEventAttributes (..),
    newChildWorkflowExecutionFailedEventAttributes,
    childWorkflowExecutionFailedEventAttributes_details,
    childWorkflowExecutionFailedEventAttributes_reason,
    childWorkflowExecutionFailedEventAttributes_workflowExecution,
    childWorkflowExecutionFailedEventAttributes_workflowType,
    childWorkflowExecutionFailedEventAttributes_initiatedEventId,
    childWorkflowExecutionFailedEventAttributes_startedEventId,

    -- * ChildWorkflowExecutionStartedEventAttributes
    ChildWorkflowExecutionStartedEventAttributes (..),
    newChildWorkflowExecutionStartedEventAttributes,
    childWorkflowExecutionStartedEventAttributes_workflowExecution,
    childWorkflowExecutionStartedEventAttributes_workflowType,
    childWorkflowExecutionStartedEventAttributes_initiatedEventId,

    -- * ChildWorkflowExecutionTerminatedEventAttributes
    ChildWorkflowExecutionTerminatedEventAttributes (..),
    newChildWorkflowExecutionTerminatedEventAttributes,
    childWorkflowExecutionTerminatedEventAttributes_workflowExecution,
    childWorkflowExecutionTerminatedEventAttributes_workflowType,
    childWorkflowExecutionTerminatedEventAttributes_initiatedEventId,
    childWorkflowExecutionTerminatedEventAttributes_startedEventId,

    -- * ChildWorkflowExecutionTimedOutEventAttributes
    ChildWorkflowExecutionTimedOutEventAttributes (..),
    newChildWorkflowExecutionTimedOutEventAttributes,
    childWorkflowExecutionTimedOutEventAttributes_workflowExecution,
    childWorkflowExecutionTimedOutEventAttributes_workflowType,
    childWorkflowExecutionTimedOutEventAttributes_timeoutType,
    childWorkflowExecutionTimedOutEventAttributes_initiatedEventId,
    childWorkflowExecutionTimedOutEventAttributes_startedEventId,

    -- * CloseStatusFilter
    CloseStatusFilter (..),
    newCloseStatusFilter,
    closeStatusFilter_status,

    -- * CompleteWorkflowExecutionDecisionAttributes
    CompleteWorkflowExecutionDecisionAttributes (..),
    newCompleteWorkflowExecutionDecisionAttributes,
    completeWorkflowExecutionDecisionAttributes_result,

    -- * CompleteWorkflowExecutionFailedEventAttributes
    CompleteWorkflowExecutionFailedEventAttributes (..),
    newCompleteWorkflowExecutionFailedEventAttributes,
    completeWorkflowExecutionFailedEventAttributes_cause,
    completeWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * ContinueAsNewWorkflowExecutionDecisionAttributes
    ContinueAsNewWorkflowExecutionDecisionAttributes (..),
    newContinueAsNewWorkflowExecutionDecisionAttributes,
    continueAsNewWorkflowExecutionDecisionAttributes_input,
    continueAsNewWorkflowExecutionDecisionAttributes_lambdaRole,
    continueAsNewWorkflowExecutionDecisionAttributes_childPolicy,
    continueAsNewWorkflowExecutionDecisionAttributes_taskPriority,
    continueAsNewWorkflowExecutionDecisionAttributes_taskList,
    continueAsNewWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout,
    continueAsNewWorkflowExecutionDecisionAttributes_workflowTypeVersion,
    continueAsNewWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout,
    continueAsNewWorkflowExecutionDecisionAttributes_tagList,

    -- * ContinueAsNewWorkflowExecutionFailedEventAttributes
    ContinueAsNewWorkflowExecutionFailedEventAttributes (..),
    newContinueAsNewWorkflowExecutionFailedEventAttributes,
    continueAsNewWorkflowExecutionFailedEventAttributes_cause,
    continueAsNewWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * Decision
    Decision (..),
    newDecision,
    decision_completeWorkflowExecutionDecisionAttributes,
    decision_failWorkflowExecutionDecisionAttributes,
    decision_startChildWorkflowExecutionDecisionAttributes,
    decision_requestCancelExternalWorkflowExecutionDecisionAttributes,
    decision_recordMarkerDecisionAttributes,
    decision_cancelWorkflowExecutionDecisionAttributes,
    decision_requestCancelActivityTaskDecisionAttributes,
    decision_startTimerDecisionAttributes,
    decision_signalExternalWorkflowExecutionDecisionAttributes,
    decision_scheduleActivityTaskDecisionAttributes,
    decision_scheduleLambdaFunctionDecisionAttributes,
    decision_continueAsNewWorkflowExecutionDecisionAttributes,
    decision_cancelTimerDecisionAttributes,
    decision_decisionType,

    -- * DecisionTaskCompletedEventAttributes
    DecisionTaskCompletedEventAttributes (..),
    newDecisionTaskCompletedEventAttributes,
    decisionTaskCompletedEventAttributes_executionContext,
    decisionTaskCompletedEventAttributes_scheduledEventId,
    decisionTaskCompletedEventAttributes_startedEventId,

    -- * DecisionTaskScheduledEventAttributes
    DecisionTaskScheduledEventAttributes (..),
    newDecisionTaskScheduledEventAttributes,
    decisionTaskScheduledEventAttributes_taskPriority,
    decisionTaskScheduledEventAttributes_startToCloseTimeout,
    decisionTaskScheduledEventAttributes_taskList,

    -- * DecisionTaskStartedEventAttributes
    DecisionTaskStartedEventAttributes (..),
    newDecisionTaskStartedEventAttributes,
    decisionTaskStartedEventAttributes_identity,
    decisionTaskStartedEventAttributes_scheduledEventId,

    -- * DecisionTaskTimedOutEventAttributes
    DecisionTaskTimedOutEventAttributes (..),
    newDecisionTaskTimedOutEventAttributes,
    decisionTaskTimedOutEventAttributes_timeoutType,
    decisionTaskTimedOutEventAttributes_scheduledEventId,
    decisionTaskTimedOutEventAttributes_startedEventId,

    -- * DomainConfiguration
    DomainConfiguration (..),
    newDomainConfiguration,
    domainConfiguration_workflowExecutionRetentionPeriodInDays,

    -- * DomainInfo
    DomainInfo (..),
    newDomainInfo,
    domainInfo_arn,
    domainInfo_description,
    domainInfo_name,
    domainInfo_status,

    -- * ExecutionTimeFilter
    ExecutionTimeFilter (..),
    newExecutionTimeFilter,
    executionTimeFilter_latestDate,
    executionTimeFilter_oldestDate,

    -- * ExternalWorkflowExecutionCancelRequestedEventAttributes
    ExternalWorkflowExecutionCancelRequestedEventAttributes (..),
    newExternalWorkflowExecutionCancelRequestedEventAttributes,
    externalWorkflowExecutionCancelRequestedEventAttributes_workflowExecution,
    externalWorkflowExecutionCancelRequestedEventAttributes_initiatedEventId,

    -- * ExternalWorkflowExecutionSignaledEventAttributes
    ExternalWorkflowExecutionSignaledEventAttributes (..),
    newExternalWorkflowExecutionSignaledEventAttributes,
    externalWorkflowExecutionSignaledEventAttributes_workflowExecution,
    externalWorkflowExecutionSignaledEventAttributes_initiatedEventId,

    -- * FailWorkflowExecutionDecisionAttributes
    FailWorkflowExecutionDecisionAttributes (..),
    newFailWorkflowExecutionDecisionAttributes,
    failWorkflowExecutionDecisionAttributes_details,
    failWorkflowExecutionDecisionAttributes_reason,

    -- * FailWorkflowExecutionFailedEventAttributes
    FailWorkflowExecutionFailedEventAttributes (..),
    newFailWorkflowExecutionFailedEventAttributes,
    failWorkflowExecutionFailedEventAttributes_cause,
    failWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * HistoryEvent
    HistoryEvent (..),
    newHistoryEvent,
    historyEvent_childWorkflowExecutionCanceledEventAttributes,
    historyEvent_requestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    historyEvent_childWorkflowExecutionTimedOutEventAttributes,
    historyEvent_startChildWorkflowExecutionInitiatedEventAttributes,
    historyEvent_decisionTaskScheduledEventAttributes,
    historyEvent_lambdaFunctionStartedEventAttributes,
    historyEvent_activityTaskCanceledEventAttributes,
    historyEvent_activityTaskTimedOutEventAttributes,
    historyEvent_cancelTimerFailedEventAttributes,
    historyEvent_lambdaFunctionTimedOutEventAttributes,
    historyEvent_childWorkflowExecutionStartedEventAttributes,
    historyEvent_externalWorkflowExecutionCancelRequestedEventAttributes,
    historyEvent_timerCanceledEventAttributes,
    historyEvent_childWorkflowExecutionFailedEventAttributes,
    historyEvent_lambdaFunctionCompletedEventAttributes,
    historyEvent_activityTaskFailedEventAttributes,
    historyEvent_completeWorkflowExecutionFailedEventAttributes,
    historyEvent_timerFiredEventAttributes,
    historyEvent_workflowExecutionTimedOutEventAttributes,
    historyEvent_workflowExecutionCanceledEventAttributes,
    historyEvent_scheduleActivityTaskFailedEventAttributes,
    historyEvent_markerRecordedEventAttributes,
    historyEvent_startLambdaFunctionFailedEventAttributes,
    historyEvent_workflowExecutionSignaledEventAttributes,
    historyEvent_activityTaskCancelRequestedEventAttributes,
    historyEvent_activityTaskScheduledEventAttributes,
    historyEvent_decisionTaskStartedEventAttributes,
    historyEvent_recordMarkerFailedEventAttributes,
    historyEvent_lambdaFunctionScheduledEventAttributes,
    historyEvent_startTimerFailedEventAttributes,
    historyEvent_requestCancelActivityTaskFailedEventAttributes,
    historyEvent_cancelWorkflowExecutionFailedEventAttributes,
    historyEvent_workflowExecutionCompletedEventAttributes,
    historyEvent_workflowExecutionTerminatedEventAttributes,
    historyEvent_workflowExecutionCancelRequestedEventAttributes,
    historyEvent_decisionTaskTimedOutEventAttributes,
    historyEvent_workflowExecutionContinuedAsNewEventAttributes,
    historyEvent_workflowExecutionFailedEventAttributes,
    historyEvent_continueAsNewWorkflowExecutionFailedEventAttributes,
    historyEvent_activityTaskCompletedEventAttributes,
    historyEvent_signalExternalWorkflowExecutionInitiatedEventAttributes,
    historyEvent_workflowExecutionStartedEventAttributes,
    historyEvent_startChildWorkflowExecutionFailedEventAttributes,
    historyEvent_failWorkflowExecutionFailedEventAttributes,
    historyEvent_decisionTaskCompletedEventAttributes,
    historyEvent_requestCancelExternalWorkflowExecutionFailedEventAttributes,
    historyEvent_scheduleLambdaFunctionFailedEventAttributes,
    historyEvent_externalWorkflowExecutionSignaledEventAttributes,
    historyEvent_timerStartedEventAttributes,
    historyEvent_childWorkflowExecutionCompletedEventAttributes,
    historyEvent_lambdaFunctionFailedEventAttributes,
    historyEvent_childWorkflowExecutionTerminatedEventAttributes,
    historyEvent_signalExternalWorkflowExecutionFailedEventAttributes,
    historyEvent_activityTaskStartedEventAttributes,
    historyEvent_eventTimestamp,
    historyEvent_eventType,
    historyEvent_eventId,

    -- * LambdaFunctionCompletedEventAttributes
    LambdaFunctionCompletedEventAttributes (..),
    newLambdaFunctionCompletedEventAttributes,
    lambdaFunctionCompletedEventAttributes_result,
    lambdaFunctionCompletedEventAttributes_scheduledEventId,
    lambdaFunctionCompletedEventAttributes_startedEventId,

    -- * LambdaFunctionFailedEventAttributes
    LambdaFunctionFailedEventAttributes (..),
    newLambdaFunctionFailedEventAttributes,
    lambdaFunctionFailedEventAttributes_details,
    lambdaFunctionFailedEventAttributes_reason,
    lambdaFunctionFailedEventAttributes_scheduledEventId,
    lambdaFunctionFailedEventAttributes_startedEventId,

    -- * LambdaFunctionScheduledEventAttributes
    LambdaFunctionScheduledEventAttributes (..),
    newLambdaFunctionScheduledEventAttributes,
    lambdaFunctionScheduledEventAttributes_input,
    lambdaFunctionScheduledEventAttributes_control,
    lambdaFunctionScheduledEventAttributes_startToCloseTimeout,
    lambdaFunctionScheduledEventAttributes_id,
    lambdaFunctionScheduledEventAttributes_name,
    lambdaFunctionScheduledEventAttributes_decisionTaskCompletedEventId,

    -- * LambdaFunctionStartedEventAttributes
    LambdaFunctionStartedEventAttributes (..),
    newLambdaFunctionStartedEventAttributes,
    lambdaFunctionStartedEventAttributes_scheduledEventId,

    -- * LambdaFunctionTimedOutEventAttributes
    LambdaFunctionTimedOutEventAttributes (..),
    newLambdaFunctionTimedOutEventAttributes,
    lambdaFunctionTimedOutEventAttributes_timeoutType,
    lambdaFunctionTimedOutEventAttributes_scheduledEventId,
    lambdaFunctionTimedOutEventAttributes_startedEventId,

    -- * MarkerRecordedEventAttributes
    MarkerRecordedEventAttributes (..),
    newMarkerRecordedEventAttributes,
    markerRecordedEventAttributes_details,
    markerRecordedEventAttributes_markerName,
    markerRecordedEventAttributes_decisionTaskCompletedEventId,

    -- * PendingTaskCount
    PendingTaskCount (..),
    newPendingTaskCount,
    pendingTaskCount_truncated,
    pendingTaskCount_count,

    -- * RecordMarkerDecisionAttributes
    RecordMarkerDecisionAttributes (..),
    newRecordMarkerDecisionAttributes,
    recordMarkerDecisionAttributes_details,
    recordMarkerDecisionAttributes_markerName,

    -- * RecordMarkerFailedEventAttributes
    RecordMarkerFailedEventAttributes (..),
    newRecordMarkerFailedEventAttributes,
    recordMarkerFailedEventAttributes_markerName,
    recordMarkerFailedEventAttributes_cause,
    recordMarkerFailedEventAttributes_decisionTaskCompletedEventId,

    -- * RequestCancelActivityTaskDecisionAttributes
    RequestCancelActivityTaskDecisionAttributes (..),
    newRequestCancelActivityTaskDecisionAttributes,
    requestCancelActivityTaskDecisionAttributes_activityId,

    -- * RequestCancelActivityTaskFailedEventAttributes
    RequestCancelActivityTaskFailedEventAttributes (..),
    newRequestCancelActivityTaskFailedEventAttributes,
    requestCancelActivityTaskFailedEventAttributes_activityId,
    requestCancelActivityTaskFailedEventAttributes_cause,
    requestCancelActivityTaskFailedEventAttributes_decisionTaskCompletedEventId,

    -- * RequestCancelExternalWorkflowExecutionDecisionAttributes
    RequestCancelExternalWorkflowExecutionDecisionAttributes (..),
    newRequestCancelExternalWorkflowExecutionDecisionAttributes,
    requestCancelExternalWorkflowExecutionDecisionAttributes_runId,
    requestCancelExternalWorkflowExecutionDecisionAttributes_control,
    requestCancelExternalWorkflowExecutionDecisionAttributes_workflowId,

    -- * RequestCancelExternalWorkflowExecutionFailedEventAttributes
    RequestCancelExternalWorkflowExecutionFailedEventAttributes (..),
    newRequestCancelExternalWorkflowExecutionFailedEventAttributes,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_runId,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_control,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_workflowId,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_cause,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_initiatedEventId,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (..),
    newRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes_runId,
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes_control,
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes_workflowId,
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_value,
    resourceTag_key,

    -- * ScheduleActivityTaskDecisionAttributes
    ScheduleActivityTaskDecisionAttributes (..),
    newScheduleActivityTaskDecisionAttributes,
    scheduleActivityTaskDecisionAttributes_input,
    scheduleActivityTaskDecisionAttributes_heartbeatTimeout,
    scheduleActivityTaskDecisionAttributes_scheduleToCloseTimeout,
    scheduleActivityTaskDecisionAttributes_scheduleToStartTimeout,
    scheduleActivityTaskDecisionAttributes_taskPriority,
    scheduleActivityTaskDecisionAttributes_taskList,
    scheduleActivityTaskDecisionAttributes_control,
    scheduleActivityTaskDecisionAttributes_startToCloseTimeout,
    scheduleActivityTaskDecisionAttributes_activityType,
    scheduleActivityTaskDecisionAttributes_activityId,

    -- * ScheduleActivityTaskFailedEventAttributes
    ScheduleActivityTaskFailedEventAttributes (..),
    newScheduleActivityTaskFailedEventAttributes,
    scheduleActivityTaskFailedEventAttributes_activityType,
    scheduleActivityTaskFailedEventAttributes_activityId,
    scheduleActivityTaskFailedEventAttributes_cause,
    scheduleActivityTaskFailedEventAttributes_decisionTaskCompletedEventId,

    -- * ScheduleLambdaFunctionDecisionAttributes
    ScheduleLambdaFunctionDecisionAttributes (..),
    newScheduleLambdaFunctionDecisionAttributes,
    scheduleLambdaFunctionDecisionAttributes_input,
    scheduleLambdaFunctionDecisionAttributes_control,
    scheduleLambdaFunctionDecisionAttributes_startToCloseTimeout,
    scheduleLambdaFunctionDecisionAttributes_id,
    scheduleLambdaFunctionDecisionAttributes_name,

    -- * ScheduleLambdaFunctionFailedEventAttributes
    ScheduleLambdaFunctionFailedEventAttributes (..),
    newScheduleLambdaFunctionFailedEventAttributes,
    scheduleLambdaFunctionFailedEventAttributes_id,
    scheduleLambdaFunctionFailedEventAttributes_name,
    scheduleLambdaFunctionFailedEventAttributes_cause,
    scheduleLambdaFunctionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * SignalExternalWorkflowExecutionDecisionAttributes
    SignalExternalWorkflowExecutionDecisionAttributes (..),
    newSignalExternalWorkflowExecutionDecisionAttributes,
    signalExternalWorkflowExecutionDecisionAttributes_runId,
    signalExternalWorkflowExecutionDecisionAttributes_input,
    signalExternalWorkflowExecutionDecisionAttributes_control,
    signalExternalWorkflowExecutionDecisionAttributes_workflowId,
    signalExternalWorkflowExecutionDecisionAttributes_signalName,

    -- * SignalExternalWorkflowExecutionFailedEventAttributes
    SignalExternalWorkflowExecutionFailedEventAttributes (..),
    newSignalExternalWorkflowExecutionFailedEventAttributes,
    signalExternalWorkflowExecutionFailedEventAttributes_runId,
    signalExternalWorkflowExecutionFailedEventAttributes_control,
    signalExternalWorkflowExecutionFailedEventAttributes_workflowId,
    signalExternalWorkflowExecutionFailedEventAttributes_cause,
    signalExternalWorkflowExecutionFailedEventAttributes_initiatedEventId,
    signalExternalWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * SignalExternalWorkflowExecutionInitiatedEventAttributes
    SignalExternalWorkflowExecutionInitiatedEventAttributes (..),
    newSignalExternalWorkflowExecutionInitiatedEventAttributes,
    signalExternalWorkflowExecutionInitiatedEventAttributes_runId,
    signalExternalWorkflowExecutionInitiatedEventAttributes_input,
    signalExternalWorkflowExecutionInitiatedEventAttributes_control,
    signalExternalWorkflowExecutionInitiatedEventAttributes_workflowId,
    signalExternalWorkflowExecutionInitiatedEventAttributes_signalName,
    signalExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId,

    -- * StartChildWorkflowExecutionDecisionAttributes
    StartChildWorkflowExecutionDecisionAttributes (..),
    newStartChildWorkflowExecutionDecisionAttributes,
    startChildWorkflowExecutionDecisionAttributes_input,
    startChildWorkflowExecutionDecisionAttributes_lambdaRole,
    startChildWorkflowExecutionDecisionAttributes_childPolicy,
    startChildWorkflowExecutionDecisionAttributes_taskPriority,
    startChildWorkflowExecutionDecisionAttributes_taskList,
    startChildWorkflowExecutionDecisionAttributes_control,
    startChildWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout,
    startChildWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout,
    startChildWorkflowExecutionDecisionAttributes_tagList,
    startChildWorkflowExecutionDecisionAttributes_workflowType,
    startChildWorkflowExecutionDecisionAttributes_workflowId,

    -- * StartChildWorkflowExecutionFailedEventAttributes
    StartChildWorkflowExecutionFailedEventAttributes (..),
    newStartChildWorkflowExecutionFailedEventAttributes,
    startChildWorkflowExecutionFailedEventAttributes_control,
    startChildWorkflowExecutionFailedEventAttributes_workflowType,
    startChildWorkflowExecutionFailedEventAttributes_cause,
    startChildWorkflowExecutionFailedEventAttributes_workflowId,
    startChildWorkflowExecutionFailedEventAttributes_initiatedEventId,
    startChildWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * StartChildWorkflowExecutionInitiatedEventAttributes
    StartChildWorkflowExecutionInitiatedEventAttributes (..),
    newStartChildWorkflowExecutionInitiatedEventAttributes,
    startChildWorkflowExecutionInitiatedEventAttributes_input,
    startChildWorkflowExecutionInitiatedEventAttributes_lambdaRole,
    startChildWorkflowExecutionInitiatedEventAttributes_taskPriority,
    startChildWorkflowExecutionInitiatedEventAttributes_control,
    startChildWorkflowExecutionInitiatedEventAttributes_executionStartToCloseTimeout,
    startChildWorkflowExecutionInitiatedEventAttributes_taskStartToCloseTimeout,
    startChildWorkflowExecutionInitiatedEventAttributes_tagList,
    startChildWorkflowExecutionInitiatedEventAttributes_workflowId,
    startChildWorkflowExecutionInitiatedEventAttributes_workflowType,
    startChildWorkflowExecutionInitiatedEventAttributes_taskList,
    startChildWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId,
    startChildWorkflowExecutionInitiatedEventAttributes_childPolicy,

    -- * StartLambdaFunctionFailedEventAttributes
    StartLambdaFunctionFailedEventAttributes (..),
    newStartLambdaFunctionFailedEventAttributes,
    startLambdaFunctionFailedEventAttributes_message,
    startLambdaFunctionFailedEventAttributes_scheduledEventId,
    startLambdaFunctionFailedEventAttributes_cause,

    -- * StartTimerDecisionAttributes
    StartTimerDecisionAttributes (..),
    newStartTimerDecisionAttributes,
    startTimerDecisionAttributes_control,
    startTimerDecisionAttributes_timerId,
    startTimerDecisionAttributes_startToFireTimeout,

    -- * StartTimerFailedEventAttributes
    StartTimerFailedEventAttributes (..),
    newStartTimerFailedEventAttributes,
    startTimerFailedEventAttributes_timerId,
    startTimerFailedEventAttributes_cause,
    startTimerFailedEventAttributes_decisionTaskCompletedEventId,

    -- * TagFilter
    TagFilter (..),
    newTagFilter,
    tagFilter_tag,

    -- * TaskList
    TaskList (..),
    newTaskList,
    taskList_name,

    -- * TimerCanceledEventAttributes
    TimerCanceledEventAttributes (..),
    newTimerCanceledEventAttributes,
    timerCanceledEventAttributes_timerId,
    timerCanceledEventAttributes_startedEventId,
    timerCanceledEventAttributes_decisionTaskCompletedEventId,

    -- * TimerFiredEventAttributes
    TimerFiredEventAttributes (..),
    newTimerFiredEventAttributes,
    timerFiredEventAttributes_timerId,
    timerFiredEventAttributes_startedEventId,

    -- * TimerStartedEventAttributes
    TimerStartedEventAttributes (..),
    newTimerStartedEventAttributes,
    timerStartedEventAttributes_control,
    timerStartedEventAttributes_timerId,
    timerStartedEventAttributes_startToFireTimeout,
    timerStartedEventAttributes_decisionTaskCompletedEventId,

    -- * WorkflowExecution
    WorkflowExecution (..),
    newWorkflowExecution,
    workflowExecution_workflowId,
    workflowExecution_runId,

    -- * WorkflowExecutionCancelRequestedEventAttributes
    WorkflowExecutionCancelRequestedEventAttributes (..),
    newWorkflowExecutionCancelRequestedEventAttributes,
    workflowExecutionCancelRequestedEventAttributes_externalInitiatedEventId,
    workflowExecutionCancelRequestedEventAttributes_externalWorkflowExecution,
    workflowExecutionCancelRequestedEventAttributes_cause,

    -- * WorkflowExecutionCanceledEventAttributes
    WorkflowExecutionCanceledEventAttributes (..),
    newWorkflowExecutionCanceledEventAttributes,
    workflowExecutionCanceledEventAttributes_details,
    workflowExecutionCanceledEventAttributes_decisionTaskCompletedEventId,

    -- * WorkflowExecutionCompletedEventAttributes
    WorkflowExecutionCompletedEventAttributes (..),
    newWorkflowExecutionCompletedEventAttributes,
    workflowExecutionCompletedEventAttributes_result,
    workflowExecutionCompletedEventAttributes_decisionTaskCompletedEventId,

    -- * WorkflowExecutionConfiguration
    WorkflowExecutionConfiguration (..),
    newWorkflowExecutionConfiguration,
    workflowExecutionConfiguration_lambdaRole,
    workflowExecutionConfiguration_taskPriority,
    workflowExecutionConfiguration_taskStartToCloseTimeout,
    workflowExecutionConfiguration_executionStartToCloseTimeout,
    workflowExecutionConfiguration_taskList,
    workflowExecutionConfiguration_childPolicy,

    -- * WorkflowExecutionContinuedAsNewEventAttributes
    WorkflowExecutionContinuedAsNewEventAttributes (..),
    newWorkflowExecutionContinuedAsNewEventAttributes,
    workflowExecutionContinuedAsNewEventAttributes_input,
    workflowExecutionContinuedAsNewEventAttributes_lambdaRole,
    workflowExecutionContinuedAsNewEventAttributes_taskPriority,
    workflowExecutionContinuedAsNewEventAttributes_executionStartToCloseTimeout,
    workflowExecutionContinuedAsNewEventAttributes_taskStartToCloseTimeout,
    workflowExecutionContinuedAsNewEventAttributes_tagList,
    workflowExecutionContinuedAsNewEventAttributes_decisionTaskCompletedEventId,
    workflowExecutionContinuedAsNewEventAttributes_newExecutionRunId,
    workflowExecutionContinuedAsNewEventAttributes_taskList,
    workflowExecutionContinuedAsNewEventAttributes_childPolicy,
    workflowExecutionContinuedAsNewEventAttributes_workflowType,

    -- * WorkflowExecutionCount
    WorkflowExecutionCount (..),
    newWorkflowExecutionCount,
    workflowExecutionCount_truncated,
    workflowExecutionCount_count,

    -- * WorkflowExecutionFailedEventAttributes
    WorkflowExecutionFailedEventAttributes (..),
    newWorkflowExecutionFailedEventAttributes,
    workflowExecutionFailedEventAttributes_details,
    workflowExecutionFailedEventAttributes_reason,
    workflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * WorkflowExecutionFilter
    WorkflowExecutionFilter (..),
    newWorkflowExecutionFilter,
    workflowExecutionFilter_workflowId,

    -- * WorkflowExecutionInfo
    WorkflowExecutionInfo (..),
    newWorkflowExecutionInfo,
    workflowExecutionInfo_parent,
    workflowExecutionInfo_closeStatus,
    workflowExecutionInfo_cancelRequested,
    workflowExecutionInfo_closeTimestamp,
    workflowExecutionInfo_tagList,
    workflowExecutionInfo_execution,
    workflowExecutionInfo_workflowType,
    workflowExecutionInfo_startTimestamp,
    workflowExecutionInfo_executionStatus,

    -- * WorkflowExecutionInfos
    WorkflowExecutionInfos (..),
    newWorkflowExecutionInfos,
    workflowExecutionInfos_nextPageToken,
    workflowExecutionInfos_executionInfos,

    -- * WorkflowExecutionOpenCounts
    WorkflowExecutionOpenCounts (..),
    newWorkflowExecutionOpenCounts,
    workflowExecutionOpenCounts_openLambdaFunctions,
    workflowExecutionOpenCounts_openActivityTasks,
    workflowExecutionOpenCounts_openDecisionTasks,
    workflowExecutionOpenCounts_openTimers,
    workflowExecutionOpenCounts_openChildWorkflowExecutions,

    -- * WorkflowExecutionSignaledEventAttributes
    WorkflowExecutionSignaledEventAttributes (..),
    newWorkflowExecutionSignaledEventAttributes,
    workflowExecutionSignaledEventAttributes_input,
    workflowExecutionSignaledEventAttributes_externalInitiatedEventId,
    workflowExecutionSignaledEventAttributes_externalWorkflowExecution,
    workflowExecutionSignaledEventAttributes_signalName,

    -- * WorkflowExecutionStartedEventAttributes
    WorkflowExecutionStartedEventAttributes (..),
    newWorkflowExecutionStartedEventAttributes,
    workflowExecutionStartedEventAttributes_input,
    workflowExecutionStartedEventAttributes_lambdaRole,
    workflowExecutionStartedEventAttributes_continuedExecutionRunId,
    workflowExecutionStartedEventAttributes_parentInitiatedEventId,
    workflowExecutionStartedEventAttributes_taskPriority,
    workflowExecutionStartedEventAttributes_executionStartToCloseTimeout,
    workflowExecutionStartedEventAttributes_taskStartToCloseTimeout,
    workflowExecutionStartedEventAttributes_tagList,
    workflowExecutionStartedEventAttributes_parentWorkflowExecution,
    workflowExecutionStartedEventAttributes_childPolicy,
    workflowExecutionStartedEventAttributes_taskList,
    workflowExecutionStartedEventAttributes_workflowType,

    -- * WorkflowExecutionTerminatedEventAttributes
    WorkflowExecutionTerminatedEventAttributes (..),
    newWorkflowExecutionTerminatedEventAttributes,
    workflowExecutionTerminatedEventAttributes_details,
    workflowExecutionTerminatedEventAttributes_reason,
    workflowExecutionTerminatedEventAttributes_cause,
    workflowExecutionTerminatedEventAttributes_childPolicy,

    -- * WorkflowExecutionTimedOutEventAttributes
    WorkflowExecutionTimedOutEventAttributes (..),
    newWorkflowExecutionTimedOutEventAttributes,
    workflowExecutionTimedOutEventAttributes_timeoutType,
    workflowExecutionTimedOutEventAttributes_childPolicy,

    -- * WorkflowType
    WorkflowType (..),
    newWorkflowType,
    workflowType_name,
    workflowType_version,

    -- * WorkflowTypeConfiguration
    WorkflowTypeConfiguration (..),
    newWorkflowTypeConfiguration,
    workflowTypeConfiguration_defaultExecutionStartToCloseTimeout,
    workflowTypeConfiguration_defaultTaskPriority,
    workflowTypeConfiguration_defaultTaskList,
    workflowTypeConfiguration_defaultChildPolicy,
    workflowTypeConfiguration_defaultTaskStartToCloseTimeout,
    workflowTypeConfiguration_defaultLambdaRole,

    -- * WorkflowTypeFilter
    WorkflowTypeFilter (..),
    newWorkflowTypeFilter,
    workflowTypeFilter_version,
    workflowTypeFilter_name,

    -- * WorkflowTypeInfo
    WorkflowTypeInfo (..),
    newWorkflowTypeInfo,
    workflowTypeInfo_deprecationDate,
    workflowTypeInfo_description,
    workflowTypeInfo_workflowType,
    workflowTypeInfo_status,
    workflowTypeInfo_creationDate,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.ActivityTaskCancelRequestedEventAttributes
import Network.AWS.SWF.Types.ActivityTaskCanceledEventAttributes
import Network.AWS.SWF.Types.ActivityTaskCompletedEventAttributes
import Network.AWS.SWF.Types.ActivityTaskFailedEventAttributes
import Network.AWS.SWF.Types.ActivityTaskScheduledEventAttributes
import Network.AWS.SWF.Types.ActivityTaskStartedEventAttributes
import Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes
import Network.AWS.SWF.Types.ActivityTaskTimeoutType
import Network.AWS.SWF.Types.ActivityType
import Network.AWS.SWF.Types.ActivityTypeConfiguration
import Network.AWS.SWF.Types.ActivityTypeInfo
import Network.AWS.SWF.Types.CancelTimerDecisionAttributes
import Network.AWS.SWF.Types.CancelTimerFailedCause
import Network.AWS.SWF.Types.CancelTimerFailedEventAttributes
import Network.AWS.SWF.Types.CancelWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.CancelWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.CancelWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
import Network.AWS.SWF.Types.CloseStatus
import Network.AWS.SWF.Types.CloseStatusFilter
import Network.AWS.SWF.Types.CompleteWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.CompleteWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.CompleteWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.Decision
import Network.AWS.SWF.Types.DecisionTaskCompletedEventAttributes
import Network.AWS.SWF.Types.DecisionTaskScheduledEventAttributes
import Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes
import Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes
import Network.AWS.SWF.Types.DecisionTaskTimeoutType
import Network.AWS.SWF.Types.DecisionType
import Network.AWS.SWF.Types.DomainConfiguration
import Network.AWS.SWF.Types.DomainInfo
import Network.AWS.SWF.Types.EventType
import Network.AWS.SWF.Types.ExecutionStatus
import Network.AWS.SWF.Types.ExecutionTimeFilter
import Network.AWS.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
import Network.AWS.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes
import Network.AWS.SWF.Types.FailWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.FailWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.FailWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.HistoryEvent
import Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionTimedOutEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionTimeoutType
import Network.AWS.SWF.Types.MarkerRecordedEventAttributes
import Network.AWS.SWF.Types.PendingTaskCount
import Network.AWS.SWF.Types.RecordMarkerDecisionAttributes
import Network.AWS.SWF.Types.RecordMarkerFailedCause
import Network.AWS.SWF.Types.RecordMarkerFailedEventAttributes
import Network.AWS.SWF.Types.RegistrationStatus
import Network.AWS.SWF.Types.RequestCancelActivityTaskDecisionAttributes
import Network.AWS.SWF.Types.RequestCancelActivityTaskFailedCause
import Network.AWS.SWF.Types.RequestCancelActivityTaskFailedEventAttributes
import Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
import Network.AWS.SWF.Types.ResourceTag
import Network.AWS.SWF.Types.ScheduleActivityTaskDecisionAttributes
import Network.AWS.SWF.Types.ScheduleActivityTaskFailedCause
import Network.AWS.SWF.Types.ScheduleActivityTaskFailedEventAttributes
import Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
import Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedCause
import Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
import Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
import Network.AWS.SWF.Types.StartLambdaFunctionFailedCause
import Network.AWS.SWF.Types.StartLambdaFunctionFailedEventAttributes
import Network.AWS.SWF.Types.StartTimerDecisionAttributes
import Network.AWS.SWF.Types.StartTimerFailedCause
import Network.AWS.SWF.Types.StartTimerFailedEventAttributes
import Network.AWS.SWF.Types.TagFilter
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.TimerCanceledEventAttributes
import Network.AWS.SWF.Types.TimerFiredEventAttributes
import Network.AWS.SWF.Types.TimerStartedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedCause
import Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionConfiguration
import Network.AWS.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionCount
import Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionFilter
import Network.AWS.SWF.Types.WorkflowExecutionInfo
import Network.AWS.SWF.Types.WorkflowExecutionInfos
import Network.AWS.SWF.Types.WorkflowExecutionOpenCounts
import Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause
import Network.AWS.SWF.Types.WorkflowExecutionTerminatedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionTimedOutEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionTimeoutType
import Network.AWS.SWF.Types.WorkflowType
import Network.AWS.SWF.Types.WorkflowTypeConfiguration
import Network.AWS.SWF.Types.WorkflowTypeFilter
import Network.AWS.SWF.Types.WorkflowTypeInfo
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-01-25@ of the Amazon Simple Workflow Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SWF",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "swf",
      Core._serviceSigningName = "swf",
      Core._serviceVersion = "2012-01-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "SWF",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Returned if the domain already exists. You may get this fault if you are
-- registering a domain that is either already registered or deprecated, or
-- if you undeprecate a domain that is currently registered.
_DomainAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DomainAlreadyExistsFault"

-- | Returned by StartWorkflowExecution when an open execution with the same
-- workflowId is already running in the specified domain.
_WorkflowExecutionAlreadyStartedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_WorkflowExecutionAlreadyStartedFault =
  Core._MatchServiceError
    defaultService
    "WorkflowExecutionAlreadyStartedFault"

-- | Returned by any operation if a system imposed limitation has been
-- reached. To address this fault you should either clean up unused
-- resources or increase the limit by contacting AWS.
_LimitExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError
    defaultService
    "LimitExceededFault"

-- | Returned when the specified domain has been deprecated.
_DomainDeprecatedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainDeprecatedFault =
  Core._MatchServiceError
    defaultService
    "DomainDeprecatedFault"

-- | Returned when the named resource cannot be found with in the scope of
-- this operation (region or domain). This could happen if the named
-- resource was never created or is no longer available for this operation.
_UnknownResourceFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnknownResourceFault =
  Core._MatchServiceError
    defaultService
    "UnknownResourceFault"

-- | Returned when the caller doesn\'t have sufficient permissions to invoke
-- the action.
_OperationNotPermittedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedFault =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedFault"

-- | Returned if the type already exists in the specified domain. You may get
-- this fault if you are registering a type that is either already
-- registered or deprecated, or if you undeprecate a type that is currently
-- registered.
_TypeAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TypeAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "TypeAlreadyExistsFault"

-- | You\'ve exceeded the number of tags allowed for a domain.
_TooManyTagsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsFault =
  Core._MatchServiceError
    defaultService
    "TooManyTagsFault"

-- | Returned when the specified activity or workflow type was already
-- deprecated.
_TypeDeprecatedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TypeDeprecatedFault =
  Core._MatchServiceError
    defaultService
    "TypeDeprecatedFault"

-- | The @StartWorkflowExecution@ API action was called without the required
-- parameters set.
--
-- Some workflow execution parameters, such as the decision @taskList@,
-- must be set to start the execution. However, these parameters might have
-- been set as defaults when the workflow type was registered. In this
-- case, you can omit these parameters from the @StartWorkflowExecution@
-- call and Amazon SWF uses the values defined in the workflow type.
--
-- If these parameters aren\'t set and no default parameters were defined
-- in the workflow type, this error is displayed.
_DefaultUndefinedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DefaultUndefinedFault =
  Core._MatchServiceError
    defaultService
    "DefaultUndefinedFault"

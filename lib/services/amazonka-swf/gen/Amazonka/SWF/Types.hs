{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SWF.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DefaultUndefinedFault,
    _DomainAlreadyExistsFault,
    _DomainDeprecatedFault,
    _LimitExceededFault,
    _OperationNotPermittedFault,
    _TooManyTagsFault,
    _TypeAlreadyExistsFault,
    _TypeDeprecatedFault,
    _UnknownResourceFault,
    _WorkflowExecutionAlreadyStartedFault,

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
    activityTaskCanceledEventAttributes_details,
    activityTaskCanceledEventAttributes_latestCancelRequestedEventId,
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
    activityTypeConfiguration_defaultTaskHeartbeatTimeout,
    activityTypeConfiguration_defaultTaskList,
    activityTypeConfiguration_defaultTaskPriority,
    activityTypeConfiguration_defaultTaskScheduleToCloseTimeout,
    activityTypeConfiguration_defaultTaskScheduleToStartTimeout,
    activityTypeConfiguration_defaultTaskStartToCloseTimeout,

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
    continueAsNewWorkflowExecutionDecisionAttributes_childPolicy,
    continueAsNewWorkflowExecutionDecisionAttributes_executionStartToCloseTimeout,
    continueAsNewWorkflowExecutionDecisionAttributes_input,
    continueAsNewWorkflowExecutionDecisionAttributes_lambdaRole,
    continueAsNewWorkflowExecutionDecisionAttributes_tagList,
    continueAsNewWorkflowExecutionDecisionAttributes_taskList,
    continueAsNewWorkflowExecutionDecisionAttributes_taskPriority,
    continueAsNewWorkflowExecutionDecisionAttributes_taskStartToCloseTimeout,
    continueAsNewWorkflowExecutionDecisionAttributes_workflowTypeVersion,

    -- * ContinueAsNewWorkflowExecutionFailedEventAttributes
    ContinueAsNewWorkflowExecutionFailedEventAttributes (..),
    newContinueAsNewWorkflowExecutionFailedEventAttributes,
    continueAsNewWorkflowExecutionFailedEventAttributes_cause,
    continueAsNewWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * Decision
    Decision (..),
    newDecision,
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

    -- * DecisionTaskCompletedEventAttributes
    DecisionTaskCompletedEventAttributes (..),
    newDecisionTaskCompletedEventAttributes,
    decisionTaskCompletedEventAttributes_executionContext,
    decisionTaskCompletedEventAttributes_scheduledEventId,
    decisionTaskCompletedEventAttributes_startedEventId,

    -- * DecisionTaskScheduledEventAttributes
    DecisionTaskScheduledEventAttributes (..),
    newDecisionTaskScheduledEventAttributes,
    decisionTaskScheduledEventAttributes_startToCloseTimeout,
    decisionTaskScheduledEventAttributes_taskPriority,
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
    lambdaFunctionScheduledEventAttributes_control,
    lambdaFunctionScheduledEventAttributes_input,
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
    requestCancelExternalWorkflowExecutionDecisionAttributes_control,
    requestCancelExternalWorkflowExecutionDecisionAttributes_runId,
    requestCancelExternalWorkflowExecutionDecisionAttributes_workflowId,

    -- * RequestCancelExternalWorkflowExecutionFailedEventAttributes
    RequestCancelExternalWorkflowExecutionFailedEventAttributes (..),
    newRequestCancelExternalWorkflowExecutionFailedEventAttributes,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_control,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_runId,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_workflowId,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_cause,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_initiatedEventId,
    requestCancelExternalWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (..),
    newRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes_control,
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes_runId,
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
    scheduleLambdaFunctionDecisionAttributes_control,
    scheduleLambdaFunctionDecisionAttributes_input,
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
    signalExternalWorkflowExecutionDecisionAttributes_control,
    signalExternalWorkflowExecutionDecisionAttributes_input,
    signalExternalWorkflowExecutionDecisionAttributes_runId,
    signalExternalWorkflowExecutionDecisionAttributes_workflowId,
    signalExternalWorkflowExecutionDecisionAttributes_signalName,

    -- * SignalExternalWorkflowExecutionFailedEventAttributes
    SignalExternalWorkflowExecutionFailedEventAttributes (..),
    newSignalExternalWorkflowExecutionFailedEventAttributes,
    signalExternalWorkflowExecutionFailedEventAttributes_control,
    signalExternalWorkflowExecutionFailedEventAttributes_runId,
    signalExternalWorkflowExecutionFailedEventAttributes_workflowId,
    signalExternalWorkflowExecutionFailedEventAttributes_cause,
    signalExternalWorkflowExecutionFailedEventAttributes_initiatedEventId,
    signalExternalWorkflowExecutionFailedEventAttributes_decisionTaskCompletedEventId,

    -- * SignalExternalWorkflowExecutionInitiatedEventAttributes
    SignalExternalWorkflowExecutionInitiatedEventAttributes (..),
    newSignalExternalWorkflowExecutionInitiatedEventAttributes,
    signalExternalWorkflowExecutionInitiatedEventAttributes_control,
    signalExternalWorkflowExecutionInitiatedEventAttributes_input,
    signalExternalWorkflowExecutionInitiatedEventAttributes_runId,
    signalExternalWorkflowExecutionInitiatedEventAttributes_workflowId,
    signalExternalWorkflowExecutionInitiatedEventAttributes_signalName,
    signalExternalWorkflowExecutionInitiatedEventAttributes_decisionTaskCompletedEventId,

    -- * StartChildWorkflowExecutionDecisionAttributes
    StartChildWorkflowExecutionDecisionAttributes (..),
    newStartChildWorkflowExecutionDecisionAttributes,
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

    -- * StartLambdaFunctionFailedEventAttributes
    StartLambdaFunctionFailedEventAttributes (..),
    newStartLambdaFunctionFailedEventAttributes,
    startLambdaFunctionFailedEventAttributes_cause,
    startLambdaFunctionFailedEventAttributes_message,
    startLambdaFunctionFailedEventAttributes_scheduledEventId,

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
    workflowExecutionCancelRequestedEventAttributes_cause,
    workflowExecutionCancelRequestedEventAttributes_externalInitiatedEventId,
    workflowExecutionCancelRequestedEventAttributes_externalWorkflowExecution,

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
    workflowExecutionInfo_cancelRequested,
    workflowExecutionInfo_closeStatus,
    workflowExecutionInfo_closeTimestamp,
    workflowExecutionInfo_parent,
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
    workflowExecutionSignaledEventAttributes_externalInitiatedEventId,
    workflowExecutionSignaledEventAttributes_externalWorkflowExecution,
    workflowExecutionSignaledEventAttributes_input,
    workflowExecutionSignaledEventAttributes_signalName,

    -- * WorkflowExecutionStartedEventAttributes
    WorkflowExecutionStartedEventAttributes (..),
    newWorkflowExecutionStartedEventAttributes,
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

    -- * WorkflowExecutionTerminatedEventAttributes
    WorkflowExecutionTerminatedEventAttributes (..),
    newWorkflowExecutionTerminatedEventAttributes,
    workflowExecutionTerminatedEventAttributes_cause,
    workflowExecutionTerminatedEventAttributes_details,
    workflowExecutionTerminatedEventAttributes_reason,
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
    workflowTypeConfiguration_defaultChildPolicy,
    workflowTypeConfiguration_defaultExecutionStartToCloseTimeout,
    workflowTypeConfiguration_defaultLambdaRole,
    workflowTypeConfiguration_defaultTaskList,
    workflowTypeConfiguration_defaultTaskPriority,
    workflowTypeConfiguration_defaultTaskStartToCloseTimeout,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.ActivityTaskCancelRequestedEventAttributes
import Amazonka.SWF.Types.ActivityTaskCanceledEventAttributes
import Amazonka.SWF.Types.ActivityTaskCompletedEventAttributes
import Amazonka.SWF.Types.ActivityTaskFailedEventAttributes
import Amazonka.SWF.Types.ActivityTaskScheduledEventAttributes
import Amazonka.SWF.Types.ActivityTaskStartedEventAttributes
import Amazonka.SWF.Types.ActivityTaskTimedOutEventAttributes
import Amazonka.SWF.Types.ActivityTaskTimeoutType
import Amazonka.SWF.Types.ActivityType
import Amazonka.SWF.Types.ActivityTypeConfiguration
import Amazonka.SWF.Types.ActivityTypeInfo
import Amazonka.SWF.Types.CancelTimerDecisionAttributes
import Amazonka.SWF.Types.CancelTimerFailedCause
import Amazonka.SWF.Types.CancelTimerFailedEventAttributes
import Amazonka.SWF.Types.CancelWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.CancelWorkflowExecutionFailedCause
import Amazonka.SWF.Types.CancelWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.ChildPolicy
import Amazonka.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
import Amazonka.SWF.Types.CloseStatus
import Amazonka.SWF.Types.CloseStatusFilter
import Amazonka.SWF.Types.CompleteWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.CompleteWorkflowExecutionFailedCause
import Amazonka.SWF.Types.CompleteWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
import Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.Decision
import Amazonka.SWF.Types.DecisionTaskCompletedEventAttributes
import Amazonka.SWF.Types.DecisionTaskScheduledEventAttributes
import Amazonka.SWF.Types.DecisionTaskStartedEventAttributes
import Amazonka.SWF.Types.DecisionTaskTimedOutEventAttributes
import Amazonka.SWF.Types.DecisionTaskTimeoutType
import Amazonka.SWF.Types.DecisionType
import Amazonka.SWF.Types.DomainConfiguration
import Amazonka.SWF.Types.DomainInfo
import Amazonka.SWF.Types.EventType
import Amazonka.SWF.Types.ExecutionStatus
import Amazonka.SWF.Types.ExecutionTimeFilter
import Amazonka.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
import Amazonka.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes
import Amazonka.SWF.Types.FailWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.FailWorkflowExecutionFailedCause
import Amazonka.SWF.Types.FailWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.HistoryEvent
import Amazonka.SWF.Types.LambdaFunctionCompletedEventAttributes
import Amazonka.SWF.Types.LambdaFunctionFailedEventAttributes
import Amazonka.SWF.Types.LambdaFunctionScheduledEventAttributes
import Amazonka.SWF.Types.LambdaFunctionStartedEventAttributes
import Amazonka.SWF.Types.LambdaFunctionTimedOutEventAttributes
import Amazonka.SWF.Types.LambdaFunctionTimeoutType
import Amazonka.SWF.Types.MarkerRecordedEventAttributes
import Amazonka.SWF.Types.PendingTaskCount
import Amazonka.SWF.Types.RecordMarkerDecisionAttributes
import Amazonka.SWF.Types.RecordMarkerFailedCause
import Amazonka.SWF.Types.RecordMarkerFailedEventAttributes
import Amazonka.SWF.Types.RegistrationStatus
import Amazonka.SWF.Types.RequestCancelActivityTaskDecisionAttributes
import Amazonka.SWF.Types.RequestCancelActivityTaskFailedCause
import Amazonka.SWF.Types.RequestCancelActivityTaskFailedEventAttributes
import Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionFailedCause
import Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
import Amazonka.SWF.Types.ResourceTag
import Amazonka.SWF.Types.ScheduleActivityTaskDecisionAttributes
import Amazonka.SWF.Types.ScheduleActivityTaskFailedCause
import Amazonka.SWF.Types.ScheduleActivityTaskFailedEventAttributes
import Amazonka.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
import Amazonka.SWF.Types.ScheduleLambdaFunctionFailedCause
import Amazonka.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
import Amazonka.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.SignalExternalWorkflowExecutionFailedCause
import Amazonka.SWF.Types.SignalExternalWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
import Amazonka.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.StartChildWorkflowExecutionFailedCause
import Amazonka.SWF.Types.StartChildWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
import Amazonka.SWF.Types.StartLambdaFunctionFailedCause
import Amazonka.SWF.Types.StartLambdaFunctionFailedEventAttributes
import Amazonka.SWF.Types.StartTimerDecisionAttributes
import Amazonka.SWF.Types.StartTimerFailedCause
import Amazonka.SWF.Types.StartTimerFailedEventAttributes
import Amazonka.SWF.Types.TagFilter
import Amazonka.SWF.Types.TaskList
import Amazonka.SWF.Types.TimerCanceledEventAttributes
import Amazonka.SWF.Types.TimerFiredEventAttributes
import Amazonka.SWF.Types.TimerStartedEventAttributes
import Amazonka.SWF.Types.WorkflowExecution
import Amazonka.SWF.Types.WorkflowExecutionCancelRequestedCause
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
import Amazonka.SWF.Types.WorkflowExecutionTerminatedCause
import Amazonka.SWF.Types.WorkflowExecutionTerminatedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionTimedOutEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionTimeoutType
import Amazonka.SWF.Types.WorkflowType
import Amazonka.SWF.Types.WorkflowTypeConfiguration
import Amazonka.SWF.Types.WorkflowTypeFilter
import Amazonka.SWF.Types.WorkflowTypeInfo
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2012-01-25@ of the Amazon Simple Workflow Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SWF",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "swf",
      Core.signingName = "swf",
      Core.version = "2012-01-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SWF",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

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

-- | Returned if the domain already exists. You may get this fault if you are
-- registering a domain that is either already registered or deprecated, or
-- if you undeprecate a domain that is currently registered.
_DomainAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DomainAlreadyExistsFault"

-- | Returned when the specified domain has been deprecated.
_DomainDeprecatedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainDeprecatedFault =
  Core._MatchServiceError
    defaultService
    "DomainDeprecatedFault"

-- | Returned by any operation if a system imposed limitation has been
-- reached. To address this fault you should either clean up unused
-- resources or increase the limit by contacting AWS.
_LimitExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError
    defaultService
    "LimitExceededFault"

-- | Returned when the caller doesn\'t have sufficient permissions to invoke
-- the action.
_OperationNotPermittedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedFault =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedFault"

-- | You\'ve exceeded the number of tags allowed for a domain.
_TooManyTagsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsFault =
  Core._MatchServiceError
    defaultService
    "TooManyTagsFault"

-- | Returned if the type already exists in the specified domain. You may get
-- this fault if you are registering a type that is either already
-- registered or deprecated, or if you undeprecate a type that is currently
-- registered.
_TypeAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TypeAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "TypeAlreadyExistsFault"

-- | Returned when the specified activity or workflow type was already
-- deprecated.
_TypeDeprecatedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TypeDeprecatedFault =
  Core._MatchServiceError
    defaultService
    "TypeDeprecatedFault"

-- | Returned when the named resource cannot be found with in the scope of
-- this operation (region or domain). This could happen if the named
-- resource was never created or is no longer available for this operation.
_UnknownResourceFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnknownResourceFault =
  Core._MatchServiceError
    defaultService
    "UnknownResourceFault"

-- | Returned by StartWorkflowExecution when an open execution with the same
-- workflowId is already running in the specified domain.
_WorkflowExecutionAlreadyStartedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_WorkflowExecutionAlreadyStartedFault =
  Core._MatchServiceError
    defaultService
    "WorkflowExecutionAlreadyStartedFault"

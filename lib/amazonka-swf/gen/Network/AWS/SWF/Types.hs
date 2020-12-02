{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types
  ( -- * Service Configuration
    swf,

    -- * Errors

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
    ActivityTaskCancelRequestedEventAttributes,
    activityTaskCancelRequestedEventAttributes,
    atcreaDecisionTaskCompletedEventId,
    atcreaActivityId,

    -- * ActivityTaskCanceledEventAttributes
    ActivityTaskCanceledEventAttributes,
    activityTaskCanceledEventAttributes,
    aLatestCancelRequestedEventId,
    aDetails,
    aScheduledEventId,
    aStartedEventId,

    -- * ActivityTaskCompletedEventAttributes
    ActivityTaskCompletedEventAttributes,
    activityTaskCompletedEventAttributes,
    atceaResult,
    atceaScheduledEventId,
    atceaStartedEventId,

    -- * ActivityTaskFailedEventAttributes
    ActivityTaskFailedEventAttributes,
    activityTaskFailedEventAttributes,
    atfeaReason,
    atfeaDetails,
    atfeaScheduledEventId,
    atfeaStartedEventId,

    -- * ActivityTaskScheduledEventAttributes
    ActivityTaskScheduledEventAttributes,
    activityTaskScheduledEventAttributes,
    atseaControl,
    atseaHeartbeatTimeout,
    atseaScheduleToCloseTimeout,
    atseaInput,
    atseaTaskPriority,
    atseaScheduleToStartTimeout,
    atseaStartToCloseTimeout,
    atseaActivityType,
    atseaActivityId,
    atseaTaskList,
    atseaDecisionTaskCompletedEventId,

    -- * ActivityTaskStartedEventAttributes
    ActivityTaskStartedEventAttributes,
    activityTaskStartedEventAttributes,
    atseaIdentity,
    atseaScheduledEventId,

    -- * ActivityTaskTimedOutEventAttributes
    ActivityTaskTimedOutEventAttributes,
    activityTaskTimedOutEventAttributes,
    attoeaDetails,
    attoeaTimeoutType,
    attoeaScheduledEventId,
    attoeaStartedEventId,

    -- * ActivityType
    ActivityType,
    activityType,
    atName,
    atVersion,

    -- * ActivityTypeConfiguration
    ActivityTypeConfiguration,
    activityTypeConfiguration,
    atcDefaultTaskScheduleToStartTimeout,
    atcDefaultTaskList,
    atcDefaultTaskPriority,
    atcDefaultTaskHeartbeatTimeout,
    atcDefaultTaskScheduleToCloseTimeout,
    atcDefaultTaskStartToCloseTimeout,

    -- * ActivityTypeInfo
    ActivityTypeInfo,
    activityTypeInfo,
    atiDeprecationDate,
    atiDescription,
    atiActivityType,
    atiStatus,
    atiCreationDate,

    -- * CancelTimerDecisionAttributes
    CancelTimerDecisionAttributes,
    cancelTimerDecisionAttributes,
    ctdaTimerId,

    -- * CancelTimerFailedEventAttributes
    CancelTimerFailedEventAttributes,
    cancelTimerFailedEventAttributes,
    ctfeaTimerId,
    ctfeaCause,
    ctfeaDecisionTaskCompletedEventId,

    -- * CancelWorkflowExecutionDecisionAttributes
    CancelWorkflowExecutionDecisionAttributes,
    cancelWorkflowExecutionDecisionAttributes,
    cwedaDetails,

    -- * CancelWorkflowExecutionFailedEventAttributes
    CancelWorkflowExecutionFailedEventAttributes,
    cancelWorkflowExecutionFailedEventAttributes,
    cCause,
    cDecisionTaskCompletedEventId,

    -- * ChildWorkflowExecutionCanceledEventAttributes
    ChildWorkflowExecutionCanceledEventAttributes,
    childWorkflowExecutionCanceledEventAttributes,
    cDetails,
    cWorkflowExecution,
    cWorkflowType,
    cInitiatedEventId,
    cStartedEventId,

    -- * ChildWorkflowExecutionCompletedEventAttributes
    ChildWorkflowExecutionCompletedEventAttributes,
    childWorkflowExecutionCompletedEventAttributes,
    cweceaResult,
    cweceaWorkflowExecution,
    cweceaWorkflowType,
    cweceaInitiatedEventId,
    cweceaStartedEventId,

    -- * ChildWorkflowExecutionFailedEventAttributes
    ChildWorkflowExecutionFailedEventAttributes,
    childWorkflowExecutionFailedEventAttributes,
    cwefeaReason,
    cwefeaDetails,
    cwefeaWorkflowExecution,
    cwefeaWorkflowType,
    cwefeaInitiatedEventId,
    cwefeaStartedEventId,

    -- * ChildWorkflowExecutionStartedEventAttributes
    ChildWorkflowExecutionStartedEventAttributes,
    childWorkflowExecutionStartedEventAttributes,
    cweseaWorkflowExecution,
    cweseaWorkflowType,
    cweseaInitiatedEventId,

    -- * ChildWorkflowExecutionTerminatedEventAttributes
    ChildWorkflowExecutionTerminatedEventAttributes,
    childWorkflowExecutionTerminatedEventAttributes,
    cweteaWorkflowExecution,
    cweteaWorkflowType,
    cweteaInitiatedEventId,
    cweteaStartedEventId,

    -- * ChildWorkflowExecutionTimedOutEventAttributes
    ChildWorkflowExecutionTimedOutEventAttributes,
    childWorkflowExecutionTimedOutEventAttributes,
    cwetoeaWorkflowExecution,
    cwetoeaWorkflowType,
    cwetoeaTimeoutType,
    cwetoeaInitiatedEventId,
    cwetoeaStartedEventId,

    -- * CloseStatusFilter
    CloseStatusFilter,
    closeStatusFilter,
    csfStatus,

    -- * CompleteWorkflowExecutionDecisionAttributes
    CompleteWorkflowExecutionDecisionAttributes,
    completeWorkflowExecutionDecisionAttributes,
    cwedaResult,

    -- * CompleteWorkflowExecutionFailedEventAttributes
    CompleteWorkflowExecutionFailedEventAttributes,
    completeWorkflowExecutionFailedEventAttributes,
    cwefeaCause,
    cwefeaDecisionTaskCompletedEventId,

    -- * ContinueAsNewWorkflowExecutionDecisionAttributes
    ContinueAsNewWorkflowExecutionDecisionAttributes,
    continueAsNewWorkflowExecutionDecisionAttributes,
    canwedaTagList,
    canwedaTaskStartToCloseTimeout,
    canwedaLambdaRole,
    canwedaInput,
    canwedaWorkflowTypeVersion,
    canwedaExecutionStartToCloseTimeout,
    canwedaTaskList,
    canwedaTaskPriority,
    canwedaChildPolicy,

    -- * ContinueAsNewWorkflowExecutionFailedEventAttributes
    ContinueAsNewWorkflowExecutionFailedEventAttributes,
    continueAsNewWorkflowExecutionFailedEventAttributes,
    canwefeaCause,
    canwefeaDecisionTaskCompletedEventId,

    -- * Decision
    Decision,
    decision,
    dRequestCancelExternalWorkflowExecutionDecisionAttributes,
    dScheduleActivityTaskDecisionAttributes,
    dSignalExternalWorkflowExecutionDecisionAttributes,
    dStartTimerDecisionAttributes,
    dRecordMarkerDecisionAttributes,
    dFailWorkflowExecutionDecisionAttributes,
    dStartChildWorkflowExecutionDecisionAttributes,
    dCompleteWorkflowExecutionDecisionAttributes,
    dScheduleLambdaFunctionDecisionAttributes,
    dRequestCancelActivityTaskDecisionAttributes,
    dCancelWorkflowExecutionDecisionAttributes,
    dCancelTimerDecisionAttributes,
    dContinueAsNewWorkflowExecutionDecisionAttributes,
    dDecisionType,

    -- * DecisionTaskCompletedEventAttributes
    DecisionTaskCompletedEventAttributes,
    decisionTaskCompletedEventAttributes,
    dtceaExecutionContext,
    dtceaScheduledEventId,
    dtceaStartedEventId,

    -- * DecisionTaskScheduledEventAttributes
    DecisionTaskScheduledEventAttributes,
    decisionTaskScheduledEventAttributes,
    dtseaTaskPriority,
    dtseaStartToCloseTimeout,
    dtseaTaskList,

    -- * DecisionTaskStartedEventAttributes
    DecisionTaskStartedEventAttributes,
    decisionTaskStartedEventAttributes,
    dtseaIdentity,
    dtseaScheduledEventId,

    -- * DecisionTaskTimedOutEventAttributes
    DecisionTaskTimedOutEventAttributes,
    decisionTaskTimedOutEventAttributes,
    dttoeaTimeoutType,
    dttoeaScheduledEventId,
    dttoeaStartedEventId,

    -- * DomainConfiguration
    DomainConfiguration,
    domainConfiguration,
    dcWorkflowExecutionRetentionPeriodInDays,

    -- * DomainInfo
    DomainInfo,
    domainInfo,
    diArn,
    diDescription,
    diName,
    diStatus,

    -- * ExecutionTimeFilter
    ExecutionTimeFilter,
    executionTimeFilter,
    etfLatestDate,
    etfOldestDate,

    -- * ExternalWorkflowExecutionCancelRequestedEventAttributes
    ExternalWorkflowExecutionCancelRequestedEventAttributes,
    externalWorkflowExecutionCancelRequestedEventAttributes,
    ewecreaWorkflowExecution,
    ewecreaInitiatedEventId,

    -- * ExternalWorkflowExecutionSignaledEventAttributes
    ExternalWorkflowExecutionSignaledEventAttributes,
    externalWorkflowExecutionSignaledEventAttributes,
    eweseaWorkflowExecution,
    eweseaInitiatedEventId,

    -- * FailWorkflowExecutionDecisionAttributes
    FailWorkflowExecutionDecisionAttributes,
    failWorkflowExecutionDecisionAttributes,
    fwedaReason,
    fwedaDetails,

    -- * FailWorkflowExecutionFailedEventAttributes
    FailWorkflowExecutionFailedEventAttributes,
    failWorkflowExecutionFailedEventAttributes,
    fwefeaCause,
    fwefeaDecisionTaskCompletedEventId,

    -- * HistoryEvent
    HistoryEvent,
    historyEvent,
    heWorkflowExecutionCancelRequestedEventAttributes,
    heRecordMarkerFailedEventAttributes,
    heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    heLambdaFunctionStartedEventAttributes,
    heDecisionTaskScheduledEventAttributes,
    heWorkflowExecutionCompletedEventAttributes,
    heStartTimerFailedEventAttributes,
    heActivityTaskScheduledEventAttributes,
    heScheduleActivityTaskFailedEventAttributes,
    heChildWorkflowExecutionCompletedEventAttributes,
    heMarkerRecordedEventAttributes,
    heScheduleLambdaFunctionFailedEventAttributes,
    heCompleteWorkflowExecutionFailedEventAttributes,
    heLambdaFunctionCompletedEventAttributes,
    heRequestCancelExternalWorkflowExecutionFailedEventAttributes,
    heTimerCanceledEventAttributes,
    heWorkflowExecutionStartedEventAttributes,
    heActivityTaskCompletedEventAttributes,
    heDecisionTaskTimedOutEventAttributes,
    heCancelTimerFailedEventAttributes,
    heChildWorkflowExecutionStartedEventAttributes,
    heActivityTaskCanceledEventAttributes,
    heActivityTaskTimedOutEventAttributes,
    heDecisionTaskStartedEventAttributes,
    heWorkflowExecutionTerminatedEventAttributes,
    heChildWorkflowExecutionCanceledEventAttributes,
    heRequestCancelActivityTaskFailedEventAttributes,
    heLambdaFunctionScheduledEventAttributes,
    heChildWorkflowExecutionTimedOutEventAttributes,
    heCancelWorkflowExecutionFailedEventAttributes,
    heStartChildWorkflowExecutionInitiatedEventAttributes,
    heSignalExternalWorkflowExecutionFailedEventAttributes,
    heActivityTaskStartedEventAttributes,
    heStartLambdaFunctionFailedEventAttributes,
    heChildWorkflowExecutionTerminatedEventAttributes,
    heLambdaFunctionFailedEventAttributes,
    heWorkflowExecutionCanceledEventAttributes,
    heTimerStartedEventAttributes,
    heActivityTaskCancelRequestedEventAttributes,
    heWorkflowExecutionTimedOutEventAttributes,
    heWorkflowExecutionSignaledEventAttributes,
    heTimerFiredEventAttributes,
    heActivityTaskFailedEventAttributes,
    heExternalWorkflowExecutionSignaledEventAttributes,
    heDecisionTaskCompletedEventAttributes,
    heStartChildWorkflowExecutionFailedEventAttributes,
    heChildWorkflowExecutionFailedEventAttributes,
    heFailWorkflowExecutionFailedEventAttributes,
    heContinueAsNewWorkflowExecutionFailedEventAttributes,
    heSignalExternalWorkflowExecutionInitiatedEventAttributes,
    heLambdaFunctionTimedOutEventAttributes,
    heWorkflowExecutionFailedEventAttributes,
    heWorkflowExecutionContinuedAsNewEventAttributes,
    heExternalWorkflowExecutionCancelRequestedEventAttributes,
    heEventTimestamp,
    heEventType,
    heEventId,

    -- * LambdaFunctionCompletedEventAttributes
    LambdaFunctionCompletedEventAttributes,
    lambdaFunctionCompletedEventAttributes,
    lfceaResult,
    lfceaScheduledEventId,
    lfceaStartedEventId,

    -- * LambdaFunctionFailedEventAttributes
    LambdaFunctionFailedEventAttributes,
    lambdaFunctionFailedEventAttributes,
    lffeaReason,
    lffeaDetails,
    lffeaScheduledEventId,
    lffeaStartedEventId,

    -- * LambdaFunctionScheduledEventAttributes
    LambdaFunctionScheduledEventAttributes,
    lambdaFunctionScheduledEventAttributes,
    lfseaControl,
    lfseaInput,
    lfseaStartToCloseTimeout,
    lfseaId,
    lfseaName,
    lfseaDecisionTaskCompletedEventId,

    -- * LambdaFunctionStartedEventAttributes
    LambdaFunctionStartedEventAttributes,
    lambdaFunctionStartedEventAttributes,
    lfseaScheduledEventId,

    -- * LambdaFunctionTimedOutEventAttributes
    LambdaFunctionTimedOutEventAttributes,
    lambdaFunctionTimedOutEventAttributes,
    lftoeaTimeoutType,
    lftoeaScheduledEventId,
    lftoeaStartedEventId,

    -- * MarkerRecordedEventAttributes
    MarkerRecordedEventAttributes,
    markerRecordedEventAttributes,
    mreaDetails,
    mreaMarkerName,
    mreaDecisionTaskCompletedEventId,

    -- * PendingTaskCount
    PendingTaskCount,
    pendingTaskCount,
    ptcTruncated,
    ptcCount,

    -- * RecordMarkerDecisionAttributes
    RecordMarkerDecisionAttributes,
    recordMarkerDecisionAttributes,
    rmdaDetails,
    rmdaMarkerName,

    -- * RecordMarkerFailedEventAttributes
    RecordMarkerFailedEventAttributes,
    recordMarkerFailedEventAttributes,
    rmfeaMarkerName,
    rmfeaCause,
    rmfeaDecisionTaskCompletedEventId,

    -- * RequestCancelActivityTaskDecisionAttributes
    RequestCancelActivityTaskDecisionAttributes,
    requestCancelActivityTaskDecisionAttributes,
    rcatdaActivityId,

    -- * RequestCancelActivityTaskFailedEventAttributes
    RequestCancelActivityTaskFailedEventAttributes,
    requestCancelActivityTaskFailedEventAttributes,
    rcatfeaActivityId,
    rcatfeaCause,
    rcatfeaDecisionTaskCompletedEventId,

    -- * RequestCancelExternalWorkflowExecutionDecisionAttributes
    RequestCancelExternalWorkflowExecutionDecisionAttributes,
    requestCancelExternalWorkflowExecutionDecisionAttributes,
    rcewedaControl,
    rcewedaRunId,
    rcewedaWorkflowId,

    -- * RequestCancelExternalWorkflowExecutionFailedEventAttributes
    RequestCancelExternalWorkflowExecutionFailedEventAttributes,
    requestCancelExternalWorkflowExecutionFailedEventAttributes,
    rcewefeaControl,
    rcewefeaRunId,
    rcewefeaWorkflowId,
    rcewefeaCause,
    rcewefeaInitiatedEventId,
    rcewefeaDecisionTaskCompletedEventId,

    -- * RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    rceweieaControl,
    rceweieaRunId,
    rceweieaWorkflowId,
    rceweieaDecisionTaskCompletedEventId,

    -- * ResourceTag
    ResourceTag,
    resourceTag,
    rtValue,
    rtKey,

    -- * ScheduleActivityTaskDecisionAttributes
    ScheduleActivityTaskDecisionAttributes,
    scheduleActivityTaskDecisionAttributes,
    satdaControl,
    satdaHeartbeatTimeout,
    satdaScheduleToCloseTimeout,
    satdaInput,
    satdaTaskList,
    satdaTaskPriority,
    satdaScheduleToStartTimeout,
    satdaStartToCloseTimeout,
    satdaActivityType,
    satdaActivityId,

    -- * ScheduleActivityTaskFailedEventAttributes
    ScheduleActivityTaskFailedEventAttributes,
    scheduleActivityTaskFailedEventAttributes,
    satfeaActivityType,
    satfeaActivityId,
    satfeaCause,
    satfeaDecisionTaskCompletedEventId,

    -- * ScheduleLambdaFunctionDecisionAttributes
    ScheduleLambdaFunctionDecisionAttributes,
    scheduleLambdaFunctionDecisionAttributes,
    slfdaControl,
    slfdaInput,
    slfdaStartToCloseTimeout,
    slfdaId,
    slfdaName,

    -- * ScheduleLambdaFunctionFailedEventAttributes
    ScheduleLambdaFunctionFailedEventAttributes,
    scheduleLambdaFunctionFailedEventAttributes,
    slffeaId,
    slffeaName,
    slffeaCause,
    slffeaDecisionTaskCompletedEventId,

    -- * SignalExternalWorkflowExecutionDecisionAttributes
    SignalExternalWorkflowExecutionDecisionAttributes,
    signalExternalWorkflowExecutionDecisionAttributes,
    sewedaControl,
    sewedaInput,
    sewedaRunId,
    sewedaWorkflowId,
    sewedaSignalName,

    -- * SignalExternalWorkflowExecutionFailedEventAttributes
    SignalExternalWorkflowExecutionFailedEventAttributes,
    signalExternalWorkflowExecutionFailedEventAttributes,
    sewefeaControl,
    sewefeaRunId,
    sewefeaWorkflowId,
    sewefeaCause,
    sewefeaInitiatedEventId,
    sewefeaDecisionTaskCompletedEventId,

    -- * SignalExternalWorkflowExecutionInitiatedEventAttributes
    SignalExternalWorkflowExecutionInitiatedEventAttributes,
    signalExternalWorkflowExecutionInitiatedEventAttributes,
    seweieaControl,
    seweieaInput,
    seweieaRunId,
    seweieaWorkflowId,
    seweieaSignalName,
    seweieaDecisionTaskCompletedEventId,

    -- * StartChildWorkflowExecutionDecisionAttributes
    StartChildWorkflowExecutionDecisionAttributes,
    startChildWorkflowExecutionDecisionAttributes,
    scwedaControl,
    scwedaTagList,
    scwedaTaskStartToCloseTimeout,
    scwedaLambdaRole,
    scwedaInput,
    scwedaExecutionStartToCloseTimeout,
    scwedaTaskList,
    scwedaTaskPriority,
    scwedaChildPolicy,
    scwedaWorkflowType,
    scwedaWorkflowId,

    -- * StartChildWorkflowExecutionFailedEventAttributes
    StartChildWorkflowExecutionFailedEventAttributes,
    startChildWorkflowExecutionFailedEventAttributes,
    scwefeaControl,
    scwefeaWorkflowType,
    scwefeaCause,
    scwefeaWorkflowId,
    scwefeaInitiatedEventId,
    scwefeaDecisionTaskCompletedEventId,

    -- * StartChildWorkflowExecutionInitiatedEventAttributes
    StartChildWorkflowExecutionInitiatedEventAttributes,
    startChildWorkflowExecutionInitiatedEventAttributes,
    scweieaControl,
    scweieaTagList,
    scweieaTaskStartToCloseTimeout,
    scweieaLambdaRole,
    scweieaInput,
    scweieaExecutionStartToCloseTimeout,
    scweieaTaskPriority,
    scweieaWorkflowId,
    scweieaWorkflowType,
    scweieaTaskList,
    scweieaDecisionTaskCompletedEventId,
    scweieaChildPolicy,

    -- * StartLambdaFunctionFailedEventAttributes
    StartLambdaFunctionFailedEventAttributes,
    startLambdaFunctionFailedEventAttributes,
    sScheduledEventId,
    sCause,
    sMessage,

    -- * StartTimerDecisionAttributes
    StartTimerDecisionAttributes,
    startTimerDecisionAttributes,
    stdaControl,
    stdaTimerId,
    stdaStartToFireTimeout,

    -- * StartTimerFailedEventAttributes
    StartTimerFailedEventAttributes,
    startTimerFailedEventAttributes,
    stfeaTimerId,
    stfeaCause,
    stfeaDecisionTaskCompletedEventId,

    -- * TagFilter
    TagFilter,
    tagFilter,
    tfTag,

    -- * TaskList
    TaskList,
    taskList,
    tlName,

    -- * TimerCanceledEventAttributes
    TimerCanceledEventAttributes,
    timerCanceledEventAttributes,
    tceaTimerId,
    tceaStartedEventId,
    tceaDecisionTaskCompletedEventId,

    -- * TimerFiredEventAttributes
    TimerFiredEventAttributes,
    timerFiredEventAttributes,
    tfeaTimerId,
    tfeaStartedEventId,

    -- * TimerStartedEventAttributes
    TimerStartedEventAttributes,
    timerStartedEventAttributes,
    tseaControl,
    tseaTimerId,
    tseaStartToFireTimeout,
    tseaDecisionTaskCompletedEventId,

    -- * WorkflowExecution
    WorkflowExecution,
    workflowExecution,
    weWorkflowId,
    weRunId,

    -- * WorkflowExecutionCancelRequestedEventAttributes
    WorkflowExecutionCancelRequestedEventAttributes,
    workflowExecutionCancelRequestedEventAttributes,
    wecreaExternalWorkflowExecution,
    wecreaExternalInitiatedEventId,
    wecreaCause,

    -- * WorkflowExecutionCanceledEventAttributes
    WorkflowExecutionCanceledEventAttributes,
    workflowExecutionCanceledEventAttributes,
    wDetails,
    wDecisionTaskCompletedEventId,

    -- * WorkflowExecutionCompletedEventAttributes
    WorkflowExecutionCompletedEventAttributes,
    workflowExecutionCompletedEventAttributes,
    weceaResult,
    weceaDecisionTaskCompletedEventId,

    -- * WorkflowExecutionConfiguration
    WorkflowExecutionConfiguration,
    workflowExecutionConfiguration,
    wecLambdaRole,
    wecTaskPriority,
    wecTaskStartToCloseTimeout,
    wecExecutionStartToCloseTimeout,
    wecTaskList,
    wecChildPolicy,

    -- * WorkflowExecutionContinuedAsNewEventAttributes
    WorkflowExecutionContinuedAsNewEventAttributes,
    workflowExecutionContinuedAsNewEventAttributes,
    wecaneaTagList,
    wecaneaTaskStartToCloseTimeout,
    wecaneaLambdaRole,
    wecaneaInput,
    wecaneaExecutionStartToCloseTimeout,
    wecaneaTaskPriority,
    wecaneaDecisionTaskCompletedEventId,
    wecaneaNewExecutionRunId,
    wecaneaTaskList,
    wecaneaChildPolicy,
    wecaneaWorkflowType,

    -- * WorkflowExecutionCount
    WorkflowExecutionCount,
    workflowExecutionCount,
    wecTruncated,
    wecCount,

    -- * WorkflowExecutionFailedEventAttributes
    WorkflowExecutionFailedEventAttributes,
    workflowExecutionFailedEventAttributes,
    wefeaReason,
    wefeaDetails,
    wefeaDecisionTaskCompletedEventId,

    -- * WorkflowExecutionFilter
    WorkflowExecutionFilter,
    workflowExecutionFilter,
    wefWorkflowId,

    -- * WorkflowExecutionInfo
    WorkflowExecutionInfo,
    workflowExecutionInfo,
    weiParent,
    weiTagList,
    weiCloseStatus,
    weiCloseTimestamp,
    weiCancelRequested,
    weiExecution,
    weiWorkflowType,
    weiStartTimestamp,
    weiExecutionStatus,

    -- * WorkflowExecutionInfos
    WorkflowExecutionInfos,
    workflowExecutionInfos,
    weiNextPageToken,
    weiExecutionInfos,

    -- * WorkflowExecutionOpenCounts
    WorkflowExecutionOpenCounts,
    workflowExecutionOpenCounts,
    weocOpenLambdaFunctions,
    weocOpenActivityTasks,
    weocOpenDecisionTasks,
    weocOpenTimers,
    weocOpenChildWorkflowExecutions,

    -- * WorkflowExecutionSignaledEventAttributes
    WorkflowExecutionSignaledEventAttributes,
    workflowExecutionSignaledEventAttributes,
    wExternalWorkflowExecution,
    wExternalInitiatedEventId,
    wInput,
    wSignalName,

    -- * WorkflowExecutionStartedEventAttributes
    WorkflowExecutionStartedEventAttributes,
    workflowExecutionStartedEventAttributes,
    weseaParentInitiatedEventId,
    weseaTagList,
    weseaTaskStartToCloseTimeout,
    weseaLambdaRole,
    weseaInput,
    weseaExecutionStartToCloseTimeout,
    weseaTaskPriority,
    weseaParentWorkflowExecution,
    weseaContinuedExecutionRunId,
    weseaChildPolicy,
    weseaTaskList,
    weseaWorkflowType,

    -- * WorkflowExecutionTerminatedEventAttributes
    WorkflowExecutionTerminatedEventAttributes,
    workflowExecutionTerminatedEventAttributes,
    weteaCause,
    weteaReason,
    weteaDetails,
    weteaChildPolicy,

    -- * WorkflowExecutionTimedOutEventAttributes
    WorkflowExecutionTimedOutEventAttributes,
    workflowExecutionTimedOutEventAttributes,
    wetoeaTimeoutType,
    wetoeaChildPolicy,

    -- * WorkflowType
    WorkflowType,
    workflowType,
    wtName,
    wtVersion,

    -- * WorkflowTypeConfiguration
    WorkflowTypeConfiguration,
    workflowTypeConfiguration,
    wtcDefaultLambdaRole,
    wtcDefaultChildPolicy,
    wtcDefaultTaskList,
    wtcDefaultTaskPriority,
    wtcDefaultExecutionStartToCloseTimeout,
    wtcDefaultTaskStartToCloseTimeout,

    -- * WorkflowTypeFilter
    WorkflowTypeFilter,
    workflowTypeFilter,
    wtfVersion,
    wtfName,

    -- * WorkflowTypeInfo
    WorkflowTypeInfo,
    workflowTypeInfo,
    wtiDeprecationDate,
    wtiDescription,
    wtiWorkflowType,
    wtiStatus,
    wtiCreationDate,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
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
import Network.AWS.Sign.V4

-- | API version @2012-01-25@ of the Amazon Simple Workflow Service SDK configuration.
swf :: Service
swf =
  Service
    { _svcAbbrev = "SWF",
      _svcSigner = v4,
      _svcPrefix = "swf",
      _svcVersion = "2012-01-25",
      _svcEndpoint = defaultEndpoint swf,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "SWF",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

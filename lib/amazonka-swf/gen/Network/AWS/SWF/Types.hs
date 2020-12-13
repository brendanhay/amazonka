-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types
  ( -- * Service configuration
    swfService,

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
    ActivityTaskCancelRequestedEventAttributes (..),
    mkActivityTaskCancelRequestedEventAttributes,
    atcreaActivityId,
    atcreaDecisionTaskCompletedEventId,

    -- * ActivityTaskCanceledEventAttributes
    ActivityTaskCanceledEventAttributes (..),
    mkActivityTaskCanceledEventAttributes,
    aLatestCancelRequestedEventId,
    aScheduledEventId,
    aDetails,
    aStartedEventId,

    -- * ActivityTaskCompletedEventAttributes
    ActivityTaskCompletedEventAttributes (..),
    mkActivityTaskCompletedEventAttributes,
    atceaScheduledEventId,
    atceaResult,
    atceaStartedEventId,

    -- * ActivityTaskFailedEventAttributes
    ActivityTaskFailedEventAttributes (..),
    mkActivityTaskFailedEventAttributes,
    atfeaScheduledEventId,
    atfeaReason,
    atfeaDetails,
    atfeaStartedEventId,

    -- * ActivityTaskScheduledEventAttributes
    ActivityTaskScheduledEventAttributes (..),
    mkActivityTaskScheduledEventAttributes,
    atseaControl,
    atseaActivityType,
    atseaActivityId,
    atseaHeartbeatTimeout,
    atseaScheduleToCloseTimeout,
    atseaInput,
    atseaTaskList,
    atseaTaskPriority,
    atseaScheduleToStartTimeout,
    atseaStartToCloseTimeout,
    atseaDecisionTaskCompletedEventId,

    -- * ActivityTaskStartedEventAttributes
    ActivityTaskStartedEventAttributes (..),
    mkActivityTaskStartedEventAttributes,
    atseaScheduledEventId,
    atseaIdentity,

    -- * ActivityTaskTimedOutEventAttributes
    ActivityTaskTimedOutEventAttributes (..),
    mkActivityTaskTimedOutEventAttributes,
    attoeaScheduledEventId,
    attoeaTimeoutType,
    attoeaDetails,
    attoeaStartedEventId,

    -- * ActivityType
    ActivityType (..),
    mkActivityType,
    atName,
    atVersion,

    -- * ActivityTypeConfiguration
    ActivityTypeConfiguration (..),
    mkActivityTypeConfiguration,
    atcDefaultTaskScheduleToStartTimeout,
    atcDefaultTaskList,
    atcDefaultTaskPriority,
    atcDefaultTaskHeartbeatTimeout,
    atcDefaultTaskScheduleToCloseTimeout,
    atcDefaultTaskStartToCloseTimeout,

    -- * ActivityTypeInfo
    ActivityTypeInfo (..),
    mkActivityTypeInfo,
    atiStatus,
    atiActivityType,
    atiDeprecationDate,
    atiCreationDate,
    atiDescription,

    -- * CancelTimerDecisionAttributes
    CancelTimerDecisionAttributes (..),
    mkCancelTimerDecisionAttributes,
    ctdaTimerId,

    -- * CancelTimerFailedEventAttributes
    CancelTimerFailedEventAttributes (..),
    mkCancelTimerFailedEventAttributes,
    ctfeaCause,
    ctfeaTimerId,
    ctfeaDecisionTaskCompletedEventId,

    -- * CancelWorkflowExecutionDecisionAttributes
    CancelWorkflowExecutionDecisionAttributes (..),
    mkCancelWorkflowExecutionDecisionAttributes,
    cwedaDetails,

    -- * CancelWorkflowExecutionFailedEventAttributes
    CancelWorkflowExecutionFailedEventAttributes (..),
    mkCancelWorkflowExecutionFailedEventAttributes,
    cCause,
    cDecisionTaskCompletedEventId,

    -- * ChildWorkflowExecutionCanceledEventAttributes
    ChildWorkflowExecutionCanceledEventAttributes (..),
    mkChildWorkflowExecutionCanceledEventAttributes,
    cWorkflowType,
    cDetails,
    cStartedEventId,
    cInitiatedEventId,
    cWorkflowExecution,

    -- * ChildWorkflowExecutionCompletedEventAttributes
    ChildWorkflowExecutionCompletedEventAttributes (..),
    mkChildWorkflowExecutionCompletedEventAttributes,
    cweceaWorkflowType,
    cweceaResult,
    cweceaStartedEventId,
    cweceaInitiatedEventId,
    cweceaWorkflowExecution,

    -- * ChildWorkflowExecutionFailedEventAttributes
    ChildWorkflowExecutionFailedEventAttributes (..),
    mkChildWorkflowExecutionFailedEventAttributes,
    cwefeaWorkflowType,
    cwefeaReason,
    cwefeaDetails,
    cwefeaStartedEventId,
    cwefeaInitiatedEventId,
    cwefeaWorkflowExecution,

    -- * ChildWorkflowExecutionStartedEventAttributes
    ChildWorkflowExecutionStartedEventAttributes (..),
    mkChildWorkflowExecutionStartedEventAttributes,
    cweseaWorkflowType,
    cweseaInitiatedEventId,
    cweseaWorkflowExecution,

    -- * ChildWorkflowExecutionTerminatedEventAttributes
    ChildWorkflowExecutionTerminatedEventAttributes (..),
    mkChildWorkflowExecutionTerminatedEventAttributes,
    cweteaWorkflowType,
    cweteaStartedEventId,
    cweteaInitiatedEventId,
    cweteaWorkflowExecution,

    -- * ChildWorkflowExecutionTimedOutEventAttributes
    ChildWorkflowExecutionTimedOutEventAttributes (..),
    mkChildWorkflowExecutionTimedOutEventAttributes,
    cwetoeaWorkflowType,
    cwetoeaTimeoutType,
    cwetoeaStartedEventId,
    cwetoeaInitiatedEventId,
    cwetoeaWorkflowExecution,

    -- * CloseStatusFilter
    CloseStatusFilter (..),
    mkCloseStatusFilter,
    csfStatus,

    -- * CompleteWorkflowExecutionDecisionAttributes
    CompleteWorkflowExecutionDecisionAttributes (..),
    mkCompleteWorkflowExecutionDecisionAttributes,
    cwedaResult,

    -- * CompleteWorkflowExecutionFailedEventAttributes
    CompleteWorkflowExecutionFailedEventAttributes (..),
    mkCompleteWorkflowExecutionFailedEventAttributes,
    cwefeaCause,
    cwefeaDecisionTaskCompletedEventId,

    -- * ContinueAsNewWorkflowExecutionDecisionAttributes
    ContinueAsNewWorkflowExecutionDecisionAttributes (..),
    mkContinueAsNewWorkflowExecutionDecisionAttributes,
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
    ContinueAsNewWorkflowExecutionFailedEventAttributes (..),
    mkContinueAsNewWorkflowExecutionFailedEventAttributes,
    canwefeaCause,
    canwefeaDecisionTaskCompletedEventId,

    -- * Decision
    Decision (..),
    mkDecision,
    dRequestCancelExternalWorkflowExecutionDecisionAttributes,
    dScheduleActivityTaskDecisionAttributes,
    dSignalExternalWorkflowExecutionDecisionAttributes,
    dStartTimerDecisionAttributes,
    dDecisionType,
    dRecordMarkerDecisionAttributes,
    dFailWorkflowExecutionDecisionAttributes,
    dStartChildWorkflowExecutionDecisionAttributes,
    dCompleteWorkflowExecutionDecisionAttributes,
    dScheduleLambdaFunctionDecisionAttributes,
    dRequestCancelActivityTaskDecisionAttributes,
    dCancelWorkflowExecutionDecisionAttributes,
    dCancelTimerDecisionAttributes,
    dContinueAsNewWorkflowExecutionDecisionAttributes,

    -- * DecisionTaskCompletedEventAttributes
    DecisionTaskCompletedEventAttributes (..),
    mkDecisionTaskCompletedEventAttributes,
    dtceaScheduledEventId,
    dtceaExecutionContext,
    dtceaStartedEventId,

    -- * DecisionTaskScheduledEventAttributes
    DecisionTaskScheduledEventAttributes (..),
    mkDecisionTaskScheduledEventAttributes,
    dtseaTaskList,
    dtseaTaskPriority,
    dtseaStartToCloseTimeout,

    -- * DecisionTaskStartedEventAttributes
    DecisionTaskStartedEventAttributes (..),
    mkDecisionTaskStartedEventAttributes,
    dtseaScheduledEventId,
    dtseaIdentity,

    -- * DecisionTaskTimedOutEventAttributes
    DecisionTaskTimedOutEventAttributes (..),
    mkDecisionTaskTimedOutEventAttributes,
    dttoeaScheduledEventId,
    dttoeaTimeoutType,
    dttoeaStartedEventId,

    -- * DomainConfiguration
    DomainConfiguration (..),
    mkDomainConfiguration,
    dcWorkflowExecutionRetentionPeriodInDays,

    -- * DomainInfo
    DomainInfo (..),
    mkDomainInfo,
    diStatus,
    diArn,
    diName,
    diDescription,

    -- * ExecutionTimeFilter
    ExecutionTimeFilter (..),
    mkExecutionTimeFilter,
    etfLatestDate,
    etfOldestDate,

    -- * ExternalWorkflowExecutionCancelRequestedEventAttributes
    ExternalWorkflowExecutionCancelRequestedEventAttributes (..),
    mkExternalWorkflowExecutionCancelRequestedEventAttributes,
    ewecreaInitiatedEventId,
    ewecreaWorkflowExecution,

    -- * ExternalWorkflowExecutionSignaledEventAttributes
    ExternalWorkflowExecutionSignaledEventAttributes (..),
    mkExternalWorkflowExecutionSignaledEventAttributes,
    eweseaInitiatedEventId,
    eweseaWorkflowExecution,

    -- * FailWorkflowExecutionDecisionAttributes
    FailWorkflowExecutionDecisionAttributes (..),
    mkFailWorkflowExecutionDecisionAttributes,
    fwedaReason,
    fwedaDetails,

    -- * FailWorkflowExecutionFailedEventAttributes
    FailWorkflowExecutionFailedEventAttributes (..),
    mkFailWorkflowExecutionFailedEventAttributes,
    fwefeaCause,
    fwefeaDecisionTaskCompletedEventId,

    -- * HistoryEvent
    HistoryEvent (..),
    mkHistoryEvent,
    heWorkflowExecutionCancelRequestedEventAttributes,
    heRecordMarkerFailedEventAttributes,
    heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    heLambdaFunctionStartedEventAttributes,
    heDecisionTaskScheduledEventAttributes,
    heWorkflowExecutionCompletedEventAttributes,
    heStartTimerFailedEventAttributes,
    heEventTimestamp,
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
    heEventType,
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
    heEventId,
    heWorkflowExecutionFailedEventAttributes,
    heWorkflowExecutionContinuedAsNewEventAttributes,
    heExternalWorkflowExecutionCancelRequestedEventAttributes,

    -- * LambdaFunctionCompletedEventAttributes
    LambdaFunctionCompletedEventAttributes (..),
    mkLambdaFunctionCompletedEventAttributes,
    lfceaScheduledEventId,
    lfceaResult,
    lfceaStartedEventId,

    -- * LambdaFunctionFailedEventAttributes
    LambdaFunctionFailedEventAttributes (..),
    mkLambdaFunctionFailedEventAttributes,
    lffeaScheduledEventId,
    lffeaReason,
    lffeaDetails,
    lffeaStartedEventId,

    -- * LambdaFunctionScheduledEventAttributes
    LambdaFunctionScheduledEventAttributes (..),
    mkLambdaFunctionScheduledEventAttributes,
    lfseaControl,
    lfseaInput,
    lfseaName,
    lfseaId,
    lfseaStartToCloseTimeout,
    lfseaDecisionTaskCompletedEventId,

    -- * LambdaFunctionStartedEventAttributes
    LambdaFunctionStartedEventAttributes (..),
    mkLambdaFunctionStartedEventAttributes,
    lfseaScheduledEventId,

    -- * LambdaFunctionTimedOutEventAttributes
    LambdaFunctionTimedOutEventAttributes (..),
    mkLambdaFunctionTimedOutEventAttributes,
    lftoeaScheduledEventId,
    lftoeaTimeoutType,
    lftoeaStartedEventId,

    -- * MarkerRecordedEventAttributes
    MarkerRecordedEventAttributes (..),
    mkMarkerRecordedEventAttributes,
    mreaMarkerName,
    mreaDetails,
    mreaDecisionTaskCompletedEventId,

    -- * PendingTaskCount
    PendingTaskCount (..),
    mkPendingTaskCount,
    ptcTruncated,
    ptcCount,

    -- * RecordMarkerDecisionAttributes
    RecordMarkerDecisionAttributes (..),
    mkRecordMarkerDecisionAttributes,
    rmdaMarkerName,
    rmdaDetails,

    -- * RecordMarkerFailedEventAttributes
    RecordMarkerFailedEventAttributes (..),
    mkRecordMarkerFailedEventAttributes,
    rmfeaMarkerName,
    rmfeaCause,
    rmfeaDecisionTaskCompletedEventId,

    -- * RequestCancelActivityTaskDecisionAttributes
    RequestCancelActivityTaskDecisionAttributes (..),
    mkRequestCancelActivityTaskDecisionAttributes,
    rcatdaActivityId,

    -- * RequestCancelActivityTaskFailedEventAttributes
    RequestCancelActivityTaskFailedEventAttributes (..),
    mkRequestCancelActivityTaskFailedEventAttributes,
    rcatfeaActivityId,
    rcatfeaCause,
    rcatfeaDecisionTaskCompletedEventId,

    -- * RequestCancelExternalWorkflowExecutionDecisionAttributes
    RequestCancelExternalWorkflowExecutionDecisionAttributes (..),
    mkRequestCancelExternalWorkflowExecutionDecisionAttributes,
    rcewedaControl,
    rcewedaRunId,
    rcewedaWorkflowId,

    -- * RequestCancelExternalWorkflowExecutionFailedEventAttributes
    RequestCancelExternalWorkflowExecutionFailedEventAttributes (..),
    mkRequestCancelExternalWorkflowExecutionFailedEventAttributes,
    rcewefeaControl,
    rcewefeaCause,
    rcewefeaRunId,
    rcewefeaInitiatedEventId,
    rcewefeaWorkflowId,
    rcewefeaDecisionTaskCompletedEventId,

    -- * RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (..),
    mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    rceweieaControl,
    rceweieaRunId,
    rceweieaWorkflowId,
    rceweieaDecisionTaskCompletedEventId,

    -- * ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rtValue,
    rtKey,

    -- * ScheduleActivityTaskDecisionAttributes
    ScheduleActivityTaskDecisionAttributes (..),
    mkScheduleActivityTaskDecisionAttributes,
    satdaControl,
    satdaActivityType,
    satdaActivityId,
    satdaHeartbeatTimeout,
    satdaScheduleToCloseTimeout,
    satdaInput,
    satdaTaskList,
    satdaTaskPriority,
    satdaScheduleToStartTimeout,
    satdaStartToCloseTimeout,

    -- * ScheduleActivityTaskFailedEventAttributes
    ScheduleActivityTaskFailedEventAttributes (..),
    mkScheduleActivityTaskFailedEventAttributes,
    satfeaActivityType,
    satfeaActivityId,
    satfeaCause,
    satfeaDecisionTaskCompletedEventId,

    -- * ScheduleLambdaFunctionDecisionAttributes
    ScheduleLambdaFunctionDecisionAttributes (..),
    mkScheduleLambdaFunctionDecisionAttributes,
    slfdaControl,
    slfdaInput,
    slfdaName,
    slfdaId,
    slfdaStartToCloseTimeout,

    -- * ScheduleLambdaFunctionFailedEventAttributes
    ScheduleLambdaFunctionFailedEventAttributes (..),
    mkScheduleLambdaFunctionFailedEventAttributes,
    slffeaCause,
    slffeaName,
    slffeaId,
    slffeaDecisionTaskCompletedEventId,

    -- * SignalExternalWorkflowExecutionDecisionAttributes
    SignalExternalWorkflowExecutionDecisionAttributes (..),
    mkSignalExternalWorkflowExecutionDecisionAttributes,
    sewedaControl,
    sewedaInput,
    sewedaRunId,
    sewedaWorkflowId,
    sewedaSignalName,

    -- * SignalExternalWorkflowExecutionFailedEventAttributes
    SignalExternalWorkflowExecutionFailedEventAttributes (..),
    mkSignalExternalWorkflowExecutionFailedEventAttributes,
    sewefeaControl,
    sewefeaCause,
    sewefeaRunId,
    sewefeaInitiatedEventId,
    sewefeaWorkflowId,
    sewefeaDecisionTaskCompletedEventId,

    -- * SignalExternalWorkflowExecutionInitiatedEventAttributes
    SignalExternalWorkflowExecutionInitiatedEventAttributes (..),
    mkSignalExternalWorkflowExecutionInitiatedEventAttributes,
    seweieaControl,
    seweieaInput,
    seweieaRunId,
    seweieaWorkflowId,
    seweieaSignalName,
    seweieaDecisionTaskCompletedEventId,

    -- * StartChildWorkflowExecutionDecisionAttributes
    StartChildWorkflowExecutionDecisionAttributes (..),
    mkStartChildWorkflowExecutionDecisionAttributes,
    scwedaControl,
    scwedaTagList,
    scwedaTaskStartToCloseTimeout,
    scwedaLambdaRole,
    scwedaWorkflowType,
    scwedaInput,
    scwedaExecutionStartToCloseTimeout,
    scwedaTaskList,
    scwedaTaskPriority,
    scwedaChildPolicy,
    scwedaWorkflowId,

    -- * StartChildWorkflowExecutionFailedEventAttributes
    StartChildWorkflowExecutionFailedEventAttributes (..),
    mkStartChildWorkflowExecutionFailedEventAttributes,
    scwefeaControl,
    scwefeaWorkflowType,
    scwefeaCause,
    scwefeaInitiatedEventId,
    scwefeaWorkflowId,
    scwefeaDecisionTaskCompletedEventId,

    -- * StartChildWorkflowExecutionInitiatedEventAttributes
    StartChildWorkflowExecutionInitiatedEventAttributes (..),
    mkStartChildWorkflowExecutionInitiatedEventAttributes,
    scweieaControl,
    scweieaTagList,
    scweieaTaskStartToCloseTimeout,
    scweieaLambdaRole,
    scweieaWorkflowType,
    scweieaInput,
    scweieaExecutionStartToCloseTimeout,
    scweieaTaskList,
    scweieaTaskPriority,
    scweieaChildPolicy,
    scweieaWorkflowId,
    scweieaDecisionTaskCompletedEventId,

    -- * StartLambdaFunctionFailedEventAttributes
    StartLambdaFunctionFailedEventAttributes (..),
    mkStartLambdaFunctionFailedEventAttributes,
    sScheduledEventId,
    sCause,
    sMessage,

    -- * StartTimerDecisionAttributes
    StartTimerDecisionAttributes (..),
    mkStartTimerDecisionAttributes,
    stdaControl,
    stdaTimerId,
    stdaStartToFireTimeout,

    -- * StartTimerFailedEventAttributes
    StartTimerFailedEventAttributes (..),
    mkStartTimerFailedEventAttributes,
    stfeaCause,
    stfeaTimerId,
    stfeaDecisionTaskCompletedEventId,

    -- * TagFilter
    TagFilter (..),
    mkTagFilter,
    tfTag,

    -- * TaskList
    TaskList (..),
    mkTaskList,
    tlName,

    -- * TimerCanceledEventAttributes
    TimerCanceledEventAttributes (..),
    mkTimerCanceledEventAttributes,
    tceaTimerId,
    tceaStartedEventId,
    tceaDecisionTaskCompletedEventId,

    -- * TimerFiredEventAttributes
    TimerFiredEventAttributes (..),
    mkTimerFiredEventAttributes,
    tfeaTimerId,
    tfeaStartedEventId,

    -- * TimerStartedEventAttributes
    TimerStartedEventAttributes (..),
    mkTimerStartedEventAttributes,
    tseaControl,
    tseaTimerId,
    tseaStartToFireTimeout,
    tseaDecisionTaskCompletedEventId,

    -- * WorkflowExecution
    WorkflowExecution (..),
    mkWorkflowExecution,
    weRunId,
    weWorkflowId,

    -- * WorkflowExecutionCancelRequestedEventAttributes
    WorkflowExecutionCancelRequestedEventAttributes (..),
    mkWorkflowExecutionCancelRequestedEventAttributes,
    wecreaExternalWorkflowExecution,
    wecreaExternalInitiatedEventId,
    wecreaCause,

    -- * WorkflowExecutionCanceledEventAttributes
    WorkflowExecutionCanceledEventAttributes (..),
    mkWorkflowExecutionCanceledEventAttributes,
    wDetails,
    wDecisionTaskCompletedEventId,

    -- * WorkflowExecutionCompletedEventAttributes
    WorkflowExecutionCompletedEventAttributes (..),
    mkWorkflowExecutionCompletedEventAttributes,
    weceaResult,
    weceaDecisionTaskCompletedEventId,

    -- * WorkflowExecutionConfiguration
    WorkflowExecutionConfiguration (..),
    mkWorkflowExecutionConfiguration,
    wecTaskStartToCloseTimeout,
    wecLambdaRole,
    wecExecutionStartToCloseTimeout,
    wecTaskList,
    wecTaskPriority,
    wecChildPolicy,

    -- * WorkflowExecutionContinuedAsNewEventAttributes
    WorkflowExecutionContinuedAsNewEventAttributes (..),
    mkWorkflowExecutionContinuedAsNewEventAttributes,
    wecaneaTagList,
    wecaneaTaskStartToCloseTimeout,
    wecaneaLambdaRole,
    wecaneaWorkflowType,
    wecaneaInput,
    wecaneaExecutionStartToCloseTimeout,
    wecaneaTaskList,
    wecaneaTaskPriority,
    wecaneaChildPolicy,
    wecaneaNewExecutionRunId,
    wecaneaDecisionTaskCompletedEventId,

    -- * WorkflowExecutionCount
    WorkflowExecutionCount (..),
    mkWorkflowExecutionCount,
    wecTruncated,
    wecCount,

    -- * WorkflowExecutionFailedEventAttributes
    WorkflowExecutionFailedEventAttributes (..),
    mkWorkflowExecutionFailedEventAttributes,
    wefeaReason,
    wefeaDetails,
    wefeaDecisionTaskCompletedEventId,

    -- * WorkflowExecutionFilter
    WorkflowExecutionFilter (..),
    mkWorkflowExecutionFilter,
    wefWorkflowId,

    -- * WorkflowExecutionInfo
    WorkflowExecutionInfo (..),
    mkWorkflowExecutionInfo,
    weiParent,
    weiTagList,
    weiWorkflowType,
    weiExecutionStatus,
    weiExecution,
    weiCloseStatus,
    weiCloseTimestamp,
    weiStartTimestamp,
    weiCancelRequested,

    -- * WorkflowExecutionInfos
    WorkflowExecutionInfos (..),
    mkWorkflowExecutionInfos,
    weiExecutionInfos,
    weiNextPageToken,

    -- * WorkflowExecutionOpenCounts
    WorkflowExecutionOpenCounts (..),
    mkWorkflowExecutionOpenCounts,
    weocOpenChildWorkflowExecutions,
    weocOpenActivityTasks,
    weocOpenLambdaFunctions,
    weocOpenDecisionTasks,
    weocOpenTimers,

    -- * WorkflowExecutionSignaledEventAttributes
    WorkflowExecutionSignaledEventAttributes (..),
    mkWorkflowExecutionSignaledEventAttributes,
    wExternalWorkflowExecution,
    wExternalInitiatedEventId,
    wInput,
    wSignalName,

    -- * WorkflowExecutionStartedEventAttributes
    WorkflowExecutionStartedEventAttributes (..),
    mkWorkflowExecutionStartedEventAttributes,
    weseaParentInitiatedEventId,
    weseaTagList,
    weseaTaskStartToCloseTimeout,
    weseaLambdaRole,
    weseaWorkflowType,
    weseaInput,
    weseaExecutionStartToCloseTimeout,
    weseaTaskList,
    weseaTaskPriority,
    weseaChildPolicy,
    weseaParentWorkflowExecution,
    weseaContinuedExecutionRunId,

    -- * WorkflowExecutionTerminatedEventAttributes
    WorkflowExecutionTerminatedEventAttributes (..),
    mkWorkflowExecutionTerminatedEventAttributes,
    weteaCause,
    weteaReason,
    weteaChildPolicy,
    weteaDetails,

    -- * WorkflowExecutionTimedOutEventAttributes
    WorkflowExecutionTimedOutEventAttributes (..),
    mkWorkflowExecutionTimedOutEventAttributes,
    wetoeaTimeoutType,
    wetoeaChildPolicy,

    -- * WorkflowType
    WorkflowType (..),
    mkWorkflowType,
    wtName,
    wtVersion,

    -- * WorkflowTypeConfiguration
    WorkflowTypeConfiguration (..),
    mkWorkflowTypeConfiguration,
    wtcDefaultLambdaRole,
    wtcDefaultChildPolicy,
    wtcDefaultTaskList,
    wtcDefaultTaskPriority,
    wtcDefaultExecutionStartToCloseTimeout,
    wtcDefaultTaskStartToCloseTimeout,

    -- * WorkflowTypeFilter
    WorkflowTypeFilter (..),
    mkWorkflowTypeFilter,
    wtfName,
    wtfVersion,

    -- * WorkflowTypeInfo
    WorkflowTypeInfo (..),
    mkWorkflowTypeInfo,
    wtiStatus,
    wtiWorkflowType,
    wtiDeprecationDate,
    wtiCreationDate,
    wtiDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
swfService :: Lude.Service
swfService =
  Lude.Service
    { Lude._svcAbbrev = "SWF",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "swf",
      Lude._svcVersion = "2012-01-25",
      Lude._svcEndpoint = Lude.defaultEndpoint swfService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "SWF",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

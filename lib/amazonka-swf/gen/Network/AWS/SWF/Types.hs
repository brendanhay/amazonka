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
    mkServiceConfig,

    -- * Errors
    _DomainAlreadyExistsFault,
    _LimitExceededFault,
    _WorkflowExecutionAlreadyStartedFault,
    _OperationNotPermittedFault,
    _UnknownResourceFault,
    _DefaultUndefinedFault,
    _TypeDeprecatedFault,
    _TooManyTagsFault,
    _TypeAlreadyExistsFault,
    _DomainDeprecatedFault,

    -- * FailureReason
    FailureReason (..),

    -- * WorkflowExecutionCancelRequestedEventAttributes
    WorkflowExecutionCancelRequestedEventAttributes (..),
    mkWorkflowExecutionCancelRequestedEventAttributes,
    wecreaCause,
    wecreaExternalInitiatedEventId,
    wecreaExternalWorkflowExecution,

    -- * RequestCancelExternalWorkflowExecutionDecisionAttributes
    RequestCancelExternalWorkflowExecutionDecisionAttributes (..),
    mkRequestCancelExternalWorkflowExecutionDecisionAttributes,
    rcewedaWorkflowId,
    rcewedaControl,
    rcewedaRunId,

    -- * LimitedData
    LimitedData (..),

    -- * DecisionTaskScheduledEventAttributes
    DecisionTaskScheduledEventAttributes (..),
    mkDecisionTaskScheduledEventAttributes,
    dtseaTaskList,
    dtseaStartToCloseTimeout,
    dtseaTaskPriority,

    -- * ResourceTagKey
    ResourceTagKey (..),

    -- * WorkflowExecutionCompletedEventAttributes
    WorkflowExecutionCompletedEventAttributes (..),
    mkWorkflowExecutionCompletedEventAttributes,
    wDecisionTaskCompletedEventId,
    wResult,

    -- * LambdaFunctionStartedEventAttributes
    LambdaFunctionStartedEventAttributes (..),
    mkLambdaFunctionStartedEventAttributes,
    lfseaScheduledEventId,

    -- * ExecutionTimeFilter
    ExecutionTimeFilter (..),
    mkExecutionTimeFilter,
    etfOldestDate,
    etfLatestDate,

    -- * StartLambdaFunctionFailedCause
    StartLambdaFunctionFailedCause (..),

    -- * StartTimerFailedEventAttributes
    StartTimerFailedEventAttributes (..),
    mkStartTimerFailedEventAttributes,
    stfeaTimerId,
    stfeaCause,
    stfeaDecisionTaskCompletedEventId,

    -- * RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (..),
    mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    rceweieaWorkflowId,
    rceweieaDecisionTaskCompletedEventId,
    rceweieaControl,
    rceweieaRunId,

    -- * RecordMarkerFailedEventAttributes
    RecordMarkerFailedEventAttributes (..),
    mkRecordMarkerFailedEventAttributes,
    rmfeaMarkerName,
    rmfeaCause,
    rmfeaDecisionTaskCompletedEventId,

    -- * WorkflowExecutionCount
    WorkflowExecutionCount (..),
    mkWorkflowExecutionCount,
    wecCount,
    wecTruncated,

    -- * ActivityTaskScheduledEventAttributes
    ActivityTaskScheduledEventAttributes (..),
    mkActivityTaskScheduledEventAttributes,
    atseaActivityType,
    atseaActivityId,
    atseaTaskList,
    atseaDecisionTaskCompletedEventId,
    atseaControl,
    atseaHeartbeatTimeout,
    atseaInput,
    atseaScheduleToCloseTimeout,
    atseaScheduleToStartTimeout,
    atseaStartToCloseTimeout,
    atseaTaskPriority,

    -- * CloseStatusFilter
    CloseStatusFilter (..),
    mkCloseStatusFilter,
    csfStatus,

    -- * Tag
    Tag (..),

    -- * WorkflowExecutionTimeoutType
    WorkflowExecutionTimeoutType (..),

    -- * ScheduleActivityTaskDecisionAttributes
    ScheduleActivityTaskDecisionAttributes (..),
    mkScheduleActivityTaskDecisionAttributes,
    satdaActivityType,
    satdaActivityId,
    satdaControl,
    satdaHeartbeatTimeout,
    satdaInput,
    satdaScheduleToCloseTimeout,
    satdaScheduleToStartTimeout,
    satdaStartToCloseTimeout,
    satdaTaskList,
    satdaTaskPriority,

    -- * CauseMessage
    CauseMessage (..),

    -- * MarkerName
    MarkerName (..),

    -- * ActivityTypeConfiguration
    ActivityTypeConfiguration (..),
    mkActivityTypeConfiguration,
    atcDefaultTaskHeartbeatTimeout,
    atcDefaultTaskList,
    atcDefaultTaskPriority,
    atcDefaultTaskScheduleToCloseTimeout,
    atcDefaultTaskScheduleToStartTimeout,
    atcDefaultTaskStartToCloseTimeout,

    -- * Arn
    Arn (..),

    -- * ActivityType
    ActivityType (..),
    mkActivityType,
    atName,
    atVersion,

    -- * WorkflowTypeInfo
    WorkflowTypeInfo (..),
    mkWorkflowTypeInfo,
    wtiWorkflowType,
    wtiStatus,
    wtiCreationDate,
    wtiDeprecationDate,
    wtiDescription,

    -- * ChildWorkflowExecutionCompletedEventAttributes
    ChildWorkflowExecutionCompletedEventAttributes (..),
    mkChildWorkflowExecutionCompletedEventAttributes,
    cWorkflowExecution,
    cWorkflowType,
    cInitiatedEventId,
    cStartedEventId,
    cResult,

    -- * WorkflowExecutionOpenCounts
    WorkflowExecutionOpenCounts (..),
    mkWorkflowExecutionOpenCounts,
    weocOpenActivityTasks,
    weocOpenDecisionTasks,
    weocOpenTimers,
    weocOpenChildWorkflowExecutions,
    weocOpenLambdaFunctions,

    -- * RequestCancelActivityTaskFailedCause
    RequestCancelActivityTaskFailedCause (..),

    -- * ScheduleActivityTaskFailedEventAttributes
    ScheduleActivityTaskFailedEventAttributes (..),
    mkScheduleActivityTaskFailedEventAttributes,
    satfeaActivityType,
    satfeaActivityId,
    satfeaCause,
    satfeaDecisionTaskCompletedEventId,

    -- * MarkerRecordedEventAttributes
    MarkerRecordedEventAttributes (..),
    mkMarkerRecordedEventAttributes,
    mreaMarkerName,
    mreaDecisionTaskCompletedEventId,
    mreaDetails,

    -- * TerminateReason
    TerminateReason (..),

    -- * SignalExternalWorkflowExecutionDecisionAttributes
    SignalExternalWorkflowExecutionDecisionAttributes (..),
    mkSignalExternalWorkflowExecutionDecisionAttributes,
    sewedaWorkflowId,
    sewedaSignalName,
    sewedaControl,
    sewedaInput,
    sewedaRunId,

    -- * WorkflowExecutionTerminatedCause
    WorkflowExecutionTerminatedCause (..),

    -- * CancelWorkflowExecutionFailedCause
    CancelWorkflowExecutionFailedCause (..),

    -- * Data
    Data (..),

    -- * ScheduleLambdaFunctionFailedEventAttributes
    ScheduleLambdaFunctionFailedEventAttributes (..),
    mkScheduleLambdaFunctionFailedEventAttributes,
    slffeaId,
    slffeaName,
    slffeaCause,
    slffeaDecisionTaskCompletedEventId,

    -- * SignalExternalWorkflowExecutionFailedCause
    SignalExternalWorkflowExecutionFailedCause (..),

    -- * RecordMarkerDecisionAttributes
    RecordMarkerDecisionAttributes (..),
    mkRecordMarkerDecisionAttributes,
    rmdaMarkerName,
    rmdaDetails,

    -- * CompleteWorkflowExecutionFailedEventAttributes
    CompleteWorkflowExecutionFailedEventAttributes (..),
    mkCompleteWorkflowExecutionFailedEventAttributes,
    cCause,
    cDecisionTaskCompletedEventId,

    -- * StartTimerDecisionAttributes
    StartTimerDecisionAttributes (..),
    mkStartTimerDecisionAttributes,
    stdaTimerId,
    stdaStartToFireTimeout,
    stdaControl,

    -- * DecisionType
    DecisionType (..),

    -- * ActivityId
    ActivityId (..),

    -- * RequestCancelExternalWorkflowExecutionFailedEventAttributes
    RequestCancelExternalWorkflowExecutionFailedEventAttributes (..),
    mkRequestCancelExternalWorkflowExecutionFailedEventAttributes,
    rcewefeaWorkflowId,
    rcewefeaCause,
    rcewefeaInitiatedEventId,
    rcewefeaDecisionTaskCompletedEventId,
    rcewefeaControl,
    rcewefeaRunId,

    -- * ActivityTypeInfo
    ActivityTypeInfo (..),
    mkActivityTypeInfo,
    atiActivityType,
    atiStatus,
    atiCreationDate,
    atiDeprecationDate,
    atiDescription,

    -- * TimerCanceledEventAttributes
    TimerCanceledEventAttributes (..),
    mkTimerCanceledEventAttributes,
    tceaTimerId,
    tceaStartedEventId,
    tceaDecisionTaskCompletedEventId,

    -- * WorkflowExecutionStartedEventAttributes
    WorkflowExecutionStartedEventAttributes (..),
    mkWorkflowExecutionStartedEventAttributes,
    wChildPolicy,
    wTaskList,
    wWorkflowType,
    wContinuedExecutionRunId,
    wExecutionStartToCloseTimeout,
    wInput,
    wLambdaRole,
    wParentInitiatedEventId,
    wParentWorkflowExecution,
    wTagList,
    wTaskPriority,
    wTaskStartToCloseTimeout,

    -- * WorkflowTypeConfiguration
    WorkflowTypeConfiguration (..),
    mkWorkflowTypeConfiguration,
    wtcDefaultChildPolicy,
    wtcDefaultExecutionStartToCloseTimeout,
    wtcDefaultLambdaRole,
    wtcDefaultTaskList,
    wtcDefaultTaskPriority,
    wtcDefaultTaskStartToCloseTimeout,

    -- * ActivityTaskTimeoutType
    ActivityTaskTimeoutType (..),

    -- * LambdaFunctionCompletedEventAttributes
    LambdaFunctionCompletedEventAttributes (..),
    mkLambdaFunctionCompletedEventAttributes,
    lfceaScheduledEventId,
    lfceaStartedEventId,
    lfceaResult,

    -- * WorkflowType
    WorkflowType (..),
    mkWorkflowType,
    wtName,
    wtVersion,

    -- * ActivityTaskCompletedEventAttributes
    ActivityTaskCompletedEventAttributes (..),
    mkActivityTaskCompletedEventAttributes,
    aScheduledEventId,
    aStartedEventId,
    aResult,

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * DecisionTaskTimeoutType
    DecisionTaskTimeoutType (..),

    -- * DurationInDays
    DurationInDays (..),

    -- * WorkflowExecutionCancelRequestedCause
    WorkflowExecutionCancelRequestedCause (..),

    -- * StartChildWorkflowExecutionFailedCause
    StartChildWorkflowExecutionFailedCause (..),

    -- * DecisionTaskTimedOutEventAttributes
    DecisionTaskTimedOutEventAttributes (..),
    mkDecisionTaskTimedOutEventAttributes,
    dttoeaTimeoutType,
    dttoeaScheduledEventId,
    dttoeaStartedEventId,

    -- * ChildWorkflowExecutionStartedEventAttributes
    ChildWorkflowExecutionStartedEventAttributes (..),
    mkChildWorkflowExecutionStartedEventAttributes,
    cweseaWorkflowExecution,
    cweseaWorkflowType,
    cweseaInitiatedEventId,

    -- * CancelTimerFailedEventAttributes
    CancelTimerFailedEventAttributes (..),
    mkCancelTimerFailedEventAttributes,
    ctfeaTimerId,
    ctfeaCause,
    ctfeaDecisionTaskCompletedEventId,

    -- * FailWorkflowExecutionFailedCause
    FailWorkflowExecutionFailedCause (..),

    -- * WorkflowExecutionFilter
    WorkflowExecutionFilter (..),
    mkWorkflowExecutionFilter,
    wefWorkflowId,

    -- * FunctionInput
    FunctionInput (..),

    -- * ActivityTaskCanceledEventAttributes
    ActivityTaskCanceledEventAttributes (..),
    mkActivityTaskCanceledEventAttributes,
    atceaScheduledEventId,
    atceaStartedEventId,
    atceaDetails,
    atceaLatestCancelRequestedEventId,

    -- * WorkflowExecutionInfos
    WorkflowExecutionInfos (..),
    mkWorkflowExecutionInfos,
    weiExecutionInfos,
    weiNextPageToken,

    -- * StartChildWorkflowExecutionDecisionAttributes
    StartChildWorkflowExecutionDecisionAttributes (..),
    mkStartChildWorkflowExecutionDecisionAttributes,
    scwedaWorkflowType,
    scwedaWorkflowId,
    scwedaChildPolicy,
    scwedaControl,
    scwedaExecutionStartToCloseTimeout,
    scwedaInput,
    scwedaLambdaRole,
    scwedaTagList,
    scwedaTaskList,
    scwedaTaskPriority,
    scwedaTaskStartToCloseTimeout,

    -- * ContinueAsNewWorkflowExecutionFailedCause
    ContinueAsNewWorkflowExecutionFailedCause (..),

    -- * FailWorkflowExecutionDecisionAttributes
    FailWorkflowExecutionDecisionAttributes (..),
    mkFailWorkflowExecutionDecisionAttributes,
    fwedaDetails,
    fwedaReason,

    -- * VersionOptional
    VersionOptional (..),

    -- * DurationInSecondsOptional
    DurationInSecondsOptional (..),

    -- * EventType
    EventType (..),

    -- * ActivityTaskTimedOutEventAttributes
    ActivityTaskTimedOutEventAttributes (..),
    mkActivityTaskTimedOutEventAttributes,
    attoeaTimeoutType,
    attoeaScheduledEventId,
    attoeaStartedEventId,
    attoeaDetails,

    -- * FunctionId
    FunctionId (..),

    -- * RequestCancelActivityTaskFailedEventAttributes
    RequestCancelActivityTaskFailedEventAttributes (..),
    mkRequestCancelActivityTaskFailedEventAttributes,
    rcatfeaActivityId,
    rcatfeaCause,
    rcatfeaDecisionTaskCompletedEventId,

    -- * CompleteWorkflowExecutionDecisionAttributes
    CompleteWorkflowExecutionDecisionAttributes (..),
    mkCompleteWorkflowExecutionDecisionAttributes,
    cwedaResult,

    -- * DecisionTaskStartedEventAttributes
    DecisionTaskStartedEventAttributes (..),
    mkDecisionTaskStartedEventAttributes,
    dtseaScheduledEventId,
    dtseaIdentity,

    -- * ChildWorkflowExecutionTimedOutEventAttributes
    ChildWorkflowExecutionTimedOutEventAttributes (..),
    mkChildWorkflowExecutionTimedOutEventAttributes,
    cwetoeaWorkflowExecution,
    cwetoeaWorkflowType,
    cwetoeaTimeoutType,
    cwetoeaInitiatedEventId,
    cwetoeaStartedEventId,

    -- * StartChildWorkflowExecutionInitiatedEventAttributes
    StartChildWorkflowExecutionInitiatedEventAttributes (..),
    mkStartChildWorkflowExecutionInitiatedEventAttributes,
    scweieaWorkflowId,
    scweieaWorkflowType,
    scweieaTaskList,
    scweieaDecisionTaskCompletedEventId,
    scweieaChildPolicy,
    scweieaControl,
    scweieaExecutionStartToCloseTimeout,
    scweieaInput,
    scweieaLambdaRole,
    scweieaTagList,
    scweieaTaskPriority,
    scweieaTaskStartToCloseTimeout,

    -- * CancelWorkflowExecutionFailedEventAttributes
    CancelWorkflowExecutionFailedEventAttributes (..),
    mkCancelWorkflowExecutionFailedEventAttributes,
    cwefeaCause,
    cwefeaDecisionTaskCompletedEventId,

    -- * WorkflowExecutionTerminatedEventAttributes
    WorkflowExecutionTerminatedEventAttributes (..),
    mkWorkflowExecutionTerminatedEventAttributes,
    weteaChildPolicy,
    weteaCause,
    weteaDetails,
    weteaReason,

    -- * DomainName
    DomainName (..),

    -- * TaskList
    TaskList (..),
    mkTaskList,
    tlName,

    -- * TaskPriority
    TaskPriority (..),

    -- * WorkflowRunId
    WorkflowRunId (..),

    -- * ScheduleLambdaFunctionDecisionAttributes
    ScheduleLambdaFunctionDecisionAttributes (..),
    mkScheduleLambdaFunctionDecisionAttributes,
    slfdaId,
    slfdaName,
    slfdaControl,
    slfdaInput,
    slfdaStartToCloseTimeout,

    -- * LambdaFunctionScheduledEventAttributes
    LambdaFunctionScheduledEventAttributes (..),
    mkLambdaFunctionScheduledEventAttributes,
    lfseaId,
    lfseaName,
    lfseaDecisionTaskCompletedEventId,
    lfseaControl,
    lfseaInput,
    lfseaStartToCloseTimeout,

    -- * Name
    Name (..),

    -- * ScheduleActivityTaskFailedCause
    ScheduleActivityTaskFailedCause (..),

    -- * ChildWorkflowExecutionCanceledEventAttributes
    ChildWorkflowExecutionCanceledEventAttributes (..),
    mkChildWorkflowExecutionCanceledEventAttributes,
    cweceaWorkflowExecution,
    cweceaWorkflowType,
    cweceaInitiatedEventId,
    cweceaStartedEventId,
    cweceaDetails,

    -- * WorkflowExecutionInfo
    WorkflowExecutionInfo (..),
    mkWorkflowExecutionInfo,
    weiExecution,
    weiWorkflowType,
    weiStartTimestamp,
    weiExecutionStatus,
    weiCancelRequested,
    weiCloseStatus,
    weiCloseTimestamp,
    weiParent,
    weiTagList,

    -- * SignalExternalWorkflowExecutionFailedEventAttributes
    SignalExternalWorkflowExecutionFailedEventAttributes (..),
    mkSignalExternalWorkflowExecutionFailedEventAttributes,
    sewefeaWorkflowId,
    sewefeaCause,
    sewefeaInitiatedEventId,
    sewefeaDecisionTaskCompletedEventId,
    sewefeaControl,
    sewefeaRunId,

    -- * TagFilter
    TagFilter (..),
    mkTagFilter,
    tfTag,

    -- * Version
    Version (..),

    -- * ScheduleLambdaFunctionFailedCause
    ScheduleLambdaFunctionFailedCause (..),

    -- * ChildPolicy
    ChildPolicy (..),

    -- * ActivityTaskStartedEventAttributes
    ActivityTaskStartedEventAttributes (..),
    mkActivityTaskStartedEventAttributes,
    atseaScheduledEventId,
    atseaIdentity,

    -- * TimerId
    TimerId (..),

    -- * DurationInSeconds
    DurationInSeconds (..),

    -- * CloseStatus
    CloseStatus (..),

    -- * CompleteWorkflowExecutionFailedCause
    CompleteWorkflowExecutionFailedCause (..),

    -- * FunctionName
    FunctionName (..),

    -- * StartTimerFailedCause
    StartTimerFailedCause (..),

    -- * ActivityTaskCancelRequestedEventAttributes
    ActivityTaskCancelRequestedEventAttributes (..),
    mkActivityTaskCancelRequestedEventAttributes,
    atcreaDecisionTaskCompletedEventId,
    atcreaActivityId,

    -- * WorkflowExecutionTimedOutEventAttributes
    WorkflowExecutionTimedOutEventAttributes (..),
    mkWorkflowExecutionTimedOutEventAttributes,
    wetoeaTimeoutType,
    wetoeaChildPolicy,

    -- * ChildWorkflowExecutionTerminatedEventAttributes
    ChildWorkflowExecutionTerminatedEventAttributes (..),
    mkChildWorkflowExecutionTerminatedEventAttributes,
    cweteaWorkflowExecution,
    cweteaWorkflowType,
    cweteaInitiatedEventId,
    cweteaStartedEventId,

    -- * WorkflowExecutionCanceledEventAttributes
    WorkflowExecutionCanceledEventAttributes (..),
    mkWorkflowExecutionCanceledEventAttributes,
    weceaDecisionTaskCompletedEventId,
    weceaDetails,

    -- * StartLambdaFunctionFailedEventAttributes
    StartLambdaFunctionFailedEventAttributes (..),
    mkStartLambdaFunctionFailedEventAttributes,
    sCause,
    sMessage,
    sScheduledEventId,

    -- * WorkflowExecutionSignaledEventAttributes
    WorkflowExecutionSignaledEventAttributes (..),
    mkWorkflowExecutionSignaledEventAttributes,
    weseaSignalName,
    weseaExternalInitiatedEventId,
    weseaExternalWorkflowExecution,
    weseaInput,

    -- * RecordMarkerFailedCause
    RecordMarkerFailedCause (..),

    -- * RegistrationStatus
    RegistrationStatus (..),

    -- * TimerStartedEventAttributes
    TimerStartedEventAttributes (..),
    mkTimerStartedEventAttributes,
    tseaTimerId,
    tseaStartToFireTimeout,
    tseaDecisionTaskCompletedEventId,
    tseaControl,

    -- * LambdaFunctionFailedEventAttributes
    LambdaFunctionFailedEventAttributes (..),
    mkLambdaFunctionFailedEventAttributes,
    lffeaScheduledEventId,
    lffeaStartedEventId,
    lffeaDetails,
    lffeaReason,

    -- * RequestCancelActivityTaskDecisionAttributes
    RequestCancelActivityTaskDecisionAttributes (..),
    mkRequestCancelActivityTaskDecisionAttributes,
    rcatdaActivityId,

    -- * Decision
    Decision (..),
    mkDecision,
    dDecisionType,
    dCancelTimerDecisionAttributes,
    dCancelWorkflowExecutionDecisionAttributes,
    dCompleteWorkflowExecutionDecisionAttributes,
    dContinueAsNewWorkflowExecutionDecisionAttributes,
    dFailWorkflowExecutionDecisionAttributes,
    dRecordMarkerDecisionAttributes,
    dRequestCancelActivityTaskDecisionAttributes,
    dRequestCancelExternalWorkflowExecutionDecisionAttributes,
    dScheduleActivityTaskDecisionAttributes,
    dScheduleLambdaFunctionDecisionAttributes,
    dSignalExternalWorkflowExecutionDecisionAttributes,
    dStartChildWorkflowExecutionDecisionAttributes,
    dStartTimerDecisionAttributes,

    -- * ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rtKey,
    rtValue,

    -- * TimerFiredEventAttributes
    TimerFiredEventAttributes (..),
    mkTimerFiredEventAttributes,
    tfeaTimerId,
    tfeaStartedEventId,

    -- * PageToken
    PageToken (..),

    -- * DomainConfiguration
    DomainConfiguration (..),
    mkDomainConfiguration,
    dcWorkflowExecutionRetentionPeriodInDays,

    -- * ExternalWorkflowExecutionSignaledEventAttributes
    ExternalWorkflowExecutionSignaledEventAttributes (..),
    mkExternalWorkflowExecutionSignaledEventAttributes,
    eweseaWorkflowExecution,
    eweseaInitiatedEventId,

    -- * CancelWorkflowExecutionDecisionAttributes
    CancelWorkflowExecutionDecisionAttributes (..),
    mkCancelWorkflowExecutionDecisionAttributes,
    cwedaDetails,

    -- * ActivityTaskFailedEventAttributes
    ActivityTaskFailedEventAttributes (..),
    mkActivityTaskFailedEventAttributes,
    atfeaScheduledEventId,
    atfeaStartedEventId,
    atfeaDetails,
    atfeaReason,

    -- * FailWorkflowExecutionFailedEventAttributes
    FailWorkflowExecutionFailedEventAttributes (..),
    mkFailWorkflowExecutionFailedEventAttributes,
    fwefeaCause,
    fwefeaDecisionTaskCompletedEventId,

    -- * StartChildWorkflowExecutionFailedEventAttributes
    StartChildWorkflowExecutionFailedEventAttributes (..),
    mkStartChildWorkflowExecutionFailedEventAttributes,
    scwefeaWorkflowType,
    scwefeaCause,
    scwefeaWorkflowId,
    scwefeaInitiatedEventId,
    scwefeaDecisionTaskCompletedEventId,
    scwefeaControl,

    -- * WorkflowTypeFilter
    WorkflowTypeFilter (..),
    mkWorkflowTypeFilter,
    wtfName,
    wtfVersion,

    -- * CancelTimerFailedCause
    CancelTimerFailedCause (..),

    -- * DecisionTaskCompletedEventAttributes
    DecisionTaskCompletedEventAttributes (..),
    mkDecisionTaskCompletedEventAttributes,
    dtceaScheduledEventId,
    dtceaStartedEventId,
    dtceaExecutionContext,

    -- * ChildWorkflowExecutionFailedEventAttributes
    ChildWorkflowExecutionFailedEventAttributes (..),
    mkChildWorkflowExecutionFailedEventAttributes,
    cwefeaWorkflowExecution,
    cwefeaWorkflowType,
    cwefeaInitiatedEventId,
    cwefeaStartedEventId,
    cwefeaDetails,
    cwefeaReason,

    -- * DomainInfo
    DomainInfo (..),
    mkDomainInfo,
    diName,
    diStatus,
    diArn,
    diDescription,

    -- * HistoryEvent
    HistoryEvent (..),
    mkHistoryEvent,
    heEventTimestamp,
    heEventType,
    heEventId,
    heActivityTaskCancelRequestedEventAttributes,
    heActivityTaskCanceledEventAttributes,
    heActivityTaskCompletedEventAttributes,
    heActivityTaskFailedEventAttributes,
    heActivityTaskScheduledEventAttributes,
    heActivityTaskStartedEventAttributes,
    heActivityTaskTimedOutEventAttributes,
    heCancelTimerFailedEventAttributes,
    heCancelWorkflowExecutionFailedEventAttributes,
    heChildWorkflowExecutionCanceledEventAttributes,
    heChildWorkflowExecutionCompletedEventAttributes,
    heChildWorkflowExecutionFailedEventAttributes,
    heChildWorkflowExecutionStartedEventAttributes,
    heChildWorkflowExecutionTerminatedEventAttributes,
    heChildWorkflowExecutionTimedOutEventAttributes,
    heCompleteWorkflowExecutionFailedEventAttributes,
    heContinueAsNewWorkflowExecutionFailedEventAttributes,
    heDecisionTaskCompletedEventAttributes,
    heDecisionTaskScheduledEventAttributes,
    heDecisionTaskStartedEventAttributes,
    heDecisionTaskTimedOutEventAttributes,
    heExternalWorkflowExecutionCancelRequestedEventAttributes,
    heExternalWorkflowExecutionSignaledEventAttributes,
    heFailWorkflowExecutionFailedEventAttributes,
    heLambdaFunctionCompletedEventAttributes,
    heLambdaFunctionFailedEventAttributes,
    heLambdaFunctionScheduledEventAttributes,
    heLambdaFunctionStartedEventAttributes,
    heLambdaFunctionTimedOutEventAttributes,
    heMarkerRecordedEventAttributes,
    heRecordMarkerFailedEventAttributes,
    heRequestCancelActivityTaskFailedEventAttributes,
    heRequestCancelExternalWorkflowExecutionFailedEventAttributes,
    heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    heScheduleActivityTaskFailedEventAttributes,
    heScheduleLambdaFunctionFailedEventAttributes,
    heSignalExternalWorkflowExecutionFailedEventAttributes,
    heSignalExternalWorkflowExecutionInitiatedEventAttributes,
    heStartChildWorkflowExecutionFailedEventAttributes,
    heStartChildWorkflowExecutionInitiatedEventAttributes,
    heStartLambdaFunctionFailedEventAttributes,
    heStartTimerFailedEventAttributes,
    heTimerCanceledEventAttributes,
    heTimerFiredEventAttributes,
    heTimerStartedEventAttributes,
    heWorkflowExecutionCancelRequestedEventAttributes,
    heWorkflowExecutionCanceledEventAttributes,
    heWorkflowExecutionCompletedEventAttributes,
    heWorkflowExecutionContinuedAsNewEventAttributes,
    heWorkflowExecutionFailedEventAttributes,
    heWorkflowExecutionSignaledEventAttributes,
    heWorkflowExecutionStartedEventAttributes,
    heWorkflowExecutionTerminatedEventAttributes,
    heWorkflowExecutionTimedOutEventAttributes,

    -- * WorkflowId
    WorkflowId (..),

    -- * LambdaFunctionTimeoutType
    LambdaFunctionTimeoutType (..),

    -- * Description
    Description (..),

    -- * Identity
    Identity (..),

    -- * ContinueAsNewWorkflowExecutionFailedEventAttributes
    ContinueAsNewWorkflowExecutionFailedEventAttributes (..),
    mkContinueAsNewWorkflowExecutionFailedEventAttributes,
    canwefeaCause,
    canwefeaDecisionTaskCompletedEventId,

    -- * SignalExternalWorkflowExecutionInitiatedEventAttributes
    SignalExternalWorkflowExecutionInitiatedEventAttributes (..),
    mkSignalExternalWorkflowExecutionInitiatedEventAttributes,
    seweieaWorkflowId,
    seweieaSignalName,
    seweieaDecisionTaskCompletedEventId,
    seweieaControl,
    seweieaInput,
    seweieaRunId,

    -- * TaskToken
    TaskToken (..),

    -- * CancelTimerDecisionAttributes
    CancelTimerDecisionAttributes (..),
    mkCancelTimerDecisionAttributes,
    ctdaTimerId,

    -- * SignalName
    SignalName (..),

    -- * WorkflowExecutionFailedEventAttributes
    WorkflowExecutionFailedEventAttributes (..),
    mkWorkflowExecutionFailedEventAttributes,
    wefeaDecisionTaskCompletedEventId,
    wefeaDetails,
    wefeaReason,

    -- * WorkflowExecutionConfiguration
    WorkflowExecutionConfiguration (..),
    mkWorkflowExecutionConfiguration,
    wecTaskStartToCloseTimeout,
    wecExecutionStartToCloseTimeout,
    wecTaskList,
    wecChildPolicy,
    wecLambdaRole,
    wecTaskPriority,

    -- * WorkflowExecution
    WorkflowExecution (..),
    mkWorkflowExecution,
    weWorkflowId,
    weRunId,

    -- * RequestCancelExternalWorkflowExecutionFailedCause
    RequestCancelExternalWorkflowExecutionFailedCause (..),

    -- * ContinueAsNewWorkflowExecutionDecisionAttributes
    ContinueAsNewWorkflowExecutionDecisionAttributes (..),
    mkContinueAsNewWorkflowExecutionDecisionAttributes,
    canwedaChildPolicy,
    canwedaExecutionStartToCloseTimeout,
    canwedaInput,
    canwedaLambdaRole,
    canwedaTagList,
    canwedaTaskList,
    canwedaTaskPriority,
    canwedaTaskStartToCloseTimeout,
    canwedaWorkflowTypeVersion,

    -- * ExternalWorkflowExecutionCancelRequestedEventAttributes
    ExternalWorkflowExecutionCancelRequestedEventAttributes (..),
    mkExternalWorkflowExecutionCancelRequestedEventAttributes,
    ewecreaWorkflowExecution,
    ewecreaInitiatedEventId,

    -- * PendingTaskCount
    PendingTaskCount (..),
    mkPendingTaskCount,
    ptcCount,
    ptcTruncated,

    -- * LambdaFunctionTimedOutEventAttributes
    LambdaFunctionTimedOutEventAttributes (..),
    mkLambdaFunctionTimedOutEventAttributes,
    lftoeaScheduledEventId,
    lftoeaStartedEventId,
    lftoeaTimeoutType,

    -- * WorkflowExecutionContinuedAsNewEventAttributes
    WorkflowExecutionContinuedAsNewEventAttributes (..),
    mkWorkflowExecutionContinuedAsNewEventAttributes,
    wecaneaDecisionTaskCompletedEventId,
    wecaneaNewExecutionRunId,
    wecaneaTaskList,
    wecaneaChildPolicy,
    wecaneaWorkflowType,
    wecaneaExecutionStartToCloseTimeout,
    wecaneaInput,
    wecaneaLambdaRole,
    wecaneaTagList,
    wecaneaTaskPriority,
    wecaneaTaskStartToCloseTimeout,

    -- * Domain
    Domain (..),

    -- * NextPageToken
    NextPageToken (..),

    -- * Control
    Control (..),

    -- * RunId
    RunId (..),

    -- * StartToCloseTimeout
    StartToCloseTimeout (..),

    -- * WorkflowExecutionRetentionPeriodInDays
    WorkflowExecutionRetentionPeriodInDays (..),

    -- * Result
    Result (..),

    -- * ExecutionContext
    ExecutionContext (..),

    -- * HeartbeatTimeout
    HeartbeatTimeout (..),

    -- * Input
    Input (..),

    -- * ScheduleToCloseTimeout
    ScheduleToCloseTimeout (..),

    -- * ScheduleToStartTimeout
    ScheduleToStartTimeout (..),

    -- * DefaultTaskHeartbeatTimeout
    DefaultTaskHeartbeatTimeout (..),

    -- * DefaultTaskPriority
    DefaultTaskPriority (..),

    -- * DefaultTaskScheduleToCloseTimeout
    DefaultTaskScheduleToCloseTimeout (..),

    -- * DefaultTaskScheduleToStartTimeout
    DefaultTaskScheduleToStartTimeout (..),

    -- * DefaultTaskStartToCloseTimeout
    DefaultTaskStartToCloseTimeout (..),

    -- * Details
    Details (..),

    -- * Id
    Id (..),

    -- * StartToFireTimeout
    StartToFireTimeout (..),

    -- * ContinuedExecutionRunId
    ContinuedExecutionRunId (..),

    -- * ExecutionStartToCloseTimeout
    ExecutionStartToCloseTimeout (..),

    -- * TaskStartToCloseTimeout
    TaskStartToCloseTimeout (..),

    -- * DefaultExecutionStartToCloseTimeout
    DefaultExecutionStartToCloseTimeout (..),

    -- * Value
    Value (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.SWF.Types.ActivityId
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
import Network.AWS.SWF.Types.Arn
import Network.AWS.SWF.Types.CancelTimerDecisionAttributes
import Network.AWS.SWF.Types.CancelTimerFailedCause
import Network.AWS.SWF.Types.CancelTimerFailedEventAttributes
import Network.AWS.SWF.Types.CancelWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.CancelWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.CancelWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.CauseMessage
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
import Network.AWS.SWF.Types.ContinuedExecutionRunId
import Network.AWS.SWF.Types.Control
import Network.AWS.SWF.Types.Data
import Network.AWS.SWF.Types.Decision
import Network.AWS.SWF.Types.DecisionTaskCompletedEventAttributes
import Network.AWS.SWF.Types.DecisionTaskScheduledEventAttributes
import Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes
import Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes
import Network.AWS.SWF.Types.DecisionTaskTimeoutType
import Network.AWS.SWF.Types.DecisionType
import Network.AWS.SWF.Types.DefaultExecutionStartToCloseTimeout
import Network.AWS.SWF.Types.DefaultTaskHeartbeatTimeout
import Network.AWS.SWF.Types.DefaultTaskPriority
import Network.AWS.SWF.Types.DefaultTaskScheduleToCloseTimeout
import Network.AWS.SWF.Types.DefaultTaskScheduleToStartTimeout
import Network.AWS.SWF.Types.DefaultTaskStartToCloseTimeout
import Network.AWS.SWF.Types.Description
import Network.AWS.SWF.Types.Details
import Network.AWS.SWF.Types.Domain
import Network.AWS.SWF.Types.DomainConfiguration
import Network.AWS.SWF.Types.DomainInfo
import Network.AWS.SWF.Types.DomainName
import Network.AWS.SWF.Types.DurationInDays
import Network.AWS.SWF.Types.DurationInSeconds
import Network.AWS.SWF.Types.DurationInSecondsOptional
import Network.AWS.SWF.Types.EventType
import Network.AWS.SWF.Types.ExecutionContext
import Network.AWS.SWF.Types.ExecutionStartToCloseTimeout
import Network.AWS.SWF.Types.ExecutionStatus
import Network.AWS.SWF.Types.ExecutionTimeFilter
import Network.AWS.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
import Network.AWS.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes
import Network.AWS.SWF.Types.FailWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.FailWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.FailWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.FailureReason
import Network.AWS.SWF.Types.FunctionId
import Network.AWS.SWF.Types.FunctionInput
import Network.AWS.SWF.Types.FunctionName
import Network.AWS.SWF.Types.HeartbeatTimeout
import Network.AWS.SWF.Types.HistoryEvent
import Network.AWS.SWF.Types.Id
import Network.AWS.SWF.Types.Identity
import Network.AWS.SWF.Types.Input
import Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionTimedOutEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionTimeoutType
import Network.AWS.SWF.Types.LimitedData
import Network.AWS.SWF.Types.MarkerName
import Network.AWS.SWF.Types.MarkerRecordedEventAttributes
import Network.AWS.SWF.Types.Name
import Network.AWS.SWF.Types.NextPageToken
import Network.AWS.SWF.Types.PageToken
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
import Network.AWS.SWF.Types.ResourceTagKey
import Network.AWS.SWF.Types.Result
import Network.AWS.SWF.Types.RunId
import Network.AWS.SWF.Types.ScheduleActivityTaskDecisionAttributes
import Network.AWS.SWF.Types.ScheduleActivityTaskFailedCause
import Network.AWS.SWF.Types.ScheduleActivityTaskFailedEventAttributes
import Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
import Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedCause
import Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
import Network.AWS.SWF.Types.ScheduleToCloseTimeout
import Network.AWS.SWF.Types.ScheduleToStartTimeout
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
import Network.AWS.SWF.Types.SignalName
import Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedCause
import Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
import Network.AWS.SWF.Types.StartLambdaFunctionFailedCause
import Network.AWS.SWF.Types.StartLambdaFunctionFailedEventAttributes
import Network.AWS.SWF.Types.StartTimerDecisionAttributes
import Network.AWS.SWF.Types.StartTimerFailedCause
import Network.AWS.SWF.Types.StartTimerFailedEventAttributes
import Network.AWS.SWF.Types.StartToCloseTimeout
import Network.AWS.SWF.Types.StartToFireTimeout
import Network.AWS.SWF.Types.Tag
import Network.AWS.SWF.Types.TagFilter
import Network.AWS.SWF.Types.TaskList
import Network.AWS.SWF.Types.TaskPriority
import Network.AWS.SWF.Types.TaskStartToCloseTimeout
import Network.AWS.SWF.Types.TaskToken
import Network.AWS.SWF.Types.TerminateReason
import Network.AWS.SWF.Types.TimerCanceledEventAttributes
import Network.AWS.SWF.Types.TimerFiredEventAttributes
import Network.AWS.SWF.Types.TimerId
import Network.AWS.SWF.Types.TimerStartedEventAttributes
import Network.AWS.SWF.Types.Value
import Network.AWS.SWF.Types.Version
import Network.AWS.SWF.Types.VersionOptional
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
import Network.AWS.SWF.Types.WorkflowExecutionRetentionPeriodInDays
import Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause
import Network.AWS.SWF.Types.WorkflowExecutionTerminatedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionTimedOutEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionTimeoutType
import Network.AWS.SWF.Types.WorkflowId
import Network.AWS.SWF.Types.WorkflowRunId
import Network.AWS.SWF.Types.WorkflowType
import Network.AWS.SWF.Types.WorkflowTypeConfiguration
import Network.AWS.SWF.Types.WorkflowTypeFilter
import Network.AWS.SWF.Types.WorkflowTypeInfo
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-01-25@ of the Amazon Simple Workflow Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "SWF",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "swf",
      Core._svcVersion = "2012-01-25",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "SWF",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | Returned if the domain already exists. You may get this fault if you are registering a domain that is either already registered or deprecated, or if you undeprecate a domain that is currently registered.
_DomainAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DomainAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "DomainAlreadyExistsFault"
{-# DEPRECATED _DomainAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | Returned by any operation if a system imposed limitation has been reached. To address this fault you should either clean up unused resources or increase the limit by contacting AWS.
_LimitExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError mkServiceConfig "LimitExceededFault"
{-# DEPRECATED _LimitExceededFault "Use generic-lens or generic-optics instead." #-}

-- | Returned by 'StartWorkflowExecution' when an open execution with the same workflowId is already running in the specified domain.
_WorkflowExecutionAlreadyStartedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WorkflowExecutionAlreadyStartedFault =
  Core._MatchServiceError
    mkServiceConfig
    "WorkflowExecutionAlreadyStartedFault"
{-# DEPRECATED _WorkflowExecutionAlreadyStartedFault "Use generic-lens or generic-optics instead." #-}

-- | Returned when the caller doesn't have sufficient permissions to invoke the action.
_OperationNotPermittedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedFault =
  Core._MatchServiceError
    mkServiceConfig
    "OperationNotPermittedFault"
{-# DEPRECATED _OperationNotPermittedFault "Use generic-lens or generic-optics instead." #-}

-- | Returned when the named resource cannot be found with in the scope of this operation (region or domain). This could happen if the named resource was never created or is no longer available for this operation.
_UnknownResourceFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnknownResourceFault =
  Core._MatchServiceError mkServiceConfig "UnknownResourceFault"
{-# DEPRECATED _UnknownResourceFault "Use generic-lens or generic-optics instead." #-}

-- | The @StartWorkflowExecution@ API action was called without the required parameters set.
--
-- Some workflow execution parameters, such as the decision @taskList@ , must be set to start the execution. However, these parameters might have been set as defaults when the workflow type was registered. In this case, you can omit these parameters from the @StartWorkflowExecution@ call and Amazon SWF uses the values defined in the workflow type.
_DefaultUndefinedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DefaultUndefinedFault =
  Core._MatchServiceError mkServiceConfig "DefaultUndefinedFault"
{-# DEPRECATED _DefaultUndefinedFault "Use generic-lens or generic-optics instead." #-}

-- | Returned when the specified activity or workflow type was already deprecated.
_TypeDeprecatedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TypeDeprecatedFault =
  Core._MatchServiceError mkServiceConfig "TypeDeprecatedFault"
{-# DEPRECATED _TypeDeprecatedFault "Use generic-lens or generic-optics instead." #-}

-- | You've exceeded the number of tags allowed for a domain.
_TooManyTagsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsFault =
  Core._MatchServiceError mkServiceConfig "TooManyTagsFault"
{-# DEPRECATED _TooManyTagsFault "Use generic-lens or generic-optics instead." #-}

-- | Returned if the type already exists in the specified domain. You may get this fault if you are registering a type that is either already registered or deprecated, or if you undeprecate a type that is currently registered.
_TypeAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TypeAlreadyExistsFault =
  Core._MatchServiceError mkServiceConfig "TypeAlreadyExistsFault"
{-# DEPRECATED _TypeAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | Returned when the specified domain has been deprecated.
_DomainDeprecatedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DomainDeprecatedFault =
  Core._MatchServiceError mkServiceConfig "DomainDeprecatedFault"
{-# DEPRECATED _DomainDeprecatedFault "Use generic-lens or generic-optics instead." #-}

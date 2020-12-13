{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Simple Workflow Service__
--
-- The Amazon Simple Workflow Service (Amazon SWF) makes it easy to build applications that use Amazon's cloud to coordinate work across distributed components. In Amazon SWF, a /task/ represents a logical unit of work that is performed by a component of your workflow. Coordinating tasks in a workflow involves managing intertask dependencies, scheduling, and concurrency in accordance with the logical flow of the application.
-- Amazon SWF gives you full control over implementing tasks and coordinating them without worrying about underlying complexities such as tracking their progress and maintaining their state.
-- This documentation serves as reference only. For a broader overview of the Amazon SWF programming model, see the /<https:\/\/docs.aws.amazon.com\/amazonswf\/latest\/developerguide\/ Amazon SWF Developer Guide> / .
module Network.AWS.SWF
  ( -- * Service configuration
    swfService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListOpenWorkflowExecutions (Paginated)
    module Network.AWS.SWF.ListOpenWorkflowExecutions,

    -- ** RegisterActivityType
    module Network.AWS.SWF.RegisterActivityType,

    -- ** ListActivityTypes (Paginated)
    module Network.AWS.SWF.ListActivityTypes,

    -- ** CountPendingActivityTasks
    module Network.AWS.SWF.CountPendingActivityTasks,

    -- ** RegisterWorkflowType
    module Network.AWS.SWF.RegisterWorkflowType,

    -- ** ListWorkflowTypes (Paginated)
    module Network.AWS.SWF.ListWorkflowTypes,

    -- ** ListTagsForResource
    module Network.AWS.SWF.ListTagsForResource,

    -- ** RespondActivityTaskFailed
    module Network.AWS.SWF.RespondActivityTaskFailed,

    -- ** CountOpenWorkflowExecutions
    module Network.AWS.SWF.CountOpenWorkflowExecutions,

    -- ** UndeprecateDomain
    module Network.AWS.SWF.UndeprecateDomain,

    -- ** DescribeWorkflowType
    module Network.AWS.SWF.DescribeWorkflowType,

    -- ** DeprecateWorkflowType
    module Network.AWS.SWF.DeprecateWorkflowType,

    -- ** RequestCancelWorkflowExecution
    module Network.AWS.SWF.RequestCancelWorkflowExecution,

    -- ** RegisterDomain
    module Network.AWS.SWF.RegisterDomain,

    -- ** RespondDecisionTaskCompleted
    module Network.AWS.SWF.RespondDecisionTaskCompleted,

    -- ** PollForActivityTask
    module Network.AWS.SWF.PollForActivityTask,

    -- ** RespondActivityTaskCompleted
    module Network.AWS.SWF.RespondActivityTaskCompleted,

    -- ** DescribeWorkflowExecution
    module Network.AWS.SWF.DescribeWorkflowExecution,

    -- ** SignalWorkflowExecution
    module Network.AWS.SWF.SignalWorkflowExecution,

    -- ** CountPendingDecisionTasks
    module Network.AWS.SWF.CountPendingDecisionTasks,

    -- ** ListClosedWorkflowExecutions (Paginated)
    module Network.AWS.SWF.ListClosedWorkflowExecutions,

    -- ** RecordActivityTaskHeartbeat
    module Network.AWS.SWF.RecordActivityTaskHeartbeat,

    -- ** DescribeDomain
    module Network.AWS.SWF.DescribeDomain,

    -- ** GetWorkflowExecutionHistory (Paginated)
    module Network.AWS.SWF.GetWorkflowExecutionHistory,

    -- ** DeprecateDomain
    module Network.AWS.SWF.DeprecateDomain,

    -- ** UndeprecateWorkflowType
    module Network.AWS.SWF.UndeprecateWorkflowType,

    -- ** TerminateWorkflowExecution
    module Network.AWS.SWF.TerminateWorkflowExecution,

    -- ** DescribeActivityType
    module Network.AWS.SWF.DescribeActivityType,

    -- ** TagResource
    module Network.AWS.SWF.TagResource,

    -- ** DeprecateActivityType
    module Network.AWS.SWF.DeprecateActivityType,

    -- ** UndeprecateActivityType
    module Network.AWS.SWF.UndeprecateActivityType,

    -- ** CountClosedWorkflowExecutions
    module Network.AWS.SWF.CountClosedWorkflowExecutions,

    -- ** UntagResource
    module Network.AWS.SWF.UntagResource,

    -- ** RespondActivityTaskCanceled
    module Network.AWS.SWF.RespondActivityTaskCanceled,

    -- ** StartWorkflowExecution
    module Network.AWS.SWF.StartWorkflowExecution,

    -- ** PollForDecisionTask (Paginated)
    module Network.AWS.SWF.PollForDecisionTask,

    -- ** ListDomains (Paginated)
    module Network.AWS.SWF.ListDomains,

    -- * Types

    -- ** ActivityTaskTimeoutType
    ActivityTaskTimeoutType (..),

    -- ** CancelTimerFailedCause
    CancelTimerFailedCause (..),

    -- ** CancelWorkflowExecutionFailedCause
    CancelWorkflowExecutionFailedCause (..),

    -- ** ChildPolicy
    ChildPolicy (..),

    -- ** CloseStatus
    CloseStatus (..),

    -- ** CompleteWorkflowExecutionFailedCause
    CompleteWorkflowExecutionFailedCause (..),

    -- ** ContinueAsNewWorkflowExecutionFailedCause
    ContinueAsNewWorkflowExecutionFailedCause (..),

    -- ** DecisionTaskTimeoutType
    DecisionTaskTimeoutType (..),

    -- ** DecisionType
    DecisionType (..),

    -- ** EventType
    EventType (..),

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** FailWorkflowExecutionFailedCause
    FailWorkflowExecutionFailedCause (..),

    -- ** LambdaFunctionTimeoutType
    LambdaFunctionTimeoutType (..),

    -- ** RecordMarkerFailedCause
    RecordMarkerFailedCause (..),

    -- ** RegistrationStatus
    RegistrationStatus (..),

    -- ** RequestCancelActivityTaskFailedCause
    RequestCancelActivityTaskFailedCause (..),

    -- ** RequestCancelExternalWorkflowExecutionFailedCause
    RequestCancelExternalWorkflowExecutionFailedCause (..),

    -- ** ScheduleActivityTaskFailedCause
    ScheduleActivityTaskFailedCause (..),

    -- ** ScheduleLambdaFunctionFailedCause
    ScheduleLambdaFunctionFailedCause (..),

    -- ** SignalExternalWorkflowExecutionFailedCause
    SignalExternalWorkflowExecutionFailedCause (..),

    -- ** StartChildWorkflowExecutionFailedCause
    StartChildWorkflowExecutionFailedCause (..),

    -- ** StartLambdaFunctionFailedCause
    StartLambdaFunctionFailedCause (..),

    -- ** StartTimerFailedCause
    StartTimerFailedCause (..),

    -- ** WorkflowExecutionCancelRequestedCause
    WorkflowExecutionCancelRequestedCause (..),

    -- ** WorkflowExecutionTerminatedCause
    WorkflowExecutionTerminatedCause (..),

    -- ** WorkflowExecutionTimeoutType
    WorkflowExecutionTimeoutType (..),

    -- ** ActivityTaskCancelRequestedEventAttributes
    ActivityTaskCancelRequestedEventAttributes (..),
    mkActivityTaskCancelRequestedEventAttributes,
    atcreaActivityId,
    atcreaDecisionTaskCompletedEventId,

    -- ** ActivityTaskCanceledEventAttributes
    ActivityTaskCanceledEventAttributes (..),
    mkActivityTaskCanceledEventAttributes,
    aLatestCancelRequestedEventId,
    aScheduledEventId,
    aDetails,
    aStartedEventId,

    -- ** ActivityTaskCompletedEventAttributes
    ActivityTaskCompletedEventAttributes (..),
    mkActivityTaskCompletedEventAttributes,
    atceaScheduledEventId,
    atceaResult,
    atceaStartedEventId,

    -- ** ActivityTaskFailedEventAttributes
    ActivityTaskFailedEventAttributes (..),
    mkActivityTaskFailedEventAttributes,
    atfeaScheduledEventId,
    atfeaReason,
    atfeaDetails,
    atfeaStartedEventId,

    -- ** ActivityTaskScheduledEventAttributes
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

    -- ** ActivityTaskStartedEventAttributes
    ActivityTaskStartedEventAttributes (..),
    mkActivityTaskStartedEventAttributes,
    atseaScheduledEventId,
    atseaIdentity,

    -- ** ActivityTaskTimedOutEventAttributes
    ActivityTaskTimedOutEventAttributes (..),
    mkActivityTaskTimedOutEventAttributes,
    attoeaScheduledEventId,
    attoeaTimeoutType,
    attoeaDetails,
    attoeaStartedEventId,

    -- ** ActivityType
    ActivityType (..),
    mkActivityType,
    atName,
    atVersion,

    -- ** ActivityTypeConfiguration
    ActivityTypeConfiguration (..),
    mkActivityTypeConfiguration,
    atcDefaultTaskScheduleToStartTimeout,
    atcDefaultTaskList,
    atcDefaultTaskPriority,
    atcDefaultTaskHeartbeatTimeout,
    atcDefaultTaskScheduleToCloseTimeout,
    atcDefaultTaskStartToCloseTimeout,

    -- ** ActivityTypeInfo
    ActivityTypeInfo (..),
    mkActivityTypeInfo,
    atiStatus,
    atiActivityType,
    atiDeprecationDate,
    atiCreationDate,
    atiDescription,

    -- ** CancelTimerDecisionAttributes
    CancelTimerDecisionAttributes (..),
    mkCancelTimerDecisionAttributes,
    ctdaTimerId,

    -- ** CancelTimerFailedEventAttributes
    CancelTimerFailedEventAttributes (..),
    mkCancelTimerFailedEventAttributes,
    ctfeaCause,
    ctfeaTimerId,
    ctfeaDecisionTaskCompletedEventId,

    -- ** CancelWorkflowExecutionDecisionAttributes
    CancelWorkflowExecutionDecisionAttributes (..),
    mkCancelWorkflowExecutionDecisionAttributes,
    cwedaDetails,

    -- ** CancelWorkflowExecutionFailedEventAttributes
    CancelWorkflowExecutionFailedEventAttributes (..),
    mkCancelWorkflowExecutionFailedEventAttributes,
    cCause,
    cDecisionTaskCompletedEventId,

    -- ** ChildWorkflowExecutionCanceledEventAttributes
    ChildWorkflowExecutionCanceledEventAttributes (..),
    mkChildWorkflowExecutionCanceledEventAttributes,
    cWorkflowType,
    cDetails,
    cStartedEventId,
    cInitiatedEventId,
    cWorkflowExecution,

    -- ** ChildWorkflowExecutionCompletedEventAttributes
    ChildWorkflowExecutionCompletedEventAttributes (..),
    mkChildWorkflowExecutionCompletedEventAttributes,
    cweceaWorkflowType,
    cweceaResult,
    cweceaStartedEventId,
    cweceaInitiatedEventId,
    cweceaWorkflowExecution,

    -- ** ChildWorkflowExecutionFailedEventAttributes
    ChildWorkflowExecutionFailedEventAttributes (..),
    mkChildWorkflowExecutionFailedEventAttributes,
    cwefeaWorkflowType,
    cwefeaReason,
    cwefeaDetails,
    cwefeaStartedEventId,
    cwefeaInitiatedEventId,
    cwefeaWorkflowExecution,

    -- ** ChildWorkflowExecutionStartedEventAttributes
    ChildWorkflowExecutionStartedEventAttributes (..),
    mkChildWorkflowExecutionStartedEventAttributes,
    cweseaWorkflowType,
    cweseaInitiatedEventId,
    cweseaWorkflowExecution,

    -- ** ChildWorkflowExecutionTerminatedEventAttributes
    ChildWorkflowExecutionTerminatedEventAttributes (..),
    mkChildWorkflowExecutionTerminatedEventAttributes,
    cweteaWorkflowType,
    cweteaStartedEventId,
    cweteaInitiatedEventId,
    cweteaWorkflowExecution,

    -- ** ChildWorkflowExecutionTimedOutEventAttributes
    ChildWorkflowExecutionTimedOutEventAttributes (..),
    mkChildWorkflowExecutionTimedOutEventAttributes,
    cwetoeaWorkflowType,
    cwetoeaTimeoutType,
    cwetoeaStartedEventId,
    cwetoeaInitiatedEventId,
    cwetoeaWorkflowExecution,

    -- ** CloseStatusFilter
    CloseStatusFilter (..),
    mkCloseStatusFilter,
    csfStatus,

    -- ** CompleteWorkflowExecutionDecisionAttributes
    CompleteWorkflowExecutionDecisionAttributes (..),
    mkCompleteWorkflowExecutionDecisionAttributes,
    cwedaResult,

    -- ** CompleteWorkflowExecutionFailedEventAttributes
    CompleteWorkflowExecutionFailedEventAttributes (..),
    mkCompleteWorkflowExecutionFailedEventAttributes,
    cwefeaCause,
    cwefeaDecisionTaskCompletedEventId,

    -- ** ContinueAsNewWorkflowExecutionDecisionAttributes
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

    -- ** ContinueAsNewWorkflowExecutionFailedEventAttributes
    ContinueAsNewWorkflowExecutionFailedEventAttributes (..),
    mkContinueAsNewWorkflowExecutionFailedEventAttributes,
    canwefeaCause,
    canwefeaDecisionTaskCompletedEventId,

    -- ** Decision
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

    -- ** DecisionTaskCompletedEventAttributes
    DecisionTaskCompletedEventAttributes (..),
    mkDecisionTaskCompletedEventAttributes,
    dtceaScheduledEventId,
    dtceaExecutionContext,
    dtceaStartedEventId,

    -- ** DecisionTaskScheduledEventAttributes
    DecisionTaskScheduledEventAttributes (..),
    mkDecisionTaskScheduledEventAttributes,
    dtseaTaskList,
    dtseaTaskPriority,
    dtseaStartToCloseTimeout,

    -- ** DecisionTaskStartedEventAttributes
    DecisionTaskStartedEventAttributes (..),
    mkDecisionTaskStartedEventAttributes,
    dtseaScheduledEventId,
    dtseaIdentity,

    -- ** DecisionTaskTimedOutEventAttributes
    DecisionTaskTimedOutEventAttributes (..),
    mkDecisionTaskTimedOutEventAttributes,
    dttoeaScheduledEventId,
    dttoeaTimeoutType,
    dttoeaStartedEventId,

    -- ** DomainConfiguration
    DomainConfiguration (..),
    mkDomainConfiguration,
    dcWorkflowExecutionRetentionPeriodInDays,

    -- ** DomainInfo
    DomainInfo (..),
    mkDomainInfo,
    diStatus,
    diArn,
    diName,
    diDescription,

    -- ** ExecutionTimeFilter
    ExecutionTimeFilter (..),
    mkExecutionTimeFilter,
    etfLatestDate,
    etfOldestDate,

    -- ** ExternalWorkflowExecutionCancelRequestedEventAttributes
    ExternalWorkflowExecutionCancelRequestedEventAttributes (..),
    mkExternalWorkflowExecutionCancelRequestedEventAttributes,
    ewecreaInitiatedEventId,
    ewecreaWorkflowExecution,

    -- ** ExternalWorkflowExecutionSignaledEventAttributes
    ExternalWorkflowExecutionSignaledEventAttributes (..),
    mkExternalWorkflowExecutionSignaledEventAttributes,
    eweseaInitiatedEventId,
    eweseaWorkflowExecution,

    -- ** FailWorkflowExecutionDecisionAttributes
    FailWorkflowExecutionDecisionAttributes (..),
    mkFailWorkflowExecutionDecisionAttributes,
    fwedaReason,
    fwedaDetails,

    -- ** FailWorkflowExecutionFailedEventAttributes
    FailWorkflowExecutionFailedEventAttributes (..),
    mkFailWorkflowExecutionFailedEventAttributes,
    fwefeaCause,
    fwefeaDecisionTaskCompletedEventId,

    -- ** HistoryEvent
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

    -- ** LambdaFunctionCompletedEventAttributes
    LambdaFunctionCompletedEventAttributes (..),
    mkLambdaFunctionCompletedEventAttributes,
    lfceaScheduledEventId,
    lfceaResult,
    lfceaStartedEventId,

    -- ** LambdaFunctionFailedEventAttributes
    LambdaFunctionFailedEventAttributes (..),
    mkLambdaFunctionFailedEventAttributes,
    lffeaScheduledEventId,
    lffeaReason,
    lffeaDetails,
    lffeaStartedEventId,

    -- ** LambdaFunctionScheduledEventAttributes
    LambdaFunctionScheduledEventAttributes (..),
    mkLambdaFunctionScheduledEventAttributes,
    lfseaControl,
    lfseaInput,
    lfseaName,
    lfseaId,
    lfseaStartToCloseTimeout,
    lfseaDecisionTaskCompletedEventId,

    -- ** LambdaFunctionStartedEventAttributes
    LambdaFunctionStartedEventAttributes (..),
    mkLambdaFunctionStartedEventAttributes,
    lfseaScheduledEventId,

    -- ** LambdaFunctionTimedOutEventAttributes
    LambdaFunctionTimedOutEventAttributes (..),
    mkLambdaFunctionTimedOutEventAttributes,
    lftoeaScheduledEventId,
    lftoeaTimeoutType,
    lftoeaStartedEventId,

    -- ** MarkerRecordedEventAttributes
    MarkerRecordedEventAttributes (..),
    mkMarkerRecordedEventAttributes,
    mreaMarkerName,
    mreaDetails,
    mreaDecisionTaskCompletedEventId,

    -- ** PendingTaskCount
    PendingTaskCount (..),
    mkPendingTaskCount,
    ptcTruncated,
    ptcCount,

    -- ** RecordMarkerDecisionAttributes
    RecordMarkerDecisionAttributes (..),
    mkRecordMarkerDecisionAttributes,
    rmdaMarkerName,
    rmdaDetails,

    -- ** RecordMarkerFailedEventAttributes
    RecordMarkerFailedEventAttributes (..),
    mkRecordMarkerFailedEventAttributes,
    rmfeaMarkerName,
    rmfeaCause,
    rmfeaDecisionTaskCompletedEventId,

    -- ** RequestCancelActivityTaskDecisionAttributes
    RequestCancelActivityTaskDecisionAttributes (..),
    mkRequestCancelActivityTaskDecisionAttributes,
    rcatdaActivityId,

    -- ** RequestCancelActivityTaskFailedEventAttributes
    RequestCancelActivityTaskFailedEventAttributes (..),
    mkRequestCancelActivityTaskFailedEventAttributes,
    rcatfeaActivityId,
    rcatfeaCause,
    rcatfeaDecisionTaskCompletedEventId,

    -- ** RequestCancelExternalWorkflowExecutionDecisionAttributes
    RequestCancelExternalWorkflowExecutionDecisionAttributes (..),
    mkRequestCancelExternalWorkflowExecutionDecisionAttributes,
    rcewedaControl,
    rcewedaRunId,
    rcewedaWorkflowId,

    -- ** RequestCancelExternalWorkflowExecutionFailedEventAttributes
    RequestCancelExternalWorkflowExecutionFailedEventAttributes (..),
    mkRequestCancelExternalWorkflowExecutionFailedEventAttributes,
    rcewefeaControl,
    rcewefeaCause,
    rcewefeaRunId,
    rcewefeaInitiatedEventId,
    rcewefeaWorkflowId,
    rcewefeaDecisionTaskCompletedEventId,

    -- ** RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (..),
    mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    rceweieaControl,
    rceweieaRunId,
    rceweieaWorkflowId,
    rceweieaDecisionTaskCompletedEventId,

    -- ** ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rtValue,
    rtKey,

    -- ** ScheduleActivityTaskDecisionAttributes
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

    -- ** ScheduleActivityTaskFailedEventAttributes
    ScheduleActivityTaskFailedEventAttributes (..),
    mkScheduleActivityTaskFailedEventAttributes,
    satfeaActivityType,
    satfeaActivityId,
    satfeaCause,
    satfeaDecisionTaskCompletedEventId,

    -- ** ScheduleLambdaFunctionDecisionAttributes
    ScheduleLambdaFunctionDecisionAttributes (..),
    mkScheduleLambdaFunctionDecisionAttributes,
    slfdaControl,
    slfdaInput,
    slfdaName,
    slfdaId,
    slfdaStartToCloseTimeout,

    -- ** ScheduleLambdaFunctionFailedEventAttributes
    ScheduleLambdaFunctionFailedEventAttributes (..),
    mkScheduleLambdaFunctionFailedEventAttributes,
    slffeaCause,
    slffeaName,
    slffeaId,
    slffeaDecisionTaskCompletedEventId,

    -- ** SignalExternalWorkflowExecutionDecisionAttributes
    SignalExternalWorkflowExecutionDecisionAttributes (..),
    mkSignalExternalWorkflowExecutionDecisionAttributes,
    sewedaControl,
    sewedaInput,
    sewedaRunId,
    sewedaWorkflowId,
    sewedaSignalName,

    -- ** SignalExternalWorkflowExecutionFailedEventAttributes
    SignalExternalWorkflowExecutionFailedEventAttributes (..),
    mkSignalExternalWorkflowExecutionFailedEventAttributes,
    sewefeaControl,
    sewefeaCause,
    sewefeaRunId,
    sewefeaInitiatedEventId,
    sewefeaWorkflowId,
    sewefeaDecisionTaskCompletedEventId,

    -- ** SignalExternalWorkflowExecutionInitiatedEventAttributes
    SignalExternalWorkflowExecutionInitiatedEventAttributes (..),
    mkSignalExternalWorkflowExecutionInitiatedEventAttributes,
    seweieaControl,
    seweieaInput,
    seweieaRunId,
    seweieaWorkflowId,
    seweieaSignalName,
    seweieaDecisionTaskCompletedEventId,

    -- ** StartChildWorkflowExecutionDecisionAttributes
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

    -- ** StartChildWorkflowExecutionFailedEventAttributes
    StartChildWorkflowExecutionFailedEventAttributes (..),
    mkStartChildWorkflowExecutionFailedEventAttributes,
    scwefeaControl,
    scwefeaWorkflowType,
    scwefeaCause,
    scwefeaInitiatedEventId,
    scwefeaWorkflowId,
    scwefeaDecisionTaskCompletedEventId,

    -- ** StartChildWorkflowExecutionInitiatedEventAttributes
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

    -- ** StartLambdaFunctionFailedEventAttributes
    StartLambdaFunctionFailedEventAttributes (..),
    mkStartLambdaFunctionFailedEventAttributes,
    sScheduledEventId,
    sCause,
    sMessage,

    -- ** StartTimerDecisionAttributes
    StartTimerDecisionAttributes (..),
    mkStartTimerDecisionAttributes,
    stdaControl,
    stdaTimerId,
    stdaStartToFireTimeout,

    -- ** StartTimerFailedEventAttributes
    StartTimerFailedEventAttributes (..),
    mkStartTimerFailedEventAttributes,
    stfeaCause,
    stfeaTimerId,
    stfeaDecisionTaskCompletedEventId,

    -- ** TagFilter
    TagFilter (..),
    mkTagFilter,
    tfTag,

    -- ** TaskList
    TaskList (..),
    mkTaskList,
    tlName,

    -- ** TimerCanceledEventAttributes
    TimerCanceledEventAttributes (..),
    mkTimerCanceledEventAttributes,
    tceaTimerId,
    tceaStartedEventId,
    tceaDecisionTaskCompletedEventId,

    -- ** TimerFiredEventAttributes
    TimerFiredEventAttributes (..),
    mkTimerFiredEventAttributes,
    tfeaTimerId,
    tfeaStartedEventId,

    -- ** TimerStartedEventAttributes
    TimerStartedEventAttributes (..),
    mkTimerStartedEventAttributes,
    tseaControl,
    tseaTimerId,
    tseaStartToFireTimeout,
    tseaDecisionTaskCompletedEventId,

    -- ** WorkflowExecution
    WorkflowExecution (..),
    mkWorkflowExecution,
    weRunId,
    weWorkflowId,

    -- ** WorkflowExecutionCancelRequestedEventAttributes
    WorkflowExecutionCancelRequestedEventAttributes (..),
    mkWorkflowExecutionCancelRequestedEventAttributes,
    wecreaExternalWorkflowExecution,
    wecreaExternalInitiatedEventId,
    wecreaCause,

    -- ** WorkflowExecutionCanceledEventAttributes
    WorkflowExecutionCanceledEventAttributes (..),
    mkWorkflowExecutionCanceledEventAttributes,
    wDetails,
    wDecisionTaskCompletedEventId,

    -- ** WorkflowExecutionCompletedEventAttributes
    WorkflowExecutionCompletedEventAttributes (..),
    mkWorkflowExecutionCompletedEventAttributes,
    weceaResult,
    weceaDecisionTaskCompletedEventId,

    -- ** WorkflowExecutionConfiguration
    WorkflowExecutionConfiguration (..),
    mkWorkflowExecutionConfiguration,
    wecTaskStartToCloseTimeout,
    wecLambdaRole,
    wecExecutionStartToCloseTimeout,
    wecTaskList,
    wecTaskPriority,
    wecChildPolicy,

    -- ** WorkflowExecutionContinuedAsNewEventAttributes
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

    -- ** WorkflowExecutionCount
    WorkflowExecutionCount (..),
    mkWorkflowExecutionCount,
    wecTruncated,
    wecCount,

    -- ** WorkflowExecutionFailedEventAttributes
    WorkflowExecutionFailedEventAttributes (..),
    mkWorkflowExecutionFailedEventAttributes,
    wefeaReason,
    wefeaDetails,
    wefeaDecisionTaskCompletedEventId,

    -- ** WorkflowExecutionFilter
    WorkflowExecutionFilter (..),
    mkWorkflowExecutionFilter,
    wefWorkflowId,

    -- ** WorkflowExecutionInfo
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

    -- ** WorkflowExecutionInfos
    WorkflowExecutionInfos (..),
    mkWorkflowExecutionInfos,
    weiExecutionInfos,
    weiNextPageToken,

    -- ** WorkflowExecutionOpenCounts
    WorkflowExecutionOpenCounts (..),
    mkWorkflowExecutionOpenCounts,
    weocOpenChildWorkflowExecutions,
    weocOpenActivityTasks,
    weocOpenLambdaFunctions,
    weocOpenDecisionTasks,
    weocOpenTimers,

    -- ** WorkflowExecutionSignaledEventAttributes
    WorkflowExecutionSignaledEventAttributes (..),
    mkWorkflowExecutionSignaledEventAttributes,
    wExternalWorkflowExecution,
    wExternalInitiatedEventId,
    wInput,
    wSignalName,

    -- ** WorkflowExecutionStartedEventAttributes
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

    -- ** WorkflowExecutionTerminatedEventAttributes
    WorkflowExecutionTerminatedEventAttributes (..),
    mkWorkflowExecutionTerminatedEventAttributes,
    weteaCause,
    weteaReason,
    weteaChildPolicy,
    weteaDetails,

    -- ** WorkflowExecutionTimedOutEventAttributes
    WorkflowExecutionTimedOutEventAttributes (..),
    mkWorkflowExecutionTimedOutEventAttributes,
    wetoeaTimeoutType,
    wetoeaChildPolicy,

    -- ** WorkflowType
    WorkflowType (..),
    mkWorkflowType,
    wtName,
    wtVersion,

    -- ** WorkflowTypeConfiguration
    WorkflowTypeConfiguration (..),
    mkWorkflowTypeConfiguration,
    wtcDefaultLambdaRole,
    wtcDefaultChildPolicy,
    wtcDefaultTaskList,
    wtcDefaultTaskPriority,
    wtcDefaultExecutionStartToCloseTimeout,
    wtcDefaultTaskStartToCloseTimeout,

    -- ** WorkflowTypeFilter
    WorkflowTypeFilter (..),
    mkWorkflowTypeFilter,
    wtfName,
    wtfVersion,

    -- ** WorkflowTypeInfo
    WorkflowTypeInfo (..),
    mkWorkflowTypeInfo,
    wtiStatus,
    wtiWorkflowType,
    wtiDeprecationDate,
    wtiCreationDate,
    wtiDescription,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.CountClosedWorkflowExecutions
import Network.AWS.SWF.CountOpenWorkflowExecutions
import Network.AWS.SWF.CountPendingActivityTasks
import Network.AWS.SWF.CountPendingDecisionTasks
import Network.AWS.SWF.DeprecateActivityType
import Network.AWS.SWF.DeprecateDomain
import Network.AWS.SWF.DeprecateWorkflowType
import Network.AWS.SWF.DescribeActivityType
import Network.AWS.SWF.DescribeDomain
import Network.AWS.SWF.DescribeWorkflowExecution
import Network.AWS.SWF.DescribeWorkflowType
import Network.AWS.SWF.GetWorkflowExecutionHistory
import Network.AWS.SWF.ListActivityTypes
import Network.AWS.SWF.ListClosedWorkflowExecutions
import Network.AWS.SWF.ListDomains
import Network.AWS.SWF.ListOpenWorkflowExecutions
import Network.AWS.SWF.ListTagsForResource
import Network.AWS.SWF.ListWorkflowTypes
import Network.AWS.SWF.PollForActivityTask
import Network.AWS.SWF.PollForDecisionTask
import Network.AWS.SWF.RecordActivityTaskHeartbeat
import Network.AWS.SWF.RegisterActivityType
import Network.AWS.SWF.RegisterDomain
import Network.AWS.SWF.RegisterWorkflowType
import Network.AWS.SWF.RequestCancelWorkflowExecution
import Network.AWS.SWF.RespondActivityTaskCanceled
import Network.AWS.SWF.RespondActivityTaskCompleted
import Network.AWS.SWF.RespondActivityTaskFailed
import Network.AWS.SWF.RespondDecisionTaskCompleted
import Network.AWS.SWF.SignalWorkflowExecution
import Network.AWS.SWF.StartWorkflowExecution
import Network.AWS.SWF.TagResource
import Network.AWS.SWF.TerminateWorkflowExecution
import Network.AWS.SWF.Types
import Network.AWS.SWF.UndeprecateActivityType
import Network.AWS.SWF.UndeprecateDomain
import Network.AWS.SWF.UndeprecateWorkflowType
import Network.AWS.SWF.UntagResource
import Network.AWS.SWF.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SWF'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

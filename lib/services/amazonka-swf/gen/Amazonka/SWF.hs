{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SWF
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2012-01-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Simple Workflow Service
--
-- The Amazon Simple Workflow Service (Amazon SWF) makes it easy to build
-- applications that use Amazon\'s cloud to coordinate work across
-- distributed components. In Amazon SWF, a /task/ represents a logical
-- unit of work that is performed by a component of your workflow.
-- Coordinating tasks in a workflow involves managing intertask
-- dependencies, scheduling, and concurrency in accordance with the logical
-- flow of the application.
--
-- Amazon SWF gives you full control over implementing tasks and
-- coordinating them without worrying about underlying complexities such as
-- tracking their progress and maintaining their state.
--
-- This documentation serves as reference only. For a broader overview of
-- the Amazon SWF programming model, see the
-- /<https://docs.aws.amazon.com/amazonswf/latest/developerguide/ Amazon SWF Developer Guide>/
-- .
module Amazonka.SWF
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DefaultUndefinedFault
    _DefaultUndefinedFault,

    -- ** DomainAlreadyExistsFault
    _DomainAlreadyExistsFault,

    -- ** DomainDeprecatedFault
    _DomainDeprecatedFault,

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** OperationNotPermittedFault
    _OperationNotPermittedFault,

    -- ** TooManyTagsFault
    _TooManyTagsFault,

    -- ** TypeAlreadyExistsFault
    _TypeAlreadyExistsFault,

    -- ** TypeDeprecatedFault
    _TypeDeprecatedFault,

    -- ** UnknownResourceFault
    _UnknownResourceFault,

    -- ** WorkflowExecutionAlreadyStartedFault
    _WorkflowExecutionAlreadyStartedFault,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CountClosedWorkflowExecutions
    CountClosedWorkflowExecutions (CountClosedWorkflowExecutions'),
    newCountClosedWorkflowExecutions,
    WorkflowExecutionCount (WorkflowExecutionCount'),
    newWorkflowExecutionCount,

    -- ** CountOpenWorkflowExecutions
    CountOpenWorkflowExecutions (CountOpenWorkflowExecutions'),
    newCountOpenWorkflowExecutions,
    WorkflowExecutionCount (WorkflowExecutionCount'),
    newWorkflowExecutionCount,

    -- ** CountPendingActivityTasks
    CountPendingActivityTasks (CountPendingActivityTasks'),
    newCountPendingActivityTasks,
    PendingTaskCount (PendingTaskCount'),
    newPendingTaskCount,

    -- ** CountPendingDecisionTasks
    CountPendingDecisionTasks (CountPendingDecisionTasks'),
    newCountPendingDecisionTasks,
    PendingTaskCount (PendingTaskCount'),
    newPendingTaskCount,

    -- ** DeprecateActivityType
    DeprecateActivityType (DeprecateActivityType'),
    newDeprecateActivityType,
    DeprecateActivityTypeResponse (DeprecateActivityTypeResponse'),
    newDeprecateActivityTypeResponse,

    -- ** DeprecateDomain
    DeprecateDomain (DeprecateDomain'),
    newDeprecateDomain,
    DeprecateDomainResponse (DeprecateDomainResponse'),
    newDeprecateDomainResponse,

    -- ** DeprecateWorkflowType
    DeprecateWorkflowType (DeprecateWorkflowType'),
    newDeprecateWorkflowType,
    DeprecateWorkflowTypeResponse (DeprecateWorkflowTypeResponse'),
    newDeprecateWorkflowTypeResponse,

    -- ** DescribeActivityType
    DescribeActivityType (DescribeActivityType'),
    newDescribeActivityType,
    DescribeActivityTypeResponse (DescribeActivityTypeResponse'),
    newDescribeActivityTypeResponse,

    -- ** DescribeDomain
    DescribeDomain (DescribeDomain'),
    newDescribeDomain,
    DescribeDomainResponse (DescribeDomainResponse'),
    newDescribeDomainResponse,

    -- ** DescribeWorkflowExecution
    DescribeWorkflowExecution (DescribeWorkflowExecution'),
    newDescribeWorkflowExecution,
    DescribeWorkflowExecutionResponse (DescribeWorkflowExecutionResponse'),
    newDescribeWorkflowExecutionResponse,

    -- ** DescribeWorkflowType
    DescribeWorkflowType (DescribeWorkflowType'),
    newDescribeWorkflowType,
    DescribeWorkflowTypeResponse (DescribeWorkflowTypeResponse'),
    newDescribeWorkflowTypeResponse,

    -- ** GetWorkflowExecutionHistory (Paginated)
    GetWorkflowExecutionHistory (GetWorkflowExecutionHistory'),
    newGetWorkflowExecutionHistory,
    GetWorkflowExecutionHistoryResponse (GetWorkflowExecutionHistoryResponse'),
    newGetWorkflowExecutionHistoryResponse,

    -- ** ListActivityTypes (Paginated)
    ListActivityTypes (ListActivityTypes'),
    newListActivityTypes,
    ListActivityTypesResponse (ListActivityTypesResponse'),
    newListActivityTypesResponse,

    -- ** ListClosedWorkflowExecutions (Paginated)
    ListClosedWorkflowExecutions (ListClosedWorkflowExecutions'),
    newListClosedWorkflowExecutions,
    WorkflowExecutionInfos (WorkflowExecutionInfos'),
    newWorkflowExecutionInfos,

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** ListOpenWorkflowExecutions (Paginated)
    ListOpenWorkflowExecutions (ListOpenWorkflowExecutions'),
    newListOpenWorkflowExecutions,
    WorkflowExecutionInfos (WorkflowExecutionInfos'),
    newWorkflowExecutionInfos,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListWorkflowTypes (Paginated)
    ListWorkflowTypes (ListWorkflowTypes'),
    newListWorkflowTypes,
    ListWorkflowTypesResponse (ListWorkflowTypesResponse'),
    newListWorkflowTypesResponse,

    -- ** PollForActivityTask
    PollForActivityTask (PollForActivityTask'),
    newPollForActivityTask,
    PollForActivityTaskResponse (PollForActivityTaskResponse'),
    newPollForActivityTaskResponse,

    -- ** PollForDecisionTask (Paginated)
    PollForDecisionTask (PollForDecisionTask'),
    newPollForDecisionTask,
    PollForDecisionTaskResponse (PollForDecisionTaskResponse'),
    newPollForDecisionTaskResponse,

    -- ** RecordActivityTaskHeartbeat
    RecordActivityTaskHeartbeat (RecordActivityTaskHeartbeat'),
    newRecordActivityTaskHeartbeat,
    RecordActivityTaskHeartbeatResponse (RecordActivityTaskHeartbeatResponse'),
    newRecordActivityTaskHeartbeatResponse,

    -- ** RegisterActivityType
    RegisterActivityType (RegisterActivityType'),
    newRegisterActivityType,
    RegisterActivityTypeResponse (RegisterActivityTypeResponse'),
    newRegisterActivityTypeResponse,

    -- ** RegisterDomain
    RegisterDomain (RegisterDomain'),
    newRegisterDomain,
    RegisterDomainResponse (RegisterDomainResponse'),
    newRegisterDomainResponse,

    -- ** RegisterWorkflowType
    RegisterWorkflowType (RegisterWorkflowType'),
    newRegisterWorkflowType,
    RegisterWorkflowTypeResponse (RegisterWorkflowTypeResponse'),
    newRegisterWorkflowTypeResponse,

    -- ** RequestCancelWorkflowExecution
    RequestCancelWorkflowExecution (RequestCancelWorkflowExecution'),
    newRequestCancelWorkflowExecution,
    RequestCancelWorkflowExecutionResponse (RequestCancelWorkflowExecutionResponse'),
    newRequestCancelWorkflowExecutionResponse,

    -- ** RespondActivityTaskCanceled
    RespondActivityTaskCanceled (RespondActivityTaskCanceled'),
    newRespondActivityTaskCanceled,
    RespondActivityTaskCanceledResponse (RespondActivityTaskCanceledResponse'),
    newRespondActivityTaskCanceledResponse,

    -- ** RespondActivityTaskCompleted
    RespondActivityTaskCompleted (RespondActivityTaskCompleted'),
    newRespondActivityTaskCompleted,
    RespondActivityTaskCompletedResponse (RespondActivityTaskCompletedResponse'),
    newRespondActivityTaskCompletedResponse,

    -- ** RespondActivityTaskFailed
    RespondActivityTaskFailed (RespondActivityTaskFailed'),
    newRespondActivityTaskFailed,
    RespondActivityTaskFailedResponse (RespondActivityTaskFailedResponse'),
    newRespondActivityTaskFailedResponse,

    -- ** RespondDecisionTaskCompleted
    RespondDecisionTaskCompleted (RespondDecisionTaskCompleted'),
    newRespondDecisionTaskCompleted,
    RespondDecisionTaskCompletedResponse (RespondDecisionTaskCompletedResponse'),
    newRespondDecisionTaskCompletedResponse,

    -- ** SignalWorkflowExecution
    SignalWorkflowExecution (SignalWorkflowExecution'),
    newSignalWorkflowExecution,
    SignalWorkflowExecutionResponse (SignalWorkflowExecutionResponse'),
    newSignalWorkflowExecutionResponse,

    -- ** StartWorkflowExecution
    StartWorkflowExecution (StartWorkflowExecution'),
    newStartWorkflowExecution,
    StartWorkflowExecutionResponse (StartWorkflowExecutionResponse'),
    newStartWorkflowExecutionResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TerminateWorkflowExecution
    TerminateWorkflowExecution (TerminateWorkflowExecution'),
    newTerminateWorkflowExecution,
    TerminateWorkflowExecutionResponse (TerminateWorkflowExecutionResponse'),
    newTerminateWorkflowExecutionResponse,

    -- ** UndeprecateActivityType
    UndeprecateActivityType (UndeprecateActivityType'),
    newUndeprecateActivityType,
    UndeprecateActivityTypeResponse (UndeprecateActivityTypeResponse'),
    newUndeprecateActivityTypeResponse,

    -- ** UndeprecateDomain
    UndeprecateDomain (UndeprecateDomain'),
    newUndeprecateDomain,
    UndeprecateDomainResponse (UndeprecateDomainResponse'),
    newUndeprecateDomainResponse,

    -- ** UndeprecateWorkflowType
    UndeprecateWorkflowType (UndeprecateWorkflowType'),
    newUndeprecateWorkflowType,
    UndeprecateWorkflowTypeResponse (UndeprecateWorkflowTypeResponse'),
    newUndeprecateWorkflowTypeResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

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
    ActivityTaskCancelRequestedEventAttributes (ActivityTaskCancelRequestedEventAttributes'),
    newActivityTaskCancelRequestedEventAttributes,

    -- ** ActivityTaskCanceledEventAttributes
    ActivityTaskCanceledEventAttributes (ActivityTaskCanceledEventAttributes'),
    newActivityTaskCanceledEventAttributes,

    -- ** ActivityTaskCompletedEventAttributes
    ActivityTaskCompletedEventAttributes (ActivityTaskCompletedEventAttributes'),
    newActivityTaskCompletedEventAttributes,

    -- ** ActivityTaskFailedEventAttributes
    ActivityTaskFailedEventAttributes (ActivityTaskFailedEventAttributes'),
    newActivityTaskFailedEventAttributes,

    -- ** ActivityTaskScheduledEventAttributes
    ActivityTaskScheduledEventAttributes (ActivityTaskScheduledEventAttributes'),
    newActivityTaskScheduledEventAttributes,

    -- ** ActivityTaskStartedEventAttributes
    ActivityTaskStartedEventAttributes (ActivityTaskStartedEventAttributes'),
    newActivityTaskStartedEventAttributes,

    -- ** ActivityTaskTimedOutEventAttributes
    ActivityTaskTimedOutEventAttributes (ActivityTaskTimedOutEventAttributes'),
    newActivityTaskTimedOutEventAttributes,

    -- ** ActivityType
    ActivityType (ActivityType'),
    newActivityType,

    -- ** ActivityTypeConfiguration
    ActivityTypeConfiguration (ActivityTypeConfiguration'),
    newActivityTypeConfiguration,

    -- ** ActivityTypeInfo
    ActivityTypeInfo (ActivityTypeInfo'),
    newActivityTypeInfo,

    -- ** CancelTimerDecisionAttributes
    CancelTimerDecisionAttributes (CancelTimerDecisionAttributes'),
    newCancelTimerDecisionAttributes,

    -- ** CancelTimerFailedEventAttributes
    CancelTimerFailedEventAttributes (CancelTimerFailedEventAttributes'),
    newCancelTimerFailedEventAttributes,

    -- ** CancelWorkflowExecutionDecisionAttributes
    CancelWorkflowExecutionDecisionAttributes (CancelWorkflowExecutionDecisionAttributes'),
    newCancelWorkflowExecutionDecisionAttributes,

    -- ** CancelWorkflowExecutionFailedEventAttributes
    CancelWorkflowExecutionFailedEventAttributes (CancelWorkflowExecutionFailedEventAttributes'),
    newCancelWorkflowExecutionFailedEventAttributes,

    -- ** ChildWorkflowExecutionCanceledEventAttributes
    ChildWorkflowExecutionCanceledEventAttributes (ChildWorkflowExecutionCanceledEventAttributes'),
    newChildWorkflowExecutionCanceledEventAttributes,

    -- ** ChildWorkflowExecutionCompletedEventAttributes
    ChildWorkflowExecutionCompletedEventAttributes (ChildWorkflowExecutionCompletedEventAttributes'),
    newChildWorkflowExecutionCompletedEventAttributes,

    -- ** ChildWorkflowExecutionFailedEventAttributes
    ChildWorkflowExecutionFailedEventAttributes (ChildWorkflowExecutionFailedEventAttributes'),
    newChildWorkflowExecutionFailedEventAttributes,

    -- ** ChildWorkflowExecutionStartedEventAttributes
    ChildWorkflowExecutionStartedEventAttributes (ChildWorkflowExecutionStartedEventAttributes'),
    newChildWorkflowExecutionStartedEventAttributes,

    -- ** ChildWorkflowExecutionTerminatedEventAttributes
    ChildWorkflowExecutionTerminatedEventAttributes (ChildWorkflowExecutionTerminatedEventAttributes'),
    newChildWorkflowExecutionTerminatedEventAttributes,

    -- ** ChildWorkflowExecutionTimedOutEventAttributes
    ChildWorkflowExecutionTimedOutEventAttributes (ChildWorkflowExecutionTimedOutEventAttributes'),
    newChildWorkflowExecutionTimedOutEventAttributes,

    -- ** CloseStatusFilter
    CloseStatusFilter (CloseStatusFilter'),
    newCloseStatusFilter,

    -- ** CompleteWorkflowExecutionDecisionAttributes
    CompleteWorkflowExecutionDecisionAttributes (CompleteWorkflowExecutionDecisionAttributes'),
    newCompleteWorkflowExecutionDecisionAttributes,

    -- ** CompleteWorkflowExecutionFailedEventAttributes
    CompleteWorkflowExecutionFailedEventAttributes (CompleteWorkflowExecutionFailedEventAttributes'),
    newCompleteWorkflowExecutionFailedEventAttributes,

    -- ** ContinueAsNewWorkflowExecutionDecisionAttributes
    ContinueAsNewWorkflowExecutionDecisionAttributes (ContinueAsNewWorkflowExecutionDecisionAttributes'),
    newContinueAsNewWorkflowExecutionDecisionAttributes,

    -- ** ContinueAsNewWorkflowExecutionFailedEventAttributes
    ContinueAsNewWorkflowExecutionFailedEventAttributes (ContinueAsNewWorkflowExecutionFailedEventAttributes'),
    newContinueAsNewWorkflowExecutionFailedEventAttributes,

    -- ** Decision
    Decision (Decision'),
    newDecision,

    -- ** DecisionTaskCompletedEventAttributes
    DecisionTaskCompletedEventAttributes (DecisionTaskCompletedEventAttributes'),
    newDecisionTaskCompletedEventAttributes,

    -- ** DecisionTaskScheduledEventAttributes
    DecisionTaskScheduledEventAttributes (DecisionTaskScheduledEventAttributes'),
    newDecisionTaskScheduledEventAttributes,

    -- ** DecisionTaskStartedEventAttributes
    DecisionTaskStartedEventAttributes (DecisionTaskStartedEventAttributes'),
    newDecisionTaskStartedEventAttributes,

    -- ** DecisionTaskTimedOutEventAttributes
    DecisionTaskTimedOutEventAttributes (DecisionTaskTimedOutEventAttributes'),
    newDecisionTaskTimedOutEventAttributes,

    -- ** DomainConfiguration
    DomainConfiguration (DomainConfiguration'),
    newDomainConfiguration,

    -- ** DomainInfo
    DomainInfo (DomainInfo'),
    newDomainInfo,

    -- ** ExecutionTimeFilter
    ExecutionTimeFilter (ExecutionTimeFilter'),
    newExecutionTimeFilter,

    -- ** ExternalWorkflowExecutionCancelRequestedEventAttributes
    ExternalWorkflowExecutionCancelRequestedEventAttributes (ExternalWorkflowExecutionCancelRequestedEventAttributes'),
    newExternalWorkflowExecutionCancelRequestedEventAttributes,

    -- ** ExternalWorkflowExecutionSignaledEventAttributes
    ExternalWorkflowExecutionSignaledEventAttributes (ExternalWorkflowExecutionSignaledEventAttributes'),
    newExternalWorkflowExecutionSignaledEventAttributes,

    -- ** FailWorkflowExecutionDecisionAttributes
    FailWorkflowExecutionDecisionAttributes (FailWorkflowExecutionDecisionAttributes'),
    newFailWorkflowExecutionDecisionAttributes,

    -- ** FailWorkflowExecutionFailedEventAttributes
    FailWorkflowExecutionFailedEventAttributes (FailWorkflowExecutionFailedEventAttributes'),
    newFailWorkflowExecutionFailedEventAttributes,

    -- ** HistoryEvent
    HistoryEvent (HistoryEvent'),
    newHistoryEvent,

    -- ** LambdaFunctionCompletedEventAttributes
    LambdaFunctionCompletedEventAttributes (LambdaFunctionCompletedEventAttributes'),
    newLambdaFunctionCompletedEventAttributes,

    -- ** LambdaFunctionFailedEventAttributes
    LambdaFunctionFailedEventAttributes (LambdaFunctionFailedEventAttributes'),
    newLambdaFunctionFailedEventAttributes,

    -- ** LambdaFunctionScheduledEventAttributes
    LambdaFunctionScheduledEventAttributes (LambdaFunctionScheduledEventAttributes'),
    newLambdaFunctionScheduledEventAttributes,

    -- ** LambdaFunctionStartedEventAttributes
    LambdaFunctionStartedEventAttributes (LambdaFunctionStartedEventAttributes'),
    newLambdaFunctionStartedEventAttributes,

    -- ** LambdaFunctionTimedOutEventAttributes
    LambdaFunctionTimedOutEventAttributes (LambdaFunctionTimedOutEventAttributes'),
    newLambdaFunctionTimedOutEventAttributes,

    -- ** MarkerRecordedEventAttributes
    MarkerRecordedEventAttributes (MarkerRecordedEventAttributes'),
    newMarkerRecordedEventAttributes,

    -- ** PendingTaskCount
    PendingTaskCount (PendingTaskCount'),
    newPendingTaskCount,

    -- ** RecordMarkerDecisionAttributes
    RecordMarkerDecisionAttributes (RecordMarkerDecisionAttributes'),
    newRecordMarkerDecisionAttributes,

    -- ** RecordMarkerFailedEventAttributes
    RecordMarkerFailedEventAttributes (RecordMarkerFailedEventAttributes'),
    newRecordMarkerFailedEventAttributes,

    -- ** RequestCancelActivityTaskDecisionAttributes
    RequestCancelActivityTaskDecisionAttributes (RequestCancelActivityTaskDecisionAttributes'),
    newRequestCancelActivityTaskDecisionAttributes,

    -- ** RequestCancelActivityTaskFailedEventAttributes
    RequestCancelActivityTaskFailedEventAttributes (RequestCancelActivityTaskFailedEventAttributes'),
    newRequestCancelActivityTaskFailedEventAttributes,

    -- ** RequestCancelExternalWorkflowExecutionDecisionAttributes
    RequestCancelExternalWorkflowExecutionDecisionAttributes (RequestCancelExternalWorkflowExecutionDecisionAttributes'),
    newRequestCancelExternalWorkflowExecutionDecisionAttributes,

    -- ** RequestCancelExternalWorkflowExecutionFailedEventAttributes
    RequestCancelExternalWorkflowExecutionFailedEventAttributes (RequestCancelExternalWorkflowExecutionFailedEventAttributes'),
    newRequestCancelExternalWorkflowExecutionFailedEventAttributes,

    -- ** RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'),
    newRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,

    -- ** ResourceTag
    ResourceTag (ResourceTag'),
    newResourceTag,

    -- ** ScheduleActivityTaskDecisionAttributes
    ScheduleActivityTaskDecisionAttributes (ScheduleActivityTaskDecisionAttributes'),
    newScheduleActivityTaskDecisionAttributes,

    -- ** ScheduleActivityTaskFailedEventAttributes
    ScheduleActivityTaskFailedEventAttributes (ScheduleActivityTaskFailedEventAttributes'),
    newScheduleActivityTaskFailedEventAttributes,

    -- ** ScheduleLambdaFunctionDecisionAttributes
    ScheduleLambdaFunctionDecisionAttributes (ScheduleLambdaFunctionDecisionAttributes'),
    newScheduleLambdaFunctionDecisionAttributes,

    -- ** ScheduleLambdaFunctionFailedEventAttributes
    ScheduleLambdaFunctionFailedEventAttributes (ScheduleLambdaFunctionFailedEventAttributes'),
    newScheduleLambdaFunctionFailedEventAttributes,

    -- ** SignalExternalWorkflowExecutionDecisionAttributes
    SignalExternalWorkflowExecutionDecisionAttributes (SignalExternalWorkflowExecutionDecisionAttributes'),
    newSignalExternalWorkflowExecutionDecisionAttributes,

    -- ** SignalExternalWorkflowExecutionFailedEventAttributes
    SignalExternalWorkflowExecutionFailedEventAttributes (SignalExternalWorkflowExecutionFailedEventAttributes'),
    newSignalExternalWorkflowExecutionFailedEventAttributes,

    -- ** SignalExternalWorkflowExecutionInitiatedEventAttributes
    SignalExternalWorkflowExecutionInitiatedEventAttributes (SignalExternalWorkflowExecutionInitiatedEventAttributes'),
    newSignalExternalWorkflowExecutionInitiatedEventAttributes,

    -- ** StartChildWorkflowExecutionDecisionAttributes
    StartChildWorkflowExecutionDecisionAttributes (StartChildWorkflowExecutionDecisionAttributes'),
    newStartChildWorkflowExecutionDecisionAttributes,

    -- ** StartChildWorkflowExecutionFailedEventAttributes
    StartChildWorkflowExecutionFailedEventAttributes (StartChildWorkflowExecutionFailedEventAttributes'),
    newStartChildWorkflowExecutionFailedEventAttributes,

    -- ** StartChildWorkflowExecutionInitiatedEventAttributes
    StartChildWorkflowExecutionInitiatedEventAttributes (StartChildWorkflowExecutionInitiatedEventAttributes'),
    newStartChildWorkflowExecutionInitiatedEventAttributes,

    -- ** StartLambdaFunctionFailedEventAttributes
    StartLambdaFunctionFailedEventAttributes (StartLambdaFunctionFailedEventAttributes'),
    newStartLambdaFunctionFailedEventAttributes,

    -- ** StartTimerDecisionAttributes
    StartTimerDecisionAttributes (StartTimerDecisionAttributes'),
    newStartTimerDecisionAttributes,

    -- ** StartTimerFailedEventAttributes
    StartTimerFailedEventAttributes (StartTimerFailedEventAttributes'),
    newStartTimerFailedEventAttributes,

    -- ** TagFilter
    TagFilter (TagFilter'),
    newTagFilter,

    -- ** TaskList
    TaskList (TaskList'),
    newTaskList,

    -- ** TimerCanceledEventAttributes
    TimerCanceledEventAttributes (TimerCanceledEventAttributes'),
    newTimerCanceledEventAttributes,

    -- ** TimerFiredEventAttributes
    TimerFiredEventAttributes (TimerFiredEventAttributes'),
    newTimerFiredEventAttributes,

    -- ** TimerStartedEventAttributes
    TimerStartedEventAttributes (TimerStartedEventAttributes'),
    newTimerStartedEventAttributes,

    -- ** WorkflowExecution
    WorkflowExecution (WorkflowExecution'),
    newWorkflowExecution,

    -- ** WorkflowExecutionCancelRequestedEventAttributes
    WorkflowExecutionCancelRequestedEventAttributes (WorkflowExecutionCancelRequestedEventAttributes'),
    newWorkflowExecutionCancelRequestedEventAttributes,

    -- ** WorkflowExecutionCanceledEventAttributes
    WorkflowExecutionCanceledEventAttributes (WorkflowExecutionCanceledEventAttributes'),
    newWorkflowExecutionCanceledEventAttributes,

    -- ** WorkflowExecutionCompletedEventAttributes
    WorkflowExecutionCompletedEventAttributes (WorkflowExecutionCompletedEventAttributes'),
    newWorkflowExecutionCompletedEventAttributes,

    -- ** WorkflowExecutionConfiguration
    WorkflowExecutionConfiguration (WorkflowExecutionConfiguration'),
    newWorkflowExecutionConfiguration,

    -- ** WorkflowExecutionContinuedAsNewEventAttributes
    WorkflowExecutionContinuedAsNewEventAttributes (WorkflowExecutionContinuedAsNewEventAttributes'),
    newWorkflowExecutionContinuedAsNewEventAttributes,

    -- ** WorkflowExecutionCount
    WorkflowExecutionCount (WorkflowExecutionCount'),
    newWorkflowExecutionCount,

    -- ** WorkflowExecutionFailedEventAttributes
    WorkflowExecutionFailedEventAttributes (WorkflowExecutionFailedEventAttributes'),
    newWorkflowExecutionFailedEventAttributes,

    -- ** WorkflowExecutionFilter
    WorkflowExecutionFilter (WorkflowExecutionFilter'),
    newWorkflowExecutionFilter,

    -- ** WorkflowExecutionInfo
    WorkflowExecutionInfo (WorkflowExecutionInfo'),
    newWorkflowExecutionInfo,

    -- ** WorkflowExecutionInfos
    WorkflowExecutionInfos (WorkflowExecutionInfos'),
    newWorkflowExecutionInfos,

    -- ** WorkflowExecutionOpenCounts
    WorkflowExecutionOpenCounts (WorkflowExecutionOpenCounts'),
    newWorkflowExecutionOpenCounts,

    -- ** WorkflowExecutionSignaledEventAttributes
    WorkflowExecutionSignaledEventAttributes (WorkflowExecutionSignaledEventAttributes'),
    newWorkflowExecutionSignaledEventAttributes,

    -- ** WorkflowExecutionStartedEventAttributes
    WorkflowExecutionStartedEventAttributes (WorkflowExecutionStartedEventAttributes'),
    newWorkflowExecutionStartedEventAttributes,

    -- ** WorkflowExecutionTerminatedEventAttributes
    WorkflowExecutionTerminatedEventAttributes (WorkflowExecutionTerminatedEventAttributes'),
    newWorkflowExecutionTerminatedEventAttributes,

    -- ** WorkflowExecutionTimedOutEventAttributes
    WorkflowExecutionTimedOutEventAttributes (WorkflowExecutionTimedOutEventAttributes'),
    newWorkflowExecutionTimedOutEventAttributes,

    -- ** WorkflowType
    WorkflowType (WorkflowType'),
    newWorkflowType,

    -- ** WorkflowTypeConfiguration
    WorkflowTypeConfiguration (WorkflowTypeConfiguration'),
    newWorkflowTypeConfiguration,

    -- ** WorkflowTypeFilter
    WorkflowTypeFilter (WorkflowTypeFilter'),
    newWorkflowTypeFilter,

    -- ** WorkflowTypeInfo
    WorkflowTypeInfo (WorkflowTypeInfo'),
    newWorkflowTypeInfo,
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
import Amazonka.SWF.Lens
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
import Amazonka.SWF.Types
import Amazonka.SWF.UndeprecateActivityType
import Amazonka.SWF.UndeprecateDomain
import Amazonka.SWF.UndeprecateWorkflowType
import Amazonka.SWF.UntagResource
import Amazonka.SWF.Waiters

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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
module Network.AWS.SWF
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** DomainAlreadyExistsFault
    _DomainAlreadyExistsFault,

    -- ** WorkflowExecutionAlreadyStartedFault
    _WorkflowExecutionAlreadyStartedFault,

    -- ** DomainDeprecatedFault
    _DomainDeprecatedFault,

    -- ** UnknownResourceFault
    _UnknownResourceFault,

    -- ** OperationNotPermittedFault
    _OperationNotPermittedFault,

    -- ** TypeAlreadyExistsFault
    _TypeAlreadyExistsFault,

    -- ** TooManyTagsFault
    _TooManyTagsFault,

    -- ** TypeDeprecatedFault
    _TypeDeprecatedFault,

    -- ** DefaultUndefinedFault
    _DefaultUndefinedFault,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** SignalWorkflowExecution
    SignalWorkflowExecution (SignalWorkflowExecution'),
    newSignalWorkflowExecution,
    SignalWorkflowExecutionResponse (SignalWorkflowExecutionResponse'),
    newSignalWorkflowExecutionResponse,

    -- ** DescribeWorkflowExecution
    DescribeWorkflowExecution (DescribeWorkflowExecution'),
    newDescribeWorkflowExecution,
    DescribeWorkflowExecutionResponse (DescribeWorkflowExecutionResponse'),
    newDescribeWorkflowExecutionResponse,

    -- ** PollForActivityTask
    PollForActivityTask (PollForActivityTask'),
    newPollForActivityTask,
    PollForActivityTaskResponse (PollForActivityTaskResponse'),
    newPollForActivityTaskResponse,

    -- ** RegisterActivityType
    RegisterActivityType (RegisterActivityType'),
    newRegisterActivityType,
    RegisterActivityTypeResponse (RegisterActivityTypeResponse'),
    newRegisterActivityTypeResponse,

    -- ** ListOpenWorkflowExecutions (Paginated)
    ListOpenWorkflowExecutions (ListOpenWorkflowExecutions'),
    newListOpenWorkflowExecutions,
    WorkflowExecutionInfos (WorkflowExecutionInfos'),
    newWorkflowExecutionInfos,

    -- ** RegisterDomain
    RegisterDomain (RegisterDomain'),
    newRegisterDomain,
    RegisterDomainResponse (RegisterDomainResponse'),
    newRegisterDomainResponse,

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** RespondDecisionTaskCompleted
    RespondDecisionTaskCompleted (RespondDecisionTaskCompleted'),
    newRespondDecisionTaskCompleted,
    RespondDecisionTaskCompletedResponse (RespondDecisionTaskCompletedResponse'),
    newRespondDecisionTaskCompletedResponse,

    -- ** PollForDecisionTask (Paginated)
    PollForDecisionTask (PollForDecisionTask'),
    newPollForDecisionTask,
    PollForDecisionTaskResponse (PollForDecisionTaskResponse'),
    newPollForDecisionTaskResponse,

    -- ** DeprecateWorkflowType
    DeprecateWorkflowType (DeprecateWorkflowType'),
    newDeprecateWorkflowType,
    DeprecateWorkflowTypeResponse (DeprecateWorkflowTypeResponse'),
    newDeprecateWorkflowTypeResponse,

    -- ** UndeprecateDomain
    UndeprecateDomain (UndeprecateDomain'),
    newUndeprecateDomain,
    UndeprecateDomainResponse (UndeprecateDomainResponse'),
    newUndeprecateDomainResponse,

    -- ** UndeprecateActivityType
    UndeprecateActivityType (UndeprecateActivityType'),
    newUndeprecateActivityType,
    UndeprecateActivityTypeResponse (UndeprecateActivityTypeResponse'),
    newUndeprecateActivityTypeResponse,

    -- ** CountClosedWorkflowExecutions
    CountClosedWorkflowExecutions (CountClosedWorkflowExecutions'),
    newCountClosedWorkflowExecutions,
    WorkflowExecutionCount (WorkflowExecutionCount'),
    newWorkflowExecutionCount,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeprecateActivityType
    DeprecateActivityType (DeprecateActivityType'),
    newDeprecateActivityType,
    DeprecateActivityTypeResponse (DeprecateActivityTypeResponse'),
    newDeprecateActivityTypeResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeDomain
    DescribeDomain (DescribeDomain'),
    newDescribeDomain,
    DescribeDomainResponse (DescribeDomainResponse'),
    newDescribeDomainResponse,

    -- ** CountOpenWorkflowExecutions
    CountOpenWorkflowExecutions (CountOpenWorkflowExecutions'),
    newCountOpenWorkflowExecutions,
    WorkflowExecutionCount (WorkflowExecutionCount'),
    newWorkflowExecutionCount,

    -- ** CountPendingDecisionTasks
    CountPendingDecisionTasks (CountPendingDecisionTasks'),
    newCountPendingDecisionTasks,
    PendingTaskCount (PendingTaskCount'),
    newPendingTaskCount,

    -- ** RegisterWorkflowType
    RegisterWorkflowType (RegisterWorkflowType'),
    newRegisterWorkflowType,
    RegisterWorkflowTypeResponse (RegisterWorkflowTypeResponse'),
    newRegisterWorkflowTypeResponse,

    -- ** CountPendingActivityTasks
    CountPendingActivityTasks (CountPendingActivityTasks'),
    newCountPendingActivityTasks,
    PendingTaskCount (PendingTaskCount'),
    newPendingTaskCount,

    -- ** RespondActivityTaskCompleted
    RespondActivityTaskCompleted (RespondActivityTaskCompleted'),
    newRespondActivityTaskCompleted,
    RespondActivityTaskCompletedResponse (RespondActivityTaskCompletedResponse'),
    newRespondActivityTaskCompletedResponse,

    -- ** ListActivityTypes (Paginated)
    ListActivityTypes (ListActivityTypes'),
    newListActivityTypes,
    ListActivityTypesResponse (ListActivityTypesResponse'),
    newListActivityTypesResponse,

    -- ** StartWorkflowExecution
    StartWorkflowExecution (StartWorkflowExecution'),
    newStartWorkflowExecution,
    StartWorkflowExecutionResponse (StartWorkflowExecutionResponse'),
    newStartWorkflowExecutionResponse,

    -- ** DescribeWorkflowType
    DescribeWorkflowType (DescribeWorkflowType'),
    newDescribeWorkflowType,
    DescribeWorkflowTypeResponse (DescribeWorkflowTypeResponse'),
    newDescribeWorkflowTypeResponse,

    -- ** RespondActivityTaskCanceled
    RespondActivityTaskCanceled (RespondActivityTaskCanceled'),
    newRespondActivityTaskCanceled,
    RespondActivityTaskCanceledResponse (RespondActivityTaskCanceledResponse'),
    newRespondActivityTaskCanceledResponse,

    -- ** RequestCancelWorkflowExecution
    RequestCancelWorkflowExecution (RequestCancelWorkflowExecution'),
    newRequestCancelWorkflowExecution,
    RequestCancelWorkflowExecutionResponse (RequestCancelWorkflowExecutionResponse'),
    newRequestCancelWorkflowExecutionResponse,

    -- ** DescribeActivityType
    DescribeActivityType (DescribeActivityType'),
    newDescribeActivityType,
    DescribeActivityTypeResponse (DescribeActivityTypeResponse'),
    newDescribeActivityTypeResponse,

    -- ** TerminateWorkflowExecution
    TerminateWorkflowExecution (TerminateWorkflowExecution'),
    newTerminateWorkflowExecution,
    TerminateWorkflowExecutionResponse (TerminateWorkflowExecutionResponse'),
    newTerminateWorkflowExecutionResponse,

    -- ** RecordActivityTaskHeartbeat
    RecordActivityTaskHeartbeat (RecordActivityTaskHeartbeat'),
    newRecordActivityTaskHeartbeat,
    RecordActivityTaskHeartbeatResponse (RecordActivityTaskHeartbeatResponse'),
    newRecordActivityTaskHeartbeatResponse,

    -- ** GetWorkflowExecutionHistory (Paginated)
    GetWorkflowExecutionHistory (GetWorkflowExecutionHistory'),
    newGetWorkflowExecutionHistory,
    GetWorkflowExecutionHistoryResponse (GetWorkflowExecutionHistoryResponse'),
    newGetWorkflowExecutionHistoryResponse,

    -- ** DeprecateDomain
    DeprecateDomain (DeprecateDomain'),
    newDeprecateDomain,
    DeprecateDomainResponse (DeprecateDomainResponse'),
    newDeprecateDomainResponse,

    -- ** UndeprecateWorkflowType
    UndeprecateWorkflowType (UndeprecateWorkflowType'),
    newUndeprecateWorkflowType,
    UndeprecateWorkflowTypeResponse (UndeprecateWorkflowTypeResponse'),
    newUndeprecateWorkflowTypeResponse,

    -- ** RespondActivityTaskFailed
    RespondActivityTaskFailed (RespondActivityTaskFailed'),
    newRespondActivityTaskFailed,
    RespondActivityTaskFailedResponse (RespondActivityTaskFailedResponse'),
    newRespondActivityTaskFailedResponse,

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

    -- ** ListClosedWorkflowExecutions (Paginated)
    ListClosedWorkflowExecutions (ListClosedWorkflowExecutions'),
    newListClosedWorkflowExecutions,
    WorkflowExecutionInfos (WorkflowExecutionInfos'),
    newWorkflowExecutionInfos,

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
import Network.AWS.SWF.Lens
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

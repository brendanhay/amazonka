{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.HistoryEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.HistoryEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SWF.Types.ActivityTaskCancelRequestedEventAttributes
import Network.AWS.SWF.Types.ActivityTaskCanceledEventAttributes
import Network.AWS.SWF.Types.ActivityTaskCompletedEventAttributes
import Network.AWS.SWF.Types.ActivityTaskFailedEventAttributes
import Network.AWS.SWF.Types.ActivityTaskScheduledEventAttributes
import Network.AWS.SWF.Types.ActivityTaskStartedEventAttributes
import Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes
import Network.AWS.SWF.Types.CancelTimerFailedEventAttributes
import Network.AWS.SWF.Types.CancelWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes
import Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
import Network.AWS.SWF.Types.CompleteWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.DecisionTaskCompletedEventAttributes
import Network.AWS.SWF.Types.DecisionTaskScheduledEventAttributes
import Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes
import Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes
import Network.AWS.SWF.Types.EventType
import Network.AWS.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
import Network.AWS.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes
import Network.AWS.SWF.Types.FailWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes
import Network.AWS.SWF.Types.LambdaFunctionTimedOutEventAttributes
import Network.AWS.SWF.Types.MarkerRecordedEventAttributes
import Network.AWS.SWF.Types.RecordMarkerFailedEventAttributes
import Network.AWS.SWF.Types.RequestCancelActivityTaskFailedEventAttributes
import Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
import Network.AWS.SWF.Types.ScheduleActivityTaskFailedEventAttributes
import Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
import Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
import Network.AWS.SWF.Types.StartLambdaFunctionFailedEventAttributes
import Network.AWS.SWF.Types.StartTimerFailedEventAttributes
import Network.AWS.SWF.Types.TimerCanceledEventAttributes
import Network.AWS.SWF.Types.TimerFiredEventAttributes
import Network.AWS.SWF.Types.TimerStartedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionTerminatedEventAttributes
import Network.AWS.SWF.Types.WorkflowExecutionTimedOutEventAttributes

-- | Event within a workflow execution. A history event can be one of these
-- types:
--
-- -   @ActivityTaskCancelRequested@ – A @RequestCancelActivityTask@
--     decision was received by the system.
--
-- -   @ActivityTaskCanceled@ – The activity task was successfully
--     canceled.
--
-- -   @ActivityTaskCompleted@ – An activity worker successfully completed
--     an activity task by calling RespondActivityTaskCompleted.
--
-- -   @ActivityTaskFailed@ – An activity worker failed an activity task by
--     calling RespondActivityTaskFailed.
--
-- -   @ActivityTaskScheduled@ – An activity task was scheduled for
--     execution.
--
-- -   @ActivityTaskStarted@ – The scheduled activity task was dispatched
--     to a worker.
--
-- -   @ActivityTaskTimedOut@ – The activity task timed out.
--
-- -   @CancelTimerFailed@ – Failed to process CancelTimer decision. This
--     happens when the decision isn\'t configured properly, for example no
--     timer exists with the specified timer Id.
--
-- -   @CancelWorkflowExecutionFailed@ – A request to cancel a workflow
--     execution failed.
--
-- -   @ChildWorkflowExecutionCanceled@ – A child workflow execution,
--     started by this workflow execution, was canceled and closed.
--
-- -   @ChildWorkflowExecutionCompleted@ – A child workflow execution,
--     started by this workflow execution, completed successfully and was
--     closed.
--
-- -   @ChildWorkflowExecutionFailed@ – A child workflow execution, started
--     by this workflow execution, failed to complete successfully and was
--     closed.
--
-- -   @ChildWorkflowExecutionStarted@ – A child workflow execution was
--     successfully started.
--
-- -   @ChildWorkflowExecutionTerminated@ – A child workflow execution,
--     started by this workflow execution, was terminated.
--
-- -   @ChildWorkflowExecutionTimedOut@ – A child workflow execution,
--     started by this workflow execution, timed out and was closed.
--
-- -   @CompleteWorkflowExecutionFailed@ – The workflow execution failed to
--     complete.
--
-- -   @ContinueAsNewWorkflowExecutionFailed@ – The workflow execution
--     failed to complete after being continued as a new workflow
--     execution.
--
-- -   @DecisionTaskCompleted@ – The decider successfully completed a
--     decision task by calling RespondDecisionTaskCompleted.
--
-- -   @DecisionTaskScheduled@ – A decision task was scheduled for the
--     workflow execution.
--
-- -   @DecisionTaskStarted@ – The decision task was dispatched to a
--     decider.
--
-- -   @DecisionTaskTimedOut@ – The decision task timed out.
--
-- -   @ExternalWorkflowExecutionCancelRequested@ – Request to cancel an
--     external workflow execution was successfully delivered to the target
--     execution.
--
-- -   @ExternalWorkflowExecutionSignaled@ – A signal, requested by this
--     workflow execution, was successfully delivered to the target
--     external workflow execution.
--
-- -   @FailWorkflowExecutionFailed@ – A request to mark a workflow
--     execution as failed, itself failed.
--
-- -   @MarkerRecorded@ – A marker was recorded in the workflow history as
--     the result of a @RecordMarker@ decision.
--
-- -   @RecordMarkerFailed@ – A @RecordMarker@ decision was returned as
--     failed.
--
-- -   @RequestCancelActivityTaskFailed@ – Failed to process
--     RequestCancelActivityTask decision. This happens when the decision
--     isn\'t configured properly.
--
-- -   @RequestCancelExternalWorkflowExecutionFailed@ – Request to cancel
--     an external workflow execution failed.
--
-- -   @RequestCancelExternalWorkflowExecutionInitiated@ – A request was
--     made to request the cancellation of an external workflow execution.
--
-- -   @ScheduleActivityTaskFailed@ – Failed to process
--     ScheduleActivityTask decision. This happens when the decision isn\'t
--     configured properly, for example the activity type specified isn\'t
--     registered.
--
-- -   @SignalExternalWorkflowExecutionFailed@ – The request to signal an
--     external workflow execution failed.
--
-- -   @SignalExternalWorkflowExecutionInitiated@ – A request to signal an
--     external workflow was made.
--
-- -   @StartActivityTaskFailed@ – A scheduled activity task failed to
--     start.
--
-- -   @StartChildWorkflowExecutionFailed@ – Failed to process
--     StartChildWorkflowExecution decision. This happens when the decision
--     isn\'t configured properly, for example the workflow type specified
--     isn\'t registered.
--
-- -   @StartChildWorkflowExecutionInitiated@ – A request was made to start
--     a child workflow execution.
--
-- -   @StartTimerFailed@ – Failed to process StartTimer decision. This
--     happens when the decision isn\'t configured properly, for example a
--     timer already exists with the specified timer Id.
--
-- -   @TimerCanceled@ – A timer, previously started for this workflow
--     execution, was successfully canceled.
--
-- -   @TimerFired@ – A timer, previously started for this workflow
--     execution, fired.
--
-- -   @TimerStarted@ – A timer was started for the workflow execution due
--     to a @StartTimer@ decision.
--
-- -   @WorkflowExecutionCancelRequested@ – A request to cancel this
--     workflow execution was made.
--
-- -   @WorkflowExecutionCanceled@ – The workflow execution was
--     successfully canceled and closed.
--
-- -   @WorkflowExecutionCompleted@ – The workflow execution was closed due
--     to successful completion.
--
-- -   @WorkflowExecutionContinuedAsNew@ – The workflow execution was
--     closed and a new execution of the same type was created with the
--     same workflowId.
--
-- -   @WorkflowExecutionFailed@ – The workflow execution closed due to a
--     failure.
--
-- -   @WorkflowExecutionSignaled@ – An external signal was received for
--     the workflow execution.
--
-- -   @WorkflowExecutionStarted@ – The workflow execution was started.
--
-- -   @WorkflowExecutionTerminated@ – The workflow execution was
--     terminated.
--
-- -   @WorkflowExecutionTimedOut@ – The workflow execution was closed
--     because a time out was exceeded.
--
-- /See:/ 'newHistoryEvent' smart constructor.
data HistoryEvent = HistoryEvent'
  { -- | If the event is of type @ChildWorkflowExecutionTimedOut@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    childWorkflowExecutionTimedOutEventAttributes :: Core.Maybe ChildWorkflowExecutionTimedOutEventAttributes,
    -- | If the event is of type
    -- @RequestCancelExternalWorkflowExecutionInitiated@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Core.Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    -- | Provides the details of the @LambdaFunctionStarted@ event. It isn\'t set
    -- for other event types.
    lambdaFunctionStartedEventAttributes :: Core.Maybe LambdaFunctionStartedEventAttributes,
    -- | If the event is of type @StartChildWorkflowExecutionInitiated@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    startChildWorkflowExecutionInitiatedEventAttributes :: Core.Maybe StartChildWorkflowExecutionInitiatedEventAttributes,
    -- | If the event is of type @DecisionTaskScheduled@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    decisionTaskScheduledEventAttributes :: Core.Maybe DecisionTaskScheduledEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionCanceled@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    childWorkflowExecutionCanceledEventAttributes :: Core.Maybe ChildWorkflowExecutionCanceledEventAttributes,
    -- | If the event is of type @ActivityTaskCanceled@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    activityTaskCanceledEventAttributes :: Core.Maybe ActivityTaskCanceledEventAttributes,
    -- | If the event is of type @ActivityTaskTimedOut@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    activityTaskTimedOutEventAttributes :: Core.Maybe ActivityTaskTimedOutEventAttributes,
    -- | If the event is of type @ExternalWorkflowExecutionCancelRequested@ then
    -- this member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    externalWorkflowExecutionCancelRequestedEventAttributes :: Core.Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes,
    -- | If the event is of type @CancelTimerFailed@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    cancelTimerFailedEventAttributes :: Core.Maybe CancelTimerFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionStarted@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    childWorkflowExecutionStartedEventAttributes :: Core.Maybe ChildWorkflowExecutionStartedEventAttributes,
    -- | Provides the details of the @LambdaFunctionTimedOut@ event. It isn\'t
    -- set for other event types.
    lambdaFunctionTimedOutEventAttributes :: Core.Maybe LambdaFunctionTimedOutEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionFailed@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    childWorkflowExecutionFailedEventAttributes :: Core.Maybe ChildWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @TimerCanceled@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    timerCanceledEventAttributes :: Core.Maybe TimerCanceledEventAttributes,
    -- | Provides the details of the @LambdaFunctionCompleted@ event. It isn\'t
    -- set for other event types.
    lambdaFunctionCompletedEventAttributes :: Core.Maybe LambdaFunctionCompletedEventAttributes,
    -- | If the event is of type @CompleteWorkflowExecutionFailed@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    completeWorkflowExecutionFailedEventAttributes :: Core.Maybe CompleteWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @ActivityTaskFailed@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    activityTaskFailedEventAttributes :: Core.Maybe ActivityTaskFailedEventAttributes,
    -- | If the event is of type @TimerFired@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    timerFiredEventAttributes :: Core.Maybe TimerFiredEventAttributes,
    -- | If the event is of type @WorkflowExecutionSignaled@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionSignaledEventAttributes :: Core.Maybe WorkflowExecutionSignaledEventAttributes,
    -- | If the event is of type @ActivityTaskcancelRequested@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    activityTaskCancelRequestedEventAttributes :: Core.Maybe ActivityTaskCancelRequestedEventAttributes,
    -- | If the event is of type @WorkflowExecutionCanceled@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionCanceledEventAttributes :: Core.Maybe WorkflowExecutionCanceledEventAttributes,
    -- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn\'t
    -- set for other event types.
    startLambdaFunctionFailedEventAttributes :: Core.Maybe StartLambdaFunctionFailedEventAttributes,
    -- | If the event is of type @ScheduleActivityTaskFailed@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    scheduleActivityTaskFailedEventAttributes :: Core.Maybe ScheduleActivityTaskFailedEventAttributes,
    -- | If the event is of type @WorkflowExecutionTimedOut@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionTimedOutEventAttributes :: Core.Maybe WorkflowExecutionTimedOutEventAttributes,
    -- | If the event is of type @MarkerRecorded@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    markerRecordedEventAttributes :: Core.Maybe MarkerRecordedEventAttributes,
    -- | If the event is of type @ActivityTaskScheduled@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    activityTaskScheduledEventAttributes :: Core.Maybe ActivityTaskScheduledEventAttributes,
    -- | If the event is of type @StartTimerFailed@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    startTimerFailedEventAttributes :: Core.Maybe StartTimerFailedEventAttributes,
    -- | If the event is of type @WorkflowExecutionTerminated@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    workflowExecutionTerminatedEventAttributes :: Core.Maybe WorkflowExecutionTerminatedEventAttributes,
    -- | If the event is of type @WorkflowExecutionCompleted@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionCompletedEventAttributes :: Core.Maybe WorkflowExecutionCompletedEventAttributes,
    -- | If the event is of type @RequestCancelActivityTaskFailed@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    requestCancelActivityTaskFailedEventAttributes :: Core.Maybe RequestCancelActivityTaskFailedEventAttributes,
    -- | If the event is of type @DecisionTaskFailed@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    recordMarkerFailedEventAttributes :: Core.Maybe RecordMarkerFailedEventAttributes,
    -- | If the event is of type @CancelWorkflowExecutionFailed@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    cancelWorkflowExecutionFailedEventAttributes :: Core.Maybe CancelWorkflowExecutionFailedEventAttributes,
    -- | Provides the details of the @LambdaFunctionScheduled@ event. It isn\'t
    -- set for other event types.
    lambdaFunctionScheduledEventAttributes :: Core.Maybe LambdaFunctionScheduledEventAttributes,
    -- | If the event is of type @DecisionTaskStarted@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    decisionTaskStartedEventAttributes :: Core.Maybe DecisionTaskStartedEventAttributes,
    -- | If the event is of type @WorkflowExecutionCancelRequested@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    workflowExecutionCancelRequestedEventAttributes :: Core.Maybe WorkflowExecutionCancelRequestedEventAttributes,
    -- | If the event is of type @WorkflowExecutionContinuedAsNew@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    workflowExecutionContinuedAsNewEventAttributes :: Core.Maybe WorkflowExecutionContinuedAsNewEventAttributes,
    -- | If the event is of type @DecisionTaskTimedOut@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    decisionTaskTimedOutEventAttributes :: Core.Maybe DecisionTaskTimedOutEventAttributes,
    -- | If the event is of type @WorkflowExecutionFailed@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionFailedEventAttributes :: Core.Maybe WorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @ActivityTaskCompleted@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    activityTaskCompletedEventAttributes :: Core.Maybe ActivityTaskCompletedEventAttributes,
    -- | If the event is of type @SignalExternalWorkflowExecutionInitiated@ then
    -- this member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    signalExternalWorkflowExecutionInitiatedEventAttributes :: Core.Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes,
    -- | If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    continueAsNewWorkflowExecutionFailedEventAttributes :: Core.Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @DecisionTaskCompleted@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    decisionTaskCompletedEventAttributes :: Core.Maybe DecisionTaskCompletedEventAttributes,
    -- | If the event is of type @RequestCancelExternalWorkflowExecutionFailed@
    -- then this member is set and provides detailed information about the
    -- event. It isn\'t set for other event types.
    requestCancelExternalWorkflowExecutionFailedEventAttributes :: Core.Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @FailWorkflowExecutionFailed@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    failWorkflowExecutionFailedEventAttributes :: Core.Maybe FailWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @WorkflowExecutionStarted@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionStartedEventAttributes :: Core.Maybe WorkflowExecutionStartedEventAttributes,
    -- | If the event is of type @StartChildWorkflowExecutionFailed@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    startChildWorkflowExecutionFailedEventAttributes :: Core.Maybe StartChildWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @ExternalWorkflowExecutionSignaled@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    externalWorkflowExecutionSignaledEventAttributes :: Core.Maybe ExternalWorkflowExecutionSignaledEventAttributes,
    -- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It
    -- isn\'t set for other event types.
    scheduleLambdaFunctionFailedEventAttributes :: Core.Maybe ScheduleLambdaFunctionFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionCompleted@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    childWorkflowExecutionCompletedEventAttributes :: Core.Maybe ChildWorkflowExecutionCompletedEventAttributes,
    -- | Provides the details of the @LambdaFunctionFailed@ event. It isn\'t set
    -- for other event types.
    lambdaFunctionFailedEventAttributes :: Core.Maybe LambdaFunctionFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionTerminated@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    childWorkflowExecutionTerminatedEventAttributes :: Core.Maybe ChildWorkflowExecutionTerminatedEventAttributes,
    -- | If the event is of type @TimerStarted@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    timerStartedEventAttributes :: Core.Maybe TimerStartedEventAttributes,
    -- | If the event is of type @ActivityTaskStarted@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    activityTaskStartedEventAttributes :: Core.Maybe ActivityTaskStartedEventAttributes,
    -- | If the event is of type @SignalExternalWorkflowExecutionFailed@ then
    -- this member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    signalExternalWorkflowExecutionFailedEventAttributes :: Core.Maybe SignalExternalWorkflowExecutionFailedEventAttributes,
    -- | The date and time when the event occurred.
    eventTimestamp :: Core.POSIX,
    -- | The type of the history event.
    eventType :: EventType,
    -- | The system generated ID of the event. This ID uniquely identifies the
    -- event with in the workflow execution history.
    eventId :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HistoryEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childWorkflowExecutionTimedOutEventAttributes', 'historyEvent_childWorkflowExecutionTimedOutEventAttributes' - If the event is of type @ChildWorkflowExecutionTimedOut@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes', 'historyEvent_requestCancelExternalWorkflowExecutionInitiatedEventAttributes' - If the event is of type
-- @RequestCancelExternalWorkflowExecutionInitiated@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'lambdaFunctionStartedEventAttributes', 'historyEvent_lambdaFunctionStartedEventAttributes' - Provides the details of the @LambdaFunctionStarted@ event. It isn\'t set
-- for other event types.
--
-- 'startChildWorkflowExecutionInitiatedEventAttributes', 'historyEvent_startChildWorkflowExecutionInitiatedEventAttributes' - If the event is of type @StartChildWorkflowExecutionInitiated@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'decisionTaskScheduledEventAttributes', 'historyEvent_decisionTaskScheduledEventAttributes' - If the event is of type @DecisionTaskScheduled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'childWorkflowExecutionCanceledEventAttributes', 'historyEvent_childWorkflowExecutionCanceledEventAttributes' - If the event is of type @ChildWorkflowExecutionCanceled@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'activityTaskCanceledEventAttributes', 'historyEvent_activityTaskCanceledEventAttributes' - If the event is of type @ActivityTaskCanceled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'activityTaskTimedOutEventAttributes', 'historyEvent_activityTaskTimedOutEventAttributes' - If the event is of type @ActivityTaskTimedOut@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'externalWorkflowExecutionCancelRequestedEventAttributes', 'historyEvent_externalWorkflowExecutionCancelRequestedEventAttributes' - If the event is of type @ExternalWorkflowExecutionCancelRequested@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'cancelTimerFailedEventAttributes', 'historyEvent_cancelTimerFailedEventAttributes' - If the event is of type @CancelTimerFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'childWorkflowExecutionStartedEventAttributes', 'historyEvent_childWorkflowExecutionStartedEventAttributes' - If the event is of type @ChildWorkflowExecutionStarted@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'lambdaFunctionTimedOutEventAttributes', 'historyEvent_lambdaFunctionTimedOutEventAttributes' - Provides the details of the @LambdaFunctionTimedOut@ event. It isn\'t
-- set for other event types.
--
-- 'childWorkflowExecutionFailedEventAttributes', 'historyEvent_childWorkflowExecutionFailedEventAttributes' - If the event is of type @ChildWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'timerCanceledEventAttributes', 'historyEvent_timerCanceledEventAttributes' - If the event is of type @TimerCanceled@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'lambdaFunctionCompletedEventAttributes', 'historyEvent_lambdaFunctionCompletedEventAttributes' - Provides the details of the @LambdaFunctionCompleted@ event. It isn\'t
-- set for other event types.
--
-- 'completeWorkflowExecutionFailedEventAttributes', 'historyEvent_completeWorkflowExecutionFailedEventAttributes' - If the event is of type @CompleteWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'activityTaskFailedEventAttributes', 'historyEvent_activityTaskFailedEventAttributes' - If the event is of type @ActivityTaskFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'timerFiredEventAttributes', 'historyEvent_timerFiredEventAttributes' - If the event is of type @TimerFired@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'workflowExecutionSignaledEventAttributes', 'historyEvent_workflowExecutionSignaledEventAttributes' - If the event is of type @WorkflowExecutionSignaled@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'activityTaskCancelRequestedEventAttributes', 'historyEvent_activityTaskCancelRequestedEventAttributes' - If the event is of type @ActivityTaskcancelRequested@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'workflowExecutionCanceledEventAttributes', 'historyEvent_workflowExecutionCanceledEventAttributes' - If the event is of type @WorkflowExecutionCanceled@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'startLambdaFunctionFailedEventAttributes', 'historyEvent_startLambdaFunctionFailedEventAttributes' - Provides the details of the @StartLambdaFunctionFailed@ event. It isn\'t
-- set for other event types.
--
-- 'scheduleActivityTaskFailedEventAttributes', 'historyEvent_scheduleActivityTaskFailedEventAttributes' - If the event is of type @ScheduleActivityTaskFailed@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'workflowExecutionTimedOutEventAttributes', 'historyEvent_workflowExecutionTimedOutEventAttributes' - If the event is of type @WorkflowExecutionTimedOut@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'markerRecordedEventAttributes', 'historyEvent_markerRecordedEventAttributes' - If the event is of type @MarkerRecorded@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'activityTaskScheduledEventAttributes', 'historyEvent_activityTaskScheduledEventAttributes' - If the event is of type @ActivityTaskScheduled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'startTimerFailedEventAttributes', 'historyEvent_startTimerFailedEventAttributes' - If the event is of type @StartTimerFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'workflowExecutionTerminatedEventAttributes', 'historyEvent_workflowExecutionTerminatedEventAttributes' - If the event is of type @WorkflowExecutionTerminated@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'workflowExecutionCompletedEventAttributes', 'historyEvent_workflowExecutionCompletedEventAttributes' - If the event is of type @WorkflowExecutionCompleted@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'requestCancelActivityTaskFailedEventAttributes', 'historyEvent_requestCancelActivityTaskFailedEventAttributes' - If the event is of type @RequestCancelActivityTaskFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'recordMarkerFailedEventAttributes', 'historyEvent_recordMarkerFailedEventAttributes' - If the event is of type @DecisionTaskFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'cancelWorkflowExecutionFailedEventAttributes', 'historyEvent_cancelWorkflowExecutionFailedEventAttributes' - If the event is of type @CancelWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'lambdaFunctionScheduledEventAttributes', 'historyEvent_lambdaFunctionScheduledEventAttributes' - Provides the details of the @LambdaFunctionScheduled@ event. It isn\'t
-- set for other event types.
--
-- 'decisionTaskStartedEventAttributes', 'historyEvent_decisionTaskStartedEventAttributes' - If the event is of type @DecisionTaskStarted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'workflowExecutionCancelRequestedEventAttributes', 'historyEvent_workflowExecutionCancelRequestedEventAttributes' - If the event is of type @WorkflowExecutionCancelRequested@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'workflowExecutionContinuedAsNewEventAttributes', 'historyEvent_workflowExecutionContinuedAsNewEventAttributes' - If the event is of type @WorkflowExecutionContinuedAsNew@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'decisionTaskTimedOutEventAttributes', 'historyEvent_decisionTaskTimedOutEventAttributes' - If the event is of type @DecisionTaskTimedOut@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'workflowExecutionFailedEventAttributes', 'historyEvent_workflowExecutionFailedEventAttributes' - If the event is of type @WorkflowExecutionFailed@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'activityTaskCompletedEventAttributes', 'historyEvent_activityTaskCompletedEventAttributes' - If the event is of type @ActivityTaskCompleted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'signalExternalWorkflowExecutionInitiatedEventAttributes', 'historyEvent_signalExternalWorkflowExecutionInitiatedEventAttributes' - If the event is of type @SignalExternalWorkflowExecutionInitiated@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'continueAsNewWorkflowExecutionFailedEventAttributes', 'historyEvent_continueAsNewWorkflowExecutionFailedEventAttributes' - If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'decisionTaskCompletedEventAttributes', 'historyEvent_decisionTaskCompletedEventAttributes' - If the event is of type @DecisionTaskCompleted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'requestCancelExternalWorkflowExecutionFailedEventAttributes', 'historyEvent_requestCancelExternalWorkflowExecutionFailedEventAttributes' - If the event is of type @RequestCancelExternalWorkflowExecutionFailed@
-- then this member is set and provides detailed information about the
-- event. It isn\'t set for other event types.
--
-- 'failWorkflowExecutionFailedEventAttributes', 'historyEvent_failWorkflowExecutionFailedEventAttributes' - If the event is of type @FailWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'workflowExecutionStartedEventAttributes', 'historyEvent_workflowExecutionStartedEventAttributes' - If the event is of type @WorkflowExecutionStarted@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'startChildWorkflowExecutionFailedEventAttributes', 'historyEvent_startChildWorkflowExecutionFailedEventAttributes' - If the event is of type @StartChildWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'externalWorkflowExecutionSignaledEventAttributes', 'historyEvent_externalWorkflowExecutionSignaledEventAttributes' - If the event is of type @ExternalWorkflowExecutionSignaled@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'scheduleLambdaFunctionFailedEventAttributes', 'historyEvent_scheduleLambdaFunctionFailedEventAttributes' - Provides the details of the @ScheduleLambdaFunctionFailed@ event. It
-- isn\'t set for other event types.
--
-- 'childWorkflowExecutionCompletedEventAttributes', 'historyEvent_childWorkflowExecutionCompletedEventAttributes' - If the event is of type @ChildWorkflowExecutionCompleted@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'lambdaFunctionFailedEventAttributes', 'historyEvent_lambdaFunctionFailedEventAttributes' - Provides the details of the @LambdaFunctionFailed@ event. It isn\'t set
-- for other event types.
--
-- 'childWorkflowExecutionTerminatedEventAttributes', 'historyEvent_childWorkflowExecutionTerminatedEventAttributes' - If the event is of type @ChildWorkflowExecutionTerminated@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'timerStartedEventAttributes', 'historyEvent_timerStartedEventAttributes' - If the event is of type @TimerStarted@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'activityTaskStartedEventAttributes', 'historyEvent_activityTaskStartedEventAttributes' - If the event is of type @ActivityTaskStarted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'signalExternalWorkflowExecutionFailedEventAttributes', 'historyEvent_signalExternalWorkflowExecutionFailedEventAttributes' - If the event is of type @SignalExternalWorkflowExecutionFailed@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'eventTimestamp', 'historyEvent_eventTimestamp' - The date and time when the event occurred.
--
-- 'eventType', 'historyEvent_eventType' - The type of the history event.
--
-- 'eventId', 'historyEvent_eventId' - The system generated ID of the event. This ID uniquely identifies the
-- event with in the workflow execution history.
newHistoryEvent ::
  -- | 'eventTimestamp'
  Core.UTCTime ->
  -- | 'eventType'
  EventType ->
  -- | 'eventId'
  Core.Integer ->
  HistoryEvent
newHistoryEvent
  pEventTimestamp_
  pEventType_
  pEventId_ =
    HistoryEvent'
      { childWorkflowExecutionTimedOutEventAttributes =
          Core.Nothing,
        requestCancelExternalWorkflowExecutionInitiatedEventAttributes =
          Core.Nothing,
        lambdaFunctionStartedEventAttributes = Core.Nothing,
        startChildWorkflowExecutionInitiatedEventAttributes =
          Core.Nothing,
        decisionTaskScheduledEventAttributes = Core.Nothing,
        childWorkflowExecutionCanceledEventAttributes =
          Core.Nothing,
        activityTaskCanceledEventAttributes = Core.Nothing,
        activityTaskTimedOutEventAttributes = Core.Nothing,
        externalWorkflowExecutionCancelRequestedEventAttributes =
          Core.Nothing,
        cancelTimerFailedEventAttributes = Core.Nothing,
        childWorkflowExecutionStartedEventAttributes =
          Core.Nothing,
        lambdaFunctionTimedOutEventAttributes = Core.Nothing,
        childWorkflowExecutionFailedEventAttributes =
          Core.Nothing,
        timerCanceledEventAttributes = Core.Nothing,
        lambdaFunctionCompletedEventAttributes =
          Core.Nothing,
        completeWorkflowExecutionFailedEventAttributes =
          Core.Nothing,
        activityTaskFailedEventAttributes = Core.Nothing,
        timerFiredEventAttributes = Core.Nothing,
        workflowExecutionSignaledEventAttributes =
          Core.Nothing,
        activityTaskCancelRequestedEventAttributes =
          Core.Nothing,
        workflowExecutionCanceledEventAttributes =
          Core.Nothing,
        startLambdaFunctionFailedEventAttributes =
          Core.Nothing,
        scheduleActivityTaskFailedEventAttributes =
          Core.Nothing,
        workflowExecutionTimedOutEventAttributes =
          Core.Nothing,
        markerRecordedEventAttributes = Core.Nothing,
        activityTaskScheduledEventAttributes = Core.Nothing,
        startTimerFailedEventAttributes = Core.Nothing,
        workflowExecutionTerminatedEventAttributes =
          Core.Nothing,
        workflowExecutionCompletedEventAttributes =
          Core.Nothing,
        requestCancelActivityTaskFailedEventAttributes =
          Core.Nothing,
        recordMarkerFailedEventAttributes = Core.Nothing,
        cancelWorkflowExecutionFailedEventAttributes =
          Core.Nothing,
        lambdaFunctionScheduledEventAttributes =
          Core.Nothing,
        decisionTaskStartedEventAttributes = Core.Nothing,
        workflowExecutionCancelRequestedEventAttributes =
          Core.Nothing,
        workflowExecutionContinuedAsNewEventAttributes =
          Core.Nothing,
        decisionTaskTimedOutEventAttributes = Core.Nothing,
        workflowExecutionFailedEventAttributes =
          Core.Nothing,
        activityTaskCompletedEventAttributes = Core.Nothing,
        signalExternalWorkflowExecutionInitiatedEventAttributes =
          Core.Nothing,
        continueAsNewWorkflowExecutionFailedEventAttributes =
          Core.Nothing,
        decisionTaskCompletedEventAttributes = Core.Nothing,
        requestCancelExternalWorkflowExecutionFailedEventAttributes =
          Core.Nothing,
        failWorkflowExecutionFailedEventAttributes =
          Core.Nothing,
        workflowExecutionStartedEventAttributes =
          Core.Nothing,
        startChildWorkflowExecutionFailedEventAttributes =
          Core.Nothing,
        externalWorkflowExecutionSignaledEventAttributes =
          Core.Nothing,
        scheduleLambdaFunctionFailedEventAttributes =
          Core.Nothing,
        childWorkflowExecutionCompletedEventAttributes =
          Core.Nothing,
        lambdaFunctionFailedEventAttributes = Core.Nothing,
        childWorkflowExecutionTerminatedEventAttributes =
          Core.Nothing,
        timerStartedEventAttributes = Core.Nothing,
        activityTaskStartedEventAttributes = Core.Nothing,
        signalExternalWorkflowExecutionFailedEventAttributes =
          Core.Nothing,
        eventTimestamp = Core._Time Lens.# pEventTimestamp_,
        eventType = pEventType_,
        eventId = pEventId_
      }

-- | If the event is of type @ChildWorkflowExecutionTimedOut@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_childWorkflowExecutionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ChildWorkflowExecutionTimedOutEventAttributes)
historyEvent_childWorkflowExecutionTimedOutEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionTimedOutEventAttributes} -> childWorkflowExecutionTimedOutEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionTimedOutEventAttributes = a} :: HistoryEvent)

-- | If the event is of type
-- @RequestCancelExternalWorkflowExecutionInitiated@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_requestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
historyEvent_requestCancelExternalWorkflowExecutionInitiatedEventAttributes = Lens.lens (\HistoryEvent' {requestCancelExternalWorkflowExecutionInitiatedEventAttributes} -> requestCancelExternalWorkflowExecutionInitiatedEventAttributes) (\s@HistoryEvent' {} a -> s {requestCancelExternalWorkflowExecutionInitiatedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @LambdaFunctionStarted@ event. It isn\'t set
-- for other event types.
historyEvent_lambdaFunctionStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionStartedEventAttributes)
historyEvent_lambdaFunctionStartedEventAttributes = Lens.lens (\HistoryEvent' {lambdaFunctionStartedEventAttributes} -> lambdaFunctionStartedEventAttributes) (\s@HistoryEvent' {} a -> s {lambdaFunctionStartedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @StartChildWorkflowExecutionInitiated@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_startChildWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe StartChildWorkflowExecutionInitiatedEventAttributes)
historyEvent_startChildWorkflowExecutionInitiatedEventAttributes = Lens.lens (\HistoryEvent' {startChildWorkflowExecutionInitiatedEventAttributes} -> startChildWorkflowExecutionInitiatedEventAttributes) (\s@HistoryEvent' {} a -> s {startChildWorkflowExecutionInitiatedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @DecisionTaskScheduled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_decisionTaskScheduledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe DecisionTaskScheduledEventAttributes)
historyEvent_decisionTaskScheduledEventAttributes = Lens.lens (\HistoryEvent' {decisionTaskScheduledEventAttributes} -> decisionTaskScheduledEventAttributes) (\s@HistoryEvent' {} a -> s {decisionTaskScheduledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionCanceled@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_childWorkflowExecutionCanceledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ChildWorkflowExecutionCanceledEventAttributes)
historyEvent_childWorkflowExecutionCanceledEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionCanceledEventAttributes} -> childWorkflowExecutionCanceledEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionCanceledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskCanceled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_activityTaskCanceledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ActivityTaskCanceledEventAttributes)
historyEvent_activityTaskCanceledEventAttributes = Lens.lens (\HistoryEvent' {activityTaskCanceledEventAttributes} -> activityTaskCanceledEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskCanceledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskTimedOut@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_activityTaskTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ActivityTaskTimedOutEventAttributes)
historyEvent_activityTaskTimedOutEventAttributes = Lens.lens (\HistoryEvent' {activityTaskTimedOutEventAttributes} -> activityTaskTimedOutEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskTimedOutEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ExternalWorkflowExecutionCancelRequested@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_externalWorkflowExecutionCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes)
historyEvent_externalWorkflowExecutionCancelRequestedEventAttributes = Lens.lens (\HistoryEvent' {externalWorkflowExecutionCancelRequestedEventAttributes} -> externalWorkflowExecutionCancelRequestedEventAttributes) (\s@HistoryEvent' {} a -> s {externalWorkflowExecutionCancelRequestedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @CancelTimerFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_cancelTimerFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe CancelTimerFailedEventAttributes)
historyEvent_cancelTimerFailedEventAttributes = Lens.lens (\HistoryEvent' {cancelTimerFailedEventAttributes} -> cancelTimerFailedEventAttributes) (\s@HistoryEvent' {} a -> s {cancelTimerFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionStarted@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_childWorkflowExecutionStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ChildWorkflowExecutionStartedEventAttributes)
historyEvent_childWorkflowExecutionStartedEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionStartedEventAttributes} -> childWorkflowExecutionStartedEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionStartedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @LambdaFunctionTimedOut@ event. It isn\'t
-- set for other event types.
historyEvent_lambdaFunctionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionTimedOutEventAttributes)
historyEvent_lambdaFunctionTimedOutEventAttributes = Lens.lens (\HistoryEvent' {lambdaFunctionTimedOutEventAttributes} -> lambdaFunctionTimedOutEventAttributes) (\s@HistoryEvent' {} a -> s {lambdaFunctionTimedOutEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_childWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ChildWorkflowExecutionFailedEventAttributes)
historyEvent_childWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionFailedEventAttributes} -> childWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @TimerCanceled@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_timerCanceledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe TimerCanceledEventAttributes)
historyEvent_timerCanceledEventAttributes = Lens.lens (\HistoryEvent' {timerCanceledEventAttributes} -> timerCanceledEventAttributes) (\s@HistoryEvent' {} a -> s {timerCanceledEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @LambdaFunctionCompleted@ event. It isn\'t
-- set for other event types.
historyEvent_lambdaFunctionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionCompletedEventAttributes)
historyEvent_lambdaFunctionCompletedEventAttributes = Lens.lens (\HistoryEvent' {lambdaFunctionCompletedEventAttributes} -> lambdaFunctionCompletedEventAttributes) (\s@HistoryEvent' {} a -> s {lambdaFunctionCompletedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @CompleteWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_completeWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe CompleteWorkflowExecutionFailedEventAttributes)
historyEvent_completeWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {completeWorkflowExecutionFailedEventAttributes} -> completeWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {completeWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_activityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ActivityTaskFailedEventAttributes)
historyEvent_activityTaskFailedEventAttributes = Lens.lens (\HistoryEvent' {activityTaskFailedEventAttributes} -> activityTaskFailedEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @TimerFired@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_timerFiredEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe TimerFiredEventAttributes)
historyEvent_timerFiredEventAttributes = Lens.lens (\HistoryEvent' {timerFiredEventAttributes} -> timerFiredEventAttributes) (\s@HistoryEvent' {} a -> s {timerFiredEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionSignaled@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionSignaledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe WorkflowExecutionSignaledEventAttributes)
historyEvent_workflowExecutionSignaledEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionSignaledEventAttributes} -> workflowExecutionSignaledEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionSignaledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskcancelRequested@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_activityTaskCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ActivityTaskCancelRequestedEventAttributes)
historyEvent_activityTaskCancelRequestedEventAttributes = Lens.lens (\HistoryEvent' {activityTaskCancelRequestedEventAttributes} -> activityTaskCancelRequestedEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskCancelRequestedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionCanceled@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionCanceledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe WorkflowExecutionCanceledEventAttributes)
historyEvent_workflowExecutionCanceledEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionCanceledEventAttributes} -> workflowExecutionCanceledEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionCanceledEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn\'t
-- set for other event types.
historyEvent_startLambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe StartLambdaFunctionFailedEventAttributes)
historyEvent_startLambdaFunctionFailedEventAttributes = Lens.lens (\HistoryEvent' {startLambdaFunctionFailedEventAttributes} -> startLambdaFunctionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {startLambdaFunctionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ScheduleActivityTaskFailed@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_scheduleActivityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ScheduleActivityTaskFailedEventAttributes)
historyEvent_scheduleActivityTaskFailedEventAttributes = Lens.lens (\HistoryEvent' {scheduleActivityTaskFailedEventAttributes} -> scheduleActivityTaskFailedEventAttributes) (\s@HistoryEvent' {} a -> s {scheduleActivityTaskFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionTimedOut@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe WorkflowExecutionTimedOutEventAttributes)
historyEvent_workflowExecutionTimedOutEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionTimedOutEventAttributes} -> workflowExecutionTimedOutEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionTimedOutEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @MarkerRecorded@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_markerRecordedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe MarkerRecordedEventAttributes)
historyEvent_markerRecordedEventAttributes = Lens.lens (\HistoryEvent' {markerRecordedEventAttributes} -> markerRecordedEventAttributes) (\s@HistoryEvent' {} a -> s {markerRecordedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskScheduled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_activityTaskScheduledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ActivityTaskScheduledEventAttributes)
historyEvent_activityTaskScheduledEventAttributes = Lens.lens (\HistoryEvent' {activityTaskScheduledEventAttributes} -> activityTaskScheduledEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskScheduledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @StartTimerFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_startTimerFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe StartTimerFailedEventAttributes)
historyEvent_startTimerFailedEventAttributes = Lens.lens (\HistoryEvent' {startTimerFailedEventAttributes} -> startTimerFailedEventAttributes) (\s@HistoryEvent' {} a -> s {startTimerFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionTerminated@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_workflowExecutionTerminatedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe WorkflowExecutionTerminatedEventAttributes)
historyEvent_workflowExecutionTerminatedEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionTerminatedEventAttributes} -> workflowExecutionTerminatedEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionTerminatedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionCompleted@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe WorkflowExecutionCompletedEventAttributes)
historyEvent_workflowExecutionCompletedEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionCompletedEventAttributes} -> workflowExecutionCompletedEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionCompletedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @RequestCancelActivityTaskFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_requestCancelActivityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe RequestCancelActivityTaskFailedEventAttributes)
historyEvent_requestCancelActivityTaskFailedEventAttributes = Lens.lens (\HistoryEvent' {requestCancelActivityTaskFailedEventAttributes} -> requestCancelActivityTaskFailedEventAttributes) (\s@HistoryEvent' {} a -> s {requestCancelActivityTaskFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @DecisionTaskFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_recordMarkerFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe RecordMarkerFailedEventAttributes)
historyEvent_recordMarkerFailedEventAttributes = Lens.lens (\HistoryEvent' {recordMarkerFailedEventAttributes} -> recordMarkerFailedEventAttributes) (\s@HistoryEvent' {} a -> s {recordMarkerFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @CancelWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_cancelWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe CancelWorkflowExecutionFailedEventAttributes)
historyEvent_cancelWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {cancelWorkflowExecutionFailedEventAttributes} -> cancelWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {cancelWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @LambdaFunctionScheduled@ event. It isn\'t
-- set for other event types.
historyEvent_lambdaFunctionScheduledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionScheduledEventAttributes)
historyEvent_lambdaFunctionScheduledEventAttributes = Lens.lens (\HistoryEvent' {lambdaFunctionScheduledEventAttributes} -> lambdaFunctionScheduledEventAttributes) (\s@HistoryEvent' {} a -> s {lambdaFunctionScheduledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @DecisionTaskStarted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_decisionTaskStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe DecisionTaskStartedEventAttributes)
historyEvent_decisionTaskStartedEventAttributes = Lens.lens (\HistoryEvent' {decisionTaskStartedEventAttributes} -> decisionTaskStartedEventAttributes) (\s@HistoryEvent' {} a -> s {decisionTaskStartedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionCancelRequested@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_workflowExecutionCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe WorkflowExecutionCancelRequestedEventAttributes)
historyEvent_workflowExecutionCancelRequestedEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionCancelRequestedEventAttributes} -> workflowExecutionCancelRequestedEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionCancelRequestedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionContinuedAsNew@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_workflowExecutionContinuedAsNewEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe WorkflowExecutionContinuedAsNewEventAttributes)
historyEvent_workflowExecutionContinuedAsNewEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionContinuedAsNewEventAttributes} -> workflowExecutionContinuedAsNewEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionContinuedAsNewEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @DecisionTaskTimedOut@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_decisionTaskTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe DecisionTaskTimedOutEventAttributes)
historyEvent_decisionTaskTimedOutEventAttributes = Lens.lens (\HistoryEvent' {decisionTaskTimedOutEventAttributes} -> decisionTaskTimedOutEventAttributes) (\s@HistoryEvent' {} a -> s {decisionTaskTimedOutEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionFailed@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe WorkflowExecutionFailedEventAttributes)
historyEvent_workflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionFailedEventAttributes} -> workflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskCompleted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_activityTaskCompletedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ActivityTaskCompletedEventAttributes)
historyEvent_activityTaskCompletedEventAttributes = Lens.lens (\HistoryEvent' {activityTaskCompletedEventAttributes} -> activityTaskCompletedEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskCompletedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @SignalExternalWorkflowExecutionInitiated@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_signalExternalWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes)
historyEvent_signalExternalWorkflowExecutionInitiatedEventAttributes = Lens.lens (\HistoryEvent' {signalExternalWorkflowExecutionInitiatedEventAttributes} -> signalExternalWorkflowExecutionInitiatedEventAttributes) (\s@HistoryEvent' {} a -> s {signalExternalWorkflowExecutionInitiatedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_continueAsNewWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes)
historyEvent_continueAsNewWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {continueAsNewWorkflowExecutionFailedEventAttributes} -> continueAsNewWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {continueAsNewWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @DecisionTaskCompleted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_decisionTaskCompletedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe DecisionTaskCompletedEventAttributes)
historyEvent_decisionTaskCompletedEventAttributes = Lens.lens (\HistoryEvent' {decisionTaskCompletedEventAttributes} -> decisionTaskCompletedEventAttributes) (\s@HistoryEvent' {} a -> s {decisionTaskCompletedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @RequestCancelExternalWorkflowExecutionFailed@
-- then this member is set and provides detailed information about the
-- event. It isn\'t set for other event types.
historyEvent_requestCancelExternalWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes)
historyEvent_requestCancelExternalWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {requestCancelExternalWorkflowExecutionFailedEventAttributes} -> requestCancelExternalWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {requestCancelExternalWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @FailWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_failWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe FailWorkflowExecutionFailedEventAttributes)
historyEvent_failWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {failWorkflowExecutionFailedEventAttributes} -> failWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {failWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionStarted@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe WorkflowExecutionStartedEventAttributes)
historyEvent_workflowExecutionStartedEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionStartedEventAttributes} -> workflowExecutionStartedEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionStartedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @StartChildWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_startChildWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe StartChildWorkflowExecutionFailedEventAttributes)
historyEvent_startChildWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {startChildWorkflowExecutionFailedEventAttributes} -> startChildWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {startChildWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ExternalWorkflowExecutionSignaled@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_externalWorkflowExecutionSignaledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ExternalWorkflowExecutionSignaledEventAttributes)
historyEvent_externalWorkflowExecutionSignaledEventAttributes = Lens.lens (\HistoryEvent' {externalWorkflowExecutionSignaledEventAttributes} -> externalWorkflowExecutionSignaledEventAttributes) (\s@HistoryEvent' {} a -> s {externalWorkflowExecutionSignaledEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It
-- isn\'t set for other event types.
historyEvent_scheduleLambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ScheduleLambdaFunctionFailedEventAttributes)
historyEvent_scheduleLambdaFunctionFailedEventAttributes = Lens.lens (\HistoryEvent' {scheduleLambdaFunctionFailedEventAttributes} -> scheduleLambdaFunctionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {scheduleLambdaFunctionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionCompleted@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_childWorkflowExecutionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ChildWorkflowExecutionCompletedEventAttributes)
historyEvent_childWorkflowExecutionCompletedEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionCompletedEventAttributes} -> childWorkflowExecutionCompletedEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionCompletedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @LambdaFunctionFailed@ event. It isn\'t set
-- for other event types.
historyEvent_lambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionFailedEventAttributes)
historyEvent_lambdaFunctionFailedEventAttributes = Lens.lens (\HistoryEvent' {lambdaFunctionFailedEventAttributes} -> lambdaFunctionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {lambdaFunctionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionTerminated@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_childWorkflowExecutionTerminatedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ChildWorkflowExecutionTerminatedEventAttributes)
historyEvent_childWorkflowExecutionTerminatedEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionTerminatedEventAttributes} -> childWorkflowExecutionTerminatedEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionTerminatedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @TimerStarted@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_timerStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe TimerStartedEventAttributes)
historyEvent_timerStartedEventAttributes = Lens.lens (\HistoryEvent' {timerStartedEventAttributes} -> timerStartedEventAttributes) (\s@HistoryEvent' {} a -> s {timerStartedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskStarted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_activityTaskStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe ActivityTaskStartedEventAttributes)
historyEvent_activityTaskStartedEventAttributes = Lens.lens (\HistoryEvent' {activityTaskStartedEventAttributes} -> activityTaskStartedEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskStartedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @SignalExternalWorkflowExecutionFailed@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_signalExternalWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe SignalExternalWorkflowExecutionFailedEventAttributes)
historyEvent_signalExternalWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {signalExternalWorkflowExecutionFailedEventAttributes} -> signalExternalWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {signalExternalWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | The date and time when the event occurred.
historyEvent_eventTimestamp :: Lens.Lens' HistoryEvent Core.UTCTime
historyEvent_eventTimestamp = Lens.lens (\HistoryEvent' {eventTimestamp} -> eventTimestamp) (\s@HistoryEvent' {} a -> s {eventTimestamp = a} :: HistoryEvent) Core.. Core._Time

-- | The type of the history event.
historyEvent_eventType :: Lens.Lens' HistoryEvent EventType
historyEvent_eventType = Lens.lens (\HistoryEvent' {eventType} -> eventType) (\s@HistoryEvent' {} a -> s {eventType = a} :: HistoryEvent)

-- | The system generated ID of the event. This ID uniquely identifies the
-- event with in the workflow execution history.
historyEvent_eventId :: Lens.Lens' HistoryEvent Core.Integer
historyEvent_eventId = Lens.lens (\HistoryEvent' {eventId} -> eventId) (\s@HistoryEvent' {} a -> s {eventId = a} :: HistoryEvent)

instance Core.FromJSON HistoryEvent where
  parseJSON =
    Core.withObject
      "HistoryEvent"
      ( \x ->
          HistoryEvent'
            Core.<$> ( x
                         Core..:? "childWorkflowExecutionTimedOutEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "requestCancelExternalWorkflowExecutionInitiatedEventAttributes"
                     )
            Core.<*> (x Core..:? "lambdaFunctionStartedEventAttributes")
            Core.<*> ( x
                         Core..:? "startChildWorkflowExecutionInitiatedEventAttributes"
                     )
            Core.<*> (x Core..:? "decisionTaskScheduledEventAttributes")
            Core.<*> ( x
                         Core..:? "childWorkflowExecutionCanceledEventAttributes"
                     )
            Core.<*> (x Core..:? "activityTaskCanceledEventAttributes")
            Core.<*> (x Core..:? "activityTaskTimedOutEventAttributes")
            Core.<*> ( x
                         Core..:? "externalWorkflowExecutionCancelRequestedEventAttributes"
                     )
            Core.<*> (x Core..:? "cancelTimerFailedEventAttributes")
            Core.<*> ( x
                         Core..:? "childWorkflowExecutionStartedEventAttributes"
                     )
            Core.<*> (x Core..:? "lambdaFunctionTimedOutEventAttributes")
            Core.<*> ( x
                         Core..:? "childWorkflowExecutionFailedEventAttributes"
                     )
            Core.<*> (x Core..:? "timerCanceledEventAttributes")
            Core.<*> (x Core..:? "lambdaFunctionCompletedEventAttributes")
            Core.<*> ( x
                         Core..:? "completeWorkflowExecutionFailedEventAttributes"
                     )
            Core.<*> (x Core..:? "activityTaskFailedEventAttributes")
            Core.<*> (x Core..:? "timerFiredEventAttributes")
            Core.<*> ( x
                         Core..:? "workflowExecutionSignaledEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "activityTaskCancelRequestedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "workflowExecutionCanceledEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "startLambdaFunctionFailedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "scheduleActivityTaskFailedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "workflowExecutionTimedOutEventAttributes"
                     )
            Core.<*> (x Core..:? "markerRecordedEventAttributes")
            Core.<*> (x Core..:? "activityTaskScheduledEventAttributes")
            Core.<*> (x Core..:? "startTimerFailedEventAttributes")
            Core.<*> ( x
                         Core..:? "workflowExecutionTerminatedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "workflowExecutionCompletedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "requestCancelActivityTaskFailedEventAttributes"
                     )
            Core.<*> (x Core..:? "recordMarkerFailedEventAttributes")
            Core.<*> ( x
                         Core..:? "cancelWorkflowExecutionFailedEventAttributes"
                     )
            Core.<*> (x Core..:? "lambdaFunctionScheduledEventAttributes")
            Core.<*> (x Core..:? "decisionTaskStartedEventAttributes")
            Core.<*> ( x
                         Core..:? "workflowExecutionCancelRequestedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "workflowExecutionContinuedAsNewEventAttributes"
                     )
            Core.<*> (x Core..:? "decisionTaskTimedOutEventAttributes")
            Core.<*> (x Core..:? "workflowExecutionFailedEventAttributes")
            Core.<*> (x Core..:? "activityTaskCompletedEventAttributes")
            Core.<*> ( x
                         Core..:? "signalExternalWorkflowExecutionInitiatedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "continueAsNewWorkflowExecutionFailedEventAttributes"
                     )
            Core.<*> (x Core..:? "decisionTaskCompletedEventAttributes")
            Core.<*> ( x
                         Core..:? "requestCancelExternalWorkflowExecutionFailedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "failWorkflowExecutionFailedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "workflowExecutionStartedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "startChildWorkflowExecutionFailedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "externalWorkflowExecutionSignaledEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "scheduleLambdaFunctionFailedEventAttributes"
                     )
            Core.<*> ( x
                         Core..:? "childWorkflowExecutionCompletedEventAttributes"
                     )
            Core.<*> (x Core..:? "lambdaFunctionFailedEventAttributes")
            Core.<*> ( x
                         Core..:? "childWorkflowExecutionTerminatedEventAttributes"
                     )
            Core.<*> (x Core..:? "timerStartedEventAttributes")
            Core.<*> (x Core..:? "activityTaskStartedEventAttributes")
            Core.<*> ( x
                         Core..:? "signalExternalWorkflowExecutionFailedEventAttributes"
                     )
            Core.<*> (x Core..: "eventTimestamp")
            Core.<*> (x Core..: "eventType")
            Core.<*> (x Core..: "eventId")
      )

instance Core.Hashable HistoryEvent

instance Core.NFData HistoryEvent

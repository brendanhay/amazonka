{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.HistoryEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.HistoryEvent
  ( HistoryEvent (..),

    -- * Smart constructor
    mkHistoryEvent,

    -- * Lenses
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

-- | Event within a workflow execution. A history event can be one of these types:
--
--
--     * @ActivityTaskCancelRequested@ – A @RequestCancelActivityTask@ decision was received by the system.
--
--
--     * @ActivityTaskCanceled@ – The activity task was successfully canceled.
--
--
--     * @ActivityTaskCompleted@ – An activity worker successfully completed an activity task by calling 'RespondActivityTaskCompleted' .
--
--
--     * @ActivityTaskFailed@ – An activity worker failed an activity task by calling 'RespondActivityTaskFailed' .
--
--
--     * @ActivityTaskScheduled@ – An activity task was scheduled for execution.
--
--
--     * @ActivityTaskStarted@ – The scheduled activity task was dispatched to a worker.
--
--
--     * @ActivityTaskTimedOut@ – The activity task timed out.
--
--
--     * @CancelTimerFailed@ – Failed to process CancelTimer decision. This happens when the decision isn't configured properly, for example no timer exists with the specified timer Id.
--
--
--     * @CancelWorkflowExecutionFailed@ – A request to cancel a workflow execution failed.
--
--
--     * @ChildWorkflowExecutionCanceled@ – A child workflow execution, started by this workflow execution, was canceled and closed.
--
--
--     * @ChildWorkflowExecutionCompleted@ – A child workflow execution, started by this workflow execution, completed successfully and was closed.
--
--
--     * @ChildWorkflowExecutionFailed@ – A child workflow execution, started by this workflow execution, failed to complete successfully and was closed.
--
--
--     * @ChildWorkflowExecutionStarted@ – A child workflow execution was successfully started.
--
--
--     * @ChildWorkflowExecutionTerminated@ – A child workflow execution, started by this workflow execution, was terminated.
--
--
--     * @ChildWorkflowExecutionTimedOut@ – A child workflow execution, started by this workflow execution, timed out and was closed.
--
--
--     * @CompleteWorkflowExecutionFailed@ – The workflow execution failed to complete.
--
--
--     * @ContinueAsNewWorkflowExecutionFailed@ – The workflow execution failed to complete after being continued as a new workflow execution.
--
--
--     * @DecisionTaskCompleted@ – The decider successfully completed a decision task by calling 'RespondDecisionTaskCompleted' .
--
--
--     * @DecisionTaskScheduled@ – A decision task was scheduled for the workflow execution.
--
--
--     * @DecisionTaskStarted@ – The decision task was dispatched to a decider.
--
--
--     * @DecisionTaskTimedOut@ – The decision task timed out.
--
--
--     * @ExternalWorkflowExecutionCancelRequested@ – Request to cancel an external workflow execution was successfully delivered to the target execution.
--
--
--     * @ExternalWorkflowExecutionSignaled@ – A signal, requested by this workflow execution, was successfully delivered to the target external workflow execution.
--
--
--     * @FailWorkflowExecutionFailed@ – A request to mark a workflow execution as failed, itself failed.
--
--
--     * @MarkerRecorded@ – A marker was recorded in the workflow history as the result of a @RecordMarker@ decision.
--
--
--     * @RecordMarkerFailed@ – A @RecordMarker@ decision was returned as failed.
--
--
--     * @RequestCancelActivityTaskFailed@ – Failed to process RequestCancelActivityTask decision. This happens when the decision isn't configured properly.
--
--
--     * @RequestCancelExternalWorkflowExecutionFailed@ – Request to cancel an external workflow execution failed.
--
--
--     * @RequestCancelExternalWorkflowExecutionInitiated@ – A request was made to request the cancellation of an external workflow execution.
--
--
--     * @ScheduleActivityTaskFailed@ – Failed to process ScheduleActivityTask decision. This happens when the decision isn't configured properly, for example the activity type specified isn't registered.
--
--
--     * @SignalExternalWorkflowExecutionFailed@ – The request to signal an external workflow execution failed.
--
--
--     * @SignalExternalWorkflowExecutionInitiated@ – A request to signal an external workflow was made.
--
--
--     * @StartActivityTaskFailed@ – A scheduled activity task failed to start.
--
--
--     * @StartChildWorkflowExecutionFailed@ – Failed to process StartChildWorkflowExecution decision. This happens when the decision isn't configured properly, for example the workflow type specified isn't registered.
--
--
--     * @StartChildWorkflowExecutionInitiated@ – A request was made to start a child workflow execution.
--
--
--     * @StartTimerFailed@ – Failed to process StartTimer decision. This happens when the decision isn't configured properly, for example a timer already exists with the specified timer Id.
--
--
--     * @TimerCanceled@ – A timer, previously started for this workflow execution, was successfully canceled.
--
--
--     * @TimerFired@ – A timer, previously started for this workflow execution, fired.
--
--
--     * @TimerStarted@ – A timer was started for the workflow execution due to a @StartTimer@ decision.
--
--
--     * @WorkflowExecutionCancelRequested@ – A request to cancel this workflow execution was made.
--
--
--     * @WorkflowExecutionCanceled@ – The workflow execution was successfully canceled and closed.
--
--
--     * @WorkflowExecutionCompleted@ – The workflow execution was closed due to successful completion.
--
--
--     * @WorkflowExecutionContinuedAsNew@ – The workflow execution was closed and a new execution of the same type was created with the same workflowId.
--
--
--     * @WorkflowExecutionFailed@ – The workflow execution closed due to a failure.
--
--
--     * @WorkflowExecutionSignaled@ – An external signal was received for the workflow execution.
--
--
--     * @WorkflowExecutionStarted@ – The workflow execution was started.
--
--
--     * @WorkflowExecutionTerminated@ – The workflow execution was terminated.
--
--
--     * @WorkflowExecutionTimedOut@ – The workflow execution was closed because a time out was exceeded.
--
--
--
-- /See:/ 'mkHistoryEvent' smart constructor.
data HistoryEvent = HistoryEvent'
  { -- | If the event is of type @WorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    workflowExecutionCancelRequestedEventAttributes :: Lude.Maybe WorkflowExecutionCancelRequestedEventAttributes,
    -- | If the event is of type @DecisionTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    recordMarkerFailedEventAttributes :: Lude.Maybe RecordMarkerFailedEventAttributes,
    -- | If the event is of type @RequestCancelExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Lude.Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    -- | Provides the details of the @LambdaFunctionStarted@ event. It isn't set for other event types.
    lambdaFunctionStartedEventAttributes :: Lude.Maybe LambdaFunctionStartedEventAttributes,
    -- | If the event is of type @DecisionTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    decisionTaskScheduledEventAttributes :: Lude.Maybe DecisionTaskScheduledEventAttributes,
    -- | If the event is of type @WorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    workflowExecutionCompletedEventAttributes :: Lude.Maybe WorkflowExecutionCompletedEventAttributes,
    -- | If the event is of type @StartTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    startTimerFailedEventAttributes :: Lude.Maybe StartTimerFailedEventAttributes,
    -- | The date and time when the event occurred.
    eventTimestamp :: Lude.Timestamp,
    -- | If the event is of type @ActivityTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    activityTaskScheduledEventAttributes :: Lude.Maybe ActivityTaskScheduledEventAttributes,
    -- | If the event is of type @ScheduleActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    scheduleActivityTaskFailedEventAttributes :: Lude.Maybe ScheduleActivityTaskFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    childWorkflowExecutionCompletedEventAttributes :: Lude.Maybe ChildWorkflowExecutionCompletedEventAttributes,
    -- | If the event is of type @MarkerRecorded@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    markerRecordedEventAttributes :: Lude.Maybe MarkerRecordedEventAttributes,
    -- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It isn't set for other event types.
    scheduleLambdaFunctionFailedEventAttributes :: Lude.Maybe ScheduleLambdaFunctionFailedEventAttributes,
    -- | If the event is of type @CompleteWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    completeWorkflowExecutionFailedEventAttributes :: Lude.Maybe CompleteWorkflowExecutionFailedEventAttributes,
    -- | Provides the details of the @LambdaFunctionCompleted@ event. It isn't set for other event types.
    lambdaFunctionCompletedEventAttributes :: Lude.Maybe LambdaFunctionCompletedEventAttributes,
    -- | If the event is of type @RequestCancelExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    requestCancelExternalWorkflowExecutionFailedEventAttributes :: Lude.Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @TimerCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    timerCanceledEventAttributes :: Lude.Maybe TimerCanceledEventAttributes,
    -- | If the event is of type @WorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    workflowExecutionStartedEventAttributes :: Lude.Maybe WorkflowExecutionStartedEventAttributes,
    -- | If the event is of type @ActivityTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    activityTaskCompletedEventAttributes :: Lude.Maybe ActivityTaskCompletedEventAttributes,
    -- | If the event is of type @DecisionTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    decisionTaskTimedOutEventAttributes :: Lude.Maybe DecisionTaskTimedOutEventAttributes,
    -- | If the event is of type @CancelTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    cancelTimerFailedEventAttributes :: Lude.Maybe CancelTimerFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    childWorkflowExecutionStartedEventAttributes :: Lude.Maybe ChildWorkflowExecutionStartedEventAttributes,
    -- | The type of the history event.
    eventType :: EventType,
    -- | If the event is of type @ActivityTaskCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    activityTaskCanceledEventAttributes :: Lude.Maybe ActivityTaskCanceledEventAttributes,
    -- | If the event is of type @ActivityTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    activityTaskTimedOutEventAttributes :: Lude.Maybe ActivityTaskTimedOutEventAttributes,
    -- | If the event is of type @DecisionTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    decisionTaskStartedEventAttributes :: Lude.Maybe DecisionTaskStartedEventAttributes,
    -- | If the event is of type @WorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    workflowExecutionTerminatedEventAttributes :: Lude.Maybe WorkflowExecutionTerminatedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    childWorkflowExecutionCanceledEventAttributes :: Lude.Maybe ChildWorkflowExecutionCanceledEventAttributes,
    -- | If the event is of type @RequestCancelActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    requestCancelActivityTaskFailedEventAttributes :: Lude.Maybe RequestCancelActivityTaskFailedEventAttributes,
    -- | Provides the details of the @LambdaFunctionScheduled@ event. It isn't set for other event types.
    lambdaFunctionScheduledEventAttributes :: Lude.Maybe LambdaFunctionScheduledEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    childWorkflowExecutionTimedOutEventAttributes :: Lude.Maybe ChildWorkflowExecutionTimedOutEventAttributes,
    -- | If the event is of type @CancelWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    cancelWorkflowExecutionFailedEventAttributes :: Lude.Maybe CancelWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @StartChildWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    startChildWorkflowExecutionInitiatedEventAttributes :: Lude.Maybe StartChildWorkflowExecutionInitiatedEventAttributes,
    -- | If the event is of type @SignalExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    signalExternalWorkflowExecutionFailedEventAttributes :: Lude.Maybe SignalExternalWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @ActivityTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    activityTaskStartedEventAttributes :: Lude.Maybe ActivityTaskStartedEventAttributes,
    -- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn't set for other event types.
    startLambdaFunctionFailedEventAttributes :: Lude.Maybe StartLambdaFunctionFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    childWorkflowExecutionTerminatedEventAttributes :: Lude.Maybe ChildWorkflowExecutionTerminatedEventAttributes,
    -- | Provides the details of the @LambdaFunctionFailed@ event. It isn't set for other event types.
    lambdaFunctionFailedEventAttributes :: Lude.Maybe LambdaFunctionFailedEventAttributes,
    -- | If the event is of type @WorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    workflowExecutionCanceledEventAttributes :: Lude.Maybe WorkflowExecutionCanceledEventAttributes,
    -- | If the event is of type @TimerStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    timerStartedEventAttributes :: Lude.Maybe TimerStartedEventAttributes,
    -- | If the event is of type @ActivityTaskcancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    activityTaskCancelRequestedEventAttributes :: Lude.Maybe ActivityTaskCancelRequestedEventAttributes,
    -- | If the event is of type @WorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    workflowExecutionTimedOutEventAttributes :: Lude.Maybe WorkflowExecutionTimedOutEventAttributes,
    -- | If the event is of type @WorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    workflowExecutionSignaledEventAttributes :: Lude.Maybe WorkflowExecutionSignaledEventAttributes,
    -- | If the event is of type @TimerFired@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    timerFiredEventAttributes :: Lude.Maybe TimerFiredEventAttributes,
    -- | If the event is of type @ActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    activityTaskFailedEventAttributes :: Lude.Maybe ActivityTaskFailedEventAttributes,
    -- | If the event is of type @ExternalWorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    externalWorkflowExecutionSignaledEventAttributes :: Lude.Maybe ExternalWorkflowExecutionSignaledEventAttributes,
    -- | If the event is of type @DecisionTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    decisionTaskCompletedEventAttributes :: Lude.Maybe DecisionTaskCompletedEventAttributes,
    -- | If the event is of type @StartChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    startChildWorkflowExecutionFailedEventAttributes :: Lude.Maybe StartChildWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    childWorkflowExecutionFailedEventAttributes :: Lude.Maybe ChildWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @FailWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    failWorkflowExecutionFailedEventAttributes :: Lude.Maybe FailWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    continueAsNewWorkflowExecutionFailedEventAttributes :: Lude.Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @SignalExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    signalExternalWorkflowExecutionInitiatedEventAttributes :: Lude.Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes,
    -- | Provides the details of the @LambdaFunctionTimedOut@ event. It isn't set for other event types.
    lambdaFunctionTimedOutEventAttributes :: Lude.Maybe LambdaFunctionTimedOutEventAttributes,
    -- | The system generated ID of the event. This ID uniquely identifies the event with in the workflow execution history.
    eventId :: Lude.Integer,
    -- | If the event is of type @WorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    workflowExecutionFailedEventAttributes :: Lude.Maybe WorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @WorkflowExecutionContinuedAsNew@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    workflowExecutionContinuedAsNewEventAttributes :: Lude.Maybe WorkflowExecutionContinuedAsNewEventAttributes,
    -- | If the event is of type @ExternalWorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
    externalWorkflowExecutionCancelRequestedEventAttributes :: Lude.Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HistoryEvent' with the minimum fields required to make a request.
--
-- * 'workflowExecutionCancelRequestedEventAttributes' - If the event is of type @WorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'recordMarkerFailedEventAttributes' - If the event is of type @DecisionTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes' - If the event is of type @RequestCancelExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'lambdaFunctionStartedEventAttributes' - Provides the details of the @LambdaFunctionStarted@ event. It isn't set for other event types.
-- * 'decisionTaskScheduledEventAttributes' - If the event is of type @DecisionTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'workflowExecutionCompletedEventAttributes' - If the event is of type @WorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'startTimerFailedEventAttributes' - If the event is of type @StartTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'eventTimestamp' - The date and time when the event occurred.
-- * 'activityTaskScheduledEventAttributes' - If the event is of type @ActivityTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'scheduleActivityTaskFailedEventAttributes' - If the event is of type @ScheduleActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'childWorkflowExecutionCompletedEventAttributes' - If the event is of type @ChildWorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'markerRecordedEventAttributes' - If the event is of type @MarkerRecorded@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'scheduleLambdaFunctionFailedEventAttributes' - Provides the details of the @ScheduleLambdaFunctionFailed@ event. It isn't set for other event types.
-- * 'completeWorkflowExecutionFailedEventAttributes' - If the event is of type @CompleteWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'lambdaFunctionCompletedEventAttributes' - Provides the details of the @LambdaFunctionCompleted@ event. It isn't set for other event types.
-- * 'requestCancelExternalWorkflowExecutionFailedEventAttributes' - If the event is of type @RequestCancelExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'timerCanceledEventAttributes' - If the event is of type @TimerCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'workflowExecutionStartedEventAttributes' - If the event is of type @WorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'activityTaskCompletedEventAttributes' - If the event is of type @ActivityTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'decisionTaskTimedOutEventAttributes' - If the event is of type @DecisionTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'cancelTimerFailedEventAttributes' - If the event is of type @CancelTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'childWorkflowExecutionStartedEventAttributes' - If the event is of type @ChildWorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'eventType' - The type of the history event.
-- * 'activityTaskCanceledEventAttributes' - If the event is of type @ActivityTaskCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'activityTaskTimedOutEventAttributes' - If the event is of type @ActivityTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'decisionTaskStartedEventAttributes' - If the event is of type @DecisionTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'workflowExecutionTerminatedEventAttributes' - If the event is of type @WorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'childWorkflowExecutionCanceledEventAttributes' - If the event is of type @ChildWorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'requestCancelActivityTaskFailedEventAttributes' - If the event is of type @RequestCancelActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'lambdaFunctionScheduledEventAttributes' - Provides the details of the @LambdaFunctionScheduled@ event. It isn't set for other event types.
-- * 'childWorkflowExecutionTimedOutEventAttributes' - If the event is of type @ChildWorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'cancelWorkflowExecutionFailedEventAttributes' - If the event is of type @CancelWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'startChildWorkflowExecutionInitiatedEventAttributes' - If the event is of type @StartChildWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'signalExternalWorkflowExecutionFailedEventAttributes' - If the event is of type @SignalExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'activityTaskStartedEventAttributes' - If the event is of type @ActivityTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'startLambdaFunctionFailedEventAttributes' - Provides the details of the @StartLambdaFunctionFailed@ event. It isn't set for other event types.
-- * 'childWorkflowExecutionTerminatedEventAttributes' - If the event is of type @ChildWorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'lambdaFunctionFailedEventAttributes' - Provides the details of the @LambdaFunctionFailed@ event. It isn't set for other event types.
-- * 'workflowExecutionCanceledEventAttributes' - If the event is of type @WorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'timerStartedEventAttributes' - If the event is of type @TimerStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'activityTaskCancelRequestedEventAttributes' - If the event is of type @ActivityTaskcancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'workflowExecutionTimedOutEventAttributes' - If the event is of type @WorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'workflowExecutionSignaledEventAttributes' - If the event is of type @WorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'timerFiredEventAttributes' - If the event is of type @TimerFired@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'activityTaskFailedEventAttributes' - If the event is of type @ActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'externalWorkflowExecutionSignaledEventAttributes' - If the event is of type @ExternalWorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'decisionTaskCompletedEventAttributes' - If the event is of type @DecisionTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'startChildWorkflowExecutionFailedEventAttributes' - If the event is of type @StartChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'childWorkflowExecutionFailedEventAttributes' - If the event is of type @ChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'failWorkflowExecutionFailedEventAttributes' - If the event is of type @FailWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'continueAsNewWorkflowExecutionFailedEventAttributes' - If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'signalExternalWorkflowExecutionInitiatedEventAttributes' - If the event is of type @SignalExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'lambdaFunctionTimedOutEventAttributes' - Provides the details of the @LambdaFunctionTimedOut@ event. It isn't set for other event types.
-- * 'eventId' - The system generated ID of the event. This ID uniquely identifies the event with in the workflow execution history.
-- * 'workflowExecutionFailedEventAttributes' - If the event is of type @WorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'workflowExecutionContinuedAsNewEventAttributes' - If the event is of type @WorkflowExecutionContinuedAsNew@ then this member is set and provides detailed information about the event. It isn't set for other event types.
-- * 'externalWorkflowExecutionCancelRequestedEventAttributes' - If the event is of type @ExternalWorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
mkHistoryEvent ::
  -- | 'eventTimestamp'
  Lude.Timestamp ->
  -- | 'eventType'
  EventType ->
  -- | 'eventId'
  Lude.Integer ->
  HistoryEvent
mkHistoryEvent pEventTimestamp_ pEventType_ pEventId_ =
  HistoryEvent'
    { workflowExecutionCancelRequestedEventAttributes =
        Lude.Nothing,
      recordMarkerFailedEventAttributes = Lude.Nothing,
      requestCancelExternalWorkflowExecutionInitiatedEventAttributes =
        Lude.Nothing,
      lambdaFunctionStartedEventAttributes = Lude.Nothing,
      decisionTaskScheduledEventAttributes = Lude.Nothing,
      workflowExecutionCompletedEventAttributes = Lude.Nothing,
      startTimerFailedEventAttributes = Lude.Nothing,
      eventTimestamp = pEventTimestamp_,
      activityTaskScheduledEventAttributes = Lude.Nothing,
      scheduleActivityTaskFailedEventAttributes = Lude.Nothing,
      childWorkflowExecutionCompletedEventAttributes = Lude.Nothing,
      markerRecordedEventAttributes = Lude.Nothing,
      scheduleLambdaFunctionFailedEventAttributes = Lude.Nothing,
      completeWorkflowExecutionFailedEventAttributes = Lude.Nothing,
      lambdaFunctionCompletedEventAttributes = Lude.Nothing,
      requestCancelExternalWorkflowExecutionFailedEventAttributes =
        Lude.Nothing,
      timerCanceledEventAttributes = Lude.Nothing,
      workflowExecutionStartedEventAttributes = Lude.Nothing,
      activityTaskCompletedEventAttributes = Lude.Nothing,
      decisionTaskTimedOutEventAttributes = Lude.Nothing,
      cancelTimerFailedEventAttributes = Lude.Nothing,
      childWorkflowExecutionStartedEventAttributes = Lude.Nothing,
      eventType = pEventType_,
      activityTaskCanceledEventAttributes = Lude.Nothing,
      activityTaskTimedOutEventAttributes = Lude.Nothing,
      decisionTaskStartedEventAttributes = Lude.Nothing,
      workflowExecutionTerminatedEventAttributes = Lude.Nothing,
      childWorkflowExecutionCanceledEventAttributes = Lude.Nothing,
      requestCancelActivityTaskFailedEventAttributes = Lude.Nothing,
      lambdaFunctionScheduledEventAttributes = Lude.Nothing,
      childWorkflowExecutionTimedOutEventAttributes = Lude.Nothing,
      cancelWorkflowExecutionFailedEventAttributes = Lude.Nothing,
      startChildWorkflowExecutionInitiatedEventAttributes = Lude.Nothing,
      signalExternalWorkflowExecutionFailedEventAttributes =
        Lude.Nothing,
      activityTaskStartedEventAttributes = Lude.Nothing,
      startLambdaFunctionFailedEventAttributes = Lude.Nothing,
      childWorkflowExecutionTerminatedEventAttributes = Lude.Nothing,
      lambdaFunctionFailedEventAttributes = Lude.Nothing,
      workflowExecutionCanceledEventAttributes = Lude.Nothing,
      timerStartedEventAttributes = Lude.Nothing,
      activityTaskCancelRequestedEventAttributes = Lude.Nothing,
      workflowExecutionTimedOutEventAttributes = Lude.Nothing,
      workflowExecutionSignaledEventAttributes = Lude.Nothing,
      timerFiredEventAttributes = Lude.Nothing,
      activityTaskFailedEventAttributes = Lude.Nothing,
      externalWorkflowExecutionSignaledEventAttributes = Lude.Nothing,
      decisionTaskCompletedEventAttributes = Lude.Nothing,
      startChildWorkflowExecutionFailedEventAttributes = Lude.Nothing,
      childWorkflowExecutionFailedEventAttributes = Lude.Nothing,
      failWorkflowExecutionFailedEventAttributes = Lude.Nothing,
      continueAsNewWorkflowExecutionFailedEventAttributes = Lude.Nothing,
      signalExternalWorkflowExecutionInitiatedEventAttributes =
        Lude.Nothing,
      lambdaFunctionTimedOutEventAttributes = Lude.Nothing,
      eventId = pEventId_,
      workflowExecutionFailedEventAttributes = Lude.Nothing,
      workflowExecutionContinuedAsNewEventAttributes = Lude.Nothing,
      externalWorkflowExecutionCancelRequestedEventAttributes =
        Lude.Nothing
    }

-- | If the event is of type @WorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionCancelRequestedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe WorkflowExecutionCancelRequestedEventAttributes)
heWorkflowExecutionCancelRequestedEventAttributes = Lens.lens (workflowExecutionCancelRequestedEventAttributes :: HistoryEvent -> Lude.Maybe WorkflowExecutionCancelRequestedEventAttributes) (\s a -> s {workflowExecutionCancelRequestedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heWorkflowExecutionCancelRequestedEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionCancelRequestedEventAttributes' instead." #-}

-- | If the event is of type @DecisionTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'recordMarkerFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heRecordMarkerFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe RecordMarkerFailedEventAttributes)
heRecordMarkerFailedEventAttributes = Lens.lens (recordMarkerFailedEventAttributes :: HistoryEvent -> Lude.Maybe RecordMarkerFailedEventAttributes) (\s a -> s {recordMarkerFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heRecordMarkerFailedEventAttributes "Use generic-lens or generic-optics with 'recordMarkerFailedEventAttributes' instead." #-}

-- | If the event is of type @RequestCancelExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = Lens.lens (requestCancelExternalWorkflowExecutionInitiatedEventAttributes :: HistoryEvent -> Lude.Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes) (\s a -> s {requestCancelExternalWorkflowExecutionInitiatedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes "Use generic-lens or generic-optics with 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes' instead." #-}

-- | Provides the details of the @LambdaFunctionStarted@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'lambdaFunctionStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionStartedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionStartedEventAttributes)
heLambdaFunctionStartedEventAttributes = Lens.lens (lambdaFunctionStartedEventAttributes :: HistoryEvent -> Lude.Maybe LambdaFunctionStartedEventAttributes) (\s a -> s {lambdaFunctionStartedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionStartedEventAttributes "Use generic-lens or generic-optics with 'lambdaFunctionStartedEventAttributes' instead." #-}

-- | If the event is of type @DecisionTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'decisionTaskScheduledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heDecisionTaskScheduledEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe DecisionTaskScheduledEventAttributes)
heDecisionTaskScheduledEventAttributes = Lens.lens (decisionTaskScheduledEventAttributes :: HistoryEvent -> Lude.Maybe DecisionTaskScheduledEventAttributes) (\s a -> s {decisionTaskScheduledEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heDecisionTaskScheduledEventAttributes "Use generic-lens or generic-optics with 'decisionTaskScheduledEventAttributes' instead." #-}

-- | If the event is of type @WorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionCompletedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe WorkflowExecutionCompletedEventAttributes)
heWorkflowExecutionCompletedEventAttributes = Lens.lens (workflowExecutionCompletedEventAttributes :: HistoryEvent -> Lude.Maybe WorkflowExecutionCompletedEventAttributes) (\s a -> s {workflowExecutionCompletedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heWorkflowExecutionCompletedEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionCompletedEventAttributes' instead." #-}

-- | If the event is of type @StartTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'startTimerFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStartTimerFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe StartTimerFailedEventAttributes)
heStartTimerFailedEventAttributes = Lens.lens (startTimerFailedEventAttributes :: HistoryEvent -> Lude.Maybe StartTimerFailedEventAttributes) (\s a -> s {startTimerFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heStartTimerFailedEventAttributes "Use generic-lens or generic-optics with 'startTimerFailedEventAttributes' instead." #-}

-- | The date and time when the event occurred.
--
-- /Note:/ Consider using 'eventTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heEventTimestamp :: Lens.Lens' HistoryEvent Lude.Timestamp
heEventTimestamp = Lens.lens (eventTimestamp :: HistoryEvent -> Lude.Timestamp) (\s a -> s {eventTimestamp = a} :: HistoryEvent)
{-# DEPRECATED heEventTimestamp "Use generic-lens or generic-optics with 'eventTimestamp' instead." #-}

-- | If the event is of type @ActivityTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskScheduledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskScheduledEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityTaskScheduledEventAttributes)
heActivityTaskScheduledEventAttributes = Lens.lens (activityTaskScheduledEventAttributes :: HistoryEvent -> Lude.Maybe ActivityTaskScheduledEventAttributes) (\s a -> s {activityTaskScheduledEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heActivityTaskScheduledEventAttributes "Use generic-lens or generic-optics with 'activityTaskScheduledEventAttributes' instead." #-}

-- | If the event is of type @ScheduleActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'scheduleActivityTaskFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heScheduleActivityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ScheduleActivityTaskFailedEventAttributes)
heScheduleActivityTaskFailedEventAttributes = Lens.lens (scheduleActivityTaskFailedEventAttributes :: HistoryEvent -> Lude.Maybe ScheduleActivityTaskFailedEventAttributes) (\s a -> s {scheduleActivityTaskFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heScheduleActivityTaskFailedEventAttributes "Use generic-lens or generic-optics with 'scheduleActivityTaskFailedEventAttributes' instead." #-}

-- | If the event is of type @ChildWorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionCompletedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ChildWorkflowExecutionCompletedEventAttributes)
heChildWorkflowExecutionCompletedEventAttributes = Lens.lens (childWorkflowExecutionCompletedEventAttributes :: HistoryEvent -> Lude.Maybe ChildWorkflowExecutionCompletedEventAttributes) (\s a -> s {childWorkflowExecutionCompletedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heChildWorkflowExecutionCompletedEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionCompletedEventAttributes' instead." #-}

-- | If the event is of type @MarkerRecorded@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'markerRecordedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMarkerRecordedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe MarkerRecordedEventAttributes)
heMarkerRecordedEventAttributes = Lens.lens (markerRecordedEventAttributes :: HistoryEvent -> Lude.Maybe MarkerRecordedEventAttributes) (\s a -> s {markerRecordedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heMarkerRecordedEventAttributes "Use generic-lens or generic-optics with 'markerRecordedEventAttributes' instead." #-}

-- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'scheduleLambdaFunctionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heScheduleLambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ScheduleLambdaFunctionFailedEventAttributes)
heScheduleLambdaFunctionFailedEventAttributes = Lens.lens (scheduleLambdaFunctionFailedEventAttributes :: HistoryEvent -> Lude.Maybe ScheduleLambdaFunctionFailedEventAttributes) (\s a -> s {scheduleLambdaFunctionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heScheduleLambdaFunctionFailedEventAttributes "Use generic-lens or generic-optics with 'scheduleLambdaFunctionFailedEventAttributes' instead." #-}

-- | If the event is of type @CompleteWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'completeWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heCompleteWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe CompleteWorkflowExecutionFailedEventAttributes)
heCompleteWorkflowExecutionFailedEventAttributes = Lens.lens (completeWorkflowExecutionFailedEventAttributes :: HistoryEvent -> Lude.Maybe CompleteWorkflowExecutionFailedEventAttributes) (\s a -> s {completeWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heCompleteWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'completeWorkflowExecutionFailedEventAttributes' instead." #-}

-- | Provides the details of the @LambdaFunctionCompleted@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'lambdaFunctionCompletedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionCompletedEventAttributes)
heLambdaFunctionCompletedEventAttributes = Lens.lens (lambdaFunctionCompletedEventAttributes :: HistoryEvent -> Lude.Maybe LambdaFunctionCompletedEventAttributes) (\s a -> s {lambdaFunctionCompletedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionCompletedEventAttributes "Use generic-lens or generic-optics with 'lambdaFunctionCompletedEventAttributes' instead." #-}

-- | If the event is of type @RequestCancelExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'requestCancelExternalWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes)
heRequestCancelExternalWorkflowExecutionFailedEventAttributes = Lens.lens (requestCancelExternalWorkflowExecutionFailedEventAttributes :: HistoryEvent -> Lude.Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes) (\s a -> s {requestCancelExternalWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heRequestCancelExternalWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'requestCancelExternalWorkflowExecutionFailedEventAttributes' instead." #-}

-- | If the event is of type @TimerCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'timerCanceledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTimerCanceledEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe TimerCanceledEventAttributes)
heTimerCanceledEventAttributes = Lens.lens (timerCanceledEventAttributes :: HistoryEvent -> Lude.Maybe TimerCanceledEventAttributes) (\s a -> s {timerCanceledEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heTimerCanceledEventAttributes "Use generic-lens or generic-optics with 'timerCanceledEventAttributes' instead." #-}

-- | If the event is of type @WorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionStartedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe WorkflowExecutionStartedEventAttributes)
heWorkflowExecutionStartedEventAttributes = Lens.lens (workflowExecutionStartedEventAttributes :: HistoryEvent -> Lude.Maybe WorkflowExecutionStartedEventAttributes) (\s a -> s {workflowExecutionStartedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heWorkflowExecutionStartedEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionStartedEventAttributes' instead." #-}

-- | If the event is of type @ActivityTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskCompletedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskCompletedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityTaskCompletedEventAttributes)
heActivityTaskCompletedEventAttributes = Lens.lens (activityTaskCompletedEventAttributes :: HistoryEvent -> Lude.Maybe ActivityTaskCompletedEventAttributes) (\s a -> s {activityTaskCompletedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heActivityTaskCompletedEventAttributes "Use generic-lens or generic-optics with 'activityTaskCompletedEventAttributes' instead." #-}

-- | If the event is of type @DecisionTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'decisionTaskTimedOutEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heDecisionTaskTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe DecisionTaskTimedOutEventAttributes)
heDecisionTaskTimedOutEventAttributes = Lens.lens (decisionTaskTimedOutEventAttributes :: HistoryEvent -> Lude.Maybe DecisionTaskTimedOutEventAttributes) (\s a -> s {decisionTaskTimedOutEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heDecisionTaskTimedOutEventAttributes "Use generic-lens or generic-optics with 'decisionTaskTimedOutEventAttributes' instead." #-}

-- | If the event is of type @CancelTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'cancelTimerFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heCancelTimerFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe CancelTimerFailedEventAttributes)
heCancelTimerFailedEventAttributes = Lens.lens (cancelTimerFailedEventAttributes :: HistoryEvent -> Lude.Maybe CancelTimerFailedEventAttributes) (\s a -> s {cancelTimerFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heCancelTimerFailedEventAttributes "Use generic-lens or generic-optics with 'cancelTimerFailedEventAttributes' instead." #-}

-- | If the event is of type @ChildWorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionStartedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ChildWorkflowExecutionStartedEventAttributes)
heChildWorkflowExecutionStartedEventAttributes = Lens.lens (childWorkflowExecutionStartedEventAttributes :: HistoryEvent -> Lude.Maybe ChildWorkflowExecutionStartedEventAttributes) (\s a -> s {childWorkflowExecutionStartedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heChildWorkflowExecutionStartedEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionStartedEventAttributes' instead." #-}

-- | The type of the history event.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heEventType :: Lens.Lens' HistoryEvent EventType
heEventType = Lens.lens (eventType :: HistoryEvent -> EventType) (\s a -> s {eventType = a} :: HistoryEvent)
{-# DEPRECATED heEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | If the event is of type @ActivityTaskCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskCanceledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskCanceledEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityTaskCanceledEventAttributes)
heActivityTaskCanceledEventAttributes = Lens.lens (activityTaskCanceledEventAttributes :: HistoryEvent -> Lude.Maybe ActivityTaskCanceledEventAttributes) (\s a -> s {activityTaskCanceledEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heActivityTaskCanceledEventAttributes "Use generic-lens or generic-optics with 'activityTaskCanceledEventAttributes' instead." #-}

-- | If the event is of type @ActivityTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskTimedOutEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityTaskTimedOutEventAttributes)
heActivityTaskTimedOutEventAttributes = Lens.lens (activityTaskTimedOutEventAttributes :: HistoryEvent -> Lude.Maybe ActivityTaskTimedOutEventAttributes) (\s a -> s {activityTaskTimedOutEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heActivityTaskTimedOutEventAttributes "Use generic-lens or generic-optics with 'activityTaskTimedOutEventAttributes' instead." #-}

-- | If the event is of type @DecisionTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'decisionTaskStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heDecisionTaskStartedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe DecisionTaskStartedEventAttributes)
heDecisionTaskStartedEventAttributes = Lens.lens (decisionTaskStartedEventAttributes :: HistoryEvent -> Lude.Maybe DecisionTaskStartedEventAttributes) (\s a -> s {decisionTaskStartedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heDecisionTaskStartedEventAttributes "Use generic-lens or generic-optics with 'decisionTaskStartedEventAttributes' instead." #-}

-- | If the event is of type @WorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionTerminatedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionTerminatedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe WorkflowExecutionTerminatedEventAttributes)
heWorkflowExecutionTerminatedEventAttributes = Lens.lens (workflowExecutionTerminatedEventAttributes :: HistoryEvent -> Lude.Maybe WorkflowExecutionTerminatedEventAttributes) (\s a -> s {workflowExecutionTerminatedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heWorkflowExecutionTerminatedEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionTerminatedEventAttributes' instead." #-}

-- | If the event is of type @ChildWorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionCanceledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionCanceledEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ChildWorkflowExecutionCanceledEventAttributes)
heChildWorkflowExecutionCanceledEventAttributes = Lens.lens (childWorkflowExecutionCanceledEventAttributes :: HistoryEvent -> Lude.Maybe ChildWorkflowExecutionCanceledEventAttributes) (\s a -> s {childWorkflowExecutionCanceledEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heChildWorkflowExecutionCanceledEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionCanceledEventAttributes' instead." #-}

-- | If the event is of type @RequestCancelActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'requestCancelActivityTaskFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heRequestCancelActivityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe RequestCancelActivityTaskFailedEventAttributes)
heRequestCancelActivityTaskFailedEventAttributes = Lens.lens (requestCancelActivityTaskFailedEventAttributes :: HistoryEvent -> Lude.Maybe RequestCancelActivityTaskFailedEventAttributes) (\s a -> s {requestCancelActivityTaskFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heRequestCancelActivityTaskFailedEventAttributes "Use generic-lens or generic-optics with 'requestCancelActivityTaskFailedEventAttributes' instead." #-}

-- | Provides the details of the @LambdaFunctionScheduled@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'lambdaFunctionScheduledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionScheduledEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionScheduledEventAttributes)
heLambdaFunctionScheduledEventAttributes = Lens.lens (lambdaFunctionScheduledEventAttributes :: HistoryEvent -> Lude.Maybe LambdaFunctionScheduledEventAttributes) (\s a -> s {lambdaFunctionScheduledEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionScheduledEventAttributes "Use generic-lens or generic-optics with 'lambdaFunctionScheduledEventAttributes' instead." #-}

-- | If the event is of type @ChildWorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionTimedOutEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ChildWorkflowExecutionTimedOutEventAttributes)
heChildWorkflowExecutionTimedOutEventAttributes = Lens.lens (childWorkflowExecutionTimedOutEventAttributes :: HistoryEvent -> Lude.Maybe ChildWorkflowExecutionTimedOutEventAttributes) (\s a -> s {childWorkflowExecutionTimedOutEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heChildWorkflowExecutionTimedOutEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionTimedOutEventAttributes' instead." #-}

-- | If the event is of type @CancelWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'cancelWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heCancelWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe CancelWorkflowExecutionFailedEventAttributes)
heCancelWorkflowExecutionFailedEventAttributes = Lens.lens (cancelWorkflowExecutionFailedEventAttributes :: HistoryEvent -> Lude.Maybe CancelWorkflowExecutionFailedEventAttributes) (\s a -> s {cancelWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heCancelWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'cancelWorkflowExecutionFailedEventAttributes' instead." #-}

-- | If the event is of type @StartChildWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'startChildWorkflowExecutionInitiatedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStartChildWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe StartChildWorkflowExecutionInitiatedEventAttributes)
heStartChildWorkflowExecutionInitiatedEventAttributes = Lens.lens (startChildWorkflowExecutionInitiatedEventAttributes :: HistoryEvent -> Lude.Maybe StartChildWorkflowExecutionInitiatedEventAttributes) (\s a -> s {startChildWorkflowExecutionInitiatedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heStartChildWorkflowExecutionInitiatedEventAttributes "Use generic-lens or generic-optics with 'startChildWorkflowExecutionInitiatedEventAttributes' instead." #-}

-- | If the event is of type @SignalExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'signalExternalWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heSignalExternalWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe SignalExternalWorkflowExecutionFailedEventAttributes)
heSignalExternalWorkflowExecutionFailedEventAttributes = Lens.lens (signalExternalWorkflowExecutionFailedEventAttributes :: HistoryEvent -> Lude.Maybe SignalExternalWorkflowExecutionFailedEventAttributes) (\s a -> s {signalExternalWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heSignalExternalWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'signalExternalWorkflowExecutionFailedEventAttributes' instead." #-}

-- | If the event is of type @ActivityTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskStartedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityTaskStartedEventAttributes)
heActivityTaskStartedEventAttributes = Lens.lens (activityTaskStartedEventAttributes :: HistoryEvent -> Lude.Maybe ActivityTaskStartedEventAttributes) (\s a -> s {activityTaskStartedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heActivityTaskStartedEventAttributes "Use generic-lens or generic-optics with 'activityTaskStartedEventAttributes' instead." #-}

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'startLambdaFunctionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStartLambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe StartLambdaFunctionFailedEventAttributes)
heStartLambdaFunctionFailedEventAttributes = Lens.lens (startLambdaFunctionFailedEventAttributes :: HistoryEvent -> Lude.Maybe StartLambdaFunctionFailedEventAttributes) (\s a -> s {startLambdaFunctionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heStartLambdaFunctionFailedEventAttributes "Use generic-lens or generic-optics with 'startLambdaFunctionFailedEventAttributes' instead." #-}

-- | If the event is of type @ChildWorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionTerminatedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionTerminatedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ChildWorkflowExecutionTerminatedEventAttributes)
heChildWorkflowExecutionTerminatedEventAttributes = Lens.lens (childWorkflowExecutionTerminatedEventAttributes :: HistoryEvent -> Lude.Maybe ChildWorkflowExecutionTerminatedEventAttributes) (\s a -> s {childWorkflowExecutionTerminatedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heChildWorkflowExecutionTerminatedEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionTerminatedEventAttributes' instead." #-}

-- | Provides the details of the @LambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'lambdaFunctionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionFailedEventAttributes)
heLambdaFunctionFailedEventAttributes = Lens.lens (lambdaFunctionFailedEventAttributes :: HistoryEvent -> Lude.Maybe LambdaFunctionFailedEventAttributes) (\s a -> s {lambdaFunctionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionFailedEventAttributes "Use generic-lens or generic-optics with 'lambdaFunctionFailedEventAttributes' instead." #-}

-- | If the event is of type @WorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionCanceledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionCanceledEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe WorkflowExecutionCanceledEventAttributes)
heWorkflowExecutionCanceledEventAttributes = Lens.lens (workflowExecutionCanceledEventAttributes :: HistoryEvent -> Lude.Maybe WorkflowExecutionCanceledEventAttributes) (\s a -> s {workflowExecutionCanceledEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heWorkflowExecutionCanceledEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionCanceledEventAttributes' instead." #-}

-- | If the event is of type @TimerStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'timerStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTimerStartedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe TimerStartedEventAttributes)
heTimerStartedEventAttributes = Lens.lens (timerStartedEventAttributes :: HistoryEvent -> Lude.Maybe TimerStartedEventAttributes) (\s a -> s {timerStartedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heTimerStartedEventAttributes "Use generic-lens or generic-optics with 'timerStartedEventAttributes' instead." #-}

-- | If the event is of type @ActivityTaskcancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskCancelRequestedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityTaskCancelRequestedEventAttributes)
heActivityTaskCancelRequestedEventAttributes = Lens.lens (activityTaskCancelRequestedEventAttributes :: HistoryEvent -> Lude.Maybe ActivityTaskCancelRequestedEventAttributes) (\s a -> s {activityTaskCancelRequestedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heActivityTaskCancelRequestedEventAttributes "Use generic-lens or generic-optics with 'activityTaskCancelRequestedEventAttributes' instead." #-}

-- | If the event is of type @WorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionTimedOutEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe WorkflowExecutionTimedOutEventAttributes)
heWorkflowExecutionTimedOutEventAttributes = Lens.lens (workflowExecutionTimedOutEventAttributes :: HistoryEvent -> Lude.Maybe WorkflowExecutionTimedOutEventAttributes) (\s a -> s {workflowExecutionTimedOutEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heWorkflowExecutionTimedOutEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionTimedOutEventAttributes' instead." #-}

-- | If the event is of type @WorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionSignaledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionSignaledEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe WorkflowExecutionSignaledEventAttributes)
heWorkflowExecutionSignaledEventAttributes = Lens.lens (workflowExecutionSignaledEventAttributes :: HistoryEvent -> Lude.Maybe WorkflowExecutionSignaledEventAttributes) (\s a -> s {workflowExecutionSignaledEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heWorkflowExecutionSignaledEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionSignaledEventAttributes' instead." #-}

-- | If the event is of type @TimerFired@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'timerFiredEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTimerFiredEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe TimerFiredEventAttributes)
heTimerFiredEventAttributes = Lens.lens (timerFiredEventAttributes :: HistoryEvent -> Lude.Maybe TimerFiredEventAttributes) (\s a -> s {timerFiredEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heTimerFiredEventAttributes "Use generic-lens or generic-optics with 'timerFiredEventAttributes' instead." #-}

-- | If the event is of type @ActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ActivityTaskFailedEventAttributes)
heActivityTaskFailedEventAttributes = Lens.lens (activityTaskFailedEventAttributes :: HistoryEvent -> Lude.Maybe ActivityTaskFailedEventAttributes) (\s a -> s {activityTaskFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heActivityTaskFailedEventAttributes "Use generic-lens or generic-optics with 'activityTaskFailedEventAttributes' instead." #-}

-- | If the event is of type @ExternalWorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'externalWorkflowExecutionSignaledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExternalWorkflowExecutionSignaledEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ExternalWorkflowExecutionSignaledEventAttributes)
heExternalWorkflowExecutionSignaledEventAttributes = Lens.lens (externalWorkflowExecutionSignaledEventAttributes :: HistoryEvent -> Lude.Maybe ExternalWorkflowExecutionSignaledEventAttributes) (\s a -> s {externalWorkflowExecutionSignaledEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heExternalWorkflowExecutionSignaledEventAttributes "Use generic-lens or generic-optics with 'externalWorkflowExecutionSignaledEventAttributes' instead." #-}

-- | If the event is of type @DecisionTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heDecisionTaskCompletedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe DecisionTaskCompletedEventAttributes)
heDecisionTaskCompletedEventAttributes = Lens.lens (decisionTaskCompletedEventAttributes :: HistoryEvent -> Lude.Maybe DecisionTaskCompletedEventAttributes) (\s a -> s {decisionTaskCompletedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heDecisionTaskCompletedEventAttributes "Use generic-lens or generic-optics with 'decisionTaskCompletedEventAttributes' instead." #-}

-- | If the event is of type @StartChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'startChildWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStartChildWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe StartChildWorkflowExecutionFailedEventAttributes)
heStartChildWorkflowExecutionFailedEventAttributes = Lens.lens (startChildWorkflowExecutionFailedEventAttributes :: HistoryEvent -> Lude.Maybe StartChildWorkflowExecutionFailedEventAttributes) (\s a -> s {startChildWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heStartChildWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'startChildWorkflowExecutionFailedEventAttributes' instead." #-}

-- | If the event is of type @ChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ChildWorkflowExecutionFailedEventAttributes)
heChildWorkflowExecutionFailedEventAttributes = Lens.lens (childWorkflowExecutionFailedEventAttributes :: HistoryEvent -> Lude.Maybe ChildWorkflowExecutionFailedEventAttributes) (\s a -> s {childWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heChildWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionFailedEventAttributes' instead." #-}

-- | If the event is of type @FailWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'failWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heFailWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe FailWorkflowExecutionFailedEventAttributes)
heFailWorkflowExecutionFailedEventAttributes = Lens.lens (failWorkflowExecutionFailedEventAttributes :: HistoryEvent -> Lude.Maybe FailWorkflowExecutionFailedEventAttributes) (\s a -> s {failWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heFailWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'failWorkflowExecutionFailedEventAttributes' instead." #-}

-- | If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'continueAsNewWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heContinueAsNewWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes)
heContinueAsNewWorkflowExecutionFailedEventAttributes = Lens.lens (continueAsNewWorkflowExecutionFailedEventAttributes :: HistoryEvent -> Lude.Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes) (\s a -> s {continueAsNewWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heContinueAsNewWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'continueAsNewWorkflowExecutionFailedEventAttributes' instead." #-}

-- | If the event is of type @SignalExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'signalExternalWorkflowExecutionInitiatedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heSignalExternalWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes)
heSignalExternalWorkflowExecutionInitiatedEventAttributes = Lens.lens (signalExternalWorkflowExecutionInitiatedEventAttributes :: HistoryEvent -> Lude.Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes) (\s a -> s {signalExternalWorkflowExecutionInitiatedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heSignalExternalWorkflowExecutionInitiatedEventAttributes "Use generic-lens or generic-optics with 'signalExternalWorkflowExecutionInitiatedEventAttributes' instead." #-}

-- | Provides the details of the @LambdaFunctionTimedOut@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'lambdaFunctionTimedOutEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe LambdaFunctionTimedOutEventAttributes)
heLambdaFunctionTimedOutEventAttributes = Lens.lens (lambdaFunctionTimedOutEventAttributes :: HistoryEvent -> Lude.Maybe LambdaFunctionTimedOutEventAttributes) (\s a -> s {lambdaFunctionTimedOutEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heLambdaFunctionTimedOutEventAttributes "Use generic-lens or generic-optics with 'lambdaFunctionTimedOutEventAttributes' instead." #-}

-- | The system generated ID of the event. This ID uniquely identifies the event with in the workflow execution history.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heEventId :: Lens.Lens' HistoryEvent Lude.Integer
heEventId = Lens.lens (eventId :: HistoryEvent -> Lude.Integer) (\s a -> s {eventId = a} :: HistoryEvent)
{-# DEPRECATED heEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

-- | If the event is of type @WorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe WorkflowExecutionFailedEventAttributes)
heWorkflowExecutionFailedEventAttributes = Lens.lens (workflowExecutionFailedEventAttributes :: HistoryEvent -> Lude.Maybe WorkflowExecutionFailedEventAttributes) (\s a -> s {workflowExecutionFailedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionFailedEventAttributes' instead." #-}

-- | If the event is of type @WorkflowExecutionContinuedAsNew@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionContinuedAsNewEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionContinuedAsNewEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe WorkflowExecutionContinuedAsNewEventAttributes)
heWorkflowExecutionContinuedAsNewEventAttributes = Lens.lens (workflowExecutionContinuedAsNewEventAttributes :: HistoryEvent -> Lude.Maybe WorkflowExecutionContinuedAsNewEventAttributes) (\s a -> s {workflowExecutionContinuedAsNewEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heWorkflowExecutionContinuedAsNewEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionContinuedAsNewEventAttributes' instead." #-}

-- | If the event is of type @ExternalWorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'externalWorkflowExecutionCancelRequestedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExternalWorkflowExecutionCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Lude.Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes)
heExternalWorkflowExecutionCancelRequestedEventAttributes = Lens.lens (externalWorkflowExecutionCancelRequestedEventAttributes :: HistoryEvent -> Lude.Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes) (\s a -> s {externalWorkflowExecutionCancelRequestedEventAttributes = a} :: HistoryEvent)
{-# DEPRECATED heExternalWorkflowExecutionCancelRequestedEventAttributes "Use generic-lens or generic-optics with 'externalWorkflowExecutionCancelRequestedEventAttributes' instead." #-}

instance Lude.FromJSON HistoryEvent where
  parseJSON =
    Lude.withObject
      "HistoryEvent"
      ( \x ->
          HistoryEvent'
            Lude.<$> (x Lude..:? "workflowExecutionCancelRequestedEventAttributes")
            Lude.<*> (x Lude..:? "recordMarkerFailedEventAttributes")
            Lude.<*> ( x
                         Lude..:? "requestCancelExternalWorkflowExecutionInitiatedEventAttributes"
                     )
            Lude.<*> (x Lude..:? "lambdaFunctionStartedEventAttributes")
            Lude.<*> (x Lude..:? "decisionTaskScheduledEventAttributes")
            Lude.<*> (x Lude..:? "workflowExecutionCompletedEventAttributes")
            Lude.<*> (x Lude..:? "startTimerFailedEventAttributes")
            Lude.<*> (x Lude..: "eventTimestamp")
            Lude.<*> (x Lude..:? "activityTaskScheduledEventAttributes")
            Lude.<*> (x Lude..:? "scheduleActivityTaskFailedEventAttributes")
            Lude.<*> (x Lude..:? "childWorkflowExecutionCompletedEventAttributes")
            Lude.<*> (x Lude..:? "markerRecordedEventAttributes")
            Lude.<*> (x Lude..:? "scheduleLambdaFunctionFailedEventAttributes")
            Lude.<*> (x Lude..:? "completeWorkflowExecutionFailedEventAttributes")
            Lude.<*> (x Lude..:? "lambdaFunctionCompletedEventAttributes")
            Lude.<*> ( x
                         Lude..:? "requestCancelExternalWorkflowExecutionFailedEventAttributes"
                     )
            Lude.<*> (x Lude..:? "timerCanceledEventAttributes")
            Lude.<*> (x Lude..:? "workflowExecutionStartedEventAttributes")
            Lude.<*> (x Lude..:? "activityTaskCompletedEventAttributes")
            Lude.<*> (x Lude..:? "decisionTaskTimedOutEventAttributes")
            Lude.<*> (x Lude..:? "cancelTimerFailedEventAttributes")
            Lude.<*> (x Lude..:? "childWorkflowExecutionStartedEventAttributes")
            Lude.<*> (x Lude..: "eventType")
            Lude.<*> (x Lude..:? "activityTaskCanceledEventAttributes")
            Lude.<*> (x Lude..:? "activityTaskTimedOutEventAttributes")
            Lude.<*> (x Lude..:? "decisionTaskStartedEventAttributes")
            Lude.<*> (x Lude..:? "workflowExecutionTerminatedEventAttributes")
            Lude.<*> (x Lude..:? "childWorkflowExecutionCanceledEventAttributes")
            Lude.<*> (x Lude..:? "requestCancelActivityTaskFailedEventAttributes")
            Lude.<*> (x Lude..:? "lambdaFunctionScheduledEventAttributes")
            Lude.<*> (x Lude..:? "childWorkflowExecutionTimedOutEventAttributes")
            Lude.<*> (x Lude..:? "cancelWorkflowExecutionFailedEventAttributes")
            Lude.<*> (x Lude..:? "startChildWorkflowExecutionInitiatedEventAttributes")
            Lude.<*> (x Lude..:? "signalExternalWorkflowExecutionFailedEventAttributes")
            Lude.<*> (x Lude..:? "activityTaskStartedEventAttributes")
            Lude.<*> (x Lude..:? "startLambdaFunctionFailedEventAttributes")
            Lude.<*> (x Lude..:? "childWorkflowExecutionTerminatedEventAttributes")
            Lude.<*> (x Lude..:? "lambdaFunctionFailedEventAttributes")
            Lude.<*> (x Lude..:? "workflowExecutionCanceledEventAttributes")
            Lude.<*> (x Lude..:? "timerStartedEventAttributes")
            Lude.<*> (x Lude..:? "activityTaskCancelRequestedEventAttributes")
            Lude.<*> (x Lude..:? "workflowExecutionTimedOutEventAttributes")
            Lude.<*> (x Lude..:? "workflowExecutionSignaledEventAttributes")
            Lude.<*> (x Lude..:? "timerFiredEventAttributes")
            Lude.<*> (x Lude..:? "activityTaskFailedEventAttributes")
            Lude.<*> (x Lude..:? "externalWorkflowExecutionSignaledEventAttributes")
            Lude.<*> (x Lude..:? "decisionTaskCompletedEventAttributes")
            Lude.<*> (x Lude..:? "startChildWorkflowExecutionFailedEventAttributes")
            Lude.<*> (x Lude..:? "childWorkflowExecutionFailedEventAttributes")
            Lude.<*> (x Lude..:? "failWorkflowExecutionFailedEventAttributes")
            Lude.<*> (x Lude..:? "continueAsNewWorkflowExecutionFailedEventAttributes")
            Lude.<*> ( x
                         Lude..:? "signalExternalWorkflowExecutionInitiatedEventAttributes"
                     )
            Lude.<*> (x Lude..:? "lambdaFunctionTimedOutEventAttributes")
            Lude.<*> (x Lude..: "eventId")
            Lude.<*> (x Lude..:? "workflowExecutionFailedEventAttributes")
            Lude.<*> (x Lude..:? "workflowExecutionContinuedAsNewEventAttributes")
            Lude.<*> ( x
                         Lude..:? "externalWorkflowExecutionCancelRequestedEventAttributes"
                     )
      )

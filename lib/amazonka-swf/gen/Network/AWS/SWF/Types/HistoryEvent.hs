{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.HistoryEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.HistoryEvent
  ( HistoryEvent (..)
  -- * Smart constructor
  , mkHistoryEvent
  -- * Lenses
  , heEventTimestamp
  , heEventType
  , heEventId
  , heActivityTaskCancelRequestedEventAttributes
  , heActivityTaskCanceledEventAttributes
  , heActivityTaskCompletedEventAttributes
  , heActivityTaskFailedEventAttributes
  , heActivityTaskScheduledEventAttributes
  , heActivityTaskStartedEventAttributes
  , heActivityTaskTimedOutEventAttributes
  , heCancelTimerFailedEventAttributes
  , heCancelWorkflowExecutionFailedEventAttributes
  , heChildWorkflowExecutionCanceledEventAttributes
  , heChildWorkflowExecutionCompletedEventAttributes
  , heChildWorkflowExecutionFailedEventAttributes
  , heChildWorkflowExecutionStartedEventAttributes
  , heChildWorkflowExecutionTerminatedEventAttributes
  , heChildWorkflowExecutionTimedOutEventAttributes
  , heCompleteWorkflowExecutionFailedEventAttributes
  , heContinueAsNewWorkflowExecutionFailedEventAttributes
  , heDecisionTaskCompletedEventAttributes
  , heDecisionTaskScheduledEventAttributes
  , heDecisionTaskStartedEventAttributes
  , heDecisionTaskTimedOutEventAttributes
  , heExternalWorkflowExecutionCancelRequestedEventAttributes
  , heExternalWorkflowExecutionSignaledEventAttributes
  , heFailWorkflowExecutionFailedEventAttributes
  , heLambdaFunctionCompletedEventAttributes
  , heLambdaFunctionFailedEventAttributes
  , heLambdaFunctionScheduledEventAttributes
  , heLambdaFunctionStartedEventAttributes
  , heLambdaFunctionTimedOutEventAttributes
  , heMarkerRecordedEventAttributes
  , heRecordMarkerFailedEventAttributes
  , heRequestCancelActivityTaskFailedEventAttributes
  , heRequestCancelExternalWorkflowExecutionFailedEventAttributes
  , heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  , heScheduleActivityTaskFailedEventAttributes
  , heScheduleLambdaFunctionFailedEventAttributes
  , heSignalExternalWorkflowExecutionFailedEventAttributes
  , heSignalExternalWorkflowExecutionInitiatedEventAttributes
  , heStartChildWorkflowExecutionFailedEventAttributes
  , heStartChildWorkflowExecutionInitiatedEventAttributes
  , heStartLambdaFunctionFailedEventAttributes
  , heStartTimerFailedEventAttributes
  , heTimerCanceledEventAttributes
  , heTimerFiredEventAttributes
  , heTimerStartedEventAttributes
  , heWorkflowExecutionCancelRequestedEventAttributes
  , heWorkflowExecutionCanceledEventAttributes
  , heWorkflowExecutionCompletedEventAttributes
  , heWorkflowExecutionContinuedAsNewEventAttributes
  , heWorkflowExecutionFailedEventAttributes
  , heWorkflowExecutionSignaledEventAttributes
  , heWorkflowExecutionStartedEventAttributes
  , heWorkflowExecutionTerminatedEventAttributes
  , heWorkflowExecutionTimedOutEventAttributes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.ActivityTaskCancelRequestedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ActivityTaskCanceledEventAttributes as Types
import qualified Network.AWS.SWF.Types.ActivityTaskCompletedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ActivityTaskFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ActivityTaskScheduledEventAttributes as Types
import qualified Network.AWS.SWF.Types.ActivityTaskStartedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ActivityTaskTimedOutEventAttributes as Types
import qualified Network.AWS.SWF.Types.CancelTimerFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.CancelWorkflowExecutionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes as Types
import qualified Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ChildWorkflowExecutionStartedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes as Types
import qualified Network.AWS.SWF.Types.CompleteWorkflowExecutionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.DecisionTaskCompletedEventAttributes as Types
import qualified Network.AWS.SWF.Types.DecisionTaskScheduledEventAttributes as Types
import qualified Network.AWS.SWF.Types.DecisionTaskStartedEventAttributes as Types
import qualified Network.AWS.SWF.Types.DecisionTaskTimedOutEventAttributes as Types
import qualified Network.AWS.SWF.Types.EventType as Types
import qualified Network.AWS.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes as Types
import qualified Network.AWS.SWF.Types.FailWorkflowExecutionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.LambdaFunctionCompletedEventAttributes as Types
import qualified Network.AWS.SWF.Types.LambdaFunctionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.LambdaFunctionScheduledEventAttributes as Types
import qualified Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes as Types
import qualified Network.AWS.SWF.Types.LambdaFunctionTimedOutEventAttributes as Types
import qualified Network.AWS.SWF.Types.MarkerRecordedEventAttributes as Types
import qualified Network.AWS.SWF.Types.RecordMarkerFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.RequestCancelActivityTaskFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ScheduleActivityTaskFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes as Types
import qualified Network.AWS.SWF.Types.StartChildWorkflowExecutionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes as Types
import qualified Network.AWS.SWF.Types.StartLambdaFunctionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.StartTimerFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.TimerCanceledEventAttributes as Types
import qualified Network.AWS.SWF.Types.TimerFiredEventAttributes as Types
import qualified Network.AWS.SWF.Types.TimerStartedEventAttributes as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionStartedEventAttributes as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionTerminatedEventAttributes as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionTimedOutEventAttributes as Types

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
  { eventTimestamp :: Core.NominalDiffTime
    -- ^ The date and time when the event occurred.
  , eventType :: Types.EventType
    -- ^ The type of the history event.
  , eventId :: Core.Integer
    -- ^ The system generated ID of the event. This ID uniquely identifies the event with in the workflow execution history.
  , activityTaskCancelRequestedEventAttributes :: Core.Maybe Types.ActivityTaskCancelRequestedEventAttributes
    -- ^ If the event is of type @ActivityTaskcancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , activityTaskCanceledEventAttributes :: Core.Maybe Types.ActivityTaskCanceledEventAttributes
    -- ^ If the event is of type @ActivityTaskCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , activityTaskCompletedEventAttributes :: Core.Maybe Types.ActivityTaskCompletedEventAttributes
    -- ^ If the event is of type @ActivityTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , activityTaskFailedEventAttributes :: Core.Maybe Types.ActivityTaskFailedEventAttributes
    -- ^ If the event is of type @ActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , activityTaskScheduledEventAttributes :: Core.Maybe Types.ActivityTaskScheduledEventAttributes
    -- ^ If the event is of type @ActivityTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , activityTaskStartedEventAttributes :: Core.Maybe Types.ActivityTaskStartedEventAttributes
    -- ^ If the event is of type @ActivityTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , activityTaskTimedOutEventAttributes :: Core.Maybe Types.ActivityTaskTimedOutEventAttributes
    -- ^ If the event is of type @ActivityTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , cancelTimerFailedEventAttributes :: Core.Maybe Types.CancelTimerFailedEventAttributes
    -- ^ If the event is of type @CancelTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , cancelWorkflowExecutionFailedEventAttributes :: Core.Maybe Types.CancelWorkflowExecutionFailedEventAttributes
    -- ^ If the event is of type @CancelWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , childWorkflowExecutionCanceledEventAttributes :: Core.Maybe Types.ChildWorkflowExecutionCanceledEventAttributes
    -- ^ If the event is of type @ChildWorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , childWorkflowExecutionCompletedEventAttributes :: Core.Maybe Types.ChildWorkflowExecutionCompletedEventAttributes
    -- ^ If the event is of type @ChildWorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , childWorkflowExecutionFailedEventAttributes :: Core.Maybe Types.ChildWorkflowExecutionFailedEventAttributes
    -- ^ If the event is of type @ChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , childWorkflowExecutionStartedEventAttributes :: Core.Maybe Types.ChildWorkflowExecutionStartedEventAttributes
    -- ^ If the event is of type @ChildWorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , childWorkflowExecutionTerminatedEventAttributes :: Core.Maybe Types.ChildWorkflowExecutionTerminatedEventAttributes
    -- ^ If the event is of type @ChildWorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , childWorkflowExecutionTimedOutEventAttributes :: Core.Maybe Types.ChildWorkflowExecutionTimedOutEventAttributes
    -- ^ If the event is of type @ChildWorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , completeWorkflowExecutionFailedEventAttributes :: Core.Maybe Types.CompleteWorkflowExecutionFailedEventAttributes
    -- ^ If the event is of type @CompleteWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , continueAsNewWorkflowExecutionFailedEventAttributes :: Core.Maybe Types.ContinueAsNewWorkflowExecutionFailedEventAttributes
    -- ^ If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , decisionTaskCompletedEventAttributes :: Core.Maybe Types.DecisionTaskCompletedEventAttributes
    -- ^ If the event is of type @DecisionTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , decisionTaskScheduledEventAttributes :: Core.Maybe Types.DecisionTaskScheduledEventAttributes
    -- ^ If the event is of type @DecisionTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , decisionTaskStartedEventAttributes :: Core.Maybe Types.DecisionTaskStartedEventAttributes
    -- ^ If the event is of type @DecisionTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , decisionTaskTimedOutEventAttributes :: Core.Maybe Types.DecisionTaskTimedOutEventAttributes
    -- ^ If the event is of type @DecisionTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , externalWorkflowExecutionCancelRequestedEventAttributes :: Core.Maybe Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
    -- ^ If the event is of type @ExternalWorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types. 
  , externalWorkflowExecutionSignaledEventAttributes :: Core.Maybe Types.ExternalWorkflowExecutionSignaledEventAttributes
    -- ^ If the event is of type @ExternalWorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , failWorkflowExecutionFailedEventAttributes :: Core.Maybe Types.FailWorkflowExecutionFailedEventAttributes
    -- ^ If the event is of type @FailWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , lambdaFunctionCompletedEventAttributes :: Core.Maybe Types.LambdaFunctionCompletedEventAttributes
    -- ^ Provides the details of the @LambdaFunctionCompleted@ event. It isn't set for other event types.
  , lambdaFunctionFailedEventAttributes :: Core.Maybe Types.LambdaFunctionFailedEventAttributes
    -- ^ Provides the details of the @LambdaFunctionFailed@ event. It isn't set for other event types.
  , lambdaFunctionScheduledEventAttributes :: Core.Maybe Types.LambdaFunctionScheduledEventAttributes
    -- ^ Provides the details of the @LambdaFunctionScheduled@ event. It isn't set for other event types.
  , lambdaFunctionStartedEventAttributes :: Core.Maybe Types.LambdaFunctionStartedEventAttributes
    -- ^ Provides the details of the @LambdaFunctionStarted@ event. It isn't set for other event types.
  , lambdaFunctionTimedOutEventAttributes :: Core.Maybe Types.LambdaFunctionTimedOutEventAttributes
    -- ^ Provides the details of the @LambdaFunctionTimedOut@ event. It isn't set for other event types.
  , markerRecordedEventAttributes :: Core.Maybe Types.MarkerRecordedEventAttributes
    -- ^ If the event is of type @MarkerRecorded@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , recordMarkerFailedEventAttributes :: Core.Maybe Types.RecordMarkerFailedEventAttributes
    -- ^ If the event is of type @DecisionTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , requestCancelActivityTaskFailedEventAttributes :: Core.Maybe Types.RequestCancelActivityTaskFailedEventAttributes
    -- ^ If the event is of type @RequestCancelActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , requestCancelExternalWorkflowExecutionFailedEventAttributes :: Core.Maybe Types.RequestCancelExternalWorkflowExecutionFailedEventAttributes
    -- ^ If the event is of type @RequestCancelExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , requestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Core.Maybe Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    -- ^ If the event is of type @RequestCancelExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , scheduleActivityTaskFailedEventAttributes :: Core.Maybe Types.ScheduleActivityTaskFailedEventAttributes
    -- ^ If the event is of type @ScheduleActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , scheduleLambdaFunctionFailedEventAttributes :: Core.Maybe Types.ScheduleLambdaFunctionFailedEventAttributes
    -- ^ Provides the details of the @ScheduleLambdaFunctionFailed@ event. It isn't set for other event types.
  , signalExternalWorkflowExecutionFailedEventAttributes :: Core.Maybe Types.SignalExternalWorkflowExecutionFailedEventAttributes
    -- ^ If the event is of type @SignalExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , signalExternalWorkflowExecutionInitiatedEventAttributes :: Core.Maybe Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
    -- ^ If the event is of type @SignalExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , startChildWorkflowExecutionFailedEventAttributes :: Core.Maybe Types.StartChildWorkflowExecutionFailedEventAttributes
    -- ^ If the event is of type @StartChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , startChildWorkflowExecutionInitiatedEventAttributes :: Core.Maybe Types.StartChildWorkflowExecutionInitiatedEventAttributes
    -- ^ If the event is of type @StartChildWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , startLambdaFunctionFailedEventAttributes :: Core.Maybe Types.StartLambdaFunctionFailedEventAttributes
    -- ^ Provides the details of the @StartLambdaFunctionFailed@ event. It isn't set for other event types.
  , startTimerFailedEventAttributes :: Core.Maybe Types.StartTimerFailedEventAttributes
    -- ^ If the event is of type @StartTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , timerCanceledEventAttributes :: Core.Maybe Types.TimerCanceledEventAttributes
    -- ^ If the event is of type @TimerCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , timerFiredEventAttributes :: Core.Maybe Types.TimerFiredEventAttributes
    -- ^ If the event is of type @TimerFired@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , timerStartedEventAttributes :: Core.Maybe Types.TimerStartedEventAttributes
    -- ^ If the event is of type @TimerStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , workflowExecutionCancelRequestedEventAttributes :: Core.Maybe Types.WorkflowExecutionCancelRequestedEventAttributes
    -- ^ If the event is of type @WorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , workflowExecutionCanceledEventAttributes :: Core.Maybe Types.WorkflowExecutionCanceledEventAttributes
    -- ^ If the event is of type @WorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , workflowExecutionCompletedEventAttributes :: Core.Maybe Types.WorkflowExecutionCompletedEventAttributes
    -- ^ If the event is of type @WorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , workflowExecutionContinuedAsNewEventAttributes :: Core.Maybe Types.WorkflowExecutionContinuedAsNewEventAttributes
    -- ^ If the event is of type @WorkflowExecutionContinuedAsNew@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , workflowExecutionFailedEventAttributes :: Core.Maybe Types.WorkflowExecutionFailedEventAttributes
    -- ^ If the event is of type @WorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , workflowExecutionSignaledEventAttributes :: Core.Maybe Types.WorkflowExecutionSignaledEventAttributes
    -- ^ If the event is of type @WorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , workflowExecutionStartedEventAttributes :: Core.Maybe Types.WorkflowExecutionStartedEventAttributes
    -- ^ If the event is of type @WorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , workflowExecutionTerminatedEventAttributes :: Core.Maybe Types.WorkflowExecutionTerminatedEventAttributes
    -- ^ If the event is of type @WorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  , workflowExecutionTimedOutEventAttributes :: Core.Maybe Types.WorkflowExecutionTimedOutEventAttributes
    -- ^ If the event is of type @WorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'HistoryEvent' value with any optional fields omitted.
mkHistoryEvent
    :: Core.NominalDiffTime -- ^ 'eventTimestamp'
    -> Types.EventType -- ^ 'eventType'
    -> Core.Integer -- ^ 'eventId'
    -> HistoryEvent
mkHistoryEvent eventTimestamp eventType eventId
  = HistoryEvent'{eventTimestamp, eventType, eventId,
                  activityTaskCancelRequestedEventAttributes = Core.Nothing,
                  activityTaskCanceledEventAttributes = Core.Nothing,
                  activityTaskCompletedEventAttributes = Core.Nothing,
                  activityTaskFailedEventAttributes = Core.Nothing,
                  activityTaskScheduledEventAttributes = Core.Nothing,
                  activityTaskStartedEventAttributes = Core.Nothing,
                  activityTaskTimedOutEventAttributes = Core.Nothing,
                  cancelTimerFailedEventAttributes = Core.Nothing,
                  cancelWorkflowExecutionFailedEventAttributes = Core.Nothing,
                  childWorkflowExecutionCanceledEventAttributes = Core.Nothing,
                  childWorkflowExecutionCompletedEventAttributes = Core.Nothing,
                  childWorkflowExecutionFailedEventAttributes = Core.Nothing,
                  childWorkflowExecutionStartedEventAttributes = Core.Nothing,
                  childWorkflowExecutionTerminatedEventAttributes = Core.Nothing,
                  childWorkflowExecutionTimedOutEventAttributes = Core.Nothing,
                  completeWorkflowExecutionFailedEventAttributes = Core.Nothing,
                  continueAsNewWorkflowExecutionFailedEventAttributes = Core.Nothing,
                  decisionTaskCompletedEventAttributes = Core.Nothing,
                  decisionTaskScheduledEventAttributes = Core.Nothing,
                  decisionTaskStartedEventAttributes = Core.Nothing,
                  decisionTaskTimedOutEventAttributes = Core.Nothing,
                  externalWorkflowExecutionCancelRequestedEventAttributes =
                    Core.Nothing,
                  externalWorkflowExecutionSignaledEventAttributes = Core.Nothing,
                  failWorkflowExecutionFailedEventAttributes = Core.Nothing,
                  lambdaFunctionCompletedEventAttributes = Core.Nothing,
                  lambdaFunctionFailedEventAttributes = Core.Nothing,
                  lambdaFunctionScheduledEventAttributes = Core.Nothing,
                  lambdaFunctionStartedEventAttributes = Core.Nothing,
                  lambdaFunctionTimedOutEventAttributes = Core.Nothing,
                  markerRecordedEventAttributes = Core.Nothing,
                  recordMarkerFailedEventAttributes = Core.Nothing,
                  requestCancelActivityTaskFailedEventAttributes = Core.Nothing,
                  requestCancelExternalWorkflowExecutionFailedEventAttributes =
                    Core.Nothing,
                  requestCancelExternalWorkflowExecutionInitiatedEventAttributes =
                    Core.Nothing,
                  scheduleActivityTaskFailedEventAttributes = Core.Nothing,
                  scheduleLambdaFunctionFailedEventAttributes = Core.Nothing,
                  signalExternalWorkflowExecutionFailedEventAttributes =
                    Core.Nothing,
                  signalExternalWorkflowExecutionInitiatedEventAttributes =
                    Core.Nothing,
                  startChildWorkflowExecutionFailedEventAttributes = Core.Nothing,
                  startChildWorkflowExecutionInitiatedEventAttributes = Core.Nothing,
                  startLambdaFunctionFailedEventAttributes = Core.Nothing,
                  startTimerFailedEventAttributes = Core.Nothing,
                  timerCanceledEventAttributes = Core.Nothing,
                  timerFiredEventAttributes = Core.Nothing,
                  timerStartedEventAttributes = Core.Nothing,
                  workflowExecutionCancelRequestedEventAttributes = Core.Nothing,
                  workflowExecutionCanceledEventAttributes = Core.Nothing,
                  workflowExecutionCompletedEventAttributes = Core.Nothing,
                  workflowExecutionContinuedAsNewEventAttributes = Core.Nothing,
                  workflowExecutionFailedEventAttributes = Core.Nothing,
                  workflowExecutionSignaledEventAttributes = Core.Nothing,
                  workflowExecutionStartedEventAttributes = Core.Nothing,
                  workflowExecutionTerminatedEventAttributes = Core.Nothing,
                  workflowExecutionTimedOutEventAttributes = Core.Nothing}

-- | The date and time when the event occurred.
--
-- /Note:/ Consider using 'eventTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heEventTimestamp :: Lens.Lens' HistoryEvent Core.NominalDiffTime
heEventTimestamp = Lens.field @"eventTimestamp"
{-# INLINEABLE heEventTimestamp #-}
{-# DEPRECATED eventTimestamp "Use generic-lens or generic-optics with 'eventTimestamp' instead"  #-}

-- | The type of the history event.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heEventType :: Lens.Lens' HistoryEvent Types.EventType
heEventType = Lens.field @"eventType"
{-# INLINEABLE heEventType #-}
{-# DEPRECATED eventType "Use generic-lens or generic-optics with 'eventType' instead"  #-}

-- | The system generated ID of the event. This ID uniquely identifies the event with in the workflow execution history.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heEventId :: Lens.Lens' HistoryEvent Core.Integer
heEventId = Lens.field @"eventId"
{-# INLINEABLE heEventId #-}
{-# DEPRECATED eventId "Use generic-lens or generic-optics with 'eventId' instead"  #-}

-- | If the event is of type @ActivityTaskcancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskCancelRequestedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityTaskCancelRequestedEventAttributes)
heActivityTaskCancelRequestedEventAttributes = Lens.field @"activityTaskCancelRequestedEventAttributes"
{-# INLINEABLE heActivityTaskCancelRequestedEventAttributes #-}
{-# DEPRECATED activityTaskCancelRequestedEventAttributes "Use generic-lens or generic-optics with 'activityTaskCancelRequestedEventAttributes' instead"  #-}

-- | If the event is of type @ActivityTaskCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskCanceledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskCanceledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityTaskCanceledEventAttributes)
heActivityTaskCanceledEventAttributes = Lens.field @"activityTaskCanceledEventAttributes"
{-# INLINEABLE heActivityTaskCanceledEventAttributes #-}
{-# DEPRECATED activityTaskCanceledEventAttributes "Use generic-lens or generic-optics with 'activityTaskCanceledEventAttributes' instead"  #-}

-- | If the event is of type @ActivityTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskCompletedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskCompletedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityTaskCompletedEventAttributes)
heActivityTaskCompletedEventAttributes = Lens.field @"activityTaskCompletedEventAttributes"
{-# INLINEABLE heActivityTaskCompletedEventAttributes #-}
{-# DEPRECATED activityTaskCompletedEventAttributes "Use generic-lens or generic-optics with 'activityTaskCompletedEventAttributes' instead"  #-}

-- | If the event is of type @ActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityTaskFailedEventAttributes)
heActivityTaskFailedEventAttributes = Lens.field @"activityTaskFailedEventAttributes"
{-# INLINEABLE heActivityTaskFailedEventAttributes #-}
{-# DEPRECATED activityTaskFailedEventAttributes "Use generic-lens or generic-optics with 'activityTaskFailedEventAttributes' instead"  #-}

-- | If the event is of type @ActivityTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskScheduledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskScheduledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityTaskScheduledEventAttributes)
heActivityTaskScheduledEventAttributes = Lens.field @"activityTaskScheduledEventAttributes"
{-# INLINEABLE heActivityTaskScheduledEventAttributes #-}
{-# DEPRECATED activityTaskScheduledEventAttributes "Use generic-lens or generic-optics with 'activityTaskScheduledEventAttributes' instead"  #-}

-- | If the event is of type @ActivityTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityTaskStartedEventAttributes)
heActivityTaskStartedEventAttributes = Lens.field @"activityTaskStartedEventAttributes"
{-# INLINEABLE heActivityTaskStartedEventAttributes #-}
{-# DEPRECATED activityTaskStartedEventAttributes "Use generic-lens or generic-optics with 'activityTaskStartedEventAttributes' instead"  #-}

-- | If the event is of type @ActivityTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'activityTaskTimedOutEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heActivityTaskTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ActivityTaskTimedOutEventAttributes)
heActivityTaskTimedOutEventAttributes = Lens.field @"activityTaskTimedOutEventAttributes"
{-# INLINEABLE heActivityTaskTimedOutEventAttributes #-}
{-# DEPRECATED activityTaskTimedOutEventAttributes "Use generic-lens or generic-optics with 'activityTaskTimedOutEventAttributes' instead"  #-}

-- | If the event is of type @CancelTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'cancelTimerFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heCancelTimerFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.CancelTimerFailedEventAttributes)
heCancelTimerFailedEventAttributes = Lens.field @"cancelTimerFailedEventAttributes"
{-# INLINEABLE heCancelTimerFailedEventAttributes #-}
{-# DEPRECATED cancelTimerFailedEventAttributes "Use generic-lens or generic-optics with 'cancelTimerFailedEventAttributes' instead"  #-}

-- | If the event is of type @CancelWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'cancelWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heCancelWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.CancelWorkflowExecutionFailedEventAttributes)
heCancelWorkflowExecutionFailedEventAttributes = Lens.field @"cancelWorkflowExecutionFailedEventAttributes"
{-# INLINEABLE heCancelWorkflowExecutionFailedEventAttributes #-}
{-# DEPRECATED cancelWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'cancelWorkflowExecutionFailedEventAttributes' instead"  #-}

-- | If the event is of type @ChildWorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionCanceledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionCanceledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ChildWorkflowExecutionCanceledEventAttributes)
heChildWorkflowExecutionCanceledEventAttributes = Lens.field @"childWorkflowExecutionCanceledEventAttributes"
{-# INLINEABLE heChildWorkflowExecutionCanceledEventAttributes #-}
{-# DEPRECATED childWorkflowExecutionCanceledEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionCanceledEventAttributes' instead"  #-}

-- | If the event is of type @ChildWorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionCompletedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ChildWorkflowExecutionCompletedEventAttributes)
heChildWorkflowExecutionCompletedEventAttributes = Lens.field @"childWorkflowExecutionCompletedEventAttributes"
{-# INLINEABLE heChildWorkflowExecutionCompletedEventAttributes #-}
{-# DEPRECATED childWorkflowExecutionCompletedEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionCompletedEventAttributes' instead"  #-}

-- | If the event is of type @ChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ChildWorkflowExecutionFailedEventAttributes)
heChildWorkflowExecutionFailedEventAttributes = Lens.field @"childWorkflowExecutionFailedEventAttributes"
{-# INLINEABLE heChildWorkflowExecutionFailedEventAttributes #-}
{-# DEPRECATED childWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionFailedEventAttributes' instead"  #-}

-- | If the event is of type @ChildWorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ChildWorkflowExecutionStartedEventAttributes)
heChildWorkflowExecutionStartedEventAttributes = Lens.field @"childWorkflowExecutionStartedEventAttributes"
{-# INLINEABLE heChildWorkflowExecutionStartedEventAttributes #-}
{-# DEPRECATED childWorkflowExecutionStartedEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionStartedEventAttributes' instead"  #-}

-- | If the event is of type @ChildWorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionTerminatedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionTerminatedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ChildWorkflowExecutionTerminatedEventAttributes)
heChildWorkflowExecutionTerminatedEventAttributes = Lens.field @"childWorkflowExecutionTerminatedEventAttributes"
{-# INLINEABLE heChildWorkflowExecutionTerminatedEventAttributes #-}
{-# DEPRECATED childWorkflowExecutionTerminatedEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionTerminatedEventAttributes' instead"  #-}

-- | If the event is of type @ChildWorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'childWorkflowExecutionTimedOutEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heChildWorkflowExecutionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ChildWorkflowExecutionTimedOutEventAttributes)
heChildWorkflowExecutionTimedOutEventAttributes = Lens.field @"childWorkflowExecutionTimedOutEventAttributes"
{-# INLINEABLE heChildWorkflowExecutionTimedOutEventAttributes #-}
{-# DEPRECATED childWorkflowExecutionTimedOutEventAttributes "Use generic-lens or generic-optics with 'childWorkflowExecutionTimedOutEventAttributes' instead"  #-}

-- | If the event is of type @CompleteWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'completeWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heCompleteWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.CompleteWorkflowExecutionFailedEventAttributes)
heCompleteWorkflowExecutionFailedEventAttributes = Lens.field @"completeWorkflowExecutionFailedEventAttributes"
{-# INLINEABLE heCompleteWorkflowExecutionFailedEventAttributes #-}
{-# DEPRECATED completeWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'completeWorkflowExecutionFailedEventAttributes' instead"  #-}

-- | If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'continueAsNewWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heContinueAsNewWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ContinueAsNewWorkflowExecutionFailedEventAttributes)
heContinueAsNewWorkflowExecutionFailedEventAttributes = Lens.field @"continueAsNewWorkflowExecutionFailedEventAttributes"
{-# INLINEABLE heContinueAsNewWorkflowExecutionFailedEventAttributes #-}
{-# DEPRECATED continueAsNewWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'continueAsNewWorkflowExecutionFailedEventAttributes' instead"  #-}

-- | If the event is of type @DecisionTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heDecisionTaskCompletedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.DecisionTaskCompletedEventAttributes)
heDecisionTaskCompletedEventAttributes = Lens.field @"decisionTaskCompletedEventAttributes"
{-# INLINEABLE heDecisionTaskCompletedEventAttributes #-}
{-# DEPRECATED decisionTaskCompletedEventAttributes "Use generic-lens or generic-optics with 'decisionTaskCompletedEventAttributes' instead"  #-}

-- | If the event is of type @DecisionTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'decisionTaskScheduledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heDecisionTaskScheduledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.DecisionTaskScheduledEventAttributes)
heDecisionTaskScheduledEventAttributes = Lens.field @"decisionTaskScheduledEventAttributes"
{-# INLINEABLE heDecisionTaskScheduledEventAttributes #-}
{-# DEPRECATED decisionTaskScheduledEventAttributes "Use generic-lens or generic-optics with 'decisionTaskScheduledEventAttributes' instead"  #-}

-- | If the event is of type @DecisionTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'decisionTaskStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heDecisionTaskStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.DecisionTaskStartedEventAttributes)
heDecisionTaskStartedEventAttributes = Lens.field @"decisionTaskStartedEventAttributes"
{-# INLINEABLE heDecisionTaskStartedEventAttributes #-}
{-# DEPRECATED decisionTaskStartedEventAttributes "Use generic-lens or generic-optics with 'decisionTaskStartedEventAttributes' instead"  #-}

-- | If the event is of type @DecisionTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'decisionTaskTimedOutEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heDecisionTaskTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.DecisionTaskTimedOutEventAttributes)
heDecisionTaskTimedOutEventAttributes = Lens.field @"decisionTaskTimedOutEventAttributes"
{-# INLINEABLE heDecisionTaskTimedOutEventAttributes #-}
{-# DEPRECATED decisionTaskTimedOutEventAttributes "Use generic-lens or generic-optics with 'decisionTaskTimedOutEventAttributes' instead"  #-}

-- | If the event is of type @ExternalWorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types. 
--
-- /Note:/ Consider using 'externalWorkflowExecutionCancelRequestedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExternalWorkflowExecutionCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ExternalWorkflowExecutionCancelRequestedEventAttributes)
heExternalWorkflowExecutionCancelRequestedEventAttributes = Lens.field @"externalWorkflowExecutionCancelRequestedEventAttributes"
{-# INLINEABLE heExternalWorkflowExecutionCancelRequestedEventAttributes #-}
{-# DEPRECATED externalWorkflowExecutionCancelRequestedEventAttributes "Use generic-lens or generic-optics with 'externalWorkflowExecutionCancelRequestedEventAttributes' instead"  #-}

-- | If the event is of type @ExternalWorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'externalWorkflowExecutionSignaledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heExternalWorkflowExecutionSignaledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ExternalWorkflowExecutionSignaledEventAttributes)
heExternalWorkflowExecutionSignaledEventAttributes = Lens.field @"externalWorkflowExecutionSignaledEventAttributes"
{-# INLINEABLE heExternalWorkflowExecutionSignaledEventAttributes #-}
{-# DEPRECATED externalWorkflowExecutionSignaledEventAttributes "Use generic-lens or generic-optics with 'externalWorkflowExecutionSignaledEventAttributes' instead"  #-}

-- | If the event is of type @FailWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'failWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heFailWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.FailWorkflowExecutionFailedEventAttributes)
heFailWorkflowExecutionFailedEventAttributes = Lens.field @"failWorkflowExecutionFailedEventAttributes"
{-# INLINEABLE heFailWorkflowExecutionFailedEventAttributes #-}
{-# DEPRECATED failWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'failWorkflowExecutionFailedEventAttributes' instead"  #-}

-- | Provides the details of the @LambdaFunctionCompleted@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'lambdaFunctionCompletedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionCompletedEventAttributes)
heLambdaFunctionCompletedEventAttributes = Lens.field @"lambdaFunctionCompletedEventAttributes"
{-# INLINEABLE heLambdaFunctionCompletedEventAttributes #-}
{-# DEPRECATED lambdaFunctionCompletedEventAttributes "Use generic-lens or generic-optics with 'lambdaFunctionCompletedEventAttributes' instead"  #-}

-- | Provides the details of the @LambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'lambdaFunctionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionFailedEventAttributes)
heLambdaFunctionFailedEventAttributes = Lens.field @"lambdaFunctionFailedEventAttributes"
{-# INLINEABLE heLambdaFunctionFailedEventAttributes #-}
{-# DEPRECATED lambdaFunctionFailedEventAttributes "Use generic-lens or generic-optics with 'lambdaFunctionFailedEventAttributes' instead"  #-}

-- | Provides the details of the @LambdaFunctionScheduled@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'lambdaFunctionScheduledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionScheduledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionScheduledEventAttributes)
heLambdaFunctionScheduledEventAttributes = Lens.field @"lambdaFunctionScheduledEventAttributes"
{-# INLINEABLE heLambdaFunctionScheduledEventAttributes #-}
{-# DEPRECATED lambdaFunctionScheduledEventAttributes "Use generic-lens or generic-optics with 'lambdaFunctionScheduledEventAttributes' instead"  #-}

-- | Provides the details of the @LambdaFunctionStarted@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'lambdaFunctionStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionStartedEventAttributes)
heLambdaFunctionStartedEventAttributes = Lens.field @"lambdaFunctionStartedEventAttributes"
{-# INLINEABLE heLambdaFunctionStartedEventAttributes #-}
{-# DEPRECATED lambdaFunctionStartedEventAttributes "Use generic-lens or generic-optics with 'lambdaFunctionStartedEventAttributes' instead"  #-}

-- | Provides the details of the @LambdaFunctionTimedOut@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'lambdaFunctionTimedOutEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heLambdaFunctionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.LambdaFunctionTimedOutEventAttributes)
heLambdaFunctionTimedOutEventAttributes = Lens.field @"lambdaFunctionTimedOutEventAttributes"
{-# INLINEABLE heLambdaFunctionTimedOutEventAttributes #-}
{-# DEPRECATED lambdaFunctionTimedOutEventAttributes "Use generic-lens or generic-optics with 'lambdaFunctionTimedOutEventAttributes' instead"  #-}

-- | If the event is of type @MarkerRecorded@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'markerRecordedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heMarkerRecordedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.MarkerRecordedEventAttributes)
heMarkerRecordedEventAttributes = Lens.field @"markerRecordedEventAttributes"
{-# INLINEABLE heMarkerRecordedEventAttributes #-}
{-# DEPRECATED markerRecordedEventAttributes "Use generic-lens or generic-optics with 'markerRecordedEventAttributes' instead"  #-}

-- | If the event is of type @DecisionTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'recordMarkerFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heRecordMarkerFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.RecordMarkerFailedEventAttributes)
heRecordMarkerFailedEventAttributes = Lens.field @"recordMarkerFailedEventAttributes"
{-# INLINEABLE heRecordMarkerFailedEventAttributes #-}
{-# DEPRECATED recordMarkerFailedEventAttributes "Use generic-lens or generic-optics with 'recordMarkerFailedEventAttributes' instead"  #-}

-- | If the event is of type @RequestCancelActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'requestCancelActivityTaskFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heRequestCancelActivityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.RequestCancelActivityTaskFailedEventAttributes)
heRequestCancelActivityTaskFailedEventAttributes = Lens.field @"requestCancelActivityTaskFailedEventAttributes"
{-# INLINEABLE heRequestCancelActivityTaskFailedEventAttributes #-}
{-# DEPRECATED requestCancelActivityTaskFailedEventAttributes "Use generic-lens or generic-optics with 'requestCancelActivityTaskFailedEventAttributes' instead"  #-}

-- | If the event is of type @RequestCancelExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'requestCancelExternalWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.RequestCancelExternalWorkflowExecutionFailedEventAttributes)
heRequestCancelExternalWorkflowExecutionFailedEventAttributes = Lens.field @"requestCancelExternalWorkflowExecutionFailedEventAttributes"
{-# INLINEABLE heRequestCancelExternalWorkflowExecutionFailedEventAttributes #-}
{-# DEPRECATED requestCancelExternalWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'requestCancelExternalWorkflowExecutionFailedEventAttributes' instead"  #-}

-- | If the event is of type @RequestCancelExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = Lens.field @"requestCancelExternalWorkflowExecutionInitiatedEventAttributes"
{-# INLINEABLE heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes #-}
{-# DEPRECATED requestCancelExternalWorkflowExecutionInitiatedEventAttributes "Use generic-lens or generic-optics with 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes' instead"  #-}

-- | If the event is of type @ScheduleActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'scheduleActivityTaskFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heScheduleActivityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ScheduleActivityTaskFailedEventAttributes)
heScheduleActivityTaskFailedEventAttributes = Lens.field @"scheduleActivityTaskFailedEventAttributes"
{-# INLINEABLE heScheduleActivityTaskFailedEventAttributes #-}
{-# DEPRECATED scheduleActivityTaskFailedEventAttributes "Use generic-lens or generic-optics with 'scheduleActivityTaskFailedEventAttributes' instead"  #-}

-- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'scheduleLambdaFunctionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heScheduleLambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.ScheduleLambdaFunctionFailedEventAttributes)
heScheduleLambdaFunctionFailedEventAttributes = Lens.field @"scheduleLambdaFunctionFailedEventAttributes"
{-# INLINEABLE heScheduleLambdaFunctionFailedEventAttributes #-}
{-# DEPRECATED scheduleLambdaFunctionFailedEventAttributes "Use generic-lens or generic-optics with 'scheduleLambdaFunctionFailedEventAttributes' instead"  #-}

-- | If the event is of type @SignalExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'signalExternalWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heSignalExternalWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.SignalExternalWorkflowExecutionFailedEventAttributes)
heSignalExternalWorkflowExecutionFailedEventAttributes = Lens.field @"signalExternalWorkflowExecutionFailedEventAttributes"
{-# INLINEABLE heSignalExternalWorkflowExecutionFailedEventAttributes #-}
{-# DEPRECATED signalExternalWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'signalExternalWorkflowExecutionFailedEventAttributes' instead"  #-}

-- | If the event is of type @SignalExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'signalExternalWorkflowExecutionInitiatedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heSignalExternalWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.SignalExternalWorkflowExecutionInitiatedEventAttributes)
heSignalExternalWorkflowExecutionInitiatedEventAttributes = Lens.field @"signalExternalWorkflowExecutionInitiatedEventAttributes"
{-# INLINEABLE heSignalExternalWorkflowExecutionInitiatedEventAttributes #-}
{-# DEPRECATED signalExternalWorkflowExecutionInitiatedEventAttributes "Use generic-lens or generic-optics with 'signalExternalWorkflowExecutionInitiatedEventAttributes' instead"  #-}

-- | If the event is of type @StartChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'startChildWorkflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStartChildWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.StartChildWorkflowExecutionFailedEventAttributes)
heStartChildWorkflowExecutionFailedEventAttributes = Lens.field @"startChildWorkflowExecutionFailedEventAttributes"
{-# INLINEABLE heStartChildWorkflowExecutionFailedEventAttributes #-}
{-# DEPRECATED startChildWorkflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'startChildWorkflowExecutionFailedEventAttributes' instead"  #-}

-- | If the event is of type @StartChildWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'startChildWorkflowExecutionInitiatedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStartChildWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.StartChildWorkflowExecutionInitiatedEventAttributes)
heStartChildWorkflowExecutionInitiatedEventAttributes = Lens.field @"startChildWorkflowExecutionInitiatedEventAttributes"
{-# INLINEABLE heStartChildWorkflowExecutionInitiatedEventAttributes #-}
{-# DEPRECATED startChildWorkflowExecutionInitiatedEventAttributes "Use generic-lens or generic-optics with 'startChildWorkflowExecutionInitiatedEventAttributes' instead"  #-}

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn't set for other event types.
--
-- /Note:/ Consider using 'startLambdaFunctionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStartLambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.StartLambdaFunctionFailedEventAttributes)
heStartLambdaFunctionFailedEventAttributes = Lens.field @"startLambdaFunctionFailedEventAttributes"
{-# INLINEABLE heStartLambdaFunctionFailedEventAttributes #-}
{-# DEPRECATED startLambdaFunctionFailedEventAttributes "Use generic-lens or generic-optics with 'startLambdaFunctionFailedEventAttributes' instead"  #-}

-- | If the event is of type @StartTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'startTimerFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heStartTimerFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.StartTimerFailedEventAttributes)
heStartTimerFailedEventAttributes = Lens.field @"startTimerFailedEventAttributes"
{-# INLINEABLE heStartTimerFailedEventAttributes #-}
{-# DEPRECATED startTimerFailedEventAttributes "Use generic-lens or generic-optics with 'startTimerFailedEventAttributes' instead"  #-}

-- | If the event is of type @TimerCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'timerCanceledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTimerCanceledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.TimerCanceledEventAttributes)
heTimerCanceledEventAttributes = Lens.field @"timerCanceledEventAttributes"
{-# INLINEABLE heTimerCanceledEventAttributes #-}
{-# DEPRECATED timerCanceledEventAttributes "Use generic-lens or generic-optics with 'timerCanceledEventAttributes' instead"  #-}

-- | If the event is of type @TimerFired@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'timerFiredEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTimerFiredEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.TimerFiredEventAttributes)
heTimerFiredEventAttributes = Lens.field @"timerFiredEventAttributes"
{-# INLINEABLE heTimerFiredEventAttributes #-}
{-# DEPRECATED timerFiredEventAttributes "Use generic-lens or generic-optics with 'timerFiredEventAttributes' instead"  #-}

-- | If the event is of type @TimerStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'timerStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heTimerStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.TimerStartedEventAttributes)
heTimerStartedEventAttributes = Lens.field @"timerStartedEventAttributes"
{-# INLINEABLE heTimerStartedEventAttributes #-}
{-# DEPRECATED timerStartedEventAttributes "Use generic-lens or generic-optics with 'timerStartedEventAttributes' instead"  #-}

-- | If the event is of type @WorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionCancelRequestedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.WorkflowExecutionCancelRequestedEventAttributes)
heWorkflowExecutionCancelRequestedEventAttributes = Lens.field @"workflowExecutionCancelRequestedEventAttributes"
{-# INLINEABLE heWorkflowExecutionCancelRequestedEventAttributes #-}
{-# DEPRECATED workflowExecutionCancelRequestedEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionCancelRequestedEventAttributes' instead"  #-}

-- | If the event is of type @WorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionCanceledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionCanceledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.WorkflowExecutionCanceledEventAttributes)
heWorkflowExecutionCanceledEventAttributes = Lens.field @"workflowExecutionCanceledEventAttributes"
{-# INLINEABLE heWorkflowExecutionCanceledEventAttributes #-}
{-# DEPRECATED workflowExecutionCanceledEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionCanceledEventAttributes' instead"  #-}

-- | If the event is of type @WorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionCompletedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.WorkflowExecutionCompletedEventAttributes)
heWorkflowExecutionCompletedEventAttributes = Lens.field @"workflowExecutionCompletedEventAttributes"
{-# INLINEABLE heWorkflowExecutionCompletedEventAttributes #-}
{-# DEPRECATED workflowExecutionCompletedEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionCompletedEventAttributes' instead"  #-}

-- | If the event is of type @WorkflowExecutionContinuedAsNew@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionContinuedAsNewEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionContinuedAsNewEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.WorkflowExecutionContinuedAsNewEventAttributes)
heWorkflowExecutionContinuedAsNewEventAttributes = Lens.field @"workflowExecutionContinuedAsNewEventAttributes"
{-# INLINEABLE heWorkflowExecutionContinuedAsNewEventAttributes #-}
{-# DEPRECATED workflowExecutionContinuedAsNewEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionContinuedAsNewEventAttributes' instead"  #-}

-- | If the event is of type @WorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionFailedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.WorkflowExecutionFailedEventAttributes)
heWorkflowExecutionFailedEventAttributes = Lens.field @"workflowExecutionFailedEventAttributes"
{-# INLINEABLE heWorkflowExecutionFailedEventAttributes #-}
{-# DEPRECATED workflowExecutionFailedEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionFailedEventAttributes' instead"  #-}

-- | If the event is of type @WorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionSignaledEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionSignaledEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.WorkflowExecutionSignaledEventAttributes)
heWorkflowExecutionSignaledEventAttributes = Lens.field @"workflowExecutionSignaledEventAttributes"
{-# INLINEABLE heWorkflowExecutionSignaledEventAttributes #-}
{-# DEPRECATED workflowExecutionSignaledEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionSignaledEventAttributes' instead"  #-}

-- | If the event is of type @WorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionStartedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionStartedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.WorkflowExecutionStartedEventAttributes)
heWorkflowExecutionStartedEventAttributes = Lens.field @"workflowExecutionStartedEventAttributes"
{-# INLINEABLE heWorkflowExecutionStartedEventAttributes #-}
{-# DEPRECATED workflowExecutionStartedEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionStartedEventAttributes' instead"  #-}

-- | If the event is of type @WorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionTerminatedEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionTerminatedEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.WorkflowExecutionTerminatedEventAttributes)
heWorkflowExecutionTerminatedEventAttributes = Lens.field @"workflowExecutionTerminatedEventAttributes"
{-# INLINEABLE heWorkflowExecutionTerminatedEventAttributes #-}
{-# DEPRECATED workflowExecutionTerminatedEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionTerminatedEventAttributes' instead"  #-}

-- | If the event is of type @WorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- /Note:/ Consider using 'workflowExecutionTimedOutEventAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heWorkflowExecutionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Core.Maybe Types.WorkflowExecutionTimedOutEventAttributes)
heWorkflowExecutionTimedOutEventAttributes = Lens.field @"workflowExecutionTimedOutEventAttributes"
{-# INLINEABLE heWorkflowExecutionTimedOutEventAttributes #-}
{-# DEPRECATED workflowExecutionTimedOutEventAttributes "Use generic-lens or generic-optics with 'workflowExecutionTimedOutEventAttributes' instead"  #-}

instance Core.FromJSON HistoryEvent where
        parseJSON
          = Core.withObject "HistoryEvent" Core.$
              \ x ->
                HistoryEvent' Core.<$>
                  (x Core..: "eventTimestamp") Core.<*> x Core..: "eventType"
                    Core.<*> x Core..: "eventId"
                    Core.<*> x Core..:? "activityTaskCancelRequestedEventAttributes"
                    Core.<*> x Core..:? "activityTaskCanceledEventAttributes"
                    Core.<*> x Core..:? "activityTaskCompletedEventAttributes"
                    Core.<*> x Core..:? "activityTaskFailedEventAttributes"
                    Core.<*> x Core..:? "activityTaskScheduledEventAttributes"
                    Core.<*> x Core..:? "activityTaskStartedEventAttributes"
                    Core.<*> x Core..:? "activityTaskTimedOutEventAttributes"
                    Core.<*> x Core..:? "cancelTimerFailedEventAttributes"
                    Core.<*> x Core..:? "cancelWorkflowExecutionFailedEventAttributes"
                    Core.<*> x Core..:? "childWorkflowExecutionCanceledEventAttributes"
                    Core.<*>
                    x Core..:? "childWorkflowExecutionCompletedEventAttributes"
                    Core.<*> x Core..:? "childWorkflowExecutionFailedEventAttributes"
                    Core.<*> x Core..:? "childWorkflowExecutionStartedEventAttributes"
                    Core.<*>
                    x Core..:? "childWorkflowExecutionTerminatedEventAttributes"
                    Core.<*> x Core..:? "childWorkflowExecutionTimedOutEventAttributes"
                    Core.<*>
                    x Core..:? "completeWorkflowExecutionFailedEventAttributes"
                    Core.<*>
                    x Core..:? "continueAsNewWorkflowExecutionFailedEventAttributes"
                    Core.<*> x Core..:? "decisionTaskCompletedEventAttributes"
                    Core.<*> x Core..:? "decisionTaskScheduledEventAttributes"
                    Core.<*> x Core..:? "decisionTaskStartedEventAttributes"
                    Core.<*> x Core..:? "decisionTaskTimedOutEventAttributes"
                    Core.<*>
                    x Core..:?
                      "externalWorkflowExecutionCancelRequestedEventAttributes"
                    Core.<*>
                    x Core..:? "externalWorkflowExecutionSignaledEventAttributes"
                    Core.<*> x Core..:? "failWorkflowExecutionFailedEventAttributes"
                    Core.<*> x Core..:? "lambdaFunctionCompletedEventAttributes"
                    Core.<*> x Core..:? "lambdaFunctionFailedEventAttributes"
                    Core.<*> x Core..:? "lambdaFunctionScheduledEventAttributes"
                    Core.<*> x Core..:? "lambdaFunctionStartedEventAttributes"
                    Core.<*> x Core..:? "lambdaFunctionTimedOutEventAttributes"
                    Core.<*> x Core..:? "markerRecordedEventAttributes"
                    Core.<*> x Core..:? "recordMarkerFailedEventAttributes"
                    Core.<*>
                    x Core..:? "requestCancelActivityTaskFailedEventAttributes"
                    Core.<*>
                    x Core..:?
                      "requestCancelExternalWorkflowExecutionFailedEventAttributes"
                    Core.<*>
                    x Core..:?
                      "requestCancelExternalWorkflowExecutionInitiatedEventAttributes"
                    Core.<*> x Core..:? "scheduleActivityTaskFailedEventAttributes"
                    Core.<*> x Core..:? "scheduleLambdaFunctionFailedEventAttributes"
                    Core.<*>
                    x Core..:? "signalExternalWorkflowExecutionFailedEventAttributes"
                    Core.<*>
                    x Core..:?
                      "signalExternalWorkflowExecutionInitiatedEventAttributes"
                    Core.<*>
                    x Core..:? "startChildWorkflowExecutionFailedEventAttributes"
                    Core.<*>
                    x Core..:? "startChildWorkflowExecutionInitiatedEventAttributes"
                    Core.<*> x Core..:? "startLambdaFunctionFailedEventAttributes"
                    Core.<*> x Core..:? "startTimerFailedEventAttributes"
                    Core.<*> x Core..:? "timerCanceledEventAttributes"
                    Core.<*> x Core..:? "timerFiredEventAttributes"
                    Core.<*> x Core..:? "timerStartedEventAttributes"
                    Core.<*>
                    x Core..:? "workflowExecutionCancelRequestedEventAttributes"
                    Core.<*> x Core..:? "workflowExecutionCanceledEventAttributes"
                    Core.<*> x Core..:? "workflowExecutionCompletedEventAttributes"
                    Core.<*>
                    x Core..:? "workflowExecutionContinuedAsNewEventAttributes"
                    Core.<*> x Core..:? "workflowExecutionFailedEventAttributes"
                    Core.<*> x Core..:? "workflowExecutionSignaledEventAttributes"
                    Core.<*> x Core..:? "workflowExecutionStartedEventAttributes"
                    Core.<*> x Core..:? "workflowExecutionTerminatedEventAttributes"
                    Core.<*> x Core..:? "workflowExecutionTimedOutEventAttributes"

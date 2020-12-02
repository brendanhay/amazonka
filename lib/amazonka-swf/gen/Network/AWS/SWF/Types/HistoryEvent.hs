{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.HistoryEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.HistoryEvent where

import Network.AWS.Lens
import Network.AWS.Prelude
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
--     * @ActivityTaskCanceled@ – The activity task was successfully canceled.
--
--     * @ActivityTaskCompleted@ – An activity worker successfully completed an activity task by calling 'RespondActivityTaskCompleted' .
--
--     * @ActivityTaskFailed@ – An activity worker failed an activity task by calling 'RespondActivityTaskFailed' .
--
--     * @ActivityTaskScheduled@ – An activity task was scheduled for execution.
--
--     * @ActivityTaskStarted@ – The scheduled activity task was dispatched to a worker.
--
--     * @ActivityTaskTimedOut@ – The activity task timed out.
--
--     * @CancelTimerFailed@ – Failed to process CancelTimer decision. This happens when the decision isn't configured properly, for example no timer exists with the specified timer Id.
--
--     * @CancelWorkflowExecutionFailed@ – A request to cancel a workflow execution failed.
--
--     * @ChildWorkflowExecutionCanceled@ – A child workflow execution, started by this workflow execution, was canceled and closed.
--
--     * @ChildWorkflowExecutionCompleted@ – A child workflow execution, started by this workflow execution, completed successfully and was closed.
--
--     * @ChildWorkflowExecutionFailed@ – A child workflow execution, started by this workflow execution, failed to complete successfully and was closed.
--
--     * @ChildWorkflowExecutionStarted@ – A child workflow execution was successfully started.
--
--     * @ChildWorkflowExecutionTerminated@ – A child workflow execution, started by this workflow execution, was terminated.
--
--     * @ChildWorkflowExecutionTimedOut@ – A child workflow execution, started by this workflow execution, timed out and was closed.
--
--     * @CompleteWorkflowExecutionFailed@ – The workflow execution failed to complete.
--
--     * @ContinueAsNewWorkflowExecutionFailed@ – The workflow execution failed to complete after being continued as a new workflow execution.
--
--     * @DecisionTaskCompleted@ – The decider successfully completed a decision task by calling 'RespondDecisionTaskCompleted' .
--
--     * @DecisionTaskScheduled@ – A decision task was scheduled for the workflow execution.
--
--     * @DecisionTaskStarted@ – The decision task was dispatched to a decider.
--
--     * @DecisionTaskTimedOut@ – The decision task timed out.
--
--     * @ExternalWorkflowExecutionCancelRequested@ – Request to cancel an external workflow execution was successfully delivered to the target execution.
--
--     * @ExternalWorkflowExecutionSignaled@ – A signal, requested by this workflow execution, was successfully delivered to the target external workflow execution.
--
--     * @FailWorkflowExecutionFailed@ – A request to mark a workflow execution as failed, itself failed.
--
--     * @MarkerRecorded@ – A marker was recorded in the workflow history as the result of a @RecordMarker@ decision.
--
--     * @RecordMarkerFailed@ – A @RecordMarker@ decision was returned as failed.
--
--     * @RequestCancelActivityTaskFailed@ – Failed to process RequestCancelActivityTask decision. This happens when the decision isn't configured properly.
--
--     * @RequestCancelExternalWorkflowExecutionFailed@ – Request to cancel an external workflow execution failed.
--
--     * @RequestCancelExternalWorkflowExecutionInitiated@ – A request was made to request the cancellation of an external workflow execution.
--
--     * @ScheduleActivityTaskFailed@ – Failed to process ScheduleActivityTask decision. This happens when the decision isn't configured properly, for example the activity type specified isn't registered.
--
--     * @SignalExternalWorkflowExecutionFailed@ – The request to signal an external workflow execution failed.
--
--     * @SignalExternalWorkflowExecutionInitiated@ – A request to signal an external workflow was made.
--
--     * @StartActivityTaskFailed@ – A scheduled activity task failed to start.
--
--     * @StartChildWorkflowExecutionFailed@ – Failed to process StartChildWorkflowExecution decision. This happens when the decision isn't configured properly, for example the workflow type specified isn't registered.
--
--     * @StartChildWorkflowExecutionInitiated@ – A request was made to start a child workflow execution.
--
--     * @StartTimerFailed@ – Failed to process StartTimer decision. This happens when the decision isn't configured properly, for example a timer already exists with the specified timer Id.
--
--     * @TimerCanceled@ – A timer, previously started for this workflow execution, was successfully canceled.
--
--     * @TimerFired@ – A timer, previously started for this workflow execution, fired.
--
--     * @TimerStarted@ – A timer was started for the workflow execution due to a @StartTimer@ decision.
--
--     * @WorkflowExecutionCancelRequested@ – A request to cancel this workflow execution was made.
--
--     * @WorkflowExecutionCanceled@ – The workflow execution was successfully canceled and closed.
--
--     * @WorkflowExecutionCompleted@ – The workflow execution was closed due to successful completion.
--
--     * @WorkflowExecutionContinuedAsNew@ – The workflow execution was closed and a new execution of the same type was created with the same workflowId.
--
--     * @WorkflowExecutionFailed@ – The workflow execution closed due to a failure.
--
--     * @WorkflowExecutionSignaled@ – An external signal was received for the workflow execution.
--
--     * @WorkflowExecutionStarted@ – The workflow execution was started.
--
--     * @WorkflowExecutionTerminated@ – The workflow execution was terminated.
--
--     * @WorkflowExecutionTimedOut@ – The workflow execution was closed because a time out was exceeded.
--
--
--
--
-- /See:/ 'historyEvent' smart constructor.
data HistoryEvent = HistoryEvent'
  { _heWorkflowExecutionCancelRequestedEventAttributes ::
      !(Maybe WorkflowExecutionCancelRequestedEventAttributes),
    _heRecordMarkerFailedEventAttributes ::
      !(Maybe RecordMarkerFailedEventAttributes),
    _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes ::
      !( Maybe
           RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
       ),
    _heLambdaFunctionStartedEventAttributes ::
      !(Maybe LambdaFunctionStartedEventAttributes),
    _heDecisionTaskScheduledEventAttributes ::
      !(Maybe DecisionTaskScheduledEventAttributes),
    _heWorkflowExecutionCompletedEventAttributes ::
      !(Maybe WorkflowExecutionCompletedEventAttributes),
    _heStartTimerFailedEventAttributes ::
      !(Maybe StartTimerFailedEventAttributes),
    _heActivityTaskScheduledEventAttributes ::
      !(Maybe ActivityTaskScheduledEventAttributes),
    _heScheduleActivityTaskFailedEventAttributes ::
      !(Maybe ScheduleActivityTaskFailedEventAttributes),
    _heChildWorkflowExecutionCompletedEventAttributes ::
      !(Maybe ChildWorkflowExecutionCompletedEventAttributes),
    _heMarkerRecordedEventAttributes ::
      !(Maybe MarkerRecordedEventAttributes),
    _heScheduleLambdaFunctionFailedEventAttributes ::
      !(Maybe ScheduleLambdaFunctionFailedEventAttributes),
    _heCompleteWorkflowExecutionFailedEventAttributes ::
      !(Maybe CompleteWorkflowExecutionFailedEventAttributes),
    _heLambdaFunctionCompletedEventAttributes ::
      !(Maybe LambdaFunctionCompletedEventAttributes),
    _heRequestCancelExternalWorkflowExecutionFailedEventAttributes ::
      !( Maybe
           RequestCancelExternalWorkflowExecutionFailedEventAttributes
       ),
    _heTimerCanceledEventAttributes ::
      !(Maybe TimerCanceledEventAttributes),
    _heWorkflowExecutionStartedEventAttributes ::
      !(Maybe WorkflowExecutionStartedEventAttributes),
    _heActivityTaskCompletedEventAttributes ::
      !(Maybe ActivityTaskCompletedEventAttributes),
    _heDecisionTaskTimedOutEventAttributes ::
      !(Maybe DecisionTaskTimedOutEventAttributes),
    _heCancelTimerFailedEventAttributes ::
      !(Maybe CancelTimerFailedEventAttributes),
    _heChildWorkflowExecutionStartedEventAttributes ::
      !(Maybe ChildWorkflowExecutionStartedEventAttributes),
    _heActivityTaskCanceledEventAttributes ::
      !(Maybe ActivityTaskCanceledEventAttributes),
    _heActivityTaskTimedOutEventAttributes ::
      !(Maybe ActivityTaskTimedOutEventAttributes),
    _heDecisionTaskStartedEventAttributes ::
      !(Maybe DecisionTaskStartedEventAttributes),
    _heWorkflowExecutionTerminatedEventAttributes ::
      !(Maybe WorkflowExecutionTerminatedEventAttributes),
    _heChildWorkflowExecutionCanceledEventAttributes ::
      !(Maybe ChildWorkflowExecutionCanceledEventAttributes),
    _heRequestCancelActivityTaskFailedEventAttributes ::
      !(Maybe RequestCancelActivityTaskFailedEventAttributes),
    _heLambdaFunctionScheduledEventAttributes ::
      !(Maybe LambdaFunctionScheduledEventAttributes),
    _heChildWorkflowExecutionTimedOutEventAttributes ::
      !(Maybe ChildWorkflowExecutionTimedOutEventAttributes),
    _heCancelWorkflowExecutionFailedEventAttributes ::
      !(Maybe CancelWorkflowExecutionFailedEventAttributes),
    _heStartChildWorkflowExecutionInitiatedEventAttributes ::
      !(Maybe StartChildWorkflowExecutionInitiatedEventAttributes),
    _heSignalExternalWorkflowExecutionFailedEventAttributes ::
      !(Maybe SignalExternalWorkflowExecutionFailedEventAttributes),
    _heActivityTaskStartedEventAttributes ::
      !(Maybe ActivityTaskStartedEventAttributes),
    _heStartLambdaFunctionFailedEventAttributes ::
      !(Maybe StartLambdaFunctionFailedEventAttributes),
    _heChildWorkflowExecutionTerminatedEventAttributes ::
      !(Maybe ChildWorkflowExecutionTerminatedEventAttributes),
    _heLambdaFunctionFailedEventAttributes ::
      !(Maybe LambdaFunctionFailedEventAttributes),
    _heWorkflowExecutionCanceledEventAttributes ::
      !(Maybe WorkflowExecutionCanceledEventAttributes),
    _heTimerStartedEventAttributes ::
      !(Maybe TimerStartedEventAttributes),
    _heActivityTaskCancelRequestedEventAttributes ::
      !(Maybe ActivityTaskCancelRequestedEventAttributes),
    _heWorkflowExecutionTimedOutEventAttributes ::
      !(Maybe WorkflowExecutionTimedOutEventAttributes),
    _heWorkflowExecutionSignaledEventAttributes ::
      !(Maybe WorkflowExecutionSignaledEventAttributes),
    _heTimerFiredEventAttributes ::
      !(Maybe TimerFiredEventAttributes),
    _heActivityTaskFailedEventAttributes ::
      !(Maybe ActivityTaskFailedEventAttributes),
    _heExternalWorkflowExecutionSignaledEventAttributes ::
      !(Maybe ExternalWorkflowExecutionSignaledEventAttributes),
    _heDecisionTaskCompletedEventAttributes ::
      !(Maybe DecisionTaskCompletedEventAttributes),
    _heStartChildWorkflowExecutionFailedEventAttributes ::
      !(Maybe StartChildWorkflowExecutionFailedEventAttributes),
    _heChildWorkflowExecutionFailedEventAttributes ::
      !(Maybe ChildWorkflowExecutionFailedEventAttributes),
    _heFailWorkflowExecutionFailedEventAttributes ::
      !(Maybe FailWorkflowExecutionFailedEventAttributes),
    _heContinueAsNewWorkflowExecutionFailedEventAttributes ::
      !(Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes),
    _heSignalExternalWorkflowExecutionInitiatedEventAttributes ::
      !(Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes),
    _heLambdaFunctionTimedOutEventAttributes ::
      !(Maybe LambdaFunctionTimedOutEventAttributes),
    _heWorkflowExecutionFailedEventAttributes ::
      !(Maybe WorkflowExecutionFailedEventAttributes),
    _heWorkflowExecutionContinuedAsNewEventAttributes ::
      !(Maybe WorkflowExecutionContinuedAsNewEventAttributes),
    _heExternalWorkflowExecutionCancelRequestedEventAttributes ::
      !(Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes),
    _heEventTimestamp :: !POSIX,
    _heEventType :: !EventType,
    _heEventId :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HistoryEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'heWorkflowExecutionCancelRequestedEventAttributes' - If the event is of type @WorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heRecordMarkerFailedEventAttributes' - If the event is of type @DecisionTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes' - If the event is of type @RequestCancelExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heLambdaFunctionStartedEventAttributes' - Provides the details of the @LambdaFunctionStarted@ event. It isn't set for other event types.
--
-- * 'heDecisionTaskScheduledEventAttributes' - If the event is of type @DecisionTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heWorkflowExecutionCompletedEventAttributes' - If the event is of type @WorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heStartTimerFailedEventAttributes' - If the event is of type @StartTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heActivityTaskScheduledEventAttributes' - If the event is of type @ActivityTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heScheduleActivityTaskFailedEventAttributes' - If the event is of type @ScheduleActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heChildWorkflowExecutionCompletedEventAttributes' - If the event is of type @ChildWorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heMarkerRecordedEventAttributes' - If the event is of type @MarkerRecorded@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heScheduleLambdaFunctionFailedEventAttributes' - Provides the details of the @ScheduleLambdaFunctionFailed@ event. It isn't set for other event types.
--
-- * 'heCompleteWorkflowExecutionFailedEventAttributes' - If the event is of type @CompleteWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heLambdaFunctionCompletedEventAttributes' - Provides the details of the @LambdaFunctionCompleted@ event. It isn't set for other event types.
--
-- * 'heRequestCancelExternalWorkflowExecutionFailedEventAttributes' - If the event is of type @RequestCancelExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heTimerCanceledEventAttributes' - If the event is of type @TimerCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heWorkflowExecutionStartedEventAttributes' - If the event is of type @WorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heActivityTaskCompletedEventAttributes' - If the event is of type @ActivityTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heDecisionTaskTimedOutEventAttributes' - If the event is of type @DecisionTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heCancelTimerFailedEventAttributes' - If the event is of type @CancelTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heChildWorkflowExecutionStartedEventAttributes' - If the event is of type @ChildWorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heActivityTaskCanceledEventAttributes' - If the event is of type @ActivityTaskCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heActivityTaskTimedOutEventAttributes' - If the event is of type @ActivityTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heDecisionTaskStartedEventAttributes' - If the event is of type @DecisionTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heWorkflowExecutionTerminatedEventAttributes' - If the event is of type @WorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heChildWorkflowExecutionCanceledEventAttributes' - If the event is of type @ChildWorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heRequestCancelActivityTaskFailedEventAttributes' - If the event is of type @RequestCancelActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heLambdaFunctionScheduledEventAttributes' - Provides the details of the @LambdaFunctionScheduled@ event. It isn't set for other event types.
--
-- * 'heChildWorkflowExecutionTimedOutEventAttributes' - If the event is of type @ChildWorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heCancelWorkflowExecutionFailedEventAttributes' - If the event is of type @CancelWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heStartChildWorkflowExecutionInitiatedEventAttributes' - If the event is of type @StartChildWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heSignalExternalWorkflowExecutionFailedEventAttributes' - If the event is of type @SignalExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heActivityTaskStartedEventAttributes' - If the event is of type @ActivityTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heStartLambdaFunctionFailedEventAttributes' - Provides the details of the @StartLambdaFunctionFailed@ event. It isn't set for other event types.
--
-- * 'heChildWorkflowExecutionTerminatedEventAttributes' - If the event is of type @ChildWorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heLambdaFunctionFailedEventAttributes' - Provides the details of the @LambdaFunctionFailed@ event. It isn't set for other event types.
--
-- * 'heWorkflowExecutionCanceledEventAttributes' - If the event is of type @WorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heTimerStartedEventAttributes' - If the event is of type @TimerStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heActivityTaskCancelRequestedEventAttributes' - If the event is of type @ActivityTaskcancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heWorkflowExecutionTimedOutEventAttributes' - If the event is of type @WorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heWorkflowExecutionSignaledEventAttributes' - If the event is of type @WorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heTimerFiredEventAttributes' - If the event is of type @TimerFired@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heActivityTaskFailedEventAttributes' - If the event is of type @ActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heExternalWorkflowExecutionSignaledEventAttributes' - If the event is of type @ExternalWorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heDecisionTaskCompletedEventAttributes' - If the event is of type @DecisionTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heStartChildWorkflowExecutionFailedEventAttributes' - If the event is of type @StartChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heChildWorkflowExecutionFailedEventAttributes' - If the event is of type @ChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heFailWorkflowExecutionFailedEventAttributes' - If the event is of type @FailWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heContinueAsNewWorkflowExecutionFailedEventAttributes' - If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heSignalExternalWorkflowExecutionInitiatedEventAttributes' - If the event is of type @SignalExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heLambdaFunctionTimedOutEventAttributes' - Provides the details of the @LambdaFunctionTimedOut@ event. It isn't set for other event types.
--
-- * 'heWorkflowExecutionFailedEventAttributes' - If the event is of type @WorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heWorkflowExecutionContinuedAsNewEventAttributes' - If the event is of type @WorkflowExecutionContinuedAsNew@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heExternalWorkflowExecutionCancelRequestedEventAttributes' - If the event is of type @ExternalWorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
--
-- * 'heEventTimestamp' - The date and time when the event occurred.
--
-- * 'heEventType' - The type of the history event.
--
-- * 'heEventId' - The system generated ID of the event. This ID uniquely identifies the event with in the workflow execution history.
historyEvent ::
  -- | 'heEventTimestamp'
  UTCTime ->
  -- | 'heEventType'
  EventType ->
  -- | 'heEventId'
  Integer ->
  HistoryEvent
historyEvent pEventTimestamp_ pEventType_ pEventId_ =
  HistoryEvent'
    { _heWorkflowExecutionCancelRequestedEventAttributes =
        Nothing,
      _heRecordMarkerFailedEventAttributes = Nothing,
      _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes =
        Nothing,
      _heLambdaFunctionStartedEventAttributes = Nothing,
      _heDecisionTaskScheduledEventAttributes = Nothing,
      _heWorkflowExecutionCompletedEventAttributes = Nothing,
      _heStartTimerFailedEventAttributes = Nothing,
      _heActivityTaskScheduledEventAttributes = Nothing,
      _heScheduleActivityTaskFailedEventAttributes = Nothing,
      _heChildWorkflowExecutionCompletedEventAttributes = Nothing,
      _heMarkerRecordedEventAttributes = Nothing,
      _heScheduleLambdaFunctionFailedEventAttributes = Nothing,
      _heCompleteWorkflowExecutionFailedEventAttributes = Nothing,
      _heLambdaFunctionCompletedEventAttributes = Nothing,
      _heRequestCancelExternalWorkflowExecutionFailedEventAttributes =
        Nothing,
      _heTimerCanceledEventAttributes = Nothing,
      _heWorkflowExecutionStartedEventAttributes = Nothing,
      _heActivityTaskCompletedEventAttributes = Nothing,
      _heDecisionTaskTimedOutEventAttributes = Nothing,
      _heCancelTimerFailedEventAttributes = Nothing,
      _heChildWorkflowExecutionStartedEventAttributes = Nothing,
      _heActivityTaskCanceledEventAttributes = Nothing,
      _heActivityTaskTimedOutEventAttributes = Nothing,
      _heDecisionTaskStartedEventAttributes = Nothing,
      _heWorkflowExecutionTerminatedEventAttributes = Nothing,
      _heChildWorkflowExecutionCanceledEventAttributes = Nothing,
      _heRequestCancelActivityTaskFailedEventAttributes = Nothing,
      _heLambdaFunctionScheduledEventAttributes = Nothing,
      _heChildWorkflowExecutionTimedOutEventAttributes = Nothing,
      _heCancelWorkflowExecutionFailedEventAttributes = Nothing,
      _heStartChildWorkflowExecutionInitiatedEventAttributes = Nothing,
      _heSignalExternalWorkflowExecutionFailedEventAttributes = Nothing,
      _heActivityTaskStartedEventAttributes = Nothing,
      _heStartLambdaFunctionFailedEventAttributes = Nothing,
      _heChildWorkflowExecutionTerminatedEventAttributes = Nothing,
      _heLambdaFunctionFailedEventAttributes = Nothing,
      _heWorkflowExecutionCanceledEventAttributes = Nothing,
      _heTimerStartedEventAttributes = Nothing,
      _heActivityTaskCancelRequestedEventAttributes = Nothing,
      _heWorkflowExecutionTimedOutEventAttributes = Nothing,
      _heWorkflowExecutionSignaledEventAttributes = Nothing,
      _heTimerFiredEventAttributes = Nothing,
      _heActivityTaskFailedEventAttributes = Nothing,
      _heExternalWorkflowExecutionSignaledEventAttributes = Nothing,
      _heDecisionTaskCompletedEventAttributes = Nothing,
      _heStartChildWorkflowExecutionFailedEventAttributes = Nothing,
      _heChildWorkflowExecutionFailedEventAttributes = Nothing,
      _heFailWorkflowExecutionFailedEventAttributes = Nothing,
      _heContinueAsNewWorkflowExecutionFailedEventAttributes = Nothing,
      _heSignalExternalWorkflowExecutionInitiatedEventAttributes =
        Nothing,
      _heLambdaFunctionTimedOutEventAttributes = Nothing,
      _heWorkflowExecutionFailedEventAttributes = Nothing,
      _heWorkflowExecutionContinuedAsNewEventAttributes = Nothing,
      _heExternalWorkflowExecutionCancelRequestedEventAttributes =
        Nothing,
      _heEventTimestamp = _Time # pEventTimestamp_,
      _heEventType = pEventType_,
      _heEventId = pEventId_
    }

-- | If the event is of type @WorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heWorkflowExecutionCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCancelRequestedEventAttributes)
heWorkflowExecutionCancelRequestedEventAttributes = lens _heWorkflowExecutionCancelRequestedEventAttributes (\s a -> s {_heWorkflowExecutionCancelRequestedEventAttributes = a})

-- | If the event is of type @DecisionTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heRecordMarkerFailedEventAttributes :: Lens' HistoryEvent (Maybe RecordMarkerFailedEventAttributes)
heRecordMarkerFailedEventAttributes = lens _heRecordMarkerFailedEventAttributes (\s a -> s {_heRecordMarkerFailedEventAttributes = a})

-- | If the event is of type @RequestCancelExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = lens _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes (\s a -> s {_heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = a})

-- | Provides the details of the @LambdaFunctionStarted@ event. It isn't set for other event types.
heLambdaFunctionStartedEventAttributes :: Lens' HistoryEvent (Maybe LambdaFunctionStartedEventAttributes)
heLambdaFunctionStartedEventAttributes = lens _heLambdaFunctionStartedEventAttributes (\s a -> s {_heLambdaFunctionStartedEventAttributes = a})

-- | If the event is of type @DecisionTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heDecisionTaskScheduledEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskScheduledEventAttributes)
heDecisionTaskScheduledEventAttributes = lens _heDecisionTaskScheduledEventAttributes (\s a -> s {_heDecisionTaskScheduledEventAttributes = a})

-- | If the event is of type @WorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heWorkflowExecutionCompletedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCompletedEventAttributes)
heWorkflowExecutionCompletedEventAttributes = lens _heWorkflowExecutionCompletedEventAttributes (\s a -> s {_heWorkflowExecutionCompletedEventAttributes = a})

-- | If the event is of type @StartTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heStartTimerFailedEventAttributes :: Lens' HistoryEvent (Maybe StartTimerFailedEventAttributes)
heStartTimerFailedEventAttributes = lens _heStartTimerFailedEventAttributes (\s a -> s {_heStartTimerFailedEventAttributes = a})

-- | If the event is of type @ActivityTaskScheduled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heActivityTaskScheduledEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskScheduledEventAttributes)
heActivityTaskScheduledEventAttributes = lens _heActivityTaskScheduledEventAttributes (\s a -> s {_heActivityTaskScheduledEventAttributes = a})

-- | If the event is of type @ScheduleActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heScheduleActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe ScheduleActivityTaskFailedEventAttributes)
heScheduleActivityTaskFailedEventAttributes = lens _heScheduleActivityTaskFailedEventAttributes (\s a -> s {_heScheduleActivityTaskFailedEventAttributes = a})

-- | If the event is of type @ChildWorkflowExecutionCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heChildWorkflowExecutionCompletedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionCompletedEventAttributes)
heChildWorkflowExecutionCompletedEventAttributes = lens _heChildWorkflowExecutionCompletedEventAttributes (\s a -> s {_heChildWorkflowExecutionCompletedEventAttributes = a})

-- | If the event is of type @MarkerRecorded@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heMarkerRecordedEventAttributes :: Lens' HistoryEvent (Maybe MarkerRecordedEventAttributes)
heMarkerRecordedEventAttributes = lens _heMarkerRecordedEventAttributes (\s a -> s {_heMarkerRecordedEventAttributes = a})

-- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It isn't set for other event types.
heScheduleLambdaFunctionFailedEventAttributes :: Lens' HistoryEvent (Maybe ScheduleLambdaFunctionFailedEventAttributes)
heScheduleLambdaFunctionFailedEventAttributes = lens _heScheduleLambdaFunctionFailedEventAttributes (\s a -> s {_heScheduleLambdaFunctionFailedEventAttributes = a})

-- | If the event is of type @CompleteWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heCompleteWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe CompleteWorkflowExecutionFailedEventAttributes)
heCompleteWorkflowExecutionFailedEventAttributes = lens _heCompleteWorkflowExecutionFailedEventAttributes (\s a -> s {_heCompleteWorkflowExecutionFailedEventAttributes = a})

-- | Provides the details of the @LambdaFunctionCompleted@ event. It isn't set for other event types.
heLambdaFunctionCompletedEventAttributes :: Lens' HistoryEvent (Maybe LambdaFunctionCompletedEventAttributes)
heLambdaFunctionCompletedEventAttributes = lens _heLambdaFunctionCompletedEventAttributes (\s a -> s {_heLambdaFunctionCompletedEventAttributes = a})

-- | If the event is of type @RequestCancelExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes)
heRequestCancelExternalWorkflowExecutionFailedEventAttributes = lens _heRequestCancelExternalWorkflowExecutionFailedEventAttributes (\s a -> s {_heRequestCancelExternalWorkflowExecutionFailedEventAttributes = a})

-- | If the event is of type @TimerCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heTimerCanceledEventAttributes :: Lens' HistoryEvent (Maybe TimerCanceledEventAttributes)
heTimerCanceledEventAttributes = lens _heTimerCanceledEventAttributes (\s a -> s {_heTimerCanceledEventAttributes = a})

-- | If the event is of type @WorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heWorkflowExecutionStartedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionStartedEventAttributes)
heWorkflowExecutionStartedEventAttributes = lens _heWorkflowExecutionStartedEventAttributes (\s a -> s {_heWorkflowExecutionStartedEventAttributes = a})

-- | If the event is of type @ActivityTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heActivityTaskCompletedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCompletedEventAttributes)
heActivityTaskCompletedEventAttributes = lens _heActivityTaskCompletedEventAttributes (\s a -> s {_heActivityTaskCompletedEventAttributes = a})

-- | If the event is of type @DecisionTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heDecisionTaskTimedOutEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskTimedOutEventAttributes)
heDecisionTaskTimedOutEventAttributes = lens _heDecisionTaskTimedOutEventAttributes (\s a -> s {_heDecisionTaskTimedOutEventAttributes = a})

-- | If the event is of type @CancelTimerFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heCancelTimerFailedEventAttributes :: Lens' HistoryEvent (Maybe CancelTimerFailedEventAttributes)
heCancelTimerFailedEventAttributes = lens _heCancelTimerFailedEventAttributes (\s a -> s {_heCancelTimerFailedEventAttributes = a})

-- | If the event is of type @ChildWorkflowExecutionStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heChildWorkflowExecutionStartedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionStartedEventAttributes)
heChildWorkflowExecutionStartedEventAttributes = lens _heChildWorkflowExecutionStartedEventAttributes (\s a -> s {_heChildWorkflowExecutionStartedEventAttributes = a})

-- | If the event is of type @ActivityTaskCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heActivityTaskCanceledEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCanceledEventAttributes)
heActivityTaskCanceledEventAttributes = lens _heActivityTaskCanceledEventAttributes (\s a -> s {_heActivityTaskCanceledEventAttributes = a})

-- | If the event is of type @ActivityTaskTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heActivityTaskTimedOutEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskTimedOutEventAttributes)
heActivityTaskTimedOutEventAttributes = lens _heActivityTaskTimedOutEventAttributes (\s a -> s {_heActivityTaskTimedOutEventAttributes = a})

-- | If the event is of type @DecisionTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heDecisionTaskStartedEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskStartedEventAttributes)
heDecisionTaskStartedEventAttributes = lens _heDecisionTaskStartedEventAttributes (\s a -> s {_heDecisionTaskStartedEventAttributes = a})

-- | If the event is of type @WorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heWorkflowExecutionTerminatedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionTerminatedEventAttributes)
heWorkflowExecutionTerminatedEventAttributes = lens _heWorkflowExecutionTerminatedEventAttributes (\s a -> s {_heWorkflowExecutionTerminatedEventAttributes = a})

-- | If the event is of type @ChildWorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heChildWorkflowExecutionCanceledEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionCanceledEventAttributes)
heChildWorkflowExecutionCanceledEventAttributes = lens _heChildWorkflowExecutionCanceledEventAttributes (\s a -> s {_heChildWorkflowExecutionCanceledEventAttributes = a})

-- | If the event is of type @RequestCancelActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heRequestCancelActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelActivityTaskFailedEventAttributes)
heRequestCancelActivityTaskFailedEventAttributes = lens _heRequestCancelActivityTaskFailedEventAttributes (\s a -> s {_heRequestCancelActivityTaskFailedEventAttributes = a})

-- | Provides the details of the @LambdaFunctionScheduled@ event. It isn't set for other event types.
heLambdaFunctionScheduledEventAttributes :: Lens' HistoryEvent (Maybe LambdaFunctionScheduledEventAttributes)
heLambdaFunctionScheduledEventAttributes = lens _heLambdaFunctionScheduledEventAttributes (\s a -> s {_heLambdaFunctionScheduledEventAttributes = a})

-- | If the event is of type @ChildWorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heChildWorkflowExecutionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionTimedOutEventAttributes)
heChildWorkflowExecutionTimedOutEventAttributes = lens _heChildWorkflowExecutionTimedOutEventAttributes (\s a -> s {_heChildWorkflowExecutionTimedOutEventAttributes = a})

-- | If the event is of type @CancelWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heCancelWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe CancelWorkflowExecutionFailedEventAttributes)
heCancelWorkflowExecutionFailedEventAttributes = lens _heCancelWorkflowExecutionFailedEventAttributes (\s a -> s {_heCancelWorkflowExecutionFailedEventAttributes = a})

-- | If the event is of type @StartChildWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heStartChildWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe StartChildWorkflowExecutionInitiatedEventAttributes)
heStartChildWorkflowExecutionInitiatedEventAttributes = lens _heStartChildWorkflowExecutionInitiatedEventAttributes (\s a -> s {_heStartChildWorkflowExecutionInitiatedEventAttributes = a})

-- | If the event is of type @SignalExternalWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heSignalExternalWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe SignalExternalWorkflowExecutionFailedEventAttributes)
heSignalExternalWorkflowExecutionFailedEventAttributes = lens _heSignalExternalWorkflowExecutionFailedEventAttributes (\s a -> s {_heSignalExternalWorkflowExecutionFailedEventAttributes = a})

-- | If the event is of type @ActivityTaskStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heActivityTaskStartedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskStartedEventAttributes)
heActivityTaskStartedEventAttributes = lens _heActivityTaskStartedEventAttributes (\s a -> s {_heActivityTaskStartedEventAttributes = a})

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn't set for other event types.
heStartLambdaFunctionFailedEventAttributes :: Lens' HistoryEvent (Maybe StartLambdaFunctionFailedEventAttributes)
heStartLambdaFunctionFailedEventAttributes = lens _heStartLambdaFunctionFailedEventAttributes (\s a -> s {_heStartLambdaFunctionFailedEventAttributes = a})

-- | If the event is of type @ChildWorkflowExecutionTerminated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heChildWorkflowExecutionTerminatedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionTerminatedEventAttributes)
heChildWorkflowExecutionTerminatedEventAttributes = lens _heChildWorkflowExecutionTerminatedEventAttributes (\s a -> s {_heChildWorkflowExecutionTerminatedEventAttributes = a})

-- | Provides the details of the @LambdaFunctionFailed@ event. It isn't set for other event types.
heLambdaFunctionFailedEventAttributes :: Lens' HistoryEvent (Maybe LambdaFunctionFailedEventAttributes)
heLambdaFunctionFailedEventAttributes = lens _heLambdaFunctionFailedEventAttributes (\s a -> s {_heLambdaFunctionFailedEventAttributes = a})

-- | If the event is of type @WorkflowExecutionCanceled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heWorkflowExecutionCanceledEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCanceledEventAttributes)
heWorkflowExecutionCanceledEventAttributes = lens _heWorkflowExecutionCanceledEventAttributes (\s a -> s {_heWorkflowExecutionCanceledEventAttributes = a})

-- | If the event is of type @TimerStarted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heTimerStartedEventAttributes :: Lens' HistoryEvent (Maybe TimerStartedEventAttributes)
heTimerStartedEventAttributes = lens _heTimerStartedEventAttributes (\s a -> s {_heTimerStartedEventAttributes = a})

-- | If the event is of type @ActivityTaskcancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heActivityTaskCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCancelRequestedEventAttributes)
heActivityTaskCancelRequestedEventAttributes = lens _heActivityTaskCancelRequestedEventAttributes (\s a -> s {_heActivityTaskCancelRequestedEventAttributes = a})

-- | If the event is of type @WorkflowExecutionTimedOut@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heWorkflowExecutionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionTimedOutEventAttributes)
heWorkflowExecutionTimedOutEventAttributes = lens _heWorkflowExecutionTimedOutEventAttributes (\s a -> s {_heWorkflowExecutionTimedOutEventAttributes = a})

-- | If the event is of type @WorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heWorkflowExecutionSignaledEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionSignaledEventAttributes)
heWorkflowExecutionSignaledEventAttributes = lens _heWorkflowExecutionSignaledEventAttributes (\s a -> s {_heWorkflowExecutionSignaledEventAttributes = a})

-- | If the event is of type @TimerFired@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heTimerFiredEventAttributes :: Lens' HistoryEvent (Maybe TimerFiredEventAttributes)
heTimerFiredEventAttributes = lens _heTimerFiredEventAttributes (\s a -> s {_heTimerFiredEventAttributes = a})

-- | If the event is of type @ActivityTaskFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskFailedEventAttributes)
heActivityTaskFailedEventAttributes = lens _heActivityTaskFailedEventAttributes (\s a -> s {_heActivityTaskFailedEventAttributes = a})

-- | If the event is of type @ExternalWorkflowExecutionSignaled@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heExternalWorkflowExecutionSignaledEventAttributes :: Lens' HistoryEvent (Maybe ExternalWorkflowExecutionSignaledEventAttributes)
heExternalWorkflowExecutionSignaledEventAttributes = lens _heExternalWorkflowExecutionSignaledEventAttributes (\s a -> s {_heExternalWorkflowExecutionSignaledEventAttributes = a})

-- | If the event is of type @DecisionTaskCompleted@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heDecisionTaskCompletedEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskCompletedEventAttributes)
heDecisionTaskCompletedEventAttributes = lens _heDecisionTaskCompletedEventAttributes (\s a -> s {_heDecisionTaskCompletedEventAttributes = a})

-- | If the event is of type @StartChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heStartChildWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe StartChildWorkflowExecutionFailedEventAttributes)
heStartChildWorkflowExecutionFailedEventAttributes = lens _heStartChildWorkflowExecutionFailedEventAttributes (\s a -> s {_heStartChildWorkflowExecutionFailedEventAttributes = a})

-- | If the event is of type @ChildWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heChildWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionFailedEventAttributes)
heChildWorkflowExecutionFailedEventAttributes = lens _heChildWorkflowExecutionFailedEventAttributes (\s a -> s {_heChildWorkflowExecutionFailedEventAttributes = a})

-- | If the event is of type @FailWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heFailWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe FailWorkflowExecutionFailedEventAttributes)
heFailWorkflowExecutionFailedEventAttributes = lens _heFailWorkflowExecutionFailedEventAttributes (\s a -> s {_heFailWorkflowExecutionFailedEventAttributes = a})

-- | If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heContinueAsNewWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes)
heContinueAsNewWorkflowExecutionFailedEventAttributes = lens _heContinueAsNewWorkflowExecutionFailedEventAttributes (\s a -> s {_heContinueAsNewWorkflowExecutionFailedEventAttributes = a})

-- | If the event is of type @SignalExternalWorkflowExecutionInitiated@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heSignalExternalWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes)
heSignalExternalWorkflowExecutionInitiatedEventAttributes = lens _heSignalExternalWorkflowExecutionInitiatedEventAttributes (\s a -> s {_heSignalExternalWorkflowExecutionInitiatedEventAttributes = a})

-- | Provides the details of the @LambdaFunctionTimedOut@ event. It isn't set for other event types.
heLambdaFunctionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe LambdaFunctionTimedOutEventAttributes)
heLambdaFunctionTimedOutEventAttributes = lens _heLambdaFunctionTimedOutEventAttributes (\s a -> s {_heLambdaFunctionTimedOutEventAttributes = a})

-- | If the event is of type @WorkflowExecutionFailed@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionFailedEventAttributes)
heWorkflowExecutionFailedEventAttributes = lens _heWorkflowExecutionFailedEventAttributes (\s a -> s {_heWorkflowExecutionFailedEventAttributes = a})

-- | If the event is of type @WorkflowExecutionContinuedAsNew@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heWorkflowExecutionContinuedAsNewEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionContinuedAsNewEventAttributes)
heWorkflowExecutionContinuedAsNewEventAttributes = lens _heWorkflowExecutionContinuedAsNewEventAttributes (\s a -> s {_heWorkflowExecutionContinuedAsNewEventAttributes = a})

-- | If the event is of type @ExternalWorkflowExecutionCancelRequested@ then this member is set and provides detailed information about the event. It isn't set for other event types.
heExternalWorkflowExecutionCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes)
heExternalWorkflowExecutionCancelRequestedEventAttributes = lens _heExternalWorkflowExecutionCancelRequestedEventAttributes (\s a -> s {_heExternalWorkflowExecutionCancelRequestedEventAttributes = a})

-- | The date and time when the event occurred.
heEventTimestamp :: Lens' HistoryEvent UTCTime
heEventTimestamp = lens _heEventTimestamp (\s a -> s {_heEventTimestamp = a}) . _Time

-- | The type of the history event.
heEventType :: Lens' HistoryEvent EventType
heEventType = lens _heEventType (\s a -> s {_heEventType = a})

-- | The system generated ID of the event. This ID uniquely identifies the event with in the workflow execution history.
heEventId :: Lens' HistoryEvent Integer
heEventId = lens _heEventId (\s a -> s {_heEventId = a})

instance FromJSON HistoryEvent where
  parseJSON =
    withObject
      "HistoryEvent"
      ( \x ->
          HistoryEvent'
            <$> (x .:? "workflowExecutionCancelRequestedEventAttributes")
            <*> (x .:? "recordMarkerFailedEventAttributes")
            <*> ( x
                    .:? "requestCancelExternalWorkflowExecutionInitiatedEventAttributes"
                )
            <*> (x .:? "lambdaFunctionStartedEventAttributes")
            <*> (x .:? "decisionTaskScheduledEventAttributes")
            <*> (x .:? "workflowExecutionCompletedEventAttributes")
            <*> (x .:? "startTimerFailedEventAttributes")
            <*> (x .:? "activityTaskScheduledEventAttributes")
            <*> (x .:? "scheduleActivityTaskFailedEventAttributes")
            <*> (x .:? "childWorkflowExecutionCompletedEventAttributes")
            <*> (x .:? "markerRecordedEventAttributes")
            <*> (x .:? "scheduleLambdaFunctionFailedEventAttributes")
            <*> (x .:? "completeWorkflowExecutionFailedEventAttributes")
            <*> (x .:? "lambdaFunctionCompletedEventAttributes")
            <*> ( x
                    .:? "requestCancelExternalWorkflowExecutionFailedEventAttributes"
                )
            <*> (x .:? "timerCanceledEventAttributes")
            <*> (x .:? "workflowExecutionStartedEventAttributes")
            <*> (x .:? "activityTaskCompletedEventAttributes")
            <*> (x .:? "decisionTaskTimedOutEventAttributes")
            <*> (x .:? "cancelTimerFailedEventAttributes")
            <*> (x .:? "childWorkflowExecutionStartedEventAttributes")
            <*> (x .:? "activityTaskCanceledEventAttributes")
            <*> (x .:? "activityTaskTimedOutEventAttributes")
            <*> (x .:? "decisionTaskStartedEventAttributes")
            <*> (x .:? "workflowExecutionTerminatedEventAttributes")
            <*> (x .:? "childWorkflowExecutionCanceledEventAttributes")
            <*> (x .:? "requestCancelActivityTaskFailedEventAttributes")
            <*> (x .:? "lambdaFunctionScheduledEventAttributes")
            <*> (x .:? "childWorkflowExecutionTimedOutEventAttributes")
            <*> (x .:? "cancelWorkflowExecutionFailedEventAttributes")
            <*> (x .:? "startChildWorkflowExecutionInitiatedEventAttributes")
            <*> (x .:? "signalExternalWorkflowExecutionFailedEventAttributes")
            <*> (x .:? "activityTaskStartedEventAttributes")
            <*> (x .:? "startLambdaFunctionFailedEventAttributes")
            <*> (x .:? "childWorkflowExecutionTerminatedEventAttributes")
            <*> (x .:? "lambdaFunctionFailedEventAttributes")
            <*> (x .:? "workflowExecutionCanceledEventAttributes")
            <*> (x .:? "timerStartedEventAttributes")
            <*> (x .:? "activityTaskCancelRequestedEventAttributes")
            <*> (x .:? "workflowExecutionTimedOutEventAttributes")
            <*> (x .:? "workflowExecutionSignaledEventAttributes")
            <*> (x .:? "timerFiredEventAttributes")
            <*> (x .:? "activityTaskFailedEventAttributes")
            <*> (x .:? "externalWorkflowExecutionSignaledEventAttributes")
            <*> (x .:? "decisionTaskCompletedEventAttributes")
            <*> (x .:? "startChildWorkflowExecutionFailedEventAttributes")
            <*> (x .:? "childWorkflowExecutionFailedEventAttributes")
            <*> (x .:? "failWorkflowExecutionFailedEventAttributes")
            <*> (x .:? "continueAsNewWorkflowExecutionFailedEventAttributes")
            <*> (x .:? "signalExternalWorkflowExecutionInitiatedEventAttributes")
            <*> (x .:? "lambdaFunctionTimedOutEventAttributes")
            <*> (x .:? "workflowExecutionFailedEventAttributes")
            <*> (x .:? "workflowExecutionContinuedAsNewEventAttributes")
            <*> (x .:? "externalWorkflowExecutionCancelRequestedEventAttributes")
            <*> (x .: "eventTimestamp")
            <*> (x .: "eventType")
            <*> (x .: "eventId")
      )

instance Hashable HistoryEvent

instance NFData HistoryEvent

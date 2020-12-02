{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.Decision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.Decision where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.CancelTimerDecisionAttributes
import Network.AWS.SWF.Types.CancelWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.CompleteWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.DecisionType
import Network.AWS.SWF.Types.FailWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.RecordMarkerDecisionAttributes
import Network.AWS.SWF.Types.RequestCancelActivityTaskDecisionAttributes
import Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.ScheduleActivityTaskDecisionAttributes
import Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
import Network.AWS.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
import Network.AWS.SWF.Types.StartTimerDecisionAttributes

-- | Specifies a decision made by the decider. A decision can be one of these types:
--
--
--     * @CancelTimer@ – Cancels a previously started timer and records a @TimerCanceled@ event in the history.
--
--     * @CancelWorkflowExecution@ – Closes the workflow execution and records a @WorkflowExecutionCanceled@ event in the history.
--
--     * @CompleteWorkflowExecution@ – Closes the workflow execution and records a @WorkflowExecutionCompleted@ event in the history .
--
--     * @ContinueAsNewWorkflowExecution@ – Closes the workflow execution and starts a new workflow execution of the same type using the same workflow ID and a unique run Id. A @WorkflowExecutionContinuedAsNew@ event is recorded in the history.
--
--     * @FailWorkflowExecution@ – Closes the workflow execution and records a @WorkflowExecutionFailed@ event in the history.
--
--     * @RecordMarker@ – Records a @MarkerRecorded@ event in the history. Markers can be used for adding custom information in the history for instance to let deciders know that they don't need to look at the history beyond the marker event.
--
--     * @RequestCancelActivityTask@ – Attempts to cancel a previously scheduled activity task. If the activity task was scheduled but has not been assigned to a worker, then it is canceled. If the activity task was already assigned to a worker, then the worker is informed that cancellation has been requested in the response to 'RecordActivityTaskHeartbeat' .
--
--     * @RequestCancelExternalWorkflowExecution@ – Requests that a request be made to cancel the specified external workflow execution and records a @RequestCancelExternalWorkflowExecutionInitiated@ event in the history.
--
--     * @ScheduleActivityTask@ – Schedules an activity task.
--
--     * @SignalExternalWorkflowExecution@ – Requests a signal to be delivered to the specified external workflow execution and records a @SignalExternalWorkflowExecutionInitiated@ event in the history.
--
--     * @StartChildWorkflowExecution@ – Requests that a child workflow execution be started and records a @StartChildWorkflowExecutionInitiated@ event in the history. The child workflow execution is a separate workflow execution with its own history.
--
--     * @StartTimer@ – Starts a timer for this workflow execution and records a @TimerStarted@ event in the history. This timer fires after the specified delay and record a @TimerFired@ event.
--
--
--
-- __Access Control__
--
-- If you grant permission to use @RespondDecisionTaskCompleted@ , you can use IAM policies to express permissions for the list of decisions returned by this action as if they were members of the API. Treating decisions as a pseudo API maintains a uniform conceptual model and helps keep policies readable. For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- __Decision Failure__
--
-- Decisions can fail for several reasons
--
--     * The ordering of decisions should follow a logical flow. Some decisions might not make sense in the current context of the workflow execution and therefore fails.
--
--     * A limit on your account was reached.
--
--     * The decision lacks sufficient permissions.
--
--
--
-- One of the following events might be added to the history to indicate an error. The event attribute's @cause@ parameter indicates the cause. If @cause@ is set to @OPERATION_NOT_PERMITTED@ , the decision failed because it lacked sufficient permissions. For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
--     * @ScheduleActivityTaskFailed@ – A @ScheduleActivityTask@ decision failed. This could happen if the activity type specified in the decision isn't registered, is in a deprecated state, or the decision isn't properly configured.
--
--     * @RequestCancelActivityTaskFailed@ – A @RequestCancelActivityTask@ decision failed. This could happen if there is no open activity task with the specified activityId.
--
--     * @StartTimerFailed@ – A @StartTimer@ decision failed. This could happen if there is another open timer with the same timerId.
--
--     * @CancelTimerFailed@ – A @CancelTimer@ decision failed. This could happen if there is no open timer with the specified timerId.
--
--     * @StartChildWorkflowExecutionFailed@ – A @StartChildWorkflowExecution@ decision failed. This could happen if the workflow type specified isn't registered, is deprecated, or the decision isn't properly configured.
--
--     * @SignalExternalWorkflowExecutionFailed@ – A @SignalExternalWorkflowExecution@ decision failed. This could happen if the @workflowID@ specified in the decision was incorrect.
--
--     * @RequestCancelExternalWorkflowExecutionFailed@ – A @RequestCancelExternalWorkflowExecution@ decision failed. This could happen if the @workflowID@ specified in the decision was incorrect.
--
--     * @CancelWorkflowExecutionFailed@ – A @CancelWorkflowExecution@ decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.
--
--     * @CompleteWorkflowExecutionFailed@ – A @CompleteWorkflowExecution@ decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.
--
--     * @ContinueAsNewWorkflowExecutionFailed@ – A @ContinueAsNewWorkflowExecution@ decision failed. This could happen if there is an unhandled decision task pending in the workflow execution or the ContinueAsNewWorkflowExecution decision was not configured correctly.
--
--     * @FailWorkflowExecutionFailed@ – A @FailWorkflowExecution@ decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.
--
--
--
-- The preceding error events might occur due to an error in the decider logic, which might put the workflow execution in an unstable state The cause field in the event structure for the error event indicates the cause of the error.
--
-- __How to Code a Decision__
--
-- You code a decision by first setting the decision type field to one of the above decision values, and then set the corresponding attributes field shown below:
--
--     * @'ScheduleActivityTaskDecisionAttributes' @
--
--     * @'RequestCancelActivityTaskDecisionAttributes' @
--
--     * @'CompleteWorkflowExecutionDecisionAttributes' @
--
--     * @'FailWorkflowExecutionDecisionAttributes' @
--
--     * @'CancelWorkflowExecutionDecisionAttributes' @
--
--     * @'ContinueAsNewWorkflowExecutionDecisionAttributes' @
--
--     * @'RecordMarkerDecisionAttributes' @
--
--     * @'StartTimerDecisionAttributes' @
--
--     * @'CancelTimerDecisionAttributes' @
--
--     * @'SignalExternalWorkflowExecutionDecisionAttributes' @
--
--     * @'RequestCancelExternalWorkflowExecutionDecisionAttributes' @
--
--     * @'StartChildWorkflowExecutionDecisionAttributes' @
--
--
--
--
-- /See:/ 'decision' smart constructor.
data Decision = Decision'
  { _dRequestCancelExternalWorkflowExecutionDecisionAttributes ::
      !(Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes),
    _dScheduleActivityTaskDecisionAttributes ::
      !(Maybe ScheduleActivityTaskDecisionAttributes),
    _dSignalExternalWorkflowExecutionDecisionAttributes ::
      !(Maybe SignalExternalWorkflowExecutionDecisionAttributes),
    _dStartTimerDecisionAttributes ::
      !(Maybe StartTimerDecisionAttributes),
    _dRecordMarkerDecisionAttributes ::
      !(Maybe RecordMarkerDecisionAttributes),
    _dFailWorkflowExecutionDecisionAttributes ::
      !(Maybe FailWorkflowExecutionDecisionAttributes),
    _dStartChildWorkflowExecutionDecisionAttributes ::
      !(Maybe StartChildWorkflowExecutionDecisionAttributes),
    _dCompleteWorkflowExecutionDecisionAttributes ::
      !(Maybe CompleteWorkflowExecutionDecisionAttributes),
    _dScheduleLambdaFunctionDecisionAttributes ::
      !(Maybe ScheduleLambdaFunctionDecisionAttributes),
    _dRequestCancelActivityTaskDecisionAttributes ::
      !(Maybe RequestCancelActivityTaskDecisionAttributes),
    _dCancelWorkflowExecutionDecisionAttributes ::
      !(Maybe CancelWorkflowExecutionDecisionAttributes),
    _dCancelTimerDecisionAttributes ::
      !(Maybe CancelTimerDecisionAttributes),
    _dContinueAsNewWorkflowExecutionDecisionAttributes ::
      !(Maybe ContinueAsNewWorkflowExecutionDecisionAttributes),
    _dDecisionType :: !DecisionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Decision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dRequestCancelExternalWorkflowExecutionDecisionAttributes' - Provides the details of the @RequestCancelExternalWorkflowExecution@ decision. It isn't set for other decision types.
--
-- * 'dScheduleActivityTaskDecisionAttributes' - Provides the details of the @ScheduleActivityTask@ decision. It isn't set for other decision types.
--
-- * 'dSignalExternalWorkflowExecutionDecisionAttributes' - Provides the details of the @SignalExternalWorkflowExecution@ decision. It isn't set for other decision types.
--
-- * 'dStartTimerDecisionAttributes' - Provides the details of the @StartTimer@ decision. It isn't set for other decision types.
--
-- * 'dRecordMarkerDecisionAttributes' - Provides the details of the @RecordMarker@ decision. It isn't set for other decision types.
--
-- * 'dFailWorkflowExecutionDecisionAttributes' - Provides the details of the @FailWorkflowExecution@ decision. It isn't set for other decision types.
--
-- * 'dStartChildWorkflowExecutionDecisionAttributes' - Provides the details of the @StartChildWorkflowExecution@ decision. It isn't set for other decision types.
--
-- * 'dCompleteWorkflowExecutionDecisionAttributes' - Provides the details of the @CompleteWorkflowExecution@ decision. It isn't set for other decision types.
--
-- * 'dScheduleLambdaFunctionDecisionAttributes' - Provides the details of the @ScheduleLambdaFunction@ decision. It isn't set for other decision types.
--
-- * 'dRequestCancelActivityTaskDecisionAttributes' - Provides the details of the @RequestCancelActivityTask@ decision. It isn't set for other decision types.
--
-- * 'dCancelWorkflowExecutionDecisionAttributes' - Provides the details of the @CancelWorkflowExecution@ decision. It isn't set for other decision types.
--
-- * 'dCancelTimerDecisionAttributes' - Provides the details of the @CancelTimer@ decision. It isn't set for other decision types.
--
-- * 'dContinueAsNewWorkflowExecutionDecisionAttributes' - Provides the details of the @ContinueAsNewWorkflowExecution@ decision. It isn't set for other decision types.
--
-- * 'dDecisionType' - Specifies the type of the decision.
decision ::
  -- | 'dDecisionType'
  DecisionType ->
  Decision
decision pDecisionType_ =
  Decision'
    { _dRequestCancelExternalWorkflowExecutionDecisionAttributes =
        Nothing,
      _dScheduleActivityTaskDecisionAttributes = Nothing,
      _dSignalExternalWorkflowExecutionDecisionAttributes = Nothing,
      _dStartTimerDecisionAttributes = Nothing,
      _dRecordMarkerDecisionAttributes = Nothing,
      _dFailWorkflowExecutionDecisionAttributes = Nothing,
      _dStartChildWorkflowExecutionDecisionAttributes = Nothing,
      _dCompleteWorkflowExecutionDecisionAttributes = Nothing,
      _dScheduleLambdaFunctionDecisionAttributes = Nothing,
      _dRequestCancelActivityTaskDecisionAttributes = Nothing,
      _dCancelWorkflowExecutionDecisionAttributes = Nothing,
      _dCancelTimerDecisionAttributes = Nothing,
      _dContinueAsNewWorkflowExecutionDecisionAttributes = Nothing,
      _dDecisionType = pDecisionType_
    }

-- | Provides the details of the @RequestCancelExternalWorkflowExecution@ decision. It isn't set for other decision types.
dRequestCancelExternalWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes)
dRequestCancelExternalWorkflowExecutionDecisionAttributes = lens _dRequestCancelExternalWorkflowExecutionDecisionAttributes (\s a -> s {_dRequestCancelExternalWorkflowExecutionDecisionAttributes = a})

-- | Provides the details of the @ScheduleActivityTask@ decision. It isn't set for other decision types.
dScheduleActivityTaskDecisionAttributes :: Lens' Decision (Maybe ScheduleActivityTaskDecisionAttributes)
dScheduleActivityTaskDecisionAttributes = lens _dScheduleActivityTaskDecisionAttributes (\s a -> s {_dScheduleActivityTaskDecisionAttributes = a})

-- | Provides the details of the @SignalExternalWorkflowExecution@ decision. It isn't set for other decision types.
dSignalExternalWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe SignalExternalWorkflowExecutionDecisionAttributes)
dSignalExternalWorkflowExecutionDecisionAttributes = lens _dSignalExternalWorkflowExecutionDecisionAttributes (\s a -> s {_dSignalExternalWorkflowExecutionDecisionAttributes = a})

-- | Provides the details of the @StartTimer@ decision. It isn't set for other decision types.
dStartTimerDecisionAttributes :: Lens' Decision (Maybe StartTimerDecisionAttributes)
dStartTimerDecisionAttributes = lens _dStartTimerDecisionAttributes (\s a -> s {_dStartTimerDecisionAttributes = a})

-- | Provides the details of the @RecordMarker@ decision. It isn't set for other decision types.
dRecordMarkerDecisionAttributes :: Lens' Decision (Maybe RecordMarkerDecisionAttributes)
dRecordMarkerDecisionAttributes = lens _dRecordMarkerDecisionAttributes (\s a -> s {_dRecordMarkerDecisionAttributes = a})

-- | Provides the details of the @FailWorkflowExecution@ decision. It isn't set for other decision types.
dFailWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe FailWorkflowExecutionDecisionAttributes)
dFailWorkflowExecutionDecisionAttributes = lens _dFailWorkflowExecutionDecisionAttributes (\s a -> s {_dFailWorkflowExecutionDecisionAttributes = a})

-- | Provides the details of the @StartChildWorkflowExecution@ decision. It isn't set for other decision types.
dStartChildWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe StartChildWorkflowExecutionDecisionAttributes)
dStartChildWorkflowExecutionDecisionAttributes = lens _dStartChildWorkflowExecutionDecisionAttributes (\s a -> s {_dStartChildWorkflowExecutionDecisionAttributes = a})

-- | Provides the details of the @CompleteWorkflowExecution@ decision. It isn't set for other decision types.
dCompleteWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe CompleteWorkflowExecutionDecisionAttributes)
dCompleteWorkflowExecutionDecisionAttributes = lens _dCompleteWorkflowExecutionDecisionAttributes (\s a -> s {_dCompleteWorkflowExecutionDecisionAttributes = a})

-- | Provides the details of the @ScheduleLambdaFunction@ decision. It isn't set for other decision types.
dScheduleLambdaFunctionDecisionAttributes :: Lens' Decision (Maybe ScheduleLambdaFunctionDecisionAttributes)
dScheduleLambdaFunctionDecisionAttributes = lens _dScheduleLambdaFunctionDecisionAttributes (\s a -> s {_dScheduleLambdaFunctionDecisionAttributes = a})

-- | Provides the details of the @RequestCancelActivityTask@ decision. It isn't set for other decision types.
dRequestCancelActivityTaskDecisionAttributes :: Lens' Decision (Maybe RequestCancelActivityTaskDecisionAttributes)
dRequestCancelActivityTaskDecisionAttributes = lens _dRequestCancelActivityTaskDecisionAttributes (\s a -> s {_dRequestCancelActivityTaskDecisionAttributes = a})

-- | Provides the details of the @CancelWorkflowExecution@ decision. It isn't set for other decision types.
dCancelWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe CancelWorkflowExecutionDecisionAttributes)
dCancelWorkflowExecutionDecisionAttributes = lens _dCancelWorkflowExecutionDecisionAttributes (\s a -> s {_dCancelWorkflowExecutionDecisionAttributes = a})

-- | Provides the details of the @CancelTimer@ decision. It isn't set for other decision types.
dCancelTimerDecisionAttributes :: Lens' Decision (Maybe CancelTimerDecisionAttributes)
dCancelTimerDecisionAttributes = lens _dCancelTimerDecisionAttributes (\s a -> s {_dCancelTimerDecisionAttributes = a})

-- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision. It isn't set for other decision types.
dContinueAsNewWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe ContinueAsNewWorkflowExecutionDecisionAttributes)
dContinueAsNewWorkflowExecutionDecisionAttributes = lens _dContinueAsNewWorkflowExecutionDecisionAttributes (\s a -> s {_dContinueAsNewWorkflowExecutionDecisionAttributes = a})

-- | Specifies the type of the decision.
dDecisionType :: Lens' Decision DecisionType
dDecisionType = lens _dDecisionType (\s a -> s {_dDecisionType = a})

instance Hashable Decision

instance NFData Decision

instance ToJSON Decision where
  toJSON Decision' {..} =
    object
      ( catMaybes
          [ ("requestCancelExternalWorkflowExecutionDecisionAttributes" .=)
              <$> _dRequestCancelExternalWorkflowExecutionDecisionAttributes,
            ("scheduleActivityTaskDecisionAttributes" .=)
              <$> _dScheduleActivityTaskDecisionAttributes,
            ("signalExternalWorkflowExecutionDecisionAttributes" .=)
              <$> _dSignalExternalWorkflowExecutionDecisionAttributes,
            ("startTimerDecisionAttributes" .=)
              <$> _dStartTimerDecisionAttributes,
            ("recordMarkerDecisionAttributes" .=)
              <$> _dRecordMarkerDecisionAttributes,
            ("failWorkflowExecutionDecisionAttributes" .=)
              <$> _dFailWorkflowExecutionDecisionAttributes,
            ("startChildWorkflowExecutionDecisionAttributes" .=)
              <$> _dStartChildWorkflowExecutionDecisionAttributes,
            ("completeWorkflowExecutionDecisionAttributes" .=)
              <$> _dCompleteWorkflowExecutionDecisionAttributes,
            ("scheduleLambdaFunctionDecisionAttributes" .=)
              <$> _dScheduleLambdaFunctionDecisionAttributes,
            ("requestCancelActivityTaskDecisionAttributes" .=)
              <$> _dRequestCancelActivityTaskDecisionAttributes,
            ("cancelWorkflowExecutionDecisionAttributes" .=)
              <$> _dCancelWorkflowExecutionDecisionAttributes,
            ("cancelTimerDecisionAttributes" .=)
              <$> _dCancelTimerDecisionAttributes,
            ("continueAsNewWorkflowExecutionDecisionAttributes" .=)
              <$> _dContinueAsNewWorkflowExecutionDecisionAttributes,
            Just ("decisionType" .= _dDecisionType)
          ]
      )

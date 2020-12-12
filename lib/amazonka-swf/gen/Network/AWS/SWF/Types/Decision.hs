{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.Decision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.Decision
  ( Decision (..),

    -- * Smart constructor
    mkDecision,

    -- * Lenses
    dRequestCancelExternalWorkflowExecutionDecisionAttributes,
    dScheduleActivityTaskDecisionAttributes,
    dSignalExternalWorkflowExecutionDecisionAttributes,
    dStartTimerDecisionAttributes,
    dRecordMarkerDecisionAttributes,
    dFailWorkflowExecutionDecisionAttributes,
    dStartChildWorkflowExecutionDecisionAttributes,
    dCompleteWorkflowExecutionDecisionAttributes,
    dScheduleLambdaFunctionDecisionAttributes,
    dRequestCancelActivityTaskDecisionAttributes,
    dCancelWorkflowExecutionDecisionAttributes,
    dCancelTimerDecisionAttributes,
    dContinueAsNewWorkflowExecutionDecisionAttributes,
    dDecisionType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
--
--     * @CancelWorkflowExecution@ – Closes the workflow execution and records a @WorkflowExecutionCanceled@ event in the history.
--
--
--     * @CompleteWorkflowExecution@ – Closes the workflow execution and records a @WorkflowExecutionCompleted@ event in the history .
--
--
--     * @ContinueAsNewWorkflowExecution@ – Closes the workflow execution and starts a new workflow execution of the same type using the same workflow ID and a unique run Id. A @WorkflowExecutionContinuedAsNew@ event is recorded in the history.
--
--
--     * @FailWorkflowExecution@ – Closes the workflow execution and records a @WorkflowExecutionFailed@ event in the history.
--
--
--     * @RecordMarker@ – Records a @MarkerRecorded@ event in the history. Markers can be used for adding custom information in the history for instance to let deciders know that they don't need to look at the history beyond the marker event.
--
--
--     * @RequestCancelActivityTask@ – Attempts to cancel a previously scheduled activity task. If the activity task was scheduled but has not been assigned to a worker, then it is canceled. If the activity task was already assigned to a worker, then the worker is informed that cancellation has been requested in the response to 'RecordActivityTaskHeartbeat' .
--
--
--     * @RequestCancelExternalWorkflowExecution@ – Requests that a request be made to cancel the specified external workflow execution and records a @RequestCancelExternalWorkflowExecutionInitiated@ event in the history.
--
--
--     * @ScheduleActivityTask@ – Schedules an activity task.
--
--
--     * @SignalExternalWorkflowExecution@ – Requests a signal to be delivered to the specified external workflow execution and records a @SignalExternalWorkflowExecutionInitiated@ event in the history.
--
--
--     * @StartChildWorkflowExecution@ – Requests that a child workflow execution be started and records a @StartChildWorkflowExecutionInitiated@ event in the history. The child workflow execution is a separate workflow execution with its own history.
--
--
--     * @StartTimer@ – Starts a timer for this workflow execution and records a @TimerStarted@ event in the history. This timer fires after the specified delay and record a @TimerFired@ event.
--
--
-- __Access Control__
-- If you grant permission to use @RespondDecisionTaskCompleted@ , you can use IAM policies to express permissions for the list of decisions returned by this action as if they were members of the API. Treating decisions as a pseudo API maintains a uniform conceptual model and helps keep policies readable. For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
-- __Decision Failure__
-- Decisions can fail for several reasons
--
--     * The ordering of decisions should follow a logical flow. Some decisions might not make sense in the current context of the workflow execution and therefore fails.
--
--
--     * A limit on your account was reached.
--
--
--     * The decision lacks sufficient permissions.
--
--
-- One of the following events might be added to the history to indicate an error. The event attribute's @cause@ parameter indicates the cause. If @cause@ is set to @OPERATION_NOT_PERMITTED@ , the decision failed because it lacked sufficient permissions. For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
--     * @ScheduleActivityTaskFailed@ – A @ScheduleActivityTask@ decision failed. This could happen if the activity type specified in the decision isn't registered, is in a deprecated state, or the decision isn't properly configured.
--
--
--     * @RequestCancelActivityTaskFailed@ – A @RequestCancelActivityTask@ decision failed. This could happen if there is no open activity task with the specified activityId.
--
--
--     * @StartTimerFailed@ – A @StartTimer@ decision failed. This could happen if there is another open timer with the same timerId.
--
--
--     * @CancelTimerFailed@ – A @CancelTimer@ decision failed. This could happen if there is no open timer with the specified timerId.
--
--
--     * @StartChildWorkflowExecutionFailed@ – A @StartChildWorkflowExecution@ decision failed. This could happen if the workflow type specified isn't registered, is deprecated, or the decision isn't properly configured.
--
--
--     * @SignalExternalWorkflowExecutionFailed@ – A @SignalExternalWorkflowExecution@ decision failed. This could happen if the @workflowID@ specified in the decision was incorrect.
--
--
--     * @RequestCancelExternalWorkflowExecutionFailed@ – A @RequestCancelExternalWorkflowExecution@ decision failed. This could happen if the @workflowID@ specified in the decision was incorrect.
--
--
--     * @CancelWorkflowExecutionFailed@ – A @CancelWorkflowExecution@ decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.
--
--
--     * @CompleteWorkflowExecutionFailed@ – A @CompleteWorkflowExecution@ decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.
--
--
--     * @ContinueAsNewWorkflowExecutionFailed@ – A @ContinueAsNewWorkflowExecution@ decision failed. This could happen if there is an unhandled decision task pending in the workflow execution or the ContinueAsNewWorkflowExecution decision was not configured correctly.
--
--
--     * @FailWorkflowExecutionFailed@ – A @FailWorkflowExecution@ decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.
--
--
-- The preceding error events might occur due to an error in the decider logic, which might put the workflow execution in an unstable state The cause field in the event structure for the error event indicates the cause of the error.
-- __How to Code a Decision__
-- You code a decision by first setting the decision type field to one of the above decision values, and then set the corresponding attributes field shown below:
--
--     * @'ScheduleActivityTaskDecisionAttributes' @
--
--
--     * @'RequestCancelActivityTaskDecisionAttributes' @
--
--
--     * @'CompleteWorkflowExecutionDecisionAttributes' @
--
--
--     * @'FailWorkflowExecutionDecisionAttributes' @
--
--
--     * @'CancelWorkflowExecutionDecisionAttributes' @
--
--
--     * @'ContinueAsNewWorkflowExecutionDecisionAttributes' @
--
--
--     * @'RecordMarkerDecisionAttributes' @
--
--
--     * @'StartTimerDecisionAttributes' @
--
--
--     * @'CancelTimerDecisionAttributes' @
--
--
--     * @'SignalExternalWorkflowExecutionDecisionAttributes' @
--
--
--     * @'RequestCancelExternalWorkflowExecutionDecisionAttributes' @
--
--
--     * @'StartChildWorkflowExecutionDecisionAttributes' @
--
--
--
-- /See:/ 'mkDecision' smart constructor.
data Decision = Decision'
  { requestCancelExternalWorkflowExecutionDecisionAttributes ::
      Lude.Maybe
        RequestCancelExternalWorkflowExecutionDecisionAttributes,
    scheduleActivityTaskDecisionAttributes ::
      Lude.Maybe ScheduleActivityTaskDecisionAttributes,
    signalExternalWorkflowExecutionDecisionAttributes ::
      Lude.Maybe SignalExternalWorkflowExecutionDecisionAttributes,
    startTimerDecisionAttributes ::
      Lude.Maybe StartTimerDecisionAttributes,
    recordMarkerDecisionAttributes ::
      Lude.Maybe RecordMarkerDecisionAttributes,
    failWorkflowExecutionDecisionAttributes ::
      Lude.Maybe FailWorkflowExecutionDecisionAttributes,
    startChildWorkflowExecutionDecisionAttributes ::
      Lude.Maybe StartChildWorkflowExecutionDecisionAttributes,
    completeWorkflowExecutionDecisionAttributes ::
      Lude.Maybe CompleteWorkflowExecutionDecisionAttributes,
    scheduleLambdaFunctionDecisionAttributes ::
      Lude.Maybe ScheduleLambdaFunctionDecisionAttributes,
    requestCancelActivityTaskDecisionAttributes ::
      Lude.Maybe RequestCancelActivityTaskDecisionAttributes,
    cancelWorkflowExecutionDecisionAttributes ::
      Lude.Maybe CancelWorkflowExecutionDecisionAttributes,
    cancelTimerDecisionAttributes ::
      Lude.Maybe CancelTimerDecisionAttributes,
    continueAsNewWorkflowExecutionDecisionAttributes ::
      Lude.Maybe ContinueAsNewWorkflowExecutionDecisionAttributes,
    decisionType :: DecisionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Decision' with the minimum fields required to make a request.
--
-- * 'cancelTimerDecisionAttributes' - Provides the details of the @CancelTimer@ decision. It isn't set for other decision types.
-- * 'cancelWorkflowExecutionDecisionAttributes' - Provides the details of the @CancelWorkflowExecution@ decision. It isn't set for other decision types.
-- * 'completeWorkflowExecutionDecisionAttributes' - Provides the details of the @CompleteWorkflowExecution@ decision. It isn't set for other decision types.
-- * 'continueAsNewWorkflowExecutionDecisionAttributes' - Provides the details of the @ContinueAsNewWorkflowExecution@ decision. It isn't set for other decision types.
-- * 'decisionType' - Specifies the type of the decision.
-- * 'failWorkflowExecutionDecisionAttributes' - Provides the details of the @FailWorkflowExecution@ decision. It isn't set for other decision types.
-- * 'recordMarkerDecisionAttributes' - Provides the details of the @RecordMarker@ decision. It isn't set for other decision types.
-- * 'requestCancelActivityTaskDecisionAttributes' - Provides the details of the @RequestCancelActivityTask@ decision. It isn't set for other decision types.
-- * 'requestCancelExternalWorkflowExecutionDecisionAttributes' - Provides the details of the @RequestCancelExternalWorkflowExecution@ decision. It isn't set for other decision types.
-- * 'scheduleActivityTaskDecisionAttributes' - Provides the details of the @ScheduleActivityTask@ decision. It isn't set for other decision types.
-- * 'scheduleLambdaFunctionDecisionAttributes' - Provides the details of the @ScheduleLambdaFunction@ decision. It isn't set for other decision types.
-- * 'signalExternalWorkflowExecutionDecisionAttributes' - Provides the details of the @SignalExternalWorkflowExecution@ decision. It isn't set for other decision types.
-- * 'startChildWorkflowExecutionDecisionAttributes' - Provides the details of the @StartChildWorkflowExecution@ decision. It isn't set for other decision types.
-- * 'startTimerDecisionAttributes' - Provides the details of the @StartTimer@ decision. It isn't set for other decision types.
mkDecision ::
  -- | 'decisionType'
  DecisionType ->
  Decision
mkDecision pDecisionType_ =
  Decision'
    { requestCancelExternalWorkflowExecutionDecisionAttributes =
        Lude.Nothing,
      scheduleActivityTaskDecisionAttributes = Lude.Nothing,
      signalExternalWorkflowExecutionDecisionAttributes = Lude.Nothing,
      startTimerDecisionAttributes = Lude.Nothing,
      recordMarkerDecisionAttributes = Lude.Nothing,
      failWorkflowExecutionDecisionAttributes = Lude.Nothing,
      startChildWorkflowExecutionDecisionAttributes = Lude.Nothing,
      completeWorkflowExecutionDecisionAttributes = Lude.Nothing,
      scheduleLambdaFunctionDecisionAttributes = Lude.Nothing,
      requestCancelActivityTaskDecisionAttributes = Lude.Nothing,
      cancelWorkflowExecutionDecisionAttributes = Lude.Nothing,
      cancelTimerDecisionAttributes = Lude.Nothing,
      continueAsNewWorkflowExecutionDecisionAttributes = Lude.Nothing,
      decisionType = pDecisionType_
    }

-- | Provides the details of the @RequestCancelExternalWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'requestCancelExternalWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRequestCancelExternalWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes)
dRequestCancelExternalWorkflowExecutionDecisionAttributes = Lens.lens (requestCancelExternalWorkflowExecutionDecisionAttributes :: Decision -> Lude.Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes) (\s a -> s {requestCancelExternalWorkflowExecutionDecisionAttributes = a} :: Decision)
{-# DEPRECATED dRequestCancelExternalWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'requestCancelExternalWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @ScheduleActivityTask@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'scheduleActivityTaskDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScheduleActivityTaskDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe ScheduleActivityTaskDecisionAttributes)
dScheduleActivityTaskDecisionAttributes = Lens.lens (scheduleActivityTaskDecisionAttributes :: Decision -> Lude.Maybe ScheduleActivityTaskDecisionAttributes) (\s a -> s {scheduleActivityTaskDecisionAttributes = a} :: Decision)
{-# DEPRECATED dScheduleActivityTaskDecisionAttributes "Use generic-lens or generic-optics with 'scheduleActivityTaskDecisionAttributes' instead." #-}

-- | Provides the details of the @SignalExternalWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'signalExternalWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSignalExternalWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe SignalExternalWorkflowExecutionDecisionAttributes)
dSignalExternalWorkflowExecutionDecisionAttributes = Lens.lens (signalExternalWorkflowExecutionDecisionAttributes :: Decision -> Lude.Maybe SignalExternalWorkflowExecutionDecisionAttributes) (\s a -> s {signalExternalWorkflowExecutionDecisionAttributes = a} :: Decision)
{-# DEPRECATED dSignalExternalWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'signalExternalWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @StartTimer@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'startTimerDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStartTimerDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe StartTimerDecisionAttributes)
dStartTimerDecisionAttributes = Lens.lens (startTimerDecisionAttributes :: Decision -> Lude.Maybe StartTimerDecisionAttributes) (\s a -> s {startTimerDecisionAttributes = a} :: Decision)
{-# DEPRECATED dStartTimerDecisionAttributes "Use generic-lens or generic-optics with 'startTimerDecisionAttributes' instead." #-}

-- | Provides the details of the @RecordMarker@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'recordMarkerDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRecordMarkerDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe RecordMarkerDecisionAttributes)
dRecordMarkerDecisionAttributes = Lens.lens (recordMarkerDecisionAttributes :: Decision -> Lude.Maybe RecordMarkerDecisionAttributes) (\s a -> s {recordMarkerDecisionAttributes = a} :: Decision)
{-# DEPRECATED dRecordMarkerDecisionAttributes "Use generic-lens or generic-optics with 'recordMarkerDecisionAttributes' instead." #-}

-- | Provides the details of the @FailWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'failWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFailWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe FailWorkflowExecutionDecisionAttributes)
dFailWorkflowExecutionDecisionAttributes = Lens.lens (failWorkflowExecutionDecisionAttributes :: Decision -> Lude.Maybe FailWorkflowExecutionDecisionAttributes) (\s a -> s {failWorkflowExecutionDecisionAttributes = a} :: Decision)
{-# DEPRECATED dFailWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'failWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @StartChildWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'startChildWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStartChildWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe StartChildWorkflowExecutionDecisionAttributes)
dStartChildWorkflowExecutionDecisionAttributes = Lens.lens (startChildWorkflowExecutionDecisionAttributes :: Decision -> Lude.Maybe StartChildWorkflowExecutionDecisionAttributes) (\s a -> s {startChildWorkflowExecutionDecisionAttributes = a} :: Decision)
{-# DEPRECATED dStartChildWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'startChildWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @CompleteWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'completeWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCompleteWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe CompleteWorkflowExecutionDecisionAttributes)
dCompleteWorkflowExecutionDecisionAttributes = Lens.lens (completeWorkflowExecutionDecisionAttributes :: Decision -> Lude.Maybe CompleteWorkflowExecutionDecisionAttributes) (\s a -> s {completeWorkflowExecutionDecisionAttributes = a} :: Decision)
{-# DEPRECATED dCompleteWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'completeWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @ScheduleLambdaFunction@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'scheduleLambdaFunctionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScheduleLambdaFunctionDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe ScheduleLambdaFunctionDecisionAttributes)
dScheduleLambdaFunctionDecisionAttributes = Lens.lens (scheduleLambdaFunctionDecisionAttributes :: Decision -> Lude.Maybe ScheduleLambdaFunctionDecisionAttributes) (\s a -> s {scheduleLambdaFunctionDecisionAttributes = a} :: Decision)
{-# DEPRECATED dScheduleLambdaFunctionDecisionAttributes "Use generic-lens or generic-optics with 'scheduleLambdaFunctionDecisionAttributes' instead." #-}

-- | Provides the details of the @RequestCancelActivityTask@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'requestCancelActivityTaskDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRequestCancelActivityTaskDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe RequestCancelActivityTaskDecisionAttributes)
dRequestCancelActivityTaskDecisionAttributes = Lens.lens (requestCancelActivityTaskDecisionAttributes :: Decision -> Lude.Maybe RequestCancelActivityTaskDecisionAttributes) (\s a -> s {requestCancelActivityTaskDecisionAttributes = a} :: Decision)
{-# DEPRECATED dRequestCancelActivityTaskDecisionAttributes "Use generic-lens or generic-optics with 'requestCancelActivityTaskDecisionAttributes' instead." #-}

-- | Provides the details of the @CancelWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'cancelWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCancelWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe CancelWorkflowExecutionDecisionAttributes)
dCancelWorkflowExecutionDecisionAttributes = Lens.lens (cancelWorkflowExecutionDecisionAttributes :: Decision -> Lude.Maybe CancelWorkflowExecutionDecisionAttributes) (\s a -> s {cancelWorkflowExecutionDecisionAttributes = a} :: Decision)
{-# DEPRECATED dCancelWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'cancelWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @CancelTimer@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'cancelTimerDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCancelTimerDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe CancelTimerDecisionAttributes)
dCancelTimerDecisionAttributes = Lens.lens (cancelTimerDecisionAttributes :: Decision -> Lude.Maybe CancelTimerDecisionAttributes) (\s a -> s {cancelTimerDecisionAttributes = a} :: Decision)
{-# DEPRECATED dCancelTimerDecisionAttributes "Use generic-lens or generic-optics with 'cancelTimerDecisionAttributes' instead." #-}

-- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'continueAsNewWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dContinueAsNewWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Lude.Maybe ContinueAsNewWorkflowExecutionDecisionAttributes)
dContinueAsNewWorkflowExecutionDecisionAttributes = Lens.lens (continueAsNewWorkflowExecutionDecisionAttributes :: Decision -> Lude.Maybe ContinueAsNewWorkflowExecutionDecisionAttributes) (\s a -> s {continueAsNewWorkflowExecutionDecisionAttributes = a} :: Decision)
{-# DEPRECATED dContinueAsNewWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'continueAsNewWorkflowExecutionDecisionAttributes' instead." #-}

-- | Specifies the type of the decision.
--
-- /Note:/ Consider using 'decisionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDecisionType :: Lens.Lens' Decision DecisionType
dDecisionType = Lens.lens (decisionType :: Decision -> DecisionType) (\s a -> s {decisionType = a} :: Decision)
{-# DEPRECATED dDecisionType "Use generic-lens or generic-optics with 'decisionType' instead." #-}

instance Lude.ToJSON Decision where
  toJSON Decision' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ( "requestCancelExternalWorkflowExecutionDecisionAttributes"
                Lude..=
            )
              Lude.<$> requestCancelExternalWorkflowExecutionDecisionAttributes,
            ("scheduleActivityTaskDecisionAttributes" Lude..=)
              Lude.<$> scheduleActivityTaskDecisionAttributes,
            ("signalExternalWorkflowExecutionDecisionAttributes" Lude..=)
              Lude.<$> signalExternalWorkflowExecutionDecisionAttributes,
            ("startTimerDecisionAttributes" Lude..=)
              Lude.<$> startTimerDecisionAttributes,
            ("recordMarkerDecisionAttributes" Lude..=)
              Lude.<$> recordMarkerDecisionAttributes,
            ("failWorkflowExecutionDecisionAttributes" Lude..=)
              Lude.<$> failWorkflowExecutionDecisionAttributes,
            ("startChildWorkflowExecutionDecisionAttributes" Lude..=)
              Lude.<$> startChildWorkflowExecutionDecisionAttributes,
            ("completeWorkflowExecutionDecisionAttributes" Lude..=)
              Lude.<$> completeWorkflowExecutionDecisionAttributes,
            ("scheduleLambdaFunctionDecisionAttributes" Lude..=)
              Lude.<$> scheduleLambdaFunctionDecisionAttributes,
            ("requestCancelActivityTaskDecisionAttributes" Lude..=)
              Lude.<$> requestCancelActivityTaskDecisionAttributes,
            ("cancelWorkflowExecutionDecisionAttributes" Lude..=)
              Lude.<$> cancelWorkflowExecutionDecisionAttributes,
            ("cancelTimerDecisionAttributes" Lude..=)
              Lude.<$> cancelTimerDecisionAttributes,
            ("continueAsNewWorkflowExecutionDecisionAttributes" Lude..=)
              Lude.<$> continueAsNewWorkflowExecutionDecisionAttributes,
            Lude.Just ("decisionType" Lude..= decisionType)
          ]
      )

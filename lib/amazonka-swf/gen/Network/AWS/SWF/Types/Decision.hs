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
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.CancelTimerDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.CancelWorkflowExecutionDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.CompleteWorkflowExecutionDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.DecisionType as Types
import qualified Network.AWS.SWF.Types.FailWorkflowExecutionDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.RecordMarkerDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.RequestCancelActivityTaskDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.ScheduleActivityTaskDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.ScheduleLambdaFunctionDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.StartChildWorkflowExecutionDecisionAttributes as Types
import qualified Network.AWS.SWF.Types.StartTimerDecisionAttributes as Types

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
  { -- | Specifies the type of the decision.
    decisionType :: Types.DecisionType,
    -- | Provides the details of the @CancelTimer@ decision. It isn't set for other decision types.
    cancelTimerDecisionAttributes :: Core.Maybe Types.CancelTimerDecisionAttributes,
    -- | Provides the details of the @CancelWorkflowExecution@ decision. It isn't set for other decision types.
    cancelWorkflowExecutionDecisionAttributes :: Core.Maybe Types.CancelWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @CompleteWorkflowExecution@ decision. It isn't set for other decision types.
    completeWorkflowExecutionDecisionAttributes :: Core.Maybe Types.CompleteWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision. It isn't set for other decision types.
    continueAsNewWorkflowExecutionDecisionAttributes :: Core.Maybe Types.ContinueAsNewWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @FailWorkflowExecution@ decision. It isn't set for other decision types.
    failWorkflowExecutionDecisionAttributes :: Core.Maybe Types.FailWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @RecordMarker@ decision. It isn't set for other decision types.
    recordMarkerDecisionAttributes :: Core.Maybe Types.RecordMarkerDecisionAttributes,
    -- | Provides the details of the @RequestCancelActivityTask@ decision. It isn't set for other decision types.
    requestCancelActivityTaskDecisionAttributes :: Core.Maybe Types.RequestCancelActivityTaskDecisionAttributes,
    -- | Provides the details of the @RequestCancelExternalWorkflowExecution@ decision. It isn't set for other decision types.
    requestCancelExternalWorkflowExecutionDecisionAttributes :: Core.Maybe Types.RequestCancelExternalWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @ScheduleActivityTask@ decision. It isn't set for other decision types.
    scheduleActivityTaskDecisionAttributes :: Core.Maybe Types.ScheduleActivityTaskDecisionAttributes,
    -- | Provides the details of the @ScheduleLambdaFunction@ decision. It isn't set for other decision types.
    scheduleLambdaFunctionDecisionAttributes :: Core.Maybe Types.ScheduleLambdaFunctionDecisionAttributes,
    -- | Provides the details of the @SignalExternalWorkflowExecution@ decision. It isn't set for other decision types.
    signalExternalWorkflowExecutionDecisionAttributes :: Core.Maybe Types.SignalExternalWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @StartChildWorkflowExecution@ decision. It isn't set for other decision types.
    startChildWorkflowExecutionDecisionAttributes :: Core.Maybe Types.StartChildWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @StartTimer@ decision. It isn't set for other decision types.
    startTimerDecisionAttributes :: Core.Maybe Types.StartTimerDecisionAttributes
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Decision' value with any optional fields omitted.
mkDecision ::
  -- | 'decisionType'
  Types.DecisionType ->
  Decision
mkDecision decisionType =
  Decision'
    { decisionType,
      cancelTimerDecisionAttributes = Core.Nothing,
      cancelWorkflowExecutionDecisionAttributes = Core.Nothing,
      completeWorkflowExecutionDecisionAttributes = Core.Nothing,
      continueAsNewWorkflowExecutionDecisionAttributes = Core.Nothing,
      failWorkflowExecutionDecisionAttributes = Core.Nothing,
      recordMarkerDecisionAttributes = Core.Nothing,
      requestCancelActivityTaskDecisionAttributes = Core.Nothing,
      requestCancelExternalWorkflowExecutionDecisionAttributes =
        Core.Nothing,
      scheduleActivityTaskDecisionAttributes = Core.Nothing,
      scheduleLambdaFunctionDecisionAttributes = Core.Nothing,
      signalExternalWorkflowExecutionDecisionAttributes = Core.Nothing,
      startChildWorkflowExecutionDecisionAttributes = Core.Nothing,
      startTimerDecisionAttributes = Core.Nothing
    }

-- | Specifies the type of the decision.
--
-- /Note:/ Consider using 'decisionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDecisionType :: Lens.Lens' Decision Types.DecisionType
dDecisionType = Lens.field @"decisionType"
{-# DEPRECATED dDecisionType "Use generic-lens or generic-optics with 'decisionType' instead." #-}

-- | Provides the details of the @CancelTimer@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'cancelTimerDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCancelTimerDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.CancelTimerDecisionAttributes)
dCancelTimerDecisionAttributes = Lens.field @"cancelTimerDecisionAttributes"
{-# DEPRECATED dCancelTimerDecisionAttributes "Use generic-lens or generic-optics with 'cancelTimerDecisionAttributes' instead." #-}

-- | Provides the details of the @CancelWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'cancelWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCancelWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.CancelWorkflowExecutionDecisionAttributes)
dCancelWorkflowExecutionDecisionAttributes = Lens.field @"cancelWorkflowExecutionDecisionAttributes"
{-# DEPRECATED dCancelWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'cancelWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @CompleteWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'completeWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCompleteWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.CompleteWorkflowExecutionDecisionAttributes)
dCompleteWorkflowExecutionDecisionAttributes = Lens.field @"completeWorkflowExecutionDecisionAttributes"
{-# DEPRECATED dCompleteWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'completeWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'continueAsNewWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dContinueAsNewWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.ContinueAsNewWorkflowExecutionDecisionAttributes)
dContinueAsNewWorkflowExecutionDecisionAttributes = Lens.field @"continueAsNewWorkflowExecutionDecisionAttributes"
{-# DEPRECATED dContinueAsNewWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'continueAsNewWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @FailWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'failWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFailWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.FailWorkflowExecutionDecisionAttributes)
dFailWorkflowExecutionDecisionAttributes = Lens.field @"failWorkflowExecutionDecisionAttributes"
{-# DEPRECATED dFailWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'failWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @RecordMarker@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'recordMarkerDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRecordMarkerDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.RecordMarkerDecisionAttributes)
dRecordMarkerDecisionAttributes = Lens.field @"recordMarkerDecisionAttributes"
{-# DEPRECATED dRecordMarkerDecisionAttributes "Use generic-lens or generic-optics with 'recordMarkerDecisionAttributes' instead." #-}

-- | Provides the details of the @RequestCancelActivityTask@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'requestCancelActivityTaskDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRequestCancelActivityTaskDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.RequestCancelActivityTaskDecisionAttributes)
dRequestCancelActivityTaskDecisionAttributes = Lens.field @"requestCancelActivityTaskDecisionAttributes"
{-# DEPRECATED dRequestCancelActivityTaskDecisionAttributes "Use generic-lens or generic-optics with 'requestCancelActivityTaskDecisionAttributes' instead." #-}

-- | Provides the details of the @RequestCancelExternalWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'requestCancelExternalWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRequestCancelExternalWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.RequestCancelExternalWorkflowExecutionDecisionAttributes)
dRequestCancelExternalWorkflowExecutionDecisionAttributes = Lens.field @"requestCancelExternalWorkflowExecutionDecisionAttributes"
{-# DEPRECATED dRequestCancelExternalWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'requestCancelExternalWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @ScheduleActivityTask@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'scheduleActivityTaskDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScheduleActivityTaskDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.ScheduleActivityTaskDecisionAttributes)
dScheduleActivityTaskDecisionAttributes = Lens.field @"scheduleActivityTaskDecisionAttributes"
{-# DEPRECATED dScheduleActivityTaskDecisionAttributes "Use generic-lens or generic-optics with 'scheduleActivityTaskDecisionAttributes' instead." #-}

-- | Provides the details of the @ScheduleLambdaFunction@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'scheduleLambdaFunctionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScheduleLambdaFunctionDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.ScheduleLambdaFunctionDecisionAttributes)
dScheduleLambdaFunctionDecisionAttributes = Lens.field @"scheduleLambdaFunctionDecisionAttributes"
{-# DEPRECATED dScheduleLambdaFunctionDecisionAttributes "Use generic-lens or generic-optics with 'scheduleLambdaFunctionDecisionAttributes' instead." #-}

-- | Provides the details of the @SignalExternalWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'signalExternalWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSignalExternalWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.SignalExternalWorkflowExecutionDecisionAttributes)
dSignalExternalWorkflowExecutionDecisionAttributes = Lens.field @"signalExternalWorkflowExecutionDecisionAttributes"
{-# DEPRECATED dSignalExternalWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'signalExternalWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @StartChildWorkflowExecution@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'startChildWorkflowExecutionDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStartChildWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.StartChildWorkflowExecutionDecisionAttributes)
dStartChildWorkflowExecutionDecisionAttributes = Lens.field @"startChildWorkflowExecutionDecisionAttributes"
{-# DEPRECATED dStartChildWorkflowExecutionDecisionAttributes "Use generic-lens or generic-optics with 'startChildWorkflowExecutionDecisionAttributes' instead." #-}

-- | Provides the details of the @StartTimer@ decision. It isn't set for other decision types.
--
-- /Note:/ Consider using 'startTimerDecisionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStartTimerDecisionAttributes :: Lens.Lens' Decision (Core.Maybe Types.StartTimerDecisionAttributes)
dStartTimerDecisionAttributes = Lens.field @"startTimerDecisionAttributes"
{-# DEPRECATED dStartTimerDecisionAttributes "Use generic-lens or generic-optics with 'startTimerDecisionAttributes' instead." #-}

instance Core.FromJSON Decision where
  toJSON Decision {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("decisionType" Core..= decisionType),
            ("cancelTimerDecisionAttributes" Core..=)
              Core.<$> cancelTimerDecisionAttributes,
            ("cancelWorkflowExecutionDecisionAttributes" Core..=)
              Core.<$> cancelWorkflowExecutionDecisionAttributes,
            ("completeWorkflowExecutionDecisionAttributes" Core..=)
              Core.<$> completeWorkflowExecutionDecisionAttributes,
            ("continueAsNewWorkflowExecutionDecisionAttributes" Core..=)
              Core.<$> continueAsNewWorkflowExecutionDecisionAttributes,
            ("failWorkflowExecutionDecisionAttributes" Core..=)
              Core.<$> failWorkflowExecutionDecisionAttributes,
            ("recordMarkerDecisionAttributes" Core..=)
              Core.<$> recordMarkerDecisionAttributes,
            ("requestCancelActivityTaskDecisionAttributes" Core..=)
              Core.<$> requestCancelActivityTaskDecisionAttributes,
            ( "requestCancelExternalWorkflowExecutionDecisionAttributes"
                Core..=
            )
              Core.<$> requestCancelExternalWorkflowExecutionDecisionAttributes,
            ("scheduleActivityTaskDecisionAttributes" Core..=)
              Core.<$> scheduleActivityTaskDecisionAttributes,
            ("scheduleLambdaFunctionDecisionAttributes" Core..=)
              Core.<$> scheduleLambdaFunctionDecisionAttributes,
            ("signalExternalWorkflowExecutionDecisionAttributes" Core..=)
              Core.<$> signalExternalWorkflowExecutionDecisionAttributes,
            ("startChildWorkflowExecutionDecisionAttributes" Core..=)
              Core.<$> startChildWorkflowExecutionDecisionAttributes,
            ("startTimerDecisionAttributes" Core..=)
              Core.<$> startTimerDecisionAttributes
          ]
      )

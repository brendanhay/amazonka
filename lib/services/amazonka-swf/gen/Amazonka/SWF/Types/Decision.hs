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
-- Module      : Amazonka.SWF.Types.Decision
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.Decision where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.CancelTimerDecisionAttributes
import Amazonka.SWF.Types.CancelWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.CompleteWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.DecisionType
import Amazonka.SWF.Types.FailWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.RecordMarkerDecisionAttributes
import Amazonka.SWF.Types.RequestCancelActivityTaskDecisionAttributes
import Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.ScheduleActivityTaskDecisionAttributes
import Amazonka.SWF.Types.ScheduleLambdaFunctionDecisionAttributes
import Amazonka.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.StartChildWorkflowExecutionDecisionAttributes
import Amazonka.SWF.Types.StartTimerDecisionAttributes

-- | Specifies a decision made by the decider. A decision can be one of these
-- types:
--
-- -   @CancelTimer@ – Cancels a previously started timer and records a
--     @TimerCanceled@ event in the history.
--
-- -   @CancelWorkflowExecution@ – Closes the workflow execution and
--     records a @WorkflowExecutionCanceled@ event in the history.
--
-- -   @CompleteWorkflowExecution@ – Closes the workflow execution and
--     records a @WorkflowExecutionCompleted@ event in the history .
--
-- -   @ContinueAsNewWorkflowExecution@ – Closes the workflow execution and
--     starts a new workflow execution of the same type using the same
--     workflow ID and a unique run Id. A @WorkflowExecutionContinuedAsNew@
--     event is recorded in the history.
--
-- -   @FailWorkflowExecution@ – Closes the workflow execution and records
--     a @WorkflowExecutionFailed@ event in the history.
--
-- -   @RecordMarker@ – Records a @MarkerRecorded@ event in the history.
--     Markers can be used for adding custom information in the history for
--     instance to let deciders know that they don\'t need to look at the
--     history beyond the marker event.
--
-- -   @RequestCancelActivityTask@ – Attempts to cancel a previously
--     scheduled activity task. If the activity task was scheduled but has
--     not been assigned to a worker, then it is canceled. If the activity
--     task was already assigned to a worker, then the worker is informed
--     that cancellation has been requested in the response to
--     RecordActivityTaskHeartbeat.
--
-- -   @RequestCancelExternalWorkflowExecution@ – Requests that a request
--     be made to cancel the specified external workflow execution and
--     records a @RequestCancelExternalWorkflowExecutionInitiated@ event in
--     the history.
--
-- -   @ScheduleActivityTask@ – Schedules an activity task.
--
-- -   @SignalExternalWorkflowExecution@ – Requests a signal to be
--     delivered to the specified external workflow execution and records a
--     @SignalExternalWorkflowExecutionInitiated@ event in the history.
--
-- -   @StartChildWorkflowExecution@ – Requests that a child workflow
--     execution be started and records a
--     @StartChildWorkflowExecutionInitiated@ event in the history. The
--     child workflow execution is a separate workflow execution with its
--     own history.
--
-- -   @StartTimer@ – Starts a timer for this workflow execution and
--     records a @TimerStarted@ event in the history. This timer fires
--     after the specified delay and record a @TimerFired@ event.
--
-- __Access Control__
--
-- If you grant permission to use @RespondDecisionTaskCompleted@, you can
-- use IAM policies to express permissions for the list of decisions
-- returned by this action as if they were members of the API. Treating
-- decisions as a pseudo API maintains a uniform conceptual model and helps
-- keep policies readable. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- __Decision Failure__
--
-- Decisions can fail for several reasons
--
-- -   The ordering of decisions should follow a logical flow. Some
--     decisions might not make sense in the current context of the
--     workflow execution and therefore fails.
--
-- -   A limit on your account was reached.
--
-- -   The decision lacks sufficient permissions.
--
-- One of the following events might be added to the history to indicate an
-- error. The event attribute\'s @cause@ parameter indicates the cause. If
-- @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed because
-- it lacked sufficient permissions. For details and example IAM policies,
-- see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- -   @ScheduleActivityTaskFailed@ – A @ScheduleActivityTask@ decision
--     failed. This could happen if the activity type specified in the
--     decision isn\'t registered, is in a deprecated state, or the
--     decision isn\'t properly configured.
--
-- -   @RequestCancelActivityTaskFailed@ – A @RequestCancelActivityTask@
--     decision failed. This could happen if there is no open activity task
--     with the specified activityId.
--
-- -   @StartTimerFailed@ – A @StartTimer@ decision failed. This could
--     happen if there is another open timer with the same timerId.
--
-- -   @CancelTimerFailed@ – A @CancelTimer@ decision failed. This could
--     happen if there is no open timer with the specified timerId.
--
-- -   @StartChildWorkflowExecutionFailed@ – A
--     @StartChildWorkflowExecution@ decision failed. This could happen if
--     the workflow type specified isn\'t registered, is deprecated, or the
--     decision isn\'t properly configured.
--
-- -   @SignalExternalWorkflowExecutionFailed@ – A
--     @SignalExternalWorkflowExecution@ decision failed. This could happen
--     if the @workflowID@ specified in the decision was incorrect.
--
-- -   @RequestCancelExternalWorkflowExecutionFailed@ – A
--     @RequestCancelExternalWorkflowExecution@ decision failed. This could
--     happen if the @workflowID@ specified in the decision was incorrect.
--
-- -   @CancelWorkflowExecutionFailed@ – A @CancelWorkflowExecution@
--     decision failed. This could happen if there is an unhandled decision
--     task pending in the workflow execution.
--
-- -   @CompleteWorkflowExecutionFailed@ – A @CompleteWorkflowExecution@
--     decision failed. This could happen if there is an unhandled decision
--     task pending in the workflow execution.
--
-- -   @ContinueAsNewWorkflowExecutionFailed@ – A
--     @ContinueAsNewWorkflowExecution@ decision failed. This could happen
--     if there is an unhandled decision task pending in the workflow
--     execution or the ContinueAsNewWorkflowExecution decision was not
--     configured correctly.
--
-- -   @FailWorkflowExecutionFailed@ – A @FailWorkflowExecution@ decision
--     failed. This could happen if there is an unhandled decision task
--     pending in the workflow execution.
--
-- The preceding error events might occur due to an error in the decider
-- logic, which might put the workflow execution in an unstable state The
-- cause field in the event structure for the error event indicates the
-- cause of the error.
--
-- A workflow execution may be closed by the decider by returning one of
-- the following decisions when completing a decision task:
-- @CompleteWorkflowExecution@, @FailWorkflowExecution@,
-- @CancelWorkflowExecution@ and @ContinueAsNewWorkflowExecution@. An
-- @UnhandledDecision@ fault is returned if a workflow closing decision is
-- specified and a signal or activity event had been added to the history
-- while the decision task was being performed by the decider. Unlike the
-- above situations which are logic issues, this fault is always possible
-- because of race conditions in a distributed system. The right action
-- here is to call RespondDecisionTaskCompleted without any decisions. This
-- would result in another decision task with these new events included in
-- the history. The decider should handle the new events and may decide to
-- close the workflow execution.
--
-- __How to Code a Decision__
--
-- You code a decision by first setting the decision type field to one of
-- the above decision values, and then set the corresponding attributes
-- field shown below:
--
-- -   @ ScheduleActivityTaskDecisionAttributes @
--
-- -   @ RequestCancelActivityTaskDecisionAttributes @
--
-- -   @ CompleteWorkflowExecutionDecisionAttributes @
--
-- -   @ FailWorkflowExecutionDecisionAttributes @
--
-- -   @ CancelWorkflowExecutionDecisionAttributes @
--
-- -   @ ContinueAsNewWorkflowExecutionDecisionAttributes @
--
-- -   @ RecordMarkerDecisionAttributes @
--
-- -   @ StartTimerDecisionAttributes @
--
-- -   @ CancelTimerDecisionAttributes @
--
-- -   @ SignalExternalWorkflowExecutionDecisionAttributes @
--
-- -   @ RequestCancelExternalWorkflowExecutionDecisionAttributes @
--
-- -   @ StartChildWorkflowExecutionDecisionAttributes @
--
-- /See:/ 'newDecision' smart constructor.
data Decision = Decision'
  { -- | Provides the details of the @CancelTimer@ decision. It isn\'t set for
    -- other decision types.
    cancelTimerDecisionAttributes :: Prelude.Maybe CancelTimerDecisionAttributes,
    -- | Provides the details of the @CancelWorkflowExecution@ decision. It
    -- isn\'t set for other decision types.
    cancelWorkflowExecutionDecisionAttributes :: Prelude.Maybe CancelWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @CompleteWorkflowExecution@ decision. It
    -- isn\'t set for other decision types.
    completeWorkflowExecutionDecisionAttributes :: Prelude.Maybe CompleteWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision.
    -- It isn\'t set for other decision types.
    continueAsNewWorkflowExecutionDecisionAttributes :: Prelude.Maybe ContinueAsNewWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @FailWorkflowExecution@ decision. It isn\'t
    -- set for other decision types.
    failWorkflowExecutionDecisionAttributes :: Prelude.Maybe FailWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @RecordMarker@ decision. It isn\'t set for
    -- other decision types.
    recordMarkerDecisionAttributes :: Prelude.Maybe RecordMarkerDecisionAttributes,
    -- | Provides the details of the @RequestCancelActivityTask@ decision. It
    -- isn\'t set for other decision types.
    requestCancelActivityTaskDecisionAttributes :: Prelude.Maybe RequestCancelActivityTaskDecisionAttributes,
    -- | Provides the details of the @RequestCancelExternalWorkflowExecution@
    -- decision. It isn\'t set for other decision types.
    requestCancelExternalWorkflowExecutionDecisionAttributes :: Prelude.Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @ScheduleActivityTask@ decision. It isn\'t
    -- set for other decision types.
    scheduleActivityTaskDecisionAttributes :: Prelude.Maybe ScheduleActivityTaskDecisionAttributes,
    -- | Provides the details of the @ScheduleLambdaFunction@ decision. It isn\'t
    -- set for other decision types.
    scheduleLambdaFunctionDecisionAttributes :: Prelude.Maybe ScheduleLambdaFunctionDecisionAttributes,
    -- | Provides the details of the @SignalExternalWorkflowExecution@ decision.
    -- It isn\'t set for other decision types.
    signalExternalWorkflowExecutionDecisionAttributes :: Prelude.Maybe SignalExternalWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @StartChildWorkflowExecution@ decision. It
    -- isn\'t set for other decision types.
    startChildWorkflowExecutionDecisionAttributes :: Prelude.Maybe StartChildWorkflowExecutionDecisionAttributes,
    -- | Provides the details of the @StartTimer@ decision. It isn\'t set for
    -- other decision types.
    startTimerDecisionAttributes :: Prelude.Maybe StartTimerDecisionAttributes,
    -- | Specifies the type of the decision.
    decisionType :: DecisionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Decision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancelTimerDecisionAttributes', 'decision_cancelTimerDecisionAttributes' - Provides the details of the @CancelTimer@ decision. It isn\'t set for
-- other decision types.
--
-- 'cancelWorkflowExecutionDecisionAttributes', 'decision_cancelWorkflowExecutionDecisionAttributes' - Provides the details of the @CancelWorkflowExecution@ decision. It
-- isn\'t set for other decision types.
--
-- 'completeWorkflowExecutionDecisionAttributes', 'decision_completeWorkflowExecutionDecisionAttributes' - Provides the details of the @CompleteWorkflowExecution@ decision. It
-- isn\'t set for other decision types.
--
-- 'continueAsNewWorkflowExecutionDecisionAttributes', 'decision_continueAsNewWorkflowExecutionDecisionAttributes' - Provides the details of the @ContinueAsNewWorkflowExecution@ decision.
-- It isn\'t set for other decision types.
--
-- 'failWorkflowExecutionDecisionAttributes', 'decision_failWorkflowExecutionDecisionAttributes' - Provides the details of the @FailWorkflowExecution@ decision. It isn\'t
-- set for other decision types.
--
-- 'recordMarkerDecisionAttributes', 'decision_recordMarkerDecisionAttributes' - Provides the details of the @RecordMarker@ decision. It isn\'t set for
-- other decision types.
--
-- 'requestCancelActivityTaskDecisionAttributes', 'decision_requestCancelActivityTaskDecisionAttributes' - Provides the details of the @RequestCancelActivityTask@ decision. It
-- isn\'t set for other decision types.
--
-- 'requestCancelExternalWorkflowExecutionDecisionAttributes', 'decision_requestCancelExternalWorkflowExecutionDecisionAttributes' - Provides the details of the @RequestCancelExternalWorkflowExecution@
-- decision. It isn\'t set for other decision types.
--
-- 'scheduleActivityTaskDecisionAttributes', 'decision_scheduleActivityTaskDecisionAttributes' - Provides the details of the @ScheduleActivityTask@ decision. It isn\'t
-- set for other decision types.
--
-- 'scheduleLambdaFunctionDecisionAttributes', 'decision_scheduleLambdaFunctionDecisionAttributes' - Provides the details of the @ScheduleLambdaFunction@ decision. It isn\'t
-- set for other decision types.
--
-- 'signalExternalWorkflowExecutionDecisionAttributes', 'decision_signalExternalWorkflowExecutionDecisionAttributes' - Provides the details of the @SignalExternalWorkflowExecution@ decision.
-- It isn\'t set for other decision types.
--
-- 'startChildWorkflowExecutionDecisionAttributes', 'decision_startChildWorkflowExecutionDecisionAttributes' - Provides the details of the @StartChildWorkflowExecution@ decision. It
-- isn\'t set for other decision types.
--
-- 'startTimerDecisionAttributes', 'decision_startTimerDecisionAttributes' - Provides the details of the @StartTimer@ decision. It isn\'t set for
-- other decision types.
--
-- 'decisionType', 'decision_decisionType' - Specifies the type of the decision.
newDecision ::
  -- | 'decisionType'
  DecisionType ->
  Decision
newDecision pDecisionType_ =
  Decision'
    { cancelTimerDecisionAttributes =
        Prelude.Nothing,
      cancelWorkflowExecutionDecisionAttributes =
        Prelude.Nothing,
      completeWorkflowExecutionDecisionAttributes =
        Prelude.Nothing,
      continueAsNewWorkflowExecutionDecisionAttributes =
        Prelude.Nothing,
      failWorkflowExecutionDecisionAttributes =
        Prelude.Nothing,
      recordMarkerDecisionAttributes = Prelude.Nothing,
      requestCancelActivityTaskDecisionAttributes =
        Prelude.Nothing,
      requestCancelExternalWorkflowExecutionDecisionAttributes =
        Prelude.Nothing,
      scheduleActivityTaskDecisionAttributes =
        Prelude.Nothing,
      scheduleLambdaFunctionDecisionAttributes =
        Prelude.Nothing,
      signalExternalWorkflowExecutionDecisionAttributes =
        Prelude.Nothing,
      startChildWorkflowExecutionDecisionAttributes =
        Prelude.Nothing,
      startTimerDecisionAttributes = Prelude.Nothing,
      decisionType = pDecisionType_
    }

-- | Provides the details of the @CancelTimer@ decision. It isn\'t set for
-- other decision types.
decision_cancelTimerDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe CancelTimerDecisionAttributes)
decision_cancelTimerDecisionAttributes = Lens.lens (\Decision' {cancelTimerDecisionAttributes} -> cancelTimerDecisionAttributes) (\s@Decision' {} a -> s {cancelTimerDecisionAttributes = a} :: Decision)

-- | Provides the details of the @CancelWorkflowExecution@ decision. It
-- isn\'t set for other decision types.
decision_cancelWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe CancelWorkflowExecutionDecisionAttributes)
decision_cancelWorkflowExecutionDecisionAttributes = Lens.lens (\Decision' {cancelWorkflowExecutionDecisionAttributes} -> cancelWorkflowExecutionDecisionAttributes) (\s@Decision' {} a -> s {cancelWorkflowExecutionDecisionAttributes = a} :: Decision)

-- | Provides the details of the @CompleteWorkflowExecution@ decision. It
-- isn\'t set for other decision types.
decision_completeWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe CompleteWorkflowExecutionDecisionAttributes)
decision_completeWorkflowExecutionDecisionAttributes = Lens.lens (\Decision' {completeWorkflowExecutionDecisionAttributes} -> completeWorkflowExecutionDecisionAttributes) (\s@Decision' {} a -> s {completeWorkflowExecutionDecisionAttributes = a} :: Decision)

-- | Provides the details of the @ContinueAsNewWorkflowExecution@ decision.
-- It isn\'t set for other decision types.
decision_continueAsNewWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe ContinueAsNewWorkflowExecutionDecisionAttributes)
decision_continueAsNewWorkflowExecutionDecisionAttributes = Lens.lens (\Decision' {continueAsNewWorkflowExecutionDecisionAttributes} -> continueAsNewWorkflowExecutionDecisionAttributes) (\s@Decision' {} a -> s {continueAsNewWorkflowExecutionDecisionAttributes = a} :: Decision)

-- | Provides the details of the @FailWorkflowExecution@ decision. It isn\'t
-- set for other decision types.
decision_failWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe FailWorkflowExecutionDecisionAttributes)
decision_failWorkflowExecutionDecisionAttributes = Lens.lens (\Decision' {failWorkflowExecutionDecisionAttributes} -> failWorkflowExecutionDecisionAttributes) (\s@Decision' {} a -> s {failWorkflowExecutionDecisionAttributes = a} :: Decision)

-- | Provides the details of the @RecordMarker@ decision. It isn\'t set for
-- other decision types.
decision_recordMarkerDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe RecordMarkerDecisionAttributes)
decision_recordMarkerDecisionAttributes = Lens.lens (\Decision' {recordMarkerDecisionAttributes} -> recordMarkerDecisionAttributes) (\s@Decision' {} a -> s {recordMarkerDecisionAttributes = a} :: Decision)

-- | Provides the details of the @RequestCancelActivityTask@ decision. It
-- isn\'t set for other decision types.
decision_requestCancelActivityTaskDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe RequestCancelActivityTaskDecisionAttributes)
decision_requestCancelActivityTaskDecisionAttributes = Lens.lens (\Decision' {requestCancelActivityTaskDecisionAttributes} -> requestCancelActivityTaskDecisionAttributes) (\s@Decision' {} a -> s {requestCancelActivityTaskDecisionAttributes = a} :: Decision)

-- | Provides the details of the @RequestCancelExternalWorkflowExecution@
-- decision. It isn\'t set for other decision types.
decision_requestCancelExternalWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes)
decision_requestCancelExternalWorkflowExecutionDecisionAttributes = Lens.lens (\Decision' {requestCancelExternalWorkflowExecutionDecisionAttributes} -> requestCancelExternalWorkflowExecutionDecisionAttributes) (\s@Decision' {} a -> s {requestCancelExternalWorkflowExecutionDecisionAttributes = a} :: Decision)

-- | Provides the details of the @ScheduleActivityTask@ decision. It isn\'t
-- set for other decision types.
decision_scheduleActivityTaskDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe ScheduleActivityTaskDecisionAttributes)
decision_scheduleActivityTaskDecisionAttributes = Lens.lens (\Decision' {scheduleActivityTaskDecisionAttributes} -> scheduleActivityTaskDecisionAttributes) (\s@Decision' {} a -> s {scheduleActivityTaskDecisionAttributes = a} :: Decision)

-- | Provides the details of the @ScheduleLambdaFunction@ decision. It isn\'t
-- set for other decision types.
decision_scheduleLambdaFunctionDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe ScheduleLambdaFunctionDecisionAttributes)
decision_scheduleLambdaFunctionDecisionAttributes = Lens.lens (\Decision' {scheduleLambdaFunctionDecisionAttributes} -> scheduleLambdaFunctionDecisionAttributes) (\s@Decision' {} a -> s {scheduleLambdaFunctionDecisionAttributes = a} :: Decision)

-- | Provides the details of the @SignalExternalWorkflowExecution@ decision.
-- It isn\'t set for other decision types.
decision_signalExternalWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe SignalExternalWorkflowExecutionDecisionAttributes)
decision_signalExternalWorkflowExecutionDecisionAttributes = Lens.lens (\Decision' {signalExternalWorkflowExecutionDecisionAttributes} -> signalExternalWorkflowExecutionDecisionAttributes) (\s@Decision' {} a -> s {signalExternalWorkflowExecutionDecisionAttributes = a} :: Decision)

-- | Provides the details of the @StartChildWorkflowExecution@ decision. It
-- isn\'t set for other decision types.
decision_startChildWorkflowExecutionDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe StartChildWorkflowExecutionDecisionAttributes)
decision_startChildWorkflowExecutionDecisionAttributes = Lens.lens (\Decision' {startChildWorkflowExecutionDecisionAttributes} -> startChildWorkflowExecutionDecisionAttributes) (\s@Decision' {} a -> s {startChildWorkflowExecutionDecisionAttributes = a} :: Decision)

-- | Provides the details of the @StartTimer@ decision. It isn\'t set for
-- other decision types.
decision_startTimerDecisionAttributes :: Lens.Lens' Decision (Prelude.Maybe StartTimerDecisionAttributes)
decision_startTimerDecisionAttributes = Lens.lens (\Decision' {startTimerDecisionAttributes} -> startTimerDecisionAttributes) (\s@Decision' {} a -> s {startTimerDecisionAttributes = a} :: Decision)

-- | Specifies the type of the decision.
decision_decisionType :: Lens.Lens' Decision DecisionType
decision_decisionType = Lens.lens (\Decision' {decisionType} -> decisionType) (\s@Decision' {} a -> s {decisionType = a} :: Decision)

instance Prelude.Hashable Decision where
  hashWithSalt _salt Decision' {..} =
    _salt
      `Prelude.hashWithSalt` cancelTimerDecisionAttributes
      `Prelude.hashWithSalt` cancelWorkflowExecutionDecisionAttributes
      `Prelude.hashWithSalt` completeWorkflowExecutionDecisionAttributes
      `Prelude.hashWithSalt` continueAsNewWorkflowExecutionDecisionAttributes
      `Prelude.hashWithSalt` failWorkflowExecutionDecisionAttributes
      `Prelude.hashWithSalt` recordMarkerDecisionAttributes
      `Prelude.hashWithSalt` requestCancelActivityTaskDecisionAttributes
      `Prelude.hashWithSalt` requestCancelExternalWorkflowExecutionDecisionAttributes
      `Prelude.hashWithSalt` scheduleActivityTaskDecisionAttributes
      `Prelude.hashWithSalt` scheduleLambdaFunctionDecisionAttributes
      `Prelude.hashWithSalt` signalExternalWorkflowExecutionDecisionAttributes
      `Prelude.hashWithSalt` startChildWorkflowExecutionDecisionAttributes
      `Prelude.hashWithSalt` startTimerDecisionAttributes
      `Prelude.hashWithSalt` decisionType

instance Prelude.NFData Decision where
  rnf Decision' {..} =
    Prelude.rnf cancelTimerDecisionAttributes
      `Prelude.seq` Prelude.rnf cancelWorkflowExecutionDecisionAttributes
      `Prelude.seq` Prelude.rnf
        completeWorkflowExecutionDecisionAttributes
      `Prelude.seq` Prelude.rnf
        continueAsNewWorkflowExecutionDecisionAttributes
      `Prelude.seq` Prelude.rnf failWorkflowExecutionDecisionAttributes
      `Prelude.seq` Prelude.rnf recordMarkerDecisionAttributes
      `Prelude.seq` Prelude.rnf
        requestCancelActivityTaskDecisionAttributes
      `Prelude.seq` Prelude.rnf
        requestCancelExternalWorkflowExecutionDecisionAttributes
      `Prelude.seq` Prelude.rnf scheduleActivityTaskDecisionAttributes
      `Prelude.seq` Prelude.rnf
        scheduleLambdaFunctionDecisionAttributes
      `Prelude.seq` Prelude.rnf
        signalExternalWorkflowExecutionDecisionAttributes
      `Prelude.seq` Prelude.rnf
        startChildWorkflowExecutionDecisionAttributes
      `Prelude.seq` Prelude.rnf startTimerDecisionAttributes
      `Prelude.seq` Prelude.rnf decisionType

instance Data.ToJSON Decision where
  toJSON Decision' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cancelTimerDecisionAttributes" Data..=)
              Prelude.<$> cancelTimerDecisionAttributes,
            ("cancelWorkflowExecutionDecisionAttributes" Data..=)
              Prelude.<$> cancelWorkflowExecutionDecisionAttributes,
            ( "completeWorkflowExecutionDecisionAttributes"
                Data..=
            )
              Prelude.<$> completeWorkflowExecutionDecisionAttributes,
            ( "continueAsNewWorkflowExecutionDecisionAttributes"
                Data..=
            )
              Prelude.<$> continueAsNewWorkflowExecutionDecisionAttributes,
            ("failWorkflowExecutionDecisionAttributes" Data..=)
              Prelude.<$> failWorkflowExecutionDecisionAttributes,
            ("recordMarkerDecisionAttributes" Data..=)
              Prelude.<$> recordMarkerDecisionAttributes,
            ( "requestCancelActivityTaskDecisionAttributes"
                Data..=
            )
              Prelude.<$> requestCancelActivityTaskDecisionAttributes,
            ( "requestCancelExternalWorkflowExecutionDecisionAttributes"
                Data..=
            )
              Prelude.<$> requestCancelExternalWorkflowExecutionDecisionAttributes,
            ("scheduleActivityTaskDecisionAttributes" Data..=)
              Prelude.<$> scheduleActivityTaskDecisionAttributes,
            ("scheduleLambdaFunctionDecisionAttributes" Data..=)
              Prelude.<$> scheduleLambdaFunctionDecisionAttributes,
            ( "signalExternalWorkflowExecutionDecisionAttributes"
                Data..=
            )
              Prelude.<$> signalExternalWorkflowExecutionDecisionAttributes,
            ( "startChildWorkflowExecutionDecisionAttributes"
                Data..=
            )
              Prelude.<$> startChildWorkflowExecutionDecisionAttributes,
            ("startTimerDecisionAttributes" Data..=)
              Prelude.<$> startTimerDecisionAttributes,
            Prelude.Just ("decisionType" Data..= decisionType)
          ]
      )

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
-- Module      : Network.AWS.SSM.Types.StepExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.StepExecution where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.FailureDetails
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Detailed information about an the execution state of an Automation step.
--
-- /See:/ 'newStepExecution' smart constructor.
data StepExecution = StepExecution'
  { -- | Returned values from the execution of the step.
    outputs :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The action to take if the step fails. The default value is Abort.
    onFailure :: Core.Maybe Core.Text,
    -- | A message associated with the response code for an execution.
    response :: Core.Maybe Core.Text,
    -- | If a step failed, this message explains why the execution failed.
    failureMessage :: Core.Maybe Core.Text,
    -- | If a step has finished execution, this contains the time the execution
    -- ended. If the step has not yet concluded, this field is not populated.
    executionEndTime :: Core.Maybe Core.POSIX,
    -- | The next step after the step succeeds.
    nextStep :: Core.Maybe Core.Text,
    -- | The flag which can be used to end automation no matter whether the step
    -- succeeds or fails.
    isEnd :: Core.Maybe Core.Bool,
    -- | The maximum number of tries to run the action of the step. The default
    -- value is 1.
    maxAttempts :: Core.Maybe Core.Int,
    -- | Information about the Automation failure.
    failureDetails :: Core.Maybe FailureDetails,
    -- | The targets for the step execution.
    targets :: Core.Maybe [Target],
    -- | If a step has begun execution, this contains the time the step started.
    -- If the step is in Pending status, this field is not populated.
    executionStartTime :: Core.Maybe Core.POSIX,
    -- | The combination of AWS Regions and accounts targeted by the current
    -- Automation execution.
    targetLocation :: Core.Maybe TargetLocation,
    -- | A user-specified list of parameters to override when running a step.
    overriddenParameters :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The flag which can be used to help decide whether the failure of current
    -- step leads to the Automation failure.
    isCritical :: Core.Maybe Core.Bool,
    -- | The response code returned by the execution of the step.
    responseCode :: Core.Maybe Core.Text,
    -- | The execution status for this step.
    stepStatus :: Core.Maybe AutomationExecutionStatus,
    -- | The action this step performs. The action determines the behavior of the
    -- step.
    action :: Core.Maybe Core.Text,
    -- | Strategies used when step fails, we support Continue and Abort. Abort
    -- will fail the automation when the step fails. Continue will ignore the
    -- failure of current step and allow automation to run the next step. With
    -- conditional branching, we add step:stepName to support the automation to
    -- go to another specific step.
    validNextSteps :: Core.Maybe [Core.Text],
    -- | The timeout seconds of the step.
    timeoutSeconds :: Core.Maybe Core.Integer,
    -- | Fully-resolved values passed into the step before execution.
    inputs :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The unique ID of a step execution.
    stepExecutionId :: Core.Maybe Core.Text,
    -- | The name of this execution step.
    stepName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StepExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputs', 'stepExecution_outputs' - Returned values from the execution of the step.
--
-- 'onFailure', 'stepExecution_onFailure' - The action to take if the step fails. The default value is Abort.
--
-- 'response', 'stepExecution_response' - A message associated with the response code for an execution.
--
-- 'failureMessage', 'stepExecution_failureMessage' - If a step failed, this message explains why the execution failed.
--
-- 'executionEndTime', 'stepExecution_executionEndTime' - If a step has finished execution, this contains the time the execution
-- ended. If the step has not yet concluded, this field is not populated.
--
-- 'nextStep', 'stepExecution_nextStep' - The next step after the step succeeds.
--
-- 'isEnd', 'stepExecution_isEnd' - The flag which can be used to end automation no matter whether the step
-- succeeds or fails.
--
-- 'maxAttempts', 'stepExecution_maxAttempts' - The maximum number of tries to run the action of the step. The default
-- value is 1.
--
-- 'failureDetails', 'stepExecution_failureDetails' - Information about the Automation failure.
--
-- 'targets', 'stepExecution_targets' - The targets for the step execution.
--
-- 'executionStartTime', 'stepExecution_executionStartTime' - If a step has begun execution, this contains the time the step started.
-- If the step is in Pending status, this field is not populated.
--
-- 'targetLocation', 'stepExecution_targetLocation' - The combination of AWS Regions and accounts targeted by the current
-- Automation execution.
--
-- 'overriddenParameters', 'stepExecution_overriddenParameters' - A user-specified list of parameters to override when running a step.
--
-- 'isCritical', 'stepExecution_isCritical' - The flag which can be used to help decide whether the failure of current
-- step leads to the Automation failure.
--
-- 'responseCode', 'stepExecution_responseCode' - The response code returned by the execution of the step.
--
-- 'stepStatus', 'stepExecution_stepStatus' - The execution status for this step.
--
-- 'action', 'stepExecution_action' - The action this step performs. The action determines the behavior of the
-- step.
--
-- 'validNextSteps', 'stepExecution_validNextSteps' - Strategies used when step fails, we support Continue and Abort. Abort
-- will fail the automation when the step fails. Continue will ignore the
-- failure of current step and allow automation to run the next step. With
-- conditional branching, we add step:stepName to support the automation to
-- go to another specific step.
--
-- 'timeoutSeconds', 'stepExecution_timeoutSeconds' - The timeout seconds of the step.
--
-- 'inputs', 'stepExecution_inputs' - Fully-resolved values passed into the step before execution.
--
-- 'stepExecutionId', 'stepExecution_stepExecutionId' - The unique ID of a step execution.
--
-- 'stepName', 'stepExecution_stepName' - The name of this execution step.
newStepExecution ::
  StepExecution
newStepExecution =
  StepExecution'
    { outputs = Core.Nothing,
      onFailure = Core.Nothing,
      response = Core.Nothing,
      failureMessage = Core.Nothing,
      executionEndTime = Core.Nothing,
      nextStep = Core.Nothing,
      isEnd = Core.Nothing,
      maxAttempts = Core.Nothing,
      failureDetails = Core.Nothing,
      targets = Core.Nothing,
      executionStartTime = Core.Nothing,
      targetLocation = Core.Nothing,
      overriddenParameters = Core.Nothing,
      isCritical = Core.Nothing,
      responseCode = Core.Nothing,
      stepStatus = Core.Nothing,
      action = Core.Nothing,
      validNextSteps = Core.Nothing,
      timeoutSeconds = Core.Nothing,
      inputs = Core.Nothing,
      stepExecutionId = Core.Nothing,
      stepName = Core.Nothing
    }

-- | Returned values from the execution of the step.
stepExecution_outputs :: Lens.Lens' StepExecution (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
stepExecution_outputs = Lens.lens (\StepExecution' {outputs} -> outputs) (\s@StepExecution' {} a -> s {outputs = a} :: StepExecution) Core.. Lens.mapping Lens._Coerce

-- | The action to take if the step fails. The default value is Abort.
stepExecution_onFailure :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
stepExecution_onFailure = Lens.lens (\StepExecution' {onFailure} -> onFailure) (\s@StepExecution' {} a -> s {onFailure = a} :: StepExecution)

-- | A message associated with the response code for an execution.
stepExecution_response :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
stepExecution_response = Lens.lens (\StepExecution' {response} -> response) (\s@StepExecution' {} a -> s {response = a} :: StepExecution)

-- | If a step failed, this message explains why the execution failed.
stepExecution_failureMessage :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
stepExecution_failureMessage = Lens.lens (\StepExecution' {failureMessage} -> failureMessage) (\s@StepExecution' {} a -> s {failureMessage = a} :: StepExecution)

-- | If a step has finished execution, this contains the time the execution
-- ended. If the step has not yet concluded, this field is not populated.
stepExecution_executionEndTime :: Lens.Lens' StepExecution (Core.Maybe Core.UTCTime)
stepExecution_executionEndTime = Lens.lens (\StepExecution' {executionEndTime} -> executionEndTime) (\s@StepExecution' {} a -> s {executionEndTime = a} :: StepExecution) Core.. Lens.mapping Core._Time

-- | The next step after the step succeeds.
stepExecution_nextStep :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
stepExecution_nextStep = Lens.lens (\StepExecution' {nextStep} -> nextStep) (\s@StepExecution' {} a -> s {nextStep = a} :: StepExecution)

-- | The flag which can be used to end automation no matter whether the step
-- succeeds or fails.
stepExecution_isEnd :: Lens.Lens' StepExecution (Core.Maybe Core.Bool)
stepExecution_isEnd = Lens.lens (\StepExecution' {isEnd} -> isEnd) (\s@StepExecution' {} a -> s {isEnd = a} :: StepExecution)

-- | The maximum number of tries to run the action of the step. The default
-- value is 1.
stepExecution_maxAttempts :: Lens.Lens' StepExecution (Core.Maybe Core.Int)
stepExecution_maxAttempts = Lens.lens (\StepExecution' {maxAttempts} -> maxAttempts) (\s@StepExecution' {} a -> s {maxAttempts = a} :: StepExecution)

-- | Information about the Automation failure.
stepExecution_failureDetails :: Lens.Lens' StepExecution (Core.Maybe FailureDetails)
stepExecution_failureDetails = Lens.lens (\StepExecution' {failureDetails} -> failureDetails) (\s@StepExecution' {} a -> s {failureDetails = a} :: StepExecution)

-- | The targets for the step execution.
stepExecution_targets :: Lens.Lens' StepExecution (Core.Maybe [Target])
stepExecution_targets = Lens.lens (\StepExecution' {targets} -> targets) (\s@StepExecution' {} a -> s {targets = a} :: StepExecution) Core.. Lens.mapping Lens._Coerce

-- | If a step has begun execution, this contains the time the step started.
-- If the step is in Pending status, this field is not populated.
stepExecution_executionStartTime :: Lens.Lens' StepExecution (Core.Maybe Core.UTCTime)
stepExecution_executionStartTime = Lens.lens (\StepExecution' {executionStartTime} -> executionStartTime) (\s@StepExecution' {} a -> s {executionStartTime = a} :: StepExecution) Core.. Lens.mapping Core._Time

-- | The combination of AWS Regions and accounts targeted by the current
-- Automation execution.
stepExecution_targetLocation :: Lens.Lens' StepExecution (Core.Maybe TargetLocation)
stepExecution_targetLocation = Lens.lens (\StepExecution' {targetLocation} -> targetLocation) (\s@StepExecution' {} a -> s {targetLocation = a} :: StepExecution)

-- | A user-specified list of parameters to override when running a step.
stepExecution_overriddenParameters :: Lens.Lens' StepExecution (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
stepExecution_overriddenParameters = Lens.lens (\StepExecution' {overriddenParameters} -> overriddenParameters) (\s@StepExecution' {} a -> s {overriddenParameters = a} :: StepExecution) Core.. Lens.mapping Lens._Coerce

-- | The flag which can be used to help decide whether the failure of current
-- step leads to the Automation failure.
stepExecution_isCritical :: Lens.Lens' StepExecution (Core.Maybe Core.Bool)
stepExecution_isCritical = Lens.lens (\StepExecution' {isCritical} -> isCritical) (\s@StepExecution' {} a -> s {isCritical = a} :: StepExecution)

-- | The response code returned by the execution of the step.
stepExecution_responseCode :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
stepExecution_responseCode = Lens.lens (\StepExecution' {responseCode} -> responseCode) (\s@StepExecution' {} a -> s {responseCode = a} :: StepExecution)

-- | The execution status for this step.
stepExecution_stepStatus :: Lens.Lens' StepExecution (Core.Maybe AutomationExecutionStatus)
stepExecution_stepStatus = Lens.lens (\StepExecution' {stepStatus} -> stepStatus) (\s@StepExecution' {} a -> s {stepStatus = a} :: StepExecution)

-- | The action this step performs. The action determines the behavior of the
-- step.
stepExecution_action :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
stepExecution_action = Lens.lens (\StepExecution' {action} -> action) (\s@StepExecution' {} a -> s {action = a} :: StepExecution)

-- | Strategies used when step fails, we support Continue and Abort. Abort
-- will fail the automation when the step fails. Continue will ignore the
-- failure of current step and allow automation to run the next step. With
-- conditional branching, we add step:stepName to support the automation to
-- go to another specific step.
stepExecution_validNextSteps :: Lens.Lens' StepExecution (Core.Maybe [Core.Text])
stepExecution_validNextSteps = Lens.lens (\StepExecution' {validNextSteps} -> validNextSteps) (\s@StepExecution' {} a -> s {validNextSteps = a} :: StepExecution) Core.. Lens.mapping Lens._Coerce

-- | The timeout seconds of the step.
stepExecution_timeoutSeconds :: Lens.Lens' StepExecution (Core.Maybe Core.Integer)
stepExecution_timeoutSeconds = Lens.lens (\StepExecution' {timeoutSeconds} -> timeoutSeconds) (\s@StepExecution' {} a -> s {timeoutSeconds = a} :: StepExecution)

-- | Fully-resolved values passed into the step before execution.
stepExecution_inputs :: Lens.Lens' StepExecution (Core.Maybe (Core.HashMap Core.Text Core.Text))
stepExecution_inputs = Lens.lens (\StepExecution' {inputs} -> inputs) (\s@StepExecution' {} a -> s {inputs = a} :: StepExecution) Core.. Lens.mapping Lens._Coerce

-- | The unique ID of a step execution.
stepExecution_stepExecutionId :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
stepExecution_stepExecutionId = Lens.lens (\StepExecution' {stepExecutionId} -> stepExecutionId) (\s@StepExecution' {} a -> s {stepExecutionId = a} :: StepExecution)

-- | The name of this execution step.
stepExecution_stepName :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
stepExecution_stepName = Lens.lens (\StepExecution' {stepName} -> stepName) (\s@StepExecution' {} a -> s {stepName = a} :: StepExecution)

instance Core.FromJSON StepExecution where
  parseJSON =
    Core.withObject
      "StepExecution"
      ( \x ->
          StepExecution'
            Core.<$> (x Core..:? "Outputs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "OnFailure")
            Core.<*> (x Core..:? "Response")
            Core.<*> (x Core..:? "FailureMessage")
            Core.<*> (x Core..:? "ExecutionEndTime")
            Core.<*> (x Core..:? "NextStep")
            Core.<*> (x Core..:? "IsEnd")
            Core.<*> (x Core..:? "MaxAttempts")
            Core.<*> (x Core..:? "FailureDetails")
            Core.<*> (x Core..:? "Targets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ExecutionStartTime")
            Core.<*> (x Core..:? "TargetLocation")
            Core.<*> ( x Core..:? "OverriddenParameters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "IsCritical")
            Core.<*> (x Core..:? "ResponseCode")
            Core.<*> (x Core..:? "StepStatus")
            Core.<*> (x Core..:? "Action")
            Core.<*> (x Core..:? "ValidNextSteps" Core..!= Core.mempty)
            Core.<*> (x Core..:? "TimeoutSeconds")
            Core.<*> (x Core..:? "Inputs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "StepExecutionId")
            Core.<*> (x Core..:? "StepName")
      )

instance Core.Hashable StepExecution

instance Core.NFData StepExecution

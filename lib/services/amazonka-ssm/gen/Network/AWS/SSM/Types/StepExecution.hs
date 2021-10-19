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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.FailureDetails
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Detailed information about an the execution state of an Automation step.
--
-- /See:/ 'newStepExecution' smart constructor.
data StepExecution = StepExecution'
  { -- | Information about the Automation failure.
    failureDetails :: Prelude.Maybe FailureDetails,
    -- | The flag which can be used to end automation no matter whether the step
    -- succeeds or fails.
    isEnd :: Prelude.Maybe Prelude.Bool,
    -- | Fully-resolved values passed into the step before execution.
    inputs :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of this execution step.
    stepName :: Prelude.Maybe Prelude.Text,
    -- | If a step has finished execution, this contains the time the execution
    -- ended. If the step hasn\'t yet concluded, this field isn\'t populated.
    executionEndTime :: Prelude.Maybe Core.POSIX,
    -- | If a step failed, this message explains why the execution failed.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | A message associated with the response code for an execution.
    response :: Prelude.Maybe Prelude.Text,
    -- | The action this step performs. The action determines the behavior of the
    -- step.
    action :: Prelude.Maybe Prelude.Text,
    -- | The response code returned by the execution of the step.
    responseCode :: Prelude.Maybe Prelude.Text,
    -- | The execution status for this step.
    stepStatus :: Prelude.Maybe AutomationExecutionStatus,
    -- | The combination of Amazon Web Services Regions and Amazon Web Services
    -- accounts targeted by the current Automation execution.
    targetLocation :: Prelude.Maybe TargetLocation,
    -- | A user-specified list of parameters to override when running a step.
    overriddenParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | Returned values from the execution of the step.
    outputs :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | If a step has begun execution, this contains the time the step started.
    -- If the step is in Pending status, this field isn\'t populated.
    executionStartTime :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of tries to run the action of the step. The default
    -- value is @1@.
    maxAttempts :: Prelude.Maybe Prelude.Int,
    -- | The targets for the step execution.
    targets :: Prelude.Maybe [Target],
    -- | The next step after the step succeeds.
    nextStep :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of a step execution.
    stepExecutionId :: Prelude.Maybe Prelude.Text,
    -- | Strategies used when step fails, we support Continue and Abort. Abort
    -- will fail the automation when the step fails. Continue will ignore the
    -- failure of current step and allow automation to run the next step. With
    -- conditional branching, we add step:stepName to support the automation to
    -- go to another specific step.
    validNextSteps :: Prelude.Maybe [Prelude.Text],
    -- | The timeout seconds of the step.
    timeoutSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The action to take if the step fails. The default value is @Abort@.
    onFailure :: Prelude.Maybe Prelude.Text,
    -- | The flag which can be used to help decide whether the failure of current
    -- step leads to the Automation failure.
    isCritical :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureDetails', 'stepExecution_failureDetails' - Information about the Automation failure.
--
-- 'isEnd', 'stepExecution_isEnd' - The flag which can be used to end automation no matter whether the step
-- succeeds or fails.
--
-- 'inputs', 'stepExecution_inputs' - Fully-resolved values passed into the step before execution.
--
-- 'stepName', 'stepExecution_stepName' - The name of this execution step.
--
-- 'executionEndTime', 'stepExecution_executionEndTime' - If a step has finished execution, this contains the time the execution
-- ended. If the step hasn\'t yet concluded, this field isn\'t populated.
--
-- 'failureMessage', 'stepExecution_failureMessage' - If a step failed, this message explains why the execution failed.
--
-- 'response', 'stepExecution_response' - A message associated with the response code for an execution.
--
-- 'action', 'stepExecution_action' - The action this step performs. The action determines the behavior of the
-- step.
--
-- 'responseCode', 'stepExecution_responseCode' - The response code returned by the execution of the step.
--
-- 'stepStatus', 'stepExecution_stepStatus' - The execution status for this step.
--
-- 'targetLocation', 'stepExecution_targetLocation' - The combination of Amazon Web Services Regions and Amazon Web Services
-- accounts targeted by the current Automation execution.
--
-- 'overriddenParameters', 'stepExecution_overriddenParameters' - A user-specified list of parameters to override when running a step.
--
-- 'outputs', 'stepExecution_outputs' - Returned values from the execution of the step.
--
-- 'executionStartTime', 'stepExecution_executionStartTime' - If a step has begun execution, this contains the time the step started.
-- If the step is in Pending status, this field isn\'t populated.
--
-- 'maxAttempts', 'stepExecution_maxAttempts' - The maximum number of tries to run the action of the step. The default
-- value is @1@.
--
-- 'targets', 'stepExecution_targets' - The targets for the step execution.
--
-- 'nextStep', 'stepExecution_nextStep' - The next step after the step succeeds.
--
-- 'stepExecutionId', 'stepExecution_stepExecutionId' - The unique ID of a step execution.
--
-- 'validNextSteps', 'stepExecution_validNextSteps' - Strategies used when step fails, we support Continue and Abort. Abort
-- will fail the automation when the step fails. Continue will ignore the
-- failure of current step and allow automation to run the next step. With
-- conditional branching, we add step:stepName to support the automation to
-- go to another specific step.
--
-- 'timeoutSeconds', 'stepExecution_timeoutSeconds' - The timeout seconds of the step.
--
-- 'onFailure', 'stepExecution_onFailure' - The action to take if the step fails. The default value is @Abort@.
--
-- 'isCritical', 'stepExecution_isCritical' - The flag which can be used to help decide whether the failure of current
-- step leads to the Automation failure.
newStepExecution ::
  StepExecution
newStepExecution =
  StepExecution'
    { failureDetails = Prelude.Nothing,
      isEnd = Prelude.Nothing,
      inputs = Prelude.Nothing,
      stepName = Prelude.Nothing,
      executionEndTime = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      response = Prelude.Nothing,
      action = Prelude.Nothing,
      responseCode = Prelude.Nothing,
      stepStatus = Prelude.Nothing,
      targetLocation = Prelude.Nothing,
      overriddenParameters = Prelude.Nothing,
      outputs = Prelude.Nothing,
      executionStartTime = Prelude.Nothing,
      maxAttempts = Prelude.Nothing,
      targets = Prelude.Nothing,
      nextStep = Prelude.Nothing,
      stepExecutionId = Prelude.Nothing,
      validNextSteps = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      onFailure = Prelude.Nothing,
      isCritical = Prelude.Nothing
    }

-- | Information about the Automation failure.
stepExecution_failureDetails :: Lens.Lens' StepExecution (Prelude.Maybe FailureDetails)
stepExecution_failureDetails = Lens.lens (\StepExecution' {failureDetails} -> failureDetails) (\s@StepExecution' {} a -> s {failureDetails = a} :: StepExecution)

-- | The flag which can be used to end automation no matter whether the step
-- succeeds or fails.
stepExecution_isEnd :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Bool)
stepExecution_isEnd = Lens.lens (\StepExecution' {isEnd} -> isEnd) (\s@StepExecution' {} a -> s {isEnd = a} :: StepExecution)

-- | Fully-resolved values passed into the step before execution.
stepExecution_inputs :: Lens.Lens' StepExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stepExecution_inputs = Lens.lens (\StepExecution' {inputs} -> inputs) (\s@StepExecution' {} a -> s {inputs = a} :: StepExecution) Prelude.. Lens.mapping Lens.coerced

-- | The name of this execution step.
stepExecution_stepName :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Text)
stepExecution_stepName = Lens.lens (\StepExecution' {stepName} -> stepName) (\s@StepExecution' {} a -> s {stepName = a} :: StepExecution)

-- | If a step has finished execution, this contains the time the execution
-- ended. If the step hasn\'t yet concluded, this field isn\'t populated.
stepExecution_executionEndTime :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.UTCTime)
stepExecution_executionEndTime = Lens.lens (\StepExecution' {executionEndTime} -> executionEndTime) (\s@StepExecution' {} a -> s {executionEndTime = a} :: StepExecution) Prelude.. Lens.mapping Core._Time

-- | If a step failed, this message explains why the execution failed.
stepExecution_failureMessage :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Text)
stepExecution_failureMessage = Lens.lens (\StepExecution' {failureMessage} -> failureMessage) (\s@StepExecution' {} a -> s {failureMessage = a} :: StepExecution)

-- | A message associated with the response code for an execution.
stepExecution_response :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Text)
stepExecution_response = Lens.lens (\StepExecution' {response} -> response) (\s@StepExecution' {} a -> s {response = a} :: StepExecution)

-- | The action this step performs. The action determines the behavior of the
-- step.
stepExecution_action :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Text)
stepExecution_action = Lens.lens (\StepExecution' {action} -> action) (\s@StepExecution' {} a -> s {action = a} :: StepExecution)

-- | The response code returned by the execution of the step.
stepExecution_responseCode :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Text)
stepExecution_responseCode = Lens.lens (\StepExecution' {responseCode} -> responseCode) (\s@StepExecution' {} a -> s {responseCode = a} :: StepExecution)

-- | The execution status for this step.
stepExecution_stepStatus :: Lens.Lens' StepExecution (Prelude.Maybe AutomationExecutionStatus)
stepExecution_stepStatus = Lens.lens (\StepExecution' {stepStatus} -> stepStatus) (\s@StepExecution' {} a -> s {stepStatus = a} :: StepExecution)

-- | The combination of Amazon Web Services Regions and Amazon Web Services
-- accounts targeted by the current Automation execution.
stepExecution_targetLocation :: Lens.Lens' StepExecution (Prelude.Maybe TargetLocation)
stepExecution_targetLocation = Lens.lens (\StepExecution' {targetLocation} -> targetLocation) (\s@StepExecution' {} a -> s {targetLocation = a} :: StepExecution)

-- | A user-specified list of parameters to override when running a step.
stepExecution_overriddenParameters :: Lens.Lens' StepExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
stepExecution_overriddenParameters = Lens.lens (\StepExecution' {overriddenParameters} -> overriddenParameters) (\s@StepExecution' {} a -> s {overriddenParameters = a} :: StepExecution) Prelude.. Lens.mapping Lens.coerced

-- | Returned values from the execution of the step.
stepExecution_outputs :: Lens.Lens' StepExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
stepExecution_outputs = Lens.lens (\StepExecution' {outputs} -> outputs) (\s@StepExecution' {} a -> s {outputs = a} :: StepExecution) Prelude.. Lens.mapping Lens.coerced

-- | If a step has begun execution, this contains the time the step started.
-- If the step is in Pending status, this field isn\'t populated.
stepExecution_executionStartTime :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.UTCTime)
stepExecution_executionStartTime = Lens.lens (\StepExecution' {executionStartTime} -> executionStartTime) (\s@StepExecution' {} a -> s {executionStartTime = a} :: StepExecution) Prelude.. Lens.mapping Core._Time

-- | The maximum number of tries to run the action of the step. The default
-- value is @1@.
stepExecution_maxAttempts :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Int)
stepExecution_maxAttempts = Lens.lens (\StepExecution' {maxAttempts} -> maxAttempts) (\s@StepExecution' {} a -> s {maxAttempts = a} :: StepExecution)

-- | The targets for the step execution.
stepExecution_targets :: Lens.Lens' StepExecution (Prelude.Maybe [Target])
stepExecution_targets = Lens.lens (\StepExecution' {targets} -> targets) (\s@StepExecution' {} a -> s {targets = a} :: StepExecution) Prelude.. Lens.mapping Lens.coerced

-- | The next step after the step succeeds.
stepExecution_nextStep :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Text)
stepExecution_nextStep = Lens.lens (\StepExecution' {nextStep} -> nextStep) (\s@StepExecution' {} a -> s {nextStep = a} :: StepExecution)

-- | The unique ID of a step execution.
stepExecution_stepExecutionId :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Text)
stepExecution_stepExecutionId = Lens.lens (\StepExecution' {stepExecutionId} -> stepExecutionId) (\s@StepExecution' {} a -> s {stepExecutionId = a} :: StepExecution)

-- | Strategies used when step fails, we support Continue and Abort. Abort
-- will fail the automation when the step fails. Continue will ignore the
-- failure of current step and allow automation to run the next step. With
-- conditional branching, we add step:stepName to support the automation to
-- go to another specific step.
stepExecution_validNextSteps :: Lens.Lens' StepExecution (Prelude.Maybe [Prelude.Text])
stepExecution_validNextSteps = Lens.lens (\StepExecution' {validNextSteps} -> validNextSteps) (\s@StepExecution' {} a -> s {validNextSteps = a} :: StepExecution) Prelude.. Lens.mapping Lens.coerced

-- | The timeout seconds of the step.
stepExecution_timeoutSeconds :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Integer)
stepExecution_timeoutSeconds = Lens.lens (\StepExecution' {timeoutSeconds} -> timeoutSeconds) (\s@StepExecution' {} a -> s {timeoutSeconds = a} :: StepExecution)

-- | The action to take if the step fails. The default value is @Abort@.
stepExecution_onFailure :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Text)
stepExecution_onFailure = Lens.lens (\StepExecution' {onFailure} -> onFailure) (\s@StepExecution' {} a -> s {onFailure = a} :: StepExecution)

-- | The flag which can be used to help decide whether the failure of current
-- step leads to the Automation failure.
stepExecution_isCritical :: Lens.Lens' StepExecution (Prelude.Maybe Prelude.Bool)
stepExecution_isCritical = Lens.lens (\StepExecution' {isCritical} -> isCritical) (\s@StepExecution' {} a -> s {isCritical = a} :: StepExecution)

instance Core.FromJSON StepExecution where
  parseJSON =
    Core.withObject
      "StepExecution"
      ( \x ->
          StepExecution'
            Prelude.<$> (x Core..:? "FailureDetails")
            Prelude.<*> (x Core..:? "IsEnd")
            Prelude.<*> (x Core..:? "Inputs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "StepName")
            Prelude.<*> (x Core..:? "ExecutionEndTime")
            Prelude.<*> (x Core..:? "FailureMessage")
            Prelude.<*> (x Core..:? "Response")
            Prelude.<*> (x Core..:? "Action")
            Prelude.<*> (x Core..:? "ResponseCode")
            Prelude.<*> (x Core..:? "StepStatus")
            Prelude.<*> (x Core..:? "TargetLocation")
            Prelude.<*> ( x Core..:? "OverriddenParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Outputs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ExecutionStartTime")
            Prelude.<*> (x Core..:? "MaxAttempts")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "NextStep")
            Prelude.<*> (x Core..:? "StepExecutionId")
            Prelude.<*> (x Core..:? "ValidNextSteps" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TimeoutSeconds")
            Prelude.<*> (x Core..:? "OnFailure")
            Prelude.<*> (x Core..:? "IsCritical")
      )

instance Prelude.Hashable StepExecution

instance Prelude.NFData StepExecution

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StepExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.StepExecution
  ( StepExecution (..),

    -- * Smart constructor
    mkStepExecution,

    -- * Lenses
    seFailureDetails,
    seIsEnd,
    seInputs,
    seStepName,
    seExecutionEndTime,
    seFailureMessage,
    seResponse,
    seAction,
    seResponseCode,
    seStepStatus,
    seTargetLocation,
    seOverriddenParameters,
    seOutputs,
    seExecutionStartTime,
    seMaxAttempts,
    seTargets,
    seNextStep,
    seStepExecutionId,
    seValidNextSteps,
    seTimeoutSeconds,
    seOnFailure,
    seIsCritical,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.FailureDetails
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Detailed information about an the execution state of an Automation step.
--
-- /See:/ 'mkStepExecution' smart constructor.
data StepExecution = StepExecution'
  { -- | Information about the Automation failure.
    failureDetails :: Lude.Maybe FailureDetails,
    -- | The flag which can be used to end automation no matter whether the step succeeds or fails.
    isEnd :: Lude.Maybe Lude.Bool,
    -- | Fully-resolved values passed into the step before execution.
    inputs :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The name of this execution step.
    stepName :: Lude.Maybe Lude.Text,
    -- | If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
    executionEndTime :: Lude.Maybe Lude.Timestamp,
    -- | If a step failed, this message explains why the execution failed.
    failureMessage :: Lude.Maybe Lude.Text,
    -- | A message associated with the response code for an execution.
    response :: Lude.Maybe Lude.Text,
    -- | The action this step performs. The action determines the behavior of the step.
    action :: Lude.Maybe Lude.Text,
    -- | The response code returned by the execution of the step.
    responseCode :: Lude.Maybe Lude.Text,
    -- | The execution status for this step.
    stepStatus :: Lude.Maybe AutomationExecutionStatus,
    -- | The combination of AWS Regions and accounts targeted by the current Automation execution.
    targetLocation :: Lude.Maybe TargetLocation,
    -- | A user-specified list of parameters to override when running a step.
    overriddenParameters :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | Returned values from the execution of the step.
    outputs :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
    executionStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The maximum number of tries to run the action of the step. The default value is 1.
    maxAttempts :: Lude.Maybe Lude.Int,
    -- | The targets for the step execution.
    targets :: Lude.Maybe [Target],
    -- | The next step after the step succeeds.
    nextStep :: Lude.Maybe Lude.Text,
    -- | The unique ID of a step execution.
    stepExecutionId :: Lude.Maybe Lude.Text,
    -- | Strategies used when step fails, we support Continue and Abort. Abort will fail the automation when the step fails. Continue will ignore the failure of current step and allow automation to run the next step. With conditional branching, we add step:stepName to support the automation to go to another specific step.
    validNextSteps :: Lude.Maybe [Lude.Text],
    -- | The timeout seconds of the step.
    timeoutSeconds :: Lude.Maybe Lude.Integer,
    -- | The action to take if the step fails. The default value is Abort.
    onFailure :: Lude.Maybe Lude.Text,
    -- | The flag which can be used to help decide whether the failure of current step leads to the Automation failure.
    isCritical :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StepExecution' with the minimum fields required to make a request.
--
-- * 'failureDetails' - Information about the Automation failure.
-- * 'isEnd' - The flag which can be used to end automation no matter whether the step succeeds or fails.
-- * 'inputs' - Fully-resolved values passed into the step before execution.
-- * 'stepName' - The name of this execution step.
-- * 'executionEndTime' - If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
-- * 'failureMessage' - If a step failed, this message explains why the execution failed.
-- * 'response' - A message associated with the response code for an execution.
-- * 'action' - The action this step performs. The action determines the behavior of the step.
-- * 'responseCode' - The response code returned by the execution of the step.
-- * 'stepStatus' - The execution status for this step.
-- * 'targetLocation' - The combination of AWS Regions and accounts targeted by the current Automation execution.
-- * 'overriddenParameters' - A user-specified list of parameters to override when running a step.
-- * 'outputs' - Returned values from the execution of the step.
-- * 'executionStartTime' - If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
-- * 'maxAttempts' - The maximum number of tries to run the action of the step. The default value is 1.
-- * 'targets' - The targets for the step execution.
-- * 'nextStep' - The next step after the step succeeds.
-- * 'stepExecutionId' - The unique ID of a step execution.
-- * 'validNextSteps' - Strategies used when step fails, we support Continue and Abort. Abort will fail the automation when the step fails. Continue will ignore the failure of current step and allow automation to run the next step. With conditional branching, we add step:stepName to support the automation to go to another specific step.
-- * 'timeoutSeconds' - The timeout seconds of the step.
-- * 'onFailure' - The action to take if the step fails. The default value is Abort.
-- * 'isCritical' - The flag which can be used to help decide whether the failure of current step leads to the Automation failure.
mkStepExecution ::
  StepExecution
mkStepExecution =
  StepExecution'
    { failureDetails = Lude.Nothing,
      isEnd = Lude.Nothing,
      inputs = Lude.Nothing,
      stepName = Lude.Nothing,
      executionEndTime = Lude.Nothing,
      failureMessage = Lude.Nothing,
      response = Lude.Nothing,
      action = Lude.Nothing,
      responseCode = Lude.Nothing,
      stepStatus = Lude.Nothing,
      targetLocation = Lude.Nothing,
      overriddenParameters = Lude.Nothing,
      outputs = Lude.Nothing,
      executionStartTime = Lude.Nothing,
      maxAttempts = Lude.Nothing,
      targets = Lude.Nothing,
      nextStep = Lude.Nothing,
      stepExecutionId = Lude.Nothing,
      validNextSteps = Lude.Nothing,
      timeoutSeconds = Lude.Nothing,
      onFailure = Lude.Nothing,
      isCritical = Lude.Nothing
    }

-- | Information about the Automation failure.
--
-- /Note:/ Consider using 'failureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seFailureDetails :: Lens.Lens' StepExecution (Lude.Maybe FailureDetails)
seFailureDetails = Lens.lens (failureDetails :: StepExecution -> Lude.Maybe FailureDetails) (\s a -> s {failureDetails = a} :: StepExecution)
{-# DEPRECATED seFailureDetails "Use generic-lens or generic-optics with 'failureDetails' instead." #-}

-- | The flag which can be used to end automation no matter whether the step succeeds or fails.
--
-- /Note:/ Consider using 'isEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seIsEnd :: Lens.Lens' StepExecution (Lude.Maybe Lude.Bool)
seIsEnd = Lens.lens (isEnd :: StepExecution -> Lude.Maybe Lude.Bool) (\s a -> s {isEnd = a} :: StepExecution)
{-# DEPRECATED seIsEnd "Use generic-lens or generic-optics with 'isEnd' instead." #-}

-- | Fully-resolved values passed into the step before execution.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seInputs :: Lens.Lens' StepExecution (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
seInputs = Lens.lens (inputs :: StepExecution -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {inputs = a} :: StepExecution)
{-# DEPRECATED seInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | The name of this execution step.
--
-- /Note:/ Consider using 'stepName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStepName :: Lens.Lens' StepExecution (Lude.Maybe Lude.Text)
seStepName = Lens.lens (stepName :: StepExecution -> Lude.Maybe Lude.Text) (\s a -> s {stepName = a} :: StepExecution)
{-# DEPRECATED seStepName "Use generic-lens or generic-optics with 'stepName' instead." #-}

-- | If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
--
-- /Note:/ Consider using 'executionEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seExecutionEndTime :: Lens.Lens' StepExecution (Lude.Maybe Lude.Timestamp)
seExecutionEndTime = Lens.lens (executionEndTime :: StepExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {executionEndTime = a} :: StepExecution)
{-# DEPRECATED seExecutionEndTime "Use generic-lens or generic-optics with 'executionEndTime' instead." #-}

-- | If a step failed, this message explains why the execution failed.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seFailureMessage :: Lens.Lens' StepExecution (Lude.Maybe Lude.Text)
seFailureMessage = Lens.lens (failureMessage :: StepExecution -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: StepExecution)
{-# DEPRECATED seFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | A message associated with the response code for an execution.
--
-- /Note:/ Consider using 'response' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seResponse :: Lens.Lens' StepExecution (Lude.Maybe Lude.Text)
seResponse = Lens.lens (response :: StepExecution -> Lude.Maybe Lude.Text) (\s a -> s {response = a} :: StepExecution)
{-# DEPRECATED seResponse "Use generic-lens or generic-optics with 'response' instead." #-}

-- | The action this step performs. The action determines the behavior of the step.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seAction :: Lens.Lens' StepExecution (Lude.Maybe Lude.Text)
seAction = Lens.lens (action :: StepExecution -> Lude.Maybe Lude.Text) (\s a -> s {action = a} :: StepExecution)
{-# DEPRECATED seAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The response code returned by the execution of the step.
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seResponseCode :: Lens.Lens' StepExecution (Lude.Maybe Lude.Text)
seResponseCode = Lens.lens (responseCode :: StepExecution -> Lude.Maybe Lude.Text) (\s a -> s {responseCode = a} :: StepExecution)
{-# DEPRECATED seResponseCode "Use generic-lens or generic-optics with 'responseCode' instead." #-}

-- | The execution status for this step.
--
-- /Note:/ Consider using 'stepStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStepStatus :: Lens.Lens' StepExecution (Lude.Maybe AutomationExecutionStatus)
seStepStatus = Lens.lens (stepStatus :: StepExecution -> Lude.Maybe AutomationExecutionStatus) (\s a -> s {stepStatus = a} :: StepExecution)
{-# DEPRECATED seStepStatus "Use generic-lens or generic-optics with 'stepStatus' instead." #-}

-- | The combination of AWS Regions and accounts targeted by the current Automation execution.
--
-- /Note:/ Consider using 'targetLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTargetLocation :: Lens.Lens' StepExecution (Lude.Maybe TargetLocation)
seTargetLocation = Lens.lens (targetLocation :: StepExecution -> Lude.Maybe TargetLocation) (\s a -> s {targetLocation = a} :: StepExecution)
{-# DEPRECATED seTargetLocation "Use generic-lens or generic-optics with 'targetLocation' instead." #-}

-- | A user-specified list of parameters to override when running a step.
--
-- /Note:/ Consider using 'overriddenParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOverriddenParameters :: Lens.Lens' StepExecution (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
seOverriddenParameters = Lens.lens (overriddenParameters :: StepExecution -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {overriddenParameters = a} :: StepExecution)
{-# DEPRECATED seOverriddenParameters "Use generic-lens or generic-optics with 'overriddenParameters' instead." #-}

-- | Returned values from the execution of the step.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOutputs :: Lens.Lens' StepExecution (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
seOutputs = Lens.lens (outputs :: StepExecution -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {outputs = a} :: StepExecution)
{-# DEPRECATED seOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
--
-- /Note:/ Consider using 'executionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seExecutionStartTime :: Lens.Lens' StepExecution (Lude.Maybe Lude.Timestamp)
seExecutionStartTime = Lens.lens (executionStartTime :: StepExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {executionStartTime = a} :: StepExecution)
{-# DEPRECATED seExecutionStartTime "Use generic-lens or generic-optics with 'executionStartTime' instead." #-}

-- | The maximum number of tries to run the action of the step. The default value is 1.
--
-- /Note:/ Consider using 'maxAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMaxAttempts :: Lens.Lens' StepExecution (Lude.Maybe Lude.Int)
seMaxAttempts = Lens.lens (maxAttempts :: StepExecution -> Lude.Maybe Lude.Int) (\s a -> s {maxAttempts = a} :: StepExecution)
{-# DEPRECATED seMaxAttempts "Use generic-lens or generic-optics with 'maxAttempts' instead." #-}

-- | The targets for the step execution.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTargets :: Lens.Lens' StepExecution (Lude.Maybe [Target])
seTargets = Lens.lens (targets :: StepExecution -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: StepExecution)
{-# DEPRECATED seTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The next step after the step succeeds.
--
-- /Note:/ Consider using 'nextStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seNextStep :: Lens.Lens' StepExecution (Lude.Maybe Lude.Text)
seNextStep = Lens.lens (nextStep :: StepExecution -> Lude.Maybe Lude.Text) (\s a -> s {nextStep = a} :: StepExecution)
{-# DEPRECATED seNextStep "Use generic-lens or generic-optics with 'nextStep' instead." #-}

-- | The unique ID of a step execution.
--
-- /Note:/ Consider using 'stepExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStepExecutionId :: Lens.Lens' StepExecution (Lude.Maybe Lude.Text)
seStepExecutionId = Lens.lens (stepExecutionId :: StepExecution -> Lude.Maybe Lude.Text) (\s a -> s {stepExecutionId = a} :: StepExecution)
{-# DEPRECATED seStepExecutionId "Use generic-lens or generic-optics with 'stepExecutionId' instead." #-}

-- | Strategies used when step fails, we support Continue and Abort. Abort will fail the automation when the step fails. Continue will ignore the failure of current step and allow automation to run the next step. With conditional branching, we add step:stepName to support the automation to go to another specific step.
--
-- /Note:/ Consider using 'validNextSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seValidNextSteps :: Lens.Lens' StepExecution (Lude.Maybe [Lude.Text])
seValidNextSteps = Lens.lens (validNextSteps :: StepExecution -> Lude.Maybe [Lude.Text]) (\s a -> s {validNextSteps = a} :: StepExecution)
{-# DEPRECATED seValidNextSteps "Use generic-lens or generic-optics with 'validNextSteps' instead." #-}

-- | The timeout seconds of the step.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTimeoutSeconds :: Lens.Lens' StepExecution (Lude.Maybe Lude.Integer)
seTimeoutSeconds = Lens.lens (timeoutSeconds :: StepExecution -> Lude.Maybe Lude.Integer) (\s a -> s {timeoutSeconds = a} :: StepExecution)
{-# DEPRECATED seTimeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead." #-}

-- | The action to take if the step fails. The default value is Abort.
--
-- /Note:/ Consider using 'onFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOnFailure :: Lens.Lens' StepExecution (Lude.Maybe Lude.Text)
seOnFailure = Lens.lens (onFailure :: StepExecution -> Lude.Maybe Lude.Text) (\s a -> s {onFailure = a} :: StepExecution)
{-# DEPRECATED seOnFailure "Use generic-lens or generic-optics with 'onFailure' instead." #-}

-- | The flag which can be used to help decide whether the failure of current step leads to the Automation failure.
--
-- /Note:/ Consider using 'isCritical' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seIsCritical :: Lens.Lens' StepExecution (Lude.Maybe Lude.Bool)
seIsCritical = Lens.lens (isCritical :: StepExecution -> Lude.Maybe Lude.Bool) (\s a -> s {isCritical = a} :: StepExecution)
{-# DEPRECATED seIsCritical "Use generic-lens or generic-optics with 'isCritical' instead." #-}

instance Lude.FromJSON StepExecution where
  parseJSON =
    Lude.withObject
      "StepExecution"
      ( \x ->
          StepExecution'
            Lude.<$> (x Lude..:? "FailureDetails")
            Lude.<*> (x Lude..:? "IsEnd")
            Lude.<*> (x Lude..:? "Inputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StepName")
            Lude.<*> (x Lude..:? "ExecutionEndTime")
            Lude.<*> (x Lude..:? "FailureMessage")
            Lude.<*> (x Lude..:? "Response")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "ResponseCode")
            Lude.<*> (x Lude..:? "StepStatus")
            Lude.<*> (x Lude..:? "TargetLocation")
            Lude.<*> (x Lude..:? "OverriddenParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Outputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ExecutionStartTime")
            Lude.<*> (x Lude..:? "MaxAttempts")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NextStep")
            Lude.<*> (x Lude..:? "StepExecutionId")
            Lude.<*> (x Lude..:? "ValidNextSteps" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TimeoutSeconds")
            Lude.<*> (x Lude..:? "OnFailure")
            Lude.<*> (x Lude..:? "IsCritical")
      )

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
    seAction,
    seExecutionEndTime,
    seExecutionStartTime,
    seFailureDetails,
    seFailureMessage,
    seInputs,
    seIsCritical,
    seIsEnd,
    seMaxAttempts,
    seNextStep,
    seOnFailure,
    seOutputs,
    seOverriddenParameters,
    seResponse,
    seResponseCode,
    seStepExecutionId,
    seStepName,
    seStepStatus,
    seTargetLocation,
    seTargets,
    seTimeoutSeconds,
    seValidNextSteps,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Action as Types
import qualified Network.AWS.SSM.Types.AutomationExecutionStatus as Types
import qualified Network.AWS.SSM.Types.AutomationParameterKey as Types
import qualified Network.AWS.SSM.Types.AutomationParameterValue as Types
import qualified Network.AWS.SSM.Types.FailureDetails as Types
import qualified Network.AWS.SSM.Types.String as Types
import qualified Network.AWS.SSM.Types.Target as Types
import qualified Network.AWS.SSM.Types.TargetLocation as Types
import qualified Network.AWS.SSM.Types.ValidNextStep as Types

-- | Detailed information about an the execution state of an Automation step.
--
-- /See:/ 'mkStepExecution' smart constructor.
data StepExecution = StepExecution'
  { -- | The action this step performs. The action determines the behavior of the step.
    action :: Core.Maybe Types.Action,
    -- | If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
    executionEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
    executionStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | Information about the Automation failure.
    failureDetails :: Core.Maybe Types.FailureDetails,
    -- | If a step failed, this message explains why the execution failed.
    failureMessage :: Core.Maybe Types.String,
    -- | Fully-resolved values passed into the step before execution.
    inputs :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The flag which can be used to help decide whether the failure of current step leads to the Automation failure.
    isCritical :: Core.Maybe Core.Bool,
    -- | The flag which can be used to end automation no matter whether the step succeeds or fails.
    isEnd :: Core.Maybe Core.Bool,
    -- | The maximum number of tries to run the action of the step. The default value is 1.
    maxAttempts :: Core.Maybe Core.Int,
    -- | The next step after the step succeeds.
    nextStep :: Core.Maybe Types.String,
    -- | The action to take if the step fails. The default value is Abort.
    onFailure :: Core.Maybe Types.String,
    -- | Returned values from the execution of the step.
    outputs :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]),
    -- | A user-specified list of parameters to override when running a step.
    overriddenParameters :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]),
    -- | A message associated with the response code for an execution.
    response :: Core.Maybe Types.String,
    -- | The response code returned by the execution of the step.
    responseCode :: Core.Maybe Types.String,
    -- | The unique ID of a step execution.
    stepExecutionId :: Core.Maybe Types.String,
    -- | The name of this execution step.
    stepName :: Core.Maybe Types.String,
    -- | The execution status for this step.
    stepStatus :: Core.Maybe Types.AutomationExecutionStatus,
    -- | The combination of AWS Regions and accounts targeted by the current Automation execution.
    targetLocation :: Core.Maybe Types.TargetLocation,
    -- | The targets for the step execution.
    targets :: Core.Maybe [Types.Target],
    -- | The timeout seconds of the step.
    timeoutSeconds :: Core.Maybe Core.Integer,
    -- | Strategies used when step fails, we support Continue and Abort. Abort will fail the automation when the step fails. Continue will ignore the failure of current step and allow automation to run the next step. With conditional branching, we add step:stepName to support the automation to go to another specific step.
    validNextSteps :: Core.Maybe [Types.ValidNextStep]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StepExecution' value with any optional fields omitted.
mkStepExecution ::
  StepExecution
mkStepExecution =
  StepExecution'
    { action = Core.Nothing,
      executionEndTime = Core.Nothing,
      executionStartTime = Core.Nothing,
      failureDetails = Core.Nothing,
      failureMessage = Core.Nothing,
      inputs = Core.Nothing,
      isCritical = Core.Nothing,
      isEnd = Core.Nothing,
      maxAttempts = Core.Nothing,
      nextStep = Core.Nothing,
      onFailure = Core.Nothing,
      outputs = Core.Nothing,
      overriddenParameters = Core.Nothing,
      response = Core.Nothing,
      responseCode = Core.Nothing,
      stepExecutionId = Core.Nothing,
      stepName = Core.Nothing,
      stepStatus = Core.Nothing,
      targetLocation = Core.Nothing,
      targets = Core.Nothing,
      timeoutSeconds = Core.Nothing,
      validNextSteps = Core.Nothing
    }

-- | The action this step performs. The action determines the behavior of the step.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seAction :: Lens.Lens' StepExecution (Core.Maybe Types.Action)
seAction = Lens.field @"action"
{-# DEPRECATED seAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
--
-- /Note:/ Consider using 'executionEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seExecutionEndTime :: Lens.Lens' StepExecution (Core.Maybe Core.NominalDiffTime)
seExecutionEndTime = Lens.field @"executionEndTime"
{-# DEPRECATED seExecutionEndTime "Use generic-lens or generic-optics with 'executionEndTime' instead." #-}

-- | If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
--
-- /Note:/ Consider using 'executionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seExecutionStartTime :: Lens.Lens' StepExecution (Core.Maybe Core.NominalDiffTime)
seExecutionStartTime = Lens.field @"executionStartTime"
{-# DEPRECATED seExecutionStartTime "Use generic-lens or generic-optics with 'executionStartTime' instead." #-}

-- | Information about the Automation failure.
--
-- /Note:/ Consider using 'failureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seFailureDetails :: Lens.Lens' StepExecution (Core.Maybe Types.FailureDetails)
seFailureDetails = Lens.field @"failureDetails"
{-# DEPRECATED seFailureDetails "Use generic-lens or generic-optics with 'failureDetails' instead." #-}

-- | If a step failed, this message explains why the execution failed.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seFailureMessage :: Lens.Lens' StepExecution (Core.Maybe Types.String)
seFailureMessage = Lens.field @"failureMessage"
{-# DEPRECATED seFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | Fully-resolved values passed into the step before execution.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seInputs :: Lens.Lens' StepExecution (Core.Maybe (Core.HashMap Types.String Types.String))
seInputs = Lens.field @"inputs"
{-# DEPRECATED seInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | The flag which can be used to help decide whether the failure of current step leads to the Automation failure.
--
-- /Note:/ Consider using 'isCritical' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seIsCritical :: Lens.Lens' StepExecution (Core.Maybe Core.Bool)
seIsCritical = Lens.field @"isCritical"
{-# DEPRECATED seIsCritical "Use generic-lens or generic-optics with 'isCritical' instead." #-}

-- | The flag which can be used to end automation no matter whether the step succeeds or fails.
--
-- /Note:/ Consider using 'isEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seIsEnd :: Lens.Lens' StepExecution (Core.Maybe Core.Bool)
seIsEnd = Lens.field @"isEnd"
{-# DEPRECATED seIsEnd "Use generic-lens or generic-optics with 'isEnd' instead." #-}

-- | The maximum number of tries to run the action of the step. The default value is 1.
--
-- /Note:/ Consider using 'maxAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMaxAttempts :: Lens.Lens' StepExecution (Core.Maybe Core.Int)
seMaxAttempts = Lens.field @"maxAttempts"
{-# DEPRECATED seMaxAttempts "Use generic-lens or generic-optics with 'maxAttempts' instead." #-}

-- | The next step after the step succeeds.
--
-- /Note:/ Consider using 'nextStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seNextStep :: Lens.Lens' StepExecution (Core.Maybe Types.String)
seNextStep = Lens.field @"nextStep"
{-# DEPRECATED seNextStep "Use generic-lens or generic-optics with 'nextStep' instead." #-}

-- | The action to take if the step fails. The default value is Abort.
--
-- /Note:/ Consider using 'onFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOnFailure :: Lens.Lens' StepExecution (Core.Maybe Types.String)
seOnFailure = Lens.field @"onFailure"
{-# DEPRECATED seOnFailure "Use generic-lens or generic-optics with 'onFailure' instead." #-}

-- | Returned values from the execution of the step.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOutputs :: Lens.Lens' StepExecution (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
seOutputs = Lens.field @"outputs"
{-# DEPRECATED seOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | A user-specified list of parameters to override when running a step.
--
-- /Note:/ Consider using 'overriddenParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOverriddenParameters :: Lens.Lens' StepExecution (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
seOverriddenParameters = Lens.field @"overriddenParameters"
{-# DEPRECATED seOverriddenParameters "Use generic-lens or generic-optics with 'overriddenParameters' instead." #-}

-- | A message associated with the response code for an execution.
--
-- /Note:/ Consider using 'response' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seResponse :: Lens.Lens' StepExecution (Core.Maybe Types.String)
seResponse = Lens.field @"response"
{-# DEPRECATED seResponse "Use generic-lens or generic-optics with 'response' instead." #-}

-- | The response code returned by the execution of the step.
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seResponseCode :: Lens.Lens' StepExecution (Core.Maybe Types.String)
seResponseCode = Lens.field @"responseCode"
{-# DEPRECATED seResponseCode "Use generic-lens or generic-optics with 'responseCode' instead." #-}

-- | The unique ID of a step execution.
--
-- /Note:/ Consider using 'stepExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStepExecutionId :: Lens.Lens' StepExecution (Core.Maybe Types.String)
seStepExecutionId = Lens.field @"stepExecutionId"
{-# DEPRECATED seStepExecutionId "Use generic-lens or generic-optics with 'stepExecutionId' instead." #-}

-- | The name of this execution step.
--
-- /Note:/ Consider using 'stepName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStepName :: Lens.Lens' StepExecution (Core.Maybe Types.String)
seStepName = Lens.field @"stepName"
{-# DEPRECATED seStepName "Use generic-lens or generic-optics with 'stepName' instead." #-}

-- | The execution status for this step.
--
-- /Note:/ Consider using 'stepStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStepStatus :: Lens.Lens' StepExecution (Core.Maybe Types.AutomationExecutionStatus)
seStepStatus = Lens.field @"stepStatus"
{-# DEPRECATED seStepStatus "Use generic-lens or generic-optics with 'stepStatus' instead." #-}

-- | The combination of AWS Regions and accounts targeted by the current Automation execution.
--
-- /Note:/ Consider using 'targetLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTargetLocation :: Lens.Lens' StepExecution (Core.Maybe Types.TargetLocation)
seTargetLocation = Lens.field @"targetLocation"
{-# DEPRECATED seTargetLocation "Use generic-lens or generic-optics with 'targetLocation' instead." #-}

-- | The targets for the step execution.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTargets :: Lens.Lens' StepExecution (Core.Maybe [Types.Target])
seTargets = Lens.field @"targets"
{-# DEPRECATED seTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The timeout seconds of the step.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTimeoutSeconds :: Lens.Lens' StepExecution (Core.Maybe Core.Integer)
seTimeoutSeconds = Lens.field @"timeoutSeconds"
{-# DEPRECATED seTimeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead." #-}

-- | Strategies used when step fails, we support Continue and Abort. Abort will fail the automation when the step fails. Continue will ignore the failure of current step and allow automation to run the next step. With conditional branching, we add step:stepName to support the automation to go to another specific step.
--
-- /Note:/ Consider using 'validNextSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seValidNextSteps :: Lens.Lens' StepExecution (Core.Maybe [Types.ValidNextStep])
seValidNextSteps = Lens.field @"validNextSteps"
{-# DEPRECATED seValidNextSteps "Use generic-lens or generic-optics with 'validNextSteps' instead." #-}

instance Core.FromJSON StepExecution where
  parseJSON =
    Core.withObject "StepExecution" Core.$
      \x ->
        StepExecution'
          Core.<$> (x Core..:? "Action")
          Core.<*> (x Core..:? "ExecutionEndTime")
          Core.<*> (x Core..:? "ExecutionStartTime")
          Core.<*> (x Core..:? "FailureDetails")
          Core.<*> (x Core..:? "FailureMessage")
          Core.<*> (x Core..:? "Inputs")
          Core.<*> (x Core..:? "IsCritical")
          Core.<*> (x Core..:? "IsEnd")
          Core.<*> (x Core..:? "MaxAttempts")
          Core.<*> (x Core..:? "NextStep")
          Core.<*> (x Core..:? "OnFailure")
          Core.<*> (x Core..:? "Outputs")
          Core.<*> (x Core..:? "OverriddenParameters")
          Core.<*> (x Core..:? "Response")
          Core.<*> (x Core..:? "ResponseCode")
          Core.<*> (x Core..:? "StepExecutionId")
          Core.<*> (x Core..:? "StepName")
          Core.<*> (x Core..:? "StepStatus")
          Core.<*> (x Core..:? "TargetLocation")
          Core.<*> (x Core..:? "Targets")
          Core.<*> (x Core..:? "TimeoutSeconds")
          Core.<*> (x Core..:? "ValidNextSteps")

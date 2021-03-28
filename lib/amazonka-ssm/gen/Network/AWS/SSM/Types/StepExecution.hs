{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StepExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.StepExecution
  ( StepExecution (..)
  -- * Smart constructor
  , mkStepExecution
  -- * Lenses
  , seAction
  , seExecutionEndTime
  , seExecutionStartTime
  , seFailureDetails
  , seFailureMessage
  , seInputs
  , seIsCritical
  , seIsEnd
  , seMaxAttempts
  , seNextStep
  , seOnFailure
  , seOutputs
  , seOverriddenParameters
  , seResponse
  , seResponseCode
  , seStepExecutionId
  , seStepName
  , seStepStatus
  , seTargetLocation
  , seTargets
  , seTimeoutSeconds
  , seValidNextSteps
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Action as Types
import qualified Network.AWS.SSM.Types.AutomationExecutionStatus as Types
import qualified Network.AWS.SSM.Types.AutomationParameterKey as Types
import qualified Network.AWS.SSM.Types.AutomationParameterValue as Types
import qualified Network.AWS.SSM.Types.FailureDetails as Types
import qualified Network.AWS.SSM.Types.Target as Types
import qualified Network.AWS.SSM.Types.TargetLocation as Types
import qualified Network.AWS.SSM.Types.ValidNextStep as Types

-- | Detailed information about an the execution state of an Automation step.
--
-- /See:/ 'mkStepExecution' smart constructor.
data StepExecution = StepExecution'
  { action :: Core.Maybe Types.Action
    -- ^ The action this step performs. The action determines the behavior of the step.
  , executionEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
  , executionStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
  , failureDetails :: Core.Maybe Types.FailureDetails
    -- ^ Information about the Automation failure.
  , failureMessage :: Core.Maybe Core.Text
    -- ^ If a step failed, this message explains why the execution failed.
  , inputs :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Fully-resolved values passed into the step before execution.
  , isCritical :: Core.Maybe Core.Bool
    -- ^ The flag which can be used to help decide whether the failure of current step leads to the Automation failure.
  , isEnd :: Core.Maybe Core.Bool
    -- ^ The flag which can be used to end automation no matter whether the step succeeds or fails.
  , maxAttempts :: Core.Maybe Core.Int
    -- ^ The maximum number of tries to run the action of the step. The default value is 1.
  , nextStep :: Core.Maybe Core.Text
    -- ^ The next step after the step succeeds.
  , onFailure :: Core.Maybe Core.Text
    -- ^ The action to take if the step fails. The default value is Abort.
  , outputs :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue])
    -- ^ Returned values from the execution of the step.
  , overriddenParameters :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue])
    -- ^ A user-specified list of parameters to override when running a step.
  , response :: Core.Maybe Core.Text
    -- ^ A message associated with the response code for an execution.
  , responseCode :: Core.Maybe Core.Text
    -- ^ The response code returned by the execution of the step.
  , stepExecutionId :: Core.Maybe Core.Text
    -- ^ The unique ID of a step execution.
  , stepName :: Core.Maybe Core.Text
    -- ^ The name of this execution step.
  , stepStatus :: Core.Maybe Types.AutomationExecutionStatus
    -- ^ The execution status for this step.
  , targetLocation :: Core.Maybe Types.TargetLocation
    -- ^ The combination of AWS Regions and accounts targeted by the current Automation execution.
  , targets :: Core.Maybe [Types.Target]
    -- ^ The targets for the step execution.
  , timeoutSeconds :: Core.Maybe Core.Integer
    -- ^ The timeout seconds of the step.
  , validNextSteps :: Core.Maybe [Types.ValidNextStep]
    -- ^ Strategies used when step fails, we support Continue and Abort. Abort will fail the automation when the step fails. Continue will ignore the failure of current step and allow automation to run the next step. With conditional branching, we add step:stepName to support the automation to go to another specific step.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StepExecution' value with any optional fields omitted.
mkStepExecution
    :: StepExecution
mkStepExecution
  = StepExecution'{action = Core.Nothing,
                   executionEndTime = Core.Nothing, executionStartTime = Core.Nothing,
                   failureDetails = Core.Nothing, failureMessage = Core.Nothing,
                   inputs = Core.Nothing, isCritical = Core.Nothing,
                   isEnd = Core.Nothing, maxAttempts = Core.Nothing,
                   nextStep = Core.Nothing, onFailure = Core.Nothing,
                   outputs = Core.Nothing, overriddenParameters = Core.Nothing,
                   response = Core.Nothing, responseCode = Core.Nothing,
                   stepExecutionId = Core.Nothing, stepName = Core.Nothing,
                   stepStatus = Core.Nothing, targetLocation = Core.Nothing,
                   targets = Core.Nothing, timeoutSeconds = Core.Nothing,
                   validNextSteps = Core.Nothing}

-- | The action this step performs. The action determines the behavior of the step.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seAction :: Lens.Lens' StepExecution (Core.Maybe Types.Action)
seAction = Lens.field @"action"
{-# INLINEABLE seAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
--
-- /Note:/ Consider using 'executionEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seExecutionEndTime :: Lens.Lens' StepExecution (Core.Maybe Core.NominalDiffTime)
seExecutionEndTime = Lens.field @"executionEndTime"
{-# INLINEABLE seExecutionEndTime #-}
{-# DEPRECATED executionEndTime "Use generic-lens or generic-optics with 'executionEndTime' instead"  #-}

-- | If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
--
-- /Note:/ Consider using 'executionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seExecutionStartTime :: Lens.Lens' StepExecution (Core.Maybe Core.NominalDiffTime)
seExecutionStartTime = Lens.field @"executionStartTime"
{-# INLINEABLE seExecutionStartTime #-}
{-# DEPRECATED executionStartTime "Use generic-lens or generic-optics with 'executionStartTime' instead"  #-}

-- | Information about the Automation failure.
--
-- /Note:/ Consider using 'failureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seFailureDetails :: Lens.Lens' StepExecution (Core.Maybe Types.FailureDetails)
seFailureDetails = Lens.field @"failureDetails"
{-# INLINEABLE seFailureDetails #-}
{-# DEPRECATED failureDetails "Use generic-lens or generic-optics with 'failureDetails' instead"  #-}

-- | If a step failed, this message explains why the execution failed.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seFailureMessage :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
seFailureMessage = Lens.field @"failureMessage"
{-# INLINEABLE seFailureMessage #-}
{-# DEPRECATED failureMessage "Use generic-lens or generic-optics with 'failureMessage' instead"  #-}

-- | Fully-resolved values passed into the step before execution.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seInputs :: Lens.Lens' StepExecution (Core.Maybe (Core.HashMap Core.Text Core.Text))
seInputs = Lens.field @"inputs"
{-# INLINEABLE seInputs #-}
{-# DEPRECATED inputs "Use generic-lens or generic-optics with 'inputs' instead"  #-}

-- | The flag which can be used to help decide whether the failure of current step leads to the Automation failure.
--
-- /Note:/ Consider using 'isCritical' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seIsCritical :: Lens.Lens' StepExecution (Core.Maybe Core.Bool)
seIsCritical = Lens.field @"isCritical"
{-# INLINEABLE seIsCritical #-}
{-# DEPRECATED isCritical "Use generic-lens or generic-optics with 'isCritical' instead"  #-}

-- | The flag which can be used to end automation no matter whether the step succeeds or fails.
--
-- /Note:/ Consider using 'isEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seIsEnd :: Lens.Lens' StepExecution (Core.Maybe Core.Bool)
seIsEnd = Lens.field @"isEnd"
{-# INLINEABLE seIsEnd #-}
{-# DEPRECATED isEnd "Use generic-lens or generic-optics with 'isEnd' instead"  #-}

-- | The maximum number of tries to run the action of the step. The default value is 1.
--
-- /Note:/ Consider using 'maxAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMaxAttempts :: Lens.Lens' StepExecution (Core.Maybe Core.Int)
seMaxAttempts = Lens.field @"maxAttempts"
{-# INLINEABLE seMaxAttempts #-}
{-# DEPRECATED maxAttempts "Use generic-lens or generic-optics with 'maxAttempts' instead"  #-}

-- | The next step after the step succeeds.
--
-- /Note:/ Consider using 'nextStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seNextStep :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
seNextStep = Lens.field @"nextStep"
{-# INLINEABLE seNextStep #-}
{-# DEPRECATED nextStep "Use generic-lens or generic-optics with 'nextStep' instead"  #-}

-- | The action to take if the step fails. The default value is Abort.
--
-- /Note:/ Consider using 'onFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOnFailure :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
seOnFailure = Lens.field @"onFailure"
{-# INLINEABLE seOnFailure #-}
{-# DEPRECATED onFailure "Use generic-lens or generic-optics with 'onFailure' instead"  #-}

-- | Returned values from the execution of the step.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOutputs :: Lens.Lens' StepExecution (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
seOutputs = Lens.field @"outputs"
{-# INLINEABLE seOutputs #-}
{-# DEPRECATED outputs "Use generic-lens or generic-optics with 'outputs' instead"  #-}

-- | A user-specified list of parameters to override when running a step.
--
-- /Note:/ Consider using 'overriddenParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seOverriddenParameters :: Lens.Lens' StepExecution (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
seOverriddenParameters = Lens.field @"overriddenParameters"
{-# INLINEABLE seOverriddenParameters #-}
{-# DEPRECATED overriddenParameters "Use generic-lens or generic-optics with 'overriddenParameters' instead"  #-}

-- | A message associated with the response code for an execution.
--
-- /Note:/ Consider using 'response' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seResponse :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
seResponse = Lens.field @"response"
{-# INLINEABLE seResponse #-}
{-# DEPRECATED response "Use generic-lens or generic-optics with 'response' instead"  #-}

-- | The response code returned by the execution of the step.
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seResponseCode :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
seResponseCode = Lens.field @"responseCode"
{-# INLINEABLE seResponseCode #-}
{-# DEPRECATED responseCode "Use generic-lens or generic-optics with 'responseCode' instead"  #-}

-- | The unique ID of a step execution.
--
-- /Note:/ Consider using 'stepExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStepExecutionId :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
seStepExecutionId = Lens.field @"stepExecutionId"
{-# INLINEABLE seStepExecutionId #-}
{-# DEPRECATED stepExecutionId "Use generic-lens or generic-optics with 'stepExecutionId' instead"  #-}

-- | The name of this execution step.
--
-- /Note:/ Consider using 'stepName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStepName :: Lens.Lens' StepExecution (Core.Maybe Core.Text)
seStepName = Lens.field @"stepName"
{-# INLINEABLE seStepName #-}
{-# DEPRECATED stepName "Use generic-lens or generic-optics with 'stepName' instead"  #-}

-- | The execution status for this step.
--
-- /Note:/ Consider using 'stepStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStepStatus :: Lens.Lens' StepExecution (Core.Maybe Types.AutomationExecutionStatus)
seStepStatus = Lens.field @"stepStatus"
{-# INLINEABLE seStepStatus #-}
{-# DEPRECATED stepStatus "Use generic-lens or generic-optics with 'stepStatus' instead"  #-}

-- | The combination of AWS Regions and accounts targeted by the current Automation execution.
--
-- /Note:/ Consider using 'targetLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTargetLocation :: Lens.Lens' StepExecution (Core.Maybe Types.TargetLocation)
seTargetLocation = Lens.field @"targetLocation"
{-# INLINEABLE seTargetLocation #-}
{-# DEPRECATED targetLocation "Use generic-lens or generic-optics with 'targetLocation' instead"  #-}

-- | The targets for the step execution.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTargets :: Lens.Lens' StepExecution (Core.Maybe [Types.Target])
seTargets = Lens.field @"targets"
{-# INLINEABLE seTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | The timeout seconds of the step.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seTimeoutSeconds :: Lens.Lens' StepExecution (Core.Maybe Core.Integer)
seTimeoutSeconds = Lens.field @"timeoutSeconds"
{-# INLINEABLE seTimeoutSeconds #-}
{-# DEPRECATED timeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead"  #-}

-- | Strategies used when step fails, we support Continue and Abort. Abort will fail the automation when the step fails. Continue will ignore the failure of current step and allow automation to run the next step. With conditional branching, we add step:stepName to support the automation to go to another specific step.
--
-- /Note:/ Consider using 'validNextSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seValidNextSteps :: Lens.Lens' StepExecution (Core.Maybe [Types.ValidNextStep])
seValidNextSteps = Lens.field @"validNextSteps"
{-# INLINEABLE seValidNextSteps #-}
{-# DEPRECATED validNextSteps "Use generic-lens or generic-optics with 'validNextSteps' instead"  #-}

instance Core.FromJSON StepExecution where
        parseJSON
          = Core.withObject "StepExecution" Core.$
              \ x ->
                StepExecution' Core.<$>
                  (x Core..:? "Action") Core.<*> x Core..:? "ExecutionEndTime"
                    Core.<*> x Core..:? "ExecutionStartTime"
                    Core.<*> x Core..:? "FailureDetails"
                    Core.<*> x Core..:? "FailureMessage"
                    Core.<*> x Core..:? "Inputs"
                    Core.<*> x Core..:? "IsCritical"
                    Core.<*> x Core..:? "IsEnd"
                    Core.<*> x Core..:? "MaxAttempts"
                    Core.<*> x Core..:? "NextStep"
                    Core.<*> x Core..:? "OnFailure"
                    Core.<*> x Core..:? "Outputs"
                    Core.<*> x Core..:? "OverriddenParameters"
                    Core.<*> x Core..:? "Response"
                    Core.<*> x Core..:? "ResponseCode"
                    Core.<*> x Core..:? "StepExecutionId"
                    Core.<*> x Core..:? "StepName"
                    Core.<*> x Core..:? "StepStatus"
                    Core.<*> x Core..:? "TargetLocation"
                    Core.<*> x Core..:? "Targets"
                    Core.<*> x Core..:? "TimeoutSeconds"
                    Core.<*> x Core..:? "ValidNextSteps"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AutomationExecution
  ( AutomationExecution (..)
  -- * Smart constructor
  , mkAutomationExecution
  -- * Lenses
  , aeAutomationExecutionId
  , aeAutomationExecutionStatus
  , aeCurrentAction
  , aeCurrentStepName
  , aeDocumentName
  , aeDocumentVersion
  , aeExecutedBy
  , aeExecutionEndTime
  , aeExecutionStartTime
  , aeFailureMessage
  , aeMaxConcurrency
  , aeMaxErrors
  , aeMode
  , aeOutputs
  , aeParameters
  , aeParentAutomationExecutionId
  , aeProgressCounters
  , aeResolvedTargets
  , aeStepExecutions
  , aeStepExecutionsTruncated
  , aeTarget
  , aeTargetLocations
  , aeTargetMaps
  , aeTargetParameterName
  , aeTargets
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AutomationExecutionId as Types
import qualified Network.AWS.SSM.Types.AutomationExecutionStatus as Types
import qualified Network.AWS.SSM.Types.AutomationParameterKey as Types
import qualified Network.AWS.SSM.Types.AutomationParameterValue as Types
import qualified Network.AWS.SSM.Types.DocumentName as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.ExecutionMode as Types
import qualified Network.AWS.SSM.Types.MaxConcurrency as Types
import qualified Network.AWS.SSM.Types.MaxErrors as Types
import qualified Network.AWS.SSM.Types.ParentAutomationExecutionId as Types
import qualified Network.AWS.SSM.Types.ProgressCounters as Types
import qualified Network.AWS.SSM.Types.ResolvedTargets as Types
import qualified Network.AWS.SSM.Types.StepExecution as Types
import qualified Network.AWS.SSM.Types.Target as Types
import qualified Network.AWS.SSM.Types.TargetLocation as Types
import qualified Network.AWS.SSM.Types.TargetMapKey as Types
import qualified Network.AWS.SSM.Types.TargetMapValue as Types
import qualified Network.AWS.SSM.Types.TargetParameterName as Types

-- | Detailed information about the current state of an individual Automation execution.
--
-- /See:/ 'mkAutomationExecution' smart constructor.
data AutomationExecution = AutomationExecution'
  { automationExecutionId :: Core.Maybe Types.AutomationExecutionId
    -- ^ The execution ID.
  , automationExecutionStatus :: Core.Maybe Types.AutomationExecutionStatus
    -- ^ The execution status of the Automation.
  , currentAction :: Core.Maybe Core.Text
    -- ^ The action of the step that is currently running.
  , currentStepName :: Core.Maybe Core.Text
    -- ^ The name of the step that is currently running.
  , documentName :: Core.Maybe Types.DocumentName
    -- ^ The name of the Automation document used during the execution.
  , documentVersion :: Core.Maybe Types.DocumentVersion
    -- ^ The version of the document to use during execution.
  , executedBy :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the user who ran the automation.
  , executionEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the execution finished.
  , executionStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the execution started.
  , failureMessage :: Core.Maybe Core.Text
    -- ^ A message describing why an execution has failed, if the status is set to Failed.
  , maxConcurrency :: Core.Maybe Types.MaxConcurrency
    -- ^ The MaxConcurrency value specified by the user when the execution started.
  , maxErrors :: Core.Maybe Types.MaxErrors
    -- ^ The MaxErrors value specified by the user when the execution started.
  , mode :: Core.Maybe Types.ExecutionMode
    -- ^ The automation execution mode.
  , outputs :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue])
    -- ^ The list of execution outputs as defined in the automation document.
  , parameters :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue])
    -- ^ The key-value map of execution parameters, which were supplied when calling StartAutomationExecution.
  , parentAutomationExecutionId :: Core.Maybe Types.ParentAutomationExecutionId
    -- ^ The AutomationExecutionId of the parent automation.
  , progressCounters :: Core.Maybe Types.ProgressCounters
    -- ^ An aggregate of step execution statuses displayed in the AWS Console for a multi-Region and multi-account Automation execution.
  , resolvedTargets :: Core.Maybe Types.ResolvedTargets
    -- ^ A list of resolved targets in the rate control execution.
  , stepExecutions :: Core.Maybe [Types.StepExecution]
    -- ^ A list of details about the current state of all steps that comprise an execution. An Automation document contains a list of steps that are run in order.
  , stepExecutionsTruncated :: Core.Maybe Core.Bool
    -- ^ A boolean value that indicates if the response contains the full list of the Automation step executions. If true, use the DescribeAutomationStepExecutions API action to get the full list of step executions.
  , target :: Core.Maybe Core.Text
    -- ^ The target of the execution.
  , targetLocations :: Core.Maybe (Core.NonEmpty Types.TargetLocation)
    -- ^ The combination of AWS Regions and/or AWS accounts where you want to run the Automation.
  , targetMaps :: Core.Maybe [Core.HashMap Types.TargetMapKey [Types.TargetMapValue]]
    -- ^ The specified key-value mapping of document parameters to target resources.
  , targetParameterName :: Core.Maybe Types.TargetParameterName
    -- ^ The parameter name.
  , targets :: Core.Maybe [Types.Target]
    -- ^ The specified targets.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AutomationExecution' value with any optional fields omitted.
mkAutomationExecution
    :: AutomationExecution
mkAutomationExecution
  = AutomationExecution'{automationExecutionId = Core.Nothing,
                         automationExecutionStatus = Core.Nothing,
                         currentAction = Core.Nothing, currentStepName = Core.Nothing,
                         documentName = Core.Nothing, documentVersion = Core.Nothing,
                         executedBy = Core.Nothing, executionEndTime = Core.Nothing,
                         executionStartTime = Core.Nothing, failureMessage = Core.Nothing,
                         maxConcurrency = Core.Nothing, maxErrors = Core.Nothing,
                         mode = Core.Nothing, outputs = Core.Nothing,
                         parameters = Core.Nothing,
                         parentAutomationExecutionId = Core.Nothing,
                         progressCounters = Core.Nothing, resolvedTargets = Core.Nothing,
                         stepExecutions = Core.Nothing,
                         stepExecutionsTruncated = Core.Nothing, target = Core.Nothing,
                         targetLocations = Core.Nothing, targetMaps = Core.Nothing,
                         targetParameterName = Core.Nothing, targets = Core.Nothing}

-- | The execution ID.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAutomationExecutionId :: Lens.Lens' AutomationExecution (Core.Maybe Types.AutomationExecutionId)
aeAutomationExecutionId = Lens.field @"automationExecutionId"
{-# INLINEABLE aeAutomationExecutionId #-}
{-# DEPRECATED automationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead"  #-}

-- | The execution status of the Automation.
--
-- /Note:/ Consider using 'automationExecutionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAutomationExecutionStatus :: Lens.Lens' AutomationExecution (Core.Maybe Types.AutomationExecutionStatus)
aeAutomationExecutionStatus = Lens.field @"automationExecutionStatus"
{-# INLINEABLE aeAutomationExecutionStatus #-}
{-# DEPRECATED automationExecutionStatus "Use generic-lens or generic-optics with 'automationExecutionStatus' instead"  #-}

-- | The action of the step that is currently running.
--
-- /Note:/ Consider using 'currentAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeCurrentAction :: Lens.Lens' AutomationExecution (Core.Maybe Core.Text)
aeCurrentAction = Lens.field @"currentAction"
{-# INLINEABLE aeCurrentAction #-}
{-# DEPRECATED currentAction "Use generic-lens or generic-optics with 'currentAction' instead"  #-}

-- | The name of the step that is currently running.
--
-- /Note:/ Consider using 'currentStepName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeCurrentStepName :: Lens.Lens' AutomationExecution (Core.Maybe Core.Text)
aeCurrentStepName = Lens.field @"currentStepName"
{-# INLINEABLE aeCurrentStepName #-}
{-# DEPRECATED currentStepName "Use generic-lens or generic-optics with 'currentStepName' instead"  #-}

-- | The name of the Automation document used during the execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeDocumentName :: Lens.Lens' AutomationExecution (Core.Maybe Types.DocumentName)
aeDocumentName = Lens.field @"documentName"
{-# INLINEABLE aeDocumentName #-}
{-# DEPRECATED documentName "Use generic-lens or generic-optics with 'documentName' instead"  #-}

-- | The version of the document to use during execution.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeDocumentVersion :: Lens.Lens' AutomationExecution (Core.Maybe Types.DocumentVersion)
aeDocumentVersion = Lens.field @"documentVersion"
{-# INLINEABLE aeDocumentVersion #-}
{-# DEPRECATED documentVersion "Use generic-lens or generic-optics with 'documentVersion' instead"  #-}

-- | The Amazon Resource Name (ARN) of the user who ran the automation.
--
-- /Note:/ Consider using 'executedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExecutedBy :: Lens.Lens' AutomationExecution (Core.Maybe Core.Text)
aeExecutedBy = Lens.field @"executedBy"
{-# INLINEABLE aeExecutedBy #-}
{-# DEPRECATED executedBy "Use generic-lens or generic-optics with 'executedBy' instead"  #-}

-- | The time the execution finished.
--
-- /Note:/ Consider using 'executionEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExecutionEndTime :: Lens.Lens' AutomationExecution (Core.Maybe Core.NominalDiffTime)
aeExecutionEndTime = Lens.field @"executionEndTime"
{-# INLINEABLE aeExecutionEndTime #-}
{-# DEPRECATED executionEndTime "Use generic-lens or generic-optics with 'executionEndTime' instead"  #-}

-- | The time the execution started.
--
-- /Note:/ Consider using 'executionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExecutionStartTime :: Lens.Lens' AutomationExecution (Core.Maybe Core.NominalDiffTime)
aeExecutionStartTime = Lens.field @"executionStartTime"
{-# INLINEABLE aeExecutionStartTime #-}
{-# DEPRECATED executionStartTime "Use generic-lens or generic-optics with 'executionStartTime' instead"  #-}

-- | A message describing why an execution has failed, if the status is set to Failed.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeFailureMessage :: Lens.Lens' AutomationExecution (Core.Maybe Core.Text)
aeFailureMessage = Lens.field @"failureMessage"
{-# INLINEABLE aeFailureMessage #-}
{-# DEPRECATED failureMessage "Use generic-lens or generic-optics with 'failureMessage' instead"  #-}

-- | The MaxConcurrency value specified by the user when the execution started.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeMaxConcurrency :: Lens.Lens' AutomationExecution (Core.Maybe Types.MaxConcurrency)
aeMaxConcurrency = Lens.field @"maxConcurrency"
{-# INLINEABLE aeMaxConcurrency #-}
{-# DEPRECATED maxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead"  #-}

-- | The MaxErrors value specified by the user when the execution started.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeMaxErrors :: Lens.Lens' AutomationExecution (Core.Maybe Types.MaxErrors)
aeMaxErrors = Lens.field @"maxErrors"
{-# INLINEABLE aeMaxErrors #-}
{-# DEPRECATED maxErrors "Use generic-lens or generic-optics with 'maxErrors' instead"  #-}

-- | The automation execution mode.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeMode :: Lens.Lens' AutomationExecution (Core.Maybe Types.ExecutionMode)
aeMode = Lens.field @"mode"
{-# INLINEABLE aeMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | The list of execution outputs as defined in the automation document.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeOutputs :: Lens.Lens' AutomationExecution (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
aeOutputs = Lens.field @"outputs"
{-# INLINEABLE aeOutputs #-}
{-# DEPRECATED outputs "Use generic-lens or generic-optics with 'outputs' instead"  #-}

-- | The key-value map of execution parameters, which were supplied when calling StartAutomationExecution.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeParameters :: Lens.Lens' AutomationExecution (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
aeParameters = Lens.field @"parameters"
{-# INLINEABLE aeParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The AutomationExecutionId of the parent automation.
--
-- /Note:/ Consider using 'parentAutomationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeParentAutomationExecutionId :: Lens.Lens' AutomationExecution (Core.Maybe Types.ParentAutomationExecutionId)
aeParentAutomationExecutionId = Lens.field @"parentAutomationExecutionId"
{-# INLINEABLE aeParentAutomationExecutionId #-}
{-# DEPRECATED parentAutomationExecutionId "Use generic-lens or generic-optics with 'parentAutomationExecutionId' instead"  #-}

-- | An aggregate of step execution statuses displayed in the AWS Console for a multi-Region and multi-account Automation execution.
--
-- /Note:/ Consider using 'progressCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeProgressCounters :: Lens.Lens' AutomationExecution (Core.Maybe Types.ProgressCounters)
aeProgressCounters = Lens.field @"progressCounters"
{-# INLINEABLE aeProgressCounters #-}
{-# DEPRECATED progressCounters "Use generic-lens or generic-optics with 'progressCounters' instead"  #-}

-- | A list of resolved targets in the rate control execution.
--
-- /Note:/ Consider using 'resolvedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeResolvedTargets :: Lens.Lens' AutomationExecution (Core.Maybe Types.ResolvedTargets)
aeResolvedTargets = Lens.field @"resolvedTargets"
{-# INLINEABLE aeResolvedTargets #-}
{-# DEPRECATED resolvedTargets "Use generic-lens or generic-optics with 'resolvedTargets' instead"  #-}

-- | A list of details about the current state of all steps that comprise an execution. An Automation document contains a list of steps that are run in order.
--
-- /Note:/ Consider using 'stepExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeStepExecutions :: Lens.Lens' AutomationExecution (Core.Maybe [Types.StepExecution])
aeStepExecutions = Lens.field @"stepExecutions"
{-# INLINEABLE aeStepExecutions #-}
{-# DEPRECATED stepExecutions "Use generic-lens or generic-optics with 'stepExecutions' instead"  #-}

-- | A boolean value that indicates if the response contains the full list of the Automation step executions. If true, use the DescribeAutomationStepExecutions API action to get the full list of step executions.
--
-- /Note:/ Consider using 'stepExecutionsTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeStepExecutionsTruncated :: Lens.Lens' AutomationExecution (Core.Maybe Core.Bool)
aeStepExecutionsTruncated = Lens.field @"stepExecutionsTruncated"
{-# INLINEABLE aeStepExecutionsTruncated #-}
{-# DEPRECATED stepExecutionsTruncated "Use generic-lens or generic-optics with 'stepExecutionsTruncated' instead"  #-}

-- | The target of the execution.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTarget :: Lens.Lens' AutomationExecution (Core.Maybe Core.Text)
aeTarget = Lens.field @"target"
{-# INLINEABLE aeTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

-- | The combination of AWS Regions and/or AWS accounts where you want to run the Automation.
--
-- /Note:/ Consider using 'targetLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTargetLocations :: Lens.Lens' AutomationExecution (Core.Maybe (Core.NonEmpty Types.TargetLocation))
aeTargetLocations = Lens.field @"targetLocations"
{-# INLINEABLE aeTargetLocations #-}
{-# DEPRECATED targetLocations "Use generic-lens or generic-optics with 'targetLocations' instead"  #-}

-- | The specified key-value mapping of document parameters to target resources.
--
-- /Note:/ Consider using 'targetMaps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTargetMaps :: Lens.Lens' AutomationExecution (Core.Maybe [Core.HashMap Types.TargetMapKey [Types.TargetMapValue]])
aeTargetMaps = Lens.field @"targetMaps"
{-# INLINEABLE aeTargetMaps #-}
{-# DEPRECATED targetMaps "Use generic-lens or generic-optics with 'targetMaps' instead"  #-}

-- | The parameter name.
--
-- /Note:/ Consider using 'targetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTargetParameterName :: Lens.Lens' AutomationExecution (Core.Maybe Types.TargetParameterName)
aeTargetParameterName = Lens.field @"targetParameterName"
{-# INLINEABLE aeTargetParameterName #-}
{-# DEPRECATED targetParameterName "Use generic-lens or generic-optics with 'targetParameterName' instead"  #-}

-- | The specified targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTargets :: Lens.Lens' AutomationExecution (Core.Maybe [Types.Target])
aeTargets = Lens.field @"targets"
{-# INLINEABLE aeTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

instance Core.FromJSON AutomationExecution where
        parseJSON
          = Core.withObject "AutomationExecution" Core.$
              \ x ->
                AutomationExecution' Core.<$>
                  (x Core..:? "AutomationExecutionId") Core.<*>
                    x Core..:? "AutomationExecutionStatus"
                    Core.<*> x Core..:? "CurrentAction"
                    Core.<*> x Core..:? "CurrentStepName"
                    Core.<*> x Core..:? "DocumentName"
                    Core.<*> x Core..:? "DocumentVersion"
                    Core.<*> x Core..:? "ExecutedBy"
                    Core.<*> x Core..:? "ExecutionEndTime"
                    Core.<*> x Core..:? "ExecutionStartTime"
                    Core.<*> x Core..:? "FailureMessage"
                    Core.<*> x Core..:? "MaxConcurrency"
                    Core.<*> x Core..:? "MaxErrors"
                    Core.<*> x Core..:? "Mode"
                    Core.<*> x Core..:? "Outputs"
                    Core.<*> x Core..:? "Parameters"
                    Core.<*> x Core..:? "ParentAutomationExecutionId"
                    Core.<*> x Core..:? "ProgressCounters"
                    Core.<*> x Core..:? "ResolvedTargets"
                    Core.<*> x Core..:? "StepExecutions"
                    Core.<*> x Core..:? "StepExecutionsTruncated"
                    Core.<*> x Core..:? "Target"
                    Core.<*> x Core..:? "TargetLocations"
                    Core.<*> x Core..:? "TargetMaps"
                    Core.<*> x Core..:? "TargetParameterName"
                    Core.<*> x Core..:? "Targets"

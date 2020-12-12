{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecution
  ( AutomationExecution (..),

    -- * Smart constructor
    mkAutomationExecution,

    -- * Lenses
    aeCurrentStepName,
    aeTargetParameterName,
    aeTargetLocations,
    aeProgressCounters,
    aeExecutedBy,
    aeDocumentName,
    aeExecutionEndTime,
    aeFailureMessage,
    aeMode,
    aeTargetMaps,
    aeStepExecutionsTruncated,
    aeAutomationExecutionStatus,
    aeParentAutomationExecutionId,
    aeOutputs,
    aeMaxErrors,
    aeExecutionStartTime,
    aeCurrentAction,
    aeTargets,
    aeResolvedTargets,
    aeParameters,
    aeDocumentVersion,
    aeAutomationExecutionId,
    aeStepExecutions,
    aeMaxConcurrency,
    aeTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.ExecutionMode
import Network.AWS.SSM.Types.ProgressCounters
import Network.AWS.SSM.Types.ResolvedTargets
import Network.AWS.SSM.Types.StepExecution
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Detailed information about the current state of an individual Automation execution.
--
-- /See:/ 'mkAutomationExecution' smart constructor.
data AutomationExecution = AutomationExecution'
  { currentStepName ::
      Lude.Maybe Lude.Text,
    targetParameterName :: Lude.Maybe Lude.Text,
    targetLocations ::
      Lude.Maybe (Lude.NonEmpty TargetLocation),
    progressCounters :: Lude.Maybe ProgressCounters,
    executedBy :: Lude.Maybe Lude.Text,
    documentName :: Lude.Maybe Lude.Text,
    executionEndTime :: Lude.Maybe Lude.Timestamp,
    failureMessage :: Lude.Maybe Lude.Text,
    mode :: Lude.Maybe ExecutionMode,
    targetMaps ::
      Lude.Maybe [Lude.HashMap Lude.Text ([Lude.Text])],
    stepExecutionsTruncated :: Lude.Maybe Lude.Bool,
    automationExecutionStatus ::
      Lude.Maybe AutomationExecutionStatus,
    parentAutomationExecutionId :: Lude.Maybe Lude.Text,
    outputs ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    maxErrors :: Lude.Maybe Lude.Text,
    executionStartTime :: Lude.Maybe Lude.Timestamp,
    currentAction :: Lude.Maybe Lude.Text,
    targets :: Lude.Maybe [Target],
    resolvedTargets :: Lude.Maybe ResolvedTargets,
    parameters ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    documentVersion :: Lude.Maybe Lude.Text,
    automationExecutionId :: Lude.Maybe Lude.Text,
    stepExecutions :: Lude.Maybe [StepExecution],
    maxConcurrency :: Lude.Maybe Lude.Text,
    target :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutomationExecution' with the minimum fields required to make a request.
--
-- * 'automationExecutionId' - The execution ID.
-- * 'automationExecutionStatus' - The execution status of the Automation.
-- * 'currentAction' - The action of the step that is currently running.
-- * 'currentStepName' - The name of the step that is currently running.
-- * 'documentName' - The name of the Automation document used during the execution.
-- * 'documentVersion' - The version of the document to use during execution.
-- * 'executedBy' - The Amazon Resource Name (ARN) of the user who ran the automation.
-- * 'executionEndTime' - The time the execution finished.
-- * 'executionStartTime' - The time the execution started.
-- * 'failureMessage' - A message describing why an execution has failed, if the status is set to Failed.
-- * 'maxConcurrency' - The MaxConcurrency value specified by the user when the execution started.
-- * 'maxErrors' - The MaxErrors value specified by the user when the execution started.
-- * 'mode' - The automation execution mode.
-- * 'outputs' - The list of execution outputs as defined in the automation document.
-- * 'parameters' - The key-value map of execution parameters, which were supplied when calling StartAutomationExecution.
-- * 'parentAutomationExecutionId' - The AutomationExecutionId of the parent automation.
-- * 'progressCounters' - An aggregate of step execution statuses displayed in the AWS Console for a multi-Region and multi-account Automation execution.
-- * 'resolvedTargets' - A list of resolved targets in the rate control execution.
-- * 'stepExecutions' - A list of details about the current state of all steps that comprise an execution. An Automation document contains a list of steps that are run in order.
-- * 'stepExecutionsTruncated' - A boolean value that indicates if the response contains the full list of the Automation step executions. If true, use the DescribeAutomationStepExecutions API action to get the full list of step executions.
-- * 'target' - The target of the execution.
-- * 'targetLocations' - The combination of AWS Regions and/or AWS accounts where you want to run the Automation.
-- * 'targetMaps' - The specified key-value mapping of document parameters to target resources.
-- * 'targetParameterName' - The parameter name.
-- * 'targets' - The specified targets.
mkAutomationExecution ::
  AutomationExecution
mkAutomationExecution =
  AutomationExecution'
    { currentStepName = Lude.Nothing,
      targetParameterName = Lude.Nothing,
      targetLocations = Lude.Nothing,
      progressCounters = Lude.Nothing,
      executedBy = Lude.Nothing,
      documentName = Lude.Nothing,
      executionEndTime = Lude.Nothing,
      failureMessage = Lude.Nothing,
      mode = Lude.Nothing,
      targetMaps = Lude.Nothing,
      stepExecutionsTruncated = Lude.Nothing,
      automationExecutionStatus = Lude.Nothing,
      parentAutomationExecutionId = Lude.Nothing,
      outputs = Lude.Nothing,
      maxErrors = Lude.Nothing,
      executionStartTime = Lude.Nothing,
      currentAction = Lude.Nothing,
      targets = Lude.Nothing,
      resolvedTargets = Lude.Nothing,
      parameters = Lude.Nothing,
      documentVersion = Lude.Nothing,
      automationExecutionId = Lude.Nothing,
      stepExecutions = Lude.Nothing,
      maxConcurrency = Lude.Nothing,
      target = Lude.Nothing
    }

-- | The name of the step that is currently running.
--
-- /Note:/ Consider using 'currentStepName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeCurrentStepName :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeCurrentStepName = Lens.lens (currentStepName :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {currentStepName = a} :: AutomationExecution)
{-# DEPRECATED aeCurrentStepName "Use generic-lens or generic-optics with 'currentStepName' instead." #-}

-- | The parameter name.
--
-- /Note:/ Consider using 'targetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTargetParameterName :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeTargetParameterName = Lens.lens (targetParameterName :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {targetParameterName = a} :: AutomationExecution)
{-# DEPRECATED aeTargetParameterName "Use generic-lens or generic-optics with 'targetParameterName' instead." #-}

-- | The combination of AWS Regions and/or AWS accounts where you want to run the Automation.
--
-- /Note:/ Consider using 'targetLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTargetLocations :: Lens.Lens' AutomationExecution (Lude.Maybe (Lude.NonEmpty TargetLocation))
aeTargetLocations = Lens.lens (targetLocations :: AutomationExecution -> Lude.Maybe (Lude.NonEmpty TargetLocation)) (\s a -> s {targetLocations = a} :: AutomationExecution)
{-# DEPRECATED aeTargetLocations "Use generic-lens or generic-optics with 'targetLocations' instead." #-}

-- | An aggregate of step execution statuses displayed in the AWS Console for a multi-Region and multi-account Automation execution.
--
-- /Note:/ Consider using 'progressCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeProgressCounters :: Lens.Lens' AutomationExecution (Lude.Maybe ProgressCounters)
aeProgressCounters = Lens.lens (progressCounters :: AutomationExecution -> Lude.Maybe ProgressCounters) (\s a -> s {progressCounters = a} :: AutomationExecution)
{-# DEPRECATED aeProgressCounters "Use generic-lens or generic-optics with 'progressCounters' instead." #-}

-- | The Amazon Resource Name (ARN) of the user who ran the automation.
--
-- /Note:/ Consider using 'executedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExecutedBy :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeExecutedBy = Lens.lens (executedBy :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {executedBy = a} :: AutomationExecution)
{-# DEPRECATED aeExecutedBy "Use generic-lens or generic-optics with 'executedBy' instead." #-}

-- | The name of the Automation document used during the execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeDocumentName :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeDocumentName = Lens.lens (documentName :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {documentName = a} :: AutomationExecution)
{-# DEPRECATED aeDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The time the execution finished.
--
-- /Note:/ Consider using 'executionEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExecutionEndTime :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Timestamp)
aeExecutionEndTime = Lens.lens (executionEndTime :: AutomationExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {executionEndTime = a} :: AutomationExecution)
{-# DEPRECATED aeExecutionEndTime "Use generic-lens or generic-optics with 'executionEndTime' instead." #-}

-- | A message describing why an execution has failed, if the status is set to Failed.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeFailureMessage :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeFailureMessage = Lens.lens (failureMessage :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: AutomationExecution)
{-# DEPRECATED aeFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | The automation execution mode.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeMode :: Lens.Lens' AutomationExecution (Lude.Maybe ExecutionMode)
aeMode = Lens.lens (mode :: AutomationExecution -> Lude.Maybe ExecutionMode) (\s a -> s {mode = a} :: AutomationExecution)
{-# DEPRECATED aeMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The specified key-value mapping of document parameters to target resources.
--
-- /Note:/ Consider using 'targetMaps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTargetMaps :: Lens.Lens' AutomationExecution (Lude.Maybe [Lude.HashMap Lude.Text ([Lude.Text])])
aeTargetMaps = Lens.lens (targetMaps :: AutomationExecution -> Lude.Maybe [Lude.HashMap Lude.Text ([Lude.Text])]) (\s a -> s {targetMaps = a} :: AutomationExecution)
{-# DEPRECATED aeTargetMaps "Use generic-lens or generic-optics with 'targetMaps' instead." #-}

-- | A boolean value that indicates if the response contains the full list of the Automation step executions. If true, use the DescribeAutomationStepExecutions API action to get the full list of step executions.
--
-- /Note:/ Consider using 'stepExecutionsTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeStepExecutionsTruncated :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Bool)
aeStepExecutionsTruncated = Lens.lens (stepExecutionsTruncated :: AutomationExecution -> Lude.Maybe Lude.Bool) (\s a -> s {stepExecutionsTruncated = a} :: AutomationExecution)
{-# DEPRECATED aeStepExecutionsTruncated "Use generic-lens or generic-optics with 'stepExecutionsTruncated' instead." #-}

-- | The execution status of the Automation.
--
-- /Note:/ Consider using 'automationExecutionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAutomationExecutionStatus :: Lens.Lens' AutomationExecution (Lude.Maybe AutomationExecutionStatus)
aeAutomationExecutionStatus = Lens.lens (automationExecutionStatus :: AutomationExecution -> Lude.Maybe AutomationExecutionStatus) (\s a -> s {automationExecutionStatus = a} :: AutomationExecution)
{-# DEPRECATED aeAutomationExecutionStatus "Use generic-lens or generic-optics with 'automationExecutionStatus' instead." #-}

-- | The AutomationExecutionId of the parent automation.
--
-- /Note:/ Consider using 'parentAutomationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeParentAutomationExecutionId :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeParentAutomationExecutionId = Lens.lens (parentAutomationExecutionId :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {parentAutomationExecutionId = a} :: AutomationExecution)
{-# DEPRECATED aeParentAutomationExecutionId "Use generic-lens or generic-optics with 'parentAutomationExecutionId' instead." #-}

-- | The list of execution outputs as defined in the automation document.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeOutputs :: Lens.Lens' AutomationExecution (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
aeOutputs = Lens.lens (outputs :: AutomationExecution -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {outputs = a} :: AutomationExecution)
{-# DEPRECATED aeOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | The MaxErrors value specified by the user when the execution started.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeMaxErrors :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeMaxErrors = Lens.lens (maxErrors :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: AutomationExecution)
{-# DEPRECATED aeMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The time the execution started.
--
-- /Note:/ Consider using 'executionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExecutionStartTime :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Timestamp)
aeExecutionStartTime = Lens.lens (executionStartTime :: AutomationExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {executionStartTime = a} :: AutomationExecution)
{-# DEPRECATED aeExecutionStartTime "Use generic-lens or generic-optics with 'executionStartTime' instead." #-}

-- | The action of the step that is currently running.
--
-- /Note:/ Consider using 'currentAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeCurrentAction :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeCurrentAction = Lens.lens (currentAction :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {currentAction = a} :: AutomationExecution)
{-# DEPRECATED aeCurrentAction "Use generic-lens or generic-optics with 'currentAction' instead." #-}

-- | The specified targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTargets :: Lens.Lens' AutomationExecution (Lude.Maybe [Target])
aeTargets = Lens.lens (targets :: AutomationExecution -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: AutomationExecution)
{-# DEPRECATED aeTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | A list of resolved targets in the rate control execution.
--
-- /Note:/ Consider using 'resolvedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeResolvedTargets :: Lens.Lens' AutomationExecution (Lude.Maybe ResolvedTargets)
aeResolvedTargets = Lens.lens (resolvedTargets :: AutomationExecution -> Lude.Maybe ResolvedTargets) (\s a -> s {resolvedTargets = a} :: AutomationExecution)
{-# DEPRECATED aeResolvedTargets "Use generic-lens or generic-optics with 'resolvedTargets' instead." #-}

-- | The key-value map of execution parameters, which were supplied when calling StartAutomationExecution.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeParameters :: Lens.Lens' AutomationExecution (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
aeParameters = Lens.lens (parameters :: AutomationExecution -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: AutomationExecution)
{-# DEPRECATED aeParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The version of the document to use during execution.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeDocumentVersion :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeDocumentVersion = Lens.lens (documentVersion :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: AutomationExecution)
{-# DEPRECATED aeDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The execution ID.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAutomationExecutionId :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeAutomationExecutionId = Lens.lens (automationExecutionId :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {automationExecutionId = a} :: AutomationExecution)
{-# DEPRECATED aeAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

-- | A list of details about the current state of all steps that comprise an execution. An Automation document contains a list of steps that are run in order.
--
-- /Note:/ Consider using 'stepExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeStepExecutions :: Lens.Lens' AutomationExecution (Lude.Maybe [StepExecution])
aeStepExecutions = Lens.lens (stepExecutions :: AutomationExecution -> Lude.Maybe [StepExecution]) (\s a -> s {stepExecutions = a} :: AutomationExecution)
{-# DEPRECATED aeStepExecutions "Use generic-lens or generic-optics with 'stepExecutions' instead." #-}

-- | The MaxConcurrency value specified by the user when the execution started.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeMaxConcurrency :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeMaxConcurrency = Lens.lens (maxConcurrency :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: AutomationExecution)
{-# DEPRECATED aeMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The target of the execution.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeTarget :: Lens.Lens' AutomationExecution (Lude.Maybe Lude.Text)
aeTarget = Lens.lens (target :: AutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {target = a} :: AutomationExecution)
{-# DEPRECATED aeTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromJSON AutomationExecution where
  parseJSON =
    Lude.withObject
      "AutomationExecution"
      ( \x ->
          AutomationExecution'
            Lude.<$> (x Lude..:? "CurrentStepName")
            Lude.<*> (x Lude..:? "TargetParameterName")
            Lude.<*> (x Lude..:? "TargetLocations")
            Lude.<*> (x Lude..:? "ProgressCounters")
            Lude.<*> (x Lude..:? "ExecutedBy")
            Lude.<*> (x Lude..:? "DocumentName")
            Lude.<*> (x Lude..:? "ExecutionEndTime")
            Lude.<*> (x Lude..:? "FailureMessage")
            Lude.<*> (x Lude..:? "Mode")
            Lude.<*> (x Lude..:? "TargetMaps" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StepExecutionsTruncated")
            Lude.<*> (x Lude..:? "AutomationExecutionStatus")
            Lude.<*> (x Lude..:? "ParentAutomationExecutionId")
            Lude.<*> (x Lude..:? "Outputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "MaxErrors")
            Lude.<*> (x Lude..:? "ExecutionStartTime")
            Lude.<*> (x Lude..:? "CurrentAction")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResolvedTargets")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "AutomationExecutionId")
            Lude.<*> (x Lude..:? "StepExecutions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "MaxConcurrency")
            Lude.<*> (x Lude..:? "Target")
      )

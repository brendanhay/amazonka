{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionMetadata
  ( AutomationExecutionMetadata (..),

    -- * Smart constructor
    mkAutomationExecutionMetadata,

    -- * Lenses
    aemCurrentStepName,
    aemTargetParameterName,
    aemLogFile,
    aemExecutedBy,
    aemDocumentName,
    aemExecutionEndTime,
    aemFailureMessage,
    aemMode,
    aemTargetMaps,
    aemAutomationExecutionStatus,
    aemParentAutomationExecutionId,
    aemOutputs,
    aemMaxErrors,
    aemExecutionStartTime,
    aemAutomationType,
    aemCurrentAction,
    aemTargets,
    aemResolvedTargets,
    aemDocumentVersion,
    aemAutomationExecutionId,
    aemMaxConcurrency,
    aemTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.AutomationType
import Network.AWS.SSM.Types.ExecutionMode
import Network.AWS.SSM.Types.ResolvedTargets
import Network.AWS.SSM.Types.Target

-- | Details about a specific Automation execution.
--
-- /See:/ 'mkAutomationExecutionMetadata' smart constructor.
data AutomationExecutionMetadata = AutomationExecutionMetadata'
  { -- | The name of the step that is currently running.
    currentStepName :: Lude.Maybe Lude.Text,
    -- | The list of execution outputs as defined in the Automation document.
    targetParameterName :: Lude.Maybe Lude.Text,
    -- | An S3 bucket where execution information is stored.
    logFile :: Lude.Maybe Lude.Text,
    -- | The IAM role ARN of the user who ran the Automation.
    executedBy :: Lude.Maybe Lude.Text,
    -- | The name of the Automation document used during execution.
    documentName :: Lude.Maybe Lude.Text,
    -- | The time the execution finished. This is not populated if the execution is still in progress.
    executionEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The list of execution outputs as defined in the Automation document.
    failureMessage :: Lude.Maybe Lude.Text,
    -- | The Automation execution mode.
    mode :: Lude.Maybe ExecutionMode,
    -- | The specified key-value mapping of document parameters to target resources.
    targetMaps :: Lude.Maybe [Lude.HashMap Lude.Text ([Lude.Text])],
    -- | The status of the execution.
    automationExecutionStatus :: Lude.Maybe AutomationExecutionStatus,
    -- | The ExecutionId of the parent Automation.
    parentAutomationExecutionId :: Lude.Maybe Lude.Text,
    -- | The list of execution outputs as defined in the Automation document.
    outputs :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | The MaxErrors value specified by the user when starting the Automation.
    maxErrors :: Lude.Maybe Lude.Text,
    -- | The time the execution started.
    executionStartTime :: Lude.Maybe Lude.Timestamp,
    -- | Use this filter with 'DescribeAutomationExecutions' . Specify either Local or CrossAccount. CrossAccount is an Automation that runs in multiple AWS Regions and accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
    automationType :: Lude.Maybe AutomationType,
    -- | The action of the step that is currently running.
    currentAction :: Lude.Maybe Lude.Text,
    -- | The targets defined by the user when starting the Automation.
    targets :: Lude.Maybe [Target],
    -- | A list of targets that resolved during the execution.
    resolvedTargets :: Lude.Maybe ResolvedTargets,
    -- | The document version used during the execution.
    documentVersion :: Lude.Maybe Lude.Text,
    -- | The execution ID.
    automationExecutionId :: Lude.Maybe Lude.Text,
    -- | The MaxConcurrency value specified by the user when starting the Automation.
    maxConcurrency :: Lude.Maybe Lude.Text,
    -- | The list of execution outputs as defined in the Automation document.
    target :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutomationExecutionMetadata' with the minimum fields required to make a request.
--
-- * 'currentStepName' - The name of the step that is currently running.
-- * 'targetParameterName' - The list of execution outputs as defined in the Automation document.
-- * 'logFile' - An S3 bucket where execution information is stored.
-- * 'executedBy' - The IAM role ARN of the user who ran the Automation.
-- * 'documentName' - The name of the Automation document used during execution.
-- * 'executionEndTime' - The time the execution finished. This is not populated if the execution is still in progress.
-- * 'failureMessage' - The list of execution outputs as defined in the Automation document.
-- * 'mode' - The Automation execution mode.
-- * 'targetMaps' - The specified key-value mapping of document parameters to target resources.
-- * 'automationExecutionStatus' - The status of the execution.
-- * 'parentAutomationExecutionId' - The ExecutionId of the parent Automation.
-- * 'outputs' - The list of execution outputs as defined in the Automation document.
-- * 'maxErrors' - The MaxErrors value specified by the user when starting the Automation.
-- * 'executionStartTime' - The time the execution started.
-- * 'automationType' - Use this filter with 'DescribeAutomationExecutions' . Specify either Local or CrossAccount. CrossAccount is an Automation that runs in multiple AWS Regions and accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
-- * 'currentAction' - The action of the step that is currently running.
-- * 'targets' - The targets defined by the user when starting the Automation.
-- * 'resolvedTargets' - A list of targets that resolved during the execution.
-- * 'documentVersion' - The document version used during the execution.
-- * 'automationExecutionId' - The execution ID.
-- * 'maxConcurrency' - The MaxConcurrency value specified by the user when starting the Automation.
-- * 'target' - The list of execution outputs as defined in the Automation document.
mkAutomationExecutionMetadata ::
  AutomationExecutionMetadata
mkAutomationExecutionMetadata =
  AutomationExecutionMetadata'
    { currentStepName = Lude.Nothing,
      targetParameterName = Lude.Nothing,
      logFile = Lude.Nothing,
      executedBy = Lude.Nothing,
      documentName = Lude.Nothing,
      executionEndTime = Lude.Nothing,
      failureMessage = Lude.Nothing,
      mode = Lude.Nothing,
      targetMaps = Lude.Nothing,
      automationExecutionStatus = Lude.Nothing,
      parentAutomationExecutionId = Lude.Nothing,
      outputs = Lude.Nothing,
      maxErrors = Lude.Nothing,
      executionStartTime = Lude.Nothing,
      automationType = Lude.Nothing,
      currentAction = Lude.Nothing,
      targets = Lude.Nothing,
      resolvedTargets = Lude.Nothing,
      documentVersion = Lude.Nothing,
      automationExecutionId = Lude.Nothing,
      maxConcurrency = Lude.Nothing,
      target = Lude.Nothing
    }

-- | The name of the step that is currently running.
--
-- /Note:/ Consider using 'currentStepName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemCurrentStepName :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemCurrentStepName = Lens.lens (currentStepName :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {currentStepName = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemCurrentStepName "Use generic-lens or generic-optics with 'currentStepName' instead." #-}

-- | The list of execution outputs as defined in the Automation document.
--
-- /Note:/ Consider using 'targetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemTargetParameterName :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemTargetParameterName = Lens.lens (targetParameterName :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {targetParameterName = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemTargetParameterName "Use generic-lens or generic-optics with 'targetParameterName' instead." #-}

-- | An S3 bucket where execution information is stored.
--
-- /Note:/ Consider using 'logFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemLogFile :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemLogFile = Lens.lens (logFile :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {logFile = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemLogFile "Use generic-lens or generic-optics with 'logFile' instead." #-}

-- | The IAM role ARN of the user who ran the Automation.
--
-- /Note:/ Consider using 'executedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemExecutedBy :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemExecutedBy = Lens.lens (executedBy :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {executedBy = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemExecutedBy "Use generic-lens or generic-optics with 'executedBy' instead." #-}

-- | The name of the Automation document used during execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemDocumentName :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemDocumentName = Lens.lens (documentName :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {documentName = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The time the execution finished. This is not populated if the execution is still in progress.
--
-- /Note:/ Consider using 'executionEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemExecutionEndTime :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Timestamp)
aemExecutionEndTime = Lens.lens (executionEndTime :: AutomationExecutionMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {executionEndTime = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemExecutionEndTime "Use generic-lens or generic-optics with 'executionEndTime' instead." #-}

-- | The list of execution outputs as defined in the Automation document.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemFailureMessage :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemFailureMessage = Lens.lens (failureMessage :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | The Automation execution mode.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemMode :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe ExecutionMode)
aemMode = Lens.lens (mode :: AutomationExecutionMetadata -> Lude.Maybe ExecutionMode) (\s a -> s {mode = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The specified key-value mapping of document parameters to target resources.
--
-- /Note:/ Consider using 'targetMaps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemTargetMaps :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe [Lude.HashMap Lude.Text ([Lude.Text])])
aemTargetMaps = Lens.lens (targetMaps :: AutomationExecutionMetadata -> Lude.Maybe [Lude.HashMap Lude.Text ([Lude.Text])]) (\s a -> s {targetMaps = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemTargetMaps "Use generic-lens or generic-optics with 'targetMaps' instead." #-}

-- | The status of the execution.
--
-- /Note:/ Consider using 'automationExecutionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemAutomationExecutionStatus :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe AutomationExecutionStatus)
aemAutomationExecutionStatus = Lens.lens (automationExecutionStatus :: AutomationExecutionMetadata -> Lude.Maybe AutomationExecutionStatus) (\s a -> s {automationExecutionStatus = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemAutomationExecutionStatus "Use generic-lens or generic-optics with 'automationExecutionStatus' instead." #-}

-- | The ExecutionId of the parent Automation.
--
-- /Note:/ Consider using 'parentAutomationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemParentAutomationExecutionId :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemParentAutomationExecutionId = Lens.lens (parentAutomationExecutionId :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {parentAutomationExecutionId = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemParentAutomationExecutionId "Use generic-lens or generic-optics with 'parentAutomationExecutionId' instead." #-}

-- | The list of execution outputs as defined in the Automation document.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemOutputs :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
aemOutputs = Lens.lens (outputs :: AutomationExecutionMetadata -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {outputs = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | The MaxErrors value specified by the user when starting the Automation.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemMaxErrors :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemMaxErrors = Lens.lens (maxErrors :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The time the execution started.
--
-- /Note:/ Consider using 'executionStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemExecutionStartTime :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Timestamp)
aemExecutionStartTime = Lens.lens (executionStartTime :: AutomationExecutionMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {executionStartTime = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemExecutionStartTime "Use generic-lens or generic-optics with 'executionStartTime' instead." #-}

-- | Use this filter with 'DescribeAutomationExecutions' . Specify either Local or CrossAccount. CrossAccount is an Automation that runs in multiple AWS Regions and accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'automationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemAutomationType :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe AutomationType)
aemAutomationType = Lens.lens (automationType :: AutomationExecutionMetadata -> Lude.Maybe AutomationType) (\s a -> s {automationType = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemAutomationType "Use generic-lens or generic-optics with 'automationType' instead." #-}

-- | The action of the step that is currently running.
--
-- /Note:/ Consider using 'currentAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemCurrentAction :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemCurrentAction = Lens.lens (currentAction :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {currentAction = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemCurrentAction "Use generic-lens or generic-optics with 'currentAction' instead." #-}

-- | The targets defined by the user when starting the Automation.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemTargets :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe [Target])
aemTargets = Lens.lens (targets :: AutomationExecutionMetadata -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | A list of targets that resolved during the execution.
--
-- /Note:/ Consider using 'resolvedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemResolvedTargets :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe ResolvedTargets)
aemResolvedTargets = Lens.lens (resolvedTargets :: AutomationExecutionMetadata -> Lude.Maybe ResolvedTargets) (\s a -> s {resolvedTargets = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemResolvedTargets "Use generic-lens or generic-optics with 'resolvedTargets' instead." #-}

-- | The document version used during the execution.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemDocumentVersion :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemDocumentVersion = Lens.lens (documentVersion :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The execution ID.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemAutomationExecutionId :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemAutomationExecutionId = Lens.lens (automationExecutionId :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {automationExecutionId = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

-- | The MaxConcurrency value specified by the user when starting the Automation.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemMaxConcurrency :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemMaxConcurrency = Lens.lens (maxConcurrency :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

-- | The list of execution outputs as defined in the Automation document.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemTarget :: Lens.Lens' AutomationExecutionMetadata (Lude.Maybe Lude.Text)
aemTarget = Lens.lens (target :: AutomationExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {target = a} :: AutomationExecutionMetadata)
{-# DEPRECATED aemTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromJSON AutomationExecutionMetadata where
  parseJSON =
    Lude.withObject
      "AutomationExecutionMetadata"
      ( \x ->
          AutomationExecutionMetadata'
            Lude.<$> (x Lude..:? "CurrentStepName")
            Lude.<*> (x Lude..:? "TargetParameterName")
            Lude.<*> (x Lude..:? "LogFile")
            Lude.<*> (x Lude..:? "ExecutedBy")
            Lude.<*> (x Lude..:? "DocumentName")
            Lude.<*> (x Lude..:? "ExecutionEndTime")
            Lude.<*> (x Lude..:? "FailureMessage")
            Lude.<*> (x Lude..:? "Mode")
            Lude.<*> (x Lude..:? "TargetMaps" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AutomationExecutionStatus")
            Lude.<*> (x Lude..:? "ParentAutomationExecutionId")
            Lude.<*> (x Lude..:? "Outputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "MaxErrors")
            Lude.<*> (x Lude..:? "ExecutionStartTime")
            Lude.<*> (x Lude..:? "AutomationType")
            Lude.<*> (x Lude..:? "CurrentAction")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResolvedTargets")
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "AutomationExecutionId")
            Lude.<*> (x Lude..:? "MaxConcurrency")
            Lude.<*> (x Lude..:? "Target")
      )

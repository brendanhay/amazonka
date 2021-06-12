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
-- Module      : Network.AWS.SSM.Types.AutomationExecutionMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.AutomationSubtype
import Network.AWS.SSM.Types.AutomationType
import Network.AWS.SSM.Types.ExecutionMode
import Network.AWS.SSM.Types.ResolvedTargets
import Network.AWS.SSM.Types.Runbook
import Network.AWS.SSM.Types.Target

-- | Details about a specific Automation execution.
--
-- /See:/ 'newAutomationExecutionMetadata' smart constructor.
data AutomationExecutionMetadata = AutomationExecutionMetadata'
  { -- | The MaxErrors value specified by the user when starting the Automation.
    maxErrors :: Core.Maybe Core.Text,
    -- | The action of the step that is currently running.
    currentAction :: Core.Maybe Core.Text,
    -- | The ExecutionId of the parent Automation.
    parentAutomationExecutionId :: Core.Maybe Core.Text,
    -- | The list of execution outputs as defined in the Automation document.
    outputs :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The Automation execution mode.
    mode :: Core.Maybe ExecutionMode,
    -- | The list of execution outputs as defined in the Automation document.
    failureMessage :: Core.Maybe Core.Text,
    -- | The time the execution finished. This is not populated if the execution
    -- is still in progress.
    executionEndTime :: Core.Maybe Core.POSIX,
    -- | The name of the Automation document used during execution.
    documentName :: Core.Maybe Core.Text,
    -- | The execution ID.
    automationExecutionId :: Core.Maybe Core.Text,
    -- | The name of the Change Manager change request.
    changeRequestName :: Core.Maybe Core.Text,
    -- | The IAM role ARN of the user who ran the Automation.
    executedBy :: Core.Maybe Core.Text,
    -- | A list of targets that resolved during the execution.
    resolvedTargets :: Core.Maybe ResolvedTargets,
    -- | The targets defined by the user when starting the Automation.
    targets :: Core.Maybe [Target],
    -- | Use this filter with DescribeAutomationExecutions. Specify either Local
    -- or CrossAccount. CrossAccount is an Automation that runs in multiple AWS
    -- Regions and accounts. For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts>
    -- in the /AWS Systems Manager User Guide/.
    automationType :: Core.Maybe AutomationType,
    -- | The list of execution outputs as defined in the Automation document.
    targetParameterName :: Core.Maybe Core.Text,
    -- | The time the execution started.
    executionStartTime :: Core.Maybe Core.POSIX,
    -- | The name of the step that is currently running.
    currentStepName :: Core.Maybe Core.Text,
    -- | The ID of a State Manager association used in the Automation operation.
    associationId :: Core.Maybe Core.Text,
    -- | The ID of an OpsItem that is created to represent a Change Manager
    -- change request.
    opsItemId :: Core.Maybe Core.Text,
    -- | The date and time the Automation operation is scheduled to start.
    scheduledTime :: Core.Maybe Core.POSIX,
    -- | The MaxConcurrency value specified by the user when starting the
    -- Automation.
    maxConcurrency :: Core.Maybe Core.Text,
    -- | The list of execution outputs as defined in the Automation document.
    target :: Core.Maybe Core.Text,
    -- | The status of the execution.
    automationExecutionStatus :: Core.Maybe AutomationExecutionStatus,
    -- | The specified key-value mapping of document parameters to target
    -- resources.
    targetMaps :: Core.Maybe [Core.HashMap Core.Text [Core.Text]],
    -- | Information about the Automation runbooks (Automation documents) that
    -- are run during a runbook workflow in Change Manager.
    --
    -- The Automation runbooks specified for the runbook workflow can\'t run
    -- until all required approvals for the change request have been received.
    runbooks :: Core.Maybe (Core.NonEmpty Runbook),
    -- | The subtype of the Automation operation. Currently, the only supported
    -- value is @ChangeRequest@.
    automationSubtype :: Core.Maybe AutomationSubtype,
    -- | The document version used during the execution.
    documentVersion :: Core.Maybe Core.Text,
    -- | An S3 bucket where execution information is stored.
    logFile :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutomationExecutionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'automationExecutionMetadata_maxErrors' - The MaxErrors value specified by the user when starting the Automation.
--
-- 'currentAction', 'automationExecutionMetadata_currentAction' - The action of the step that is currently running.
--
-- 'parentAutomationExecutionId', 'automationExecutionMetadata_parentAutomationExecutionId' - The ExecutionId of the parent Automation.
--
-- 'outputs', 'automationExecutionMetadata_outputs' - The list of execution outputs as defined in the Automation document.
--
-- 'mode', 'automationExecutionMetadata_mode' - The Automation execution mode.
--
-- 'failureMessage', 'automationExecutionMetadata_failureMessage' - The list of execution outputs as defined in the Automation document.
--
-- 'executionEndTime', 'automationExecutionMetadata_executionEndTime' - The time the execution finished. This is not populated if the execution
-- is still in progress.
--
-- 'documentName', 'automationExecutionMetadata_documentName' - The name of the Automation document used during execution.
--
-- 'automationExecutionId', 'automationExecutionMetadata_automationExecutionId' - The execution ID.
--
-- 'changeRequestName', 'automationExecutionMetadata_changeRequestName' - The name of the Change Manager change request.
--
-- 'executedBy', 'automationExecutionMetadata_executedBy' - The IAM role ARN of the user who ran the Automation.
--
-- 'resolvedTargets', 'automationExecutionMetadata_resolvedTargets' - A list of targets that resolved during the execution.
--
-- 'targets', 'automationExecutionMetadata_targets' - The targets defined by the user when starting the Automation.
--
-- 'automationType', 'automationExecutionMetadata_automationType' - Use this filter with DescribeAutomationExecutions. Specify either Local
-- or CrossAccount. CrossAccount is an Automation that runs in multiple AWS
-- Regions and accounts. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts>
-- in the /AWS Systems Manager User Guide/.
--
-- 'targetParameterName', 'automationExecutionMetadata_targetParameterName' - The list of execution outputs as defined in the Automation document.
--
-- 'executionStartTime', 'automationExecutionMetadata_executionStartTime' - The time the execution started.
--
-- 'currentStepName', 'automationExecutionMetadata_currentStepName' - The name of the step that is currently running.
--
-- 'associationId', 'automationExecutionMetadata_associationId' - The ID of a State Manager association used in the Automation operation.
--
-- 'opsItemId', 'automationExecutionMetadata_opsItemId' - The ID of an OpsItem that is created to represent a Change Manager
-- change request.
--
-- 'scheduledTime', 'automationExecutionMetadata_scheduledTime' - The date and time the Automation operation is scheduled to start.
--
-- 'maxConcurrency', 'automationExecutionMetadata_maxConcurrency' - The MaxConcurrency value specified by the user when starting the
-- Automation.
--
-- 'target', 'automationExecutionMetadata_target' - The list of execution outputs as defined in the Automation document.
--
-- 'automationExecutionStatus', 'automationExecutionMetadata_automationExecutionStatus' - The status of the execution.
--
-- 'targetMaps', 'automationExecutionMetadata_targetMaps' - The specified key-value mapping of document parameters to target
-- resources.
--
-- 'runbooks', 'automationExecutionMetadata_runbooks' - Information about the Automation runbooks (Automation documents) that
-- are run during a runbook workflow in Change Manager.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
--
-- 'automationSubtype', 'automationExecutionMetadata_automationSubtype' - The subtype of the Automation operation. Currently, the only supported
-- value is @ChangeRequest@.
--
-- 'documentVersion', 'automationExecutionMetadata_documentVersion' - The document version used during the execution.
--
-- 'logFile', 'automationExecutionMetadata_logFile' - An S3 bucket where execution information is stored.
newAutomationExecutionMetadata ::
  AutomationExecutionMetadata
newAutomationExecutionMetadata =
  AutomationExecutionMetadata'
    { maxErrors =
        Core.Nothing,
      currentAction = Core.Nothing,
      parentAutomationExecutionId = Core.Nothing,
      outputs = Core.Nothing,
      mode = Core.Nothing,
      failureMessage = Core.Nothing,
      executionEndTime = Core.Nothing,
      documentName = Core.Nothing,
      automationExecutionId = Core.Nothing,
      changeRequestName = Core.Nothing,
      executedBy = Core.Nothing,
      resolvedTargets = Core.Nothing,
      targets = Core.Nothing,
      automationType = Core.Nothing,
      targetParameterName = Core.Nothing,
      executionStartTime = Core.Nothing,
      currentStepName = Core.Nothing,
      associationId = Core.Nothing,
      opsItemId = Core.Nothing,
      scheduledTime = Core.Nothing,
      maxConcurrency = Core.Nothing,
      target = Core.Nothing,
      automationExecutionStatus = Core.Nothing,
      targetMaps = Core.Nothing,
      runbooks = Core.Nothing,
      automationSubtype = Core.Nothing,
      documentVersion = Core.Nothing,
      logFile = Core.Nothing
    }

-- | The MaxErrors value specified by the user when starting the Automation.
automationExecutionMetadata_maxErrors :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_maxErrors = Lens.lens (\AutomationExecutionMetadata' {maxErrors} -> maxErrors) (\s@AutomationExecutionMetadata' {} a -> s {maxErrors = a} :: AutomationExecutionMetadata)

-- | The action of the step that is currently running.
automationExecutionMetadata_currentAction :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_currentAction = Lens.lens (\AutomationExecutionMetadata' {currentAction} -> currentAction) (\s@AutomationExecutionMetadata' {} a -> s {currentAction = a} :: AutomationExecutionMetadata)

-- | The ExecutionId of the parent Automation.
automationExecutionMetadata_parentAutomationExecutionId :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_parentAutomationExecutionId = Lens.lens (\AutomationExecutionMetadata' {parentAutomationExecutionId} -> parentAutomationExecutionId) (\s@AutomationExecutionMetadata' {} a -> s {parentAutomationExecutionId = a} :: AutomationExecutionMetadata)

-- | The list of execution outputs as defined in the Automation document.
automationExecutionMetadata_outputs :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
automationExecutionMetadata_outputs = Lens.lens (\AutomationExecutionMetadata' {outputs} -> outputs) (\s@AutomationExecutionMetadata' {} a -> s {outputs = a} :: AutomationExecutionMetadata) Core.. Lens.mapping Lens._Coerce

-- | The Automation execution mode.
automationExecutionMetadata_mode :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe ExecutionMode)
automationExecutionMetadata_mode = Lens.lens (\AutomationExecutionMetadata' {mode} -> mode) (\s@AutomationExecutionMetadata' {} a -> s {mode = a} :: AutomationExecutionMetadata)

-- | The list of execution outputs as defined in the Automation document.
automationExecutionMetadata_failureMessage :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_failureMessage = Lens.lens (\AutomationExecutionMetadata' {failureMessage} -> failureMessage) (\s@AutomationExecutionMetadata' {} a -> s {failureMessage = a} :: AutomationExecutionMetadata)

-- | The time the execution finished. This is not populated if the execution
-- is still in progress.
automationExecutionMetadata_executionEndTime :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.UTCTime)
automationExecutionMetadata_executionEndTime = Lens.lens (\AutomationExecutionMetadata' {executionEndTime} -> executionEndTime) (\s@AutomationExecutionMetadata' {} a -> s {executionEndTime = a} :: AutomationExecutionMetadata) Core.. Lens.mapping Core._Time

-- | The name of the Automation document used during execution.
automationExecutionMetadata_documentName :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_documentName = Lens.lens (\AutomationExecutionMetadata' {documentName} -> documentName) (\s@AutomationExecutionMetadata' {} a -> s {documentName = a} :: AutomationExecutionMetadata)

-- | The execution ID.
automationExecutionMetadata_automationExecutionId :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_automationExecutionId = Lens.lens (\AutomationExecutionMetadata' {automationExecutionId} -> automationExecutionId) (\s@AutomationExecutionMetadata' {} a -> s {automationExecutionId = a} :: AutomationExecutionMetadata)

-- | The name of the Change Manager change request.
automationExecutionMetadata_changeRequestName :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_changeRequestName = Lens.lens (\AutomationExecutionMetadata' {changeRequestName} -> changeRequestName) (\s@AutomationExecutionMetadata' {} a -> s {changeRequestName = a} :: AutomationExecutionMetadata)

-- | The IAM role ARN of the user who ran the Automation.
automationExecutionMetadata_executedBy :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_executedBy = Lens.lens (\AutomationExecutionMetadata' {executedBy} -> executedBy) (\s@AutomationExecutionMetadata' {} a -> s {executedBy = a} :: AutomationExecutionMetadata)

-- | A list of targets that resolved during the execution.
automationExecutionMetadata_resolvedTargets :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe ResolvedTargets)
automationExecutionMetadata_resolvedTargets = Lens.lens (\AutomationExecutionMetadata' {resolvedTargets} -> resolvedTargets) (\s@AutomationExecutionMetadata' {} a -> s {resolvedTargets = a} :: AutomationExecutionMetadata)

-- | The targets defined by the user when starting the Automation.
automationExecutionMetadata_targets :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe [Target])
automationExecutionMetadata_targets = Lens.lens (\AutomationExecutionMetadata' {targets} -> targets) (\s@AutomationExecutionMetadata' {} a -> s {targets = a} :: AutomationExecutionMetadata) Core.. Lens.mapping Lens._Coerce

-- | Use this filter with DescribeAutomationExecutions. Specify either Local
-- or CrossAccount. CrossAccount is an Automation that runs in multiple AWS
-- Regions and accounts. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts>
-- in the /AWS Systems Manager User Guide/.
automationExecutionMetadata_automationType :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe AutomationType)
automationExecutionMetadata_automationType = Lens.lens (\AutomationExecutionMetadata' {automationType} -> automationType) (\s@AutomationExecutionMetadata' {} a -> s {automationType = a} :: AutomationExecutionMetadata)

-- | The list of execution outputs as defined in the Automation document.
automationExecutionMetadata_targetParameterName :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_targetParameterName = Lens.lens (\AutomationExecutionMetadata' {targetParameterName} -> targetParameterName) (\s@AutomationExecutionMetadata' {} a -> s {targetParameterName = a} :: AutomationExecutionMetadata)

-- | The time the execution started.
automationExecutionMetadata_executionStartTime :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.UTCTime)
automationExecutionMetadata_executionStartTime = Lens.lens (\AutomationExecutionMetadata' {executionStartTime} -> executionStartTime) (\s@AutomationExecutionMetadata' {} a -> s {executionStartTime = a} :: AutomationExecutionMetadata) Core.. Lens.mapping Core._Time

-- | The name of the step that is currently running.
automationExecutionMetadata_currentStepName :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_currentStepName = Lens.lens (\AutomationExecutionMetadata' {currentStepName} -> currentStepName) (\s@AutomationExecutionMetadata' {} a -> s {currentStepName = a} :: AutomationExecutionMetadata)

-- | The ID of a State Manager association used in the Automation operation.
automationExecutionMetadata_associationId :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_associationId = Lens.lens (\AutomationExecutionMetadata' {associationId} -> associationId) (\s@AutomationExecutionMetadata' {} a -> s {associationId = a} :: AutomationExecutionMetadata)

-- | The ID of an OpsItem that is created to represent a Change Manager
-- change request.
automationExecutionMetadata_opsItemId :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_opsItemId = Lens.lens (\AutomationExecutionMetadata' {opsItemId} -> opsItemId) (\s@AutomationExecutionMetadata' {} a -> s {opsItemId = a} :: AutomationExecutionMetadata)

-- | The date and time the Automation operation is scheduled to start.
automationExecutionMetadata_scheduledTime :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.UTCTime)
automationExecutionMetadata_scheduledTime = Lens.lens (\AutomationExecutionMetadata' {scheduledTime} -> scheduledTime) (\s@AutomationExecutionMetadata' {} a -> s {scheduledTime = a} :: AutomationExecutionMetadata) Core.. Lens.mapping Core._Time

-- | The MaxConcurrency value specified by the user when starting the
-- Automation.
automationExecutionMetadata_maxConcurrency :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_maxConcurrency = Lens.lens (\AutomationExecutionMetadata' {maxConcurrency} -> maxConcurrency) (\s@AutomationExecutionMetadata' {} a -> s {maxConcurrency = a} :: AutomationExecutionMetadata)

-- | The list of execution outputs as defined in the Automation document.
automationExecutionMetadata_target :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_target = Lens.lens (\AutomationExecutionMetadata' {target} -> target) (\s@AutomationExecutionMetadata' {} a -> s {target = a} :: AutomationExecutionMetadata)

-- | The status of the execution.
automationExecutionMetadata_automationExecutionStatus :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe AutomationExecutionStatus)
automationExecutionMetadata_automationExecutionStatus = Lens.lens (\AutomationExecutionMetadata' {automationExecutionStatus} -> automationExecutionStatus) (\s@AutomationExecutionMetadata' {} a -> s {automationExecutionStatus = a} :: AutomationExecutionMetadata)

-- | The specified key-value mapping of document parameters to target
-- resources.
automationExecutionMetadata_targetMaps :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe [Core.HashMap Core.Text [Core.Text]])
automationExecutionMetadata_targetMaps = Lens.lens (\AutomationExecutionMetadata' {targetMaps} -> targetMaps) (\s@AutomationExecutionMetadata' {} a -> s {targetMaps = a} :: AutomationExecutionMetadata) Core.. Lens.mapping Lens._Coerce

-- | Information about the Automation runbooks (Automation documents) that
-- are run during a runbook workflow in Change Manager.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
automationExecutionMetadata_runbooks :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe (Core.NonEmpty Runbook))
automationExecutionMetadata_runbooks = Lens.lens (\AutomationExecutionMetadata' {runbooks} -> runbooks) (\s@AutomationExecutionMetadata' {} a -> s {runbooks = a} :: AutomationExecutionMetadata) Core.. Lens.mapping Lens._Coerce

-- | The subtype of the Automation operation. Currently, the only supported
-- value is @ChangeRequest@.
automationExecutionMetadata_automationSubtype :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe AutomationSubtype)
automationExecutionMetadata_automationSubtype = Lens.lens (\AutomationExecutionMetadata' {automationSubtype} -> automationSubtype) (\s@AutomationExecutionMetadata' {} a -> s {automationSubtype = a} :: AutomationExecutionMetadata)

-- | The document version used during the execution.
automationExecutionMetadata_documentVersion :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_documentVersion = Lens.lens (\AutomationExecutionMetadata' {documentVersion} -> documentVersion) (\s@AutomationExecutionMetadata' {} a -> s {documentVersion = a} :: AutomationExecutionMetadata)

-- | An S3 bucket where execution information is stored.
automationExecutionMetadata_logFile :: Lens.Lens' AutomationExecutionMetadata (Core.Maybe Core.Text)
automationExecutionMetadata_logFile = Lens.lens (\AutomationExecutionMetadata' {logFile} -> logFile) (\s@AutomationExecutionMetadata' {} a -> s {logFile = a} :: AutomationExecutionMetadata)

instance Core.FromJSON AutomationExecutionMetadata where
  parseJSON =
    Core.withObject
      "AutomationExecutionMetadata"
      ( \x ->
          AutomationExecutionMetadata'
            Core.<$> (x Core..:? "MaxErrors")
            Core.<*> (x Core..:? "CurrentAction")
            Core.<*> (x Core..:? "ParentAutomationExecutionId")
            Core.<*> (x Core..:? "Outputs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Mode")
            Core.<*> (x Core..:? "FailureMessage")
            Core.<*> (x Core..:? "ExecutionEndTime")
            Core.<*> (x Core..:? "DocumentName")
            Core.<*> (x Core..:? "AutomationExecutionId")
            Core.<*> (x Core..:? "ChangeRequestName")
            Core.<*> (x Core..:? "ExecutedBy")
            Core.<*> (x Core..:? "ResolvedTargets")
            Core.<*> (x Core..:? "Targets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AutomationType")
            Core.<*> (x Core..:? "TargetParameterName")
            Core.<*> (x Core..:? "ExecutionStartTime")
            Core.<*> (x Core..:? "CurrentStepName")
            Core.<*> (x Core..:? "AssociationId")
            Core.<*> (x Core..:? "OpsItemId")
            Core.<*> (x Core..:? "ScheduledTime")
            Core.<*> (x Core..:? "MaxConcurrency")
            Core.<*> (x Core..:? "Target")
            Core.<*> (x Core..:? "AutomationExecutionStatus")
            Core.<*> (x Core..:? "TargetMaps" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Runbooks")
            Core.<*> (x Core..:? "AutomationSubtype")
            Core.<*> (x Core..:? "DocumentVersion")
            Core.<*> (x Core..:? "LogFile")
      )

instance Core.Hashable AutomationExecutionMetadata

instance Core.NFData AutomationExecutionMetadata

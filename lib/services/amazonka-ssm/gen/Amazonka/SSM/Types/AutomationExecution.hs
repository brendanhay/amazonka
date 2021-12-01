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
-- Module      : Amazonka.SSM.Types.AutomationExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AutomationExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AutomationExecutionStatus
import Amazonka.SSM.Types.AutomationSubtype
import Amazonka.SSM.Types.ExecutionMode
import Amazonka.SSM.Types.ProgressCounters
import Amazonka.SSM.Types.ResolvedTargets
import Amazonka.SSM.Types.Runbook
import Amazonka.SSM.Types.StepExecution
import Amazonka.SSM.Types.Target
import Amazonka.SSM.Types.TargetLocation

-- | Detailed information about the current state of an individual Automation
-- execution.
--
-- /See:/ 'newAutomationExecution' smart constructor.
data AutomationExecution = AutomationExecution'
  { -- | The date and time the Automation operation is scheduled to start.
    scheduledTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of a State Manager association used in the Automation operation.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of an OpsItem that is created to represent a Change Manager
    -- change request.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | The name of the step that is currently running.
    currentStepName :: Prelude.Maybe Prelude.Text,
    -- | The parameter name.
    targetParameterName :: Prelude.Maybe Prelude.Text,
    -- | The combination of Amazon Web Services Regions and\/or Amazon Web
    -- Services accounts where you want to run the Automation.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | An aggregate of step execution statuses displayed in the Amazon Web
    -- Services Systems Manager console for a multi-Region and multi-account
    -- Automation execution.
    progressCounters :: Prelude.Maybe ProgressCounters,
    -- | The Amazon Resource Name (ARN) of the user who ran the automation.
    executedBy :: Prelude.Maybe Prelude.Text,
    -- | The name of the Automation runbook used during the execution.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The time the execution finished.
    executionEndTime :: Prelude.Maybe Core.POSIX,
    -- | A message describing why an execution has failed, if the status is set
    -- to Failed.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The subtype of the Automation operation. Currently, the only supported
    -- value is @ChangeRequest@.
    automationSubtype :: Prelude.Maybe AutomationSubtype,
    -- | The automation execution mode.
    mode :: Prelude.Maybe ExecutionMode,
    -- | The specified key-value mapping of document parameters to target
    -- resources.
    targetMaps :: Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]],
    -- | A boolean value that indicates if the response contains the full list of
    -- the Automation step executions. If true, use the
    -- DescribeAutomationStepExecutions API operation to get the full list of
    -- step executions.
    stepExecutionsTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The execution status of the Automation.
    automationExecutionStatus :: Prelude.Maybe AutomationExecutionStatus,
    -- | The AutomationExecutionId of the parent automation.
    parentAutomationExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The list of execution outputs as defined in the Automation runbook.
    outputs :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The MaxErrors value specified by the user when the execution started.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The time the execution started.
    executionStartTime :: Prelude.Maybe Core.POSIX,
    -- | The action of the step that is currently running.
    currentAction :: Prelude.Maybe Prelude.Text,
    -- | The specified targets.
    targets :: Prelude.Maybe [Target],
    -- | A list of resolved targets in the rate control execution.
    resolvedTargets :: Prelude.Maybe ResolvedTargets,
    -- | The key-value map of execution parameters, which were supplied when
    -- calling StartAutomationExecution.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The version of the document to use during execution.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The execution ID.
    automationExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Change Manager change request.
    changeRequestName :: Prelude.Maybe Prelude.Text,
    -- | A list of details about the current state of all steps that comprise an
    -- execution. An Automation runbook contains a list of steps that are run
    -- in order.
    stepExecutions :: Prelude.Maybe [StepExecution],
    -- | Information about the Automation runbooks that are run as part of a
    -- runbook workflow.
    --
    -- The Automation runbooks specified for the runbook workflow can\'t run
    -- until all required approvals for the change request have been received.
    runbooks :: Prelude.Maybe (Prelude.NonEmpty Runbook),
    -- | The @MaxConcurrency@ value specified by the user when the execution
    -- started.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The target of the execution.
    target :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledTime', 'automationExecution_scheduledTime' - The date and time the Automation operation is scheduled to start.
--
-- 'associationId', 'automationExecution_associationId' - The ID of a State Manager association used in the Automation operation.
--
-- 'opsItemId', 'automationExecution_opsItemId' - The ID of an OpsItem that is created to represent a Change Manager
-- change request.
--
-- 'currentStepName', 'automationExecution_currentStepName' - The name of the step that is currently running.
--
-- 'targetParameterName', 'automationExecution_targetParameterName' - The parameter name.
--
-- 'targetLocations', 'automationExecution_targetLocations' - The combination of Amazon Web Services Regions and\/or Amazon Web
-- Services accounts where you want to run the Automation.
--
-- 'progressCounters', 'automationExecution_progressCounters' - An aggregate of step execution statuses displayed in the Amazon Web
-- Services Systems Manager console for a multi-Region and multi-account
-- Automation execution.
--
-- 'executedBy', 'automationExecution_executedBy' - The Amazon Resource Name (ARN) of the user who ran the automation.
--
-- 'documentName', 'automationExecution_documentName' - The name of the Automation runbook used during the execution.
--
-- 'executionEndTime', 'automationExecution_executionEndTime' - The time the execution finished.
--
-- 'failureMessage', 'automationExecution_failureMessage' - A message describing why an execution has failed, if the status is set
-- to Failed.
--
-- 'automationSubtype', 'automationExecution_automationSubtype' - The subtype of the Automation operation. Currently, the only supported
-- value is @ChangeRequest@.
--
-- 'mode', 'automationExecution_mode' - The automation execution mode.
--
-- 'targetMaps', 'automationExecution_targetMaps' - The specified key-value mapping of document parameters to target
-- resources.
--
-- 'stepExecutionsTruncated', 'automationExecution_stepExecutionsTruncated' - A boolean value that indicates if the response contains the full list of
-- the Automation step executions. If true, use the
-- DescribeAutomationStepExecutions API operation to get the full list of
-- step executions.
--
-- 'automationExecutionStatus', 'automationExecution_automationExecutionStatus' - The execution status of the Automation.
--
-- 'parentAutomationExecutionId', 'automationExecution_parentAutomationExecutionId' - The AutomationExecutionId of the parent automation.
--
-- 'outputs', 'automationExecution_outputs' - The list of execution outputs as defined in the Automation runbook.
--
-- 'maxErrors', 'automationExecution_maxErrors' - The MaxErrors value specified by the user when the execution started.
--
-- 'executionStartTime', 'automationExecution_executionStartTime' - The time the execution started.
--
-- 'currentAction', 'automationExecution_currentAction' - The action of the step that is currently running.
--
-- 'targets', 'automationExecution_targets' - The specified targets.
--
-- 'resolvedTargets', 'automationExecution_resolvedTargets' - A list of resolved targets in the rate control execution.
--
-- 'parameters', 'automationExecution_parameters' - The key-value map of execution parameters, which were supplied when
-- calling StartAutomationExecution.
--
-- 'documentVersion', 'automationExecution_documentVersion' - The version of the document to use during execution.
--
-- 'automationExecutionId', 'automationExecution_automationExecutionId' - The execution ID.
--
-- 'changeRequestName', 'automationExecution_changeRequestName' - The name of the Change Manager change request.
--
-- 'stepExecutions', 'automationExecution_stepExecutions' - A list of details about the current state of all steps that comprise an
-- execution. An Automation runbook contains a list of steps that are run
-- in order.
--
-- 'runbooks', 'automationExecution_runbooks' - Information about the Automation runbooks that are run as part of a
-- runbook workflow.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
--
-- 'maxConcurrency', 'automationExecution_maxConcurrency' - The @MaxConcurrency@ value specified by the user when the execution
-- started.
--
-- 'target', 'automationExecution_target' - The target of the execution.
newAutomationExecution ::
  AutomationExecution
newAutomationExecution =
  AutomationExecution'
    { scheduledTime =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      opsItemId = Prelude.Nothing,
      currentStepName = Prelude.Nothing,
      targetParameterName = Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      progressCounters = Prelude.Nothing,
      executedBy = Prelude.Nothing,
      documentName = Prelude.Nothing,
      executionEndTime = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      automationSubtype = Prelude.Nothing,
      mode = Prelude.Nothing,
      targetMaps = Prelude.Nothing,
      stepExecutionsTruncated = Prelude.Nothing,
      automationExecutionStatus = Prelude.Nothing,
      parentAutomationExecutionId = Prelude.Nothing,
      outputs = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      executionStartTime = Prelude.Nothing,
      currentAction = Prelude.Nothing,
      targets = Prelude.Nothing,
      resolvedTargets = Prelude.Nothing,
      parameters = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      automationExecutionId = Prelude.Nothing,
      changeRequestName = Prelude.Nothing,
      stepExecutions = Prelude.Nothing,
      runbooks = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | The date and time the Automation operation is scheduled to start.
automationExecution_scheduledTime :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.UTCTime)
automationExecution_scheduledTime = Lens.lens (\AutomationExecution' {scheduledTime} -> scheduledTime) (\s@AutomationExecution' {} a -> s {scheduledTime = a} :: AutomationExecution) Prelude.. Lens.mapping Core._Time

-- | The ID of a State Manager association used in the Automation operation.
automationExecution_associationId :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_associationId = Lens.lens (\AutomationExecution' {associationId} -> associationId) (\s@AutomationExecution' {} a -> s {associationId = a} :: AutomationExecution)

-- | The ID of an OpsItem that is created to represent a Change Manager
-- change request.
automationExecution_opsItemId :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_opsItemId = Lens.lens (\AutomationExecution' {opsItemId} -> opsItemId) (\s@AutomationExecution' {} a -> s {opsItemId = a} :: AutomationExecution)

-- | The name of the step that is currently running.
automationExecution_currentStepName :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_currentStepName = Lens.lens (\AutomationExecution' {currentStepName} -> currentStepName) (\s@AutomationExecution' {} a -> s {currentStepName = a} :: AutomationExecution)

-- | The parameter name.
automationExecution_targetParameterName :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_targetParameterName = Lens.lens (\AutomationExecution' {targetParameterName} -> targetParameterName) (\s@AutomationExecution' {} a -> s {targetParameterName = a} :: AutomationExecution)

-- | The combination of Amazon Web Services Regions and\/or Amazon Web
-- Services accounts where you want to run the Automation.
automationExecution_targetLocations :: Lens.Lens' AutomationExecution (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
automationExecution_targetLocations = Lens.lens (\AutomationExecution' {targetLocations} -> targetLocations) (\s@AutomationExecution' {} a -> s {targetLocations = a} :: AutomationExecution) Prelude.. Lens.mapping Lens.coerced

-- | An aggregate of step execution statuses displayed in the Amazon Web
-- Services Systems Manager console for a multi-Region and multi-account
-- Automation execution.
automationExecution_progressCounters :: Lens.Lens' AutomationExecution (Prelude.Maybe ProgressCounters)
automationExecution_progressCounters = Lens.lens (\AutomationExecution' {progressCounters} -> progressCounters) (\s@AutomationExecution' {} a -> s {progressCounters = a} :: AutomationExecution)

-- | The Amazon Resource Name (ARN) of the user who ran the automation.
automationExecution_executedBy :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_executedBy = Lens.lens (\AutomationExecution' {executedBy} -> executedBy) (\s@AutomationExecution' {} a -> s {executedBy = a} :: AutomationExecution)

-- | The name of the Automation runbook used during the execution.
automationExecution_documentName :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_documentName = Lens.lens (\AutomationExecution' {documentName} -> documentName) (\s@AutomationExecution' {} a -> s {documentName = a} :: AutomationExecution)

-- | The time the execution finished.
automationExecution_executionEndTime :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.UTCTime)
automationExecution_executionEndTime = Lens.lens (\AutomationExecution' {executionEndTime} -> executionEndTime) (\s@AutomationExecution' {} a -> s {executionEndTime = a} :: AutomationExecution) Prelude.. Lens.mapping Core._Time

-- | A message describing why an execution has failed, if the status is set
-- to Failed.
automationExecution_failureMessage :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_failureMessage = Lens.lens (\AutomationExecution' {failureMessage} -> failureMessage) (\s@AutomationExecution' {} a -> s {failureMessage = a} :: AutomationExecution)

-- | The subtype of the Automation operation. Currently, the only supported
-- value is @ChangeRequest@.
automationExecution_automationSubtype :: Lens.Lens' AutomationExecution (Prelude.Maybe AutomationSubtype)
automationExecution_automationSubtype = Lens.lens (\AutomationExecution' {automationSubtype} -> automationSubtype) (\s@AutomationExecution' {} a -> s {automationSubtype = a} :: AutomationExecution)

-- | The automation execution mode.
automationExecution_mode :: Lens.Lens' AutomationExecution (Prelude.Maybe ExecutionMode)
automationExecution_mode = Lens.lens (\AutomationExecution' {mode} -> mode) (\s@AutomationExecution' {} a -> s {mode = a} :: AutomationExecution)

-- | The specified key-value mapping of document parameters to target
-- resources.
automationExecution_targetMaps :: Lens.Lens' AutomationExecution (Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]])
automationExecution_targetMaps = Lens.lens (\AutomationExecution' {targetMaps} -> targetMaps) (\s@AutomationExecution' {} a -> s {targetMaps = a} :: AutomationExecution) Prelude.. Lens.mapping Lens.coerced

-- | A boolean value that indicates if the response contains the full list of
-- the Automation step executions. If true, use the
-- DescribeAutomationStepExecutions API operation to get the full list of
-- step executions.
automationExecution_stepExecutionsTruncated :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Bool)
automationExecution_stepExecutionsTruncated = Lens.lens (\AutomationExecution' {stepExecutionsTruncated} -> stepExecutionsTruncated) (\s@AutomationExecution' {} a -> s {stepExecutionsTruncated = a} :: AutomationExecution)

-- | The execution status of the Automation.
automationExecution_automationExecutionStatus :: Lens.Lens' AutomationExecution (Prelude.Maybe AutomationExecutionStatus)
automationExecution_automationExecutionStatus = Lens.lens (\AutomationExecution' {automationExecutionStatus} -> automationExecutionStatus) (\s@AutomationExecution' {} a -> s {automationExecutionStatus = a} :: AutomationExecution)

-- | The AutomationExecutionId of the parent automation.
automationExecution_parentAutomationExecutionId :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_parentAutomationExecutionId = Lens.lens (\AutomationExecution' {parentAutomationExecutionId} -> parentAutomationExecutionId) (\s@AutomationExecution' {} a -> s {parentAutomationExecutionId = a} :: AutomationExecution)

-- | The list of execution outputs as defined in the Automation runbook.
automationExecution_outputs :: Lens.Lens' AutomationExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
automationExecution_outputs = Lens.lens (\AutomationExecution' {outputs} -> outputs) (\s@AutomationExecution' {} a -> s {outputs = a} :: AutomationExecution) Prelude.. Lens.mapping Lens.coerced

-- | The MaxErrors value specified by the user when the execution started.
automationExecution_maxErrors :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_maxErrors = Lens.lens (\AutomationExecution' {maxErrors} -> maxErrors) (\s@AutomationExecution' {} a -> s {maxErrors = a} :: AutomationExecution)

-- | The time the execution started.
automationExecution_executionStartTime :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.UTCTime)
automationExecution_executionStartTime = Lens.lens (\AutomationExecution' {executionStartTime} -> executionStartTime) (\s@AutomationExecution' {} a -> s {executionStartTime = a} :: AutomationExecution) Prelude.. Lens.mapping Core._Time

-- | The action of the step that is currently running.
automationExecution_currentAction :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_currentAction = Lens.lens (\AutomationExecution' {currentAction} -> currentAction) (\s@AutomationExecution' {} a -> s {currentAction = a} :: AutomationExecution)

-- | The specified targets.
automationExecution_targets :: Lens.Lens' AutomationExecution (Prelude.Maybe [Target])
automationExecution_targets = Lens.lens (\AutomationExecution' {targets} -> targets) (\s@AutomationExecution' {} a -> s {targets = a} :: AutomationExecution) Prelude.. Lens.mapping Lens.coerced

-- | A list of resolved targets in the rate control execution.
automationExecution_resolvedTargets :: Lens.Lens' AutomationExecution (Prelude.Maybe ResolvedTargets)
automationExecution_resolvedTargets = Lens.lens (\AutomationExecution' {resolvedTargets} -> resolvedTargets) (\s@AutomationExecution' {} a -> s {resolvedTargets = a} :: AutomationExecution)

-- | The key-value map of execution parameters, which were supplied when
-- calling StartAutomationExecution.
automationExecution_parameters :: Lens.Lens' AutomationExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
automationExecution_parameters = Lens.lens (\AutomationExecution' {parameters} -> parameters) (\s@AutomationExecution' {} a -> s {parameters = a} :: AutomationExecution) Prelude.. Lens.mapping Lens.coerced

-- | The version of the document to use during execution.
automationExecution_documentVersion :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_documentVersion = Lens.lens (\AutomationExecution' {documentVersion} -> documentVersion) (\s@AutomationExecution' {} a -> s {documentVersion = a} :: AutomationExecution)

-- | The execution ID.
automationExecution_automationExecutionId :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_automationExecutionId = Lens.lens (\AutomationExecution' {automationExecutionId} -> automationExecutionId) (\s@AutomationExecution' {} a -> s {automationExecutionId = a} :: AutomationExecution)

-- | The name of the Change Manager change request.
automationExecution_changeRequestName :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_changeRequestName = Lens.lens (\AutomationExecution' {changeRequestName} -> changeRequestName) (\s@AutomationExecution' {} a -> s {changeRequestName = a} :: AutomationExecution)

-- | A list of details about the current state of all steps that comprise an
-- execution. An Automation runbook contains a list of steps that are run
-- in order.
automationExecution_stepExecutions :: Lens.Lens' AutomationExecution (Prelude.Maybe [StepExecution])
automationExecution_stepExecutions = Lens.lens (\AutomationExecution' {stepExecutions} -> stepExecutions) (\s@AutomationExecution' {} a -> s {stepExecutions = a} :: AutomationExecution) Prelude.. Lens.mapping Lens.coerced

-- | Information about the Automation runbooks that are run as part of a
-- runbook workflow.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
automationExecution_runbooks :: Lens.Lens' AutomationExecution (Prelude.Maybe (Prelude.NonEmpty Runbook))
automationExecution_runbooks = Lens.lens (\AutomationExecution' {runbooks} -> runbooks) (\s@AutomationExecution' {} a -> s {runbooks = a} :: AutomationExecution) Prelude.. Lens.mapping Lens.coerced

-- | The @MaxConcurrency@ value specified by the user when the execution
-- started.
automationExecution_maxConcurrency :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_maxConcurrency = Lens.lens (\AutomationExecution' {maxConcurrency} -> maxConcurrency) (\s@AutomationExecution' {} a -> s {maxConcurrency = a} :: AutomationExecution)

-- | The target of the execution.
automationExecution_target :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_target = Lens.lens (\AutomationExecution' {target} -> target) (\s@AutomationExecution' {} a -> s {target = a} :: AutomationExecution)

instance Core.FromJSON AutomationExecution where
  parseJSON =
    Core.withObject
      "AutomationExecution"
      ( \x ->
          AutomationExecution'
            Prelude.<$> (x Core..:? "ScheduledTime")
            Prelude.<*> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "OpsItemId")
            Prelude.<*> (x Core..:? "CurrentStepName")
            Prelude.<*> (x Core..:? "TargetParameterName")
            Prelude.<*> (x Core..:? "TargetLocations")
            Prelude.<*> (x Core..:? "ProgressCounters")
            Prelude.<*> (x Core..:? "ExecutedBy")
            Prelude.<*> (x Core..:? "DocumentName")
            Prelude.<*> (x Core..:? "ExecutionEndTime")
            Prelude.<*> (x Core..:? "FailureMessage")
            Prelude.<*> (x Core..:? "AutomationSubtype")
            Prelude.<*> (x Core..:? "Mode")
            Prelude.<*> (x Core..:? "TargetMaps" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "StepExecutionsTruncated")
            Prelude.<*> (x Core..:? "AutomationExecutionStatus")
            Prelude.<*> (x Core..:? "ParentAutomationExecutionId")
            Prelude.<*> (x Core..:? "Outputs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MaxErrors")
            Prelude.<*> (x Core..:? "ExecutionStartTime")
            Prelude.<*> (x Core..:? "CurrentAction")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ResolvedTargets")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "AutomationExecutionId")
            Prelude.<*> (x Core..:? "ChangeRequestName")
            Prelude.<*> (x Core..:? "StepExecutions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Runbooks")
            Prelude.<*> (x Core..:? "MaxConcurrency")
            Prelude.<*> (x Core..:? "Target")
      )

instance Prelude.Hashable AutomationExecution where
  hashWithSalt salt' AutomationExecution' {..} =
    salt' `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` runbooks
      `Prelude.hashWithSalt` stepExecutions
      `Prelude.hashWithSalt` changeRequestName
      `Prelude.hashWithSalt` automationExecutionId
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` resolvedTargets
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` currentAction
      `Prelude.hashWithSalt` executionStartTime
      `Prelude.hashWithSalt` maxErrors
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` parentAutomationExecutionId
      `Prelude.hashWithSalt` automationExecutionStatus
      `Prelude.hashWithSalt` stepExecutionsTruncated
      `Prelude.hashWithSalt` targetMaps
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` automationSubtype
      `Prelude.hashWithSalt` failureMessage
      `Prelude.hashWithSalt` executionEndTime
      `Prelude.hashWithSalt` documentName
      `Prelude.hashWithSalt` executedBy
      `Prelude.hashWithSalt` progressCounters
      `Prelude.hashWithSalt` targetLocations
      `Prelude.hashWithSalt` targetParameterName
      `Prelude.hashWithSalt` currentStepName
      `Prelude.hashWithSalt` opsItemId
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` scheduledTime

instance Prelude.NFData AutomationExecution where
  rnf AutomationExecution' {..} =
    Prelude.rnf scheduledTime
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf runbooks
      `Prelude.seq` Prelude.rnf stepExecutions
      `Prelude.seq` Prelude.rnf changeRequestName
      `Prelude.seq` Prelude.rnf automationExecutionId
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf resolvedTargets
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf currentAction
      `Prelude.seq` Prelude.rnf executionStartTime
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf parentAutomationExecutionId
      `Prelude.seq` Prelude.rnf automationExecutionStatus
      `Prelude.seq` Prelude.rnf stepExecutionsTruncated
      `Prelude.seq` Prelude.rnf targetMaps
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf automationSubtype
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf executionEndTime
      `Prelude.seq` Prelude.rnf documentName
      `Prelude.seq` Prelude.rnf executedBy
      `Prelude.seq` Prelude.rnf progressCounters
      `Prelude.seq` Prelude.rnf targetLocations
      `Prelude.seq` Prelude.rnf targetParameterName
      `Prelude.seq` Prelude.rnf currentStepName
      `Prelude.seq` Prelude.rnf opsItemId
      `Prelude.seq` Prelude.rnf associationId

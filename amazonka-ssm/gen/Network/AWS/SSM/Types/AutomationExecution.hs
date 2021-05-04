{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.Types.AutomationExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecution where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.AutomationSubtype
import Network.AWS.SSM.Types.ExecutionMode
import Network.AWS.SSM.Types.ProgressCounters
import Network.AWS.SSM.Types.ResolvedTargets
import Network.AWS.SSM.Types.Runbook
import Network.AWS.SSM.Types.StepExecution
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Detailed information about the current state of an individual Automation
-- execution.
--
-- /See:/ 'newAutomationExecution' smart constructor.
data AutomationExecution = AutomationExecution'
  { -- | The MaxErrors value specified by the user when the execution started.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The action of the step that is currently running.
    currentAction :: Prelude.Maybe Prelude.Text,
    -- | The AutomationExecutionId of the parent automation.
    parentAutomationExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The list of execution outputs as defined in the automation document.
    outputs :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The automation execution mode.
    mode :: Prelude.Maybe ExecutionMode,
    -- | A message describing why an execution has failed, if the status is set
    -- to Failed.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The time the execution finished.
    executionEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the Automation document used during the execution.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The execution ID.
    automationExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Change Manager change request.
    changeRequestName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user who ran the automation.
    executedBy :: Prelude.Maybe Prelude.Text,
    -- | An aggregate of step execution statuses displayed in the AWS Console for
    -- a multi-Region and multi-account Automation execution.
    progressCounters :: Prelude.Maybe ProgressCounters,
    -- | A list of resolved targets in the rate control execution.
    resolvedTargets :: Prelude.Maybe ResolvedTargets,
    -- | The specified targets.
    targets :: Prelude.Maybe [Target],
    -- | The combination of AWS Regions and\/or AWS accounts where you want to
    -- run the Automation.
    targetLocations :: Prelude.Maybe (Prelude.NonEmpty TargetLocation),
    -- | The parameter name.
    targetParameterName :: Prelude.Maybe Prelude.Text,
    -- | The time the execution started.
    executionStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the step that is currently running.
    currentStepName :: Prelude.Maybe Prelude.Text,
    -- | The ID of a State Manager association used in the Automation operation.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of an OpsItem that is created to represent a Change Manager
    -- change request.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | The date and time the Automation operation is scheduled to start.
    scheduledTime :: Prelude.Maybe Prelude.POSIX,
    -- | The MaxConcurrency value specified by the user when the execution
    -- started.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | A boolean value that indicates if the response contains the full list of
    -- the Automation step executions. If true, use the
    -- DescribeAutomationStepExecutions API action to get the full list of step
    -- executions.
    stepExecutionsTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The target of the execution.
    target :: Prelude.Maybe Prelude.Text,
    -- | The execution status of the Automation.
    automationExecutionStatus :: Prelude.Maybe AutomationExecutionStatus,
    -- | The specified key-value mapping of document parameters to target
    -- resources.
    targetMaps :: Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]],
    -- | Information about the Automation runbooks (Automation documents) that
    -- are run as part of a runbook workflow.
    --
    -- The Automation runbooks specified for the runbook workflow can\'t run
    -- until all required approvals for the change request have been received.
    runbooks :: Prelude.Maybe (Prelude.NonEmpty Runbook),
    -- | A list of details about the current state of all steps that comprise an
    -- execution. An Automation document contains a list of steps that are run
    -- in order.
    stepExecutions :: Prelude.Maybe [StepExecution],
    -- | The subtype of the Automation operation. Currently, the only supported
    -- value is @ChangeRequest@.
    automationSubtype :: Prelude.Maybe AutomationSubtype,
    -- | The version of the document to use during execution.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The key-value map of execution parameters, which were supplied when
    -- calling StartAutomationExecution.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text])
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutomationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxErrors', 'automationExecution_maxErrors' - The MaxErrors value specified by the user when the execution started.
--
-- 'currentAction', 'automationExecution_currentAction' - The action of the step that is currently running.
--
-- 'parentAutomationExecutionId', 'automationExecution_parentAutomationExecutionId' - The AutomationExecutionId of the parent automation.
--
-- 'outputs', 'automationExecution_outputs' - The list of execution outputs as defined in the automation document.
--
-- 'mode', 'automationExecution_mode' - The automation execution mode.
--
-- 'failureMessage', 'automationExecution_failureMessage' - A message describing why an execution has failed, if the status is set
-- to Failed.
--
-- 'executionEndTime', 'automationExecution_executionEndTime' - The time the execution finished.
--
-- 'documentName', 'automationExecution_documentName' - The name of the Automation document used during the execution.
--
-- 'automationExecutionId', 'automationExecution_automationExecutionId' - The execution ID.
--
-- 'changeRequestName', 'automationExecution_changeRequestName' - The name of the Change Manager change request.
--
-- 'executedBy', 'automationExecution_executedBy' - The Amazon Resource Name (ARN) of the user who ran the automation.
--
-- 'progressCounters', 'automationExecution_progressCounters' - An aggregate of step execution statuses displayed in the AWS Console for
-- a multi-Region and multi-account Automation execution.
--
-- 'resolvedTargets', 'automationExecution_resolvedTargets' - A list of resolved targets in the rate control execution.
--
-- 'targets', 'automationExecution_targets' - The specified targets.
--
-- 'targetLocations', 'automationExecution_targetLocations' - The combination of AWS Regions and\/or AWS accounts where you want to
-- run the Automation.
--
-- 'targetParameterName', 'automationExecution_targetParameterName' - The parameter name.
--
-- 'executionStartTime', 'automationExecution_executionStartTime' - The time the execution started.
--
-- 'currentStepName', 'automationExecution_currentStepName' - The name of the step that is currently running.
--
-- 'associationId', 'automationExecution_associationId' - The ID of a State Manager association used in the Automation operation.
--
-- 'opsItemId', 'automationExecution_opsItemId' - The ID of an OpsItem that is created to represent a Change Manager
-- change request.
--
-- 'scheduledTime', 'automationExecution_scheduledTime' - The date and time the Automation operation is scheduled to start.
--
-- 'maxConcurrency', 'automationExecution_maxConcurrency' - The MaxConcurrency value specified by the user when the execution
-- started.
--
-- 'stepExecutionsTruncated', 'automationExecution_stepExecutionsTruncated' - A boolean value that indicates if the response contains the full list of
-- the Automation step executions. If true, use the
-- DescribeAutomationStepExecutions API action to get the full list of step
-- executions.
--
-- 'target', 'automationExecution_target' - The target of the execution.
--
-- 'automationExecutionStatus', 'automationExecution_automationExecutionStatus' - The execution status of the Automation.
--
-- 'targetMaps', 'automationExecution_targetMaps' - The specified key-value mapping of document parameters to target
-- resources.
--
-- 'runbooks', 'automationExecution_runbooks' - Information about the Automation runbooks (Automation documents) that
-- are run as part of a runbook workflow.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
--
-- 'stepExecutions', 'automationExecution_stepExecutions' - A list of details about the current state of all steps that comprise an
-- execution. An Automation document contains a list of steps that are run
-- in order.
--
-- 'automationSubtype', 'automationExecution_automationSubtype' - The subtype of the Automation operation. Currently, the only supported
-- value is @ChangeRequest@.
--
-- 'documentVersion', 'automationExecution_documentVersion' - The version of the document to use during execution.
--
-- 'parameters', 'automationExecution_parameters' - The key-value map of execution parameters, which were supplied when
-- calling StartAutomationExecution.
newAutomationExecution ::
  AutomationExecution
newAutomationExecution =
  AutomationExecution'
    { maxErrors = Prelude.Nothing,
      currentAction = Prelude.Nothing,
      parentAutomationExecutionId = Prelude.Nothing,
      outputs = Prelude.Nothing,
      mode = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      executionEndTime = Prelude.Nothing,
      documentName = Prelude.Nothing,
      automationExecutionId = Prelude.Nothing,
      changeRequestName = Prelude.Nothing,
      executedBy = Prelude.Nothing,
      progressCounters = Prelude.Nothing,
      resolvedTargets = Prelude.Nothing,
      targets = Prelude.Nothing,
      targetLocations = Prelude.Nothing,
      targetParameterName = Prelude.Nothing,
      executionStartTime = Prelude.Nothing,
      currentStepName = Prelude.Nothing,
      associationId = Prelude.Nothing,
      opsItemId = Prelude.Nothing,
      scheduledTime = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      stepExecutionsTruncated = Prelude.Nothing,
      target = Prelude.Nothing,
      automationExecutionStatus = Prelude.Nothing,
      targetMaps = Prelude.Nothing,
      runbooks = Prelude.Nothing,
      stepExecutions = Prelude.Nothing,
      automationSubtype = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | The MaxErrors value specified by the user when the execution started.
automationExecution_maxErrors :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_maxErrors = Lens.lens (\AutomationExecution' {maxErrors} -> maxErrors) (\s@AutomationExecution' {} a -> s {maxErrors = a} :: AutomationExecution)

-- | The action of the step that is currently running.
automationExecution_currentAction :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_currentAction = Lens.lens (\AutomationExecution' {currentAction} -> currentAction) (\s@AutomationExecution' {} a -> s {currentAction = a} :: AutomationExecution)

-- | The AutomationExecutionId of the parent automation.
automationExecution_parentAutomationExecutionId :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_parentAutomationExecutionId = Lens.lens (\AutomationExecution' {parentAutomationExecutionId} -> parentAutomationExecutionId) (\s@AutomationExecution' {} a -> s {parentAutomationExecutionId = a} :: AutomationExecution)

-- | The list of execution outputs as defined in the automation document.
automationExecution_outputs :: Lens.Lens' AutomationExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
automationExecution_outputs = Lens.lens (\AutomationExecution' {outputs} -> outputs) (\s@AutomationExecution' {} a -> s {outputs = a} :: AutomationExecution) Prelude.. Lens.mapping Prelude._Coerce

-- | The automation execution mode.
automationExecution_mode :: Lens.Lens' AutomationExecution (Prelude.Maybe ExecutionMode)
automationExecution_mode = Lens.lens (\AutomationExecution' {mode} -> mode) (\s@AutomationExecution' {} a -> s {mode = a} :: AutomationExecution)

-- | A message describing why an execution has failed, if the status is set
-- to Failed.
automationExecution_failureMessage :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_failureMessage = Lens.lens (\AutomationExecution' {failureMessage} -> failureMessage) (\s@AutomationExecution' {} a -> s {failureMessage = a} :: AutomationExecution)

-- | The time the execution finished.
automationExecution_executionEndTime :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.UTCTime)
automationExecution_executionEndTime = Lens.lens (\AutomationExecution' {executionEndTime} -> executionEndTime) (\s@AutomationExecution' {} a -> s {executionEndTime = a} :: AutomationExecution) Prelude.. Lens.mapping Prelude._Time

-- | The name of the Automation document used during the execution.
automationExecution_documentName :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_documentName = Lens.lens (\AutomationExecution' {documentName} -> documentName) (\s@AutomationExecution' {} a -> s {documentName = a} :: AutomationExecution)

-- | The execution ID.
automationExecution_automationExecutionId :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_automationExecutionId = Lens.lens (\AutomationExecution' {automationExecutionId} -> automationExecutionId) (\s@AutomationExecution' {} a -> s {automationExecutionId = a} :: AutomationExecution)

-- | The name of the Change Manager change request.
automationExecution_changeRequestName :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_changeRequestName = Lens.lens (\AutomationExecution' {changeRequestName} -> changeRequestName) (\s@AutomationExecution' {} a -> s {changeRequestName = a} :: AutomationExecution)

-- | The Amazon Resource Name (ARN) of the user who ran the automation.
automationExecution_executedBy :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_executedBy = Lens.lens (\AutomationExecution' {executedBy} -> executedBy) (\s@AutomationExecution' {} a -> s {executedBy = a} :: AutomationExecution)

-- | An aggregate of step execution statuses displayed in the AWS Console for
-- a multi-Region and multi-account Automation execution.
automationExecution_progressCounters :: Lens.Lens' AutomationExecution (Prelude.Maybe ProgressCounters)
automationExecution_progressCounters = Lens.lens (\AutomationExecution' {progressCounters} -> progressCounters) (\s@AutomationExecution' {} a -> s {progressCounters = a} :: AutomationExecution)

-- | A list of resolved targets in the rate control execution.
automationExecution_resolvedTargets :: Lens.Lens' AutomationExecution (Prelude.Maybe ResolvedTargets)
automationExecution_resolvedTargets = Lens.lens (\AutomationExecution' {resolvedTargets} -> resolvedTargets) (\s@AutomationExecution' {} a -> s {resolvedTargets = a} :: AutomationExecution)

-- | The specified targets.
automationExecution_targets :: Lens.Lens' AutomationExecution (Prelude.Maybe [Target])
automationExecution_targets = Lens.lens (\AutomationExecution' {targets} -> targets) (\s@AutomationExecution' {} a -> s {targets = a} :: AutomationExecution) Prelude.. Lens.mapping Prelude._Coerce

-- | The combination of AWS Regions and\/or AWS accounts where you want to
-- run the Automation.
automationExecution_targetLocations :: Lens.Lens' AutomationExecution (Prelude.Maybe (Prelude.NonEmpty TargetLocation))
automationExecution_targetLocations = Lens.lens (\AutomationExecution' {targetLocations} -> targetLocations) (\s@AutomationExecution' {} a -> s {targetLocations = a} :: AutomationExecution) Prelude.. Lens.mapping Prelude._Coerce

-- | The parameter name.
automationExecution_targetParameterName :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_targetParameterName = Lens.lens (\AutomationExecution' {targetParameterName} -> targetParameterName) (\s@AutomationExecution' {} a -> s {targetParameterName = a} :: AutomationExecution)

-- | The time the execution started.
automationExecution_executionStartTime :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.UTCTime)
automationExecution_executionStartTime = Lens.lens (\AutomationExecution' {executionStartTime} -> executionStartTime) (\s@AutomationExecution' {} a -> s {executionStartTime = a} :: AutomationExecution) Prelude.. Lens.mapping Prelude._Time

-- | The name of the step that is currently running.
automationExecution_currentStepName :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_currentStepName = Lens.lens (\AutomationExecution' {currentStepName} -> currentStepName) (\s@AutomationExecution' {} a -> s {currentStepName = a} :: AutomationExecution)

-- | The ID of a State Manager association used in the Automation operation.
automationExecution_associationId :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_associationId = Lens.lens (\AutomationExecution' {associationId} -> associationId) (\s@AutomationExecution' {} a -> s {associationId = a} :: AutomationExecution)

-- | The ID of an OpsItem that is created to represent a Change Manager
-- change request.
automationExecution_opsItemId :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_opsItemId = Lens.lens (\AutomationExecution' {opsItemId} -> opsItemId) (\s@AutomationExecution' {} a -> s {opsItemId = a} :: AutomationExecution)

-- | The date and time the Automation operation is scheduled to start.
automationExecution_scheduledTime :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.UTCTime)
automationExecution_scheduledTime = Lens.lens (\AutomationExecution' {scheduledTime} -> scheduledTime) (\s@AutomationExecution' {} a -> s {scheduledTime = a} :: AutomationExecution) Prelude.. Lens.mapping Prelude._Time

-- | The MaxConcurrency value specified by the user when the execution
-- started.
automationExecution_maxConcurrency :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_maxConcurrency = Lens.lens (\AutomationExecution' {maxConcurrency} -> maxConcurrency) (\s@AutomationExecution' {} a -> s {maxConcurrency = a} :: AutomationExecution)

-- | A boolean value that indicates if the response contains the full list of
-- the Automation step executions. If true, use the
-- DescribeAutomationStepExecutions API action to get the full list of step
-- executions.
automationExecution_stepExecutionsTruncated :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Bool)
automationExecution_stepExecutionsTruncated = Lens.lens (\AutomationExecution' {stepExecutionsTruncated} -> stepExecutionsTruncated) (\s@AutomationExecution' {} a -> s {stepExecutionsTruncated = a} :: AutomationExecution)

-- | The target of the execution.
automationExecution_target :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_target = Lens.lens (\AutomationExecution' {target} -> target) (\s@AutomationExecution' {} a -> s {target = a} :: AutomationExecution)

-- | The execution status of the Automation.
automationExecution_automationExecutionStatus :: Lens.Lens' AutomationExecution (Prelude.Maybe AutomationExecutionStatus)
automationExecution_automationExecutionStatus = Lens.lens (\AutomationExecution' {automationExecutionStatus} -> automationExecutionStatus) (\s@AutomationExecution' {} a -> s {automationExecutionStatus = a} :: AutomationExecution)

-- | The specified key-value mapping of document parameters to target
-- resources.
automationExecution_targetMaps :: Lens.Lens' AutomationExecution (Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]])
automationExecution_targetMaps = Lens.lens (\AutomationExecution' {targetMaps} -> targetMaps) (\s@AutomationExecution' {} a -> s {targetMaps = a} :: AutomationExecution) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the Automation runbooks (Automation documents) that
-- are run as part of a runbook workflow.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
automationExecution_runbooks :: Lens.Lens' AutomationExecution (Prelude.Maybe (Prelude.NonEmpty Runbook))
automationExecution_runbooks = Lens.lens (\AutomationExecution' {runbooks} -> runbooks) (\s@AutomationExecution' {} a -> s {runbooks = a} :: AutomationExecution) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of details about the current state of all steps that comprise an
-- execution. An Automation document contains a list of steps that are run
-- in order.
automationExecution_stepExecutions :: Lens.Lens' AutomationExecution (Prelude.Maybe [StepExecution])
automationExecution_stepExecutions = Lens.lens (\AutomationExecution' {stepExecutions} -> stepExecutions) (\s@AutomationExecution' {} a -> s {stepExecutions = a} :: AutomationExecution) Prelude.. Lens.mapping Prelude._Coerce

-- | The subtype of the Automation operation. Currently, the only supported
-- value is @ChangeRequest@.
automationExecution_automationSubtype :: Lens.Lens' AutomationExecution (Prelude.Maybe AutomationSubtype)
automationExecution_automationSubtype = Lens.lens (\AutomationExecution' {automationSubtype} -> automationSubtype) (\s@AutomationExecution' {} a -> s {automationSubtype = a} :: AutomationExecution)

-- | The version of the document to use during execution.
automationExecution_documentVersion :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_documentVersion = Lens.lens (\AutomationExecution' {documentVersion} -> documentVersion) (\s@AutomationExecution' {} a -> s {documentVersion = a} :: AutomationExecution)

-- | The key-value map of execution parameters, which were supplied when
-- calling StartAutomationExecution.
automationExecution_parameters :: Lens.Lens' AutomationExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
automationExecution_parameters = Lens.lens (\AutomationExecution' {parameters} -> parameters) (\s@AutomationExecution' {} a -> s {parameters = a} :: AutomationExecution) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON AutomationExecution where
  parseJSON =
    Prelude.withObject
      "AutomationExecution"
      ( \x ->
          AutomationExecution'
            Prelude.<$> (x Prelude..:? "MaxErrors")
            Prelude.<*> (x Prelude..:? "CurrentAction")
            Prelude.<*> (x Prelude..:? "ParentAutomationExecutionId")
            Prelude.<*> (x Prelude..:? "Outputs" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Mode")
            Prelude.<*> (x Prelude..:? "FailureMessage")
            Prelude.<*> (x Prelude..:? "ExecutionEndTime")
            Prelude.<*> (x Prelude..:? "DocumentName")
            Prelude.<*> (x Prelude..:? "AutomationExecutionId")
            Prelude.<*> (x Prelude..:? "ChangeRequestName")
            Prelude.<*> (x Prelude..:? "ExecutedBy")
            Prelude.<*> (x Prelude..:? "ProgressCounters")
            Prelude.<*> (x Prelude..:? "ResolvedTargets")
            Prelude.<*> (x Prelude..:? "Targets" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "TargetLocations")
            Prelude.<*> (x Prelude..:? "TargetParameterName")
            Prelude.<*> (x Prelude..:? "ExecutionStartTime")
            Prelude.<*> (x Prelude..:? "CurrentStepName")
            Prelude.<*> (x Prelude..:? "AssociationId")
            Prelude.<*> (x Prelude..:? "OpsItemId")
            Prelude.<*> (x Prelude..:? "ScheduledTime")
            Prelude.<*> (x Prelude..:? "MaxConcurrency")
            Prelude.<*> (x Prelude..:? "StepExecutionsTruncated")
            Prelude.<*> (x Prelude..:? "Target")
            Prelude.<*> (x Prelude..:? "AutomationExecutionStatus")
            Prelude.<*> ( x Prelude..:? "TargetMaps"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Runbooks")
            Prelude.<*> ( x Prelude..:? "StepExecutions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "AutomationSubtype")
            Prelude.<*> (x Prelude..:? "DocumentVersion")
            Prelude.<*> ( x Prelude..:? "Parameters"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AutomationExecution

instance Prelude.NFData AutomationExecution

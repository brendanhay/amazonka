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
-- Module      : Amazonka.SSM.Types.AutomationExecutionMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AutomationExecutionMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AlarmConfiguration
import Amazonka.SSM.Types.AlarmStateInformation
import Amazonka.SSM.Types.AutomationExecutionStatus
import Amazonka.SSM.Types.AutomationSubtype
import Amazonka.SSM.Types.AutomationType
import Amazonka.SSM.Types.ExecutionMode
import Amazonka.SSM.Types.ResolvedTargets
import Amazonka.SSM.Types.Runbook
import Amazonka.SSM.Types.Target

-- | Details about a specific Automation execution.
--
-- /See:/ 'newAutomationExecutionMetadata' smart constructor.
data AutomationExecutionMetadata = AutomationExecutionMetadata'
  { -- | The details for the CloudWatch alarm applied to your automation.
    alarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | The ID of a State Manager association used in the Automation operation.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The execution ID.
    automationExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The status of the execution.
    automationExecutionStatus :: Prelude.Maybe AutomationExecutionStatus,
    -- | The subtype of the Automation operation. Currently, the only supported
    -- value is @ChangeRequest@.
    automationSubtype :: Prelude.Maybe AutomationSubtype,
    -- | Use this filter with DescribeAutomationExecutions. Specify either Local
    -- or CrossAccount. CrossAccount is an Automation that runs in multiple
    -- Amazon Web Services Regions and Amazon Web Services accounts. For more
    -- information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple Amazon Web Services Regions and accounts>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    automationType :: Prelude.Maybe AutomationType,
    -- | The name of the Change Manager change request.
    changeRequestName :: Prelude.Maybe Prelude.Text,
    -- | The action of the step that is currently running.
    currentAction :: Prelude.Maybe Prelude.Text,
    -- | The name of the step that is currently running.
    currentStepName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Automation runbook used during execution.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The document version used during the execution.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The IAM role ARN of the user who ran the automation.
    executedBy :: Prelude.Maybe Prelude.Text,
    -- | The time the execution finished. This isn\'t populated if the execution
    -- is still in progress.
    executionEndTime :: Prelude.Maybe Data.POSIX,
    -- | The time the execution started.
    executionStartTime :: Prelude.Maybe Data.POSIX,
    -- | The list of execution outputs as defined in the Automation runbook.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | An S3 bucket where execution information is stored.
    logFile :: Prelude.Maybe Prelude.Text,
    -- | The @MaxConcurrency@ value specified by the user when starting the
    -- automation.
    maxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The @MaxErrors@ value specified by the user when starting the
    -- automation.
    maxErrors :: Prelude.Maybe Prelude.Text,
    -- | The Automation execution mode.
    mode :: Prelude.Maybe ExecutionMode,
    -- | The ID of an OpsItem that is created to represent a Change Manager
    -- change request.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | The list of execution outputs as defined in the Automation runbook.
    outputs :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The execution ID of the parent automation.
    parentAutomationExecutionId :: Prelude.Maybe Prelude.Text,
    -- | A list of targets that resolved during the execution.
    resolvedTargets :: Prelude.Maybe ResolvedTargets,
    -- | Information about the Automation runbooks that are run during a runbook
    -- workflow in Change Manager.
    --
    -- The Automation runbooks specified for the runbook workflow can\'t run
    -- until all required approvals for the change request have been received.
    runbooks :: Prelude.Maybe (Prelude.NonEmpty Runbook),
    -- | The date and time the Automation operation is scheduled to start.
    scheduledTime :: Prelude.Maybe Data.POSIX,
    -- | The list of execution outputs as defined in the Automation runbook.
    target :: Prelude.Maybe Prelude.Text,
    -- | The specified key-value mapping of document parameters to target
    -- resources.
    targetMaps :: Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]],
    -- | The list of execution outputs as defined in the Automation runbook.
    targetParameterName :: Prelude.Maybe Prelude.Text,
    -- | The targets defined by the user when starting the automation.
    targets :: Prelude.Maybe [Target],
    -- | The CloudWatch alarm that was invoked by the automation.
    triggeredAlarms :: Prelude.Maybe (Prelude.NonEmpty AlarmStateInformation)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomationExecutionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmConfiguration', 'automationExecutionMetadata_alarmConfiguration' - The details for the CloudWatch alarm applied to your automation.
--
-- 'associationId', 'automationExecutionMetadata_associationId' - The ID of a State Manager association used in the Automation operation.
--
-- 'automationExecutionId', 'automationExecutionMetadata_automationExecutionId' - The execution ID.
--
-- 'automationExecutionStatus', 'automationExecutionMetadata_automationExecutionStatus' - The status of the execution.
--
-- 'automationSubtype', 'automationExecutionMetadata_automationSubtype' - The subtype of the Automation operation. Currently, the only supported
-- value is @ChangeRequest@.
--
-- 'automationType', 'automationExecutionMetadata_automationType' - Use this filter with DescribeAutomationExecutions. Specify either Local
-- or CrossAccount. CrossAccount is an Automation that runs in multiple
-- Amazon Web Services Regions and Amazon Web Services accounts. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple Amazon Web Services Regions and accounts>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'changeRequestName', 'automationExecutionMetadata_changeRequestName' - The name of the Change Manager change request.
--
-- 'currentAction', 'automationExecutionMetadata_currentAction' - The action of the step that is currently running.
--
-- 'currentStepName', 'automationExecutionMetadata_currentStepName' - The name of the step that is currently running.
--
-- 'documentName', 'automationExecutionMetadata_documentName' - The name of the Automation runbook used during execution.
--
-- 'documentVersion', 'automationExecutionMetadata_documentVersion' - The document version used during the execution.
--
-- 'executedBy', 'automationExecutionMetadata_executedBy' - The IAM role ARN of the user who ran the automation.
--
-- 'executionEndTime', 'automationExecutionMetadata_executionEndTime' - The time the execution finished. This isn\'t populated if the execution
-- is still in progress.
--
-- 'executionStartTime', 'automationExecutionMetadata_executionStartTime' - The time the execution started.
--
-- 'failureMessage', 'automationExecutionMetadata_failureMessage' - The list of execution outputs as defined in the Automation runbook.
--
-- 'logFile', 'automationExecutionMetadata_logFile' - An S3 bucket where execution information is stored.
--
-- 'maxConcurrency', 'automationExecutionMetadata_maxConcurrency' - The @MaxConcurrency@ value specified by the user when starting the
-- automation.
--
-- 'maxErrors', 'automationExecutionMetadata_maxErrors' - The @MaxErrors@ value specified by the user when starting the
-- automation.
--
-- 'mode', 'automationExecutionMetadata_mode' - The Automation execution mode.
--
-- 'opsItemId', 'automationExecutionMetadata_opsItemId' - The ID of an OpsItem that is created to represent a Change Manager
-- change request.
--
-- 'outputs', 'automationExecutionMetadata_outputs' - The list of execution outputs as defined in the Automation runbook.
--
-- 'parentAutomationExecutionId', 'automationExecutionMetadata_parentAutomationExecutionId' - The execution ID of the parent automation.
--
-- 'resolvedTargets', 'automationExecutionMetadata_resolvedTargets' - A list of targets that resolved during the execution.
--
-- 'runbooks', 'automationExecutionMetadata_runbooks' - Information about the Automation runbooks that are run during a runbook
-- workflow in Change Manager.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
--
-- 'scheduledTime', 'automationExecutionMetadata_scheduledTime' - The date and time the Automation operation is scheduled to start.
--
-- 'target', 'automationExecutionMetadata_target' - The list of execution outputs as defined in the Automation runbook.
--
-- 'targetMaps', 'automationExecutionMetadata_targetMaps' - The specified key-value mapping of document parameters to target
-- resources.
--
-- 'targetParameterName', 'automationExecutionMetadata_targetParameterName' - The list of execution outputs as defined in the Automation runbook.
--
-- 'targets', 'automationExecutionMetadata_targets' - The targets defined by the user when starting the automation.
--
-- 'triggeredAlarms', 'automationExecutionMetadata_triggeredAlarms' - The CloudWatch alarm that was invoked by the automation.
newAutomationExecutionMetadata ::
  AutomationExecutionMetadata
newAutomationExecutionMetadata =
  AutomationExecutionMetadata'
    { alarmConfiguration =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      automationExecutionId = Prelude.Nothing,
      automationExecutionStatus = Prelude.Nothing,
      automationSubtype = Prelude.Nothing,
      automationType = Prelude.Nothing,
      changeRequestName = Prelude.Nothing,
      currentAction = Prelude.Nothing,
      currentStepName = Prelude.Nothing,
      documentName = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      executedBy = Prelude.Nothing,
      executionEndTime = Prelude.Nothing,
      executionStartTime = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      logFile = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing,
      maxErrors = Prelude.Nothing,
      mode = Prelude.Nothing,
      opsItemId = Prelude.Nothing,
      outputs = Prelude.Nothing,
      parentAutomationExecutionId = Prelude.Nothing,
      resolvedTargets = Prelude.Nothing,
      runbooks = Prelude.Nothing,
      scheduledTime = Prelude.Nothing,
      target = Prelude.Nothing,
      targetMaps = Prelude.Nothing,
      targetParameterName = Prelude.Nothing,
      targets = Prelude.Nothing,
      triggeredAlarms = Prelude.Nothing
    }

-- | The details for the CloudWatch alarm applied to your automation.
automationExecutionMetadata_alarmConfiguration :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe AlarmConfiguration)
automationExecutionMetadata_alarmConfiguration = Lens.lens (\AutomationExecutionMetadata' {alarmConfiguration} -> alarmConfiguration) (\s@AutomationExecutionMetadata' {} a -> s {alarmConfiguration = a} :: AutomationExecutionMetadata)

-- | The ID of a State Manager association used in the Automation operation.
automationExecutionMetadata_associationId :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_associationId = Lens.lens (\AutomationExecutionMetadata' {associationId} -> associationId) (\s@AutomationExecutionMetadata' {} a -> s {associationId = a} :: AutomationExecutionMetadata)

-- | The execution ID.
automationExecutionMetadata_automationExecutionId :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_automationExecutionId = Lens.lens (\AutomationExecutionMetadata' {automationExecutionId} -> automationExecutionId) (\s@AutomationExecutionMetadata' {} a -> s {automationExecutionId = a} :: AutomationExecutionMetadata)

-- | The status of the execution.
automationExecutionMetadata_automationExecutionStatus :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe AutomationExecutionStatus)
automationExecutionMetadata_automationExecutionStatus = Lens.lens (\AutomationExecutionMetadata' {automationExecutionStatus} -> automationExecutionStatus) (\s@AutomationExecutionMetadata' {} a -> s {automationExecutionStatus = a} :: AutomationExecutionMetadata)

-- | The subtype of the Automation operation. Currently, the only supported
-- value is @ChangeRequest@.
automationExecutionMetadata_automationSubtype :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe AutomationSubtype)
automationExecutionMetadata_automationSubtype = Lens.lens (\AutomationExecutionMetadata' {automationSubtype} -> automationSubtype) (\s@AutomationExecutionMetadata' {} a -> s {automationSubtype = a} :: AutomationExecutionMetadata)

-- | Use this filter with DescribeAutomationExecutions. Specify either Local
-- or CrossAccount. CrossAccount is an Automation that runs in multiple
-- Amazon Web Services Regions and Amazon Web Services accounts. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple Amazon Web Services Regions and accounts>
-- in the /Amazon Web Services Systems Manager User Guide/.
automationExecutionMetadata_automationType :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe AutomationType)
automationExecutionMetadata_automationType = Lens.lens (\AutomationExecutionMetadata' {automationType} -> automationType) (\s@AutomationExecutionMetadata' {} a -> s {automationType = a} :: AutomationExecutionMetadata)

-- | The name of the Change Manager change request.
automationExecutionMetadata_changeRequestName :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_changeRequestName = Lens.lens (\AutomationExecutionMetadata' {changeRequestName} -> changeRequestName) (\s@AutomationExecutionMetadata' {} a -> s {changeRequestName = a} :: AutomationExecutionMetadata)

-- | The action of the step that is currently running.
automationExecutionMetadata_currentAction :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_currentAction = Lens.lens (\AutomationExecutionMetadata' {currentAction} -> currentAction) (\s@AutomationExecutionMetadata' {} a -> s {currentAction = a} :: AutomationExecutionMetadata)

-- | The name of the step that is currently running.
automationExecutionMetadata_currentStepName :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_currentStepName = Lens.lens (\AutomationExecutionMetadata' {currentStepName} -> currentStepName) (\s@AutomationExecutionMetadata' {} a -> s {currentStepName = a} :: AutomationExecutionMetadata)

-- | The name of the Automation runbook used during execution.
automationExecutionMetadata_documentName :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_documentName = Lens.lens (\AutomationExecutionMetadata' {documentName} -> documentName) (\s@AutomationExecutionMetadata' {} a -> s {documentName = a} :: AutomationExecutionMetadata)

-- | The document version used during the execution.
automationExecutionMetadata_documentVersion :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_documentVersion = Lens.lens (\AutomationExecutionMetadata' {documentVersion} -> documentVersion) (\s@AutomationExecutionMetadata' {} a -> s {documentVersion = a} :: AutomationExecutionMetadata)

-- | The IAM role ARN of the user who ran the automation.
automationExecutionMetadata_executedBy :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_executedBy = Lens.lens (\AutomationExecutionMetadata' {executedBy} -> executedBy) (\s@AutomationExecutionMetadata' {} a -> s {executedBy = a} :: AutomationExecutionMetadata)

-- | The time the execution finished. This isn\'t populated if the execution
-- is still in progress.
automationExecutionMetadata_executionEndTime :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.UTCTime)
automationExecutionMetadata_executionEndTime = Lens.lens (\AutomationExecutionMetadata' {executionEndTime} -> executionEndTime) (\s@AutomationExecutionMetadata' {} a -> s {executionEndTime = a} :: AutomationExecutionMetadata) Prelude.. Lens.mapping Data._Time

-- | The time the execution started.
automationExecutionMetadata_executionStartTime :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.UTCTime)
automationExecutionMetadata_executionStartTime = Lens.lens (\AutomationExecutionMetadata' {executionStartTime} -> executionStartTime) (\s@AutomationExecutionMetadata' {} a -> s {executionStartTime = a} :: AutomationExecutionMetadata) Prelude.. Lens.mapping Data._Time

-- | The list of execution outputs as defined in the Automation runbook.
automationExecutionMetadata_failureMessage :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_failureMessage = Lens.lens (\AutomationExecutionMetadata' {failureMessage} -> failureMessage) (\s@AutomationExecutionMetadata' {} a -> s {failureMessage = a} :: AutomationExecutionMetadata)

-- | An S3 bucket where execution information is stored.
automationExecutionMetadata_logFile :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_logFile = Lens.lens (\AutomationExecutionMetadata' {logFile} -> logFile) (\s@AutomationExecutionMetadata' {} a -> s {logFile = a} :: AutomationExecutionMetadata)

-- | The @MaxConcurrency@ value specified by the user when starting the
-- automation.
automationExecutionMetadata_maxConcurrency :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_maxConcurrency = Lens.lens (\AutomationExecutionMetadata' {maxConcurrency} -> maxConcurrency) (\s@AutomationExecutionMetadata' {} a -> s {maxConcurrency = a} :: AutomationExecutionMetadata)

-- | The @MaxErrors@ value specified by the user when starting the
-- automation.
automationExecutionMetadata_maxErrors :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_maxErrors = Lens.lens (\AutomationExecutionMetadata' {maxErrors} -> maxErrors) (\s@AutomationExecutionMetadata' {} a -> s {maxErrors = a} :: AutomationExecutionMetadata)

-- | The Automation execution mode.
automationExecutionMetadata_mode :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe ExecutionMode)
automationExecutionMetadata_mode = Lens.lens (\AutomationExecutionMetadata' {mode} -> mode) (\s@AutomationExecutionMetadata' {} a -> s {mode = a} :: AutomationExecutionMetadata)

-- | The ID of an OpsItem that is created to represent a Change Manager
-- change request.
automationExecutionMetadata_opsItemId :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_opsItemId = Lens.lens (\AutomationExecutionMetadata' {opsItemId} -> opsItemId) (\s@AutomationExecutionMetadata' {} a -> s {opsItemId = a} :: AutomationExecutionMetadata)

-- | The list of execution outputs as defined in the Automation runbook.
automationExecutionMetadata_outputs :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
automationExecutionMetadata_outputs = Lens.lens (\AutomationExecutionMetadata' {outputs} -> outputs) (\s@AutomationExecutionMetadata' {} a -> s {outputs = a} :: AutomationExecutionMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The execution ID of the parent automation.
automationExecutionMetadata_parentAutomationExecutionId :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_parentAutomationExecutionId = Lens.lens (\AutomationExecutionMetadata' {parentAutomationExecutionId} -> parentAutomationExecutionId) (\s@AutomationExecutionMetadata' {} a -> s {parentAutomationExecutionId = a} :: AutomationExecutionMetadata)

-- | A list of targets that resolved during the execution.
automationExecutionMetadata_resolvedTargets :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe ResolvedTargets)
automationExecutionMetadata_resolvedTargets = Lens.lens (\AutomationExecutionMetadata' {resolvedTargets} -> resolvedTargets) (\s@AutomationExecutionMetadata' {} a -> s {resolvedTargets = a} :: AutomationExecutionMetadata)

-- | Information about the Automation runbooks that are run during a runbook
-- workflow in Change Manager.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
automationExecutionMetadata_runbooks :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe (Prelude.NonEmpty Runbook))
automationExecutionMetadata_runbooks = Lens.lens (\AutomationExecutionMetadata' {runbooks} -> runbooks) (\s@AutomationExecutionMetadata' {} a -> s {runbooks = a} :: AutomationExecutionMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the Automation operation is scheduled to start.
automationExecutionMetadata_scheduledTime :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.UTCTime)
automationExecutionMetadata_scheduledTime = Lens.lens (\AutomationExecutionMetadata' {scheduledTime} -> scheduledTime) (\s@AutomationExecutionMetadata' {} a -> s {scheduledTime = a} :: AutomationExecutionMetadata) Prelude.. Lens.mapping Data._Time

-- | The list of execution outputs as defined in the Automation runbook.
automationExecutionMetadata_target :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_target = Lens.lens (\AutomationExecutionMetadata' {target} -> target) (\s@AutomationExecutionMetadata' {} a -> s {target = a} :: AutomationExecutionMetadata)

-- | The specified key-value mapping of document parameters to target
-- resources.
automationExecutionMetadata_targetMaps :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe [Prelude.HashMap Prelude.Text [Prelude.Text]])
automationExecutionMetadata_targetMaps = Lens.lens (\AutomationExecutionMetadata' {targetMaps} -> targetMaps) (\s@AutomationExecutionMetadata' {} a -> s {targetMaps = a} :: AutomationExecutionMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The list of execution outputs as defined in the Automation runbook.
automationExecutionMetadata_targetParameterName :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe Prelude.Text)
automationExecutionMetadata_targetParameterName = Lens.lens (\AutomationExecutionMetadata' {targetParameterName} -> targetParameterName) (\s@AutomationExecutionMetadata' {} a -> s {targetParameterName = a} :: AutomationExecutionMetadata)

-- | The targets defined by the user when starting the automation.
automationExecutionMetadata_targets :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe [Target])
automationExecutionMetadata_targets = Lens.lens (\AutomationExecutionMetadata' {targets} -> targets) (\s@AutomationExecutionMetadata' {} a -> s {targets = a} :: AutomationExecutionMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The CloudWatch alarm that was invoked by the automation.
automationExecutionMetadata_triggeredAlarms :: Lens.Lens' AutomationExecutionMetadata (Prelude.Maybe (Prelude.NonEmpty AlarmStateInformation))
automationExecutionMetadata_triggeredAlarms = Lens.lens (\AutomationExecutionMetadata' {triggeredAlarms} -> triggeredAlarms) (\s@AutomationExecutionMetadata' {} a -> s {triggeredAlarms = a} :: AutomationExecutionMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AutomationExecutionMetadata where
  parseJSON =
    Data.withObject
      "AutomationExecutionMetadata"
      ( \x ->
          AutomationExecutionMetadata'
            Prelude.<$> (x Data..:? "AlarmConfiguration")
            Prelude.<*> (x Data..:? "AssociationId")
            Prelude.<*> (x Data..:? "AutomationExecutionId")
            Prelude.<*> (x Data..:? "AutomationExecutionStatus")
            Prelude.<*> (x Data..:? "AutomationSubtype")
            Prelude.<*> (x Data..:? "AutomationType")
            Prelude.<*> (x Data..:? "ChangeRequestName")
            Prelude.<*> (x Data..:? "CurrentAction")
            Prelude.<*> (x Data..:? "CurrentStepName")
            Prelude.<*> (x Data..:? "DocumentName")
            Prelude.<*> (x Data..:? "DocumentVersion")
            Prelude.<*> (x Data..:? "ExecutedBy")
            Prelude.<*> (x Data..:? "ExecutionEndTime")
            Prelude.<*> (x Data..:? "ExecutionStartTime")
            Prelude.<*> (x Data..:? "FailureMessage")
            Prelude.<*> (x Data..:? "LogFile")
            Prelude.<*> (x Data..:? "MaxConcurrency")
            Prelude.<*> (x Data..:? "MaxErrors")
            Prelude.<*> (x Data..:? "Mode")
            Prelude.<*> (x Data..:? "OpsItemId")
            Prelude.<*> (x Data..:? "Outputs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ParentAutomationExecutionId")
            Prelude.<*> (x Data..:? "ResolvedTargets")
            Prelude.<*> (x Data..:? "Runbooks")
            Prelude.<*> (x Data..:? "ScheduledTime")
            Prelude.<*> (x Data..:? "Target")
            Prelude.<*> (x Data..:? "TargetMaps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TargetParameterName")
            Prelude.<*> (x Data..:? "Targets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TriggeredAlarms")
      )

instance Prelude.Hashable AutomationExecutionMetadata where
  hashWithSalt _salt AutomationExecutionMetadata' {..} =
    _salt `Prelude.hashWithSalt` alarmConfiguration
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` automationExecutionId
      `Prelude.hashWithSalt` automationExecutionStatus
      `Prelude.hashWithSalt` automationSubtype
      `Prelude.hashWithSalt` automationType
      `Prelude.hashWithSalt` changeRequestName
      `Prelude.hashWithSalt` currentAction
      `Prelude.hashWithSalt` currentStepName
      `Prelude.hashWithSalt` documentName
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` executedBy
      `Prelude.hashWithSalt` executionEndTime
      `Prelude.hashWithSalt` executionStartTime
      `Prelude.hashWithSalt` failureMessage
      `Prelude.hashWithSalt` logFile
      `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` maxErrors
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` opsItemId
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` parentAutomationExecutionId
      `Prelude.hashWithSalt` resolvedTargets
      `Prelude.hashWithSalt` runbooks
      `Prelude.hashWithSalt` scheduledTime
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` targetMaps
      `Prelude.hashWithSalt` targetParameterName
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` triggeredAlarms

instance Prelude.NFData AutomationExecutionMetadata where
  rnf AutomationExecutionMetadata' {..} =
    Prelude.rnf alarmConfiguration
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf automationExecutionId
      `Prelude.seq` Prelude.rnf automationExecutionStatus
      `Prelude.seq` Prelude.rnf automationSubtype
      `Prelude.seq` Prelude.rnf automationType
      `Prelude.seq` Prelude.rnf changeRequestName
      `Prelude.seq` Prelude.rnf currentAction
      `Prelude.seq` Prelude.rnf currentStepName
      `Prelude.seq` Prelude.rnf documentName
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf executedBy
      `Prelude.seq` Prelude.rnf executionEndTime
      `Prelude.seq` Prelude.rnf executionStartTime
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf logFile
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf maxErrors
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf opsItemId
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf
        parentAutomationExecutionId
      `Prelude.seq` Prelude.rnf
        resolvedTargets
      `Prelude.seq` Prelude.rnf runbooks
      `Prelude.seq` Prelude.rnf
        scheduledTime
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf
        targetMaps
      `Prelude.seq` Prelude.rnf
        targetParameterName
      `Prelude.seq` Prelude.rnf
        targets
      `Prelude.seq` Prelude.rnf
        triggeredAlarms

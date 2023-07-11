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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.WorkflowStepSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.WorkflowStepSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types.Owner
import Amazonka.MigrationHubOrchestrator.Types.StepActionType
import Amazonka.MigrationHubOrchestrator.Types.StepStatus
import qualified Amazonka.Prelude as Prelude

-- | The summary of the step in a migration workflow.
--
-- /See:/ 'newWorkflowStepSummary' smart constructor.
data WorkflowStepSummary = WorkflowStepSummary'
  { -- | The description of the step.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next step.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The number of servers that have been migrated.
    noOfSrvCompleted :: Prelude.Maybe Prelude.Int,
    -- | The number of servers that have failed to migrate.
    noOfSrvFailed :: Prelude.Maybe Prelude.Int,
    -- | The owner of the step.
    owner :: Prelude.Maybe Owner,
    -- | The previous step.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The location of the script.
    scriptLocation :: Prelude.Maybe Prelude.Text,
    -- | The status of the step.
    status :: Prelude.Maybe StepStatus,
    -- | The status message of the migration workflow.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The action type of the step. You must run and update the status of a
    -- manual step for the workflow to continue after the completion of the
    -- step.
    stepActionType :: Prelude.Maybe StepActionType,
    -- | The ID of the step.
    stepId :: Prelude.Maybe Prelude.Text,
    -- | The total number of servers that have been migrated.
    totalNoOfSrv :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowStepSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'workflowStepSummary_description' - The description of the step.
--
-- 'name', 'workflowStepSummary_name' - The name of the step.
--
-- 'next', 'workflowStepSummary_next' - The next step.
--
-- 'noOfSrvCompleted', 'workflowStepSummary_noOfSrvCompleted' - The number of servers that have been migrated.
--
-- 'noOfSrvFailed', 'workflowStepSummary_noOfSrvFailed' - The number of servers that have failed to migrate.
--
-- 'owner', 'workflowStepSummary_owner' - The owner of the step.
--
-- 'previous', 'workflowStepSummary_previous' - The previous step.
--
-- 'scriptLocation', 'workflowStepSummary_scriptLocation' - The location of the script.
--
-- 'status', 'workflowStepSummary_status' - The status of the step.
--
-- 'statusMessage', 'workflowStepSummary_statusMessage' - The status message of the migration workflow.
--
-- 'stepActionType', 'workflowStepSummary_stepActionType' - The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
--
-- 'stepId', 'workflowStepSummary_stepId' - The ID of the step.
--
-- 'totalNoOfSrv', 'workflowStepSummary_totalNoOfSrv' - The total number of servers that have been migrated.
newWorkflowStepSummary ::
  WorkflowStepSummary
newWorkflowStepSummary =
  WorkflowStepSummary'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      next = Prelude.Nothing,
      noOfSrvCompleted = Prelude.Nothing,
      noOfSrvFailed = Prelude.Nothing,
      owner = Prelude.Nothing,
      previous = Prelude.Nothing,
      scriptLocation = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      stepActionType = Prelude.Nothing,
      stepId = Prelude.Nothing,
      totalNoOfSrv = Prelude.Nothing
    }

-- | The description of the step.
workflowStepSummary_description :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe Prelude.Text)
workflowStepSummary_description = Lens.lens (\WorkflowStepSummary' {description} -> description) (\s@WorkflowStepSummary' {} a -> s {description = a} :: WorkflowStepSummary)

-- | The name of the step.
workflowStepSummary_name :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe Prelude.Text)
workflowStepSummary_name = Lens.lens (\WorkflowStepSummary' {name} -> name) (\s@WorkflowStepSummary' {} a -> s {name = a} :: WorkflowStepSummary)

-- | The next step.
workflowStepSummary_next :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe [Prelude.Text])
workflowStepSummary_next = Lens.lens (\WorkflowStepSummary' {next} -> next) (\s@WorkflowStepSummary' {} a -> s {next = a} :: WorkflowStepSummary) Prelude.. Lens.mapping Lens.coerced

-- | The number of servers that have been migrated.
workflowStepSummary_noOfSrvCompleted :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe Prelude.Int)
workflowStepSummary_noOfSrvCompleted = Lens.lens (\WorkflowStepSummary' {noOfSrvCompleted} -> noOfSrvCompleted) (\s@WorkflowStepSummary' {} a -> s {noOfSrvCompleted = a} :: WorkflowStepSummary)

-- | The number of servers that have failed to migrate.
workflowStepSummary_noOfSrvFailed :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe Prelude.Int)
workflowStepSummary_noOfSrvFailed = Lens.lens (\WorkflowStepSummary' {noOfSrvFailed} -> noOfSrvFailed) (\s@WorkflowStepSummary' {} a -> s {noOfSrvFailed = a} :: WorkflowStepSummary)

-- | The owner of the step.
workflowStepSummary_owner :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe Owner)
workflowStepSummary_owner = Lens.lens (\WorkflowStepSummary' {owner} -> owner) (\s@WorkflowStepSummary' {} a -> s {owner = a} :: WorkflowStepSummary)

-- | The previous step.
workflowStepSummary_previous :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe [Prelude.Text])
workflowStepSummary_previous = Lens.lens (\WorkflowStepSummary' {previous} -> previous) (\s@WorkflowStepSummary' {} a -> s {previous = a} :: WorkflowStepSummary) Prelude.. Lens.mapping Lens.coerced

-- | The location of the script.
workflowStepSummary_scriptLocation :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe Prelude.Text)
workflowStepSummary_scriptLocation = Lens.lens (\WorkflowStepSummary' {scriptLocation} -> scriptLocation) (\s@WorkflowStepSummary' {} a -> s {scriptLocation = a} :: WorkflowStepSummary)

-- | The status of the step.
workflowStepSummary_status :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe StepStatus)
workflowStepSummary_status = Lens.lens (\WorkflowStepSummary' {status} -> status) (\s@WorkflowStepSummary' {} a -> s {status = a} :: WorkflowStepSummary)

-- | The status message of the migration workflow.
workflowStepSummary_statusMessage :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe Prelude.Text)
workflowStepSummary_statusMessage = Lens.lens (\WorkflowStepSummary' {statusMessage} -> statusMessage) (\s@WorkflowStepSummary' {} a -> s {statusMessage = a} :: WorkflowStepSummary)

-- | The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
workflowStepSummary_stepActionType :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe StepActionType)
workflowStepSummary_stepActionType = Lens.lens (\WorkflowStepSummary' {stepActionType} -> stepActionType) (\s@WorkflowStepSummary' {} a -> s {stepActionType = a} :: WorkflowStepSummary)

-- | The ID of the step.
workflowStepSummary_stepId :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe Prelude.Text)
workflowStepSummary_stepId = Lens.lens (\WorkflowStepSummary' {stepId} -> stepId) (\s@WorkflowStepSummary' {} a -> s {stepId = a} :: WorkflowStepSummary)

-- | The total number of servers that have been migrated.
workflowStepSummary_totalNoOfSrv :: Lens.Lens' WorkflowStepSummary (Prelude.Maybe Prelude.Int)
workflowStepSummary_totalNoOfSrv = Lens.lens (\WorkflowStepSummary' {totalNoOfSrv} -> totalNoOfSrv) (\s@WorkflowStepSummary' {} a -> s {totalNoOfSrv = a} :: WorkflowStepSummary)

instance Data.FromJSON WorkflowStepSummary where
  parseJSON =
    Data.withObject
      "WorkflowStepSummary"
      ( \x ->
          WorkflowStepSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "next" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "noOfSrvCompleted")
            Prelude.<*> (x Data..:? "noOfSrvFailed")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "previous" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "scriptLocation")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "stepActionType")
            Prelude.<*> (x Data..:? "stepId")
            Prelude.<*> (x Data..:? "totalNoOfSrv")
      )

instance Prelude.Hashable WorkflowStepSummary where
  hashWithSalt _salt WorkflowStepSummary' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` noOfSrvCompleted
      `Prelude.hashWithSalt` noOfSrvFailed
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` previous
      `Prelude.hashWithSalt` scriptLocation
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` stepActionType
      `Prelude.hashWithSalt` stepId
      `Prelude.hashWithSalt` totalNoOfSrv

instance Prelude.NFData WorkflowStepSummary where
  rnf WorkflowStepSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf next
      `Prelude.seq` Prelude.rnf noOfSrvCompleted
      `Prelude.seq` Prelude.rnf noOfSrvFailed
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf previous
      `Prelude.seq` Prelude.rnf scriptLocation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf stepActionType
      `Prelude.seq` Prelude.rnf stepId
      `Prelude.seq` Prelude.rnf totalNoOfSrv

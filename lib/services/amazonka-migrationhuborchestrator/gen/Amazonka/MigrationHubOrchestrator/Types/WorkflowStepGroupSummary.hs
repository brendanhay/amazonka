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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.WorkflowStepGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.WorkflowStepGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types.Owner
import Amazonka.MigrationHubOrchestrator.Types.StepGroupStatus
import qualified Amazonka.Prelude as Prelude

-- | The summary of a step group in a workflow.
--
-- /See:/ 'newWorkflowStepGroupSummary' smart constructor.
data WorkflowStepGroupSummary = WorkflowStepGroupSummary'
  { -- | The ID of the step group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the step group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next step group.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The owner of the step group.
    owner :: Prelude.Maybe Owner,
    -- | The previous step group.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The status of the step group.
    status :: Prelude.Maybe StepGroupStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowStepGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'workflowStepGroupSummary_id' - The ID of the step group.
--
-- 'name', 'workflowStepGroupSummary_name' - The name of the step group.
--
-- 'next', 'workflowStepGroupSummary_next' - The next step group.
--
-- 'owner', 'workflowStepGroupSummary_owner' - The owner of the step group.
--
-- 'previous', 'workflowStepGroupSummary_previous' - The previous step group.
--
-- 'status', 'workflowStepGroupSummary_status' - The status of the step group.
newWorkflowStepGroupSummary ::
  WorkflowStepGroupSummary
newWorkflowStepGroupSummary =
  WorkflowStepGroupSummary'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      next = Prelude.Nothing,
      owner = Prelude.Nothing,
      previous = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ID of the step group.
workflowStepGroupSummary_id :: Lens.Lens' WorkflowStepGroupSummary (Prelude.Maybe Prelude.Text)
workflowStepGroupSummary_id = Lens.lens (\WorkflowStepGroupSummary' {id} -> id) (\s@WorkflowStepGroupSummary' {} a -> s {id = a} :: WorkflowStepGroupSummary)

-- | The name of the step group.
workflowStepGroupSummary_name :: Lens.Lens' WorkflowStepGroupSummary (Prelude.Maybe Prelude.Text)
workflowStepGroupSummary_name = Lens.lens (\WorkflowStepGroupSummary' {name} -> name) (\s@WorkflowStepGroupSummary' {} a -> s {name = a} :: WorkflowStepGroupSummary)

-- | The next step group.
workflowStepGroupSummary_next :: Lens.Lens' WorkflowStepGroupSummary (Prelude.Maybe [Prelude.Text])
workflowStepGroupSummary_next = Lens.lens (\WorkflowStepGroupSummary' {next} -> next) (\s@WorkflowStepGroupSummary' {} a -> s {next = a} :: WorkflowStepGroupSummary) Prelude.. Lens.mapping Lens.coerced

-- | The owner of the step group.
workflowStepGroupSummary_owner :: Lens.Lens' WorkflowStepGroupSummary (Prelude.Maybe Owner)
workflowStepGroupSummary_owner = Lens.lens (\WorkflowStepGroupSummary' {owner} -> owner) (\s@WorkflowStepGroupSummary' {} a -> s {owner = a} :: WorkflowStepGroupSummary)

-- | The previous step group.
workflowStepGroupSummary_previous :: Lens.Lens' WorkflowStepGroupSummary (Prelude.Maybe [Prelude.Text])
workflowStepGroupSummary_previous = Lens.lens (\WorkflowStepGroupSummary' {previous} -> previous) (\s@WorkflowStepGroupSummary' {} a -> s {previous = a} :: WorkflowStepGroupSummary) Prelude.. Lens.mapping Lens.coerced

-- | The status of the step group.
workflowStepGroupSummary_status :: Lens.Lens' WorkflowStepGroupSummary (Prelude.Maybe StepGroupStatus)
workflowStepGroupSummary_status = Lens.lens (\WorkflowStepGroupSummary' {status} -> status) (\s@WorkflowStepGroupSummary' {} a -> s {status = a} :: WorkflowStepGroupSummary)

instance Data.FromJSON WorkflowStepGroupSummary where
  parseJSON =
    Data.withObject
      "WorkflowStepGroupSummary"
      ( \x ->
          WorkflowStepGroupSummary'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "next" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "previous" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable WorkflowStepGroupSummary where
  hashWithSalt _salt WorkflowStepGroupSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` previous
      `Prelude.hashWithSalt` status

instance Prelude.NFData WorkflowStepGroupSummary where
  rnf WorkflowStepGroupSummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf next
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf previous
      `Prelude.seq` Prelude.rnf status

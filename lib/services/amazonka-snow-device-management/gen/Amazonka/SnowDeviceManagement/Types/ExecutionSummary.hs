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
-- Module      : Amazonka.SnowDeviceManagement.Types.ExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.ExecutionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SnowDeviceManagement.Types.ExecutionState

-- | The summary of a task execution on a specified device.
--
-- /See:/ 'newExecutionSummary' smart constructor.
data ExecutionSummary = ExecutionSummary'
  { -- | The ID of the execution.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The state of the execution.
    state :: Prelude.Maybe ExecutionState,
    -- | The ID of the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the managed device that the task is being executed on.
    managedDeviceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionId', 'executionSummary_executionId' - The ID of the execution.
--
-- 'state', 'executionSummary_state' - The state of the execution.
--
-- 'taskId', 'executionSummary_taskId' - The ID of the task.
--
-- 'managedDeviceId', 'executionSummary_managedDeviceId' - The ID of the managed device that the task is being executed on.
newExecutionSummary ::
  ExecutionSummary
newExecutionSummary =
  ExecutionSummary'
    { executionId = Prelude.Nothing,
      state = Prelude.Nothing,
      taskId = Prelude.Nothing,
      managedDeviceId = Prelude.Nothing
    }

-- | The ID of the execution.
executionSummary_executionId :: Lens.Lens' ExecutionSummary (Prelude.Maybe Prelude.Text)
executionSummary_executionId = Lens.lens (\ExecutionSummary' {executionId} -> executionId) (\s@ExecutionSummary' {} a -> s {executionId = a} :: ExecutionSummary)

-- | The state of the execution.
executionSummary_state :: Lens.Lens' ExecutionSummary (Prelude.Maybe ExecutionState)
executionSummary_state = Lens.lens (\ExecutionSummary' {state} -> state) (\s@ExecutionSummary' {} a -> s {state = a} :: ExecutionSummary)

-- | The ID of the task.
executionSummary_taskId :: Lens.Lens' ExecutionSummary (Prelude.Maybe Prelude.Text)
executionSummary_taskId = Lens.lens (\ExecutionSummary' {taskId} -> taskId) (\s@ExecutionSummary' {} a -> s {taskId = a} :: ExecutionSummary)

-- | The ID of the managed device that the task is being executed on.
executionSummary_managedDeviceId :: Lens.Lens' ExecutionSummary (Prelude.Maybe Prelude.Text)
executionSummary_managedDeviceId = Lens.lens (\ExecutionSummary' {managedDeviceId} -> managedDeviceId) (\s@ExecutionSummary' {} a -> s {managedDeviceId = a} :: ExecutionSummary)

instance Core.FromJSON ExecutionSummary where
  parseJSON =
    Core.withObject
      "ExecutionSummary"
      ( \x ->
          ExecutionSummary'
            Prelude.<$> (x Core..:? "executionId")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "taskId")
            Prelude.<*> (x Core..:? "managedDeviceId")
      )

instance Prelude.Hashable ExecutionSummary where
  hashWithSalt _salt ExecutionSummary' {..} =
    _salt `Prelude.hashWithSalt` executionId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` taskId
      `Prelude.hashWithSalt` managedDeviceId

instance Prelude.NFData ExecutionSummary where
  rnf ExecutionSummary' {..} =
    Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf managedDeviceId

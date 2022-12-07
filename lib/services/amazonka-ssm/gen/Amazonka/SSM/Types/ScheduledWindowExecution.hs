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
-- Module      : Amazonka.SSM.Types.ScheduledWindowExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ScheduledWindowExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a scheduled execution for a maintenance window.
--
-- /See:/ 'newScheduledWindowExecution' smart constructor.
data ScheduledWindowExecution = ScheduledWindowExecution'
  { -- | The name of the maintenance window to be run.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in ISO-8601 Extended format, that the maintenance window is
    -- scheduled to be run.
    executionTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window to be run.
    windowId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledWindowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'scheduledWindowExecution_name' - The name of the maintenance window to be run.
--
-- 'executionTime', 'scheduledWindowExecution_executionTime' - The time, in ISO-8601 Extended format, that the maintenance window is
-- scheduled to be run.
--
-- 'windowId', 'scheduledWindowExecution_windowId' - The ID of the maintenance window to be run.
newScheduledWindowExecution ::
  ScheduledWindowExecution
newScheduledWindowExecution =
  ScheduledWindowExecution'
    { name = Prelude.Nothing,
      executionTime = Prelude.Nothing,
      windowId = Prelude.Nothing
    }

-- | The name of the maintenance window to be run.
scheduledWindowExecution_name :: Lens.Lens' ScheduledWindowExecution (Prelude.Maybe Prelude.Text)
scheduledWindowExecution_name = Lens.lens (\ScheduledWindowExecution' {name} -> name) (\s@ScheduledWindowExecution' {} a -> s {name = a} :: ScheduledWindowExecution)

-- | The time, in ISO-8601 Extended format, that the maintenance window is
-- scheduled to be run.
scheduledWindowExecution_executionTime :: Lens.Lens' ScheduledWindowExecution (Prelude.Maybe Prelude.Text)
scheduledWindowExecution_executionTime = Lens.lens (\ScheduledWindowExecution' {executionTime} -> executionTime) (\s@ScheduledWindowExecution' {} a -> s {executionTime = a} :: ScheduledWindowExecution)

-- | The ID of the maintenance window to be run.
scheduledWindowExecution_windowId :: Lens.Lens' ScheduledWindowExecution (Prelude.Maybe Prelude.Text)
scheduledWindowExecution_windowId = Lens.lens (\ScheduledWindowExecution' {windowId} -> windowId) (\s@ScheduledWindowExecution' {} a -> s {windowId = a} :: ScheduledWindowExecution)

instance Data.FromJSON ScheduledWindowExecution where
  parseJSON =
    Data.withObject
      "ScheduledWindowExecution"
      ( \x ->
          ScheduledWindowExecution'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ExecutionTime")
            Prelude.<*> (x Data..:? "WindowId")
      )

instance Prelude.Hashable ScheduledWindowExecution where
  hashWithSalt _salt ScheduledWindowExecution' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` executionTime
      `Prelude.hashWithSalt` windowId

instance Prelude.NFData ScheduledWindowExecution where
  rnf ScheduledWindowExecution' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf executionTime
      `Prelude.seq` Prelude.rnf windowId

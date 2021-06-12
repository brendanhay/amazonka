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
-- Module      : Network.AWS.SSM.Types.ScheduledWindowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ScheduledWindowExecution where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a scheduled execution for a maintenance window.
--
-- /See:/ 'newScheduledWindowExecution' smart constructor.
data ScheduledWindowExecution = ScheduledWindowExecution'
  { -- | The time, in ISO-8601 Extended format, that the maintenance window is
    -- scheduled to be run.
    executionTime :: Core.Maybe Core.Text,
    -- | The name of the maintenance window to be run.
    name :: Core.Maybe Core.Text,
    -- | The ID of the maintenance window to be run.
    windowId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduledWindowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionTime', 'scheduledWindowExecution_executionTime' - The time, in ISO-8601 Extended format, that the maintenance window is
-- scheduled to be run.
--
-- 'name', 'scheduledWindowExecution_name' - The name of the maintenance window to be run.
--
-- 'windowId', 'scheduledWindowExecution_windowId' - The ID of the maintenance window to be run.
newScheduledWindowExecution ::
  ScheduledWindowExecution
newScheduledWindowExecution =
  ScheduledWindowExecution'
    { executionTime =
        Core.Nothing,
      name = Core.Nothing,
      windowId = Core.Nothing
    }

-- | The time, in ISO-8601 Extended format, that the maintenance window is
-- scheduled to be run.
scheduledWindowExecution_executionTime :: Lens.Lens' ScheduledWindowExecution (Core.Maybe Core.Text)
scheduledWindowExecution_executionTime = Lens.lens (\ScheduledWindowExecution' {executionTime} -> executionTime) (\s@ScheduledWindowExecution' {} a -> s {executionTime = a} :: ScheduledWindowExecution)

-- | The name of the maintenance window to be run.
scheduledWindowExecution_name :: Lens.Lens' ScheduledWindowExecution (Core.Maybe Core.Text)
scheduledWindowExecution_name = Lens.lens (\ScheduledWindowExecution' {name} -> name) (\s@ScheduledWindowExecution' {} a -> s {name = a} :: ScheduledWindowExecution)

-- | The ID of the maintenance window to be run.
scheduledWindowExecution_windowId :: Lens.Lens' ScheduledWindowExecution (Core.Maybe Core.Text)
scheduledWindowExecution_windowId = Lens.lens (\ScheduledWindowExecution' {windowId} -> windowId) (\s@ScheduledWindowExecution' {} a -> s {windowId = a} :: ScheduledWindowExecution)

instance Core.FromJSON ScheduledWindowExecution where
  parseJSON =
    Core.withObject
      "ScheduledWindowExecution"
      ( \x ->
          ScheduledWindowExecution'
            Core.<$> (x Core..:? "ExecutionTime")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "WindowId")
      )

instance Core.Hashable ScheduledWindowExecution

instance Core.NFData ScheduledWindowExecution

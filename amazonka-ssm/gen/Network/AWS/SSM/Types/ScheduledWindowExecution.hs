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
-- Module      : Network.AWS.SSM.Types.ScheduledWindowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ScheduledWindowExecution where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a scheduled execution for a maintenance window.
--
-- /See:/ 'newScheduledWindowExecution' smart constructor.
data ScheduledWindowExecution = ScheduledWindowExecution'
  { -- | The time, in ISO-8601 Extended format, that the maintenance window is
    -- scheduled to be run.
    executionTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the maintenance window to be run.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window to be run.
    windowId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      name = Prelude.Nothing,
      windowId = Prelude.Nothing
    }

-- | The time, in ISO-8601 Extended format, that the maintenance window is
-- scheduled to be run.
scheduledWindowExecution_executionTime :: Lens.Lens' ScheduledWindowExecution (Prelude.Maybe Prelude.Text)
scheduledWindowExecution_executionTime = Lens.lens (\ScheduledWindowExecution' {executionTime} -> executionTime) (\s@ScheduledWindowExecution' {} a -> s {executionTime = a} :: ScheduledWindowExecution)

-- | The name of the maintenance window to be run.
scheduledWindowExecution_name :: Lens.Lens' ScheduledWindowExecution (Prelude.Maybe Prelude.Text)
scheduledWindowExecution_name = Lens.lens (\ScheduledWindowExecution' {name} -> name) (\s@ScheduledWindowExecution' {} a -> s {name = a} :: ScheduledWindowExecution)

-- | The ID of the maintenance window to be run.
scheduledWindowExecution_windowId :: Lens.Lens' ScheduledWindowExecution (Prelude.Maybe Prelude.Text)
scheduledWindowExecution_windowId = Lens.lens (\ScheduledWindowExecution' {windowId} -> windowId) (\s@ScheduledWindowExecution' {} a -> s {windowId = a} :: ScheduledWindowExecution)

instance Prelude.FromJSON ScheduledWindowExecution where
  parseJSON =
    Prelude.withObject
      "ScheduledWindowExecution"
      ( \x ->
          ScheduledWindowExecution'
            Prelude.<$> (x Prelude..:? "ExecutionTime")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "WindowId")
      )

instance Prelude.Hashable ScheduledWindowExecution

instance Prelude.NFData ScheduledWindowExecution

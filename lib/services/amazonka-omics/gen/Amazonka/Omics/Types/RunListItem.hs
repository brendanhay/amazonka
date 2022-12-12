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
-- Module      : Amazonka.Omics.Types.RunListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.RunListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.RunStatus
import qualified Amazonka.Prelude as Prelude

-- | A workflow run.
--
-- /See:/ 'newRunListItem' smart constructor.
data RunListItem = RunListItem'
  { -- | The run\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the run was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The run\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The run\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The run\'s priority.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | When the run started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The run\'s status.
    status :: Prelude.Maybe RunStatus,
    -- | When the run stopped.
    stopTime :: Prelude.Maybe Data.POSIX,
    -- | The run\'s storage capacity.
    storageCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The run\'s workflow ID.
    workflowId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'runListItem_arn' - The run\'s ARN.
--
-- 'creationTime', 'runListItem_creationTime' - When the run was created.
--
-- 'id', 'runListItem_id' - The run\'s ID.
--
-- 'name', 'runListItem_name' - The run\'s name.
--
-- 'priority', 'runListItem_priority' - The run\'s priority.
--
-- 'startTime', 'runListItem_startTime' - When the run started.
--
-- 'status', 'runListItem_status' - The run\'s status.
--
-- 'stopTime', 'runListItem_stopTime' - When the run stopped.
--
-- 'storageCapacity', 'runListItem_storageCapacity' - The run\'s storage capacity.
--
-- 'workflowId', 'runListItem_workflowId' - The run\'s workflow ID.
newRunListItem ::
  RunListItem
newRunListItem =
  RunListItem'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      priority = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      stopTime = Prelude.Nothing,
      storageCapacity = Prelude.Nothing,
      workflowId = Prelude.Nothing
    }

-- | The run\'s ARN.
runListItem_arn :: Lens.Lens' RunListItem (Prelude.Maybe Prelude.Text)
runListItem_arn = Lens.lens (\RunListItem' {arn} -> arn) (\s@RunListItem' {} a -> s {arn = a} :: RunListItem)

-- | When the run was created.
runListItem_creationTime :: Lens.Lens' RunListItem (Prelude.Maybe Prelude.UTCTime)
runListItem_creationTime = Lens.lens (\RunListItem' {creationTime} -> creationTime) (\s@RunListItem' {} a -> s {creationTime = a} :: RunListItem) Prelude.. Lens.mapping Data._Time

-- | The run\'s ID.
runListItem_id :: Lens.Lens' RunListItem (Prelude.Maybe Prelude.Text)
runListItem_id = Lens.lens (\RunListItem' {id} -> id) (\s@RunListItem' {} a -> s {id = a} :: RunListItem)

-- | The run\'s name.
runListItem_name :: Lens.Lens' RunListItem (Prelude.Maybe Prelude.Text)
runListItem_name = Lens.lens (\RunListItem' {name} -> name) (\s@RunListItem' {} a -> s {name = a} :: RunListItem)

-- | The run\'s priority.
runListItem_priority :: Lens.Lens' RunListItem (Prelude.Maybe Prelude.Natural)
runListItem_priority = Lens.lens (\RunListItem' {priority} -> priority) (\s@RunListItem' {} a -> s {priority = a} :: RunListItem)

-- | When the run started.
runListItem_startTime :: Lens.Lens' RunListItem (Prelude.Maybe Prelude.UTCTime)
runListItem_startTime = Lens.lens (\RunListItem' {startTime} -> startTime) (\s@RunListItem' {} a -> s {startTime = a} :: RunListItem) Prelude.. Lens.mapping Data._Time

-- | The run\'s status.
runListItem_status :: Lens.Lens' RunListItem (Prelude.Maybe RunStatus)
runListItem_status = Lens.lens (\RunListItem' {status} -> status) (\s@RunListItem' {} a -> s {status = a} :: RunListItem)

-- | When the run stopped.
runListItem_stopTime :: Lens.Lens' RunListItem (Prelude.Maybe Prelude.UTCTime)
runListItem_stopTime = Lens.lens (\RunListItem' {stopTime} -> stopTime) (\s@RunListItem' {} a -> s {stopTime = a} :: RunListItem) Prelude.. Lens.mapping Data._Time

-- | The run\'s storage capacity.
runListItem_storageCapacity :: Lens.Lens' RunListItem (Prelude.Maybe Prelude.Natural)
runListItem_storageCapacity = Lens.lens (\RunListItem' {storageCapacity} -> storageCapacity) (\s@RunListItem' {} a -> s {storageCapacity = a} :: RunListItem)

-- | The run\'s workflow ID.
runListItem_workflowId :: Lens.Lens' RunListItem (Prelude.Maybe Prelude.Text)
runListItem_workflowId = Lens.lens (\RunListItem' {workflowId} -> workflowId) (\s@RunListItem' {} a -> s {workflowId = a} :: RunListItem)

instance Data.FromJSON RunListItem where
  parseJSON =
    Data.withObject
      "RunListItem"
      ( \x ->
          RunListItem'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "priority")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "stopTime")
            Prelude.<*> (x Data..:? "storageCapacity")
            Prelude.<*> (x Data..:? "workflowId")
      )

instance Prelude.Hashable RunListItem where
  hashWithSalt _salt RunListItem' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` stopTime
      `Prelude.hashWithSalt` storageCapacity
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData RunListItem where
  rnf RunListItem' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf stopTime
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf workflowId

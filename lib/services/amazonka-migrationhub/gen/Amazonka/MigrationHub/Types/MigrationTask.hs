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
-- Module      : Amazonka.MigrationHub.Types.MigrationTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHub.Types.MigrationTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types.ResourceAttribute
import Amazonka.MigrationHub.Types.Task
import qualified Amazonka.Prelude as Prelude

-- | Represents a migration task in a migration tool.
--
-- /See:/ 'newMigrationTask' smart constructor.
data MigrationTask = MigrationTask'
  { -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Prelude.Maybe Prelude.Text,
    -- | A name that identifies the vendor of the migration tool being used.
    progressUpdateStream :: Prelude.Maybe Prelude.Text,
    -- | Information about the resource that is being migrated. This data will be
    -- used to map the task to a resource in the Application Discovery Service
    -- repository.
    resourceAttributeList :: Prelude.Maybe [ResourceAttribute],
    -- | Task object encapsulating task information.
    task :: Prelude.Maybe Task,
    -- | The timestamp when the task was gathered.
    updateDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MigrationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'migrationTaskName', 'migrationTask_migrationTaskName' - Unique identifier that references the migration task. /Do not store
-- personal data in this field./
--
-- 'progressUpdateStream', 'migrationTask_progressUpdateStream' - A name that identifies the vendor of the migration tool being used.
--
-- 'resourceAttributeList', 'migrationTask_resourceAttributeList' - Information about the resource that is being migrated. This data will be
-- used to map the task to a resource in the Application Discovery Service
-- repository.
--
-- 'task', 'migrationTask_task' - Task object encapsulating task information.
--
-- 'updateDateTime', 'migrationTask_updateDateTime' - The timestamp when the task was gathered.
newMigrationTask ::
  MigrationTask
newMigrationTask =
  MigrationTask'
    { migrationTaskName = Prelude.Nothing,
      progressUpdateStream = Prelude.Nothing,
      resourceAttributeList = Prelude.Nothing,
      task = Prelude.Nothing,
      updateDateTime = Prelude.Nothing
    }

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
migrationTask_migrationTaskName :: Lens.Lens' MigrationTask (Prelude.Maybe Prelude.Text)
migrationTask_migrationTaskName = Lens.lens (\MigrationTask' {migrationTaskName} -> migrationTaskName) (\s@MigrationTask' {} a -> s {migrationTaskName = a} :: MigrationTask)

-- | A name that identifies the vendor of the migration tool being used.
migrationTask_progressUpdateStream :: Lens.Lens' MigrationTask (Prelude.Maybe Prelude.Text)
migrationTask_progressUpdateStream = Lens.lens (\MigrationTask' {progressUpdateStream} -> progressUpdateStream) (\s@MigrationTask' {} a -> s {progressUpdateStream = a} :: MigrationTask)

-- | Information about the resource that is being migrated. This data will be
-- used to map the task to a resource in the Application Discovery Service
-- repository.
migrationTask_resourceAttributeList :: Lens.Lens' MigrationTask (Prelude.Maybe [ResourceAttribute])
migrationTask_resourceAttributeList = Lens.lens (\MigrationTask' {resourceAttributeList} -> resourceAttributeList) (\s@MigrationTask' {} a -> s {resourceAttributeList = a} :: MigrationTask) Prelude.. Lens.mapping Lens.coerced

-- | Task object encapsulating task information.
migrationTask_task :: Lens.Lens' MigrationTask (Prelude.Maybe Task)
migrationTask_task = Lens.lens (\MigrationTask' {task} -> task) (\s@MigrationTask' {} a -> s {task = a} :: MigrationTask)

-- | The timestamp when the task was gathered.
migrationTask_updateDateTime :: Lens.Lens' MigrationTask (Prelude.Maybe Prelude.UTCTime)
migrationTask_updateDateTime = Lens.lens (\MigrationTask' {updateDateTime} -> updateDateTime) (\s@MigrationTask' {} a -> s {updateDateTime = a} :: MigrationTask) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON MigrationTask where
  parseJSON =
    Data.withObject
      "MigrationTask"
      ( \x ->
          MigrationTask'
            Prelude.<$> (x Data..:? "MigrationTaskName")
            Prelude.<*> (x Data..:? "ProgressUpdateStream")
            Prelude.<*> ( x
                            Data..:? "ResourceAttributeList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Task")
            Prelude.<*> (x Data..:? "UpdateDateTime")
      )

instance Prelude.Hashable MigrationTask where
  hashWithSalt _salt MigrationTask' {..} =
    _salt
      `Prelude.hashWithSalt` migrationTaskName
      `Prelude.hashWithSalt` progressUpdateStream
      `Prelude.hashWithSalt` resourceAttributeList
      `Prelude.hashWithSalt` task
      `Prelude.hashWithSalt` updateDateTime

instance Prelude.NFData MigrationTask where
  rnf MigrationTask' {..} =
    Prelude.rnf migrationTaskName
      `Prelude.seq` Prelude.rnf progressUpdateStream
      `Prelude.seq` Prelude.rnf resourceAttributeList
      `Prelude.seq` Prelude.rnf task
      `Prelude.seq` Prelude.rnf updateDateTime

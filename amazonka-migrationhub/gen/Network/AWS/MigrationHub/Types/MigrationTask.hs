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
-- Module      : Network.AWS.MigrationHub.Types.MigrationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.MigrationTask where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.ResourceAttribute
import Network.AWS.MigrationHub.Types.Task
import qualified Network.AWS.Prelude as Prelude

-- | Represents a migration task in a migration tool.
--
-- /See:/ 'newMigrationTask' smart constructor.
data MigrationTask = MigrationTask'
  { -- | Information about the resource that is being migrated. This data will be
    -- used to map the task to a resource in the Application Discovery Service
    -- repository.
    resourceAttributeList :: Prelude.Maybe [ResourceAttribute],
    -- | The timestamp when the task was gathered.
    updateDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | Task object encapsulating task information.
    task :: Prelude.Maybe Task,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Prelude.Maybe Prelude.Text,
    -- | A name that identifies the vendor of the migration tool being used.
    progressUpdateStream :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MigrationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceAttributeList', 'migrationTask_resourceAttributeList' - Information about the resource that is being migrated. This data will be
-- used to map the task to a resource in the Application Discovery Service
-- repository.
--
-- 'updateDateTime', 'migrationTask_updateDateTime' - The timestamp when the task was gathered.
--
-- 'task', 'migrationTask_task' - Task object encapsulating task information.
--
-- 'migrationTaskName', 'migrationTask_migrationTaskName' - Unique identifier that references the migration task. /Do not store
-- personal data in this field./
--
-- 'progressUpdateStream', 'migrationTask_progressUpdateStream' - A name that identifies the vendor of the migration tool being used.
newMigrationTask ::
  MigrationTask
newMigrationTask =
  MigrationTask'
    { resourceAttributeList =
        Prelude.Nothing,
      updateDateTime = Prelude.Nothing,
      task = Prelude.Nothing,
      migrationTaskName = Prelude.Nothing,
      progressUpdateStream = Prelude.Nothing
    }

-- | Information about the resource that is being migrated. This data will be
-- used to map the task to a resource in the Application Discovery Service
-- repository.
migrationTask_resourceAttributeList :: Lens.Lens' MigrationTask (Prelude.Maybe [ResourceAttribute])
migrationTask_resourceAttributeList = Lens.lens (\MigrationTask' {resourceAttributeList} -> resourceAttributeList) (\s@MigrationTask' {} a -> s {resourceAttributeList = a} :: MigrationTask) Prelude.. Lens.mapping Prelude._Coerce

-- | The timestamp when the task was gathered.
migrationTask_updateDateTime :: Lens.Lens' MigrationTask (Prelude.Maybe Prelude.UTCTime)
migrationTask_updateDateTime = Lens.lens (\MigrationTask' {updateDateTime} -> updateDateTime) (\s@MigrationTask' {} a -> s {updateDateTime = a} :: MigrationTask) Prelude.. Lens.mapping Prelude._Time

-- | Task object encapsulating task information.
migrationTask_task :: Lens.Lens' MigrationTask (Prelude.Maybe Task)
migrationTask_task = Lens.lens (\MigrationTask' {task} -> task) (\s@MigrationTask' {} a -> s {task = a} :: MigrationTask)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
migrationTask_migrationTaskName :: Lens.Lens' MigrationTask (Prelude.Maybe Prelude.Text)
migrationTask_migrationTaskName = Lens.lens (\MigrationTask' {migrationTaskName} -> migrationTaskName) (\s@MigrationTask' {} a -> s {migrationTaskName = a} :: MigrationTask)

-- | A name that identifies the vendor of the migration tool being used.
migrationTask_progressUpdateStream :: Lens.Lens' MigrationTask (Prelude.Maybe Prelude.Text)
migrationTask_progressUpdateStream = Lens.lens (\MigrationTask' {progressUpdateStream} -> progressUpdateStream) (\s@MigrationTask' {} a -> s {progressUpdateStream = a} :: MigrationTask)

instance Prelude.FromJSON MigrationTask where
  parseJSON =
    Prelude.withObject
      "MigrationTask"
      ( \x ->
          MigrationTask'
            Prelude.<$> ( x Prelude..:? "ResourceAttributeList"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "UpdateDateTime")
            Prelude.<*> (x Prelude..:? "Task")
            Prelude.<*> (x Prelude..:? "MigrationTaskName")
            Prelude.<*> (x Prelude..:? "ProgressUpdateStream")
      )

instance Prelude.Hashable MigrationTask

instance Prelude.NFData MigrationTask

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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.ResourceAttribute
import Network.AWS.MigrationHub.Types.Task

-- | Represents a migration task in a migration tool.
--
-- /See:/ 'newMigrationTask' smart constructor.
data MigrationTask = MigrationTask'
  { -- | Information about the resource that is being migrated. This data will be
    -- used to map the task to a resource in the Application Discovery Service
    -- repository.
    resourceAttributeList :: Core.Maybe [ResourceAttribute],
    -- | The timestamp when the task was gathered.
    updateDateTime :: Core.Maybe Core.POSIX,
    -- | Task object encapsulating task information.
    task :: Core.Maybe Task,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Core.Maybe Core.Text,
    -- | A name that identifies the vendor of the migration tool being used.
    progressUpdateStream :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      updateDateTime = Core.Nothing,
      task = Core.Nothing,
      migrationTaskName = Core.Nothing,
      progressUpdateStream = Core.Nothing
    }

-- | Information about the resource that is being migrated. This data will be
-- used to map the task to a resource in the Application Discovery Service
-- repository.
migrationTask_resourceAttributeList :: Lens.Lens' MigrationTask (Core.Maybe [ResourceAttribute])
migrationTask_resourceAttributeList = Lens.lens (\MigrationTask' {resourceAttributeList} -> resourceAttributeList) (\s@MigrationTask' {} a -> s {resourceAttributeList = a} :: MigrationTask) Core.. Lens.mapping Lens._Coerce

-- | The timestamp when the task was gathered.
migrationTask_updateDateTime :: Lens.Lens' MigrationTask (Core.Maybe Core.UTCTime)
migrationTask_updateDateTime = Lens.lens (\MigrationTask' {updateDateTime} -> updateDateTime) (\s@MigrationTask' {} a -> s {updateDateTime = a} :: MigrationTask) Core.. Lens.mapping Core._Time

-- | Task object encapsulating task information.
migrationTask_task :: Lens.Lens' MigrationTask (Core.Maybe Task)
migrationTask_task = Lens.lens (\MigrationTask' {task} -> task) (\s@MigrationTask' {} a -> s {task = a} :: MigrationTask)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
migrationTask_migrationTaskName :: Lens.Lens' MigrationTask (Core.Maybe Core.Text)
migrationTask_migrationTaskName = Lens.lens (\MigrationTask' {migrationTaskName} -> migrationTaskName) (\s@MigrationTask' {} a -> s {migrationTaskName = a} :: MigrationTask)

-- | A name that identifies the vendor of the migration tool being used.
migrationTask_progressUpdateStream :: Lens.Lens' MigrationTask (Core.Maybe Core.Text)
migrationTask_progressUpdateStream = Lens.lens (\MigrationTask' {progressUpdateStream} -> progressUpdateStream) (\s@MigrationTask' {} a -> s {progressUpdateStream = a} :: MigrationTask)

instance Core.FromJSON MigrationTask where
  parseJSON =
    Core.withObject
      "MigrationTask"
      ( \x ->
          MigrationTask'
            Core.<$> ( x Core..:? "ResourceAttributeList"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "UpdateDateTime")
            Core.<*> (x Core..:? "Task")
            Core.<*> (x Core..:? "MigrationTaskName")
            Core.<*> (x Core..:? "ProgressUpdateStream")
      )

instance Core.Hashable MigrationTask

instance Core.NFData MigrationTask

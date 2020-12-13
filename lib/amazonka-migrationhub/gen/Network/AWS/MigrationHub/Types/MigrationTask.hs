{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.MigrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.MigrationTask
  ( MigrationTask (..),

    -- * Smart constructor
    mkMigrationTask,

    -- * Lenses
    mtUpdateDateTime,
    mtResourceAttributeList,
    mtTask,
    mtProgressUpdateStream,
    mtMigrationTaskName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.ResourceAttribute
import Network.AWS.MigrationHub.Types.Task
import qualified Network.AWS.Prelude as Lude

-- | Represents a migration task in a migration tool.
--
-- /See:/ 'mkMigrationTask' smart constructor.
data MigrationTask = MigrationTask'
  { -- | The timestamp when the task was gathered.
    updateDateTime :: Lude.Maybe Lude.Timestamp,
    -- | Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
    resourceAttributeList :: Lude.Maybe [ResourceAttribute],
    -- | Task object encapsulating task information.
    task :: Lude.Maybe Task,
    -- | A name that identifies the vendor of the migration tool being used.
    progressUpdateStream :: Lude.Maybe Lude.Text,
    -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MigrationTask' with the minimum fields required to make a request.
--
-- * 'updateDateTime' - The timestamp when the task was gathered.
-- * 'resourceAttributeList' - Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
-- * 'task' - Task object encapsulating task information.
-- * 'progressUpdateStream' - A name that identifies the vendor of the migration tool being used.
-- * 'migrationTaskName' - Unique identifier that references the migration task. /Do not store personal data in this field./
mkMigrationTask ::
  MigrationTask
mkMigrationTask =
  MigrationTask'
    { updateDateTime = Lude.Nothing,
      resourceAttributeList = Lude.Nothing,
      task = Lude.Nothing,
      progressUpdateStream = Lude.Nothing,
      migrationTaskName = Lude.Nothing
    }

-- | The timestamp when the task was gathered.
--
-- /Note:/ Consider using 'updateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtUpdateDateTime :: Lens.Lens' MigrationTask (Lude.Maybe Lude.Timestamp)
mtUpdateDateTime = Lens.lens (updateDateTime :: MigrationTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {updateDateTime = a} :: MigrationTask)
{-# DEPRECATED mtUpdateDateTime "Use generic-lens or generic-optics with 'updateDateTime' instead." #-}

-- | Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
--
-- /Note:/ Consider using 'resourceAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtResourceAttributeList :: Lens.Lens' MigrationTask (Lude.Maybe [ResourceAttribute])
mtResourceAttributeList = Lens.lens (resourceAttributeList :: MigrationTask -> Lude.Maybe [ResourceAttribute]) (\s a -> s {resourceAttributeList = a} :: MigrationTask)
{-# DEPRECATED mtResourceAttributeList "Use generic-lens or generic-optics with 'resourceAttributeList' instead." #-}

-- | Task object encapsulating task information.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtTask :: Lens.Lens' MigrationTask (Lude.Maybe Task)
mtTask = Lens.lens (task :: MigrationTask -> Lude.Maybe Task) (\s a -> s {task = a} :: MigrationTask)
{-# DEPRECATED mtTask "Use generic-lens or generic-optics with 'task' instead." #-}

-- | A name that identifies the vendor of the migration tool being used.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtProgressUpdateStream :: Lens.Lens' MigrationTask (Lude.Maybe Lude.Text)
mtProgressUpdateStream = Lens.lens (progressUpdateStream :: MigrationTask -> Lude.Maybe Lude.Text) (\s a -> s {progressUpdateStream = a} :: MigrationTask)
{-# DEPRECATED mtProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtMigrationTaskName :: Lens.Lens' MigrationTask (Lude.Maybe Lude.Text)
mtMigrationTaskName = Lens.lens (migrationTaskName :: MigrationTask -> Lude.Maybe Lude.Text) (\s a -> s {migrationTaskName = a} :: MigrationTask)
{-# DEPRECATED mtMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

instance Lude.FromJSON MigrationTask where
  parseJSON =
    Lude.withObject
      "MigrationTask"
      ( \x ->
          MigrationTask'
            Lude.<$> (x Lude..:? "UpdateDateTime")
            Lude.<*> (x Lude..:? "ResourceAttributeList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Task")
            Lude.<*> (x Lude..:? "ProgressUpdateStream")
            Lude.<*> (x Lude..:? "MigrationTaskName")
      )

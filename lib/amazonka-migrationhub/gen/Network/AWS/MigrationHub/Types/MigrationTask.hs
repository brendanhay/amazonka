{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.MigrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MigrationHub.Types.MigrationTask
  ( MigrationTask (..)
  -- * Smart constructor
  , mkMigrationTask
  -- * Lenses
  , mtMigrationTaskName
  , mtProgressUpdateStream
  , mtResourceAttributeList
  , mtTask
  , mtUpdateDateTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types.MigrationTaskName as Types
import qualified Network.AWS.MigrationHub.Types.ProgressUpdateStream as Types
import qualified Network.AWS.MigrationHub.Types.ResourceAttribute as Types
import qualified Network.AWS.MigrationHub.Types.Task as Types
import qualified Network.AWS.Prelude as Core

-- | Represents a migration task in a migration tool.
--
-- /See:/ 'mkMigrationTask' smart constructor.
data MigrationTask = MigrationTask'
  { migrationTaskName :: Core.Maybe Types.MigrationTaskName
    -- ^ Unique identifier that references the migration task. /Do not store personal data in this field./ 
  , progressUpdateStream :: Core.Maybe Types.ProgressUpdateStream
    -- ^ A name that identifies the vendor of the migration tool being used.
  , resourceAttributeList :: Core.Maybe [Types.ResourceAttribute]
    -- ^ Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
  , task :: Core.Maybe Types.Task
    -- ^ Task object encapsulating task information.
  , updateDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the task was gathered.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MigrationTask' value with any optional fields omitted.
mkMigrationTask
    :: MigrationTask
mkMigrationTask
  = MigrationTask'{migrationTaskName = Core.Nothing,
                   progressUpdateStream = Core.Nothing,
                   resourceAttributeList = Core.Nothing, task = Core.Nothing,
                   updateDateTime = Core.Nothing}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./ 
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtMigrationTaskName :: Lens.Lens' MigrationTask (Core.Maybe Types.MigrationTaskName)
mtMigrationTaskName = Lens.field @"migrationTaskName"
{-# INLINEABLE mtMigrationTaskName #-}
{-# DEPRECATED migrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead"  #-}

-- | A name that identifies the vendor of the migration tool being used.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtProgressUpdateStream :: Lens.Lens' MigrationTask (Core.Maybe Types.ProgressUpdateStream)
mtProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# INLINEABLE mtProgressUpdateStream #-}
{-# DEPRECATED progressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead"  #-}

-- | Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
--
-- /Note:/ Consider using 'resourceAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtResourceAttributeList :: Lens.Lens' MigrationTask (Core.Maybe [Types.ResourceAttribute])
mtResourceAttributeList = Lens.field @"resourceAttributeList"
{-# INLINEABLE mtResourceAttributeList #-}
{-# DEPRECATED resourceAttributeList "Use generic-lens or generic-optics with 'resourceAttributeList' instead"  #-}

-- | Task object encapsulating task information.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtTask :: Lens.Lens' MigrationTask (Core.Maybe Types.Task)
mtTask = Lens.field @"task"
{-# INLINEABLE mtTask #-}
{-# DEPRECATED task "Use generic-lens or generic-optics with 'task' instead"  #-}

-- | The timestamp when the task was gathered.
--
-- /Note:/ Consider using 'updateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtUpdateDateTime :: Lens.Lens' MigrationTask (Core.Maybe Core.NominalDiffTime)
mtUpdateDateTime = Lens.field @"updateDateTime"
{-# INLINEABLE mtUpdateDateTime #-}
{-# DEPRECATED updateDateTime "Use generic-lens or generic-optics with 'updateDateTime' instead"  #-}

instance Core.FromJSON MigrationTask where
        parseJSON
          = Core.withObject "MigrationTask" Core.$
              \ x ->
                MigrationTask' Core.<$>
                  (x Core..:? "MigrationTaskName") Core.<*>
                    x Core..:? "ProgressUpdateStream"
                    Core.<*> x Core..:? "ResourceAttributeList"
                    Core.<*> x Core..:? "Task"
                    Core.<*> x Core..:? "UpdateDateTime"

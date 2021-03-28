{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.Task
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MigrationHub.Types.Task
  ( Task (..)
  -- * Smart constructor
  , mkTask
  -- * Lenses
  , tStatus
  , tProgressPercent
  , tStatusDetail
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types.MigrationStatus as Types
import qualified Network.AWS.MigrationHub.Types.StatusDetail as Types
import qualified Network.AWS.Prelude as Core

-- | Task object encapsulating task information.
--
-- /See:/ 'mkTask' smart constructor.
data Task = Task'
  { status :: Types.MigrationStatus
    -- ^ Status of the task - Not Started, In-Progress, Complete.
  , progressPercent :: Core.Maybe Core.Natural
    -- ^ Indication of the percentage completion of the task.
  , statusDetail :: Core.Maybe Types.StatusDetail
    -- ^ Details of task status as notified by a migration tool. A tool might use this field to provide clarifying information about the status that is unique to that tool or that explains an error state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Task' value with any optional fields omitted.
mkTask
    :: Types.MigrationStatus -- ^ 'status'
    -> Task
mkTask status
  = Task'{status, progressPercent = Core.Nothing,
          statusDetail = Core.Nothing}

-- | Status of the task - Not Started, In-Progress, Complete.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStatus :: Lens.Lens' Task Types.MigrationStatus
tStatus = Lens.field @"status"
{-# INLINEABLE tStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Indication of the percentage completion of the task.
--
-- /Note:/ Consider using 'progressPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tProgressPercent :: Lens.Lens' Task (Core.Maybe Core.Natural)
tProgressPercent = Lens.field @"progressPercent"
{-# INLINEABLE tProgressPercent #-}
{-# DEPRECATED progressPercent "Use generic-lens or generic-optics with 'progressPercent' instead"  #-}

-- | Details of task status as notified by a migration tool. A tool might use this field to provide clarifying information about the status that is unique to that tool or that explains an error state.
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStatusDetail :: Lens.Lens' Task (Core.Maybe Types.StatusDetail)
tStatusDetail = Lens.field @"statusDetail"
{-# INLINEABLE tStatusDetail #-}
{-# DEPRECATED statusDetail "Use generic-lens or generic-optics with 'statusDetail' instead"  #-}

instance Core.FromJSON Task where
        toJSON Task{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Status" Core..= status),
                  ("ProgressPercent" Core..=) Core.<$> progressPercent,
                  ("StatusDetail" Core..=) Core.<$> statusDetail])

instance Core.FromJSON Task where
        parseJSON
          = Core.withObject "Task" Core.$
              \ x ->
                Task' Core.<$>
                  (x Core..: "Status") Core.<*> x Core..:? "ProgressPercent" Core.<*>
                    x Core..:? "StatusDetail"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.MigrationTaskSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.MigrationTaskSummary
  ( MigrationTaskSummary (..),

    -- * Smart constructor
    mkMigrationTaskSummary,

    -- * Lenses
    mtsMigrationTaskName,
    mtsProgressPercent,
    mtsProgressUpdateStream,
    mtsStatus,
    mtsStatusDetail,
    mtsUpdateDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types.MigrationStatus as Types
import qualified Network.AWS.MigrationHub.Types.MigrationTaskName as Types
import qualified Network.AWS.MigrationHub.Types.ProgressUpdateStream as Types
import qualified Network.AWS.MigrationHub.Types.StatusDetail as Types
import qualified Network.AWS.Prelude as Core

-- | MigrationTaskSummary includes @MigrationTaskName@ , @ProgressPercent@ , @ProgressUpdateStream@ , @Status@ , and @UpdateDateTime@ for each task.
--
-- /See:/ 'mkMigrationTaskSummary' smart constructor.
data MigrationTaskSummary = MigrationTaskSummary'
  { -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Core.Maybe Types.MigrationTaskName,
    -- | Indication of the percentage completion of the task.
    progressPercent :: Core.Maybe Core.Natural,
    -- | An AWS resource used for access control. It should uniquely identify the migration tool as it is used for all updates made by the tool.
    progressUpdateStream :: Core.Maybe Types.ProgressUpdateStream,
    -- | Status of the task.
    status :: Core.Maybe Types.MigrationStatus,
    -- | Detail information of what is being done within the overall status state.
    statusDetail :: Core.Maybe Types.StatusDetail,
    -- | The timestamp when the task was gathered.
    updateDateTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'MigrationTaskSummary' value with any optional fields omitted.
mkMigrationTaskSummary ::
  MigrationTaskSummary
mkMigrationTaskSummary =
  MigrationTaskSummary'
    { migrationTaskName = Core.Nothing,
      progressPercent = Core.Nothing,
      progressUpdateStream = Core.Nothing,
      status = Core.Nothing,
      statusDetail = Core.Nothing,
      updateDateTime = Core.Nothing
    }

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsMigrationTaskName :: Lens.Lens' MigrationTaskSummary (Core.Maybe Types.MigrationTaskName)
mtsMigrationTaskName = Lens.field @"migrationTaskName"
{-# DEPRECATED mtsMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Indication of the percentage completion of the task.
--
-- /Note:/ Consider using 'progressPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsProgressPercent :: Lens.Lens' MigrationTaskSummary (Core.Maybe Core.Natural)
mtsProgressPercent = Lens.field @"progressPercent"
{-# DEPRECATED mtsProgressPercent "Use generic-lens or generic-optics with 'progressPercent' instead." #-}

-- | An AWS resource used for access control. It should uniquely identify the migration tool as it is used for all updates made by the tool.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsProgressUpdateStream :: Lens.Lens' MigrationTaskSummary (Core.Maybe Types.ProgressUpdateStream)
mtsProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# DEPRECATED mtsProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Status of the task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsStatus :: Lens.Lens' MigrationTaskSummary (Core.Maybe Types.MigrationStatus)
mtsStatus = Lens.field @"status"
{-# DEPRECATED mtsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Detail information of what is being done within the overall status state.
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsStatusDetail :: Lens.Lens' MigrationTaskSummary (Core.Maybe Types.StatusDetail)
mtsStatusDetail = Lens.field @"statusDetail"
{-# DEPRECATED mtsStatusDetail "Use generic-lens or generic-optics with 'statusDetail' instead." #-}

-- | The timestamp when the task was gathered.
--
-- /Note:/ Consider using 'updateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtsUpdateDateTime :: Lens.Lens' MigrationTaskSummary (Core.Maybe Core.NominalDiffTime)
mtsUpdateDateTime = Lens.field @"updateDateTime"
{-# DEPRECATED mtsUpdateDateTime "Use generic-lens or generic-optics with 'updateDateTime' instead." #-}

instance Core.FromJSON MigrationTaskSummary where
  parseJSON =
    Core.withObject "MigrationTaskSummary" Core.$
      \x ->
        MigrationTaskSummary'
          Core.<$> (x Core..:? "MigrationTaskName")
          Core.<*> (x Core..:? "ProgressPercent")
          Core.<*> (x Core..:? "ProgressUpdateStream")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusDetail")
          Core.<*> (x Core..:? "UpdateDateTime")
